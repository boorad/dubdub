#!/usr/bin/perl

#use strict;
use warnings;

# Launch DubDub Master Instance and Attach test data
use Getopt::Std;
#use Data::Dump qw/dump/;
use Net::Amazon::EC2;

#use version 0.74; our $VERSION = qv('0.01');

# Print shite immediately
$|=1;

my %opts;
getopts('n:a:k:i:', \%opts);    # options as above. Values in %opts

unless ($opts{n})
{
    die "Cluster name required!";
}

my $cluster_name = $opts{n};

# Default AMI
my $AMI_NAME = $opts{a} || 'ami-5ccd2a35';

my $key_id     = $opts{i} || $ENV{AWS_ACCESS_KEY_ID};
my $secret_key = $opts{k} || $ENV{AWS_SECRET_ACCESS_KEY};

my $instance_type = $opts{t} || 'm1.small';

print "\nChecking keypair...\n";

# Only create a keypair if it does not already exist
unless (-e $ENV{HOME} . "/.ec2/$cluster_name.pem")
{
    print "\nGenerating keypair...\n";

    # Generate a keypair for the cluster and store it in EC2 directory
    system(  "ec2-add-keypair "
           . $cluster_name
           . ".keypair >  ~/.ec2/"
           . $cluster_name
           . ".pem");
    system("chmod 600  ~/.ec2/" . $cluster_name . ".pem");
}
else
{
    print "\nUsing existing keypair...\n";
}

my $ec2 = Net::Amazon::EC2->new(AWSAccessKeyId  => $key_id,
                                SecretAccessKey => $secret_key,);

# Add security group for this cluster
my $create_group = "ec2-add-group $cluster_name -d 'Foobar dubdub compute cluster security group.'";
print "\nCreating security group $create_group...\n";
print `$create_group`;

# Authorize ports on security group
my $ssh_port = "ec2-authorize $cluster_name -p 22 -P tcp";
print "\nCommand to autorize ssh port 22: $ssh_port\n";
print `$ssh_port`;

# Authorize epmd ports
my $epmd = "ec2-authorize $cluster_name -p 4367-4369 -P tcp";
print "\nCommand to authorize epmd ports 4367-4369: $epmd\n";
print `$epmd`;

# Authorize our limited erlang IPC port: 4000
my $erlang_port = "ec2-authorize $cluster_name -p 4000-4200 -P tcp";
print "\nCommand to authorize erlang IPC port 4000-4200: $erlang_port\n";
print `$erlang_port`;

my $instance_command =
    "ec2-run-instances " . " -g $cluster_name "
  . $AMI_NAME . " -k "
  . $cluster_name
  . ".keypair --instance-type $instance_type -z us-east-1a -n 1";
print "$instance_command\n";
print "\nLaunching master instance...\n";
my $instance_info = `$instance_command`;

print "$instance_info\n";

# Get master instance ID
my @instance_fields = split /\t/, $instance_info;

my $instance_id = $instance_fields[4];

#print dump(@instance_fields);

# Create EBS Volume for Data Store from Econ Snapshot
print "\nCreating EBS Volumes from stats data...\n\n";

my $ebs_success = 1;
my ($volume_id, $ebs_info, @ebs_fields);
EBS: while ($ebs_success)
{
    $ebs_info =
      `ec2-create-volume -s 300 -z us-east-1a --snapshot snap-0bdf3f62`;#snap-b04ba2d9`;

    # Mulligan if we get a timeout.
    if ($ebs_info =~ m/timeout/g)
    {
        print "\nEBS Allocation timeout, retrying.\n";
    }
    else
    {
        print "\nEBS Allocation successful...\n";
        $ebs_success = 0;
        print $ebs_info . "\n";
        @ebs_fields = split /\t/, $ebs_info;
        $volume_id = $ebs_fields[1];
    }
}

print "\nWaiting for EC2 instance to show up to attach EBS...\n";
my $booted = 0;
my $master_hostname;

# Wait for that instance to boot, then attach the EBS volume
while ($booted == 0)
{
    my $running_instances =
      $ec2->describe_instances({InstanceId => $instance_id});

    foreach my $reservation (@$running_instances)
    {
        foreach my $instance ($reservation->instances_set)
        {
            if ($instance->instance_state->name eq 'running')
            {
                print "\nInstance booted, attaching EBS Volume...\n";
                $booted = 1;

                # Attach EBS Volume to Master Instance
                my $attach_command =
                    "ec2-attach-volume $volume_id -i "
                  . $instance_id
                  . " -d /dev/sdf";
                print "\nCommand: $attach_command\n";
                system($attach_command);


                print "\nWaiting 30 seconds for master to boot to mount EBS Volume...\n";
                sleep 30;

                # First things first, set this thing to die in 50 minutes to save cash
                print "\nConfiguring Master to die in 50 minutes...\n";
                my $shut =
                    "ssh -o StrictHostKeyChecking=no -i ~/.ec2/"
                  . $cluster_name
                  . ".pem root@"
                  . $instance->dns_name
                  . " 'shutdown -h +50 >/dev/null &'";
                print "\nCommand: $shut\n";
                system($shut);

                print "\nMounting EBS Volume via SSH...\n";
                my $com =
                    "ssh -o StrictHostKeyChecking=no -i ~/.ec2/"
                  . $cluster_name
                  . ".pem root@"
                  . $instance->dns_name
                  . " 'ls /dev/sdf; mkdir /mnt/stats; mount /dev/sdf /mnt/stats;ls /mnt/stats'";
                print "\nCommand: $com\n";
                system($com);

                # Get ze source...
                print "\nGrabbing source from Github...\n";
                my $git =
                    "ssh -o StrictHostKeyChecking=no -i ~/.ec2/"
                  . $cluster_name
                  . ".pem root@"
                  . $instance->dns_name
                  . " 'pwd;cd dubdub;pwd;git pull;echo \"We pulled git!\n\"'";

                print "\nCommand: $git\n";
                system($git);

                # Get ze data
                my $wget =
                    "ssh -o StrictHostKeyChecking=no -i ~/.ec2/"
                  . $cluster_name
                  . ".pem root@"
                  . $instance->dns_name
                  . " 'pwd;cd dubdub/dev/data; wget http://www.perfcloud.com/tuple.dat'";
                print "\nCommand: $wget\n";
                system($wget);

                # Launch ze server
                print "\nActivating boot node...\n";
                my $boot =
                    "ssh -o StrictHostKeyChecking=no -i ~/.ec2/"
                  . $cluster_name
                  . ".pem root@"
                  . $instance->dns_name
                  . " 'pwd;cd dubdub;pwd;make clean;make'";

                print "\nCommand: $boot\n";
                system($boot);

                # Assign master hostname for booting slaves
                $master_hostname = $instance->private_dns_name;

                print "\n\nMaster hostname for slaves will be: $master_hostname\n\nYou will soon be in erl console, then try something like ./launch_slaves.pl -n $cluster_name -s 5 -m $master_hostname";

                # Launch ze server
                print "\nActivating boot node...\n";
                my $runage =
                    "ssh -o StrictHostKeyChecking=no -i ~/.ec2/"
                  . $cluster_name
                  . ".pem root@"
                  . $instance->dns_name
                  . " 'cd dubdub/ebin;../bin/start.sh -n boot'";

                print "\nCommand: $runage\n";
                system($runage);

            }
            else
            {
                print ".\n";
                sleep 5;
            }
        }
    }
}
