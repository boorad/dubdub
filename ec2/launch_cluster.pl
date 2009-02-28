#!/usr/bin/perl

#use strict;
use warnings;

# Launch DubDub Master Instance and Attach test data
use Getopt::Std;
use Data::Dump qw/dump/;
use Net::Amazon::EC2;
use Data::Dumper;

my %opts;
getopts('n:a:s:k:i:', \%opts);    # options as above. Values in %opts

unless ($opts{n})
{
    die "Cluster name required!";
}

my $cluster_name = $opts{n};

# Default AMI
my $AMI_NAME = $opts{a} || 'ami-5ccd2a35';

my $cluster_size = $opts{s} || 1;

my $key_id     = $opts{i} || $ENV{AWS_ACCESS_KEY_ID};
my $secret_key = $opts{k} || $ENV{AWS_SECRET_ACCESS_KEY};

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

my $instance_command =
    "ec2-run-instances "
  . $AMI_NAME . " -k "
  . $cluster_name
  . ".keypair --instance-type m1.small -z us-east-1a -n 1";
print "$instance_command\n";
print "\nLaunching master instance...\n";
my $instance_info = `$instance_command`;

print "$instance_info\n";

# Get master instance ID
my @instance_fields = split /\t/, $instance_info;

my $instance_id = $instance_fields[4];

print dump(@instance_fields);

# Create EBS Volume for Data Store from Econ Snapshot
print "\nCreating EBS Volumes from stats data...\n\n";

my $ebs_success = 1;
my ($volume_id, $ebs_info, @ebs_fields);
EBS: while ($ebs_success)
{
    $ebs_info =
      `ec2-create-volume -s 230 -z us-east-1a --snapshot snap-0bdf3f62`;

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

#$vol_boot = 0;
#while($vol_boot)
#{
#  my $foo = `ec2-describe-volumes|grep $volume_id`;
#  my @values = split(/\t/, $foo);
#  if($values[5] eq 'available')
#  {
#    $vol_boot = 1;
#    print "EBS available\n";
#  }
#  else
#  {
#    print ".\n";
#  }
#}

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
                my $attach_output = `$attach_command`;
                print $attach_output;

                print "\nWaiting 10 seconds to mount EBS Volume...\n";
                sleep 10;

                print "\nMounting EBS Volume via SSH...\n";
                my $com =
                    "ssh -o StrictHostKeyChecking=no -i ~/.ec2/"
                  . $cluster_name
                  . ".pem root@"
                  . $instance->dns_name
                  . " 'ls /dev/sdf; mkdir /mnt/stats; mount /dev/sdf /mnt/stats;ls /mnt/stats'";
                print `$com`;

                # Get ze source...
                print "\nGrabbing source from Github...\n";
                my $git =
                    "ssh -o StrictHostKeyChecking=no -i ~/.ec2/"
                  . $cluster_name
                  . ".pem root@"
                  . $instance->dns_name
                  . " 'pwd;cd dubdub;pwd;git pull;echo \"We pulled git!\n\"'";
                  
                print "Command: $git\n";
                print `$git`;
                
                # Launch ze server
                print "\nActivating boot node...\n";
                my $boot =
                    "ssh -o StrictHostKeyChecking=no -i ~/.ec2/"
                  . $cluster_name
                  . ".pem root@"
                  . $instance->dns_name
                  . " 'pwd;cd dubdub;pwd;make clean;make;cd ebin;../bin/start.sh -n boot'";

                print "\nCommand: $boot\n";
                system($boot);
                sleep 20;

                # Assign master hostname for booting slaves
                $master_hostname = $instance->dns_name;

            }
            else
            {
                print ".\n";
                sleep 5;
            }
        }
    }
}

# Now launch slaves.
if ($cluster_size > 1)
{
    my $num_slaves = ($cluster_size - 1);

    # For to let slaves link to master
    my $master_nodename = "boot\@$master_hostname";
    
    print "\nMaster nodename: $master_nodename\n";

    # Launch all the slaves.
    my $instance_command =
        "ec2-run-instances "
      . $AMI_NAME . " -k "
      . $cluster_name
      . ".keypair --instance-type m1.small -z us-east-1a -n $num_slaves";
    print "$instance_command\n";
    print "\nLaunching slave instance(s)...\n";
    my $instance_info = `$instance_command`;

    print "$instance_info\n";

    # Start with the master in the init array
    my @initialized_instances = ($master_hostname);
    my $worker_counter = 0;

    # Loop until all slave instances are taken care of
    while (1)
    {
        # Stop when all the instances are initialized
        exit if @initialized_instances >= $cluster_size;
        
        my $running_instances = $ec2->describe_instances();

        foreach my $reservation (@$running_instances)
        {
            foreach my $instance ($reservation->instances_set)
            {

                # Skip instances we have already initialized
                my $instance_name = $instance->dns_name;
                next unless $instance_name;
                next if grep /$instance_name/, @initialized_instances;

                if ($instance->instance_state->name eq 'running')
                {
                    print "\nInstance state: " . $instance->instance_state->name . "\n";
                    sleep 5;
                    
                    # Increment to get a unique worker ID for this instance
                    $worker_counter++;
                    
                    # Get ze source...
                    print "\nGrabbing source from Github...\n";
                    my $git =
                        "ssh -o StrictHostKeyChecking=no -i ~/.ec2/"
                      . $cluster_name
                      . ".pem root@"
                      . $instance->dns_name
                      . " 'pwd;cd dubdub;pwd;git pull;echo \"We pulled git!\n\"'";
                      
                    print "\nCommand: $git\n";
                    print `$git`;
                    
                    # Launch ze server
                    print "\nActivating slave node...\n";
                    my $boot =
                        "ssh -o StrictHostKeyChecking=no -i ~/.ec2/"
                      . $cluster_name
                      . ".pem root@"
                      . $instance->dns_name
                      . " 'pwd;cd dubdub;pwd;git pull;ls;make clean;make;ls ./ebin;cd ./ebin;../bin/start.sh -n worker$worker_counter -m $master_nodename'";

                    print "\nCommand: $boot\n";
                    print `$boot`;

                    # Set as initialized
                    push @initialized_instances, $instance_name;

                }
                else
                {
                    print ".\n";
                    sleep 5;
                }
            }
        }

    }

}

