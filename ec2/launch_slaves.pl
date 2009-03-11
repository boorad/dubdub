#!/usr/bin/perl

#use strict;
use warnings;

# Launch DubDub Master Instance and Attach test data
use Getopt::Std;
#use Data::Dump qw/dump/;
use Net::Amazon::EC2;

# Print shite immediately
$| = 1;

my %opts;
getopts('n:a:s:k:i:m:', \%opts);    # options as above. Values in %opts

unless ($opts{n})
{
    die "Cluster name (-n) required!";
}

unless ($opts{m})
{
    die "Master hostname (-m) required!";
}

my $cluster_name    = $opts{n};
my $master_hostname = $opts{m};
my $master_nodename = "boot\@$master_hostname";

print "Master nodename: $master_nodename\n";

# Default AMI
my $AMI_NAME = $opts{a} || 'ami-5ccd2a35';

my $num_slaves = $opts{s} || 1;

my $key_id     = $opts{i} || $ENV{AWS_ACCESS_KEY_ID};
my $secret_key = $opts{k} || $ENV{AWS_SECRET_ACCESS_KEY};

print "\nChecking keypair...\n";

# Only create a keypair if it does not already exist
unless (-e $ENV{HOME} . "/.ec2/$cluster_name.pem")
{
    die "Must run master and generate cluster keypair first!";
}
else
{
    print "\nUsing existing keypair...\n";
}

my $ec2 = Net::Amazon::EC2->new(AWSAccessKeyId  => $key_id,
                                SecretAccessKey => $secret_key,);

# Note existing instances so we skip them
my @initialized_instances;

# Initialize instances array with pre-existing instances
my $running_instances = $ec2->describe_instances();

foreach my $reservation (@$running_instances)
{
    foreach my $instance ($reservation->instances_set)
    {

        # Set as initialized if so
        if(defined($instance->private_dns_name))
        {
            push @initialized_instances, $instance->private_dns_name;
        }
    }
}

#print "\nExisting EC2 Instances: \n";
#print dump(@initialized_instances) . "\n";

my $num_init_instances = @initialized_instances;

# Now launch slaves.

print "\nMaster nodename: $master_nodename\n";

# Launch all the slaves.
my $instance_command =
    "ec2-run-instances " . " -g $cluster_name "
  . $AMI_NAME . " -k "
  . $cluster_name 
  . ".keypair --instance-type m1.small -z us-east-1a -n $num_slaves";
print "$instance_command\n";
print "\nLaunching slave instance(s)...\n";
my $instance_info = `$instance_command`;

print "$instance_info\n";

# Counter for assigning worker numbers to slave node names, i.e. worker1, worker2, etc.
my $worker_counter = 0;

# Flag to sleep 30 seconds before booting the first instance, giving it a chance to boot reliably
my $first_instance = 1;

# Loop until all slave instances are taken care of
while (@initialized_instances < ($num_slaves + $num_init_instances))
{
    print ".";

    my $running_instances = $ec2->describe_instances();

    foreach my $reservation (@$running_instances)
    {
        foreach my $instance ($reservation->instances_set)
        {

            # Skip instances we have already initialized
            my $instance_name = $instance->dns_name;
            my $internal_name = $instance->private_dns_name;

            next unless $instance->can('private_dns_name') and defined($instance->private_dns_name) and $instance->private_dns_name;
            next if grep /$internal_name/, @initialized_instances;

            print "Instance name: $instance_name / $internal_name\n";

            if ($instance->instance_state->name eq 'running')
            {
                if($first_instance)
                {
                    print "\nInstance $internal_name / $instance_name state: "
                      . $instance->instance_state->name
                      . "we will sleep 30 seconds while it boots...\n";
    
                    # None of the other instances need to sleep, they wil be well booted.
                    $first_instance = 0;
                    sleep 30;
                }
                else
                {
                    print "\nInstance $internal_name / $instance_name state: "
                      . $instance->instance_state->name . "\n";
                }

                # Increment to get a unique worker ID for this instance
                $worker_counter++;

                # First things first, set this thing to die in 50 minutes to save cash
                print "\nConfiguring Slave to die in 50 minutes...\n";
                my $shut =
                    "ssh -o StrictHostKeyChecking=no -i ~/.ec2/"
                  . $cluster_name
                  . ".pem root@"
                  . $instance->dns_name
                  . " 'shutdown -h +50 >/dev/null &'";
                print "\nCommand: $shut\n";
                print `$shut`;

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
                  . " 'pwd;cd dubdub/src;ls;make;ls ../ebin;cd ../ebin;../bin/start.sh -n worker$worker_counter -d -m $master_nodename'";

                print "\nCommand: $boot\n";
                system($boot);

                # Set as initialized
                push @initialized_instances, $instance->private_dns_name;

            }
            else
            {
                print ".\n";
                sleep 5;
            }
        }
    }
}

