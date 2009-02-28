#!/usr/bin/perl

#use strict;
use warnings;

# Launch DubDub Master Instance and Attach test data
use Getopt::Std;
use Data::Dump qw/dump/;
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
    die "Master name (-m) required!";
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

# Now launch slaves.

print "\nMaster nodename: $master_nodename\n";

# Launch all the slaves.
my $instance_command =
    "ec2-run-instances "
  . $AMI_NAME . " -k "
  . $cluster_name
  . ".keypair --instance-type m1.small -z us-east-1a -n $num_slaves --user-data '#!bin/sh
  shutdown -h +50 >/dev/null &' ";
print "$instance_command\n";
print "\nLaunching slave instance(s)...\n";
my $instance_info = `$instance_command`;

print "$instance_info\n";

# Start with the master in the init array
my @initialized_instances = ($master_hostname);
my $worker_counter        = 0;

print "Sleep 30 while our instances boot...";
sleep 30;

# Loop until all slave instances are taken care of
while (@initialized_instances < ($num_slaves + 1))
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

            next unless $internal_name;
            next if grep /$internal_name/, @initialized_instances;

            print "Instance name: $instance_name\n";

            if ($instance->instance_state->name eq 'running')
            {
                print "\nInstance $internal_name / $instance_name state: "
                  . $instance->instance_state->name
                  . " we will sleep 30 while it boots\n";

                # Increment to get a unique worker ID for this instance
                $worker_counter++;

                # Get ze source...
                print "\nGrabbing source from Github...\n";
                my $git =
                    "ssh -o StrictHostKeyChecking=no -i ~/.ec2/"
                  . $cluster_name
                  . ".pem root@"
                  . $instance->dns_name
                  . " 'shutdown -h +50 >/dev/null &;pwd;cd dubdub;pwd;git pull;echo \"We pulled git!\n\"'";

                print "\nCommand: $git\n";
                print `$git`;

                # Launch ze server
                print "\nActivating slave node...\n";
                my $boot =
                    "ssh -o StrictHostKeyChecking=no -i ~/.ec2/"
                  . $cluster_name
                  . ".pem root@"
                  . $instance->dns_name
                  . " 'pwd;cd dubdub;pwd;git pull;ls;make clean;make;ls ./ebin;cd ./ebin;../bin/start.sh -n worker$worker_counter -d -m $master_nodename'";

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

