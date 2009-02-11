#!/usr/bin/perl

# Launch X dubdub compute instances

unless($ARGV[0])
{
  die "Cluster name required!";
}

unless($ARGV[1])
{
  die "Must specify the number of instances to start.";
}

unless($ARGV[1] < 10)
{
    die "Cannot instantiate more than 10 instances.";
}

my $AMI_NAME = '';

my $cluster_name = $ARGV[0];
my $num_instances = $ARGV[1];

# Launch instances
my $instance_command = "ec2-run-instances " . $AMI_NAME . " -k " . $cluster_name . ".keypair --instance-type m1.small -z us-east-1a -n $num_instances";
my $instance_info = `$instance_command`;
my @instance_fields = split /\t/, $instance_info;

