#!/usr/bin/perl

# Launch DubDub Master Instance and Attach test data

unless($ARGV[0])
{
  die "Cluster name required!";
}

my $cluster_name = $ARGV[0];
my $num_instances = $ARGV[1];

# Default AMI
my $AMI_NAME='';

# Generate a keypair for the cluster and store it in EC2 directory
system("ec2-add-keypair " . $cluster_name . ".keypair >  ~/.ec2/" . $cluster_name . ".pem");
system("chmod 600  ~/.ec2/" . $cluster_name . ".pem");

# Launch master instance
my $instance_command = "ec2-run-instances " . $AMI_NAME . " -k " . $cluster_name . ".keypair --instance-type m1.small -z us-east-1a -n $num_instances";
my $instance_info = `$instance_command`;
my @instance_fields = split /\t/, $instance_info;

# Create EBS Volume for Data Store from Econ Snapshot
my $ebs_info = `ec2-create-volume -s 230 -z us-east-1a --snapshot snap-0bdf3f62`;
my @ebs_fields = split /\t/, $ebs_info;
my $volume_id = $ebs_fields[1];

# Attach EBS Volume to Master Instance
my $attach_command = "ec2-attach-volume $volume_id -i " . $instance_fields[1] . " -d /dev/econdata";
my $attach_output = `$attach_command`;




