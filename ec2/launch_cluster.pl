#!/usr/bin/perl

# This script will launch a cluster of dubdub/Erlang compute nodes named $1 of $2
# size, optionally mount an S3 image $3 to an EBS block on all of them and they
# will all discover one another via s3nodefinder (and ec2nodefinder once the bug
# is fixed).

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

system("./launch_master.pl $ARGV[0]");

system("./launch_slaves.pl $ARGV[1] $ARGV[2]");
