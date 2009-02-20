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
my $AMI_NAME = $opts{a} || 'ami-aafa1dc3';

my $cluster_size = $opts{s} || 1;

my $key_id     = $opts{i} || $ENV{AWS_ACCESS_KEY_ID};
my $secret_key = $opts{k} || $ENV{AWS_SECRET_ACCESS_KEY};

# Generate a keypair for the cluster and store it in EC2 directory
system(  "ec2-add-keypair "
       . $cluster_name
       . ".keypair >  ~/.ec2/"
       . $cluster_name
       . ".pem");
system("chmod 600  ~/.ec2/" . $cluster_name . ".pem");

my $ec2 = Net::Amazon::EC2->new(AWSAccessKeyId  => $key_id,
                                SecretAccessKey => $secret_key,);

#$ec2->create_security_group({GroupName => $cluster_name, GroupDescription => $cluster_name});

# Run master instance
#my $instance =
#  $ec2->run_instances(
#                      ImageId                      => $AMI_NAME,
#                      MinCount                     => 1,
#                      MaxCount                     => 1,
#                      #KeyName                      => "$cluster_name.keypair",
#                      #SecurityGroup                => $cluster_name,
#                      InstanceType                 => 'm1.small',
#                      'Placement.AvailabilityZone' => 'us-east-1a',
#                     );
#
#print dump($instance);

my $instance_command =
    "ec2-run-instances "
  . $AMI_NAME . " -k "
  . $cluster_name
  . ".keypair --instance-type m1.small -z us-east-1a -n $cluster_size";
print "$instance_command\n";
my $instance_info = `$instance_command`;

print "$instance_info\n";

# Get master instance ID
my @instance_fields = split /\t/, $instance_info;
my $instance_id = $instance_fields[4];

#print dump(@instance_fields);

# Create EBS Volume for Data Store from Econ Snapshot
my $ebs_info =
  `ec2-create-volume -s 230 -z us-east-1a --snapshot snap-0bdf3f62`;
print $ebs_info;
my @ebs_fields = split /\t/, $ebs_info;
my $volume_id = $ebs_fields[1];

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

my $booted = 0;
# Wait for that instance to boot, then attach the EBS volume
while($booted == 0)
{
  my $running_instances = $ec2->describe_instances({InstanceId => $instance_id});
  
   foreach my $reservation (@$running_instances) {
      foreach my $instance ($reservation->instances_set)
      {
          if($instance->instance_state->name eq 'running')
          {
            $booted = 1;
            
            # Attach EBS Volume to Master Instance
            my $attach_command = "ec2-attach-volume $volume_id -i " . $instance_id . " -d /dev/sdf";
            my $attach_output = `$attach_command`;
            print $attach_output;
            
            sleep 10;
            
            my $com = "ssh -i ~/.ec2/" . $cluster_name . ".pem root@" . $instance->dns_name . " 'ls /dev/sdf; mkdir /mnt/stats; mount /dev/sdf /mnt/stats;ls /mnt/stats'";
            print `$com`;
          }
          else
          {
            print ".\n";
            sleep 5;
          }
      }
   }
}




