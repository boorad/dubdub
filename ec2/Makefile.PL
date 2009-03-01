use inc::Module::Install;

name        ('DubDub Launcher');
abstract    ('DubDub Launch Scripts');
author      ('Russell Jurney <russell.jurney@gmail.com>');
include     ('ExtUtils::AutoInstall');
version     ('0.01');

requires    ('Net::Amazon::EC2' => 0, 'Getopt::Std' => 0, 'Data::Dump' => 0);            # mandatory modules

auto_install();
&WriteAll;