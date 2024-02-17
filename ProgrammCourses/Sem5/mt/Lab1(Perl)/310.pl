#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

my $res = "";
while (<>) {
    s/^( )*//g;
    s/ *$//g;
    s/ {2,}/ /g;
    $res = $res . $_
}

$res =~ s/^(\s)*//;
$res =~ s/(\s)*$//;
$res =~ s/((\s*)\n){2,}/\n\n/gm;
#
# s/(\s)*//;
print $res;
