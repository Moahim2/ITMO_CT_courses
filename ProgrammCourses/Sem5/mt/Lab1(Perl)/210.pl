#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';


while (<>) {
    s/(a.*?a){3}/bad/g;
    print
}


