#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';


while (<>) {
    print if /\b(.+)\g1\b/
}

