#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

while (<>) {
    print if /^(\b0\b)$|^(\b1((0(1*|(00)*)*01)|1)(0*|(11)*|10(1*(00)*)*01)*\b)$/
}

