#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

while (<>) {
    print if /\i^.*\bcat\b.*$/
}

