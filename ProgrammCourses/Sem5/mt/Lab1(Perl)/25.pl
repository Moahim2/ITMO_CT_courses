#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';


while (<>) {
    s/\b(\w)(\w)/$2$1/g;
    print
}
