#!/usr/bin/perl
use strict;
use warnings FATAL => 'all';

while (<>) {
    print if /^.*(x|y|z).{5,17}(x|y|z).*$/
}

