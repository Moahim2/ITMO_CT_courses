#!/bin/bash
echo "$@"| tr ' ' '\n' | sort -n | tail -n 1

