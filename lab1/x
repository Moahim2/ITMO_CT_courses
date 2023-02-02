#!/bin/bash
man bash | grep -o "[a-zA-Z]\{4\}[a-zA-Z]*" | sort | uniq -c -d -i | sort -n | tail -n 3 |
awk '{print $2}'

