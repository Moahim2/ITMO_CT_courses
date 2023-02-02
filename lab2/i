#!/bin/bash
ps -au root | sed 1d | wc -l > file1.log
ps a -u root | sed 1d | awk '{printf $1":"; for (i=5; i<=NF; i++) {printf $i}; printf "\n"}' >>\
 file1.log
