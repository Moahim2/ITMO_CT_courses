#!/bin/bash
path="/var/log/anaconda/X.log"
sed -n -E "s/\(WW\)/Warning:/p" "$path" > "full.log"
sed -n -E "s/\(II\)/Information:/p" "$path" >> "full.log"
cat "full.log"
