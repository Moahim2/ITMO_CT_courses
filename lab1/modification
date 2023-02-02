#!/bin/bash
cat students | awk '{print $3}' | grep -o -E "[0-9]+$" | grep -o -E "^[[:alnum:]][[:alnum:]]" |
sed -E "s/31/1/g" | sed -E "s/32/2/g" | sed -E "s/33/3/g" | sed -E "s/34/4/g" | sed -E "s/41/5/g" |
sed -E "s/42/6/g" | sort | uniq -d -c | sort -n | tail -n 1 | awk '{print $2}' 
