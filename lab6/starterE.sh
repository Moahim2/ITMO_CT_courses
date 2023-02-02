#!/bin/bash
if [[ "$#" -ne "1" ]]; then 
	echo "starterscript error count of args"
	exit 1
fi
let N="$1+2"

for ((i=2; i < $N; i++)); do
	./getValueE1 "$i"
done
