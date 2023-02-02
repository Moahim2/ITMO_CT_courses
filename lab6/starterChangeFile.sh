#!/bin/bash
if [[ "$#" -ne "1" ]]; then 
	echo "starterscript error count of args"
	exit 1
fi
let N="$1"

for ((i=1; i <= $N; i++)); do
	./changeFile.sh "fm$i"
done
