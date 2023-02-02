#!/bin/bash
while read x; do
	if [[ "$x" -eq "2" ]]; then
		break
	fi
	let x="$x * 2"
	echo "$x"
done < $1 >> $1
