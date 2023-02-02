#!/bin/bash
if [[ "$#" != "1" ]]; then
	echo "error count arguments for newmem.bash"
fi
N="$1"
array=()
i="0"
while [[ "$i" -lt "$N" ]]; do	
	array+=(1 2 3 4 5 6 7 8 9 10)
	let i="$i+10"
done

