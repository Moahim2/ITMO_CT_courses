#!/bin/bash
if [[ "$#" != "2" ]]; then
	echo "error count arguments"
fi
N="$1"
K="$2"
i="0"
while [[ "$i" -lt "$K" ]]; do
	./newmem.bash "$N" &
	let i="$i+1"
	sleep 1
done
