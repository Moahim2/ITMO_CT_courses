#!/bin/bash
echo "" > results1.log
for ((i=1; i <= 20; i++)); do
	tmp="0.0"
	for ((j=1; j <= 10; j++)); do
		c=$(/bin/time -f "%e" "./starterE.sh" "$i" 2>&1 1>/dev/null)
		tmp=$(awk "BEGIN {print $tmp+$c}")
	done
	tmp=$(awk "BEGIN {print $tmp/10.0}")
	echo $tmp >> results1.log
done
