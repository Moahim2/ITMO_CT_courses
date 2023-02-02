#!/bin/bash
report="$HOME/report.log"
echo "" > $report

array=()
i="0"
while true; do
	let x="$i%100000"
	if [[ "$x" -eq "0" ]]; then
		echo "${#array[@]}" >> $report
	fi	
	array+=(1 2 3 4 5 6 7 8 9 10)

	let i="$i+1"
done


