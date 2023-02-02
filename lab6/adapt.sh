#!/bin/bash

for ((i=1; i <= 20; i++)); do
	echo -n > "fm$i"	
	for ((j=1; j <= 200000; j++)); do
		echo "1"	
	done >> "fm$i"

done
