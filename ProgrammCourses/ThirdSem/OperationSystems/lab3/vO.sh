#!/bin/bash
cur="1"
type="+"
NUM="^[0-9]*\.*[0-9]*$"
(tail -f pipe) |
while true; do
	read LINE;
	case $LINE in
		QUIT)
			echo "QUIT"	
			echo "$cur"
			killall tail
			exit
			;;
		"*")
			type="*"
			;;
		"+")	type="+"
			;;
		"-")
			type="-"
			;;
		"/")
			type="/"
			;;
		*)
			if ! grep -q "$NUM" <<< $LINE; then
				echo "ERROR"
				killall tail
				exit
			fi
			cur=$(awk "BEGIN {print $cur$type$LINE}")
			;;
	esac
done
