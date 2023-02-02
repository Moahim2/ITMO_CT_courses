#!/bin/bash
echo $$ > .pid
cur="1"
MODE="+"

term()
{
	MODE="TERM"
}

usr1()
{
	MODE="+"
}

usr2()
{
	MODE="*"
}

trap 'term' SIGTERM
trap 'usr1' USR1
trap 'usr2' USR2

while true; do
	case $MODE in
		TERM)
			echo "Stopped by SIGTERM from viG"
			exit
			;;
		"+")
			let cur="$cur+2"
			echo $cur
			;;
		"*")	let cur="$cur*2"
			echo $cur
			;;
		*)
			;;
	esac
	sleep 1
done
