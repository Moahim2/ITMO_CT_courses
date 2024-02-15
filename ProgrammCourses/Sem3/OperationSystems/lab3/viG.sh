#!/bin/bash
while true
do
	read LINE
	case $LINE in 
		TERM)
			kill -SIGTERM $(cat .pid)
			exit
			;;
		"*")	kill -USR2 $(cat .pid)
			;;
		"+")
			kill -USR1 $(cat .pid)
			;;
		*);;
	esac
done
