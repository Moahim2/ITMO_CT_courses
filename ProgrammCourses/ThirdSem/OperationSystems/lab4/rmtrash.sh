#!/bin/bash

if [[ $# -ne 1 ]]; then
	echo "incorrect number of arguments"
	exit
fi

find "$PWD/$1" -maxdepth 0 2>/dev/null 1>/dev/null ||
       	{ 
		echo "file $1 not found" 
		exit 1
       	}
dir_trash="$HOME/.trash"

if [[ ! -d $dir_trash ]]; then 
	mkdir $dir_trash
fi

number=$(ls "$dir_trash" | grep -Eo "^file_[0-9]+$" | grep -Eo "[0-9]+" | sort -n | tail -n 1)
if [[ number = "" ]]; then
	number="0"
else
	let number="$number+1"
fi

ln "$PWD/$1" "$dir_trash/file_$number"
rm -rf "$PWD/$1"
echo "$PWD/$1 to file_$number" >> "$HOME/.trash.log"

