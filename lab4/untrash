#!/bin/bash

if [[ $# -ne 1 ]]; then
	echo "incorrect number of arguments"
	exit
fi

ftrash="$HOME/.trash.log"
dir_trash="$HOME/.trash"
find "$ftrash" -maxdepth 0 2>/dev/null 1>/dev/null ||
       	{ 
		echo "file .trash.log not found" 
		exit 1
       	}
if [[ ! -d $dir_trash ]]; then 
	echo "directory .trash not found"
	exit 1
fi


while read -u3 inf; do
	link=$(echo $inf | awk '{print $NF}')
	path_to_last_f=$(echo $inf | awk '{print $1}')
	
	####bad_link
	find "$dir_trash/$link" -maxdepth 0 2>/dev/null 1>/dev/null || continue
	
	echo "$path_to_last_f"
	echo "Enter y/n"
	ans=""
	while true; do
		read ans
		if [[ "$ans" = "y" || "$ans" = "n" ]]; then
			break
		else
			echo "Enter only y or n"
		fi
	done
	if [[ "$ans" = "n" ]]; then
		continue
	fi
	
	###OK
	dir_last_f=$(echo $path_to_last_f | awk -F/ '{for(i=2;i<NF;i++) {printf("/%s",$i);}}')
	catalog=""
	if [[ -d $dir_last_f ]]; then
		catalog="$dir_last_f"
	else
		catalog="$HOME"
		echo "old catalog is died, restore in HOME_DIRECTORY"
	fi
	new_name="$1"
	while true; do
		find "$catalog/$new_name" -maxdepth 0 2>/dev/null 1>/dev/null || break 
		echo "name conflict detected, please choose new file_name"
		read new_name	
	done

	ln "$dir_trash/$link" "$catalog/$new_name"
	rm -rf "$dir_trash/$link"
	break
done 3< <(cat $ftrash | grep -Eo "^.+\ to\ file_[0-9]+$" | grep -E "^(\/.+)*\/$1\ ")

