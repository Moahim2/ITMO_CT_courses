#!/bin/bash
res="$HOME/restore"
if [[ ! -d "$res" ]]; then
	mkdir "$res"
fi

cur_date_sec="0"
last_dir=""
for i in $(ls $HOME); do
	if [[ ! -d "$HOME/$i" ]]; then
		continue
	fi
	date=$(echo $i | grep -E "^Backup-" | grep -Eo "[0-9]{4}-[0-9]{2}-[0-9]{2}$")
	if [[ "$date" = "" ]]; then
		continue
	fi
	date_sec=$(date --date=$date +%s)
	if [[ "$date_sec" -gt "$cur_date_sec" ]]; then
		last_dir="$HOME/Backup-$date"
		cur_date_sec="$date_sec"
	fi
done

if [[ "$last_dir" = "" ]]; then
	echo "reserve catalog not found"
	exit 1
fi

for file in $(ls $last_dir | grep -E -v "[^\/]+[0-9]{4}-[0-9]{2}-[0-9]{2}$"); do
	cp -r "$last_dir/$file" "$res"
done


