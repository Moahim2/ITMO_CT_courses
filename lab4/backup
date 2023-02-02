#!/bin/bash
dir_s="$HOME/source"
file_br="$HOME/backup-report"
if  [[ ! -d $dir_s ]]; then
	echo "not found $dir_s, create it please"
	exit 1
fi
find "$file_br" -maxdepth 0 2>/dev/null 1>/dev/null ||
	{
		echo "not found $file_br, create it please"
		exit 1
	}

cur_date=$(date +%Y-%m-%d)
cur_date_sec=$(date --date=$cur_date +%s)
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
	let d=($cur_date_sec-$date_sec)/60/60/24
	if [[ "$d" -ge "0" && "$d" -lt "7" ]]; then
		last_dir="$HOME/Backup-$date"
		break
	fi
done

if [[ "$last_dir" = "" ]]; then
	new_dir="$HOME/Backup-$cur_date"
	mkdir "$new_dir"
	for file in $(ls "$dir_s"); do
		cp -r "$dir_s/$file" "$new_dir"
	done
	echo -e "New_reserve_dir: $new_dir, create in $cur_date:
$(ls $dir_s)" >> $file_br
else
	str_1=""
	str_2=""
	####
	
	for file in $(ls "$dir_s"); do
		if [[ ! -f "$last_dir/$file" ]]; then
			cp -r "$dir_s/$file" "$last_dir"
			str_1="$str_1 new file $file\n"
		else
			size_orig=$(ls -l "$dir_s/$file" | awk '{print $5}')
			size_reserv=$(ls -l "$last_dir/$file" | awk '{print $5}')
			if [[ "$size_orig" -eq "$size_reserv" ]]; then
				continue
			fi
			if [[ -f "$last_dir/$file.$cur_date" ]]; then
				rm -rf "$last_dir/$file.$cur_date"
			fi
			mv "$last_dir/$file" "$last_dir/$file.$cur_date"
			cp -r "$dir_s/$file" "$last_dir"
			str_2="$str_2 update file: $file, last version rename to $file.$cur_date\n"
		fi
	done
	echo -e "Reserve_dir $last_dir was updated in $cur_date" >> $file_br
	echo -e "$str_1" >> $file_br
	echo -e "$str_2" >> $file_br
fi

