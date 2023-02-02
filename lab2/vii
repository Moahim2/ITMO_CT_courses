#!/bin/bash
count="0"
for PPI in $(ps -ef | sed 1d | awk '{print $2}')
do
let count="$count+1"
process[$count]="$PPI"
done

for ((i=1; i<=count; i++))
do
process_io[$i]=$(cat 2>/dev/null /proc/${process[$i]}/io | head -n 1 | awk '{print $2}')
done

sleep 60

for ((i=1; i<=count; i++))
do
let process_io_res[$i]="$(cat 2>/dev/null /proc/${process[$i]}/io | head -n 1 | awk '{print $2}')-${process_io[$i]}"
done


for ((i=1; i<=count; i++))
do
read -r cmd < /proc/${process[$i]}/cmdline 
if [[ -z $cmd ]]; then
    cmd="NULL"
fi
data+="${process[$i]}:$cmd:${process_io_res[$i]}\n"

done
echo -e $data | sort -t: -k 3 -nr | head -n 3 > ans_vii
