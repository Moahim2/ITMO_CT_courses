#!/bin/bash
calculate(){
    local M=$(awk "BEGIN {print $sum/$count}")
    output+="Average_Running_Children_of_ParentID=$curPPid is $M\n"
}

curPPid="0"
count="0"
sum="0"
output=""

while read string
do

PPid=$(echo "$string" | awk '{print $3}' | grep -Eo "[0-9]+(\.[0-9]+)?")
ART=$(echo "$string" | awk '{print $5}' | grep -Eo "[0-9]+(\.[0-9]+)?")

if [[ "$curPPid" -ne "$PPid" ]]; then
    calculate
    curPPid="$PPid"
    count="0"
    sum="0.0"
fi

let count="$count+1"
sum=$(awk "BEGIN {print $sum+$ART}")
output+="$string\n"

done <iv.log

calculate
echo -e $output > iv.log
