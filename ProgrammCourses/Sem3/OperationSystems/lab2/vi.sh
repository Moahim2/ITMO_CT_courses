#!/bin/bash
max_mem="0"
ans=""
for PID in $(ps -ef | sed 1d | awk '{print $2}')
do
    MEM=$(cat 2>/dev/null /proc/$PID/status | grep -Eo "VmSize:.*" | grep -Eo "[0-9]*")
    if [[ -z MEM ]]; then
        continue
    fi
    if [[ max_mem -lt MEM  ]]; then
        max_mem="$MEM"
        ans="$PID"
    fi
done
echo "proc: PID=$ans MEM=$max_mem"

echo $(top -b -n1 -o VIRT | sed -n 8p | awk '{print  "top: PID="$1 " " "MEM="$5}')
