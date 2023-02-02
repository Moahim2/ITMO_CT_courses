#!/bin/bash
: > iv.log
for PID in $(ps -ef | sed 1d | sort -nk 3 | awk '{print $2}')
do 
PPid=$(cat 2>/dev/null /proc/$PID/status | grep -E "PPid" | grep -Eo "[0-9]+")
SER=$(cat 2>/dev/null /proc/$PID/sched | grep -E "sum_exec_runtime" | grep -Eo "[0-9]+(\.[0-9]+)?")
NS=$(cat 2>/dev/null /proc/$PID/sched | grep -E "nr_switches" | grep -Eo "[0-9]+(\.[0-9]+)?")

if [[ -z "$NS" ]]; then
continue
else
ART=$(awk "BEGIN {print $SER/$NS}")
fi

echo "ProcessID=$PID : Parent_ProcessID=$PPid : Average_Running_Time=$ART" >> iv.log
done
