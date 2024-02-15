#1/bin/bash
read var
ans=""
while [[ "$var" != "q" ]]
do
    ans="$ans$var"
    read var
done
echo "$ans"
