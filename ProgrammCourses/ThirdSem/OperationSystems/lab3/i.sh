#!/bin/bash
time_start=$(date +%y.%m.%d_%H:%M:%S)
file=~/report


mkdir ~/test && echo "catalog test was created successfuly" > $file && touch ~/test/"$time_start"
ping "www.net_nikogo.ru" || echo "$(date +%y.%m.%d_%H:%M:%S) hostError" >> $file
