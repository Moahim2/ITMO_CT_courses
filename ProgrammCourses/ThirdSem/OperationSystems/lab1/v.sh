#!/bin/bash
grep -E "^[^\ ]+\ INFO" /var/log/anaconda/syslog > "info.log"
