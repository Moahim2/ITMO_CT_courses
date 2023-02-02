#!/bin/bash
ps -eF | awk '{print $11" "$2}' | grep -E "\/sbin\/" | awk '{print $2}'
