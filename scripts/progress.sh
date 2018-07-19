#!/bin/bash

while true; do
    sleep $(( 3600 - ($(date +%s) % 3600) ))
    printf "%(%H:%M)T %s\n" -1 $(grep ^finished *.out | wc -l) >> progress.log
done
