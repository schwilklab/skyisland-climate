#!/usr/bin/env bash

# create field sheetsmerge iButton data (only does so if there are new folders)
./build-merged-ibutton.py -v
# create rds files (slow!)  only does so if there is new data
Rscript ./load-sensor-data.R
