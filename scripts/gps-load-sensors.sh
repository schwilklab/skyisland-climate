#!/usr/bin/env bash

# create field sheets
Rscript ./make-sensors-to-upload.R
# load result into GPS connected via USB
gpsbabel -i unicsv -f ../results/field-sheets/sensors-to-upload.csv -o garmin -F usb:
