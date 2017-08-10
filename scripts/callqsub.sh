#!/usr/bin/env bash

for file in qs_DM*
do
  qsub "$file" 
done
