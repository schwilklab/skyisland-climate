#!/usr/bin/env bash

for file in qs_CM*
do
  qsub "$file" 
done
