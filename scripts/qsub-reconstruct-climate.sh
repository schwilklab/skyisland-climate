#!/bin/bash

## Run the full landscape historical temperature reconstruction


#$ -V
#$ -N reconstruct_temps
#$ -o ../results/$JOB_NAME.o$JOB_ID
#$ -e ../results/$JOB_NAME.e$JOB_ID
#$ -cwd
#$ -S /bin/bash
#$ -P hrothgar
#$ -pe fill 6
#$ -q ivy-highmem

R --vanilla < ~/projects/skyisland-climate/scripts/reconstruct-climate.R
