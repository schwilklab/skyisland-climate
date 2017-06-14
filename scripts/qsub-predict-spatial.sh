#!/bin/bash

## Run the topo model fitting

#$ -V
#$ -N predict-spatial
#$ -o ../results/$JOB_NAME.o$JOB_ID
#$ -e ../results/$JOB_NAME.e$JOB_ID
#$ -cwd
#$ -S /bin/bash
#$ -P hrothgar
#$ -pe fill 1

R --vanilla < ~/projects/skyisland-climate/scripts/predict-spatial.R
