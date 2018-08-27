#!/bin/bash
#SBATCH --time=6:0:0
#SBATCH --mem=4gb
#SBATCH --job-name=stan

module load R/3.4.4

if [ ! -z "$SLURM_ARRAY_TASK_ID" ]
then
	i=$SLURM_ARRAY_TASK_ID
    Rscript ./Process_DaysGTE32.R -y $i
fi


#Sample Usage:
#  sbatch -a 1957-2016 Process_DaysGTE32.sh



