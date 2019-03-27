#!/bin/bash
#SBATCH --time=7:0:0
#SBATCH --mem=4gb
#SBATCH --job-name=stan

module load R/3.4.4

if [ ! -z "$SLURM_ARRAY_TASK_ID" ]
then
	i=$SLURM_ARRAY_TASK_ID
    Rscript ./15_Collate_Regional_FromFile.R -l $i
fi


#Sample Usage:
#  sbatch -a 1-10 Collate_Regional_15.sh
#  sbatch -a 1-135%40 Collate_Regional_15.sh



