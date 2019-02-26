#!/bin/bash
#SBATCH --time=6:0:0
#SBATCH --mem=4gb
#SBATCH --job-name=stan

module load R/3.4.4

if [ ! -z "$SLURM_ARRAY_TASK_ID" ]
then
	i=$SLURM_ARRAY_TASK_ID
    Rscript ./12_Collate_Regional.R -l $i
fi


#Sample Usage:
#  sbatch -a 1-2 Collate_Regional.sh
#  sbatch -a 3-16 Collate_Regional.sh
#  sbatch -a 17-26 Collate_Regional.sh
#  sbatch -a 27-36 Collate_Regional.sh
#  sbatch -a 37-46 Collate_Regional.sh
#  sbatch -a 47-53 Collate_Regional.sh



