#!/bin/bash -l
#SBATCH --time=6:0:0
#SBATCH --mem=4gb
#SBATCH --job-name=stan

module load python/3.6.1

if [ ! -z "$SLURM_ARRAY_TASK_ID" ]
then
	i=$SLURM_ARRAY_TASK_ID
	echo SLURM_ARRAY_TASK_ID is $i or $SLURM_ARRAY_TASK_ID
	CMD="./Process_Summary.py -f filelist.txt  -p 07_GrainFilling -y ${i}"
	echo $CMD
	eval ${CMD}
fi


#Sample Usage:
#  sbatch -a 1957-2016 Process_Summary_hli001.sh



