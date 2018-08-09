#!/bin/bash
#SBATCH --time=2:0:0
#SBATCH --mem=4gb
#SBATCH --job-name=zadoc

module load parallel python/3.6.1

if [! -z "$SLURM_ARRAY_TASK_ID" ]
then
	i = $SLURM_ARRAY_TASK_ID
    ./Apsim_ProcessPhases.py -f "filelist.txt"  -p "07_GrainFilling" -y $i
done


#Sample Usage:
#  sbatch -a 1957-2016 Process_Phases.sh



