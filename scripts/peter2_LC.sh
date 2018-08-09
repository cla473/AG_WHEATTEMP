#!/bin/bash
#SBATCH --time=12:0:0
#SBATCH --mem=30gb
#SBATCH --array=0-1148%30
#SBATCH --job-name=zadoc

module load parallel python/3.6.1

files_per_job=50

(( SLURM_ARRAY_TASK_ID < 30 )) && sleep $((SLURM_ARRAY_TASK_ID*20))

for f in $(sed -n $((SLURM_ARRAY_TASK_ID*files_per_job+1)),~${files_per_job}p file_list/filelist.list); do
    # sem -j $SLURM_CPUS_PER_TASK --id=$SLURM_JOBID
    cp /OSM/CBR/AG_WHEATTEMP/source/$f $MEMDIR
    ./Apsim_ProcessPhases.py -f $MEMDIR/$f
    rm $MEMDIR/$f
done

#sem --wait --id=$SLURM_JOBID

