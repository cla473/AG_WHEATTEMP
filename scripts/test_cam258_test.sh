#!/bin/bash
#SBATCH --time=2:0:0
#SBATCH --mem=10gb
#SBATCH --cpus-per-task=1
#SBATCH --export=i
##SBATCH --array=0-1148%50
#SBATCH --array=0-1

module load parallel python/3.6.1

files_per_job=1

echo the valie of i is ${i}

for f in $(sed -n $((SLURM_ARRAY_TASK_ID*files_per_job+1)),~${files_per_job}p file_list/filelist.list); do
    sem -j $SLURM_CPUS_PER_TASK --id=$SLURM_JOBID ./Apsim_SqLite.py -f /OSM/CBR/AG_WHEATTEMP/source/${f}
done

sem --wait --id=$SLURM_JOBID



