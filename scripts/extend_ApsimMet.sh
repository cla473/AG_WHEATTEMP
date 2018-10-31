#!/bin/bash
#SBATCH --time=0:10:0
#SBATCH --mem=4gb
#SBATCH --job-name=stan


module load python/3.6.1

if [ ! -z "$SLURM_ARRAY_TASK_ID" ]
then
	i=$SLURM_ARRAY_TASK_ID
    ./extend_ApsimMet.py -f "/OSM/CBR/AG_WHEATTEMP/work/ApsimNG-LC/met/metList.csv" \
					     -o "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/modifiedMet" \
                         -r $i
fi


#Sample Usage:
#  sbatch -a 1-21 extend_ApsimMet.sh
#  sbatch -a 1-51032 extend_ApsimMet.sh



