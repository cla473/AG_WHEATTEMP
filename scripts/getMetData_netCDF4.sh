#!/bin/bash
#SBATCH --time=0:30:0
#SBATCH --mem=1gb
#SBATCH --job-name=stan

module load python/3.6.1

if [ ! -z "$SLURM_ARRAY_TASK_ID" ]
then
	i=$SLURM_ARRAY_TASK_ID
    ./getMetData_netCDF4.py -i $i -f ${FILENAME}
fi


#Sample Usage:
#  python getMetData_netCDF4.py -i 6 -f "LocationList.csv"
#  sbatch -a 1-3 --export FILENAME="LocationList.csv" getMetData_netCDF4.sh
