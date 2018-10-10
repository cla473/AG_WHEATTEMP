#!/bin/bash
#SBATCH --time=0:5:0
#SBATCH --mem=1gb
#SBATCH --job-name=stan
#SBATCH --nodes=1
#SBATCH --ntasks=5
#SBATCH --export=IDNO,FILENAME

echo ${IDNO}
echo ${FILENAME}

#submit with:
#IDNO=5 FILENAME=LocationList.csv sbatch getMetData_netCDF4_parallel_test2.sh

