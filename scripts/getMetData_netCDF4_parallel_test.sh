#!/bin/bash
#SBATCH --time=0:5:0
#SBATCH --mem=1gb
#SBATCH --job-name=stan
#SBATCH --nodes=1
#SBATCH --ntasks=5
#SBATCH --export=IDNO,FILENAME


module load python/3.6.1 parallel-rust

echo ${IDNO}

seq 1 "${IDNO}" | parallel -j 5 --joblog parallel-logfile.txt  ./getMetData_netCDF4.py -f ${FILENAME} -i {}
#seq 1 5 | parallel -j 5 --joblog parallel-logfile.txt  ./getMetData_netCDF4.py -f ${FILENAME} -i {}


#Sample Usage:
#  python getMetData_netCDF4.py -i 6 -f "LocationList.csv"
#  IDNO=5 FILENAME=LocationList.csv sbatch getMetData_netCDF4_parallel_test.sh
