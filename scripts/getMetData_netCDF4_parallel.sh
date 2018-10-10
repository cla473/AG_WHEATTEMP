#!/bin/bash
#SBATCH --time=2:0:0
#SBATCH --mem=1gb
#SBATCH --job-name=stan
#SBATCH --nodes=1
#SBATCH --ntasks=20
#SBATCH --export=IDNO,FILENAME

module load python/3.6.1 parallel-rust

seq 1 ${IDNO} | parallel -j 20 --joblog parallel-logfile.txt  ./getMetData_netCDF4.py -f ${FILENAME} -i {}


#Sample Usage:
#  python getMetData_netCDF4.py -i 6 -f "LocationList_Draft_GRDC_Subregions.csv"
#  IDNO=51034 FILENAME="LocationList_Draft_GRDC_Subregions.csv" sbatch getMetData_netCDF4_parallel.sh
