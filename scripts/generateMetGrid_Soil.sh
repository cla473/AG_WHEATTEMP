#!/bin/bash
#SBATCH --time=10:00:0
#SBATCH --mem=1gb
#SBATCH --job-name=stan
#sbatch --export STARTNO, FINALNO

module load python/3.6.1

#echo ${STARTNO}
#echo ${FINALNO}

./generateMetGrid_Soil.py -s ${STARTNO} -f ${FINALNO}

#Sample Usage:
#  python generateMetGrid_Soil.py -s 0 -f 10
#  STARTNO=0 FINALNO=5000 sbatch generateMetGrid_Soil.sh

