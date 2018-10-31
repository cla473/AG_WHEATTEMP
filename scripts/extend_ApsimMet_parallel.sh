#!/bin/bash
#SBATCH --time=1:00:0
#SBATCH --mem=4gb
#SBATCH --job-name=stan
#SBATCH --nodes=1
#SBATCH --ntasks=20
#SBATCH --export=STARTNO,ENDNO


module load python/3.6.1 parallel-rust
#echo $STARTNO
#echo $ENDNO


FILELIST="/OSM/CBR/AG_WHEATTEMP/work/ApsimNG-LC/met/metList.csv"
OUTPATH="/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/modifiedMet"

#echo ${FILELIST}
#echo ${OUTPATH}

seq ${STARTNO}  ${ENDNO} | parallel -j 20 --joblog parallel-logfile.txt ./extend_ApsimMet.py -f ${FILELIST} -o ${OUTPATH} -r {}


#Sample Usage:
#  python extend_ApsimMet.py -r 2 -f "/OSM/CBR/AG_WHEATTEMP/work/ApsimNG-LC/met/metList.csv" -o "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/modifiedMet"
#  STARTNO=1 ENDO=2 sbatch extend_ApsimMet.sh
#  STARTNO=1 ENDO=4999 sbatch extend_ApsimMet.sh


