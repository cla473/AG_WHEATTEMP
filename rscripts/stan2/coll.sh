#!/bin/bash
#SBATCH --time=2:0:0
#SBATCH --mem=2gb
#SBATCH --job-name=stan2

module load R/3.4.4

#generate a list of directories - this needs to be run from OUTSIDE of the array job initially:
#ls /OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output > dir_list.txt

hex=0123456789abcdef
(( set = SLURM_ARRAY_TASK_ID % 128 ))
(( cultivar = SLURM_ARRAY_TASK_ID / 128 + 1 ))
cultivar=$(sed -n ${cultivar}p dir_list.txt)
pattern="${hex:$((set/8)):1}[${hex:$(((set%8)*2)):2}]"

#Rscript ./CollateSummariseApsim.R -f /OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output/$x
Rscript ./coll_LC.R -f /OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output/$cultivar -p "$pattern"

##Rscript ./CollateSummariseApsim.R -f "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output/Bolac"


#Sample Usage:
# ./CollateSummariseApsim.sh


