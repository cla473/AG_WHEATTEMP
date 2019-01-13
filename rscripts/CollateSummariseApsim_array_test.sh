#!/bin/bash
#SBATCH --time=0:5:0
#SBATCH --mem=2gb
#SBATCH --job-name=stan
#SBATCH --array=1-65
#SBATCH --out=/flush2/cla473/log/R1/apsim_%A_%a.out
#SBATCH --error=/flush2/cla473/log/R1/apsim_%A_%a.err

module load R/3.4.4


#generate a list of directories - this needs to be run from OUTSIDE of the array job initially:
#ls /OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output > dir_list.txt

x=$(sed -n ${SLURM_ARRAY_TASK_ID}p dir_list.txt)
echo $x


#Rscript ./CollateSummariseApsim.R -f /OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output/$x

##Rscript ./CollateSummariseApsim.R -f "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output/Bolac"


#Sample Usage:
# ./CollateSummariseApsim.sh


