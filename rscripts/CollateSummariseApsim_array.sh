#!/bin/bash
#SBATCH --time=24:0:0
#SBATCH --nodes=1
#SBATCH --ntasks=19
#SBATCH --mem=1gb
#SBATCH --job-name=stan
##SBATCH --array=1-2
#SBATCH --array=1-65
#SBATCH --out=/flush2/cla473/log/R1/apsim_%A_%a.out
#SBATCH --error=/flush2/cla473/log/R1/apsim_%A_%a.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=loretta.clancy@csiro.au

module load R/3.4.4

#generate a list of directories - this needs to be run from OUTSIDE of the array job initially:
#ls /OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output > dir_list.txt

x=$(sed -n ${SLURM_ARRAY_TASK_ID}p dir_list.txt)
echo $x

Rscript ./CollateSummariseApsim.R -f /OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output/$x

##Rscript ./CollateSummariseApsim.R -f "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output/Bolac"


#Sample Usage:
# ./CollateSummariseApsim.sh


