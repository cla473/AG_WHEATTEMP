#!/bin/bash
#SBATCH --time=6:0:0
#SBATCH --mem=4gb
#SBATCH --job-name=stan

module load R/3.4.4

MYDIR="/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output/*"

for DIR in find -type d -path ${MYDIR}; do
	echo ${DIR}
	Rscript ./CollateSummariseApsim.R -f ${DIR}
done
#Rscript ./CollateSummariseApsim.R -f "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output/Bolac"


#Sample Usage:
# ./CollateSummariseApsim.sh


