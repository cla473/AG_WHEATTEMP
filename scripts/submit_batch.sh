#for now this will just echo the command
for i in `seq 113 152`; do x=$(eval "cat file_list/${i}.list | wc -l");  echo sbatch --array=1-${x}%50 test_cam258.sh ; done


