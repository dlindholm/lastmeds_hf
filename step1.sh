#!/bin/bash                                                                                
#SBATCH -Asens2019006                                                                      
#SBATCH --time=00:30:00                                                                    
#SBATCH --nodes=1                                                                          
#SBATCH --ntasks-per-node=16                                                              
#SBATCH --job-name=lastmedsDM

# Script to be run from the project home directory:
#   sbatch step1.sh

# Mounts project directory and data directory to specific points within the
# container. Update paths below if files are moved.
apptainer exec -C --mount src=.,dst=/lastmeds \
                  --mount src=/proj/sens2019006/hf_daniel/,dst=/data \
                  ./container/dlindholm_lastmeds_latest.sif \
                  Rscript /lastmeds/src/dm.R