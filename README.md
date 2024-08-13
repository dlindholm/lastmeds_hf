# Heart failure treatment in the last years of life

This repository contains code for the above project. 

For reproducibility, the analysis environment was containerized using
_Apptainer_ - thereby locking operating system (Ubuntu), R version (4.3.1) and
R package versions.

Running the code is of course only possible with access to the data, which is 
__not__ publically available due to its sensitive nature. 

To run the code:
1. Build the _Apptainer_ container and upload to the HPC environment (see README in the container directory).
2. Log into HPC environment and go to project directory.
3. Run `sbatch step1.sh` and wait for it to finish.
4. Run `sbatch step2.sh` and wait for it to finish.
5. Done!