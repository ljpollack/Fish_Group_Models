#!/bin/bash

# setting the home directory
#SBATCH -D /home/mjculsha

# set the number of nodes to 1
#SBATCH --nodes=1

# set number of processes per node
#SBATCH --ntasks-per-node=16

# set max wall time to 2 hours
#SBATCH --time=02:00:00

# set the name of the job
#SBATCH --job-name=test_brms

# mail alerts at beginning and end
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END

# send mail here
#SBATCH --mail-user=mjculshawmaurer@ucdavis.edu

# where to send standard output
#SBATCH --error /home/mjculsha/Fish_Group_Models_FARM/latency_group_size/outputs

# start job from the directory it was submitted
cd $SLURM_SUBMIT_DIR

# load NetLogo
module load R

Rscript testing_brms_FARM.R