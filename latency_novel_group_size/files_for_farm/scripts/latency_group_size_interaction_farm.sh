#!/bin/bash

# setting the home directory
#SBATCH -D /home/mjculsha/Fish_Group_Models_FARM/latency_group_size

# set the number of nodes to 1
#SBATCH --nodes=1

# total number of tasks
#SBATCH --ntasks=3

# set number of processes per node
#SBATCH --ntasks-per-node=3

# set max memory to 8gb per process
#sbatch --mem-per-cpu=8000

# set max wall time to 48 hours
#SBATCH --time=48:00:00

# set the name of the job
#SBATCH --job-name=fish_model

# mail alerts at beginning and end
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END

# send mail here
#SBATCH --mail-user=mjculshawmaurer@ucdavis.edu

# where to send standard output
# --output=outputs/slurm-%j.out

# start job from the directory it was submitted
cd $SLURM_SUBMIT_DIR

# load NetLogo
module load R

Rscript scripts/latency_group_size_interaction_farm.R
