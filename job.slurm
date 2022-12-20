#!/bin/bash
#SBATCH --job-name=fish_analysis
#SBATCH --ntasks=31
#SBATCH --partition=cpuq
#SBATCH --mem=200GB
#SBATCH --output=fish_analysis_%j.log

cd scripts
module load singularity
singularity exec -B .:/home/Project ../inshore_fish.sif Rscript 32_fitModel_gbm_temporal.R 
