#!/bin/bash
#SBATCH --partition=kamiak		# Partition (like a queue in PBS)
#SBATCH --job-name=GWAS			# Job Name
#SBATCH --output=GWAS.out
#SBATCH --error=GWAS.err
#SBATCH --time=3-00:00:00		# Wall clock time limit in Days-HH:MM:SS
#SBATCH --nodes=5			# Node count required for the job
#SBATCH --ntasks-per-node=1		# Number of tasks to be launched per Node
#SBATCH --cpus-per-task=20		# Number of threads per task (OMP threads)
#SBATCH --mem=220GB			# Amount of memory per node

module load r
Rscript --vanilla GWAS_kamiak.R
