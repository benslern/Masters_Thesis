#!/bin/bash
#SBATCH --account=def-bprotas
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32
#SBATCH --mem=0M
#SBATCH --time=0-1:00
#SBATCH --output=test_solver.out



srun ./test_solver
