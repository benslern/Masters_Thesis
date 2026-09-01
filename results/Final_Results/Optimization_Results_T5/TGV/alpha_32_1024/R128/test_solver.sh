#!/bin/bash
#SBATCH --account=def-bprotas
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=128
#SBATCH --cpus-per-task=1
#SBATCH --time=0-24:00
#SBATCH --output=./results/test_solver_%j.out
#SBATCH --job-name=visualize_S5_R128

cd $SLURM_SUBMIT_DIR

module load StdEnv/2023
module load fftw-mpi/3.3.10
module load netcdf-fortran-mpi/4.6.1 
module load intel/2023.2.1

source /scinet/vast/etc/vastpreload-openmpi.bash

# EXECUTION COMMAND; ampersand off 3 sub-jobs, 64-tasks-each and wait
(mpirun -N 128 ./test_solver && echo "job 1 finished") &
wait

#(mpirun -N 64 ./test_solver 2 && echo "job 2 finished") &
#(mpirun -N 64 ./test_solver 3 && echo "job 3 finished") &
