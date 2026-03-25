# Makefile to easily update the compilation of a program (prog)
# --------
#
# by Alain Veilleux, 4 August 1993
#    Modified to run Di Kang's Code, by Pritpal Matharu: 2021/04/08
#
# GOAL AND FUNCTIONING OF THIS SCRIPT:
#    Script in the form of a "Makefile" allowing to update a program containing
#    multiple separated routines on the disk. This script is not executed by itself,
#    but is instead read and interpreted by the "make" command. When it is called,
#    the "make" command verifies the dates of the various components your program is
#    built from. Only routines that were modified after the last compilation of the
#    program are recompiled in object form (files ending in .o). Recompiled .o files
#    are subsequently linked together to form an updated version of the final program.
#
# TO ADAPT THIS SCRIPT TO YOUR PROGRAM:
#    Modify the contents of the variables hereunder. Comments will guide you how and
#    where.
#
# USING "make" ON THE UNIX COMMAND LINE:
#    1- Type "make" to update the whole program.
#    2- Type "make RoutineName" to only update the RoutineName routine.
#
#================================================================================================
#
# Before running make command, must load the modules!!!
# Load Modules : StdEnv/2020 fftw-mpi/3.3.8 netcdf-fortran-mpi/4.5.2
# Modules were loaded using:    module load StdEnv/2020 fftw-mpi/3.3.8 netcdf-fortran-mpi/4.5.2

# To send the job (prog) to the scheduler, appropriately modify the shell file "run____.sh"
# then run: sbatch run____.sh
#
#================================================================================================
#
# The instructions outlined in this makefile are equivalent to manually running the following commands:
# module load StdEnv/2020 fftw-mpi/3.3.8 netcdf-fortran-mpi/4.5.2
# export FFTWDIR=/cvmfs/soft.computecanada.ca/easybuild/software/2020/avx2/MPI/intel2020/openmpi4/fftw-mpi/3.3.8/
# export nCDF_DIR=/cvmfs/soft.computecanada.ca/easybuild/software/2020/avx2/MPI/intel2020/openmpi4/netcdf-fortran-mpi/4.5.2
# mpif90 -lmpi -o prog global_variables.f90 initialize.f90 data_ops.f90 fftwfunction.f90 databinary_handle.f90 function_ops.f90 adj_solvers.f90 solvers.f90 optimization.f90 LPS_main.f90 -lm -I${FFTWDIR}/include -L${FFTWDIR}/lib -lfftw3_mpi -lfftw3 -I${nCDF_DIR}/include -L${nCDF_DIR}/lib -lnetcdff -lnetcdf
#
#================================================================================================
#================================================================================================


#====================  Definition of variables  =====================
# Remark : variables are sometimes called "macros" in Makefiles.

# Compiler to use (MPI FORTRAN)
CompilerName= mpif90

# Libraries
BASIC_LIB  = -lm -lmpi                # Linking flags

# With fftw-mpi and netcdf-fortran-mpi modules loaded no need to specify paths explicitly
FFTW_DIR   =                          # Include FFTW3 Library
FFTW_LIB   = -lfftw3_mpi -lfftw3 -lm  # Load FFTW3 Library
nCDF_DIR   =                          # Include nCDF Library
nCDF_LIB   = -lnetcdff -lnetcdf       # Load nCDF Library
#FFTW_DIR   = -I/cvmfs/soft.computecanada.ca/easybuild/software/2020/avx2/MPI/intel2020/openmpi4/fftw-mpi/3.3.8/include/                            # Include FFTW3 Library
#FFTW_LIB   = -L/cvmfs/soft.computecanada.ca/easybuild/software/2020/avx2/MPI/intel2020/openmpi4/fftw-mpi/3.3.8/lib -lfftw3_mpi -lfftw3 -lm         # Load FFTW3 Library
#nCDF_DIR   = -I/cvmfs/soft.computecanada.ca/easybuild/software/2020/avx2/MPI/intel2020/openmpi4/netcdf-fortran-mpi/4.5.2/include                   # Include nCDF Library
#nCDF_LIB   = -L/cvmfs/soft.computecanada.ca/easybuild/software/2020/avx2/MPI/intel2020/openmpi4/netcdf-fortran-mpi/4.5.2/lib -lnetcdff -lnetcdf    # Load nCDF Library

# Compilation options: the below options are usually used to compile FORTRAN
#                      code. You can assign other values than those suggested
#                      in the "CompilationOptions" variables.
#CompilationOptions= -O3
# Remove the below "#" to activate compilation in debug mode
#CompilationOptions= -g
# Remove the below "#" to use "gprof", which indicates the computation time in
#    each subroutine
CompilationOptions= -fopenmp -O3 -pg

# List of routines to compile: here we list all object files that are needed.
# Put a "\" at the end of each line that if you want to continue the list of
#    routines on the following line.



#=====  End of variable definitions =====

#===============  There is nothing to change below this line  =============


# Defines a rule: how to build an object file (ending in ".o")
#                 from a source file (ending in ".f90")
# note: "$<" symbols will be replaced by the name of the file that is compiled


# Dependencies of the main executable on the object files (".o") it is built from.
# The dependency of object files on source files (".f") is implied by the above
# implicit rules.
LPS_main: global_variables.o initialize.o fftwfunction.o  data_ops.o  databinary_handle.o \
function_ops.o adj_solvers.o solvers.o optimization.o LPS_main.o
	$(CompilerName) $(CompilationOptions) -o $@ $^ $(BASIC_LIB) $(FFTW_LIB) $(nCDF_LIB)



test_solver: global_variables.o initialize.o fftwfunction.o  data_ops.o  databinary_handle.o \
function_ops.o solvers.o optimization.o test_solver.o
	$(CompilerName) $(CompilationOptions) -o $@ $^ $(BASIC_LIB) $(FFTW_LIB) $(nCDF_LIB)

test_solver_1: global_variables.o initialize.o fftwfunction.o  data_ops.o  databinary_handle.o \
function_ops.o solvers.o optimization.o test_solver_1.o
	$(CompilerName) $(CompilationOptions) -o $@ $^ $(BASIC_LIB) $(FFTW_LIB) $(nCDF_LIB)

test_solver_2: global_variables.o initialize.o fftwfunction.o  data_ops.o  databinary_handle.o \
function_ops.o solvers.o optimization.o test_solver_2.o
	$(CompilerName) $(CompilationOptions) -o $@ $^ $(BASIC_LIB) $(FFTW_LIB) $(nCDF_LIB)

test_solver_3: global_variables.o initialize.o fftwfunction.o  data_ops.o  databinary_handle.o \
function_ops.o solvers.o optimization.o test_solver_3.o
	$(CompilerName) $(CompilationOptions) -o $@ $^ $(BASIC_LIB) $(FFTW_LIB) $(nCDF_LIB)

test_solver_4: global_variables.o initialize.o fftwfunction.o  data_ops.o  databinary_handle.o \
function_ops.o solvers.o optimization.o test_solver_4.o
	$(CompilerName) $(CompilationOptions) -o $@ $^ $(BASIC_LIB) $(FFTW_LIB) $(nCDF_LIB)

test_solver_5: global_variables.o initialize.o fftwfunction.o  data_ops.o  databinary_handle.o \
function_ops.o solvers.o optimization.o test_solver_5.o
	$(CompilerName) $(CompilationOptions) -o $@ $^ $(BASIC_LIB) $(FFTW_LIB) $(nCDF_LIB)

test_solver_6: global_variables.o initialize.o fftwfunction.o  data_ops.o  databinary_handle.o \
function_ops.o solvers.o optimization.o test_solver_6.o
	$(CompilerName) $(CompilationOptions) -o $@ $^ $(BASIC_LIB) $(FFTW_LIB) $(nCDF_LIB)






# Compiling Fortran files:
%.o: %.f90
	$(CompilerName) $(CompilationOptions) -c $(nCDF_DIR) $(FFTW_DIR) $<

clean:
	rm -f *.o *~ *.mod
