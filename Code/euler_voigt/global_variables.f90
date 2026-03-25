MODULE global_variables
  use, intrinsic :: iso_c_binding   ! Newly Added April 17, 2017
  IMPLICIT NONE
!   INCLUDE "mpif.h"
!   include 'fftw3-mpi.f03'
 
  INTEGER, PARAMETER :: pr = KIND (1.0d0)
  INTEGER, PARAMETER :: MAX_ITER = 100
  INTEGER, PARAMETER :: KappaPoints = 16
  REAL, PARAMETER :: OPTIM_TOL = 1.0e-10_pr
  REAL, PARAMETER :: MACH_EPSILON = 1.0e-10_pr 
  REAL, PARAMETER :: TAU_MAX = 100

  REAL(pr), parameter :: WEIGHT = 1.0_pr   ! Newly added on July 15, 2017, WEIGHT*R(u)+(1-WEIGHT)
  integer :: RESOL = 128, K0_index = 0, E0_index = 12000  ! E0 here is the Lq norm (to the power q) of U or Sobolev norm of U
  real(pr) :: fix_dt1  = 0.001_pr, fix_dt2  = 0.00005_pr
  real(pr) :: iniTime = 0.0_pr, endTime = 5.0_pr
  REAL(pr) :: lambda1 = 2.0_pr
  REAL(pr) :: alpha0  = 100.0_pr
  REAL(pr), SAVE :: Jorig                  ! April 3, 2018, added by me.
  Real(pr):: nullvortexrate = 0.01
  Real(pr):: alpha

  Real(pr) :: LPS_q = 4.0_pr, LPS_p , LPSnorm ,LPShalfnorm, Sobolev_order = 0.75_pr     !q>3, 2/p+3/q \leq 1


  !- Run on orca or goblin
  !CHARACTER(len=*), parameter :: work_pathname = "/global/c/work/rrg-bprotas/maxET/E37/T002/L5_N256_dt00008"
  !!CHARACTER(len=*), parameter :: scratch_pathname = "/scratch/kangdi/maxET/E37/T002/L5_N256_dt00008"
  !CHARACTER(len=*), parameter :: scratch_pathname = "/global/c/work/rrg-bprotas/maxET/E37/T002/L5_N256_dt00008"


  !- Run on graham
  CHARACTER(len=*), parameter :: work_pathname = "./results/"
  CHARACTER(len=*), parameter :: scratch_pathname = work_pathname


  CHARACTER(len=*), parameter :: IC_type = "NumMaximizer"
  !CHARACTER(len=*), parameter :: IC_type = "Refine"
  CHARACTER(len=*), parameter :: InitCond_pathname  = "./initial_data/_Uvec_fwdTE0_OPT1.nc"
  !CHARACTER(len=100) :: InitCond_pathname 
  !CHARACTER(len=*), parameter :: IC_type = "BinaryDataFile"
  LOGICAL, parameter :: save_binary2nc = .FALSE.
  !CHARACTER(len=*), parameter :: InitCond_pathname = "/work/kangdi/maxET/E39/T01/4FRT02_L3_N256_dt0003_E37_Uvec_fwdTE0"


  LOGICAL :: kappaTest
  LOGICAL :: toDealias
  LOGICAL :: timing 
  LOGICAL :: save_diag_NS
  LOGICAL :: save_data_NS
  LOGICAL :: calc_geom_NS
  LOGICAL :: calc_ExactSol
  LOGICAL :: save_diag_Constr
  LOGICAL :: save_data_Constr
  LOGICAL :: save_diag_Optim
  LOGICAL :: save_data_Optim
  LOGICAL :: save_diag_lineMin
  LOGICAL :: save_data_lineMin
  LOGICAL :: parallel_data
  LOGICAL :: save_null_vortex


  INTEGER, DIMENSION(3), SAVE :: n
  INTEGER, SAVE :: n_dim  
  INTEGER, SAVE :: DT_index, NU_index, ConsType, iniIndex 
  REAL(pr), SAVE :: E0, K0, PI, visc, dV, Kcut, Kmax
  Integer :: kkmax
  !--NOTE: Kcut = cut frequency used for dealiasing. 
  !-       Kmax = maximal frequency present in solution.
  !-       kkmax = ceiling(sqrt(real(n(1),pr)**2/4_pr+real(n(2),pr)**2/4_pr+real(n(3),pr)**2/4_pr)) 

  REAL(pr), DIMENSION (:), ALLOCATABLE, SAVE :: K1, K2, K3
  REAL(pr), DIMENSION (:), ALLOCATABLE, SAVE :: K1_filter, K2_filter, K3_filter ! added 26/09/21
  REAL(pr), DIMENSION (:), ALLOCATABLE, SAVE :: spectral_k ! added 28/09/21
  REAL(pr), DIMENSION (:,:,:,:), ALLOCATABLE, SAVE :: Uvec, Wvec
  
  !========================================================================== 
  !                            MPI VARIABLES
  !==========================================================================
  INTEGER, SAVE :: rank, Statinfo, np
!  INTEGER, SAVE :: local_nlast, local_last_start, local_nlast_after_trans 
!  INTEGER, SAVE :: local_last_start_after_trans, total_local_size 
!  INTEGER, SAVE :: local_start, local_n

  INTEGER, SAVE :: local_nlastFixres, local_last_startFixres, local_nlast_after_transFixres
  INTEGER, SAVE :: local_last_start_after_transFixres, total_local_sizeFixres, local_startFixres, local_nFixres

  INTEGER, SAVE :: local_nlastHighres, local_last_startHighres, local_nlast_after_transHighres
  INTEGER, SAVE :: local_last_start_after_transHighres, total_local_sizeHighres, local_startHighres, local_nHighres


  !========================================================================== 
  !                            FFTW VARIABLES
  !==========================================================================



  INTEGER(C_INTPTR_T), DIMENSION(3), SAVE :: C_n
  INTEGER(C_INTPTR_T), SAVE :: C_local_alloc, C_local_N, C_local_k_offset       ! Newly Added July 14, 2017
  INTEGER, SAVE :: local_alloc, local_N, local_k_offset, total_local_size       ! Newly Added July 14, 2017


  INTEGER, SAVE :: final_time_iter, reclen
  REAL(pr), DIMENSION (:,:,:,:), ALLOCATABLE, SAVE :: fwd_Field1, fwd_Field2, adj_Uvec, adj_Wvec, Uvec0, adj_Uvec0, adj_Uvec0_direction
  real(pr), dimension(:,:,:), allocatable, save :: global_u




END MODULE
