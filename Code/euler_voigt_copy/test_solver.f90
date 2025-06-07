!-----------------------------------------------------!
! Program used to solve the Cauchy problem            !
! in the  Navier-Stokes system                        !
!                                                     !
! Parallel version, Complex-Complex FFT               !   
!                                                     !
! July, 2014.                                         !
!                                                     !
! Author: Diego Ayala                                 !
! Department of Mathematics and Statistics            !
! McMaster University                                 !
!-----------------------------------------------------!

PROGRAM EULER_VOIGT
   USE global_variables
   USE data_ops
   USE function_ops
   USE solvers
   USE fftwfunction
   USE optimization
   IMPLICIT NONE
   INCLUDE "mpif.h"
   include 'fftw3-mpi.f03'
   
   INTEGER  :: i,ii, FixConstr_flag , iii1,iii2,iii3 ! ??
   REAL(pr) :: aux ,aaa1,aaa2                        ! ??
   ! Initial kinetic energy vector, Initial entrophy vector, 
   REAL(pr), DIMENSION (:), ALLOCATABLE, SAVE :: K0_vec, E0_vec !, DT_vec ! Unused parameter 
   CHARACTER(2)   :: K0txt, E0txt ! String format of K0_index, and E0_index
   CHARACTER(200) :: filename  ! filename
   character(4) :: indexchar   ! ??
   REAL(pr), DIMENSION(:,:), ALLOCATABLE :: Spectrum ! ??

   !REAL(pr), DIMENSION(:,:,:,:), Allocatable ::  , grad_J0, phi_0 , phi_pert ! Unused parameters      !for kappa test
   !Real(pr) :: J_1, J_0, epsil, grad_J0L2, kappa , local_kappa ! Unused parameters
   integer :: stepper 							  ! which time stepper to use
   real(pr) :: t_start, t_end 					  ! record execution time
   real(pr) :: A, B, C                            ! ??
   real(pr), dimension(1:3) :: tau_brack		  ! tau bracket 
   integer omp_get_thread_num,omp_get_num_threads ! thread number and number of threads

   REAL(pr) :: PHI1 = 0.0_pr                       ! Objective function testing
   REAL(pr) :: PHI2 = 0.0_pr

   !=============================================
   ! MPI
   !=============================================
   CALL MPI_INIT(Statinfo)
   CALL MPI_COMM_RANK(MPI_COMM_WORLD,rank,Statinfo)
   CALL MPI_COMM_SIZE(MPI_COMM_WORLD,np,Statinfo)
   call fftw_mpi_init()
   !=============================================
   ! Set parameters' values
   !=============================================

   LPSnorm = 0.0_pr								   ! ??
   LPS_p = 2.0_pr * LPS_q /(LPS_q - 3.0_pr)        ! ??

   CALL nse_msg_handle(100)   ! Mar 1, 2018, used to create file in LOGFILES
   WRITE(K0txt, '(i2.2)') K0_index    
   WRITE(E0txt, '(i2.2)') E0_index    
   NU_index = 0                      ! viscosity index ?
   iniIndex = 0                       ! ??
   CALL MPI_BARRIER(MPI_COMM_WORLD,Statinfo)
   CALL MPI_BCAST(RESOL, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, Statinfo)
   CALL MPI_BCAST(K0_index, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, Statinfo)
   CALL MPI_BCAST(E0_index, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, Statinfo) 
   CALL MPI_BCAST(NU_index, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, Statinfo) 
   CALL MPI_BCAST(iniIndex, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, Statinfo)
   CALL MPI_BCAST(fix_dt1, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
   CALL MPI_BCAST(fix_dt2, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
   CALL MPI_BCAST(iniTime, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
   CALL MPI_BCAST(endTime, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
   !CALL MPI_BCAST (alpha0, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
   
   C_n = RESOL  ! set C_n vector to RESOL
   n   = RESOL  ! set n vector to RESOL
   
   ! Unused since visc is always zero
   !IF (K0_index==0) THEN
   !   ConsType = 1
   !ELSE
   !   ConsType = 2
   !END IF
   !IF (NU_index==0) THEN
   !   visc = 1.0e-2_pr
   !ELSE
   !   aux = REAL(NU_index-1,pr)/3.0_pr 
   !   visc = 1.0e-6_pr*(10.0_pr**aux) 
   !END IF
   
   visc = 0.0e-2_pr

   call initialize
   call function_ops_allocate()

   


   
   IF (rank==0) THEN
      OPEN(10, FILE="./LOGFILES/maxET_parameter_info.log", STATUS='REPLACE')
      WRITE(10,*) "======================================= "
      WRITE(10,*) "  Resolution N      = ", n(1)
      WRITE(10,*) "  Energy K0         = ", K0
      WRITE(10,*) "  Enstrophy E0      = ", E0
      WRITE(10,*) "  Initial time      = ", iniTime
      WRITE(10,*) "  Final time        = ", endTime
      WRITE(10,*) "  Viscosity         = ", visc
      WRITE(10,*) "  Processors        = ", np
      WRITE(10,*) "  Initial condition = ", IC_type
      WRITE(10,*) "======================================= "
      CLOSE(10)
   END IF
   
   
   CALL MPI_BARRIER(MPI_COMM_WORLD,Statinfo)

   !=======================================================
   !- Load/Create initial condition, then do fix constraint
   !=======================================================
 
   CALL init_fft
  
     




   !=======================================================
   !- Save the origional velocity
   !=======================================================
   if (rank == 0) call cpu_time(t_start)

   
   CALL nse_msg_handle(0)
   CALL MPI_BARRIER(MPI_COMM_WORLD,Statinfo)
  
   Uvec0           = Uvec
   final_time_iter = 0      ! This variable acts as a time iteration counter



   CALL nse_msg_handle(1)
            
   !compute the foward time evolution: save_sign = 1, myindex = 1


   if (0) then
      stepper = 3
      call solvers_allocate(stepper)
      do i = 0,10
         visc = 10.0_pr**(-i)
         call set_initial(Uvec0,4,11111,2222,31234)
         call fwd_3D(Uvec0, fix_dt1, 1, stepper, i)
      end do
      call solvers_deallocate()

   end if

   
   
   !=======================================================
   !- Euler-Voigt Simulations
   !=======================================================
   if (1) then
      endTime = 0.25_pr
      stepper = 3
      visc = 0.0_pr
      alpha = 4.0_pr/256.0_pr
      fix_dt1 = 2.0_pr**(-5)
      call solvers_allocate(stepper)
      ! s=3, l=1, sigma = 1E-1, ..., 1E-5, ! norm_constr = 1
      call optimization_allocate(1.0_pr,1.0_pr, 3.0_pr, 0.001_pr, stepper)
      call set_initial(Uvec0, 2, 11111,2222,31234)
      tau_brack(1) = 0.0_pr
      tau_brack(2) = 5000000.0_pr
      !call maximization(tau_brack)
      call report_PHI(Uvec0,tau_brack, 100, fix_dt1)

      !PHI1 = compute_PHI_L2(Uvec0, fix_dt1, 1, 1, 0)      

      !filename = "./LOGFILES/PHI_TEST.dat"
      !OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'REPLACE')
      !WRITE(10, "(1 G20.12)") PHI1
      !CLOSE(10)

      call optimization_deallocate()
      call solvers_deallocate()
   

   end if

   ! kappa test
   if (0) then
      endTime = 1.0_pr
      stepper = 3
      visc = 0.0_pr
      alpha = 4.0_pr/256.0_pr
      stepper = 3
      visc = 0.0_pr
      call solvers_allocate(stepper)

      do i = 4,8
         fix_dt1 = 2.0_pr**(-i)
         if (rank == 0) then
            WRITE(indexchar, '(i4)') i
            filename = TRIM(scratch_pathname)//"kappa_test_error"//trim(adjustl(indexchar))//".dat"
            open(30, file = filename, status = 'replace')
            close(30)
         endif
         
         call kappa_test(Uvec0,adj_Uvec0_direction,fix_dt1,1,stepper,i)
      end do
      
      call solvers_deallocate()
   end if
   

   if (0) then
      visc = 0.0e-2_pr
      stepper = 3
      call solvers_allocate(stepper)
      do i = 2,16,2
         fix_dt1 = 2.0_pr**(-i)
         call set_initial(Uvec0,3,123456,7778446,5213445)
         call fwd_3D(Uvec0, fix_dt1, 1, stepper, i)         
      end do
      call solvers_deallocate()
   end if



  DEALLOCATE(Uvec)
  DEALLOCATE(Wvec)
  DEALLOCATE(Uvec0)
  DEALLOCATE(adj_Uvec)
  !DEALLOCATE(adj_Wvec)
  DEALLOCATE(adj_Uvec0)
  DEALLOCATE(adj_Uvec0_direction)
  DEALLOCATE(K1)
  DEALLOCATE(K2)
  DEALLOCATE(K3)
  DEALLOCATE(K1_filter)
  DEALLOCATE(K2_filter)
  DEALLOCATE(K3_filter)
  DEALLOCATE(fwd_Field1)
  DEALLOCATE(fwd_Field2)
  
!  call close_fft
  CALL fft_deallocate()
  call function_ops_deallocate()

  CALL MPI_FINALIZE (Statinfo)
if (rank == 0) then
   call cpu_time(t_end)
   print *, "exit normally"
   print *, "stepper = ", stepper
   print *, "execution time = ", (t_end - t_start)/3600.0_pr
end if
END PROGRAM EULER_VOIGT



