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
   REAL(pr) :: PHI3 = 0.0_pr
   REAL(pr) :: tau = 0.0_pr
   LOGICAL :: TEST1, TEST2, TEST3, TEST4
   real(pr) :: norm2_grad
   REAL(pr) :: val1, val2, val3, val4

   INTEGER :: arg
   CHARACTER(len=32) :: arg_char
   call get_command_argument(1,arg_char)
   READ (arg_char, '(I13)') arg
   arg_char = TRIM(arg_char)//"/"
   
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
      OPEN(10, FILE="./LOGFILES/"//TRIM(arg_char)//"maxET_parameter_info.log", STATUS='REPLACE')
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

   !=======================================================
   !- Viscosity Test: Navier-Stokes Simulations
   !=======================================================
   if (0) then
      stepper = 3
      call solvers_allocate(stepper, arg_char)
      do i = 0,10
         visc = 10.0_pr**(-i)
         call set_initial(Uvec0,4,11111,2222,31234)
         call fwd_3D(Uvec0, fix_dt1, 1, stepper, i)
      end do
      call solvers_deallocate()
   end if

   !=======================================================
   !- Maximization Test: Euler-Voigt Simulations
   !=======================================================
   if (0) then
      ! Set Constants
      endTime = 10.0_pr*arg;
      stepper = 3
      visc = 0.0_pr
      alpha = 16.0_pr/256.0_pr ! (2.0_pr**(arg-1))/256.0_pr
      fix_dt1 = 2.0_pr**(-5)

      IF (rank==0) THEN
        OPEN(10, FILE="./LOGFILES/"//TRIM(arg_char)//"maxET_parameter_info.log", STATUS='REPLACE')
        WRITE(10,*) "======================================= "
        WRITE(10,*) "  Resolution N      = ", n(1)
        WRITE(10,*) "  Energy K0         = ", K0
        WRITE(10,*) "  Enstrophy E0      = ", E0
        WRITE(10,*) "  Initial time      = ", iniTime
        WRITE(10,*) "  Final time        = ", endTime
        WRITE(10,*) "  Viscosity         = ", visc
        WRITE(10,*) "  Processors        = ", np
        WRITE(10,*) "  Alpha             = ", alpha
        WRITE(10,*) "  Initial condition = ", IC_type
        WRITE(10,*) "======================================= "
        CLOSE(10)
      END IF
      
      ! Allocate
      call solvers_allocate(stepper, arg_char)
      call optimization_allocate(1.0_pr,1.0_pr, 3.0_pr, 0.1_pr, stepper, arg_char)

      ! set 3D Taylor Green Initial Condition
      call set_initial(Uvec0, 2, 11111,2222,31234)
      call rescale_H1(Uvec0, val1)

      ! init taubrak
      tau_brack(1) = 0.0_pr
      tau_brack(2) = 500000.0_pr
      
      ! maximize
      call maximization_RCG(tau_brack)

      ! Deallocate
      call optimization_deallocate()
      call solvers_deallocate()
   end if

   !=======================================================
   !- Brent Test: Euler-Voigt Simulations
   !=======================================================
   if (0) then
      ! Set Constants
      endTime = 5.0_pr
      stepper = 3
      visc = 0.0_pr
      alpha = 4.0_pr/256.0_pr
      fix_dt1 = 2.0_pr**(-5)
      
      ! Allocate
      call solvers_allocate(stepper, arg_char)
      call optimization_allocate(1.0_pr,1.0_pr, 3.0_pr, 0.001_pr, stepper, arg_char)

      !set 3D Taylor Green Initial Conditions
      call set_initial(Uvec0, 2, 11111,2222,31234)
      call rescale_H1(Uvec0,PHI1)

      ! compute projected gradient
      PHI1 = compute_PHI_L2(Uvec0, fix_dt1, 1, 1, 0, 1)
      call compute_gradPHI(Uvec0, fix_dt2, 0, gradPHI_opt, 1)
      call projection(Uvec0, gradPHI_opt, d_opt, norm2_grad)

      ! Minimize Taubrak
      tau_brack(1) = 0.0_pr
      tau_brack(2) = 500000.0_pr
      i = 0
      tau_brack = mnbrak(Uvec0, d_opt, tau_brack(1), tau_brack(2), i, 1)
      
      ! report PHI
      call report_PHI(Uvec0,tau_brack, 100, fix_dt1, 0)

      ! reset 3D Taylor Green Initial Conditions
      call set_initial(Uvec0, 2, 11111,2222,31234)
      call rescale_H1(Uvec0,PHI1)

      ! compute projected gradient
      PHI1 = compute_PHI_L2(Uvec0, fix_dt1, 1, 1, 0, 1)
      call compute_gradPHI(Uvec0, fix_dt2, 0, gradPHI_opt, 1)
      call projection(Uvec0, gradPHI_opt, d_opt, norm2_grad)

      ! find peak tau and phi
      i = 1
      tau = brent(i, "maxET", Uvec0, d_opt, tau_brack)
      Uvec0 = Uvec0 + tau*d_opt
      call rescale_H1(Uvec0,PHI1)
      PHI1 = compute_PHI_L2(Uvec0, fix_dt1, 1, i, 0, 1)
      
      ! record peak phi
      if (rank == 0) then
            filename = TRIM(scratch_pathname)//TRIM(arg_char)//"peak_cost"//".dat"
            open(10, file = filename, status = 'REPLACE')
            write(10, "(2 G20.12)"), tau, PHI1
            close(10)
      end if

      ! Deallocate
      call optimization_deallocate()
      call solvers_deallocate()
   end if

   !=======================================================
   !- mnbrak Test: Euler-Voigt Simulations
   !=======================================================
   if (0) then
      ! Set Constants
      endTime = 0.25_pr
      stepper = 3
      visc = 0.0_pr
      alpha = 4.0_pr/256.0_pr
      fix_dt1 = 2.0_pr**(-5)
      
      ! Allocate
      call solvers_allocate(stepper, arg_char)
      call optimization_allocate(1.0_pr,1.0_pr, 3.0_pr, 0.001_pr, stepper, arg_char)
      
      ! Set 3D Taylor Green Initial Condition
      call set_initial(Uvec0, 2, 11111, 2222, 31234)
      call rescale_H1(Uvec0, PHI1)
      
      ! Compute Projected Gradient
      PHI1 = compute_PHI_L2(Uvec0, fix_dt1, 1, 1, 0, 1)
      call compute_gradPHI(Uvec0, fix_dt2, 0, gradPHI_opt, 1)
      call projection(Uvec0, gradPHI_opt, d_opt, norm2_grad)      
      ! Minimize Taubrak
      tau_brack(1) = 0.0_pr
      tau_brack(2) = 500000.0_pr
      i = 0
      tau_brack = mnbrak(Uvec0, d_opt, tau_brack(1), tau_brack(2), i, 1)
      
      !report PHI
      call report_PHI(Uvec0, tau_brack, 100, fix_dt1, 0)

      ! Deallocate
      call optimization_deallocate()
      call solvers_deallocate()
   
   end if
   
   !=======================================================
   !- report_PHI Test: Euler-Voigt Simulations
   !=======================================================
   if (0) then
      ! Set Constants
      endTime = 32.0_pr
      stepper = 3
      visc = 0.0_pr
      alpha = 1.0_pr/256.0_pr
      fix_dt1 = 2.0_pr**(-7)

      ! Allocate
      call solvers_allocate(stepper, arg_char)
      call optimization_allocate(1.0_pr,1.0_pr, 3.0_pr, 0.001_pr, stepper, arg_char)
      
      ! Set 3D Taylor Green Initial Condition
      call set_initial(Uvec0, 2, 11111,2222,31234)
      call rescale_H1(Uvec0,PHI1)

      ! Report Phi
      tau_brack(1) = 0.0_pr
      tau_brack(2) = 50000.0_pr
      call report_PHI(Uvec0, tau_brack, 100, fix_dt1, 1)

      ! Deallocate
      call optimization_deallocate()
      call solvers_deallocate()
   end if

   !=======================================================
   !- Timescale Test: Euler-Voigt Simulations
   !=======================================================
   if (1) then
      ! Set Constants
      endTime = 10.0_pr
      stepper = 3
      visc = 0.0_pr
      alpha = 0.0_pr ! (2.0_pr**(arg-1))/256.0_pr
      fix_dt1 = 2.0_pr**(-5)

      ! Allocate
      call solvers_allocate(stepper, arg_char)
      call optimization_allocate(1.0_pr,1.0_pr, 3.0_pr, 0.001_pr, stepper, arg_char)

      ! Set 3D Taylor Green Initial Condition
      call set_initial(Uvec0, 2, 11111,2222,31234)
      !call rescale_H1(Uvec0, PHI1)

      ! Evolve forward
      PHI1 = compute_PHI_L2(Uvec0, fix_dt1, 1, arg, 0, 1)

      IF (rank==0) THEN
        OPEN(10, FILE="./LOGFILES/"//TRIM(arg_char)//"maxET_parameter_info.log", STATUS='REPLACE')
        WRITE(10,*) "======================================= "
        WRITE(10,*) "  Resolution N      = ", n(1)
        WRITE(10,*) "  Energy K0         = ", K0
        WRITE(10,*) "  Enstrophy E0      = ", E0
        WRITE(10,*) "  Initial time      = ", iniTime
        WRITE(10,*) "  Final time        = ", endTime
        WRITE(10,*) "  Viscosity         = ", visc
        WRITE(10,*) "  Processors        = ", np
        WRITE(10,*) "  Alpha             = ", alpha
        WRITE(10,*) "  Initial condition = ", IC_type
        WRITE(10,*) "  Time Step         = ", fix_dt1
        WRITE(10,*) "======================================= "
        WRITE(10,*) "  Phi               = ", PHI1
        CLOSE(10)
      END IF

      ! Deallocate
      call optimization_deallocate()
      call solvers_deallocate()
   end if

   !=======================================================
   !- Kappa Test: Euler-Voigt Simulations
   !=======================================================
   if (0) then
      endTime = 25.0_pr
      stepper = 3
      visc = 0.0_pr
      alpha = 4.0_pr/256.0_pr

      ! Allocate
      call solvers_allocate(stepper, arg_char)

      ! Iterate over time step size
      do i = 4,11
         fix_dt1 = 2.0_pr**(-i)
         if (rank == 0) then
            WRITE(indexchar, '(i4)') i
            filename = TRIM(scratch_pathname)//TRIM(arg_char)//"kappa_test_error"//trim(adjustl(indexchar))//".dat"
            open(30, file = filename, status = 'replace')
            close(30)
         endif
         
         ! call kappa tests
         call kappa_test(Uvec0,adj_Uvec0_direction,fix_dt1,1,stepper,i)
      end do
      
      ! Deallocate
      call solvers_deallocate()
   end if
   
   !=======================================================
   !- Time Step Test: Euler-Voigt Simulations
   !=======================================================
   if (0) then
      visc = 0.0e-2_pr
      stepper = 3
      call solvers_allocate(stepper, arg_char)
      do i = 2,16,2
         fix_dt1 = 2.0_pr**(-i)
         call set_initial(Uvec0,3,123456,7778446,5213445)
         call fwd_3D(Uvec0, fix_dt1, 1, stepper, i)         
      end do
      call solvers_deallocate()
   end if

! s=3, l=1, sigma = 1E-1, ..., 1E-5, ! norm_constr = 1

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





