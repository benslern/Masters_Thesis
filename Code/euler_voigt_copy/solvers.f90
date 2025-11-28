!======================================
! MODULE CONTAINING INTERFACES FOR 
! SOLVERS FOR 3D INCOMPRESSIBLE EULER EQUATIONS
!
! MODULE
! (*) SOLVERS
! 
! SUBROUTINE
! (*) solvers_allocate
! (*) solvers_deallocate
! (*) set_initial
! (*) FFt
! (*) leapfrog
! (*) ssprk3
! (*) rk4
! (*) rk5
! (*) rk45
! (*) fwd_3D
! (*) FFt_bkd
! (*) inter_Hermite
! (*) leapfrog_bk
! (*) ssprk3_bk
! (*) rk4_bk
! (*) rk5_bk
! (*) adj_initialize
! (*) bkd_3D
! (*) kappa_test
! (*) save_spectral
! (*) save_energy

!
!======================================
module solvers
  !==================================================
  ! computing the time evolution of 3D incompressible Euler equations 
  ! stepper =
  ! 1: leapfrog 
  ! 2: SSPRK3
  ! 3: RK4
  ! 4: RK5
  ! 5: RK45
  !================================================== 
  use global_variables
  IMPLICIT NONE
  REAL(pr), DIMENSION(:,:), ALLOCATABLE :: A_solver
  REAL(pr), DIMENSION(:), ALLOCATABLE :: b_solver, b2_solver
  REAL(pr), DIMENSION(:,:,:,:,:), ALLOCATABLE :: K_solver
  REAL(pr), DIMENSION(:,:,:,:), ALLOCATABLE ::ff_temp1
  REAL(pr), DIMENSION(:,:,:,:), ALLOCATABLE :: temp1_solver, temp2_solver, temp3_solver, temp4_solver, Umid_solver
  COMPLEX(pr), DIMENSION(:,:,:,:), ALLOCATABLE :: temp1_solver_cx, temp2_solver_cx, temp3_solver_cx, temp4_solver_cx, ff_temp1_cx
  REAL(pr), DIMENSION(:,:,:,:), ALLOCATABLE :: U2d_solver, W2d_solver
  COMPLEX(pr), DIMENSION(:,:,:,:), ALLOCATABLE :: U2d_solver_cx, W2d_solver_cx
  
  
 !needed for report
  real(pr), dimension(:,:), allocatable :: spectral_data

  real(pr) :: K_total, E_total, H_total, maxW_global, H1_norm, H1_seminorm, H2_seminorm, H3_seminorm
  real(pr), dimension(1:3) :: E_component
  integer :: mm = 10 ! vorticity moments
  real(pr), dimension(:), allocatable :: Omega ! vorticity_moments
  character(10) :: subpath
 

CONTAINS
  !========================================================= 
  ! SUBROUTINE: solver_allocate(stepper)
  !
  ! allocate the variables needed for solver
  !=========================================================
  SUBROUTINE solvers_allocate(stepper, subpath_)
    use global_variables
    IMPLICIT NONE
    integer, intent(in) :: stepper
    character(len=*), intent(in) :: subpath_
    
    subpath = subpath_
    if (.not. allocated(ff_temp1)) allocate(ff_temp1(1:n(1), 1:n(2), 1:local_N, 1:3))
    
    if (.not. allocated(temp1_solver)) allocate(temp1_solver(1:n(1), 1:n(2), 1:local_N, 1:3))
    if (.not. allocated(temp2_solver)) allocate(temp2_solver(1:n(1), 1:n(2), 1:local_N, 1:3))
    if (.not. allocated(temp3_solver)) allocate(temp3_solver(1:n(1), 1:n(2), 1:local_N, 1:3))   
    if (.not. allocated(temp4_solver)) allocate(temp4_solver(1:n(1), 1:n(2), 1:local_N, 1:3))

    if (.not. allocated(temp1_solver_cx)) allocate(temp1_solver_cx(1:n(1)/2+1, 1:n(2), 1:local_N, 1:3))
    if (.not. allocated(temp2_solver_cx)) allocate(temp2_solver_cx(1:n(1)/2+1, 1:n(2), 1:local_N, 1:3))
    if (.not. allocated(temp3_solver_cx)) allocate(temp3_solver_cx(1:n(1)/2+1, 1:n(2), 1:local_N, 1:3))
    if (.not. allocated(temp4_solver_cx)) allocate(temp4_solver_cx(1:n(1)/2+1, 1:n(2), 1:local_N, 1:3))

    if (.not. allocated(ff_temp1_cx)) allocate(ff_temp1_cx(1:n(1)/2+1, 1:n(2), 1:local_N, 1:3))
    if (.not. allocated(spectral_data)) allocate(spectral_data(1:kkmax,1:2))
    if (.not. allocated(Omega)) allocate(Omega(1:mm))

    if (0) then
       if (.not. allocated(U2d_solver)) allocate(U2d_solver(1:n(1), 1:n(2), 1:local_N, 1:3))
       if (.not. allocated(U2d_solver_cx)) allocate(U2d_solver_cx(1:n(1)/2+1, 1:n(2), 1:local_N, 1:3))
       if (.not. allocated(W2d_solver)) allocate(W2d_solver(1:n(1), 1:n(2), 1:local_N, 1:3))
       if (.not. allocated(W2d_solver_cx)) allocate(W2d_solver_cx(1:n(1)/2+1, 1:n(2), 1:local_N, 1:3))
    end if
   
    select case (stepper)
    case(1)
       if (.not. allocated(Umid_solver)) allocate(Umid_solver(1:n(1), 1:n(2), 1:local_N, 1:3))
       if (.not. allocated(K_solver)) allocate(K_solver(1:n(1), 1:n(2), 1:local_N, 1:3, 1))
    case(2)
       if(.not. allocated(K_solver)) allocate(K_solver(1:n(1), 1:n(2), 1:local_N, 1:3, 1:3))
    case(3)
       if(.not. allocated(K_solver)) allocate(K_solver(1:n(1), 1:n(2), 1:local_N, 1:3, 1:4))
    case(4)
       if(.not. allocated(A_solver)) allocate(A_solver(1:5, 1:5))
       if(.not. allocated(b_solver)) allocate(b_solver(1:6))
       if(.not. allocated(K_solver)) allocate(K_solver(1:n(1), 1:n(2), 1:local_N, 1:3, 1:6))
       A_solver(1,1) = 1.0_pr/5.0_pr
       A_solver(2,1) = 3.0_pr/40.0_pr
       A_solver(2,2) = 9.0_pr/40.0_pr
       A_solver(3,1) = 44.0_pr/45.0_pr
       A_solver(3,2) = -56.0_pr/15.0_pr
       A_solver(3,3) = 32.0_pr/9.0_pr
       A_solver(4,1) = 19372.0_pr/6561.0_pr
       A_solver(4,2) = -25360.0_pr/2187.0_pr
       A_solver(4,3) = 64448.0_pr/6561.0_pr
       A_solver(4,4) = -212.0_pr/729.0_pr
       A_solver(5,1) = 9017.0_pr/3168.0_pr
       A_solver(5,2) = -355.0_pr/33.0_pr
       A_solver(5,3) = 46732.0_pr/5247.0_pr
       A_solver(5,4) = 49.0_pr/176.0_pr
       A_solver(5,5) = -5103.0_pr/18656.0_pr

       b_solver(1) = 35.0_pr/384.0_pr
       b_solver(2) = 0.0_pr
       b_solver(3) = 500.0_pr/1113.0_pr
       b_solver(4) = 125.0_pr/192.0_pr
       b_solver(5) = -2187.0_pr/6784.0_pr
       b_solver(6) = 11.0_pr/84.0_pr

    case(5)
       if(.not. allocated(A_solver)) allocate(A_solver(1:5, 1:5))
       if(.not. allocated(b_solver)) allocate(b_solver(1:6))
       if(.not. allocated(b2_solver)) allocate(b2_solver(1:6))
       if(.not. allocated(K_solver)) allocate(K_solver(1:n(1), 1:n(2), 1:local_N, 1:3, 1:6))
       A_solver(1,1) = 1.0_pr/4.0_pr
       A_solver(2,1) = 3.0_pr/32.0_pr
       A_solver(2,2) = 9.0_pr/32.0_pr
       A_solver(3,1) = 1932.0_pr/2197.0_pr
       A_solver(3,2) = -7200.0_pr/2197.0_pr
       A_solver(3,3) = 7296.0_pr/2197.0_pr
       A_solver(4,1) = 439.0_pr/216.0_pr
       A_solver(4,2) = -8.0_pr
       A_solver(4,3) = 3680.0_pr/513.0_pr
       A_solver(4,4) = -845.0_pr/4104.0_pr
       A_solver(5,1) = -8.0_pr/27.0_pr
       A_solver(5,2) = 2.0_pr
       A_solver(5,3) = -3544.0_pr/2565.0_pr
       A_solver(5,4) = 1859.0_pr/4104.0_pr
       A_solver(5,5) = -11.0_pr/40.0_pr

       b_solver(1) = 25.0_pr/216.0_pr
       b_solver(2) = 0.0_pr
       b_solver(3) = 1408.0_pr/2565.0_pr
       b_solver(4) = 2197.0_pr/4104.0_pr
       b_solver(5) = -1.0_pr/5.0_pr
       b_solver(6) = 0.0_pr

       b2_solver(1) = 16.0_pr/135.0_pr
       b2_solver(2) = 0.0_pr
       b2_solver(3) = 6656.0_pr/12825.0_pr
       b2_solver(4) = 28561.0_pr/56430.0_pr
       b2_solver(5) = -9.0_pr/50.0_pr
       b2_solver(6) = 2.0_pr/55.0_pr

       

       
    end select

       
    
  END SUBROUTINE solvers_allocate

  !===================================================

  !========================================================= 
  ! SUBROUTINE: solver_deallocate()
  !
  ! deallocate the variables needed for solver
  !=========================================================
  SUBROUTINE solvers_deallocate()
    use global_variables
    IMPLICIT NONE
    
    if(allocated(ff_temp1)) deallocate(ff_temp1)

    if(allocated(temp1_solver)) deallocate(temp1_solver)
    if(allocated(temp2_solver)) deallocate(temp2_solver)
    if(allocated(temp3_solver)) deallocate(temp3_solver)
    if(allocated(temp4_solver)) deallocate(temp4_solver)
    if(allocated(temp1_solver_cx)) deallocate(temp1_solver_cx)
    if(allocated(temp2_solver_cx)) deallocate(temp2_solver_cx)
    if(allocated(temp3_solver_cx)) deallocate(temp3_solver_cx)
    if(allocated(temp4_solver_cx)) deallocate(temp4_solver_cx)
    if(allocated(ff_temp1_cx)) deallocate(ff_temp1_cx)
    if(allocated(A_solver)) deallocate(A_solver)
    if(allocated(b_solver)) deallocate(b_solver)
    if(allocated(b2_solver)) deallocate(b2_solver)
    if(allocated(K_solver)) deallocate(K_solver)
    
    if(allocated(spectral_data)) deallocate(spectral_data)
    if(allocated(Omega)) deallocate(Omega)
    if(allocated(Umid_solver)) deallocate(Umid_solver)
    if(allocated(U2d_solver)) deallocate(U2d_solver)
    if(allocated(U2d_solver_cx)) deallocate(U2d_solver_cx)
    if(allocated(W2d_solver)) deallocate(W2d_solver)
    if(allocated(W2d_solver_cx)) deallocate(W2d_solver_cx)


  END SUBROUTINE solvers_deallocate

  !===================================================
  !  SUBROUTINE: set_initial(inifield, type)
  !  USE: temp1_solver_cx
  !  type:
  !  1: 2d Taylor-Green
  !  2: 3d Taylor-Green
  !  3: 3d abc
  !  4: 3d random
  !=================================================== 

  SUBROUTINE set_initial(inifield, type, seed1, seed2, seed3)
  
    USE global_variables
    USE fftwfunction
    USE function_ops
    
    IMPLICIT NONE
    INCLUDE "mpif.h"
    REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(OUT) :: inifield
    real(pr), dimension(:), allocatable :: random1, random2, random3
    integer, dimension(:), allocatable :: Nk, Nk_global
    integer, dimension(:), allocatable :: seed
    integer, INTENT(IN) :: type
    integer :: iii1, iii2, iii3, ii
    integer :: nn,range
    real(pr) :: A, B, C
    real(pr) :: norm_k
    real(pr) :: delta_y1, delta_y2, delta_x, delta_z, x0, z0, RR, r, Lx, Ly, Lz, x,y,z, s, wr,y2, xs, zs, val, ur
    integer, intent(in) :: seed1, seed2, seed3


    select case (type)
    case (1)
       ! 2d taylor-green

       ! HatW_k(t) = exp(-|k|^2*visc*t)hatW_k(0)
       ii = 1
      
       ! set up the vorticity field
       inifield = 0.0_pr

       do iii3 = 1, local_N
          do iii2 = 1, n(2)
             do iii1 = 1, n(1)
                inifield(iii1, iii2, iii3, 1) = sin(2.0_pr*PI*ii*(iii1-1.0_pr)/(1.0_pr*n(1))) &
                     * cos(2.0_pr*PI*ii*(iii2-1.0_pr)/(1.0_pr*n(2)))
                inifield(iii1, iii2, iii3, 2) = - cos(2.0_pr*PI*ii*(iii1-1.0_pr)/(1.0_pr*n(1))) &
                     * sin(2.0_pr*PI*ii*(iii2-1.0_pr)/(1.0_pr*n(2)))
                
             end do
          end do
       end do

       CALL MPI_BARRIER(MPI_COMM_WORLD,Statinfo)
       
    

    case (2)
       ! 3d taylor-green
       inifield = 0.0_pr
       Do iii3 = 1, local_N
          Do iii2 = 1, n(2)
             Do iii1 = 1, n(1)
                inifield(iii1,iii2,iii3,1) = sin(2*PI/real(n(1), pr)*real(iii1-1, pr)) &
                     * cos(2*PI/real(n(2), pr)*real(iii2-1, pr)) &
                     * cos(2*PI/real(n(3), pr)*real(iii3+local_k_offset-1, pr))
                inifield(iii1,iii2,iii3,2) = -cos(2*PI/real(n(1), pr)*real(iii1-1, pr)) &
                     * sin(2*PI/real(n(2), pr)*real(iii2-1, pr)) &
                     * cos(2*PI/real(n(3), pr)*real(iii3+local_k_offset-1, pr))
                inifield(iii1,iii2,iii3,3) = 0.0_pr
             END DO
          END DO
       END DO

       CALL MPI_BARRIER(MPI_COMM_WORLD,Statinfo)
       call fftfwd_m(inifield, temp1_solver_cx, 3)
       call div_free_fourier(temp1_solver_cx)
       call dealiasing_cutoff_m(temp1_solver_cx, 3)
       !call dealiasing_fourier_m(temp1_solver_cx, 3)
       call fftbwd_m(temp1_solver_cx, inifield,3)

    case (3)
       ! 3d taylor-green
          inifield = 0.0_pr
          ii = 3
          Do iii3 = 1, local_N
             Do iii2 = 1, n(2)
                Do iii1 = 1, n(1)
                   inifield(iii1,iii2,iii3,1) = sin(ii*2*PI/real(n(1), pr)*real(iii1-1, pr)) &
                        * cos(2*PI*ii/real(n(2), pr)*real(iii2-1, pr)) &
                        * cos(2*PI*ii/real(n(3), pr)*real(iii3+local_k_offset-1, pr))
                inifield(iii1,iii2,iii3,2) = -cos(2*PI*ii/real(n(1), pr)*real(iii1-1, pr)) &
                     * sin(2*PI*ii/real(n(2), pr)*real(iii2-1, pr)) &
                     * cos(2*PI*ii/real(n(3), pr)*real(iii3+local_k_offset-1, pr))
                inifield(iii1,iii2,iii3,3) = 0.0_pr
             END DO
          END DO
       END DO

       CALL MPI_BARRIER(MPI_COMM_WORLD,Statinfo)
       call fftfwd_m(inifield, temp1_solver_cx, 3)
       call div_free_fourier(temp1_solver_cx)
       call dealiasing_cutoff_m(temp1_solver_cx, 3)
       !call dealiasing_fourier_m(temp1_solver_cx, 3)
       call fftbwd_m(temp1_solver_cx, inifield,3)

    case(4)
       ! 3d abc
       inifield = 0.0_pr
       A = 1.0_pr
       B = 1.0_pr
       C = 1.0_pr
       Do iii3 = 1, local_N
          Do iii2 = 1, n(2)
             Do iii1 = 1, n(1)
                inifield(iii1,iii2,iii3,1) = A*sin(2*PI/n(3)*(iii3-1+local_k_offset)) + C*cos(2*PI/n(2)*(iii2-1))
                inifield(iii1,iii2,iii3,2) = B*sin(2*PI/n(1)*(iii1-1)) + A*cos(2*PI/n(3)*(iii3-1+local_k_offset))
                inifield(iii1,iii2,iii3,3) = C*sin(2*PI/n(2)*(iii2-1)) + B*cos(2*PI/n(1)*(iii1-1))
             END DO
          END DO
       END DO
       CALL MPI_BARRIER(MPI_COMM_WORLD,Statinfo)
       call fftfwd_m(inifield, temp1_solver_cx, 3)
       call div_free_fourier(temp1_solver_cx)
       call dealiasing_cutoff_m(temp1_solver_cx, 3)
       !call dealiasing_fourier_m(temp1_solver_cx, 3)
       call fftbwd_m(temp1_solver_cx, inifield,3)

    case(5)
       ! 3d abc mixture
       inifield = 0.0_pr
       A = 1.0_pr
       B = 2.0_pr
       C = 3.0_pr
       Do iii3 = 1, local_N
          Do iii2 = 1, n(2)
             Do iii1 = 1, n(1)
                inifield(iii1,iii2,iii3,1) = A*sin(2*PI/n(3)*(iii3-1+local_k_offset)) + C*cos(2*PI/n(2)*(iii2-1)) &
                     + A*sin(14*PI/n(3)*(iii3-1+local_k_offset)) + C*cos(14*PI/n(2)*(iii2-1))
                inifield(iii1,iii2,iii3,2) = B*sin(2*PI/n(1)*(iii1-1)) + A*cos(2*PI/n(3)*(iii3-1+local_k_offset)) &
                     + B*sin(14*PI/n(1)*(iii1-1)) + A*cos(14*PI/n(3)*(iii3-1+local_k_offset))
                inifield(iii1,iii2,iii3,3) = C*sin(2*PI/n(2)*(iii2-1)) + B*cos(2*PI/n(1)*(iii1-1)) &
                     + C*sin(14*PI/n(2)*(iii2-1)) + B*cos(14*PI/n(1)*(iii1-1))
             END DO
          END DO
       END DO
       CALL MPI_BARRIER(MPI_COMM_WORLD,Statinfo) 
       call fftfwd_m(inifield, temp1_solver_cx, 3)
       call div_free_fourier(temp1_solver_cx)
       call dealiasing_cutoff_m(temp1_solver_cx, 3)
       !call dealiasing_fourier_m(temp1_solver_cx, 3)
       call fftbwd_m(temp1_solver_cx, inifield,3)

    case(6)
       inifield = 0.0_pr
       ! random initial condition
       ! Hatu_k = |k|^2/3*exp(-|k|^2/(4PI^2))/N(k)*e^i(theta), theta uniform distribution between [0,2pi)
       temp1_solver_cx = cmplx(0.0_pr)
       !range = floor(2.0_pr/3.0_pr*kkmax)
       range = 64
       allocate(random1(1:range))
       allocate(random2(1:range))
       allocate(random3(1:range))
       allocate(Nk(1:range))
       allocate(Nk_global(1:range))
       
       
   
      ! generate a seed
       if (rank == 0) then
          call random_seed(size=nn)
          allocate(seed(1:nn))
          seed = seed1    ! putting arbitrary seed to all elements
          call random_seed(put=seed)
          call random_number(random1)

          seed = seed2    ! putting arbitrary seed to all elements
          call random_seed(put=seed)
          call random_number(random2)

          seed = seed3    ! putting arbitrary seed to all elements
          call random_seed(put=seed)
          call random_number(random3)

          deallocate(seed)

          

          
       end if

       CALL MPI_BARRIER(MPI_COMM_WORLD,Statinfo)
       CALL MPI_BCAST(random1, range, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
       CALL MPI_BCAST(random2, range, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
       CALL MPI_BCAST(random3, range, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)

       Nk = 0
       do iii3=1,local_N
          do iii2=1,n(2)
             do iii1=1,n(1)/2
                if (iii2 == n(2)/2+1 .or. iii3+local_k_offset == n(3)/2+1) then
                   temp1_solver_cx(iii1,iii2,iii3,1) = cmplx(0.0_pr)
                   temp1_solver_cx(iii1,iii2,iii3,2) = cmplx(0.0_pr)
                   temp1_solver_cx(iii1,iii2,iii3,3) = cmplx(0.0_pr)
                else                    
                   norm_k = sqrt( K1(iii1)**2 + K2(iii2)**2 + K3(iii3+local_k_offset)**2 )
                   ii = floor(norm_k/2.0_pr/PI) + 1
                   if (ii <= range) then
                      Nk(ii) = Nk(ii) + 1
                      temp1_solver_cx(iii1,iii2,iii3,1) = exp(-norm_k/4.0_pr/PI) &
                           * cmplx(cos(2*PI*random1(ii)), sin(2*PI*random1(ii)))
                      temp1_solver_cx(iii1,iii2,iii3,2) = exp(-norm_k/4.0_pr/PI) &
                           * cmplx(cos(2*PI*random2(ii)), sin(2*PI*random2(ii)))
                      temp1_solver_cx(iii1,iii2,iii3,3) = exp(-norm_k/4.0_pr/PI) &
                           * cmplx(cos(2*PI*random3(ii)), sin(2*PI*random3(ii)))
                   end if
                end if
                   
                
             end do
          end do
       end do
       do iii3 = 1,local_N
          do iii2 = 2,n(2)
             if (iii2 == n(2)/2+1 .or. iii3+local_k_offset == n(3)/2+1) then
                temp1_solver_cx(1,iii2,iii3,1) = cmplx(0.0_pr)
                temp1_solver_cx(1,iii2,iii3,2) = cmplx(0.0_pr)
                temp1_solver_cx(1,iii2,iii3,3) = cmplx(0.0_pr)
             else                    
                norm_k = sqrt( K1(1)**2 + K2(iii2)**2 + K3(iii3+local_k_offset)**2 )
                ii = floor(norm_k/2.0_pr/PI) + 1
                if (ii <= range) then
                   Nk(ii) = Nk(ii) + 1
                   temp1_solver_cx(1,iii2,iii3,1) = exp(-norm_k/4.0_pr/PI) &
                        * cmplx(cos(2*PI*random1(ii)), sin(2*PI*random1(ii))*sign(1.0_pr, K2(iii2)))
                   temp1_solver_cx(1,iii2,iii3,2) = exp(-norm_k/4.0_pr/PI) &
                        * cmplx(cos(2*PI*random2(ii)), sin(2*PI*random2(ii))*sign(1.0_pr, K2(iii2)))
                   temp1_solver_cx(1,iii2,iii3,3) = exp(-norm_k/4.0_pr/PI) &
                        * cmplx(cos(2*PI*random3(ii)), sin(2*PI*random3(ii))*sign(1.0_pr, K2(iii2)))
                end if
             end if
             
          end do
       end do
       do iii3 = 1,local_N
          if (iii3+local_k_offset == n(3)/2+1) then
             temp1_solver_cx(1,1,iii3,1) = cmplx(0.0_pr)
             temp1_solver_cx(1,1,iii3,2) = cmplx(0.0_pr)
             temp1_solver_cx(1,1,iii3,3) = cmplx(0.0_pr)
          else   
             norm_k = sqrt( K1(1)**2 + K2(1)**2 + K3(iii3+local_k_offset)**2 )
             ii = floor(norm_k/2.0_pr/PI) + 1
             if (ii <= range) then
                Nk(ii) = Nk(ii) + 1
                temp1_solver_cx(1,1,iii3,1) = exp(-norm_k/4.0_pr/PI) &
                     * cmplx(cos(2*PI*random1(ii)), sin(2*PI*random1(ii))*sign(1.0_pr, K3(iii3+local_k_offset)))
                temp1_solver_cx(1,1,iii3,2) = exp(-norm_k/4.0_pr/PI) &
                     * cmplx(cos(2*PI*random2(ii)), sin(2*PI*random2(ii))*sign(1.0_pr, K3(iii3+local_k_offset)))
                temp1_solver_cx(1,1,iii3,3) = exp(-norm_k/4.0_pr/PI) &
                     * cmplx(cos(2*PI*random3(ii)), sin(2*PI*random3(ii))*sign(1.0_pr, K3(iii3+local_k_offset)))
             end if
          end if
          
       end do
       
          
       
      
         
       call mpi_allreduce(Nk, Nk_global, range, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, Statinfo)


       
       do iii3=1,local_N
          do iii2=1,n(2)
             do iii1=1,n(1)/2+1
                norm_k = sqrt( K1(iii1)**2 + K2(iii2)**2 + K3(iii3+local_k_offset)**2 )/2.0_pr/PI
                ii = floor(norm_k) + 1
                if (ii <= range.and.Nk_global(ii)>0) then
                  
                   temp1_solver_cx(iii1,iii2,iii3,1) = temp1_solver_cx(iii1,iii2,iii3,1)/Nk_global(ii)
                   temp1_solver_cx(iii1,iii2,iii3,2) = temp1_solver_cx(iii1,iii2,iii3,2)/Nk_global(ii)
                   temp1_solver_cx(iii1,iii2,iii3,3) = temp1_solver_cx(iii1,iii2,iii3,3)/Nk_global(ii)
                end if
                if (norm_k<1.0e-15_pr) then
                   temp1_solver_cx(iii1,iii2,iii3,1) = cmplx(0.0_pr)
                   temp1_solver_cx(iii1,iii2,iii3,2) = cmplx(0.0_pr)
                   temp1_solver_cx(iii1,iii2,iii3,3) = cmplx(0.0_pr)
                end if
                
             end do
          end do
       end do

       CALL MPI_BARRIER(MPI_COMM_WORLD,Statinfo)
       call div_free_fourier(temp1_solver_cx)
       call dealiasing_cutoff_m(temp1_solver_cx, 3)
       !call dealiasing_fourier_m(temp1_solver_cx, 3)
       call fftbwd_m(temp1_solver_cx, inifield,3)

       deallocate(random1)
       deallocate(random2)
       deallocate(random3)
       deallocate(Nk)
       deallocate(Nk_global)

       case (7)
       ! Kerr's initial condition from Hou 2018
          inifield = 0.0_pr
          temp1_solver = 0.0_pr
          temp2_solver = 0.0_pr
          delta_y1 = 0.5_pr
          delta_y2 = 0.4_pr 
          delta_x = -1.6_pr
          delta_z = 0.0_pr
          x0 = 0.0_pr
          z0 = 1.57_pr
          RR = 0.75_pr
          Lx = 4*PI
          Ly = 4*PI
          Lz = 2*PI
          do iii3 = 1,local_N
             do iii2 = 1, n(2)
                do iii1 = 1, n(1)
                   if (iii1 <= n(1)/2+1) then
                      x = (iii1-1)*1.0_pr/n(1)*4*PI
                   else
                      x = -(n(1)-iii1+1)*1.0_pr/n(1)*4*PI
                   end if
                   if (iii2 <= n(1)/2+1) then
                      y = (iii2-1)*1.0_pr/n(2)*4*PI
                   else
                      y = -(n(2)-iii2+1)*1.0_pr/n(2)*4*PI
                   end if
     
                   y2 = y + Ly*delta_y2*sin(PI*y/Ly)
                   s = y2 + Ly*delta_y1*sin(PI*y2/Ly)
                   xs = x0 + delta_x*cos(PI*s/Lx)
                   zs = z0 + delta_z*cos(PI*s/Lz)
                   if (iii3+local_k_offset <= n(3)/2+1) then
                      z = (iii3+local_k_offset-1)*1.0_pr/n(3)*4*PI
                      r = sqrt((x-xs)**2 + (z-zs)**2)/RR
                      wr = 0
                      if (r<1) then
                         wr = exp(-r**2/(1-r**2)+r**4*(1+r**2+r**4))
                      end if
                      temp1_solver(iii1,iii2,iii3,1) = -wr*PI*delta_x/Lx*(1+PI*delta_y2*cos(PI*y/Ly)) &
                           *(1+PI*delta_y1*cos(PI*y2/Ly))*sin(PI*s/Lx)
                      temp1_solver(iii1,iii2,iii3,2) = wr
                      temp1_solver(iii1,iii2,iii3,3) = -wr*PI*delta_z/Lz*(1+PI*delta_y2*cos(PI*y/Ly)) &
                           *(1+PI*delta_y1*cos(PI*y2/Ly))*sin(PI*s/Lz);
                   else
                      z = (n(3)-iii3-local_k_offset+1)*1.0_pr/n(3)*4*PI
                      r = sqrt((x-xs)**2 + (z-zs)**2)/RR
                      wr = 0
                      if (r<1) then
                         wr = exp(-r**2/(1-r**2)+r**4*(1+r**2+r**4))
                      end if
                      temp1_solver(iii1,iii2,iii3,1) = wr*PI*delta_x/Lx*(1+PI*delta_y2*cos(PI*y/Ly)) &
                           *(1+PI*delta_y1*cos(PI*y2/Ly))*sin(PI*s/Lx)
                      temp1_solver(iii1,iii2,iii3,2) = -wr
                      temp1_solver(iii1,iii2,iii3,3) = wr*PI*delta_z/Lz*(1+PI*delta_y2*cos(PI*y/Ly)) &
                           *(1+PI*delta_y1*cos(PI*y2/Ly))*sin(PI*s/Lz);
                   end if
                end do
             end do
          end do
         
          CALL MPI_BARRIER(MPI_COMM_WORLD,Statinfo)

          temp1_solver = temp1_solver*8.0_pr*4*PI
          call fftfwd_m(temp1_solver, temp1_solver_cx, 3)
          do nn = 1, 3
             do iii3 = 1, local_N
                do iii2 = 1, n(2)
                   do iii1 = 1, n(1)/2+1                      
                      norm_k = exp(-0.05_pr*(K1(iii1)/2/PI)**4)*exp(-0.05_pr*(K2(iii2)/2/PI)**4)*exp(-0.05_pr*(K3(iii3+local_k_offset)/2/PI)**4)
                      temp1_solver_cx(iii1,iii2,iii3,nn) = temp1_solver_cx(iii1,iii2,iii3,nn)*norm_k
                   end do
                end do
             end do
          end do
          call vort2vel_fourier(temp1_solver_cx, temp2_solver_cx)
          call div_free_fourier(temp2_solver_cx)
          call dealiasing_cutoff_m(temp2_solver_cx, 3)
          !call dealiasing_fourier_m(temp2_solver_cx, 3)
          call fftbwd_m(temp2_solver_cx, inifield,3)

       case (8)
       ! Hou's initial condition from Hou 2018
          inifield = 0.0_pr
          
          RR = 0.9_pr
         
          do iii3 = 1,local_N
             do iii2 = 1, n(2)
                do iii1 = 1, n(1)
                   if (iii1 <= n(1)/2+1) then
                      x = (iii1-1)*1.0_pr/n(1)
                   else
                      x = -(n(1)-iii1+1)*1.0_pr/n(1)
                   end if
                   if (iii2 <= n(1)/2+1) then
                      y = (iii2-1)*1.0_pr/n(2)
                   else
                      y = -(n(2)-iii2+1)*1.0_pr/n(2)
                   end if
                   z = (iii3-1+local_k_offset)*1.0_pr/n(3)

                   r = sqrt((x**2 + y**2))/RR
                   ur = 0.0_pr
                   if (r < 1.0_pr) THEN
                      ur = exp(-r**2/(1-r**2))*12000*(1-r**2)**(18) &
                      *sin(2*PI*z)/(1+12.5*(sin(PI*z))**2)
                      ur = r*ur
                   end if
                   if (r > 1e-15) then
                      inifield(iii1,iii2,iii3,1) = -ur*y/r
                      inifield(iii1,iii2,iii3,2) = ur*x/r
                   end if
                   
                   
                   
     
                end do
             end do
          end do
         
          CALL MPI_BARRIER(MPI_COMM_WORLD,Statinfo)

          
          call fftfwd_m(inifield, temp1_solver_cx, 3)
          do nn = 1, 3
             do iii3 = 1, local_N
                do iii2 = 1, n(2)
                   do iii1 = 1, n(1)/2+1                      
                      norm_k = exp(-0.05_pr*(K1(iii1)/2/PI)**4)*exp(-0.05_pr*(K2(iii2)/2/PI)**4)*exp(-0.05_pr*(K3(iii3+local_k_offset)/2/PI)**4)
                      temp1_solver_cx(iii1,iii2,iii3,nn) = temp1_solver_cx(iii1,iii2,iii3,nn)*norm_k
                   end do
                end do
             end do
          end do
          call div_free_fourier(temp1_solver_cx)
          call dealiasing_cutoff_m(temp1_solver_cx, 3)
          !call dealiasing_fourier_m(temp1_solver_cx, 3)
          call fftbwd_m(temp1_solver_cx, inifield,3)

          
          
          
      
      


      
      
       
       
     
       end select
    
      
      

      
       
    

    
    
    END SUBROUTINE set_initial


  !===================================================
  ! SUBROUTINE: FFt(U,Ft)
  ! ff_temp1, temp1_solver_cx, temp2_solver_cx
  ! du/dt = Ft compute the RHS
  !===================================================
  SUBROUTINE FFt(U, Ft)
    use global_variables
    use function_ops
    use fftwfunction
    implicit none
    real(pr), dimension(1:n(1),1:n(2),1:local_N,1:3), intent(in) :: U
    real(pr), dimension(1:n(1),1:n(2),1:local_N,1:3), intent(out) :: Ft
    integer :: flag
    real(pr) :: val

    ! flag = 1: use 3+6 FFTs
    ! flag = 2: use 3+5 FFTs
    flag = 2
    ff_temp1 = 0.0_pr
    Ft = 0.0_pr
    if (flag == 1) then
       
       ! temp1_solver_cx = HatU
       call fftfwd_m(U, temp1_solver_cx, 3)
       ! temp2_solver_cx = HatW
       call vel2vort_fourier(temp1_solver_cx, temp2_solver_cx)
       call fftbwd_m(temp2_solver_cx, ff_temp1, 3)
       ! ff_temp1 = W
       Ft(:,:,:,1) = U(:,:,:,2)*ff_temp1(:,:,:,3) - U(:,:,:,3)*ff_temp1(:,:,:,2)
       Ft(:,:,:,2) = U(:,:,:,3)*ff_temp1(:,:,:,1) - U(:,:,:,1)*ff_temp1(:,:,:,3)
       Ft(:,:,:,3) = U(:,:,:,1)*ff_temp1(:,:,:,2) - U(:,:,:,2)*ff_temp1(:,:,:,1)

       call fftfwd_m(Ft, temp2_solver_cx, 3)

       call div_free_fourier(temp2_solver_cx)
       call fftbwd_m(temp2_solver_cx, Ft, 3)

    
       ! if needed to compute laplacian U
       ! temp1_solver_cx = HatU
       if (0) then
          call laplacian_fourier(temp1_solver_cx, temp1_solver_cx)
          call div_free_fourier(temp1_solver_cx)
          call fftbwd_m(temp1_solver_cx, ff_temp1, 3)
          Ft = Ft + visc*ff_temp1
       end if
    else

       ! ff_temp1 = (u^2-v^2, v^2-w^2, u^2-w^2)
      
       ff_temp1(:,:,:,1) = U(:,:,:,1)**2 - U(:,:,:,2)**2
       ff_temp1(:,:,:,2) = U(:,:,:,2)**2 - U(:,:,:,3)**2
       
       call fftfwd(ff_temp1(:,:,:,1), temp1_solver_cx(:,:,:,1))
       call fftfwd(ff_temp1(:,:,:,2), temp1_solver_cx(:,:,:,2))
       temp1_solver_cx(:,:,:,3) = temp1_solver_cx(:,:,:,1) + temp1_solver_cx(:,:,:,2)

       call derivative_fourier(temp1_solver_cx(:,:,:,1), temp2_solver_cx(:,:,:,1), 1)
       call derivative_fourier(temp1_solver_cx(:,:,:,1), temp2_solver_cx(:,:,:,2), 2)
       temp2_solver_cx(:,:,:,2) = -temp2_solver_cx(:,:,:,2)
       call derivative_fourier(temp1_solver_cx(:,:,:,3), temp2_solver_cx(:,:,:,3), 3)
       temp2_solver_cx(:,:,:,3) = -temp2_solver_cx(:,:,:,3)
      

       ff_temp1_cx = -1.0_pr/3.0_pr*temp2_solver_cx

       call derivative_fourier(temp1_solver_cx(:,:,:,3), temp2_solver_cx(:,:,:,1), 1)
       call derivative_fourier(temp1_solver_cx(:,:,:,2), temp2_solver_cx(:,:,:,2), 2)
       call derivative_fourier(temp1_solver_cx(:,:,:,2), temp2_solver_cx(:,:,:,3), 3)
       temp2_solver_cx(:,:,:,3) = - temp2_solver_cx(:,:,:,3)
       

       ff_temp1_cx = ff_temp1_cx - 1.0_pr/3.0_pr*temp2_solver_cx

       ! ff_temp1 = (uv, vw, wu)

       ff_temp1(:,:,:,1) = U(:,:,:,1)*U(:,:,:,2)
       ff_temp1(:,:,:,2) = U(:,:,:,2)*U(:,:,:,3)
       ff_temp1(:,:,:,3) = U(:,:,:,1)*U(:,:,:,3)

       call fftfwd_m(ff_temp1, temp1_solver_cx, 3)

       call derivative_fourier(temp1_solver_cx(:,:,:,1), temp2_solver_cx(:,:,:,1), 2)
       call derivative_fourier(temp1_solver_cx(:,:,:,1), temp2_solver_cx(:,:,:,2), 1)
       call derivative_fourier(temp1_solver_cx(:,:,:,3), temp2_solver_cx(:,:,:,3), 1)


       ff_temp1_cx = ff_temp1_cx - temp2_solver_cx

       call derivative_fourier(temp1_solver_cx(:,:,:,3), temp2_solver_cx(:,:,:,1), 3)
       call derivative_fourier(temp1_solver_cx(:,:,:,2), temp2_solver_cx(:,:,:,2), 3)
       call derivative_fourier(temp1_solver_cx(:,:,:,2), temp2_solver_cx(:,:,:,3), 2)



       ff_temp1_cx = ff_temp1_cx - temp2_solver_cx

       

       call div_free_fourier(ff_temp1_cx)
       call G_alpha_fourier(ff_temp1_cx, ff_temp1_cx, alpha)
       call fftbwd_m(ff_temp1_cx, Ft, 3)

       
       
    end if
    
       
   

       
  END SUBROUTINE FFt
  !===================================================
   
  !========================================================= 
  ! SUBROUTINE: leapfrog(Umid, U, dt, iter_count)
  !
  ! stepper=1 leapfrog
  !=========================================================
  SUBROUTINE leapfrog(Umid, U, dt, iter_count)
    USE global_variables
    USE fftwfunction
    USE function_ops
    IMPLICIT NONE
    INCLUDE "mpif.h"
    REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(INOUT) :: U
    REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(INOUT) :: Umid
    REAL(pr), INTENT(IN) :: dt
    integer, intent(in) :: iter_count
    integer :: iii1, iii2, iii3

    ! initialize Umid
    if (iter_count == 0) then
       
       ! Compute Umid
       ! k1 = Ft(U^n)
       call FFt(U, K_solver(:,:,:,:,1))

    
       ! k2 = Ft(U^n + 0.5dt*k1)
       temp1_solver = U + 0.5_pr*dt*K_solver(:,:,:,:,1)
       call FFt(temp1_solver, K_solver(:,:,:,:,1))

       Umid = U + dt*K_solver(:,:,:,:,1)

   
       call fftfwd_m(Umid, temp1_solver_cx, 3)
    
       
       call div_free_fourier(temp1_solver_cx);
       call dealiasing_cutoff_m(temp1_solver_cx, 3)
       !call dealiasing_fourier_m(temp1_solver_cx, 3)

    
       call fftbwd_m(temp1_solver_cx, Umid, 3)
    else if (mod(iter_count+1, 2) == 0) then
       ! U = U + 2*dt*F(Umid)
       call FFt(Umid, K_solver(:,:,:,:,1))
       U = U + 2.0_pr*dt*K_solver(:,:,:,:,1)

       call fftfwd_m(U, temp1_solver_cx, 3)
    
       
       call div_free_fourier(temp1_solver_cx);
   
       !call dealiasing_fourier_m(temp1_solver_cx, 3)
       call dealiasing_cutoff_m(temp1_solver_cx, 3)
    
       call fftbwd_m(temp1_solver_cx,U,3)
    else if (mod(iter_count+1, 2) == 1) then
       ! Umid = Umid + 2*dt*F(U)
       call FFt(U, K_solver(:,:,:,:,1))
       Umid = Umid + 2.0_pr*dt*K_solver(:,:,:,:,1)
       call fftfwd_m(Umid, temp1_solver_cx, 3)
    
       
       
       call div_free_fourier(temp1_solver_cx);
       !call dealiasing_fourier_m(temp1_solver_cx, 3)
       call dealiasing_cutoff_m(temp1_solver_cx, 3)
    
    
       call fftbwd_m(temp1_solver_cx, Umid,3)
    end if
       
  END SUBROUTINE leapfrog

  !===================================================

  
  !========================================================= 
  ! SUBROUTINE: SSPRK3(U, dt)
  !
  ! stepper=2 ssprk3
  !=========================================================
  SUBROUTINE ssprk3(U, dt)
    USE global_variables
    USE fftwfunction
    USE function_ops
    IMPLICIT NONE
    INCLUDE "mpif.h"
    REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(INOUT) :: U
    REAL(pr), INTENT(IN) :: dt
    

    ! k1
    call FFt(U, K_solver(:,:,:,:,1))

    ! k2
    temp1_solver = U + dt*K_solver(:,:,:,:,1)
    call FFt(temp1_solver, K_solver(:,:,:,:,2))

    !k3
    temp1_solver = U + dt*0.25_pr*(K_solver(:,:,:,:,1) + K_solver(:,:,:,:,2))
    call FFt(temp1_solver, K_solver(:,:,:,:,3))


    U = U + dt*(1.0_pr/6.0_pr*K_solver(:,:,:,:,1) + 1.0_pr/6.0_pr*K_solver(:,:,:,:,2) + 2.0_pr/3.0_pr*K_solver(:,:,:,:,3))

    ! filter
    call fftfwd_m(U, temp1_solver_cx, 3)
    call div_free_fourier(temp1_solver_cx);
    !call dealiasing_fourier_m(temp1_solver_cx, 3)
    call dealiasing_cutoff_m(temp1_solver_cx, 3)
    call fftbwd_m(temp1_solver_cx, U,3)


    
     
  END SUBROUTINE ssprk3
  !===================================================
  
  !===================================================
  !  SUBROUTINE: rk4(U, dt)
  !  stepper=3 RK4
  !=================================================== 
  SUBROUTINE rk4(U, dt)
    USE global_variables
    USE fftwfunction
    USE function_ops
    IMPLICIT NONE
    INCLUDE "mpif.h"
    REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(INOUT) :: U
    REAL(pr), INTENT(IN) :: dt
    
    
    ! k1
    call FFt(U,  K_solver(:,:,:,:,1))
    
 
    ! k2
    temp1_solver = U + dt*0.5_pr*K_solver(:,:,:,:,1)
    call FFt(temp1_solver, K_solver(:,:,:,:,2))

    
    ! k3
    temp1_solver = U + dt*0.5_pr*K_solver(:,:,:,:,2)
    call FFt(temp1_solver, K_solver(:,:,:,:,3))


    ! k4
    temp1_solver = U + dt*K_solver(:,:,:,:,3)
    call FFt(temp1_solver, K_solver(:,:,:,:,4))

    U = U + dt*(1.0_pr/6.0_pr*K_solver(:,:,:,:,1) &
         + 1.0_pr/3.0_pr*K_solver(:,:,:,:,2) &
         + 1.0_pr/3.0_pr*K_solver(:,:,:,:,3) &
         + 1.0_pr/6.0_pr*K_solver(:,:,:,:,4))
    

    ! filter
    call fftfwd_m(U, temp1_solver_cx, 3)
    call div_free_fourier(temp1_solver_cx);
    call dealiasing_cutoff_m(temp1_solver_cx, 3)
    !call dealiasing_fourier_m(temp1_solver_cx, 3)
    call fftbwd_m(temp1_solver_cx, U,3)
    
     
  END SUBROUTINE rk4

  !===================================================

  !===================================================
  !  SUBROUTINE: rk5(U, dt)
  !  stepper=4 rk5
  !=================================================== 
  SUBROUTINE rk5(U, dt)
    USE global_variables
    USE fftwfunction
    USE function_ops
    IMPLICIT NONE
    INCLUDE "mpif.h"
    REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(INOUT) :: U
    REAL(pr), INTENT(IN) :: dt

    
  
    ! k1
    call FFt(U, K_solver(:,:,:,:,1))
   
    ! k2
    temp1_solver = U + dt*A_solver(1,1)*K_solver(:,:,:,:,1)
    call FFt(temp1_solver, K_solver(:,:,:,:,2))
        
    ! k3
    temp1_solver = U + dt*(A_solver(2,1)*K_solver(:,:,:,:,1) &
         + A_solver(2,2)*K_solver(:,:,:,:,2))
    call FFt(temp1_solver, K_solver(:,:,:,:,3))

    ! k4
    temp1_solver = U + dt*(A_solver(3,1)*K_solver(:,:,:,:,1) &
         + A_solver(3,2)*K_solver(:,:,:,:,2) + A_solver(3,3)*K_solver(:,:,:,:,3))
    call FFt(temp1_solver,  K_solver(:,:,:,:,4))

    ! k5
    temp1_solver = U + dt*(A_solver(4,1)*K_solver(:,:,:,:,1) &
         + A_solver(4,2)*K_solver(:,:,:,:,2) + A_solver(4,3)*K_solver(:,:,:,:,3) &
         + A_solver(4,4)*K_solver(:,:,:,:,4))
    call FFt(temp1_solver, K_solver(:,:,:,:,5))

    ! k6
    temp1_solver = U + dt*(A_solver(5,1)*K_solver(:,:,:,:,1) &
         + A_solver(5,2)*K_solver(:,:,:,:,2) &
         + A_solver(5,3)*K_solver(:,:,:,:,3) &
         + A_solver(5,4)*K_solver(:,:,:,:,4) &
         + A_solver(5,5)*K_solver(:,:,:,:,5))
    
    call FFt(temp1_solver,  K_solver(:,:,:,:,6))

    U = U + dt*(b_solver(1)*K_solver(:,:,:,:,1) &
         + b_solver(2)*K_solver(:,:,:,:,2) &
         + b_solver(3)*K_solver(:,:,:,:,3) &
         + b_solver(4)*K_solver(:,:,:,:,4) &
         + b_solver(5)*K_solver(:,:,:,:,5) &
         + b_solver(6)*K_solver(:,:,:,:,6))


    ! filter
    call fftfwd_m(U, temp1_solver_cx, 3)
    call div_free_fourier(temp1_solver_cx);
    call dealiasing_cutoff_m(temp1_solver_cx, 3)
    !call dealiasing_fourier_m(temp1_solver_cx, 3)
    call fftbwd_m(temp1_solver_cx, U,3)
    
     
  END SUBROUTINE rk5

  !===================================================



  !===================================================
  !  SUBROUTINE: rk45(U, dt, dt_after, t, flag_rk45)
  !  stepper=5 rk45
  !=================================================== 
  SUBROUTINE rk45(U, dt, dt_after, t, flag_rk45)
    USE global_variables
    USE fftwfunction
    USE function_ops
    IMPLICIT NONE
    INCLUDE "mpif.h"
    REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(INOUT) :: U
    REAL(pr), INTENT(INOUT) :: dt
    real(pr), intent(inout) :: dt_after
    integer, intent(inout) :: flag_rk45
    real(pr), intent(in) :: t
    real(pr), dimension(1:3):: error_local_vec
    real(pr) :: error_local, error_global, error_tol
    real(pr) :: fac
    integer :: iter_count, iter_max
    integer :: i1, i2, i3
    

    error_tol = 1.0e-8_pr
    iter_max = 10
    error_global = 1.0_pr
    iter_count = 0
    do while (error_global > error_tol)
       dt = min(dt, endTime - t)
       ! k1
       call FFt(U, K_solver(:,:,:,:,1))
   
       ! k2
       temp1_solver = U + dt*A_solver(1,1)*K_solver(:,:,:,:,1)
       call FFt(temp1_solver, K_solver(:,:,:,:,2))
        
       ! k3
       temp1_solver = U + dt*(A_solver(2,1)*K_solver(:,:,:,:,1) &
            + A_solver(2,2)*K_solver(:,:,:,:,2))
       call FFt(temp1_solver, K_solver(:,:,:,:,3))

       ! k4
       temp1_solver = U + dt*(A_solver(3,1)*K_solver(:,:,:,:,1) &
            + A_solver(3,2)*K_solver(:,:,:,:,2) + A_solver(3,3)*K_solver(:,:,:,:,3))
       call FFt(temp1_solver,  K_solver(:,:,:,:,4))

       ! k5
       temp1_solver = U + dt*(A_solver(4,1)*K_solver(:,:,:,:,1) &
            + A_solver(4,2)*K_solver(:,:,:,:,2) + A_solver(4,3)*K_solver(:,:,:,:,3) &
            + A_solver(4,4)*K_solver(:,:,:,:,4))
       call FFt(temp1_solver, K_solver(:,:,:,:,5))

       ! k6
       temp1_solver = U + dt*(A_solver(5,1)*K_solver(:,:,:,:,1) &
            + A_solver(5,2)*K_solver(:,:,:,:,2) &
            + A_solver(5,3)*K_solver(:,:,:,:,3) &
            + A_solver(5,4)*K_solver(:,:,:,:,4) &
            + A_solver(5,5)*K_solver(:,:,:,:,5))
    
       call FFt(temp1_solver,  K_solver(:,:,:,:,6))

       ! compute the truncation error
       temp1_solver = (b_solver(1) - b2_solver(1))*K_solver(:,:,:,:,1) + &
            (b_solver(3) - b2_solver(3))*K_solver(:,:,:,:,3) + &
            (b_solver(4) - b2_solver(4))*K_solver(:,:,:,:,4) + &
            (b_solver(5) - b2_solver(5))*K_solver(:,:,:,:,5) + &
            (b_solver(6) - b2_solver(6))*K_solver(:,:,:,:,6)

       error_local_vec = energy(temp1_solver)*2.0_pr
       error_local = sqrt(error_local_vec(1) + error_local_vec(2) &
            + error_local_vec(3))

       call mpi_allreduce(error_local, error_global, 1, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, Statinfo)

       
       fac = 0.84_pr*(error_tol/error_global)**0.25_pr
       
       if (error_global <= error_tol .or. flag_rk45 == 0 ) then
          
          U = U + dt*(b_solver(1)*K_solver(:,:,:,:,1) &
               + b_solver(3)*K_solver(:,:,:,:,3)  &
               + b_solver(4)*K_solver(:,:,:,:,4)  &
               + b_solver(5)*K_solver(:,:,:,:,5))
          
          ! filter
          call fftfwd_m(U, temp1_solver_cx, 3)
          
          
          call div_free_fourier(temp1_solver_cx);
          !call dealiasing_fourier_m(temp1_solver_cx, 3)
          call dealiasing_cutoff_m(temp1_solver_cx, 3)
          
          call fftbwd_m(temp1_solver_cx, U,3)
          if (rank == 0) then
             if (error_global <= error_tol) then
                dt_after = fac*dt
             else
                dt_after = dt
             end if
          end if
          CALL MPI_BCAST(dt_after, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
          exit
       else
          if (rank == 0) then
             dt = fac*dt
             iter_count = iter_count + 1
          end if
          CALL MPI_BCAST(dt, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
          call mpi_bcast(iter_count, 1, mpi_integer, 0, mpi_comm_world, statinfo)
          
       end if
       
       if (iter_count > iter_max) then
          flag_rk45 = 0
          if(rank == 0) print *, t, dt, "stop adapting time step"
       end if
       
      
    end do

    if (rank == 0) then
       OPEN(10, FILE="./results/error", STATUS="REPLACE")
       write(3,*), t, dt, error_global
       close(10)
    end if
    
          
          

          


       
    
     
  END SUBROUTINE rk45

  !===================================================


  !===================================================
  !  SUBROUTINE: rk4_reverse(U, dt)
  !  stepper=3 RK4
  !=================================================== 
  SUBROUTINE rk4_reverse(U, dt)
    USE global_variables
    USE fftwfunction
    USE function_ops
    IMPLICIT NONE
    INCLUDE "mpif.h"
    REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(INOUT) :: U
    REAL(pr), INTENT(IN) :: dt
    
    
    ! k1
    call FFt(U,  K_solver(:,:,:,:,1))
    K_solver(:,:,:,:,1) = - K_solver(:,:,:,:,1)
    
 
    ! k2
    temp1_solver = U + dt*0.5_pr*K_solver(:,:,:,:,1)
    call FFt(temp1_solver, K_solver(:,:,:,:,2))
    K_solver(:,:,:,:,2) = - K_solver(:,:,:,:,2)

    
    ! k3
    temp1_solver = U + dt*0.5_pr*K_solver(:,:,:,:,2)
    call FFt(temp1_solver, K_solver(:,:,:,:,3))
    K_solver(:,:,:,:,3) = - K_solver(:,:,:,:,3)

    ! k4
    temp1_solver = U + dt*K_solver(:,:,:,:,3)
    call FFt(temp1_solver, K_solver(:,:,:,:,4))
    K_solver(:,:,:,:,4) = - K_solver(:,:,:,:,4);

    U = U + dt*(1.0_pr/6.0_pr*K_solver(:,:,:,:,1) &
         + 1.0_pr/3.0_pr*K_solver(:,:,:,:,2) &
         + 1.0_pr/3.0_pr*K_solver(:,:,:,:,3) &
         + 1.0_pr/6.0_pr*K_solver(:,:,:,:,4))
    

    ! filter
    call fftfwd_m(U, temp1_solver_cx, 3)
    call div_free_fourier(temp1_solver_cx);
    call dealiasing_cutoff_m(temp1_solver_cx, 3)
    !call dealiasing_fourier_m(temp1_solver_cx, 3)
    call fftbwd_m(temp1_solver_cx, U,3)
    
     
  END SUBROUTINE rk4_reverse

  !===================================================

  !===================================================
  !  SUBROUTINE: fwd_3D(inifield, mydt, savesign, stepper, myindex)
  !  inifield: the initial velocity
  !  stepper =
  !  1: leapfrog 
  !  2: SSPRK3
  !  3: RK4
  !  4: RK5
  !  5: RK45
  !=================================================== 
  
  SUBROUTINE fwd_3D(inifield,mydt,savesign,stepper,myindex)
  
    USE global_variables
    USE fftwfunction
    USE function_ops
    USE data_ops
    USE databinary_handle
    IMPLICIT NONE
    INCLUDE "mpif.h"
    REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(IN) :: inifield
    REAL(pr), INTENT(IN) :: mydt
    integer, INTENT(IN) :: savesign, stepper, myindex

    REAL(pr) ::  dt, time
    INTEGER :: Time_iter
    integer :: nn
    integer :: i1, i2, i3
    character(200) :: file_spectrum, file_energy, file_moments, file_plane
    character(4) :: indexchar
    Real(pr) :: cnst
    integer :: flag_rk45
    real(pr) :: dt_after
    real(pr), dimension(1:3) :: max_val
    real(pr), dimension(1:6) :: max_pos


    

    

    
    WRITE(indexchar, '(i4)') myindex

    ! store the initial condition
    Uvec         = inifield
    dt           = mydt   ! For simplicity, set dt fixed, not change adaptively       
    Time_iter    = iniIndex
    time         = iniTime
    
    flag_rk45 = 1

    
    IF (savesign == 1) THEN
       file_spectrum = TRIM(scratch_pathname)//trim(subpath)//"spectrum_fwd_"//trim(adjustl(indexchar))//".dat"
       file_energy = TRIM(scratch_pathname)//trim(subpath)//"energy_fwd_"//trim(adjustl(indexchar))//".dat"
      

       if (rank == 0) then
          
          open(10, file = file_spectrum, status = 'replace')
          close(10)
         
          open(21, file = file_energy, status = 'replace')
          close(21)

     
          
       end if
       

       
       
       ! temp1_solver_cx = HatU
       call fftfwd_m(Uvec, temp1_solver_cx, 3)
       call div_free_fourier(temp1_solver_cx)
       call dealiasing_cutoff_m(temp1_solver_cx, 3)
       !call dealiasing_fourier_m(temp1_solver_cx, 3)
       call fftbwd_m(temp1_solver_cx, Uvec, 3)

       call save2binary2(Uvec,Time_iter, "fwdTE", subpath)
  
       

       call save_velocity(Uvec, Time_iter, subpath)

!       if (parallel_data) then
!          call save_velocity_cx(temp1_solver_cx, myindex)
!       else
!          call save_velocity_cx(temp1_solver_cx, myindex)

!       end if
       

       call calculate_spectrum(temp1_solver_cx, spectral_data)

       
       
       call save_spectrum(spectral_data, file_spectrum)
       call L2_grad(temp1_solver_cx, H1_norm)

                     
        !h1 seminorm
         call abs_deriv_fourier(temp1_solver_cx, temp2_solver_cx, 1.0_pr)
         call L2_product_fourier(temp2_solver_cx, temp2_solver_cx, H1_seminorm)
         !h2 seminorm
         call abs_deriv_fourier(temp1_solver_cx, temp2_solver_cx, 2.0_pr)
         call L2_product_fourier(temp2_solver_cx, temp2_solver_cx, H2_seminorm)
         !h3 seminorm
         call abs_deriv_fourier(temp1_solver_cx, temp2_solver_cx, 3.0_pr)
         call L2_product_fourier(temp2_solver_cx, temp2_solver_cx, H3_seminorm) 


       ! temp2_solver_cx = HatW
       call vel2vort_fourier(temp1_solver_cx, temp2_solver_cx)
       ! temp2_solver_cx = W
       call fftbwd_m(temp2_solver_cx, Wvec, 3)


       
      
     
      
       call save_vorticity(Wvec, Time_iter, subpath) 
       call calculate_total_energy(Uvec, Wvec, K_total, E_total, H_total, maxW_global, E_component)
       call save_energy(K_total, E_total, H_total, maxW_global, H1_norm, E_component, time, file_energy, H1_seminorm, H2_seminorm, H3_seminorm)

       
    END IF

    

    ! evolve the equations
    DO WHILE ( time < endTime - 1.0e-12_pr)   ! 1.0e-12_pr is just an constant error
       ! time stepper       
       SELECT CASE ( stepper )
       CASE (1)
          CALL leapfrog(Umid_solver, Uvec, dt, Time_iter)
       CASE (2)
          CALL ssprk3(Uvec, dt)
       CASE (3)
          CALL rk4(Uvec, dt)
       CASE (4)
          CALL rk5(Uvec, dt)
       CASE (5)
          CALL rk45(Uvec, dt, dt_after, time, flag_rk45)
       END SELECT

       Time_iter       = Time_iter + 1
       time            = time + dt
       final_time_iter = Time_iter

       

       

       

       CALL MPI_BARRIER(MPI_COMM_WORLD,Statinfo)
       IF (savesign == 1 .and. MODULO(Time_iter,32)==0) THEN
           
          
          !temp1_solver_cx = HatU
          !temp1_solver = U
          if (stepper == 1 .and. mod(Time_iter, 2) == 1) then       
             temp1_solver = Umid_solver
          else
             temp1_solver = Uvec
          end if

          
          call save2binary2(Uvec,Time_iter, "fwdTE", subpath)
       
         
          
          call fftfwd_m(temp1_solver, temp1_solver_cx, 3)
          call calculate_spectrum(temp1_solver_cx, spectral_data)
          call save_spectrum(spectral_data, file_spectrum)

          call L2_grad(temp1_solver_cx, H1_norm)

          !h1 seminorm
         call abs_deriv_fourier(temp1_solver_cx, temp2_solver_cx, 1.0_pr)
         call L2_product_fourier(temp2_solver_cx, temp2_solver_cx, H1_seminorm)
         !h2 seminorm
         call abs_deriv_fourier(temp1_solver_cx, temp2_solver_cx, 2.0_pr)
         call L2_product_fourier(temp2_solver_cx, temp2_solver_cx, H2_seminorm)
         !h3 seminorm
         call abs_deriv_fourier(temp1_solver_cx, temp2_solver_cx, 3.0_pr)
         call L2_product_fourier(temp2_solver_cx, temp2_solver_cx, H3_seminorm)

          ! temp2_solver_cx = HatW
          call vel2vort_fourier(temp1_solver_cx, temp2_solver_cx)
          ! temp2_solver_cx = W
          call fftbwd_m(temp2_solver_cx, Wvec, 3)

          !if (mod(Time_iter, 32) == 0) call save_velocity(Uvec, Time_iter)
          !if (mod(Time_iter, 32) == 0) call save_vorticity(Wvec, Time_iter)


          call calculate_total_energy(Uvec, Wvec, K_total, E_total, H_total, maxW_global, E_component)
          call save_energy(K_total, E_total, H_total, maxW_global, H1_norm, E_component, time, file_energy, H1_seminorm, H2_seminorm, H3_seminorm)

    
              
       END IF

       if (stepper == 5) dt = dt_after
    END DO
    

    if (stepper ==1 .and. mod(final_time_iter,2) == 1) Uvec = Umid_solver
       
    IF (savesign == 1) THEN
       IF (rank == 0) THEN
          print *, "solvers; time =", time
       END IF
       !call save_NS_velocity(Uvec, Time_iter, ".nc", myindex)
       !call fftfwd_m(Uvec, temp1_solver_cx, 3)
       !call save_velocity_cx(temp1_solver_cx, Time_iter)
    END IF
    CALL MPI_BCAST (final_time_iter, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)  

  END SUBROUTINE fwd_3D


  !===================================================
  ! SUBROUTINE: reverse_test(inifield,mydt,savesign,stepper,myindex)
  ! test the backward solver
  !===================================================

  SUBROUTINE reverse_test(inifield,mydt,stepper,myindex)
  
    USE global_variables
    USE fftwfunction
    USE function_ops
    USE data_ops
    USE databinary_handle
    IMPLICIT NONE
    INCLUDE "mpif.h"
    REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(IN) :: inifield
    REAL(pr), INTENT(IN) :: mydt
    integer, INTENT(IN) :: stepper, myindex

    REAL(pr) ::  time
    integer :: nn
    integer :: i1, i2, i3
    character(200) :: file_error
    character(4) :: indexchar
    Real(pr) :: val1, val2
    real(pr) :: dt_after
    integer :: fwdindex, reverse_iter

    WRITE(indexchar, '(i4)') myindex
    
    call fwd_3D(inifield, mydt, 1, stepper, myindex)
    


    file_error = TRIM(scratch_pathname)//TRIM(subpath)//"reverse_error"//trim(adjustl(indexchar))//".dat"

    if (rank == 0) then
       open(17, file = file_error, status = 'replace')
       close(17)
    end if

    reverse_iter = 0
   
    fwdindex = final_time_iter - reverse_iter
    
    adj_Uvec = Uvec
    call read4binary2(fwdindex, fwd_field1, "fwdTE", subpath)
    temp1_solver = adj_Uvec - fwd_field1
    call L2_product(temp1_solver, temp1_solver, val1)
    call L2_product(fwd_field1, fwd_field1, val2)
    val2 = val1/val2
    time = fwdindex*mydt
       
    if (rank == 0) then
       open(17, file = file_error, status = 'old', position = 'append')
       write(17, "(2 G20.12)") time, val2
       close(17)
    end if
    
    
    do while (reverse_iter < final_time_iter)       
       
       call rk4_reverse(adj_Uvec, mydt)

       reverse_iter =reverse_iter + 1
       fwdindex = final_time_iter - reverse_iter
       CALL MPI_BARRIER(MPI_COMM_WORLD,Statinfo)
       call read4binary2(fwdindex, fwd_field1, "fwdTE", subpath)
       temp1_solver = adj_Uvec - fwd_field1
       call L2_product(temp1_solver, temp1_solver, val1)
       call L2_product(fwd_field1, fwd_field1, val2)
       val2 = val1/val2
       time = fwdindex*mydt
       
       if (rank == 0) then
          open(17, file = file_error, status = 'old', position = 'append')
          write(17, "(2 G20.12)") time, val2
          close(17)
       end if
    end do
    

 

  END SUBROUTINE reverse_test


  
  !===================================================
  ! SUBROUTINE: FFt_bkd(U, U_fwd, Ft)
  ! ff_temp1, temp1_solver_cx, temp2_solver_cx
  ! solve the adjoint equation backward
  !===================================================
  SUBROUTINE FFt_bk(U, U_fwd, Ft)
    use global_variables
    use function_ops
    use fftwfunction
    implicit none
    real(pr), dimension(1:n(1),1:n(2),1:local_N,1:3), intent(in) :: U
    real(pr), dimension(1:n(1),1:n(2),1:local_N,1:3), intent(in) :: U_fwd
    real(pr), dimension(1:n(1),1:n(2),1:local_N,1:3), intent(out) :: Ft

    Ft(:,:,:,:) = 0.0_pr
    ! temp1_solver_cx = Hat(U)
    call fftfwd_m(U, temp1_solver_cx, 3)
    call G_alpha_fourier(temp1_solver_cx, temp1_solver_cx, alpha)

    ! temp2_solver_cx = Hat(Gradient U1)
    call derivative_fourier(temp1_solver_cx(:,:,:,1), temp2_solver_cx(:,:,:,1), 1)
    call derivative_fourier(temp1_solver_cx(:,:,:,1), temp2_solver_cx(:,:,:,2), 2)
    call derivative_fourier(temp1_solver_cx(:,:,:,1), temp2_solver_cx(:,:,:,3), 3)
    ! ff_temp1 = Gradient U1
    call fftbwd_m(temp2_solver_cx, ff_temp1, 3)  
    Ft(:,:,:,1) = Ft(:,:,:,1) + 2.0_pr*ff_temp1(:,:,:,1)*U_fwd(:,:,:,1) &
         + ff_temp1(:,:,:,2)*U_fwd(:,:,:,2) + ff_temp1(:,:,:,3)*U_fwd(:,:,:,3)
    Ft(:,:,:,2) = Ft(:,:,:,2) + ff_temp1(:,:,:,2)*U_fwd(:,:,:,1)
    Ft(:,:,:,3) = Ft(:,:,:,3) + ff_temp1(:,:,:,3)*U_fwd(:,:,:,1)

    ! temp2_solver_cx = Hat(Gradient U2)
    call derivative_fourier(temp1_solver_cx(:,:,:,2), temp2_solver_cx(:,:,:,1), 1)
    call derivative_fourier(temp1_solver_cx(:,:,:,2), temp2_solver_cx(:,:,:,2), 2)
    call derivative_fourier(temp1_solver_cx(:,:,:,2), temp2_solver_cx(:,:,:,3), 3)
    ! ff_temp1 = Gradient U2
    call fftbwd_m(temp2_solver_cx, ff_temp1, 3)
    Ft(:,:,:,1) = Ft(:,:,:,1) + ff_temp1(:,:,:,1)*U_fwd(:,:,:,2)
    Ft(:,:,:,2) = Ft(:,:,:,2) + 2.0_pr*ff_temp1(:,:,:,2)*U_fwd(:,:,:,2) &
         + ff_temp1(:,:,:,1)*U_fwd(:,:,:,1) + ff_temp1(:,:,:,3)*U_fwd(:,:,:,3)
    Ft(:,:,:,3) = Ft(:,:,:,3) + ff_temp1(:,:,:,3)*U_fwd(:,:,:,2)

    ! temp2_solver_cx = Hat(Gradient U3)
    call derivative_fourier(temp1_solver_cx(:,:,:,3), temp2_solver_cx(:,:,:,1), 1)
    call derivative_fourier(temp1_solver_cx(:,:,:,3), temp2_solver_cx(:,:,:,2), 2)
    call derivative_fourier(temp1_solver_cx(:,:,:,3), temp2_solver_cx(:,:,:,3), 3)
    ! ff_temp1 = Gradient U3
    call fftbwd_m(temp2_solver_cx, ff_temp1, 3)
    Ft(:,:,:,1) = Ft(:,:,:,1) + ff_temp1(:,:,:,1)*U_fwd(:,:,:,3)
    Ft(:,:,:,2) = Ft(:,:,:,2) + ff_temp1(:,:,:,2)*U_fwd(:,:,:,3)
    Ft(:,:,:,3) = Ft(:,:,:,3) + 2.0_pr*ff_temp1(:,:,:,3)*U_fwd(:,:,:,3) &
         + ff_temp1(:,:,:,1)*U_fwd(:,:,:,1) + ff_temp1(:,:,:,2)*U_fwd(:,:,:,2)

    call fftfwd_m(Ft, temp2_solver_cx, 3)

    call div_free_fourier(temp2_solver_cx)
    call fftbwd_m(temp2_solver_cx, Ft, 3)
    
    ! if needed to compute laplacian U
    ! temp1_solver_cx = HatU
    if (0) then
       call laplacian_fourier(temp1_solver_cx, temp1_solver_cx)
       call div_free_fourier(temp1_solver_cx)
       call fftbwd_m(temp1_solver_cx, ff_temp1, 3)
       Ft = Ft + visc*ff_temp1
    end if
  END SUBROUTINE FFt_bk
  !===================================================

  !===================================================
  ! SUBROUTINE: inter_Hermite(Ul,Ur,t, dt, U_inter)
  ! compute the Hermite cubic interpolation given Ul and Ur
  ! the interval is [0,dt]
  ! Use temp1_solver
  !===================================================
  SUBROUTINE inter_hermite(Ul, Ur, t, dt, U_inter)
    use global_variables
    use function_ops
    use fftwfunction
    implicit none
    real(pr), dimension(1:n(1),1:n(2),1:local_N,1:3), intent(in) :: Ul
    real(pr), dimension(1:n(1),1:n(2),1:local_N,1:3), intent(in) :: Ur
    real(pr), dimension(1:n(1),1:n(2),1:local_N,1:3), intent(out) :: U_inter
    real(pr), intent(in) :: t, dt
    real(pr) :: tl, tr
    real(pr), dimension(1:4) :: h

    tl = 0.0_pr
    tr = dt

    h(1) = (dt + 2.0_pr*(t-tl))*(tr-t)**2/dt**3 !h00
    h(2) = (dt + 2.0_pr*(tr-t))*(t-tl)**2/dt**3 !h10
    h(3) = (t-tl)*(tr-t)**2/dt**2 ! h01
    h(4) = (tr-t)*(t-tl)**2/dt**2 ! h11

    U_inter = h(1)*Ul + h(2)*Ur
    call FFt(Ul,temp1_solver)
    U_inter = U_inter + h(3)*temp1_solver
    call FFt(Ur, temp1_solver)
    U_inter = U_inter + h(4)*temp1_solver

    
    
   
  END SUBROUTINE inter_hermite
  !===================================================

  !========================================================= 
  ! SUBROUTINE: leapfrog_bkd(Umid, U, U_fwdl, U_fwdr, dt, iter_count)
  !
  ! stepper=1 leapfrog
  !=========================================================
  SUBROUTINE leapfrog_bk(Umid, U, U_fwdl, U_fwdr, dt, iter_count)
    USE global_variables
    USE fftwfunction
    USE function_ops
    IMPLICIT NONE
    INCLUDE "mpif.h"
    REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(INOUT) :: U
    REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(INOUT) :: Umid
    real(pr), dimension(1:n(1), 1:n(2), 1:local_N, 1:3), intent(in) :: U_fwdl
    real(pr), dimension(1:n(1), 1:n(2), 1:local_N, 1:3), intent(in) :: U_fwdr
    REAL(pr), INTENT(IN) :: dt
    integer, intent(in) :: iter_count
    integer :: iii1, iii2, iii3

    ! initialize Umid
    if (iter_count == 0) then
       
       ! Compute Umid
       ! k1 = Ft(U^n)
       call FFt_bk(U, U_fwdl, K_solver(:,:,:,:,1))

    
       ! k2 = Ft(U^n + 0.5dt*k1)      
       call inter_hermite(U_fwdl, U_fwdr, 0.5_pr*dt, dt, temp2_solver)
       temp1_solver = U + 0.5_pr*dt*K_solver(:,:,:,:,1)
       call FFt_bk(temp1_solver, temp2_solver, K_solver(:,:,:,:,1))

       Umid = U + dt*K_solver(:,:,:,:,1)

   
       call fftfwd_m(Umid, temp1_solver_cx, 3)
    
       
       call div_free_fourier(temp1_solver_cx);
       !call dealiasing_fourier_m(temp1_solver_cx, 3)
       call dealiasing_cutoff_m(temp1_solver_cx, 3)
    
       call fftbwd_m(temp1_solver_cx, Umid, 3)
    else if (mod(iter_count+1, 2) == 0) then
       ! U = U + 2*dt*F(Umid)
       call FFt_bk(Umid, U_fwdl, K_solver(:,:,:,:,1))
       U = U + 2.0_pr*dt*K_solver(:,:,:,:,1)

       call fftfwd_m(U, temp1_solver_cx, 3)
    
       
       call div_free_fourier(temp1_solver_cx);
   
       !call dealiasing_fourier_m(temp1_solver_cx, 3)
       call dealiasing_cutoff_m(temp1_solver_cx, 3)
    
       call fftbwd_m(temp1_solver_cx,U,3)
    else if (mod(iter_count+1, 2) == 1) then
       ! Umid = Umid + 2*dt*F(U)
       call FFt_bk(U, U_fwdl, K_solver(:,:,:,:,1))
       Umid = Umid + 2.0_pr*dt*K_solver(:,:,:,:,1)
       call fftfwd_m(Umid, temp1_solver_cx, 3)
    
       
       
       call div_free_fourier(temp1_solver_cx);
       !call dealiasing_fourier_m(temp1_solver_cx, 3)
       call dealiasing_cutoff_m(temp1_solver_cx, 3)
    
    
       call fftbwd_m(temp1_solver_cx, Umid,3)
    end if
       
  END SUBROUTINE leapfrog_bk

  !===================================================

  
  !========================================================= 
  ! SUBROUTINE: SSPRK3_bk(U, U_fwdl, U_fwdr, dt)
  !
  ! stepper=2 ssprk3
  !=========================================================
  SUBROUTINE ssprk3_bk(U, U_fwdl, U_fwdr, dt)
    USE global_variables
    USE fftwfunction
    USE function_ops
    IMPLICIT NONE
    INCLUDE "mpif.h"
    REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(INOUT) :: U
    real(pr), dimension(1:n(1), 1:n(2), 1:local_N, 1:3), intent(in) :: U_fwdl
    real(pr), dimension(1:n(1), 1:n(2), 1:local_N, 1:3), intent(in) :: U_fwdr
    REAL(pr), INTENT(IN) :: dt
    

    ! k1
    call FFt_bk(U, U_fwdl, K_solver(:,:,:,:,1))

    ! k2
    temp1_solver = U + dt*K_solver(:,:,:,:,1)
    call FFt_bk(temp1_solver, U_fwdr, K_solver(:,:,:,:,2))

    !k3
    call inter_hermite(U_fwdl, U_fwdr, 0.5_pr*dt, dt, temp2_solver)
    temp1_solver = U + dt*0.25_pr*(K_solver(:,:,:,:,1) + K_solver(:,:,:,:,2))
    call FFt_bk(temp1_solver, temp2_solver, K_solver(:,:,:,:,3))


    U = U + dt*(1.0_pr/6.0_pr*K_solver(:,:,:,:,1) + 1.0_pr/6.0_pr*K_solver(:,:,:,:,2) + 2.0_pr/3.0_pr*K_solver(:,:,:,:,3))

    ! filter
    call fftfwd_m(U, temp1_solver_cx, 3)
    call div_free_fourier(temp1_solver_cx);
    !call dealiasing_fourier_m(temp1_solver_cx, 3)
    call dealiasing_cutoff_m(temp1_solver_cx, 3)
    call fftbwd_m(temp1_solver_cx, U,3)


    
     
  END SUBROUTINE ssprk3_bk
  !===================================================
  
  !===================================================
  !  SUBROUTINE: rk4_bk(U, U_fwdl, U_fwdr, dt)
  !  stepper=3 RK4
  !=================================================== 
  SUBROUTINE rk4_bk(U, U_fwdl, U_fwdr, dt)
    USE global_variables
    USE fftwfunction
    USE function_ops
    IMPLICIT NONE
    INCLUDE "mpif.h"
    REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(INOUT) :: U
    real(pr), dimension(1:n(1), 1:n(2), 1:local_N, 1:3), intent(in) :: U_fwdl
    real(pr), dimension(1:n(1), 1:n(2), 1:local_N, 1:3), intent(in) :: U_fwdr
    REAL(pr), INTENT(IN) :: dt
    
    
    ! k1
    call FFt_bk(U,  U_fwdl, K_solver(:,:,:,:,1))
    
 
    ! k2
    call inter_hermite(U_fwdl, U_fwdr, 0.5_pr*dt, dt, temp2_solver)
    temp1_solver = U + dt*0.5_pr*K_solver(:,:,:,:,1)
    call FFt_bk(temp1_solver, temp2_solver, K_solver(:,:,:,:,2))

    
    ! k3
    temp1_solver = U + dt*0.5_pr*K_solver(:,:,:,:,2)
    call FFt_bk(temp1_solver, temp2_solver, K_solver(:,:,:,:,3))


    ! k4
    temp1_solver = U + dt*K_solver(:,:,:,:,3)
    call FFt_bk(temp1_solver, U_fwdr, K_solver(:,:,:,:,4))

    U = U + dt*(1.0_pr/6.0_pr*K_solver(:,:,:,:,1) &
         + 1.0_pr/3.0_pr*K_solver(:,:,:,:,2) &
         + 1.0_pr/3.0_pr*K_solver(:,:,:,:,3) &
         + 1.0_pr/6.0_pr*K_solver(:,:,:,:,4))
    

    ! filter
    call fftfwd_m(U, temp1_solver_cx, 3)
    call div_free_fourier(temp1_solver_cx);
    call dealiasing_cutoff_m(temp1_solver_cx, 3)
    !call dealiasing_fourier_m(temp1_solver_cx, 3)
    call fftbwd_m(temp1_solver_cx, U,3)
    
     
  END SUBROUTINE rk4_bk

  !===================================================

  !===================================================
  !  SUBROUTINE: rk5_bk(U, U_fwdl, U_fwdr, dt)
  !  stepper=4 RK5
  !=================================================== 
  SUBROUTINE rk5_bk(U, U_fwdl, U_fwdr, dt)
    USE global_variables
    USE fftwfunction
    USE function_ops
    IMPLICIT NONE
    INCLUDE "mpif.h"
    REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(INOUT) :: U
    real(pr), dimension(1:n(1), 1:n(2), 1:local_N, 1:3), intent(in) :: U_fwdl
    real(pr), dimension(1:n(1), 1:n(2), 1:local_N, 1:3), intent(in) :: U_fwdr
    REAL(pr), INTENT(IN) :: dt

    
  
    ! k1
    call FFt_bk(U, U_fwdl, K_solver(:,:,:,:,1))
   
    ! k2
    call inter_hermite(U_fwdl, U_fwdr, 1.0_pr/5.0_pr*dt, dt, temp2_solver)
    temp1_solver = U + dt*A_solver(1,1)*K_solver(:,:,:,:,1)
    call FFt_bk(temp1_solver, temp2_solver, K_solver(:,:,:,:,2))
        
    ! k3
    call inter_hermite(U_fwdl, U_fwdr, 3.0_pr/10.0_pr*dt, dt, temp2_solver)
    temp1_solver = U + dt*(A_solver(2,1)*K_solver(:,:,:,:,1) &
         + A_solver(2,2)*K_solver(:,:,:,:,2))
    call FFt_bk(temp1_solver, temp2_solver, K_solver(:,:,:,:,3))

    ! k4
    call inter_hermite(U_fwdl, U_fwdr, 4.0_pr/5.0_pr*dt, dt, temp2_solver)
    temp1_solver = U + dt*(A_solver(3,1)*K_solver(:,:,:,:,1) &
         + A_solver(3,2)*K_solver(:,:,:,:,2) + A_solver(3,3)*K_solver(:,:,:,:,3))
    call FFt_bk(temp1_solver, temp2_solver, K_solver(:,:,:,:,4))

    ! k5
    call inter_hermite(U_fwdl, U_fwdr, 8.0_pr/9.0_pr*dt, dt, temp2_solver)
    temp1_solver = U + dt*(A_solver(4,1)*K_solver(:,:,:,:,1) &
         + A_solver(4,2)*K_solver(:,:,:,:,2) + A_solver(4,3)*K_solver(:,:,:,:,3) &
         + A_solver(4,4)*K_solver(:,:,:,:,4))
    call FFt_bk(temp1_solver, temp2_solver, K_solver(:,:,:,:,5))

    ! k6
    temp1_solver = U + dt*(A_solver(5,1)*K_solver(:,:,:,:,1) &
         + A_solver(5,2)*K_solver(:,:,:,:,2) &
         + A_solver(5,3)*K_solver(:,:,:,:,3) &
         + A_solver(5,4)*K_solver(:,:,:,:,4) &
         + A_solver(5,5)*K_solver(:,:,:,:,5))
    
    call FFt_bk(temp1_solver, U_fwdr, K_solver(:,:,:,:,6))

    U = U + dt*(b_solver(1)*K_solver(:,:,:,:,1) &
         + b_solver(2)*K_solver(:,:,:,:,2) &
         + b_solver(3)*K_solver(:,:,:,:,3) &
         + b_solver(4)*K_solver(:,:,:,:,4) &
         + b_solver(5)*K_solver(:,:,:,:,5) &
         + b_solver(6)*K_solver(:,:,:,:,6))


    ! filter
    call fftfwd_m(U, temp1_solver_cx, 3)
    call div_free_fourier(temp1_solver_cx);
    call dealiasing_cutoff_m(temp1_solver_cx, 3)
    !call dealiasing_fourier_m(temp1_solver_cx, 3)
    call fftbwd_m(temp1_solver_cx, U,3)

    
    
     
  END SUBROUTINE rk5_bk

  !===================================================

  !===================================================
  !  SUBROUTINE: adj_initialize(inifield, fwdindex)
  !  USE: temp1_solver_cx
  !=================================================== 

  SUBROUTINE adj_initialize(inifield, fwdindex)
  
    USE global_variables
    USE fftwfunction
    USE function_ops
    USE data_ops
    USE databinary_handle
    IMPLICIT NONE
    INCLUDE "mpif.h"
    REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(OUT) :: inifield
    integer, INTENT(IN) :: fwdindex
    real(pr) :: max_local, max_global
    real(pr) :: val
    integer :: i1, i2, i3, nn

    ! u_star(T) = 2|D|^{-2} U(T)
    inifield = Uvec
    !call read4binary2(fwdindex, inifield, "fwdTE")
    call fftfwd_m(inifield, temp1_solver_cx, 3)
    call abs_deriv_fourier(temp1_solver_cx, temp1_solver_cx, 2.0_pr)
    call div_free_fourier(temp1_solver_cx)
    temp1_solver_cx = 2.0_pr*temp1_solver_cx

 
    !call div_free_fourier(temp1_solver_cx)
    !call dealiasing_fourier_m(temp1_solver_cx, 3)
    call dealiasing_cutoff_m(temp1_solver_cx, 3)
    
    call fftbwd_m(temp1_solver_cx, inifield,3)
    
    END SUBROUTINE adj_initialize


    
  !===================================================
  !  SUBROUTINE: bkd_3D(inifield, mydt, savesign, stepper, myindex)
  !  inifield: the initial velocity
  !  stepper =
  !  1: leapfrog 
  !  2: SSPRK3
  !  3: RK4
  !  4: RK5
  !=================================================== 
  
  SUBROUTINE bkd_3D(inifield,mydt,savesign,stepper,myindex)
  
    USE global_variables
    USE fftwfunction
    USE function_ops
    USE data_ops
    USE databinary_handle
    IMPLICIT NONE
    INCLUDE "mpif.h"
    REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(INOUT) :: inifield
    REAL(pr), INTENT(IN) :: mydt
    integer, INTENT(IN) :: savesign, stepper, myindex

    REAL(pr) ::  dt, time
    INTEGER :: adj_Time_iter, fwdindex
    integer :: nn
    integer :: i1, i2, i3
    character(200) :: file_spectrum, file_energy
    character(4) :: indexchar
    Real(pr) :: cnst
       
    WRITE(indexchar, '(i4)') myindex

    
    dt           = mydt   ! For simplicity, set dt fixed, not change adaptively       
    adj_Time_iter = iniIndex
    fwdindex      = final_time_iter - adj_Time_iter
    time         =  iniTime

    call adj_initialize(inifield, fwdindex)
    
    adj_Uvec = inifield
    
    IF (savesign == 1) THEN
       file_spectrum = TRIM(scratch_pathname)//TRIM(subpath)//"spectrum_bkd_"//trim(adjustl(indexchar))//".dat"
       file_energy = TRIM(scratch_pathname)//TRIM(subpath)//"energy_bkd_"//trim(adjustl(indexchar))//".dat"
    

       if (rank == 0) then
          
          open(10, file = file_spectrum, status = 'replace')
          close(10)
          
          open(21, file = file_energy, status = 'replace')
          close(21)
       end if
       
       

       
       !CALL vel2vort(Uvec, Wvec)
       !CALL diagnostics_nse(time, dt, Uvec, Wvec, 1, Time_iter, "fwdTE", myindex)
       
       ! temp1_solver_cx = HatU
       call fftfwd_m(adj_Uvec, temp1_solver_cx, 3)
       call calculate_spectrum(temp1_solver_cx, spectral_data)
       
       
       call save_spectrum(spectral_data, file_spectrum)

       call L2_grad(temp1_solver_cx, H1_norm)

       ! temp2_solver_cx = HatW
       call vel2vort_fourier(temp1_solver_cx, temp2_solver_cx)
       ! temp2_solver_cx = W
       call fftbwd_m(temp2_solver_cx, Wvec, 3)
    

       call calculate_total_energy(adj_Uvec, Wvec, K_total, E_total, H_total, maxW_global, E_component)
       call save_energy(K_total, E_total, H_total, maxW_global, H1_norm, E_component, time, file_energy, H1_norm, H1_norm, H1_norm)
       
       
    END IF

    

    fwd_Field1 = Uvec
    fwd_Field2 = Uvec
    ! evolve the equations
    DO WHILE ( adj_Time_iter < final_time_iter)
       
       
       if (parallel_data) then
          fwdindex      = final_time_iter - adj_Time_iter
          fwd_Field1 = fwd_Field2
          call read4binary2(fwdindex, fwd_Field1, "fwdTE", subpath)
          call read4binary2(fwdindex-1, fwd_Field2, "fwdTE", subpath)

          ! evolve the euler equation backwards
          !fwd_Field1 = fwd_Field2
          !call rk4_reverse(fwd_Field2, dt)
          
          

        
       else
          fwdindex      = final_time_iter - adj_Time_iter
          call read4binary2(fwdindex, fwd_Field1, "fwdTE", subpath)
          call read4binary2(fwdindex-1, fwd_Field2, "fwdTE", subpath)
       end if
       
       ! time stepper       
       SELECT CASE ( stepper )
       CASE (1)
          CALL leapfrog_bk(Umid_solver, adj_Uvec, fwd_Field1, fwd_Field2, dt, adj_Time_iter)
       CASE (2)
          CALL ssprk3_bk(adj_Uvec, fwd_Field1, fwd_Field2, dt)
       CASE (3)
          CALL rk4_bk(adj_Uvec, fwd_Field1, fwd_Field2, dt)
       CASE (4)
          CALL rk5_bk(adj_Uvec, fwd_Field1, fwd_Field2, dt)
       END SELECT

       adj_Time_iter       = adj_Time_iter + 1
       time            = time + dt

       

       

       

       CALL MPI_BARRIER(MPI_COMM_WORLD,Statinfo)
       IF (savesign == 1) THEN
           !if (mod(Time_iter, 100) == 0) call save_NS_velocity(Uvec, Time_iter, ".nc", Time_iter)
          !CALL vel2vort(Uvec, Wvec)
          !CALL diagnostics_nse(time, dt, Uvec, Wvec, 1, Time_iter, "fwdTE", myindex)

          
          !temp1_solver_cx = HatU
          !temp1_solver = U
          if (stepper == 1 .and. mod(adj_Time_iter, 2) == 1) then       
             temp1_solver = Umid_solver
          else
             temp1_solver = adj_Uvec
          end if
       
       
          
          call fftfwd_m(adj_Uvec, temp1_solver_cx, 3)
 
          call calculate_spectrum(temp1_solver_cx, spectral_data)
          call save_spectrum(spectral_data, file_spectrum)

         call L2_grad(temp1_solver_cx, H1_norm)
         

          ! temp2_solver_cx = HatW
          call vel2vort_fourier(temp1_solver_cx, temp2_solver_cx)
          ! temp2_solver_cx = W
          call fftbwd_m(temp2_solver_cx, Wvec, 3)
   

     

           call calculate_total_energy(adj_Uvec, Wvec, K_total, E_total, H_total, maxW_global, E_component)
       call save_energy(K_total, E_total, H_total, maxW_global, H1_norm, E_component, time, file_energy, H1_norm, H1_norm, H1_norm)
       
          
              
       END IF

    END DO
    

    if (stepper ==1 .and. mod(final_time_iter,2) == 1) adj_Uvec = Umid_solver
       
    IF (savesign == 1) THEN
       IF (rank == 0) THEN
          print *, "adj_solvers; time =", time
       END IF
       !call save_NS_velocity_bk(adj_Uvec, adj_Time_iter, ".nc", adj_Time_iter)
    END IF


  END SUBROUTINE bkd_3D

  !===================================================
  !  SUBROUTINE: kappa_test(inifield, direction, epsilon, mydt, stepper, myindex)
  !  inifield: the initial velocity
  !  stepper =
  !  1: leapfrog 
  !  2: SSPRK3
  !  3: RK4
  !  4: RK5
  !=================================================== 
  
  SUBROUTINE kappa_test(inifield,direction,mydt,savesign,stepper,myindex)
  
    USE global_variables
    USE fftwfunction
    USE function_ops
    USE data_ops
    USE databinary_handle
    IMPLICIT NONE
    INCLUDE "mpif.h"
    REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(INOUT) :: inifield
    REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(INOUT) :: direction
    REAL(pr), INTENT(IN) :: mydt
    integer, INTENT(IN) :: savesign, stepper, myindex

    character(4) :: indexchar
    character(200) :: file_error
    real(pr) :: val1, val2, val3, val
    real(pr) :: epsilon
    real(pr) :: norm
    integer :: i

    WRITE(indexchar, '(i4)') myindex
    
    file_error = TRIM(scratch_pathname)//TRIM(subpath)//"kappa_test_error"//trim(adjustl(indexchar))//".dat"

    
    call set_initial(inifield,2,5346689, 123455, 662341)
    call set_initial(direction, 6, 123345, 5566478, 662345)

    ! rescale the initial condition 
    call fftfwd_m(inifield, temp1_solver_cx, 3)
    !call abs_deriv_fourier(temp1_solver_cx, temp1_solver_cx, 3.0_pr)
    call L2_product_fourier(temp1_solver_cx, temp1_solver_cx, norm)
    inifield = inifield/sqrt(norm)

    call fftfwd_m(direction, temp1_solver_cx, 3)
    !call abs_deriv_fourier(temp1_solver_cx, temp1_solver_cx, 3.0_pr)
    call L2_product_fourier(temp1_solver_cx, temp1_solver_cx, norm)
    direction = direction/sqrt(norm)
    
    
    
    call fwd_3D(inifield, mydt, savesign, stepper,myindex)
    call fftfwd_m(Uvec, temp1_solver_cx, 3)
    call abs_deriv_fourier(temp1_solver_cx, temp1_solver_cx, 1.0_pr)
    call L2_product_fourier(temp1_solver_cx, temp1_solver_cx, val)

    
    call bkd_3D(adj_Uvec0, mydt, savesign, stepper, myindex)
    call L2_product(adj_Uvec, direction, val2)
    !call L2_product(inifield, direction, val2)
    
    do i = 0,9
       epsilon = 10.0_pr**(-i)
       call set_initial(inifield,2,5346689, 123455, 662341)
       call set_initial(direction, 6, 123345, 5566478, 662345)
       
       ! rescale the initial condition 
       call fftfwd_m(inifield, temp1_solver_cx, 3)
       !call abs_deriv_fourier(temp1_solver_cx, temp1_solver_cx, 3.0_pr)
       call L2_product_fourier(temp1_solver_cx, temp1_solver_cx, norm)
       inifield = inifield/sqrt(norm)

       call fftfwd_m(direction, temp1_solver_cx, 3)
       !call abs_deriv_fourier(temp1_solver_cx, temp1_solver_cx, 3.0_pr)
       call L2_product_fourier(temp1_solver_cx, temp1_solver_cx, norm)
       direction = direction/sqrt(norm)
       
       inifield = inifield + epsilon*direction
       
       
       call fwd_3D(inifield, mydt, savesign, stepper, myindex)
       call fftfwd_m(Uvec, temp1_solver_cx, 3)
       call abs_deriv_fourier(temp1_solver_cx, temp1_solver_cx, 1.0_pr)
       call L2_product_fourier(temp1_solver_cx, temp1_solver_cx, val1)
       
       

       if(rank == 0) then
          val3 = (val1 - val)/epsilon
          open(30, file = file_error, status = 'old', position = 'append')
          write(30, "(6 G20.12)") epsilon, val1, val, val3, val2, val3/val2
          close(30)
       end if
    end do

 
    
    

       

  END SUBROUTINE kappa_test

  

  !========================================================= 
  ! SUBROUTINE: SAVE_SPECTRUM
  ! input: spectral_data(1:kkmax, 1:2)
  ! input: filename
  !=========================================================
  SUBROUTINE save_spectrum(spectral_data, filename)
    USE global_variables
    IMPLICIT NONE  
    real(pr), dimension(1:kkmax, 1:2), intent(in) :: spectral_data
    character(len = *), intent(in) :: filename
    integer :: i
    if (rank == 0) then 
       open(10, file = filename, status = 'old', position = 'append')
       do i = 1, kkmax
          write(10, "(2 G20.12)") spectral_data(i,1), spectral_data(i,2)
       end do
       close(10)
    end if

    
  END SUBROUTINE save_spectrum
  !========================================================

  !========================================================= 
  ! SUBROUTINE: SAVE_ENERGY(K_total_, E_total_, H_total_, maxW_global_, filename)
  ! input: K_total_, E_total_, H_total_, maxW_global_, filename
  !=========================================================
  SUBROUTINE save_energy(K_total_, E_total_, H_total_, maxW_global_, H1_norm_, E_component_, time_, filename, H1_seminorm, H2_seminorm, H3_seminorm)
    USE global_variables
    IMPLICIT NONE  
    real(pr), intent(in) :: K_total_, E_total_, H_total_, maxW_global_, H1_norm_, time_, H1_seminorm, H2_seminorm, H3_seminorm
    real(pr), dimension(1:3), intent(in) :: E_component_
    character(len = *), intent(in) :: filename
    if (rank == 0) then
       open(21, file = filename, status = 'old', position = 'append')    
       write(21, "(12 G20.12)") time_, K_total_, E_total_, H_total_, maxW_global_, H1_norm_, E_component_(1), E_component_(2), E_component_(3), H1_seminorm, H2_seminorm, H3_seminorm
       close(21)
    end if
    
  END SUBROUTINE save_energy
  !==========================================================


  

END MODULE

