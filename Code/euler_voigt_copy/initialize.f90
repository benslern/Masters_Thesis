SUBROUTINE initialize
   use, intrinsic :: iso_c_binding     ! Newly Added July 14, 2017
   USE global_variables
   IMPLICIT NONE
   INCLUDE "mpif.h"
   INCLUDE "fftw3-mpi.f03"             ! Needed, as there is a fftw_mpi_local_size_3d command below

   INTEGER :: i,j,k
   !real(pr) :: mode ! Unused parameter
         
   ! amount of memory to allocate
   C_local_alloc = fftw_mpi_local_size_3d(C_n(3), C_n(2), C_n(1)/2+1, MPI_COMM_WORLD, C_local_N, C_local_k_offset)   ! Newly Added July 14, 2017
   local_N = int( C_local_N ) !Size of ranks splice
   local_k_offset = int( C_local_k_offset ) ! splice offset
   total_local_size = n(1)*n(2)*local_N ! total size of splice

   ALLOCATE( Uvec(1:n(1),1:n(2),1:local_N,1:3) ) ! local velocity vector ux,uy,uz
   ALLOCATE( Wvec(1:n(1),1:n(2),1:local_N,1:3) ) ! local vorticity vector

   ALLOCATE( fwd_Field1(1:n(1),1:n(2),1:local_N,1:3) )   ! fwd_Field, adj_Uvec, adj_Wvec are Newly added for adjoint problem
   ALLOCATE( fwd_Field2(1:n(1),1:n(2),1:local_N,1:3) ) 
   ALLOCATE( adj_Uvec(1:n(1),1:n(2),1:local_N,1:3) )
   !ALLOCATE( adj_Wvec(1:n(1),1:n(2),1:local_N,1:3) )
   ALLOCATE( Uvec0(1:n(1),1:n(2),1:local_N,1:3) )
   ALLOCATE( adj_Uvec0(1:n(1),1:n(2),1:local_N,1:3) )
   ALLOCATE( adj_Uvec0_direction(1:n(1),1:n(2),1:local_N,1:3) )

 
  ALLOCATE ( K1(1:n(1)) ) ! Fourier wave numbers
  ALLOCATE ( K2(1:n(2)) )
  ALLOCATE ( K3(1:n(3)) )
  
  ALLOCATE ( K1_filter(1:n(1)) ) ! added by xy
  ALLOCATE ( K2_filter(1:n(2)) ) ! added by xy
  ALLOCATE ( K3_filter(1:n(3)) ) ! added by xy
  

  if (rank == 0) THEN
     allocate(global_u(1:n(1), 1:n(2), 1:n(3)))
  end if
  
  
  PI = 4.0_pr*ATAN2(1.0_pr,1.0_pr)
  n_dim = 3*n(1)*n(2)*local_N
  dV = 1.0_pr/PRODUCT(REAL(n,pr))

!  Kcut = 4.0_pr*PI*REAL(n(1),pr)/5.0_pr

  Kcut = 2.0_pr*PI*10_pr*REAL(n(1),pr)/21.0_pr

  Kmax = Kcut/2.0_pr

  !--Set up wavenumbers
  DO i = 0, n(1)-1
    IF (i<=n(1)/2) THEN
       K1(i+1) = 2.0_pr*PI*REAL(i,pr)
    ELSE
       K1(i+1) = 2.0_pr*PI*REAL(i-n(1),pr)
    END IF
    K1_filter(i+1) = exp(-36_pr*(K1(i+1)/real(n(1), pr)/PI)**36_pr)
  END DO

  DO i = 0,n(2)-1
    IF (i <= n(2)/2) THEN
      K2(i+1) = 2.0_pr*PI*REAL(i,pr)
    ELSE
      K2(i+1) = 2.0_pr*PI*REAL(i-n(2),pr)
   END IF
   K2_filter(i+1) = exp(-36_pr*(K2(i+1)/real(n(2), pr)/PI)**36_pr)
  END DO

  DO i = 0, n(3)-1
    IF (i<=n(3)/2) THEN
       K3(i+1) = 2.0_pr*PI*REAL(i,pr)
    ELSE
       K3(i+1) = 2.0_pr*PI*REAL(i-n(3),pr)
    END IF
    K3_filter(i+1) = exp(-36_pr*(K3(i+1)/real(n(3), pr)/PI)**36_pr)
  END DO

!  IF (rank==0) THEN
!     OPEN(10, FILE="./LOGFILES/test", STATUS="REPLACE")
!     do k = 1,n(3)
!        do j = 1,n(2)
!           do i = 1,n(1)
!              write(10, *) K1(i), K2(j), K3(k), K1_filter(i), K2_filter(j), K3_filter(k), K1_filter(i)*K2_filter(j)*K3_filter(k)
!           end do
!        end do
!     end do
     
!     close(10)
!  END IF

  kkmax = ceiling(sqrt(real(n(1),pr)**2/4_pr+real(n(2),pr)**2/4_pr+real(n(3),pr)**2/4_pr)) 

  ! Unused Flags
  !kappaTest = .FALSE.
  !toDealias = .TRUE.
  !timing = .FALSE.
  !save_diag_NS = .TRUE.
  !save_data_NS =.TRUE.
  !calc_geom_NS = .FALSE.
  !calc_ExactSol = .FALSE.
  !save_diag_lineMin = .TRUE.
  !save_data_lineMin = .TRUE.
  !save_diag_Constr = .TRUE.
  !save_data_Constr = .TRUE.
  !save_diag_Optim = .TRUE.
  !save_data_Optim = .TRUE.
  !save_null_vortex = .False.

  IF (n(1)<64) THEN
     parallel_data = .FALSE.
  ELSE
     parallel_data = .TRUE.
  END IF
 
END SUBROUTINE
