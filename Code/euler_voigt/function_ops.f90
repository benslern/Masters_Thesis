!=======================================================
! MODULE CONTAINS ROUTINES REPRESENTING OPERATIONS 
! APPLIED TO FUNCTIONS.
!
! (*) function_ops_allocate
! (*) function_ops_deallocate
! (*) derivative_fourier
! (*) G_sigma_fourier
! (*) laplacian_fourier
! (*) abs_deriv_fourier
! (*) vel2vort_fourier
! (*) vort2vel_fourier
! (*) L2_product
! (*) calculate_total_energy
! (*) calculate_spectrum
! (*) div_fourier
 

! (*) vel2vort
! (*) Energy
! (*) Enstrophy
! (*) laplacian
! (*) and more...
! (*) calculate_spectral
!=======================================================

MODULE function_ops
  use global_variables
  IMPLICIT NONE

  real(pr), dimension(:,:,:), allocatable :: Epoint ! needed in calculate_total_energy
  real(pr), dimension(:,:), allocatable :: vort_plane
  complex(pr), dimension(:,:,:), allocatable :: temp1_function_cx ! needed in vel2vort_fourier
  complex(pr), dimension(:,:,:), allocatable :: temp2_function_cx ! needed in vel2vort_fourier
  
  



CONTAINS
  !========================================================= 
  ! SUBROUTINE: function_ops_allocate()
  !
  ! allocate the variables needed for solver
  !=========================================================
  SUBROUTINE function_ops_allocate()
    use global_variables
    IMPLICIT NONE
    if (.not. allocated(Epoint)) allocate(Epoint(1:n(1), 1:n(2), 1:local_N))
    if (.not. allocated(temp1_function_cx)) allocate(temp1_function_cx(1:n(1)/2+1, 1:n(2), 1:local_N))
    if (.not. allocated(temp2_function_cx)) allocate(temp2_function_cx(1:n(1)/2+1, 1:n(2), 1:local_N))
    if (rank == 0) then
       if (.not. allocated(vort_plane)) allocate(vort_plane(1:n(1), 1:n(3)))
    end if
    
    
  END SUBROUTINE function_ops_allocate

  !========================================================= 
  ! SUBROUTINE: solver_deallocate()
  !
  ! deallocate the variables needed for solver
  !=========================================================
  SUBROUTINE function_ops_deallocate()
     use global_variables
    IMPLICIT NONE
    
    if(allocated(Epoint)) deallocate(Epoint)
    if(allocated(temp1_function_cx)) deallocate(temp1_function_cx)
    if(allocated(temp2_function_cx)) deallocate(temp2_function_cx)
    if(rank == 0) then
       if (allocated(vort_plane)) deallocate(vort_plane)
    end if
    
  END SUBROUTINE function_ops_deallocate


  !==============================================================================
  !  SUBROUTINE derivarive_fourier(u_cx, du_cx, i)
  !  compute i-th derivative of u_cx in Fourier space
  !  input: u_cx(1:n(1)/2+1,1:n(2),1:local_N)
  !  input: i (integer)
  !  output: du_cx(1:n(1)/2+1,1:n(2),1:local_N)
  !===============================================================================
  SUBROUTINE derivative_fourier(u_cx,du_cx,i)    
    USE global_variables
    IMPLICIT NONE
    
    integer, intent(in) :: i ! 1,2,3       
    complex(pr), dimension(1:n(1)/2+1,1:n(2),1:local_N), intent(in) :: u_cx
    complex(pr), dimension(1:n(1)/2+1,1:n(2),1:local_N), intent(out) :: du_cx
    integer :: i1, i2, i3
    do i3 = 1, local_N
       do i2 = 1, n(2)
          do i1 = 1, n(1)/2+1
             if (abs(u_cx(i1,i2,i3)) < 1e-15) then
                du_cx(i1,i2,i3) = cmplx(0.0_pr)
             else
                
                select case (i)
                case (1)
                   du_cx(i1,i2,i3) = u_cx(i1,i2,i3)*cmplx(0, K1(i1), pr)
                case (2)
                   du_cx(i1,i2,i3) = u_cx(i1,i2,i3)*cmplx(0, K2(i2), pr)
                case (3)
                   du_cx(i1,i2,i3) = u_cx(i1,i2,i3)*cmplx(0, K3(i3+local_k_offset), pr)
                end select
             endif
             
             
          end do
       end do
    end do 
    
  

  END SUBROUTINE derivative_fourier


   !==============================================================================
  !  SUBROUTINE L2_grad(u_cx, rslt)
  !  compute ||Grad_u||_{L^2}
  !  input: u_cx(1:n(1)/2+1,1:n(2),1:local_N,1:3)
  !  output: rslt
  !===============================================================================
  SUBROUTINE L2_grad(u_cx,rslt)    
    USE global_variables
    IMPLICIT NONE
       
    complex(pr), dimension(1:n(1)/2+1,1:n(2),1:local_N, 1:3), intent(in) :: u_cx
    real(pr), intent(out) :: rslt
    integer :: i1, i2, i3, ii, i
    real(pr) :: val

    rslt = 0

    do ii = 1,3
       do i = 1,3
          call derivative_fourier(u_cx(:,:,:,ii), temp1_function_cx, i)
          call L2_product_fourier_1(temp1_function_cx, temp1_function_cx, val)
          rslt = rslt + val
       end do
    end do
    
        
          
    
    
  
  

  END SUBROUTINE L2_grad


  !==============================================================================
  !  SUBROUTINE div_fourier(u_cx, div_val)
  !  compute the divergence of a function 
  
  !===============================================================================
  SUBROUTINE div_fourier(u_cx,div_val)    
    USE global_variables
    IMPLICIT NONE
    INCLUDE "mpif.h"
    
    complex(pr), dimension(1:n(1)/2+1,1:n(2),1:local_N, 1:3), intent(in) :: u_cx
    real(pr), intent(out) :: div_val
    integer :: i1, i2, i3
    complex(pr) :: local_val
    real(pr) :: local_val_r
    

    temp1_function_cx = cmplx(0.0_pr)
    temp2_function_cx = cmplx(0.0_pr)
    call derivative_fourier(u_cx(:,:,:,1), temp1_function_cx, 1)
    temp2_function_cx = temp2_function_cx + temp1_function_cx
    temp1_function_cx = cmplx(0.0_pr)
    call derivative_fourier(u_cx(:,:,:,2), temp1_function_cx, 2)
    temp2_function_cx = temp2_function_cx + temp1_function_cx
    temp1_function_cx = cmplx(0.0_pr)
    call derivative_fourier(u_cx(:,:,:,3), temp1_function_cx, 3)
    temp2_function_cx = temp2_function_cx + temp1_function_cx

    do i3=1,local_N
       do i2=1,n(2)
          do i1=1,n(1)/2+1
             if (i1 == 1 .or. i1 == n(1)/2+1) then
                local_val = local_val + temp2_function_cx(i1,i2,i3)*conjg(temp2_function_cx(i1,i2,i3))
             else
                local_val = local_val + temp2_function_cx(i1,i2,i3)*conjg(temp2_function_cx(i1,i2,i3)) &
                     + conjg(temp2_function_cx(i1,i2,i3))*temp2_function_cx(i1,i2,i3)
             end if             
          end do          
       end do       
    end do

    div_val = 0.0_pr
    local_val_r = real(local_val)
    call mpi_allreduce(local_val_r, div_val, 1, mpi_double_precision, mpi_sum, mpi_comm_world, statinfo)
    




  

  END SUBROUTINE div_fourier

  !==============================================================================
  !  SUBROUTINE abs_deriv_fourier(u_cx, du_cx)
  !  du_cx = |k|*u_cx
  !  input: u_cx(1:n(1)/2+1,1:n(2),1:local_N)
  !  output: du_cx(1:n(1)/2+1,1:n(2),1:local_N)
  !===============================================================================
  SUBROUTINE abs_deriv_fourier(u_cx,du_cx,power)    
    USE global_variables
    IMPLICIT NONE
          
    complex(pr), dimension(1:n(1)/2+1,1:n(2),1:local_N, 1:3), intent(in) :: u_cx
    complex(pr), dimension(1:n(1)/2+1,1:n(2),1:local_N, 1:3), intent(out) :: du_cx
    real(pr), intent(in) :: power
    integer :: i1, i2, i3, nn
    do nn = 1, 3
       do i3 = 1, local_N
          do i2 = 1, n(2)
             do i1 = 1, n(1)/2+1
                if (abs(u_cx(i1,i2,i3,nn)) < 1e-15_pr .or. i1*i2*(i3+local_k_offset)==1) then
                   du_cx(i1,i2,i3,nn) = cmplx(0.0_pr)
                else
                   du_cx(i1,i2,i3,nn) = u_cx(i1,i2,i3,nn)*sqrt(K1(i1)**2 + K2(i2)**2 + K3(i3+local_k_offset)**2)**power
                endif
                
                
             end do
          end do 
       end do
    end do
    
  

  END SUBROUTINE abs_deriv_fourier

  !==============================================================================
  !  SUBROUTINE G_sigma_fourier(u_cx, du_cx, sigma)
  !  usigma_cx: exp(sigma|D|)*u_cx
  !  input: u_cx(1:n(1)/2+1,1:n(2),1:local_N)
  !  output: usigma_cx(1:n(1)/2+1,1:n(2),1:local_N)
  !===============================================================================
  SUBROUTINE G_sigma_fourier(u_cx,usigma_cx,sigma)    
    USE global_variables
    IMPLICIT NONE
          
    complex(pr), dimension(1:n(1)/2+1,1:n(2),1:local_N, 1:3), intent(in) :: u_cx
    complex(pr), dimension(1:n(1)/2+1,1:n(2),1:local_N, 1:3), intent(out) :: usigma_cx
    real(pr), intent(in) :: sigma
    real(pr) :: norm
    integer :: i1, i2, i3, nn
    do nn = 1, 3
       do i3 = 1, local_N
          do i2 = 1, n(2)
             do i1 = 1, n(1)/2+1
                if (abs(u_cx(i1,i2,i3,nn)) < 1e-15_pr .or. i1*i2*(i3+local_k_offset) == 1) then
                   usigma_cx(i1,i2,i3, nn) = cmplx(0.0_pr)
                else
                   norm = sqrt(K1(i1)**2 + K2(i2)**2 + K3(i3+local_k_offset)**2)
                   usigma_cx(i1,i2,i3,nn) = u_cx(i1,i2,i3,nn)*exp(norm*sigma)
                end if
                

             end do
          end do 
       end do
    end do
    
  

  END SUBROUTINE G_sigma_fourier

  !==============================================================================
  !  SUBROUTINE G_l_s_sigma_fourier(u_cx, du_cx, l, s, sigma)
  !  usigma_cx: (1+|D|^2)^s*exp(sigma|D|)*u_cx
  !  input: u_cx(1:n(1)/2+1,1:n(2),1:local_N)
  !  output: usigma_cx(1:n(1)/2+1,1:n(2),1:local_N)
  !===============================================================================
  SUBROUTINE G_l_s_sigma_fourier(u_cx,usigma_cx, l, s, sigma)    
    USE global_variables
    IMPLICIT NONE
          
    complex(pr), dimension(1:n(1)/2+1,1:n(2),1:local_N, 1:3), intent(in) :: u_cx
    complex(pr), dimension(1:n(1)/2+1,1:n(2),1:local_N, 1:3), intent(out) :: usigma_cx
    real(pr), intent(in) :: sigma, l, s
    real(pr) :: norm2, norm
    integer :: i1, i2, i3, nn
    do nn = 1, 3
       do i3 = 1, local_N
          do i2 = 1, n(2)
             do i1 = 1, n(1)/2+1
                if (abs(u_cx(i1,i2,i3,nn)) < 1e-15_pr) then
                   usigma_cx(i1,i2,i3, nn) = cmplx(0.0_pr)
                else
                   norm2 = K1(i1)**2 + K2(i2)**2 + K3(i3+local_k_offset)**2
                   norm = sqrt(norm2)
                   usigma_cx(i1,i2,i3,nn) = u_cx(i1,i2,i3,nn)*(1.0_pr+l**2*norm2)**s*exp(norm*sigma)
                end if
                

             end do
          end do 
       end do
    end do
    
  

  END SUBROUTINE G_l_s_sigma_fourier


  !==============================================================================
  !  SUBROUTINE G_alpha_fourier(u_cx, du_cx, alpha)
  !  usigma_cx: (1-alpha^2|D|^2)^(-1)*exp(sigma|D|)*u_cx
  !  input: u_cx(1:n(1)/2+1,1:n(2),1:local_N)
  !  output: usigma_cx(1:n(1)/2+1,1:n(2),1:local_N)
  !===============================================================================
  SUBROUTINE G_alpha_fourier(u_cx, du_cx, alpha)    
    USE global_variables
    IMPLICIT NONE
          
    complex(pr), dimension(1:n(1)/2+1,1:n(2),1:local_N, 1:3), intent(in) :: u_cx
    complex(pr), dimension(1:n(1)/2+1,1:n(2),1:local_N, 1:3), intent(out) :: du_cx
    real(pr), intent(in) :: alpha
    real(pr) :: norm
    integer :: i1, i2, i3, nn
    do nn = 1, 3
       do i3 = 1, local_N
          do i2 = 1, n(2)
             do i1 = 1, n(1)/2+1
                if (abs(u_cx(i1,i2,i3,nn)) < 1e-15_pr) then
                   du_cx(i1,i2,i3, nn) = cmplx(0.0_pr)
                else
                   norm = K1(i1)**2 + K2(i2)**2 + K3(i3+local_k_offset)**2

                   du_cx(i1,i2,i3,nn) = u_cx(i1,i2,i3,nn)/(1.0_pr+alpha**2*norm)
                end if
                

             end do
          end do 
       end do
    end do
    
  

  END SUBROUTINE G_alpha_fourier
  
 !==============================================================================
  !  SUBROUTINE laplacian_fourier(u_cx, ddu_cx)
  !  compute laplacian of u_cx in Fourier space
  !  input: u_cx(1:n(1)/2+1,1:n(2),1:local_N,1:3)
  !  output: ddu_cx(1:n(1)/2+1,1:n(2),1:local_N,1:3)
  !===============================================================================
  SUBROUTINE laplacian_fourier(u_cx,ddu_cx)    
    USE global_variables
    IMPLICIT NONE
    
     
    complex(pr), dimension(1:n(1)/2+1,1:n(2),1:local_N, 1:3), intent(in) :: u_cx
    complex(pr), dimension(1:n(1)/2+1,1:n(2),1:local_N, 1:3), intent(out) :: ddu_cx
    integer :: i1, i2, i3, nn
    do nn = 1, 3
       do i3 = 1, local_N
          do i2 = 1, n(2)
             do i1 = 1, n(1)/2+1
                if (abs(u_cx(i1,i2,i3, nn)) < 1e-15 .or. i1*i2*(i3+local_k_offset)== 1) then
                   ddu_cx(i1,i2,i3, nn) = cmplx(0.0_pr)
                else                   
                   ddu_cx(i1,i2,i3,nn) = - u_cx(i1,i2,i3,nn)*(K1(i1)**2 + K2(i2)**2 + K3(i3+local_k_offset)**2)
                endif
                
                
             end do
          end do 
       end do
    end do

  

  END SUBROUTINE laplacian_fourier



  !==============================================================================
  !  SUBROUTINE vel2vort_fourier(u_cx, w_cx)
  !  compute the vorticity from velocity in Fourier space
  !  input: u_cx(1:n(1)/2+1,1:n(2),1:local_N,1:3)
  !  output: w_cx(1:n(1)/2+1,1:n(2),1:local_N,1:3)
  !
  !  use: temp1_function_cx
  !       temp2_function_cx
  !===============================================================================
  SUBROUTINE vel2vort_fourier(u_cx, w_cx)    
    USE global_variables
    IMPLICIT NONE
    complex(pr), dimension(1:n(1)/2+1,1:n(2),1:local_N,1:3), intent(in) :: u_cx
    complex(pr), dimension(1:n(1)/2+1,1:n(2),1:local_N,1:3), intent(out) :: w_cx
    integer :: nn


    ! w1 = du3/dx2 - du2/dx3
    call derivative_fourier(u_cx(:,:,:,3),temp1_function_cx(:,:,:), 2)
    call derivative_fourier(u_cx(:,:,:,2),temp2_function_cx(:,:,:), 3)
    w_cx(:,:,:,1) = temp1_function_cx - temp2_function_cx;

    ! w2 = du1/dx3 - du3/dx1
    call derivative_fourier(u_cx(:,:,:,1),temp1_function_cx(:,:,:), 3)
    call derivative_fourier(u_cx(:,:,:,3),temp2_function_cx(:,:,:), 1)
    w_cx(:,:,:,2) = temp1_function_cx - temp2_function_cx;

    ! w3 = du2/dx1 - du1/dx2
    call derivative_fourier(u_cx(:,:,:,2),temp1_function_cx(:,:,:), 1)
    call derivative_fourier(u_cx(:,:,:,1),temp2_function_cx(:,:,:), 2)
    w_cx(:,:,:,3) = temp1_function_cx - temp2_function_cx;
    
    
    
   

  END SUBROUTINE vel2vort_fourier


  !==============================================================================
  !  SUBROUTINE vort2vel_fourier(w_cx, u_cx)
  !  compute the velocity from vertocity in Fourier space
  !  input: w_cx(1:n(1)/2+1,1:n(2),1:local_N,1:3)
  !  output: u_cx(1:n(1)/2+1,1:n(2),1:local_N,1:3)
  !
  !  use: temp2_function_cx
  !  
  !===============================================================================
  SUBROUTINE vort2vel_fourier(w_cx, u_cx)    
    USE global_variables
    IMPLICIT NONE
    complex(pr), dimension(1:n(1)/2+1,1:n(2),1:local_N,1:3), intent(in) :: w_cx
    complex(pr), dimension(1:n(1)/2+1,1:n(2),1:local_N,1:3), intent(out) :: u_cx
    integer :: i1, i2, i3, nn
    real(pr) :: cnst
    ! u_cx = curl(-laplacian^(-1) w_cx) = -laplacian^(-1) curl(w_cx)
    call vel2vort_fourier(w_cx, u_cx)
    do nn = 1, 3
       do i3 = 1, local_N
          do i2 = 1, n(2)
             do i1 = 1, n(1)/2+1
                cnst = K1(i1)**2 + K2(i2)**2 + K3(i3+local_k_offset)**2
                if (abs(u_cx(i1,i2,i3,nn)) > 1.0e-15_pr .and. i1*i2*(i3+local_k_offset) .ne. 1) then
                   u_cx(i1,i2,i3,nn) = u_cx(i1,i2,i3,nn)/cnst
                else
                   u_cx(i1,i2,i3,nn) = cmplx(0.0_pr)
                end if
             end do
          end do 
       end do
    end do

   

  END SUBROUTINE vort2vel_fourier
  !==============================================================================
  !  SUBROUTINE L2_product(u1, u2)
  !  compute the inner product of two functions
  !  input: u1(1:n(1),1:n(2),1:local_N,1:3), u2(1:n(1),1:n(2),1:local_N,1:3)
  !  output: <u1, u2>_L2
  !  
  !===============================================================================
  SUBROUTINE L2_product(u1, u2, rslt)    
    USE global_variables
    IMPLICIT NONE
    INCLUDE "mpif.h"
    real(pr), dimension(1:n(1),1:n(2),1:local_N,1:3), intent(in) :: u1, u2
    real(pr), intent(out) :: rslt
    real(pr) :: local_val
    rslt = 0.0_pr
    local_val = Helicity(u1,u2)
    call mpi_allreduce(local_val, rslt, 1, mpi_double_precision, mpi_sum, mpi_comm_world, statinfo)

  END SUBROUTINE L2_product

  !==============================================================================
  !  SUBROUTINE L2_product_fourier(u1_cx, u2_cx)
  !  compute the inner product of two functions
  !  input: u1_cx(1:n(1),1:n(2),1:local_N,1:3), u2_cx(1:n(1),1:n(2),1:local_N,1:3)
  !  output: <u1, u2>_L2
  !  
  !===============================================================================
  SUBROUTINE L2_product_fourier(u1_cx, u2_cx, rslt)    
    USE global_variables
    IMPLICIT NONE
    INCLUDE "mpif.h"
    complex(pr), dimension(1:n(1)/2+1,1:n(2),1:local_N,1:3), intent(in) :: u1_cx, u2_cx
    real(pr), intent(out) :: rslt
    complex(pr) :: local_val
    real(pr) :: local_val_r
    integer :: i1, i2, i3
    
    local_val = 0.0_pr
    do i3=1,local_N
       do i2=1,n(2)
          do i1=1,n(1)/2+1
             if (i1 == 1 .or. i1 == n(1)/2+1) then
                local_val = local_val + u1_cx(i1,i2,i3,1)*conjg(u2_cx(i1,i2,i3,1)) &
                     + u1_cx(i1,i2,i3,2)*conjg(u2_cx(i1,i2,i3,2)) &
                     + u1_cx(i1,i2,i3,3)*conjg(u2_cx(i1,i2,i3,3))
             else
                local_val = local_val + u1_cx(i1,i2,i3,1)*conjg(u2_cx(i1,i2,i3,1)) &
                     + conjg(u1_cx(i1,i2,i3,1))*u2_cx(i1,i2,i3,1) &
                     + u1_cx(i1,i2,i3,2)*conjg(u2_cx(i1,i2,i3,2)) &
                     + conjg(u1_cx(i1,i2,i3,2))*u2_cx(i1,i2,i3,2) &
                     + u1_cx(i1,i2,i3,3)*conjg(u2_cx(i1,i2,i3,3)) &
                     + conjg(u1_cx(i1,i2,i3,3))*u2_cx(i1,i2,i3,3) 
             end if             
          end do          
       end do       
    end do

    local_val_r = real(local_val)
    rslt = 0.0_pr
    call mpi_allreduce(local_val_r, rslt, 1, mpi_double_precision, mpi_sum, mpi_comm_world, statinfo)

  END SUBROUTINE L2_product_fourier

    !==============================================================================
  !  SUBROUTINE L2_product_fourier(u1_cx, u2_cx)
  !  compute the inner product of two functions
  !  input: u1_cx(1:n(1),1:n(2),1:local_N,1:3), u2_cx(1:n(1),1:n(2),1:local_N,1:3)
  !  output: <u1, u2>_L2
  !  
  !===============================================================================
  SUBROUTINE L2_product_fourier_1(u1_cx, u2_cx, rslt)    
    USE global_variables
    IMPLICIT NONE
    INCLUDE "mpif.h"
    complex(pr), dimension(1:n(1)/2+1,1:n(2),1:local_N), intent(in) :: u1_cx, u2_cx
    real(pr), intent(out) :: rslt
    complex(pr) :: local_val
    real(pr) :: local_val_r
    integer :: i1, i2, i3
    
    local_val = 0.0_pr
    do i3=1,local_N
       do i2=1,n(2)
          do i1=1,n(1)/2+1
             if (i1 == 1 .or. i1 == n(1)/2+1) then
                local_val = local_val + u1_cx(i1,i2,i3)*conjg(u2_cx(i1,i2,i3)) 
             else
                local_val = local_val + u1_cx(i1,i2,i3)*conjg(u2_cx(i1,i2,i3)) &
                     + conjg(u1_cx(i1,i2,i3))*u2_cx(i1,i2,i3) 
             end if             
          end do          
       end do       
    end do

    local_val_r = real(local_val)
    rslt = 0.0_pr
    call mpi_allreduce(local_val_r, rslt, 1, mpi_double_precision, mpi_sum, mpi_comm_world, statinfo)

  END SUBROUTINE L2_product_fourier_1
  !==============================================================================
  !  SUBROUTINE calculate_total_energy(u, w, K_total, E_total, H_total, maxW_global, E_component)
  !  compute the energy(etc) of the velocity field
  !  input: velocity field u(1:n(1),1:n(2),1:local_N,1:3) 
  !  input: vorticity field u(1:n(1),1:n(2),1:local_N,1:3)
  !  output: K(energy), E(enstropy), H(helicity), maxW
  !
  !  use: Epoint(1:n(1), 1:n(2), 1:local_N)
  !===============================================================================
  SUBROUTINE calculate_total_energy(u, w, K_total, E_total, H_total, maxW_global, E_component)
    USE global_variables
    IMPLICIT NONE
    INCLUDE "mpif.h"
    
    
    real(pr), dimension(1:n(1), 1:n(2), 1:local_N, 1:3), intent(in) :: u
    real(pr), dimension(1:n(1), 1:n(2), 1:local_N, 1:3), intent(in) :: w
    
    real(pr), intent(out) :: K_total, E_total, H_total, maxW_global
    real(pr), dimension(1:3), intent(out) :: E_component
 
    real(pr) :: K_local, E_local, H_local, maxW_local
    real(pr), dimension(1:3) :: K_local_vec, E_local_vec

    integer :: i1, i2, i3

    Epoint = 0_pr
    
    K_local_vec = energy(u)
    E_local_vec = energy(w)
    H_local = helicity(u,w)

    K_local = K_local_vec(1) + K_local_vec(2) + K_local_vec(3)
    E_local = E_local_vec(1) + E_local_vec(2) + E_local_vec(3)
    
    
          
          

    do i3=1,local_N
       do i2=1,n(2)
          do i1=1,n(1)
             Epoint(i1,i2,i3) = sqrt(w(i1,i2,i3,1)**2 &
                  + w(i1,i2,i3,2)**2 + w(i1,i2,i3,3)**2)
          end do 
       end do       
    end do
    maxW_local = maxval(Epoint)
    

    CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
    ! only collect the sum at rank 0
    call mpi_reduce(K_local, K_total, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, Statinfo)
    call mpi_reduce(E_local, E_total, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, Statinfo)
    call mpi_reduce(H_local, H_total, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, Statinfo)
    call mpi_reduce(E_local_vec, E_component, 3, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, Statinfo)
    call mpi_reduce(maxW_local, maxW_global, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, MPI_COMM_WORLD, Statinfo)
    
    

  END SUBROUTINE calculate_total_energy


  !==============================================================================
  !  SUBROUTINE calculate_vorticity_moments(w, Omega, mm)
  !  compute vorticiy moments for m = 1,2,...,mm
  !===============================================================================
  SUBROUTINE calculate_vorticity_moments(w, Omega, mm)
    USE global_variables
    IMPLICIT NONE
    INCLUDE "mpif.h"
    
    real(pr), dimension(1:n(1), 1:n(2), 1:local_N, 1:3), intent(in) :: w
    integer, intent(in):: mm
    real(pr), dimension(1:mm), intent(out) :: Omega
    
    real(pr), dimension(1:mm) :: Omega_local
    real(pr) :: val, val1

    integer :: i1, i2, i3, ii

   
    Omega_local = 0.0_pr
    do i3=1,local_N
       do i2=1,n(2)
          do i1=1,n(1)
             val = sqrt(w(i1,i2,i3,1)**2 &
                  + w(i1,i2,i3,2)**2 + w(i1,i2,i3,3)**2)**(2.0_pr)
             Omega_local(1) = Omega_local(1) + val*dV
             val1 = val
             do ii=2,mm
                val1 = val1*val
                Omega_local(ii) = Omega_local(ii) + val1*dV
             end do
                
          end do
       end do
    end do

    CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
 
    ! only collect the sum at rank 0
    call mpi_reduce(Omega_local, Omega, mm, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, Statinfo)

    if(rank == 0) then
       do ii=1,mm
          Omega(ii) = Omega(ii)**(0.5_pr/ii)
       end do
    end if
    
    
    

  END SUBROUTINE calculate_vorticity_moments

  !==============================================================================
  !  SUBROUTINE calculate_plane_max(w, max_val, max_pos)
  !===============================================================================
  SUBROUTINE calculate_plane_max(U, W, Uob, max_val, max_pos)
    USE global_variables
    IMPLICIT NONE
    INCLUDE "mpif.h"
    
    real(pr), dimension(1:n(1), 1:n(2), 1:local_N, 1:3), intent(in) :: U, W
    real(pr), dimension(1:n(1), 1:n(2), 1:local_N), intent(in) :: Uob

    
    real(pr), dimension(1:3), intent(out) :: max_val
    real(pr), dimension(1:6), intent(out) :: max_pos
    integer :: nn
    integer :: i1, i3

    if (rank == 0) then
       vort_plane = 0.0_pr
    end if
    
    do nn = 1,3
       call MPI_GATHER(W(:,:,:,nn), total_local_size, MPI_DOUBLE_PRECISION, global_u, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
       if (rank == 0) then          
          do i3 = 1, n(3)
             do i1 = 1, n(1)
                vort_plane(i1,i3) = vort_plane(i1,i3) + global_u(i1,i1,i3)**2
             end do
          end do
       end if
    end do
    
    if (rank == 0) then
       
       max_val(1) = sqrt(maxval(vort_plane))
       
       max_pos(1:2) = (maxloc(vort_plane)-1.0_pr)*1.0_pr/n(1)
    end if

    if (rank == 0) then
       vort_plane = 0.0_pr
    end if
    
    do nn = 1,3
       call MPI_GATHER(U(:,:,:,nn), total_local_size, MPI_DOUBLE_PRECISION, global_u, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
       if (rank == 0) then
          do i3 = 1, n(3)
             do i1 = 1, n(1)
                vort_plane(i1,i3) = vort_plane(i1,i3) + global_u(i1,i1,i3)**2
             end do
          end do
       end if
    end do
    
    if (rank == 0) then
       
       max_val(2) = sqrt(maxval(vort_plane))
       max_pos(3:4) = (maxloc(vort_plane)-1.0_pr)*1.0_pr/n(1)
    end if

    if (rank == 0) then
       vort_plane = 0.0_pr
    end if
    

    call MPI_GATHER(Uob(:,:,:), total_local_size, MPI_DOUBLE_PRECISION, global_u, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
    if (rank == 0) then
       do i3 = 1, n(3)
          do i1 = 1, n(1)
             vort_plane(i1,i3) = global_u(i1,i1,i3)
          end do
       end do
       max_val(3) = maxval(vort_plane)
       max_pos(5:6) = (maxloc(vort_plane)-1.0_pr)*1.0_pr/n(1)
       
    end if

    
    


    

    call MPI_BCAST(max_val, 3, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
    call MPI_BCAST(max_pos, 6, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
    
    
    
    
    
    

  END SUBROUTINE calculate_plane_max


  
  
  !==============================================================================
  !  SUBROUTINE calculate_spectrum(u_cx, spectral_data)
  !  compute the spectral data of u_cx: Ek = \int_Sk |Hatu_k|^2 
  !  input: u_cx(1:n(1)/2+1,1:n(2),1:local_N,1:3)
  !  output: spectral_data(1:kk_max, 1:2), kk_max = sqrt(n(1)^2+n(2)^2+n(3)^2)/2
  !===============================================================================
  SUBROUTINE calculate_spectrum(u_cx, spectral_data)
    
    
    USE global_variables
    USE fftwfunction
    IMPLICIT NONE
    INCLUDE "mpif.h"
    
           
    complex(pr), dimension(1:n(1)/2+1,1:n(2),1:local_N,1:3), intent(in) :: u_cx
    real(pr), dimension(1:kkmax, 1:2), intent(out) :: spectral_data
    integer :: i1, i2, i3, ii
    real(pr) :: norm_k
    real(pr) :: local_val, global_val
    
    spectral_data = 0_pr


    do i3=1,local_N
       do i2=1,n(2)
          do i1=1,n(1)/2+1
             norm_k = sqrt( K1(i1)**2 + K2(i2)**2 + K3(i3+local_k_offset)**2 )/2.0_pr/PI
             ii = floor(norm_k) + 1
             if (i1 == 1.or. i1 == n(1)/2+1) then
                spectral_data(ii,1) = spectral_data(ii,1) + 0.5_pr*(abs(u_cx(i1,i2,i3,1))**2 &
                     + abs(u_cx(i1,i2,i3,2))**2 + abs(u_cx(i1,i2,i3,3))**2)
             else
                spectral_data(ii,1) = spectral_data(ii,1) + abs(u_cx(i1,i2,i3,1))**2 &
                     + abs(u_cx(i1,i2,i3,2))**2 + abs(u_cx(i1,i2,i3,3))**2
             end if             
          end do          
       end do       
    end do

    CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
 
    ! only collect the sum at rank 0
    call mpi_reduce(spectral_data(:,1), spectral_data(:,2), kkmax, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, Statinfo)
    if (rank == 0) then
       do i1 = 1, kkmax
          spectral_data(i1,1) = real(i1*2, pr)*PI
       end do
    end if
    
    

  END SUBROUTINE calculate_spectrum
  !==========================================
  ! PERFORM DEALIASING IN FOURIER SPACE
  !==========================================
  SUBROUTINE dealiasing_fourier_m(f, m)
    USE global_variables
    USE fftwfunction
    IMPLICIT NONE
    
    integer, intent(in) :: m
    COMPLEX(pr), DIMENSION(1:n(1)/2+1,1:n(2),1:local_N, 1:m), INTENT(INOUT) :: f
    
         
    INTEGER :: i1, i2, i3, ii
    REAL(pr), DIMENSION(1:3) :: k
    REAL(pr) :: mode 

    
    Do ii = 1, m
       DO i3 = 1,local_N
          DO i2 = 1,n(2)
             DO i1 = 1,n(1)/2+1
                !if(abs(K1(i1)) > n(1)/3.0_pr*2.0_pr*PI.or.abs(K2(i2)) > n(2)/3.0_pr*2.0_pr*PI.or.abs(K2(i3+local_k_offset)) > n(3)/3.0_pr*2.0_pr*PI) then
                f(i1,i2,i3,ii) = f(i1,i2,i3,ii)*K1_filter(i1)*K2_filter(i2)*K3_filter(i3+local_k_offset)
                   !f(i1,i2,i3,ii) = cmplx(0.0_pr)
                !end if
                
             END DO
          END DO
       END DO
    END DO

    !CALL bfourier(faux,aux)
    !f = REAL(aux)
    
    !DEALLOCATE(aux)
    !DEALLOCATE(faux)

  END SUBROUTINE dealiasing_fourier_m


  !==========================================
  ! PERFORM DEALIASING IN FOURIER SPACE
  !==========================================
  SUBROUTINE dealiasing_cutoff_m(f, m)
    USE global_variables
    USE fftwfunction
    IMPLICIT NONE
    
    integer, intent(in) :: m
    COMPLEX(pr), DIMENSION(1:n(1)/2+1,1:n(2),1:local_N, 1:m), INTENT(INOUT) :: f
    
         
    INTEGER :: i1, i2, i3, ii
    REAL(pr), DIMENSION(1:3) :: k
    REAL(pr) :: mode 

    
    Do ii = 1, m
       DO i3 = 1,local_N
          DO i2 = 1,n(2)
             DO i1 = 1,n(1)/2+1
                if (abs(K1(i1)) > n(1)/3.0_pr*2.0_pr*PI) f(i1,i2,i3,ii) = cmplx(0.0_pr)
                if (abs(K2(i2)) > n(2)/3.0_pr*2.0_pr*PI) f(i1,i2,i3,ii) = cmplx(0.0_pr)
                if (abs(K3(i3+local_k_offset)) > n(3)/3.0_pr*2.0_pr*PI) f(i1,i2,i3,ii) = cmplx(0.0_pr)
             END DO
          END DO
       END DO
    END DO

    !CALL bfourier(faux,aux)
    !f = REAL(aux)
    
    !DEALLOCATE(aux)
    !DEALLOCATE(faux)

  END SUBROUTINE dealiasing_cutoff_m


  
  !===================================
  !  Initial guess
  !===================================
  SUBROUTINE initial_condition(filename)
    USE global_variables
    USE data_ops
  
    
    IMPLICIT NONE
    INCLUDE "mpif.h"
    CHARACTER(len=*), INTENT(IN) :: filename
    REAL(pr), DIMENSION(1:3) :: dx
    CHARACTER(2) :: Fx_txt, Fy_txt, Fz_txt
    INTEGER :: nn,i,j,k
    INTEGER :: ii,jj,kk
    CHARACTER(4) :: Ntxt
    REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: aux,aux2
    REAL(pr), DIMENSION(:,:,:,:), ALLOCATABLE :: aux1
    
    
    !real :: cpustart, cpufinish
    dx = 1.0_pr/REAL(n, pr)         
   

    if (rank == 0) print *, InitCond_pathname
    Fx_txt = "Ux"
    Fy_txt = "Uy"
    Fz_txt = "Uz" 
 
    CALL read_field_R3toR3_ncdf2(Uvec, filename, Fx_txt, Fy_txt, Fz_txt)
  
    
  END SUBROUTINE initial_condition


  !===================================
  !  Initial guess
  !===================================
  SUBROUTINE initial_condition_refine(n_pre)
    USE global_variables
    USE fftwfunction
    IMPLICIT NONE
    INCLUDE "mpif.h"

    integer, dimension(1:3), intent(in) :: n_pre
    complex(pr), dimension(:,:,:), allocatable :: global_cx
    character(200) :: filename
    real(pr) :: val1, val2
    integer :: j1, j2, j3, jj2, jj3, nn
    integer :: local_count

    local_count = (n(1)/2+1)*n(2)*local_N
    
    
    filename = InitCond_pathname
    if (rank == 0) print *, InitCond_pathname

    if(rank == 0) then
      
       allocate(global_cx(1:n(1)/2+1, 1:n(2), 1:n(3)))
       print *, "allocation OK!"
       open(9, file=filename)
    end if
    
    do nn = 1,3
       if(rank == 0) then
          global_cx = cmplx(0.0_pr)
          do j3 = 1, n_pre(3)
             do j2 = 1, n_pre(2)
                do j1 = 1, n_pre(1)/2+1       
                   read(9, "(2 G20.12)") val1, val2
                   jj2 = j2
                   jj3 = j3
                   if (j2 > n_pre(2)/2+1) jj2 = j2-1-n_pre(2)+n(2)+1
                   if (j3 > n_pre(3)/2+1) jj3 = j3-1-n_pre(3)+n(3)+1
                   global_cx(j1,jj2,jj3) = cmplx(val1, val2)
                end do
             end do
          end do
       end if
       CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
       call MPI_Scatter(global_cx(:,:,:), local_count, MPI_DOUBLE_COMPLEX, temp1_function_cx, local_count, MPI_DOUBLE_COMPLEX, 0, MPI_COMM_WORLD, Statinfo)
       call fftbwd(temp1_function_cx, Uvec0(:,:,:,nn))
    end do

    if (rank == 0) then
       close(9)
       deallocate(global_cx)
       print *, "initail_data_refine OK!"
    end if
    
    CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)

    END SUBROUTINE initial_condition_refine

  !===================================
  !  Initial guess
  !===================================
  SUBROUTINE initial_condition_refine_mpi(n_pre)
    USE global_variables
    USE fftwfunction
    IMPLICIT NONE
    INCLUDE "mpif.h"

    integer, dimension(1:3), intent(in) :: n_pre
    
    character(200) :: filename
    integer :: local_count
    integer :: file_handle
    integer(kind=mpi_offset_kind) :: my_offset
    integer :: nn, j, j1, j2, j3
    integer(kind=8) :: offset
    complex(pr), dimension(:,:,:,:), allocatable :: u_cx
    integer :: i
    integer :: ios
    complex(pr) :: val

    filename = InitCond_pathname
    if (rank == 0) print *, InitCond_pathname


    allocate(u_cx(1:n_pre(1)/2+1,1:n_pre(2),1:local_N,1:3))
    
    
    print *, rank, "initial_condition_refine_mpi: allocation OK!"    
    
    
    u_cx = cmplx(0.0_pr)

          
       

    CALL MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_RDONLY, MPI_INFO_NULL, file_handle, Statinfo)
    CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
    print *, rank, "initial_condition_refine_mpi: mpi_file_open OK!"
    
    if(local_k_offset <= n_pre(3)/2 .or. local_k_offset >= n(3)+1-n_pre(3)/2) then
       
       if(local_k_offset <= n_pre(3)/2) then
          my_offset = 3*(n_pre(1)/2+1)*n_pre(2)*int(local_k_offset, mpi_offset_kind)*pr*2
       else
          my_offset = 3*(n_pre(1)/2+1)*n_pre(2)*int(local_k_offset-n(3)+n_pre(3), mpi_offset_kind)*pr*2
       end if
       
       local_count = 3*(n_pre(1)/2+1)*n_pre(2)*local_N*2 
       
       
       CALL MPI_File_seek(file_handle, my_offset, mpi_seek_set, Statinfo)
       print *, rank, "initial_condition_refine_mpi: mpi_file_seek OK!"
       CALL MPI_File_read(file_handle, u_cx, local_count, MPI_DOUBLE_precision, MPI_STATUS_IGNORE, Statinfo)
       print *, rank, "initial_condition_refine_mpi: input file OK!"   
   
      
    end if
    CALL MPI_File_close(file_handle, Statinfo)
    CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
    do nn = 1, 3
      temp1_function_cx = cmplx(0.0_pr)
       if(local_k_offset <= n_pre(3)/2 .or. local_k_offset >= n(3)+1-n_pre(3)/2) then
          do j3 = 1, local_N
             do j2 = 1, n_pre(2)
                do j1 = 1, n_pre(1)/2+1
                   if (j2 <= n_pre(2)/2+1) then
                      temp1_function_cx(j1,j2,j3) = u_cx(j1,j2,j3,nn)
                   else
                      
                      temp1_function_cx(j1,j2-n_pre(2)+n(2),j3) = u_cx(j1,j2,j3,nn)
                   end if
                      
                end do
             end do
          end do
       end if
       call fftbwd(temp1_function_cx, Uvec0(:,:,:,nn))
    end do
    
    
    deallocate(u_cx)
    
    if (rank == 0) then
       print *, "initial_data_refine_mpi OK!"
    end if

    

    END SUBROUTINE initial_condition_refine_mpi


  !==========================================================================
  ! translate the initial field
  !==========================================================================
  SUBROUTINE initial_condition_sym(myfield)   
    USE global_variables
    IMPLICIT NONE
    INCLUDE "mpif.h"
    
    real(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(INOUT) :: myfield
    
    INTEGER  :: i1, i2, i3, ii
    REAL(pr), DIMENSION (1:3) :: temp

    do i3 = 1, local_N
       do i2 = 1, n(2)
          do i1 = 1, n(1)/2
             temp = myfield(i1,i2,i3,:)
             myfield(i1,i2,i3,:) = myfield(i1+n(1)/2,i2,i3,:)
             myfield(i1+n(1)/2,i2,i3,:) = temp
          end do
       end do
    end do
    CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)

  END SUBROUTINE initial_condition_sym
  


    

  !=========================================================
  ! Calculate kinetic energy from function in physical space
  !=========================================================
  FUNCTION Energy(U) RESULT (kin_ener)
    USE global_variables
    IMPLICIT NONE
 
    REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(IN) :: U
    INTEGER ::  i1, i2, i3
    REAL(pr), DIMENSION(1:3) :: kin_ener
    
    kin_ener = 0.0_pr
    
    DO i3=1,local_N
       DO i2=1,n(2) 
          DO i1=1,n(1)  
             kin_ener(1) = kin_ener(1) + 0.5_pr*U(i1,i2,i3,1)**2*dV
             kin_ener(2) = kin_ener(2) + 0.5_pr*U(i1,i2,i3,2)**2*dV
             kin_ener(3) = kin_ener(3) + 0.5_pr*U(i1,i2,i3,3)**2*dV
          END DO
       END DO
    END DO
    
  END FUNCTION Energy
  
  



  !====================================================
  ! CALCULATE HELICITY FROM U AND W
  ! H = /int( U /cdot W)
  !====================================================
  FUNCTION Helicity(U,W) RESULT (H)
    USE global_variables
    IMPLICIT NONE
    REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(IN) :: U,W    
    REAL(pr) :: H
    
    INTEGER :: ii, jj, kk

    H = 0.0_pr
    
    DO kk=1,local_N
       DO jj=1,n(2)
          DO ii=1,n(1)
             H = H + ( U(ii,jj,kk,1)*W(ii,jj,kk,1) + U(ii,jj,kk,2)*W(ii,jj,kk,2) + U(ii,jj,kk,3)*W(ii,jj,kk,3) )*dV
          END DO
       END DO
    END DO
    
  END FUNCTION Helicity
  
	


  
  !==========================================================================
  ! OBTAINS THE DIV_FREE PROJECTION OF A GIVEN VECTOR FIELD IN FOURIER SPACE
  !==========================================================================
  SUBROUTINE div_free_fourier(myfield_cx)   
    USE global_variables
    IMPLICIT NONE
    
    COMPLEX(pr), DIMENSION(1:n(1)/2+1,1:n(2),1:local_N,1:3), INTENT(INOUT) :: myfield_cx
    
    INTEGER  :: i1, i2, i3, ii
    REAL(pr) :: ksq
    REAL(pr), DIMENSION (1:3) :: k
    COMPLEX(pr), DIMENSION(1:3) :: tmp_cx
    
    DO i3 = 1, local_N
       k(3) = K3(i3+local_k_offset)
       DO i2 = 1, n(2)
          k(2) = K2(i2)
          DO i1 = 1, n(1)/2+1        
             k(1) = K1(i1)
             ksq = K1(i1)**2 + K2(i2)**2 + K3(i3+local_k_offset)**2
             tmp_cx = myfield_cx(i1,i2,i3,:)
             IF (i1*i2*(i3+local_k_offset) .ne. 1) THEN
                myfield_cx(i1,i2,i3,1) = ((k(2)**2+k(3)**2)*tmp_cx(1) - k(1)*k(2)*tmp_cx(2) - k(1)*k(3)*tmp_cx(3))/ksq
                myfield_cx(i1,i2,i3,2) = (-k(1)*k(2)*tmp_cx(1) +  (k(1)**2+k(3)**2)*tmp_cx(2) - k(2)*k(3)*tmp_cx(3))/ksq
                myfield_cx(i1,i2,i3,3) = (-k(1)*k(3)*tmp_cx(1) - k(2)*k(3)*tmp_cx(2) + (k(1)**2+k(2)**2)*tmp_cx(3))/ksq
             ELSE
                myfield_cx(i1,i2,i3,1) = cmplx(0.0_pr)
                myfield_cx(i1,i2,i3,2) = cmplx(0.0_pr)
                myfield_cx(i1,i2,i3,3) = cmplx(0.0_pr)
             END IF
          END DO
       END DO
    END DO

  END SUBROUTINE div_free_fourier


  

 




  !==========================================
  !- Save a binary data file into .nc data file
  !==========================================
  SUBROUTINE binary2nc(filename)
    USE global_variables
    USE data_ops
    IMPLICIT NONE
    INCLUDE "mpif.h"
    CHARACTER(len=*), INTENT(IN) :: filename
    REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: local_fx, local_fy, local_fz
    REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: global_Ux, global_Uy, global_Uz
    CHARACTER(200) :: save2nc
    !real :: cpustart, cpufinish
    ALLOCATE( local_fx(1:n(1),1:n(2),1:local_N))
    ALLOCATE( local_fy(1:n(1),1:n(2),1:local_N))
    ALLOCATE( local_fz(1:n(1),1:n(2),1:local_N))
    IF (rank == 0) THEN   ! Note that we use: if rank == 0, be followed that MPI_GATHER gives data to global_field in rank = 0
       ALLOCATE( global_Ux(1:n(1),1:n(2),1:n(3)) )
       ALLOCATE( global_Uy(1:n(1),1:n(2),1:n(3)) )
       ALLOCATE( global_Uz(1:n(1),1:n(2),1:n(3)) )
       !call cpu_time(cpustart)
       open(unit=200,FILE = filename, FORM = 'UNFORMATTED', ACTION='READ',ACCESS= 'DIRECT',recl=n(1)*n(2)*n(3)*2)
       read(200,rec=1) global_Ux
       read(200,rec=2) global_Uy
       read(200,rec=3) global_Uz
       close(200)
       !call cpu_time(cpufinish)
       !print '("Reading from binary file takes = ",f9.3," seconds.")', cpufinish-cpustart
    end if
    CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
    !call cpu_time(cpustart)
    CALL MPI_Scatter(global_Ux, total_local_size, MPI_DOUBLE_PRECISION, local_fx, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
    CALL MPI_Scatter(global_Uy, total_local_size, MPI_DOUBLE_PRECISION, local_fy, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
    CALL MPI_Scatter(global_Uz, total_local_size, MPI_DOUBLE_PRECISION, local_fz, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
    !call cpu_time(cpufinish)
    !print '("MPI_Scatter velocity field takes = ",f9.3," seconds.")', cpufinish-cpustart
    CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
    Uvec(:,:,:,1) = local_fx
    Uvec(:,:,:,2) = local_fy
    Uvec(:,:,:,3) = local_fz
    if (save_binary2nc) then
       save2nc = filename//".nc"
       CALL save_field_R3toR3_ncdf(local_fx, local_fy, local_fz, "Ux", "Uy", "Uz", save2nc, "netCDF")
    end if
    DEALLOCATE( local_fx )
    DEALLOCATE( local_fy )
    DEALLOCATE( local_fz )
    if (rank ==0) then
       DEALLOCATE( global_Ux )
       DEALLOCATE( global_Uy )
       DEALLOCATE( global_Uz )
    end if
    CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
  END SUBROUTINE binary2nc



     

END MODULE function_ops
