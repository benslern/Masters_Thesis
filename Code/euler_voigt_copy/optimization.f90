!======================================
! MODULE CONTAINING INTERFACES FOR 
! THE OPTMIZATION PROBLEM
!
! MODULE
! (*) optimization
! 
! SUBROUTINE
! (*) maximization
! (*) maximization_RCG
! (*) projection
! (*) projection_RCG
! (*) rescale
! (*) compute_J
! (*) compute_gradJ
! (*) mnbrak
! (*) brent
! 
!======================================
MODULE optimization
!=======================================  
  use global_variables
  IMPLICIT NONE
  real(pr) :: norm_constr
  real(pr) :: sigma, l, s
  integer :: stepper_opt
  real(pr), dimension(:,:,:,:), allocatable :: gradJ_opt, gradJ_pre_opt
  real(pr), dimension(:,:,:,:), allocatable :: d_opt, d1_opt
  
CONTAINS
!=======================================
! SUBROUTINE optimization_allocate(norm_const_, sigma_, stepper_)
! allocation    
!=======================================
  
  SUBROUTINE optimization_allocate(norm_constr_,l_, s_, sigma_, stepper_)
    implicit none
    real(pr), intent(in) :: norm_constr_
    real(pr), intent(in) :: l_, s_, sigma_
    integer, intent(in) :: stepper_
    

    norm_constr = norm_constr_
    l = l_
    s = s_
    sigma = sigma_
    stepper_opt = stepper_
    if (.not. allocated(gradJ_opt)) allocate(gradJ_opt(1:n(1), 1:n(2), 1:local_N, 1:3))
    if (.not. allocated(gradJ_pre_opt)) allocate(gradJ_pre_opt(1:n(1), 1:n(2), 1:local_N, 1:3))
    if (.not. allocated(d_opt)) allocate(d_opt(1:n(1), 1:n(2), 1:local_N, 1:3))
    if (.not. allocated(d1_opt)) allocate(d1_opt(1:n(1), 1:n(2), 1:local_N, 1:3))


  END SUBROUTINE optimization_allocate

!=======================================
! SUBROUTINE optimization_deallocate()
! deallocation    
!=======================================
  
  SUBROUTINE optimization_deallocate()
    implicit none
    
    if (allocated(gradJ_opt)) deallocate(gradJ_opt)
    if (allocated(gradJ_pre_opt)) deallocate(gradJ_pre_opt)
    if (allocated(d_opt)) deallocate(d_opt)
    if (allocated(d1_opt)) deallocate(d1_opt)


  END SUBROUTINE optimization_deallocate
!=======================================



!======================================= 
! maximize the cost function 
!======================================= 
  SUBROUTINE maximization(tau_brack)
    USE global_variables
    use fftwfunction
    USE data_ops
    USE function_ops
    use solvers
    IMPLICIT NONE
    INCLUDE "mpif.h"
         

    REAL(pr) :: J0, J1, deltaJ, tau
    character(200) :: file_cost, file_grad
    INTEGER :: iter, mnbrak_flag, FixConstr_flag , i
    real(pr), dimension(1:3), intent(inout) :: tau_brack
    real(pr) :: val1, val2
    real(pr) :: norm2_grad
    
   

!======================================================
!- Initialize; Start iteration;
!======================================================
    if (rank == 0) then
       file_cost = TRIM(scratch_pathname)//"maximization_cost"//".dat"
       OPEN(3, FILE = file_cost, STATUS = 'REPLACE')
       close(3)
       file_grad = TRIM(scratch_pathname)//"maximization_grad"//".dat"
       OPEN(4, FILE = file_grad, STATUS = 'REPLACE')
       close(4)
    end if
      
    iter = 0
    J0 = 0.0_pr
    J1 = 0.0_pr
    deltaJ = 1.0_pr
    tau = 0.0_pr
    norm2_grad = 0.0_pr

    if (rank == 0) then
       print *, "eval_J; main_iter =", iter
    end if
       
    J1 = compute_J(Uvec0, fix_dt1, 1, iter, 1)   

    if (rank == 0) then
       open(3, file = file_cost, status = 'old', position = 'append')
       write(3, "(4 G20.12)"), iter, J1, norm2_grad, tau
       close(3)
    end if

    iter = 1
    
    DO WHILE ( (ABS(deltaJ) > OPTIM_TOL) .AND. (iter<=MAX_ITER) )

        if (rank == 0) then
          print *, "maximization; main_iter =", iter
       end if

   
!======================================================
!- compute the gradient
!======================================================
       if (rank == 0) then
          print *, "eval_grad_J; main_iter =", iter
       end if
       gradJ_opt = 0.0_pr
       call compute_gradJ(Uvec0, fix_dt2, 0, gradJ_opt, iter)
       call projection(Uvec0, gradJ_opt, gradJ_opt, norm2_grad)
      
     
       

!======================================================
!- maximiaztion using mnbrak and brent
!======================================================

       
       if (rank==0) then
          print *, "Start mnbrak; main_iter =", iter
       end if
       tau_brack = mnbrak("maxET", Uvec0, gradJ_opt, tau_brack(1), tau_brack(2), mnbrak_flag, iter)  
       IF (mnbrak_flag /= 0) THEN
          if (rank ==0) then
             print *, "mnbrack iteration beyond maximum, the maxdEdt stops iterating ... " , mnbrak_flag
          end if          
          CALL optim_error_handle(mnbrak_flag)  
       ELSE
          CALL optim_msg_handle(21)
       END IF
       if (rank==0) then
          print *, "Start brent; main_iter =", iter
       end if
       tau = brent(iter, "maxET", Uvec0, gradJ_opt, tau_brack)
       tau_brack(1) = 0.0_pr
       tau_brack(2) = 2.0_pr*tau
       IF (tau == TAU_MAX) THEN
          CALL optim_msg_handle(32)
       END IF
!======================================
! UPDATE (initial condition) VELOCITY
!======================================
       Uvec0 = Uvec0 + tau*gradJ_opt
       J0 = J1
       

!======================================================
! UPDATE cost 
!======================================================

       if (rank == 0) then
          print *, "eval_J; main_iter =", iter
       end if
       
       J1 = compute_J(Uvec0, fix_dt1, 1, iter, 1)   

       
       
       
       IF (iter > 0) THEN   ! Feb 17, 2018
          deltaJ = abs(J1-J0)/ABS(J0)
          IF (J1-J0 > -1.0e-15_pr) THEN 
             CALL optim_msg_handle(0)
             EXIT
          ELSEIF (deltaJ<OPTIM_TOL) THEN
             if (rank == 0) then
                open(3, file = file_cost, status = 'old', position = 'append')
                write(3, "(4 G20.12)"), iter, J1, norm2_grad, tau
                close(3)
                PRINT *, "Relative difference reaches tolerance, iteration exit ... ..."
             end if
             EXIT
          END IF
       END IF

       if (rank == 0) then
          open(3, file = file_cost, status = 'old', position = 'append')
          write(3, "(4 G20.12)"), iter, J1, norm2_grad, tau
          close(3)
       end if
       
       iter = iter + 1
    END DO
   
  END SUBROUTINE maximization


  


!======================================= 
! maximize the cost function using RCG
!======================================= 
  SUBROUTINE maximization_RCG(tau_brack)
    USE global_variables
    use fftwfunction
    USE databinary_handle
    USE data_ops
    USE function_ops
    use solvers
    IMPLICIT NONE
    INCLUDE "mpif.h"
         

    REAL(pr) :: J0, J1, deltaJ, tau
    character(200) :: file_cost, file_grad
    INTEGER :: iter, mnbrak_flag, FixConstr_flag , i
    real(pr), dimension(1:3), intent(inout) :: tau_brack
    real(pr) :: norm2_grad, norm2_grad_pre
    real(pr) :: val1, val2, val3, beta
    integer :: restart_flag
    integer :: restart_freq = 20
    integer :: sigma_freq = 10

    ! restart_flag = 1: conjugate gradient is not an ascent direction
    ! restart_flag = 2: far from quadratic function
    ! restart_flag = 3: frequent restart
    ! mod(iter, restart_freq) = 0 then restart

!======================================================
!- Initialize; Start iteration;
!======================================================
    if (rank == 0) then
       file_cost = TRIM(scratch_pathname)//"maximization_cost"//".dat"
       OPEN(3, FILE = file_cost, STATUS = 'REPLACE')
       close(3)
       file_grad = TRIM(scratch_pathname)//"maximization_grad"//".dat"
       OPEN(4, FILE = file_grad, STATUS = 'REPLACE')
       close(4)
    end if
      
    iter = 0
    norm2_grad = 0.0_pr
    J0 = 0.0_pr
    J1 = 0.0_pr
    deltaJ = 1.0_pr
    tau = 0.0_pr
    beta = 0.0_pr
    restart_flag = 0



    if (rank == 0) then
       print *, "eval_J; main_iter =", iter
    end if
       
    J1 = compute_J(Uvec0, fix_dt1, 1, iter, 1)
    if (0) then
       call read4binary2(iter+1, Uvec, "fwdTE")
       call fftfwd_m(Uvec, temp1_solver_cx, 3)
       call abs_deriv_fourier(temp1_solver_cx, temp1_solver_cx, 3.0_pr)
       call L2_product_fourier(temp1_solver_cx, temp1_solver_cx, J1)
       J1 = -J1
       final_time_iter = floor(endTime/fix_dt2)
    else if (1) THEN
       call save2binary2(Uvec, iter, "fwdTE")
       
    end if
    

    if (rank == 0) then
       open(3, file = file_cost, status = 'old', position = 'append')
       write(3, "(6 G20.12)"), iter, J1, norm2_grad, tau, beta, restart_flag
       close(3)
    end if

    iter = 1
    
    DO WHILE ( (ABS(deltaJ) > OPTIM_TOL) .AND. (iter<=MAX_ITER) )

       restart_flag = 0
       if (rank == 0) then
          print *, "maximization_RCG; main_iter =", iter
       end if


       if (0) then
          
          if (mod(iter,sigma_freq) == 0) then
             sigma = sigma/2.0_pr
             if (rank == 0) print *, "sigma =: ", sigma
          end if
       end if
       
       
!======================================================
!- compute the gradient
!======================================================
       if (rank == 0) then
          print *, "eval_grad_J; main_iter =", iter
       end if

       ! save gradJ for one iteration
       if (1) then
          call compute_gradJ(Uvec0, fix_dt2, 1, gradJ_opt, iter)
          
       else if (0) then
          call save2binary2(Uvec, iter, "fwdTE")
          exit
       else if (0) then
          call read4binary2(iter, Uvec, "fwdTE")
          call compute_gradJ(Uvec0, fix_dt2, 1, gradJ_opt, iter)
          call save2binary2(gradJ_opt, iter+1, "fwdTE")
          exit
       else if (0) then
          call read4binary2(iter+1, gradJ_opt, "fwdTE")
          
       end if
       
       if (iter == 1 .or. mod(iter, restart_freq) == 0) then
          restart_flag = 3
          beta = 0.0_pr          
          call projection(Uvec0, gradJ_opt, d_opt, norm2_grad)
          if (rank == 0) then
             print *, "norm2_grad = ", norm2_grad
          end if
          
          d1_opt = gradJ_opt
          ! save intermediate results
          if (0) then
             ! norm2_grad_pre = 1.0_pr
             call read4binary2(iter+2, gradJ_pre_opt, "fwdTE")
             call read4binary2(iter+3, d1_opt, "fwdTE")
             call projection_RCG(Uvec0, gradJ_opt, gradJ_pre_opt, d1_opt, d_opt, norm2_grad, norm2_grad_pre, beta, 2, restart_flag)
             call save2binary2(gradJ_opt, iter+2, "fwdTE")
             call save2binary2(d1_opt, iter+3, "fwdTE")
             if (rank == 0) then
                print *, "norm2_grad = ", norm2_grad
             end if
          else if (1) then
             call save2binary2(gradJ_opt, iter+2, "fwdTE")
             call save2binary2(d1_opt, iter+3, "fwdTE")
             
          end if
       else
          call projection_RCG(Uvec0, gradJ_opt, gradJ_pre_opt, d1_opt, d_opt, norm2_grad, norm2_grad_pre, beta, 2, restart_flag)
       
          
       end if
       
       

       

!======================================================
!- maximiaztion using mnbrak and brent
!======================================================

       
       if (rank==0) then
          print *, "Start mnbrak; main_iter =", iter
       end if
       call rescale(Uvec0, val1)
       tau_brack = mnbrak("maxET", Uvec0, d_opt, tau_brack(1), tau_brack(2), mnbrak_flag, iter)  
       IF (mnbrak_flag /= 0) THEN
          if (rank ==0) then
             print *, "mnbrack iteration beyond maximum, the maxdEdt stops iterating ... " , mnbrak_flag
          end if          
          CALL optim_error_handle(mnbrak_flag)  
       ELSE
          CALL optim_msg_handle(21)
       END IF
       if (rank==0) then
          print *, "Start brent; main_iter =", iter
       end if
       tau = brent(iter, "maxET", Uvec0, d_opt, tau_brack)
       tau_brack(1) = 0.0_pr
       tau_brack(2) = 2.0_pr*tau
       IF (tau == TAU_MAX) THEN
          CALL optim_msg_handle(32)
       END IF
!======================================
! UPDATE (initial condition) VELOCITY
!======================================
       Uvec0 = Uvec0 + tau*d_opt
       J0 = J1
       gradJ_pre_opt = gradJ_opt
       norm2_grad_pre = norm2_grad
       
       
!======================================================
! UPDATE cost 
!======================================================

       if (rank == 0) then
          print *, "eval_J; main_iter =", iter
       end if
       
       J1 = compute_J(Uvec0, fix_dt1, 1, iter, 1)
       if (1) then
          call save2binary2(Uvec, iter, "fwdTE")
       end if
       
          

       IF (iter > 0) THEN   ! Feb 17, 2018
          deltaJ = abs(J1-J0)/ABS(J0)
          IF (J1-J0 > -1.0e-15_pr) THEN 
             CALL optim_msg_handle(0)
             EXIT
          ELSEIF (deltaJ<OPTIM_TOL) THEN
             if (rank == 0) then
                open(3, file = file_cost, status = 'old', position = 'append')
                write(3, "(6 G20.12)"), iter, J1, norm2_grad, tau, beta, restart_flag
                close(3)
                PRINT *, "Relative difference reaches tolerance, iteration exit ... ..."
             end if
             EXIT
          END IF
       END IF

       if (rank == 0) then
          open(3, file = file_cost, status = 'old', position = 'append')
          write(3, "(6 G20.12)"), iter, J1, norm2_grad, tau, beta, restart_flag
          close(3)
       end if
       iter = iter + 1
    END DO
   
  END SUBROUTINE maximization_RCG


  
!================================================= 
! SUBROUTINE: projection(myfield, gradJ, norm2)
! project gradJ to the tangent space of myfield
! gradJ: L2 gradient
! norm2: <gradJ, gradJ>_Gsimga 
! USE: solvers.f90: temp1_solver, temp1_solver_cx, temp2_solver_cx      
!=================================================
      SUBROUTINE projection(myfield, gradJ, dJ_actual, norm2)
        USE global_variables
        USE fftwfunction
        USE function_ops
        USE solvers
        IMPLICIT NONE
        INCLUDE "mpif.h"
        REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(in) :: myfield
        REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(inout) :: gradJ, dJ_actual
        real(pr), intent(out):: norm2
        Real(pr) :: norm, val

        ! temp1_solver_cx = |D|^6 u_cx/norm
        
        call fftfwd_m(myfield, temp1_solver_cx, 3)
        call abs_deriv_fourier(temp1_solver_cx, temp1_solver_cx, 6.0_pr)

        ! temp2_solver_cx = n(u)_cx/norm
        ! = (1+l^2|D|^2)^(-s)exp(-2sigma|D|)|D|^6u_cx/norm
        
        call G_l_s_sigma_fourier(temp1_solver_cx, temp2_solver_cx, l, -s, -2.0_pr*sigma)
        
        
        
        call L2_product_fourier(temp1_solver_cx, temp2_solver_cx, norm)
        norm = sqrt(norm)
        temp1_solver_cx = temp1_solver_cx/norm


        

        ! temp1_solver_cx = n(u)_cx/norm
        ! = (1+l^2|D|^2)^(-s/2)exp(-sigma|D|)|D|^6u_cx/norm
        call G_l_s_sigma_fourier(temp1_solver_cx, temp1_solver_cx, l, -0.5_pr*s, -sigma)

        call fftbwd_m(temp1_solver_cx, temp2_solver, 3)

        ! temp2_solver_cx = (1+l^2|D|^2)^(-s/2)exp(-sigma|D|)gradJ(L2)
        call fftfwd_m(gradJ, temp2_solver_cx, 3)
        call G_l_s_sigma_fourier(temp2_solver_cx, temp2_solver_cx, l, -0.5_pr*s, -sigma)
        call L2_product_fourier(temp2_solver_cx,temp1_solver_cx,val)
        temp2_solver_cx = temp2_solver_cx - val*temp1_solver_cx


        call div_free_fourier(temp2_solver_cx)
        
        
        call fftbwd_m(temp2_solver_cx, gradJ, 3)
        call L2_product(gradJ, gradJ, norm2)

        call L2_product(gradJ, temp2_solver, val)
        if (rank == 0) print *, "gradJ: inner product = ", val

        
        ! temp1_solver_cx = (1+l^2|D|^2)^(-s)exp(-2sigma|D|)gradJ(L2)
        call G_l_s_sigma_fourier(temp2_solver_cx, temp1_solver_cx, l, -0.5_pr*s, -sigma)
     
        call fftbwd_m(temp1_solver_cx, dJ_actual, 3)

        
        call rescale(dJ_actual, val)
        

       
 
        
        
      END SUBROUTINE projection



!================================================= 
! SUBROUTINE: projection_RCG(myfield, gradJ, dJ, norm2, norm2_pre)
! vector transportation      
! project gradJ to the tangent space of the submanifold
! project dJ to the tangent space
! compute the conjugate gradient       
! USE: solvers.f90: temp1_solver, temp1_solver_cx, temp2_solver_cx   
!=================================================
      SUBROUTINE projection_RCG(myfield, gradJ, gradJ_pre, dJ, dJ_actual, norm2, norm2_pre, beta_, RCG_flag, restart_flag_)
        USE global_variables
        USE fftwfunction
        USE function_ops
        USE solvers
        IMPLICIT NONE
        INCLUDE "mpif.h"
        REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(in) :: myfield
        REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(inout) :: gradJ, gradJ_pre, dJ, dJ_actual
        real(pr), intent(out) :: norm2, beta_
        real(pr), intent(in) :: norm2_pre
        integer, intent(in) :: RCG_flag
        integer, intent(out) :: restart_flag_
        Real(pr) :: norm, val, val2
        real(pr) :: tol = 0.1_pr

        ! RCG_flag =  1: feltcher-reeves
        ! RCG_flag =  2: polak-ribiere 

        ! temp1_solver_cx = |D|^6 u_cx/norm
        
        call fftfwd_m(myfield, temp1_solver_cx, 3)
        call abs_deriv_fourier(temp1_solver_cx, temp1_solver_cx, 6.0_pr)

        ! temp2_solver_cx = n(u)_cx/norm
        ! = (1+l^2|D|^2)^(-s)|D|^6u_cx/norm
        
        call G_l_s_sigma_fourier(temp1_solver_cx, temp2_solver_cx, l, -s, -2.0_pr*sigma)
        
        
        call L2_product_fourier(temp1_solver_cx, temp2_solver_cx, norm)
        norm = sqrt(norm)
        temp1_solver_cx = temp1_solver_cx/norm
        
        call fftbwd_m(temp1_solver_cx, temp1_solver, 3)
        

        ! temp1_solver_cx = n(u)_cx/norm
        ! = (1+l^2|D|^2)^(-s/2)exp(-sigma|D|)|D|^6u_cx/norm
        call G_l_s_sigma_fourier(temp1_solver_cx, temp1_solver_cx, l, -0.5_pr*s, -sigma)

        call fftbwd_m(temp1_solver_cx, temp2_solver, 3)
      
        
 
        ! project gradJ_pre
        call L2_product(gradJ_pre, temp2_solver, val)
        gradJ_pre = gradJ_pre - val*temp2_solver

        call L2_product(gradJ_pre, temp2_solver, val)
        if (rank == 0) print *, "gradJpre: inner product = ", val

        ! project dJ
        call L2_product(dJ, temp2_solver, val)
        dJ = dJ - val*temp2_solver
       
        call L2_product(dJ, temp2_solver, val)
        if (rank == 0) print *, "dJ: inner product = ", val
       

        

        

        ! temp2_solver_cx = (1+l^2|D|^2)^(-s/2)exp(-sigma|D|)gradJ(L2)
        call fftfwd_m(gradJ, temp2_solver_cx, 3)
        call G_l_s_sigma_fourier(temp2_solver_cx, temp2_solver_cx, l, -0.5_pr*s, -sigma)
        call L2_product_fourier(temp2_solver_cx,temp1_solver_cx,val)
        temp2_solver_cx = temp2_solver_cx - val*temp1_solver_cx

        call div_free_fourier(temp2_solver_cx)
        call fftbwd_m(temp2_solver_cx, gradJ, 3)
        call L2_product(gradJ, gradJ, norm2)
                
        call L2_product(gradJ, temp2_solver, val)
        if (rank == 0) print *, "gradJ: inner product = ", val



       
        ! RCG_flag =  1: feltcher-reeves
        ! RCG_flag =  2: polak-ribiere 


        if (RCG_flag == 1) then

           beta_ = norm2/norm2_pre
           dJ = gradJ + beta_*dJ
              
        else
           temp2_solver = gradJ - gradJ_pre
           call L2_product(temp2_solver, gradJ, val)
           beta_ = val/norm2_pre
           dJ = gradJ + beta_*dJ
        end if
           

        
        call L2_product(dJ, gradJ, val)
        call L2_product(dJ, dJ, val2)

        
        if (val/sqrt(norm2)/sqrt(val2) < 1e-6_pr) then
           beta_ = 0.0_pr
           dJ = gradJ
           restart_flag_ = 1
        end if

     
        
        
        
        call fftfwd_m(dJ, temp1_solver_cx, 3)
        call G_l_s_sigma_fourier(temp1_solver_cx, temp1_solver_cx, l, -0.5_pr*s, -sigma)
       
        call fftbwd_m(temp1_solver_cx, dJ_actual, 3)

        call rescale(dJ_actual, val)

        
        
           
           
        

        
        
     END SUBROUTINE projection_RCG
      
!=========================================================
! SUBROUTINE rescale(myfield, val)
! scale myfield such that its dotH_3 is norm_constr     
!=========================================================
     SUBROUTINE rescale(myfield, scaling)
       USE global_variables
       USE fftwfunction
       USE function_ops
       USE solvers
       IMPLICIT NONE
       INCLUDE "mpif.h"
       REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(inout) :: myfield
       real(pr) :: norm
       real(pr), intent(out) :: scaling
       norm = 0.0_pr
       call fftfwd_m(myfield, temp1_solver_cx, 3)
       call abs_deriv_fourier(temp1_solver_cx, temp1_solver_cx, 3.0_pr)
       call div_free_fourier(temp1_solver_cx)
       call L2_product_fourier(temp1_solver_cx, temp1_solver_cx, norm)
       scaling = norm_constr/sqrt(norm)
       myfield = myfield*norm_constr/sqrt(norm)
       CALL MPI_BARRIER(MPI_COMM_WORLD,Statinfo)
       if(rank == 0) print *, "norm = ", norm, "norm_constr = ", norm_constr

     END SUBROUTINE rescale

!=========================================================
! SUBROUTINE rescale_cx(myfield_cx)
! scale myfield such that its dotH_3 is norm_constr     
!=========================================================
     SUBROUTINE rescale_cx(myfield_cx, scaling)
       USE global_variables
       USE fftwfunction
       USE function_ops
       USE solvers
       IMPLICIT NONE
       INCLUDE "mpif.h"
       complex(pr), DIMENSION(1:n(1)/2+1,1:n(2),1:local_N,1:3), INTENT(inout) :: myfield_cx
       real(pr), intent(out) :: scaling
       real(pr) :: norm
       
       norm = 0.0_pr
       call abs_deriv_fourier(myfield_cx, myfield_cx, 3.0_pr)
       call div_free_fourier(temp1_solver_cx)
       call L2_product_fourier(myfield_cx, myfield_cx, norm)
       call abs_deriv_fourier(myfield_cx, myfield_cx, -3.0_pr)
       scaling = norm_constr/sqrt(norm)
       myfield_cx = myfield_cx*scaling
       CALL MPI_BARRIER(MPI_COMM_WORLD,Statinfo)
       !if(rank == 0) print *, "norm = ", norm, "norm_constr = ", norm_constr

     END SUBROUTINE rescale_cx

!=========================================================
! FUNCTION compute_PHI_L2(myfield, mydt, savesign, myiter, constr_flag)
! Compute the cost function PHI = (||GRAD u(T)||_L^2)^2
! INPUT: myfield (u(0))
!=========================================================
     FUNCTION compute_PHI_L2(myfield, mydt, savesign, myiter, constr_flag) RESULT(PHI)
       USE global_variables
       USE fftwfunction
       USE function_ops
       use solvers
       IMPLICIT NONE
       INCLUDE "mpif.h"
       REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), intent(inout) :: myfield
       REAL(pr), INTENT(IN) :: mydt
       integer, INTENT(IN) :: savesign, myiter, constr_flag
       real(pr) :: val


       REAL(pr) :: PHI

       PHI = 0.0_pr

       if(constr_flag .ne. 0) call rescale(myfield, val)       

       call fwd_3D(myfield, mydt, savesign, stepper_opt, myiter)
       call fftfwd_m(Uvec, temp1_solver_cx, 3)
       call L2_grad(temp1_solver_cx,PHI)
       PHI = PHI**2

     END FUNCTION compute_PHI_L2
   
!=========================================================
! FUNCTION compute_J(myfield, mydt, savesign, myiter, constr_flag)     
! Compute the cost function J = -1/2(||u(T)||_dotH^3)^2
! Trying to minimize J, which is equivalent to maximize -J
! INPUT: myfield (u(0))     
! Usage: solvers.f90 temp1_solver_cx
!=========================================================
     FUNCTION compute_J(myfield, mydt, savesign, myiter, constr_flag) RESULT (J)
       USE global_variables
       USE fftwfunction
       USE function_ops
       use solvers
       IMPLICIT NONE
       INCLUDE "mpif.h"
       REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), intent(inout) :: myfield
       REAL(pr), INTENT(IN) :: mydt
       integer, INTENT(IN) :: savesign, myiter, constr_flag
       real(pr) :: val


       REAL(pr) :: J
      

       J = 0.0_pr

       
       if(constr_flag .ne. 0) call rescale(myfield, val)
          
       call fwd_3D(myfield, mydt, savesign, stepper_opt, myiter)
       call fftfwd_m(Uvec, temp1_solver_cx, 3)
       call abs_deriv_fourier(temp1_solver_cx, temp1_solver_cx, 3.0_pr)
       call L2_product_fourier(temp1_solver_cx, temp1_solver_cx, J)
       J = -J
       
    END FUNCTION compute_J


!================================================
! SUBROUTINE: compute_gradJ(myfield, mydt, savesign, gradJ, myiter)
! INPUT: inifield (u(T))
! OUTPUT: gradJ
! We use the Hsigma space
! Use: solvers.f90: temp1_solver_cx
!================================================
    SUBROUTINE compute_gradJ(myfield, mydt, savesign, gradJ, myiter)
      USE global_variables
      USE fftwfunction
      USE function_ops
      use solvers
      IMPLICIT NONE
      INCLUDE "mpif.h"
      REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(IN) :: myfield
      REAL(pr), INTENT(IN) :: mydt
      REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(OUT) :: gradJ
      INTEGER, INTENT(IN) :: savesign, myiter

      
      call bkd_3D(adj_Uvec0, mydt, savesign, stepper_opt, myiter)
      gradJ = adj_Uvec
      CALL MPI_BARRIER(MPI_COMM_WORLD,Statinfo)
      
    END SUBROUTINE compute_gradJ
      
!=========================================================
! SUBROUTINE: report_J(myfield,tau_brack, count, mydt)
! compute the cost function along myfield + tau*gradJ
! Use Uvec
!==========================================================
    SUBROUTINE report_J(myfield,tau_brack, count, mydt)
      USE global_variables
      USE data_ops
      USE function_ops
      USE solvers
      IMPLICIT NONE
      INCLUDE "mpif.h"
      REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(INOUT) :: myfield
      REAL(pr), DIMENSION(1:3) :: tau_brack
      integer, intent(in) :: count
      real(pr), intent(in) :: mydt
      character(200) :: file_name
      Real(pr) :: A, B, tau, dtau
      integer :: i
      Real(pr) :: J
      real(pr) :: norm2

      A = MIN(tau_brack(1),tau_brack(2))
      B = MAX(tau_brack(1),tau_brack(2))
      dtau = (B-A)/count
      
      if (rank == 0) then
         file_name = TRIM(scratch_pathname)//"report_cost"//".dat"
         OPEN(10, FILE = file_name, STATUS = 'REPLACE')
         close(10)
      end if

      J = 0.0_pr
      J = compute_J(myfield, fix_dt1, 1, 1, 1)

      call compute_gradJ(myfield, fix_dt2, 0, gradJ_opt, 1)
      CALL MPI_BARRIER(MPI_COMM_WORLD,Statinfo)
      
      
      
      do i = 0,count
         J = 0.0_pr
         tau = A + i*dtau
         Uvec = myfield + tau*gradJ_opt
         !Uvec = tau*gradJ_opt
         !Uvec = myfield
         !Uvec = Uvec + tau*gradJ_opt
         CALL MPI_BARRIER(MPI_COMM_WORLD,Statinfo)
         J = compute_J(Uvec, fix_dt1, 1, i,  1)
         if (rank == 0) then
            open(10, file = file_name, status = 'old', position = 'append')
            write(10, "(2 G20.12)"), tau, -J 
         end if
      end do
      
            
      close(10)
         
    END SUBROUTINE report_J
         
      
      
       
      
!================================================
! FUNCTION: mnbrak(mysystem, myfield, gradJ, tA0, tB0, myflag, myindex)      
! OUTPUT: tau_brack(2)
! Use: Uvec
!================================================
    FUNCTION mnbrak(mysystem, myfield, gradJ, tA0, tB0, myflag, myindex) RESULT (tau_brack)
      USE global_variables
      USE fftwfunction
      USE data_ops
      USE function_ops
      USE solvers
      IMPLICIT NONE
      CHARACTER(len=*), INTENT(IN) :: mysystem
      REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(IN) :: myfield
      REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(IN) :: gradJ
      REAL(pr), INTENT(IN) :: tA0, tB0
      INTEGER, INTENT(INOUT) :: myflag
      INTEGER, INTENT(IN) :: myindex
      REAL(pr), DIMENSION(1:3) :: tau_brack
      REAL(pr) :: aux, tP, FP, Pmax, R, Q
      REAL(pr) :: FA, FB, FC, tA, tB, tC
      REAL(pr), PARAMETER :: GOLD = (1.0_pr + SQRT(5.0_pr))/2.0_pr
      REAL(pr), PARAMETER :: CGOLD = 1.0_pr/GOLD  
      REAL(pr), PARAMETER :: GLIMIT = 10.0_pr
      REAL(pr), PARAMETER :: tMAX = 10.0_pr
      INTEGER, PARAMETER :: ITMAX = 200       ! maximal iterations in mnbrak method
      INTEGER :: FuncEval, iter 
      LOGICAL :: saveLineMin
      CHARACTER(100) :: filename1
      CHARACTER(5) :: itertxt

      Real(pr) ::J_val
      
      REAL(pr), PARAMETER :: mnbrak_TOL = 1E-10  ! Mar 3, 2018
      WRITE(itertxt, '(i4)') myindex
      filename1 = "./LOGFILES/maxET_brakbrent_OPT"//trim(adjustl(itertxt))//".dat"

      saveLineMin = .TRUE.

      J_val = 0.0_pr

      FuncEval    = 0
      iter        = 0
      tA = tA0
      tB = MAX(tB0, MACH_EPSILON)
      ! tA0 = 0
      !Uvec = myfield + tA*gradJ
      !FA = compute_J(Uvec, fix_dt1, 0, myindex, 1)
      
      call fftfwd_m(Uvec, temp1_solver_cx, 3)
      call abs_deriv_fourier(temp1_solver_cx, temp1_solver_cx, 3.0_pr)
      call L2_product_fourier(temp1_solver_cx, temp1_solver_cx, FA)
      FA =  -FA
      
      if (rank==0) then
         OPEN(10, FILE = filename1, FORM = 'FORMATTED', STATUS = 'REPLACE') 
         WRITE(10, "(G20.12, G20.12)") tA, FA
         CLOSE(10)
      end if

      FuncEval = FuncEval+1

      Uvec = myfield + tB*gradJ
      FB = compute_J(Uvec, fix_dt1, 0, myindex, 1)
      if (rank==0) then
            OPEN(10, FILE = filename1, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
            WRITE(10, "(G20.12, G20.12)") tB, FB
            CLOSE(10)
         end if
      FuncEval = FuncEval+1

      IF (saveLineMin) CALL save_linemin_data(tA, tB, tC, FA, FB, FC, iter, mysystem, "replace", myindex)


      DO WHILE ((FB > FA) .AND. (tB > MACH_EPSILON) .AND. (abs(FB-FA)/abs(FA) > mnbrak_TOL)) 
         tB = CGOLD*tB/10.0_pr
        Uvec = myfield + tB*gradJ
         if (rank == 0 ) then
            print *, "      mnbrak; do while NO. 1 ... FuncEval =", FuncEval
         end if
         FB = compute_J(Uvec, fix_dt1, 0, myindex, 1)
         if (rank==0) then
            OPEN(10, FILE = filename1, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
            WRITE(10, "(G20.12, G20.12)") tB, FB
            CLOSE(10)
         end if
         FuncEval = FuncEval+1
         IF (saveLineMin) CALL save_linemin_data(tA, tB, tC, FA, FB, FC, iter, mysystem, "append", myindex)
      END DO

      IF ((tB .LE. MACH_EPSILON) .OR. (abs(FB-FA)/abs(FA) .LE. mnbrak_TOL)) THEN
         myflag = 1
         RETURN
      END IF
      tC = GOLD*tB
      Uvec = myfield + tC*gradJ
      FC = compute_J(Uvec, fix_dt1, 0, myindex, 1)
      if (rank==0) then
            OPEN(10, FILE = filename1, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
            WRITE(10, "(G20.12, G20.12)") tC, FC
            CLOSE(10)
         end if
      FuncEval = FuncEval+1
      IF (saveLineMin) CALL save_linemin_data(tA, tB, tC, FA, FB, FC, iter, mysystem, "append", myindex)
      DO WHILE (FB>=FC .AND. iter<ITMAX)
         if (rank == 0 ) then
            print *, "      mnbrak; do while NO. 2 ... mnbrak_iter =", iter
         end if
         iter = iter+1
     
         FuncEval = FuncEval+1
         R = (tB-tA)*(FB-FC)
         Q = (tB-tC)*(FB-FA)
         tP = tB - 0.5_pr*((tB-tC)*Q - (tB-tA)*R)/( SIGN( MAX(ABS(Q-R),MACH_EPSILON), Q-R) )
         Pmax = tB + GLIMIT*(tC-tB)
         IF ( (tB-tP)*(tP-tC)>0 ) THEN
            if (rank == 0) then
               print *, "            mnbrak; do while NO. 2; case 1"
            end if
           Uvec = myfield + tP*gradJ
            FP = compute_J(Uvec, fix_dt1, 0, myindex, 1)
            if (rank==0) then
            OPEN(10, FILE = filename1, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
            WRITE(10, "(G20.12, G20.12)") tP, FP
            CLOSE(10)
         end if
            IF (FP<FC) THEN
               if (rank == 0) then
                  print *, "            mnbrak; do while NO. 2; case 1; sub_case 1"
               end if
               tA = tB
               FA = FB
               tB = tP
               FB = FP
               EXIT 
            ELSEIF (FP>FB) THEN
               if (rank == 0) then
                  print *, "            mnbrak; do while NO. 2; case 1; sub_case 2"
               end if
               tC = tP
               FC = FP
               EXIT
            END IF
            tP = tC + GOLD*(tC-tB)
            Uvec = myfield + tP*gradJ
            FP = compute_J(Uvec, fix_dt1, 0, myindex, 1)
            if (rank==0) then
            OPEN(10, FILE = filename1, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
            WRITE(10, "(G20.12, G20.12)") tP, FP
            CLOSE(10)
         end if
         ELSEIF ( (tC-tP)*(tP-Pmax)>0 ) THEN
            if (rank == 0) then
               print *, "            mnbrak; do while NO. 2; case 2"
            end if
            Uvec = myfield + tP*gradJ
            FP = compute_J(Uvec, fix_dt1, 0, myindex, 1)
            if (rank==0) then
            OPEN(10, FILE = filename1, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
            WRITE(10, "(G20.12, G20.12)") tP, FP
            CLOSE(10)
            end if

            IF (FP<FC) THEN
               tB = tC
               tC = tP
               FB = FC
               FC = FP
               tP = tC+GOLD*(tC-tB)
               Uvec = myfield + tP*gradJ
               FP = compute_J(Uvec, fix_dt1, 0, myindex, 1)
            if (rank==0) then
            OPEN(10, FILE = filename1, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
            WRITE(10, "(G20.12, G20.12)") tP, FP
            CLOSE(10)
         end if
            END IF
        ELSEIF ( (tP-Pmax)*(Pmax-tC)>=0 ) THEN
            if (rank == 0) then
               print *, "            mnbrak; do while NO. 2; case 3"
            end if
            tP = Pmax
            Uvec = myfield + tP*gradJ
            FP = compute_J(Uvec, fix_dt1, 0, myindex, 1)
           if (rank==0) then
            OPEN(10, FILE = filename1, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
            WRITE(10, "(G20.12, G20.12)") tP, FP
            CLOSE(10)
         end if
         ELSE
            if (rank == 0) then
               print *, "            mnbrak; do while NO. 2; case 4"
            end if
            tP = tC + GOLD*(tC-tB)
            Uvec = myfield + tP*gradJ
            FP = compute_J(Uvec, fix_dt1, 0, myindex, 1)
            if (rank==0) then
            OPEN(10, FILE = filename1, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
            WRITE(10, "(G20.12, G20.12)") tP, FP
            CLOSE(10)
         end if
         END IF
         tA = tB
         tB = tC
         tC = tP
         FA = FB
         FB = FC
         FC = FP        
         IF (saveLineMin) CALL save_linemin_data(tA, tB, tC, FA, FB, FC, iter, mysystem, "append", myindex)

        
      END DO

     
      tau_brack(1) = tA
      tau_brack(2) = tC
      tau_brack(3) = tB

      IF (iter .GE. ITMAX) THEN
         myflag = 2
      ELSE
         myflag = 0
      END IF

    END FUNCTION mnbrak


!====================================================
! FUNCTION: brent(iteration, mysystem, myfield, gradJ, tau_brack)     
! OUTPUT: X
! Use: Uvec
!====================================================
    FUNCTION brent(iteration, mysystem, myfield, gradJ, tau_brack) RESULT (X)  
      USE global_variables
      USE data_ops
      USE function_ops
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: iteration
      CHARACTER(len=*), INTENT(IN) :: mysystem
      REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(IN) :: myfield, gradJ
      REAL(pr), DIMENSION(1:3), INTENT(IN) :: tau_brack
      REAL(pr) :: X
      REAL(pr) :: X_old, FX_old                 ! Mar 3, 2018
      REAL(pr) :: X_err = 1E-2, FX_err = 1E-5   ! Mar 3, 2018
   
      REAL(pr) :: D, A, B, V, W, E, ETEMP, P, Q, R, U, XM
      REAL(pr) :: FV, FW, FU, FX
      REAL(pr) :: TOL1, TOL2
      INTEGER :: FLAG, j
      INTEGER, PARAMETER :: ITMAX = 200       ! Maximal iterations in brent method
      !REAL(pr), PARAMETER :: TOL = 1E-4
      real(pr), parameter :: TOL= 1E-4
      REAL(pr), PARAMETER :: ZEPS = 1E-4
      REAL(pr), PARAMETER :: CGOLD = .381966
      CHARACTER(2) :: E0txt
      CHARACTER(5) :: itertxt
      CHARACTER(100) :: filename, filename1 
      WRITE(E0txt, '(i2.2)') E0_index
      WRITE(itertxt, '(i4)') iteration
      filename1 = "./LOGFILES/maxET_brakbrent_OPT"//trim(adjustl(itertxt))//".dat"
       

      D = 0.0_pr
      A = MIN(tau_brack(1),tau_brack(2))
      B = MAX(tau_brack(1),tau_brack(2))
      !V = tau_brack(2)*CGOLD
      D = A
      V = tau_brack(3)
      !V = (1-CGOlD)*A + CGOLD*B
      !V = B*CGOLD
      W = V
      X = V 
      E = 0.0_pr
     ! Uvec = myfield + D*gradJ
     ! FX = compute_J(Uvec, fix_dt1, 1, iteration, 1)
      if (rank==0) then
         filename = "./LOGFILES/maxET_brent_OPT"//trim(adjustl(itertxt))//".dat"
         OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'REPLACE')
         WRITE(10,*) "# Tau J" 
        ! WRITE(10, "(G20.12, G20.12)") D, FX
         CLOSE(10)
      end if
      
      !if (rank==0) then
      !      OPEN(10, FILE = filename1, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
      !      WRITE(10, "(G20.12, G20.12)") D, FX
      !      CLOSE(10)
      !   end if
      Uvec = myfield + X*gradJ
      FX = compute_J(Uvec, fix_dt1, 0, iteration, 1)
       if (rank==0) then
            OPEN(10, FILE = filename1, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
            WRITE(10, "(G20.12, G20.12)") X, FX
            CLOSE(10)
         end if
      FV = FX 
      FW = FX
      !X_old  = X    ! Mar 3, 2018
      !FX_old = FX   ! Mar 3, 2018

      DO j=1,ITMAX
         
        
         XM = 0.5_pr*(A+B)
         TOL1 = TOL*ABS(X)+ZEPS
         TOL2 = 2.0_pr*TOL1
    
         IF ( ABS(X-XM) <= (TOL2-0.5*(B-A)) ) EXIT
    
         FLAG = 1
         IF ( ABS(E) > TOL1 ) THEN
            R = (X-W)*(FX-FV)
            Q = (X-V)*(FX-FW)
            P = (X-V)*Q - (X-W)*R
            Q = 2.0_pr*(Q-R)
            IF ( Q > 0.0_pr ) P = -P 
            Q = ABS(Q)
            ETEMP = E
            E = D
            IF ( (ABS(P) >= ABS(0.5_pr*Q*ETEMP)) .OR. (P <= Q*(A-X)) .OR. (P >= Q*(B-X)) ) THEN
               FLAG = 1
            ELSE
               FLAG = 2
            END IF
            if (rank==0) then
               print *, "      brent; do iteration; case 1; FLAG =", FLAG
            end if
         END IF

         SELECT CASE (FLAG)
            CASE (1)
               IF (X >= XM) THEN
                  if (rank==0) then
                     print *, "      brent; do iteration; case 2 ... "
                  end if
                  E = A-X
               ELSE
                  if (rank==0) then
                     print *, "      brent; do iteration; case 3 ... "
                  end if
                  E=B-X
               END IF
               D = CGOLD*E
            CASE (2)
               if (rank==0) then
                  print *, "      brent; do iteration; case 4 .. "
               end if
               D = P/Q
               U = X+D
               IF ( (U-A < TOL2) .OR. (B-U < TOL2) ) D = SIGN(TOL1, XM-X)
         END SELECT
    
         IF ( ABS(D) >= TOL1 ) THEN
            U = X+D
         ELSE
            U = X + SIGN(TOL1,D)
         END IF
    
         Uvec = myfield + U*gradJ
         FU = compute_J(Uvec, fix_dt1, 0, iteration, 1)
          if (rank==0) then
            OPEN(10, FILE = filename1, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
            WRITE(10, "(G20.12, G20.12)") U, FU
            CLOSE(10)
         end if
 
         IF ( FU <= FX ) THEN
            IF ( U >= X ) THEN
               if (rank==0) then
                  print *, "      brent; do iteration; case 5 ... "
               end if
               A = X
            ELSE
               if (rank==0) then
                  print *, "      brent; do iteration; case 6 ... "
               end if
               B = X 
            END IF
            V = W
            FV = FW
            W = X
            FW = FX
            X = U
            FX = FU
         ELSE
            IF ( U < X ) THEN
               if (rank==0) then
                  print *, "      brent; do iteration; case 7 ... "
               end if
               A = U
            ELSE
               if (rank==0) then
                  print *, "      brent; do iteration; case 8 ... "
               end if
               B = U
            END IF
        
            IF ( (FU <= FW) .OR. (W == X) ) THEN
               if (rank==0) then
                  print *, "      brent; do iteration; case 9 ... "
               end if
               V = W
               FV = FW
               W = U
               FW = FU
            ELSEIF ( (FU <= FV) .OR. (V==X) .OR. (V==W)) THEN 
               if (rank==0) then
                  print *, "      brent; do iteration; case 10 ... "
               end if
               V = U
               FV = FU
            END IF
         END IF

         if (rank==0) then
            print *, "      brent; do iteration j =", j
            OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
            WRITE(10, "(G20.12, G20.12)") X, FX
            CLOSE(10)
         end if

         !X_old  = X
         !FX_old = FX
      END DO
      
   
     END FUNCTION brent






 
END MODULE
