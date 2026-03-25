MODULE adj_solvers
   IMPLICIT NONE
   CONTAINS
      !==================================================
      ! Set initial condition for the adjoint problem;
      ! The initial condition is: 0;
      !==================================================
      SUBROUTINE adj_initial_condition(myfield,myindex,OPTindex)
         USE global_variables
         use function_ops
         USE data_ops
         use databinary_handle
         IMPLICIT NONE
         INCLUDE "mpif.h"
         REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(IN) :: myfield
         integer, INTENT(IN) :: myindex
         integer, INTENT(IN) ::OPTindex
         REAL(pr), DIMENSION(:,:,:,:), ALLOCATABLE :: aux
          REAL(pr), DIMENSION(:,:), ALLOCATABLE :: Spectrum
         ALLOCATE( aux(1:n(1),1:n(2),1:local_N,1:3) )
         ALLOCATE( Spectrum(1:n(1)/2,1:2) )
         !call read4binary( myindex, aux, "fwdTE" )
         aux       = myfield
         !fwd_Field = aux
         ! CALL laplacian( aux )
         adj_Uvec = 0.0_pr
         CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
         CALL calculate_spectral_data(adj_Uvec, Spectrum)
         IF (rank==0) THEN
            CALL save_spectral_data(Spectrum, 0, "before_CUT", OPTindex)
         END IF
         call filter_ADJnoise(Uvec,adj_Uvec)   ! adj_Uvec is filtered.
         CALL calculate_spectral_data(adj_Uvec, Spectrum)
         IF (rank==0) THEN
            CALL save_spectral_data(Spectrum, 0, "after_CUT", OPTindex)
         END IF
         DEALLOCATE( aux )
      END SUBROUTINE adj_initial_condition

      !==================================================
      ! Start the Navier-Stokes Time Evolution solver;
      !==================================================
      SUBROUTINE adj_bwd_NS3D(inifield,mydt,myindex)
         USE global_variables
         USE data_ops
         IMPLICIT NONE 
         REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(IN) :: inifield
         REAL(pr), INTENT(IN) :: mydt
         integer, INTENT(IN) :: myindex
         REAL(pr) :: rk3_tol = 1.0e-3_pr             ! Manually set tolerance
         CALL adj_si_rk3(inifield,mydt,myindex,rk3_tol)   ! Adjoint problem Solver
      END SUBROUTINE adj_bwd_NS3D

      !===================================================
      ! SEMI-IMPLICIT RUNGE-KUTTA OF ORDER 3, WITH 
      ! RICHARDSON EXTRAPOLATION FOR ERROR CONTROL
      !=================================================== 
      SUBROUTINE adj_si_rk3(inifield,mydt,myindex,TOL)
         USE global_variables
         USE fftwfunction
         USE function_ops
         USE data_ops
         USE databinary_handle
         IMPLICIT NONE
         INCLUDE "mpif.h"
         REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(IN) :: inifield
         REAL(pr), INTENT(IN) :: mydt
         integer, INTENT(IN) :: myindex
         REAL(pr), INTENT(IN) :: TOL
         REAL(pr), DIMENSION(:,:), ALLOCATABLE :: Spectrum
         REAL(pr), DIMENSION(:,:,:,:), ALLOCATABLE :: adj_Uvec1, adj_Uvec2
         INTEGER :: adj_Time_iter, fwdindex
         INTEGER :: nn, ii, jj, kk
         REAL(pr) :: dt, dt_max, adj_time, cputime_start, cputime_end 
         REAL(pr) :: local_q1, global_Umax, global_error
         REAL(pr) :: a, b, c, linA
         LOGICAL :: error_too_large
         ALLOCATE( Spectrum(1:n(1)/2,1:2) )
         ALLOCATE( adj_Uvec1(1:n(1),1:n(2),1:local_N,1:3) )
         ALLOCATE( adj_Uvec2(1:n(1),1:n(2),1:local_N,1:3) )
         !===============================================================
         !- Initialize parameters;
         !===============================================================
         adj_Uvec      = inifield
         dt            = mydt   !For simplicity, fixed dt
         adj_Time_iter = iniIndex
         adj_time      = iniTime
         fwdindex      = final_time_iter - adj_Time_iter
         global_error  = 0.0_pr
         CALL calculate_spectral_data(adj_Uvec, Spectrum)
         IF (rank==0) THEN
            CALL save_spectral_data(Spectrum, adj_Time_iter, "bwdADJ", myindex)
         END IF
         !DO WHILE ( adj_Time_iter <= final_time_iter )
         DO WHILE ( adj_Time_iter < final_time_iter-1.0e-12_pr )
            !===============================================================
            !- dt that fits CFL condition;
            !===============================================================
            local_q1 = MAXVAL(ABS(adj_Uvec))
            CALL MPI_ALLREDUCE(local_q1, global_Umax, 1, MPI_REAL8, MPI_MAX, MPI_COMM_WORLD, Statinfo)
            dt_max = 1.0_pr/(global_Umax*REAL(n(1),pr))
            !===============================================================
            !- Two-step Runge-Kutta Method;
            !===============================================================
            adj_Time_iter = adj_Time_iter + 1
            fwdindex      = final_time_iter - adj_Time_iter
            call read4binary( fwdindex, fwd_Field, "fwdTE" )
            adj_Uvec1 = adj_Uvec
            adj_Uvec2 = adj_Uvec
            CALL adj_step_sirk3(adj_Uvec1, fwd_Field, dt, 1)
            CALL adj_step_sirk3(adj_Uvec2, fwd_Field, dt/2.0_pr, 2)
            adj_Uvec = (4.0_pr/3.0_pr)*adj_Uvec2 - (1.0_pr/3.0_pr)*adj_Uvec1
            !===============================================================
            !- save spectrum of adjoint problem;
            !===============================================================
            IF (MODULO(adj_Time_iter, 20) == 0) THEN
               CALL calculate_spectral_data(fwd_Field, Spectrum)
               IF (rank==0) THEN
                  CALL save_spectral_data(Spectrum, adj_Time_iter, "readbinary", myindex)
               END IF
            END IF
            IF (MODULO(adj_Time_iter, 10) == 0) THEN
               CALL calculate_spectral_data(adj_Uvec, Spectrum)
               IF (rank==0) THEN
                  CALL save_spectral_data(Spectrum, adj_Time_iter, "bwdADJ", myindex)
               END IF
            END IF
            !===============================================================
            !- Compute Difference; And update information;
            !===============================================================
            local_q1 = MAXVAL(ABS(adj_Uvec1-adj_Uvec2))
            CALL MPI_ALLREDUCE(local_q1, global_error, 1, MPI_REAL8, MPI_MAX, MPI_COMM_WORLD, Statinfo)
            CALL save_CFL_dt(adj_Time_iter, dt_max, global_error, "bwdADJ", myindex)
            CALL MPI_BARRIER(MPI_COMM_WORLD,Statinfo) 
         END DO
         DEALLOCATE( adj_Uvec1 )
         DEALLOCATE( adj_Uvec2 )
      END SUBROUTINE adj_si_rk3


      !===================================================
      ! One-step and two-step SEMI-IMPLICIT RUNGE-KUTTA 
      !=================================================== 
      SUBROUTINE adj_step_sirk3(U, F, dt, nsteps)
         USE global_variables
         USE fftwfunction
         USE function_ops
         USE data_ops
         IMPLICIT NONE
         INCLUDE "mpif.h"

         REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(INOUT) :: U
         REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(IN) :: F
         REAL(pr), INTENT(IN) :: dt
         INTEGER, INTENT(IN) :: nsteps
         COMPLEX(pr), DIMENSION(:,:,:), ALLOCATABLE :: aux, faux
         COMPLEX(pr), DIMENSION(:,:,:,:), ALLOCATABLE :: u1, r1, r2, r1_lps
         INTEGER :: mm, nn, ii, jj, kk
         REAL(pr) :: a, b, c, d, linA

         ALLOCATE( aux(1:n(1),1:n(2),1:local_N) )
         ALLOCATE( faux(1:n(1),1:n(2),1:local_N) )
         ALLOCATE( u1(1:n(1),1:n(2),1:local_N,1:3) )
         ALLOCATE( r1(1:n(1),1:n(2),1:local_N,1:3) )       
         ALLOCATE( r2(1:n(1),1:n(2),1:local_N,1:3) )
	 ALLOCATE( r1_lps(1:n(1),1:n(2),1:local_N,1:3) )
	 

         DO nn=1,3
            aux = CMPLX(U(:,:,:,nn), 0.0_pr,kind=8)
            CALL fftfwd(aux, faux)
            u1(:,:,:,nn) = faux
         END DO
         DO mm = 1, nsteps
            !- Evaluate the first stage
            a = 4.0_pr*dt/15.0_pr
            b = 2.0_pr*a
            call adj_RHS_sirk3(U, F, r1)
	    call adj_LPS(F,r1_lps)
	    r1 = r1 + r1_lps
            DO nn=1,3
               DO kk=1,local_N
                  DO jj=1,n(2)
                     DO ii=1,n(1)
                        linA = -visc*( K1(ii)**2 + K2(jj)**2 + K3(kk+local_k_offset)**2 )
                        u1(ii,jj,kk,nn) = (1.0_pr + a*linA)/(1.0_pr - a*linA)*u1(ii,jj,kk,nn) + b/(1.0_pr - a*linA)*r1(ii,jj,kk,nn)
                     END DO
                  END DO
               END DO 
            END DO
            CALL div_free_fourier(u1)
            DO nn=1,3
               faux = u1(:,:,:,nn) 
               CALL fftbwd(faux, aux)
               U(:,:,:,nn) = REAL(aux)
            END DO
            !- Evaluate the second stage
            a = dt/15.0_pr
            b = 5.0_pr*dt/12.0_pr
            c = 17.0_pr*dt/60.0_pr
            call adj_RHS_sirk3(U, F, r2)	    
	    r2 = r2 + r1_lps

            DO nn=1,3
               DO kk=1,local_N
                  DO jj=1,n(2)
                     DO ii=1,n(1)
                        linA = -visc*( K1(ii)**2 + K2(jj)**2 + K3(kk+local_k_offset)**2 )
                        u1(ii,jj,kk,nn) = (1.0_pr + a*linA)/(1.0_pr - a*linA)*u1(ii,jj,kk,nn) + b/(1.0_pr - a*linA)*r2(ii,jj,kk,nn) - c/(1.0_pr - a*linA)*r1(ii,jj,kk,nn)
                     END DO
                  END DO
               END DO 
            END DO
            CALL div_free_fourier(u1)
            DO nn=1,3
               faux = u1(:,:,:,nn) 
               CALL fftbwd(faux, aux)
               U(:,:,:,nn) = REAL(aux)
            END DO
            !- Evaluate the third stage
            a = dt/6.0_pr
            b = 3.0_pr*dt/4.0_pr
            c = 5.0_pr*dt/12.0_pr
            CALL adj_RHS_sirk3(U, F, r1)
	    r1 = r1 + r1_lps
            DO nn=1,3
               DO kk=1,local_N
                  DO jj=1,n(2)
                     DO ii=1,n(1)
                        linA = -visc*( K1(ii)**2 + K2(jj)**2 + K3(kk+local_k_offset)**2 )
                        u1(ii,jj,kk,nn) = (1.0_pr + a*linA)/(1.0_pr - a*linA)*u1(ii,jj,kk,nn) + b/(1.0_pr - a*linA)*r1(ii,jj,kk,nn) - c/(1.0_pr - a*linA)*r2(ii,jj,kk,nn)
                     END DO
                  END DO
               END DO 
            END DO
            !- Update the vector fields
            CALL div_free_fourier(u1) 
            DO nn=1,3
               faux = u1(:,:,:,nn)
               CALL fftbwd(faux, aux)
               U(:,:,:,nn) = REAL(aux)
            END DO
         END DO

         DEALLOCATE( aux )
         DEALLOCATE( faux )
         DEALLOCATE( u1 )
         DEALLOCATE( r1 )
         DEALLOCATE( r2 ) 
 	 DEALLOCATE( r1_lps )

      END SUBROUTINE adj_step_sirk3


     !===================================================
     ! NONLINEAR PART OF THE RHS 
     !===================================================
     SUBROUTINE adj_RHS_sirk3(U, fwd_U, rhs_hat)
        USE global_variables
        USE function_ops
        USE fftwfunction
        ! USE data_ops
        IMPLICIT NONE

        REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(IN) :: U
        REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(IN) :: fwd_U   ! Loaded 
        COMPLEX(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(OUT) :: rhs_hat
        REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: aux1, aux2, aux3, aux4, aux5, raux
        COMPLEX(pr), DIMENSION(:,:,:), ALLOCATABLE :: caux, faux
        INTEGER :: nn
        ALLOCATE( aux1(1:n(1),1:n(2),1:local_N) )
        ALLOCATE( aux2(1:n(1),1:n(2),1:local_N) )
        ALLOCATE( aux3(1:n(1),1:n(2),1:local_N) )
        ALLOCATE( aux4(1:n(1),1:n(2),1:local_N) )
        ALLOCATE( aux5(1:n(1),1:n(2),1:local_N) )
        ALLOCATE( raux(1:n(1),1:n(2),1:local_N) )
        ALLOCATE( caux(1:n(1),1:n(2),1:local_N) )
        ALLOCATE( faux(1:n(1),1:n(2),1:local_N) )
        ! Component X-direction
        aux1 = U(:,:,:,1)
        call derivative(aux1, 1)
        raux(:,:,:) =               fwd_U(:,:,:,1)*aux1(:,:,:)*2.0_pr
        aux1 = U(:,:,:,1)
        call derivative(aux1, 2)
        aux2 = U(:,:,:,2)
        call derivative(aux2, 1)
        aux3 = aux1 + aux2
        raux(:,:,:) = raux(:,:,:) + fwd_U(:,:,:,2)*aux3(:,:,:)
        aux1 = U(:,:,:,1)
        call derivative(aux1, 3)
        aux2 = U(:,:,:,3)
        call derivative(aux2, 1)
        aux4 = aux1 + aux2
        raux(:,:,:) = raux(:,:,:) + fwd_U(:,:,:,3)*aux4(:,:,:)
        caux = CMPLX(raux, 0.0_pr,kind=8)
        CALL fftfwd(caux, faux)
        CALL dealiasing_fourier(faux)
        rhs_hat(:,:,:,1) = faux
        ! Component Y-direction
        raux(:,:,:) =               fwd_U(:,:,:,1)*aux3(:,:,:)
        aux1 = U(:,:,:,2)
        call derivative(aux1, 2)
        raux(:,:,:) = raux(:,:,:) + fwd_U(:,:,:,2)*aux1(:,:,:)*2.0_pr
        aux1 = U(:,:,:,2)
        call derivative(aux1, 3)
        aux2 = U(:,:,:,3)
        call derivative(aux2, 2)
        aux5 = aux1 + aux2
        raux(:,:,:) = raux(:,:,:) + fwd_U(:,:,:,3)*aux5(:,:,:)
        caux = CMPLX(raux, 0.0_pr,kind=8)
        CALL fftfwd(caux, faux)
        CALL dealiasing_fourier(faux)
        rhs_hat(:,:,:,2) = faux
        ! Component Z-direction
        raux(:,:,:) =               fwd_U(:,:,:,1)*aux4(:,:,:)
        raux(:,:,:) = raux(:,:,:) + fwd_U(:,:,:,2)*aux5(:,:,:)
        aux1 = U(:,:,:,3)
        call derivative(aux1, 3)
        raux(:,:,:) = raux(:,:,:) + fwd_U(:,:,:,3)*aux1(:,:,:)*2.0_pr
        caux = CMPLX(raux, 0.0_pr,kind=8)
        CALL fftfwd(caux, faux)
        CALL dealiasing_fourier(faux)
        rhs_hat(:,:,:,3) = faux
        ! Do the divergence free operator
        CALL div_free_fourier(rhs_hat)

        DEALLOCATE( aux1 )
        DEALLOCATE( aux2 )
        DEALLOCATE( aux3 )
        DEALLOCATE( aux4 )
        DEALLOCATE( aux5 )
        DEALLOCATE( raux )
        DEALLOCATE( caux )
        DEALLOCATE( faux )
     END SUBROUTINE adj_RHS_sirk3



     SubRoutine adj_LPS(fwd_U,adj_lps_hat)
	 USE global_variables
         USE function_ops
         USE fftwfunction
        
         IMPLICIT NONE
	INCLUDE "mpif.h"
        REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(IN) :: fwd_U  
        COMPLEX(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(OUT) :: adj_lps_hat
        Real(pr) :: Lqn, Lqnglobal
	REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: aux1
	COMPLEX(pr), DIMENSION(:,:,:), ALLOCATABLE :: caux,faux
	Integer :: nn

 	ALLOCATE( aux1(1:n(1),1:n(2),1:local_N) )
	ALLOCATE( faux(1:n(1),1:n(2),1:local_N) )
	ALLOCATE( caux(1:n(1),1:n(2),1:local_N) )

	Lqn = Lp_Norm(fwd_U, Lps_q)
        CALL MPI_ALLREDUCE(Lqn, Lqnglobal, 1, MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, Statinfo) 
	CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
	Do nn=1,3
	     aux1= Lps_p*Lqnglobal**((Lps_p-Lps_q)/Lps_q)*Sqrt(fwd_U(:,:,:,1)**2+fwd_U(:,:,:,2)**2+fwd_U(:,:,:,3)**2)**(Lps_q- 2.0_pr)*fwd_U(:,:,:,nn)
	     caux = CMPLX(aux1, 0.0_pr,kind=8)
	     CALL fftfwd(caux, faux)
             CALL dealiasing_fourier(faux)
             adj_lps_hat(:,:,:,nn) = faux
	End Do

	DEALLOCATE( aux1 )
        DEALLOCATE( caux )
        DEALLOCATE( faux )
     End Subroutine adj_LPS

END MODULE
