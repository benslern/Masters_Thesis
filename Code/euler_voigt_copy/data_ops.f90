MODULE data_ops

  IMPLICIT NONE

  CONTAINS

        !============================
        ! SAVE DIAGNOSTIC FIELDS 
        !============================
        SUBROUTINE save_diagnosticFields(myfield, num_fields, myindex, mynames, mysystem)
          USE global_variables  
          IMPLICIT NONE

          REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:num_fields), INTENT(IN) :: myfield
         INTEGER, INTENT(IN) :: num_fields, myindex
          CHARACTER(len=*), INTENT(IN) :: mynames, mysystem

          CHARACTER(2) :: E0txt
          CHARACTER(4) :: optchar
          CHARACTER(200) :: filename
      
          WRITE(E0txt, '(i2.2)') E0_index
          WRITE(optchar, '(i4)') myindex 
 
          SELECT CASE (mysystem)
            CASE ("maxET")
              !filename = TRIM(work_pathname)//"_E"//E0txt//"_diagnosticFields_fwdTE0_OPT"//trim(adjustl(optchar))//".nc"
              filename = TRIM(scratch_pathname)//"_diagnosticFields_fwdTE0_OPT"//trim(adjustl(optchar))//".nc"
              CALL save_field_R3toRn_ncdf(myfield, num_fields, mynames, filename)
 
          END SELECT 
         
        END SUBROUTINE save_diagnosticFields



        ! Save the minimal vortex magnitude and save the locations.   Add by Di Kang in Aug 2019

	Subroutine save_Wmin( myindex, time, dt,E,Wmin, Wmean,  Wminloc, optindex,iiii) 
           USE global_variables  
         
          USE fftwfunction
          IMPLICIT NONE
          INCLUDE "mpif.h"
           
           INTEGER, INTENT(IN) :: myindex , iiii
           REAL(pr), DIMENSION(1:3), INTENT(IN) :: E
          
  	        
           REAL(pr), INTENT(IN) :: time,  Wmean , Wmin !Wmin is the smallest vortex magnitude
           REAL(pr), INTENT(IN) :: dt        ! Newly added to save time step, on Aug 4, 2017
           INTEGER, INTENT(IN) :: optindex   ! Feb 20, 2018
	   REAL(pr), DIMENSION(1:3),Intent(In) :: Wminloc
           CHARACTER(2) :: K0txt
           CHARACTER(2) :: E0txt
           CHARACTER(2) :: NUtxt
           CHARACTER(4) :: indexchar
           CHARACTER(200) :: filename
           CHARACTER(4) :: optchar
    
           WRITE(K0txt, '(i2.2)') K0_index
           WRITE(E0txt, '(i2.2)') E0_index
           WRITE(NUtxt, '(i2.2)') NU_index
           WRITE(indexchar, '(i4.4)') myindex
           WRITE(optchar, '(i4)') optindex

 
                 !filename = TRIM(work_pathname)//"_E"//E0txt//"_fwdTE_OPT"//trim(adjustl(optchar))//".dat"
                 filename = TRIM(scratch_pathname)//"_Wmin"//trim(adjustl(optchar))//".dat"
                 IF ((myindex==iniIndex) .and. (iiii==1)) THEN
                    OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'REPLACE')
                 ELSE
                    OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
                 END IF
       
	    WRITE(10, "(13 G20.12)") time, SUM(E), Wmin,Wmin/Wmean,wminloc(1)/resol,wminloc(2)/resol,wminloc(3)/resol
	  
           CLOSE(10)
          
        END Subroutine save_Wmin




        !====================================
        ! SAVE GLOBAL DIAGNOSTICS OF FIELDS 
        !====================================
        SUBROUTINE save_diagnosticScalars(mysystem, myindex, time, dt, K, E, dEdt, divU, Umax, Wmax, magUmax, magWmax, H,L3,hnorm, Lqnorm, maxHel, minHel, vorCoreData, optindex,hhalf)
           USE global_variables  
           IMPLICIT NONE

           CHARACTER(len=*), INTENT(IN) :: mysystem
           INTEGER, INTENT(IN) :: myindex
           REAL(pr), DIMENSION(1:3), INTENT(IN) :: K, E, Umax, Wmax
           REAL(pr), DIMENSION(1:4), INTENT(IN) :: vorCoreData
           REAL(pr), DIMENSION(1:2), INTENT(IN) :: dEdt
           REAL(pr), INTENT(IN) :: time, divU, magUmax, magWmax, H, maxHel, minHel, Lqnorm , hhalf , L3, hnorm
           REAL(pr), INTENT(IN) :: dt        ! Newly added to save time step, on Aug 4, 2017
           INTEGER, INTENT(IN) :: optindex   ! Feb 20, 2018

           CHARACTER(2) :: K0txt
           CHARACTER(2) :: E0txt
           CHARACTER(2) :: NUtxt
           CHARACTER(4) :: indexchar
           CHARACTER(200) :: filename
           CHARACTER(4) :: optchar

           WRITE(K0txt, '(i2.2)') K0_index
           WRITE(E0txt, '(i2.2)') E0_index
           WRITE(NUtxt, '(i2.2)') NU_index
           WRITE(indexchar, '(i4.4)') myindex
           WRITE(optchar, '(i4)') optindex

           SELECT CASE (mysystem)
              case("fwdTE")
                 !filename = TRIM(work_pathname)//"_E"//E0txt//"_fwdTE_OPT"//trim(adjustl(optchar))//".dat"
                 filename = TRIM(scratch_pathname)//"_fwdTE_OPT"//trim(adjustl(optchar))//".dat"
                 IF (myindex==iniIndex) THEN
                    OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'REPLACE')
                 ELSE
                    OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
                 END IF
              case("bwdADJ")
                 !filename = TRIM(work_pathname)//"_E"//E0txt//"_bwdADJ_OPT"//trim(adjustl(optchar))//".dat"
                 filename = TRIM(scratch_pathname)//"_bwdADJ_OPT"//trim(adjustl(optchar))//".dat"
                 IF (myindex==final_time_iter) THEN
                    OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'REPLACE')
                 ELSE
                    OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
                 END IF
           END SELECT
        !   WRITE(10, "(7 G20.12)") time, dt, SUM(K), SUM(E), dEdt(1), dEdt(2), H, E(1),E(2),E(3),Lqnorm    ! Modified on Aug 4, 2017, to save less information !Modified on Aug 2019
           !    WRITE(10, "(13 G20.12)") time, SUM(K), SUM(E), E(1),E(2),E(3),Lqnorm, LPSnorm,hnorm,L3, hhalf, LPShalfnorm
           WRITE(10, "(9 G20.12)") time, SUM(K), SUM(E), E(1),E(2),E(3), magUmax, magWmax, H
           CLOSE(10)
           IF (myindex==iniIndex) THEN
              !filename = TRIM(work_pathname)//"_E"//E0txt//"_fwdTE0_diagnosticScalars_OPT"//trim(adjustl(optchar))//".dat"
              filename = TRIM(scratch_pathname)//"_fwdTE0_diagnosticScalars_OPT"//trim(adjustl(optchar))//".dat"
              OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'REPLACE')
              WRITE(10,*) "# K, E, Umax, Wmax, magUmax, magWmax, H, MaxminH, MaxminS "
              WRITE(10,*) "# x   y   z "
              WRITE(10, "(3 G20.12)") dEdt(1), dEdt(2), H
              WRITE(10, "(4 G20.12)") vorCoreData(1), vorCoreData(2), vorCoreData(3), vorCoreData(4)
              WRITE(10, "(3 G20.12)") K(1), K(2), K(3) 
              WRITE(10, "(3 G20.12)") E(1), E(2), E(3) 
              WRITE(10, "(3 G20.12)") Umax(1), Umax(2), Umax(3) 
              WRITE(10, "(3 G20.12)") Wmax(1), Wmax(2), Wmax(3)
              WRITE(10, "(2 G20.12)") magUmax, magWmax
              WRITE(10, "(2 G20.12)") maxHel, minHel 
              CLOSE(10)
           END IF
        END SUBROUTINE save_diagnosticScalars

!        !====================================
!        ! SAVE VORTICITY MOMENTS
!        !====================================
!        SUBROUTINE save_vorticityMoments(myindex, time, Dm)
!           USE global_variables  
!           IMPLICIT NONE

!           INTEGER, INTENT(IN) :: myindex
!           REAL(pr), DIMENSION(1:5), INTENT(IN) :: Dm
!           REAL(pr), INTENT(IN) :: time

!           CHARACTER(2) :: K0txt
!           CHARACTER(2) :: E0txt
!           CHARACTER(2) :: IGtxt
!           CHARACTER(2) :: NUtxt
!           CHARACTER(4) :: indexchar
!           CHARACTER(200) :: filename
!      
!           WRITE(K0txt, '(i2.2)') K0_index
!           WRITE(E0txt, '(i2.2)') E0_index
!           WRITE(NUtxt, '(i2.2)') NU_index
!           WRITE(indexchar, '(i4.4)') myindex 
!          
!           filename = TRIM(scratch_pathname)//"/DNS_NSE_3D/K"//K0txt//"/E"//E0txt//"/"//TRIM(IC_type)//"/vorticityMoments_nu"//NUtxt//".dat"

!           IF (myindex==iniIndex) THEN
!              OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'REPLACE')
!           ELSE
!              OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
!           END IF 
!           WRITE(10, "(6 G20.12)") time, Dm(1), Dm(2), Dm(3), Dm(4), Dm(5) 
!           CLOSE(10)
!  
!        END SUBROUTINE save_vorticityMoments

       !==================================================
        ! SAVE L_inf NORM OF ERROR W.R.T. EXACT SOLUTION 
        !==================================================
        SUBROUTINE save_ExactSol_error(myindex, time, Linf_error)
           USE global_variables  
           IMPLICIT NONE

           INTEGER, INTENT(IN) :: myindex
           REAL(pr), INTENT(IN) :: time
           REAL(pr), DIMENSION(1:4), INTENT(IN) :: Linf_error

           CHARACTER(2) :: K0txt
           CHARACTER(2) :: E0txt
           CHARACTER(2) :: DTtxt
           CHARACTER(4) :: N0txt
           CHARACTER(4) :: indexchar
           CHARACTER(200) :: filename
      
           WRITE(K0txt, '(i2.2)') K0_index
           WRITE(E0txt, '(i2.2)') E0_index
           WRITE(DTtxt, '(i2.2)') DT_index
           WRITE(N0txt, '(i4.4)') n(1)
           WRITE(indexchar, '(i4.4)') myindex 
          
           filename = TRIM(work_pathname)//"_exactSol_error_N"//N0txt//"_DT"//DTtxt//".dat"

           IF (myindex==iniIndex) THEN
              OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'REPLACE')
           ELSE
              OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
           END IF 
           WRITE(10, "(5 G20.12)") time, Linf_error(1), Linf_error(2), Linf_error(3), Linf_error(4)
           CLOSE(10)
  
        END SUBROUTINE save_ExactSol_error

        !=================================
        !    SAVE VELOCITY FROM NS SYSTEM
        !=================================
        SUBROUTINE save_velocity(u_mat, optiter,subpath)
          USE global_variables  
          IMPLICIT NONE
          REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(IN) :: u_mat

          INTEGER, INTENT(IN) :: optiter
          CHARACTER(len=*), INTENT(IN) :: subpath

          CHARACTER(200) :: filename
          CHARACTER(4) :: optchar
 
          WRITE(optchar, '(i4)') optiter

          filename = TRIM(scratch_pathname)//trim(subpath)//"/Uvec_fwdTE_"//trim(adjustl(optchar))//".nc"

          CALL save_field_R3toR3_ncdf2(u_mat, "Ux", "Uy", "Uz", filename, "netCDF")
        END SUBROUTINE save_velocity

        !=================================
        !    SAVE VELOCITY FROM NS SYSTEM
        !=================================
        SUBROUTINE save_velocity_cx(u_cx, myiter)
          USE global_variables  
          IMPLICIT NONE
          INCLUDE "mpif.h"
          
          complex(pr), DIMENSION(1:n(1)/2+1,1:n(2),1:local_N,1:3), INTENT(IN) :: u_cx
          INTEGER, INTENT(IN) :: myiter
          CHARACTER(200) :: filename
          CHARACTER(4) :: optchar
          complex(pr), DIMENSION(:,:,:), allocatable :: aux_cx

          integer :: local_count, j1, j2, j3

          local_count = (n(1)/2+1)*n(2)*local_N
  
          WRITE(optchar, '(i4)') myiter
         
          filename = TRIM(scratch_pathname)//"_Uvec_fwdTE0_OPT_CX_"//trim(adjustl(optchar))//".dat"
          
          if(rank == 0) then
             allocate(aux_cx(1:n(1)/2+1, 1:n(2), 1:n(3)))
          end if
          call mpi_barrier(mpi_comm_world, statinfo)
          call mpi_gather(u_cx(:,:,:,1), local_count, MPI_DOUBLE_COMPLEX, aux_cx, local_count, MPI_DOUBLE_COMPLEX, 0, MPI_COMM_WORLD, statinfo)
          
          if (rank == 0) then
             open(11, file = filename, status = 'replace')
             do j3 = 1,n(3)
                do j2 = 1,n(2)
                   do j1 = 1, n(1)/2+1
                      write(11, "(2 G20.12)") real(aux_cx(j1,j2,j3)), aimag(aux_cx(j1,j2,j3))
                   end do
                end do
             end do
             close(11)
             aux_cx = cmplx(0.0_pr)
          end if
          call mpi_barrier(mpi_comm_world, statinfo)
          call mpi_gather(u_cx(:,:,:,2), local_count, mpi_double_complex, aux_cx, local_count, mpi_double_complex, 0, mpi_comm_world, statinfo)
          
          if (rank == 0) then
             open(11, file = filename, status = 'old', position = 'append')
             do j3 = 1,n(3)
                do j2 = 1,n(2)
                   do j1 = 1, n(1)/2+1
                      write(11, "(2 G20.12)") real(aux_cx(j1,j2,j3)), aimag(aux_cx(j1,j2,j3))
                   end do

                end do
             end do
             close(11)
             aux_cx = cmplx(0.0_pr)
          end if
          call mpi_barrier(mpi_comm_world, statinfo)
          call mpi_gather(u_cx(:,:,:,3), local_count, mpi_double_complex, aux_cx, local_count, mpi_double_complex, 0, mpi_comm_world, statinfo)
          
          if (rank == 0) then
             open(11, file = filename, status = 'old', position = 'append')
             do j3 = 1,n(3)
                do j2 = 1,n(2)
                   do j1 = 1, n(1)/2+1
                      write(11, "(2 G20.12)") real(aux_cx(j1,j2,j3)), aimag(aux_cx(j1,j2,j3))
                   end do
                end do
             end do
             close(11)
             call system('gzip -f '//filename)
             deallocate(aux_cx)
          end if
          call mpi_barrier(mpi_comm_world, statinfo)



          
          
        END SUBROUTINE save_velocity_cx


        !=================================
        !    SAVE VELOCITY FROM NS SYSTEM
        !=================================
        SUBROUTINE save_velocity_cx_mpi(u_cx, myiter)
          USE global_variables  
          IMPLICIT NONE
          INCLUDE "mpif.h"
          
          complex(pr), DIMENSION(1:n(1)/2+1,1:n(2),1:local_N,1:3), INTENT(IN) :: u_cx
          INTEGER, INTENT(IN) :: myiter
          CHARACTER(200) :: filename
          CHARACTER(4) :: optchar
          integer :: file_handle
          integer(kind=mpi_offset_kind) :: my_offset
        
          integer :: local_count
          complex(pr) :: val
          integer :: i, j1, j2, j3, nn
          integer :: ios

          
          WRITE(optchar, '(i4)') myiter
         
          filename = TRIM(scratch_pathname)//"_Uvec_fwdTE0_OPT_CX"//trim(adjustl(optchar))//".bin"



          

     

          my_offset = 3*(n(1)/2+1)*n(2)*int(local_k_offset,mpi_offset_kind)*pr*2

      
          local_count = 3*(n(1)/2+1)*n(2)*local_N*2



          CALL MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_WRONLY + MPI_MODE_CREATE, MPI_INFO_NULL, file_handle, Statinfo)

          CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
!          CALL MPI_File_seek(file_handle, my_offset, MPI_SEEK_SET, Statinfo)
          call MPI_File_set_view(file_handle, my_offset, MPI_DOUBLE_PRECISION, MPI_DOUBLE_PRECISION, "native", MPI_INFO_NULL, Statinfo);
          CALL MPI_File_write_all(file_handle, u_cx, local_count, MPI_DOUBLE_PRECISION, MPI_STATUS_IGNORE, Statinfo)
!          CALL MPI_File_write(file_handle, u_cx, local_count, MPI_DOUBLE_PRECISION, MPI_STATUS_IGNORE, Statinfo)
         
          CALL MPI_File_close(file_handle, Statinfo)



          CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)


          
          
        END SUBROUTINE save_velocity_cx_mpi


        

        !=================================
        !    SAVE VELOCITY FROM NS SYSTEM
        !=================================
        SUBROUTINE save_NS_velocity_bk(u_mat, myindex, myformat, optiter)
          USE global_variables  
          IMPLICIT NONE
          REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(IN) :: u_mat
          INTEGER, INTENT(IN) :: myindex
          CHARACTER(len=*), INTENT(IN) :: myformat
          INTEGER, INTENT(IN) :: optiter
          CHARACTER(2) :: E0txt
          CHARACTER(4) :: indexchar
          CHARACTER(200) :: filename
          CHARACTER(4) :: optchar
          WRITE(E0txt, '(i2.2)') E0_index
          WRITE(indexchar, '(i4.4)') myindex 
          WRITE(optchar, '(i4)') optiter
          !filename = TRIM(work_pathname)//"_E"//E0txt//"_Uvec_fwdTE0_OPT"//trim(adjustl(optchar))//".nc"
          filename = TRIM(scratch_pathname)//"_Uvec_bkdTE0_OPT"//trim(adjustl(optchar))//".nc"
          CALL save_field_R3toR3_ncdf(U_mat(:,:,:,1), U_mat(:,:,:,2), U_mat(:,:,:,3), "Ux", "Uy", "Uz", filename, "netCDF")
          !CALL save_field_R3toRn_ncdf(u_mat, 3, "Ux,Uy,Uz", filename)
        END SUBROUTINE save_NS_velocity_bk

        !============================
        !    SAVE VORTICITY
        !============================
       SUBROUTINE save_vorticity(w_mat, optiter, subpath)
          USE global_variables  
          IMPLICIT NONE
          REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(IN) :: w_mat
          
          INTEGER, INTENT(IN) :: optiter
          CHARACTER(len=*), INTENT(IN) :: subpath
          CHARACTER(200) :: filename
          CHARACTER(4) :: optchar
          WRITE(optchar, '(i4)') optiter
          
          filename = TRIM(scratch_pathname)//trim(subpath)//"/Wvec_fwdTE_"//trim(adjustl(optchar))//".nc"


          CALL save_field_R3toR3_ncdf2(w_mat,"Wx", "Wy", "Wz", filename, "netCDF")
          
          
        END SUBROUTINE save_vorticity

       
        !============================
        !    SAVE H3 seminorm field
        !============================
       SUBROUTINE save_NS_Uob(myfield, myindex, myformat, optiter)
          USE global_variables  
          IMPLICIT NONE
          REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N), INTENT(IN) :: myfield
          INTEGER, INTENT(IN) :: myindex
          CHARACTER(len=*), INTENT(IN) :: myformat
          INTEGER, INTENT(IN) :: optiter
          CHARACTER(4) :: indexchar
          CHARACTER(4) :: optchar
          CHARACTER(200) :: filename

          WRITE(indexchar, '(i4.4)') myindex 
          WRITE(optchar, '(i4)') optiter

          filename = TRIM(scratch_pathname)//"_UOBvec_fwdTE0_OPT"//trim(adjustl(optchar))//".nc"
          CALL save_field_R3toR1_ncdf2(myfield, "Uob", filename)
          
        END SUBROUTINE save_NS_Uob
 
        !=====================================================
        ! SAVE VORTEX CORE
        !=====================================================
        SUBROUTINE save_vortexCore(P, myindex)
          USE global_variables
          IMPLICIT NONE

          REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N), INTENT(IN) :: P
          INTEGER, INTENT(IN) :: myindex

          !REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: myfield

          CHARACTER(2) :: K0txt, E0txt
          CHARACTER(4) :: indexchar
          CHARACTER(200) :: filename

          WRITE(K0txt, '(i2.2)') K0_index
          WRITE(E0txt, '(i2.2)') E0_index
          WRITE(indexchar, '(i4.4)') myindex 

          filename = TRIM(work_pathname)//"_Fields_vortexCore_"//indexchar//".nc"
          CALL save_field_R3toR1_ncdf(P, "w_Core", filename)

        END SUBROUTINE save_vortexCore 

!        !==========================================
!        ! SAVE 2D SCALAR IN netCDF FORMAT
!        !==========================================
!        SUBROUTINE save_field_R2toR1_ncdf(myfield, field_name, file_name)
!          USE global_variables
!          USE netcdf
!          IMPLICIT NONE
!          INCLUDE "mpif.h"

!          REAL(pr), DIMENSION(:,:), INTENT(IN) :: myfield
!          CHARACTER(len=*) :: field_name
!          CHARACTER(len=*) :: file_name
!         
!          REAL(pr), DIMENSION(:,:), ALLOCATABLE :: global_field
!          INTEGER, DIMENSION(1:2) :: starts, counts
!          INTEGER :: ncout, ncid, varid, dimids(2)
!          INTEGER :: x_dimid, y_dimid, z_dimid

!          INTEGER :: ii

!          IF (parallel_data) THEN
!             IF (rank==0) THEN
!                ncout = nf90_create(file_name, NF90_CLOBBER, ncid=ncid)
!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!                ncout = nf90_def_dim(ncid, "x", n(1), x_dimid)
!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!                ncout = nf90_def_dim(ncid, "y", NF90_UNLIMITED, y_dimid)
!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!                dimids = (/ x_dimid, y_dimid /)

!                ncout = nf90_def_var(ncid, TRIM(field_name), NF90_DOUBLE, dimids, varid)
!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

!                ncout = nf90_enddef(ncid)
!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!                ncout = nf90_close(ncid)
!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!             END IF
!             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)

!             starts = (/ 1, rank*local_N+1 /)
!             counts = (/ n(1), local_N /)            
! 
!             !!--------------------------
!             !! START netCDF ROUTINES
!             !!--------------------------
!             DO ii=0,np-1
!                IF (rank==ii) THEN 
!                   ncout = nf90_open(file_name, NF90_WRITE, ncid)
!                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

!                   ncout = nf90_inq_varid(ncid, TRIM(field_name), varid)
!                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!                   ncout = nf90_put_var(ncid, varid, myfield, start = starts, count = counts)
!                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!                   
!                   ncout = nf90_close(ncid)
!                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!                END IF
!                CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo) 
!             END DO
!             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
!          ELSE
!             IF (rank == 0) THEN 
!                ALLOCATE( global_field(1:n(1),1:n(2)) )
!             END IF
!             CALL MPI_GATHER(myfield, n(1)*local_N, MPI_REAL8, global_field, n(1)*local_N, MPI_REAL8, 0, MPI_COMM_WORLD, Statinfo)

!             IF (rank==0) THEN
!                ncout = nf90_create(file_name, NF90_CLOBBER, ncid)
!                ncout = nf90_def_dim(ncid, "x", n(1), x_dimid)
!                ncout = nf90_def_dim(ncid, "y", n(2), y_dimid)
!          
!                dimids =  (/ x_dimid, y_dimid /)

!                ncout = nf90_def_var(ncid, field_name, NF90_DOUBLE, dimids, varid)
!                ncout = nf90_enddef(ncid)
!          
!                ncout = nf90_put_var(ncid, varid, global_field)
!                ncout = nf90_close(ncid)
!             
!                DEALLOCATE( global_field ) 
!             END IF  
!             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)

!          END IF

!        END SUBROUTINE save_field_R2toR1_ncdf


        !==========================================
        ! SAVE 3D SCALAR IN netCDF FORMAT
        !==========================================
        SUBROUTINE save_field_R3toR1_ncdf(myfield, field_name, file_name)
          USE global_variables
          USE netcdf
          IMPLICIT NONE
          INCLUDE "mpif.h"

          REAL(pr), DIMENSION(:,:,:), INTENT(IN) :: myfield
          CHARACTER(len=*) :: field_name
          CHARACTER(len=*) :: file_name
         
          REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: global_field
          INTEGER, DIMENSION(1:3) :: starts, counts
          INTEGER :: ncout, ncid, varid, dimids(3)
          INTEGER :: x_dimid, y_dimid, z_dimid

          INTEGER :: ii

          IF (parallel_data) THEN
             IF (rank==0) THEN
                ncout = nf90_create(file_name, NF90_CLOBBER, ncid=ncid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_def_dim(ncid, "x", n(1), x_dimid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_def_dim(ncid, "y", n(2), y_dimid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_def_dim(ncid, "z", NF90_UNLIMITED, z_dimid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                dimids = (/ x_dimid, y_dimid, z_dimid /)

                ncout = nf90_def_var(ncid, TRIM(field_name), NF90_DOUBLE, dimids, varid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

                ncout = nf90_enddef(ncid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_close(ncid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             END IF
             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)

             starts = (/ 1, 1, rank*local_N+1 /)
             counts = (/ n(1), n(2), local_N /)            
 
             !!--------------------------
             !! START netCDF ROUTINES
             !!--------------------------
             DO ii=0,np-1
                IF (rank==ii) THEN 
                   ncout = nf90_open(file_name, NF90_WRITE, ncid)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

                   ncout = nf90_inq_varid(ncid, TRIM(field_name), varid)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   ncout = nf90_put_var(ncid, varid, myfield, start = starts, count = counts)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   
                   ncout = nf90_close(ncid)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                END IF
                CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo) 
             END DO
             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
          ELSE
             IF (rank == 0) THEN 
                ALLOCATE( global_field(1:n(1),1:n(2),1:n(3)) )
             END IF
             CALL MPI_GATHER(myfield, total_local_size, MPI_DOUBLE_PRECISION, global_field, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)

             IF (rank==0) THEN
                ncout = nf90_create(file_name, NF90_CLOBBER, ncid)
                ncout = nf90_def_dim(ncid, "x", n(1), x_dimid)
                ncout = nf90_def_dim(ncid, "y", n(2), y_dimid)
                ncout = nf90_def_dim(ncid, "z", n(3), z_dimid)
          
                dimids =  (/ x_dimid, y_dimid, z_dimid /)

                ncout = nf90_def_var(ncid, field_name, NF90_DOUBLE, dimids, varid)
                ncout = nf90_enddef(ncid)
          
                ncout = nf90_put_var(ncid, varid, global_field)
                ncout = nf90_close(ncid)
             
                DEALLOCATE( global_field ) 
             END IF  
             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)

          END IF

        END SUBROUTINE save_field_R3toR1_ncdf
         
        !==========================================
        ! SAVE FIELD IN R3
        !==========================================
        SUBROUTINE save_field_R3toR1_ncdf2(myfield, field_name, file_name)
          USE global_variables
          USE netcdf
          IMPLICIT NONE
          INCLUDE "mpif.h"

          REAL(pr), DIMENSION(:,:,:), INTENT(IN) :: myfield
          CHARACTER(len=*) :: field_name
          CHARACTER(len=*) :: file_name
         
          INTEGER, DIMENSION(1:3) :: starts, counts
          INTEGER :: ncout, ncid, varid, dimids(3)
          INTEGER :: x_dimid, y_dimid, z_dimid

          INTEGER :: ii
         
          !REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: global_field

          if (rank == 0) then
             !allocate(global_field(1:n(1), 1:n(2), 1:n(3)))
             global_u = 0.0_pr
          end if
          CALL MPI_GATHER(myfield, total_local_size, MPI_DOUBLE_PRECISION, global_u, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
        

          IF (rank == 0) THEN 
             
             ncout = nf90_create(file_name, NF90_CLOBBER, ncid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_dim(ncid, "x", n(1), x_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_dim(ncid, "y", n(2), y_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_dim(ncid, "z", NF90_UNLIMITED, z_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             
             dimids =  (/ x_dimid, y_dimid, z_dimid /)
             ncout = nf90_def_var(ncid, field_name, NF90_DOUBLE, dimids, varid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_enddef(ncid)

             
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             
             do ii = 0, np-1

                starts = (/1,1,local_N*ii+1/)
                counts = (/n(1), n(2), local_N/)
                
                ncout = nf90_put_var(ncid, varid, global_u(:,:,ii*local_N+1:(ii+1)*local_N), start = starts, count = counts)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)   

                
                !deallocate(global_field)
             
             END Do
             ncout = nf90_close(ncid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
          end If
          CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
          
           END SUBROUTINE save_field_R3toR1_ncdf2






        !==========================================
        ! SAVE FIELD IN R3
        !==========================================
        SUBROUTINE save_field_R3toR3_ncdf2(myfield, f1_name, f2_name, f3_name, file_name, myformat)
          USE global_variables
          USE netcdf
          IMPLICIT NONE
          INCLUDE "mpif.h"

          REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N, 1:3), INTENT(IN) :: myfield
          CHARACTER(len=*) :: file_name
          CHARACTER(len=*) :: f1_name, f2_name, f3_name
          CHARACTER(len=*) :: myformat
         
          !REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: u


          REAL(pr) :: local_maxf1, local_maxf2, local_maxf3, maxf1, maxf2, maxf3 
          
          INTEGER :: ncout, ncid, varids(3), dimids(3), f_id
          INTEGER :: x_dimid, y_dimid, z_dimid, ux_id, uy_id, uz_id, uvec_id, maxUx_id, maxUy_id, maxUz_id       

          CHARACTER(200) :: parallel_file
          CHARACTER(2) :: RANKtxt
          INTEGER :: fname_len, ii, nn
          INTEGER, DIMENSION(1:3) :: starts, counts

          IF (rank == 0) THEN 
             
             ncout = nf90_create(file_name, NF90_CLOBBER, ncid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_dim(ncid, "x", n(1), x_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_dim(ncid, "y", n(2), y_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_dim(ncid, "z", NF90_UNLIMITED, z_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             dimids =  (/ x_dimid, y_dimid, z_dimid /)
             ncout = nf90_def_var(ncid, TRIM(f1_name), NF90_DOUBLE, dimids, ux_id)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_var(ncid, TRIM(f2_name), NF90_DOUBLE, dimids, uy_id)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_var(ncid, TRIM(f3_name), NF90_DOUBLE, dimids, uz_id)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                     
             ncout = nf90_enddef(ncid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             
             !ALLOCATE( u(1:n(1),1:n(2),1:n(3)) )
             global_u = 0.0_pr
          END IF
          CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
          do nn = 1, 3
             CALL MPI_GATHER(myfield(:,:,:,nn), total_local_size, MPI_DOUBLE_PRECISION, global_u, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
             if (rank == 0) then
                do ii = 0, np-1
                   starts = (/1,1,local_N*ii+1/)
                   counts = (/n(1), n(2), local_N/)
                   select case (nn)
                   case (1)
                   
                      ncout = nf90_put_var(ncid, ux_id, global_u(:,:,local_N*ii+1:local_N*(ii+1)), start = starts, count = counts)
                      IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)               
                   case (2)
                      ncout = nf90_put_var(ncid, uy_id, global_u(:,:,local_N*ii+1:local_N*(ii+1)), start = starts, count = counts)
                      IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   case (3)
                      ncout = nf90_put_var(ncid, uz_id, global_u(:,:,local_N*ii+1:local_N*(ii+1)), start = starts, count = counts)
                      IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   end select
                end do
                
             end if
             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)

          end do
          

          if (rank == 0) then
             ncout = nf90_close(ncid)
             !deallocate(u)
          end if
          
          CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
 
        END SUBROUTINE save_field_R3toR3_ncdf2


         !==========================================
        ! SAVE FIELD IN R3
        !==========================================
        SUBROUTINE save_field_R3toR3_ncdf(f1, f2, f3, f1_name, f2_name, f3_name, file_name, myformat)
          USE global_variables
          USE netcdf
          IMPLICIT NONE
          INCLUDE "mpif.h"

          REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N), INTENT(IN) :: f1, f2, f3
          CHARACTER(len=*) :: file_name
          CHARACTER(len=*) :: f1_name, f2_name, f3_name
          CHARACTER(len=*) :: myformat
         
          REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: u1, u2, u3
          REAL(pr), DIMENSION(:,:,:,:), ALLOCATABLE :: myfield

          REAL(pr) :: local_maxf1, local_maxf2, local_maxf3, maxf1, maxf2, maxf3 
          
          INTEGER :: ncout, ncid, varids(3), dimids(3), f_id
          INTEGER :: x_dimid, y_dimid, z_dimid, ux_id, uy_id, uz_id, uvec_id, maxUx_id, maxUy_id, maxUz_id       

          CHARACTER(200) :: parallel_file
          CHARACTER(2) :: RANKtxt
          INTEGER :: fname_len, ii
          INTEGER, DIMENSION(1:3) :: starts, counts
          

          IF (parallel_data) THEN
             
             IF (rank==0) THEN
                ncout = nf90_create(file_name, NF90_CLOBBER, ncid=ncid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_def_dim(ncid, "x", n(1), x_dimid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_def_dim(ncid, "y", n(2), y_dimid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_def_dim(ncid, "z", NF90_UNLIMITED, z_dimid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                dimids = (/ x_dimid, y_dimid, z_dimid /)

                ncout = nf90_def_var(ncid, TRIM(f1_name), NF90_DOUBLE, dimids, ux_id)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_def_var(ncid, TRIM(f2_name), NF90_DOUBLE, dimids, uy_id)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_def_var(ncid, TRIM(f3_name), NF90_DOUBLE, dimids, uz_id)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                     
                ncout = nf90_enddef(ncid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_close(ncid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             END IF
             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
             starts = (/ 1, 1, local_k_offset+1 /)

            
             
             counts = (/ n(1), n(2), local_N /)            
 
             !!--------------------------
             !! START netCDF ROUTINES
             !!--------------------------
             DO ii=0,np-1
                IF (rank==ii) THEN 
                   ncout = nf90_open(file_name, NF90_WRITE, ncid)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

                   ncout = nf90_inq_varid(ncid, TRIM(f1_name), f_id)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   ncout = nf90_put_var(ncid, f_id, f1, start = starts, count = counts)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   
                   ncout = nf90_inq_varid(ncid, TRIM(f2_name), f_id)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   ncout = nf90_put_var(ncid, f_id, f2, start = starts, count = counts)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   
                   ncout = nf90_inq_varid(ncid, TRIM(f3_name), f_id)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   ncout = nf90_put_var(ncid, f_id, f3, start = starts, count = counts)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

                   ncout = nf90_close(ncid)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                END IF
                CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo) 
             END DO
             
             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
 
          ELSE 
             IF (rank == 0) THEN 
                ALLOCATE( u1(1:n(1),1:n(2),1:n(3)) )
                ALLOCATE( u2(1:n(1),1:n(2),1:n(3)) )
                ALLOCATE( u3(1:n(1),1:n(2),1:n(3)) )
             END IF
             CALL MPI_GATHER(f1, total_local_size, MPI_DOUBLE_PRECISION, u1, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
             CALL MPI_GATHER(f2, total_local_size, MPI_DOUBLE_PRECISION, u2, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
             CALL MPI_GATHER(f3, total_local_size, MPI_DOUBLE_PRECISION, u3, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)

             IF (rank==0) THEN
                ncout = nf90_create(file_name, NF90_CLOBBER, ncid)
                ncout = nf90_def_dim(ncid, "x", n(1), x_dimid)
                ncout = nf90_def_dim(ncid, "y", n(2), y_dimid)
                ncout = nf90_def_dim(ncid, "z", n(3), z_dimid)
                dimids =  (/ x_dimid, y_dimid, z_dimid /)
                
                ncout = nf90_def_var(ncid, TRIM(f1_name), NF90_DOUBLE, dimids, ux_id)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_def_var(ncid, TRIM(f2_name), NF90_DOUBLE, dimids, uy_id)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_def_var(ncid, TRIM(f3_name), NF90_DOUBLE, dimids, uz_id)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_enddef(ncid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
          
                ncout = nf90_put_var(ncid, ux_id, u1)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_put_var(ncid, uy_id, u2)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_put_var(ncid, uz_id, u3)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

                ncout = nf90_close(ncid)
             
                DEALLOCATE( u1 ) 
                DEALLOCATE( u2 )
                DEALLOCATE( u3 )

             END IF  
             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)

          END IF 
 
        END SUBROUTINE save_field_R3toR3_ncdf

      
!!        !==========================================
!!        ! SAVE FIELD IN netCDF FORMAT
!!        !==========================================
!!        SUBROUTINE save_field_R3toR3_ncdf(f1, f2, f3, f1_name, f2_name, f3_name, file_name, myformat)
!!          USE global_variables
!!          USE netcdf
!!          IMPLICIT NONE
!!          INCLUDE "mpif.h"
!!
!!          REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N), INTENT(IN) :: f1, f2, f3
!!          CHARACTER(len=*) :: file_name
!!          CHARACTER(len=*) :: f1_name, f2_name, f3_name
!!          CHARACTER(len=*) :: myformat
!!         
!!          REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: u1, u2, u3
!!          REAL(pr), DIMENSION(:,:,:,:), ALLOCATABLE :: myfield
!!          REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N) :: u_aux
!!
!!          REAL(pr) :: local_maxf1, local_maxf2, local_maxf3, maxf1, maxf2, maxf3 
!!          INTEGER :: ncout, ncid, varids(3), dimids(3), ii
!!          INTEGER :: x_dimid, y_dimid, z_dimid, ux_id, uy_id, uz_id, f_id    
!!          INTEGER, DIMENSION(1:3) :: starts, counts
!!
!!
!!          IF (rank==0) THEN
!!             ncout = nf90_create(file_name, NF90_CLOBBER, ncid=ncid)
!!             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!!             ncout = nf90_def_dim(ncid, "x", n(1), x_dimid)
!!             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!!             ncout = nf90_def_dim(ncid, "y", n(2), y_dimid)
!!             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!!             ncout = nf90_def_dim(ncid, "z", NF90_UNLIMITED, z_dimid)
!!             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!!             dimids = (/ x_dimid, y_dimid, z_dimid /)
!!
!!             ncout = nf90_def_var(ncid, TRIM(f1_name), NF90_DOUBLE, dimids, ux_id)
!!             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!!
!!             ncout = nf90_def_var(ncid, TRIM(f2_name), NF90_DOUBLE, dimids, uy_id)
!!             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!!
!!             ncout = nf90_def_var(ncid, TRIM(f3_name), NF90_DOUBLE, dimids, uz_id)
!!             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!!                     
!!             ncout = nf90_enddef(ncid)
!!             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!!             ncout = nf90_close(ncid)
!!             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!!          END IF
!!          CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
!!
!!          starts = (/ 1, 1, rank*local_N+1 /)
!!          counts = (/ n(1), n(2), local_N /)            
!! 
!!          !!--------------------------
!!          !! START netCDF ROUTINES
!!          !!--------------------------
!!          DO ii=0,np-1
!!
!!             IF (rank==ii) THEN 
!!                ncout = nf90_open(file_name, NF90_WRITE, ncid)
!!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!!
!!                ncout = nf90_inq_varid(ncid, TRIM(f1_name), f_id)
!!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!!                ncout = nf90_put_var(ncid, f_id, f1, start = starts, count = counts)
!!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!!                   
!!                ncout = nf90_inq_varid(ncid, TRIM(f2_name), f_id)
!!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!!                ncout = nf90_put_var(ncid, f_id, f2, start = starts, count = counts)
!!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!!                
!!                ncout = nf90_inq_varid(ncid, TRIM(f3_name), f_id)
!!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!!                ncout = nf90_put_var(ncid, f_id, f3, start = starts, count = counts)
!!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!!
!!                ncout = nf90_close(ncid)
!!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!!             END IF
!!             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo) 
!!          END DO
!!          CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
!! 
!!        END SUBROUTINE save_field_R3toR3_ncdf
!!
        !==========================================
        ! SAVE FIELD IN netCDF FORMAT
        !==========================================
        SUBROUTINE save_field_R3toRn_ncdf(myfield, dimRange, mynames, file_name)
          USE global_variables
          USE netcdf
          IMPLICIT NONE
          INCLUDE "mpif.h"

          REAL(pr), DIMENSION(:,:,:,:), INTENT(IN) :: myfield
          INTEGER, INTENT(IN) :: dimRange
          CHARACTER(len=*), INTENT(IN) :: mynames
          CHARACTER(len=*), INTENT(IN) :: file_name
         
          REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: f
          REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: local_f

          REAL(pr) :: local_maxf, local_minf, maxf, minf
          INTEGER :: ncout, ncid, ndims, nvars, include_parents, dimids(3)
          INTEGER :: x_dimid, y_dimid, z_dimid, f_id       
          INTEGER :: ii, kk, coma_index, start_index, mynames_length
          CHARACTER(10) :: varName
          CHARACTER(50) :: auxName
          CHARACTER(200) :: parallel_file
          CHARACTER(2) :: RANKtxt
          INTEGER :: fname_len, local_N_LR
          INTEGER, DIMENSION(1:3) :: starts, counts

          mynames_length = LEN(mynames)
          coma_index = 0
          start_index = 1

          ALLOCATE(local_f(1:n(1),1:n(2),1:local_N))

          IF (rank==0) THEN
             ncout = nf90_create(file_name, NF90_CLOBBER, ncid=ncid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_dim(ncid, "x", n(1), x_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_dim(ncid, "y", n(2), y_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_dim(ncid, "z", NF90_UNLIMITED, z_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             dimids = (/ x_dimid, y_dimid, z_dimid /)

             auxName = mynames
             DO kk=1,dimRange
                IF ( kk < dimRange ) THEN
                   coma_index = SCAN(auxName, ",", .FALSE.)
                   varName = auxName(1:coma_index-1)
                   start_index = coma_index+1
                ELSE
                   varName = auxName
                END IF
                ncout = nf90_def_var(ncid, TRIM(varName), NF90_DOUBLE, dimids, f_id)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                auxName = auxName(start_index:mynames_length)
             END DO
                     
             ncout = nf90_enddef(ncid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_close(ncid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
          END IF
          CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)

          starts = (/ 1, 1, rank*local_N+1 /)
          counts = (/ n(1), n(2), local_N /)            
 
          !!--------------------------
          !! START netCDF ROUTINES
          !!--------------------------
          DO ii=0,np-1
             IF (rank==ii) THEN 
                ncout = nf90_open(file_name, NF90_WRITE, ncid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

                coma_index = 0
                start_index = 1
                auxName = mynames
                DO kk=1,dimRange
                   IF ( kk < dimRange ) THEN
                      coma_index = SCAN(auxName, ",", .FALSE.)
                      varName = auxName(1:coma_index-1)
                      start_index = coma_index+1
                   ELSE
                      varName = auxName
                   END IF
                   auxName = auxName(start_index:mynames_length)
 
                   local_f = myfield(:,:,:,kk)

                   ncout = nf90_inq_varid(ncid, TRIM(varName), f_id)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   ncout = nf90_put_var(ncid, f_id, local_f, start = starts, count = counts)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                END DO  

                ncout = nf90_close(ncid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             END IF
             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo) 
          END DO

          DEALLOCATE( local_f )
          CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
 
        END SUBROUTINE save_field_R3toRn_ncdf

        !===========================================================
        !              SAVE CPU TIME
        !===========================================================
        SUBROUTINE save_cpu_time(myindex, dt, t_ini, t_fin)
          USE global_variables
          IMPLICIT NONE

          INTEGER, INTENT(IN) :: myindex
          REAL(pr), INTENT(IN) :: dt, t_ini, t_fin

          CHARACTER(200) :: filename
          CHARACTER(2) :: K0txt
          CHARACTER(2) :: E0txt

          WRITE(K0txt, '(i2.2)') K0_index
          WRITE(E0txt, '(i2.2)') E0_index
          
          filename = TRIM(work_pathname)//"_computing_time.dat"
          IF (myindex==1) THEN
             OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'REPLACE')
          ELSE
             OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
          END IF
          WRITE(10, "(2 G20.12)") dt, t_fin - t_ini
          CLOSE(10)
 
        END SUBROUTINE save_cpu_time
! 
!        !===========================================================
!        !              SAVE ELAPSED TIME
!        !===========================================================
!        SUBROUTINE save_elapsed_time(mysystem, myflag, dt, t_ini, t_fin)
!          USE global_variables
!          IMPLICIT NONE

!          CHARACTER(len=*), INTENT(IN) :: mysystem
!          INTEGER, INTENT(IN) :: myflag
!          REAL(pr), INTENT(IN) :: dt
!          INTEGER, DIMENSION(8), INTENT(IN) :: t_ini, t_fin
!          INTEGER :: elapsed_minutes, elapsed_hours, elapsed_days, elapsed_seconds

!          CHARACTER(200) :: filename
!          CHARACTER(2) :: K0txt
!          CHARACTER(2) :: E0txt

!          WRITE(K0txt, '(i2.2)') K0_index
!          WRITE(E0txt, '(i2.2)') E0_index
!          
!          elapsed_days = 0
!          elapsed_hours = 0
!          elapsed_minutes = 0
!          elapsed_seconds = 0
! 
!          IF (t_fin(1)==t_ini(1)) THEN
!             IF (t_fin(2)==t_ini(2)) THEN
!                elapsed_days = t_fin(3) - t_ini(3)
!                elapsed_hours = t_fin(5) - t_ini(5) + 24*elapsed_days
!                IF (elapsed_hours < 24) THEN
!                   elapsed_days = 0
!                END IF
!                elapsed_minutes = t_fin(6) - t_ini(6) + 60*elapsed_hours + 1440*elapsed_days
!                IF (elapsed_minutes < 60) THEN
!                   elapsed_hours = 0
!                END IF                      
!             ELSE
!                IF (t_fin(2)==2 .OR. t_fin(2)==4 .OR. t_fin(2)==6 .OR. t_fin(2)==8 .OR. t_fin(2)==9 .OR. t_fin(2)==11) THEN
!                   elapsed_days = 31 + t_fin(3) - t_ini(3)
!                   elapsed_hours = t_fin(5) - t_ini(5) + 24*elapsed_days
!                   IF (elapsed_hours < 24) THEN
!                      elapsed_days = 0
!                   END IF
!                   elapsed_minutes = t_fin(6) - t_ini(6) + 60*elapsed_hours + 1440*elapsed_days
!                   IF (elapsed_minutes < 60) THEN
!                      elapsed_hours = 0
!                   END IF                      
!                ELSEIF (t_fin(2)==5 .OR. t_fin(2)==7 .OR. t_fin(2)==10 .OR. t_fin(2)==12) THEN
!                   elapsed_days = 30 + t_fin(3) - t_ini(3)
!                   elapsed_hours = t_fin(5) - t_ini(5) + 24*elapsed_days
!                   IF (elapsed_hours < 24) THEN
!                      elapsed_days = 0
!                   END IF
!                   elapsed_minutes = t_fin(6) - t_ini(6) + 60*elapsed_hours + 1440*elapsed_days
!                   IF (elapsed_minutes < 60) THEN
!                      elapsed_hours = 0
!                   END IF                      
!                ELSEIF (t_fin(2)==3) THEN
!                   IF (MOD(t_fin(1),4)==0) THEN
!                      elapsed_days = 29 + t_fin(3) - t_ini(3)
!                      elapsed_hours = t_fin(5) - t_ini(5) + 24*elapsed_days
!                      IF (elapsed_hours < 24) THEN
!                         elapsed_days = 0
!                      END IF
!                      elapsed_minutes = t_fin(6) - t_ini(6) + 60*elapsed_hours + 1440*elapsed_days
!                      IF (elapsed_minutes < 60) THEN
!                         elapsed_hours = 0
!                      END IF                      
!                   ELSE
!                      elapsed_days = 28 + t_fin(3) - t_ini(3)
!                      elapsed_hours = t_fin(5) - t_ini(5) + 24*elapsed_days
!                      IF (elapsed_hours < 24) THEN
!                         elapsed_days = 0
!                      END IF
!                      elapsed_minutes = t_fin(6) - t_ini(6) + 60*elapsed_hours + 1440*elapsed_days
!                      IF (elapsed_minutes < 60) THEN
!                         elapsed_hours = 0
!                      END IF                      
!                   
!                   END IF
!                END IF
!             END IF
!          ELSE
!             elapsed_days = 31 + t_fin(3) - t_ini(3)
!             elapsed_hours = t_fin(5) - t_ini(5) + 24*elapsed_days
!             IF (elapsed_hours < 24) THEN
!                elapsed_days = 0
!             END IF
!             elapsed_minutes = t_fin(6) - t_ini(6) + 60*elapsed_hours + 1440*elapsed_days
!             IF (elapsed_minutes < 60) THEN
!                elapsed_hours = 0
!             END IF                      
!          END IF   
!  

!          SELECT CASE (mysystem)
!            CASE ("nse")
!              filename = TRIM(scratch_pathname)//"/DNS_NSE_3D/K"//K0txt//"/E"//E0txt//"/"//TRIM(IC_type)//"/computing_time.dat"
!              IF (myflag==0) THEN
!                  OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'REPLACE')
!              ELSE
!                  OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
!              END IF
!              WRITE(10,*) "E0 = ",E0, ", T = ",endTime,", N = ",n(1),", N_proc = ",np
!              WRITE(10,*) "   ",elapsed_days," d"
!              WRITE(10,*) "   ",elapsed_hours," h"
!              WRITE(10,*) "   ",elapsed_minutes," m"
!              !WRITE(10,*) "   ",elapsed_seconds," s"  
!              CLOSE(10)
!          END SELECT

!        END SUBROUTINE save_elapsed_time

        !============================================================
        !          SAVE SPECTRAL DATA
        !============================================================
        SUBROUTINE save_spectral_data(mydata, myindex, mysystem, optindex)
          USE global_variables
          IMPLICIT NONE
 
          REAL(pr), DIMENSION(1:n(1)/2,1:2), INTENT(IN) :: mydata
          INTEGER, INTENT(IN) :: myindex
          CHARACTER(len=*), INTENT(IN) :: mysystem
          INTEGER, INTENT(IN) :: optindex
          CHARACTER(2) :: K0txt
          CHARACTER(2) :: E0txt
          CHARACTER(4) :: indexchar
          CHARACTER(4) :: optchar
          CHARACTER(200) :: filename
          INTEGER :: i
          WRITE(K0txt, '(i2.2)') K0_index
          WRITE(E0txt, '(i2.2)') E0_index
          WRITE(indexchar, '(i4)') myindex
          WRITE(optchar, '(i4)') optindex
          SELECT CASE (mysystem)
             case("fwdTE")
                !filename = TRIM(work_pathname)//"_E"//E0txt//"_spectrum_fwdTE"//trim(adjustl(indexchar))//"_OPT"//trim(adjustl(optchar))//".dat"
                !filename = TRIM(work_pathname)//"_E"//E0txt//"_spectrum_fwdTE"//trim(adjustl(indexchar))//".dat"
                filename = TRIM(scratch_pathname)//"_spectrum_fwdTE"//trim(adjustl(indexchar))//".dat"
             case("bwdADJ")
                !filename = TRIM(work_pathname)//"_E"//E0txt//"_spectrum_bwdADJ"//trim(adjustl(indexchar))//"_OPT"//trim(adjustl(optchar))//".dat"
                !filename = TRIM(work_pathname)//"_E"//E0txt//"_spectrum_bwdADJ"//trim(adjustl(indexchar))//".dat"
                filename = TRIM(scratch_pathname)//"_spectrum_bwdADJ"//trim(adjustl(indexchar))//".dat"
             case("before_CUT")
                filename = TRIM(scratch_pathname)//"_spectrum_bwdADJ_before_CUT_OPT"//trim(adjustl(optchar))//".dat"
             case("after_CUT")
                filename = TRIM(scratch_pathname)//"_spectrum_bwdADJ_after_CUT_OPT"//trim(adjustl(optchar))//".dat"
             case("readbinary")
                filename = TRIM(scratch_pathname)//"_spectrum_bwdADJ"//trim(adjustl(indexchar))//"_OPT"//trim(adjustl(optchar))//".dat"
          END SELECT
          OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'REPLACE')
          DO i=1,n(1)/2
             WRITE(10, "(2 G20.12)") mydata(i,1), mydata(i,2)
          END DO
          CLOSE(10)

        END SUBROUTINE save_spectral_data


!!        !===================================================
!!        ! SAVE FILE SUITABLE FOR IMAGE GENERATION IN GNUPLOT
!!        !===================================================
!!        SUBROUTINE save_image(iter, f, mytype, myflag)
!!          USE global_variables
!!          IMPLICIT NONE
!!          INCLUDE "mpif.h"
!!
!!          REAL(pr), DIMENSION(:,:), INTENT(IN) :: f
!!          INTEGER, INTENT(IN) :: iter
!!          CHARACTER(len=*), INTENT(IN) :: mytype
!!          INTEGER, INTENT(IN) :: myflag          
!!
!!          REAL(pr), DIMENSION(:,:), ALLOCATABLE :: f_image, f_global
!!          CHARACTER(200) :: filename
!!          CHARACTER(20) :: Pdir, Pchar, Tchar
!!          CHARACTER(4) :: Iterchar, resol
!!
!!          INTEGER :: ii, jj, r
!!          REAL(pr) :: dx, dy
!!
!!          r = 2          
!!
!!          IF (rank==0) THEN
!!             ALLOCATE(f_global(1:n(1),1:n(2)))
!!             ALLOCATE(f_image(1:n(1)/r,1:n(2)/r))
!!          END IF    
!!          CALL MPI_GATHER(f, total_local_size, MPI_REAL8, f_global, total_local_size, MPI_REAL8, 0, MPI_COMM_WORLD, Statinfo)
!!          
!!          IF (rank==0) THEN
!!             DO jj=1,n(2)/r
!!                DO ii=1,n(1)/r
!!                   f_image(ii,jj) = f_global(r*(ii-1)+1,r*(jj-1)+1)
!!                END DO
!!             END DO
!!                 
!!             WRITE(Pchar, "(f12.3)") P0
!!             WRITE(Pdir, "(f12.2)") P0
!!             WRITE(Tchar, "(G9.3)") T
!!             WRITE(resol, "(i4.4)") n(1)
!! 
!!             !===================================================
!!             !  myflag controls if new image is generated. 
!!             !  myflag==0  ==>  current image is overwritten. 
!!             !  myflag==1  ==>  a new image is generated.
!!             !===================================================
!! 
!!             SELECT CASE (mytype)
!!               CASE ("control")
!!                 IF (myflag==0) THEN
!!                   WRITE(Iterchar, "(i4.4)") 0                
!!                   filename = "/gwork/ayalada/MAXPALINS/DataGenerated/P"//TRIM(ADJUSTL(Pdir))//"/Diagnostics/T"//TRIM(ADJUSTL(Tchar))//"_P"//TRIM(ADJUSTL(Pchar))// &
!!                              "_N"//resol//"_CtrlVort_"//Iterchar//".plot"
!!                 ELSE
!!                   WRITE(Iterchar, "(i4.4)") iter                
!!                   filename = "/gwork/ayalada/MAXPALINS/DataGenerated/P"//TRIM(ADJUSTL(Pdir))//"/Diagnostics/T"//TRIM(ADJUSTL(Tchar))//"_P"//TRIM(ADJUSTL(Pchar))// &
!!                              "_N"//resol//"_CtrlVort_"//Iterchar//".plot"
!!                 END IF
!!             
!!                 OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'REPLACE')
!!                 dx = REAL(r,pr)/REAL(n(1),pr)
!!                 dy = REAL(r,pr)/REAL(n(2),pr)
!!
!!                 DO ii=1,n(1)/r
!!                    DO jj=1,n(2)/r
!!                       WRITE(10, "(3 F16.9)") (ii-1)*dx, (jj-1)*dy, f_image(ii,jj) 
!!                    END DO 
!!                    WRITE(10,*) " "
!!                 END DO
!!                 CLOSE(10)
!!
!!               CASE ("nse")
!!                 IF (myflag==0) THEN
!!                   WRITE(Iterchar, "(i4.4)") 0                
!!                   filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/NS_Diag/vort_"//Iterchar//".plot"
!!                 ELSE
!!                   WRITE(Iterchar, "(i4.4)") iter                
!!                   filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/NS_Diag/vort_"//Iterchar//".plot"
!!                 END IF
!!             
!!                 OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'REPLACE')
!!                 dx = REAL(r,pr)/REAL(n(1),pr)
!!                 dy = REAL(r,pr)/REAL(n(2),pr)
!!
!!                 DO ii=1,n(1)/r
!!                    DO jj=1,n(2)/r
!!                       WRITE(10, "(3 F16.9)") (ii-1)*dx, (jj-1)*dy, f_image(ii,jj) 
!!                    END DO 
!!                    WRITE(10,*) " "
!!                 END DO
!!                 CLOSE(10)
!!
!!               CASE ("adj")
!!                 IF (myflag==0) THEN
!!                   WRITE(Iterchar, "(i4.4)") 0                
!!                   filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/Adj_Diag/stream_"//Iterchar//".plot"
!!                 ELSE
!!                   WRITE(Iterchar, "(i4.4)") iter                
!!                   filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/Adj_Diag/stream_"//Iterchar//".plot"
!!                 END IF
!!             
!!                 OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'REPLACE')
!!                 dx = REAL(r,pr)/REAL(n(1),pr)
!!                 dy = REAL(r,pr)/REAL(n(2),pr)
!!
!!                 DO ii=1,n(1)/r
!!                    DO jj=1,n(2)/r
!!                       WRITE(10, "(3 F16.9)") (ii-1)*dx, (jj-1)*dy, f_image(ii,jj) 
!!                    END DO 
!!                    WRITE(10,*) " "
!!                 END DO
!!                 CLOSE(10)
!!
!!             END SELECT
!! 
!!             DEALLOCATE(f_global)
!!             DEALLOCATE(f_image)
!!
!!          END IF
!!          CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)          
!!
!!        END SUBROUTINE save_image
!!


!============================================================================================
!============================================================================================


SUBROUTINE read_field_R3toR1_ncdf2(myfield, filename, Fx_txt)
          USE global_variables
          USE netcdf
          IMPLICIT NONE
          INCLUDE "mpif.h"

          REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N), INTENT(OUT) :: myfield
          CHARACTER(len=*), INTENT(IN) :: filename, Fx_txt
         
          !REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: u
 
          INTEGER :: ncout, ncid, fid, dimids(3)
          INTEGER :: x_dimid, y_dimid, z_dimid
          INTEGER :: fname_len, ii, nx_ncdf, ny_ncdf, nz_ncdf, nn

          INTEGER, DIMENSION(1:3) :: starts, counts

          if  (rank == 0) then
             ncout = nf90_open(filename, NF90_NOWRITE, ncid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

             ncout = nf90_inq_dimid(ncid, "x", x_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_inq_dimid(ncid, "y", y_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_inq_dimid(ncid, "z", z_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

             ncout = nf90_inquire_dimension(ncid, x_dimid, len = nx_ncdf)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_inquire_dimension(ncid, y_dimid, len = ny_ncdf)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_inquire_dimension(ncid, z_dimid, len = nz_ncdf)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

             global_u = 0.0_pr  
             !allocate(u(1:n(1), 1:n(2), 1:n(3)))
          end if
          CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
          if (rank == 0) then
             do ii = 0, np-1
                starts = (/ 1, 1, local_N*ii+1 /)
                counts = (/ n(1), n(2), local_N /)
                ncout = nf90_inq_varid(ncid, Fx_txt, fid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_get_var(ncid, fid, global_u(:,:,local_N*ii+1:local_N*(ii+1)), start = starts, count = counts)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

             end do
             
                
                
          end if
                   
             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
             CALL MPI_SCATTER(global_u, total_local_size, MPI_DOUBLE_PRECISION, myfield, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)

             

          if (rank == 0) then
             ncout = nf90_close(ncid)
             !deallocate(u)
          end if
                   
          CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)  
                         
                            
                            
              
           END SUBROUTINE read_field_R3toR1_ncdf2

        !============================================================================================
!============================================================================================


SUBROUTINE read_field_R3toR3_ncdf2(myfield, filename, Fx_txt, Fy_txt, Fz_txt)
          USE global_variables
          USE netcdf
          IMPLICIT NONE
          INCLUDE "mpif.h"

          REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(OUT) :: myfield
          CHARACTER(len=*), INTENT(IN) :: filename, Fx_txt, Fy_txt, Fz_txt
         
          !REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: u
 
          INTEGER :: ncout, ncid, fid, dimids(3)
          INTEGER :: x_dimid, y_dimid, z_dimid
          INTEGER :: fname_len, ii, nx_ncdf, ny_ncdf, nz_ncdf, nn

          INTEGER, DIMENSION(1:3) :: starts, counts

          if  (rank == 0) then
             ncout = nf90_open(filename, NF90_NOWRITE, ncid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

             ncout = nf90_inq_dimid(ncid, "x", x_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_inq_dimid(ncid, "y", y_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_inq_dimid(ncid, "z", z_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

             ncout = nf90_inquire_dimension(ncid, x_dimid, len = nx_ncdf)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_inquire_dimension(ncid, y_dimid, len = ny_ncdf)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_inquire_dimension(ncid, z_dimid, len = nz_ncdf)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

             global_u = 0.0_pr  
             !allocate(u(1:n(1), 1:n(2), 1:n(3)))
          end if
          CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
          do nn = 1,3
             if (rank == 0) then
                do ii = 0, np-1
                   starts = (/ 1, 1, local_N*ii+1 /)
                   counts = (/ n(1), n(2), local_N /)
                   select case (nn)
                   case (1)
                      ncout = nf90_inq_varid(ncid, Fx_txt, fid)
                      IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                      ncout = nf90_get_var(ncid, fid, global_u(:,:,local_N*ii+1:local_N*(ii+1)), start = starts, count = counts)
                      IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

                   case (2)
                      ncout = nf90_inq_varid(ncid, Fy_txt, fid)
                      IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                      ncout = nf90_get_var(ncid, fid, global_u(:,:,local_N*ii+1:local_N*(ii+1)), start = starts, count = counts)
                      IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                      
                   case (3)
                      ncout = nf90_inq_varid(ncid, Fz_txt, fid)
                      IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                      ncout = nf90_get_var(ncid, fid, global_u(:,:,local_N*ii+1:local_N*(ii+1)), start = starts, count = counts)
                      IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   end select
                end do
                
                
             end if
                   
             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
             CALL MPI_SCATTER(global_u, total_local_size, MPI_DOUBLE_PRECISION, myfield(:,:,:,nn), total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
          end do
             

          if (rank == 0) then
             ncout = nf90_close(ncid)
             !deallocate(u)
          end if
                   
          CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)  
                         
                            
                            
              
           END SUBROUTINE read_field_R3toR3_ncdf2


        !============================================================
        ! READ VORTICITY IN netCDF FORMAT
        !============================================================
        SUBROUTINE read_field_R3toR3_ncdf(myfield, filename, Fx_txt, Fy_txt, Fz_txt)
          USE global_variables
          USE netcdf
          IMPLICIT NONE
          INCLUDE "mpif.h"

          REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(OUT) :: myfield
          CHARACTER(len=*), INTENT(IN) :: filename, Fx_txt, Fy_txt, Fz_txt
         
          REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: local_f, global_f
 
          INTEGER :: ncout, ncid, fid, dimids(3)
          INTEGER :: x_dimid, y_dimid, z_dimid
          INTEGER :: fname_len, ii, nx_ncdf, ny_ncdf, nz_ncdf

          INTEGER, DIMENSION(1:3) :: starts, counts

          
          
          IF (parallel_data) THEN
             ALLOCATE( local_f(1:n(1),1:n(2),1:local_N) )
             starts = (/ 1, 1, local_k_offset+1 /)
             
             counts = (/ n(1), n(2), local_N /)            
 
             !--------------------------
             ! START netCDF ROUTINES
             !--------------------------
             DO ii=0,np-1
                IF (rank == ii) THEN 
                   ncout = nf90_open(filename, NF90_NOWRITE, ncid)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

                   ncout = nf90_inq_dimid(ncid, "x", x_dimid)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   ncout = nf90_inq_dimid(ncid, "y", y_dimid)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   ncout = nf90_inq_dimid(ncid, "z", z_dimid)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

                   ncout = nf90_inquire_dimension(ncid, x_dimid, len = nx_ncdf)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   ncout = nf90_inquire_dimension(ncid, y_dimid, len = ny_ncdf)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   ncout = nf90_inquire_dimension(ncid, z_dimid, len = nz_ncdf)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

                   ncout = nf90_inq_varid(ncid, Fx_txt, fid)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   ncout = nf90_get_var(ncid, fid, local_f, start = starts, count = counts)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   myfield(:,:,:,1) = local_f
 
                   ncout = nf90_inq_varid(ncid, Fy_txt, fid)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   ncout = nf90_get_var(ncid, fid, local_f, start = starts, count = counts)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   myfield(:,:,:,2) = local_f
 
                   ncout = nf90_inq_varid(ncid, Fz_txt, fid)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   ncout = nf90_get_var(ncid, fid, local_f, start = starts, count = counts)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   myfield(:,:,:,3) = local_f
 
                   ncout = nf90_close(ncid)
 
                   DEALLOCATE(local_f)
                END IF
                CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
             END DO

             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)

          ELSE
             IF (rank == 0) THEN
                ALLOCATE( global_f(1:n(1),1:n(2),1:n(3)) )
             END IF
            
             ALLOCATE( local_f(1:n(1),1:n(2),1:local_N) )

             IF (rank == 0) THEN 
                ncout = nf90_open(filename, NF90_NOWRITE, ncid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

                ncout = nf90_inq_dimid(ncid, "x", x_dimid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_inq_dimid(ncid, "y", y_dimid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_inq_dimid(ncid, "z", z_dimid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

                ncout = nf90_inq_varid(ncid, Fx_txt, fid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_get_var(ncid, fid, global_f)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             END IF
             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
             CALL MPI_SCATTER(global_f, total_local_size, MPI_DOUBLE_PRECISION, local_f, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
             myfield(:,:,:,1) = local_f
 
             IF (rank == 0) THEN 
                ncout = nf90_inq_varid(ncid, Fy_txt, fid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_get_var(ncid, fid, global_f)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             END IF
             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
             CALL MPI_SCATTER(global_f, total_local_size, MPI_DOUBLE_PRECISION, local_f, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
             myfield(:,:,:,2) = local_f
 
             IF (rank == 0) THEN 
                ncout = nf90_inq_varid(ncid, Fz_txt, fid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_get_var(ncid, fid, global_f)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_close(ncid)
             END IF
             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
             CALL MPI_SCATTER(global_f, total_local_size, MPI_DOUBLE_PRECISION, local_f, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
             myfield(:,:,:,3) = local_f
 
             IF (rank == 0) THEN
                DEALLOCATE( global_f )
             END IF
             DEALLOCATE( local_f )
             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
          END IF  

        END SUBROUTINE read_field_R3toR3_ncdf


	SUBROUTINE read_field_R3toR3_ncdf_refine(myfield, filename, Fx_txt, Fy_txt, Fz_txt)
          USE global_variables
          USE netcdf
          IMPLICIT NONE
          INCLUDE "mpif.h"

          REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(OUT) :: myfield
          CHARACTER(len=*), INTENT(IN) :: filename, Fx_txt, Fy_txt, Fz_txt
         
          REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: local_f, global_f
 
          INTEGER :: ncout, ncid, fid, dimids(3)
          INTEGER :: x_dimid, y_dimid, z_dimid
          INTEGER :: fname_len, ii, nx_ncdf, ny_ncdf, nz_ncdf

          INTEGER, DIMENSION(1:3) :: starts, counts

          IF (parallel_data) THEN
             ALLOCATE( local_f(1:n(1)/2,1:n(2)/2,1:local_N/2) )

             starts = (/ 1, 1, rank*local_N/2+1 /)
             counts = (/ n(1)/2, n(2)/2, local_N/2 /)            
 
             !--------------------------
             ! START netCDF ROUTINES
             !--------------------------
             DO ii=0,np-1
                IF (rank == ii) THEN 
                   ncout = nf90_open(filename, NF90_NOWRITE, ncid)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

                   ncout = nf90_inq_dimid(ncid, "x", x_dimid)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   ncout = nf90_inq_dimid(ncid, "y", y_dimid)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   ncout = nf90_inq_dimid(ncid, "z", z_dimid)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

                   ncout = nf90_inquire_dimension(ncid, x_dimid, len = nx_ncdf)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   ncout = nf90_inquire_dimension(ncid, y_dimid, len = ny_ncdf)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   ncout = nf90_inquire_dimension(ncid, z_dimid, len = nz_ncdf)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

                   ncout = nf90_inq_varid(ncid, Fx_txt, fid)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   ncout = nf90_get_var(ncid, fid, local_f, start = starts, count = counts)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   myfield(:,:,:,1) = local_f
 
                   ncout = nf90_inq_varid(ncid, Fy_txt, fid)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   ncout = nf90_get_var(ncid, fid, local_f, start = starts, count = counts)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   myfield(:,:,:,2) = local_f
 
                   ncout = nf90_inq_varid(ncid, Fz_txt, fid)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   ncout = nf90_get_var(ncid, fid, local_f, start = starts, count = counts)
                   IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   myfield(:,:,:,3) = local_f
 
                   ncout = nf90_close(ncid)
 
                   DEALLOCATE(local_f)
                END IF
                CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
             END DO     
             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)

          ELSE
             IF (rank == 0) THEN
                ALLOCATE( global_f(1:n(1)/2,1:n(2)/2,1:n(3)/2) )
             END IF
            
             ALLOCATE( local_f(1:n(1)/2,1:n(2)/2,1:local_N/2) )

             IF (rank == 0) THEN 
                ncout = nf90_open(filename, NF90_NOWRITE, ncid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

                ncout = nf90_inq_dimid(ncid, "x", x_dimid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_inq_dimid(ncid, "y", y_dimid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_inq_dimid(ncid, "z", z_dimid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)

                ncout = nf90_inq_varid(ncid, Fx_txt, fid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_get_var(ncid, fid, global_f)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             END IF
             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
             CALL MPI_SCATTER(global_f, total_local_size, MPI_DOUBLE_PRECISION, local_f, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
             myfield(:,:,:,1) = local_f
 
             IF (rank == 0) THEN 
                ncout = nf90_inq_varid(ncid, Fy_txt, fid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_get_var(ncid, fid, global_f)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             END IF
             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
             CALL MPI_SCATTER(global_f, total_local_size, MPI_DOUBLE_PRECISION, local_f, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
             myfield(:,:,:,2) = local_f
 
             IF (rank == 0) THEN 
                ncout = nf90_inq_varid(ncid, Fz_txt, fid)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_get_var(ncid, fid, global_f)
                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                ncout = nf90_close(ncid)
             END IF
             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
             CALL MPI_SCATTER(global_f, total_local_size, MPI_DOUBLE_PRECISION, local_f, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
             myfield(:,:,:,3) = local_f
 
             IF (rank == 0) THEN
                DEALLOCATE( global_f )
             END IF
             DEALLOCATE( local_f )
             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
          END IF  

        END SUBROUTINE read_field_R3toR3_ncdf_refine

!        !============================================================
!        ! READ VELOCITY IN netCDF FORMAT
!        !============================================================
!        SUBROUTINE read_field_R3toR3_ncdf(myfield, filename, Fx_txt, Fy_txt, Fz_txt)
!          USE global_variables
!          USE netcdf
!          IMPLICIT NONE
!          INCLUDE "mpif.h"
!
!          REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(OUT) :: myfield
!          CHARACTER(len=*), INTENT(IN) :: filename, Fx_txt, Fy_txt, Fz_txt
!         
!          REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: global_fx, global_fy, global_fz
!          REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: global_fx_ncdf, global_fy_ncdf, global_fz_ncdf
!          REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: local_fx, local_fy, local_fz
! 
!          INTEGER :: ncout, ncid, fx_id, fy_id, fz_id, dimids(3), starts(1:3), counts(1:3)
!          INTEGER :: x_dimid, y_dimid, z_dimid, nx_ncdf, ny_ncdf, nz_ncdf 
!          INTEGER :: nz_fixres, local_size_fixres, ii
!          INTEGER, DIMENSION(1:9) :: ncdf_info 
!
!         
!          ALLOCATE( global_fx(1:n(1),1:n(2),1:local_N) )
!          ALLOCATE( global_fy(1:n(1),1:n(2),1:local_N) )
!          ALLOCATE( global_fz(1:n(1),1:n(2),1:local_N) )
!
!          starts = (/ 1, 1, rank*local_N+1 /)
!          counts = (/ n(1), n(2), local_N /)            
! 
!          !--------------------------
!          ! START netCDF ROUTINES
!          !--------------------------
!          DO ii=0,np-1
!             IF (rank == ii) THEN 
!                ncout = nf90_open(filename, NF90_NOWRITE, ncid)
!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!
!                ncout = nf90_inq_dimid(ncid, "x", x_dimid)
!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!                ncout = nf90_inq_dimid(ncid, "y", y_dimid)
!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!                ncout = nf90_inq_dimid(ncid, "z", z_dimid)
!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!
!                ncout = nf90_inq_varid(ncid, Fx_txt, fx_id)
!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!                ncout = nf90_inq_varid(ncid, Fy_txt, fy_id)
!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!                ncout = nf90_inq_varid(ncid, Fz_txt, fz_id)
!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!
!                ncout = nf90_inquire_dimension(ncid, x_dimid, len = nx_ncdf)
!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!                ncout = nf90_inquire_dimension(ncid, y_dimid, len = ny_ncdf)
!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!                ncout = nf90_inquire_dimension(ncid, z_dimid, len = nz_ncdf)
!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!
!                ncout = nf90_get_var(ncid, fx_id, global_fx, start = starts, count = counts)
!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!                ncout = nf90_get_var(ncid, fy_id, global_fy, start = starts, count = counts)
!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!                ncout = nf90_get_var(ncid, fz_id, global_fz, start = starts, count = counts)
!                IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
!
!                ncout = nf90_close(ncid)
!
!                myfield(:,:,:,1) = global_fx
!                myfield(:,:,:,2) = global_fy
!                myfield(:,:,:,3) = global_fz
! 
!                DEALLOCATE(global_fx)
!                DEALLOCATE(global_fy)
!                DEALLOCATE(global_fz)
! 
!             END IF
!             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
!          END DO     
!
!        END SUBROUTINE read_field_R3toR3_ncdf
!!
!!        !=============================================
!!        ! READ SPECTRAL INFORMATION
!!        !=============================================
!!        SUBROUTINE read_spectral_data(spectrum, mysystem, myindex)
!!          USE global_variables
!!          IMPLICIT NONE
!! 
!!          REAL(pr), DIMENSION(:,:), INTENT(INOUT) :: spectrum
!!          CHARACTER(len=*), INTENT(IN) :: mysystem
!!          INTEGER, INTENT(IN) :: myindex
!!
!!          CHARACTER(80) :: filename
!!          CHARACTER(4) :: indexchar, resol
!!          INTEGER :: i
!!
!!          WRITE(resol, '(i4.4)') n(1)
!!          WRITE(indexchar, '(i4.4)') myindex
!! 
!!          SELECT CASE (mysystem)
!!            CASE ("nse")
!!              filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/NS_Diag/spectra_"//indexchar//".dat"
!!              OPEN (10, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD')
!!              DO i=1, n(1)/2
!!                 READ(10, "(3 G20.12)") spectrum(i,:)   
!!              END DO
!!              CLOSE(10)
!!
!!            CASE ("d1_nse")
!!              filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/D1NS_Diag/spectra_"//indexchar//".dat"
!!              OPEN (10, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD')
!!              DO i=1, n(1)/2
!!                 READ(10, "(3 G20.12)") spectrum(i,:)   
!!              END DO
!!              CLOSE(10)
!!
!!            CASE ("adj")
!!              filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/Adj_Diag/spectra_"//indexchar//".dat"
!!              OPEN (10, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD')
!!              DO i=1, n(1)/2
!!                 READ(10, "(3 G20.12)") spectrum(i,:)   
!!              END DO
!!              CLOSE(10)
!!
!!            CASE ("kappa")
!!              filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/KappaTest/spectra_"//indexchar//".dat"
!!              OPEN (10, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD')
!!              DO i=1, n(1)/2
!!                 READ(10, "(3 G20.12)") spectrum(i,:)   
!!              END DO
!!              CLOSE(10)
!!
!!            CASE ("optim")
!!              filename = "/work/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/Diagnostics/spectra_"//indexchar//".dat"
!!              OPEN (10, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD')
!!              DO i=1, n(1)/2
!!                 READ(10, "(3 G20.12)") spectrum(i,:)   
!!              END DO
!!              CLOSE(10)
!!
!!          END SELECT
!!
!!        END SUBROUTINE read_spectral_data

!!        !=============================================
!!        ! Get UMAX
!!        !=============================================
!!        SUBROUTINE get_umax(umax_global)
!!          USE global_variables
!!          IMPLICIT NONE
!!
!!          REAL(pr), INTENT(OUT) :: umax_global
!!
!!          REAL(pr), DIMENSION(1:4) :: mydata
!!          REAL(pr) :: umax
!!          CHARACTER(200) :: filename
!!          CHARACTER(4) :: resol
!!          INTEGER :: i
!!
!!          WRITE(resol, '(i4.4)') n(1)
!!          filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/NS_Diag/Linf_norms.dat"
!!          
!!          umax_global = -1.0_pr
!!          OPEN (10, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD')
!!          DO i=1,time_steps
!!             READ(10, "(4 G20.12)") mydata
!!             umax = MAX(mydata(2), mydata(3))
!!             IF (umax > umax_global) THEN
!!                umax_global = umax
!!             END IF   
!!          END DO
!!          CLOSE(10)
!! 
!!        END SUBROUTINE get_umax
!!
!!        !===================================================
!!        ! OBTAIN MAX MODE PRESENT IN SOLUTION TO NS
!!        !===================================================
!!        SUBROUTINE get_max_mode(mysystem, myindex) 
!!          USE global_variables
!!          IMPLICIT NONE  
!!          INCLUDE "mpif.h"
!!
!!          CHARACTER(len=*), INTENT(IN) :: mysystem
!!          INTEGER, INTENT(IN) :: myindex
!!          REAL(pr), DIMENSION(1:n(1)/2, 3) :: spectral_data
!!          INTEGER, DIMENSION(1:3) :: index_min, index_max 
!!          INTEGER :: ii
!!          REAL(pr) :: Ener_ratio
!!
!!          IF (rank==0) THEN
!!            CALL read_spectral_data(spectral_data, mysystem, myindex)
!!            !index_min = MINLOC(spectral_data, 1)
!!            index_max = MAXLOC(spectral_data, 1)
!!            ii = index_max(3)+1
!!            Ener_ratio = spectral_data(ii,3)/spectral_data(index_max(3),3)
!!            DO WHILE (Ener_ratio > MACH_EPSILON .AND. ii < n(1)/2)
!!               ii = ii+1
!!               Ener_ratio = spectral_data(ii,3)/spectral_data(index_max(3),3)
!!            END DO
!!  
!!            IF (ii < n(1)/2) THEN
!!               Kmax = spectral_data(ii,1)
!!               !Kmax = MAX(spectral_data(ii,1), spectral_data(index_min(2),1))
!!            ELSE
!!               Kmax = Kcut
!!            END IF
!!             
!!
!!            !IF (index_min(2) < index_max(2)) THEN
!!            !   index_min = MINLOC(spectral_data(index_max(2):n(1)/2,:), 1)
!!            !   Kmax = 1.1_pr*spectral_data(index_min(2) + index_max(2), 1)
!!            !ELSE
!!            !   Kmax = 1.1_pr*spectral_data(index_min(2),1)
!!            !END IF
!!          END IF
!!          CALL MPI_BCAST(Kmax, 1, MPI_REAL8, 0, MPI_COMM_WORLD, Statinfo)
!!
!!        END SUBROUTINE get_max_mode
!!
!!        !===================================================
!!        ! INTERPOLATE IN TIME BETWEEN TWO 2D FUNCTIONS
!!        !=================================================== 
!!        SUBROUTINE interp_2Dfunc(phi, time, myflag)
!!          USE global_variables
!!          IMPLICIT NONE
!!          INCLUDE "mpif.h"
!!
!!          REAL(pr), DIMENSION(:,:), INTENT(OUT) :: phi
!!          REAL(pr), INTENT(IN) :: time
!!          CHARACTER(len=*), INTENT(IN) :: myflag
!!          
!!          REAL(pr), DIMENSION(1:n(1), 1:local_N) :: phiaux
!!          REAL(pr) :: aux, c1, c2
!!          INTEGER :: kk = 1
!!     
!!          CHARACTER(len = 100) :: filename
!!          CHARACTER(3) :: rang
!!          CHARACTER(4) :: resol
!!          CHARACTER(4) :: indexchar
!!
!!          WRITE(rang, '(i3.3)') rank
!!          WRITE(resol, '(i4.4)') n(1)
!!
!!          aux = timevec(kk)
!!          DO WHILE (aux <= time)
!!             kk = kk+1
!!             aux = timevec(kk)
!!          END DO
!!          kk = kk-1
!!          c1 = 1.0_pr - (time - timevec(kk))/(timevec(kk+1) - timevec(kk))
!!          c2 = 1.0_pr - c1
!!
!!          SELECT CASE (myflag)
!!
!!          CASE ("vort")
!!             WRITE(indexchar, '(i4.4)') kk-1
!!             filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/NS_W/w_"//indexchar//"_"//rang//".bin"
!!             CALL read_data(phiaux, n(1), local_N, filename, ".bin")
!!             phi = c1*phiaux
!!
!!             WRITE(indexchar, '(i4.4)') kk             
!!             filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/NS_W/w_"//indexchar//"_"//rang//".bin"
!!             CALL read_data(phiaux, n(1), local_N, filename, ".bin")
!!             phi = phi + c2*phiaux
!!
!!          CASE ("kappa")
!!             WRITE(indexchar, '(i4.4)') kk-1
!!             filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/KappaTest/W_Kappa/w_"//indexchar//"_"//rang//".bin"
!!             CALL read_data(phiaux, n(1), local_N, filename, ".bin")
!!             phi = c1*phiaux
!!
!!             WRITE(indexchar, '(i4.4)') kk             
!!             filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/KappaTest/W_Kappa/w_"//indexchar//"_"//rang//".bin"
!!             CALL read_data(phiaux, n(1), local_N, filename, ".bin")
!!             phi = phi + c2*phiaux
!!
!!          END SELECT
!!
!!        END SUBROUTINE interp_2Dfunc
!!
!!
!!        !===================================================
!!        ! INTERPOLATE IN TIME BETWEEN TWO 2D FIELDS
!!        !=================================================== 
!!        SUBROUTINE interp_2Dfield(phi, time, mytype)
!!          USE global_variables
!!          IMPLICIT NONE
!!          INCLUDE "mpif.h"
!!
!!          REAL(pr), DIMENSION(:,:,:), INTENT(OUT) :: phi
!!          REAL(pr), INTENT(IN) :: time
!!          CHARACTER(len=*), INTENT(IN) :: mytype
!!          
!!          REAL(pr), DIMENSION(1:n(1), 1:local_N, 2) :: phiaux
!!          REAL(pr) :: aux, c1, c2
!!          INTEGER :: kk 
!!     
!!          CHARACTER(len = 100) :: filename
!!          CHARACTER(3) :: rang
!!          CHARACTER(4) :: resol
!!          CHARACTER(4) :: indexchar
!!
!!          WRITE(rang, '(i3.3)') rank
!!          WRITE(resol, '(i4.4)') n(1)
!!
!!          SELECT CASE (mytype)
!!             CASE ("nse")
!!                kk = 1
!!                aux = timevec(kk)
!!                DO WHILE (aux <= time)
!!                   kk = kk+1
!!                   aux = timevec(kk)
!!                END DO
!!                kk = kk-1
!!                c1 = 1.0_pr - (time - timevec(kk))/(timevec(kk+1) - timevec(kk))
!!                c2 = 1.0_pr - c1
!!
!!                WRITE(indexchar, '(i4.4)') kk-1
!!                filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/NS_U/U_"//indexchar//"_"//rang//".bin"
!!                CALL read_field(phiaux, filename)
!!                phi = c1*phiaux
!!
!!                WRITE(indexchar, '(i4.4)') kk             
!!                filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/NS_U/U_"//indexchar//"_"//rang//".bin"
!!                CALL read_field(phiaux, filename)
!!                phi = phi + c2*phiaux
!! 
!!             CASE ("d1_nse")
!!                kk = 1
!!                aux = d1NSE_timevec(kk)
!!                DO WHILE (aux <= time)
!!                   kk = kk+1
!!                   aux = d1NSE_timevec(kk)
!!                END DO
!!                kk = kk-1
!!                c1 = 1.0_pr - (time - d1NSE_timevec(kk))/(d1NSE_timevec(kk+1) - d1NSE_timevec(kk))
!!                c2 = 1.0_pr - c1
!!
!!                WRITE(indexchar, '(i4.4)') kk-1
!!                filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/D1NS_U/U_"//indexchar//"_"//rang//".bin"
!!                CALL read_field(phiaux, filename)
!!                phi = c1*phiaux
!!
!!                WRITE(indexchar, '(i4.4)') kk             
!!                filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/D1NS_U/U_"//indexchar//"_"//rang//".bin"
!!                CALL read_field(phiaux, filename)
!!                phi = phi + c2*phiaux
!!             
!!             CASE ("adj")
!!                kk = 1
!!                aux = adj_timevec(kk)
!!                DO WHILE (aux >= time)
!!                   kk = kk+1
!!                   aux = adj_timevec(kk)
!!                END DO
!!                kk = kk-1
!!                c1 = 1.0_pr - (time - adj_timevec(kk))/(adj_timevec(kk+1) - adj_timevec(kk))
!!                c2 = 1.0_pr - c1
!!
!!                WRITE(indexchar, '(i4.4)') kk-1
!!                filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/Adj_U/U_"//indexchar//"_"//rang//".bin"
!!                CALL read_field(phiaux, filename)
!!                phi = c1*phiaux
!!
!!                WRITE(indexchar, '(i4.4)') kk             
!!                filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/Adj_U/U_"//indexchar//"_"//rang//".bin"
!!                CALL read_field(phiaux, filename)
!!                phi = phi + c2*phiaux
!!
!!              CASE ("kappa")
!!                kk = 1
!!                aux = timevec(kk)
!!                DO WHILE (aux <= time)
!!                   kk = kk+1
!!                   aux = timevec(kk)
!!                END DO
!!                kk = kk-1
!!                c1 = 1.0_pr - (time - timevec(kk))/(timevec(kk+1) - timevec(kk))
!!                c2 = 1.0_pr - c1
!!
!!                WRITE(indexchar, '(i4.4)') kk-1
!!                filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/KappaTest/U_Kappa/U_"//indexchar//"_"//rang//".bin"
!!                CALL read_field(phiaux, filename)
!!                phi = c1*phiaux
!!
!!                WRITE(indexchar, '(i4.4)') kk             
!!                filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/KappaTest/U_Kappa/U_"//indexchar//"_"//rang//".bin"
!!                CALL read_field(phiaux, filename)
!!                phi = phi + c2*phiaux
!!
!!              CASE ("file")
!!                IF (T0 < MACH_EPSILON) THEN
!!                   filename = "/work/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/Initialread_field_R3toR3_ncdfData/U_NS/U_"//indexchar//"_"//rang//".bin"
!!                   CALL read_field(phiaux, filename)
!!                   phi = phiaux
!!                ELSE 
!!                   kk = 1
!!                   aux = timevec(kk)
!!                   DO WHILE (aux <= time)
!!                      kk = kk+1
!!                      aux = timevec(kk)
!!                   END DO
!!                   kk = kk-1
!!                   c1 = 1.0_pr - (time - timevec(kk))/(timevec(kk+1) - timevec(kk))
!!                   c2 = 1.0_pr - c1
!!
!!                   WRITE(indexchar, '(i4.4)') kk-1
!!                   filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/KappaTest/U_Kappa/U_"//indexchar//"_"//rang//".bin"
!!                   CALL read_field(phiaux, filename)
!!                   phi = c1*phiaux
!!
!!                   WRITE(indexchar, '(i4.4)') kk             
!!                   filename = "/scratch/ayalada/MAXPALINS/DataGenerated/N_"//resol//"/KappaTest/U_Kappa/U_"//indexchar//"_"//rang//".bin"
!!                   CALL read_field(phiaux, filename)
!!                   phi = phi + c2*phiaux
!!                END IF
!!
!!          END SELECT
!!
!!        END SUBROUTINE interp_2Dfield
!!
!!
!        !======================================================
!        ! This subroutine changes the storage of the velocity
!        ! from matrix to vector one (for sub. krylov) MATVEC
!        !======================================================
!        SUBROUTINE matvec (mat, vec)       
!          USE global_variables
!          IMPLICIT NONE
!          REAL (pr), DIMENSION (:,:,:,:), INTENT (IN) :: mat
!          REAL (pr), DIMENSION (1:n_dim), INTENT (OUT) :: vec

!          INTEGER ix, iy, iz, ii, i
! 
!          ii = 1
!          DO i = 1, 3
!            DO iz = 1, local_N
!               DO iy = 1, n(2)   
!                  DO ix = 1, n(1)
!                     vec(ii) = mat(ix, iy, iz, i)
!                     ii = ii + 1
!                  END DO
!               END DO
!             END DO
!          END DO

!        END SUBROUTINE matvec

!        !========================================================
!        !  This subroutine change the storage of the velocity
!        !  from vector to matrix (for sub. Krylov)   VECMAT
!        !========================================================
!        SUBROUTINE vecmat (vec, mat)
!          USE global_variables
!          IMPLICIT NONE

!          REAL(pr), DIMENSION (1:n_dim), INTENT (IN) :: vec
!          REAL(pr), DIMENSION (:,:,:,:), INTENT (OUT) :: mat
! 
!          INTEGER ix, iy, iz, ii, i

!          !--vector form  --->  matrix form 
!          ii = 1
!          DO i = 1, 3
!             DO iz = 1,local_N
!                DO iy = 1,n(2)
!                   DO ix = 1, n(1)
!                      mat(ix,iy,iz, i) = vec(ii)
!                      ii = ii + 1
!                   END DO
!                END DO
!             END DO
!          END DO

!        END SUBROUTINE vecmat

        !===============================================
        ! NETCDF ERROR HANDLE ROUTINE
        !===============================================
        SUBROUTINE ncdf_error_handle(nerror)
          USE global_variables
          USE netcdf
          IMPLICIT NONE

          INTEGER, INTENT(IN) :: nerror
          CHARACTER(80) :: error_string
          CHARACTER(2) :: K0txt, E0txt
          WRITE(K0txt,'(i2.2)') K0_index
          WRITE(E0txt,'(i2.2)') E0_index
 
          error_string = NF90_STRERROR(nerror)
 
          OPEN(10, FILE="./LOGFILES/maxET_info.log", STATUS='OLD', POSITION='APPEND')
          WRITE(10,*) " Error reading netCDF file. "//error_string
          CLOSE(10)
  
        END SUBROUTINE ncdf_error_handle


        !===============================================
        ! MESSAGE HANDLE ROUTINE
        !===============================================
        SUBROUTINE nse_msg_handle(nmsg)
          USE global_variables
          IMPLICIT NONE
          INCLUDE "mpif.h"
          INTEGER, INTENT(IN) :: nmsg
          CHARACTER(80) :: msg_string
          CHARACTER(2) :: K0txt, E0txt
          WRITE(K0txt,'(i2.2)') K0_index
          WRITE(E0txt,'(i2.2)') E0_index
          SELECT CASE (nmsg)
            CASE (0)
               msg_string = "      Loading initial data..."
            CASE (1)
               msg_string = "      Initial data OK!" 
            CASE (2)
               msg_string = "      Starting NSE Solver" 
            CASE (3)
               msg_string = "      NSE Solver OK!"
            CASE (4)
               msg_string = "      Time step OK!"
            CASE (5)
               msg_string = "      Reducing time step..."
            CASE (6)
               msg_string = "      Time step too small... Reduce tolerance!"
            CASE (10)
               msg_string = "      Saving velocity field..."
            CASE (11)
               msg_string = "      Velocity field OK!"
            CASE (12)
               msg_string = "      Saving vorticity field..."
            CASE (13)
               msg_string = "      Vorticity field OK!"
            CASE (14)
               msg_string = "      Saving diagnostic fields..."
            CASE (15)
               msg_string = "      Diagnostic fields OK!"
            CASE (20)
               msg_string = "      Loading adjoint initial data..."
            CASE (21)
               msg_string = "      Adjoint initial data OK!" 
            CASE (22)
               msg_string = "      Starting adjoint Solver" 
            CASE (23)
               msg_string = "      Adjoint Solver OK!"
            CASE (24)
               msg_string = "      Adjoint Time step OK!"
            CASE (25)
               msg_string = "      Reducing adjoint time step..."
            CASE (26)
               msg_string = "      Adjoint Time step too small... Reduce tolerance!"
            CASE (30)
               msg_string = "      Calculating diagnostics..."
            CASE (31)
               msg_string = "      Diagnostics OK!"
            CASE (32)
               msg_string = "      Optimal tau is too large!"
            CASE (40)
               msg_string = "      Evaluating RK3..."
            CASE (41)
               msg_string = "      RK3 OK!"
            CASE (42)
               msg_string = "      Evaluating RHS for RK3..."
            CASE (43) 
               msg_string = "      RHS for RK3 OK!"
            CASE (50)
               msg_string = "      Evaluating adjoint RK3..."
            CASE (51)
               msg_string = "      Adjoint RK3 OK!"
            CASE (52)
               msg_string = "      Evaluating RHS for adjoint RK3..."
            CASE (53) 
               msg_string = "      RHS for adjoint RK3 OK!"
            CASE (60) 
               msg_string = "      Set Initial Velocity Field ... ..."
          END SELECT

          IF (rank==0) THEN
             if (nmsg==100) then
                OPEN(10, FILE="./LOGFILES/maxET_info.log", STATUS='REPLACE')
             else
                OPEN(10, FILE="./LOGFILES/maxET_info.log", STATUS = 'OLD', POSITION = 'APPEND')
             end if
             WRITE(10,*) msg_string
             CLOSE(10)
          END IF 
          CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)

        END SUBROUTINE nse_msg_handle



        !===============================================
        ! OPTIMZATION ERROR HANDLE ROUTINE
        !===============================================
        SUBROUTINE optim_msg_handle(nmsg)
          USE global_variables
          IMPLICIT NONE
          INCLUDE "mpif.h"

          INTEGER, INTENT(IN) :: nmsg
          CHARACTER(80) :: msg_string
          CHARACTER(2) :: E0txt
          WRITE(E0txt,'(i2.2)') E0_index

  
          SELECT CASE (nmsg)
            CASE (0)
               msg_string = "      Cost functional not increasing."
            CASE (1)
               msg_string = "      Optimization terminated." 
            CASE (10)
               msg_string = "   Starting FixK0E0..."
            CASE (11)
               msg_string = "   FixK0E0 OK!"
            CASE (12)
               msg_string = "   Could not FixK0E0... Stop optimization!"
            CASE (13)
               msg_string = "   Could not move to constraint... Stop optimization!"
            CASE (14)
               msg_string = "   Could not move to constraint... Iterations continue...!" 
            CASE (20)
               msg_string = "      Starting mnbrak..."
            CASE (21)
               msg_string = "      mnbrak OK!"
            CASE (30)
               msg_string = "      Starting Brent method..."
            CASE (31)
               msg_string = "      Brent method OK!"
            CASE (32)
               msg_string = "      Optimal tau is too large!"
            CASE (40)
               msg_string = "      Start finite time optimization maxET under fixed_E0 ... ..."
          END SELECT

          IF (rank==0) THEN   
             OPEN(10, FILE="./LOGFILES/maxET_info.log", STATUS='OLD', POSITION='APPEND')
             WRITE(10,*) msg_string
             CLOSE(10)
          END IF 
          CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)

        END SUBROUTINE optim_msg_handle



!        !=====================================================
!        ! DOUBLE THE RESOLUTION OF A GIVEN FIELD N1= 2*N0 
!        ! IN FOURIER SPACE
!        !=====================================================
!        SUBROUTINE interpolate3DFourier(f0, f1, N0, N1)
!          USE global_variables
!          USE FFT2
!          IMPLICIT NONE
!          INCLUDE "mpif.h"

!          REAL(pr), DIMENSION(:,:,:), INTENT(IN) :: f0
!          REAL(pr), DIMENSION(:,:,:), INTENT(OUT) :: f1
!          INTEGER, INTENT(IN) :: N0, N1

!          COMPLEX(pr), DIMENSION(:,:,:), ALLOCATABLE :: aux0, faux0, aux1, faux1, faux0_2D, faux1_2D         

!          INTEGER :: request, rank_dest, ranks_received, nn
!          INTEGER :: group_all, group_new, comm_new
!          INTEGER, DIMENSION(:), ALLOCATABLE :: group_ind

!          CALL fftw3d_fortran_mpi_create_plan(fwdplan_Fixres, MPI_COMM_WORLD, N0, N0, N0, FFTW_FORWARD, FFTW_ESTIMATE)
!          CALL fftw3d_fortran_mpi_create_plan(bwdplan_Fixres, MPI_COMM_WORLD, N0, N0, N0, FFTW_BACKWARD, FFTW_ESTIMATE)
!          CALL fftwnd_fortran_mpi_local_sizes(fwdplan_Fixres, local_nlastFixres, local_last_startFixres,&
!               local_nlast_after_transFixres, local_last_start_after_transFixres, total_local_sizeFixres)

!          ALLOCATE( aux0(1:N0,1:N0,1:local_nlastFixres) )
!          ALLOCATE( aux1(1:n(1),1:n(2),1:local_nlast) )
!          ALLOCATE( faux0(1:N0,1:N0,1:local_nlastFixres) )
!          ALLOCATE( faux1(1:n(1),1:n(2),1:local_nlast) )

!          aux0 = CMPLX(f0, 0.0_pr)
!          CALL ffourier_Fixres(aux0, faux0, N0) 
!          
!          !DO kk=1,local_nlastFixres
!          !   CALL zeroPadding2D(faux0,faux1Padd)

!          !END DO
!             

!          !IF (rank==0) THEN
!          !   ALLOCATE( faux0_global(1:N0,1:N0,1:N0) ) 
!          !   ALLOCATE( faux1_global(1:N1,1:N1,1:N1) )  
!          !END IF
!          !CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
!          !CALL MPI_GATHER(faux0, total_local_sizeFixres, MPI_COMPLEX, faux0_global, total_local_sizeFixres, MPI_COMPLEX, 0, MPI_COMM_WORLD, Statinfo)
!     
!          !IF (rank==0) THEN
!          !   faux1_global(1:N0/2,1:N0/2,1:N0/2) = faux0_global(1:N0/2,1:N0/2,1:N0/2)
!          !   faux1_global(N1-N0/2+1:N1,1:N0/2,1:N0/2) = faux0_global(N0/2+1:N0,1:N0/2,1:N0/2)
!          !   faux1_global(1:N0/2,N1-N0/2+1:N1,1:N0/2) = faux0_global(1:N0/2,N0/2+1:N0,1:N0/2)
!          !   faux1_global(N1-N0/2+1:N1,N1-N0/2+1:N1,1:N0/2) = faux0_global(N0/2+1:N0,N0/2+1:N0,1:N0/2)
!             
!          !   faux1_global(1:N0/2,1:N0/2,N1-N0/2+1:N1) = faux0_global(1:N0/2,1:N0/2,N0/2+1:N0)
!          !   faux1_global(N1-N0/2+1:N1,1:N0/2,N1-N0/2+1:N1) = faux0_global(N0/2+1:N0,1:N0/2,N0/2+1:N0)
!          !   faux1_global(1:N0/2,N1-N0/2+1:N1,N1-N0/2+1:N1) = faux0_global(1:N0/2,N0/2+1:N0,N0/2+1:N0)
!          !   faux1_global(N1-N0/2+1:N1,N1-N0/2+1:N1,N1-N0/2+1:N1) = faux0_global(N0/2+1:N0,N0/2+1:N0,N0/2+1:N0)
!          !END IF
!          !CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
!          !CALL MPI_SCATTER(faux1_global, total_local_size, MPI_COMPLEX, faux1, total_local_size, MPI_COMPLEX, 0, MPI_COMM_WORLD, Statinfo)
!          !faux1 = REAL(N1/N0,pr)**3*faux1 

!          CALL bfourier(faux1, aux1)
!          f1 = REAL(aux1)

!          !IF (rank==0) THEN
!          !   DEALLOCATE( faux0_global )
!          !   DEALLOCATE( faux1_global )
!          !END IF
!          !CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)


!          !CALL MPI_COMM_GROUP(MPI_COMM_WORLD, group_all, Statinfo)
!          !DO nn=1,np/2
!          !   IF (rank < np/2) THEN
!          !      group_ind(nn) = nn-1
!          !   ELSE 
!          !      group_ind(nn) = nn-1+np/2
!          !   END IF
!          !END DO
!             
!          !CALL MPI_GROUP_INCL(group_all, np/2, group_ind, group_new, Statinfo)
!          !CALL MPI_COMM_CREATE(MPI_COMM_WORLD, group_new, comm_new, Statinfo)
!                

!          !IF (N1 > N0) THEN
!          !   IF (rank < np/2) THEN
!          !     ranks_received = 0
!          !     DO nn=0, np/2-1
!          !         rank_dest = nn/2
!          !         IF (rank==nn) THEN
!          !            IF (rank_dest==rank) THEN
!          !               faux1(1:N0/2,1:N0/2,ranks_received*local_nlastFixres+1:(ranks_received+1)*local_nlastFixres) = faux0(1:N0/2,1:N0/2,:)
!          !               faux1(N1-N0/2+1:N1,1:N0/2,ranks_received*local_nlastFixres+1:(ranks_received+1)*local_nlastFixres) = faux0(N0/2+1:N0,1:N0/2,:)
!          !               faux1(1:N0/2,N1-N0/2+1:N1,ranks_received*local_nlastFixres+1:(ranks_received+1)*local_nlastFixres) = faux0(1:N0/2,N0/2+1:N0,:)
!          !               faux1(N1-N0/2+1:N1,N1-N0/2+1:N1,ranks_received*local_nlastFixres+1:(ranks_received+1)*local_nlastFixres) = faux0(N0/2+1:N0,N0/2+1:N0,:)
!          !            
!          !               ranks_received = ranks_received+1
!          !            ELSE
!          !               CALL MPI_SEND(faux0, total_local_sizeFixres, MPI_COMPLEX, rank_dest, nn, MPI_COMM_WORLD, Statinfo)
!          !            END IF
!          !         END IF

!          !         IF (rank==rank_dest) THEN
!          !            IF (rank .NE. nn) THEN
!          !               CALL MPI_RECV(aux0, total_local_sizeFixres, MPI_COMPLEX, 2*rank+ranks_received, nn, MPI_COMM_WORLD, Statinfo)
!          !               faux1(1:N0/2,1:N0/2,ranks_received*local_nlastFixres+1:(ranks_received+1)*local_nlastFixres) = aux0(1:N0/2,1:N0/2,:)
!          !               faux1(N1-N0/2+1:N1,1:N0/2,ranks_received*local_nlastFixres+1:(ranks_received+1)*local_nlastFixres) = aux0(N0/2+1:N0,1:N0/2,:)
!          !               faux1(1:N0/2,N1-N0/2+1:N1,ranks_received*local_nlastFixres+1:(ranks_received+1)*local_nlastFixres) = aux0(1:N0/2,N0/2+1:N0,:)
!          !               faux1(N1-N0/2+1:N1,N1-N0/2+1:N1,ranks_received*local_nlastFixres+1:(ranks_received+1)*local_nlastFixres) = aux0(N0/2+1:N0,N0/2+1:N0,:)
!                      
!          !               ranks_received = ranks_received+1

!          !            END IF
!          !         END IF
!          !         CALL MPI_BARRIER(comm_new, Statinfo)
!          !      END DO
!                
!          !   ELSE
!          !      ranks_received = 1
!          !      DO nn=np-1, np/2, -1
!          !         rank_dest = nn/2 + np/2
!          !         IF (rank==nn) THEN
!          !            IF (rank_dest==rank) THEN
!          !               faux1(1:N0/2,1:N0/2,ranks_received*local_nlastFixres+1:(ranks_received+1)*local_nlastFixres) = faux0(1:N0/2,1:N0/2,:)
!          !               faux1(N1-N0/2+1:N1,1:N0/2,ranks_received*local_nlastFixres+1:(ranks_received+1)*local_nlastFixres) = faux0(N0/2+1:N0,1:N0/2,:)
!          !               faux1(1:N0/2,N1-N0/2+1:N1,ranks_received*local_nlastFixres+1:(ranks_received+1)*local_nlastFixres) = faux0(1:N0/2,N0/2+1:N0,:)
!          !               faux1(N1-N0/2+1:N1,N1-N0/2+1:N1,ranks_received*local_nlastFixres+1:(ranks_received+1)*local_nlastFixres) = faux0(N0/2+1:N0,N0/2+1:N0,:)
!                      
!          !               ranks_received = ranks_received-1
!          !            ELSE
!          !               CALL MPI_SEND(faux0, total_local_sizeFixres, MPI_COMPLEX, rank_dest, nn, MPI_COMM_WORLD, Statinfo)
!          !            END IF
!          !         END IF

!          !         IF (rank==rank_dest) THEN
!          !            IF (rank .NE. nn) THEN
!          !               CALL MPI_RECV(aux0, total_local_sizeFixres, MPI_COMPLEX, 2*(rank-np/2)+ranks_received, nn, MPI_COMM_WORLD, Statinfo)
!          !               faux1(1:N0/2,1:N0/2,ranks_received*local_nlastFixres+1:(ranks_received+1)*local_nlastFixres) = aux0(1:N0/2,1:N0/2,:)
!          !               faux1(N1-N0/2+1:N1,1:N0/2,ranks_received*local_nlastFixres+1:(ranks_received+1)*local_nlastFixres) = aux0(N0/2+1:N0,1:N0/2,:)
!          !               faux1(1:N0/2,N1-N0/2+1:N1,ranks_received*local_nlastFixres+1:(ranks_received+1)*local_nlastFixres) = aux0(1:N0/2,N0/2+1:N0,:)
!          !               faux1(N1-N0/2+1:N1,N1-N0/2+1:N1,ranks_received*local_nlastFixres+1:(ranks_received+1)*local_nlastFixres) = aux0(N0/2+1:N0,N0/2+1:N0,:)
!          !            
!          !               ranks_received = ranks_received-1

!          !            END IF
!          !         END IF
!          !         CALL MPI_BARRIER(comm_new, Statinfo)
!          !      END DO
! 
!          !   END IF

!          !   CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
!          !   CALL bfourier(faux1, aux1)
!          !   f1 = REAL(aux1)

!          !ELSE

!          !END IF

!          DEALLOCATE( aux0 )
!          DEALLOCATE( aux1 )
!          DEALLOCATE( faux0 )
!          DEALLOCATE( faux1 )

!        END SUBROUTINE interpolate3DFourier


!        !=====================================================
!        ! DOUBLE THE RESOLUTION OF A GIVEN FIELD N1= 2*N0
!        !=====================================================
!        SUBROUTINE interpolate3D(f0, f1, N0, N1)
!          USE global_variables
!          IMPLICIT NONE
!          INCLUDE "mpif.h"

!          REAL(pr), DIMENSION(:,:,:), INTENT(IN) :: f0
!          REAL(pr), DIMENSION(:,:,:), INTENT(OUT) :: f1
!          INTEGER, INTENT(IN) :: N0, N1

!          INTEGER :: ii1, jj1, kk1, ii0, jj0, kk0, ii0_aux, jj0_aux, kk0_aux
!          REAL(pr) :: dx1, dy1, dz1, dx0, dy0, dz0, xp, yp, zp
!          LOGICAL :: same_plane, same_line, same_point         

!          dx0 = 1.0_pr/REAL(N0,pr)
!          dy0 = dx0
!          dz0 = dx0
!          dx1 = 1.0_pr/REAL(N1,pr)
!          dy1 = dx1
!          dz1 = dx1

!          IF (N1>N0) THEN
!             DO kk1=1,N1
!                IF (MOD(kk1-1,2)==0) THEN
!                   same_plane = .TRUE.
!                ELSE
!                   same_plane = .FALSE.
!                END IF
!              
!                DO jj1=1,N1
!                   IF (MOD(jj1-1,2)==0) THEN
!                      same_line = .TRUE.
!                   ELSE
!                      same_line = .FALSE.
!                   END IF
!                   
!                   DO ii1=1,N1
!                      IF (MOD(jj1-1,2)==0) THEN
!                         same_point = .TRUE.
!                      ELSE
!                         same_point = .FALSE.
!                      END IF
!                      
!                      IF ( same_plane .AND. same_line .AND. same_point ) THEN
!                         ii0 = (ii1+1)/2
!                         jj0 = (jj1+1)/2
!                         kk0 = (kk1+1)/2
!                         f1(ii1,jj1,kk1) = f0(ii0,jj0,kk0)

!                      ELSEIF ( same_plane .AND. same_line .AND. .NOT. same_point) THEN
!                         ii0 = (ii1)/2
!                         jj0 = (jj1+1)/2
!                         kk0 = (kk1+1)/2
!                         IF (ii0 == N0) THEN
!                            ii0_aux = 1
!                         ELSE
!                            ii0_aux = ii0+1
!                         END IF
!                         f1(ii1,jj1,kk1) = 0.5_pr*( f0(ii0,jj0,kk0) + f0(ii0_aux,jj0,kk0) )

!                      ELSEIF ( same_plane .AND. .NOT. same_line .AND. same_point) THEN
!                         ii0 = (ii1+1)/2
!                         jj0 = (jj1)/2
!                         kk0 = (kk1+1)/2
!                         IF (jj0 == N0) THEN
!                            jj0_aux = 1
!                         ELSE
!                            jj0_aux = jj0+1
!                         END IF
!                         f1(ii1,jj1,kk1) = 0.5_pr*( f0(ii0,jj0,kk0) + f0(ii0,jj0_aux,kk0) )

!                      ELSEIF ( same_plane .AND. .NOT. same_line .AND. .NOT. same_point) THEN
!                         ii0 = (ii1)/2
!                         jj0 = (jj1)/2
!                         kk0 = (kk1+1)/2
!                         IF (ii0 == N0) THEN
!                            ii0_aux = 1
!                         ELSE
!                            ii0_aux = ii0+1
!                         END IF
!                         IF (jj0 == N0) THEN
!                            jj0_aux = 1
!                         ELSE
!                            jj0_aux = jj0+1
!                         END IF
!                         f1(ii1,jj1,kk1) = 0.25_pr*( f0(ii0,jj0,kk0) + f0(ii0_aux,jj0,kk0) + f0(ii0,jj0_aux,kk0) + f0(ii0_aux,jj0_aux,kk0) )

!                      ELSEIF ( .NOT. same_plane .AND. same_line .AND. same_point) THEN
!                         ii0 = (ii1+1)/2
!                         jj0 = (jj1+1)/2
!                         kk0 = (kk1)/2
!                         IF (kk0 == N0) THEN
!                            kk0_aux = 1
!                         ELSE
!                            kk0_aux = kk0+1
!                         END IF
!                         f1(ii1,jj1,kk1) = 0.5_pr*( f0(ii0,jj0,kk0) + f0(ii0,jj0,kk0_aux) )

!                      ELSEIF ( .NOT. same_plane .AND. same_line .AND. .NOT. same_point) THEN
!                         ii0 = (ii1)/2
!                         jj0 = (jj1+1)/2
!                         kk0 = (kk1)/2
!                         IF (ii0 == N0) THEN
!                            ii0_aux = 1
!                         ELSE
!                            ii0_aux = ii0+1
!                         END IF
!                         IF (kk0 == N0) THEN
!                            kk0_aux = 1
!                         ELSE
!                            kk0_aux = kk0+1
!                         END IF
!                         f1(ii1,jj1,kk1) = 0.25_pr*( f0(ii0,jj0,kk0) + f0(ii0_aux,jj0,kk0) + f0(ii0,jj0,kk0_aux) + f0(ii0_aux,jj0,kk0_aux) )

!                      ELSEIF ( .NOT. same_plane .AND. .NOT. same_line .AND. same_point) THEN
!                         ii0 = (ii1+1)/2
!                         jj0 = (jj1)/2
!                         kk0 = (kk1)/2
!                         IF (jj0 == N0) THEN
!                            jj0_aux = 1
!                         ELSE
!                            jj0_aux = jj0+1
!                         END IF
!                         IF (kk0 == N0) THEN
!                            kk0_aux = 1
!                         ELSE
!                            kk0_aux = kk0+1
!                         END IF
!                         f1(ii1,jj1,kk1) = 0.25_pr*( f0(ii0,jj0,kk0) + f0(ii0,jj0_aux,kk0) + f0(ii0,jj0,kk0_aux) + f0(ii0,jj0_aux,kk0_aux) )

!                      ELSEIF ( .NOT. same_plane .AND. .NOT. same_line .AND. .NOT. same_point) THEN
!                         ii0 = (ii1)/2
!                         jj0 = (jj1)/2
!                         kk0 = (kk1)/2
!                         IF (ii0 == N0) THEN
!                            ii0_aux = 1
!                         ELSE
!                            ii0_aux = ii0+1
!                         END IF
!                         IF (jj0 == N0) THEN
!                            jj0_aux = 1
!                         ELSE
!                            jj0_aux = jj0+1
!                         END IF
!                         IF (kk0 == N0) THEN
!                            kk0_aux = 1
!                         ELSE
!                            kk0_aux = kk0+1
!                         END IF
!                         f1(ii1,jj1,kk1) = 0.125_pr*( f0(ii0,jj0,kk0) + f0(ii0_aux,jj0,kk0) + f0(ii0,jj0_aux,kk0) + f0(ii0,jj0,kk0_aux) + &
!                                                      f0(ii0,jj0_aux,kk0_aux) + f0(ii0_aux,jj0,kk0_aux) + f0(ii0_aux,jj0_aux,kk0) + f0(ii0_aux,jj0_aux,kk0_aux) )
!                      END IF
!                   END DO
!                END DO
!             END DO
!          END IF

!          !jj0 = 1
!          !DO jj=local_last_start,local_last_start+local_nlast-1
!          !     yp = REAL(jj,pr)*dy1 
!          !     DO WHILE ( yp > REAL(jj0,pr)*dy0)
!          !        jj0 = jj0 + 1
!          !     END DO
!          !     ii0 = 1   
!          !     DO ii=0,N1-1
!          !        xp = REAL(ii,pr)*dx1
!          !        DO WHILE ( xp > REAL(ii0,pr)*dx0 )
!          !           ii0 = ii0 + 1
!          !        END DO                

!          !        f1(ii,jj) = eval_function2D(f0,N0,ii0,jj0,xp,yp)

!          !    END DO
!          ! END DO

!        END SUBROUTINE interpolate3D 



        !==========================================
        !     SAVE OPTIMIZATION DIAGNOSTICS
        !==========================================
        SUBROUTINE save_diagnostics_optim(myOptimType, iter, tau, beta, J, ener, ens, dEdt_visc, dEdt_NL)   ! Feb 15, 2018
          USE global_variables
          IMPLICIT NONE
          CHARACTER(len=*), INTENT(IN) :: myOptimType
          REAL(pr), DIMENSION(1:3), INTENT(IN) :: ener, ens
          REAL(pr), INTENT(IN) :: tau, beta, J, dEdt_visc, dEdt_NL
          INTEGER, INTENT(IN) :: iter

          CHARACTER(100) :: filename
          CHARACTER(2) :: E0txt
          WRITE(E0txt, '(i2.2)') E0_index
          !filename = TRIM(work_pathname)//"_E"//E0txt//"_maxETiterinfo.dat"   ! Newly added on May 8, 2017
          filename = TRIM(scratch_pathname)//"_maxETiterinfo.dat"
          IF (iter == 0) THEN
             OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'REPLACE')
             WRITE(10,*) "# Iter  Tau  Beta  J  Ener  Ens  LPSnorm  LPSoverT" 
          ELSE
             OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
          END IF
          WRITE(10, "(I5.4, 7 G20.12)") iter, tau, beta, J, SUM(ener), SUM(ens), LPSnorm, LPSnorm/endTime
          CLOSE(10)

        END SUBROUTINE save_diagnostics_optim
 


        !============================================
        ! SAVE LINE MINIMIZATION DATA
        !============================================
        SUBROUTINE save_linemin_data(tA, tB, tC, FA, FB, FC, iter, mymode, myindex)
          USE global_variables
          IMPLICIT NONE
          INCLUDE "mpif.h"
          
          REAL(pr), INTENT(IN) :: tA, tB, tC, FA, FB, FC
          INTEGER, INTENT(IN) :: iter
          !CHARACTER(len=*), INTENT(IN) :: mysystem
          CHARACTER(len=*), INTENT(IN) :: mymode
          INTEGER, INTENT(IN) :: myindex
          CHARACTER(100) :: filename
          CHARACTER(2) :: E0txt, IGtxt
          CHARACTER(4) :: indextxt
          WRITE(E0txt,'(i2.2)') E0_index
          WRITE(indextxt,'(i4)') myindex

          IF (rank==0) THEN
             !filename = "/work/yund0050/MultiObjective_095_01/WEIGHT"//WEIGHTtxt//"_E"//E0txt//"_"//mysystem//"_IG"//IGtxt//"_lineMin_info.dat"
             !filename = work_pathname//"_E"//E0txt//"_lineMin_OPT"//trim(adjustl(indextxt))//".dat"
             filename = "./LOGFILES/maxET_E"//"_mnbrak_LineMin_OPT"//trim(adjustl(indextxt))//".dat"
             SELECT CASE (mymode)
               CASE ("replace")
                 OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'REPLACE')
               CASE ("append")
                OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
             END SELECT
             WRITE(10, "(I5.4, 6 G20.12)") iter, tA, tB, tC, FA, FB, FC
             CLOSE(10)
          END IF 
          CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)

        END SUBROUTINE save_linemin_data










        !============================================
        ! SAVE LINE MINIMIZATION DATA
        !============================================
        SUBROUTINE save_CFL_dt(myiter, mydt, myerror, mysystem, myindex)
          USE global_variables
          IMPLICIT NONE
          INCLUDE "mpif.h"
          INTEGER, INTENT(IN) :: myiter
          REAL(pr), INTENT(IN) :: mydt, myerror
          CHARACTER(len=*), INTENT(IN) :: mysystem
          INTEGER, INTENT(IN) :: myindex
          CHARACTER(100) :: filename
          CHARACTER(2) :: E0txt
          CHARACTER(4) :: indextxt
          WRITE(E0txt,'(i2.2)') E0_index
          WRITE(indextxt,'(i4)') myindex
          IF (rank==0) THEN
             SELECT CASE (mysystem)
                case("fwdTE")
                   filename = "./LOGFILES/CFLDT_fwdTE_OPT"//trim(adjustl(indextxt))//".dat"
                   IF (myiter==1) THEN
                      OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'REPLACE')
                   ELSE
                      OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
                   END IF
                case("bwdADJ")
                   filename = "./LOGFILES/CFLDT_bwdADJ_OPT"//trim(adjustl(indextxt))//".dat"
                   IF (myiter==1) THEN
                      OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'REPLACE')
                   ELSE
                      OPEN(10, FILE = filename, FORM = 'FORMATTED', STATUS = 'OLD', POSITION = 'APPEND')
                   END IF
             END SELECT
             WRITE(10, "(I5.4, 2 G20.12)") myiter, mydt, myerror
             CLOSE(10)
          END IF 
          CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
        END SUBROUTINE save_CFL_dt






        !===============================================
        ! OPTIMIZATION ERROR HANDLE ROUTINE
        !===============================================
        SUBROUTINE optim_error_handle(nerror)
          USE global_variables
          IMPLICIT NONE
          INCLUDE "mpif.h"
          INTEGER, INTENT(IN) :: nerror
          CHARACTER(80) :: error_string
          CHARACTER(2) :: K0txt, E0txt
          WRITE(K0txt,'(i2.2)') K0_index
          WRITE(E0txt,'(i2.2)') E0_index
          SELECT CASE (nerror)
            CASE (1)
               error_string = " maxdEdt: Going uphill... Verify gradient!"
            CASE (2)
               error_string = " maxdEdt: Could not bracket minimum."
            CASE (3)
               error_string = " maxdEdt: Decreasing tau..."
            CASE (11)
               error_string = " FixK0E0: Going uphill... Verify gradient!"
            CASE (12)
               error_string = " FixK0E0: Could not bracket minimum."
            CASE (13)
               error_string = " FixK0E0: Decreasing tau..."
            CASE (15)
               error_string = " FixK0E0: Could not bracket minimum. Trying tau = TauMax..."
          END SELECT
          IF (rank==0) THEN   
             OPEN(10, FILE="./LOGFILES/maxET_info.log", STATUS='OLD', POSITION='APPEND')
             WRITE(10,*) "      Error during optimization. "//error_string
             CLOSE(10)
          END IF 
          CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)

        END SUBROUTINE optim_error_handle

!==========================================
! SAVE FIELD IN R3
!==========================================
        SUBROUTINE coarse(idx0, idx1, step, ratio)
          USE global_variables
          USE netcdf
          IMPLICIT NONE
          INCLUDE "mpif.h"

          integer, intent(in) :: idx0, idx1, step, ratio
          CHARACTER(2) :: Fx_txt, Fy_txt, Fz_txt
          CHARACTER(200) :: f_in, f_out
          CHARACTER(4) :: optchar
          CHARACTER(3) :: Fob_txt
          
          
          INTEGER :: nn,i1,i2,i3,idx
          real(pr), dimension(:,:,:), allocatable :: aux
          INTEGER :: ncout, ncid, varids(3), dimids(3), f_id
          INTEGER :: x_dimid, y_dimid, z_dimid, ux_id, uy_id, uz_id
          
          if (rank == 0) then
             allocate(aux(1:n(1)/ratio, 1:n(2)/ratio, 1:n(3)/ratio))
          end if

          
          do idx = idx0, idx1, step
             WRITE(optchar, '(i4)') idx

             ! save Uvec
             f_in = TRIM(scratch_pathname)//"_Uvec_fwdTE0_OPT"//trim(adjustl(optchar))//".nc"
             Fx_txt = "Ux"
             Fy_txt = "Uy"
             Fz_txt = "Uz"
             CALL read_field_R3toR3_ncdf2(Uvec, f_in, Fx_txt, Fy_txt, Fz_txt)
             f_out = TRIM(scratch_pathname)//"_Uvec_fwdTE0_OPT_coarse"//trim(adjustl(optchar))//".nc"
             if (rank ==0 ) then
                ncout = nf90_create(f_out, NF90_CLOBBER, ncid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_dim(ncid, "x", n(1)/ratio, x_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_dim(ncid, "y", n(2)/ratio, y_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_dim(ncid, "z", NF90_UNLIMITED, z_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             dimids =  (/ x_dimid, y_dimid, z_dimid /)
             ncout = nf90_def_var(ncid, TRIM(fx_txt), NF90_DOUBLE, dimids, ux_id)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_var(ncid, TRIM(fy_txt), NF90_DOUBLE, dimids, uy_id)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_var(ncid, TRIM(fz_txt), NF90_DOUBLE, dimids, uz_id)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                     
             ncout = nf90_enddef(ncid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
          end if
          
             
             do nn = 1, 3
                CALL MPI_GATHER(Uvec(:,:,:,nn), total_local_size, MPI_DOUBLE_PRECISION, global_u, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
                if (rank == 0) then
                   aux = 0.0_pr
                   do i3 = 1, n(3)/ratio
                      do i2 = 1, n(2)/ratio
                         do i1 = 1, n(1)/ratio
                            aux(i1,i2,i3) = global_u(i1*ratio-(ratio-1), i2*ratio-(ratio-1), i3*ratio-(ratio-1))
                         end do
                      end do
                   end do
                   select case (nn)
                   case (1)
                      ncout = nf90_put_var(ncid, ux_id, aux)
                      IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   case (2)
                      ncout = nf90_put_var(ncid, uy_id, aux)
                      IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   case (3)
                      ncout = nf90_put_var(ncid, uz_id, aux)
                      IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   end select
                   
                end if
             end do
             if (rank == 0) then
                ncout = nf90_close(ncid)
             end if
             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
       
          
             ! save Wvec
             f_in = TRIM(scratch_pathname)//"_Wvec_fwdTE0_OPT"//trim(adjustl(optchar))//".nc"
             Fx_txt = "Wx"
             Fy_txt = "Wy"
             Fz_txt = "Wz"
             CALL read_field_R3toR3_ncdf2(Wvec, f_in, Fx_txt, Fy_txt, Fz_txt)
             f_out = TRIM(scratch_pathname)//"_Wvec_fwdTE0_OPT_coarse"//trim(adjustl(optchar))//".nc"
             if (rank ==0 ) then
                ncout = nf90_create(f_out, NF90_CLOBBER, ncid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_dim(ncid, "x", n(1)/ratio, x_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_dim(ncid, "y", n(2)/ratio, y_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_dim(ncid, "z", NF90_UNLIMITED, z_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             dimids =  (/ x_dimid, y_dimid, z_dimid /)
             ncout = nf90_def_var(ncid, TRIM(fx_txt), NF90_DOUBLE, dimids, ux_id)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_var(ncid, TRIM(fy_txt), NF90_DOUBLE, dimids, uy_id)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_var(ncid, TRIM(fz_txt), NF90_DOUBLE, dimids, uz_id)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                     
             ncout = nf90_enddef(ncid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
          end if
          
             
             do nn = 1, 3
                CALL MPI_GATHER(Wvec(:,:,:,nn), total_local_size, MPI_DOUBLE_PRECISION, global_u, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
                if (rank == 0) then
                   aux = 0.0_pr
                   do i3 = 1, n(3)/ratio
                      do i2 = 1, n(2)/ratio
                         do i1 = 1, n(1)/ratio
                            aux(i1,i2,i3) = global_u(i1*ratio-(ratio-1), i2*ratio-(ratio-1), i3*ratio-(ratio-1))
                         end do
                      end do
                   end do
                   select case (nn)
                   case (1)
                      ncout = nf90_put_var(ncid, ux_id, aux)
                      IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   case (2)
                      ncout = nf90_put_var(ncid, uy_id, aux)
                      IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   case (3)
                      ncout = nf90_put_var(ncid, uz_id, aux)
                      IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
                   end select
                   
                end if
             end do
             if (rank == 0) then
                ncout = nf90_close(ncid)
             end if
             CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)

          

             ! save Uob
             f_in = TRIM(scratch_pathname)//"_UOBvec_fwdTE0_OPT"//trim(adjustl(optchar))//".nc"
             Fob_txt = "Uob"
             
             call read_field_R3toR1_ncdf2(Uvec(:,:,:,1), f_in, Fob_txt)
             f_out = TRIM(scratch_pathname)//"_UOBvec_fwdTE0_OPT_coarse"//trim(adjustl(optchar))//".nc"
             if (rank ==0 ) then
                ncout = nf90_create(f_out, NF90_CLOBBER, ncid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_dim(ncid, "x", n(1)/ratio, x_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_dim(ncid, "y", n(2)/ratio, y_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_def_dim(ncid, "z", NF90_UNLIMITED, z_dimid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             dimids =  (/ x_dimid, y_dimid, z_dimid /)
             ncout = nf90_def_var(ncid, TRIM(Fob_txt), NF90_DOUBLE, dimids, ux_id)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             
             ncout = nf90_enddef(ncid)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
          end if
          
             
          CALL MPI_GATHER(Uvec(:,:,:,1), total_local_size, MPI_DOUBLE_PRECISION, global_u, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
          if (rank == 0) then
             aux = 0.0_pr
             do i3 = 1, n(3)/ratio
                do i2 = 1, n(2)/ratio
                   do i1 = 1, n(1)/ratio
                      aux(i1,i2,i3) = global_u(i1*ratio-(ratio-1), i2*ratio-(ratio-1), i3*ratio-(ratio-1))
                   end do
                end do
             end do
             
             ncout = nf90_put_var(ncid, ux_id, aux)
             IF (ncout /= NF90_NOERR) CALL ncdf_error_handle(ncout)
             ncout = nf90_close(ncid)
          end if
       end do
       
          
          if (rank == 0) then
          deallocate(aux)      
          end if
             
END SUBROUTINE coarse
             

                
                

                 
                   
                   
                   
             
          



END MODULE 
