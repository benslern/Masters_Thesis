MODULE databinary_handle   ! Newly added on March 20, 2017
   IMPLICIT NONE  

   CONTAINS

!=======================================
! save2binary
!=======================================
   SUBROUTINE save2binary( myfield,myindex,mysystem,subpath)
      USE global_variables
      IMPLICIT NONE 
      INCLUDE "mpif.h"

      REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(IN) :: myfield
      INTEGER, INTENT(IN) :: myindex
      CHARACTER(len=*), INTENT(IN) :: mysystem
      character(len=*), intent(in) :: subpath

      CHARACTER(2) :: E0txt
      CHARACTER(5) :: indexchar
      CHARACTER(200) :: filename
      integer     :: ios

      REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: global_Ux, global_Uy, global_Uz

      WRITE(E0txt, '(i2.2)') E0_index
      WRITE(indexchar, '(i5)') myindex
      SELECT CASE (mysystem)
         case("fwdTE")
           
            filename = TRIM(scratch_pathname)//TRIM(subpath)//"_Uvec_fwdTE"//trim(adjustl(indexchar))
         case("bwdADJ")
           
            filename = TRIM(scratch_pathname)//TRIM(subpath)//"_Uvec_bwdADJ"//trim(adjustl(indexchar))
      end select

      IF (rank == 0) THEN   ! Note that we use: if rank == 0, be followed that MPI_GATHER gives data to global_field in rank = 0
         ALLOCATE( global_Ux(1:n(1),1:n(2),1:n(3)) )
         ALLOCATE( global_Uy(1:n(1),1:n(2),1:n(3)) )
         ALLOCATE( global_Uz(1:n(1),1:n(2),1:n(3)) )
      END IF



      CALL MPI_GATHER(myfield(:,:,:,1), total_local_size, MPI_DOUBLE_PRECISION, global_Ux, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
      CALL MPI_GATHER(myfield(:,:,:,2), total_local_size, MPI_DOUBLE_PRECISION, global_Uy, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
      CALL MPI_GATHER(myfield(:,:,:,3), total_local_size, MPI_DOUBLE_PRECISION, global_Uz, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
      CALL MPI_BARRIER(MPI_COMM_WORLD,Statinfo)

      if (rank ==0) then   ! As MPI_Gather collect data to global_field in rank = 0, global_field in other ranks don't have data, so should save global_field in rank = 0.
         inquire(iolength=reclen) global_Ux
         ! print *, "Ux reclen =", reclen
         OPEN(unit=20, FILE = filename, FORM = 'unformatted', ACCESS= 'DIRECT', STATUS = 'replace', ACTION= 'WRITE',IOSTAT=ios, RECL=reclen)
         IF (ios .NE. 0) THEN
            PRINT *, "Error happens to opening file and writing data ... ... "
         END IF
         write(20,rec=1) global_Ux

         inquire(iolength=reclen) global_Uy
         ! print *, "Uy reclen =", reclen
         write(20,rec=2) global_Uy

         inquire(iolength=reclen) global_Uz
         ! print *, "Uz reclen =", reclen
         write(20,rec=3) global_Uz
         close(20)
         DEALLOCATE( global_Ux )
         DEALLOCATE( global_Uy )
         DEALLOCATE( global_Uz )
      end if


      CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)

   END SUBROUTINE save2binary




!=======================================
! read4binary
!=======================================
   SUBROUTINE read4binary( myindex, myfield, mysystem, subpath)
      USE global_variables
      IMPLICIT NONE 
      INCLUDE "mpif.h"

      INTEGER, INTENT(IN) :: myindex
      REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(OUT) :: myfield
      CHARACTER(len=*), INTENT(IN) :: mysystem
      character(len=*), intent(in) :: subpath
      CHARACTER(2)   :: E0txt
      CHARACTER(5)   :: indexchar
      CHARACTER(200) :: filename
      integer        :: ios

      REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: local_fx, local_fy, local_fz
      REAL(pr), DIMENSION(:,:,:), ALLOCATABLE :: global_Ux, global_Uy, global_Uz

      ALLOCATE( local_fx(1:n(1),1:n(2),1:local_N))
      ALLOCATE( local_fy(1:n(1),1:n(2),1:local_N))
      ALLOCATE( local_fz(1:n(1),1:n(2),1:local_N))
      IF (rank == 0) THEN   ! Note that we use: if rank == 0, be followed that MPI_GATHER gives data to global_field in rank = 0
         ALLOCATE( global_Ux(1:n(1),1:n(2),1:n(3)) )
         ALLOCATE( global_Uy(1:n(1),1:n(2),1:n(3)) )
         ALLOCATE( global_Uz(1:n(1),1:n(2),1:n(3)) )
      END IF

      WRITE(E0txt, '(i2.2)') E0_index
      WRITE(indexchar, '(i5)') myindex
      SELECT CASE (mysystem)
         case("fwdTE")
            !filename = TRIM(work_pathname)//"_E"//E0txt//"_Uvec_fwdTE"//trim(adjustl(indexchar))
            filename = TRIM(scratch_pathname)//TRIM(subpath)//"_Uvec_fwdTE"//trim(adjustl(indexchar))
      END SELECT
      if ( rank == 0 ) then
         open(unit=200,FILE = filename, FORM = 'UNFORMATTED', ACTION='READ',ACCESS= 'DIRECT',recl=reclen)
         read(200,rec=1) global_Ux
         read(200,rec=2) global_Uy
         read(200,rec=3) global_Uz
         close(200)
      end if
      CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
      call MPI_Scatter(global_Ux, total_local_size, MPI_DOUBLE_PRECISION, local_fx, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
      call MPI_Scatter(global_Uy, total_local_size, MPI_DOUBLE_PRECISION, local_fy, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
      call MPI_Scatter(global_Uz, total_local_size, MPI_DOUBLE_PRECISION, local_fz, total_local_size, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, Statinfo)
      if (rank ==0) then
         DEALLOCATE( global_Ux )
         DEALLOCATE( global_Uy )
         DEALLOCATE( global_Uz )
      end if
      CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
      myfield(:,:,:,1) = local_fx
      myfield(:,:,:,2) = local_fy
      myfield(:,:,:,3) = local_fz
      DEALLOCATE( local_fx )
      DEALLOCATE( local_fy )
      DEALLOCATE( local_fz )
      CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)

   END SUBROUTINE read4binary

!=======================================
! save2binary
!=======================================
   SUBROUTINE save2binary_mpi( myfield,myindex,mysystem,subpath )
      USE global_variables
      IMPLICIT NONE 
      INCLUDE "mpif.h"

      REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(IN) :: myfield
      INTEGER, INTENT(IN) :: myindex
      CHARACTER(len=*), INTENT(IN) :: mysystem
      character(len=*), intent(in) :: subpath

      CHARACTER(2) :: E0txt
      CHARACTER(5) :: indexchar
      CHARACTER(200) :: filename
      integer :: file_handle
      integer(kind=mpi_offset_kind) :: my_offset
      integer :: local_count
      character(512) :: msg
      integer :: resultlen
      integer :: eclass
      integer :: error

      
      

      

      WRITE(E0txt, '(i2.2)') E0_index
      WRITE(indexchar, '(i5)') myindex
      SELECT CASE (mysystem)
         case("fwdTE")
           
            filename = TRIM(scratch_pathname)//TRIM(subpath)//"_Uvec_fwdTE"//trim(adjustl(indexchar))
         case("bwdADJ")
           
            filename = TRIM(scratch_pathname)//TRIM(subpath)//"_Uvec_bwdADJ"//trim(adjustl(indexchar))
      end select

      
     
      my_offset = 3*n(1)*n(2)*int(local_k_offset, mpi_offset_kind)*pr
      
      
      local_count = 3*total_local_size

      if (rank == 0) print *, filename
      CALL MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_WRONLY + MPI_MODE_CREATE, MPI_INFO_NULL, file_handle, error)
      call MPI_Error_class(error, eclass, Statinfo)
      call mpi_error_string(error, msg, resultlen, Statinfo)
      print *, "save2binary:mpi_file_open: ", rank, msg
      
      CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
     
      !CALL MPI_File_seek(file_handle, my_offset, MPI_SEEK_SET, Statinfo)
      call MPI_File_set_view(file_handle, my_offset,MPI_DOUBLE_PRECISION, MPI_DOUBLE_PRECISION, "native", MPI_INFO_NULL, error);
      call MPI_Error_class(error, eclass, Statinfo)
      call mpi_error_string(error, msg, resultlen, Statinfo)
      print *, "save2binary:mpi_file_set_view: ", rank, msg
      CALL MPI_File_write_all(file_handle, myfield, local_count, MPI_DOUBLE_PRECISION, MPI_STATUS_IGNORE, error)
      call MPI_Error_class(error, eclass, Statinfo)
      call mpi_error_string(error, msg, resultlen, Statinfo)
      print *, "save2binary:mpi_file_write_all: ", rank, msg
      CALL MPI_File_close(file_handle, error)
      call MPI_Error_class(error, eclass, Statinfo)
      call mpi_error_string(error, msg, resultlen, Statinfo)
      print *, "save2binary:mpi_file_close: ", rank, msg


      CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
     

   END SUBROUTINE save2binary_mpi




!=======================================
! read4binary
!=======================================
   SUBROUTINE read4binary_mpi( myindex, myfield, mysystem, subpath )
      USE global_variables
      IMPLICIT NONE 
      INCLUDE "mpif.h"

      REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(OUT) :: myfield
      INTEGER, INTENT(IN) :: myindex
      CHARACTER(len=*), INTENT(IN) :: mysystem
      character(len=*), intent(in) :: subpath

      CHARACTER(2) :: E0txt
      CHARACTER(5) :: indexchar
      CHARACTER(200) :: filename
      integer :: file_handle
      integer(kind=mpi_offset_kind) :: my_offset
      integer :: local_count
      character(512) :: msg
      integer :: resultlen
      integer :: eclass
      integer :: error
      
      

      

      WRITE(E0txt, '(i2.2)') E0_index
      WRITE(indexchar, '(i5)') myindex
      SELECT CASE (mysystem)
         case("fwdTE")
           
            filename = TRIM(scratch_pathname)//TRIM(subpath)//"_Uvec_fwdTE"//trim(adjustl(indexchar))
         case("bwdADJ")
           
            filename = TRIM(scratch_pathname)//TRIM(subpath)//"_Uvec_bwdADJ"//trim(adjustl(indexchar))
      end select

     
      
      my_offset = 3*n(1)*n(2)*int(local_k_offset, mpi_offset_kind)*pr
      local_count = 3*total_local_size

      if (rank == 0) print *, filename
      
      CALL MPI_File_open(MPI_COMM_WORLD, filename, MPI_MODE_RDONLY, MPI_INFO_NULL, file_handle, error)
      call MPI_Error_class(error, eclass, Statinfo)
      call mpi_error_string(error, msg, resultlen, Statinfo)
      print *, "read4binary:mpi_file_open: ", rank, msg
      CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
      
      !CALL MPI_File_seek(file_handle, my_offset, MPI_SEEK_SET, Statinfo)
      call MPI_File_set_view(file_handle, my_offset, MPI_DOUBLE_PRECISION, MPI_DOUBLE_PRECISION, "native", MPI_INFO_NULL, error)
      call MPI_Error_class(error, eclass, Statinfo)
      call mpi_error_string(error, msg, resultlen, Statinfo)
      print *, "read4binary:mpi_file_set_view: ", rank, msg
      CALL MPI_File_read_all(file_handle, myfield, local_count, MPI_DOUBLE_PRECISION, MPI_STATUS_IGNORE, error)
      call MPI_Error_class(error, eclass, Statinfo)
      call mpi_error_string(error, msg, resultlen, Statinfo)
      print *, "read4binary:mpi_file_read_all: ", rank, msg
      CALL MPI_File_close(file_handle, error)
      call MPI_Error_class(error, eclass, Statinfo)
      call mpi_error_string(error, msg, resultlen, Statinfo)
      print *, "read4binary:mpi_file_close: ", rank, msg



      CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)
   END SUBROUTINE read4binary_mpi

!=======================================
! save2binary2
!=======================================
   SUBROUTINE save2binary2( myfield,myindex,mysystem,subpath )
      USE global_variables
      IMPLICIT NONE 
      INCLUDE "mpif.h"

      REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(IN) :: myfield
      INTEGER, INTENT(IN) :: myindex
      CHARACTER(len=*), INTENT(IN) :: mysystem
      CHARACTER(len=*), INTENT(IN) :: subpath

      CHARACTER(2) :: E0txt
      CHARACTER(5) :: indexchar
      CHARACTER(200) :: filename
      integer :: ios
      integer :: i, nn
      !Real(pr), dimension(:,:,:), allocatable :: global_u
      WRITE(E0txt, '(i2.2)') E0_index
      WRITE(indexchar, '(i5)') myindex
      SELECT CASE (mysystem)
         case("fwdTE")
           
            filename = TRIM(scratch_pathname)//TRIM(subpath)//"_Uvec_fwdTE"//trim(adjustl(indexchar))
         case("bwdADJ")
           
            filename = TRIM(scratch_pathname)//TRIM(subpath)//"_Uvec_bwdADJ"//trim(adjustl(indexchar))
      end select

      IF (rank == 0) THEN   
         !allocate(global_u(1:n(1), 1:n(2), 1:n(3)))
         inquire(iolength=reclen) global_u(:,:,1:local_N)
         OPEN(unit=20, FILE = filename, FORM = 'unformatted', ACCESS= 'DIRECT', STATUS = 'replace', ACTION= 'WRITE',IOSTAT=ios, RECL=reclen)
          IF (ios .NE. 0) THEN
             PRINT *, "save2binary_mpi2:Error happens to opening file and writing data ... ... "
          END IF
      END IF
      call mpi_barrier(mpi_comm_world, statinfo)
      do nn = 1, 3
         call mpi_gather(myfield(:,:,:,nn), total_local_size, mpi_double_precision, global_u, total_local_size, mpi_double_precision, 0, mpi_comm_world, statinfo)
         if (rank == 0) then            
            do i = 0, np-1
               write(20,rec=i+1+(nn-1)*np) global_u(:,:,i*local_N+1:(i+1)*local_N)
            end do
         end if
         call MPI_BARRIER(MPI_COMM_WORLD,Statinfo)
      end do
      if(rank == 0) then
         close(20)
         !deallocate(global_u)
      end if

      CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)

    END SUBROUTINE save2binary2

!=======================================
! read4binary2
!=======================================
   SUBROUTINE read4binary2( myindex, myfield, mysystem, subpath )
      USE global_variables
      IMPLICIT NONE 
      INCLUDE "mpif.h"

      REAL(pr), DIMENSION(1:n(1),1:n(2),1:local_N,1:3), INTENT(OUT) :: myfield
      INTEGER, INTENT(IN) :: myindex
      CHARACTER(len=*), INTENT(IN) :: mysystem
      character(len=*), intent(in) :: subpath

      CHARACTER(2) :: E0txt
      CHARACTER(5) :: indexchar
      CHARACTER(200) :: filename
      integer :: ios
      integer :: i, nn
      !Real(pr), dimension(:,:,:), allocatable :: global_u
      WRITE(E0txt, '(i2.2)') E0_index
      WRITE(indexchar, '(i5)') myindex
      SELECT CASE (mysystem)
         case("fwdTE")
           
            filename = TRIM(scratch_pathname)//TRIM(subpath)//"_Uvec_fwdTE"//trim(adjustl(indexchar))
         case("bwdADJ")
           
            filename = TRIM(scratch_pathname)//TRIM(subpath)//"_Uvec_bwdADJ"//trim(adjustl(indexchar))
      end select

      IF (rank == 0) THEN   
         !allocate(global_u(1:n(1), 1:n(2), 1:n(3)))
         global_u = 0.0_pr
         inquire(iolength=reclen) global_u(:,:,1:local_N)
         OPEN(unit=200, FILE = filename, FORM = 'unformatted', ACCESS= 'DIRECT', ACTION= 'READ',IOSTAT=ios, RECL=reclen)
         IF (ios .NE. 0) THEN
            PRINT *, "read2binary_mpi2:Error happens to opening file and writing data ... ... "
         END IF
      end if
       
      call mpi_barrier(mpi_comm_world, statinfo)
      do nn = 1, 3          
         if (rank == 0) then           
            do i = 0, np-1
               read(200,rec=i+1+(nn-1)*np) global_u(:,:,i*local_N+1:(i+1)*local_N)
            end do
         end if
         call MPI_BARRIER(MPI_COMM_WORLD,Statinfo)
         call mpi_scatter(global_u, total_local_size, mpi_double_precision, myfield(:,:,:,nn), total_local_size, mpi_double_precision, 0, mpi_comm_world, statinfo)
      end do

      if(rank == 0) then
         close(200)
         !deallocate(global_u)
      end if

      CALL MPI_BARRIER(MPI_COMM_WORLD, Statinfo)

    END SUBROUTINE read4binary2




   
END MODULE

