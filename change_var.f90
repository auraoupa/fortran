PROGRAM change_var
  !--------------------------------------------------------------------------------------
  !                            *** PROGRAM change_var  ***
  !
  !        ** Purpose: add a vertical dimension to variable
  !
  !   History:
  !---------------------------------------------------------------------------------------
  USE netcdf
  IMPLICIT NONE

  INTEGER :: narg, iargc, jarg, ncol
  INTEGER :: n_bef, n_aft
  INTEGER :: npx, npy, npt, npk, jt, npz
  CHARACTER(LEN=80) :: cfile1, cfile2, varname1, varname2, cfilout, cdep, cdim,varzname1, varzname2
  CHARACTER(LEN=1)  :: cobc, cvar
  REAL(KIND=4), DIMENSION (:), ALLOCATABLE :: rdep, rtime
  REAL(KIND=4), DIMENSION (:,:), ALLOCATABLE :: rdum
  REAL(KIND=4), DIMENSION (:,:,:), ALLOCATABLE :: var1
  REAL(KIND=4), DIMENSION (:,:,:,:), ALLOCATABLE :: var2

  ! Netcdf Stuff
  INTEGER :: istatus, ncid, ncid2, id_time, id_x, id_y, id_z, idvar1, idvar2, id_t,idvarz1, idvarz2
  INTEGER :: ncout, ids, idt, idep,idtim, ji, jj
  ! * 
  narg=iargc()
  IF ( narg == 0 ) THEN
     PRINT *,' USAGE : change_var file1 var1 file2 var2 '
     STOP
  ENDIF

  CALL getarg (1, cfile1)
  CALL getarg (2, cfile2)
  CALL getarg( 3, varname1)
  CALL getarg( 4, varname2)

  istatus=NF90_OPEN(cfile1,NF90_WRITE,ncid); PRINT *,'Open file :',NF90_STRERROR(istatus)
  istatus=NF90_OPEN(cfile2,NF90_WRITE,ncid2); PRINT *,'Open file :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncid,'time_counter',id_t); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_t,len=npt)
  istatus=NF90_INQ_DIMID(ncid,'x',id_x); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_x,len=npx)
  istatus=NF90_INQ_DIMID(ncid,'y',id_y); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_y,len=npy)

  istatus=NF90_INQ_DIMID(ncid2,'z',id_z); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid2,id_z,len=npz)
 
  ALLOCATE( var1(npx,npy,npt), var2(npx,npy,npz,npt) )

  istatus=NF90_INQ_VARID(ncid,varname1,idvar1); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncid,idvar1,var1); PRINT *,'Get var :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_VARID(ncid2,varname2,idvar2); PRINT *,'Inquire varid:',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncid2,idvar2,var2); PRINT *,'Get var :',NF90_STRERROR(istatus) 


  var1(:,:,:)=var2(:,:,1,:)

  DO ji = 1, npx
    DO jj = 1, npy
      DO jt = 1, npt

        IF ( var1(ji,jj,jt) < -1000. )  var1(ji,jj,jt) = 0.

      END DO
    END DO
  END DO




  istatus=NF90_PUT_VAR(ncid,idvar1,var1); PRINT *,'Put var :',NF90_STRERROR(istatus)

  istatus=NF90_CLOSE(ncid)

  DEALLOCATE(var1,var2)

END PROGRAM change_var

