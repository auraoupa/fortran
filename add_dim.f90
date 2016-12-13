PROGRAM add_dim
  !--------------------------------------------------------------------------------------
  !                            *** PROGRAM add_dim  ***
  !
  !        ** Purpose: add a vertical dimension to variable
  !
  !   History:
  !---------------------------------------------------------------------------------------
  USE netcdf
  IMPLICIT NONE

  INTEGER :: narg, iargc, jarg, ncol
  INTEGER :: n_bef, n_aft
  INTEGER :: npx, npy, npt, npk, jt
  CHARACTER(LEN=80) :: cfile, varname1, varname2, cfilout, cdep, cdim,varzname1, varzname2
  CHARACTER(LEN=1)  :: cobc, cvar
  REAL(KIND=4), DIMENSION (:), ALLOCATABLE :: rdep, rtime
  REAL(KIND=4), DIMENSION (:,:), ALLOCATABLE :: rdum
  REAL(KIND=4), DIMENSION (:,:,:), ALLOCATABLE :: var1, var2
  REAL(KIND=4), DIMENSION (:,:,:,:), ALLOCATABLE :: varz1, varz2

  ! Netcdf Stuff
  INTEGER :: istatus, ncid, id_time, id_x, id_y, id_z, idvar1, idvar2, id_t,idvarz1, idvarz2
  INTEGER :: ncout, ids, idt, idep,idtim
  ! * 
  narg=iargc()
  IF ( narg == 0 ) THEN
     PRINT *,' USAGE : add_dim file var1 var2 '
     STOP
  ENDIF

  CALL getarg (1, cfile)
  CALL getarg( 2, varname1)
  CALL getarg( 3, varname2)

  istatus=NF90_OPEN(cfile,NF90_WRITE,ncid); PRINT *,'Open file :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncid,'time_counter',id_t); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_t,len=npt)
  istatus=NF90_INQ_DIMID(ncid,'x',id_x); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_x,len=npx)
  istatus=NF90_INQ_DIMID(ncid,'y',id_y); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_y,len=npy)

  ALLOCATE( var1(npx,npy,npt), var2(npx,npy,npt) )

  istatus=NF90_INQ_VARID(ncid,varname1,idvar1); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncid,idvar1,var1); PRINT *,'Get var :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_VARID(ncid,varname2,idvar2); PRINT *,'Inquire varid:',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncid,idvar2,var2); PRINT *,'Get var :',NF90_STRERROR(istatus) 

  ALLOCATE( varz1(npx,npy,1,npt), varz2(npx,npy,1,npt) )

  varz1(:,:,1,:)=var1(:,:,:)
  varz2(:,:,1,:)=var2(:,:,:)

  istatus=NF90_REDEF(ncid)

  istatus=NF90_DEF_DIM(ncid,'deptht',1,id_z); ; PRINT *,'Def dim :',NF90_STRERROR(istatus)

  varzname1=TRIM(varname1)//'_z'
  varzname2=TRIM(varname2)//'_z'

  istatus=NF90_DEF_VAR(ncid,varzname1,NF90_FLOAT,(/id_x,id_y,id_z,id_t/), idvarz1 ); PRINT *,'Def var :',NF90_STRERROR(istatus)
  istatus=NF90_DEF_VAR(ncid,varzname2,NF90_FLOAT,(/id_x,id_y,id_z,id_t/), idvarz2 ); PRINT *,'Def var :',NF90_STRERROR(istatus)

  istatus=NF90_ENDDEF(ncid)

  istatus=NF90_PUT_VAR(ncid,idvarz1,varz1); PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncid,idvarz2,varz2); PRINT *,'Put var :',NF90_STRERROR(istatus)

  istatus=NF90_CLOSE(ncid)

  DEALLOCATE(varz1,varz2,var1,var2)

END PROGRAM add_dim

