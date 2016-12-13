PROGRAM squeeze_dim
  !--------------------------------------------------------------------------------------
  !                            *** PROGRAM squeeze_dim  ***
  !
  !        ** Purpose: squeeze a vertical dimension to variable
  !
  !   History:
  !---------------------------------------------------------------------------------------
  USE netcdf
  IMPLICIT NONE

  INTEGER :: narg, iargc, jarg, ncol
  INTEGER :: n_bef, n_aft
  INTEGER :: npx, npy, npt, npk, jt, npz, jk
  CHARACTER(LEN=80) :: cfile, varname, cfilout, cdep, cdim,varzname1, varzname2
  CHARACTER(LEN=1)  :: cobc, cvar
  REAL(KIND=4), DIMENSION (:), ALLOCATABLE :: dep
  REAL(KIND=4), DIMENSION (:), ALLOCATABLE :: deprev
  REAL(KIND=4), DIMENSION (:), ALLOCATABLE :: time
  REAL(KIND=4), DIMENSION (:), ALLOCATABLE :: lonred
  REAL(KIND=4), DIMENSION (:,:), ALLOCATABLE :: lon
  REAL(KIND=4), DIMENSION (:,:,:), ALLOCATABLE :: varred
  REAL(KIND=4), DIMENSION (:,:,:), ALLOCATABLE :: varredrev
  REAL(KIND=4), DIMENSION (:,:,:,:), ALLOCATABLE :: var

  ! Netcdf Stuff
  INTEGER :: istatus, ncid, id_t, id_x, id_y, id_z, idvar, idlonout, idtimout, id_dep, id_depout
  INTEGER :: ncidout, ids, idt, idep, idtime, idlon, id_xout, id_zout, id_tout, idvarout
  ! * 
  narg=iargc()
  IF ( narg == 0 ) THEN
     PRINT *,' USAGE : squeeze_dim file var '
     STOP
  ENDIF

  CALL getarg (1, cfile)
  CALL getarg( 2, varname)

  istatus=NF90_OPEN(cfile,NF90_WRITE,ncid); PRINT *,'Open file :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncid,'time_counter',id_t); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_t,len=npt)
  istatus=NF90_INQ_DIMID(ncid,'x',id_x); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_x,len=npx)
  istatus=NF90_INQ_DIMID(ncid,'y',id_y); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_y,len=npy)
  istatus=NF90_INQ_DIMID(ncid,'depthw',id_z); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_z,len=npz)

  ALLOCATE( var(npx,npy,npz,npt), varred(npx,npz,npt), varredrev(npx,npz,npt) )
  ALLOCATE( lon(npx,npy), lonred(npx), time(npt), dep(npz), deprev(npz))

  istatus=NF90_INQ_VARID(ncid,varname,idvar); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncid,idvar,var); PRINT *,'Get var :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_VARID(ncid,'nav_lon',idlon); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncid,idlon,lon); PRINT *,'Get var :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_VARID(ncid,'time_counter',idtime); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncid,idtime,time); PRINT *,'Get var :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_VARID(ncid,'depthw',id_dep); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncid,id_dep,dep); PRINT *,'Get var :',NF90_STRERROR(istatus)

  istatus=NF90_CLOSE(ncid)

  varred(:,:,:)=var(:,1,:,:)
  lonred(:)=lon(:,1)

  DO jk = 1, npz
    PRINT *,'jk =',jk
    PRINT *,'npz-jk+1 =',npz-jk+1
    deprev(jk) = -1. * dep(npz-jk+1.)
    varredrev(:,jk,:) = varred(:,npz-jk+1,:)
 
  END DO

  cfilout=TRIM(cfile)//'_redrev'

  istatus = NF90_CREATE(cfilout,NF90_CLOBBER,ncidout)

  istatus=NF90_DEF_DIM(ncidout,'time_counter',NF90_UNLIMITED,id_tout); ; PRINT *,'Def dim :',NF90_STRERROR(istatus)
  istatus=NF90_DEF_DIM(ncidout,'depthw',npz,id_zout); ; PRINT *,'Def dim :',NF90_STRERROR(istatus)
  istatus=NF90_DEF_DIM(ncidout,'x',npx,id_xout); ; PRINT *,'Def dim :',NF90_STRERROR(istatus)


  istatus=NF90_DEF_VAR(ncidout,varname,NF90_FLOAT,(/id_xout,id_zout,id_tout/), idvarout ); PRINT *,'Def var :',NF90_STRERROR(istatus)
  istatus=NF90_DEF_VAR(ncidout,'lon',NF90_FLOAT,(/id_xout/), idlonout ); PRINT *,'Def var :',NF90_STRERROR(istatus)
  istatus=NF90_DEF_VAR(ncidout,'time_counter',NF90_FLOAT,(/id_tout/), idtimout ); PRINT *,'Def var :',NF90_STRERROR(istatus)
  istatus=NF90_DEF_VAR(ncidout,'depthw',NF90_FLOAT,(/id_zout/), id_depout ); PRINT *,'Def var :',NF90_STRERROR(istatus)

  istatus=NF90_ENDDEF(ncidout)

  istatus=NF90_PUT_VAR(ncidout,idvarout,varredrev); PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncidout,idlonout,lonred); PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncidout,idtimout,time); PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncidout,id_depout,deprev); PRINT *,'Put var :',NF90_STRERROR(istatus)

  istatus=NF90_CLOSE(ncid)

  DEALLOCATE(var,varred, varredrev,dep,deprev,lon,lonred)

END PROGRAM squeeze_dim

