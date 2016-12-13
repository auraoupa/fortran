PROGRAM nwd
  !--------------------------------------------------------------------------------------
  !                            *** PROGRAM new interp GPCP  ***
  !
  !        ** Purpose: interpolate 1deg GPCP data to a 0.5x0.5deg grid
  !        by copying the data instead of interpolate 
  !
  !---------------------------------------------------------------------------------------

  USE netcdf
  IMPLICIT NONE
  INTEGER :: narg, iargc, jarg, npt, jt
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:,:,:) :: precipin
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:,:,:) :: precipout0
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:,:,:) :: precipout001
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:,:,:) :: precipout01
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:,:,:) :: precipout05
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:,:,:) :: precipout1
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:,:,:) :: intens0
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:,:,:) :: intens001
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:,:,:) :: intens01
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:,:,:) :: intens05
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:,:,:) :: intens1

  CHARACTER(LEN=256) :: cfilein,cfileout0,cfileout001,cfileout01,cfileout05,cfileout1, cdum, ctag='none'

  INTEGER :: istatus, ncidin, ncidout0,ncidout001,ncidout01,ncidout05,ncidout1, id_xin, id_yin, id_xout, id_yout, npxin, id_tin, nptin
  INTEGER :: npyin, npxout, npyout, id_varlatin, id_varlonin, id_varlatout
  INTEGER :: id_varlonout, id_varprecipin,id_varprecipout0,id_varprecipout001,id_varprecipout01,id_varprecipout05,id_varprecipout1
  INTEGER :: id_varintens0,id_varintens001,id_varintens01,id_varintens05,id_varintens1
  INTEGER :: ji, jj, jii, jjj
  ! * 
  narg=iargc()
  IF ( narg == 0 ) THEN
     PRINT *,' USAGE : nwd -f filein -o fileout'
     PRINT *,'         '
     STOP
  ENDIF

  jarg=1
  DO WHILE ( jarg <= narg )
     CALL getarg(jarg, cdum) ; jarg=jarg + 1
     SELECT CASE (cdum)
     CASE ( '-f' )
        CALL getarg(jarg,cfilein) ;jarg=jarg +1
     CASE ( '-o' )
        CALL getarg(jarg,cfileout0) ;jarg=jarg +1
     CASE ( '-p' )
        CALL getarg(jarg,cfileout001) ;jarg=jarg +1
     CASE ( '-q' )
        CALL getarg(jarg,cfileout01) ;jarg=jarg +1
     CASE ( '-r' )
        CALL getarg(jarg,cfileout05) ;jarg=jarg +1
     CASE ( '-s' )
        CALL getarg(jarg,cfileout1) ;jarg=jarg +1
     CASE DEFAULT
         PRINT *,' Option ',TRIM(cdum),' unknown'
         STOP
     END SELECT
  END DO

  istatus=NF90_OPEN(cfilein,NF90_WRITE,ncidin)  ; PRINT *,'Open file :',NF90_STRERROR(istatus)
  istatus=NF90_OPEN(cfileout0,NF90_WRITE,ncidout0)  ; PRINT *,'Open file :',NF90_STRERROR(istatus)
  istatus=NF90_OPEN(cfileout001,NF90_WRITE,ncidout001)  ; PRINT *,'Open file :',NF90_STRERROR(istatus)
  istatus=NF90_OPEN(cfileout01,NF90_WRITE,ncidout01)  ; PRINT *,'Open file :',NF90_STRERROR(istatus)
  istatus=NF90_OPEN(cfileout05,NF90_WRITE,ncidout05)  ; PRINT *,'Open file :',NF90_STRERROR(istatus)
  istatus=NF90_OPEN(cfileout1,NF90_WRITE,ncidout1)  ; PRINT *,'Open file :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncidin,'lon',id_xin)  ; PRINT *,'Inq dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncidin,id_xin,len=npxin)  ; PRINT *,'Inquire dimension :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncidin,'lat',id_yin)  ; PRINT *,'Inq dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncidin,id_yin,len=npyin)  ; PRINT *,'Inquire dimension :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncidin,'time',id_tin)  ; PRINT *,'Inq dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncidin,id_tin,len=nptin)  ; PRINT *,'Inquire dimension :',NF90_STRERROR(istatus)


  ALLOCATE( precipin(npxin,npyin,nptin) )
  ALLOCATE( precipout0(npxin,npyin,1) )
  ALLOCATE( precipout001(npxin,npyin,1) )
  ALLOCATE( precipout01(npxin,npyin,1) )
  ALLOCATE( precipout05(npxin,npyin,1) )
  ALLOCATE( precipout1(npxin,npyin,1) )
  ALLOCATE( intens0(npxin,npyin,1) )
  ALLOCATE( intens001(npxin,npyin,1) )
  ALLOCATE( intens01(npxin,npyin,1) )
  ALLOCATE( intens05(npxin,npyin,1) )
  ALLOCATE( intens1(npxin,npyin,1) )


  istatus=NF90_INQ_VARID(ncidin,'precip',id_varprecipin)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncidin,id_varprecipin,precipin)  ; PRINT *,'Get var :',NF90_STRERROR(istatus)



  precipout0(:,:,1)=0.
  precipout001(:,:,1)=0.
  precipout01(:,:,1)=0.
  precipout05(:,:,1)=0.
  precipout1(:,:,1)=0.
  intens0(:,:,1)=0.
  intens001(:,:,1)=0.
  intens01(:,:,1)=0.
  intens05(:,:,1)=0.
  intens1(:,:,1)=0.


  DO ji = 1, npxin
    DO jj = 1, npyin

      DO jt = 1, nptin

        IF ( precipin(ji,jj,jt) > 0. ) THEN
          precipout0(ji,jj,1) = precipout0(ji,jj,1) + 1.
        ENDIF
        IF ( precipin(ji,jj,jt) > 0.01 ) THEN
          precipout001(ji,jj,1) = precipout001(ji,jj,1) + 1.
        ENDIF
        IF ( precipin(ji,jj,jt) > 0.1 ) THEN
          precipout01(ji,jj,1) = precipout01(ji,jj,1) + 1.
        ENDIF
        IF ( precipin(ji,jj,jt) > 0.5 ) THEN
          precipout05(ji,jj,1) = precipout05(ji,jj,1) + 1.
        ENDIF
        IF ( precipin(ji,jj,jt) > 1. ) THEN
          precipout1(ji,jj,1) = precipout1(ji,jj,1) + 1.
        ENDIF

      END DO

      IF ( precipout0(ji,jj,1) > 0. ) THEN
        intens0(ji,jj,1)  = SUM(precipin(ji,jj,:),mask=precipin(ji,jj,:)>-1.) / precipout0(ji,jj,1)
      ELSE
        intens0(ji,jj,1)  = 0.
      ENDIF

      IF ( precipout001(ji,jj,1) > 0. ) THEN
        intens001(ji,jj,1)= SUM(precipin(ji,jj,:),mask=precipin(ji,jj,:)>-1.) / precipout001(ji,jj,1)
      ELSE
        intens001(ji,jj,1)  = 0.
      ENDIF

      IF ( precipout01(ji,jj,1) > 0. ) THEN
        intens01(ji,jj,1) = SUM(precipin(ji,jj,:),mask=precipin(ji,jj,:)>-1.) / precipout01(ji,jj,1)
      ELSE
        intens01(ji,jj,1)  = 0.
      ENDIF

      IF ( precipout05(ji,jj,1) > 0. ) THEN
        intens05(ji,jj,1) = SUM(precipin(ji,jj,:),mask=precipin(ji,jj,:)>-1.) / precipout05(ji,jj,1)
      ELSE
        intens05(ji,jj,1)  = 0.
      ENDIF

      IF ( precipout1(ji,jj,1) > 0. ) THEN
        intens1(ji,jj,1)  = SUM(precipin(ji,jj,:),mask=precipin(ji,jj,:)>-1.) / precipout1(ji,jj,1)
      ELSE
        intens1(ji,jj,1)  = 0.
      ENDIF


      precipout0(ji,jj,1)   = 100. * precipout0(ji,jj,1)   / 4748.
      precipout001(ji,jj,1) = 100. * precipout001(ji,jj,1) / 4748.
      precipout01(ji,jj,1)  = 100. * precipout01(ji,jj,1)  / 4748.
      precipout05(ji,jj,1)  = 100. * precipout05(ji,jj,1)  / 4748.
      precipout1(ji,jj,1)   = 100. * precipout1(ji,jj,1)   / 4748.


    END DO
  END DO

  istatus=NF90_INQ_VARID(ncidout0,'precip',id_varprecipout0)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_VARID(ncidout001,'precip',id_varprecipout001)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_VARID(ncidout01,'precip',id_varprecipout01)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_VARID(ncidout05,'precip',id_varprecipout05)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_VARID(ncidout1,'precip',id_varprecipout1)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_VARID(ncidout0,'intens',id_varintens0)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_VARID(ncidout001,'intens',id_varintens001)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_VARID(ncidout01,'intens',id_varintens01)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_VARID(ncidout05,'intens',id_varintens05)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_VARID(ncidout1,'intens',id_varintens1)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)

  istatus=NF90_PUT_VAR(ncidout0,id_varprecipout0,precipout0)  ; PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncidout001,id_varprecipout001,precipout001)  ; PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncidout01,id_varprecipout01,precipout01)  ; PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncidout05,id_varprecipout05,precipout05)  ; PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncidout1,id_varprecipout1,precipout1)  ; PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncidout0,id_varintens0,intens0)  ; PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncidout001,id_varintens001,intens001)  ; PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncidout01,id_varintens01,intens01)  ; PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncidout05,id_varintens05,intens05)  ; PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncidout1,id_varintens1,intens1)  ; PRINT *,'Put var :',NF90_STRERROR(istatus)



  DEALLOCATE( precipin )
  DEALLOCATE( precipout0 )
  DEALLOCATE( precipout001 )
  DEALLOCATE( precipout01 )
  DEALLOCATE( precipout05 )
  DEALLOCATE( precipout1 )
  DEALLOCATE( intens0 )
  DEALLOCATE( intens001 )
  DEALLOCATE( intens01 )
  DEALLOCATE( intens05 )
  DEALLOCATE( intens1 )

  istatus=NF90_CLOSE(ncidin)  ; PRINT *,'Close file :',NF90_STRERROR(istatus)

  istatus=NF90_CLOSE(ncidout0)  ; PRINT *,'Close file :',NF90_STRERROR(istatus)
  istatus=NF90_CLOSE(ncidout001)  ; PRINT *,'Close file :',NF90_STRERROR(istatus)
  istatus=NF90_CLOSE(ncidout01)  ; PRINT *,'Close file :',NF90_STRERROR(istatus)
  istatus=NF90_CLOSE(ncidout05)  ; PRINT *,'Close file :',NF90_STRERROR(istatus)
  istatus=NF90_CLOSE(ncidout1)  ; PRINT *,'Close file :',NF90_STRERROR(istatus)




























END PROGRAM nwd

