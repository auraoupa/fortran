PROGRAM return_lon
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
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:,:,:) :: intensin
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:,:,:) :: precipout
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:,:,:) :: intensout
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:) :: lonin
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:) :: lonout
  CHARACTER(LEN=256) :: cfilein, cfileout, cdum, ctag='none'

  INTEGER :: istatus, ncidin, ncidout, id_xin, id_yin, id_xout, id_yout, npxin, id_tin, nptin
  INTEGER :: npyin, npxout, npyout, id_varlatin, id_varlonin, id_varlatout
  INTEGER :: id_varlonout, id_varprecipin, id_varprecipout, id_varintensin, id_varintensout
  INTEGER :: ji, jj, jii, jjj
  ! * 
  narg=iargc()
  IF ( narg == 0 ) THEN
     PRINT *,' USAGE : return_lon -f filein '
     PRINT *,'         '
     STOP
  ENDIF

  jarg=1
  DO WHILE ( jarg <= narg )
     CALL getarg(jarg, cdum) ; jarg=jarg + 1
     SELECT CASE (cdum)
     CASE ( '-f' )
        CALL getarg(jarg,cfilein) ;jarg=jarg +1
     CASE DEFAULT
         PRINT *,' Option ',TRIM(cdum),' unknown'
         STOP
     END SELECT
  END DO

  istatus=NF90_OPEN(cfilein,NF90_WRITE,ncidin)  ; PRINT *,'Open file :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncidin,'lon',id_xin)  ; PRINT *,'Inq dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncidin,id_xin,len=npxin)  ; PRINT *,'Inquire dimension :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncidin,'lat',id_yin)  ; PRINT *,'Inq dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncidin,id_yin,len=npyin)  ; PRINT *,'Inquire dimension :',NF90_STRERROR(istatus)



  ALLOCATE( precipin(npxin,npyin,1), precipout(npxin,npyin,1) )
  ALLOCATE( intensin(npxin,npyin,1), intensout(npxin,npyin,1) )
  ALLOCATE( lonin(npxin), lonout(npxin) )


  istatus=NF90_INQ_VARID(ncidin,'precip',id_varprecipin)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncidin,id_varprecipin,precipin)  ; PRINT *,'Get var :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_VARID(ncidin,'intens',id_varintensin)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncidin,id_varintensin,intensin)  ; PRINT *,'Get var :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_VARID(ncidin,'lon',id_varlonin)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncidin,id_varlonin,lonin)  ; PRINT *,'Get var :',NF90_STRERROR(istatus)

  precipout(:,:,1)=0.
  intensout(:,:,1)=0.
  lonout(:)=0.

  DO ji = 1, 270
    DO jj = 1, npyin

      precipout(ji,jj,1) = precipin(ji+270,jj,1) 
      intensout(ji,jj,1) = intensin(ji+270,jj,1) 

    END DO

    lonout(ji) = lonin(ji+270)

  END DO

  DO ji = 271, 540
    DO jj = 1, npyin

      precipout(ji,jj,1) = precipin(ji-270,jj,1)
      intensout(ji,jj,1) = intensin(ji-270,jj,1)

    END DO

    lonout(ji) = lonin(ji-270)+360.

  END DO


  istatus=NF90_PUT_VAR(ncidin,id_varprecipin,precipout)  ; PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncidin,id_varintensin,intensout)  ; PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncidin,id_varlonin,lonout)  ; PRINT *,'Put var :',NF90_STRERROR(istatus)



  DEALLOCATE( precipin, precipout )
  DEALLOCATE( intensin, intensout )

  istatus=NF90_CLOSE(ncidin)  ; PRINT *,'Close file :',NF90_STRERROR(istatus)





























END PROGRAM return_lon

