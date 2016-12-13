PROGRAM new_interp
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
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:) :: latin
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:) :: lonin
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:) :: latout
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:) :: lonout
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:,:,:) :: precipin
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:,:,:) :: precipout

  CHARACTER(LEN=256) :: cfilein, cfileout, cdum, ctag='none'

  INTEGER :: istatus, ncidin, ncidout, id_xin, id_yin, id_xout, id_yout, npxin
  INTEGER :: npyin, npxout, npyout, id_varlatin, id_varlonin, id_varlatout
  INTEGER :: id_varlonout, id_varprecipin, id_varprecipout
  INTEGER :: ji, jj, jii, jjj
  ! * 
  narg=iargc()
  IF ( narg == 0 ) THEN
     PRINT *,' USAGE : new_interp_GPCP -f filein -o fileout'
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
        CALL getarg(jarg,cfileout) ;jarg=jarg +1
     CASE DEFAULT
         PRINT *,' Option ',TRIM(cdum),' unknown'
         STOP
     END SELECT
  END DO

  istatus=NF90_OPEN(cfilein,NF90_WRITE,ncidin)  ; PRINT *,'Open file :',NF90_STRERROR(istatus)
  istatus=NF90_OPEN(cfileout,NF90_WRITE,ncidout)  ; PRINT *,'Open file :',NF90_STRERROR(istatus)
  !  look for time_counter variable :
  istatus=NF90_INQ_DIMID(ncidin,'lon',id_xin)  ; PRINT *,'Inq dimid :',NF90_STRERROR(istatus)

  istatus=NF90_INQUIRE_DIMENSION(ncidin,id_xin,len=npxin)  ; PRINT *,'Inquire dimension :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncidin,'lat',id_yin)  ; PRINT *,'Inq dimid :',NF90_STRERROR(istatus)

  istatus=NF90_INQUIRE_DIMENSION(ncidin,id_yin,len=npyin)  ; PRINT *,'Inquire dimension :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncidout,'lon',id_xout)  ; PRINT *,'Inq dimid :',NF90_STRERROR(istatus)

  istatus=NF90_INQUIRE_DIMENSION(ncidout,id_xout,len=npxout)  ; PRINT *,'Inquire dimension :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncidout,'lat',id_yout)  ; PRINT *,'Inq dimid :',NF90_STRERROR(istatus)

  istatus=NF90_INQUIRE_DIMENSION(ncidout,id_yout,len=npyout)  ; PRINT *,'Inquire dimension :',NF90_STRERROR(istatus)


  ALLOCATE( latin(npyin), lonin(npxin), latout(npyout), lonout(npxout) ) 
  ALLOCATE( precipin(npxin,npyin,1), precipout(npxout,npyout,1) )

  istatus=NF90_INQ_VARID(ncidin,'lat',id_varlatin)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)

  istatus=NF90_GET_VAR(ncidin,id_varlatin,latin)  ; PRINT *,'Get var :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_VARID(ncidin,'lon',id_varlonin)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)

  istatus=NF90_GET_VAR(ncidin,id_varlonin,lonin)  ; PRINT *,'Get var :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_VARID(ncidout,'lat',id_varlatout)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)

  istatus=NF90_GET_VAR(ncidout,id_varlatout,latout)  ; PRINT *,'Get var :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_VARID(ncidout,'lon',id_varlonout)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)

  istatus=NF90_GET_VAR(ncidout,id_varlonout,lonout)  ; PRINT *,'Get var :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_VARID(ncidin,'PREC',id_varprecipin)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)

  istatus=NF90_GET_VAR(ncidin,id_varprecipin,precipin)  ; PRINT *,'Get var :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_VARID(ncidout,'Precip',id_varprecipout)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)

  istatus=NF90_GET_VAR(ncidout,id_varprecipout,precipout)  ; PRINT *,'Get var :',NF90_STRERROR(istatus)


  precipout(:,:,1)=0.

  DO ji = 1, npxin
    DO jj = 1, npyin

      DO jii = 1, npxout
        IF ( lonout(jii) == lonin(ji) ) THEN
          DO jjj = 1, npyout
            IF ( latout(jjj) == latin(jj) ) THEN

              precipout(jii  ,jjj  ,1)=precipin(ji,jj,1)

              IF ( jii+1 < npxout ) THEN
                precipout(jii+1,jjj  ,1)=precipin(ji,jj,1)
                IF ( jjj+1 < npyout ) THEN
                  precipout(jii+1,jjj+1,1)=precipin(ji,jj,1)
                  precipout(jii  ,jjj+1,1)=precipin(ji,jj,1)
                ENDIF
              ENDIF
    
            ENDIF    
          END DO
        ENDIF
      END DO 

    END DO
  END DO

  istatus=NF90_PUT_VAR(ncidout,id_varprecipout,precipout)  ; PRINT *,'Put var :',NF90_STRERROR(istatus)



  DEALLOCATE( latin, lonin, latout, lonout ) 
  DEALLOCATE( precipin, precipout )

  istatus=NF90_CLOSE(ncidin)  ; PRINT *,'Close file :',NF90_STRERROR(istatus)

  istatus=NF90_CLOSE(ncidout)  ; PRINT *,'Close file :',NF90_STRERROR(istatus)




























END PROGRAM new_interp

