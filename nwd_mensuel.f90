PROGRAM nwd_mensuel
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
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:,:,:) :: precipout
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:) :: nbmonth

  CHARACTER(LEN=256) :: cfilein, cfileout, cdum, ctag='none'

  INTEGER :: istatus, ncidin, ncidout, id_xin, id_yin, id_xout, id_yout, npxin, id_tin, nptin
  INTEGER :: npyin, npxout, npyout, id_varlatin, id_varlonin, id_varlatout, nptout, id_tout
  INTEGER :: id_varlonout, id_varprecipin, id_varprecipout
  INTEGER :: ji, jj, jii, jjj, it, it1, it2
  ! * 
  narg=iargc()
  IF ( narg == 0 ) THEN
     PRINT *,' USAGE : nwd_mensuel -f filein -o fileout'
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

  istatus=NF90_INQ_DIMID(ncidin,'lon',id_xin)  ; PRINT *,'Inq dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncidin,id_xin,len=npxin)  ; PRINT *,'Inquire dimension :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncidin,'lat',id_yin)  ; PRINT *,'Inq dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncidin,id_yin,len=npyin)  ; PRINT *,'Inquire dimension :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncidin,'time',id_tin)  ; PRINT *,'Inq dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncidin,id_tin,len=nptin)  ; PRINT *,'Inquire dimension :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncidout,'time',id_tout)  ; PRINT *,'Inq dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncidout,id_tout,len=nptout)  ; PRINT *,'Inquire dimension :',NF90_STRERROR(istatus)

  ALLOCATE( precipin(npxin,npyin,nptin), precipout(npxin,npyin,nptout) )
  ALLOCATE( nbmonth(nptout) )


  istatus=NF90_INQ_VARID(ncidin,'precip',id_varprecipin)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncidin,id_varprecipin,precipin)  ; PRINT *,'Get var :',NF90_STRERROR(istatus)


  istatus=NF90_INQ_VARID(ncidout,'precip',id_varprecipout)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)

  precipout(:,:,:)=0.

  nbmonth=[31,28,31,30,31,30,31,31,30,31,30,31,31,28,31,30,31,30,31,31,30,31,30,31,31,29,31,30,31,30,31,31,30,31,30,31,31,28,31,30,31,30,31,31,30,31,30,31,31,28,31,30,31,30,31,31,30,31,30,31,31,28,31,30,31,30,31,31,30,31,30,31,31,29,31,30,31,30,31,31,30,31,30,31,31,28,31,30,31,30,31,31,30,31,30,31,31,28,31,30,31,30,31,31,30,31,30,31,31,28,31,30,31,30,31,31,30,31,30,31,31,29,31,30,31,30,31,31,30,31,30,31,31,28,31,30,31,30,31,31,30,31,30,31,31,28,31,30,31,30,31,31,30,31,30,31]


  DO ji = 1, npxin
    DO jj = 1, npyin

      it1 = 1
      it2 = nbmonth(1)


      DO it = it1, it2

        IF ( precipin(ji,jj,it) >= 0.1 ) THEN

           precipout(ji,jj,1) = precipout(ji,jj,1) + 1.

        ENDIF

      END DO

      precipout(ji,jj,1) = 100.* precipout(ji,jj,1) / nbmonth(1)

      DO jt = 2, nptout

        it1 = it2 + 1
        it2 = it1 + nbmonth(jt) - 1


        DO it = it1, it2

          IF ( precipin(ji,jj,it) >= 0.1 ) THEN

             precipout(ji,jj,jt) = precipout(ji,jj,jt) + 1.

          ENDIF

        END DO

        precipout(ji,jj,jt) = 100.* precipout(ji,jj,jt) / nbmonth(jt)

      END DO

    END DO
  END DO


  istatus=NF90_PUT_VAR(ncidout,id_varprecipout,precipout)  ; PRINT *,'Put var :',NF90_STRERROR(istatus)



  DEALLOCATE( precipin, precipout )

  istatus=NF90_CLOSE(ncidin)  ; PRINT *,'Close file :',NF90_STRERROR(istatus)

  istatus=NF90_CLOSE(ncidout)  ; PRINT *,'Close file :',NF90_STRERROR(istatus)




























END PROGRAM nwd_mensuel

