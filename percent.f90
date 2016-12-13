PROGRAM percent
  !--------------------------------------------------------------------------------------
  !
  !---------------------------------------------------------------------------------------

  USE netcdf
  IMPLICIT NONE
  INTEGER :: narg, iargc, jarg, npt, jt
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:,:,:) :: precipin
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:) :: allval 
  REAL(KIND=4) :: a ,temp, selec

  CHARACTER(LEN=256) :: cfilein, cfileout1, cfileout2, cdum, ctag='none'

  INTEGER :: istatus, ncidin, ncidout1, ncidout2, id_xin, id_yin, id_xout, id_yout, npxin, id_tin, nptin
  INTEGER :: npyin, npxout, npyout, id_varlatin, id_varlonin, id_varlatout, nptout, id_tout
  INTEGER :: id_varlonout, id_varprecipin, id_varprecipout, id_varintens
  INTEGER :: ji, jj, jii, jjj, it, it1, it2, quant, siz
  INTEGER :: i, ir, j, l, mid

  ! * 
  narg=iargc()
  IF ( narg == 0 ) THEN
     PRINT *,' USAGE : percent -f filein -o fileout'
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
        CALL getarg(jarg,cfileout1) ;jarg=jarg +1
     CASE DEFAULT
         PRINT *,' Option ',TRIM(cdum),' unknown'
         STOP
     END SELECT
  END DO

  istatus=NF90_OPEN(cfilein,NF90_WRITE,ncidin)  ; PRINT *,'Open file :',NF90_STRERROR(istatus)
  istatus=NF90_OPEN(cfileout1,NF90_WRITE,ncidout1)  ; PRINT *,'Open file :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncidin,'lon',id_xin)  ; PRINT *,'Inq dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncidin,id_xin,len=npxin)  ; PRINT *,'Inquire dimension :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncidin,'lat',id_yin)  ; PRINT *,'Inq dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncidin,id_yin,len=npyin)  ; PRINT *,'Inquire dimension :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncidin,'time',id_tin)  ; PRINT *,'Inq dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncidin,id_tin,len=nptin)  ; PRINT *,'Inquire dimension :',NF90_STRERROR(istatus)


  ALLOCATE( precipin(npxin,npyin,nptin) )
  ALLOCATE( allval(npxin*npyin*nptin) )

  istatus=NF90_INQ_VARID(ncidin,'precip',id_varprecipin)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncidin,id_varprecipin,precipin)  ; PRINT *,'Get var :',NF90_STRERROR(istatus)


  istatus=NF90_INQ_VARID(ncidout1,'precip',id_varprecipout)  ; PRINT *,'Inq varid :',NF90_STRERROR(istatus)




  DO ji = 1, npxin
    DO jj = 1, npyin
      DO it = 1, nptin

        allval( (ji - 1) + (jj - 1)*npxin + (jt - 1)*npxin*npyin + 1) = precipin(ji,jj,jt)

      END DO
    END DO
  END DO

  siz=INT(npxin*npyin*nptin)
  quant=INT(npxin*npyin*nptin*0.9)
  PRINT *, '90% des precips sont plus faibles que la ',quant,'eme valeur sur ',siz


  l=1.
  ir=siz

1 IF ( ir-l < 1. ) THEN
  
    IF ( ir-l == 1. ) THEN

      IF ( allval(ir) < allval(l) ) THEN

        temp=allval(l)
        allval(l)=allval(ir)
        allval(ir)=temp

      END IF

    END IF

    selec=allval(quant)

  ELSE

    mid=(l+ir)/2.
    temp=allval(mid)
    allval(mid)=allval(l+1)
    allval(l+1)=temp

    IF ( allval(l) > allval(ir) ) THEN
      temp=allval(l)
      allval(l)=allval(ir)
      allval(ir)=temp
    ENDIF

    IF ( allval(l+1) > allval(ir) ) THEN
      temp=allval(l+1)
      allval(l+1)=allval(ir)
      allval(ir)=temp
    ENDIF

    IF ( allval(l) > allval(l+1) ) THEN
      temp=allval(l)
      allval(l)=allval(l+1)
      allval(l+1)=temp
    ENDIF

    i=l+1
    j=ir
    a=allval(l+1)

3   continue

      i=i+1
      IF ( allval(i) < a ) goto 3

4   continue

      j=j-1
      IF ( allval(j) > a ) goto 4
      IF ( j < i ) goto 5

      temp=allval(i)
      allval(i)=allval(j)
      allval(j)=temp
      
      goto 3

5     allval(l+1)=allval(j)
      allval(j)=a

      IF ( j >  quant) ir=j-1
      IF ( j < quant ) l=i
  END IF  

  goto 1

  PRINT *, '90% des precips sont plus faibles que ',allval(quant)

  DO ji = 1, npxin
    DO jj = 1, npyin
      DO it = 1, nptin

        IF ( precipin(ji,jj,jt) < allval(quant) ) precipin(ji,jj,jt)=0.

      END DO
    END DO
  END DO


  istatus=NF90_PUT_VAR(ncidout1,id_varprecipout,precipin)  ; PRINT *,'Put var :',NF90_STRERROR(istatus)



  DEALLOCATE( precipin,  allval)

  istatus=NF90_CLOSE(ncidin)  ; PRINT *,'Close file :',NF90_STRERROR(istatus)

  istatus=NF90_CLOSE(ncidout1)  ; PRINT *,'Close file :',NF90_STRERROR(istatus)




























END PROGRAM percent

