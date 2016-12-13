PROGRAM cdfangle
  !!======================================================================
  !!                     ***  PROGRAM  cdfangle  ***
  !!=====================================================================
  !!  ** Purpose : Return the angles between model grid lines 
  !!               and the North direction
  !!
  !!  ** Method  : Read the coordinate/mesh_hgr file and look for the glam,
  !!               gphi variables.
  !!               Then Compute (gsint, gcost, gsinu, gcosu, gsinv, gcosv,
  !!               gsinf, gcosf) arrays:
  !!               sinus and cosinus of the angle between the north-south axe 
  !!               and the j-direction at t, u, v and f-points
  !!               Add angles in the coord file
  !!----------------------------------------------------------------------
  USE netcdf
  !!----------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER(KIND=4)    :: ji,jj
  INTEGER(KIND=4)    :: ncin, id_x, id_y, id_glamt, id_gphit, id_glamu, id_gphiu, id_glamv, id_gphiv, id_glamf, id_gphif
  INTEGER(KIND=4)    :: id_gsint, id_gcost, id_gsinu, id_gcosu, id_gsinv, id_gcosv, id_gsinf, id_gcosf
  INTEGER(KIND=4)    :: istatus

  CHARACTER(LEN=256) :: cf_in='coord.nc'

  REAL(KIND=8), SAVE, DIMENSION(:,:), ALLOCATABLE :: dl_glamt, dl_gphit
  REAL(KIND=8), SAVE, DIMENSION(:,:), ALLOCATABLE :: dl_glamu, dl_gphiu
  REAL(KIND=8), SAVE, DIMENSION(:,:), ALLOCATABLE :: dl_glamv, dl_gphiv
  REAL(KIND=8), SAVE, DIMENSION(:,:), ALLOCATABLE :: dl_glamf, dl_gphif
  REAL(KIND=8), SAVE, DIMENSION(:,:), ALLOCATABLE :: gsint, gcost, gsinu, gcosu, gsinv, gcosv, gsinf, gcosf

  INTEGER(KIND=4)    :: npiglo, npjglo     

  REAL(KIND=8), PARAMETER :: rpi = 3.141592653e0
  REAL(KIND=8), PARAMETER :: rad = rpi / 180.e0
  REAL(KIND=4)       :: zglamfound, zglamin, zglamax, zgphmin, zgphmax

  REAL(KIND=8) ::   &
         zlam, zphi,            &  ! temporary scalars
         zlan, zphh,            &  !    "         "
         zxnpt, zynpt, znnpt,   &  ! x,y components and norm of the vector: T point to North Pole
         zxnpu, zynpu, znnpu,   &  ! x,y components and norm of the vector: U point to North Pole
         zxnpv, zynpv, znnpv,   &  ! x,y components and norm of the vector: V point to North Pole
         zxnpf, zynpf, znnpf,   &  ! x,y components and norm of the vector: F point to North Pole
         zxvvt, zyvvt, znvvt,   &  ! x,y components and norm of the vector: between V points below and above a T point
         zxffu, zyffu, znffu,   &  ! x,y components and norm of the vector: between F points below and above a U point
         zxffv, zyffv, znffv,   &  ! x,y components and norm of the vector: between F points left  and right a V point
         zxuuf, zyuuf, znuuf       ! x,y components and norm of the vector: between U points below and above a F point

  !!----------------------------------------------------------------------

  istatus=NF90_OPEN(cf_in,NF90_WRITE,ncin); PRINT *,'Open file :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncin,'x',id_x); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncin,id_x,len=npiglo)
  istatus=NF90_INQ_DIMID(ncin,'y',id_y); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncin,id_y,len=npjglo)


  ALLOCATE (dl_glamt(npiglo,npjglo), dl_gphit(npiglo,npjglo) )
  ALLOCATE (dl_glamu(npiglo,npjglo), dl_gphiu(npiglo,npjglo) )
  ALLOCATE (dl_glamv(npiglo,npjglo), dl_gphiv(npiglo,npjglo) )
  ALLOCATE (dl_glamf(npiglo,npjglo), dl_gphif(npiglo,npjglo) )
  ALLOCATE (  gsint(npiglo,npjglo),  gcost(npiglo,npjglo) )
  ALLOCATE (  gsinu(npiglo,npjglo),  gcosu(npiglo,npjglo) )
  ALLOCATE (  gsinv(npiglo,npjglo),  gcosv(npiglo,npjglo) )
  ALLOCATE (  gsinf(npiglo,npjglo),  gcosf(npiglo,npjglo) )

  istatus=NF90_INQ_VARID(ncin,'glamt',id_glamt); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncin,id_glamt,dl_glamt); PRINT *,'Get var :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_VARID(ncin,'gphit',id_gphit); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncin,id_gphit,dl_gphit); PRINT *,'Get var :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_VARID(ncin,'glamu',id_glamu); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncin,id_glamu,dl_glamu); PRINT *,'Get var :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_VARID(ncin,'gphiu',id_gphiu); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncin,id_gphiu,dl_gphiu); PRINT *,'Get var :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_VARID(ncin,'glamv',id_glamv); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncin,id_glamv,dl_glamv); PRINT *,'Get var :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_VARID(ncin,'gphiv',id_gphiv); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncin,id_gphiv,dl_gphiv); PRINT *,'Get var :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_VARID(ncin,'glamf',id_glamf); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncin,id_glamf,dl_glamf); PRINT *,'Get var :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_VARID(ncin,'gphif',id_gphif); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncin,id_gphif,dl_gphif); PRINT *,'Get var :',NF90_STRERROR(istatus)


      DO jj = 2, npjglo-1
         DO ji = 2, npiglo   ! vector opt.

            ! north pole direction & modulous (at t-point)
            zlam = dl_glamt(ji,jj)
            zphi = dl_gphit(ji,jj)
            zxnpt = 0. - 2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            zynpt = 0. - 2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            znnpt = zxnpt*zxnpt + zynpt*zynpt

            ! north pole direction & modulous (at u-point)
            zlam = dl_glamu(ji,jj)
            zphi = dl_gphiu(ji,jj)
            zxnpu = 0. - 2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            zynpu = 0. - 2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            znnpu = zxnpu*zxnpu + zynpu*zynpu

            ! north pole direction & modulous (at v-point)
            zlam = dl_glamv(ji,jj)
            zphi = dl_gphiv(ji,jj)
            zxnpv = 0. - 2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            zynpv = 0. - 2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            znnpv = zxnpv*zxnpv + zynpv*zynpv

            ! north pole direction & modulous (at f-point)
            zlam = dl_glamf(ji,jj)
            zphi = dl_gphif(ji,jj)
            zxnpf = 0. - 2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            zynpf = 0. - 2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            znnpf = zxnpf*zxnpf + zynpf*zynpf
            ! j-direction: v-point segment direction (around t-point)
            zlam = dl_glamv(ji,jj  )
            zphi = dl_gphiv(ji,jj  )
            zlan = dl_glamv(ji,jj-1)
            zphh = dl_gphiv(ji,jj-1)
            zxvvt =  2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )   &
               &  -  2. * COS( rad*zlan ) * TAN( rpi/4. - rad*zphh/2. )
            zyvvt =  2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )   &
               &  -  2. * SIN( rad*zlan ) * TAN( rpi/4. - rad*zphh/2. )
            znvvt = SQRT( znnpt * ( zxvvt*zxvvt + zyvvt*zyvvt )  )
            znvvt = MAX( znvvt, 1.e-14 )

            ! j-direction: f-point segment direction (around u-point)
            zlam = dl_glamf(ji,jj  )
            zphi = dl_gphif(ji,jj  )
            zlan = dl_glamf(ji,jj-1)
            zphh = dl_gphif(ji,jj-1)
            zxffu =  2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )   &
               &  -  2. * COS( rad*zlan ) * TAN( rpi/4. - rad*zphh/2. )
            zyffu =  2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )   &
               &  -  2. * SIN( rad*zlan ) * TAN( rpi/4. - rad*zphh/2. )
            znffu = SQRT( znnpu * ( zxffu*zxffu + zyffu*zyffu )  )
            znffu = MAX( znffu, 1.e-14 )

            ! i-direction: f-point segment direction (around v-point)
            zlam = dl_glamf(ji  ,jj)
            zphi = dl_gphif(ji  ,jj)
            zlan = dl_glamf(ji-1,jj)
            zphh = dl_gphif(ji-1,jj)
            zxffv =  2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )   &
               &  -  2. * COS( rad*zlan ) * TAN( rpi/4. - rad*zphh/2. )
            zyffv =  2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )   &
               &  -  2. * SIN( rad*zlan ) * TAN( rpi/4. - rad*zphh/2. )
            znffv = SQRT( znnpv * ( zxffv*zxffv + zyffv*zyffv )  )
            znffv = MAX( znffv, 1.e-14 )

            ! j-direction: u-point segment direction (around f-point)
            zlam = dl_glamu(ji,jj+1)
            zphi = dl_gphiu(ji,jj+1)
            zlan = dl_glamu(ji,jj  )
            zphh = dl_gphiu(ji,jj  )
            zxuuf =  2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )   &
               &  -  2. * COS( rad*zlan ) * TAN( rpi/4. - rad*zphh/2. )
            zyuuf =  2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )   &
               &  -  2. * SIN( rad*zlan ) * TAN( rpi/4. - rad*zphh/2. )
            znuuf = SQRT( znnpf * ( zxuuf*zxuuf + zyuuf*zyuuf )  )
            znuuf = MAX( znuuf, 1.e-14 )

            ! cosinus and sinus using scalar and vectorial products
            gsint(ji,jj) = ( zxnpt*zyvvt - zynpt*zxvvt ) / znvvt
            gcost(ji,jj) = ( zxnpt*zxvvt + zynpt*zyvvt ) / znvvt

            gsinu(ji,jj) = ( zxnpu*zyffu - zynpu*zxffu ) / znffu
            gcosu(ji,jj) = ( zxnpu*zxffu + zynpu*zyffu ) / znffu

            gsinf(ji,jj) = ( zxnpf*zyuuf - zynpf*zxuuf ) / znuuf
            gcosf(ji,jj) = ( zxnpf*zxuuf + zynpf*zyuuf ) / znuuf

            ! (caution, rotation of 90 degres)
            gsinv(ji,jj) = ( zxnpv*zxffv + zynpv*zyffv ) / znffv
            gcosv(ji,jj) =-( zxnpv*zyffv - zynpv*zxffv ) / znffv

         END DO
      END DO

      DO jj = 2, npjglo-1
         DO ji = 2, npiglo   ! vector opt.
            IF( MOD( ABS( dl_glamv(ji,jj) - dl_glamv(ji,jj-1) ), 360. ) < 1.e-8 ) THEN
               gsint(ji,jj) = 0.
               gcost(ji,jj) = 1.
            ENDIF
            IF( MOD( ABS( dl_glamf(ji,jj) - dl_glamf(ji,jj-1) ), 360. ) < 1.e-8 ) THEN
               gsinu(ji,jj) = 0.
               gcosu(ji,jj) = 1.
            ENDIF
            IF(      ABS( dl_gphif(ji,jj) - dl_gphif(ji-1,jj) )         < 1.e-8 ) THEN
               gsinv(ji,jj) = 0.
               gcosv(ji,jj) = 1.
            ENDIF
            IF( MOD( ABS( dl_glamu(ji,jj) - dl_glamu(ji,jj+1) ), 360. ) < 1.e-8 ) THEN
               gsinf(ji,jj) = 0.
               gcosf(ji,jj) = 1.
            ENDIF
         END DO
      END DO

  ! create output fileset
  ! create output file taking the sizes in cf_in
  istatus=NF90_REDEF(ncin)
  istatus=NF90_DEF_VAR(ncin,'gsint',NF90_FLOAT,(/id_x,id_y/), id_gsint ); PRINT *,'Def var :',NF90_STRERROR(istatus)
  istatus=NF90_DEF_VAR(ncin,'gcost',NF90_FLOAT,(/id_x,id_y/), id_gcost ); PRINT *,'Def var :',NF90_STRERROR(istatus)
  istatus=NF90_DEF_VAR(ncin,'gsinu',NF90_FLOAT,(/id_x,id_y/), id_gsinu ); PRINT *,'Def var :',NF90_STRERROR(istatus)
  istatus=NF90_DEF_VAR(ncin,'gcosu',NF90_FLOAT,(/id_x,id_y/), id_gcosu ); PRINT *,'Def var :',NF90_STRERROR(istatus)
  istatus=NF90_DEF_VAR(ncin,'gsinv',NF90_FLOAT,(/id_x,id_y/), id_gsinv ); PRINT *,'Def var :',NF90_STRERROR(istatus)
  istatus=NF90_DEF_VAR(ncin,'gcosv',NF90_FLOAT,(/id_x,id_y/), id_gcosv ); PRINT *,'Def var :',NF90_STRERROR(istatus)
  istatus=NF90_DEF_VAR(ncin,'gsinf',NF90_FLOAT,(/id_x,id_y/), id_gsinf ); PRINT *,'Def var :',NF90_STRERROR(istatus)
  istatus=NF90_DEF_VAR(ncin,'gcosf',NF90_FLOAT,(/id_x,id_y/), id_gcosf ); PRINT *,'Def var :',NF90_STRERROR(istatus)
  istatus=NF90_ENDDEF(ncin)
  istatus=NF90_PUT_VAR(ncin,id_gsint,gsint); PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncin,id_gcost,gcost); PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncin,id_gsinu,gsinu); PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncin,id_gcosu,gcosu); PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncin,id_gsinv,gsinv); PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncin,id_gcosv,gcosv); PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncin,id_gsinf,gsinf); PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncin,id_gcosf,gcosf); PRINT *,'Put var :',NF90_STRERROR(istatus)

  istatus=NF90_CLOSE(ncin)



    DEALLOCATE (dl_glamt, dl_gphit )
    DEALLOCATE (dl_glamu, dl_gphiu )
    DEALLOCATE (dl_glamv, dl_gphiv )
    DEALLOCATE (dl_glamf, dl_gphif )
    DEALLOCATE (  gsint,  gcost )
    DEALLOCATE (  gsinu,  gcosu )
    DEALLOCATE (  gsinv,  gcosv )
    DEALLOCATE (  gsinf,  gcosf )

END PROGRAM cdfangle
