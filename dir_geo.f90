PROGRAM dir_geo
  !--------------------------------------------------------------------------------------
  !                            *** PROGRAM dir_geo  ***
  !
  !        ** Purpose: squeeze a vertical dimension to variable
  !
  !   History:
  !---------------------------------------------------------------------------------------
  USE netcdf
  IMPLICIT NONE

  INTEGER :: narg, iargc, jarg, ncol
  INTEGER :: n_bef, n_aft
  INTEGER :: npx, npy, npt, npk, jt, npz, jk, jj, ji
  CHARACTER(LEN=80) :: ufile, vfile, pxinname, pyinname, meshfile
  CHARACTER(LEN=1)  :: cobc, cvar
  REAL(KIND=4), DIMENSION (:,:,:,:), ALLOCATABLE :: pxin
  REAL(KIND=4), DIMENSION (:,:,:,:), ALLOCATABLE :: pyin
  REAL(KIND=4), DIMENSION (:,:,:,:), ALLOCATABLE :: pxout
  REAL(KIND=4), DIMENSION (:,:,:,:), ALLOCATABLE :: pyout
  REAL(KIND=4), DIMENSION (:,:,:), ALLOCATABLE :: glamt
  REAL(KIND=4), DIMENSION (:,:,:), ALLOCATABLE :: glamu
  REAL(KIND=4), DIMENSION (:,:,:), ALLOCATABLE :: glamv
  REAL(KIND=4), DIMENSION (:,:,:), ALLOCATABLE :: glamf
  REAL(KIND=4), DIMENSION (:,:,:), ALLOCATABLE :: gphit
  REAL(KIND=4), DIMENSION (:,:,:), ALLOCATABLE :: gphiu
  REAL(KIND=4), DIMENSION (:,:,:), ALLOCATABLE :: gphiv
  REAL(KIND=4), DIMENSION (:,:,:), ALLOCATABLE :: gphif
  REAL(KIND=4), ALLOCATABLE, DIMENSION(:,:) ::   &
      gsint, gcost,   &  ! cos/sin between model grid lines and NP direction at T point
      gsinu, gcosu,   &  ! cos/sin between model grid lines and NP direction at U point
      gsinv, gcosv,   &  ! cos/sin between model grid lines and NP direction at V point
      gsinf, gcosf       ! cos/sin between model grid lines and NP direction at F point
  REAL(KIND=4) ::   &
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
  REAL(KIND=4), PARAMETER :: rpi = 3.141592653e0
  REAL(KIND=4), PARAMETER :: rad = rpi / 180.e0
  ! Netcdf Stuff
  INTEGER :: istatus, ncidu, ncidv, ncmeshid, id_t, id_x, id_y, id_z, idxin, idyin, idxout, idyout
  INTEGER :: id_tv, id_xv, id_yv, id_zv
  INTEGER :: id_glt, id_glu, id_glv, id_glf, id_gpt, id_gpu, id_gpv, id_gpf
  INTEGER :: ncidout, ids, idt, idep, idtime, idlon, id_xout, id_zout, id_tout, idvarout1,idvarout2,idvarout3,idvarout4
  ! * 
  narg=iargc()
  IF ( narg == 0 ) THEN
     PRINT *,' USAGE : dir_geo ufile vfile meshfile '
     STOP
  ENDIF

  CALL getarg( 1, ufile)
  CALL getarg( 2, vfile)
  CALL getarg( 3, meshfile)

  istatus=NF90_OPEN(ufile,NF90_WRITE,ncidu); PRINT *,'Open file :',NF90_STRERROR(istatus)
  istatus=NF90_OPEN(vfile,NF90_WRITE,ncidv); PRINT *,'Open file :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncidu,'time_counter',id_t); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_DIMID(ncidv,'time_counter',id_tv); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncidu,id_t,len=npt)
  istatus=NF90_INQ_DIMID(ncidu,'x',id_x); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_DIMID(ncidv,'x',id_xv); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncidu,id_x,len=npx)
  istatus=NF90_INQ_DIMID(ncidu,'y',id_y); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_DIMID(ncidv,'y',id_yv); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncidu,id_y,len=npy)
  istatus=NF90_INQ_DIMID(ncidu,'depthu',id_z); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_DIMID(ncidv,'depthv',id_zv); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncidu,id_z,len=npz)

  ALLOCATE( pxin(npx,npy,npz,npt), pyin(npx,npy,npz,npt) )

  istatus=NF90_INQ_VARID(ncidu,'vozocrtx',idxin); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncidu,idxin,pxin); PRINT *,'Get var :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_VARID(ncidv,'vomecrty',idyin); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncidv,idyin,pyin); PRINT *,'Get var :',NF90_STRERROR(istatus)


  istatus=NF90_OPEN(meshfile,NF90_WRITE,ncmeshid); PRINT *,'Open file :',NF90_STRERROR(istatus)

  ALLOCATE( glamt(npx,npy,npt), glamu(npx,npy,npt), glamv(npx,npy,npt), glamf(npx,npy,npt) )
  ALLOCATE( gphit(npx,npy,npt), gphiu(npx,npy,npt), gphiv(npx,npy,npt), gphif(npx,npy,npt) )

  istatus=NF90_INQ_VARID(ncmeshid,'glamt',id_glt); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_VARID(ncmeshid,'glamu',id_glu); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_VARID(ncmeshid,'glamv',id_glv); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_VARID(ncmeshid,'glamf',id_glf); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_VARID(ncmeshid,'gphit',id_gpt); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_VARID(ncmeshid,'gphiu',id_gpu); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_VARID(ncmeshid,'gphiv',id_gpv); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_VARID(ncmeshid,'gphif',id_gpf); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)

  istatus=NF90_GET_VAR(ncmeshid,id_glt,glamt); PRINT *,'Get var :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncmeshid,id_glu,glamu); PRINT *,'Get var :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncmeshid,id_glv,glamv); PRINT *,'Get var :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncmeshid,id_glf,glamf); PRINT *,'Get var :',NF90_STRERROR(istatus)

  istatus=NF90_GET_VAR(ncmeshid,id_gpt,gphit); PRINT *,'Get var :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncmeshid,id_gpu,gphiu); PRINT *,'Get var :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncmeshid,id_gpv,gphiv); PRINT *,'Get var :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncmeshid,id_gpf,gphif); PRINT *,'Get var :',NF90_STRERROR(istatus)

      ALLOCATE( gsint(npx,npy), gcost(npx,npy),   &
         &      gsinu(npx,npy), gcosu(npx,npy),   &
         &      gsinv(npx,npy), gcosv(npx,npy),   &
         &      gsinf(npx,npy), gcosf(npx,npy) )

      DO jj = 2, npy-1
         DO ji = 2, npx   ! vector opt.

            ! north pole direction & modulous (at t-point)
            zlam = glamt(ji,jj,1)
            zphi = gphit(ji,jj,1)
            zxnpt = 0. - 2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            zynpt = 0. - 2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            znnpt = zxnpt*zxnpt + zynpt*zynpt

            ! north pole direction & modulous (at u-point)
            zlam = glamu(ji,jj,1)
            zphi = gphiu(ji,jj,1)
            zxnpu = 0. - 2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            zynpu = 0. - 2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            znnpu = zxnpu*zxnpu + zynpu*zynpu

            ! north pole direction & modulous (at v-point)
            zlam = glamv(ji,jj,1)
            zphi = gphiv(ji,jj,1)
            zxnpv = 0. - 2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            zynpv = 0. - 2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            znnpv = zxnpv*zxnpv + zynpv*zynpv

            ! north pole direction & modulous (at f-point)
            zlam = glamf(ji,jj,1)
            zphi = gphif(ji,jj,1)
            zxnpf = 0. - 2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            zynpf = 0. - 2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )
            znnpf = zxnpf*zxnpf + zynpf*zynpf

            ! j-direction: v-point segment direction (around t-point)
            zlam = glamv(ji,jj,1  )
            zphi = gphiv(ji,jj,1  )
            zlan = glamv(ji,jj-1,1)
            zphh = gphiv(ji,jj-1,1)
            zxvvt =  2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )   &
               &  -  2. * COS( rad*zlan ) * TAN( rpi/4. - rad*zphh/2. )
            zyvvt =  2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )   &
               &  -  2. * SIN( rad*zlan ) * TAN( rpi/4. - rad*zphh/2. )
            znvvt = SQRT( znnpt * ( zxvvt*zxvvt + zyvvt*zyvvt )  )
            znvvt = MAX( znvvt, 1.e-14 )

            ! j-direction: f-point segment direction (around u-point)
            zlam = glamf(ji,jj,1  )
            zphi = gphif(ji,jj,1  )
            zlan = glamf(ji,jj-1,1)
            zphh = gphif(ji,jj-1,1)
            zxffu =  2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )   &
               &  -  2. * COS( rad*zlan ) * TAN( rpi/4. - rad*zphh/2. )
            zyffu =  2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )   &
               &  -  2. * SIN( rad*zlan ) * TAN( rpi/4. - rad*zphh/2. )
            znffu = SQRT( znnpu * ( zxffu*zxffu + zyffu*zyffu )  )
            znffu = MAX( znffu, 1.e-14 )

            ! i-direction: f-point segment direction (around v-point)
            zlam = glamf(ji  ,jj,1)
            zphi = gphif(ji  ,jj,1)
            zlan = glamf(ji-1,jj,1)
            zphh = gphif(ji-1,jj,1)
            zxffv =  2. * COS( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )   &
               &  -  2. * COS( rad*zlan ) * TAN( rpi/4. - rad*zphh/2. )
            zyffv =  2. * SIN( rad*zlam ) * TAN( rpi/4. - rad*zphi/2. )   &
               &  -  2. * SIN( rad*zlan ) * TAN( rpi/4. - rad*zphh/2. )
            znffv = SQRT( znnpv * ( zxffv*zxffv + zyffv*zyffv )  )
            znffv = MAX( znffv, 1.e-14 )

            ! j-direction: u-point segment direction (around f-point)
            zlam = glamu(ji,jj+1,1)
            zphi = gphiu(ji,jj+1,1)
            zlan = glamu(ji,jj,1  )
            zphh = gphiu(ji,jj,1  )
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
      DO jj = 2, npy-1
         DO ji = 2, npx   ! vector opt.
            IF( MOD( ABS( glamv(ji,jj,1) - glamv(ji,jj-1,1) ), 360. ) < 1.e-8 ) THEN
               gsint(ji,jj) = 0.
               gcost(ji,jj) = 1.
            ENDIF
            IF( MOD( ABS( glamf(ji,jj,1) - glamf(ji,jj-1,1) ), 360. ) < 1.e-8 ) THEN
               gsinu(ji,jj) = 0.
               gcosu(ji,jj) = 1.
            ENDIF
            IF(      ABS( gphif(ji,jj,1) - gphif(ji-1,jj,1) )         < 1.e-8 ) THEN
               gsinv(ji,jj) = 0.
               gcosv(ji,jj) = 1.
            ENDIF
            IF( MOD( ABS( glamu(ji,jj,1) - glamu(ji,jj+1,1) ), 360. ) < 1.e-8 ) THEN
               gsinf(ji,jj) = 0.
               gcosf(ji,jj) = 1.
            ENDIF
         END DO
      END DO

  ALLOCATE( pxout(npx,npy,npz,npt), pyout(npx,npy,npz,npt) )

  DO jk = 1, npz
    DO jt = 1, npt
      pxout(:,:,jk,jt)= pxin(:,:,jk,jt) * gcosu(:,:) - pyin(:,:,jk,jt) * gsinu(:,:)
      pyout(:,:,jk,jt)= pyin(:,:,jk,jt) * gcosv(:,:) + pxin(:,:,jk,jt) * gsinv(:,:)
    END DO
  END DO

!  istatus=NF90_REDEF(ncidu)

!  istatus=NF90_DEF_VAR(ncidu,'gcosu',NF90_FLOAT,(/id_x,id_y/),idvarout1 ); PRINT *,'Def var :',NF90_STRERROR(istatus)
!  istatus=NF90_DEF_VAR(ncidu,'gcosv',NF90_FLOAT,(/id_x,id_y/),idvarout2 ); PRINT *,'Def var :',NF90_STRERROR(istatus)
!  istatus=NF90_DEF_VAR(ncidu,'gsinu',NF90_FLOAT,(/id_x,id_y/),idvarout3 ); PRINT *,'Def var :',NF90_STRERROR(istatus)
!  istatus=NF90_DEF_VAR(ncidu,'gsinv',NF90_FLOAT,(/id_x,id_y/),idvarout4 ); PRINT *,'Def var :',NF90_STRERROR(istatus)
!
!  istatus=NF90_ENDDEF(ncidu)
!
!  istatus=NF90_PUT_VAR(ncidu,idvarout1,gcosu); PRINT *,'Put var :',NF90_STRERROR(istatus)
!  istatus=NF90_PUT_VAR(ncidu,idvarout2,gcosv); PRINT *,'Put var :',NF90_STRERROR(istatus)
!  istatus=NF90_PUT_VAR(ncidu,idvarout3,gsinu); PRINT *,'Put var :',NF90_STRERROR(istatus)
!  istatus=NF90_PUT_VAR(ncidu,idvarout4,gsinv); PRINT *,'Put var :',NF90_STRERROR(istatus)

  istatus=NF90_PUT_VAR(ncidu,idxin,pxout); PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_CLOSE(ncidu)

  istatus=NF90_PUT_VAR(ncidv,idyin,pyout); PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_CLOSE(ncidv)

  DEALLOCATE( pxin, pyin, pxout, pyout)
  DEALLOCATE( gsint, gcost,   &
         &      gsinu, gcosu,   &
         &      gsinv, gcosv,   &
         &      gsinf, gcosf )
  DEALLOCATE( glamt, glamu, glamv,glamf )
  DEALLOCATE( gphit, gphiu, gphiv,gphif )
 

END PROGRAM dir_geo

