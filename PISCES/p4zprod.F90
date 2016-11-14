#include "cppdefs.h"

MODULE p4zprod
   !!======================================================================
   !!                         ***  MODULE p4zprod  ***
   !! TOP :   PISCES 
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!----------------------------------------------------------------------
#if defined key_pisces
   !!----------------------------------------------------------------------
   !!   'key_pisces'                                       PISCES bio-model
   !!----------------------------------------------------------------------
   !!   p4z_prod       :  
   !!----------------------------------------------------------------------
   USE sms_pisces      ! 
   USE p4zopt
   USE p4zint
   USE p4zlim

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_prod    ! called in p4zbio.F90
   PUBLIC   p4z_prod_alloc    ! called in p4zbio.F90
   PUBLIC   p4z_prod_nam    ! called in p4zbio.F90

   !!* Substitution
#  include "ocean2pisces.h90"
#  include "top_substitute.h90"


   !! * Shared module variables
   REAL(wp), PUBLIC ::   &
     pislope   = 3.0          ,  &  !:
     pislope2  = 3.0          ,  &  !:
     excret    = 10.e-5       , &   !:
     excret2   = 0.05         , &   !:
     chlcnm    = 0.033        , &   !:
     chlcdm    = 0.05         , &   !:
     fecnm     = 10.E-6       , &   !:
     fecdm     = 15.E-6       , &   !:
     grosip    = 0.151

   REAL(wp), PUBLIC, DIMENSION(:,:,:), ALLOCATABLE, SAVE  ::        &
     &                   prmax
   
   REAL(wp) ::   &
      texcret                    ,  &  !: 1 - excret 
      texcret2                   ,  &  !: 1 - excret2        
      rpis180                    ,  &  !: rpi / 180
      tpp                              !: Total primary production

   !!----------------------------------------------------------------------
   !! NEMO/TOP 2.0 , LOCEAN-IPSL (2007) 
   !! $Id: p4zprod.F90 2504 2010-12-23 08:18:45Z cetlod $ 
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE p4z_prod( kt , jnt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_prod  ***
      !!
      !! ** Purpose :   Compute the phytoplankton production depending on
      !!              light, temperature and nutrient availability
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) :: kt, jnt
      INTEGER  ::   ji, jj, jk
      REAL(wp) ::   zsilfac, zfact, zmsk
      REAL(wp) ::   zprdiachl, zprbiochl, zsilim, ztn, zadap, zadap2
      REAL(wp) ::   zlim, zsilfac2, zsiborn, zprod, zetot2, zmax, zproreg, zproreg2
      REAL(wp) ::   zmxltst, zmxlday, zlim1
      REAL(wp) ::   zpislopen  , zpislope2n
      REAL(wp) ::   zrum, zcodel, zargu, zvol
#if defined key_trc_diaadd
      REAL(wp) ::   zrfact2
#if defined key_iomput 
      REAL(wp), DIMENSION(PRIV_2D_BIOARRAY) ::   zw2d
#endif
#endif
      REAL(wp), DIMENSION(PRIV_2D_BIOARRAY)     ::   zmixnano   , zmixdiat, zstrn
      REAL(wp), DIMENSION(PRIV_3D_BIOARRAY) ::   zpislopead , zpislopead2
      REAL(wp), DIMENSION(PRIV_3D_BIOARRAY) ::   zprdia     , zprbio, zysopt
      REAL(wp), DIMENSION(PRIV_3D_BIOARRAY) ::   zprorca    , zprorcad, zprofed
      REAL(wp), DIMENSION(PRIV_3D_BIOARRAY) ::   zprofen   , zprochln, zprochld
      REAL(wp), DIMENSION(PRIV_3D_BIOARRAY) ::   zpronew    , zpronewd
      CHARACTER (len=25) :: charout
      !!---------------------------------------------------------------------


      zprorca (:,:,:) = 0.0
      zprorcad(:,:,:) = 0.0
      zprofed(:,:,:) = 0.0
      zprofen(:,:,:) = 0.0
      zprochln(:,:,:) = 0.0
      zprochld(:,:,:) = 0.0
      zpronew (:,:,:) = 0.0
      zpronewd(:,:,:) = 0.0
      zprdia  (:,:,:) = 0.0
      zprbio  (:,:,:) = 0.0
      zysopt  (:,:,:) = 0.0

      ! Computation of the optimal production

# if defined key_off_degrad
      prmax(:,:,:) = 0.6 / rday * tgfunc(:,:,:) * facvol(:,:,:)
# else
      prmax(:,:,:) = 0.6 / rday * tgfunc(:,:,:)
# endif

      ! compute the day length depending on latitude and the day
!      IF(lwp) write(numout,*)
!      IF(lwp) write(numout,*) 'p4zday : - Julian day ', nday_year
!      IF(lwp) write(numout,*) '~~~~~~'

!      IF( nleapy == 1 .AND. MOD( nyear, 4 ) == 0 ) THEN
!         zrum = FLOAT( nday_year - 80 ) / 366.
!      ELSE
!         zrum = FLOAT( nday_year - 80 ) / 365.
!      ENDIF
      zrum = FLOAT( nday_year - 80 ) / nyear_len
      zcodel = ASIN(  SIN( zrum * rpi * 2. ) * SIN( rpi * 23.5 / 180.)  )

      ! day length in hours
      zstrn(:,:) = 0.
      DO jj = JRANGE
         DO ji = IRANGE
            zargu = TAN( zcodel ) * TAN( gphit(ji,jj) * rpi / 180. )
            zargu = MAX( -1., MIN(  1., zargu ) )
            zstrn(ji,jj) = MAX( 0.0, 24. - 2. * ACOS( zargu ) * 180 / rpi / 15. )
         END DO
      END DO


!CDIR NOVERRCHK
      DO jk = KRANGE
!CDIR NOVERRCHK
         DO jj = JRANGE
!CDIR NOVERRCHK
            DO ji = IRANGE

               ! Computation of the P-I slope for nanos and diatoms
               IF( etot(ji,jj,jk) > 1.E-3 ) THEN
                   ztn    = MAX( 0., tsn(ji,jj,K,jp_tem) - 15. )
                   zadap  = 0.+ 1.* ztn / ( 2.+ ztn )
                   zadap2 = 0.e0

                   zfact  = EXP( -0.21 * emoy(ji,jj,jk) )

                   zpislopead (ji,jj,jk) = pislope  * ( 1.+ zadap  * zfact )
                   zpislopead2(ji,jj,jk) = pislope2 * ( 1.+ zadap2 * zfact )

                   zpislopen = zpislopead(ji,jj,jk) * trn(ji,jj,K,jpnch)                 &
                     &         / ( trn(ji,jj,K,jpphy) * 12.                   + rtrn )   &
                     &         / ( prmax(ji,jj,jk) * rday * xlimphy(ji,jj,jk) + rtrn )

                   zpislope2n = zpislopead2(ji,jj,jk) * trn(ji,jj,K,jpdch)                &
                     &          / ( trn(ji,jj,K,jpdia) * 12.                   + rtrn )   &
                     &          / ( prmax(ji,jj,jk) * rday * xlimdia(ji,jj,jk) + rtrn )

                   ! Computation of production function
                   zprbio(ji,jj,jk) = prmax(ji,jj,jk) * &
                     &                (  1.- EXP( -zpislopen * enano(ji,jj,jk) )  )
                   zprdia(ji,jj,jk) = prmax(ji,jj,jk) * &
                     &                (  1.- EXP( -zpislope2n * ediat(ji,jj,jk) )  )
               ENDIF
            END DO
         END DO
      END DO


      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE

                IF( etot(ji,jj,jk) > 1.E-3 ) THEN
                   !    Si/C of diatoms
                   !    ------------------------
                   !    Si/C increases with iron stress and silicate availability
                   !    Si/C is arbitrariliy increased for very high Si concentrations
                   !    to mimic the very high ratios observed in the Southern Ocean (silpot2)

                  zlim1  = trn(ji,jj,K,jpsil) / ( trn(ji,jj,K,jpsil) + xksi1 )
                  zlim   = xdiatno3(ji,jj,jk) + xdiatnh4(ji,jj,jk)

                  zsilim = MIN( zprdia(ji,jj,jk)    / ( rtrn + prmax(ji,jj,jk) ),                 &
                  &          trn(ji,jj,K,jpfer) / ( concdfe(ji,jj,jk) &
                  &         + trn(ji,jj,K,jpfer) ),   &
                  &          trn(ji,jj,K,jppo4) / ( concdnh4   &
                  &            + trn(ji,jj,K,jppo4) ),            &
                  &          zlim )
                  zsilfac = 5.4 * EXP( -4.23 * zsilim ) * MAX( 0.e0, MIN( 1., 2.2 * ( zlim1 - 0.5 ) )  ) + 1.e0
                  zsiborn = MAX( 0.e0, ( trn(ji,jj,K,jpsil) - 15.e-6 ) )
                  zsilfac2 = 1.+ 3.* zsiborn / ( zsiborn + xksi2 )
                  zsilfac = MIN( 6.4,zsilfac * zsilfac2)
                  zysopt(ji,jj,jk) = grosip * zlim1 * zsilfac

              ENDIF
            END DO
         END DO
      END DO

      !  Computation of the limitation term due to
      !  A mixed layer deeper than the euphotic depth
      DO jj = JRANGE
         DO ji = IRANGE
            zmxltst = MAX( 0.e0, hmld(ji,jj) - heup(ji,jj) )
            zmxlday = zmxltst**2 / rday
            zmixnano(ji,jj) = 1.- zmxlday / ( 1.+ zmxlday )
            zmixdiat(ji,jj) = 1.- zmxlday / ( 3.+ zmxlday )
         END DO
      END DO
 
      !  Mixed-layer effect on production                                                                               
      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               IF( fsdepw(ji,jj,jk) <= hmld(ji,jj) ) THEN
                  zprbio(ji,jj,jk) = zprbio(ji,jj,jk) * zmixnano(ji,jj)
                  zprdia(ji,jj,jk) = zprdia(ji,jj,jk) * zmixdiat(ji,jj)
               ENDIF
            END DO
         END DO
      END DO


      WHERE( zstrn(:,:) < 1.e0 ) zstrn(:,:) = 24.
      zstrn(:,:) = 24. / zstrn(:,:)

!CDIR NOVERRCHK
      DO jk = KRANGE
!CDIR NOVERRCHK
         DO jj = JRANGE
!CDIR NOVERRCHK
            DO ji = IRANGE

               IF( etot(ji,jj,jk) > 1.E-3 ) THEN
                  !     Computation of the various production terms for nanophyto.
                  zetot2 = enano(ji,jj,jk) * zstrn(ji,jj)
                  zmax = MAX( 0.1, xlimphy(ji,jj,jk) )
                  zpislopen = zpislopead(ji,jj,jk)          &
                  &         * trn(ji,jj,K,jpnch)   &
                  &        / ( rtrn + trn(ji,jj,K,jpphy) * 12.)         &
                  &         / ( prmax(ji,jj,jk) * rday * zmax + rtrn )

                  zprbiochl = prmax(ji,jj,jk) * (  1.- EXP( -zpislopen * zetot2 )  )

                  zprorca(ji,jj,jk) = zprbio(ji,jj,jk)  * xlimphy(ji,jj,jk) * trn(ji,jj,K,jpphy) * rfact2

                  zpronew(ji,jj,jk) = zprorca(ji,jj,jk) * xnanono3(ji,jj,jk)    &
                  &             / ( xnanono3(ji,jj,jk) + xnanonh4(ji,jj,jk) + rtrn )
                  zprod = rday * zprorca(ji,jj,jk) * zprbiochl * trn(ji,jj,K,jpphy) *zmax

                  zprofen(ji,jj,jk) = (fecnm)**2 * zprod / chlcnm            &
                  &              / (  zpislopead(ji,jj,jk) * zetot2 * trn(ji,jj,K,jpnfe) + rtrn  )

                  zprochln(ji,jj,jk) = chlcnm * 144. * zprod                  &
                  &              / (  zpislopead(ji,jj,jk) * zetot2 * trn(ji,jj,K,jpnch) + rtrn  )
               ENDIF
            END DO
         END DO
      END DO

!CDIR NOVERRCHK
      DO jk = KRANGE
!CDIR NOVERRCHK
         DO jj = JRANGE
!CDIR NOVERRCHK
            DO ji = IRANGE
               IF( etot(ji,jj,jk) > 1.E-3 ) THEN
                  !  Computation of the various production terms for diatoms
                  zetot2 = ediat(ji,jj,jk) * zstrn(ji,jj)
                  zmax = MAX( 0.1, xlimdia(ji,jj,jk) )
                  zpislope2n = zpislopead2(ji,jj,jk) * trn(ji,jj,K,jpdch)        &
                  &           / ( rtrn + trn(ji,jj,K,jpdia) * 12.)        &
                  &           / ( prmax(ji,jj,jk) * rday * zmax + rtrn )

                  zprdiachl = prmax(ji,jj,jk) * (  1.- EXP( -zetot2 * zpislope2n )  )

                  zprorcad(ji,jj,jk) = zprdia(ji,jj,jk) * xlimdia(ji,jj,jk) * trn(ji,jj,K,jpdia) * rfact2

                  zpronewd(ji,jj,jk) = zprorcad(ji,jj,jk) * xdiatno3(ji,jj,jk)     &
                  &              / ( xdiatno3(ji,jj,jk) + xdiatnh4(ji,jj,jk) + rtrn )

                  zprod = rday * zprorcad(ji,jj,jk) * zprdiachl * trn(ji,jj,K,jpdia) * zmax

                  zprofed(ji,jj,jk) = (fecdm)**2 * zprod / chlcdm                   &
                  &              / ( zpislopead2(ji,jj,jk) * zetot2 * trn(ji,jj,K,jpdfe) + rtrn )

                  zprochld(ji,jj,jk) = chlcdm * 144. * zprod       &
                  &              / ( zpislopead2(ji,jj,jk) * zetot2 * trn(ji,jj,K,jpdch) + rtrn )

               ENDIF
            END DO
         END DO
      END DO
      !

      !   Update the arrays TRA which contain the biological sources and sinks
      DO jk = KRANGE
         DO jj = JRANGE
           DO ji = IRANGE
              zproreg  = zprorca(ji,jj,jk) - zpronew(ji,jj,jk)
              zproreg2 = zprorcad(ji,jj,jk) - zpronewd(ji,jj,jk)
              tra(ji,jj,jk,jppo4) = tra(ji,jj,jk,jppo4) - zprorca(ji,jj,jk) - zprorcad(ji,jj,jk)
              tra(ji,jj,jk,jpno3) = tra(ji,jj,jk,jpno3) - zpronew(ji,jj,jk) - zpronewd(ji,jj,jk)
              tra(ji,jj,jk,jpnh4) = tra(ji,jj,jk,jpnh4) - zproreg - zproreg2
              tra(ji,jj,jk,jpphy) = tra(ji,jj,jk,jpphy) + zprorca(ji,jj,jk) * texcret
              tra(ji,jj,jk,jpnch) = tra(ji,jj,jk,jpnch) + zprochln(ji,jj,jk) * texcret
              tra(ji,jj,jk,jpnfe) = tra(ji,jj,jk,jpnfe) + zprofen(ji,jj,jk) * texcret
              tra(ji,jj,jk,jpdia) = tra(ji,jj,jk,jpdia) + zprorcad(ji,jj,jk) * texcret2
              tra(ji,jj,jk,jpdch) = tra(ji,jj,jk,jpdch) + zprochld(ji,jj,jk) * texcret2
              tra(ji,jj,jk,jpdfe) = tra(ji,jj,jk,jpdfe) + zprofed(ji,jj,jk) * texcret2
              tra(ji,jj,jk,jpbsi) = tra(ji,jj,jk,jpbsi) + zprorcad(ji,jj,jk) * zysopt(ji,jj,jk) * texcret2
              tra(ji,jj,jk,jpdoc) = tra(ji,jj,jk,jpdoc) + &
              &                     excret2 * zprorcad(ji,jj,jk) + excret * zprorca(ji,jj,jk)
              tra(ji,jj,jk,jpoxy) = tra(ji,jj,jk,jpoxy) + o2ut * ( zproreg + zproreg2) &
              &                    + ( o2ut + o2nit ) * ( zpronew(ji,jj,jk) + zpronewd(ji,jj,jk) )
              tra(ji,jj,jk,jpfer) = tra(ji,jj,jk,jpfer) &
              &                     - texcret * zprofen(ji,jj,jk) - texcret2 * zprofed(ji,jj,jk)
              tra(ji,jj,jk,jpsil) = tra(ji,jj,jk,jpsil) &
              &                     - texcret2 * zprorcad(ji,jj,jk) * zysopt(ji,jj,jk)
              tra(ji,jj,jk,jpdic) = tra(ji,jj,jk,jpdic) - zprorca(ji,jj,jk) - zprorcad(ji,jj,jk)
              tra(ji,jj,jk,jptal) = tra(ji,jj,jk,jptal) &
              &                    + rno3 * ( zpronew(ji,jj,jk) + zpronewd(ji,jj,jk) )
          END DO
        END DO
     END DO

     ! Total primary production per year
     DO jk = KRANGE
        DO jj = JRANGE
          DO ji = IRANGE
             tpp  = tpp + ( zprorca(ji,jj,jk)   &
                  &       + zprorcad(ji,jj,jk) ) * cvol(ji,jj,jk)
          END DO
        END DO
      END DO

     IF( kt == nitend .AND. jnt == nrdttrc ) THEN
        IF( lk_mpp ) CALL mpp_sum( tpp )
        WRITE(numout,*) 'Total PP (Gtc) :'
        WRITE(numout,*) '-------------------- : ',tpp * 12. / 1.E12
        WRITE(numout,*)
      ENDIF

#if defined key_trc_diaadd 
      !   Supplementary diagnostics
     zrfact2 = 1.e3 * rfact2r
     DO jk = KRANGE
        DO jj = JRANGE
          DO ji = IRANGE
             zmsk = zrfact2 * tmask(ji,jj,K)
             trc3d(ji,jj,K,jp_pphy  )  = zprorca (ji,jj,jk) * zmsk  ! primary production by nanophyto
             trc3d(ji,jj,K,jp_pphy2 )  = zprorcad(ji,jj,jk) * zmsk  ! primary production by diatom
             trc3d(ji,jj,K,jp_pnew  )  = zpronew (ji,jj,jk) * zmsk ! new primary production by nanophyto
             trc3d(ji,jj,K,jp_pnew2 )  = zpronewd(ji,jj,jk) * zmsk ! new primary production by diatom
             trc3d(ji,jj,K,jp_pbsi  )  = zprorcad(ji,jj,jk) * zysopt(ji,jj,jk) * zmsk ! biogenic silica production
             trc3d(ji,jj,K,jp_pfed  )  = zprofed (ji,jj,jk) * zmsk  ! biogenic iron production by diatom
             trc3d(ji,jj,K,jp_pfen  )  = zprofen (ji,jj,jk) * zmsk!  biogenic iron production by nanophyto
             trc3d(ji,jj,K,jp_pnewo2)  = ( o2ut + o2nit ) &  ! Oxygen production by the New Produc.
                &                      * ( zpronew(ji,jj,jk) + zpronewd(ji,jj,jk) ) * zmsk
             trc3d(ji,jj,K,jp_prego2)  = o2ut * &        ! Oxygen production by the Regen Produc.
                &                 (   zprorca (ji,jj,jk) - zpronew (ji,jj,jk)  &
                &                  + zprorcad(ji,jj,jk) - zpronewd(ji,jj,jk) ) * zmsk
         END DO
        END DO
      END DO
#endif

       IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('prod')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc( charout, ltra='tra')
!         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
       ENDIF

   END SUBROUTINE p4z_prod

   SUBROUTINE p4z_prod_nam

      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_prod_init  ***
      !!
      !! ** Purpose :   Initialization of phytoplankton production parameters
      !!
      !! ** Method  :   Read the nampisprod namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist nampisprod
      !!
      !!----------------------------------------------------------------------

      NAMELIST/nampisprod/ pislope, pislope2, excret, excret2, chlcnm, chlcdm,   &
         &              fecnm, fecdm, grosip

      REWIND( numnatp )                     ! read numnatp
      READ  ( numnatp, nampisprod )

      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' '
         WRITE(numout,*) ' Namelist parameters for phytoplankton growth, nampisprod'
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '    mean Si/C ratio                           grosip    =', grosip
         WRITE(numout,*) '    P-I slope                                 pislope   =', pislope
         WRITE(numout,*) '    excretion ratio of nanophytoplankton      excret    =', excret
         WRITE(numout,*) '    excretion ratio of diatoms                excret2   =', excret2
         WRITE(numout,*) '    P-I slope  for diatoms                    pislope2  =', pislope2
         WRITE(numout,*) '    Minimum Chl/C in nanophytoplankton        chlcnm    =', chlcnm
         WRITE(numout,*) '    Minimum Chl/C in diatoms                  chlcdm    =', chlcdm
         WRITE(numout,*) '    Maximum Fe/C in nanophytoplankton         fecnm     =', fecnm
         WRITE(numout,*) '    Minimum Fe/C in diatoms                   fecdm     =', fecdm
      ENDIF

      rpis180   = rpi / 180.
      texcret   = 1.0 - excret
      texcret2  = 1.0 - excret2
      tpp       = 0.


   END SUBROUTINE p4z_prod_nam

   INTEGER FUNCTION p4z_prod_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_prod_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( prmax(PRIV_3D_BIOARRAY), STAT = p4z_prod_alloc )
      !
      IF( p4z_prod_alloc /= 0 ) CALL ctl_warn('p4z_prod_alloc : failed to allocate arrays.')
      !
   END FUNCTION p4z_prod_alloc


#else
   !!======================================================================
   !!  Dummy module :                                   No PISCES bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE p4z_prod                    ! Empty routine
   END SUBROUTINE p4z_prod
#endif 

   !!======================================================================
END MODULE  p4zprod
