#include "cppdefs.h"

MODULE p4zmeso
   !!======================================================================
   !!                         ***  MODULE p4zmeso  ***
   !! TOP :   PISCES Compute the sources/sinks for mesozooplankton
   !!======================================================================
   !! History :   1.0  !  2002     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!----------------------------------------------------------------------
#if defined key_pisces
   !!----------------------------------------------------------------------
   !!   'key_pisces'                                       PISCES bio-model
   !!----------------------------------------------------------------------
   !!   p4z_meso       :   Compute the sources/sinks for mesozooplankton
   !!   p4z_meso_init  :   Initialization of the parameters for mesozooplankton
   !!----------------------------------------------------------------------
   USE sms_pisces      ! 
   USE p4zint
   USE p4zsink

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_meso         ! called in p4zbio.F90
   PUBLIC   p4z_meso_nam         ! called in p4zbio.F90


   !!* Substitution
#  include "ocean2pisces.h90"
#  include "top_substitute.h90"


   !! * Shared module variables
   REAL(wp), PUBLIC ::   &
      xprefc   = 1.0     ,  &  !: 
      xprefp   = 0.2     ,  &  !:
      xprefz   = 1.0     ,  &  !:
      xprefpoc = 0.0     ,  &  !:
      resrat2  = 0.005   ,  &  !:
      mzrat2   = 0.03    ,  &  !:
      grazrat2 = 0.7     ,  &  !:
      xkgraz2  = 20E-6   ,  &  !:
      unass2   = 0.3     ,  &  !:
      sigma2   = 0.6     ,  &  !:
      epsher2  = 0.33    ,  &  !:   
      grazflux = 5.E3 


   !!----------------------------------------------------------------------
   !! NEMO/TOP 2.0 , LOCEAN-IPSL (2007) 
   !! $Id: p4zmeso.F90 1830 2010-04-12 13:03:51Z cetlod $ 
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE p4z_meso( kt,jnt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_meso  ***
      !!
      !! ** Purpose :   Compute the sources/sinks for mesozooplankton
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, jnt ! ocean time step
      INTEGER  :: ji, jj, jk
      REAL(wp) :: zcompadi, zcompaph, zcompapoc, zcompaz
      REAL(wp) :: zfact, zstep, zcompam, zdenom, zgraze2
      REAL(wp) :: zgrarem2, zgrafer2, zgrapoc2, zprcaca, zmortz2
#if defined key_kriest
      REAL znumpoc
#endif
      REAL(wp),DIMENSION(PRIV_3D_BIOARRAY) :: zrespz2,ztortz2,zgrazd,zgrazz,zgrazpof
      REAL(wp),DIMENSION(PRIV_3D_BIOARRAY) :: zgrazn,zgrazpoc,zgraznf,zgrazf
      REAL(wp),DIMENSION(PRIV_3D_BIOARRAY) :: zgrazfff,zgrazffe
      CHARACTER (len=25) :: charout
      REAL(wp) :: zrfact2

      !!---------------------------------------------------------------------


      zrespz2 (:,:,:) = 0.
      ztortz2 (:,:,:) = 0.
      zgrazd  (:,:,:) = 0.
      zgrazz  (:,:,:) = 0.
      zgrazpof(:,:,:) = 0.
      zgrazn  (:,:,:) = 0.
      zgrazpoc(:,:,:) = 0.
      zgraznf (:,:,:) = 0.
      zgrazf  (:,:,:) = 0.
      zgrazfff(:,:,:) = 0.
      zgrazffe(:,:,:) = 0.

      zstep = rfact2 / rday      ! Time step duration for biology

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE

               zcompam = MAX( ( trn(ji,jj,K,jpmes) - 1.e-9 ), 0.e0 )
# if defined key_off_degrad
               zfact   = zstep * tgfunc(ji,jj,jk) * zcompam * facvol(ji,jj,jk)
# else
               zfact   = zstep * tgfunc(ji,jj,jk) * zcompam
# endif

!     Respiration rates of both zooplankton
!     -------------------------------------
               zrespz2(ji,jj,jk)  = resrat2 * zfact * ( 1. + 3. * nitrfac(ji,jj,jk) )        &
                  &     * trn(ji,jj,K,jpmes) / ( xkmort + trn(ji,jj,K,jpmes) )

!     Zooplankton mortality. A square function has been selected with
!     no real reason except that it seems to be more stable and may
!     mimic predation.
!     ---------------------------------------------------------------
               ztortz2(ji,jj,jk) = mzrat2 * 1.e6 * zfact * trn(ji,jj,K,jpmes)
               !
            END DO
         END DO
      END DO


      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               zcompadi  = MAX( ( trn(ji,jj,K,jpdia) - 1.e-8 ), 0.e0 )
               zcompaz   = MAX( ( trn(ji,jj,K,jpzoo) - 1.e-8 ), 0.e0 )
               zcompaph  = MAX( ( trn(ji,jj,K,jpphy) - 2.e-7 ), 0.e0 )
               zcompapoc = MAX( ( trn(ji,jj,K,jppoc) - 1.e-8 ), 0.e0 )

!     Microzooplankton grazing
!     ------------------------
               zdenom = 1. / (  xkgraz2 + xprefc   * trn(ji,jj,K,jpdia)   &
                  &                     + xprefz   * trn(ji,jj,K,jpzoo)   &
                  &                     + xprefp   * trn(ji,jj,K,jpphy)   &
                  &                     + xprefpoc * trn(ji,jj,K,jppoc)  )

               zgraze2 = grazrat2 * zstep * Tgfunc2(ji,jj,jk) * zdenom    &
# if defined key_off_degrad
                  &     * facvol(ji,jj,jk)          &
# endif
                  &     * trn(ji,jj,K,jpmes)

               zgrazd(ji,jj,jk)   = zgraze2 * xprefc   * zcompadi
               zgrazz(ji,jj,jk)   = zgraze2 * xprefz   * zcompaz
               zgrazn(ji,jj,jk)   = zgraze2 * xprefp   * zcompaph
               zgrazpoc(ji,jj,jk) = zgraze2 * xprefpoc * zcompapoc

               zgraznf(ji,jj,jk)  = zgrazn(ji,jj,jk)   * trn(ji,jj,K,jpnfe) &
                  &                                     / (trn(ji,jj,K,jpphy) + rtrn)
               zgrazf(ji,jj,jk)   = zgrazd(ji,jj,jk)   * trn(ji,jj,K,jpdfe) &
                  &                                    / (trn(ji,jj,K,jpdia) + rtrn)
               zgrazpof(ji,jj,jk) = zgrazpoc(ji,jj,jk) * trn(ji,jj,K,jpsfe) &
                  &                                   / (trn(ji,jj,K,jppoc) + rtrn)
            END DO
         END DO
      END DO
      
      
      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               
!    Mesozooplankton flux feeding on GOC
!    ----------------------------------
# if ! defined key_kriest
#   if ! defined key_off_degrad
               zgrazffe(ji,jj,jk) = grazflux * zstep * wsbio4(ji,jj,jk)   &
                  &                 * tgfunc2(ji,jj,jk)                  &
                  &                 * trn(ji,jj,K,jpgoc)                &
                  &                 * trn(ji,jj,K,jpmes)
#   else
               zgrazffe(ji,jj,jk) = grazflux * zstep * wsbio4(ji,jj,jk) * facvol(ji,jj,jk)   &
                  &                 * tgfunc2(ji,jj,jk)     &
                  &                 * trn(ji,jj,K,jpgoc)   &
                  &                 * trn(ji,jj,K,jpmes)    
#  endif
               zgrazfff(ji,jj,jk) = zgrazffe(ji,jj,jk)       &
                  &                 * trn(ji,jj,K,jpbfe)   &
                  &                / (trn(ji,jj,K,jpgoc) + rtrn)
# else
!!--------------------------- KRIEST3 -------------------------------------------
!!               zgrazffe(ji,jj,jk) = 0.5 * 1.3e-2 / 5.5e-7 * 0.3 * zstep * wsbio3(ji,jj,jk)     &
!!                  &     * tgfunc(ji,jj,jk) * trn(ji,jj,K,jppoc) * trn(ji,jj,K,jpmes)    &
#  if defined key_off_degrad
!!                  &     * facvol(ji,jj,jk)          &
#  endif
!!                  &     /  (trn(ji,jj,K,jppoc) * 1.e7 + 0.1)
!!--------------------------- KRIEST3 -------------------------------------------

#  if ! defined key_off_degrad
              zgrazffe(ji,jj,jk) = grazflux * zstep * wsbio3(ji,jj,jk)     &
                  &                * tgfunc2(ji,jj,jk) * trn(ji,jj,K,jppoc) * trn(ji,jj,K,jpmes)
#  else
              zgrazffe(ji,jj,jk) = grazflux * zstep * wsbio3(ji,jj,jk) * facvol(ji,jj,jk)    &
                  &               * tgfunc2(ji,jj,jk) * trn(ji,jj,K,jppoc) * trn(ji,jj,K,jpmes)
#  endif

               zgrazfff(ji,jj,jk) = zgrazffe(ji,jj,jk)      &
                  &                * trn(ji,jj,K,jpsfe) / (trn(ji,jj,K,jppoc) + rtrn)
# endif
            END DO
         END DO
      END DO
      
#if defined key_trc_diaad
      ! Total grazing ( grazing by microzoo is already computed in p4zmicro ) 
      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               grazing(ji,jj,jk) = grazing(ji,jj,jk) + (  zgrazd  (ji,jj,jk) + zgrazz  (ji,jj,jk) + zgrazn(ji,jj,jk) &
                     &                                + zgrazpoc(ji,jj,jk) + zgrazffe(ji,jj,jk)  )
            END DO
         END DO
      END DO

#endif


      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE

!    Mesozooplankton efficiency
!    --------------------------
               zgrarem2 = ( zgrazd(ji,jj,jk) + zgrazz(ji,jj,jk) + zgrazn(ji,jj,jk) &
                  &     + zgrazpoc(ji,jj,jk) + zgrazffe(ji,jj,jk) )   &
                  &     * ( 1. - epsher2 - unass2 )
#if ! defined key_kriest
               zgrafer2 = (zgrazf(ji,jj,jk) + zgraznf(ji,jj,jk) + zgrazz(ji,jj,jk) &
                  &     * ferat3 + zgrazpof(ji,jj,jk) + zgrazfff (ji,jj,jk))*(1.-epsher2-unass2) &
                  &     + epsher2 * ( &
                  &      zgrazd(ji,jj,jk)   * MAX((trn(ji,jj,K,jpdfe) &
                  &       / (trn(ji,jj,K,jpdia) + rtrn)-ferat3),0.) &
                  &     + zgrazn(ji,jj,jk)   * MAX((trn(ji,jj,K,jpnfe) &
                  &      / (trn(ji,jj,K,jpphy) + rtrn)-ferat3),0.) &
                  &    + zgrazpoc(ji,jj,jk) * MAX((trn(ji,jj,K,jpsfe) &
                  &       / (trn(ji,jj,K,jppoc) + rtrn)-ferat3),0.) &
                  &    + zgrazffe(ji,jj,jk) * MAX((trn(ji,jj,K,jpbfe) &
                  &    / (trn(ji,jj,K,jpgoc) + rtrn)-ferat3),0.)  )
#else
               zgrafer2 = (zgrazf(ji,jj,jk) + zgraznf(ji,jj,jk) + zgrazz(ji,jj,jk) &
                  &    * ferat3 + zgrazpof(ji,jj,jk) + zgrazfff(ji,jj,jk) )*(1.-epsher2-unass2) &
                  &    + epsher2 * ( &
                  &    zgrazd(ji,jj,jk)   * MAX((trn(ji,jj,K,jpdfe) &
                  &            / (trn(ji,jj,K,jpdia) + rtrn)-ferat3),0.)   &
                  &    + zgrazn(ji,jj,jk)   * MAX((trn(ji,jj,K,jpnfe)       &
                  &            / (trn(ji,jj,K,jpphy) + rtrn)-ferat3),0.)   &
                  &    + zgrazpoc(ji,jj,jk) * MAX((trn(ji,jj,K,jpsfe)       & 
                  &            / (trn(ji,jj,K,jppoc) + rtrn)-ferat3),0.)   &
                  &    + zgrazffe(ji,jj,jk) * MAX((trn(ji,jj,K,jpsfe)       & 
                  &            / (trn(ji,jj,K,jppoc) + rtrn)-ferat3),0.)  )

#endif
               zgrapoc2 = (zgrazd(ji,jj,jk) + zgrazz(ji,jj,jk)  + zgrazn(ji,jj,jk) &
                  &    + zgrazpoc(ji,jj,jk) + zgrazffe(ji,jj,jk)) * unass2

               tra(ji,jj,jk,jppo4) = tra(ji,jj,jk,jppo4) + zgrarem2 * sigma2
               tra(ji,jj,jk,jpnh4) = tra(ji,jj,jk,jpnh4) + zgrarem2 * sigma2
               tra(ji,jj,jk,jpdoc) = tra(ji,jj,jk,jpdoc) + zgrarem2 * (1.-sigma2)
               tra(ji,jj,jk,jpoxy) = tra(ji,jj,jk,jpoxy) - o2ut * zgrarem2 * sigma2
               tra(ji,jj,jk,jpfer) = tra(ji,jj,jk,jpfer) + zgrafer2
               tra(ji,jj,jk,jpdic) = tra(ji,jj,jk,jpdic) + zgrarem2 * sigma2
               
#if defined key_kriest
               tra(ji,jj,jk,jppoc) = tra(ji,jj,jk,jppoc) + zgrapoc2
               tra(ji,jj,jk,jpnum) = tra(ji,jj,jk,jpnum) + zgrapoc2 * xkr_dmeso
#else
               tra(ji,jj,jk,jpgoc) = tra(ji,jj,jk,jpgoc) + zgrapoc2
#endif
            END DO
         END DO
      END DO

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               !
               !   Update the arrays TRA which contain the biological sources and sinks
               !   --------------------------------------------------------------------
               zmortz2 = ztortz2(ji,jj,jk) + zrespz2(ji,jj,jk)
               tra(ji,jj,jk,jpmes) = tra(ji,jj,jk,jpmes) - zmortz2  &
                  &    + epsher2 * ( zgrazd(ji,jj,jk) + zgrazz(ji,jj,jk) + zgrazn(ji,jj,jk) &
                  &    + zgrazpoc(ji,jj,jk) + zgrazffe(ji,jj,jk) )
               tra(ji,jj,jk,jpdia) = tra(ji,jj,jk,jpdia) - zgrazd(ji,jj,jk)
               tra(ji,jj,jk,jpzoo) = tra(ji,jj,jk,jpzoo) - zgrazz(ji,jj,jk)
               tra(ji,jj,jk,jpphy) = tra(ji,jj,jk,jpphy) - zgrazn(ji,jj,jk)
               tra(ji,jj,jk,jpnch) = tra(ji,jj,jk,jpnch) - zgrazn(ji,jj,jk)   &
                  &    * trn(ji,jj,K,jpnch)  &
                  &    / ( trn(ji,jj,K,jpphy) + rtrn )
               tra(ji,jj,jk,jpdch) = tra(ji,jj,jk,jpdch) - zgrazd(ji,jj,jk)    &
                  &    * trn(ji,jj,K,jpdch) &
                  &    / ( trn(ji,jj,K,jpdia) + rtrn )
               tra(ji,jj,jk,jpbsi) = tra(ji,jj,jk,jpbsi) - zgrazd(ji,jj,jk)    &
                  &    * trn(ji,jj,K,jpbsi) &
                  &    / ( trn(ji,jj,K,jpdia) + rtrn )
               tra(ji,jj,jk,jpdsi) = tra(ji,jj,jk,jpdsi) +  zgrazd(ji,jj,jk)   &
                  &    * trn(ji,jj,K,jpbsi) &
                  &    / ( trn(ji,jj,K,jpdia) + rtrn )
               tra(ji,jj,jk,jpnfe) = tra(ji,jj,jk,jpnfe) -  zgraznf(ji,jj,jk)
               tra(ji,jj,jk,jpdfe) = tra(ji,jj,jk,jpdfe) -  zgrazf(ji,jj,jk)

               zprcaca = xfracal(ji,jj,jk) * unass2 * zgrazn(ji,jj,jk)
#if defined key_trc_diaad
               prodcal(ji,jj,jk) = prodcal(ji,jj,jk) + zprcaca  ! prodcal=prodcal(nanophy)+prodcal(microzoo)+prodcal(mesozoo)
#endif
               zprcaca = part * zprcaca
               tra(ji,jj,jk,jpdic) = tra(ji,jj,jk,jpdic) - zprcaca
               tra(ji,jj,jk,jptal) = tra(ji,jj,jk,jptal) - 2. * zprcaca
               tra(ji,jj,jk,jpcal) = tra(ji,jj,jk,jpcal) + zprcaca
#if defined key_kriest
               znumpoc = trn(ji,jj,K,jpnum) / ( trn(ji,jj,K,jppoc) + rtrn )
               tra(ji,jj,jk,jppoc) = tra(ji,jj,jk,jppoc) + zmortz2  &
                  &    - zgrazpoc(ji,jj,jk) - zgrazffe(ji,jj,jk)    
               tra(ji,jj,jk,jpnum) = tra(ji,jj,jk,jpnum) - zgrazpoc(ji,jj,jk) * znumpoc &
                  &    + zmortz2  * xkr_dmeso &
                  &    - zgrazffe(ji,jj,jk)   * znumpoc * wsbio4(ji,jj,jk) &
                  &    / ( wsbio3(ji,jj,jk) + rtrn )
               tra(ji,jj,jk,jpsfe) = tra(ji,jj,jk,jpsfe) + ferat3 * zmortz2 &
               &       + unass2 * ( ferat3 * zgrazz(ji,jj,jk) + zgraznf(ji,jj,jk) &
               &       + zgrazf(ji,jj,jk) + zgrazpof(ji,jj,jk) + zgrazfff(ji,jj,jk) ) &
               &       - zgrazfff(ji,jj,jk) - zgrazpof(ji,jj,jk)
#else
               tra(ji,jj,jk,jppoc) = tra(ji,jj,jk,jppoc) - zgrazpoc(ji,jj,jk)
               tra(ji,jj,jk,jpgoc) = tra(ji,jj,jk,jpgoc) + zmortz2 - zgrazffe(ji,jj,jk)
               tra(ji,jj,jk,jpsfe) = tra(ji,jj,jk,jpsfe) - zgrazpof(ji,jj,jk)
               tra(ji,jj,jk,jpbfe) = tra(ji,jj,jk,jpbfe) + ferat3 * zmortz2 &
               &       + unass2 * ( ferat3 * zgrazz(ji,jj,jk) + zgraznf(ji,jj,jk) &
               &       + zgrazf(ji,jj,jk) + zgrazpof(ji,jj,jk) + zgrazfff(ji,jj,jk) ) &
               &       - zgrazfff(ji,jj,jk)
#endif

            END DO
         END DO
      END DO
      !
#if defined key_trc_diaadd
      zrfact2 = 1.e3 * rfact2r
      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               trc3d(ji,jj,K,jp_grazt) = grazing(ji,jj,jk) * zrfact2 * tmask(ji,jj,K) !  Total grazing of phyto by zoo
            END DO
         END DO
      END DO
#endif

       IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('meso')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc( charout, ltra='tra')
!         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
       ENDIF

   END SUBROUTINE p4z_meso

   SUBROUTINE p4z_meso_nam

      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_meso_init  ***
      !!
      !! ** Purpose :   Initialization of mesozooplankton parameters
      !!
      !! ** Method  :   Read the nampismes namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist nampismes
      !!
      !!----------------------------------------------------------------------

      NAMELIST/nampismes/ grazrat2,resrat2,mzrat2,xprefc, xprefp, &
         &             xprefz, xprefpoc, xkgraz2, epsher2, sigma2, unass2, grazflux

      REWIND( numnatp )                     ! read numnatp
      READ  ( numnatp, nampismes )


      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' ' 
         WRITE(numout,*) ' Namelist parameters for mesozooplankton, nampismes'
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '    zoo preference for phyto                  xprefc    =', xprefc
         WRITE(numout,*) '    zoo preference for POC                    xprefp    =', xprefp
         WRITE(numout,*) '    zoo preference for zoo                    xprefz    =', xprefz
         WRITE(numout,*) '    zoo preference for poc                    xprefpoc  =', xprefpoc
         WRITE(numout,*) '    exsudation rate of mesozooplankton        resrat2   =', resrat2
         WRITE(numout,*) '    mesozooplankton mortality rate            mzrat2    =', mzrat2
         WRITE(numout,*) '    maximal mesozoo grazing rate              grazrat2  =', grazrat2
         WRITE(numout,*) '    mesozoo flux feeding rate                 grazflux  =', grazflux
         WRITE(numout,*) '    non assimilated fraction of P by mesozoo  unass2    =', unass2
         WRITE(numout,*) '    Efficicency of Mesozoo growth             epsher2   =', epsher2
         WRITE(numout,*) '    Fraction of mesozoo excretion as DOM      sigma2    =', sigma2
         WRITE(numout,*) '    half sturation constant for grazing 2     xkgraz2   =', xkgraz2
      ENDIF

   END SUBROUTINE p4z_meso_nam


#else
   !!======================================================================
   !!  Dummy module :                                   No PISCES bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE p4z_meso                    ! Empty routine
   END SUBROUTINE p4z_meso
#endif 

   !!======================================================================
END MODULE  p4zmeso
