#include "cppdefs.h"

MODULE p4zmicro
   !!======================================================================
   !!                         ***  MODULE p4zmicro  ***
   !! TOP :   PISCES Compute the sources/sinks for microzooplankton
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!----------------------------------------------------------------------
#if defined key_pisces
   !!----------------------------------------------------------------------
   !!   'key_pisces'                                       PISCES bio-model
   !!----------------------------------------------------------------------
   !!   p4z_micro       :   Compute the sources/sinks for microzooplankton
   !!   p4z_micro_init  :   Initialize and read the appropriate namelist
   !!----------------------------------------------------------------------
   USE sms_pisces      ! 
   USE p4zint

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_micro    ! called in p4zbio.F90
   PUBLIC   p4z_micro_nam    ! called in p4zbio.F90

   !!* Substitution
#  include "top_substitute.h90"
#  include "ocean2pisces.h90"

   !! * Shared module variables
   REAL(wp), PUBLIC ::   &
      xpref2c = 0.0       ,  &  !:
      xpref2p = 0.5      ,  &  !:
      xpref2d = 0.5       ,  &  !:
      resrat  = 0.03      ,  &  !:
      mzrat   = 0.0      ,  &  !:
      grazrat = 4.0       ,  &  !:
      xkgraz  = 20E-6     ,  &  !:
      unass   = 0.3       ,  &  !:
      sigma1  = 0.6       ,  &  !:
      epsher  = 0.33


   !!----------------------------------------------------------------------
   !! NEMO/TOP 2.0 , LOCEAN-IPSL (2007) 
   !! $Id: p4zmicro.F90 1830 2010-04-12 13:03:51Z cetlod $ 
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE p4z_micro( kt,jnt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_micro  ***
      !!
      !! ** Purpose :   Compute the sources/sinks for microzooplankton
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, jnt ! ocean time step
      INTEGER  :: ji, jj, jk
      REAL(wp) :: zcompadi, zcompadi2, zcompaz , zcompaph, zcompapoc
      REAL(wp) :: zgraze  , zdenom  , zdenom2
      REAL(wp) :: zfact   , zstep   , zinano , zidiat, zipoc
      REAL(wp) :: zgrarem, zgrafer, zgrapoc, zprcaca, zmortz
      REAL(wp), DIMENSION(PRIV_3D_BIOARRAY) :: zrespz,ztortz
      REAL(wp), DIMENSION(PRIV_3D_BIOARRAY) :: zgrazp, zgrazm, zgrazsd
      REAL(wp), DIMENSION(PRIV_3D_BIOARRAY) :: zgrazmf, zgrazsf, zgrazpf
      CHARACTER (len=25) :: charout

      !!---------------------------------------------------------------------

      zrespz (:,:,:) = 0.
      ztortz (:,:,:) = 0.
      zgrazp (:,:,:) = 0.
      zgrazm (:,:,:) = 0.
      zgrazsd(:,:,:) = 0.
      zgrazmf(:,:,:) = 0.
      zgrazsf(:,:,:) = 0.
      zgrazpf(:,:,:) = 0.

#if defined key_trc_diaad
      grazing(:,:,:) = 0.  !: Initialisation of  grazing
#endif

      zstep = rfact2 / rday      ! Time step duration for biology

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               zcompaz = MAX( ( trn(ji,jj,K,jpzoo) - 1.e-9 ), 0.e0 )
# if defined key_off_degrad
               zfact   = zstep * tgfunc(ji,jj,jk) * zcompaz *facvol(ji,jj,jk)
# else
               zfact   = zstep * tgfunc(ji,jj,jk) * zcompaz
# endif

!     Respiration rates of both zooplankton
!     -------------------------------------

               zrespz(ji,jj,jk) = resrat * zfact  * ( 1.+ 3.* nitrfac(ji,jj,jk) )     &
                  &            * trn(ji,jj,K,jpzoo)                                 & 
                  &            / ( xkmort + trn(ji,jj,K,jpzoo) )

!     Zooplankton mortality. A square function has been selected with
!     no real reason except that it seems to be more stable and may
!     mimic predation.
!     ---------------------------------------------------------------
               ztortz(ji,jj,jk) = mzrat * 1.e6 * zfact * trn(ji,jj,K,jpzoo)

            END DO
         END DO
      END DO


 
      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               zcompadi  = MAX( ( trn(ji,jj,K,jpdia) - 1.e-8 ), 0.e0 )
               zcompadi2 = MIN( zcompadi, 5.e-7 )
               zcompaph  = MAX( ( trn(ji,jj,K,jpphy) - 2.e-7 ), 0.e0 )
               zcompapoc = MAX( ( trn(ji,jj,K,jppoc) - 1.e-8 ), 0.e0 )
               
               !     Microzooplankton grazing
               !     ------------------------
               zdenom2 = 1./ ( xpref2p * zcompaph + xpref2c * zcompapoc + xpref2d * zcompadi2 + rtrn )

               zgraze = grazrat * zstep * tgfunc(ji,jj,jk)     &
# if defined key_off_degrad
                  &      * facvol(ji,jj,jk)         &
# endif
                  &      * trn(ji,jj,K,jpzoo)

               zinano = xpref2p * zcompaph  * zdenom2
               zipoc  = xpref2c * zcompapoc * zdenom2
               zidiat = xpref2d * zcompadi2 * zdenom2

               zdenom = 1./ ( xkgraz + zinano * zcompaph + zipoc * zcompapoc + zidiat * zcompadi2 )

               zgrazp(ji,jj,jk)  = zgraze * zinano * zcompaph * zdenom
               zgrazm(ji,jj,jk)  = zgraze * zipoc  * zcompapoc * zdenom
               zgrazsd(ji,jj,jk) = zgraze * zidiat * zcompadi2 * zdenom

               zgrazpf (ji,jj,jk) = zgrazp(ji,jj,jk)  * trn(ji,jj,K,jpnfe)   &
                  &                / (trn(ji,jj,K,jpphy) + rtrn)
               zgrazmf(ji,jj,jk)  = zgrazm(ji,jj,jk)  * trn(ji,jj,K,jpsfe)   &
                  &                / (trn(ji,jj,K,jppoc) + rtrn)
               zgrazsf(ji,jj,jk)  = zgrazsd(ji,jj,jk) * trn(ji,jj,K,jpdfe)   &
                  &                / (trn(ji,jj,K,jpdia) + rtrn)
            END DO
         END DO
      END DO
      
#if defined key_trc_diaad
      ! Grazing by microzooplankton
      grazing(:,:,:) = grazing(:,:,:) + zgrazp(:,:,:) + zgrazm(:,:,:) + zgrazsd(:,:,:) 
#endif

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
!    Various remineralization and excretion terms
!    --------------------------------------------

               zgrarem = (  zgrazp(ji,jj,jk) + zgrazm(ji,jj,jk)  + zgrazsd(ji,jj,jk)  ) &
                  &          * ( 1.- epsher - unass )
               zgrafer = (  zgrazpf(ji,jj,jk) + zgrazsf(ji,jj,jk)  + zgrazmf(ji,jj,jk)  ) &
                  &        * ( 1.- epsher - unass ) + epsher *       &
                  &  ( zgrazm(ji,jj,jk)  * MAX((trn(ji,jj,K,jpsfe)    &
                  &   /(trn(ji,jj,K,jppoc)+ rtrn)-ferat3),0.e0)      &
                  &   + zgrazp(ji,jj,jk)  * MAX((trn(ji,jj,K,jpnfe)   &
                  &   /(trn(ji,jj,K,jpphy)+ rtrn)-ferat3),0.e0)      &
                  &   + zgrazsd(ji,jj,jk) * MAX((trn(ji,jj,K,jpdfe)   &
                  &   /(trn(ji,jj,K,jpdia)+ rtrn)-ferat3),0.e0 )  )
               zgrapoc = (  zgrazp(ji,jj,jk) + zgrazm(ji,jj,jk) + zgrazsd(ji,jj,jk)  ) * unass

               !  Update of the TRA arrays
               !  ------------------------

               tra(ji,jj,jk,jppo4) = tra(ji,jj,jk,jppo4) + zgrarem * sigma1
               tra(ji,jj,jk,jpnh4) = tra(ji,jj,jk,jpnh4) + zgrarem * sigma1
               tra(ji,jj,jk,jpdoc) = tra(ji,jj,jk,jpdoc) + zgrarem * (1.-sigma1)
               tra(ji,jj,jk,jpoxy) = tra(ji,jj,jk,jpoxy) - o2ut * zgrarem * sigma1
               tra(ji,jj,jk,jpfer) = tra(ji,jj,jk,jpfer) + zgrafer
               tra(ji,jj,jk,jppoc) = tra(ji,jj,jk,jppoc) + zgrapoc
               tra(ji,jj,jk,jpdic) = tra(ji,jj,jk,jpdic) + zgrarem * sigma1
#if defined key_kriest
               tra(ji,jj,jk,jpnum) = tra(ji,jj,jk,jpnum) + zgrapoc * xkr_ddiat
#endif
            END DO
         END DO
      END DO

!
!   Update the arrays TRA which contain the biological sources and sinks
!   --------------------------------------------------------------------

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE

               zmortz = ztortz(ji,jj,jk) + zrespz(ji,jj,jk)
               tra(ji,jj,jk,jpzoo) = tra(ji,jj,jk,jpzoo) - zmortz  &
                 &     + epsher * ( zgrazp(ji,jj,jk) + zgrazm(ji,jj,jk) + zgrazsd(ji,jj,jk))
               tra(ji,jj,jk,jpphy) = tra(ji,jj,jk,jpphy) - zgrazp(ji,jj,jk)
               tra(ji,jj,jk,jpdia) = tra(ji,jj,jk,jpdia) - zgrazsd(ji,jj,jk)
               tra(ji,jj,jk,jpnch) = tra(ji,jj,jk,jpnch) - zgrazp(ji,jj,jk)  &
                 &     * trn(ji,jj,K,jpnch)/(trn(ji,jj,K,jpphy)+rtrn)
               tra(ji,jj,jk,jpdch) = tra(ji,jj,jk,jpdch) - zgrazsd(ji,jj,jk) &
                 &     * trn(ji,jj,K,jpdch)/(trn(ji,jj,K,jpdia)+rtrn)
               tra(ji,jj,jk,jpbsi) = tra(ji,jj,jk,jpbsi) - zgrazsd(ji,jj,jk) &
                 &     * trn(ji,jj,K,jpbsi)/(trn(ji,jj,K,jpdia)+rtrn)
               tra(ji,jj,jk,jpdsi) = tra(ji,jj,jk,jpdsi) + zgrazsd(ji,jj,jk) &
                 &     * trn(ji,jj,K,jpbsi)/(trn(ji,jj,K,jpdia)+rtrn)
               tra(ji,jj,jk,jpnfe) = tra(ji,jj,jk,jpnfe) - zgrazpf(ji,jj,jk)
               tra(ji,jj,jk,jpdfe) = tra(ji,jj,jk,jpdfe) - zgrazsf(ji,jj,jk)
               tra(ji,jj,jk,jppoc) = tra(ji,jj,jk,jppoc) + zmortz - zgrazm(ji,jj,jk)
               tra(ji,jj,jk,jpsfe) = tra(ji,jj,jk,jpsfe) + ferat3 * zmortz   &
                 &     + unass * ( zgrazpf(ji,jj,jk) + zgrazsf (ji,jj,jk)) &
                 &     - (1.-unass) * zgrazmf(ji,jj,jk)
               zprcaca = xfracal(ji,jj,jk) * unass * zgrazp(ji,jj,jk)
#if defined key_trc_diaad
               prodcal(ji,jj,jk) = prodcal(ji,jj,jk) + zprcaca  ! prodcal=prodcal(nanophy)+prodcal(microzoo)+prodcal(mesozoo)
#endif
               zprcaca = part * zprcaca
               tra(ji,jj,jk,jpdic) = tra(ji,jj,jk,jpdic) - zprcaca
               tra(ji,jj,jk,jptal) = tra(ji,jj,jk,jptal) - 2. * zprcaca
               tra(ji,jj,jk,jpcal) = tra(ji,jj,jk,jpcal) + zprcaca
#if defined key_kriest
               tra(ji,jj,jk,jpnum) = tra(ji,jj,jk,jpnum) + ( zmortz - zgrazm(ji,jj,jk) ) * xkr_ddiat
#endif
            END DO
         END DO
      END DO
      !
      IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('micro')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc( charout, ltra='tra')
!         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
      ENDIF

   END SUBROUTINE p4z_micro


   SUBROUTINE p4z_micro_nam

      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_micro_init  ***
      !!
      !! ** Purpose :   Initialization of microzooplankton parameters
      !!
      !! ** Method  :   Read the nampiszoo namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist nampiszoo
      !!
      !!----------------------------------------------------------------------

      NAMELIST/nampiszoo/ grazrat,resrat,mzrat,xpref2c, xpref2p, &
         &             xpref2d, xkgraz, epsher, sigma1, unass

      REWIND( numnatp )                     ! read numnatp
      READ  ( numnatp, nampiszoo )

      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' '
         WRITE(numout,*) ' Namelist parameters for microzooplankton, nampiszoo'
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '    zoo preference for POC                    xpref2c    =', xpref2c
         WRITE(numout,*) '    zoo preference for nano                   xpref2p    =', xpref2p
         WRITE(numout,*) '    zoo preference for diatoms                xpref2d    =', xpref2d
         WRITE(numout,*) '    exsudation rate of microzooplankton       resrat    =', resrat
         WRITE(numout,*) '    microzooplankton mortality rate           mzrat     =', mzrat
         WRITE(numout,*) '    maximal microzoo grazing rate             grazrat   =', grazrat
         WRITE(numout,*) '    non assimilated fraction of P by microzoo unass     =', unass
         WRITE(numout,*) '    Efficicency of microzoo growth            epsher    =', epsher
         WRITE(numout,*) '    Fraction of microzoo excretion as DOM     sigma1    =', sigma1
         WRITE(numout,*) '    half sturation constant for grazing 1     xkgraz    =', xkgraz
      ENDIF

   END SUBROUTINE p4z_micro_nam

#else
   !!======================================================================
   !!  Dummy module :                                   No PISCES bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE p4z_micro                    ! Empty routine
   END SUBROUTINE p4z_micro
#endif 

   !!======================================================================
END MODULE  p4zmicro
