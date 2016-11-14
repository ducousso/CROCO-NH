#include "cppdefs.h"

MODULE p4zmort
   !!======================================================================
   !!                         ***  MODULE p4zmort  ***
   !! TOP :   PISCES Compute the mortality terms for phytoplankton
   !!======================================================================
   !! History :   1.0  !  2002     (O. Aumont)  Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!----------------------------------------------------------------------
#if defined key_pisces
   !!----------------------------------------------------------------------
   !!   'key_pisces'                                       PISCES bio-model
   !!----------------------------------------------------------------------
   !!   p4z_mort       :   Compute the mortality terms for phytoplankton
   !!   p4z_mort_init  :   Initialize the mortality params for phytoplankton
   !!----------------------------------------------------------------------
   USE sms_pisces      ! 
   USE p4zsink

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_mort    
   PUBLIC   p4z_mort_nam    


   !!* Substitution
#  include "ocean2pisces.h90"
#  include "top_substitute.h90"

   !! * Shared module variables
   REAL(wp), PUBLIC ::   &
     wchl   = 0.001    ,  &  !:
     wchld  = 0.02     ,  &  !:
     mprat  = 0.01     ,  &  !:
     mprat2 = 0.01     ,  &  !:
     mpratm = 0.01           !:


   !!----------------------------------------------------------------------
   !! NEMO/TOP 2.0 , LOCEAN-IPSL (2007) 
   !! $Id: p4zmort.F90 1808 2010-03-11 09:17:56Z cetlod $ 
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE p4z_mort( kt, jnt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_mort  ***
      !!
      !! ** Purpose :   Calls the different subroutine to initialize and compute
      !!                the different phytoplankton mortality terms
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, jnt ! ocean time step
      !!---------------------------------------------------------------------


      CALL p4z_nano            ! nanophytoplankton

      CALL p4z_diat            ! diatoms

   END SUBROUTINE p4z_mort


   SUBROUTINE p4z_nano
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_nano  ***
      !!
      !! ** Purpose :   Compute the mortality terms for nanophytoplankton
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER  :: ji, jj, jk
      REAL(wp) :: zcompaph
      REAL(wp) :: zfactfe,zfactch,zprcaca,zfracal
      REAL(wp) :: ztortp,zrespp,zmortp
      CHARACTER (len=25) :: charout
      !!---------------------------------------------------------------------


#if defined key_trc_diaad
     prodcal(:,:,:) = 0.  !: Initialisation of calcite production variable
#endif

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE

               zcompaph = MAX( ( trn(ji,jj,K,jpphy) - 1e-8 ), 0.e0 )

!     Squared mortality of Phyto similar to a sedimentation term during
!     blooms (Doney et al. 1996)
!     -----------------------------------------------------------------
               zrespp = wchl * 1.e6 * xstep * xdiss(ji,jj,jk)   &
# if defined key_off_degrad
                  &        * facvol(ji,jj,jk)     &
# endif
                  &        * zcompaph * trn(ji,jj,K,jpphy)

!     Phytoplankton mortality. This mortality loss is slightly
!     increased when nutrients are limiting phytoplankton growth
!     as observed for instance in case of iron limitation.
!     ----------------------------------------------------------
               ztortp = mprat * xstep * trn(ji,jj,K,jpphy)          &
# if defined key_off_degrad
                  &          * facvol(ji,jj,jk)     &
# endif
                  &   / ( xkmort + trn(ji,jj,K,jpphy) ) * zcompaph


               zmortp = zrespp + ztortp

               !   Update the arrays TRA which contains the biological sources and sinks

               zfactfe = trn(ji,jj,K,jpnfe)/(trn(ji,jj,K,jpphy)+rtrn)
               zfactch = trn(ji,jj,K,jpnch)/(trn(ji,jj,K,jpphy)+rtrn)

               tra(ji,jj,jk,jpphy) = tra(ji,jj,jk,jpphy) - zmortp
               tra(ji,jj,jk,jpnch) = tra(ji,jj,jk,jpnch) - zmortp * zfactch
               tra(ji,jj,jk,jpnfe) = tra(ji,jj,jk,jpnfe) - zmortp * zfactfe
               zprcaca = xfracal(ji,jj,jk) * zmortp
#if defined key_trc_diaad
               prodcal(ji,jj,jk) = prodcal(ji,jj,jk) + zprcaca  ! prodcal=prodcal(nanophy)+prodcal(microzoo)+prodcal(mesozoo)
#endif
               zfracal = 0.5 * xfracal(ji,jj,jk)
               tra(ji,jj,jk,jpdic) = tra(ji,jj,jk,jpdic) - zprcaca
               tra(ji,jj,jk,jptal) = tra(ji,jj,jk,jptal) - 2. * zprcaca
               tra(ji,jj,jk,jpcal) = tra(ji,jj,jk,jpcal) + zprcaca
#if defined key_kriest
               tra(ji,jj,jk,jppoc) = tra(ji,jj,jk,jppoc) + zmortp
               tra(ji,jj,jk,jpnum) = tra(ji,jj,jk,jpnum) + ztortp * xkr_dnano + zrespp * xkr_ddiat
               tra(ji,jj,jk,jpsfe) = tra(ji,jj,jk,jpsfe) + zmortp * zfactfe
#else
               tra(ji,jj,jk,jpgoc) = tra(ji,jj,jk,jpgoc) + zfracal * zmortp
               tra(ji,jj,jk,jppoc) = tra(ji,jj,jk,jppoc) + ( 1. - zfracal ) * zmortp
               tra(ji,jj,jk,jpsfe) = tra(ji,jj,jk,jpsfe) + ( 1. - zfracal ) * zmortp * zfactfe
               tra(ji,jj,jk,jpbfe) = tra(ji,jj,jk,jpbfe) + zfracal * zmortp * zfactfe
#endif
            END DO
         END DO
      END DO
      !
       IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('nano')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc( charout, ltra='tra')
!         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
       ENDIF

   END SUBROUTINE p4z_nano

   SUBROUTINE p4z_diat
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_diat  ***
      !!
      !! ** Purpose :   Compute the mortality terms for diatoms
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER  ::  ji, jj, jk
      REAL(wp) ::  zfactfe,zfactsi,zfactch, zcompadi
      REAL(wp) ::  zrespp2, ztortp2, zmortp2
      CHARACTER (len=25) :: charout
 
      !!---------------------------------------------------------------------


!    Aggregation term for diatoms is increased in case of nutrient
!    stress as observed in reality. The stressed cells become more
!    sticky and coagulate to sink quickly out of the euphotic zone
!     ------------------------------------------------------------

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE

               zcompadi = MAX( ( trn(ji,jj,K,jpdia) - 1e-8), 0. )

!    Aggregation term for diatoms is increased in case of nutrient
!    stress as observed in reality. The stressed cells become more
!    sticky and coagulate to sink quickly out of the euphotic zone
!     ------------------------------------------------------------

               zrespp2  = 1.e6 * xstep * (  wchl + wchld * ( 1.- xlimdia(ji,jj,jk) )  )    &
# if defined key_off_degrad
                  &       * facvol(ji,jj,jk)       &
# endif
                  &       * xdiss(ji,jj,jk) * zcompadi * trn(ji,jj,K,jpdia)
                                                                               

!     Phytoplankton mortality. 
!     ------------------------
               ztortp2  = mprat2 * xstep * trn(ji,jj,K,jpdia)     &
# if defined key_off_degrad
                  &        * facvol(ji,jj,jk)       &
# endif
                  &      / ( xkmort + trn(ji,jj,K,jpdia) ) * zcompadi

                zmortp2 = zrespp2 + ztortp2

!   Update the arrays tra which contains the biological sources and sinks
!   ---------------------------------------------------------------------
               zfactch = trn(ji,jj,K,jpdch) / ( trn(ji,jj,K,jpdia) + rtrn )
               zfactfe = trn(ji,jj,K,jpdfe) / ( trn(ji,jj,K,jpdia) + rtrn )
               zfactsi = trn(ji,jj,K,jpbsi) / ( trn(ji,jj,K,jpdia) + rtrn )

               tra(ji,jj,jk,jpdia) = tra(ji,jj,jk,jpdia) - zmortp2 
               tra(ji,jj,jk,jpdch) = tra(ji,jj,jk,jpdch) - zmortp2 * zfactch
               tra(ji,jj,jk,jpdfe) = tra(ji,jj,jk,jpdfe) - zmortp2 * zfactfe
               tra(ji,jj,jk,jpbsi) = tra(ji,jj,jk,jpbsi) - zmortp2 * zfactsi
               tra(ji,jj,jk,jpdsi) = tra(ji,jj,jk,jpdsi) + zmortp2 * zfactsi
#if defined key_kriest
               tra(ji,jj,jk,jppoc) = tra(ji,jj,jk,jppoc) + zmortp2  
               tra(ji,jj,jk,jpnum) = tra(ji,jj,jk,jpnum) + ztortp2 * xkr_ddiat + zrespp2 * xkr_daggr
               tra(ji,jj,jk,jpsfe) = tra(ji,jj,jk,jpsfe) + zmortp2 * zfactfe
#else
               tra(ji,jj,jk,jpgoc) = tra(ji,jj,jk,jpgoc) + zrespp2 + 0.5 * ztortp2
               tra(ji,jj,jk,jppoc) = tra(ji,jj,jk,jppoc) + 0.5 * ztortp2
               tra(ji,jj,jk,jpsfe) = tra(ji,jj,jk,jpsfe) + 0.5 * ztortp2 * zfactfe
               tra(ji,jj,jk,jpbfe) = tra(ji,jj,jk,jpbfe) + ( zrespp2 + 0.5 * ztortp2 ) * zfactfe
#endif
            END DO
         END DO
      END DO
      !
        IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('diat')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc( charout, ltra='tra')
!         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
       ENDIF
             
   END SUBROUTINE p4z_diat

   SUBROUTINE p4z_mort_nam

      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_mort_init  ***
      !!
      !! ** Purpose :   Initialization of phytoplankton parameters
      !!
      !! ** Method  :   Read the nampismort namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist nampismort
      !!
      !!----------------------------------------------------------------------

      NAMELIST/nampismort/ wchl, wchld, mprat, mprat2, mpratm

      REWIND( numnatp )                     ! read numnatp
      READ  ( numnatp, nampismort )

      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' '
         WRITE(numout,*) ' Namelist parameters for phytoplankton mortality, nampismort'
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '    quadratic mortality of phytoplankton      wchl      =', wchl
         WRITE(numout,*) '    maximum quadratic mortality of diatoms    wchld     =', wchld
         WRITE(numout,*) '    phytoplankton mortality rate              mprat     =', mprat
         WRITE(numout,*) '    Diatoms mortality rate                    mprat2    =', mprat2
         WRITE(numout,*) '    Phytoplankton minimum mortality rate      mpratm    =', mpratm
      ENDIF

   END SUBROUTINE p4z_mort_nam

#else
   !!======================================================================
   !!  Dummy module :                                   No PISCES bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE p4z_mort                    ! Empty routine
   END SUBROUTINE p4z_mort
#endif 

   !!======================================================================
END MODULE  p4zmort
