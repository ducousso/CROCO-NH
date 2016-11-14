#include "cppdefs.h"

MODULE p4zbio
   !!======================================================================
   !!                         ***  MODULE p4zbio  ***
   !! TOP :   PISCES bio-model
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!----------------------------------------------------------------------
#if defined key_pisces
   !!----------------------------------------------------------------------
   !!   'key_pisces'                                       PISCES bio-model
   !!----------------------------------------------------------------------
   !!   p4z_bio        :   computes the interactions between the different
   !!                      compartments of PISCES
   !!----------------------------------------------------------------------
   USE sms_pisces
   USE p4zsink         ! 
   USE p4zopt          ! 
   USE p4zlim          ! 
   USE p4zprod         !
   USE p4zmort         !
   USE p4zmicro        ! 
   USE p4zmeso         ! 
   USE p4zrem          ! 
  
   IMPLICIT NONE
   PRIVATE

   PUBLIC  p4z_bio    

   !!* Substitution
#  include "ocean2pisces.h90"
#  include "top_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 2.0 , LOCEAN-IPSL (2007) 
   !! $Id: p4zbio.F90 1808 2010-03-11 09:17:56Z cetlod $ 
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE p4z_bio ( kt, jnt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_bio  ***
      !!
      !! ** Purpose :   Ecosystem model in the whole ocean: computes the
      !!              different interactions between the different compartments
      !!              of PISCES
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) :: kt, jnt
      INTEGER  ::  ji, jj, jk, jn
      CHARACTER (len=25) :: charout
      LOGICAL :: ltra

      !!---------------------------------------------------------------------

      !     ASSIGN THE SHEAR RATE THAT IS USED FOR AGGREGATION
      !     OF PHYTOPLANKTON AND DETRITUS

      xdiss(:,:,:) = 1.
!!gm the use of nmld should be better here?
      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               IF( fsdepw(ji,jj,jk) > hmld(ji,jj) )   xdiss(ji,jj,jk) = 0.01
            END DO 
         END DO
      END DO

      CALL p4z_sink ( kt, jnt )     ! vertical flux of particulate organic matter
      CALL p4z_opt  ( kt, jnt )     ! Optic: PAR in the water column
      CALL p4z_lim  ( kt, jnt )     ! co-limitations by the various nutrients
      CALL p4z_prod ( kt, jnt )     ! phytoplankton growth rate over the global ocean. 
      !                             ! (for each element : C, Si, Fe, Chl )
      CALL p4z_rem  ( kt, jnt )     ! remineralization terms of organic matter+scavenging of Fe
      CALL p4z_mort ( kt, jnt )     ! phytoplankton mortality
      !                             ! zooplankton sources/sinks routines 
      CALL p4z_micro( kt, jnt )           ! microzooplankton
      CALL p4z_meso ( kt, jnt )           ! mesozooplankton

      !
      IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('bio ')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc( charout, ltra='tra')
!         CALL prt_ctl_trc(tab4d=trn, mask=tmask, clinfo=ctrcnm)
      ENDIF
      !
   END SUBROUTINE p4z_bio

#else
   !!======================================================================
   !!  Dummy module :                                   No PISCES bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE p4z_bio                         ! Empty routine
   END SUBROUTINE p4z_bio
#endif 

   !!======================================================================
END MODULE  p4zbio
