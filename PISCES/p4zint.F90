#include "cppdefs.h"

MODULE p4zint
   !!======================================================================
   !!                         ***  MODULE p4zint  ***
   !! TOP :   PISCES interpolation and computation of various accessory fields
   !!======================================================================
   !! History :   1.0  !  2004-03 (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!----------------------------------------------------------------------
#if defined key_pisces
   !!----------------------------------------------------------------------
   !!   'key_pisces'                                       PISCES bio-model
   !!----------------------------------------------------------------------
   !!   p4z_int        :  interpolation and computation of various accessory fields
   !!----------------------------------------------------------------------
   USE sms_pisces

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_int  

   !!* Substitution
#  include "ocean2pisces.h90"

   !! * Module variables
   REAL(wp) :: &
      xksilim = 16.5E-6        ! Half-saturation constant for the computation of the Si half-saturation constant


   !!----------------------------------------------------------------------
   !! NEMO/TOP 2.0 , LOCEAN-IPSL (2007) 
   !! $Id: p4zint.F90 1753 2009-11-25 12:35:09Z cetlod $ 
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE p4z_int
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_int  ***
      !!
      !! ** Purpose :   interpolation and computation of various accessory fields
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      !!
      INTEGER  ::   ji, jj, jk
      REAL(wp) ::   zdum
      !!---------------------------------------------------------------------

      ! Computation of phyto and zoo metabolic rate
      ! -------------------------------------------

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               tgfunc (ji,jj,jk) = EXP( 0.063913 * tsn(ji,jj,K,jp_tem) )
               tgfunc2(ji,jj,jk) = EXP( 0.07608  * tsn(ji,jj,K,jp_tem) )
            END DO
         END DO
      END DO

      ! Computation of the silicon dependant half saturation
      ! constant for silica uptake
      ! ---------------------------------------------------

      DO jj = JRANGE
         DO ji = IRANGE
            zdum = trn(ji,jj,KSURF,jpsil) * trn(ji,jj,KSURF,jpsil)
            xksimax(ji,jj) = MAX( xksimax(ji,jj), ( 1.+ 7.* zdum / ( xksilim * xksilim + zdum ) ) * 1e-6 )
         END DO
      END DO

      IF( nday_year == 365 ) THEN
         xksi    = xksimax
         xksimax = 0.e0
      ENDIF
      !
   END SUBROUTINE p4z_int

#else
   !!======================================================================
   !!  Dummy module :                                   No PISCES bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE p4z_int                   ! Empty routine
      WRITE(*,*) 'p4z_int: You should not have seen this print! error?'
   END SUBROUTINE p4z_int
#endif 

   !!======================================================================
END MODULE  p4zint
