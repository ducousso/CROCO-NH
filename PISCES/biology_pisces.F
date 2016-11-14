! $Id$
!
!=========================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France.
! The two other branches, from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al), are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
!
! ROMS_AGRIF website : http://www.romsagrif.org
!=========================================================================
!

       subroutine biology_tile(Istr,Iend,Jstr,Jend)
!------------------------------------------------------------------
!
!   ROUTINE biology_pisces : PISCES MODEL
!   *************************************
!
!
!     PURPOSE.
!     --------
!          *ROMS_PISCES ECOSYSTEM MODEL FOR THE WHOLE OCEAN
!                       THIS ROUTINE COMPUTES INTERACTIONS
!                       BETWEEN THE DIFFERENT COMPARTMENTS OF THE
!                       MODEL
!----------------------------------------------------------------
       USE sms_pisces
       USE trcsms_pisces

C       implicit none
       INTEGER Istr,Iend,Jstr,Jend
       INTEGER i, j, jk, k, jn
       REAL  zdiag(GLOBAL_2D_ARRAY,0:N)

#include "ocean2pisces.h90"


      CALL ocean_2_pisces( Istr,Iend,Jstr,Jend) 

      DO jn = 1, jptra
         DO jk = KRANGE
            DO j = JRANGE
               DO i = IRANGE          ! masked grid volume
                  trn(i,j,K,jn) 
     &           = MAX( 0., trn(i,j,K,jn) * 1.e-6 )
               ENDDO
            ENDDO
         ENDDO
      END DO
      DO jk = KRANGE
         DO j = JRANGE
            DO i = IRANGE          ! masked grid volume
                trn(i,j,K,jpno3) 
     &        = trn(i,j,K,jpno3) / rno3
                trn(i,j,K,jpnh4) 
     &        = trn(i,j,K,jpnh4) / rno3
                trn(i,j,K,jppo4) 
     &        = trn(i,j,K,jppo4) / po4r
            ENDDO
         ENDDO
      ENDDO

      CALL trc_sms_pisces( iic )

      DO jn = 1, jptra
         DO jk = KRANGE
            DO j = JRANGE
               DO i = IRANGE          ! masked grid volume
                  trn(i,j,K,jn) 
     &          = trn(i,j,K,jn) * 1.e6
               ENDDO
            ENDDO
         ENDDO
      END DO
      DO jk = KRANGE
         DO j = JRANGE
            DO i = IRANGE          ! masked grid volume
               trn(i,j,K,jpno3)
     &       = trn(i,j,K,jpno3) * rno3
               trn(i,j,K,jpnh4) 
     &       = trn(i,j,K,jpnh4) * rno3
               trn(i,j,K,jppo4) 
     &       = trn(i,j,K,jppo4) * po4r
            ENDDO
         ENDDO
      ENDDO


      RETURN       
      END
