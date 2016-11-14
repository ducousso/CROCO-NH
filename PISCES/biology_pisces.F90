#include "cppdefs.h"
MODULE biology_pisces


   USE ocean2pisces
   USE trcsms_pisces

   IMPLICIT NONE
   PRIVATE


   PUBLIC biology_tile

CONTAINS

      SUBROUTINE biology_tile (Istr,Iend,Jstr,Jend)
        !!------------------------------------------------------------------
        !!
        !!   ROUTINE biology_pisces : PISCES MODEL
        !!   *************************************
        !!
        !!
        !!     PURPOSE.
        !!     --------
        !!          *ROMS_PISCES ECOSYSTEM MODEL FOR THE WHOLE OCEAN
        !!                       THIS ROUTINE COMPUTES INTERACTIONS
        !!                       BETWEEN THE DIFFERENT COMPARTMENTS OF THE
        !!                       MODEL
        !!----------------------------------------------------------------
        INTEGER Istr,Iend,Jstr,Jend
        INTEGER i, j, jk, k, jn
        REAL    zdiag


        DO jn = 1, jptra
           DO jk = KRANGE
              DO j = JRANGE
                 DO i = IRANGE          ! masked grid volume
                    trn(i,j,K,jn) = &
                 &  MAX( 0., trn(i,j,K,jn) * 1.e-6 )
                 ENDDO
              ENDDO
           ENDDO
        END DO
        DO jk = KRANGE
           DO j = JRANGE
              DO i = IRANGE          ! masked grid volume
                trn(i,j,K,jpno3) = &
            &   trn(i,j,K,jpno3) / rno3
                trn(i,j,K,jpnh4) = &
            &   trn(i,j,K,jpnh4) / rno3
                trn(i,j,K,jppo4) = &
            &   trn(i,j,K,jpno3) / po4r
              ENDDO
           ENDDO
        ENDDO

        CALL ocean_2_pisces( Istr,Iend,Jstr,Jend ) 
        CALL trc_sms_pisces( iic )


        DO jn = 1, jptra
           DO jk = KRANGE
              DO j = JRANGE
                 DO i = IRANGE          ! masked grid volume
                    trn(i,j,K,jn) = &
                 &  trn(i,j,K,jn) * 1.e6
                 ENDDO
              ENDDO
           ENDDO
        END DO
        DO jk = KRANGE
           DO j = JRANGE
              DO i = IRANGE          ! masked grid volume
                trn(i,j,K,jpno3) = &
            &   trn(i,j,K,jpno3) * rno3
                trn(i,j,K,jpnh4) = &
            &   trn(i,j,K,jpnh4) * rno3
                trn(i,j,K,jppo4) = &
            &   trn(i,j,K,jpno3) * po4r
              ENDDO
           ENDDO
        ENDDO
# if defined key_trc_diaad
        DO jn = 1, jpdia3d
           DO k = KRANGE
              DO j = JRANGE
                 DO i = IRANGE
                    zdiag = trc3d(i,j,K,jn)
                    trc3d(i,j,k,jn) = zdiag
                 END DO
              END DO
           END DO
       END DO
# endif


      END SUBROUTINE biology_tile

   !!======================================================================
END MODULE biology_pisces
