#include "cppdefs.h"

MODULE p4zlim
   !!======================================================================
   !!                         ***  MODULE p4zlim  ***
   !! TOP :   PISCES 
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!----------------------------------------------------------------------
#if defined key_pisces
   !!----------------------------------------------------------------------
   !!   'key_pisces'                                       PISCES bio-model
   !!----------------------------------------------------------------------
   !!   p4z_lim        :   Compute the nutrients limitation terms 
   !!   p4z_lim_init   :   Read the namelist 
   !!----------------------------------------------------------------------
   USE sms_pisces      ! 

   IMPLICIT NONE
   PRIVATE

   PUBLIC p4z_lim    
   PUBLIC p4z_lim_nam

   !!* Substitution
#  include "ocean2pisces.h90"
#  include "top_substitute.h90"

   !! * Shared module variables
   REAL(wp), PUBLIC ::   &
     conc0     = 2.e-6      ,  &  !:
     conc1     = 10.e-6     ,  &  !:
     conc2     = 2.e-11     ,  &  !:
     conc2m    = 8.E-11     ,  &  !:
     conc3     = 1.e-10     ,  &  !:
     conc3m    = 4.e-10     ,  &  !:
     concnnh4  = 1.e-7      ,  &  !:
     concdnh4  = 5.e-7      ,  &  !:
     xksi1     = 2.E-6      ,  &  !:
     xksi2     = 3.33E-6    ,  &  !:
     xkdoc     = 417.E-6    ,  &  !:
     caco3r    = 0.3              !:


   !!----------------------------------------------------------------------
   !! NEMO/TOP 2.0 , LOCEAN-IPSL (2007) 
   !! $Id: p4zlim.F90 1808 2010-03-11 09:17:56Z cetlod $ 
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE p4z_lim( kt, jnt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_lim  ***
      !!
      !! ** Purpose :   Compute the co-limitations by the various nutrients
      !!              for the various phytoplankton species
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, jnt ! ocean time step
      INTEGER  ::   ji, jj, jk
      REAL(wp) ::   zlim1, zlim2, zlim3, zlim4, zno3, zferlim
      REAL(wp) ::   zconctemp, zconctemp2, zconctempn, zconctempn2
      REAL(wp) ::   ztemp, zdenom
      !!---------------------------------------------------------------------


!  Tuning of the iron concentration to a minimum
!  level that is set to the detection limit
!  -------------------------------------

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               zno3 = trn(ji,jj,K,jpno3)
               zferlim = MAX( 1.5e-11*(zno3/40E-6)**2, 3e-12 )
               zferlim = MIN( zferlim, 1.5e-11 )
               trn(ji,jj,K,jpfer) = MAX( trn(ji,jj,K,jpfer), zferlim )
            END DO
         END DO
      END DO

!  Computation of a variable Ks for iron on diatoms
!  taking into account that increasing biomass is
!  made of generally bigger cells
!  ------------------------------------------------

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               zconctemp   = MAX( 0.e0 , trn(ji,jj,K,jpdia)-5e-7 )
               zconctemp2  = trn(ji,jj,K,jpdia) - zconctemp
               zconctempn  = MAX( 0.e0 , trn(ji,jj,K,jpphy)-1e-6 )
               zconctempn2 = trn(ji,jj,K,jpphy) - zconctempn
               concdfe(ji,jj,jk) = ( zconctemp2 * conc3 + conc3m * zconctemp)   &
                   &             / ( trn(ji,jj,K,jpdia) + rtrn )
               concdfe(ji,jj,jk) = MAX( conc3, concdfe(ji,jj,jk) )
               concnfe(ji,jj,jk) = ( zconctempn2 * conc2 + conc2m * zconctempn)  &
                   &             / ( trn(ji,jj,K,jpphy) + rtrn )
               concnfe(ji,jj,jk) = MAX( conc2, concnfe(ji,jj,jk) )
            END DO
         END DO
      END DO

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
    
!      Michaelis-Menten Limitation term for nutrients
!      Small flagellates
!      -----------------------------------------------
              zdenom = 1. / &
                  & (  conc0 * concnnh4 + concnnh4 * trn(ji,jj,K,jpno3) &
                  &  + conc0 * trn(ji,jj,K,jpnh4) )
               xnanono3(ji,jj,jk) = trn(ji,jj,K,jpno3) * concnnh4 * zdenom
               xnanonh4(ji,jj,jk) = trn(ji,jj,K,jpnh4) * conc0    * zdenom

               zlim1 = xnanono3(ji,jj,jk) + xnanonh4(ji,jj,jk)
               zlim2 = trn(ji,jj,K,jppo4) / ( trn(ji,jj,K,jppo4) + concnnh4) 
               zlim3 = trn(ji,jj,K,jpfer) / ( trn(ji,jj,K,jpfer)   &
                  &                          + concnfe(ji,jj,jk) )
               xlimphy(ji,jj,jk) = MIN( zlim1, zlim2, zlim3 )
               zlim1 = trn(ji,jj,K,jpnh4) / ( concnnh4 + trn(ji,jj,K,jpnh4) )
               zlim3 = trn(ji,jj,K,jpfer) / ( conc2    + trn(ji,jj,K,jpfer) )
               zlim4 = trn(ji,jj,K,jpdoc) / ( xkdoc    + trn(ji,jj,K,jpdoc) )
               xlimbac(ji,jj,jk) = MIN( zlim1, zlim2, zlim3 ) * zlim4

            END DO
         END DO
      END DO

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE

!   Michaelis-Menten Limitation term for nutrients Diatoms
!   ----------------------------------------------
              zdenom = 1. / &
                  & ( conc1 * concdnh4 + concdnh4 * trn(ji,jj,K,jpno3) &
                  & + conc1 * trn(ji,jj,K,jpnh4) )

               xdiatno3(ji,jj,jk) = trn(ji,jj,K,jpno3) * concdnh4 * zdenom
               xdiatnh4(ji,jj,jk) = trn(ji,jj,K,jpnh4) * conc1    * zdenom 

               zlim1 = xdiatno3(ji,jj,jk) + xdiatnh4(ji,jj,jk)
               zlim2 = trn(ji,jj,K,jppo4) / ( trn(ji,jj,K,jppo4) + concdnh4 )
               zlim3 = trn(ji,jj,K,jpsil) / ( trn(ji,jj,K,jpsil) + xksi(ji,jj) )
               zlim4 = trn(ji,jj,K,jpfer) / ( trn(ji,jj,K,jpfer)   &
                  &                         + concdfe(ji,jj,jk))
               xlimdia(ji,jj,jk) = MIN( zlim1, zlim2, zlim3, zlim4 )

            END DO
         END DO
      END DO


      ! Compute the fraction of nanophytoplankton that is made of calcifiers
      ! --------------------------------------------------------------------

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               ztemp = MAX( 0., tsn(ji,jj,K,jp_tem) )
               xfracal(ji,jj,jk) = caco3r * xlimphy(ji,jj,jk)   &
                  &                       * MAX( 0.0001, ztemp / ( 2.+ ztemp ) )   &
                  &                       * MAX( 1., trn(ji,jj,K,jpphy) * 1.e6 / 2. )
               xfracal(ji,jj,jk) = MIN( 0.8 , xfracal(ji,jj,jk) )
               xfracal(ji,jj,jk) = MAX( 0.01, xfracal(ji,jj,jk) )
            END DO
         END DO
      END DO
      !
   END SUBROUTINE p4z_lim

   SUBROUTINE p4z_lim_nam

      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_lim_init  ***
      !!
      !! ** Purpose :   Initialization of nutrient limitation parameters
      !!
      !! ** Method  :   Read the nampislim namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist nampislim
      !!
      !!----------------------------------------------------------------------

      NAMELIST/nampislim/ conc0, conc1, conc2, conc2m, conc3, conc3m,   &
         &             concnnh4, concdnh4, xksi1, xksi2, xkdoc, caco3r

      REWIND( numnatp )                     ! read numnatp
      READ  ( numnatp, nampislim )

      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' '
         WRITE(numout,*) ' Namelist parameters for nutrient limitations, nampislim'
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '    mean rainratio                            caco3r    =', caco3r
         WRITE(numout,*) '    NO3, PO4 half saturation                  conc0      =', conc0
         WRITE(numout,*) '    half saturation constant for Si uptake    xksi1     =', xksi1
         WRITE(numout,*) '    half saturation constant for Si/C         xksi2     =', xksi2
         WRITE(numout,*) '    2nd half-sat. of DOC remineralization     xkdoc    =', xkdoc
         WRITE(numout,*) '    Phosphate half saturation for diatoms     conc1     =', conc1
         WRITE(numout,*) '    Iron half saturation for phyto            conc2     =', conc2
         WRITE(numout,*) '    Max iron half saturation for phyto        conc2m    =', conc2m
         WRITE(numout,*) '    Iron half saturation for diatoms          conc3     =', conc3
         WRITE(numout,*) '    Maxi iron half saturation for diatoms     conc3m    =', conc3m
         WRITE(numout,*) '    NH4 half saturation for phyto             concnnh4  =', concnnh4
         WRITE(numout,*) '    NH4 half saturation for diatoms           concdnh4  =', concdnh4
      ENDIF

   END SUBROUTINE p4z_lim_nam

#else
   !!======================================================================
   !!  Dummy module :                                   No PISCES bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE p4z_lim                   ! Empty routine
   END SUBROUTINE p4z_lim
#endif 

   !!======================================================================
END MODULE  p4zlim
