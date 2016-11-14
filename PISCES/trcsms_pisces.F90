#include "cppdefs.h"

MODULE trcsms_pisces
   !!======================================================================
   !!                         ***  MODULE trcsms_pisces  ***
   !! TOP :   PISCES Source Minus Sink manager
   !!======================================================================
   !! History :   1.0  !  2004-03 (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!----------------------------------------------------------------------
#if defined key_pisces
   !!----------------------------------------------------------------------
   !!   'key_pisces'                                       PISCES bio-model
   !!----------------------------------------------------------------------
   !!   trcsms_pisces        :  Time loop of passive tracers sms
   !!----------------------------------------------------------------------
   USE sms_pisces
   USE p4zint          ! 
   USE p4zche          ! 
   USE p4zbio          ! 
   USE p4zsed          ! 
   USE p4zlys          ! 
   USE p4zflx          ! 
   USE trcwri_pisces          ! 

   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_sms_pisces    ! called in trcsms.F90

#include "ocean2pisces.h90"
   !!----------------------------------------------------------------------
   !! NEMO/TOP 2.0 , LOCEAN-IPSL (2007) 
   !! $Id: trcsms_pisces.F90 1753 2009-11-25 12:35:09Z cetlod $ 
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE trc_sms_pisces( kt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_sms_pisces  ***
      !!
      !! ** Purpose :   Managment of the call to Biological sources and sinks 
      !!              routines of PISCES bio-model
      !!
      !! ** Method  : - at each new day ...
      !!              - several calls of bio and sed ???
      !!              - ...
      !!---------------------------------------------------------------------
      INTEGER, INTENT( in ) ::   kt      ! ocean time-step index      
      !!
      INTEGER  :: ji, jj, jk, jn, jnt
      REAL(wp) :: ztra
      LOGICAL :: ltra
      !!---------------------------------------------------------------------

      IF( ndayflxtr /= nday_year ) THEN      ! New days
         !
        IF(lwp) write(numout,*)
        IF(lwp) write(numout,*) ' New chemical constants and various rates for biogeochemistry at new day : ', nday_year
        IF(lwp) write(numout,*) '~~~~~~'
        ndayflxtr = nday_year

        CALL p4z_che          ! computation of chemical constants
        CALL p4z_int          ! computation of various rates for biogeochemistry
        !
      ENDIF


      DO jnt = 1, nrdttrc          ! Potential time splitting if requested
         !
         CALL p4z_bio (kt, jnt )    ! Compute soft tissue production (POC)
         CALL p4z_sed (kt, jnt )    ! compute soft tissue remineralisation
          CALL p4z_lys( kt, jnt )    ! Compute CaCO3 saturation
         CALL p4z_flx( kt, jnt )    ! Compute surface fluxes
         !
         !                             ! test if tracers concentrations fall below 0.
         xnegtr(:,:,:) = 1.e0
         DO jn = jp_pcs0, jp_pcs1
            DO jk = KRANGE
               DO jj = JRANGE
                  DO ji = IRANGE
                     IF( ( trn(ji,jj,K,jn) + tra(ji,jj,jk,jn) ) < 0.e0 ) THEN 
                        ztra             = ABS(  ( trn(ji,jj,K,jn) - rtrn ) &
                                               / ( tra(ji,jj,jk,jn) + rtrn ) )
                        xnegtr(ji,jj,jk) = MIN( xnegtr(ji,jj,jk),  ztra )
                     ENDIF
                  END DO
               END DO
            END DO
         END DO
         !                                ! where at least 1 tracer concentration becomes negative
         !                                ! 
         DO jn = jp_pcs0, jp_pcs1
            DO jk = KRANGE
               DO jj = JRANGE
                  DO ji = IRANGE
                     trn(ji,jj,K,jn) = trn(ji,jj,K,jn)    &
                         &           + xnegtr(ji,jj,jk) * tra(ji,jj,jk,jn)
                     tra(ji,jj,jk,jn) = 0.e0
                  END DO
               END DO
            END DO
         END DO
         !
      END DO

#if defined key_kriest
        ! 
        zcoef1 = 1.e0 / xkr_massp 
        zcoef2 = 1.e0 / xkr_massp / 1.1
        DO jk = KRANGE
            DO jj = JRANGE
               DO ji = IRANGE
                  trn(ji,jj,K,jpnum) = MAX(  trn(ji,jj,K,jpnum), trn(ji,jj,K,jppoc) * zcoef1 / xnumm(jk)  )
                  trn(ji,jj,K,jpnum) = MIN(  trn(ji,jj,K,jpnum), trn(ji,jj,K,jppoc) * zcoef2              )
               END DO
            END DO
        END DO
#endif

      IF( ln_ctl )  THEN
        CALL prt_ctl_trc( 'sms' ) 
        tra_ctl(:) = 0.
      ENDIF

      IF( kt - 1 == nitend )  CALL  tracer_stat( kt )

   END SUBROUTINE trc_sms_pisces

#else
   !!======================================================================
   !!  Dummy module :                                   No PISCES bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE trc_sms_pisces( kt )                   ! Empty routine
      INTEGER, INTENT( in ) ::   kt
      WRITE(*,*) 'trc_sms_pisces: You should not have seen this print! error?', kt
   END SUBROUTINE trc_sms_pisces
#endif 

   !!======================================================================
END MODULE trcsms_pisces 
