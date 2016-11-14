#include "cppdefs.h"

MODULE p4zsink
   !!======================================================================
   !!                         ***  MODULE p4zsink  ***
   !! TOP :   PISCES Compute vertical flux of particulate matter due to gravitational sinking
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
#if defined key_pisces
   !!----------------------------------------------------------------------
   !!   p4z_sink       :  Compute vertical flux of particulate matter due to gravitational sinking
   !!----------------------------------------------------------------------
   USE sms_pisces
   USE trc

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_sink    ! called in p4zbio.F90
   PUBLIC   p4z_sink_alloc

   !!* Substitution
#  include "ocean2pisces.h90"
#  include "top_substitute.h90"

   !! * Shared module variables
   REAL(wp), PUBLIC, DIMENSION(:,:,:), ALLOCATABLE, SAVE ::   &   !:
     wsbio3, wsbio4,      &    !: POC and GOC sinking speeds
     wscal                     !: Calcite and BSi sinking speeds

   !! * Module variables
   REAL(wp), PUBLIC, DIMENSION(:,:,:), ALLOCATABLE, SAVE ::   &   !:
     sinking, sinking2,   &    !: POC sinking fluxes (different meanings depending on the parameterization
     sinkcal, sinksil,    &    !: CaCO3 and BSi sinking fluxes
     sinkfer,sinkfer2                   !: Small BFe sinking flux

   INTEGER  :: ik100 = 10


   !!----------------------------------------------------------------------
   !! NEMO/TOP 2.0 , LOCEAN-IPSL (2007) 
   !! $Id: p4zsink.F90 1830 2010-04-12 13:03:51Z cetlod $ 
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS


   SUBROUTINE p4z_sink ( kt, jnt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_sink  ***
      !!
      !! ** Purpose :   Compute vertical flux of particulate matter due to 
      !!                gravitational sinking
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) :: kt, jnt
      INTEGER  ::   ji, jj, jk, jit
      INTEGER  ::   iiter1, iiter2
      REAL(wp) ::   zagg1, zagg2, zagg3, zagg4
      REAL(wp) ::   zagg , zaggfe, zaggdoc, zaggdoc2
      REAL(wp) ::   zfact, zwsmax, zmax
      REAL(wp) ::   zrfact2, zmsk
      INTEGER  ::   ik1
      CHARACTER (len=25) :: charout
      !!---------------------------------------------------------------------


!    Sinking speeds of detritus is increased with depth as shown
!    by data and from the coagulation theory
!    -----------------------------------------------------------
      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               zmax = MAX( heup(ji,jj), hmld(ji,jj) )
               zfact = MAX( 0., fsdepw(ji,jj,jk+1) - zmax ) / 5000.
               wsbio4(ji,jj,jk) = wsbio2 + ( 200.- wsbio2 ) * zfact
               wsbio3(ji,jj,jk) = wsbio
               wscal (ji,jj,jk) = wsbio4(ji,jj,jk)
            END DO
         END DO
      END DO

!      LIMIT THE VALUES OF THE SINKING SPEEDS 
!      TO AVOID NUMERICAL INSTABILITIES


      !
      ! OA This is (I hope) a temporary solution for the problem that may 
      ! OA arise in specific situation where the CFL criterion is broken 
      ! OA for vertical sedimentation of particles. To avoid this, a time
      ! OA splitting algorithm has been coded. A specific maximum
      ! OA iteration number is provided and may be specified in the namelist 
      ! OA This is to avoid very large iteration number when explicit free
      ! OA surface is used (for instance). When niter?max is set to 1, 
      ! OA this computation is skipped. The crude old threshold method is 
      ! OA then applied. This also happens when niter exceeds nitermax.
      IF( MAX( niter1max, niter2max ) == 1 ) THEN
        iiter1 = 1
        iiter2 = 1
      ELSE
        iiter1 = 1
        iiter2 = 1
        DO jk = KRANGE
          DO jj = JRANGE
             DO ji = IRANGE
                IF( tmask(ji,jj,K) == 1) THEN
                   zwsmax =  0.5 * fse3t(ji,jj,K) / xstep
                   iiter1 =  MAX( iiter1, INT( wsbio3(ji,jj,jk) / zwsmax ) )
                   iiter2 =  MAX( iiter2, INT( wsbio4(ji,jj,jk) / zwsmax ) )
                ENDIF
             END DO
          END DO
        END DO
        IF( lk_mpp ) THEN
           CALL mpp_max( iiter1 )
           CALL mpp_max( iiter2 )
        ENDIF
        iiter1 = MIN( iiter1, niter1max )
        iiter2 = MIN( iiter2, niter2max )
      ENDIF


      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               IF( tmask(ji,jj,K) == 1 ) THEN
                 zwsmax = 0.5 * fse3t(ji,jj,K) / xstep
                 wsbio3(ji,jj,jk) = MIN( wsbio3(ji,jj,jk), zwsmax * FLOAT( iiter1 ) )
                 wsbio4(ji,jj,jk) = MIN( wsbio4(ji,jj,jk), zwsmax * FLOAT( iiter2 ) )
               ENDIF
            END DO
         END DO
      END DO

!      write(numout,*)
!      write(numout,*) '  level      Depht          depthw    wsbio3      wsbio4'
!      DO jk = KRANGE
!         write(numout,*) ' jk = ',jk,' dept = ',fsdept(jip,jjp,K),'  depw =  ',fsdepw(jip,jjp,jk), &
!            &            '   e3t = ', fse3t(jip,jjp,K), '  e3w = ', fse3w(jip,jjp,jk), &
!            &            '   wsbio = ', wsbio3(jip,jjp,jk), '  wsbio4 = ', wsbio4(jip,jjp,jk)
!      ENDDO
!      write(numout,*) ' jk = ', jpk+1,'                              depw = ' ,fsdepw(jip,jjp,jpk+1), &
!            &   '                              e3w = ', fse3w(jip,jjp,jpk+1)
!      write(numout,*)

!   INITIALIZE TO ZERO ALL THE SINKING ARRAYS
!   -----------------------------------------

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               sinking (ji,jj,jk) = 0.e0
               sinking2(ji,jj,jk) = 0.e0
               sinkcal (ji,jj,jk) = 0.e0
               sinkfer (ji,jj,jk) = 0.e0
               sinksil (ji,jj,jk) = 0.e0
               sinkfer2(ji,jj,jk) = 0.e0
            END DO
         END DO
      END DO

!   Compute the sedimentation term using p4zsink2 for all
!   the sinking particles
!   -----------------------------------------------------
     DO jit = 1, iiter1
        CALL p4z_sink2( wsbio3, sinking , jppoc, iiter1 )
        CALL p4z_sink2( wsbio3, sinkfer , jpsfe, iiter1 )
      END DO

      DO jit = 1, iiter2
        CALL p4z_sink2( wsbio4, sinking2, jpgoc, iiter2 )
        CALL p4z_sink2( wsbio4, sinkfer2, jpbfe, iiter2 )
        CALL p4z_sink2( wsbio4, sinksil , jpdsi, iiter2 )
        CALL p4z_sink2( wscal , sinkcal , jpcal, iiter2 )
      END DO


!  Exchange between organic matter compartments due to
!  coagulation/disaggregation
!  ---------------------------------------------------


      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               zfact = xstep * xdiss(ji,jj,jk)
               !  Part I : Coagulation dependent on turbulence
               zagg1 = 940.* zfact * trn(ji,jj,K,jppoc) * trn(ji,jj,K,jppoc)
               zagg2 = 1.054e4 * zfact * trn(ji,jj,K,jppoc) * trn(ji,jj,K,jpgoc)

               ! Part II : Differential settling

               !  Aggregation of small into large particles
               zagg3 = 0.66 * xstep * trn(ji,jj,K,jppoc) * trn(ji,jj,K,jpgoc)
               zagg4 = 0.e0 * xstep * trn(ji,jj,K,jppoc) * trn(ji,jj,K,jppoc)

               zagg   = zagg1 + zagg2 + zagg3 + zagg4
               zaggfe = zagg * trn(ji,jj,K,jpsfe) / ( trn(ji,jj,K,jppoc) + rtrn )

               ! Aggregation of DOC to small particles
               zaggdoc = ( 80.* trn(ji,jj,K,jpdoc)         &
                  &       + 698. * trn(ji,jj,K,jppoc) )    &
                  &      *  zfact * trn(ji,jj,K,jpdoc)
               zaggdoc2 = 1.05e4 * zfact * trn(ji,jj,K,jpgoc) * trn(ji,jj,K,jpdoc)

               !  Update the trends
               tra(ji,jj,jk,jppoc) = tra(ji,jj,jk,jppoc) - zagg + zaggdoc
               tra(ji,jj,jk,jpgoc) = tra(ji,jj,jk,jpgoc) + zagg + zaggdoc2
               tra(ji,jj,jk,jpsfe) = tra(ji,jj,jk,jpsfe) - zaggfe
               tra(ji,jj,jk,jpbfe) = tra(ji,jj,jk,jpbfe) + zaggfe
               tra(ji,jj,jk,jpdoc) = tra(ji,jj,jk,jpdoc) - zaggdoc - zaggdoc2
               !
            END DO
         END DO
      END DO

#if defined key_trc_diaadd
      zrfact2 = 1.e3 * rfact2r
      ik1  = ik100 + 1
      DO jj = JRANGE
         DO ji = IRANGE
            zmsk = zrfact2 * tmask(ji,jj,KSURF)
            trc2d(ji,jj,jp_sinkco2) = ( sinking(ji,jj,ik1) + sinking2(ji,jj,ik1) ) * zmsk ! export of carbon at 100m
            trc2d(ji,jj,jp_sinkfer) = ( sinkfer(ji,jj,ik1) + sinkfer2(ji,jj,ik1) ) * zmsk ! export of biogenic iron
            trc2d(ji,jj,jp_sinkcal) =   sinkcal(ji,jj,ik1)  * zmsk   ! export of calcite
            trc2d(ji,jj,jp_sinksil) =   sinksil(ji,jj,ik1)  * zmsk   ! export of biogenic silica
         END DO
      END DO
#endif

      !
       IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('sink')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc( charout, ltra='tra')
!         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
       ENDIF

   END SUBROUTINE p4z_sink


   SUBROUTINE p4z_sink2( pwsink, psinkflx, jp_tra, kiter )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_sink2  ***
      !!
      !! ** Purpose :   Compute the sedimentation terms for the various sinking
      !!     particles. The scheme used to compute the trends is based
      !!     on MUSCL.
      !!
      !! ** Method  : - this ROUTINE compute not exactly the advection but the
      !!      transport term, i.e.  div(u*tra).
      !!---------------------------------------------------------------------
#ifdef AGRIF
      USE ocean2pisces
#endif
      INTEGER , INTENT(in   )                         ::   jp_tra    ! tracer index index      
      INTEGER , INTENT(in   )                         ::   kiter     ! number of iterations for time-splitting 
      REAL(wp), INTENT(in   ), DIMENSION(PRIV_2D_BIOARRAY, jpk) ::   pwsink    ! sinking speed
      REAL(wp), INTENT(inout), DIMENSION(PRIV_2D_BIOARRAY, jpk) ::   psinkflx  ! sinking fluxe
      !!
      INTEGER  ::   ji, jj, jk, jn, jnt
      REAL(wp) ::   zigma,zew,zign, zflx, zstep
      REAL(wp), DIMENSION(PRIV_3D_BIOARRAY) ::  ztraz
      REAL(wp), DIMENSION(PRIV_3D_BIOARRAY) ::  zwsink2
      REAL(wp), DIMENSION(PRIV_3D_BIOARRAY) ::  zakz, ztrn, ztmp, zdept, zmask
      !!---------------------------------------------------------------------

      zstep = rfact2 / FLOAT( kiter ) / 2.

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               zmask(ji,jj,jk) = tmask(ji,jj,K)
               zdept(ji,jj,jk) = fse3t(ji,jj,K)
               ztrn (ji,jj,jk) = trn  (ji,jj,K,jp_tra)
           END DO
         END DO
      ENDDO

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               ztraz(ji,jj,jk) = 0.e0
               zakz (ji,jj,jk) = 0.e0
               ztmp(ji,jj,jk)  = ztrn(ji,jj,jk)
           END DO
         END DO
      END DO

!      DO jk = KRANGE-1
      DO jk = 2, N-1
         DO jj = JRANGE
            DO ji = IRANGE
               zwsink2(ji,jj,jk+1) = -pwsink(ji,jj,jk) / rday * zmask(ji,jj,jk+1)
           END DO
         END DO
      END DO
      !
      DO jj = JRANGE
         DO ji = IRANGE
            zwsink2(ji,jj,1)   = 0.
            zwsink2(ji,jj,jpk) = 0.
         END DO
      END DO
      !
      ! Vertical advective flux
      DO jnt = 1, 2
         !  first guess of the slopes interior values
!         DO jk = KRANGEL
         DO jk = 2, N-1
            DO jj = JRANGE
               DO ji = IRANGE
                  ztraz(ji,jj,jk) = ( ztrn(ji,jj,jk-1) - ztrn(ji,jj,jk) ) * zmask(ji,jj,jk)
              END DO
            END DO
         END DO
         DO jj = JRANGE
            DO ji = IRANGE
               ztraz(ji,jj,1  ) = 0.0
               ztraz(ji,jj,jpk) = 0.0
            END DO
         END DO


         ! slopes
!         DO jk = KRANGEL
         DO jk = 2, N-1
            DO jj = JRANGE
               DO ji = IRANGE
                  zign = 0.25 + SIGN( 0.25, ztraz(ji,jj,jk) * ztraz(ji,jj,jk+1) )
                  zakz(ji,jj,jk) = ( ztraz(ji,jj,jk) + ztraz(ji,jj,jk+1) ) * zign
               END DO
            END DO
         END DO
         
         ! Slopes limitation
!         DO jk = KRANGEL
         DO jk = 2, N-1
            DO jj = JRANGE
               DO ji = IRANGE
                  zakz(ji,jj,jk) = SIGN( 1., zakz(ji,jj,jk) ) *        &
                     &             MIN( ABS( zakz(ji,jj,jk) ), 2. * ABS(ztraz(ji,jj,jk+1)), 2. * ABS(ztraz(ji,jj,jk) ) )
               END DO
            END DO
         END DO

         
         ! vertical advective flux
!         DO jk = KRANGE
         DO jk = 1, N-1
            DO jj = JRANGE   
               DO ji = IRANGE  
                  zigma = zwsink2(ji,jj,jk+1) * zstep / fse3w(ji,jj,jk+1)
                  zew   = zwsink2(ji,jj,jk+1)
                  psinkflx(ji,jj,jk+1) = -zew * ( ztrn(ji,jj,jk) - 0.5 * ( 1 + zigma ) * zakz(ji,jj,jk) ) * zstep 
               END DO
            END DO
         END DO
         !
         ! Boundary conditions
         DO jj = JRANGE
            DO ji = IRANGE
               psinkflx(ji,jj,1  ) = 0.e0
               psinkflx(ji,jj,jpk) = 0.e0
            END DO
         END DO
         

!         DO jk = KRANGE
         DO jk = 1, N-1
            DO jj = JRANGE
               DO ji = IRANGE
                  zflx = ( psinkflx(ji,jj,jk) - psinkflx(ji,jj,jk+1) ) / zdept(ji,jj,jk)
                  ztrn(ji,jj,jk) = ztrn(ji,jj,jk) + zflx
               END DO
            END DO
         END DO


      ENDDO

!      DO jk = KRANGE
      DO jk = 1, N-1
         DO jj = JRANGE
            DO ji = IRANGE
               zflx = ( psinkflx(ji,jj,jk) - psinkflx(ji,jj,jk+1) ) / zdept(ji,jj,jk)
               ztmp(ji,jj,jk) = ztmp(ji,jj,jk) + 2. * zflx
            END DO
         END DO
      END DO

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               trn(ji,jj,K,jp_tra) = ztmp(ji,jj,jk)
               psinkflx(ji,jj,jk)   = 2. * psinkflx(ji,jj,jk)
            END DO
         END DO
      END DO

      !
   END SUBROUTINE p4z_sink2

   INTEGER FUNCTION p4z_sink_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_sink_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( wsbio3 (PRIV_3D_BIOARRAY) , wsbio4  (PRIV_3D_BIOARRAY) ,                 & 
         &      wscal  (PRIV_3D_BIOARRAY) ,     &
         &      sinking(PRIV_2D_BIOARRAY,jpk+1) , sinking2(PRIV_2D_BIOARRAY,jpk+1) ,     &
         &      sinkcal(PRIV_2D_BIOARRAY,jpk+1) , sinksil (PRIV_2D_BIOARRAY,jpk+1) ,     &
#if defined key_kriest
         &      xnumm(jpk) ,     &
#else
         &      sinkfer2(PRIV_2D_BIOARRAY,jpk+1) ,     &
#endif
         &      sinkfer (PRIV_2D_BIOARRAY,jpk+1) , STAT=p4z_sink_alloc )
         !
      IF( p4z_sink_alloc /= 0 ) CALL ctl_warn('p4z_sink_alloc : failed to allocate arrays.')
      !
   END FUNCTION p4z_sink_alloc

#else
   !!======================================================================
   !!  Dummy module :                                   No PISCES bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE p4z_sink                    ! Empty routine
   END SUBROUTINE p4z_sink
#endif 

   !!======================================================================
END MODULE  p4zsink
