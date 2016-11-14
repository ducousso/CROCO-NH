#include "cppdefs.h"

MODULE p4zrem
   !!======================================================================
   !!                         ***  MODULE p4zrem  ***
   !! TOP :   PISCES Compute remineralization/scavenging of organic compounds
   !!======================================================================
   !! History :   1.0  !  2004     (O. Aumont) Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!----------------------------------------------------------------------
#if defined key_pisces
   !!----------------------------------------------------------------------
   !!   'key_top'       and                                      TOP models
   !!   'key_pisces'                                       PISCES bio-model
   !!----------------------------------------------------------------------
   !!   p4z_rem       :   Compute remineralization/scavenging of organic compounds
   !!----------------------------------------------------------------------
   USE sms_pisces      ! 
   USE p4zint
   USE p4zopt
   USE p4zmeso
   USE p4zprod
   USE p4zche

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_rem    ! called in p4zbio.F90
   PUBLIC   p4z_rem_alloc 
   PUBLIC   p4z_rem_init 
   PUBLIC   p4z_rem_nam 


   !!* Substitution
#  include "ocean2pisces.h90"
#  include "top_substitute.h90"

   !! * Shared module variables
   REAL(wp), PUBLIC ::   &
     xremik  = 0.3      ,  & !:
     xremip  = 0.025    ,  & !:
     nitrif  = 0.05     ,  & !:
     xsirem  = 0.015    ,  & !:
     xlam1   = 0.005    ,  & !:
     oxymin  = 1.e-6         !:

   REAL(wp), PUBLIC, DIMENSION(:,:,:), ALLOCATABLE, SAVE ::   denitr       !: denitrification array

   !!----------------------------------------------------------------------
   !! NEMO/TOP 2.0 , LOCEAN-IPSL (2007) 
   !! $Id: p4zrem.F90 1808 2010-03-11 09:17:56Z cetlod $ 
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE p4z_rem(kt, jnt)
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_rem  ***
      !!
      !! ** Purpose :   Compute remineralization/scavenging of organic compounds
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt, jnt ! ocean time step
      INTEGER  ::   ji, jj, jk
      REAL(wp) ::   zremip, zremik , zlam1b
      REAL(wp) ::   zkeq  , zfeequi, zsiremin
      REAL(wp) ::   zsatur, zsatur2, znusil
      REAL(wp) ::   zbactfer, zorem, zorem2, zofer
      REAL(wp) ::   zosil, zdenom1, zscave, zaggdfe
#if ! defined key_kriest
      REAL(wp) ::   zofer2, zdenom, zdenom2
#endif
      REAL(wp) ::   zlamfac, zrfact2, zmsk
      REAL(wp), DIMENSION(PRIV_2D_BIOARRAY) :: ztempbac
      REAL(wp), DIMENSION(PRIV_3D_BIOARRAY) :: zdepbac, zfesatur, zolimi, zonitr
      CHARACTER (len=25) :: charout

      !!---------------------------------------------------------------------


       ! Initialisation of temprary arrys
      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               zdepbac (ji,jj,jk) = 0.0
               zfesatur(ji,jj,jk) = 0.0
               zolimi  (ji,jj,jk) = 0.0
               zfesatur(ji,jj,jk) = 0.6e-9
            END DO
         END DO
      END DO
      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               ztempbac(ji,jj)   = 0.0
            END DO
         END DO
      END DO

!      Computation of the mean phytoplankton concentration as
!      a crude estimate of the bacterial biomass
!      --------------------------------------------------

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               IF( fsdept(ji,jj,K) < 120. ) THEN
                  zdepbac(ji,jj,jk) = MIN( 0.7 * ( trn(ji,jj,K,jpzoo)   &
                     &               + 2.* trn(ji,jj,K,jpmes) ), 4.e-6 )
                  ztempbac(ji,jj)   = zdepbac(ji,jj,jk)
               ELSE
                  zdepbac(ji,jj,jk) = MIN( 1., 120./ fsdept(ji,jj,K) ) * ztempbac(ji,jj)
               ENDIF
            END DO
         END DO
      END DO

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE

!    DENITRIFICATION FACTOR COMPUTED FROM O2 LEVELS
!    ----------------------------------------------

               nitrfac(ji,jj,jk) = MAX(  0.e0, 0.4 * ( 6.e-6  - trn(ji,jj,K,jpoxy) )    &
                  &                                / ( oxymin + trn(ji,jj,K,jpoxy) )  )
               !
               nitrfac(ji,jj,jk) = MIN( 1., nitrfac(ji,jj,jk) )
              !
            END DO
         END DO
      END DO


      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE

!     DOC ammonification. Depends on depth, phytoplankton biomass
!     and a limitation term which is supposed to be a parameterization
!     of the bacterial activity. 
!     ----------------------------------------------------------------
               zremik = xremik * xstep / 1.e-6 * xlimbac(ji,jj,jk)         &
# if defined key_off_degrad
                  &            * facvol(ji,jj,jk)              &
# endif
                  &            * zdepbac(ji,jj,jk)
               zremik = MAX( zremik, 5.5e-4 * xstep )

!     Ammonification in oxic waters with oxygen consumption
!     -----------------------------------------------------
               zolimi(ji,jj,jk) = MIN(  ( trn(ji,jj,K,jpoxy) - rtrn ) / o2ut,  &
                  &                    zremik * ( 1.- nitrfac(ji,jj,jk) ) * trn(ji,jj,K,jpdoc)  ) 

!     Ammonification in suboxic waters with denitrification
!     -------------------------------------------------------
               denitr(ji,jj,jk) = MIN(  ( trn(ji,jj,K,jpno3) - rtrn ) / rdenit,   &
                  &                     zremik * nitrfac(ji,jj,jk) * trn(ji,jj,K,jpdoc)  )
            END DO
         END DO
      END DO

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               zolimi (ji,jj,jk) = MAX( 0.e0, zolimi (ji,jj,jk) )
               denitr (ji,jj,jk) = MAX( 0.e0, denitr (ji,jj,jk) )
            END DO
         END DO
      END DO

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE

!    NH4 nitrification to NO3. Ceased for oxygen concentrations
!    below 2 umol/L. Inhibited at strong light 
!    ----------------------------------------------------------
               zonitr(ji,jj,jk)  = nitrif * xstep * trn(ji,jj,K,jpnh4)  &
                  &            / ( 1.+ emoy(ji,jj,jk) )     &
# if defined key_off_degrad
                  &           * facvol(ji,jj,jk)              &
# endif
                  &           * ( 1.- nitrfac(ji,jj,jk) )

!   Update of the tracers trends
!   ----------------------------

              tra(ji,jj,jk,jpnh4) = tra(ji,jj,jk,jpnh4) - zonitr(ji,jj,jk)
              tra(ji,jj,jk,jpno3) = tra(ji,jj,jk,jpno3) + zonitr(ji,jj,jk)
              tra(ji,jj,jk,jpoxy) = tra(ji,jj,jk,jpoxy) - o2nit * zonitr(ji,jj,jk)
              tra(ji,jj,jk,jptal) = tra(ji,jj,jk,jptal) - rno3  * zonitr(ji,jj,jk)

            END DO
         END DO
      END DO

       IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('rem1')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc( charout, ltra='tra')
!         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
       ENDIF

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE

!    Bacterial uptake of iron. No iron is available in DOC. So
!    Bacteries are obliged to take up iron from the water. Some
!    studies (especially at Papa) have shown this uptake to be
!    significant
!    ----------------------------------------------------------
               zbactfer = 15.e-6 * rfact2 * 4.* 0.4 * prmax(ji,jj,jk)           &
                  &               * ( xlimphy(ji,jj,jk) * zdepbac(ji,jj,jk))**2           &
                  &                  / ( xkgraz2 + zdepbac(ji,jj,jk) )                    &
                  &                  * ( 0.5 + SIGN( 0.5, trn(ji,jj,K,jpfer) -2.e-11 )  )

               tra(ji,jj,jk,jpfer) = tra(ji,jj,jk,jpfer) - zbactfer
#if defined key_kriest
               tra(ji,jj,jk,jpsfe) = tra(ji,jj,jk,jpsfe) + zbactfer
#else
               tra(ji,jj,jk,jpbfe) = tra(ji,jj,jk,jpbfe) + zbactfer
#endif

            END DO
         END DO
      END DO

       IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('rem2')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc( charout, ltra='tra')
!         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
       ENDIF

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE

!    POC disaggregation by turbulence and bacterial activity. 
!    -------------------------------------------------------------
               zremip = xremip * xstep * tgfunc(ji,jj,jk)   &
# if defined key_off_degrad
                  &            * facvol(ji,jj,K)              &
# endif
                  &            * ( 1.- 0.5 * nitrfac(ji,jj,jk) )

!    POC disaggregation rate is reduced in anoxic zone as shown by
!    sediment traps data. In oxic area, the exponent of the martin s
!    law is around -0.87. In anoxic zone, it is around -0.35. This
!    means a disaggregation constant about 0.5 the value in oxic zones
!    -----------------------------------------------------------------
               zorem  = zremip * trn(ji,jj,K,jppoc)
               zofer  = zremip * trn(ji,jj,K,jpsfe)
#if ! defined key_kriest
               zorem2 = zremip * trn(ji,jj,K,jpgoc)
               zofer2 = zremip * trn(ji,jj,K,jpbfe)
#else
               zorem2 = zremip * trn(ji,jj,K,jpnum)
#endif

!  Update the appropriate tracers trends
!  -------------------------------------

               tra(ji,jj,jk,jpdoc) = tra(ji,jj,jk,jpdoc) + zorem
               tra(ji,jj,jk,jpfer) = tra(ji,jj,jk,jpfer) + zofer
#if defined key_kriest
               tra(ji,jj,jk,jppoc) = tra(ji,jj,jk,jppoc) - zorem
               tra(ji,jj,jk,jpnum) = tra(ji,jj,jk,jpnum) - zorem2
               tra(ji,jj,jk,jpsfe) = tra(ji,jj,jk,jpsfe) - zofer
#else
               tra(ji,jj,jk,jppoc) = tra(ji,jj,jk,jppoc) + zorem2 - zorem
               tra(ji,jj,jk,jpgoc) = tra(ji,jj,jk,jpgoc) - zorem2
               tra(ji,jj,jk,jpsfe) = tra(ji,jj,jk,jpsfe) + zofer2 - zofer
               tra(ji,jj,jk,jpbfe) = tra(ji,jj,jk,jpbfe) - zofer2
#endif

            END DO
         END DO
      END DO

       IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('rem3')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc( charout, ltra='tra')
!         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
       ENDIF

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE

!     Remineralization rate of BSi depedant on T and saturation
!     ---------------------------------------------------------
               zsatur  = ( sio3eq(ji,jj,jk) - trn(ji,jj,K,jpsil) ) / ( sio3eq(ji,jj,jk) + rtrn )
               zsatur  = MAX( rtrn, zsatur )
               zsatur2 = zsatur * ( 1. + tsn(ji,jj,K,jp_tem) / 400.)**4
               znusil  = 0.225  * ( 1. + tsn(ji,jj,K,jp_tem) / 15.) * zsatur + 0.775 * zsatur2**9
#    if defined key_off_degrad
               zsiremin = xsirem * xstep * znusil * facvol(ji,jj,jk)
# else
               zsiremin = xsirem * xstep * znusil
#    endif
               zosil = zsiremin * trn(ji,jj,K,jpdsi)

               tra(ji,jj,jk,jpdsi) = tra(ji,jj,jk,jpdsi) - zosil
               tra(ji,jj,jk,jpsil) = tra(ji,jj,jk,jpsil) + zosil

               !
            END DO
         END DO
      END DO

      IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('rem4')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc( charout, ltra='tra')
!         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
       ENDIF

!CDIR NOVERRCHK
      DO jk = KRANGE
!CDIR NOVERRCHK
         DO jj = JRANGE
!CDIR NOVERRCHK
            DO ji = IRANGE
!
!      Compute de different ratios for scavenging of iron
!      --------------------------------------------------

#if  defined key_kriest
                zdenom1 = trn(ji,jj,K,jppoc) / &
           &           ( trn(ji,jj,K,jppoc) + trn(ji,jj,K,jpdsi) + trn(ji,jj,K,jpcal) + rtrn )
#else
                zdenom = 1. / ( trn(ji,jj,K,jppoc)   &
           &                  + trn(ji,jj,K,jpgoc)   &
           &                  + trn(ji,jj,K,jpdsi)   &
           &                  + trn(ji,jj,K,jpcal) + rtrn )

                zdenom1 = trn(ji,jj,K,jppoc) * zdenom
                zdenom2 = trn(ji,jj,K,jpgoc) * zdenom
#endif


!     scavenging rate of iron. this scavenging rate depends on the
!     load in particles on which they are adsorbed. The
!     parameterization has been taken from studies on Th
!     ------------------------------------------------------------
               zkeq = fekeq(ji,jj,jk)
               zfeequi = ( -( 1. + zfesatur(ji,jj,jk) * zkeq - zkeq   &
                  &        * trn(ji,jj,K,jpfer) )               &
                  &        + SQRT( ( 1. + zfesatur(ji,jj,jk) * zkeq - zkeq  &
                  &               * trn(ji,jj,K,jpfer) )**2       &
                  &               + 4. * trn(ji,jj,K,jpfer) * zkeq) )  &
                  &                / ( 2. * zkeq )

#if defined key_kriest
               zlam1b = 3.e-5 + xlam1 * (  trn(ji,jj,K,jppoc)                   &
                  &                      + trn(ji,jj,K,jpcal) + trn(ji,jj,K,jpdsi)  ) * 1.e6
#else
               zlam1b = 3.e-5 + xlam1 * (  trn(ji,jj,K,jppoc)   &
                  &                      + trn(ji,jj,K,jpgoc)   &
                  &                      + trn(ji,jj,K,jpcal)   &
                  &                      + trn(ji,jj,K,jpdsi)  ) * 1.e6
#endif

# if defined key_off_degrad
               zscave = zfeequi * zlam1b * xstep  * facvol(ji,jj,jk)
# else
               zscave = zfeequi * zlam1b * xstep
# endif

!  Increased scavenging for very high iron concentrations
!  found near the coasts due to increased lithogenic particles
!  and let s say it unknown processes (precipitation, ...)
!  -----------------------------------------------------------
               zlamfac = MAX( 0.e0, ( gphit(ji,jj) + 55.) / 30. )
               zlamfac = MIN( 1.  , zlamfac )
#if ! defined key_kriest
               zlam1b = (  80.* ( trn(ji,jj,K,jpdoc) + 35.e-6 )    &
                  &     + 698.*   trn(ji,jj,K,jppoc)               &
                  &     + 1.05e4 * trn(ji,jj,K,jpgoc)  )           &
                  &   * xdiss(ji,jj,jk) + 1E-4 * (1.-zlamfac)                &
                  &   + xlam1 * MAX( 0.e0, ( trn(ji,jj,K,jpfer) * 1.e9 - 1.)  )
#else
               zlam1b = (  80.* (trn(ji,jj,K,jpdoc) + 35E-6)           &
                  &     + 698.*  trn(ji,jj,K,jppoc)  )                    &
                  &   * xdiss(ji,jj,jk) + 1E-4 * (1.-zlamfac)           &
                  &   + xlam1 * MAX( 0.e0, ( trn(ji,jj,K,jpfer) * 1.e9 - 1.)  )
#endif

# if defined key_off_degrad
               zaggdfe = zlam1b * xstep * 0.5 * ( trn(ji,jj,K,jpfer) - zfeequi ) * facvol(ji,jj,jk)
# else
               zaggdfe = zlam1b * xstep * 0.5 * ( trn(ji,jj,K,jpfer) - zfeequi )
# endif

               tra(ji,jj,jk,jpfer) = tra(ji,jj,jk,jpfer) - zscave - zaggdfe

#if defined key_kriest
               tra(ji,jj,jk,jpsfe) = tra(ji,jj,jk,jpsfe) + zscave * zdenom1
#else
               tra(ji,jj,jk,jpsfe) = tra(ji,jj,jk,jpsfe) + zscave * zdenom1
               tra(ji,jj,jk,jpbfe) = tra(ji,jj,jk,jpbfe) + zscave * zdenom2
#endif

            END DO
         END DO
      END DO
      !

       IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('rem5')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc( charout, ltra='tra')
 !        CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
       ENDIF

!     Update the arrays TRA which contain the biological sources and sinks
!     --------------------------------------------------------------------

      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               tra(ji,jj,jk,jppo4) = tra(ji,jj,jk,jppo4) + zolimi(ji,jj,jk) + denitr(ji,jj,jk)
               tra(ji,jj,jk,jpnh4) = tra(ji,jj,jk,jpnh4) + zolimi(ji,jj,jk) + denitr(ji,jj,jk)
               tra(ji,jj,jk,jpno3) = tra(ji,jj,jk,jpno3) - denitr(ji,jj,jk) * rdenit
               tra(ji,jj,jk,jpdoc) = tra(ji,jj,jk,jpdoc) - zolimi(ji,jj,jk) - denitr(ji,jj,jk)
               tra(ji,jj,jk,jpoxy) = tra(ji,jj,jk,jpoxy) - zolimi(ji,jj,jk) * o2ut
               tra(ji,jj,jk,jpdic) = tra(ji,jj,jk,jpdic) + zolimi(ji,jj,jk) + denitr(ji,jj,jk)
               tra(ji,jj,jk,jptal) = tra(ji,jj,jk,jptal) + denitr(ji,jj,jk) * rno3 * rdenit
           END DO
        END DO
     END DO

# if defined key_trc_diaadd
     zrfact2 = 1.e3 * rfact2r
     DO jk = KRANGE
        DO jj = JRANGE
          DO ji = IRANGE
             zmsk = zrfact2 * tmask(ji,jj,K)
             trc3d(ji,jj,K,jp_nitrifo2) = (-o2ut)  * zolimi(ji,jj,jk) * zmsk ! O2 consumption by nitrification
             trc3d(ji,jj,K,jp_remino2)  = (-o2nit) * zonitr(ji,jj,jk) * zmsk ! O2 consumption by remin
         END DO
        END DO
      END DO
#endif

       IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('rem6')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc( charout, ltra='tra')
!         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
       ENDIF

   END SUBROUTINE p4z_rem

   SUBROUTINE p4z_rem_nam

      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_rem_init  ***
      !!
      !! ** Purpose :   Initialization of remineralization parameters
      !!
      !! ** Method  :   Read the nampisrem namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist nampisrem
      !!
      !!----------------------------------------------------------------------

      NAMELIST/nampisrem/ xremik, xremip, nitrif, xsirem, xlam1, oxymin

      REWIND( numnatp )                     ! read numnatp
      READ  ( numnatp, nampisrem )

      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' '
         WRITE(numout,*) ' Namelist parameters for remineralization, nampisrem'
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '    remineralisation rate of POC              xremip    =', xremip
         WRITE(numout,*) '    remineralization rate of DOC              xremik    =', xremik
         WRITE(numout,*) '    remineralization rate of Si               xsirem    =', xsirem
         WRITE(numout,*) '    scavenging rate of Iron                   xlam1     =', xlam1
         WRITE(numout,*) '    NH4 nitrification rate                    nitrif    =', nitrif
         WRITE(numout,*) '    halk saturation constant for anoxia       oxymin    =', oxymin
      ENDIF

      !
   END SUBROUTINE p4z_rem_nam

   SUBROUTINE p4z_rem_init

      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_rem_init  ***
      !!
      !! ** Purpose :   Initialization of remineralization parameters
      !!
      !! ** Method  :   Read the nampisrem namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !!
      !! ** input   :   Namelist nampisrem
      !!
      !!----------------------------------------------------------------------
      INTEGER :: ji, jj, jk

      !
      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               nitrfac(ji,jj,jk) = 0.e0
               denitr (ji,jj,jk) = 0.e0
            END DO
         END DO
      END DO


   END SUBROUTINE p4z_rem_init


   INTEGER FUNCTION p4z_rem_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_rem_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( denitr(PRIV_3D_BIOARRAY), STAT=p4z_rem_alloc )
      !
      IF( p4z_rem_alloc /= 0 )   CALL ctl_warn('p4z_rem_alloc: failed to allocate arrays')
      !
   END FUNCTION p4z_rem_alloc


#else
   !!======================================================================
   !!  Dummy module :                                   No PISCES bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE p4z_rem                    ! Empty routine
   END SUBROUTINE p4z_rem
#endif 

   !!======================================================================
END MODULE p4zrem
