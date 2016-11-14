#include "cppdefs.h"

MODULE p4zflx
   !!======================================================================
   !!                         ***  MODULE p4zflx  ***
   !! TOP :   PISCES CALCULATES GAS EXCHANGE AND CHEMISTRY AT SEA SURFACE
   !!======================================================================
   !! History :    -   !  1988-07  (E. MAIER-REIMER) Original code
   !!              -   !  1998     (O. Aumont) additions
   !!              -   !  1999     (C. Le Quere) modifications
   !!             1.0  !  2004     (O. Aumont) modifications
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!----------------------------------------------------------------------
#if defined key_pisces
   !!----------------------------------------------------------------------
   !!   'key_pisces'                                       PISCES bio-model
   !!----------------------------------------------------------------------
   !!   p4z_flx       :   CALCULATES GAS EXCHANGE AND CHEMISTRY AT SEA SURFACE
   !!   p4z_flx_init  :   Read the namelist
   !!----------------------------------------------------------------------
   USE sms_pisces
   USE p4zche
#if defined key_cpl_carbon_cycle
   USE sbc_oce , ONLY :  atm_co2
#endif

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_flx  
   PUBLIC   p4z_flx_alloc
   PUBLIC   p4z_flx_init
   PUBLIC   p4z_flx_nam
   
   !!* Substitution
#include "ocean2pisces.h90"
#include "top_substitute.h90"

   REAL(wp) :: &  ! pre-industrial atmospheric [co2] (ppm) 	
      atcox  = 0.20946 ,    &  !:
      atcco2 = 278.            !:

   REAL(wp) :: &
      xconv  = 0.01/3600      !: coefficients for conversion 

   REAL(wp), PUBLIC, DIMENSION(:,:), ALLOCATABLE, SAVE ::  oce_co2, fr_i            !: ocean carbon flux 
   REAL(wp), PUBLIC, DIMENSION(:,:), ALLOCATABLE, SAVE ::  satmco2            !: atmospheric pco2
   REAL(wp)                             ::  t_oce_co2_flx      !: Total ocean carbon flux 
   REAL(wp)                             ::  t_atm_co2_flx      !: global mean of atmospheric pco2
   REAL(wp)                             ::  area               !: ocean surface

   !!----------------------------------------------------------------------
   !! NEMO/TOP 2.0 , LOCEAN-IPSL (2007) 
   !! $Id: p4zflx.F90 2604 2011-02-22 09:33:21Z cetlod $ 
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE p4z_flx ( kt, jnt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_flx  ***
      !!
      !! ** Purpose :   CALCULATES GAS EXCHANGE AND CHEMISTRY AT SEA SURFACE
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) :: kt, jnt
      INTEGER  ::   ji, jj, jrorr
      REAL(wp) ::   ztc, ztc2, ztc3, zws, zkgwan
      REAL(wp) ::   zfld, zflu, zfld16, zflu16, zfact
      REAL(wp) ::   zph, zah2, zbot, zdic, zalk, zsch_o2, zalka, zsch_co2
      REAL(wp), DIMENSION(PRIV_2D_BIOARRAY) :: zkgco2, zkgo2, zh2co3
      REAL(wp), DIMENSION(PRIV_2D_BIOARRAY) :: zcflx, zoflx, zdpco2, zdpo2, zfr_i
      CHARACTER (len=25) :: charout

      !!---------------------------------------------------------------------


      ! SURFACE CHEMISTRY (PCO2 AND [H+] IN
      !     SURFACE LAYER); THE RESULT OF THIS CALCULATION
      !     IS USED TO COMPUTE AIR-SEA FLUX OF CO2

#if defined key_cpl_carbon_cycle
      satmco2(:,:) = atm_co2(:,:)
#endif

      zfr_i (:,:) = 0.
      DO jrorr = 1, 10

!CDIR NOVERRCHK
         DO jj = JRANGE
!CDIR NOVERRCHK
            DO ji = IRANGE

               ! DUMMY VARIABLES FOR DIC, H+, AND BORATE
               zbot  = borat(ji,jj,1)
               zfact = rhop(ji,jj,1) / 1000. + rtrn
               zdic  = trn(ji,jj,KSURF,jpdic) / zfact
               zph   = MAX( hi(ji,jj,1), 1.e-10 ) / zfact
               zalka = trn(ji,jj,KSURF,jptal) / zfact

               ! CALCULATE [ALK]([CO3--], [HCO3-])
               zalk  = zalka - (  akw3(ji,jj,1) / zph - zph + zbot / ( 1.+ zph / akb3(ji,jj,1) )  )

               ! CALCULATE [H+] AND [H2CO3]
               zah2   = SQRT(  (zdic-zalk)**2 + 4.* ( zalk * ak23(ji,jj,1)   &
                  &                                        / ak13(ji,jj,1) ) * ( 2.* zdic - zalk )  )
               zah2   = 0.5 * ak13(ji,jj,1) / zalk * ( ( zdic - zalk ) + zah2 )
               zh2co3(ji,jj) = ( 2.* zdic - zalk ) / ( 2.+ ak13(ji,jj,1) / zah2 ) * zfact
               hi(ji,jj,1)   = zah2 * zfact
            END DO
         END DO
      END DO


      ! --------------
      ! COMPUTE FLUXES
      ! --------------

      ! FIRST COMPUTE GAS EXCHANGE COEFFICIENTS
      ! -------------------------------------------

!CDIR NOVERRCHK
      DO jj = JRANGE
!CDIR NOVERRCHK
         DO ji = IRANGE
            ztc  = MIN( 35., tsn(ji,jj,KSURF,jp_tem) )
            ztc2 = ztc * ztc
            ztc3 = ztc * ztc2 
            ! Compute the schmidt Number both O2 and CO2
            zsch_co2 = 2073.1 - 125.62 * ztc + 3.6276 * ztc2 - 0.043126 * ztc3
            zsch_o2  = 1953.4 - 128.0  * ztc + 3.9918 * ztc2 - 0.050091 * ztc3
            !  wind speed 
            zws  =  wndm(ji,jj) &
              &   * wndm(ji,jj)
            ! Compute the piston velocity for O2 and CO2
            zkgwan = 0.3 * zws  + 2.5 * ( 0.5246 + 0.016256 * ztc + 0.00049946  * ztc2 )
 !           zkgwan = zkgwan * xconv * ( 1.- fr_i(ji,jj) ) * tmask(ji,jj,KSURF)
            zkgwan = zkgwan * xconv * ( 1.- zfr_i(ji,jj) ) * tmask(ji,jj,KSURF)
            ! compute gas exchange for CO2 and O2
            zkgco2(ji,jj) = zkgwan * SQRT( 660./ zsch_co2 )
            zkgo2 (ji,jj) = zkgwan * SQRT( 660./ zsch_o2 )
         END DO
      END DO

      DO jj = JRANGE
         DO ji = IRANGE
            ! Compute CO2 flux for the sea and air
            zfld = satmco2(ji,jj) * tmask(ji,jj,KSURF) * chemc(ji,jj,1) * zkgco2(ji,jj)
            zflu = zh2co3(ji,jj) * tmask(ji,jj,KSURF) * zkgco2(ji,jj)
            zcflx(ji,jj) = zfld - zflu
            oce_co2(ji,jj) = zcflx(ji,jj) * rfact2 &
               &             * e1t(ji,jj) * e2t(ji,jj) * tmask(ji,jj,KSURF) * 1000.
            ! compute the trend
            tra(ji,jj,1,jpdic) = tra(ji,jj,1,jpdic) &
               &                  + ( zfld - zflu ) * rfact2 / fse3t(ji,jj,KSURF)

            ! Compute O2 flux 
            zfld16 = atcox * chemc(ji,jj,2) * tmask(ji,jj,KSURF) * zkgo2(ji,jj)
            zflu16 = trn(ji,jj,KSURF,jpoxy) * tmask(ji,jj,KSURF) * zkgo2(ji,jj)
            zoflx(ji,jj) = zfld16 - zflu16
            tra(ji,jj,1,jpoxy) = tra(ji,jj,1,jpoxy) &
               &                   + zoflx(ji,jj) * rfact2 / fse3t(ji,jj,KSURF)
         END DO
      ENDDO

      DO jj = JRANGE
         DO ji = IRANGE
            t_oce_co2_flx = t_oce_co2_flx +  oce_co2(ji,jj) * tmask_i(ji,jj)            ! Cumulative Total Flux of Carbon
         END DO
      END DO
      IF( kt == nitend ) THEN
         DO jj = JRANGE
            DO ji = IRANGE
               t_atm_co2_flx = t_atm_co2_flx + satmco2(ji,jj) * e1t(ji,jj) * e2t(ji,jj) * tmask_i(ji,jj)   ! Total atmospheric pCO2
            END DO
         END DO
         !
         IF( lk_mpp ) THEN                                                         ! sum over the global domain
           CALL mpp_sum( t_atm_co2_flx )   
           CALL mpp_sum( t_oce_co2_flx )   
         ENDIF
         t_oce_co2_flx = (-1.) * t_oce_co2_flx  * 12. / 1.e15                      ! Conversion in PgC ; negative for out of the ocean
         t_atm_co2_flx = t_atm_co2_flx  / area                                     ! global mean of atmospheric pCO2
         !
         IF( lwp) THEN
            WRITE(numout,*)
!            WRITE(numout,*) ' Global mean of atmospheric pCO2 (ppm) at it= ', kt, ' : ',t_atm_co2_flx
            WRITE(numout,*) 
            WRITE(numout,9000) ' Cumulative total Flux of Carbon out of the ocean (PgC) :   ' , t_oce_co2_flx
            WRITE(numout,*) 
         ENDIF
         !
      ENDIF
9000  FORMAT(A60,F17.14)

      IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('flx ')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc( charout, ltra='tra')
!         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
      ENDIF

#if defined key_trc_diaadd 
      DO jj = JRANGE
         DO ji = IRANGE
            ! Save diagnostics
            zdpco2(ji,jj) = ( satmco2(ji,jj) - zh2co3(ji,jj) &
             &          / ( chemc(ji,jj,1) + rtrn ) ) * tmask(ji,jj,KSURF)
            !
            trc2d(ji,jj,jp_flxco2) =  zcflx(ji,jj)  * 1000.   !  carbon flux
            trc2d(ji,jj,jp_flxo2 ) =  zoflx(ji,jj)  * 1000.   !  O2 flux
            trc2d(ji,jj,jp_kgco2 ) =  zkgco2(ji,jj)           !  gas exchange for CO2 
            trc2d(ji,jj,jp_dpco2 ) =  zdpco2(ji,jj)           ! delta pco2
         END DO
      END DO
#endif

   END SUBROUTINE p4z_flx

   SUBROUTINE p4z_flx_nam

      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_flx_init  ***
      !!
      !! ** Purpose :   Initialization of atmospheric conditions
      !!
      !! ** Method  :   Read the nampisext namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !! ** input   :   Namelist nampisext
      !!
      !!----------------------------------------------------------------------
      NAMELIST/nampisext/ atcco2

      REWIND( numnatp )                     ! read numnatp
      READ  ( numnatp, nampisext )

      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' '
         WRITE(numout,*) ' Namelist parameters for air-sea exchange, nampisext'
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '    Atmospheric pCO2      atcco2      =', atcco2
      ENDIF

   END SUBROUTINE p4z_flx_nam

   SUBROUTINE p4z_flx_init

      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p4z_flx_init  ***
      !!
      !! ** Purpose :   Initialization of atmospheric conditions
      !!
      !! ** Method  :   Read the nampisext namelist and check the parameters
      !!      called at the first timestep (nittrc000)
      !! ** input   :   Namelist nampisext
      !!
      !!----------------------------------------------------------------------

      ! interior global domain surface
      area = SUM( e1t(:,:) * e2t(:,:) * tmask_i(:,:) )
      IF( lk_mpp ) CALL mpp_sum( area )

      oce_co2(:,:) = 0.
      t_atm_co2_flx = 0.
      ! Initialisation of atmospheric pco2
      satmco2(:,:) = atcco2
      t_oce_co2_flx = 0.

   END SUBROUTINE p4z_flx_init

   INTEGER FUNCTION p4z_flx_alloc()
      !!----------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_flx_alloc  ***
      !!----------------------------------------------------------------------
      ALLOCATE( oce_co2(PRIV_2D_BIOARRAY), satmco2(PRIV_2D_BIOARRAY), STAT=p4z_flx_alloc )
      !
      IF( p4z_flx_alloc /= 0 )   CALL ctl_warn('p4z_flx_alloc : failed to allocate arrays')
      !
   END FUNCTION p4z_flx_alloc

#else
   !!======================================================================
   !!  Dummy module :                                   No PISCES bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE p4z_flx( kt )                   ! Empty routine
      INTEGER, INTENT( in ) ::   kt
      WRITE(*,*) 'p4z_flx: You should not have seen this print! error?', kt
   END SUBROUTINE p4z_flx
#endif 

   !!======================================================================
END MODULE  p4zflx
