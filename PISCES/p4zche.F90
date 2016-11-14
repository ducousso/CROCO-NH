#include "cppdefs.h"

MODULE p4zche
   !!======================================================================
   !!                         ***  MODULE p4zche  ***
   !! TOP :   PISCES Sea water chemistry computed following OCMIP protocol
   !!======================================================================
   !! History :    -   !  1988     (E. Maier-Reimer)  Original code
   !!              -   !  1998     (O. Aumont)  addition
   !!              -   !  1999     (C. Le Quere)  modification
   !!             1.0  !  2004     (O. Aumont)  modification
   !!              -   !  2006     (R. Gangsto)  modification
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!----------------------------------------------------------------------
#if defined key_pisces
   !!----------------------------------------------------------------------
   !!   'key_pisces'                                       PISCES bio-model
   !!----------------------------------------------------------------------
   !!   p4z_che        :  Sea water chemistry computed following OCMIP protocol
   !!----------------------------------------------------------------------
   USE sms_pisces

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p4z_che  


   !!* Substitution
#include "ocean2pisces.h90"
#include "top_substitute.h90"

   !! * Module variables

   REAL(wp) :: &
      salchl = 1./1.80655 ! conversion factor for salinity --> chlorinity (Wooster et al. 1969)

   REAL(wp) :: &            ! coeff. for apparent solubility equilibrium 
      akcc1 = -171.9065 , &    ! Millero et al. 1995 from Mucci 1983
      akcc2 = -0.077993 , &  
      akcc3 = 2839.319  , &  
      akcc4 = 71.595    , &  
      akcc5 = -0.77712  , &  
      akcc6 = 0.0028426 , &  
      akcc7 = 178.34    , &  
      akcc8 = -0.07711  , &  
      akcc9 = 0.0041249

   REAL(wp) :: &             ! universal gas constants
      rgas = 83.143, &
      oxyco = 1./22.4144

   REAL(wp) :: &             ! borat constants
      bor1 = 0.00023, &
      bor2 = 1./10.82

   REAL(wp) :: &              !
      ca0 = -162.8301  , &
      ca1 = 218.2968   , &
      ca2 = 90.9241    , &
      ca3 = -1.47696   , &
      ca4 = 0.025695   , &
      ca5 = -0.025225  , &
      ca6 = 0.0049867

   REAL(wp) :: &              ! coeff. for 1. dissoc. of carbonic acid (Edmond and Gieskes, 1970)   
      c10 = -3670.7   , &
      c11 = 62.008    , &
      c12 = -9.7944   , &
      c13 = 0.0118    , &
      c14 = -0.000116

   REAL(wp) :: &              ! coeff. for 2. dissoc. of carbonic acid (Millero, 1995)   
      c20 = -1394.7   , &
      c21 = -4.777    , &
      c22 = 0.0184    , &
      c23 = -0.000118

   REAL(wp) :: &             ! constants for calculate concentrations 
      st1  = 0.14     , &    ! for sulfate (Morris & Riley 1966)
      st2  = 1./96.062, &
      ks0  = 141.328  , &
      ks1  = -4276.1  , &
      ks2  = -23.093  , &
      ks3  = -13856.  , &
      ks4  = 324.57   , &
      ks5  = -47.986  , &
      ks6  = 35474.   , &
      ks7  = -771.54  , &
      ks8  = 114.723  , &
      ks9  = -2698.   , &
      ks10 = 1776.    , &
      ks11 = 1.       , &
      ks12 = -0.001005 

   REAL(wp) :: &             ! constants for calculate concentrations 
      ft1  = 0.000067   , &  ! fluorides (Dickson & Riley 1979 )
      ft2  = 1./18.9984 , &
      kf0  = -12.641    , &
      kf1  = 1590.2     , &
      kf2  = 1.525      , &
      kf3  = 1.0        , &
      kf4  =-0.001005

   REAL(wp) :: &              ! coeff. for 1. dissoc. of boric acid (Dickson and Goyet, 1994)
      cb0  = -8966.90, &
      cb1  = -2890.53, &
      cb2  = -77.942 , &
      cb3  = 1.728   , &
      cb4  = -0.0996 , &
      cb5  = 148.0248, &
      cb6  = 137.1942, &
      cb7  = 1.62142 , &
      cb8  = -24.4344, &
      cb9  = -25.085 , &
      cb10 = -0.2474 , &
      cb11 = 0.053105

   REAL(wp) :: &             ! coeff. for dissoc. of water (Dickson and Riley, 1979 )
      cw0 = -13847.26  , &
      cw1 = 148.9652   , &
      cw2 = -23.6521   , &
      cw3 = 118.67     , &
      cw4 = -5.977     , &
      cw5 = 1.0495     , &
      cw6 = -0.01615
 
   REAL(wp) :: &              ! volumetric solubility constants for o2 in ml/l (Weiss, 1974)
      ox0 = -58.3877   , &
      ox1 = 85.8079    , &
      ox2 = 23.8439    , &
      ox3 = -0.034892  , &
      ox4 = 0.015568   , &
      ox5 = -0.0019387 

   REAL(wp), DIMENSION(5)  :: &  ! coeff. for seawater pressure correction 
      devk1, devk2, devk3,    &  ! (millero 95)
      devk4, devk5

   DATA devk1 / -25.5    , -15.82    , -29.48  , -25.60     , -48.76    /   
   DATA devk2 / 0.1271   , -0.0219   , 0.1622  , 0.2324     , 0.5304    /   
   DATA devk3 / 0.       , 0.        , 2.608E-3,  -3.6246E-3, 0.        /   
   DATA devk4 / -3.08E-3 , 1.13E-3   , -2.84E-3, -5.13E-3   , -11.76E-3 /   
   DATA devk5 / 0.0877E-3, -0.1475E-3,  0.     , 0.0794E-3  , 0.3692E-3 /

   !!----------------------------------------------------------------------
   !! NEMO/TOP 2.0 , LOCEAN-IPSL (2007) 
   !! $Id: p4zche.F90 1808 2010-03-11 09:17:56Z cetlod $ 
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE p4z_che
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p4z_che  ***
      !!
      !! ** Purpose :   Sea water chemistry computed following OCMIP protocol
      !!
      !! ** Method  : - ...
      !!---------------------------------------------------------------------
      INTEGER  ::   ji, jj, jk
      REAL(wp) ::   ztkel, zsal , zqtt  , zbuf1 , zbuf2
      REAL(wp) ::   zpres, ztc  , zcl   , zcpexp, zoxy  , zcpexp2
      REAL(wp) ::   zsqrt, ztr  , zlogt , zcek1
      REAL(wp) ::   zlqtt, zqtt2, zsal15, zis   , zis2 , zisqrt
      REAL(wp) ::   zckb , zck1 , zck2  , zckw  , zak1 , zak2  , zakb , zaksp0, zakw
      REAL(wp) ::   zst  , zft  , zcks  , zckf  , zaksp1
      !!---------------------------------------------------------------------


      ! CHEMICAL CONSTANTS - SURFACE LAYER
      ! ----------------------------------
!CDIR NOVERRCHK
      DO jj = JRANGE
!CDIR NOVERRCHK
         DO ji = IRANGE

            !                             ! SET ABSOLUTE TEMPERATURE
            ztkel = tsn(ji,jj,KSURF,jp_tem) + 273.16
            zqtt  = ztkel * 0.01
            zqtt2 = zqtt * zqtt
            zsal  = tsn(ji,jj,KSURF,jp_sal) + (1.- tmask(ji,jj,KSURF) ) * 35.
            zlqtt = LOG( zqtt )

            !                             ! LN(K0) OF SOLUBILITY OF CO2 (EQ. 12, WEISS, 1980)
            !                             !     AND FOR THE ATMOSPHERE FOR NON IDEAL GAS
            zcek1 = ca0 + ca1 / zqtt + ca2 * zlqtt + ca3 * zqtt2 + zsal*( ca4 + ca5 * zqtt + ca6 * zqtt2 )

            !                             ! LN(K0) OF SOLUBILITY OF O2 and N2 (EQ. 4, WEISS, 1970)
            zoxy  = ox0 + ox1 / zqtt + ox2 * zlqtt + zsal * ( ox3 + ox4 * zqtt + ox5 * zqtt2 )

            !                             ! SET SOLUBILITIES OF O2 AND CO2
            chemc(ji,jj,1) = EXP( zcek1 ) * 1.e-6 * rhop(ji,jj,KSURF) / 1000.
            chemc(ji,jj,2) = EXP( zoxy  ) * oxyco

         END DO
      END DO

      ! CHEMICAL CONSTANTS - DEEP OCEAN
      ! -------------------------------
!CDIR NOVERRCHK
      DO jk = KRANGE
!CDIR NOVERRCHK
         DO jj = JRANGE
!CDIR NOVERRCHK
            DO ji = IRANGE

               ! SET PRESSION
               zpres   = 1.025e-1 * fsdept(ji,jj,K)

               ! SET ABSOLUTE TEMPERATURE
               ztkel   = tsn(ji,jj,K,jp_tem) + 273.16
               zqtt    = ztkel * 0.01
               zsal    = tsn(ji,jj,K,jp_sal) + ( 1.-tmask(ji,jj,K) ) * 35.
               zsqrt  = SQRT( zsal )
               zsal15  = zsqrt * zsal
               zlogt  = LOG( ztkel )
               ztr    = 1. / ztkel
               zis    = 19.924 * zsal / ( 1000.- 1.005 * zsal )
               zis2   = zis * zis
               zisqrt = SQRT( zis )
               ztc     = tsn(ji,jj,K,jp_tem) + ( 1.- tmask(ji,jj,K) ) * 20.

               ! CHLORINITY (WOOSTER ET AL., 1969)
               zcl     = zsal * salchl

               ! TOTAL SULFATE CONCENTR. [MOLES/kg soln]
               zst     = st1 * zcl * st2

               ! TOTAL FLUORIDE CONCENTR. [MOLES/kg soln]
               zft     = ft1 * zcl * ft2

               ! DISSOCIATION CONSTANT FOR SULFATES on free H scale (Dickson 1990)
               zcks    = EXP(  ks1 * ztr + ks0 + ks2 * zlogt                           &
                  &                     + ( ks3 * ztr + ks4 + ks5 * zlogt ) * zisqrt   &
                  &                     + ( ks6 * ztr + ks7 + ks8 * zlogt ) * zis      &
                  &                     + ks9 * ztr * zis * zisqrt + ks10 * ztr *zis2 + LOG( ks11 + ks12 *zsal )  )

               ! DISSOCIATION CONSTANT FOR FLUORIDES on free H scale (Dickson and Riley 79)
               zckf    = EXP(  kf1 * ztr + kf0 + kf2 * zisqrt + LOG( kf3 + kf4 * zsal )  )

               ! DISSOCIATION CONSTANT FOR CARBONATE AND BORATE
               zckb    = ( cb0 + cb1 * zsqrt + cb2  * zsal + cb3 * zsal15 + cb4 * zsal * zsal ) * ztr   &
                  &    + ( cb5 + cb6 * zsqrt + cb7  * zsal )                                            &
                  &    + ( cb8 + cb9 * zsqrt + cb10 * zsal ) * zlogt + cb11 * zsqrt * ztkel             &
                  &    + LOG(  ( 1.+ zst / zcks + zft / zckf ) / ( 1.+ zst / zcks )  )
!!gm zsal**2 to be replaced by a *...
               zck1    = c10 * ztr + c11 + c12 * zlogt + c13 * zsal + c14 * zsal**2
               zck2    = c20 * ztr + c21 + c22 * zsal   + c23 * zsal**2

               ! PKW (H2O) (DICKSON AND RILEY, 1979)
               zckw    = cw0 * ztr + cw1 + cw2 * zlogt + ( cw3 * ztr + cw4 + cw5 * zlogt ) * zsqrt + cw6 * zsal


               ! APPARENT SOLUBILITY PRODUCT K'SP OF CALCITE IN SEAWATER
               !       (S=27-43, T=2-25 DEG C) at pres =0 (atmos. pressure) (MUCCI 1983)
               zaksp0  = akcc1 + akcc2 * ztkel + akcc3 * ztr + akcc4 * LOG10( ztkel )   &
                  &   + ( akcc5 + akcc6 * ztkel + akcc7 * ztr ) * zsqrt + akcc8 * zsal + akcc9 * zsal15

               ! K1, K2 OF CARBONIC ACID, KB OF BORIC ACID, KW (H2O) (LIT.?)
               zak1    = 10**(zck1)
               zak2    = 10**(zck2)
               zakb    = EXP( zckb  )
               zakw    = EXP( zckw )
               zaksp1  = 10**(zaksp0)

               ! FORMULA FOR CPEXP AFTER EDMOND & GIESKES (1970)
               !        (REFERENCE TO CULBERSON & PYTKOQICZ (1968) AS MADE
               !        IN BROECKER ET AL. (1982) IS INCORRECT; HERE RGAS IS
               !        TAKEN TENFOLD TO CORRECT FOR THE NOTATION OF pres  IN
               !        DBAR INSTEAD OF BAR AND THE EXPRESSION FOR CPEXP IS
               !        MULTIPLIED BY LN(10.) TO ALLOW USE OF EXP-FUNCTION
               !        WITH BASIS E IN THE FORMULA FOR AKSPP (CF. EDMOND
               !        & GIESKES (1970), P. 1285-1286 (THE SMALL
               !        FORMULA ON P. 1286 IS RIGHT AND CONSISTENT WITH THE
               !        SIGN IN PARTIAL MOLAR VOLUME CHANGE AS SHOWN ON P. 1285))
               zcpexp  = zpres /(rgas*ztkel)
               zcpexp2 = zpres * zpres/(rgas*ztkel)

               ! KB OF BORIC ACID, K1,K2 OF CARBONIC ACID PRESSURE
               !        CORRECTION AFTER CULBERSON AND PYTKOWICZ (1968)
               !        (CF. BROECKER ET AL., 1982)

               zbuf1  = -(devk1(1)+devk2(1)*ztc+devk3(1)*ztc*ztc)
               zbuf2  = 0.5*(devk4(1)+devk5(1)*ztc)
               ak13(ji,jj,jk) = zak1 * EXP( zbuf1 * zcpexp + zbuf2 * zcpexp2 )

               zbuf1  =     - ( devk1(2) + devk2(2) * ztc + devk3(2) * ztc * ztc )
               zbuf2  = 0.5 * ( devk4(2) + devk5(2) * ztc )
               ak23(ji,jj,jk) = zak2 * EXP( zbuf1 * zcpexp + zbuf2 * zcpexp2 )

               zbuf1  =     - ( devk1(3) + devk2(3) * ztc + devk3(3) * ztc * ztc )
               zbuf2  = 0.5 * ( devk4(3) + devk5(3) * ztc )
               akb3(ji,jj,jk) = zakb * EXP( zbuf1 * zcpexp + zbuf2 * zcpexp2 )

               zbuf1  =     - ( devk1(4) + devk2(4) * ztc + devk3(4) * ztc * ztc )
               zbuf2  = 0.5 * ( devk4(4) + devk5(4) * ztc )
               akw3(ji,jj,jk) = zakw * EXP( zbuf1 * zcpexp + zbuf2 * zcpexp2 )


               ! APPARENT SOLUBILITY PRODUCT K'SP OF CALCITE 
               !        AS FUNCTION OF PRESSURE FOLLOWING MILLERO
               !        (P. 1285) AND BERNER (1976)
               zbuf1  =     - ( devk1(5) + devk2(5) * ztc + devk3(5) * ztc * ztc )
               zbuf2  = 0.5 * ( devk4(5) + devk5(5) * ztc )
               aksp(ji,jj,jk) = zaksp1 * EXP( zbuf1 * zcpexp + zbuf2 * zcpexp2 )


               ! TOTAL BORATE CONCENTR. [MOLES/L]
               borat(ji,jj,jk) = bor1 * zcl * bor2

               ! Iron and SIO3 saturation concentration from ...
               sio3eq(ji,jj,jk) = EXP(  LOG( 10.) * ( 6.44 - 968. / ztkel )  ) * 1.e-6
               fekeq (ji,jj,jk) = 10**( 17.27 - 1565.7 / ( 273.15 + ztc ) )

            END DO
         END DO
      END DO
      !
   END SUBROUTINE p4z_che

#else
   !!======================================================================
   !!  Dummy module :                                   No PISCES bio-model
   !!======================================================================
CONTAINS
   SUBROUTINE p4z_che( kt )                   ! Empty routine
      INTEGER, INTENT( in ) ::   kt
      WRITE(*,*) 'p4z_che: You should not have seen this print! error?', kt
   END SUBROUTINE p4z_che
#endif 

   !!======================================================================
END MODULE  p4zche
