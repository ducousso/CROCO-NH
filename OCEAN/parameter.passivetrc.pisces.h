! $Id: parameter.passivetrc.pisces.h 1460 2014-02-03 15:02:02Z gcambon $
!
!=========================================================================
! CROCO is a branch of ROMS developped at IRD and INRIA, in France.
! The two other branches, from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al), are under MIT/X style license.
! CROCO specific routines (nesting) are under CeCILL-C license.
!
! CROCO website : http://www.croco-ocean.org
!=========================================================================
!
CCC---------------------------------------------------------------------
CCC
CCC                         PARAMETER passivetrc.PISCES
CCC                       *******************************
CCC
CCC  purpose :
CCC  ---------
CCC     INCLUDE PARAMETER FILE for passive tracer PISCES model
CC
CC       jptra  : number of tracers
CC
      INTEGER jptra
      PARAMETER(jptra=ntrc_bio)
CC
CC number of biological trends
CC ---------------------------
CC
      INTEGER jpdiabio
      PARAMETER (jpdiabio = 1)
CC
CC    NOW ASSIGN A PARAMETER TO NAME INDIVIDUAL TRACERS
CC    WARNING: BE CAREFUL ABOUT THE ORDER WHEN READING THE RESTART FILE
CC
CC    JPDIC : dissolved inoganic carbon concentration (mol/L)
CC    JPTAL : total alkalinity (ueq/L)
CC    JPOXY : oxygen carbon concentration (mol/L)
CC    JP13C : isotope 13 of DIC (mol/L)
CC    JPCAL : calcite  concentration (mol/L)
CC    JPPO4 : phosphate concentration (mol C/L)
CC    JPPOC : particulate organic phosphate concentration (mol C/L)
CC    JPSIL : silicate concentration (mol/L)
CC    JPPHY : phytoplancton concentration (mol/L)
CC    JPZOO : zooplancton concentration (mol/L)
CC    JPDOC : dissolved organic carbon concentration (mol/L)
CC
      INTEGER jpdic,jptal,jpoxy,jpcal,jppo4,jppoc
      INTEGER jpsil,jpphy,jpzoo,jpdoc,jpgoc,jpnfe,jpnch
      INTEGER jpdia,jpmes,jpbsi,jpfer,jpbfe,jpsfe,jpdfe,jpdsi
      INTEGER jpdch,jpno3,jpnh4
      PARAMETER(jpdic=1,jptal=2,jpoxy=3,jpcal=4,jppo4=5,jppoc=6)
      PARAMETER(jpsil=7,jpphy=8,jpzoo=9,jpdoc=10)
      PARAMETER(jpdia=11,jpmes=12,jpbsi=13,jpfer=14,jpbfe=15)
      PARAMETER(jpgoc=16, jpsfe=17, jpdfe=18, jpdsi=19)
      PARAMETER(jpnfe=20, jpnch=21, jpdch=22, jpno3=23, jpnh4=24)
CC

