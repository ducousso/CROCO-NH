! $Id: ncscrum_floats.h 1458 2014-02-03 15:01:25Z gcambon $
!
!======================================================================
! CROCO is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! CROCO specific routines (nesting) are under CeCILL-C license.
! 
! CROCO website : http://www.croco-ocean.org
!======================================================================
!
! This is include file "ncscrum_floats.h".
! ==== == ======= ==== ============
! indices in character array "vname", which holds variable names
!                                                and attributes.
! indxTime        time
! indxZ           free-surface
! indxUb,indxVb   vertically integrated 2D U,V-momentum components
!
! indxU,indxV     3D U- and V-momenta.
! indxT,indxS,.., indxZoo  tracers (temperature, salinity,
!                 biological tracers.
! indxsand,silt   sand & silt sediment tracers
! indxO,indeW     omega vertical mass flux and true vertical velocity
! indxR           density anomaly
!
! indxAkv,indxAkt,indxAks  vertical viscosity/diffusivity coeffcients
! indxHbl         depth of planetary boundary layer in KPP model
!
! indxSSH         observed sea surface height (from climatology)
! indxSUSTR,indxSVSTR  surface U-, V-momentum stress (wind forcing)
! indxShflw       net surface heat flux.
! indxShflx_rsw   shortwave radiation flux
! indxSST         sea surface temperature
! indxdQdSST      Q-correction coefficient dQdSST
! indxSSS         sea surface salinity
! indxSwflx       surface fresh water flux
!
! indxAi          fraction of cell covered by ice
! indxUi,indxVi   U,V-components of sea ice velocity
! indxHi,indxHS   depth of ice cover and depth of snow cover
! indxTIsrf       temperature of ice surface
!
! indxBSD,indxBSS bottom sediment grain Density and Size 
!                 to be read from file if(!defined ANA_BSEDIM, 
!                 && !defined SEDIMENT) 
!
! indxBTHK,       sediment bed thickness, porosity, size class fractions 
! indxBPOR,indxBFRA
!
! indxWWA,indxWWD,indxWWP   wind induced wave Amplitude,
!                 Direction and Period
!
      integer indxfltGrd, indxfltTemp, indxfltSalt,
     & indxfltRho, indxfltVel
      parameter (     indxfltGrd=1, indxfltTemp=2,
     & indxfltSalt=3, indxfltRho=4,  indxfltVel=5)
      integer fltfield
      parameter(fltfield=5)

      integer ncidflt,    nrecflt,    fltGlevel
     &      , fltTstep,   fltTime,    fltXgrd,  fltYgrd
     &      , fltZgrd,    fltVel
     &      , rstnfloats, rstTinfo, rstfltgrd
     &      , rsttrack
#ifdef SPHERICAL
     &      , fltLon,     fltLat
#else
     &      , fltX,       fltY
#endif
#ifdef SOLVE3D
     &      , fltDepth,   fltDen,   fltTemp
# ifdef SALINITY
     &      , fltSal
# endif
# ifdef IBM
     &      , fltAge, fltZoe
# endif
#endif
      logical wrtflt(fltfield)

      common/incscrum_floats/
     &        ncidflt,    nrecflt,    fltGlevel
     &      , fltTstep,   fltTime,    fltXgrd,  fltYgrd
     &      , fltZgrd,    fltVel
     &      , rstnfloats, rstTinfo, rstfltgrd
     &      , rsttrack
#ifdef SPHERICAL
     &      , fltLon,     fltLat
#else
     &      , fltX,       fltY
#endif
#ifdef SOLVE3D
     &      , fltDepth,   fltDen,   fltTemp
# ifdef SALINITY
     &      , fltSal
# endif
# ifdef IBM
     &      , fltAge, fltZoe
# endif
#endif
     &      , wrtflt


      character*80  fltname,   fposnam
      common /cncscrum_floats/ fltname,   fposnam
