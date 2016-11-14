! $Id: nc_sta.h 1458 2014-02-03 15:01:25Z gcambon $
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
! This is include file "nc_sta.h".
! ==== == ======= ==== ============
!
! stafield     Number of station fields for output
! wrtsta       Logical vector with flags for output
! indxsta[...] Index of logical flag to output several fields
!       Grd  - grid level
!       Temp - temp
!       Salt - Salt
!       Rho  - Density
!       Vel  - u and v components
! ncidsta      id of station output file  
! nrecsta      step to output station data
! sta[...]     several reference names of netcdf output
! staname      station output filename
! staposname   station input data filename

      integer stafield
      parameter(stafield=5)
      integer indxstaGrd, indxstaTemp, indxstaSalt,
     & indxstaRho, indxstaVel
      parameter (     indxstaGrd=1, indxstaTemp=2,
     & indxstaSalt=3, indxstaRho=4,  indxstaVel=5)
 

      integer ncidsta,    nrecsta,    staGlevel
     &      , staTstep,   staTime,    staXgrd,   staYgrd
     &      , staZgrd,    staZeta,    staU,      staV
#ifdef SPHERICAL
     &      , staLon,     staLat
#else
     &      , staX,       staY
#endif
#ifdef SOLVE3D
     &      , staDepth,   staDen,   staTemp
# ifdef SALINITY
     &      , staSal
# endif
#endif
      logical wrtsta(stafield)

      common/incscrum_sta/
     &        ncidsta,    nrecsta,    staGlevel
     &      , staTstep,   staTime,    staXgrd,   staYgrd
     &      , staZgrd,    staZeta,    staU,      staV
#ifdef SPHERICAL
     &      , staLon,     staLat
#else
     &      , staX,       staY
#endif
#ifdef SOLVE3D
     &      , staDepth,   staDen,   staTemp
# ifdef SALINITY
     &      , staSal
# endif
#endif
     &      , wrtsta


      character*80  staname,   staposname
      common /cncscrum_sta/ staname,   staposname
