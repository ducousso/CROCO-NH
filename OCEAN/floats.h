! $Id: floats.h 1458 2014-02-03 15:01:25Z gcambon $
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
#ifdef FLOATS
/*
** Include file "floats.h".
*************************************************** John M. Klinck ***
** Copyright (c) 2000 Rutgers/UCLA                                  **
************************************************* Hernan G. Arango ***
**                                                                  **
** NFT        Number of float time levels (minus one).              **
** NFV        Number of float variables.                            **
** Tinfo      Float trajectory initial information.                 **
** bounded    Float bounded status switch.                          **
** ifdpt      Index for float depth.                                **
** igrd       Index for float grid (embedding) location in track    **
**                  array (in Tinfo only)                           **
** ifden      Index for float density anomaly.                      **
** iflat      Index for float latitude location.                    **
** iflon      Index for float longitude location.                   **
** iftem      Index for float potential temperature.                **
** ifsal      Index for float salinity.                             **
** itstr      Index for float release time. (used for Tinfo only))  **
** ixgrd      Index for float x-grid location.                      **
** ixrhs      Index for float x-slope, u*dx.                        **
** iygrd      Index for float y-grid location.                      **
** iyrhs      Index for float y-slope, v*dy.                        **
** izgrd      Index for float z-grid location.                      **
** izrhs      Index for float z-slope, w*dz.                        **
** nfloats    Number of float trajectories to compute.              **
** nfm3       Float index for time level "n-3".                     **
** nfm2       Float index for time level "n-2".                     **
** nfm1       Float index for time level "n-1".                     **
** nf         Float index for time level "n".                       **
** nfp1       Float index for time level "n+1".                     **
** track      Multivariate float trajectory data at several time    **
**              time levels.                                        **
** trackaux   Multivariate float data for writing                   **
** fltgrid    Float/grid embedding correspondance array. The        **
**            indice 0 stands for the number of float trajectories  **
**            to compute at the corresponding grid level.           **
** nrecvel     Keeps track of how many records are used for an       **
**               average float field like Vel                       **
**********************************************************************
*/

      integer NFT,               NFV,               NDIAGS,
     &        igrd,              itstr,
     &        ixgrd,             iygrd,             izgrd,
     &        iflon,             iflat,             ifdpt,
     &        ixrhs,             iyrhs,             izrhs,
     &        iftem,             ifsal,             ifden, 
     &        ifvel
      parameter (NFT=3,          NFV=6  ,           NDIAGS=10,
     &        igrd=-1,           itstr=0,  
     &        ixgrd=1,           iygrd=2,           izgrd=3, ! for track 
                                                             ! & trackaux
     &        ixrhs=4,           iyrhs=5,           izrhs=6, ! for track
     &        iflon=4,           iflat=5,           ifdpt=6, ! for trackaux
     &        iftem=7,           ifsal=8,           ifden=9, 
     &        ifvel=10                                       ) 

      logical bounded(Mfloats), diagfloats
      common /lfloats/ bounded

      integer nfloats, fltgrd(Mfloats), nrecvel(Mfloats) 
      common /floatsn/ nfloats, diagfloats, nrecvel

      real Tinfo(igrd:izgrd,Mfloats)
      common /floats_info/ Tinfo

      real flospval, deltap2c, deltac2p
      common /floats_scalars/ flospval, deltap2c, deltac2p
# ifdef IBM
      integer ibmvars, ibmage, ibmzoe
      parameter (ibmvars=2, ibmage=1, ibmzoe=2)
      real ibmdata(ibmvars,Mfloats)
      common /ibm_stuff/ ibmdata
# endif
# ifdef AGRIF
      integer maxgrids
      parameter (maxgrids=10)
      integer floattindex(0:maxgrids)
# endif
      real track(1:NFV,0:NFT,Mfloats),trackaux(1:NDIAGS,Mfloats)
      common /floats_track/ track,trackaux,fltgrd
# ifdef AGRIF
     & ,floattindex
# endif

#endif /*FLOATS*/
