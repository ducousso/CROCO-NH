! $Id: private_scratch_AMR.h 1458 2014-02-03 15:01:25Z gcambon $
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
#ifdef SGI
      real A2d(N2d,NSA,0:NPP-1), A3d(N3d,4,0:NPP-1)
# ifdef SEDIMENT
      integer B2d(N2d,0:NPP-1)
# endif
      common /private_scratch_A2d/A2d
     &       /private_scratch_A3d/A3d
# ifdef SEDIMENT
     &       /private_scratch_B2d/B2d
# endif

#else

      real A2d(N2d,NSA,0:NPP-1), A3d(N3d,4,0:NPP-1)
#  ifdef SEDIMENT
      integer B2d(N2d,0:NPP-1)
#  endif    

# ifdef CRAY
      task common /private_scratch/ A2d,A3d
#  ifdef SEDIMENT
      task common /private_scratch_bis/ B2d
#  endif    

# else
      common /private_scratch/ A2d,A3d
#  ifdef SEDIMENT
      common /private_scratch_bis/ B2d
#  endif    
# endif
#endif
