! $Id: private_scratch.h 1458 2014-02-03 15:01:25Z gcambon $
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
#if defined AUTOTILING
      real,dimension(:,:,:), pointer :: A2d, A3d
#else
      real A2d(N2d,NSA,0:NPP-1), A3d(N3d,5,0:NPP-1)
#endif
#  if defined SEDIMENT || defined LMD_MIXING
      integer B2d(N2d,0:NPP-1)
#  endif
      common /private_scratch/ A2d,A3d
#  ifdef SEDIMENT
     &       /private_scratch_bis/ B2d 
#  endif
