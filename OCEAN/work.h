! $Id: work.h 1458 2014-02-03 15:01:25Z gcambon $
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
!
! This is "work.h": declaration of utility work array.
!
#ifdef SOLVE3D
      real work(GLOBAL_2D_ARRAY,0:N)
      common /work3d/ work
      real workr(GLOBAL_2D_ARRAY,1:N)
      common /work3d_r/ workr
#endif

      real work2d(GLOBAL_2D_ARRAY)
      common /work2d/ work2d

      real work2d2(GLOBAL_2D_ARRAY)
      common /work2d2/ work2d2
