! $Id: init_arrays_sta.F 1458 2014-02-03 15:01:25Z gcambon $
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
#include "cppdefs.h"
#ifdef STATIONS
      subroutine init_arrays_sta

      implicit none
#include "param.h"
#include "sta.h" 
#include "scalars.h"

      integer iflt, j, i, k
!
!
!  Initialize model control points
!
      do iflt=1,Msta
c       stagrd(iflt)=-1 ! -1 is the dead float value. 
        do j=istagrd,istazgrd  
           stainfo(j,iflt)=staspval
        enddo
        do j=1,NSTAVARS
          stadata(j,iflt)=0.
        enddo
# ifdef ALL_SIGMA
        do j=istadpt,istav
         do k=1,N
           staSigm(j,iflt,k)=staspval
         enddo
        enddo
# endif
      enddo

      return
      end

#else
      subroutine init_arrays_sta_empty
      return
      end

#endif /* FLOATS */

