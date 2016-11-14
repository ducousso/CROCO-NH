! $Id: init_arrays_floats.F 1458 2014-02-03 15:01:25Z gcambon $
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
#ifdef FLOATS
      subroutine init_arrays_floats

      implicit none
#include "param.h"
#include "floats.h" 
#include "scalars.h"

      integer iflt, j, i
!
!
!  Initialize floats trajectory array.
!
      do iflt=1,Mfloats
        fltgrd(iflt)=-1 ! -1 is the dead float value. 
        nrecvel(iflt)=0

        do j=igrd,izgrd  
           Tinfo(j,iflt)=flospval
        enddo
        
        do j=1,NDIAGS
        trackaux(j,iflt)=0.
        enddo

        do j=0,NFT
          do i=1,NFV
            track(i,j,iflt)=flospval
          enddo
        enddo

      enddo

# ifdef AGRIF
      do j=1,maxgrids   ! consistent with initialization of nfp1
        floattindex(j)=0
      enddo
# endif
      return
      end

#else
      subroutine init_arrays_floats_empty
      return
      end

#endif /* FLOATS */

