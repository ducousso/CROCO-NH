! $Id: step3d_uv1.F 1613 2014-12-12 15:25:54Z marchesiello $
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
#ifdef SOLVE3D
      subroutine step3d_uv1 (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call step3d_uv1_tile (Istr,Iend,Jstr,Jend,  A3d(1,1,trd),
     &                              A3d(1,2,trd), A2d(1,1,trd))
      return
      end

      subroutine step3d_uv1_tile (Istr,Iend,Jstr,Jend, ru,rv,DC)
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
# include "param.h"
      real ru(PRIVATE_2D_SCRATCH_ARRAY,N),
     &     rv(PRIVATE_2D_SCRATCH_ARRAY,N),
     &     DC(PRIVATE_1D_SCRATCH_ARRAY,0:N),  cff
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
# ifdef MRL_WCI
#   include "forces.h"
# endif
#ifdef NBQ
# include "nbq.h"
#endif
!
# include "compute_auxiliary_bounds.h"
!

      cff=0.25*dt
      do j=Jstr,Jend
        do i=IstrU,Iend
          DC(i,0)=cff*(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
        enddo
        do k=1,N
          do i=IstrU,Iend
# ifdef NBQ
            u(i,j,k,nnew)=u(i,j,k,nnew)+DC(i,0)*(ru(i,j,k)+
     &                                           ru_nbq_avg2(i,j,k))

#else
            u(i,j,k,nnew)=u(i,j,k,nnew)+DC(i,0)*ru(i,j,k)
#endif
#  if defined MRL_WCI && defined MASKING
            u(i,j,k,nnew)=u(i,j,k,nnew)*umask(i,j)
     &                    +0.5*ust(i,j,k)*(Hz(i-1,j,k)+Hz(i,j,k))
     &                                          *(umask(i,j)-1.0)
#  endif
          enddo
        enddo
        if (j.ge.JstrV) then
          do i=Istr,Iend
            DC(i,0)=cff*(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
          enddo
          do k=1,N
            do i=Istr,Iend
#ifdef NBQ
              v(i,j,k,nnew)=v(i,j,k,nnew)+DC(i,0)*(rv(i,j,k)+
     &                                             rv_nbq_avg2(i,j,k))
#else
              v(i,j,k,nnew)=v(i,j,k,nnew)+DC(i,0)*rv(i,j,k)
#endif
#  if defined MRL_WCI && defined MASKING
              v(i,j,k,nnew)=v(i,j,k,nnew)*vmask(i,j)
     &                      +0.5*vst(i,j,k)*(Hz(i,j-1,k)+Hz(i,j,k))
     &                                            *(vmask(i,j)-1.0)
#  endif
            enddo
          enddo
        endif
      enddo 
#else
      subroutine step3d_uv1_empty
#endif
      return
      end
