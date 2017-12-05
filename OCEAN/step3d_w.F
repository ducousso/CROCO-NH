! $Id: step3d_uv2.F 1480 2014-02-18 14:56:20Z rblod $
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
#if defined SOLVE3D && (defined NBQ || defined NHMG)

      subroutine step3d_w (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()

      call step3d_w_tile (Istr,Iend,Jstr,Jend, A3d(1,5,trd),
     &                                  A2d(1,1,trd), A2d(1,2,trd),
     &                                  A2d(1,3,trd), A2d(1,4,trd))

      return
      end

      subroutine step3d_w_tile (Istr,Iend,Jstr,Jend,rw,BC,CF,FC,DC)
!
!-------------------------------------------------------------------- 
!
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "coupling.h"
# include "forces.h"
# include "mixing.h"
# include "scalars.h"
# include "sources.h"
# if defined M3NUDGING && defined M3CLIMATOLOGY
#  include "climat.h"
# endif
# ifdef DIAGNOSTICS_UV
#  include "diagnostics.h"
# endif
#ifdef NBQ
# include "nbq.h"
#endif
      integer Istr,Iend,Jstr,Jend, i,j,k
# ifdef PSOURCE
     &       ,is
# endif
      real BC(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     CF(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     FC(PRIVATE_1D_SCRATCH_ARRAY,0:N), cff,
     &     DC(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     rw(PRIVATE_2D_SCRATCH_ARRAY,0:N)       
      real grad(PRIVATE_2D_SCRATCH_ARRAY)
      real dpth
!
# include "compute_auxiliary_bounds.h"
!
# ifdef MASKING
#  define SWITCH *
# else
#  define SWITCH !
# endif
!
!--------------------------------------------------------------------
!  Apply right-hand-side
!--------------------------------------------------------------------
!

!      do j=Jstr,Jend
!        do k=1,N
!          do i=Istr,Iend
!            wz(i,j,k,nnew)=wz(i,j,k,nnew)
!     &            +dt*pn(i,j)*pm(i,j)*(rw(i,j,k)
!# ifdef NBQ
!     &                                          +rw_nbq_avg2(i,j,k)
!# endif
!     &                                                             )
!          enddo
!        enddo
!      enddo

      do j=Jstr,Jend
        do k=1,N-1
          do i=Istr,Iend
            wz(i,j,k,nnew)=wz(i,j,k,nnew)
     &            +dt*pn(i,j)*pm(i,j)*rw(i,j,k)
     &            /(0.5*(Hz(i,j,k)+Hz(i,j,k+1)))
          enddo
        enddo
        do i=Istr,Iend
           wz(i,j,N,nnew)=wz(i,j,N,nnew)
     &          +dt*pn(i,j)*pm(i,j)*rw(i,N,k)
     &          /(0.5*Hz(i,j,N))
        enddo
      enddo

!care all the vert mixing part is lacking

!
!--------------------------------------------------------------------
! Set PHYSICAL lateral boundary conditions.
!--------------------------------------------------------------------
!
      call w3dbc_tile (Istr,Iend,Jstr,Jend, grad)   
!
!--------------------------------------------------------------------
! Exchange periodic boundaries and computational margins.
!--------------------------------------------------------------------
!
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend,
     &                        wz(START_2D_ARRAY,0,nnew))
# endif

      return
      end
#else
      subroutine step3d_w_empty 
      return
      end
#endif /* SOLVE3D && NBQ */
