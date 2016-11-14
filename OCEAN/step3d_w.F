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
#if defined SOLVE3D && defined NBQ

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
      do j=Jstr,Jend
        do k=1,N
          do i=Istr,Iend
            wz(i,j,k,nnew)=wz(i,j,k,nnew)
     &            +dt*pn(i,j)*pm(i,j)*(rw(i,j,k)+rw_nbq_avg2(i,j,k))
          enddo
        enddo
      enddo

# define NBQ_WDIF
# ifdef NBQ_WDIF
!
!====================================================================     
!== Turbulent vertical diffusion for wz
!== At this point wz contains rho.Hz.wz with units kg.m-1.s-1  
!== The implicit integration contains a division by rho.Hz
!==================================================================== 
!
      do j=Jstr,Jend
!
        do k=1,N-1
          do i=Istr,Iend
            FC(i,k)=-0.5*dt*( Akv(i,j,k+1)+Akv(i,j,k) )
     &                 /    ( z_w(i,j,k+1)-z_w(i,j,k) )
          enddo
        enddo
        do i=Istr,Iend
          FC(i,0)=0.
          FC(i,N)=0.
        enddo

        do k=1,N-1
          do i=Istr,Iend
            DC(i,k)=wz(i,j,k,nnew)                                    !<-- rhs
            BC(i,k)=0.5*(Hz(i,j,k)+Hz(i,j,k+1)) - FC(i,k) - FC(i,k-1) !<-- diagonal term
          enddo
        enddo  
        do i=Istr,Iend            
          BC(i,N)=0.5*Hz(i,j,N) - FC(i,N) - FC(i,N-1)    !<-- Hz contains rho.Hz
          DC(i,N)=wz(i,j,N,nnew)
        enddo 
        
! Gauss elimination
        do i=Istr,Iend
          cff=1./BC(i,1)
          CF(i,1)=cff*FC(i,1)
          DC(i,1)=cff*DC(i,1)
        enddo
        do k=2,N-1
          do i=Istr,Iend
            cff=1./(BC(i,k)-FC(i,k-1)*CF(i,k-1))
            CF(i,k)=cff*FC(i,k)
            DC(i,k)=cff*(DC(i,k)-FC(i,k-1)*DC(i,k-1))
          enddo
        enddo
        do i=Istr,Iend
          wz(i,j,N,nnew)=(DC(i,N)-FC(i,N-1)*DC(i,N-1))
     &                  /(BC(i,N)-FC(i,N-1)*CF(i,N-1))
        enddo
        do k=N-1,1,-1
          do i=Istr,Iend
             wz(i,j,k,nnew)=DC(i,k)-CF(i,k)*wz(i,j,k+1,nnew)  !<-- wz in m.s-1
          enddo
        enddo      !--> discard FC,BC; keep DC,CF(:,0)

      enddo      ! <-- j
#else
      do j=Jstr,Jend
        do k=1,N-1
          do i=Istr,Iend
            wz(i,j,k,nnew)=2.*wz(i,j,k,nnew)/(Hz(i,j,k)+Hz(i,j,k+1))
          enddo
        enddo
        do i=Istr,Iend
          wz(i,j,N,nnew)=2.*wz(i,j,N,nnew)/Hz(i,j,N)
        enddo
      enddo
#endif
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
