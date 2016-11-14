! $Id: set_nudgcof_fine.F 1458 2014-02-03 15:01:25Z gcambon $
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
#if defined AGRIF && (defined TNUDGING || defined ZNUDGING || defined SPONGE)

      subroutine set_nudgcof_fine (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call set_nudgcof_fine_tile (Istr,Iend,Jstr,Jend,A2d(1,1,trd))
      return
      end
!
      subroutine set_nudgcof_fine_tile (Istr,Iend,Jstr,Jend, wrk)
!     implicit none
      integer ierr
# if defined MPI
      include 'mpif.h'
# endif
# include "param.h"
# include "grid.h"
# include "climat.h"
# include "mixing.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i, j, k, isp, itrc, ibnd
      real wrk(PRIVATE_2D_SCRATCH_ARRAY)
!
# include "compute_extended_bounds.h"
!
!--------------------------------------------------------------------
!  Set up nudging towards climatology time-scale coeffcients [1/s]
!  and/or increase horizontal mixing in the sponge areas.
!--------------------------------------------------------------------
!
!  
      isp = 2*max(Agrif_Irhox(),Agrif_Irhoy())  
!
      do j=max(-1,JstrR-1),JendR
        do i=max(-1,IstrR-1),IendR
          ibnd=isp
# ifndef MPI
#  ifdef AGRIF_OBC_WEST
          ibnd=min(ibnd,i)
#  endif
#  ifdef AGRIF_OBC_EAST
          ibnd=min(ibnd,Lm+1-i)
#  endif
#  ifdef AGRIF_OBC_SOUTH
          ibnd=min(ibnd,j)
#  endif
#  ifdef AGRIF_OBC_NORTH
          ibnd=min(ibnd,Mm+1-j)
#  endif
# else
#  ifdef AGRIF_OBC_WEST
          ibnd=min(ibnd,i+iminmpi-1)
#  endif
#  ifdef AGRIF_OBC_EAST
          ibnd=min(ibnd,LLm+1-(i+iminmpi-1))
#  endif
#  ifdef AGRIF_OBC_SOUTH
          ibnd=min(ibnd,j+jminmpi-1)
#  endif
#  ifdef AGRIF_OBC_NORTH
          ibnd=min(ibnd,MMm+1-(j+jminmpi-1))
#  endif
# endif
!
          wrk(i,j)=.5*(cos(pi*float(ibnd)/float(isp))+1.)! cosine profile
!          wrk(i,j)=float(isp-ibnd)/float(isp)            ! linear profile
        enddo
      enddo
!
!-------------------------------------------------------------------
! Compute nudging coefficients in nudging layers
!-------------------------------------------------------------------
!
      do j=JstrR,JendR
        do i=IstrR,IendR
# ifdef ZNUDGING
          Znudgcof(i,j)=0.
# endif
# ifdef M2NUDGING
          M2nudgcof(i,j)=0.
# endif
# ifdef M3NUDGING
          M3nudgcof(i,j)=0.
# endif
        enddo
      enddo
!
!  Apply nudging to the tracers 
!
# ifdef TNUDGING
      do itrc=1,NT               ! includes BIOLOGY variables
        do k=1,N
          do j=JstrR,JendR
            do i=IstrR,IendR
              Tnudgcof(i,j,k,itrc)=0.
            enddo
          enddo
        enddo
      enddo
#  ifdef ROBUST_DIAG  
      do itrc=1,2               ! nudging everywhere
        do k=1,N
          do j=JstrR,JendR
            do i=IstrR,IendR
              Tnudgcof(i,j,k,itrc)=1./(360.*86400.)
            enddo
          enddo
        enddo
      enddo
#  endif
# endif

# ifdef SPONGE
!
!-------------------------------------------------------------------
! Add Viscosity and Diffusivity in SPONGE layers
!-------------------------------------------------------------------
!
#  if defined UV_VIS2 || defined UV_VIS4 || defined SPONGE_VIS2
!
! Add Viscosity at rho points 
!
      do j=JstrR,JendR
        do i=IstrR,IendR
          visc2_sponge_r(i,j)=(0.005/(pm(i,j)*pn(i,j)*dt))*wrk(i,j) 
#   ifdef UV_VIS4
          visc4_sponge_r(i,j)=visc2_sponge_r(i,j)/(pm(i,j)*pn(i,j))
#   endif
       enddo
      enddo
!
!  Interpolate Viscosity at psi points
!
      do j=Jstr,JendR
        do i=Istr,IendR
          visc2_sponge_p(i,j)=0.25*(0.005/(pm(i,j)*pn(i,j)*dt))*
     &                       ( wrk(i,j  )+wrk(i-1,j  )
     &                        +wrk(i,j-1)+wrk(i-1,j-1) ) 
#   ifdef UV_VIS4
          visc4_sponge_p(i,j)=visc2_sponge_p(i,j)/(pm(i,j)*pn(i,j))
#   endif
        enddo
       enddo
#  endif  /* UV_VIS2 || UV_VIS4 || SPONGE_VIS2 */

#  if defined TS_DIF2 || defined TS_DIF4 || defined SPONGE_DIF2
!
!  Add Diffusivity for all tracers
!
       do itrc=1,NT
        do j=JstrR,JendR
          do i=IstrR,IendR
            diff2_sponge(i,j)=(0.01/(pm(i,j)*pn(i,j)*dt))*wrk(i,j)
#   ifdef TS_DIF4
            diff4_sponge(i,j)=diff2_sponge(i,j)/(pm(i,j)*pn(i,j))
#   endif
        enddo
       enddo
      enddo
#  endif /* TS_DIF2 || TS_DIF4 || SPONGE_DIF2 */

# endif /* SPONGE */
!
!-------------------------------------------------------------------
!  add configuration specific stuff
!-------------------------------------------------------------------
!

#else
      subroutine set_nudgcof_fine_empty
#endif /* AGRIF && (TNUDGING || ZNUDGING || SPONGE) */
      return
      end
