! $Id: set_nudgcof.F 1458 2014-02-03 15:01:25Z gcambon $
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
#if defined ZNUDGING || defined M2NUDGING || defined M3NUDGING\
    || defined TNUDGING || defined SPONGE

      subroutine set_nudgcof (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call set_nudgcof_tile (Istr,Iend,Jstr,Jend,A2d(1,1,trd))
      return
      end
!
      subroutine set_nudgcof_tile (Istr,Iend,Jstr,Jend, wrk)
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
# include "ocean3d.h"
# include "mpi_roms.h"
      integer Istr,Iend,Jstr,Jend, i, j, k, isp, itrc, ibnd
      real    wrk(PRIVATE_2D_SCRATCH_ARRAY)
# if defined CANARY  || defined IBERIA
      real    lon0,lat0, rsponge, dx,dy,dr,cff,cff1,cff2
# endif

!
# include "compute_extended_bounds.h"
!
!--------------------------------------------------------------------
!  Set up nudging towards climatology time-scale coeffcients [1/s]
!  and/or increase horizontal mixing in the sponge areas.
!--------------------------------------------------------------------
!
# if defined SPONGE_GRID || !defined SPONGE
#  ifdef IGW
      isp=155
#  else
      isp=10
#  endif
# else
#  ifdef MPI
      if (ii.eq.0.and.jj.eq.0.and.Istr.eq.1.and.Jstr.eq.1) then
        isp=int(x_sponge*pm(1,1))
      endif      
      call MPI_Bcast(isp, 1, MPI_INTEGER,
     &                    0, MPI_COMM_WORLD, ierr)      
#  else
      isp=int(x_sponge*pm(1,1)) ! number of points in layer     
#  endif
# endif /* SPONGE_GRID */

!
      do j=max(-1,JstrR-1),JendR
        do i=max(-1,IstrR-1),IendR
          ibnd=isp
# ifndef MPI
#  ifdef OBC_WEST
          ibnd=min(ibnd,i)
#  endif
#  ifdef OBC_EAST
          ibnd=min(ibnd,Lm+1-i)
#  endif
#  ifdef OBC_SOUTH
          ibnd=min(ibnd,j)
#  endif
#  ifdef OBC_NORTH
          ibnd=min(ibnd,Mm+1-j)
#  endif
# else
#  ifdef OBC_WEST
          ibnd=min(ibnd,i+iminmpi-1)          
#  endif
#  ifdef OBC_EAST
          ibnd=min(ibnd,LLm+1-(i+iminmpi-1))          
#  endif
#  ifdef OBC_SOUTH
          ibnd=min(ibnd,j+jminmpi-1)          
#  endif
#  ifdef OBC_NORTH
          ibnd=min(ibnd,MMm+1-(j+jminmpi-1))          
#  endif
# endif

# ifdef IGW
          if (ibnd .ge. isp) then
            wrk(i,j)=0.
          else
            wrk(i,j)=exp(-5.d-5*float(ibnd)*grdmax)       ! exp. profile
          endif
# else
          wrk(i,j)=.5*(cos(pi*float(ibnd)/float(isp))+1.) ! cosine profile
!         wrk(i,j)=float(isp-ibnd)/float(isp)             ! linear profile
# endif
        enddo
      enddo
!
!-------------------------------------------------------------------
! Compute nudging coefficients in nudging layers
!-------------------------------------------------------------------
!
      do j=JstrR,JendR
        do i=IstrR,IendR
# ifdef TNUDGING
          Tnudgcof(i,j,N,itemp)=tauT_out*wrk(i,j)
# endif
# ifdef ZNUDGING
          Znudgcof(i,j)=tauM_out*wrk(i,j)
# endif
# ifdef M2NUDGING
          M2nudgcof(i,j)=tauM_out*wrk(i,j)
# endif
# ifdef M3NUDGING
          M3nudgcof(i,j)=tauM_out*wrk(i,j)
# endif
        enddo
      enddo
!
!  Apply nudging to other tracers and vertical levels
!
# ifdef TNUDGING
      do itrc=1,NT       ! includes BIOLOGY variables
        do k=1,N
          do j=JstrR,JendR
            do i=IstrR,IendR
              Tnudgcof(i,j,k,itrc)=Tnudgcof(i,j,N,itemp)
            enddo
          enddo
        enddo
      enddo
# endif
!
! Interior nudging
!
# ifdef ROBUST_DIAG
#  ifdef TNUDGING 
      do k=1,N 
        do j=JstrR,JendR
          do i=IstrR,IendR
            Tnudgcof(i,j,k,itemp)=tauT_out
#   ifdef SALINITY
            Tnudgcof(i,j,k,isalt)=tauT_out
#   endif
          enddo
        enddo
      enddo
#  endif /* TNUDGING */
#  ifdef JET
      do j=JstrR,JendR
        do i=IstrR,IendR
#   ifdef ZNUDGING
          Znudgcof(i,j)=tauM_out
#   endif
#   ifdef M2NUDGING
          M2nudgcof(i,j)=tauM_out
#   endif
#   ifdef M3NUDGING
          M3nudgcof(i,j)=tauM_out
#   endif
        enddo
      enddo
#   endif /* JET */
#  endif /* ROBUST_DIAG */

# ifdef SPONGE
!
!-------------------------------------------------------------------
!  Add Viscosity and Diffusivity in SPONGE layers
!-------------------------------------------------------------------
!
#  if defined UV_VIS2 || defined UV_VIS4 || defined SPONGE_VIS2
!
!  Add Viscosity at rho points 
!
      do j=JstrR,JendR
        do i=IstrR,IendR
          visc2_sponge_r(i,j)=(0.01/(pm(i,j)*pn(i,j)*dt))*wrk(i,j)
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
          visc2_sponge_p(i,j)=0.25*(0.01/(pm(i,j)*pn(i,j)*dt))*
     &                              ( wrk(i,j  )+wrk(i-1,j  )
     &                               +wrk(i,j-1)+wrk(i-1,j-1) ) 
#   ifdef UV_VIS4
          visc4_sponge_p(i,j)=visc2_sponge_p(i,j)/(pm(i,j)*pn(i,j))
#   endif
        enddo
      enddo
#  endif /* UV_VIS2 || UV_VIS4 || SPONGE_VIS2 */

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
# if defined CANARY || defined IBERIA
      lon0=-8.2
      lat0=35.7
      rsponge=300.e3
      cff=1./(50.*86400.)
      cff1=Eradius*cos(lat0*deg2rad)*deg2rad
      cff2=Eradius*deg2rad
      do j=JstrR,JendR
        do i=IstrR,IendR
          dx=cff1*(lonr(i,j)-lon0)
          dy=cff2*(latr(i,j)-lat0)
          dr=sqrt(dx**2+dy**2)
          if (dr .lt. rsponge) then
            do k=1,N
              Tnudgcof(i,j,k,itemp)=.5*cff*(cos(pi*dr/rsponge)+1)
     &                    *(-atan((z_r(i,j,k)+750.)*2.e-2)/pi+.5)
              Tnudgcof(i,j,k,isalt)=Tnudgcof(i,j,k,itemp)
            enddo
          endif
        enddo
      enddo
# endif
#else
      subroutine set_nudgcof_empty
#endif /* TNUDGING || ZNUDGING || SPONGE */
      return
      end
