! $Id: wvlcty.F 1458 2014-02-03 15:01:25Z gcambon $
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

      subroutine Wvlcty (tile, Wvlc)
      implicit none
# include "param.h"
      real Wvlc(GLOBAL_2D_ARRAY,N)
      integer tile, trd,omp_get_thread_num
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call Wvlcty_tile (Istr,Iend,Jstr,Jend, Wvlc, A2d(1,1,trd),
     &                               A2d(1,1,trd), A2d(1,2,trd))
      return
      end

      subroutine wvlcty_tile (Istr,Iend,Jstr,Jend, Wvlc,
     &                                   Wrk, Wxi, Weta)
!
!  Compute absolute vertical velocity, which consists of three
! components: S-coordinate vertical velocity  w*pm*pn; projection
! of (quasi-)horizontal motions along S=const surfaces; and
! vertical velocity of moving grid-box interfaces due to the motion
! of free surface.
! This computation is done solely for diagnostic/output purposes and
! does not have any feedback onto the model. Unlike W, absolute
! vertical velocity is defined at RHO-points. 
!
      implicit none
# include "param.h"
      integer Istr,Iend,Jstr,Jend, imin,imax,jmin,jmax, i,j,k
      real Wvlc(GLOBAL_2D_ARRAY,N), 
     &     Wrk(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     Wxi(PRIVATE_2D_SCRATCH_ARRAY),
     &     Weta(PRIVATE_2D_SCRATCH_ARRAY)
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"

# ifdef EW_PERIODIC
#  ifdef MPI
      if (Istr.eq.1 .and. ii.eq.0) then
#  else
      if (Istr.eq.1) then
#  endif
        imin=Istr-1
      else
        imin=Istr
      endif
#  ifdef MPI
      if (Iend.eq.Lmmpi.and. ii.eq.NP_XI-1) then
#  else
      if (Iend.eq.Lm) then
#  endif
        imax=Iend+1
      else
        imax=Iend
      endif
# else
      imin=Istr
      imax=Iend
# endif


# ifdef NS_PERIODIC
#  ifdef MPI
      if (Jstr.eq.1 .and. jj.eq.0) then
#  else
      if (Jstr.eq.1) then
#  endif
        jmin=Jstr-1
      else
        jmin=Jstr
      endif

#  ifdef MPI
      if (Jend.eq.Mmmpi .and. jj.eq.NP_ETA-1) then
#  else
      if (Jend.eq.Mm) then
#  endif
        jmax=Jend+1
      else
        jmax=Jend
      endif
# else
      jmin=Jstr
      jmax=Jend
# endif
!
!   Compute "omega" vertical velocity by means of integration of 
! mass divergence of mass fluxes from bottom up. In this computation,
! unlike that in omega.F, there is (1) immediate multiplication by 
! pm*pn so that the result has meaning of velocity, rather than
! finite volume mass flux through vertical facet of tracer grid box;
! and (2, also unlike omega.F) no subtraction of vertical velocity
! of moving grid-box interface (the effect of "breething" of vertical
! grid system due to evolving free surface) is made now.
! Consequently, Wrk(:,N).ne.0, unlike its counterpart W(:,:,N).eqv.0
! in omega.F.
!
! Once omega vertical velocity is computed, interpolate it to
! vertical RHO-points.
!
      do j=jmin,jmax
        do i=imin,imax
          Wrk(i,0)=0.D0
        enddo
        do k=1,N,+1
          do i=imin,imax
            Wrk(i,k)=Wrk(i,k-1)-pm(i,j)*pn(i,j)*(
     &                      Huon(i+1,j,k)-Huon(i,j,k)
     &                     +Hvom(i,j+1,k)-Hvom(i,j,k))
c**    Wrk(i,k)=0.!(uncomment to test the second part)
          enddo
        enddo
        do i=imin,imax
          Wvlc(i,j,N)=+0.375*Wrk(i,N) +0.75*Wrk(i,N-1)
     &                                -0.125*Wrk(i,N-2)
        enddo
        do k=N-1,2,-1
          do i=imin,imax
            Wvlc(i,j,k)=+0.5625*(Wrk(i,k  )+Wrk(i,k-1))
     &                  -0.0625*(Wrk(i,k+1)+Wrk(i,k-2))
          enddo
        enddo
        do i=imin,imax
          Wvlc(i,j,  1)= -0.125*Wrk(i,2) +0.75*Wrk(i,1)
     &                                 +0.375*Wrk(i,0)
        enddo
      enddo
!
! Compute and add contributions due to (quasi-)horizontal
! motions along S=const surfaces by multiplying horizontal
! velocity components by slops S-coordinate surfaces:
!
      do k=1,N
        do j=jmin,jmax
          do i=imin,imax+1
            Wxi(i,j)=u(i,j,k,nstp)*(pm(i,j)+pm(i-1,j))
     &                       *(z_r(i,j,k)-z_r(i-1,j,k))
          enddo
        enddo
        do j=jmin,jmax+1
          do i=imin,imax
            Weta(i,j)=v(i,j,k,nstp)*(pn(i,j)+pn(i,j-1))
     &                       *(z_r(i,j,k)-z_r(i,j-1,k))
          enddo
        enddo
        do j=jmin,jmax
          do i=imin,imax
            Wvlc(i,j,k)=Wvlc(i,j,k)+0.25*( Wxi(i,j)
     &              +Wxi(i+1,j)+Weta(i,j)+Weta(i,j+1))
          enddo
        enddo
      enddo
!
!  Set lateral boundary conditions: gradient only.
!
# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        do k=1,N
          do j=jmin,jmax
            Wvlc(imin-1,j,k)=Wvlc(imin,j,k)
          enddo
        enddo
      endif
      if (EASTERN_EDGE) then
        do k=1,N
          do j=jmin,jmax
            Wvlc(imax+1,j,k)=Wvlc(imax,j,k)
          enddo
        enddo
      endif
# endif
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do k=1,N
          do i=imin,imax
            Wvlc(i,jmin-1,k)=Wvlc(i,jmin,k)
          enddo
        enddo
      endif
      if (NORTHERN_EDGE) then
        do k=1,N
          do i=imin,imax
            Wvlc(i,jmax+1,k)=Wvlc(i,jmax,k)
          enddo
        enddo
      endif
#  ifndef EW_PERIODIC
      if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=1,N
          Wvlc(imin-1,jmin-1,k)=Wvlc(imin,jmin,k)
        enddo
      endif
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        do k=1,N
          Wvlc(imin-1,jmax+1,k)=Wvlc(imin,jmax,k)
        enddo
      endif
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=1,N
          Wvlc(imax+1,jmin-1,k)=Wvlc(imax,jmin,k)
        enddo
      endif
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        do k=1,N
          Wvlc(imax+1,jmax+1,k)=Wvlc(imax,jmax,k)
        enddo
      endif
#  endif
# endif
      return
      end
#else
      subroutine Wvlcty_empty 
      return
      end
#endif /* SOLVE3D */
