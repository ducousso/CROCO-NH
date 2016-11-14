#ifndef MP_3PTS
      subroutine exchange_3d_tile (Istr,Iend,Jstr,Jend, A)
#else
      subroutine exchange_3d_3pts_tile (Istr,Iend,Jstr,Jend, A)
#endif
!
! Set periodic boundary conditions (if any) for a three-dimensional
! field A of RHO-, U-, V- or PSI-type. This file is designed to
! generate five different subroutines, by redefining (via CPP) the
! name of the subroutine exchange_2d_tile above and the starting
! indices ISTART = [Istr for U-,PSI-type; IstrR for V-,RHO-type]
! and JSTART = [Jstr for V-,PSI-type; JstrR for U-,RHO-type] below,
! as well as macro KSTART for the vertical RHO- and W-types. See
! also mounting file exchange.F
!
      implicit none
#include "param.h"
#include "scalars.h"
      integer Npts,ipts,jpts
# ifndef MP_3PTS
      parameter (Npts=2)
# else
      parameter (Npts=3)
# endif
      real A(GLOBAL_2D_ARRAY,KSTART:N)
      integer Istr,Iend,Jstr,Jend, i,j,k
!
#include "compute_auxiliary_bounds.h"
!
#ifdef EW_PERIODIC
# ifdef NS_PERIODIC
#  define J_RANGE Jstr,Jend
# else
#  define J_RANGE JSTART,JendR
# endif
# ifdef MPI
      if (NP_XI.eq.1) then
# endif
        if (WESTERN_EDGE) then
          do k=KSTART,N
            do j=J_RANGE
              do ipts=1,Npts
                A(Lm+ipts,j,k)=A(ipts,j,k)
	      enddo
            enddo
          enddo
        endif
        if (EASTERN_EDGE) then
          do k=KSTART,N
            do j=J_RANGE
	      do ipts=1,Npts
                A(ipts-Npts,j,k)=A(Lm+ipts-Npts,j,k)
              enddo
            enddo
          enddo
        endif
# ifdef MPI
      endif
# endif
# undef J_RANGE
#endif            /* EW_PERIODIC */

#ifdef NS_PERIODIC
# ifdef EW_PERIODIC
#  define I_RANGE Istr,Iend
# else
#  define I_RANGE ISTART,IendR
# endif
# ifdef MPI
      if (NP_ETA.eq.1) then
# endif
        if (SOUTHERN_EDGE) then
          do k=KSTART,N
            do i=I_RANGE
	      do jpts=1,Npts
                A(i,Mm+jpts,k)=A(i,jpts,k)
	      enddo
            enddo
          enddo
        endif
        if (NORTHERN_EDGE) then
          do k=KSTART,N
            do i=I_RANGE
	      do jpts=1,Npts
                A(i,jpts-Npts,k)=A(i,Mm+jpts-Npts,k)
	      enddo
            enddo
          enddo
        endif
# ifdef MPI
      endif
# endif
# undef I_RANGE
#endif               /* NS_PERIODIC */

#if defined EW_PERIODIC && defined NS_PERIODIC
# ifdef MPI
      if (NP_XI.eq.1 .and. NP_ETA.eq.1) then
# endif
        if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
          do k=KSTART,N
	    do jpts=1,Npts
	      do ipts=1,Npts
	        A(Lm+ipts,Mm+jpts,k)=A(ipts,jpts,k)
	      enddo
	    enddo
          enddo
        endif
        if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
          do k=KSTART,N
	    do jpts=1,Npts
	      do ipts=1,Npts
	        A(ipts-Npts,Mm+jpts,k)=A(Lm+ipts-Npts,jpts,k)
	      enddo
	    enddo
          enddo
        endif
        if (WESTERN_EDGE .and. NORTHERN_EDGE) then
          do k=KSTART,N
	    do jpts=1,Npts
	      do ipts=1,Npts
	        A(Lm+ipts,jpts-Npts,k)=A(ipts,Mm+jpts-Npts,k)
	      enddo
	    enddo
          enddo
        endif
        if (EASTERN_EDGE .and. NORTHERN_EDGE) then
          do k=KSTART,N
	    do jpts=1,Npts
	      do ipts=1,Npts
	        A(ipts-Npts,jpts-Npts,k)=A(Lm+ipts-Npts,Mm+jpts-Npts,k)
	      enddo
	    enddo
          enddo
        endif
# ifdef MPI
      endif
# endif
#endif
#ifdef MPI
      k=N-KSTART+1
# ifndef MP_3PTS
      call MessPass3D_tile (Istr,Iend,Jstr,Jend,  A,k)
# else
      call MessPass3D_3pts_tile (Istr,Iend,Jstr,Jend,  A,k)
# endif
#endif
      return
      end

# ifndef MP_3PTS
#  define MP_3PTS
#  include "exchange_3d_tile.h"
#  undef MP_3PTS
# endif

