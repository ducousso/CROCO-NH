#ifndef MP_3PTS
      subroutine exchange_2d_tile (Istr,Iend,Jstr,Jend, A)
#else
      subroutine exchange_2d_3pts_tile (Istr,Iend,Jstr,Jend, A)
#endif
!
! Set periodic boundary conditions (if any) for a two-dimensional
! field A of ZETA-, U-, V- or PSI-type. This file is designed to
! generate four different subroutines, by redefining (via CPP) the
! name of the subroutine exchange_2d_tile above and the starting
! indices ISTART = [Istr for U-,PSI-type; IstrR for V-,ZETA-type] 
! and JSTART = [Jstr for V-,PSI-type; JstrR for U-,ZETA-type]
! below. See also mounting file exchange.F  
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
      real A(GLOBAL_2D_ARRAY)
      integer Istr,Iend,Jstr,Jend, i,j
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
          do j=J_RANGE
            do ipts=1,Npts
              A(Lm+ipts,j)=A(ipts,j)
	    enddo
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=J_RANGE
            do ipts=1,Npts
              A(ipts-Npts,j)=A(Lm+ipts-Npts,j)
            enddo
          enddo
        endif
# ifdef MPI
      endif
# endif
# undef J_RANGE
#endif

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
          do i=I_RANGE
	    do jpts=1,Npts
              A(i,Mm+jpts)=A(i,jpts)
            enddo
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=I_RANGE
	    do jpts=1,Npts
              A(i,jpts-Npts)=A(i,Mm+jpts-Npts)
	    enddo
          enddo
        endif
# ifdef MPI
      endif
# endif
# undef I_RANGE
#endif

#if defined EW_PERIODIC && defined NS_PERIODIC
# ifdef MPI
      if (NP_XI.eq.1 .and. NP_ETA.eq.1) then
# endif
        if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
	  do jpts=1,Npts
	    do ipts=1,Npts
	      A(Lm+ipts,Mm+jpts)=A(ipts,jpts)
	    enddo
	  enddo
        endif
        if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
          do jpts=1,Npts
	    do ipts=1,Npts
	      A(ipts-Npts,Mm+jpts)=A(Lm+ipts-Npts,jpts)
	    enddo
	  enddo
        endif
        if (WESTERN_EDGE .and. NORTHERN_EDGE) then
	  do jpts=1,Npts
	    do ipts=1,Npts
	      A(Lm+ipts,jpts-Npts)=A(ipts,Mm+jpts-Npts)
	    enddo
	  enddo
        endif
        if (EASTERN_EDGE .and. NORTHERN_EDGE) then
	  do jpts=1,Npts
	    do ipts=1,Npts
	      A(ipts-Npts,jpts-Npts)=A(Lm+ipts-Npts,Mm+jpts-Npts)
	    enddo
	  enddo
        endif
# ifdef MPI
      endif
# endif
#endif
#ifdef MPI
# ifndef MP_3PTS
      call MessPass2D_tile (Istr,Iend,Jstr,Jend,  A)
# else
      call MessPass2D_3pts_tile (Istr,Iend,Jstr,Jend,  A)
# endif
#endif
      return
      end

# ifndef MP_3PTS
#  define MP_3PTS
#  include "exchange_2d_tile.h"
#  undef MP_3PTS
# endif
