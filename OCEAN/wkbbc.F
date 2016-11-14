! $Id: wkbbc.F 1615 2014-12-17 13:27:07Z rblod $
!
!======================================================================
! CROCO is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! CROCO specific routines (nesting) are under CeCILL-C license.
! 
! CROCO website : http://roms.mpl.ird.fr
!======================================================================
!
#ifndef CHILD
!
# include "cppdefs.h"
# ifdef WKB_WWAVE
      subroutine wkbbc_tile(Istr,Iend,Jstr,Jend)
#  ifdef AGRIF      
      use Agrif_Util
      integer Istr,Iend,Jstr,Jend
      if (Agrif_Root()) then
        call wkbbc_parent_tile(Istr,Iend,Jstr,Jend)
      else
        call wkbbc_child_tile(Istr,Iend,Jstr,Jend)
      endif
      return
      end
!
! PARENT
!
      subroutine wkbbc_parent_tile(Istr,Iend,Jstr,Jend)
#  endif /* AGRIF */
!
! Set lateral boundary conditions for wkb fields
! for the parent grid.
!
#else
      subroutine wkbbc_empty
      end
# endif /* WKB_WWAVE */
#else
# ifdef WKB_WWAVE
!
! CHILD
!
      subroutine wkbbc_child_tile(Istr,Iend,Jstr,Jend)
!
! Set lateral boundary conditions for wkb fields wac,wkx,wke
! for the child grid.
!
# endif /* WKB_WWAVE */
#endif /* CHILD */
#ifdef WKB_WWAVE
!
! Common Code
!
# include "set_obc_definitions.h"
!
      implicit none
# include "param.h"
# include "grid.h"
# include "scalars.h"
# include "wkb_wwave.h"
# include "boundary.h"
      integer Istr,Iend,Jstr,Jend,indx,i,j
!
# include "compute_auxiliary_bounds.h"
!
! Interpolations of the parent values to get wkb_bry
!
# ifdef CHILD
      IF(.NOT. wkb_agrif_done)THEN
      call wkbbc_interp_tile(Istr,Iend,Jstr,Jend)
      wkb_agrif_done=.FALSE.
      ENDIF
# endif
!
# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
#  ifdef WKB_OBC_WEST
        do j=Jstr-1,Jend                           ! Western edge
          wkx(Istr-1,j,wnew)=wkxbry_west(j)         ! clamped BC
          wke(Istr-1,j,wnew)=wkebry_west(j)
          wac(Istr-1,j,wnew)=wacbry_west(j)
#   ifdef WAVE_ROLLER
          war(Istr-1,j,wnew)=0.0
#    ifdef AGRIF
          if( .NOT. Agrif_Root())then
!            war(Istr-1,j,wnew)=warbry_west(j)
            war(Istr-1,j,wnew)=war(Istr,j,wnew)
          endif
#    endif
#   endif
        enddo
#  else
        do j=Jstr,Jend                           ! Western edge
          wkx(Istr-1,j,wnew)=wkx(Istr,j,wnew)    ! gradient BC
          wke(Istr-1,j,wnew)=wke(Istr,j,wnew)
          wac(Istr-1,j,wnew)=wac(Istr,j,wnew)
#   ifdef WAVE_ROLLER
          war(Istr-1,j,wnew)=war(Istr,j,wnew)
#   endif
        enddo
#  endif  /* WKB_OBC_WEST */
      endif      
      if (EASTERN_EDGE) then
#  ifdef WKB_OBC_EAST
        do j=Jstr,Jend                           ! Eastern edge
          wkx(Iend+1,j,wnew)=wkxbry_east(j)         ! clamped BC
          wke(Iend+1,j,wnew)=wkebry_east(j)
          wac(Iend+1,j,wnew)=wacbry_east(j)
#   ifdef WAVE_ROLLER
          war(Iend+1,j,wnew)=0.0
#    ifdef AGRIF
          if( .NOT. Agrif_Root())then
!            war(Iend+1,j,wnew)=warbry_east(j)
            war(Iend+1,j,wnew)=war(Iend,j,wnew)
          endif
#    endif
#   endif
        enddo
#  else
        do j=Jstr,Jend                           ! Eastern edge
          wkx(Iend+1,j,wnew)=wkx(Iend,j,wnew)    ! gradient BC
          wke(Iend+1,j,wnew)=wke(Iend,j,wnew)
          wac(Iend+1,j,wnew)=wac(Iend,j,wnew)
#   ifdef WAVE_ROLLER
          war(Iend+1,j,wnew)=war(Iend,j,wnew)
#   endif
        enddo
#  endif  /* WKB_OBC_EAST */
      endif
# endif        /* !EW_PERIODIC */
!
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
#  ifdef WKB_OBC_SOUTH
        do i=Istr,Iend                           ! Southern edge
          wkx(i,Jstr-1,wnew)=wkxbry_south(i)        ! clamped BC
          wke(i,Jstr-1,wnew)=wkebry_south(i)
          wac(i,Jstr-1,wnew)=wacbry_south(i)
#   ifdef WAVE_ROLLER
          war(i,Jstr-1,wnew)=0.0
#    ifdef AGRIF
          if( .NOT. Agrif_Root())then
!            war(i,Jstr-1,wnew)=warbry_south(i)
            war(i,Jstr-1,wnew)=war(i,Jstr,wnew)
          endif
#    endif
#   endif
        enddo
#  else
        do i=Istr,Iend                           ! Southern edge
          wkx(i,Jstr-1,wnew)=wkx(i,Jstr,wnew)    ! gradient BC
          wke(i,Jstr-1,wnew)=wke(i,Jstr,wnew)
          wac(i,Jstr-1,wnew)=wac(i,Jstr,wnew)
#   ifdef WAVE_ROLLER
          war(i,Jstr-1,wnew)=war(i,Jstr,wnew)
#   endif
        enddo
#  endif  /* WKB_OBC_SOUTH */
      endif
      if (NORTHERN_EDGE) then
#  ifdef WKB_OBC_NORTH
        do i=Istr,Iend                           ! Northern edge
          wkx(i,Jend+1,wnew)=wkxbry_north(i)        ! clamped BC
          wke(i,Jend+1,wnew)=wkebry_north(i)
          wac(i,Jend+1,wnew)=wacbry_north(i)
#   ifdef WAVE_ROLLER
          war(i,Jend+1,wnew)=0.0
#    ifdef AGRIF
          if( .NOT. Agrif_Root())then
!            war(i,Jend+1,wnew)=warbry_north(i)
            war(i,Jend+1,wnew)=war(i,Jend,wnew)
          endif
#    endif
#   endif
        enddo
#  else
        do i=Istr,Iend                           ! Northern edge
          wkx(i,Jend+1,wnew)=wkx(i,Jend,wnew)    ! gradient BC
          wke(i,Jend+1,wnew)=wke(i,Jend,wnew)
          wac(i,Jend+1,wnew)=wac(i,Jend,wnew)
#   ifdef WAVE_ROLLER
          war(i,Jend+1,wnew)=war(i,Jend,wnew)
#   endif
        enddo
#  endif   /* WKB_OBC_NORTH */
      endif
# endif       /* !NS_PERIODIC */
!
! Corners between adjacent open boundaries
!
# ifndef EW_PERIODIC
#  ifndef NS_PERIODIC
      if (SOUTHERN_EDGE .and. WESTERN_EDGE) then
        wkx(Istr-1,Jstr-1,wnew)
     &          =0.5*( wkx(Istr,Jstr-1,wnew)+wkx(Istr-1,Jstr,wnew) )
        wke(Istr-1,Jstr-1,wnew)
     &          =0.5*( wke(Istr,Jstr-1,wnew)+wke(Istr-1,Jstr,wnew) )
        wac(Istr-1,Jstr-1,wnew)
     &          =0.5*( wac(Istr,Jstr-1,wnew)+wac(Istr-1,Jstr,wnew) )
#   ifdef WAVE_ROLLER
        war(Istr-1,Jstr-1,wnew)
     &          =0.5*( war(Istr,Jstr-1,wnew)+war(Istr-1,Jstr,wnew) )
#   endif
      endif
      if (SOUTHERN_EDGE .and. EASTERN_EDGE) then
        wkx(Iend+1,Jstr-1,wnew)
     &          =0.5*( wkx(Iend,Jstr-1,wnew)+wkx(Iend+1,Jstr,wnew) )
        wke(Iend+1,Jstr-1,wnew)
     &          =0.5*( wke(Iend,Jstr-1,wnew)+wke(Iend+1,Jstr,wnew) )
        wac(Iend+1,Jstr-1,wnew)
     &          =0.5*( wac(Iend,Jstr-1,wnew)+wac(Iend+1,Jstr,wnew) )
#   ifdef WAVE_ROLLER
        war(Iend+1,Jstr-1,wnew)
     &          =0.5*( war(Iend,Jstr-1,wnew)+war(Iend+1,Jstr,wnew) )
#   endif
      endif
      if (NORTHERN_EDGE .and. WESTERN_EDGE) then
        wkx(Istr-1,Jend+1,wnew)
     &          =0.5*( wkx(Istr,Jend+1,wnew)+wkx(Istr-1,Jend,wnew) )
        wke(Istr-1,Jend+1,wnew)
     &          =0.5*( wke(Istr,Jend+1,wnew)+wke(Istr-1,Jend,wnew) )
        wac(Istr-1,Jend+1,wnew)
     &          =0.5*( wac(Istr,Jend+1,wnew)+wac(Istr-1,Jend,wnew) )
#   ifdef WAVE_ROLLER
        war(Istr-1,Jend+1,wnew)
     &          =0.5*( war(Istr,Jend+1,wnew)+war(Istr-1,Jend,wnew) )
#   endif
      endif
      if (NORTHERN_EDGE .and. EASTERN_EDGE) then
        wkx(Iend+1,Jend+1,wnew)
     &          =0.5*( wkx(Iend,Jend+1,wnew)+wkx(Iend+1,Jend,wnew) )
        wke(Iend+1,Jend+1,wnew)
     &          =0.5*( wke(Iend,Jend+1,wnew)+wke(Iend+1,Jend,wnew) )
        wac(Iend+1,Jend+1,wnew)
     &          =0.5*( wac(Iend,Jend+1,wnew)+wac(Iend+1,Jend,wnew) )
#   ifdef WAVE_ROLLER
        war(Iend+1,Jend+1,wnew)
     &          =0.5*( war(Iend,Jend+1,wnew)+war(Iend+1,Jend,wnew) )
#   endif
      endif
#  endif
# endif
      return
      end
#endif /* WKB_WWAVE */
#ifndef CHILD
# define CHILD
# ifdef AGRIF
# define WKB_OBC_WEST
# define WKB_OBC_EAST
# define WKB_OBC_NORTH
# define WKB_OBC_SOUTH
#  include "wkbbc.F"
# endif
# undef WKB_OBC_WEST
# undef WKB_OBC_EAST
# undef WKB_OBC_NORTH
# undef WKB_OBC_SOUTH
# undef CHILD
#endif  /* !CHILD */
