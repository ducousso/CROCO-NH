! $Id: compute_message_bounds.h 1458 2014-02-03 15:01:25Z gcambon $
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
      integer imin,imax,ishft, jmin,jmax,jshft
# ifdef EW_PERIODIC
      if (NP_XI.eq.1) then                ! This means that if there
        imin=Istr-2                       ! is no partition in XI- 
        imax=Iend+2                       ! direction, then periodic 
      else                                ! margins are included into
        imin=Istr                         ! the message; 
        imax=Iend                         ! otherwise strip them out.
      endif
# else
      if (ii.eq.0 .and. Istr.eq.1) then   ! Extra point on either
        imin=Istr-1                       ! side to accomodate ghost
      else                                ! points associated with
        imin=Istr                         ! PHYSICAL boundaries.
      endif
      if (ii.eq.NP_XI-1 .and. Iend.eq.Lmmpi) then
        imax=Iend+1
      else
        imax=Iend
      endif
# endif
      ishft=imax-imin+1

# ifdef NS_PERIODIC
      if (NP_ETA.eq.1) then               ! This means that if there
        jmin=Jstr-2                       ! is no partition in ETA-
        jmax=Jend+2                       ! direction, then periodic
      else                                ! margins are included into 
        jmin=Jstr                         ! the message;
        jmax=Jend                         ! otherwise strip them out.
      endif
# else
      if (jj.eq.0 .and. Jstr.eq.1) then   ! Extra point on either
        jmin=Jstr-1                       ! side to accomodate ghost
      else                                ! points associated with
        jmin=Jstr                         ! PHYSICAL boundaries.
      endif
      if (jj.eq.NP_ETA-1 .and. Jend.eq.Mmmpi) then
        jmax=Jend+1
      else
        jmax=Jend
      endif
# endif
      jshft=jmax-jmin+1
