! $Id: compute_extended_bounds.h 1458 2014-02-03 15:01:25Z gcambon $
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
/* Auxiliary module "compute_extended_bounds.h":
------------------------------------------------------
 Bounds designed to cover interior points of an array and
 ghost points associated with PHYSICAL side boundaries AND
 also ghost points of internal computational boundaries of
 MPI-subdomains.

 In the case of shared memory code (non-MPI version)
 IstrR, IendR,JstrR,JendR are equivalent to that computed
 by "compute_auxiliary_bounds.h".
*/
      integer IstrR,IendR,JstrR,JendR
      if (Istr.eq.1) then
# ifdef EW_PERIODIC
        IstrR=Istr-2
# else
#  ifdef MPI
        if (WEST_INTER) then
          IstrR=Istr-2
        else
          IstrR=Istr-1
        endif
#  else
        IstrR=Istr-1
#  endif
# endif
      else
        IstrR=Istr
      endif

#ifdef MPI
      if (Iend.eq.Lmmpi) then
#else
      if (Iend.eq.Lm) then
#endif      
# ifdef EW_PERIODIC
        IendR=Iend+2
# else
#  ifdef MPI
        if (EAST_INTER) then
          IendR=Iend+2
        else
          IendR=Iend+1
        endif
#  else
        IendR=Iend+1
#  endif
# endif
      else
        IendR=Iend
      endif

      if (Jstr.eq.1) then
# ifdef NS_PERIODIC
        JstrR=Jstr-2
# else
#  ifdef MPI
        if (SOUTH_INTER) then
          JstrR=Jstr-2
        else
          JstrR=Jstr-1
        endif
#  else
        JstrR=Jstr-1
#  endif
# endif
      else
        JstrR=Jstr
      endif

#ifdef MPI
      if (Jend.eq.Mmmpi) then
#else
      if (Jend.eq.Mm) then
#endif 
# ifdef NS_PERIODIC
        JendR=Jend+2
# else
#  ifdef MPI
        if (NORTH_INTER) then
          JendR=Jend+2
        else
          JendR=Jend+1
        endif
#  else
        JendR=Jend+1
#  endif
# endif
      else
        JendR=Jend
      endif

