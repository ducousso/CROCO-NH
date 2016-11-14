! $Id: autotiling.h 1458 2014-02-03 15:01:25Z gcambon $
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
         integer nbsampling, lastiic
         common/dyndistribution/nbsampling, lastiic

#if !defined AGRIF
      integer, parameter :: MAX_NSUB_X=Lm/6
      integer, parameter :: MAX_NSUB_E=Mm/6
#else
      integer :: MAX_NSUB_X
      integer :: MAX_NSUB_E
      common/autotilingparams/MAX_NSUB_X,MAX_NSUB_E
#endif
      
      integer, parameter :: nbvalid = 3
      integer, parameter :: nbdistrib = 4
      
      real cpu_domain(0:2)
      real times(MAX_NSUB_X,MAX_NSUB_E,nbdistrib*nbvalid)
      integer nbinst(MAX_NSUB_X,MAX_NSUB_E)
      integer nsub_emin,nsub_emax, nsub_xmin,nsub_xmax
      integer nsub_decalx, nsub_decale
      integer curdistrib
      common/dyndistribution2/times,cpu_domain,
     &       nbinst,nsub_xmin, nsub_xmax,
     &  nsub_emin,nsub_emax,nsub_decalx,nsub_decale,
     &  curdistrib

      integer, parameter :: nbx=4
      integer, parameter :: nbe=4

