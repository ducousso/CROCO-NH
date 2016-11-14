! $Id: lmd_kpp.h 1458 2014-02-03 15:01:25Z gcambon $
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
#if defined LMD_SKPP || defined LMD_BKPP || defined GLS_MIXING
      integer Jwtype(GLOBAL_2D_ARRAY)
      common /nils_jerlov/ Jwtype
#endif
