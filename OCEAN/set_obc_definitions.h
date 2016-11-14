! $Id: set_obc_definitions.h 1458 2014-02-03 15:01:25Z gcambon $
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
/*
! This is "set_obc_definitions.h": It contains a set of predetermined
! macro definitions which are inserted into the open boundary files by
! C-preprocessor. They allow separate open boundaries definitions for
! the parent and the child grids in case of nesting.
*/
#undef OBC_COM_EAST 
#undef OBC_COM_WEST 
#undef OBC_COM_NORTH 
#undef OBC_COM_SOUTH
#undef OBC_COM_TSPECIFIED
#undef OBC_COM_TORLANSKI
#undef OBC_COM_TUPWIND
#undef OBC_COM_M3SPECIFIED
#undef OBC_COM_M3ORLANSKI
#undef OBC_COM_M3UPWIND
#undef OBC_COM_M2SPECIFIED
#undef OBC_COM_M2ORLANSKI
#undef OBC_COM_M2FLATHER
#undef OBC_COM_M2CHARACT
#undef OBC_COM_ZSPECIFIED
#undef OBC_COM_ZORLANSKI
#undef OBC_COM_ZCHAPMAN
#undef OBC_COM_NBQSPECIFIED
#undef OBC_COM_NBQORLANSKI
#undef EW_COM_PERIODIC
#undef NS_COM_PERIODIC



/* PARENT OBC DEFINITIONS */

#ifndef CHILD
#  ifdef OBC_EAST
#   define OBC_COM_EAST
#  endif
#  ifdef OBC_WEST
#   define OBC_COM_WEST
#  endif
#  ifdef OBC_NORTH
#   define OBC_COM_NORTH
#  endif
#  ifdef OBC_SOUTH
#   define OBC_COM_SOUTH
#  endif
#  ifdef OBC_TSPECIFIED
#   define OBC_COM_TSPECIFIED
#  endif
#  ifdef OBC_TORLANSKI
#   define OBC_COM_TORLANSKI
#  endif
#  ifdef OBC_TUPWIND
#   define OBC_COM_TUPWIND
#  endif
#  ifdef OBC_M3SPECIFIED
#   define OBC_COM_M3SPECIFIED
#  endif
#  ifdef OBC_M3ORLANSKI
#   define OBC_COM_M3ORLANSKI
#  endif
#  ifdef OBC_M2SPECIFIED
#   define OBC_COM_M2SPECIFIED
#   define OBC_COM_ZSPECIFIED
#  endif
#  ifdef OBC_M2ORLANSKI
#   define OBC_COM_M2ORLANSKI
#   define OBC_COM_ZORLANSKI
#  endif
#  ifdef OBC_M2FLATHER
#   define OBC_COM_M2FLATHER
#   define OBC_COM_ZCHAPMAN
#  endif
#  ifdef OBC_M2CHARACT
#   define OBC_COM_M2CHARACT
#   define OBC_COM_ZCHAPMAN
#  endif
#  ifdef OBC_NBQSPECIFIED
#   define OBC_COM_NBQSPECIFIED
#  endif
#  ifdef OBC_NBQORLANSKI
#   define OBC_COM_NBQORLANSKI
#  endif
#  ifdef EW_PERIODIC
#   define EW_COM_PERIODIC
#  endif
#  ifdef NS_PERIODIC
#   define NS_COM_PERIODIC
#  endif
#else

/* CHILD OBC DEFINITIONS */

#  ifdef AGRIF_OBC_EAST
#   define OBC_COM_EAST
#  endif
#  ifdef AGRIF_OBC_WEST
#   define OBC_COM_WEST
#  endif
#  ifdef AGRIF_OBC_NORTH
#   define OBC_COM_NORTH
#  endif
#  ifdef AGRIF_OBC_SOUTH
#   define OBC_COM_SOUTH
#  endif
#  ifdef AGRIF_OBC_TSPECIFIED
#   define OBC_COM_TSPECIFIED
#  endif
#  ifdef AGRIF_OBC_TORLANSKI
#   define OBC_COM_TORLANSKI
#  endif
#  ifdef AGRIF_OBC_TUPWIND
#   define OBC_COM_TUPWIND
#  endif
#  ifdef AGRIF_OBC_M3SPECIFIED
#   define OBC_COM_M3SPECIFIED
#  endif
#  ifdef AGRIF_OBC_M3ORLANSKI
#   define OBC_COM_M3ORLANSKI
#  endif
#  ifdef AGRIF_OBC_M2SPECIFIED
#   define OBC_COM_M2SPECIFIED
#   define OBC_COM_ZSPECIFIED
#  endif
#  ifdef AGRIF_OBC_M2ORLANSKI
#   define OBC_COM_M2ORLANSKI
#   define OBC_COM_ZORLANSKI
#  endif
#  ifdef AGRIF_OBC_M2FLATHER
#   define OBC_COM_M2FLATHER
#   define OBC_COM_ZCHAPMAN
#  endif
#  ifdef AGRIF_OBC_M2CHARACT
#   define OBC_COM_M2CHARACT
#   define OBC_COM_ZSPECIFIED
#  endif
#  ifdef AGRIF_OBC_NBQSPECIFIED
#   define OBC_COM_NBQSPECIFIED
#  endif
#  ifdef AGRIF_OBC_NBQORLANSKI
#   define OBC_COM_NBQORLANSKI
#  endif
#endif /* CHILD */
