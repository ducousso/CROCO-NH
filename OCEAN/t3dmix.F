! $Id: t3dmix.F 1458 2014-02-03 15:01:25Z gcambon $
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
#if defined SOLVE3D && (defined TS_DIF2 || defined TS_DIF4) &&\
 (defined TS_MIX_ISO || defined TS_MIX_GEO || defined TS_MIX_S)
# if defined TS_MIX_ISO || defined TS_MIX_GEO
#  include "t3dmix_ISO.F"
# elif defined TS_MIX_S
#  include "t3dmix_S.F"
# endif
#else
      subroutine t3dmix_empty
      end
#endif

