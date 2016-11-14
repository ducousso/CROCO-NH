! $Id: biology.F 1458 2014-02-03 15:01:25Z gcambon $
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
#if defined SOLVE3D && defined BIOLOGY && defined PISCES
# include "biology_pisces.F"
#elif defined SOLVE3D && defined BIOLOGY && defined BIO_NChlPZD
# include "bio_NChlPZD.F"
#elif defined SOLVE3D && defined BIOLOGY && defined BIO_N2ChlPZD2
# include "bio_N2ChlPZD2.F"
#elif defined SOLVE3D && defined BIOLOGY && defined BIO_BioEBUS
# include "bio_BioEBUS.F"
#else
      subroutine biology_empty
      end
#endif

