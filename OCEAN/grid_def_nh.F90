#include "cppdefs.h"
#ifdef NBQ

      subroutine grid_def_nh

!******************************************************************************************
!                   Numbering of Mass Points
!
!******************************************************************************************

      use module_nh
      use module_nbq

      implicit none

# include "param_F90.h"
# include "scalars_F90.h"
# include "grid.h"

# include "def_bounds.h"

!******************************************************************************************
! Inner MPI domain:
! cf: http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Algebrique_Numerotation_Base.htm
!******************************************************************************************

!*******************************************************************
!     Definitions: *_INTER_NBQ logical variables
!*******************************************************************

# ifdef MPI 

       WEST_INTER_NBQ  = WEST_INTER
       EAST_INTER_NBQ  = EAST_INTER
       SOUTH_INTER_NBQ = SOUTH_INTER
       NORTH_INTER_NBQ = NORTH_INTER

#  ifdef EW_PERIODIC
       if (WESTERN_EDGE) then
!         WESTERN_EDGE   = .FALSE.
          WEST_INTER_NBQ = .TRUE.
       endif
       if (EASTERN_EDGE) then
!         EASTERN_EDGE   = .FALSE.
          EAST_INTER_NBQ = .TRUE.
       endif
#  endif

#  ifdef NS_PERIODIC
       if (SOUTHERN_EDGE) then
!         SOUTHERN_EDGE   = .FALSE.
          SOUTH_INTER_NBQ = .TRUE.
       endif
       if (NORTHERN_EDGE) then
!         NORTHERN_EDGE   = .FALSE.
          NORTH_INTER_NBQ = .TRUE.
       endif
#  endif

# endif  ! MPI

!*******************************************************************
!     NH domain:  mass grid-points
!*******************************************************************

       istr_nh  = 1
       iend_nh  = LOCALLM

       jstr_nh  = 1
       jend_nh  = LOCALMM 

!......The following coef. are updated in nump_nh:
       istrq_nh = istr_nh
       jstrq_nh = jstr_nh
       iendq_nh = iend_nh 
       jendq_nh = jend_nh

!*******************************************************************
!     NH domain:  U-V velocity grid-points 
!         (coef. updated in numuvw_nh)
!*******************************************************************

       istru_nh = 2
       iendu_nh = LOCALLM 
       jstru_nh = jstr_nh
       jendu_nh = jend_nh
       istrv_nh = istr_nh
       iendv_nh = iend_nh 
       jstrv_nh = 2
       jendv_nh = LOCALMM 

# ifdef MPI 
      if (WEST_INTER_NBQ) then
       istru_nh = 1
      endif

      if (SOUTH_INTER_NBQ) then
       jstrv_nh = 1
      endif
# endif

      return

      end subroutine grid_def_nh
 
#else
        subroutine grid_def_nh_empty
        return
        end 
#endif /* NBQ */
