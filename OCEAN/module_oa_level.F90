#include "cppdefs.h"
#ifdef ONLINE_ANALYSIS
!------------------------------------------------------------------------------
!                               NHOMS
!                Non Hydrostatic Ocean Modeling System      
!------------------------------------------------------------------------------
!
!> @note <a href="http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm"> Main web documentation </a>
!
! DESCRIPTION: 
!
!> @brief
!
!> @details 
!
!
! REVISION HISTORY:
!
!> @authors
!> @date 2015 January
!> @todo
!
!------------------------------------------------------------------------------

!************************************************************************
!.....module module_oa_level
!************************************************************************

      module module_oa_level

      use module_oa_type

      integer                                                         &
           nzlevel_oa                      

      type(type_level),dimension(:),allocatable::wlev_oa

      integer,dimension(:),allocatable::lev2v_oa
      integer,dimension(:),allocatable::v2lev_oa

      end module module_oa_level
#else
      module module_oa_level_empty
      end module
#endif
