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
!.....module module_oa_space
!************************************************************************

      module module_oa_space

      real,dimension(:,:),allocatable::                               &  ! (2,nmv_oa)
           lat_oa                                                     &  ! latitude  min, max de la structure 2d du vecteur d etat
           ,lon_oa                                                    &  ! longitude  ...
           ,h_oa                                                      &  ! profondeur ...  
           ,ptij_oa                                                      ! point particulier demande par l utilisateur

      integer,dimension(:,:),allocatable::                            &  ! (2,nmv_oa)
           k_oa                                                          ! niveaux verticaux min et max de la structure 3d du vecteur d etat

      integer,dimension(:),allocatable::                              &  ! (nmv_oa)
           dx_oa                                                      &  ! resolution horizontale suivant x demandee par l utilisateur
           ,dy_oa                                                     &  ! resolution horizontale suivant y demandee par l utilisateur
           ,dk_oa                                                        ! resolution verticale   suivant z demandee par l utilisateur

      integer,dimension(200)::                                        &  ! changed 100 to 200 jwf 20070619
           tgv3d_oa                                                   & 
           ,tgv_oa                                                       ! type de point de la grille c auquel est associee la variable symphonie

      end module module_oa_space

#else
      module module_oa_space_empty
      end module
#endif

