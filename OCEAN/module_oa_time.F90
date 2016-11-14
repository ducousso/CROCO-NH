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
!.....module module_oa_time
!************************************************************************

      module module_oa_time
      
      integer                                                         & 
           nzvt_oa                                                       ! nombre de point d echantillonnage temporelle (structure temporelle du vecteur d etat)

      real                                                            & 
           unite_oa                                                      ! unite des parametres passes dnas notebook_oaavelet (h/s)
      
      integer,dimension(:),allocatable::                              &  ! (nmv_oa)
           nzpt_per_oa                                                   ! nombre de points de discretisation par periode

      real,dimension(:),allocatable::                                 &  ! (nmv_oa)
           t0_oa                                                         ! date de la premiere sortie pour swt_oa=4

      integer,dimension(:,:),allocatable::                            &  ! (3,nmv_oa)
           kount_user_oa                                                 ! description des periodes choisies par l utilisateur

      integer,dimension(:,:),allocatable::                            &  ! (2,nmvt_oa)
           kountv_oa                                                     ! calcul des kounts de debut et de fin pour chaque variable (ou configuration)

      integer*8,dimension(:),allocatable::                            &  ! (nmvp_oa)
           resv_oa                                                       ! resolution spatiale pour le calcul de la convolution


      end module module_oa_time

#else
      module module_oa_time_empty
      end module
#endif

