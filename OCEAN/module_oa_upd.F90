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
!.....module module_oa_upd
!************************************************************************
 
      module module_oa_upd

!      use module_parameter
      
      integer, parameter :: nmvar_oa = 10                               ! Nombre max de variables 'composites

      integer                                                         &
            nzupd2d_oa                                                &  ! nombres de variables 2d communes
           ,nzupd3d_oa                                                   ! nombres de variables 3d communes
 
      complex,dimension(:,:,:,:),allocatable::                        &  ! (0:meco+1,0:neco+1,1:nr,nmv_oa)
           var3d_oa
     
      complex,dimension(:,:,:),allocatable::                          &  ! (0:meco+1,0:neco+1,nmv_oa)
           var2d_oa

      integer,dimension(:,:,:),allocatable::                          &  !(nmv_oa,nb config.,nb de variables)
           tvar_oa                                                       ! localisation des variables lors de l'update

 !STDALONE     double precision, dimension(:,:,:), allocatable ::              &    
 !STDALONE          rhphat_oa_t                                                &!(0:imax+1,0:jmax+1,0:kmax+1)    
 !STDALONE         ,temhat_oa_t						                           &!(0:imax+1,0:jmax+1,0:kmax+1)    
 !STDALONE         ,salhat_oa_t 					                            !(0:imax+1,0:jmax+1,0:kmax+1)
 !STDALONE
      integer                                                         &
           des_oa (6000)                                              &
          ,nzvar_oa                                                   &    
          ,outw_oa(5,300)
                                  
      character*90                                                    &
         name_var_oa (nmvar_oa)         

      end module module_oa_upd 

#else
      module module_oa_upd_empty
      end module
#endif
