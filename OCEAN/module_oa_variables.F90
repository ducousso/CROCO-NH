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
!.....module module_oa_variables
!************************************************************************

      module module_oa_variables

!      use module_parameter
      
!.....parameters:
 
      integer,parameter:: nmsimult_oa = 900000                          ! nombre maximum de fenetres ouvertes simultanement

!.....initialisation:
!STDALONE      integer :: ifl_init_oa = 0                               ! flag pour l'initialisation.
!Initialisation: call initial_main( list_of_arguments ), call main_oa( list_of_arguments )

!.....variables:

      integer                                                         &
           nzv_oa                                                     & ! nombre de variables (configurations)
           ,nzvs_oa                                                   & ! nombre total de points (taille de la structure spatiale 2d du vecteur d etat)
           ,nzvs3d_oa                                                   ! nombre total de points (taille de la structure spatiale 3d du vecteur d etat)

      integer,dimension(:),allocatable::                              & ! (nmv_oa)
           swt_d_oa                                                   & ! caracteristiques spatiales de la variable (voir notebook_wavelet)
           ,swt_t_oa                                                  & ! caracteristiques temporelles de la variable (voir notebook_wavelet)          
           ,tv_oa                                                     & ! variable symphonie associee         
           ,cnb_oa                                                    & ! call number: position of the call in the baroclinic / barotropic time step.
           ,tvc_oa                                                    & ! configuration de la variable associee
           ,updv_oa                                                     ! flag pour la mise a jour automatique d'une variable

      integer,dimension(:),allocatable::                              & ! (nmv_oa+1)
           begvs_oa                                                   & ! structure 2d du vecteur d etat
           ,begvt_oa                                                    ! structure temporelle du vecteur d etat

      integer,dimension(:),allocatable::                              & !(nmvs_oa)
           l2i_oa                                                     & ! transformation l --> i
           ,l2j_oa                                                      ! transformation l --> j

      integer,dimension(:),allocatable::                              & ! (nmvs_oa+1)
           begvs3d_oa                                                 & ! structure 3d du vecteur d etat: debut de la colonne pour un point donne
           ,kmin3d_oa                                                   ! structure 3d du vecteur d etat: premier niveau a considerer     
      
      integer,dimension(:,:,:),allocatable::                          & ! ( 0:meco+1,0:neco+1,nmv_oa)
           ij2l_oa                                                      ! transformation (i,j) --> l (l=  structure 2d du vecteur d etat)

      integer:: nzvc_oa(200)

      integer                                                         &
           flag_nrj_oa                                                  ! flag pour le calcul de l'energie
 
      integer,dimension(:),allocatable::                              & !(nmv_oa)
           save_oa                                                    & ! sauvegarde finale de la variable dans un fichier
           ,tupd_oa                                                     ! pour variables communes

      real :: pi_oa 

!     pour tester les sorties "online":

      double precision ::                                              &
       period_test_oa(10)                                             
      double precision,dimension(:,:,:),allocatable,target ::                                              &
       vardp_test_oa
!     ,vardp_test_oa(0:imax+1,0:jmax+1,0:kmax+1)

      integer:: ifl_test_oa

      !> Variables required to read OA namelists
      character(len=50), dimension(:), allocatable :: nzc_oa_names
      integer, dimension(:), allocatable           :: nzc_oa_vartyp

      !logical              :: isopycne_analysis
      character(len=200)   :: directory_in, directory_out
      character(len=1), parameter :: txtslash='/'
      integer :: io_unit


      end module module_oa_variables

#else
      module module_oa_variables_empty
      end module
#endif
