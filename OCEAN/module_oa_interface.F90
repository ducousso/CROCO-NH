#include "cppdefs.h"
#ifdef ONLINE_ANALYSIS
!------------------------------------------------------------------------------
!                               NHOMS
!                Non Hydrostatic Ocean Modeling System      
!------------------------------------------------------------------------------
!
!> @note <a href="http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm"> Main web documentation </a>
!!
!!     equipe d'oceanographie cotiere du laboratoire d'aerologie
!!     laboratoire d'aerologie
!!     cnrs - universite paul sabatier - observatoire midi pyrenees
!!     14 avenue edouard belin - 31400 toulouse - france
!
! DESCRIPTION: 
!
!> @brief Interface module to apply the OA analysis in any calling calling program (ocean models)
!
!> @details Procedures included in the module are:
!!
!! - init_oa                 : initialization of variable, domain, scales to analyse with the OA module.
!! - rdnotebook_init_oa      : reading of a first namelist which defines the number of simultaneous OA analysis to perform during the simulation.
!! - rdnotebook_oa           : reading of namelits each describing a perticular OA analysis to conduct.
!! - init_configuration_oa   : applied for "mixed" variables (composite).
!! - var_grid_oa             : initializes type of grid point (C grid).
!! - var_space_oa       : initializes the spatial parameters of the state vector to analyse.
!! - var_per_oa         : initializes the frequency parameters of the state vector to analyse. 
!! - var_time_oa        : initializes the time parameters of the state vector to analyse.
!! - var_rec_oa         : calculates the "reconstruction factor" for Morlet wavelet, Fourier and Dirac (replaces inverse transformation).
!! - var_copy_oa        : used for "mixed" variables (composite), duplicates the variable information in another variable. 
!! - upd_init_oa        : prepares output arrays var2d_oa and var3d_oa and related parameters.
!! - main_oa            : main routine which performs the time convolution between the requested field and the OA atom (Fourier, wavelet, Dirac)
!!                        method without pointer, calls function var_oa.
!! - psi_oa             : wavelet fonction
!! - psi_p2s_oa         : function which transforms the time period to the wavelet scale (s).
!! - var_oa             : function which returns the field value at one grid point i,j,k according to the analysis code
!! - var_upd_oa         : updates output arrays var2d, var3d with the analysis when available.
!! - box_oa             : initialisation of the size of the heisenberg boxes
!! - lev_init_oa        : initilizes of the isopycne levels to analyse their positions.
!! - lev_upd_oa         : updates the wlev structured type with the curret position of the target density.
!! - update_level_oa    : searches the depth interval where the a target density is reached.
!! - subsave_oa         : calls var_upd_oa (outputs in file have been removed).
!! - allocate__*_oa     : dynamic variable allocation.
!! - deallocate_*_oa    : deallocation.
!! REMOVED:
!! - struct_oa          : REMOVED (sauvegarde de la structure spatio-temporelle de la configuration).
!! - pressure_i_oa      : REMOVED (fonction pour le calcul de l'anomalie de pression).
!! - subsave_init_oa    : REMOVED (preparation des fichiers de sauvegarde)
!
! REVISION HISTORY:
!
!> @authors 
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version
!! - Benedicte Lemieux-Dudon
!!  - Namelists, June 2014
!!  - Stand-alone version + optimization, January 2015
!
!> @date 2015 January
!> @todo
!
!------------------------------------------------------------------------------

module module_interface_oa

    implicit none

    !> Maximum number of variable index
    integer, parameter :: maxvar_oa = 200 !TODO see if remains public or private?                             
    
    !> Maximum number of analysis of the same type requested simultaneously
    integer, parameter :: maxtyp_oa = 10                           

    !> Maximum number of mixed variables (composites)
    integer, parameter :: nmvar_oa = 10                              

    !> Analysed 3D fields
    !! Dimensions are space dimensions i, j, k, and the index of the analysed variable has returned by tvar_oa
    complex, dimension(:,:,:,:), allocatable :: var3d_oa
 
    !> Analysed 2D fields
    !! Dimensions are space dimensions i, j, k, and the index of the analysed variable has returned by tvar_oa
    complex, dimension(:,:,:), allocatable   :: var2d_oa

    !> Returns the index of the analysed variable (required for var3d_oa and var2d_oa)
    !! dimension 1 : index of the OA configuration-variable as requested in the namelist, e.g., isopycne analysis has OA analysis index 20
    !! dimension 2 : if several OA configuration-variable of the same type are requested, their rank, 
    !!               e.g. 1 for first OA analysis 20, 2 for the second OA analysis 20,... etc.
    !! dimension 3 : TODO CHECK IF IT IS nmvar_oa
    ! integer, dimension(1:maxvar_oa,1:maxtyp_oa,1:10)  :: tvar_oa
    integer, dimension(1:maxvar_oa,1:maxtyp_oa,1:nmvar_oa)  :: tvar_oa

    !> Size of the last dimension of arrays var2d_oa and var3d_oa, respectively
    !! Number of 2D field analysis, number of 3D field analysis, respectively 
    integer :: nzupd2d_oa, nzupd3d_oa

   
    !> Variable useful for NHOMS energy analysis
    !! kept since applied as condition in main_oa
    integer, dimension(1:6000) :: des_oa  

    ! Variable applied in NHOMS varcomp_oa : not useful in the STAND-ALONE OA version
    ! integer, dimension(5,300)  :: outw_oa

    ! Variable applied in NHOMS varmain_oa : not useful in the STAND-ALONE OA version
    ! integer :: nzvar_oa  
 
    ! Variable applied in NHOMS varmain_oa : not useful in the STAND-ALONE OA version
    ! character(len=90) :: name_var_oa (nmvar_oa)         

    ! Variable applied in NHOMS rhmoyen_oa : not useful in the STAND-ALONE OA version
    ! double precision, dimension(:,:,:), allocatable ::              &    
    !         rhphat_oa_t                                             & !(0:imax+1,0:jmax+1,0:kmax+1)    
    !        ,temhat_oa_t	                                      & !(0:imax+1,0:jmax+1,0:kmax+1)    
    !        ,salhat_oa_t 					        !(0:imax+1,0:jmax+1,0:kmax+1)
    

 !  double precision :: pi
    logical          :: isopycne_analysis


    double precision, dimension(:,:,:), allocatable, target :: anyv3d_oa

    integer  :: n3d, n2d

    character(len=200)  :: directory_in, directory_out

    public  :: init_oa, main_oa, var2d_oa, var3d_oa, tvar_oa
    private :: des_oa, nmvar_oa,                                       &
               psi_oa, psi_p2s_oa,                                     &
               isopycne_analysis,                                      &
               n3d, n2d,                                               &
               directory_in, directory_out

CONTAINS 
                                                                                         
!------------------------------------------------------------------------------
! PROCEDURE
!
!> @note
!
! DESCRIPTION: 
!
!> @brief lecture du notebook et l'initilisation
!!     des structures spatiales, frequentielles et temporelles du 
!!     vecteur d'etat.
!!
!> @details Routines:
!! 1) The way to pass arguments has been chosen: 
!! - by reference to avoid array memory duplication,
!! - by provinding its shape and index extension as returned by lbound and ubound fonctions,   
!! - to enable initialisation with the only initial_oa routine (similarly to NHOMS).
!
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version
!! - Benedicte Lemieux-Dudon
!!  - doxygen comments
!!  - routine called with arguments
!!  - cleaning of the symphonie variables
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!! - apply a selective use of module variables. 
!------------------------------------------------------------------------------

      subroutine init_oa(         &
       directory_in_oa            & 
      ,directory_out_oa           &
      ,io_unit_oa                 &
      ,iic_oa                &
      ,kount0                     &
      ,nt_max                         & 
      ,dti                     &
      ,imin, imax                 &
      ,jmin, jmax                 &
      ,kmin, kmax                 &
      ,lon_t                      & !(-1:imax+2,-1:jmax+2)            
      ,lon_t_lbound               &
      ,lon_t_ubound               &
      ,lat_t                      & !(-1:imax+2,-1:jmax+2)            
      ,lat_t_lbound               &
      ,lat_t_ubound               &
      ,lon_u                      & !(0:imax+2,0:jmax+2)              
      ,lon_u_lbound               &
      ,lon_u_ubound               &
      ,lat_u                      & !(0:imax+2,0:jmax+2)              
      ,lat_u_lbound               &
      ,lat_u_ubound               &
      ,lon_v                      & !(0:imax+2,0:jmax+2)              
      ,lon_v_lbound               &
      ,lon_v_ubound               &
      ,lat_v                      & !(0:imax+2,0:jmax+2)              
      ,lat_v_lbound               &
      ,lat_v_ubound               &
      ,lon_f                      & !(0:imax+2,0:jmax+2)              
      ,lon_f_lbound               &
      ,lon_f_ubound               &
      ,lat_f                      & !(0:imax+2,0:jmax+2)              
      ,lat_f_lbound               &
      ,lat_f_ubound               &
      ,mask_t                     & !(-1:imax+2,-1:jmax+2,0:kmax+1)
      ,mask_t_lbound              &
      ,mask_t_ubound              &
      ,mask_f                     & !(0:imax+1,0:jmax+1,0:kmax+1)
      ,mask_f_lbound              &
      ,mask_f_ubound              &
      ,mask_u                     & !(0:imax+1,0:jmax+1,0:kmax+1)
      ,mask_u_lbound              &
      ,mask_u_ubound              &
      ,mask_v                     & !(0:imax+1,0:jmax+1,0:kmax+1)
      ,mask_v_lbound              &
      ,mask_v_ubound              &
      ,h_w                        & !(0:imax+1,0:jmax+1)              
      ,h_w_lbound                 &
      ,h_w_ubound                 &
      ,h_u                        & !(0:imax+1,0:jmax+1)              
      ,h_u_lbound                 &
      ,h_u_ubound                 &
      ,h_v                        & !(0:imax+1,0:jmax+1)              
      ,h_v_lbound                 &
      ,h_v_ubound                 &
      ,h_f                        & !(0:imax+1,0:jmax+1)              
      ,h_f_lbound                 &
      ,h_f_ubound                 &
      ,rhp_t                      &
      ,rhp_t_lbound               &
      ,rhp_t_ubound               &
      ,depth_t                    & !(-1:imax+2,-1:jmax+2,0:kmax+1) 
      ,depth_t_lbound             &
      ,depth_t_ubound         ) 


      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock
     
      implicit none

      !> Time integration step (s)
      double precision, intent(in) :: dti

      !> Current model integration iteration
      integer, intent(in) :: iic_oa 

      !> First and last simulation iteration indices
      integer, intent(in) :: kount0, nt_max                                        

      !> Grid index range for the analysis of fields passed in argument to the OA module 
      !! Fields are analysed inside grid point domain [imin,imax]x[jmin,jmax]x[kmin,kmax]:
      !! - Namelist index parameters must be set consistently
      !! - Dimension extension of the fields passed in argument for analysis must be inside [imin,imax]x[jmin,jmax]x[kmin,kmax]
      integer, intent(in) :: &
       imin, imax            & 
      ,jmin, jmax            &
      ,kmin, kmax

      !> Isopycne analysis (i.e., configuraion code 20)
      !! Lower and upper index bounds of the 3-Dimensional density array
      integer, dimension(3), intent(in) :: rhp_t_lbound, rhp_t_ubound

      !> 3-Dimensional density array at t-grid point
      double precision, dimension(rhp_t_lbound(1):rhp_t_ubound(1),rhp_t_lbound(2):rhp_t_ubound(2),rhp_t_lbound(3):rhp_t_ubound(3)), intent(in) :: &
        rhp_t

      !> Isopycne analysis (i.e., configuraion code 20)
      !! Lower and upper index bounds of the 3-Dimensional depth array
      integer, dimension(3), intent(in) :: depth_t_lbound, depth_t_ubound

      !> 3-Dimensional depth array at t-grid point
      double precision, dimension(depth_t_lbound(1):depth_t_ubound(1),depth_t_lbound(2):depth_t_ubound(2),depth_t_lbound(3):depth_t_ubound(3)), intent(in) :: &
        depth_t

      ! Lower and upper index bounds of the 2-Dimensional coordinate arrays passed from the calling code
      integer, dimension(2), intent(in) ::  &
        lon_t_lbound, lon_t_ubound          & !< Lower and upper index bounds of longitude array at t-grid point
       ,lon_u_lbound, lon_u_ubound          & !< Lower and upper index bounds of longitude array at u-grid point
       ,lon_v_lbound, lon_v_ubound          & !< Lower and upper index bounds of longitude array at v-grid point
       ,lon_f_lbound, lon_f_ubound          & !< Lower and upper index bounds of longitude array at f-grid point
       ,lat_t_lbound, lat_t_ubound          & !< Lower and upper index bounds of latitude array at t-grid point
       ,lat_u_lbound, lat_u_ubound          & !< Lower and upper index bounds of latitude array at u-grid point
       ,lat_v_lbound, lat_v_ubound          & !< Lower and upper index bounds of latitude array at v-grid point
       ,lat_f_lbound, lat_f_ubound          & !< Lower and upper index bounds of latitude array at f-grid point
       ,h_w_lbound, h_w_ubound              & !< Lower and upper index bounds of ocean thickness array at w-grid point TOCHECK
       ,h_u_lbound, h_u_ubound              & !< Lower and upper index bounds of ocean thickness array at u-grid point
       ,h_v_lbound, h_v_ubound              & !< Lower and upper index bounds of ocean thickness array at v-grid point
       ,h_f_lbound, h_f_ubound                !< Lower and upper index bounds of ocean thickness array at f-grid point


      ! 2-Dimensional array coordinate arrays passed from the calling code
      double precision, dimension(lon_t_lbound(1):lon_t_ubound(1),lon_t_lbound(2):lon_t_ubound(2)), intent(in) :: lon_t  !< Longitude array at u-grid point
      double precision, dimension(lon_u_lbound(1):lon_u_ubound(1),lon_u_lbound(2):lon_u_ubound(2)), intent(in) :: lon_u  !< Longitude array at u-grid point
      double precision, dimension(lon_v_lbound(1):lon_v_ubound(1),lon_v_lbound(2):lon_v_ubound(2)), intent(in) :: lon_v  !< Longitude array at v-grid point
      double precision, dimension(lon_f_lbound(1):lon_f_ubound(1),lon_f_lbound(2):lon_f_ubound(2)), intent(in) :: lon_f  !< Longitude array at f-grid point
      double precision, dimension(lat_t_lbound(1):lat_t_ubound(1),lat_t_lbound(2):lat_t_ubound(2)), intent(in) :: lat_t  !< Latitude array at t-grid point
      double precision, dimension(lat_u_lbound(1):lat_u_ubound(1),lat_u_lbound(2):lat_u_ubound(2)), intent(in) :: lat_u  !< Latitude array at u-grid point
      double precision, dimension(lat_v_lbound(1):lat_v_ubound(1),lat_v_lbound(2):lat_v_ubound(2)), intent(in) :: lat_v  !< Latitude array at v-grid point
      double precision, dimension(lat_f_lbound(1):lat_f_ubound(1),lat_f_lbound(2):lat_f_ubound(2)), intent(in) :: lat_f  !< Latitude array at f-grid point
      double precision, dimension(h_w_lbound(1):h_w_ubound(1),h_w_lbound(2):h_w_ubound(2)), intent(in) :: h_w            !< Ocean thickness array at w-grid point TOCHECK
      double precision, dimension(h_u_lbound(1):h_u_ubound(1),h_u_lbound(2):h_u_ubound(2)), intent(in) :: h_u            !< Ocean thickness array at u-grid point
      double precision, dimension(h_v_lbound(1):h_v_ubound(1),h_v_lbound(2):h_v_ubound(2)), intent(in) :: h_v            !< Ocean thickness array at v-grid point
      double precision, dimension(h_f_lbound(1):h_f_ubound(1),h_f_lbound(2):h_f_ubound(2)), intent(in) :: h_f            !< Ocean thickness array at f-grid point           

      ! Lower and upper index bounds for the 3-Dimensional mask passed from the calling code
      integer, dimension(3), intent(in) ::  &
        mask_t_lbound, mask_t_ubound        & !< Lower and upper index bounds of mask array at t-grid point
       ,mask_u_lbound, mask_u_ubound        & !< Lower and upper index bounds of mask array at u-grid point
       ,mask_v_lbound, mask_v_ubound        & !< Lower and upper index bounds of mask array at v-grid point
       ,mask_f_lbound, mask_f_ubound          !< Lower and upper index bounds of mask array at f-grid point

      !> 3-Dimensional mask arrays passed from the calling code
      integer, dimension(mask_t_lbound(1):mask_t_ubound(1),mask_t_lbound(2):mask_t_ubound(2),mask_t_lbound(3):mask_t_ubound(3)), intent(in) :: &
       mask_t
      integer, dimension(mask_u_lbound(1):mask_u_ubound(1),mask_u_lbound(2):mask_u_ubound(2),mask_u_lbound(3):mask_u_ubound(3)), intent(in) :: &
       mask_u
      integer, dimension(mask_v_lbound(1):mask_v_ubound(1),mask_v_lbound(2):mask_v_ubound(2),mask_v_lbound(3):mask_v_ubound(3)), intent(in) :: &
       mask_v
      integer, dimension(mask_f_lbound(1):mask_f_ubound(1),mask_f_lbound(2):mask_f_ubound(2),mask_f_lbound(3):mask_f_ubound(3)), intent(in) :: &
       mask_f

      integer                                                         &
           iv_s                                                       &
          ,ic_s                                                       &
          ,i_s                                                        &
          ,j_s                                                        &
          ,k_s                                                        &
          ,ls_s                                                       &
          ,ls1_s     
                    
      character(len=200)   :: directory_in_oa, directory_out_oa
      integer :: io_unit_oa

      pi_oa = acos(-1.)

      directory_in = directory_in_oa
      directory_out = directory_out_oa
      io_unit = io_unit_oa

!.....History file:
      call history_oa(4,-1,-1,-1,-1)

!.....nombre de variables par configuration:
!     de 1 à 99 => configuration n'impliquant qu'une seule variable
      do ic_s=1,99
         nzvc_oa(ic_s) = 1
      enddo

!     From 100 to XXX => configuration including several variables
!     TODO : parameters max_single_oa_var_index = 99, min_mixed_oa_var_index = 100
      nzvc_oa(100) = 2 

!.....definition des ondelettes de morlet:

      fb_oa=2.                ! definition de l''ondelette de morlet complexe
      fc_oa=6./2./3.14159274  ! par defaut fb_oa=2. et fc_oa=6./3.14159274/2.

!.....lecture de nzc_w et nzv_w:
      call rdnotebook_init_oa

!.....allocations dynamiques:
      call allocate_part1_oa(  &
         imin, imax            &
        ,jmin, jmax            &
        ,kmin, kmax )

!.....lecture du notebook.oa:
      call rdnotebook_oa(                            &
                          imin, imax                 &
                         ,jmin, jmax                 &
                         ,kmin, kmax                 &
                         ,lon_t                      &
                         ,lon_t_lbound               &
                         ,lon_t_ubound               &
                         ,lat_t                      &
                         ,lat_t_lbound               &
                         ,lat_t_ubound               &
                         ,dti                     & 
                         ,kount0                     &
                         ,nt_max )

!TODO MERGE init_test_oa and test_oa?
      if (ifl_test_oa==1) then
        !print*, 'INIT test_oa'
        call test_oa( ichoix=0      & ! Initialisation 
          ,iic_oa=iic_oa  & 
          ,dti=dti            & 
          ,imin=imin, imax=imax     &
          ,jmin=jmin, jmax=jmax     &
          ,kmin=kmin, kmax=kmax )
       endif

!.....quelques initialisations directes:
      
!     test si calcul ou pas
      if(nzv_oa.eq.0) return      

!.....association de chaque variable a un pt de la grille c:

      call var_grid_oa

!.....initialisation de la structure spatiale:

      call var_space_oa(.false.                     & ! calculate the dimensions of the matrices associated to the spatial structure (without putting in data)
                        ,imin, imax                 &
                        ,jmin, jmax                 &
                        ,kmin, kmax                 &
                        ,lon_t                      &
                        ,lon_t_lbound               &
                        ,lon_t_ubound               &
                        ,lat_t                      &
                        ,lat_t_lbound               &
                        ,lat_t_ubound               &
                        ,lon_u                      &
                        ,lon_u_lbound               &
                        ,lon_u_ubound               &
                        ,lat_u                      &
                        ,lat_u_lbound               &
                        ,lat_u_ubound               &
                        ,lon_v                      &
                        ,lon_v_lbound               &
                        ,lon_v_ubound               &
                        ,lat_v                      &
                        ,lat_v_lbound               &
                        ,lat_v_ubound               &
                        ,lon_f                      &
                        ,lon_f_lbound               &
                        ,lon_f_ubound               &
                        ,lat_f                      &
                        ,lat_f_lbound               &
                        ,lat_f_ubound               &
                        ,mask_t                     &
                        ,mask_t_lbound              &
                        ,mask_t_ubound              &
                        ,mask_f                     &
                        ,mask_f_lbound              &
                        ,mask_f_ubound              &
                        ,mask_u                     &
                        ,mask_u_lbound              &
                        ,mask_u_ubound              &
                        ,mask_v                     &
                        ,mask_v_lbound              &
                        ,mask_v_ubound              &
                        ,h_w                        &
                        ,h_w_lbound                 &
                        ,h_w_ubound                 &
                        ,h_u                        &
                        ,h_u_lbound                 &
                        ,h_u_ubound                 &
                        ,h_v                        &
                        ,h_v_lbound                 &
                        ,h_v_ubound                 &
                        ,h_f                        &
                        ,h_f_lbound                 &
                        ,h_f_ubound )

      call allocate_part2_oa( &                       ! allocation of all spatial variables
         imin, imax           &
        ,jmin, jmax )

      call var_space_oa(.true.                      & ! calculation of values (these 3 steps are used for frequency & time as well)
                        ,imin, imax                 &
                        ,jmin, jmax                 &
                        ,kmin, kmax                 &
                        ,lon_t                      &
                        ,lon_t_lbound               &
                        ,lon_t_ubound               &
                        ,lat_t                      &
                        ,lat_t_lbound               &
                        ,lat_t_ubound               &
                        ,lon_u                      &
                        ,lon_u_lbound               &
                        ,lon_u_ubound               &
                        ,lat_u                      &
                        ,lat_u_lbound               &
                        ,lat_u_ubound               &
                        ,lon_v                      &
                        ,lon_v_lbound               &
                        ,lon_v_ubound               &
                        ,lat_v                      &
                        ,lat_v_lbound               &
                        ,lat_v_ubound               &
                        ,lon_f                      &
                        ,lon_f_lbound               &
                        ,lon_f_ubound               &
                        ,lat_f                      &
                        ,lat_f_lbound               &
                        ,lat_f_ubound               &
                        ,mask_t                     &
                        ,mask_t_lbound              &
                        ,mask_t_ubound              &
                        ,mask_f                     &
                        ,mask_f_lbound              &
                        ,mask_f_ubound              &
                        ,mask_u                     &
                        ,mask_u_lbound              &
                        ,mask_u_ubound              &
                        ,mask_v                     &
                        ,mask_v_lbound              &
                        ,mask_v_ubound              &
                        ,h_w                        &
                        ,h_w_lbound                 &
                        ,h_w_ubound                 &
                        ,h_u                        &
                        ,h_u_lbound                 &
                        ,h_u_ubound                 &
                        ,h_v                        &
                        ,h_v_lbound                 &
                        ,h_v_ubound                 &
                        ,h_f                        &
                        ,h_f_lbound                 &
                        ,h_f_ubound )

!.....initialisation boite d'heisenberg:
!     attention reprendre calcul selon ondelette
      call box_oa

!.....initialisation de la structure frequentielle:

      call var_per_oa( .false., dti )
      call allocate_part3_oa   
      call var_per_oa( .true., dti )


!.....initialisation de la structure temporelle:
      call var_time_oa(.false., kount0, nt_max, dti )
      call allocate_part4_oa
      call var_time_oa(.true., kount0, nt_max, dti )

      call allocate_part5_oa

!.....initialisation du facteur de reconstruction

      call var_rec_oa( dti )

!.....History file:        
      call history_oa(6,-1,-1,-1,-1)

!.....ecriture du fichier de structure:

      !STDALONE call struct_oa

!.....History file:
      call history_oa( 5,-1,-1,-1,-1, iic_oa, nt_max )

!.....initialisation eventuelle des "levels"
      if (isopycne_analysis) then
        !if ( present(rhp_t) .and. present(depth_t) ) then
            call lev_init_oa( rhp_t, rhp_t_lbound, rhp_t_ubound ) 
        !else
        !    stop "3D density array must be passed as argument to the OA module (see subroutine initial_oa)" 
        !endif
      endif


    !STDALONE!......initialisation de variables symphonie:
    !STDALONE
    !STDALONE      call var_upd_oa (-1,-1,-1)

    !STDALONE !......initialisation de l'energie
    !STDALONE 
    !STDALONE       call nrj_init_oa

!......preparation des variables communes pour la sauvegarde:

      call upd_init_oa( imin, imax    &
                       ,jmin, jmax    &
                       ,kmin, kmax    )

      return

      end subroutine init_oa

!------------------------------------------------------------------------------
! PROCEDURE
!
!> @note
!
! DESCRIPTION: 
!
!> @brief Reads a first namelist which defines the number and names of all the OA analysis to 
!! simultaneously perform during the simulation.
!!
!> @details The namelist name is "namelist_oa". It provides the following parameter value:
!! - nzc_oa        \: number of simultaneous OA analysis to perform during the simulation,
!! - nzc_oa_names  \: a coma seperated list of the specific names of the requested analysis,
!! - nzc_oa_vartyp \: a coma seperated list of the specific OA configuration code corresponding to
!!   the requested analysis.
!! The number and names of OA analysis to perform are applied to read the supplementary namelists 
!! that specifically describe each type of analysis. 
!! The names of the specific namelist are constructed as follows:
!! - "namelist_" \+ nzc_oa_names 
!
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version
!! - Benedicte Lemieux-Dudon
!!  - parameters in ascii file change into namelists  
!!  - cleaning of the symphonie variables
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!! - apply a selective use of module variables. 
!! - io unit parameter
!------------------------------------------------------------------------------

      subroutine rdnotebook_init_oa

      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock

      implicit none

      integer             :: ic_r  !< Configuration index
      character(len=250)  :: filin !< namelist filename

      namelist / oa_numbers / nzc_oa
      namelist / oa_names / nzc_oa_names, nzc_oa_vartyp

      filin= trim(directory_in) // txtslash // 'namelist_oa'

!TODO io_unit parameter

      open(unit=90, file = filin)
      read(unit=90, nml  = oa_numbers)
 
      oa_analysis_requested : if (nzc_oa /= 0) then
    
      call allocate_namelist_oa 

      read(unit=90, nml  = oa_names)
     
      nzv_oa = 0

      do ic_r = 1 , nzc_oa
       
         !print*, 'OA analysis names and variable types' 
         !print*, nzc_oa_names(ic_r), nzc_oa_vartyp(ic_r) 

!........A une config ic_r peut-etre associe plusieurs variables 
!        nzvc_oa est le nombre de variables impliquees dans la config
         nzv_oa = nzv_oa + nzvc_oa( nzc_oa_vartyp(ic_r) )

      enddo

      endif oa_analysis_requested 

      close(90)

      return
      end subroutine rdnotebook_init_oa

!------------------------------------------------------------------------------
! PROCEDURE
!
!> @note
!
! DESCRIPTION: 
!
!> @brief Reads each specific namelist which describes each requested OA analysis.
!!
!> @details The specific namelist describes each configuration or variable requested for analysis, 
!! with especially the horizontal and vertical part of the domain to analyse, 
!! at which simulation time steps (e.g., defines the convolution window time characteristic,...), 
!! at which time scales (e.g., wavelet scale, fourier period).
!! The specific namelist names are constructed on the basis of parameter nzc_oa_names : 
!! "namelist_" \+ nzc_oa_names \+ "_oa"
!
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version
!! - Benedicte Lemieux-Dudon
!!  - parameters in ascii file change into namelists  
!!  - cleaning of the symphonie variables
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!! - apply a selective use of module variables. 
!
!------------------------------------------------------------------------------
      subroutine rdnotebook_oa(   &
       imin, imax                 &
      ,jmin, jmax                 &
      ,kmin, kmax                 &
      ,lon_t                      &
      ,lon_t_lbound               &
      ,lon_t_ubound               &
      ,lat_t                      &
      ,lat_t_lbound               &
      ,lat_t_ubound               &
      ,dti                     &
      ,kount0                     &
      ,nt_max )

      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock
!      use module_oa_upd
!     use module_nrj

      implicit none

      !> Grid index range for the analysis of fields passed in argument to the OA module 
      !! Fields WILL BE ANALYSED at grid points inside [imin,imax]x[jmin,jmax]x[kmin,kmax]
      !! Index parameters in the namelist must be set accordingly
      !! For all fields passed in argument for analysis: 
      !! [imin,imax] must be included in [lbound(1),ubound(1)]  
      !! [jmin,jmax] must be included in [lbound(2),ubound(2)]
      !! [kmin,kmax] must be included in [lbound(3),ubound(3)]
      integer, intent(in) :: &
       imin, imax            & 
      ,jmin, jmax            &
      ,kmin, kmax

      !> Time integration step
      double precision, intent(in) :: dti
      
      !> First and last simulation iteration index
      integer, intent(in) :: kount0, nt_max                                        

      !> Lower and upper index bounds of the 2-Dimensional coordinate arrays passed from the calling code
      integer, dimension(2), intent(in) ::  &
        lon_t_lbound, lon_t_ubound          & !< Lower and upper index bounds of longitude array at t-grid point
       ,lat_t_lbound, lat_t_ubound            !< Lower and upper index bounds of latitude array at t-grid point

      !> 2-Dimensional array coordinate arrays passed from the calling code
      double precision, dimension(lon_t_lbound(1):lon_t_ubound(1),lon_t_lbound(2):lon_t_ubound(2)), intent(in) :: lon_t  !< Longitude array at t-grid point
      double precision, dimension(lat_t_lbound(1):lat_t_ubound(1),lat_t_lbound(2):lat_t_ubound(2)), intent(in) :: lat_t  !< Latitude array at t-grid point

      integer ::                                                      &
           iv_r                                                       &
          ,ip_r                                                       &
          ,ic_r                                                      

      real    ::                                                      &
           dt_r 

      integer ::                                                      &
           nzupd2d_r                                                  &
          ,nzupd3d_r

      integer :: TPSI_W,     &  !< Type of atom (0:Dirac, 1:Ondelette de Morlet, 2:Windowed Fourier, 3: Fourier)
                 SAVE_W,     &  !< Not applied (0,1:variable utilisateur,2:variable commune)
                 UPDV_W,     &  !< Variable remise à jour interactivement? Keep it to 2.
                 SWT_WFPF_W, &  !< Outputs: real part Wf (1), Energy Pf=|Wf|^2 (2) Absolute value |Wf| (4) Full complex analysis Wf
                 FL_REC_W,   &  !< Reconstruction flag 0: raw coefficient, 1: reconstruction
                 DORI_W,     &  !< Frequency analysis configuration: Discrete (1), Integration (2)
                 NZPT_PER_W, &  !< Number of points per period of analysis
                 CNB_W,      &  !< call number: position of the call in the baroclinic / barotropic time step. Not applied. Keep it to zero.
                 SWT_T_W,    &  !< Not applied. Keep it to 4. (time domain of the simulation   (1: période [T1:T2] , 2: fin , 3: date to T1, 4: all the simulation)
                 SWT_D_W,    &  !< Domaine spatial (1: région [lat1,lat2],[lon1,lon2], 2: point (I,J), 3: Région [Hmin,Hmax] dans domaine (Lat,Lon)
                 DX_W,       &  ! Résolution en X (1 point sur...)
                 DY_W,       &  ! Résolution en Y (1 point sur...)
                 DK_W           ! Résolution en Z (1 point sur...)


      integer , dimension(1:2) :: K_W    !< Niveaux verticaux Min et Max K1,K2  (-99 pour colonne entiere)

      !> Not applied : requires a routine to convert date YYYY,MM,DD,HH,MM,SS to the simulation time step index
      integer, dimension(1:6) :: DATE_DEB, & !< Annee, Mois, Jour, Heure, Min., Sec. --> ex : 2011,01,1,00,01,18
                                 DATE_FIN    !< Annee, Mois, Jour, Heure, Min., Sec. --> ex : 2011,01,1,00,15,15

      real  :: DT_W,  &         !< Temporal period in second at wich analysis are repeated (if SWT_T_W=1 ou 4)
               T0_W,  &         !< Time lag to launch the first analysis (SWT_T_W=4)
               DELTA_T_W        !< 1/2 Largeur de l'atome ou nombre de périodes par atome (1 for Dirac, 2 or more for Wavelet)


      real, dimension(1:2)  :: LAT_W,  & !< Latitude Min et Max
                               LON_W,  & !< Longitude Min et Max
                               HH_W,   & !< Profondeur Min et Max
                               PTIJ_W    !< Point particulier demande par l utilisateur

      real, dimension(1:3)  :: PER_W     !< Minimum, Maximum time period (s) (wavelets & fourier windows) or Length (Dirac brush), 0 for delta Dirac
                                         !! step determining # of periods to be examined (-99=optimum)

      character(len=250)  :: filin !< namelist filename

      !integer :: i1, i2, i3, i4, i5, i6, kdtk_out

      namelist /oa_parameters/ TPSI_W, SAVE_W, UPDV_W, SWT_WFPF_W, FL_REC_W, PER_W,    &
                               DORI_W, DELTA_T_W, NZPT_PER_W, CNB_W, SWT_T_W, SWT_D_W, &
                               DATE_DEB, DATE_FIN, DT_W, T0_W, LAT_W, LON_W,           &
                               HH_W, K_W, DX_W, DY_W, DK_W, PTIJ_W
   
      oa_analysis_requested : if (nzc_oa /=0) then
    
!.....lecture en heures ou en secondes:
!     si heures: unite_oa = 3600
!     si secondes: unite_oa = 1   
   
      unite_oa = 1
      
      nzv_oa = 0

      oa_config_loop : do ic_r = 1 , nzc_oa
 
!..... namelist lue:
 
         filin = trim(directory_in) // txtslash // 'namelist_' // trim( nzc_oa_names(ic_r) ) // '_oa'

         open(unit=90, file=filin)
         read(unit=90, nml  = oa_parameters)

         nzv_oa = nzv_oa + 1

!......lecture d'une configuration:
!
         tv_oa(nzv_oa) = nzc_oa_vartyp(ic_r)        ! type of variable (of config. nzv_oa)
         !print*, tv_oa(nzv_oa)
         
!......mise a jour de la config.:

         tc_oa (ic_r)  = tv_oa (nzv_oa)             ! type of variable(of config nzv_oa)
         begc_oa(ic_r) = nzv_oa                     ! config pointer : several variables per config

         tpsi_oa (nzv_oa)    = TPSI_W          ! type of atom
         save_oa (nzv_oa)    = SAVE_W      
         updv_oa (nzv_oa)    = UPDV_W      
         swt_wfpf_oa(nzv_oa) = SWT_WFPF_W
         fl_rec_oa(nzv_oa)   = FL_REC_W
         per_oa (1,nzv_oa)   = PER_W(1)       ! minimum period (h)
         per_oa (2,nzv_oa)   = PER_W(2)       ! maximum period (h)
         per_oa (3,nzv_oa)   = PER_W(3)       ! step determining # of periods to be examined
         dori_oa(nzv_oa)     = DORI_W
         delta_t_oa (nzv_oa) = DELTA_T_W      ! # periods per atom
         nzpt_per_oa(nzv_oa) = NZPT_PER_W     ! # calculation points per period
         cnb_oa(nzv_oa)      = CNB_W          ! call number which may be used to define where a variable needs to be called.
!        pour les bilans energetiques cette variable est mise a jour automatiquement un peu plus loin.

         !TODO check if tv_oa == 100 ajout de rhp_t?
         if ( tv_oa (nzv_oa)==20 ) then
            isopycne_analysis = .true.
         endif

         if ( per_oa(1,nzv_oa).gt. per_oa(2,nzv_oa) ) then
            write (6,*) " per_oa (1,",nzv_oa,") trop grand"
            stop
         endif  

         if (per_oa (2,nzv_oa).ne.per_oa (1,nzv_oa).and.                        &
              per_oa (3,nzv_oa).gt.(per_oa (2,nzv_oa)-per_oa (1,nzv_oa))        &
              .and.per_oa (3,nzv_oa).ne.-99.) then                                                         
            write (6,*) " per_oa (3,",nzv_oa,") trop grand"
            stop
         endif  
         
         do ip_r = 1 , 3
            per_oa  (ip_r,nzv_oa) = per_oa(ip_r,nzv_oa) * unite_oa 
         enddo

         swt_t_oa(nzv_oa) = SWT_T_W
         swt_d_oa(nzv_oa) = SWT_D_W
       
         if  (swt_t_oa(nzv_oa) /= 4) then 
            
            print*, 'ERRROR'
            print*, 'This option is not allowed (developments required), please set SWT_T_W to 4 in the OA namelist'  
            stop
!STDALONE : requires a subroutine to convert date YYYY, MM, DD, HH, MM, SS to the model time step index
!TODO include datetokount in module_interface_oa
!        i1 = DATE_DEB(1)
!        i2 = DATE_DEB(2)
!        i3 = DATE_DEB(3)
!        i4 = DATE_DEB(4)
!        i5 = DATE_DEB(5)
!        i6 = DATE_DEB(6)
!        call datetokount(i1,i2,i3,i4,i5,i6)
!        kount_user_oa(1,nzv_oa) = kdtk_out
!        kount_user_oa(1,nzv_oa) =!        
!        kount_user_oa(1,nzv_oa) =!        i1 = DATE_FIN(1)
!        kount_user_oa(1,nzv_oa) =!        i2 = DATE_FIN(2)
!        kount_user_oa(1,nzv_oa) =!        i3 = DATE_FIN(3)
!        kount_user_oa(1,nzv_oa) =!        i4 = DATE_FIN(4)
!        kount_user_oa(1,nzv_oa) =!        i5 = DATE_FIN(5)
!        kount_user_oa(1,nzv_oa) =!        i6 = DATE_FIN(6)
!        kount_user_oa(1,nzv_oa) =!
!        kount_user_oa(1,nzv_oa) =!        call datetokount(i1,i2,i3,i4,i5,i6)
!        kount_user_oa(2,nzv_oa) = kdtk_out
                     
         else if ( swt_t_oa(nzv_oa) == 4) then
            kount_user_oa(1,nzv_oa) = kount0
            kount_user_oa(2,nzv_oa) = nt_max-1
         endif
         
         dt_r = DT_W

         kount_user_oa(3,nzv_oa) = max(1,int ( dt_r * unite_oa / dti ))
         kount_user_oa(3,nzv_oa) = max(1,int ( dt_r * unite_oa / dti ))
         
         if (swt_t_oa(nzv_oa).eq.1.and.kount_user_oa(2,nzv_oa).lt.kount_user_oa(1,nzv_oa))   &
              write (6,*) "probleme dates choisies t1 > t2" 
         
         if (swt_t_oa(nzv_oa).eq.1.and.kount_user_oa(1,nzv_oa).gt.nt_max-1)                      &
              write (6,*) "probleme dates choisies t1 > t final"

         t0_oa(nzv_oa) = T0_W
         t0_oa(nzv_oa) = t0_oa(nzv_oa) * unite_oa
         kount_user_oa(1,nzv_oa) = kount_user_oa(1,nzv_oa) + int(t0_oa(nzv_oa) / dti) 

         lat_oa(1,nzv_oa) = LAT_W(1)
         lat_oa(2,nzv_oa) = LAT_W(2)
         lon_oa(1,nzv_oa) = LON_W(1)
         lon_oa(2,nzv_oa) = LON_W(2)
         h_oa(1,nzv_oa) = HH_W(1)
         h_oa(2,nzv_oa) = HH_W(2)

         k_oa(1,nzv_oa) = K_W(1)
         k_oa(2,nzv_oa) = K_W(2)

         if (k_oa(1,nzv_oa).eq.-99) then
            k_oa(1,nzv_oa) = 1 
         endif
         if (k_oa(2,nzv_oa).eq.-99) then
         !STDALONE CHECK kmax
         !   k_oa(2,nzv_oa) = kmax+1
            k_oa(2,nzv_oa) = kmax
         endif
        
         dx_oa(nzv_oa) = DX_W
         dy_oa(nzv_oa) = DY_W
         dk_oa(nzv_oa) = DK_W
         ptij_oa(1,nzv_oa) = PTIJ_W(1)
         ptij_oa(2,nzv_oa) = PTIJ_W(2)

#ifdef SPHERICAL
!        lat_oa(1,nzv_oa)=lat_oa(1,nzv_oa)*pi_oa/180.
!        lat_oa(2,nzv_oa)=lat_oa(2,nzv_oa)*pi_oa/180.     
!        lon_oa(1,nzv_oa)=lon_oa(1,nzv_oa)*pi_oa/180.
!        lon_oa(2,nzv_oa)=lon_oa(2,nzv_oa)*pi_oa/180.
#endif

!  attention, test uniquement sur les points z et aps sur les points,x,y,rot

         if (( (lat_oa(1,nzv_oa).lt.lat_t(imin,jmin).and.             &
              lat_oa(2,nzv_oa).lt.lat_t(imin,jmin)).or.              &
              (lat_oa(1,nzv_oa).gt.lat_t(imax,jmax).and.             &
              lat_oa(2,nzv_oa).gt.lat_t(imax,jmax))).and.              &
              (swt_d_oa(nzv_oa).eq.1.or.swt_d_oa(nzv_oa).eq.3) ) then
     !      write(6,*) nzv_oa,lat_oa(1,nzv_oa),lat_oa(2,nzv_oa)
     !      write(6,*) lat_t(imin,jmin),lat_t(imax,jmax)
            write (6,*) " lat_oa hors bornes!"
            stop
         endif

         if (( (lon_oa(1,nzv_oa).lt.lon_t(imin,jmin).and.             &
              lon_oa(2,nzv_oa).lt.lon_t(imin,jmin)).or.              &
              (lon_oa(1,nzv_oa).gt.lon_t(imax,jmax).and.             &
              lon_oa(2,nzv_oa).gt.lon_t(imax,jmax))).and.              &
              (swt_d_oa(nzv_oa).eq.1.or.swt_d_oa(nzv_oa).eq.3) ) then
            write (6,*) " lon_oa trop grand ou trop petit!"
            stop
         endif
         
         if (  k_oa(1,nzv_oa).gt. k_oa(2,nzv_oa).or. k_oa(2,nzv_oa).gt.kmax+1) then
            write (6,*) " k_oa trop grand!"
            stop
         endif

!STDALONE configuration treatment and nrj eliminated

!.....test validity of the namelist parameters
!      call test_domain_validity_oa

!.....treatment of "mixed" variable configuration
         call init_test_oa(nzv_oa)
 
!.....treatment of "mixed" variable configuration
         call init_configuration_oa

      enddo oa_config_loop

!.....desallocation tableau de lecture des namelists OA

      call deallocate_namelist_oa

!.....fin de la derniere configuration:

      begc_oa(nzc_oa + 1) = nzv_oa + 1

      close(90)

!.....quelques controles:

      do ic_r=1,nzc_oa
         do iv_r=begc_oa(ic_r),begc_oa(ic_r+1)-1
            if (tpsi_oa(iv_r).eq.3) swt_t_oa(iv_r) = 4
         enddo
      enddo

      endif oa_analysis_requested

      return

      end subroutine rdnotebook_oa

!------------------------------------------------------------------------------
! PROCEDURE
!
!> @note
!
! DESCRIPTION: 
!
!> @brief Test the validity of some of the namelist parameters. 
!
!> @details Tests: 
!! - horizontal domain
!! -
!
! REVISION HISTORY:
!
!> @authors
!> @date 2015 January
!> @todo
!
!------------------------------------------------------------------------------


      subroutine test_domain_validity_oa


      end subroutine test_domain_validity_oa

!------------------------------------------------------------------------------
! PROCEDURE
!
!> @note This routine must be implemented to introduce new configuration with mixed variables,
!! and/or when you wish to call main_oa at different location of the calling code (e.g., 
!! time-splitting case where an updated variable is available at different location).
!
! DESCRIPTION: 
!
!> @brief Handles the configuration code corresponding to "mixed" variables. 
!
!> @details Mixed variables are encounterd for configuration that requires several variables of the calling code
!! to construct a "mixed variable" on the basis of "simple" variables, 
!! e.g., kinetic energy calculated from zonal and meridional velocities. 
!! 
!! Use example configuration 100 to construct new configuration with mixed variables 
!! - Configuration code from 1 to 99 corresponds to "simple" variable.
!! - Configuration code from 100 to XX corresponds to "mixed" variables. 
!! Parameter tvc_oa stores the configuration codes as requested in the OA namelist. 
!!
!! Default cnb_oa parameter is set to zero for all variables. 
!! cnb_oa can be used to call main_oa at different location of the calling code (e.g., 
!! time-splitting case where an updated variable is available at different location).
!! In such case, change the cnb_oa parameter for particular variable, and adapt main_oa accodingly.
!
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version
!! - Benedicte Lemieux-Dudon
!!  - cleaning of the specific symphonie variable treatment.
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!! - apply a selective use of module variables. 
!
!------------------------------------------------------------------------------
      subroutine init_configuration_oa

      use module_oa_variables

      implicit none

      integer :: tmp_nzv_oa

!.....configuration a multiples variables:

!-------------------------------------------
!     Example of mixed variable configuration (100)
!-------------------------------------------
         !TODO add rhp_t?
         if (tv_oa(nzv_oa).eq.100) then

            tmp_nzv_oa = nzv_oa

!------>1ere variable: velocity vel_u 
            tv_oa(nzv_oa) = 1
            nzv_oa = nzv_oa + 1

!     copie toutes les infos pour la nouvelle variable:
            !call var_copy_oa ( nzv_oa - 1 , nzv_oa )
            call var_copy_oa ( tmp_nzv_oa , nzv_oa )

!------>2eme variable: velocity vel_v
            tv_oa(nzv_oa) = 2
            nzv_oa = nzv_oa + 1

!     copie toutes les infos pour la nouvelle variable:
            !call var_copy_oa ( nzv_oa - 2 , nzv_oa )
            call var_copy_oa ( tmp_nzv_oa , nzv_oa )

!------>3eme variable: velocity vel_w
            tv_oa(nzv_oa) = 3
            nzv_oa = nzv_oa + 1

!     copie toutes les infos pour la nouvelle variable:
            !call var_copy_oa ( nzv_oa - 2 , nzv_oa )
            call var_copy_oa ( tmp_nzv_oa , nzv_oa )

         endif

! Uncomment these lines and adapt them if you wish to call the OA treatment at specific
! point of the code (cf time splitting), for specific variables that are updated
! at some irregular time step or code place. 
!-------------------------------------------
!   Eventuellement point de sortie pour les variables
!      associees aux bilans energetiques
!-------------------------------------------
         ! if (tv_oa(nzv_oa).eq.30) then
         !    outw_oa(5,2) =1
         !    outw_oa(5,5) =1
         !    cnb_oa(nzv_oa)=-30
         !    des_oa(nzv_oa)=-5005
         ! endif

      end subroutine init_configuration_oa

!------------------------------------------------------------------------------
! PROCEDURE
!
!> @note
!
! DESCRIPTION: 
!
!> @brief prepares the OA test variable.
!
!> @details 
!
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version
!! - Benedicte Lemieux-Dudon
!!  - cleaning and selective use of module variables.
!> @date 2015 January
!> @todo
!
!------------------------------------------------------------------------------
      subroutine init_test_oa(izv_oa)

      use module_oa_variables , only : ifl_test_oa, tv_oa

      implicit none

      integer, intent(in) :: izv_oa

!-------------------------------------------
!     Test configuration (99)
!-------------------------------------------
         if (tv_oa(izv_oa).eq.99) then
            ifl_test_oa = 1
         endif

      end subroutine init_test_oa

!------------------------------------------------------------------------------
! PROCEDURE
!
!> @note
!
! DESCRIPTION: 
!
!> @brief Handles "mixed" variables configuration. 
!
!> @details Configuration codes for mixed variables involves several variables,
!  and requires to copy out configuration informations to the variable parameters. 
!
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version and algorithm.
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!! - apply a selective use of module variables. 
!
!------------------------------------------------------------------------------

      subroutine var_copy_oa (                                       &
                  nzvold_c                                           &             
                 ,nzvnew_c   )

      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock

      implicit none
      integer                                                         &
        nzvold_c                                                      &
       ,nzvnew_c

        tv_oa  (nzvnew_c)         = tv_oa  (nzvold_c)
        t0_oa  (nzvnew_c)         = t0_oa  (nzvold_c)
        tpsi_oa(nzvnew_c)         = tpsi_oa(nzvold_c)      
        save_oa(nzvnew_c)         = save_oa(nzvold_c)
        updv_oa(nzvnew_c)         = updv_oa(nzvold_c)
        swt_wfpf_oa(nzvnew_c)     = swt_wfpf_oa(nzvold_c)
        fl_rec_oa(nzvnew_c)       = fl_rec_oa(nzvold_c)
        per_oa (1,nzvnew_c)       = per_oa (1,nzvold_c)
        per_oa (2,nzvnew_c)       = per_oa (2,nzvold_c)
        per_oa (3,nzvnew_c)       = per_oa (3,nzvold_c) 
        delta_t_oa(nzvnew_c)      = delta_t_oa(nzvold_c)
        nzpt_per_oa(nzvnew_c)     = nzpt_per_oa(nzvold_c)
        cnb_oa(nzvnew_c)          = cnb_oa(nzvold_c)
        dori_oa(nzvnew_c)         = dori_oa(nzvold_c)
        swt_t_oa(nzvnew_c)        = swt_t_oa(nzvold_c)
        swt_d_oa(nzvnew_c)        = swt_d_oa(nzvold_c)
        kount_user_oa(1,nzvnew_c) = kount_user_oa(1,nzvold_c)
        kount_user_oa(2,nzvnew_c) = kount_user_oa(2,nzvold_c)
        kount_user_oa(3,nzvnew_c) = kount_user_oa(3,nzvold_c)
        lat_oa(1,nzvnew_c)        = lat_oa(1,nzvold_c)
        lat_oa(2,nzvnew_c)        = lat_oa(2,nzvold_c)
        lon_oa(1,nzvnew_c)        = lon_oa(1,nzvold_c)
        lon_oa(2,nzvnew_c)        = lon_oa(2,nzvold_c)
        h_oa(1,nzvnew_c)          = h_oa(1,nzvold_c) 
        h_oa(2,nzvnew_c)          = h_oa(2,nzvold_c)
        k_oa(1,nzvnew_c)          = k_oa(1,nzvold_c)
        k_oa(2,nzvnew_c)          = k_oa(2,nzvold_c)
        dx_oa(nzvnew_c)           = dx_oa(nzvold_c)
        dy_oa(nzvnew_c)           = dy_oa(nzvold_c)
        dk_oa(nzvnew_c)           = dk_oa(nzvold_c)
        ptij_oa(1,nzvnew_c)       = ptij_oa(1,nzvold_c)
        ptij_oa(2,nzvnew_c)       = ptij_oa(2,nzvold_c)

       return
       end subroutine var_copy_oa

!------------------------------------------------------------------------------
! PROCEDURE
!
!> @note
!
! DESCRIPTION: 
!
!> @brief The size of the horizontal and vertical domain over which analysis will be conducted 
!! is calculated, i.e., nzvs_oa and nzvs3d_oa respectively (flag_s equals true). 
!! All the variable analysis are stored in a single vector, called state vector, 
!! which piles up all the domain points requested for analysis. 
!! Variables enabling conversion from vector index to domain indices 
!! are initialized (flag_s equals false).
!! initialise la structure spatiale du vecteur d'etat tel qu'il a ete specifie dans le notebook_oa. 
!
!> @details Spatial domain options are set in the OA namelist with SWT_D_W parameter.  
!! - if set to 1: analysis is requested over the region [lat1,lat2],[lon1,lon2]. 
!! - if set to 2: analysis is requested at point (I,J).
!! - if set to 3: analysis is requested over the ocean colum [Hmin,Hmax] in the horizontal 
!!   domain the (Lat,Lon)
!! The domain to analyse is more over defined by the namelist parameters:
!! - dx_oa(iv_g), dy_oa(iv_g) : steps in i,j indices
!! - k_oa(1,iv_g),k_oa(2,iv_g),dk_oa(iv_g) : first, last k indices and k index step
!!
!! Variables:
!! - nzvs_oa,   begvs_oa    : 2d struture of the state vector
!! - nzvs3d_oa, begvs3d_oa  : 3d structure of the state vector
!! - ij2l_oa, l2i_oa, l2j_oa : conversion l <--> (i,j)
!!
!! The section of the state vector defined by indices begvs3d_oa(ls_l), begvs3d_oa(ls_l+1)-1
!! corresponds to the points to analyse over one given (i,j) ocean column for a given variable
!! with index iv_g. For each variable, the number of (i,j) points to analyse is given by index ls_l
!! which ranges from begvs_oa(iv_g) to begvs_oa(iv_g+1)-1. 
!!
!! For each variables: 
!! - the grid point is needed to get the mask value, i.e., tgv_oa.
!! - the 2d/3d attribute is required to construct the vertical part of the state vector, i.e., tgv3d_oa.
!!
!! The kmin3d_oa parameter stores the lowest analysed k index.
!
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version
!! - Benedicte Lemieux-Dudon
!!  - stand alone version 
!!  - field dimension range changed to [imin,imax]x[jmin,jmax]x[kmin,kmax].
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!! - apply a selective use of module variables. 
!------------------------------------------------------------------------------
      subroutine var_space_oa(    &
       flag_s                     &
      ,imin, imax                 &
      ,jmin, jmax                 &
      ,kmin, kmax                 &
      ,lon_t                      & !(-1:imax+2,-1:jmax+2)            
      ,lon_t_lbound               &
      ,lon_t_ubound               &
      ,lat_t                      & !(-1:imax+2,-1:jmax+2)            
      ,lat_t_lbound               &
      ,lat_t_ubound               &
      ,lon_u                      & !(0:imax+2,0:jmax+2)              
      ,lon_u_lbound               &
      ,lon_u_ubound               &
      ,lat_u                      & !(0:imax+2,0:jmax+2)              
      ,lat_u_lbound               &
      ,lat_u_ubound               &
      ,lon_v                      & !(0:imax+2,0:jmax+2)              
      ,lon_v_lbound               &
      ,lon_v_ubound               &
      ,lat_v                      & !(0:imax+2,0:jmax+2)              
      ,lat_v_lbound               &
      ,lat_v_ubound               &
      ,lon_f                      & !(0:imax+2,0:jmax+2)              
      ,lon_f_lbound               &
      ,lon_f_ubound               &
      ,lat_f                      & !(0:imax+2,0:jmax+2)              
      ,lat_f_lbound               &
      ,lat_f_ubound               &
      ,mask_t                     & !(-1:imax+2,-1:jmax+2,0:kmax+1)
      ,mask_t_lbound              &
      ,mask_t_ubound              &
      ,mask_f                     & !(0:imax+1,0:jmax+1,0:kmax+1)
      ,mask_f_lbound              &
      ,mask_f_ubound              &
      ,mask_u                     & !(0:imax+1,0:jmax+1,0:kmax+1)
      ,mask_u_lbound              &
      ,mask_u_ubound              &
      ,mask_v                     & !(0:imax+1,0:jmax+1,0:kmax+1)
      ,mask_v_lbound              &
      ,mask_v_ubound              &
      ,h_w                        & !(0:imax+1,0:jmax+1)              
      ,h_w_lbound                 &
      ,h_w_ubound                 &
      ,h_u                        & !(0:imax+1,0:jmax+1)              
      ,h_u_lbound                 &
      ,h_u_ubound                 &
      ,h_v                        & !(0:imax+1,0:jmax+1)              
      ,h_v_lbound                 &
      ,h_v_ubound                 &
      ,h_f                        & !(0:imax+1,0:jmax+1)              
      ,h_f_lbound                 &
      ,h_f_ubound )

      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock

      implicit none

      !> Grid index range for the analysis of fields passed in argument to the OA module 
      !! Fields are analysed inside grid point domain [imin,imax]x[jmin,jmax]x[kmin,kmax]:
      integer, intent(in) :: &
       imin, imax            & 
      ,jmin, jmax            &
      ,kmin, kmax

      logical, intent(in) :: flag_s          !< Flag to distinguish calls to var_space_oa before and after OA array allocation

      ! Lower and upper index bounds of the 2-Dimensional coordinate arrays passed from the calling code
      integer, dimension(2), intent(in) ::  &
        lon_t_lbound, lon_t_ubound          & !< Lower and upper index bounds of longitude array at t-grid point
       ,lon_u_lbound, lon_u_ubound          & !< Lower and upper index bounds of longitude array at u-grid point
       ,lon_v_lbound, lon_v_ubound          & !< Lower and upper index bounds of longitude array at v-grid point
       ,lon_f_lbound, lon_f_ubound          & !< Lower and upper index bounds of longitude array at f-grid point
       ,lat_t_lbound, lat_t_ubound          & !< Lower and upper index bounds of latitude array at t-grid point
       ,lat_u_lbound, lat_u_ubound          & !< Lower and upper index bounds of latitude array at u-grid point
       ,lat_v_lbound, lat_v_ubound          & !< Lower and upper index bounds of latitude array at v-grid point
       ,lat_f_lbound, lat_f_ubound          & !< Lower and upper index bounds of latitude array at f-grid point
       ,h_w_lbound, h_w_ubound              & !< Lower and upper index bounds of ocean thickness array at w-grid point TOCHECK
       ,h_u_lbound, h_u_ubound              & !< Lower and upper index bounds of ocean thickness array at u-grid point
       ,h_v_lbound, h_v_ubound              & !< Lower and upper index bounds of ocean thickness array at v-grid point
       ,h_f_lbound, h_f_ubound                !< Lower and upper index bounds of ocean thickness array at f-grid point


      ! 2-Dimensional array coordinate arrays passed from the calling code
      double precision, dimension(lon_t_lbound(1):lon_t_ubound(1),lon_t_lbound(2):lon_t_ubound(2)), intent(in) :: lon_t  !< Longitude array at t-grid point
      double precision, dimension(lon_u_lbound(1):lon_u_ubound(1),lon_u_lbound(2):lon_u_ubound(2)), intent(in) :: lon_u  !< Longitude array at u-grid point
      double precision, dimension(lon_v_lbound(1):lon_v_ubound(1),lon_v_lbound(2):lon_v_ubound(2)), intent(in) :: lon_v  !< Longitude array at v-grid point
      double precision, dimension(lon_f_lbound(1):lon_f_ubound(1),lon_f_lbound(2):lon_f_ubound(2)), intent(in) :: lon_f  !< Longitude array at f-grid point
      double precision, dimension(lat_t_lbound(1):lat_t_ubound(1),lat_t_lbound(2):lat_t_ubound(2)), intent(in) :: lat_t  !< Latitude array at t-grid point
      double precision, dimension(lat_u_lbound(1):lat_u_ubound(1),lat_u_lbound(2):lat_u_ubound(2)), intent(in) :: lat_u  !< Latitude array at u-grid point
      double precision, dimension(lat_v_lbound(1):lat_v_ubound(1),lat_v_lbound(2):lat_v_ubound(2)), intent(in) :: lat_v  !< Latitude array at v-grid point
      double precision, dimension(lat_f_lbound(1):lat_f_ubound(1),lat_f_lbound(2):lat_f_ubound(2)), intent(in) :: lat_f  !< Latitude array at f-grid point
      double precision, dimension(h_w_lbound(1):h_w_ubound(1),h_w_lbound(2):h_w_ubound(2)), intent(in) :: h_w            !< Ocean thickness array at w-grid point TOCHECK
      double precision, dimension(h_u_lbound(1):h_u_ubound(1),h_u_lbound(2):h_u_ubound(2)), intent(in) :: h_u            !< Ocean thickness array at u-grid point
      double precision, dimension(h_v_lbound(1):h_v_ubound(1),h_v_lbound(2):h_v_ubound(2)), intent(in) :: h_v            !< Ocean thickness array at v-grid point
      double precision, dimension(h_f_lbound(1):h_f_ubound(1),h_f_lbound(2):h_f_ubound(2)), intent(in) :: h_f            !< Ocean thickness array at f-grid point           

      ! Lower and upper index bounds for the 3-Dimensional mask passed from the calling code
      integer, dimension(3), intent(in) ::  &
        mask_t_lbound, mask_t_ubound        & !< Lower and upper index bounds of mask array at t-grid point
       ,mask_u_lbound, mask_u_ubound        & !< Lower and upper index bounds of mask array at t-grid point
       ,mask_v_lbound, mask_v_ubound        & !< Lower and upper index bounds of mask array at t-grid point
       ,mask_f_lbound, mask_f_ubound          !< Lower and upper index bounds of mask array at t-grid point

      !> 3-Dimensional mask arrays passed from the calling code
      integer,dimension(mask_t_lbound(1):mask_t_ubound(1),mask_t_lbound(2):mask_t_ubound(2),mask_t_lbound(3):mask_t_ubound(3)), intent(in) :: &
       mask_t
      integer,dimension(mask_u_lbound(1):mask_u_ubound(1),mask_u_lbound(2):mask_u_ubound(2),mask_u_lbound(3):mask_u_ubound(3)), intent(in) :: &
       mask_u
      integer,dimension(mask_v_lbound(1):mask_v_ubound(1),mask_v_lbound(2):mask_v_ubound(2),mask_v_lbound(3):mask_v_ubound(3)), intent(in) :: &
       mask_v
      integer,dimension(mask_f_lbound(1):mask_f_ubound(1),mask_f_lbound(2):mask_f_ubound(2),mask_f_lbound(3):mask_f_ubound(3)), intent(in) :: &
       mask_f


      integer :: i, j, k                                              &                          
           ,iv_g                                                      &
           ,mykmin                                                    &
           ,msk_g

      real    ::                                                      &
            lat_g                                                     &
           ,lon_g                                                     &
           ,h_g                                                         


      nzvs_oa = 0
      nzvs3d_oa = 0      

      do iv_g = 1 , nzv_oa

!.....test k_oa type 1,jamais de niveau kmax+1:
! TODO kmax+1 ok? kmax refer to kmax t-centered cells
         if (k_oa(2,iv_g).eq.kmax+1.and.(tgv_oa(tv_oa(iv_g)).eq.1.or.tgv_oa(tv_oa(iv_g)).eq.2 .or.tgv_oa(tv_oa(iv_g)).eq.3 )) then
            k_oa(2,iv_g)=kmax
            if (k_oa(1,iv_g).eq.kmax+1) k_oa(1,iv_g)=kmax
         endif
         
         if (flag_s) begvs_oa( iv_g ) = nzvs_oa + 1

!.....configuration spatiale n°1 ou 3:

         if (swt_d_oa(iv_g).eq.1.or.swt_d_oa(iv_g).eq.3) then
          !STDALONE  do i = 0 , imax + 1 , dx_oa(iv_g)
          !STDALONE     do j = 0 , jmax + 1 , dy_oa(iv_g)
            do i = imin, imax, dx_oa(iv_g)
               do j = jmin, jmax, dy_oa(iv_g)

                  if (mod(tgv_oa(tv_oa(iv_g)),5).eq.1) then
                     h_g   = h_w     (i,j)
                     lat_g = lat_t  (i,j)
                     lon_g = lon_t  (i,j)
           !STDALONE depending on the staggered grid last k-level kmax+1 may exist or not 
           !STDALONE The calling code prerequisite is having kmax t-centered cells
           !STDALONE TODO CHECK msk_g = mask_t(i,j,kmax+1) 
                     msk_g = mask_t(i,j,kmax) 
                  elseif (mod(tgv_oa(tv_oa(iv_g)),5).eq.2) then
                     h_g   = h_u     (i,j)
                     lat_g = lat_u  (i,j)
                     lon_g = lon_u  (i,j)
           !STDALONE TODO CHECK msk_g = mask_u(i,j,kmax+1) 
                     msk_g = mask_u(i,j,kmax) 
                  elseif (mod(tgv_oa(tv_oa(iv_g)),5).eq.3) then
                     h_g   = h_v     (i,j)
                     lat_g = lat_v  (i,j)
                     lon_g = lon_v  (i,j)
            !STDALONE TODO CHECK msk_g = mask_v(i,j,kmax+1) 
                     msk_g = mask_v(i,j,kmax) 
                  else
                     h_g   = h_f     (i,j)
                     lat_g = lat_f  (i,j)
                     lon_g = lon_f  (i,j)
            !STDALONE TODO CHECK msk_g = mask_f(i,j,kmax+1) 
                     msk_g = mask_f(i,j,kmax) 
                  endif

                  if (                                                &
                       msk_g.eq.1             .and.                   &
                       lat_g.ge.lat_oa(1,iv_g) .and.                  &
                       lat_g.le.lat_oa(2,iv_g) .and.                  &
                       lon_g.ge.lon_oa(1,iv_g) .and.                  &
                       lon_g.le.lon_oa(2,iv_g) .and.                  &
                       ( (h_g.ge.h_oa(1,iv_g)  .and.                  &
                       h_g.le.h_oa(2,iv_g) )                          &
                       .or.swt_d_oa(iv_g).ne.3) )                     &
                       then

!-------->structure 2d:

                     nzvs_oa          = nzvs_oa + 1
                     if (flag_s) then
                      l2i_oa (nzvs_oa)  = i
                      l2j_oa (nzvs_oa)  = j
                      ij2l_oa(i,j,iv_g)= nzvs_oa
                     endif

!-------->structure 3d:
                     
                     mykmin=0
                     if (flag_s) begvs3d_oa(nzvs_oa) = nzvs3d_oa + 1
                     if (tgv3d_oa(tv_oa(iv_g)).eq.2) then
                        nzvs3d_oa          = nzvs3d_oa + 1
                     else
                        do k = k_oa(1,iv_g),k_oa(2,iv_g),dk_oa(iv_g)
                           if (mod(tgv_oa(tv_oa(iv_g)),5).eq.1) then
                              msk_g = mask_t(i,j,k) 
                           elseif (mod(tgv_oa(tv_oa(iv_g)),5).eq.2) then
                              msk_g = mask_u(i,j,k) 
                           elseif (mod(tgv_oa(tv_oa(iv_g)),5).eq.3) then
                              msk_g = mask_v(i,j,k) 
                           else
                              msk_g = mask_f(i,j,k) 
                           endif
                           if ( msk_g.eq.1 ) then
                              if ( mykmin.eq.0.and.flag_s ) then
                                 kmin3d_oa(nzvs_oa)=k            
                                 mykmin=1
                              endif
                              nzvs3d_oa          = nzvs3d_oa + 1
                           endif
                        enddo
                     endif
                     
                  endif
               enddo
            enddo
         endif
         
!.....configuration spatiale n°2:

         if (swt_d_oa(iv_g).eq.2) then
            i = ptij_oa(1,iv_g)
            j = ptij_oa(2,iv_g)
            if (mod(tgv_oa(tv_oa(iv_g)),5).eq.1) then
            !STDALONE TODO CHECK msk_g = mask_t(i,j,kmax+1) 
               msk_g = mask_t(i,j,kmax) 
            elseif (mod(tgv_oa(tv_oa(iv_g)),5).eq.2) then
            !STDALONE TODO CHECK msk_g = mask_u(i,j,kmax+1)
               msk_g = mask_u(i,j,kmax) 
            elseif (mod(tgv_oa(tv_oa(iv_g)),5).eq.3) then
            !STDALONE TODO CHECK msk_g = mask_v(i,j,kmax+1) 
               msk_g = mask_v(i,j,kmax) 
            else
            !STDALONE TODO CHECK msk_g = mask_f(i,j,kmax+1)  
               msk_g = mask_f(i,j,kmax) 
            endif
            if ( msk_g.eq.1 ) then
               nzvs_oa          = nzvs_oa + 1

               if (flag_s) then
                l2i_oa (nzvs_oa)  = ptij_oa(1,iv_g)
                l2j_oa (nzvs_oa)  = ptij_oa(2,iv_g)
                ij2l_oa(i,j,iv_g)= nzvs_oa
               endif

!-------->structure 3d:

               mykmin=0
               if (flag_s) begvs3d_oa(nzvs_oa) = nzvs3d_oa + 1
               if (tgv3d_oa(tv_oa(iv_g)).eq.2) then
                  nzvs3d_oa          = nzvs3d_oa + 1
               else
                  do k = k_oa(1,iv_g),k_oa(2,iv_g),dk_oa(iv_g)
                     if (mod(tgv_oa(tv_oa(iv_g)),5).eq.1) then
                        msk_g = mask_t(i,j,k) 
                     elseif (mod(tgv_oa(tv_oa(iv_g)),5).eq.2) then
                        msk_g = mask_u(i,j,k) 
                     elseif (mod(tgv_oa(tv_oa(iv_g)),5).eq.3) then
                        msk_g = mask_v(i,j,k) 
                     else
                        msk_g = mask_f(i,j,k) 
                     endif
                     if ( msk_g.eq.1 ) then
                        if ( mykmin.eq.0.and.flag_s ) then
                           kmin3d_oa(nzvs_oa)=k            
                           mykmin=1
                        endif
                        nzvs3d_oa          = nzvs3d_oa + 1
                     endif
                  enddo
               endif

            endif

         endif

      enddo

!.....last point:
      if (flag_s) then
       begvs3d_oa( nzvs_oa + 1 ) = nzvs3d_oa + 1
       begvs_oa  ( nzv_oa  + 1 ) = nzvs_oa + 1
      endif

      return
      end subroutine var_space_oa

!------------------------------------------------------------------------------
! PROCEDURE
!
!> @note
!
! DESCRIPTION: 
!
!> @brief Specifies the number of dimensions of the variable to analyse, 
!! and at which grid point fields are defined.
!
!> @details This routine must be modified when adding a new variable:
!! - tgv3d_oa must be set to 3 or 2 for three and two-dimensional variable respectively.
!! - tgv_oa must be set to:
!!   - "1" for t-grid point,
!!   - "2" for u-grid point,
!!   - "3" for v-grid point,
!!   - "4" for w-grid point.
! 
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version
!! - Benedicte Lemieux-Dudon
!!  - cleaning and selective use of module variables.
!> @date 2015 January
!> @todo
!
!------------------------------------------------------------------------------
      subroutine var_grid_oa
    

      use module_oa_space , only : tgv3d_oa, tgv_oa 

      implicit none

!.....vitesse vel_u:
      tgv_oa(1)   = 2 
      tgv3d_oa(1) = 3

!.....vitesse vel_v:
      tgv_oa(2)   = 3 
      tgv3d_oa(2) = 3

!.....vitesse vel_w:
      tgv_oa(3)   = 4 
      tgv3d_oa(3) = 3

!.....variable rhp_t:
      tgv_oa(20)   = 1 
      tgv3d_oa(20) = 3

!.....test variable vardp_test_oa:
      tgv_oa(99)   = 1 
      tgv3d_oa(99) = 3

!.....horizontal kinetic energy at t-grid point:
      tgv_oa(100)   = 1 
      tgv3d_oa(100) = 3

      return
      end subroutine var_grid_oa
!------------------------------------------------------------------------------
! PROCEDURE
!
!> @note
!
! DESCRIPTION: 
!
!> @brief Sets the frequency parameters as requested in namelist 
!! which controls the "state vector" frequency structure.
!
!> @details Variable :
!! - nzvp_oa the total number of periods to analyse in the simulation (all variables included). 
!! - begvp_oa stores the number of periods per variables. It is refered as to the "frequency" structure
!!   of the state vector.
!!
!! Namelist parameters are:
!! PER_W(1), per_oa(1) : minimum period
!! PER_W(2), per_oa(2) : maximum period
!! PER_W(3), per_oa(3) : step determining the number of periods to be examined, dp in sec. (-99=optimum).
!! NZPT_PER_W, nzpt_per_oa : number of points in the convolution window.
!! 
!! Outputs:
!! - resv_oa : integer setting the temporal resolution (i.e., number of model time step integration) at which
!!   the convolution product will be calculated (e.g., one point every 4 model time steps)
!!   TODO check this definition in the case when dori_oa equals 2.
!! - nzvp_oa and begvp_oa vector : parameters controling the frequential structure of the "state vector".
!! - perv_oa(2,:) : reconstruction factor ( replaces inverse transformation ).
!! - perv_oa(2,:) : reconstruction factor ( replaces inverse transformation ).
!
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!! - remove goto syntax. 
!! - apply a selective use of module variables. 
!! - apply the intent attribute to arguments.
!------------------------------------------------------------------------------
      subroutine var_per_oa( flag_p, dti )

      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock

      implicit none

      double precision, intent(in) :: dti                             !< Integration time step

      integer                                                         &
         iv_p                                                         &  !< Variable index
        ,iv1_p

      real                                                            &
         ip_p

      real                                                            &   
         p_p                                                          &   !< Time scale
        ,t0c,dtc,w0c,dwc                                                  !< Wavelet resolution : Heisenberg parameters

      logical :: flag_p                                                   !< Flag to distinguish calls to var_space_oa before and after OA array allocation
         

      t0c = 0.
      w0c = 6.0
      dtc = 0.707106781
      dwc = dtc
      
      nzvp_oa = 0

!.....precalcul des periodes:
      do iv_p = 1, nzv_oa

! cas resolution reelle
        if (per_oa(3,iv_p).eq.-99.*unite_oa) then
         
         if (flag_p) begvp_oa( iv_p ) = nzvp_oa + 1
         if (per_oa(1,iv_p).eq.0) per_oa(1,iv_p)=1. ! on commence pas a 0      
         p_p=per_oa(1,iv_p) 


         do while (p_p.le.per_oa(2,iv_p)) 
          nzvp_oa = nzvp_oa + 1

          if (flag_p) perv_oa (1,nzvp_oa) = p_p
          p_p=p_p*(1.+dwc/w0c)/(1.-dwc/w0c)
         enddo
         nzvp_oa = nzvp_oa + 1
         if (flag_p) perv_oa (1,nzvp_oa) =p_p

! cas resolution reguliere 
       else
         if (flag_p) begvp_oa( iv_p ) = nzvp_oa + 1
!---------------------------------------------------------
!.......periode entiere en secondes:
!       do ip_p = int(per_oa(1,iv_p)),int(per_oa(2,iv_p)),int(per_oa(3,iv_p))
!       changer aussi la declaration de ip_p
!---------------------------------------------------------
        ip_p = per_oa(1,iv_p)

 100    continue
         nzvp_oa          = nzvp_oa + 1
         if (flag_p) perv_oa(1,nzvp_oa) = ip_p
         ip_p = ip_p + per_oa(3,iv_p)
        if (ip_p.le.per_oa(2,iv_p)) goto 100 

!---------------------------------------------------------
!       enddo
!---------------------------------------------------------
       endif

      enddo
      if (flag_p) begvp_oa(nzv_oa+1)     = nzvp_oa + 1

      if (.not.(flag_p)) return
      
!.....precalcul des resolutions:

      do iv_p = 1 , nzv_oa
        do iv1_p = begvp_oa(iv_p) , begvp_oa(iv_p+1)-1

!.........atome= dirac:
          if (tpsi_oa(iv_p).eq.0.and.flag_p) then
            resv_oa (iv1_p)    = 1 
          endif

!.........atome= ondelette de morlet ou windowed fourier ou fourier:
          if (tpsi_oa(iv_p).ne.0.and.flag_p) then
           if (dori_oa(iv_p).eq.1) then
            resv_oa (iv1_p)   = max ( (  int(                         &
                             perv_oa (1,iv1_p)  /   dti            &
                              ) + 1 )                                 &
                           / nzpt_per_oa (iv_p)                       &
                                     , 1 )      
           else
            resv_oa (iv1_p)   = max ( (  int(                         &
                             perv_oa (1,begvp_oa(iv_p))               & 
                           /   dti                                 &
                              ) + 1 )                                 &
                                      , 1)                            &
                           / nzpt_per_oa (iv_p)
           endif
          endif

        enddo
       enddo
      return

      end subroutine var_per_oa

!------------------------------------------------------------------------------
! PROCEDURE
!
!> @note
!
! DESCRIPTION: 
!
!> @brief Records the temporal parameters for all the requested analysis.
!
!> @details Variables :
!! - nzvt_oa is the total number of time window requested for analysis (all variables included). 
!! - begvt_oa stores the number of time windows per variables. It is refered as to the "time" structure
!!   of the state vector.
!!
!! Corresponding namelist parameters :
!! - DELTA_T_W, delta_t_oa : set to 1 for Dirac analysis, set to 2 or more for wavelets or
!!   windowed Fourier. In the latter case, the convolution window is set 2 times the period 
!!   of interest (c.f., perv_oa).
!! - SWT_T_W, swt_t_oa : defines the time domain where analysis are requested
!!   * 1 : from T1 to T2 NOT AVAILABLE YET 
!!   * 2 : from T1 to the end, NOT AVAILABLE YET 
!!   * 3 : from date to T1, NOT AVAILABLE YET 
!!   * 4 : the entire simulation.
!!   Check : the only namelist parameters are DATE_DEB, DATE_END (pattern is 2011, 01, 1, 00, 01, 18).
!!   kount_user_oa(1:2,nzv_oa) is calculated from DATE_DEB, DATE_END in terms of model time steps.
!! - DT_W : time period over which the analysis is repetited (s). Enables to deduce kount_user_oa(3,nzv_oa) 
!!   in terms of model time steps. 
!!   
!! Outputs: 
!! - kountv_oa(1,nzvt_oa), kountv_oa(2,nzvt_oa) are delimiting each convolution window (for each variables).
!! - per_t2p_oa (nzvt_oa) : correspondance between "temporal" and "frequency" structure.
!! - dkount_oa  (1:nzv_oa): duree d'echantillonnage,
!! - nztime_v_oa(1:nzv_oa): nombre reel de points d'echantillonnage par periode.    
!! - nzvt_oa, begvt_oa    : structure temporelle du vecteur d'etat.
!! - nzw_oa , begw_oa     : structure complete (spatiale et temporelle) du vecteur d'etat.
!
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!! - apply a selective use of module variables. 
!! - apply the intent attribute to arguments.
!------------------------------------------------------------------------------
      subroutine var_time_oa( flag_t, kount0, nt_max, dti )
      
      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock

      implicit none

      double precision, intent(in) :: dti                                         !< Integration time step
      integer, intent(in)          :: kount0,                                     &  !< First simulation iteration
                                      nt_max                                             !< Last integration iteration

      integer                                                         &
            k                                                         &
           ,dkount_tot_t                                              &
           ,iv_t                                                      &
           ,ls_t                                                      &
           ,ls1_t                                                     &
           ,val                                                       &     !test nmw_oa
           ,comptt(nzv_oa)                                            &
           ,ip_t                                                      &
           ,ip1_t                                                     &
           ,ip2_t                                                     &
           ,ls_s,ls1_s                                                &
           ,i_s,j_s,k_s

      character*40                                                    &
           nameid

      logical                                                         &
           flag_t


      nzvt_oa  = 0
      nzw_oa   = 0

!---- >boucle sur toutes les variables:
      do iv_t = 1, nzv_oa

!*******************************************************************
!     preparation de la structure temporelle
!*******************************************************************
         
         if (flag_t) begvt_oa(iv_t)   = nzvt_oa + 1

         if (dori_oa(iv_t).eq.1) then
            ip1_t = begvp_oa( iv_t     ) 
            ip2_t = begvp_oa( iv_t + 1 ) - 1
         else
            ip1_t = begvp_oa( iv_t + 1 ) - 1 
            ip2_t = begvp_oa( iv_t + 1 ) - 1
         endif

!----->boucle sur toutes les periodes demandees pour cette variable:
         do ip_t = ip1_t , ip2_t

!----------------------------------------------------------------
!     precalcul de la largeur en kount de l'atome:
!----------------------------------------------------------------

!...........atome = dirac:
            if (tpsi_oa(iv_t).eq.0) then
               if (perv_oa(1,ip_t).eq.0.) then
                  dkount_tot_t    = 0
               else
                  dkount_tot_t    = int(perv_oa(1,ip_t)/2./dti-0.5)+1
               endif
            endif   

!...........atome = ondelette de morlet ou windowed fourier:     
            if (tpsi_oa(iv_t).eq.1.or.tpsi_oa(iv_t).eq.2) then
               if (dori_oa(iv_t).eq.1) then
                  dkount_tot_t    = (int(                             &
                       perv_oa (1,ip_t)  /   dti                   &
                       ) + 1)                                         &
                       * delta_t_oa(iv_t)
                      
               else
                  dkount_tot_t    = (int(                             &
                       perv_oa (1,ip2_t)  /   dti                  &
                       ) + 1)                                         &
                       * delta_t_oa(iv_t)
               endif
            endif

!...........atome = fourier :
            if (tpsi_oa(iv_t).eq.3) then
               dkount_tot_t = 99999.
            endif   

!...........initialisation du compteur de la variable:
            comptt(iv_t)=0

!----------------------------------------------------------------
!     configuration temporelle numero 1: periode
!     configuration temporelle numero 4: simu
!     
!     atomes differents de fourier
!----------------------------------------------------------------
            if ((swt_t_oa(iv_t).eq.1.or.swt_t_oa(iv_t).eq.4).and.tpsi_oa(iv_t).ne.3 ) then
               do k = kount_user_oa(1,iv_t),kount_user_oa(2,iv_t),kount_user_oa(3,iv_t)
                  nzvt_oa             = nzvt_oa + 1

                  if (flag_t) then
                   kountv_oa(1,nzvt_oa) = k - dkount_tot_t
                   kountv_oa(2,nzvt_oa) = k + dkount_tot_t
                   per_t2p_oa (nzvt_oa)  = ip_t

!----------------------------------------------------------------
                   if (    kountv_oa(1,nzvt_oa).lt.kount0             &
                       .or.kountv_oa(1,nzvt_oa).gt.nt_max-1               &
                       .or.kountv_oa(2,nzvt_oa).lt.kount0             &
                       .or.kountv_oa(2,nzvt_oa).gt.nt_max-1 )             &
                       then
                     kountv_oa(1,nzvt_oa) = -9999
                     kountv_oa(2,nzvt_oa) = -9999
                   else
!----------------------------------------------------------------
                     if (updv_oa(iv_t).eq.1) then
                      if (kountv_oa(1,nzvt_oa).lt.kount0) then
                       kountv_oa(1,nzvt_oa) = kount0
                       kountv_oa(2,nzvt_oa) = kount0 + 2*dkount_tot_t
                      endif
                      if (kountv_oa(2,nzvt_oa).gt.nt_max-1) then
                       kountv_oa(1,nzvt_oa) = nt_max     - 2*dkount_tot_t
                       kountv_oa(2,nzvt_oa) = nt_max
                      endif
                     endif

                     comptt(iv_t)=comptt(iv_t)+1
                   endif
                  endif
               enddo
            endif


!----------------------------------------------------------------
!     configuration temporelle numero 4   : simu
!     
!     atome: fourier
!----------------------------------------------------------------
            if ( tpsi_oa(iv_t).eq.3 ) then
               nzvt_oa             = nzvt_oa + 1

               if (flag_t) then
                kountv_oa(1,nzvt_oa) = nt_max-1                           &
                    - int ( int( real(nt_max-1-kount0+1) * dti         &
                           / perv_oa(1,ip_t) )                        &
                         * perv_oa(1,ip_t)/dti ) + 1             
                kountv_oa(2,nzvt_oa) = nt_max-1  
                per_t2p_oa (nzvt_oa) = ip_t
                comptt(iv_t)       = 1
               endif
            endif


!----------------------------------------------------------------
!     configuration temporelle numero 2: a la fin de la simulation:
!----------------------------------------------------------------
            if (swt_t_oa(iv_t).eq.2) then
               nzvt_oa             = nzvt_oa + 1
               if (flag_t) then
                kountv_oa(1,nzvt_oa) = nt_max - 1 - 2*dkount_tot_t
                kountv_oa(2,nzvt_oa) = nt_max - 1      
                per_t2p_oa (nzvt_oa)  = ip_t
                if (    kountv_oa(1,nzvt_oa).lt.kount0                 &
                    .or.kountv_oa(1,nzvt_oa).gt.nt_max-1                   &
                    .or.kountv_oa(2,nzvt_oa).lt.kount0                 &
                    .or.kountv_oa(2,nzvt_oa).gt.nt_max-1 )                 &
                    then
                  kountv_oa(1,nzvt_oa) = -9999
                  kountv_oa(2,nzvt_oa) = -9999
                else
                   comptt(iv_t)=1
                endif
               endif
            endif

!----------------------------------------------------------------
!     configuration temporelle numero 3: date unique
!----------------------------------------------------------------
            if (swt_t_oa(iv_t).eq.3) then
               nzvt_oa             = nzvt_oa + 1

               if (flag_t) then
               kountv_oa(1,nzvt_oa) = kount_user_oa(1,iv_t) - dkount_tot_t
               kountv_oa(2,nzvt_oa) = kount_user_oa(1,iv_t) + dkount_tot_t
               per_t2p_oa (nzvt_oa) = ip_t
               if (    kountv_oa(1,nzvt_oa).lt.kount0                  &
                    .or.kountv_oa(1,nzvt_oa).gt.nt_max - 1                 &
                    .or.kountv_oa(2,nzvt_oa).lt.kount0                 &
                    .or.kountv_oa(1,nzvt_oa).gt.nt_max - 1 )               &
                    then
                  kountv_oa(1,nzvt_oa) = -9999
                  kountv_oa(2,nzvt_oa) = -9999
               else
                  comptt(iv_t)=1   
               endif
               endif
            endif

!.....enddo ip_t

         enddo

         
!---- >dernier point:
         if (flag_t) begvt_oa( nzv_oa + 1 )   = nzvt_oa + 1

!.....enddo: boucle sur les variables...

      enddo


      return
      end subroutine var_time_oa


!------------------------------------------------------------------------------
! PROCEDURE
!------------------------------------------------------------------------------
!
!> @note 
!
!
! DESCRIPTION: 
!
!> @brief Initialise les coefficients de reconstruction pour chaque periode. 
!> @details 
!
!
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version
!! - Benedicte Lemieux-Dudon
!!  - replace "if-endif" by "if-elseif-endif" for tpsi_oa.
!!  - cleaning and pi calculation eliminated.
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!! - apply a selective use of module variables. 
!! - apply the intent attribute to arguments.
!------------------------------------------------------------------------------

      subroutine var_rec_oa( dti )

      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock

      implicit none

      double precision, intent(in) :: dti    !< Integration time step

      integer kpt_m
      real    signal_r
 !STDALONE      real    pi_p
    
      complex ::                                                      &
              signal_rec                                              &
             ,temp_r                                                  &
             ,psi_m

      integer                                                         &
            ip_p                                                      &
           ,iv_p                                                      &
           ,lt_p                                                      &
           ,kount_p


!      pi_p = acos(-1.)

      do iv_p = 1 , nzv_oa
!---------------------------------------------------------------------------
!.....initialisation (pour tous les atomes):
!---------------------------------------------------------------------------
         do ip_p = begvp_oa(iv_p) , begvp_oa(iv_p+1)-1
            perv_oa(2,ip_p) = 1.
         enddo

!.....pour certains atomes, reconstruction possible si demandee:
         if (fl_rec_oa(iv_p).eq.1) then

!---------------------------------------------------------------------------
!.....dirac:
!---------------------------------------------------------------------------
!TODO replace some if-endif by if-else if-endif

            type_of_oa_atom : if (tpsi_oa(iv_p).eq.0) then

               do ip_p = begvp_oa(iv_p) , begvp_oa(iv_p+1)-1
                  if (perv_oa(1,ip_p).eq.0) then
                     perv_oa(2,ip_p) = 1.
                  else
                     perv_oa(2,ip_p)=real(2*(int(perv_oa(1,ip_p)      & 
                          /2./dti-0.5) )/1)                        &
                          +3.
                  endif
               enddo
!TODO replace some if-endif by if-else if-endif
!            endif


!---------------------------------------------------------------------------
!.....fourier:
!---------------------------------------------------------------------------
!TODO replace some if-endif by if-else if-endif
!            if (tpsi_oa(iv_p).eq.2.or.tpsi_oa(iv_p).eq.3) then 

            else if (tpsi_oa(iv_p).eq.2.or.tpsi_oa(iv_p).eq.3) then type_of_oa_atom 
               lt_p  = begvt_oa(iv_p)
               if (dori_oa(iv_p).eq.2) stop 'a programmer...'
               do ip_p = begvp_oa(iv_p) , begvp_oa(iv_p+1)-1
                  do while ((per_t2p_oa(lt_p).ne.ip_p.or.kountv_oa(1,lt_p).eq.-9999.or.kountv_oa(2,lt_p).eq.-9999).and.lt_p.lt.begvt_oa(iv_p+1) )
                     lt_p=lt_p+1
                  enddo
                  perv_oa(2,ip_p)=real((kountv_oa(2,lt_p)-kountv_oa(1,lt_p))/resv_oa(ip_p)+1)
               enddo
              
!TODO replace some if-endif by if-else if-endif
!            endif


!---------------------------------------------------------------------------
!.....ondelette de morlet:
!---------------------------------------------------------------------------
!TODO replace some if-endif by if-else if-endif
!            if (tpsi_oa(iv_p).eq.1) then

             else if (tpsi_oa(iv_p).eq.1) then type_of_oa_atom 

!.....periodes discretisees:
               if (dori_oa(iv_p).eq.1) then
                  
                  do ip_p = begvp_oa(iv_p) , begvp_oa(iv_p+1)-1

                     temp_r = 0.

!     boucle temporelle
                     
                     lt_p = begvt_oa(iv_p)
                     do while ((per_t2p_oa(lt_p).ne.ip_p.or.kountv_oa(1,lt_p).eq.-9999.or.kountv_oa(2,lt_p).eq.-9999).and.lt_p.lt.begvt_oa(iv_p+1))
                        lt_p = lt_p + 1
                     enddo

                     if (lt_p.lt.begvt_oa(iv_p+1)) then
                        do  kount_p =  kountv_oa(1,lt_p),kountv_oa(2,lt_p),resv_oa(per_t2p_oa(lt_p))
                           kpt_m = kount_p - ( int(( kountv_oa(1,lt_p)+kountv_oa(2,lt_p))/2) )

                           
!     precalcul de psi:
                           
                           psi_m = psi_oa(                       &
                                tpsi_oa(iv_p)                         &
                                ,psi_p2s_oa(                     &
                                tpsi_oa(iv_p)                         &
                                ,perv_oa(1,ip_p),fb_oa,fc_oa )        &
                                ,real(kpt_m)* dti                  &   !- tempschoisi*dti
                                ,dti*resv_oa(per_t2p_oa(lt_p))     &
                                ,fb_oa,fc_oa  )

!     cosinus signal test
                           signal_r=cos(2*pi_oa/perv_oa(1,ip_p) *real(kpt_m)* dti)  
!                                                              *real(kount_p)* dti)  

!     reconstruction factor
                           temp_r = temp_r + conjg(psi_m) * signal_r

                        enddo

!     fin boucle temporelle

!     signal_rec=cos(2*pi_oa/perv_oa(1,ip_p)*real(kpt_m)* dti)
!    &  +(0.,1.)*sin(2*pi_oa/perv_oa(1,ip_p)*real(kpt_m)* dti) 
!     perv_oa(2,ip_p)=abs(perv_oa(2,ip_p)/signal_rec)

                        perv_oa(2,ip_p) = real(temp_r)
                     endif
                  enddo

!.....periodes integrees:
               elseif (dori_oa(iv_p).eq.2) then

                  do ip_p = begvp_oa(iv_p) , begvp_oa(iv_p+1)-1

                     temp_r = 0.

!     boucle temporelle
                     
                     lt_p = begvt_oa(iv_p)
                     do  kount_p = kountv_oa(1,lt_p) , kountv_oa(2,lt_p),resv_oa(per_t2p_oa(lt_p))
                        
                        kpt_m = kount_p - ( int((kountv_oa(1,lt_p) +kountv_oa(2,lt_p))/2) )

!     precalcul de psi:
                        
                        psi_m = psi_oa(                          &
                             tpsi_oa(iv_p)                            &
                             ,psi_p2s_oa(                        &
                             tpsi_oa(iv_p)                            &
                             ,perv_oa(1,ip_p),fb_oa,fc_oa )           &
                             ,real(kpt_m)* dti                     &  !- tempschoisi*dti
                             ,dti*resv_oa(per_t2p_oa(lt_p))        &
                             ,fb_oa,fc_oa  )

!     cosinus signal test
                        signal_r=cos(2*pi_oa/perv_oa(1,ip_p)*real(kpt_m) * dti)  
                        
!     reconstruction factor
                        temp_r = temp_r + conjg(psi_m) * signal_r

                     enddo
!     fin boucle temporelle

!     signal_rec=cos(2*pi_oa/perv_oa(1,ip_p)*real(! tempschoisi)* dti)
!     &  +(0.,1.)*sin(2*pi_oa/perv_oa(1,ip_p)*real(! tempschoisi)* dti) 
!     perv_oa(2,ip_p)=real(perv_oa(2,ip_p)/signal_rec)

                     perv_oa(2,ip_p) = real(temp_r)

                  enddo

!.....endif associe au test sur dori_oa
               endif

            endif type_of_oa_atom

!........endif associe au test: fl_rec_w=1
         endif

!......enddo associe a la variable iv_p:
      enddo
     
      return
      end subroutine var_rec_oa

!------------------------------------------------------------------------------
! PROCEDURE
!
!> @note 
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
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version
!! - Benedicte Lemieux-Dudon
!!  - stand alone version with arguments. 
!!  - ifl_l flag eliminated, replaced by counting analysis of type 20 in nzlevel_oa, and testing if nzlevel_oa>0.
!!  - enables to diminish the size of the structured type array wlev, now sized to nzlevel_oa intead of 
!!    nzv_oa (the total number of OA analysis requested in trhe simulation)
!!  - reading isopycne data in a file is not maintained for the stand alone version (todo?) but only the possibility
!!    to picked values along the rhp_t field profile (according to namelist parameters).
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!! - reading isopycne data in a file
!! - apply a selective use of module variables. 
!! - apply the intent attribute to arguments.
!------------------------------------------------------------------------------
      subroutine  lev_init_oa( rhp_t, rhp_t_lbound, rhp_t_ubound )

      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock
      use module_oa_level

      implicit none

      !> Lower and upper index bounds of the 3-Dimensional density array
      integer, dimension(3), intent(in) ::  &
        rhp_t_lbound, rhp_t_ubound

      ! 3-Dimensional density array passed in arguments to the OA module
      double precision, dimension(rhp_t_lbound(1):rhp_t_ubound(1),rhp_t_lbound(2):rhp_t_ubound(2),rhp_t_lbound(3):rhp_t_ubound(3)) :: &
        rhp_t  !< Density array at t-grid point


      integer                                                         &             
       i_l                                                            &
      ,j_l                                                            &
      ,k_l                                                            &
      ,ls_l                                                           &
      ,ls1_l                                                          &
      ,iv_l                                                           &
      ,izlevel_oa

   !STDALONE   double precision                                                &
   !STDALONE    val_l(1:kmax+1)
   !STDALONE
   !STDALONE   integer ifl_l
   !STDALONE
   !STDALONE   character*30 file_l

   !STDALONE !.....mise a jour du flag ifl_l: variable(s) de type 20 ?
   !STDALONE        ifl_l = 0
   !STDALONE        do iv_l = 1 , nzv_oa
   !STDALONE           if (tv_oa(iv_l).eq.20) then
   !STDALONE              ifl_l = 1
   !STDALONE           endif
   !STDALONE        enddo


!.....Counter of the number of isopycne analysis requested
!     Ttest nzlevel_oa>0 replaces ifl_l flag
!     This loop can be applied to reduce the size of the wlev structure allocation to nzlevel_oa
      nzlevel_oa = 0
      do iv_l = 1 , nzv_oa
         if (tv_oa(iv_l).eq.20) then
            nzlevel_oa = nzlevel_oa + 1
            !ifl_l = 1
            !print*,'yes 20'
         endif
      enddo

!.....si variables de type 20:

   !STDALONE if (ifl_l.eq.1) then

      variable_type_20_requested : if ( nzlevel_oa>0 ) then

   !STDALONE The array of structured type wlev is sized to nzlevel_oa intead of
   !STDALONE of nzv_oa (the total number of OA analysis requested in trhe simulation)

          call allocate_lev_part1_oa

          izlevel_oa = 0

    !STDALONE To read the target density in a file, adpat the following lines to your directories
    !STDALONE !.....lecture du fichier contenant les caracteristiques des isopycnes
    !STDALONE !     a suivre:
    !STDALONE       open(unit=10,file='DATA/isopycnes.in')
    !STDALONE       file_l ='DATA/'//dom_c//'isopycnes.dat'
    !STDALONE       open(unit=11,file=file_l)
    !STDALONE         iv_l = 1
    !STDALONE         ls_l  =begvs_oa(iv_l)
    !STDALONE         do ls1_l=begvs3d_oa(ls_l),begvs3d_oa(ls_l+1)-1
    !STDALONE           read (10,*) val_l(ls1_l-begvs3d_oa(ls_l)+1)
    !STDALONE           write(11,*) val_l(ls1_l-begvs3d_oa(ls_l)+1)
    !STDALONE         enddo
    !STDALONE       close(10)
    !STDALONE       close(11)

!.....construction de la structure de variables necessaire
!     au suivi des isopycnes:

       variable_loop : do iv_l = 1 , nzv_oa

         if (tv_oa(iv_l).eq.20) then

   !STDALONE allocation is performed earlier with the array of structured type wlev sized to nzlevel_oa
   !STDALONE instead of being sized to nzv_oa (the total number of OA analysis requested in trhe simulation)        
   !STDALONE if (izlevel_oa.eq.0) call allocate_lev_part1_oa
            !print*,'yes 20 : allocate_lev_part2 to come'

            izlevel_oa            = izlevel_oa + 1
            lev2v_oa (izlevel_oa) = iv_l
            v2lev_oa (iv_l)       = izlevel_oa
            call allocate_lev_part2_oa(                               &
              izlevel_oa                                              &
             ,iv_l                                                    &
             ,begvs3d_oa(begvs_oa(iv_l+1))-begvs3d_oa(begvs_oa(iv_l)) &
                                           )
            do ls_l  =begvs_oa(iv_l),begvs_oa(iv_l+1)-1
             i_l = l2i_oa(ls_l)
             j_l = l2j_oa(ls_l)
             do ls1_l=begvs3d_oa(ls_l),begvs3d_oa(ls_l+1)-1
              k_l = kmin3d_oa(ls_l) + (ls1_l - begvs3d_oa(ls_l))* dk_oa(iv_l)
              wlev_oa(izlevel_oa)%rhp(ls1_l-begvs3d_oa(begvs_oa(iv_l))+1) = rhp_t(i_l,j_l,k_l)
              !print*, i_l, j_l, k_l, rhp_t(i_l,j_l,k_l)
   !STDALONE                                                              = val_l(ls1_l - begvs3d_oa(ls_l)+1)
             enddo
            enddo
         endif

       enddo variable_loop
       !isopycne_analysis = .true.

      !else variable_type_20_requested
!......calls to lev_upd_oa will be disabled
       !isopycne_analysis = .false.

      endif variable_type_20_requested

      return
      end subroutine  lev_init_oa


!------------------------------------------------------------------------------
! PROCEDURE
!
!> @note 
!
! DESCRIPTION: 
!
!> @brief Allocate and initializes output variables and related parameters.
!
!> @details preparation des variables communes.
!
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version
!! - Benedicte Lemieux-Dudon
!!  - stand alone version : outputs limited to grid point domain [imin,imax]x[jmin,jmax]x[kmin,kmax]:
!!  - cleaning (goto,..) 
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!! - apply a selective use of module variables. 
!! - handling of the tvar_oa dimension according to parameters? tvar_oa(1:maxvar_oa,1:maxtyp_oa,1:10)
!------------------------------------------------------------------------------

      subroutine upd_init_oa(     &
       imin, imax                 &
      ,jmin, jmax                 &
      ,kmin, kmax                 )


      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock
!      use module_oa_upd

      implicit none

      !> Grid index range for the analysis of fields passed in argument to the OA module 
      !! Fields are analysed inside grid point domain [imin,imax]x[jmin,jmax]x[kmin,kmax]:
      integer, intent(in) :: &
       imin, imax            & 
      ,jmin, jmax            &
      ,kmin, kmax

      integer                                                         &
       ic_u                                                           &
      ,iv_u                                                           &
      ,nz_u                                                           &
      ,i                                                              &
      ,j                                                              &
      ,l

      integer                                                         &
       nzc_u

      nzupd3d_oa = 0
      nzupd2d_oa = 0

      !tvar_oa(1:maxvar_oa,1:maxtyp_oa,1:10)
      !do j=1,10
      ! do i=1,10
      !   do l=1,200 ! changed to 200 rather than nzv_oa (with the latter there's allocation probs. (staying below 30..)
      !      tvar_oa(l,i,j) = 0.
      !   enddo
      ! enddo
      !enddo

      tvar_oa(:,:,:) = 0.

      do ic_u = 1 , nzc_oa

      nzc_u = 1
!      nzc_u = 0
!100  continue
     do while (tvar_oa(tc_oa(ic_u),nzc_u,1).ne.0)
       nzc_u = nzc_u + 1
     enddo
!     if (tvar_oa(tc_oa(ic_u),nzc_u,1).ne.0) goto 100
!STDALONE tvc_oa : if several configuration of the same type are requested tvc_oa ordinates them.
      tvc_oa (ic_u) = nzc_u
      do iv_u = begc_oa(ic_u),begc_oa(ic_u+1)-1
      if (updv_oa(iv_u).eq.2) then
        if (tgv3d_oa(tv_oa(iv_u)).eq.3) then
          nzupd3d_oa    = nzupd3d_oa + 1
          tupd_oa(iv_u) = nzupd3d_oa 
          tvar_oa(tc_oa(ic_u),nzc_u,iv_u-begc_oa(ic_u)+1) = nzupd3d_oa
        else
          nzupd2d_oa    = nzupd2d_oa + 1
          tupd_oa(iv_u) = nzupd2d_oa 
          tvar_oa(tc_oa(ic_u),nzc_u,iv_u-begc_oa(ic_u)+1) = nzupd2d_oa
        endif
      endif
      enddo
      enddo

    !STDALONE !.....ajout d'une variable pour le calcul de l'energie:
    !STDALONE       if (tc_oa(ic_u).eq.112.and.updv_oa(begc_oa(ic_u)).eq.2) then
    !STDALONE           nzupd3d_oa    = nzupd3d_oa + 1
    !STDALONE           tvar_oa(tc_oa(ic_u),nzc_u,begc_oa(ic_u+1)-begc_oa(ic_u)+1) = nzupd3d_oa
    !STDALONE !         nzupd2d_oa    = nzupd2d_oa + 1
    !STDALONE !         tvar_oa(tc_oa(ic_u),nzc_u,begc_oa(ic_u+1)-begc_oa(ic_u)+2) = nzupd2d_oa
    !STDALONE       endif
    !STDALONE       if (tc_oa(ic_u).eq.110.and.updv_oa(begc_oa(ic_u)).eq.2) then
    !STDALONE           nzupd3d_oa    = nzupd3d_oa + 1
    !STDALONE           tvar_oa(tc_oa(ic_u),nzc_u,begc_oa(ic_u+1)-begc_oa(ic_u)+1) = nzupd3d_oa
    !STDALONE       endif
    !STDALONE       enddo  

!.....allocations dynamiques:
!STDALONE change allocation limits
      !STDALONE allocate (                                                      &
      !STDALONE  var2d_oa(0:imax+1,0:jmax+1     ,nzupd2d_oa)                    &
      !STDALONE ,var3d_oa(0:imax+1,0:jmax+1,0:kmax+1,nzupd3d_oa)                &
      !STDALONE          )

      allocate (                                                      &
       var2d_oa( imin:imax, jmin:jmax, nzupd2d_oa )                   &
      ,var3d_oa( imin:imax, jmin:jmax, kmin:kmax, nzupd3d_oa )        &
               )

      var3d_oa(:,:,:,:) = ( 0.D0, 0.D0 )
      var2d_oa(:,:,:)   = ( 0.D0, 0.D0 )

!.....History file:

      call history_oa(11,-1,-1,-1,-1)

      return
      end subroutine upd_init_oa

!------------------------------------------------------------------------------
! PROCEDURE
!------------------------------------------------------------------------------
!
!> @note
!
! DESCRIPTION: 
!
!> @brief For each variariable requested, construct time step after time step
!! the time convolution with the requested OA "atom" 
!! (i.e., wavelets, Fourier, Dirac), and stores the result in the wf_oa structure.
!
!> @details Suboutine main_oa performs time convolution with the "atoms" proposed
!! in the OA analysis module (i.e., wavelets, Fourier, Dirac). 
!! The routine should be call at each integration time step. 
!!
!! In the case of Fourier or wavelets, two types of analysis are available:
!! - dori_oa set to 1 : no scale integration, analysis at a discrete given scale 
!! - dori_oa set to 2 : scale integration
!!
!! subroutine appelee par le programme principal
!! pour chaque variable ("configuration") main_oa teste
!! si l'on se trouve dans une periode de "calcul" et si tel
!! est le cas il gere le calcul du coef...
!! outputs: 
!! wf_oa: variable contenant tous les coefs...
!
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version
!! - Benedicte Lemieux-Dudon
!!  - stand alone version
!!  - ivar_m with optional attribute
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!! - apply a selective use of module variables. 
!
!------------------------------------------------------------------------------

      subroutine main_oa(  ichoix                     &
                          ,ivar_m                     &
                          ,iic_oa                &
                          ,dti                     &
                          ,nt_max                         &
                          ,imin, imax                 &
                          ,jmin, jmax                 &
                          ,kmin, kmax                 &
                          ,rhp_t                      &
                          ,rhp_t_lbound               &
                          ,rhp_t_ubound               &
                          ,depth_t                    & !(-1:imax+2,-1:jmax+2,0:kmax+1) 
                          ,depth_t_lbound             &
                          ,depth_t_ubound          ) 


      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock
!      use module_oa_upd

      implicit none

      integer, intent(in) :: ichoix

      !> Time integration step
      double precision, intent(in) :: dti

      !> First and last simulation iteration index
      integer, intent(in) :: nt_max                                        

      !> Current model integration iteration
      integer, intent(in) :: iic_oa 

      !> To handle specific calls to main_oa with variables updated at different position in the calling code
      integer, intent(in), optional :: ivar_m
     
      !> Grid index range for the analysis of fields passed in argument to the OA module 
      !! Fields can only be annalysed at grid points inside [imin,imax]x[jmin,jmax]x[kmin,kmax]:
      integer, intent(in) :: &
       imin, imax            & 
      ,jmin, jmax            &
      ,kmin, kmax

      !> Lower and upper index bounds of the 3-Dimensional density array
      integer, dimension(3), intent(in) :: rhp_t_lbound, rhp_t_ubound

      !> 3-Dimensional density array at t-grid point
      double precision, dimension(rhp_t_lbound(1):rhp_t_ubound(1),rhp_t_lbound(2):rhp_t_ubound(2),rhp_t_lbound(3):rhp_t_ubound(3)), intent(in) :: &
        rhp_t

      !> Lower and upper index bounds of the 3-Dimensional depth array
      integer, dimension(3), intent(in) :: depth_t_lbound, depth_t_ubound

      !> 3-Dimensional depth array array at t-grid point
      double precision, dimension(depth_t_lbound(1):depth_t_ubound(1),depth_t_lbound(2):depth_t_ubound(2),depth_t_lbound(3):depth_t_ubound(3)), intent(in) :: &
        depth_t

      !complex :: psi_oa
      !real    :: psi_p2s_oa

      real    :: var_oa
      
      !TODO declare flag here if only needed here
      logical :: ifl_test_composite

      integer                                                         &
         iv_m                                                         &
        ,ic_m                                                         &
        ,lt_m                                                         &
        ,ls_m                                                         &
        ,ls1_m

      integer                                                         &
         i_m                                                          &
        ,j_m                                                          &
        ,k_m                                                          &
        ,kpt_m                                                        &
        ,lp_m                                                         &
        ,la_m
     
      complex                                                         &
         psi_m

   
      real :: tmp

!---------------------------------------------------------------------------
!.....periodes discretisees:
!---------------------------------------------------------------------------

!     if (iic_oa.eq.kount0.and.ifl_init_oa.eq.0) then

!STDALONE      if (ifl_init_oa.eq.0) then
!STDALONE         call initial_oa
!STDALONE         ifl_init_oa=1
!STDALONE      endif


      oa_analysis_requested : if (nzv_oa.ne.0) then

!.....flag variable composite 
      ifl_test_composite = .true.      

!.....test variable CHECK
       if (ifl_test_oa==1) then
        call test_oa( ichoix=1      & ! Initialisation 
          ,iic_oa=iic_oa  & 
          ,dti=dti            & 
          ,imin=imin, imax=imax     &
          ,jmin=jmin, jmax=jmax     &
          ,kmin=kmin, kmax=kmax )
       endif

      !STDALONE if (flag_nrj_oa.eq.1) call nrj_upd_oa(1)
      if ( isopycne_analysis ) then
       call lev_upd_oa(       kmin, kmax                                   &
                             ,rhp_t                                        &
                             ,rhp_t_lbound                                 &
                             ,rhp_t_ubound                                 &
                             ,depth_t                                      & !(-1:imax+2,-1:jmax+2,0:kmax+1) 
                             ,depth_t_lbound                               &
                             ,depth_t_ubound  )

      endif

      do ic_m = 1 , nzc_oa
      do iv_m = begc_oa(ic_m),begc_oa(ic_m+1)-1
    !    write(6,*) 'iv_m=======',iv_m,tv_oa(iv_m)
         !STDALONE to call main_oa at specific location for composite varaible confguration
         if (present(ivar_m)) then
            ifl_test_composite = tv_oa(iv_m).eq.ivar_m
         endif

!........ce test est utilise pour les appels cibles lors des calculs energetiques:
         if (ichoix.ge.0.or.ifl_test_composite) then
!---------------------------------------------------------------------------
!.....periodes discretisees:
!---------------------------------------------------------------------------
 !    write(6,*) 'ici1'

     period_of_analysis_case : if (dori_oa(iv_m).eq.1) then

!     write(6,*) 'ici2',begvt_oa(iv_m) , begvt_oa(iv_m+1) - 1
      time_of_analysis_loop : do lt_m  = begvt_oa(iv_m) , begvt_oa(iv_m+1) - 1

!     write(6,*) 'ici3',iic_oa,kountv_oa(1,lt_m),kountv_oa(2,lt_m)
!.....test pour le calcul:
       time_domain_test : if (iic_oa.ge.kountv_oa(1,lt_m).and.iic_oa.le.kountv_oa(2,lt_m)            &
                                .and.mod( int((iic_oa-kountv_oa(1,lt_m)),kind=8)  ,                       &
                                                                            resv_oa(per_t2p_oa(lt_m))).eq.0    &
                                                    .and.(ichoix.eq.cnb_oa(iv_m).or.ichoix.lt.0)               &! calling main_oa for particular variable
                             ) then
  !   write(6,*) 'ici4',kountv_oa(2,lt_m)

!........allocation if necessary (if variable isn't allocated yet, it is done.., at user-specified kount):

          if (tallocated_oa(lt_m).eq.-1) then
        !   write(6,*) 'allocation',lt_m,ic_m,iv_m
            call allocate_win_oa ( lt_m, ic_m, iv_m,                                          &
                                   begvs3d_oa(begvs_oa(iv_m+1)) - begvs3d_oa(begvs_oa(iv_m)), &
                                   iic_oa )
            !lt_m,ic_m,iv_m,begvs3d_oa(begvs_oa(iv_m+1)) - begvs3d_oa(begvs_oa(iv_m)) )
          endif

!........precalcul de l'indice temporel prenant en compte la translation (u):

         kpt_m =  iic_oa - ( int((kountv_oa(1,lt_m)+kountv_oa(2,lt_m))/2) )
   !     write(6,*) 'coucou5',kpt_m

!........precalcul de psi:

          psi_m = psi_oa(                                                   &
                tpsi_oa(iv_m)                                                    &
               ,psi_p2s_oa(                                                 &
                                 tpsi_oa(iv_m)                                   &         
                                ,perv_oa(1,per_t2p_oa(lt_m)),fb_oa,fc_oa )       &                                     
               ,real(kpt_m)* dti                                              &
               ,dti*resv_oa(per_t2p_oa(lt_m))                                 &           
               ,fb_oa,fc_oa  ) 
     !   write(6,*) 'coucou6',psi_m,begvs_oa(iv_m),begvs_oa(iv_m+1)-1


!........boucle spatiale:

           horizontal_space_loop : do ls_m = begvs_oa(iv_m),begvs_oa(iv_m+1)-1
            i_m = l2i_oa(ls_m)
            j_m = l2j_oa(ls_m)
     !   write(6,*) 'coucou7',begvs3d_oa(ls_m),begvs3d_oa(ls_m+1)-1

            vertical_space_loop : do ls1_m = begvs3d_oa(ls_m),begvs3d_oa(ls_m+1)-1
            k_m = kmin3d_oa(ls_m) + (ls1_m - begvs3d_oa(ls_m))* dk_oa(iv_m)
        !   write(6,*) 'coucou1',tv_oa(iv_m),ichoix 
            tmp =  var_oa( tv_oa(iv_m)                                       &
                                   ,ichoix                                       &
                                   ,i_m                                          &
                                   ,j_m                                          &
                                   ,k_m                                          &
                                   ,iv_m                                         &
                                   ,ls1_m-begvs3d_oa(begvs_oa(iv_m))+1       ) 

             wf_oa(tallocated_oa(lt_m))%coef (                                   &
                      ls1_m-begvs3d_oa(begvs_oa(iv_m))+1 )   =                   &
             wf_oa(tallocated_oa(lt_m))%coef (                                   &
                      ls1_m-begvs3d_oa(begvs_oa(iv_m))+1 )                       &
                     + conjg(psi_m)                                              &
                     * tmp                                                       &
                    /max(perv_oa(2,per_t2p_oa(lt_m)),1.e-30)

            enddo vertical_space_loop
           enddo  horizontal_space_loop
       endif time_domain_test
      enddo time_of_analysis_loop
 
!.....periodes integrees:
!---------------------------------------------------------------------------
      elseif (dori_oa(iv_m).eq.2) then period_of_analysis_case
!---------------------------------------------------------------------------

      time_of_analysis_loop2 : do lt_m  = begvt_oa(iv_m) , begvt_oa(iv_m+1) - 1

!.....test pour le calcul:

       time_domain_test2 : if ( iic_oa.ge.kountv_oa(1,lt_m).and.                                     &
                               iic_oa.le.kountv_oa(2,lt_m).and.                                      &
                               mod( int((iic_oa-kountv_oa(1,lt_m)),kind=8) ,                         &
                                                                 resv_oa(per_t2p_oa(lt_m))).eq.0          &
                                     .and.                                                                &
                                     (ichoix.eq.cnb_oa(iv_m).or.ichoix.lt.0)                              &
                             ) then

!........allocation si necessaire:

          if (tallocated_oa(lt_m).eq.-1) call allocate_win_oa( lt_m, ic_m, iv_m,                                &
                                                 begvs3d_oa(begvs_oa(iv_m+1)) - begvs3d_oa(begvs_oa(iv_m)),     &
                                                 iic_oa )

!........precalcul de l'indice temporel prenant en compte la translation (u):

         kpt_m =  iic_oa  - ( int((kountv_oa(1,lt_m)+kountv_oa(2,lt_m))/2) )
     
!........boucle sur toutes les periodes:

         period_of_analysis_loop2 : do lp_m = begvp_oa(iv_m) , begvp_oa(iv_m+1) - 1

!........precalcul de psi:
         
          psi_m = psi_oa(                                                       &
                tpsi_oa(iv_m)                                                   &
               ,psi_p2s_oa(                                                     &
                                 tpsi_oa(iv_m)                                  &
                                ,perv_oa(1,lp_m),fb_oa,fc_oa )                  &
               ,real(kpt_m)* dti                                             &
               ,dti*resv_oa(per_t2p_oa(lt_m))                                &
               ,fb_oa,fc_oa   ) 

!........boucle spatiale:
           horizontal_space_loop2 : do ls_m = begvs_oa(iv_m),begvs_oa(iv_m+1)-1
            i_m = l2i_oa(ls_m)
            j_m = l2j_oa(ls_m)

            vertical_space_loop2 : do ls1_m = begvs3d_oa(ls_m),begvs3d_oa(ls_m+1)-1
            k_m = kmin3d_oa(ls_m) + (ls1_m-begvs3d_oa(ls_m))* dk_oa(iv_m)

             wf_oa(tallocated_oa(lt_m))%coef (                                  &
                      ls1_m-begvs3d_oa(begvs_oa(iv_m))+1 ) =                    &
             wf_oa(tallocated_oa(lt_m))%coef (                                  &
                      ls1_m-begvs3d_oa(begvs_oa(iv_m))+1 )                      &
                     + conjg(psi_m)                                             &
                     * var_oa( tv_oa(iv_m)                                      &
                                   ,ichoix                                      &
                                   ,i_m                                         &
                                   ,j_m                                         &
                                   ,k_m                                         &
                                   ,iv_m                                        &
                                   ,ls1_m-begvs3d_oa(begvs_oa(iv_m))+1       )  &
                     /max(perv_oa(2,lp_m),1.e-30)

            enddo vertical_space_loop2
           enddo horizontal_space_loop2
         enddo period_of_analysis_loop2
       endif time_domain_test2

      enddo time_of_analysis_loop2
      endif period_of_analysis_case

!.....enddo associe a iv_m et ic_m:
      endif
      enddo
      enddo

!.....eventuelle desalocation:

      do la_m = 1,nmsimult_oa
         if ( associated(wf_oa(la_m)%coef) ) then
         if ((kountv_oa(2,wf_oa(la_m)%t_indice).le.iic_oa.or.iic_oa.eq.nt_max-1)      &
           .and.(ichoix.eq.cnb_oa(wf_oa(la_m)%variable).or.ichoix.lt.0)         &
           .and.(ichoix.eq.des_oa(wf_oa(la_m)%variable)                          &
                 .or.des_oa(wf_oa(la_m)%variable).eq.0)                          &
                 ) then                           
            call subsave_oa ( la_m )                           
            call deallocate_win_oa(wf_oa(la_m)%t_indice)
         endif
         endif
      enddo

      endif oa_analysis_requested

      return
      
      end subroutine main_oa


!------------------------------------------------------------------------------
! PROCEDURE
!------------------------------------------------------------------------------
!
!> @note
!
!
! DESCRIPTION: 
!
!> @brief Updates the isopycne localization (closest level and depth).
!
!> @details lev_upd_oa calls update_level_oa which searches for current
!! depth position of the target density (configuration code 20 requested in the OA namelist)
!! and stores the closest level and depth in the wlev_oa structured type array.
!
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version
!! - Benedicte Lemieux-Dudon
!!  - stand alone version
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!! - apply a selective use of module variables. 
!
!------------------------------------------------------------------------------

      subroutine  lev_upd_oa(                       & 
       kmin, kmax                                   &
      ,rhp_t                                        &
      ,rhp_t_lbound                                 &
      ,rhp_t_ubound                                 &
      ,depth_t                                      & !(-1:imax+2,-1:jmax+2,0:kmax+1) 
      ,depth_t_lbound                               &
      ,depth_t_ubound  )

      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock
      use module_oa_level

      implicit none

      !> Grid index range for the analysis of fields passed in argument to the OA module 
      !! Fields only analysed inside grid point domain [imin,imax]x[jmin,jmax]x[kmin,kmax]:
      integer, intent(in) :: kmin, kmax

      !> Lower and upper index bounds of the 3-Dimensional density array
      integer, dimension(3), intent(in) :: rhp_t_lbound, rhp_t_ubound

      !> 3-Dimensional density array at t-grid point
      double precision, dimension(rhp_t_lbound(1):rhp_t_ubound(1),rhp_t_lbound(2):rhp_t_ubound(2),rhp_t_lbound(3):rhp_t_ubound(3)), intent(in) :: &
        rhp_t

      !> Lower and upper index bounds of the 3-Dimensional depth array
      integer, dimension(3), intent(in) :: depth_t_lbound, depth_t_ubound

      !> 3-Dimensional depth array at t-grid point
      double precision, dimension(depth_t_lbound(1):depth_t_ubound(1),depth_t_lbound(2):depth_t_ubound(2),depth_t_lbound(3):depth_t_ubound(3)), intent(in) :: &
        depth_t


      integer                                                         &
       i_l                                                            &
      ,j_l                                                            &
      ,k_l                                                            &
      ,ls_l                                                           &
      ,ls1_l                                                          &
      ,iv_l                                                           &
      ,il_l

      do il_l = 1 , nzlevel_oa
         iv_l = lev2v_oa(il_l)
           do ls_l  =begvs_oa(iv_l),begvs_oa(iv_l+1)-1
            i_l = l2i_oa(ls_l)
            j_l = l2j_oa(ls_l)
            do ls1_l=begvs3d_oa(ls_l),begvs3d_oa(ls_l+1)-1
              k_l = kmin3d_oa(ls_l) + (ls1_l - begvs3d_oa(ls_l))* dk_oa(iv_l)
              call update_level_oa(                                      &
                i_l                                                   &
               ,j_l                                                   &
               ,wlev_oa(il_l)%rhp(ls1_l-begvs3d_oa(begvs_oa(iv_l))+1) &
               ,wlev_oa(il_l)%k  (ls1_l-begvs3d_oa(begvs_oa(iv_l))+1) &
               ,wlev_oa(il_l)%z  (ls1_l-begvs3d_oa(begvs_oa(iv_l))+1) &
               ,kmin, kmax                                   &
               ,rhp_t                                        &
               ,rhp_t_lbound                                 &
               ,rhp_t_ubound                                 &
               ,depth_t                                      &
               ,depth_t_lbound                               &
               ,depth_t_ubound  )

            enddo
           enddo

      enddo

      return
      end subroutine  lev_upd_oa

!------------------------------------------------------------------------------
! PROCEDURE
!------------------------------------------------------------------------------
!
!> @note
!
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
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version and algorithm.
!! - Benedicte Lemieux-Dudon
!!  - stand alone version.
!!  - lower k level set to kmin instead of 1, greater k level set to kmax instead of kmax+1.
!!  - when the search fails, extrapolation treatment is removed (requires supplementary arguments).
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!! - apply a selective use of module variables. 
!! - extrapolation treatment?
!------------------------------------------------------------------------------

      subroutine update_level_oa (                                    &
       i_c                                                            &
      ,j_c                                                            &
      ,rh_c                                                           &
      ,k_c                                                            &
      ,z_c                                                            &
      ,kmin, kmax                                   &
      ,rhp_t                                        &
      ,rhp_t_lbound                                 &
      ,rhp_t_ubound                                 &
      ,depth_t                                      & !(-1:imax+2,-1:jmax+2,0:kmax+1) 
      ,depth_t_lbound                               &
      ,depth_t_ubound  )


      implicit none

      !> Vertical grid index range for the analysis of density field passed in argument to the OA module 
      integer, intent(in) :: kmin, kmax

      !> Lower and upper index bounds of the 3-Dimensional density array
      integer, dimension(3), intent(in) :: rhp_t_lbound, rhp_t_ubound

      !> 3-Dimensional density array at t-grid point
      double precision, dimension(rhp_t_lbound(1):rhp_t_ubound(1),rhp_t_lbound(2):rhp_t_ubound(2),rhp_t_lbound(3):rhp_t_ubound(3)), intent(in) :: &
        rhp_t

      !> Lower and upper index bounds of the 3-Dimensional depth array
      integer, dimension(3), intent(in) :: depth_t_lbound, depth_t_ubound

      !> 3-Dimensional depth array at t-grid point
      double precision, dimension(depth_t_lbound(1):depth_t_ubound(1),depth_t_lbound(2):depth_t_ubound(2),depth_t_lbound(3):depth_t_ubound(3)), intent(in) :: &
        depth_t


      integer                                                         &
       i_c                                                            &
      ,j_c                                                            &
      ,k_c                                                            &
      ,iflag

      real                                                            &
       z_c                                                            &
      ,rh_c                                                           &
      ,z2_c

      integer                                                         &
       l_c                                                            &
      ,dk1_c                                                          &
      ,dk2_c                                                          &
      ,nk_c                                                           &
      ,k2_c


!.....diverses initialisations:
      nk_c = kmax
      !STDALONE k_c   = min(nk_c,max(1,k_c)) 
      k_c   = min(nk_c,max(kmin,k_c)) 
      dk1_c = 0
      dk2_c = 0
      l_c   = 0
      iflag = 0
      k2_c  = 0

!.....boucle sur l'indice l_c:
!                              l_c impair: on cherche au dessus,
!                              l_c pair  : on cherche en dessous.
!
!
!
! la sortie de la boucle est le point delicat. elle est actuellement
! basee sur 3 tests:
!
!
! test 1: la boucle est executee au moins une fois,
! test 2: 1ere partie: la recherche a ete concluante (une profondeur a ete calculee)
!         2eme partie: on sort des bornes au dessus et au dessous (iflag=2)... abandon
! test 3: la premiere recherche "en dessous" n'a rien donne et sort des bornes (<1), on cherche au dessus...
!

      do while (                                                      &
        ( dk1_c.eq.0 .and. dk2_c.eq.0 ).or.                           &
!STDALONE        (  (rh_c-rhp_t(i_c,j_c,min(nk_c,max(1,k_c+dk1_c))))*(rh_c-rhp_t(i_c,j_c,min(nk_c,max(1,k_c+dk2_c)))).gt.0   &
        (  (rh_c-rhp_t(i_c,j_c,min(nk_c,max(kmin,k_c+dk1_c))))*(rh_c-rhp_t(i_c,j_c,min(nk_c,max(kmin,k_c+dk2_c)))).gt.0   &
         .and.iflag.ne.2                                              &
         .and.k_c+dk1_c.ge.1                                          &
         .and.k_c+dk2_c.ge.1                                          &
         .and.k_c+dk1_c.le.nk_c                                       &
         .and.k_c+dk1_c.le.nk_c )                                     &
         .or.                                                         &
         k_c+dk1_c.lt.1                                               &
         )

!......incrementation de l_c l'indice de boucle:
       l_c = l_c + 1

!......calcul des increments verticaux rã©els:
       if (mod(l_c,2).ne.0) then
          dk1_c = -l_c/2-1
          dk2_c = -l_c/2
       else
          dk1_c = l_c/2-1
          dk2_c = l_c/2
       endif

!......bornes verticales:
!STDALONE      if (k_c+dk1_c.ge.1.and.k_c+dk2_c.le.nk_c) then
     if (k_c+dk1_c.ge.kmin.and.k_c+dk2_c.le.nk_c) then

!.........remise a zero de iflag (utilise pour les sorties de domaines):
          iflag = 0

!........calcul de la profondeur dans le cas ou la densite n'est pas constante.
     if (rhp_t(i_c,j_c,k_c+dk2_c)-rhp_t(i_c,j_c,k_c+dk1_c).ne.0.) then
          z2_c = depth_t(i_c,j_c,k_c+dk1_c) +                        &
          ( depth_t(i_c,j_c,k_c+dk2_c)-depth_t(i_c,j_c,k_c+dk1_c) )  &
          * ( rh_c                    -rhp_t(i_c,j_c,k_c+dk1_c) )    &  
          / ( rhp_t(i_c,j_c,k_c+dk2_c)-rhp_t(i_c,j_c,k_c+dk1_c) )     
     else
      z2_c=0.
         endif
!........calcul du niveau le plus proche (utilise le cas echeant pour demarrer
!        la recherche au prochain pas de temps).
         if ( abs(rh_c-rhp_t(i_c,j_c,k_c+dk1_c)).le. abs(rh_c-rhp_t(i_c,j_c,k_c+dk2_c))   ) then
            k2_c = k_c + dk1_c
         else 
            k2_c = k_c + dk2_c
         endif
       else
!.......dans le cas ou l'on sort du domaine (par le haut ou par le bas), 
!       on increment iflag. lorsque iflag=2, i.e. l'on sort successivement
!       par le haut et par le bas, on arrete la recherche... pas de solution! 
        iflag = iflag + 1
       endif
!.....sortie de la boucle principale:
      enddo 
!.....si l'on a quitte la boucle precedente normalement (i.e. sans etre sorti
!     du domaine...) on valide la solution z (et l'on conserve le k le plus proche
!     pour gagner du temps lors de la prochaine recherche). 
!STDALONE      if (k_c+dk1_c.ge.1.and.k_c+dk2_c.le.nk_c) then
      if (k_c+dk1_c.ge.kmin.and.k_c+dk2_c.le.nk_c) then
        z_c = z2_c
        k_c = k2_c
      else
!.......extrapolation eventuelle: attention il s'agit d'une solution tres discutable...
!       mais qui permet de fournir une solution tres approximative.
        !STDALONE
        !TODO extrapolation?

        !if (rh_c.le.rhp_t(i_c,j_c,nk_c).and.rhp_t(i_c,j_c,nk_c-1) -rhp_t(i_c,j_c,nk_c).ne.0.) then
        ! z_c = min((hssh_w(i_c,j_c,1)-h_w(i_c,j_c))*0. ,                            &
        !   depth_t(i_c,j_c,nk_c) +                                  &
        ! ( depth_t(i_c,j_c,nk_c-1)-depth_t(i_c,j_c,nk_c) )         &
        ! * ( rh_c                  -rhp_t(i_c,j_c,nk_c) )            & 
        ! / ( rhp_t(i_c,j_c,nk_c-1) -rhp_t(i_c,j_c,nk_c) )            &
        !          )
        ! k_c = nk_c
        !elseif (rhp_t(i_c,j_c,1) -rhp_t(i_c,j_c,2).ne.0.) then
        ! z_c = max(-h_w(i_c,j_c),                                    &
        !   depth_t(i_c,j_c,2) +                                     &
        ! ( depth_t(i_c,j_c,1)-depth_t(i_c,j_c,2) )                 &
        ! * ( rh_c                  -rhp_t(i_c,j_c,2) )               &
        ! / ( rhp_t(i_c,j_c,1) -rhp_t(i_c,j_c,2) )                    &
        !          )
        ! k_c = 1
        !else
          z_c = 0.
          k_c = 0
        !endif
      endif
            
      return
      end subroutine update_level_oa

!------------------------------------------------------------------------------
! PROCEDURE
!
!> @note
!
!
! DESCRIPTION: 
!
!> @brief returns the complex wavelet value.
!
!> @details 
!
!
! REVISION HISTORY:
! intent(in) specifications.
!> @authors
!! - Francis Auclair, Jochem Floor
!!  - Initial version
!! - Benedicte Lemieux-Dudon
!!   - argument attributes. 
!!   - Reducing if tests : tpsi_p has a single value: if-endif replaced by else-if.
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!! - apply a selective use of module variables. 
!
!------------------------------------------------------------------------------
      complex function psi_oa(                                        & 
                 tpsi_p                                               & 
                ,scale_p                                              & 
                ,t_p                                                  & 
                ,dti_p                                             & 
                ,fb_p                                                 & 
                ,fc_p             )


      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock

!....attention il faut encore tenir compte des normalisations...

      integer, intent(in) :: tpsi_p

      double precision, intent(in) :: dti_p, t_p

      real, intent(in)  ::                                            &
        scale_p                                                       & 
       ,fb_p                                                          & 
       ,fc_p

      real ::                                                         & 
        w0_p                                                          & 
       ,rec_p 

!STDALONE      pi_p = acos(-1.)

! attention exemple de psi incomplet pour windowed fourier...
! les coefs pour la reconstruction n'ont
! pas ete ajoutes.

!.....dirac:

      if (tpsi_p .eq. 0 ) then      
       psi_oa = 1.

!.....ondelette de morlet:
  
      else if (tpsi_p .eq. 1 ) then      
       psi_oa =                                                        & 
          exp( - ( t_p /scale_p ) ** 2 / fb_p )                        & 
        * exp( t_p / scale_p * fc_p*(2.*pi_oa) * (0.,1.) )             & 
!       / (2*pi_oa)**0.5                                               & 
        / (pi_oa)**0.25                                                & 
!       * sqrt( dti_p/ scale_p )    
        / sqrt( scale_p )    

!....."windowed fourier":
      else if (tpsi_p .eq. 2.or.tpsi_p.eq.3) then
       psi_oa =  exp(  2.* pi_oa* ( t_p - dti_p ) / scale_p * (0.,1.) )
      endif 

      end function psi_oa

!------------------------------------------------------------------------------
! PROCEDURE
!------------------------------------------------------------------------------
!
!> @note
!
! DESCRIPTION: 
!
!
! DESCRIPTION: 
!
!> @brief Transforms the requested time period of the analysis into the wavelet scale (s).
!
!> @details transformation periode --> echelle (s) pour l'ondelette choisie.
!
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair, Jochem Floor
!!  - Initial version
!! - Benedicte Lemieux-Dudon
!!  - Stand alone version, argument attributes, pi 
!!  - Reducing if tests : tpsi_p has a single value: if-endif replaced by else-if.
!> @date 2015 January
!> @todo
!
!------------------------------------------------------------------------------

      real function psi_p2s_oa(                                       &
                 tpsi_p                                               &
                ,per_p                                                &
                ,fb_p                                                 &
                ,fc_p                )

      use module_oa_variables
      implicit none

      integer, intent(in) :: tpsi_p

      real, intent(in)    ::                                          &
       per_p                                                          &
      ,fb_p                                                           &
      ,fc_p

!STDALONE      ,pi_p 

!STDALONE       pi_p = acos(-1.)

!.....dirac:

      if (tpsi_p .eq. 0 ) then
       psi_p2s_oa = 0.

!.....ondelette de morlet:

      else if (tpsi_p .eq. 1 ) then
       psi_p2s_oa = per_p / ( 4. * pi_oa ) * (fc_p*2.*pi_oa + sqrt(2.+(fc_p*2.*pi_oa)**2))  
                                                 ! t/torrence central frequency

!....."windowed fourier":

      else if (tpsi_p .eq. 2.or.tpsi_p.eq.3 ) then
       psi_p2s_oa = per_p
      endif 

      return

      end function psi_p2s_oa

!------------------------------------------------------------------------------
! PROCEDURE
!------------------------------------------------------------------------------
!
!> @note Calculus must be adapted according to the wavelet shape (Morlet,...)
!
! DESCRIPTION: 
!
!> @brief In the case of analysis with "integrated time periods", calculates the 
!! wavelet scale resolution or Heisenberg box.
!
!> @details calcul de la taille des boites d'heisenberg et sauvegarde
!! attention reprendre calcul selon ondelette
!
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair, Jochem Floor
!!  - Initial version and algorithm.
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!! - exponential overflow underflow treatment!!!!!
!! - apply a selective use of module variables. 
!------------------------------------------------------------------------------

      subroutine box_oa

      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock

      implicit none

      integer ib

      double precision tb(20001),wb(10001),w0b,npsi2,dtb,dwb
      double precision w0bb,dtbb,dwbb,t0b 

      w0b=fc_oa*2*pi_oa
      dtb=0.1
      dwb=0.1


      tb(1)=-1000.
      do ib=1,20000
       tb(ib+1)=tb(ib)+dtb
      enddo

      wb(1)=0.
      do ib=1,10000
       wb(ib+1)=wb(ib)+dwb
      enddo

      npsi2=0.
      do ib=1,20001
       npsi2=npsi2+exp(-tb(ib)*tb(ib)/fb_oa*2)
      enddo
      npsi2=npsi2*dtb/(pi_oa**0.5) !torrence

!---------------------------------------------------------------
!      npsi2=npsi2*dtb/(2.*pi_oa)  !matlab
!---------------------------------------------------------------

      t0b=0.
!---------------------------------------------------------------
! parametres (ivane)
!
!      do ib=1,20001
!      t0b=t0b+tb(ib)*exp(-tb(ib)*tb(ib)/fb_oa*2.)
!      enddo
!      t0b=sqrt(t0b*dtb/npsi2/(2.*pi_oa)) !matlab
!!      t0b=sqrt(t0b*dtb/npsi2/(pi_oa**0.5)) !torrence
!---------------------------------------------------------------

      w0bb=0.
      do ib=1,10001
      w0bb=w0bb+wb(ib)*exp(-(wb(ib)-w0b)*(wb(ib)-w0b)*fb_oa/2.)*fb_oa/2. ! torrence
      enddo
      w0bb=w0bb*dwb/npsi2/(pi_oa**0.5) !torrence

!---------------------------------------------------------------
!      w0bb=w0bb*dwb/npsi2/(2.*pi_oa) !matlab
!---------------------------------------------------------------

      dtbb=0.
      do ib=1,20001
      dtbb=dtbb+(tb(ib)-t0b)*(tb(ib)-t0b)*exp(-tb(ib)*tb(ib)/fb_oa*2.)
      enddo
      dtbb=sqrt(dtbb*dtb/npsi2/(pi_oa**0.5)) !torrence

!---------------------------------------------------------------
!      dtbb=sqrt(dtbb*dtb/npsi2/(2.*pi_oa)) !matlab
!---------------------------------------------------------------

      dwbb=0.
      do ib=1,10001
      dwbb=dwbb+(wb(ib)-w0bb)*(wb(ib)-w0bb)*exp(-(wb(ib)-w0b)*(wb(ib)-w0b)*fb_oa/2.)*fb_oa/2.   ! torrence
      enddo

      dwbb=sqrt(dwbb*dwb/npsi2/(pi_oa**0.5)) !torrence

!---------------------------------------------------------------
!      dwbb=sqrt(dwbb*dwb/npsi2/(2.*pi_oa)) !matlab
!---------------------------------------------------------------
 
      return
      end subroutine box_oa      


!------------------------------------------------------------------------------
! PROCEDURE
!------------------------------------------------------------------------------
!
!> @note
!
! DESCRIPTION: 
!
!> @brief sauvegarde des coefficients qui viennent d'etre calcules.
!
!> @details 
!
!
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version and algorithm.
!! - Benedicte Lemieux-Dudon
!!  - stand alone version. 
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!! - apply a selective use of module variables. 
!
!------------------------------------------------------------------------------

      subroutine subsave_oa( la_s )
 
      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock

      implicit none
      integer ::                                                      &
            la_s                                                      &
           ,iv_s                                                      &
           ,lt_s                                                      &
           ,ic_s                                                       

      ic_s = wf_oa(la_s)%config
      iv_s = wf_oa(la_s)%variable
      lt_s = wf_oa(la_s)%t_indice
      
      if (updv_oa(iv_s).ne.0) then
       call var_upd_oa ( ic_s,iv_s,lt_s )
      endif

      return
      end subroutine subsave_oa

!------------------------------------------------------------------------------
! PROCEDURE
!------------------------------------------------------------------------------
!
!> @note
!
! DESCRIPTION: 
!
! DESCRIPTION: 
!
!> @brief Updates output arrays var2d, var3d with the analysis when available.
!
!> @details 
!
!
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version and algorithm.
!! - Benedicte Lemieux-Dudon
!!  - unique value for swt_wfpf_oa : replacement of if endif by if elseif endif, January 2015
!!
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!! - apply a selective use of module variables. 
!
!------------------------------------------------------------------------------

      subroutine var_upd_oa(                                          &
       ic_u                                                           &
      ,iv_u                                                           &
      ,lt_u                                                           &
                                )

      use module_oa_variables !updv_oa, kmin3d_oa, begvs3d_oa, begvsoa, l2i_oa, l2j_oa, tupd_oa
      use module_oa_time
      use module_oa_space     !tgv3d_oa, dk_oa
   
      use module_oa_periode   !swt_wfpf_oa
      use module_oa_stock     !wf_oa, tallocated_oa
      use module_oa_level
!      use module_oa_upd      !var3d_oa

      implicit none
!TODO intent(in)
      integer                                                         &
       i_u                                                            &
      ,j_u                                                            &
      ,k_u                                                            &
      ,ic_u                                                           &
      ,iv_u                                                           &
      ,ls_u                                                           &
      ,ls1_u                                                          &
      ,lt_u
    
      variable_commune : if (iv_u.ne.-1.and.updv_oa(iv_u).eq.2) then
!---------------------------------------------------------------------------
!.....mise a jour des variables communes:
!---------------------------------------------------------------------------

!.....History file:

       call history_oa(14,lt_u,iv_u,-1,-1)
 
       var_number_of_dimensions : if (tgv3d_oa(tv_oa(iv_u)).eq.3) then                            ! three-dimensional variables


!.......sortie de la partie reelle du coeff
        if (swt_wfpf_oa(iv_u).eq.1) then
         do ls_u = begvs_oa(iv_u),begvs_oa(iv_u+1)-1
          i_u = l2i_oa(ls_u)
          j_u = l2j_oa(ls_u)
          do ls1_u = begvs3d_oa(ls_u),begvs3d_oa(ls_u+1)-1
           k_u = kmin3d_oa(ls_u) + (ls1_u-begvs3d_oa(ls_u))* dk_oa(iv_u)
           var3d_oa(i_u,j_u,k_u,tupd_oa(iv_u)) = real(wf_oa(tallocated_oa(lt_u))%coef (ls1_u-begvs3d_oa(begvs_oa(iv_u))+1 ) )
          enddo
         enddo
        !endif

!.......sortie du coef au carre:
        else if (swt_wfpf_oa(iv_u).eq.2) then
         do ls_u = begvs_oa(iv_u),begvs_oa(iv_u+1)-1
          i_u = l2i_oa(ls_u)
          j_u = l2j_oa(ls_u)
          do ls1_u = begvs3d_oa(ls_u),begvs3d_oa(ls_u+1)-1
           k_u = kmin3d_oa(ls_u) + (ls1_u-begvs3d_oa(ls_u))* dk_oa(iv_u)
           var3d_oa(i_u,j_u,k_u,tupd_oa(iv_u)) = abs(wf_oa(tallocated_oa(lt_u))%coef (ls1_u-begvs3d_oa(begvs_oa(iv_u))+1 ) )**2
          enddo
         enddo
        !endif

!.......sortie du valeur absolue du coef:
        else if (swt_wfpf_oa(iv_u).eq.3) then
         do ls_u = begvs_oa(iv_u),begvs_oa(iv_u+1)-1
          i_u = l2i_oa(ls_u)
          j_u = l2j_oa(ls_u)
          do ls1_u = begvs3d_oa(ls_u),begvs3d_oa(ls_u+1)-1
           k_u = kmin3d_oa(ls_u) + (ls1_u-begvs3d_oa(ls_u))* dk_oa(iv_u)
           var3d_oa(i_u,j_u,k_u,tupd_oa(iv_u)) = abs(wf_oa(tallocated_oa(lt_u))%coef (ls1_u-begvs3d_oa(begvs_oa(iv_u))+1 ) )
          enddo
         enddo
        !endif

! TODO if else if!!!!
!.......sortie du coef complexe:
        else if (swt_wfpf_oa(iv_u).eq.4) then

         do ls_u = begvs_oa(iv_u),begvs_oa(iv_u+1)-1
          i_u = l2i_oa(ls_u)
          j_u = l2j_oa(ls_u)
          do ls1_u = begvs3d_oa(ls_u),begvs3d_oa(ls_u+1)-1
           k_u = kmin3d_oa(ls_u) + (ls1_u-begvs3d_oa(ls_u))* dk_oa(iv_u)
           var3d_oa(i_u,j_u,k_u,tupd_oa(iv_u)) = (wf_oa(tallocated_oa(lt_u))%coef(ls1_u-begvs3d_oa(begvs_oa(iv_u))+1 ) )
          enddo
         enddo

        endif

       else var_number_of_dimensions                                                          ! two-dimensional variables

!.......sortie de la partie reelle du coef
        if (swt_wfpf_oa(iv_u).eq.1) then
         do ls_u = begvs_oa(iv_u),begvs_oa(iv_u+1)-1
          i_u = l2i_oa(ls_u)
          j_u = l2j_oa(ls_u)
          do ls1_u = begvs3d_oa(ls_u),begvs3d_oa(ls_u+1)-1
           k_u = kmin3d_oa(ls_u) + (ls1_u-begvs3d_oa(ls_u))* dk_oa(iv_u)
           var2d_oa(i_u,j_u,tupd_oa(iv_u)) = real(wf_oa(tallocated_oa(lt_u))%coef(ls1_u-begvs3d_oa(begvs_oa(iv_u))+1 ) )
          enddo
         enddo
        endif

!.......sortie du coef au carre (energy):
        if (swt_wfpf_oa(iv_u).eq.2) then
         do ls_u = begvs_oa(iv_u),begvs_oa(iv_u+1)-1
          i_u = l2i_oa(ls_u)
          j_u = l2j_oa(ls_u)
          do ls1_u = begvs3d_oa(ls_u),begvs3d_oa(ls_u+1)-1
           k_u = kmin3d_oa(ls_u) + (ls1_u-begvs3d_oa(ls_u))* dk_oa(iv_u)
           var2d_oa(i_u,j_u,tupd_oa(iv_u)) = abs(wf_oa(tallocated_oa(lt_u))%coef (ls1_u-begvs3d_oa(begvs_oa(iv_u))+1 ) )**2
          enddo
         enddo
        endif

!.......sortie de la valeur absolue du coef: 
        if (swt_wfpf_oa(iv_u).eq.3) then
         do ls_u = begvs_oa(iv_u),begvs_oa(iv_u+1)-1  
          i_u = l2i_oa(ls_u)
          j_u = l2j_oa(ls_u)
          do ls1_u = begvs3d_oa(ls_u),begvs3d_oa(ls_u+1)-1
           k_u = kmin3d_oa(ls_u) + (ls1_u-begvs3d_oa(ls_u))* dk_oa(iv_u)
           var2d_oa(i_u,j_u,tupd_oa(iv_u)) = abs(wf_oa(tallocated_oa(lt_u))%coef(ls1_u-begvs3d_oa(begvs_oa(iv_u))+1 ) )
          enddo
         enddo
        endif

!.......sortie du coeff complexe:
        if (swt_wfpf_oa(iv_u).eq.4) then
         do ls_u = begvs_oa(iv_u),begvs_oa(iv_u+1)-1  
          i_u = l2i_oa(ls_u)
          j_u = l2j_oa(ls_u)
          do ls1_u = begvs3d_oa(ls_u),begvs3d_oa(ls_u+1)-1
           k_u = kmin3d_oa(ls_u) + (ls1_u-begvs3d_oa(ls_u))* dk_oa(iv_u)
           var2d_oa(i_u,j_u,tupd_oa(iv_u)) = (wf_oa(tallocated_oa(lt_u))%coef (ls1_u-begvs3d_oa(begvs_oa(iv_u))+1 ) )
          enddo
         enddo
        endif

        endif var_number_of_dimensions

      endif variable_commune

      return
      end subroutine  var_upd_oa

!------------------------------------------------------------------------------
! PROCEDURE
!------------------------------------------------------------------------------
!
!> @note
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
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version and algorithm.
!! - Benedicte Lemieux-Dudon
!!  - stand alone version.
!!  - i,j,k loop order inversion.
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!! - apply a selective use of module variables. 
!
!------------------------------------------------------------------------------
! TODO must be called in initial_oa et main_oa if ifl_test_oa == 99      

      subroutine test_oa(         &   
       ichoix                     & 
      ,iic_oa                & 
      ,dti                     & 
      ,imin, imax                 &
      ,jmin, jmax                 &
      ,kmin, kmax )


      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock

      implicit none

      !> Time integration step
      double precision, intent(in) :: dti

      !> Current model integration iteration
      integer, intent(in) :: iic_oa 

      !> Grid index range for the analysis of fields passed in argument to the OA module 
      !! Fields can only be annalysed at grid points inside [imin,imax]x[jmin,jmax]x[kmin,kmax]:
      integer, intent(in) :: &
       imin, imax            & 
      ,jmin, jmax            &
      ,kmin, kmax

      integer, intent(in) ::                                          &
       ichoix

      integer :: i, j, k
      double precision                                                &
       time_t

      !print*, 'IN TEST_OA ichoix ',ichoix

      if (ichoix.eq.0) then
!************************************************************************
!      initialisations
!************************************************************************
      !TODO documenter le choix de config 99 et enlever l'affectation
      !de ifl_test_oa ici car elle est faite dans le notebook.
      !ifl_test_oa = 1

      period_test_oa(1) = 0.15
      period_test_oa(2) = 0.2
      period_test_oa(3) = 0.333
      period_test_oa(4) = 0.5

      vardp_test_oa = 0.
     
      !print*, 'Periode ', period_test_oa(3)
!STDALONE      if (ichoix.eq.1) then
!ichoix is either 0 or 1 never both
      else if (ichoix.eq.1) then

        !print*,'ichoix ',ichoix
!************************************************************************
!      mise a jour
!************************************************************************
! variable a tester
      time_t = dti * real(iic_oa)

      do k=kmin,kmax
      do j=jmin,jmax
      do i=imin,imax !0,imax+1
         if (i.le.int(imax/2)) then !TODO CHECK OK? i=0,imax+1
            vardp_test_oa(i,j,k)=  float(k)*cos(2.d0*pi_oa/period_test_oa(1)*time_t)   &
                                 + float(i)*sin(2.d0*pi_oa/period_test_oa(2)*time_t)
         else
            vardp_test_oa(i,j,k)=     10.d0                                         &
                                 +   time_t*cos(2.d0*pi_oa/period_test_oa(3)*time_t)   &
                                 + float(i)*sin(2.d0*pi_oa/period_test_oa(4)*time_t)
         endif
         !if ( ( i==5) .and. ( j==2) .and. (k==45) ) print*, 'vardp_test_oa is ',time_t, vardp_test_oa(i, j, k )
      enddo
      enddo
      enddo

      endif

      return

      end subroutine test_oa

!------------------------------------------------------------------------------
! PROCEDURE
!------------------------------------------------------------------------------
!
!> @note
!
! DESCRIPTION: 
!
!> @brief Allocates fields of array wf_oa to successively stores all the on-line analysis.
!
!> @details wf_oa fields are:
!
!
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version
!! - Benedicte Lemieux-Dudon
!!  - stand alone version.
!!  - apply a selective use of module variables. 
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!------------------------------------------------------------------------------

      subroutine allocate_win_oa (lti_a,lc_a,lv_a,dim_a,iic_oa)

      !use module_oa_time
      !use module_oa_space
      !use module_oa_periode
      use module_oa_stock , only    : wf_oa,           &  !< Array of structured type which successively stores all the on-line analysis
                                      tallocated_oa       !< Conversion of the specific time of analysis lti_a to the corresponding wf_oa array entry l_a
      use module_oa_variables , only : nmsimult_oa         !< Maximum authorized size for array wf_oa 


      implicit none

      integer, intent(in) :: iic_oa

      integer, intent(in) :: &
        lc_a                 &  !< index related to a specific configuration of analysis 
       ,lv_a                 &  !< index related to a specific variable to analyse
       ,lti_a                   !< index related to a specific time of analysis (i.e., time convolution window)

      integer, intent(in) :: dim_a !< Size of the spatial domain to analyse for variable lv_a 
      integer l_a

      l_a=1
      do while ( (associated(wf_oa(l_a)%coef)).and.l_a.lt.nmsimult_oa)
         l_a = l_a + 1
      enddo

      if (l_a.eq.nmsimult_oa.and.associated(wf_oa(l_a)%coef) ) then
         write (6,*) 'nmsimult_oa trop petit!'
         stop
      endif

      allocate( wf_oa(l_a)%coef(dim_a) )

!.....History file:
      call history_oa(2,lc_a,lv_a,l_a,lti_a,iic_oa)

      wf_oa(l_a)%coef(:)               = (0.,0.)
      tallocated_oa(lti_a)             = l_a
      wf_oa(l_a)%t_indice              = lti_a 
      wf_oa(l_a)%config                = lc_a 
      wf_oa(l_a)%variable              = lv_a 

      return
      end subroutine allocate_win_oa

!------------------------------------------------------------------------------
! PROCEDURE
!------------------------------------------------------------------------------
!
!> @note
!
! DESCRIPTION: 
!
!> @brief Deallocates fields of array wf_oa.
!
!> @details
!
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!------------------------------------------------------------------------------

      subroutine deallocate_win_oa (lt_a)    

      use module_oa_stock , only    : wf_oa,           &  !< Array of structured type which successively stores all the on-line analysis
                                      tallocated_oa       !< Conversion of the specific time of analysis lti_a to the corresponding wf_oa array entry l_a
      use module_oa_variables , only : nmsimult_oa         !< Maximum authorized size for array wf_oa 
      !use module_oa_time
      !use module_oa_space
      !use module_oa_periode

      implicit none
      integer lt_a

      
!.....History file:
      call history_oa(3,lt_a,-1,-1,-1)

      deallocate (wf_oa(tallocated_oa(lt_a))%coef )

      nullify( wf_oa(tallocated_oa(lt_a))%coef )

!     wf_oa(tallocated_oa(lt_a))%t_indice   = -1          ! 20070608 moved down a couple of lines..
      
      wf_oa(tallocated_oa(lt_a))%config     = -1
      wf_oa(tallocated_oa(lt_a))%variable   = -1 
      tallocated_oa(lt_a)                   = -1


!      wf_oa(tallocated_oa(lt_a))%t_indice   = -1

! for some reason:
! wf_oa(tallocated(lt_a))%t_indice was used in the call to this subroutine
! as a consequence, if that's changed, the subroutine can't use it any longer??
! anyhow, by moving this command to the end of the subroutine,
! the problem seems solved
      return
      end subroutine deallocate_win_oa 

!------------------------------------------------------------------------------
! PROCEDURE
!------------------------------------------------------------------------------
!
!> @note
!
! DESCRIPTION: 
!
!> @brief Handles the dynamical allocation.
!
!> @detail Allocation subroutines:
!! - allocate_namelist_oa / deallocate_namelist_oa : allocation to handle namelists.
!! - allocate_part1_oa : parameters to define the type of analysis (required to read namelists)
!!                       parameters for the 2d state vector structure, for the time and period of analysis. 
!! - allocate_part2_oa : parameters for the 3d state vector structure (and 2d/vector conversion).
!! - allocate_part3_oa : parameters relative to the time convolution resolution, and time periods to analyse (also reconstruction factor)
!! - allocate_part4_oa
!! - allocate_part5_oa
!! - allocate_lev_part1_oa : array of structured type wlev_oa allocated (for isopycne analysis).
!! - allocate_lev_part2_oa : fields of array wlev_oa allocation.
!
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version and algorithm.
!! - Benedicte Lemieux-Dudon
!!  - stand alone version
!!  - reduced allocation size to nzlevel_oa instead of nzv_oa for the wlev_oa array (allocate_lev_part1_oa)
!!  - allocation consitent with imin,imax jmin,jmax and kmin,kmax grid index domain
!!  - cleaning NHOMS/Symphonie specific variables, namelists,...
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!! - temhat_oa_t, salhat_oa_t,... arrays should be reintroduced to analyse field deviation from their mean.
!! - apply a selective use of module variables. 
!
!------------------------------------------------------------------------------

      subroutine allocate_namelist_oa

      use module_oa_variables , only : nzc_oa_names, nzc_oa_vartyp
      use module_oa_periode , only : nzc_oa

      implicit none

          allocate( nzc_oa_names(1:nzc_oa) )
          allocate( nzc_oa_vartyp(1:nzc_oa) )

      end subroutine allocate_namelist_oa

      subroutine deallocate_namelist_oa

      use module_oa_variables , only : nzc_oa_names, nzc_oa_vartyp

      implicit none

          deallocate( nzc_oa_names )
          deallocate( nzc_oa_vartyp )

      end subroutine deallocate_namelist_oa

      subroutine allocate_part1_oa( &
         imin, imax                 &
        ,jmin, jmax                 &
        ,kmin, kmax )


      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock
!      use module_oa_upd

      implicit none

      !> Grid index range for the analysis of fields passed in argument to the OA module 
      integer, intent(in) :: &
       imin, imax            & 
      ,jmin, jmax            &
      ,kmin, kmax

      integer                                                         &
       l_a

!.....definition de toutes les tailles maximales des differentes structures
!     (temporelle-spatiale-frequentielle)
!
!     module_oa_variables
!STDALONE calculation only for index range imin,imax jmin,jmax kmin,kmax
!STDALONE      allocate (vardp_test_oa(0:imax+1,0:jmax+1,0:kmax+1))
! Might be only allocated if ifl_test_oa is 1?
      allocate (vardp_test_oa(imin:imax,jmin:jmax,kmin:kmax))

!STDALONE : kept array in case needed in the STAND ALONE OA version
!STDALONE calculation only for index range imin,imax jmin,jmax kmin,kmax
!     allocate (rhphat_oa_t(1:imax,1:jmax,1:kmax))
!     allocate (temhat_oa_t(1:imax,1:jmax,1:kmax))
!     allocate (salhat_oa_t(1:imax,1:jmax,1:kmax))       

!.....module_var:

      allocate (                                                      &
         swt_d_oa(nzv_oa)                                             &   !< caracteristiques spatiales de la variable (voir notebook_oa)
        ,swt_t_oa(nzv_oa)                                             &   !< caracteristiques temporelles de la variable (voir notebook_oa)          
        ,tv_oa   (nzv_oa)                                             &   !< code variable associe 
        ,cnb_oa  (nzv_oa)                                             &   !< call number: position of the call in the baroclinic / barotropic time step.
        ,tvc_oa  (nzv_oa)                                             &   !< configuration associee a une variable    
        ,updv_oa (nzv_oa)                                             &   !< flag de remise a jour    
        ,save_oa (nzv_oa)                                             &   !< flag de sauvegarde
        ,tupd_oa (nzv_oa)                                             &   !< pour variables communes
        !,tvar_oa (200,10,10)                                         &   !STDALONE known size, allocated elsewere
         )

      allocate (                                                      &
         begvs_oa(nzv_oa+1)                                           &   ! structure 2d du vecteur d etat
        ,begvt_oa(nzv_oa+1)                                           &   ! structure temporelle du vecteur d etat
         )

      allocate (                                                      &
         lat_oa  (2,nzv_oa)                                           &   ! latitude  min, max de la structure 2d du vecteur d etat
        ,lon_oa  (2,nzv_oa)                                           &   ! longitude  ...
        ,h_oa    (2,nzv_oa)                                           &   ! profondeur ...  
        ,k_oa    (2,nzv_oa)                                           &   ! niveaux verticaux min et max de la structure 3d du vecteur d etat
        ,ptij_oa (2,nzv_oa)                                           &   ! point particulier demande par l utilisateur
         )
  
      allocate (                                                      &
         dx_oa   (nzv_oa)                                             &   ! resolution horizontale suivant x demandee par l utilisateur
        ,dy_oa   (nzv_oa)                                             &   ! resolution horizontale suivant y demandee par l utilisateur
        ,dk_oa   (nzv_oa)                                             &   ! resolution verticale   suivant z demandee par l utilisateur
         )
 
      allocate(                                                       &
         nzpt_per_oa(nzv_oa)                                          &   ! nombre de points de discretisation par periode
         )

      allocate (                                                      &
         kount_user_oa(3,nzv_oa)                                      &   ! description des periodes choisies par l utilisateur
         )

      allocate (                                                      &
         t0_oa (nzv_oa)                                               &   ! date de la premiere sortie
                 )
 
      allocate (                                                      &
         tpsi_oa (nzv_oa)                                             &   ! type d atome utilise
         )

      allocate (                                                      &
         swt_wfpf_oa (nzv_oa)                                        &    ! calcul du coef wf ou du spectre pf (choix utilisateur)
        ,fl_rec_oa   (nzv_oa)                                         &   ! flag de reconstruction
         )

      allocate (                                                      &
         dori_oa      (nzv_oa)                                        &   ! configuration frequentielle choisie par l utilisateur
        ,delta_t_oa   (nzv_oa)                                        &   ! nombre de periodes etudiees
         )

      allocate (                                                      &
         per_oa (3,nzv_oa)                                            &   ! preriodes min, max, delta de chaque variable (ou configuration)
         )

      allocate (                                                      &
         begvp_oa (nzv_oa+1)                                          &   ! structure frequentielle du vecteur d etat
         )

      allocate (                                                      &
         tc_oa (nzc_oa)                                               &
         )   

      allocate (                                                      &
         begc_oa (nzc_oa+1)                                           &
         )   

      return
      end subroutine allocate_part1_oa

      subroutine allocate_part2_oa( &     
         imin, imax                 &
        ,jmin, jmax ) 


      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock

      implicit none

      !> Grid index range for the analysis of fields passed in argument to the OA module 
      !! Fields can only be annalysed at grid points inside [imin,imax]x[jmin,jmax]x[kmin,kmax]:
      integer, intent(in) :: &
       imin, imax            & 
      ,jmin, jmax

      integer                                                         &
         l_a

      allocate (                                                      &
         l2i_oa (nzvs_oa)                                             &   ! transformation l --> i
        ,l2j_oa (nzvs_oa)                                             &   ! transformation l --> j
         )

      allocate (                                                      &
         begvs3d_oa(nzvs_oa+1)                                        &   ! structure 3d du vecteur d etat: debut de la colonne pour un point donne
        ,kmin3d_oa (nzvs_oa+1)                                        &   ! structure 3d du vecteur d etat: premier niveau a considerer     
         )
 
      allocate (                                                      &
!STDALONE         ij2l_oa ( 0:imax+1,0:jmax+1,nzv_oa)                 
         ij2l_oa ( imin:imax,jmin:jmax,nzv_oa)                        &   ! transformation (i,j) --> l (l=  structure 2d du vecteur d etat)
         )

      return
      end subroutine allocate_part2_oa

      subroutine allocate_part3_oa

      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock

      implicit none

      allocate (                                                      &   
       resv_oa (nzvp_oa)                                              &  ! resolution temporelle pour le calcul de la convolution
       )
      allocate (                                                      &  ! periodes associees a la structure vectorielle du vecteur d etat, 
       perv_oa (2,nzvp_oa)                                            &  ! facteurs de reconstruction associes a l'ondelette
       )

      return
      end subroutine allocate_part3_oa

      subroutine allocate_part4_oa

      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock

      implicit none

      allocate (                                                      &
       kountv_oa (2,nzvt_oa)                                          &  ! calcul des kounts de debut et de fin pour chaque variable (ou configuration)
       )
    
      allocate (                                                      & 
       tallocated_oa(nzvt_oa)                                         &
       )

      allocate (                                                      &
       per_t2p_oa (nzvt_oa)                                           &  ! transformation structure temporelle --> structure frequentielle
       )

      return
      end subroutine allocate_part4_oa


      subroutine allocate_part5_oa

      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock

      implicit none

      integer                                                         &
       l_a


      allocate (                                                      &
       wf_oa (nmsimult_oa)                                            &           ! vecteur d etat resultat (w) contenant les analyses
       )

      do l_a=1,nmsimult_oa
         nullify(wf_oa(l_a)%coef)
      enddo

      tallocated_oa(:)   = -1
      wf_oa(:)%t_indice  = -1
      wf_oa(:)%config    = -1
      wf_oa(:)%variable  = -1


      return
      end subroutine allocate_part5_oa

      subroutine allocate_lev_part1_oa 

      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock
      use module_oa_level

      implicit none

      !STDALONE Size can be reduced to nzlevel_oa see lev_init_oa modifications.
      !STDALONE allocate(wlev_oa (nzv_oa))
      !STDALONE allocate(lev2v_oa(nzv_oa))
      !STDALONE allocate(v2lev_oa(nzv_oa))
      allocate(wlev_oa (nzlevel_oa))
      allocate(lev2v_oa(nzlevel_oa))
      allocate(v2lev_oa(nzlevel_oa))

      return
      end subroutine allocate_lev_part1_oa 

      subroutine allocate_lev_part2_oa (l_a,lv_a,dim_a)

      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock
      use module_oa_level

      implicit none

      integer, intent(in) :: dim_a
      integer, intent(in) :: l_a, lv_a

      !STDALONE l_a now ranges from 1 to nzlevel_oa
      allocate( wlev_oa(l_a)%z  (dim_a) )
      allocate( wlev_oa(l_a)%rhp(dim_a) )
      allocate( wlev_oa(l_a)%k  (dim_a) )

!.....History file:
      call history_oa(1,l_a,lv_a,dim_a,-1)

      wlev_oa(l_a)%k(:) = 0
      wlev_oa(l_a)%z(:) = 0.

      return
      end subroutine allocate_lev_part2_oa


!------------------------------------------------------------------------------
! PROCEDURE
!------------------------------------------------------------------------------
!
!> @note
!
! DESCRIPTION: 
!
!> @brief Handling OA module history output.
!
!> @details 
!
!
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair and Ivane Pairaud:
!!  - Initial version and algorithm.
!! - Benedicte Lemieux-Dudon
!!  - stand alone version
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!! - apply a selective use of module variables. 
!
!------------------------------------------------------------------------------

      subroutine history_oa ( ichoix, i1_h, i2_h, i3_h, i4_h &
                              ,iic_oa, nt_max )

      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock
!      use module_oa_upd

      implicit none
   
#include "param_F90.h"
#include "scalars_F90.h"
 
      integer, intent(in) :: ichoix
      integer, intent(in) :: i1_h, i2_h, i3_h, i4_h

      !> Current model integration iteration
      integer, intent(in), optional :: iic_oa 

      !> Last simulation iteration index
      integer, intent(in), optional :: nt_max                                        

      integer ::                                                     &
          k_m                                                        &
         ,ir_o                                                       &
         ,l_a                                                        &
         ,lv_a                                                       &
         ,lc_a                                                       &
         ,lti_a                                                      &
         ,ic_o                                                       &
         ,ic_u                                                       &
         ,lt_u                                                       &
         ,lt_o                                                       &
         ,lp_o                                                       &
         ,iv_o                                                       &
         ,la_s                                                       &
         ,lt_a                                                       &
         ,dim_a                                                      &
         ,iv_u                                                       &
         ,iv_s

       character(len=250) :: file_hist


      if (mynode.ne.0) return

!---->Fichier de sortie:

      file_hist = trim(directory_out) // txtslash // 'history_oa'

      !write(6,*) file_hist

      if (ichoix.eq.1) then
!*******************************************************************************
! allocate_lev_part2_oa
!*******************************************************************************
       !if (ifl_oa_out.eq.1.or.(ifl_oa_out.eq.2.and.par%rank.eq.0)) then
       !if (ifl_oa_out.eq.1) call sequential_begin()
       open(unit=io_unit,file=trim(file_hist),position='append')   
       write (io_unit,*) 'allocation: level n°',i1_h,',var n°',i2_h,',dim=',i3_h
       close(io_unit)
       !if (ifl_oa_out.eq.1) call sequential_end()
       !endif
      endif

      if (ichoix.eq.2) then
!*******************************************************************************
! allocate_win_oa  
!*******************************************************************************
       !if (ifl_oa_out.eq.1.or.(ifl_oa_out.eq.2.and.par%rank.eq.0)) then
       !if (ifl_oa_out.eq.1) call sequential_begin()
       open(unit=io_unit,file=trim(file_hist),position='append')   
!       write (io_unit,*) 'allocation: fenetre n°',l_a,',iic_oa=',iic_oa       &
!                    ,',lc=',i1_h                                       &
!                    ,',lv=',i2_h                                       &
!                    ,',dim=',i3_h                                      &
!                    ,',lti=',i4_h
       write (io_unit,*) 'allocation: fenetre n°',i3_h,',iic_oa=',iic_oa       &
                    ,',lc=',i1_h                                       &
                    ,',lv=',i2_h                                       &
                    ,',dim=?'                                      &
                    ,',lti=',i4_h
       close(io_unit)
       !if (ifl_oa_out.eq.1) call sequential_end()
       !endif
      endif

      if (ichoix.eq.3) then
!*******************************************************************************
! deallocate_win_oa  
!*******************************************************************************
       !if (ifl_oa_out.eq.1.or.(ifl_oa_out.eq.2.and.par%rank.eq.0)) then
        !if (ifl_oa_out.eq.1) call sequential_begin()
        open(unit=io_unit,file=trim(file_hist),position='append')   
        write (io_unit,*) 'desallocation: fenetre n°',tallocated_oa(i1_h)
        close(io_unit)
        !if (ifl_oa_out.eq.1) call sequential_end()
       !endif
      endif


      if (ichoix.eq.4) then
!*******************************************************************************
! initial_oa
!*******************************************************************************
       !if (ifl_oa_out.eq.1.or.(ifl_oa_out.eq.2.and.par%rank.eq.0)) then
        !if (ifl_oa_out.eq.1) call sequential_begin()
        open(unit=io_unit,file=trim(file_hist),position='append')   
        write(io_unit,*)
        write(io_unit,*) '*************************'
        write(io_unit,*) 'subroutine: initial_oa'
        write(io_unit,*) '*************************'
        close(io_unit)
        !if (ifl_oa_out.eq.1) call sequential_end()
       !endif
      endif

      if (ichoix.eq.5) then
!*******************************************************************************
! initial_oa   
!*******************************************************************************
!******************************************************************
!        caracteristiques d'une variable:      
!******************************************************************
       !if (ifl_oa_out.eq.1.or.(ifl_oa_out.eq.2.and.par%rank.eq.0)) then
        !if (ifl_oa_out.eq.1) call sequential_begin()
         open(unit=io_unit,file=trim(file_hist),position='append')   
         loop_config : do ic_o=1,nzc_oa
            write(io_unit,*)
            write(io_unit,*)
            write(io_unit,*) '*************************************'
            write(io_unit,*) ' configuration de variables n°',ic_o
            write(io_unit,*) '*************************************'
            if (tc_oa(ic_o).ge.100) then
               write (io_unit,*) 'type de configuration:',tc_oa(ic_o)
            endif

            loop_variable : do iv_o=begc_oa(ic_o),begc_oa(ic_o+1)-1
               write(io_unit,*)
               write(io_unit,*) '*************************************'
               write(io_unit,*) '        variable n°',iv_o
               write(io_unit,*) '*************************************'

               write(io_unit,*) '----------------------'
               write(io_unit,*) '---caracteristiques---'
               write(io_unit,*) '----------------------'
               write(io_unit,*) 'type            : ',tv_oa(iv_o) 
               if (swt_wfpf_oa(iv_o).eq.1) then
                  write(io_unit,*) 'calcul du coef. reel'
               endif 
               if (swt_wfpf_oa(iv_o).eq.2) then
                  write(io_unit,*) 'calcul du module au carre'
               endif 
               if (swt_wfpf_oa(iv_o).eq.3) then
                  write(io_unit,*) 'calcul du coef. complexe'
               endif 
               if (save_oa(iv_o).eq.0) then
                  write(io_unit,*) 'variable non sauvegardee dans un fichier'
               endif 
               if (save_oa(iv_o).eq.1) then
                  write(io_unit,*) 'variable sauvegardee dans un fichier'
               endif 
               if (updv_oa(iv_o).eq.1) then
                  write(io_unit,*) 'variable utilisee pour une mise a jour',iv_s
               endif 

               write(io_unit,*) 
               write(io_unit,*) '----------------------------------'
               write(io_unit,*) '---caracteristiques de l''atome---'
               write(io_unit,*) '----------------------------------'

               write(io_unit,*) 'type de l''atome                : ',tpsi_oa(iv_o)
               
               if (tpsi_oa(iv_o).eq.0) then
                  write(io_unit,*) 'dirac'
               endif
               if (tpsi_oa(iv_o).eq.1) then
                  write(io_unit,*) 'ondelette morlet complexe fb_oa,fc_oa: ',fb_oa,fc_oa
               endif
               if (tpsi_oa(iv_o).eq.2) then
                  write(io_unit,*) 'windows fourier'
               endif
               if (tpsi_oa(iv_o).eq.3) then
                  write(io_unit,*) 'transformee de fourier classique'
               endif

               if (tpsi_oa(iv_o).ne.3) then
                  write(io_unit,*) 'largeur (h ou nbre de periodes): ',delta_t_oa(iv_o)
               endif
               write(io_unit,*) 'nombre de points par periode   : ',nzpt_per_oa(iv_o)

               write(io_unit,*) 
               write(io_unit,*) '------------------------------'
               write(io_unit,*) '---configuration temporelle---'
               write(io_unit,*) '------------------------------'
               if (unite_oa.eq.1.) then
                write(io_unit,*) 'unites                     : ','secondes'
               else
               if (unite_oa.eq.3600.) then
                 write(io_unit,*) 'unites                     : ','heures'
                else
                 write(io_unit,*) 'unites                     : ','autre'
                 endif
               endif
               write(io_unit,*) 'type echantillonnage       : ',swt_t_oa(iv_o)
               write(io_unit,*) 'premiere sortie (h/s)      : ',t0_oa(iv_o)
               write(io_unit,*) 'discretisation/integration : ',dori_oa(iv_o)

               if (swt_t_oa(iv_o).eq.1.or.swt_t_oa(iv_o).eq.4) then
                  write(io_unit,*) 'extraction: kount initial  : ',kount_user_oa(1,iv_o)
                  write(io_unit,*) '            kount final    : ',kount_user_oa(2,iv_o)
                  write(io_unit,*) '            delta(kount)   : ',kount_user_oa(3,iv_o)
               else if (swt_t_oa(iv_o).eq.3) then
                  write(io_unit,*) 'extraction: kount initial: ',kount_user_oa(1,iv_o)
               else if (swt_t_oa(iv_o).eq.3) then
                  write(io_unit,*) 'extraction: kount final  : ',nt_max-1
               endif 

               do lt_o  = begvt_oa(iv_o) , begvt_oa(iv_o+1) - 1
                  if (   kountv_oa(1,lt_o).ne.-9999.or.kountv_oa(2,lt_o).ne.-9999) then
                     write(io_unit,*) 'localisation de l''atome n°',lt_o-begvt_oa(iv_o)+1,' (iic_oa) :',kountv_oa(1,lt_o), kountv_oa(2,lt_o)
                  else
                     write(io_unit,*) 'pas d''analyse pour la localisation n°',lt_o-begvt_oa(iv_o)+1
                  endif
               enddo

               write(io_unit,*) 
               write(io_unit,*) '----------------------------'
               write(io_unit,*) '---configuration spatiale---'
               write(io_unit,*) '----------------------------'
               write(io_unit,*) 'type d''echantillonnage        : ',swt_d_oa(iv_o)  

               if (swt_d_oa(iv_o).eq.1.or.swt_d_oa(iv_o).eq.3) then
#ifdef SPHERICAL
                  write(io_unit,*) 'extraction: lat min,lat max   : ',lat_oa(1,iv_o)*180/pi_oa,lat_oa(2,iv_o)*180/pi_oa
                  write(io_unit,*) '            lon min,lon max   : ',lon_oa(1,iv_o)*180/pi_oa,lon_oa(2,iv_o)*180/pi_oa
#else
                  write(io_unit,*) 'extraction: lat min,lat max   : ',lat_oa(1,iv_o),lat_oa(2,iv_o)
                  write(io_unit,*) '            lon min,lon max   : ',lon_oa(1,iv_o),lon_oa(2,iv_o)
#endif
               else if (swt_d_oa(iv_o).eq.2) then
                  write(io_unit,*) 'extraction: point (i,j)       : ',ptij_oa(1,iv_o),ptij_oa(2,iv_o)
               endif

               if (swt_d_oa(iv_o).eq.3)                               &
                  write(io_unit,*) '            prof min,prof max : ',h_oa(1,iv_o),h_oa(2,iv_o)
               write(io_unit,*)    '            kmin,kmax         : ',k_oa(1,iv_o),k_oa(2,iv_o)
               if (swt_d_oa(iv_o).ne.2) then
                  write(io_unit,*) '            di,dj,dk          : ',dx_oa(iv_o),dy_oa(iv_o),dk_oa(iv_o)
               else
                  write(io_unit,*) '            dk                : ',dk_oa(iv_o)
               endif

               write(io_unit,*) 
               write(io_unit,*) '---------------------------------'
               write(io_unit,*) '---configuration frequentielle---'
               write(io_unit,*) '---------------------------------'
               if (dori_oa(iv_o).eq.1) then
                  write(io_unit,*) 'periodes (t) discretes'
               else
                  write(io_unit,*) 'periodes (t) integrees'
               endif
               write(io_unit,*)    'tmin,dt,tmax (h ou s)      : ',per_oa (1,iv_o)/unite_oa,per_oa (3,iv_o)/unite_oa,per_oa (2,iv_o)/unite_oa
               do lp_o = begvp_oa(iv_o) , begvp_oa(iv_o+1)-1
                  write (io_unit,*) 'periode n°',lp_o-begvp_oa(iv_o)+1,'en h ou s     :',perv_oa(1,lp_o)/unite_oa
                  write (io_unit,*) 'coef. reconstruction  :',perv_oa(2,lp_o)
                  write( io_unit,*) 'resolution de l''atome :',resv_oa(lp_o)
               enddo

            enddo loop_variable
         enddo loop_config

         write(io_unit,*)
         write(io_unit,*)
         write(io_unit,*) '*************************************'
         write(io_unit,*) '          tailles en memoire'
         write(io_unit,*) '       des differentes structures'
         write(io_unit,*) '*************************************'
         write(io_unit,*)

         write(io_unit,*) 'allocations simultanees (max) : ',nmsimult_oa
         write(io_unit,*) 'structure temporelle          : ',nzvt_oa
         write(io_unit,*) 'structure spatiale (2d)       : ',nzvs_oa
         write(io_unit,*) 'structure spatiale (3d)       : ',nzvs3d_oa
         write(io_unit,*) 'structure frequentielle       : ',nzvp_oa
         close(io_unit)
         !if (ifl_oa_out.eq.1) call sequential_end()
       !endif
      endif


      if (ichoix.eq.6) then
!*******************************************************************************
! initial_oa   
!*******************************************************************************
!******************************************************************
!     alocations dynamiques:
!******************************************************************
       !if (ifl_oa_out.eq.1.or.(ifl_oa_out.eq.2.and.par%rank.eq.0)) then
        !if (ifl_oa_out.eq.1) call sequential_begin()
        open(unit=io_unit,file=trim(file_hist),position='append')   
        write (io_unit,*)
        write(io_unit,*) '*************************'
        write (io_unit,*) 'subroutine: allocate::'
        write(io_unit,*) '*************************'
        write (io_unit,*)
        write (io_unit,*) 'nmsimult_oa =',nmsimult_oa
        write (io_unit,*) 'nzv_oa      =',nzv_oa
        write (io_unit,*) 'nzvs_oa     =',nzvs_oa
        write (io_unit,*) 'nzvt_oa     =',nzvt_oa
        write (io_unit,*) 'nzvp_oa     =',nzvp_oa
        write (io_unit,*) 'nzvc_oa     =',nzc_oa
        write (io_unit,*) 'nzvs3d_oa   =',nzvs3d_oa
        write (io_unit,*)
        close(io_unit)
        !if (ifl_oa_out.eq.1) call sequential_end()
       !endif
      endif

!STDALONE struct_oa eliminated      if (ichoix.eq.7) then
!*******************************************************************************
! struct_oa   
!*******************************************************************************
!      !if (ifl_oa_out.eq.1.or.(ifl_oa_out.eq.2.and.par%rank.eq.0)) then
!       !if (ifl_oa_out.eq.1) call sequential_begin()
!       open(unit=io_unit,file=trim(file_hist),position='append')   
!       write(io_unit,*)
!       write(io_unit,*) '*************************'
!       write(io_unit,*) 'subroutine: struct_oa'
!       write(io_unit,*) '*************************'
!       close(io_unit)
!       !if (ifl_oa_out.eq.1) call sequential_end()
!      !endif
!     endif

!STDALONE struct_oa eliminated     if (ichoix.eq.8) then
!*******************************************************************************
! struct_oa   
!*******************************************************************************
!      !if (ifl_oa_out.eq.1.or.(ifl_oa_out.eq.2.and.par%rank.eq.0)) then
!       !if (ifl_oa_out.eq.1) call sequential_begin()
!       !open(unit=io_unit,file=trim(dir_history_exp)//trim(file_hist)//'_'//dom_c//'.out',position='append')    
!       open(unit=io_unit,file=trim(file_hist),position='append')   
!       write(3,*) '...fichier structure initialise.'
!       close(3)
!       !if (ifl_oa_out.eq.1) call sequential_end()
!      !endif
!     endif

!STDALONE subsave_init_oa eliminated     if (ichoix.eq.9) then
!*******************************************************************************
! subsave_init_oa   
!*******************************************************************************
!      !if (ifl_oa_out.eq.1.or.(ifl_oa_out.eq.2.and.par%rank.eq.0)) then
!       !if (ifl_oa_out.eq.1) call sequential_begin()
!       !open(unit=io_unit,file=trim(dir_history_exp)//trim(file_hist)//'_'//dom_c//'.out',position='append')    
!       open(unit=io_unit,file=trim(file_hist),position='append')   
!       write(io_unit,*)
!       write(io_unit,*) '*****************************'
!       write(io_unit,*) 'subroutine: save_init_oa'
!       write(io_unit,*) '*****************************'
!       close(io_unit)
!       !if (ifl_oa_out.eq.1) call sequential_end()
!      !endif
!     endif

      if (ichoix.eq.10) then
!*******************************************************************************
! subsave_oa   
!*******************************************************************************
       !if (ifl_oa_out.eq.1.or.(ifl_oa_out.eq.2.and.par%rank.eq.0)) then
        !if (ifl_oa_out.eq.1) call sequential_begin()
        open(unit=io_unit,file=trim(file_hist),position='append')   
        write(io_unit,*) '*** sauvegarde de la fenetre n°',i1_h,'***'
        close(io_unit)
        !if (ifl_oa_out.eq.1) call sequential_end()
        !endif
      endif

      if (ichoix.eq.11) then
!*******************************************************************************
! upd_init_oa   
!*******************************************************************************
       !if (ifl_oa_out.eq.1.or.(ifl_oa_out.eq.2.and.par%rank.eq.0)) then
        !if (ifl_oa_out.eq.1) call sequential_begin()
        open(unit=io_unit,file=trim(file_hist),position='append')   
        write(io_unit,*)
        write(io_unit,*) '*************************************'
        write(io_unit,*) ' allocation dynamique des variables 2d/3d'
        write(io_unit,*) 'nzupd2d_oa,nzupd3d_oa:',nzupd2d_oa,nzupd3d_oa
        write(io_unit,*) '*************************************'
        write(io_unit,*)
        close (io_unit)
        !if (ifl_oa_out.eq.1) call sequential_end()
       !endif
      endif


      if (ichoix.eq.12) then
!*******************************************************************************
! var_upd_oa      
!*******************************************************************************
!STDALONE ichoix==12 NOT FOUND
       !if (ifl_oa_out.eq.1.or.(ifl_oa_out.eq.2.and.par%rank.eq.0)) then
       !if (ifl_oa_out.eq.1) call sequential_begin()
        open(unit=io_unit,file=trim(file_hist),position='append')   
        write(io_unit,*) 'initialisation des variables mises a jour'
        close(io_unit)
        !if (ifl_oa_out.eq.1) call sequential_end()
       !endif
      endif

      if (ichoix.eq.13) then
!*******************************************************************************
! var_upd_oa      
!*******************************************************************************
!STDALONE ichoix==13 NOT FOUND
       !if (ifl_oa_out.eq.1.or.(ifl_oa_out.eq.2.and.par%rank.eq.0)) then
        !if (ifl_oa_out.eq.1) call sequential_begin()
        open(unit=io_unit,file=trim(file_hist),position='append')   
        write(io_unit,*) 'mise a jour des variables specifiques it=',i1_h,tallocated_oa(i1_h)
        close(io_unit)
        !if (ifl_oa_out.eq.1) call sequential_end()
       !endif
      endif

      if (ichoix.eq.14) then
!*******************************************************************************
! var_upd_oa      
!*******************************************************************************
       !if (ifl_oa_out.eq.1.or.(ifl_oa_out.eq.2.and.par%rank.eq.0)) then
        !if (ifl_oa_out.eq.1) call sequential_begin()
        open(unit=io_unit,file=trim(file_hist),position='append')   
        write(io_unit,*) 'mise a jour des variables specifiques it=',i1_h,tallocated_oa(i1_h),tupd_oa(i2_h),tgv3d_oa(tv_oa(i2_h))
        close(io_unit)
        !if (ifl_oa_out.eq.1) call sequential_end()
       !endif
      endif

!STDALONE energie_oaavelet removed      if (ichoix.eq.15) then
!*******************************************************************************
! var_upd_oa      
!*******************************************************************************
!      if (ifl_oa_out.eq.1.or.(ifl_oa_out.eq.2.and.par%rank.eq.0)) then
!       if (ifl_oa_out.eq.1) call sequential_begin()
!       open(unit=io_unit,file=trim(dir_history_exp)//trim(file_hist)//'_'//dom_c//'.out',position='append')    
!       write (io_unit,*) 'appel de la routine energie_oaavelet: ','ic=',i1_h
!       close (io_unit)
!       if (ifl_oa_out.eq.1) call sequential_end()
!      endif
!      endif

!STDALONE energie_oaavelet removed      if (ichoix.eq.16) then
!*******************************************************************************
! var_upd_oa   
!*******************************************************************************
!      if (ifl_oa_out.eq.1.or.(ifl_oa_out.eq.2.and.par%rank.eq.0)) then
!       if (ifl_oa_out.eq.1) call sequential_begin()
!       open(unit=io_unit,file=trim(dir_history_exp)//trim(file_hist)//'_'//dom_c//'.out',position='append')    
!       write (io_unit,*) 'appel de la routine energie_oaavelet: ' ,'ic=',i1_h
!       close (io_unit)
!       if (ifl_oa_out.eq.1) call sequential_end()
!      endif
!      endif

       return

       end subroutine history_oa

!------------------------------------------------------------------------------
! PROCEDURE
!------------------------------------------------------------------------------
!
!> @note
!
! DESCRIPTION: 
!
!> @brief temprary array needed to store online the composite variable value.
!!
!> @details Pointers will point to this any3d_oa for composite variable.
!
! REVISION HISTORY:
!
!> @authors
!! - Benedicte Lemieux-Dudon
!> @date 2015 January
!> @todo
!! - check if allocation(var3d(ii)%prt(:,:,:)) and initialization could replace any3d_oa.
!! - check when deallocate any3d_oa
!------------------------------------------------------------------------------

    subroutine allocate_anyv3d_oa
    
    use module_parameter_oa , only  : imax, jmax, kmax  ! ATTENTION: FRANCIS

    implicit none
    integer :: err

    ! check case where n2d, n3d is zero
    if (.not. allocated(anyv3d_oa)) then
    allocate(anyv3d_oa(0:imax+1,0:jmax+1,0:kmax+1),stat=err)
    if (err /= 0) then
        write(unit=*,fmt=*)"allocation error" ; stop 4
    end if
    end if

    end subroutine allocate_anyv3d_oa

    subroutine deallocate_anyv3d_oa
    if (allocated(anyv3d_oa)) deallocate(anyv3d_oa)
    end subroutine deallocate_anyv3d_oa


end module module_interface_oa

#else
      module module_interface_oa_empty
      end module
#endif
