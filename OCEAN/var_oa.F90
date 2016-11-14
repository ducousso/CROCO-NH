#include "cppdefs.h"
#ifdef ONLINE_ANALYSIS

!------------------------------------------------------------------------------
! PROCEDURE
!------------------------------------------------------------------------------
!
!> @note
!
! DESCRIPTION: 
!
!> @brief Fonction donnant la valeur de la variable choisie
!!     en un point donne.
!
!> @details Attention lors de l'ajout d'une variable penser completer
!!     var_grid_oa pour specifier le type de point sur lequel
!!     se trouve la nouvelle variable.
!
! REVISION HISTORY:
!
!> @authors
!! - Francis Auclair, Jochem Floor
!!  - Initial version and algorithm.
!! - Benedicte Lemieux-Dudon
!!  - stand alone version with a greatly reduced amont of tested code variable.
!!  - newvar_oa implements a pointer version of var_oa which may is more efficient
!!    in the case of large set of variable code.
!!    Code test can be time consuming since they are applied inside the time, period and space loops in main_oa.
!> @date 2015 January
!> @todo Benedicte Lemieux-Dudon
!! - apply a selective use of module variables. 
!------------------------------------------------------------------------------

      real function var_oa(                                           &
            ivar_v                                                    &
           ,cnb_v                                                     &
           ,i_v                                                       &
           ,j_v                                                       &
           ,k_v                                                       &
           ,lv_v                                                      &
           ,ls1_v                                              ) 

      use module_oa_variables
      use module_oa_time
      use module_oa_space
      use module_oa_periode
      use module_oa_stock
      use module_oa_level
!     use module_oa_upd
!     use module_nrj

      implicit none

# include "param_F90.h"
# include "scalars_F90.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "grid.h"
# include "nbq.h"

      integer, intent(in) ::                                          &
            cnb_v                                                     &
           ,ivar_v                                                    &
           ,i_v                                                       &
           ,j_v                                                       &
           ,k_v                                                       &
           ,lv_v                                                      &
           ,ls1_v

!*********************************************************************
! Calling code "simple" variables
!*********************************************************************
      if (ivar_v.eq.1) then
         var_oa = u(i_v,j_v,k_v,nstp)-ubar(i_v,j_v,fast_indx_out)
      elseif (ivar_v.eq.2) then
         var_oa = v(i_v,j_v,k_v,nstp)-vbar(i_v,j_v,fast_indx_out)
#  ifdef NBQ
      elseif (ivar_v.eq.3) then
         var_oa = wz(i_v,j_v,k_v,nstp) 
#  endif
      elseif (ivar_v.eq.20) then
         var_oa = wlev_oa(v2lev_oa(lv_v))%z(ls1_v)

!*********************************************************************
! OA test variable
!*********************************************************************
      elseif (ivar_v.eq.99) then
         var_oa = vardp_test_oa(i_v,j_v,k_v)

!*********************************************************************
! Calling code "mixed" variables (composite)
!*********************************************************************
      elseif (ivar_v.eq.100) then
!........sum of the squared vel_u and vel_v at t-grid : 
!        "kinetic energy without rho factor" just to provide and example to handle "mixed" variables
!        var_oa = ( ( vel_u(i_v,j_v,k_v) + vel_u(i_v+1,j_v,k_v) )**2 + ( vel_v(i_v,j_v,k_v) + vel_v(i_v,j_v+1,k_v) )**2 ) / 8.0
      endif

      return
      end function var_oa

#endif
