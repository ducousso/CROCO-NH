#include "cppdefs.h"
#ifdef NBQ

      subroutine grid_coef_nh

!**********************************************************************
!
!                 Pre-computations for NH / NBQ modes
!
!**********************************************************************

      use module_nh
      use module_nbq
# ifdef MPI
      use module_parallel_nbq, only : mynode,ierr,par,OUEST,EST
# endif
      implicit none

# include "param_F90.h"
# include "grid.h"
# include "ocean3d.h"
# include "nbq.h"

#include "def_bounds.h"


      integer :: i,j,k,it
      double precision :: val1, val2

!**********************************************************************
!    Initialisations and updates
!**********************************************************************
        do k=1,N
          do j=jstr_nh ,jend_nh
          do i=istru_nh,iendu_nh
              gdepth_u(i,j,k) = zr_half_nbq(i,j,k)-zr_half_nbq(i-1,j,k)
              coefa_u(i,j,k)  = 0.25*pm_u(i,j)*                        &
                     (Hzr_half_nbq(i,j,k  )+Hzr_half_nbq(i-1,j,k ))/  &
                     (Hzw_half_nbq(i,j,k-1)+Hzw_half_nbq(i-1,j,k-1))
              coefb_u(i,j,k)  = 0.25*pm_u(i,j)*                        &
                     (Hzr_half_nbq(i,j,k  )+Hzr_half_nbq(i-1,j,k ))/   &
                     (Hzw_half_nbq(i,j,k  )+Hzw_half_nbq(i-1,j,k  ))
          enddo
          enddo

          do j=jstrv_nh,jendv_nh
          do i=istr_nh ,iend_nh
              gdepth_v(i,j,k) = zr_half_nbq(i,j,k)-zr_half_nbq(i ,j-1,k)
              coefa_v(i,j,k)  = 0.25*pn_v(i,j)*                        &
                     (Hzr_half_nbq(i,j,k  )+Hzr_half_nbq(i,j-1,k ))/  &
                     (Hzw_half_nbq(i,j,k-1)+Hzw_half_nbq(i,j-1,k-1))
              coefb_v(i,j,k)  = 0.25*pn_v(i,j)*                        &
                     (Hzr_half_nbq(i,j,k  )+Hzr_half_nbq(i,j-1,k ))/  &
                     (Hzw_half_nbq(i,j,k  )+Hzw_half_nbq(i,j-1,k  ))
          enddo
          enddo
        enddo 
        
        do j = jstr_nh ,jend_nh
        do i = istru_nh,iendu_nh
            gdepth_u(i,j,0)   = zw_half_nbq(i,j,0)-zw_half_nbq(i-1,j,0)
            gdepth_u(i,j,N+1) = zw_half_nbq(i,j,N)-zw_half_nbq(i-1,j,N)
            coefa_u(i,j,0)    = 0.5 * pm_u(i,j) * real (slip_nbq)  
            coefa_u(i,j,1)    = 0. 
            coefb_u(i,j,N)    = 0.     
            coefb_u(i,j,N+1)  = 0.5 * pm_u(i,j)  
        enddo
        enddo

        do j = jstrv_nh,jendv_nh
        do i = istr_nh ,iend_nh
            gdepth_v(i,j,0)   = zw_half_nbq(i,j,0)-zw_half_nbq(i,j-1,0)
            gdepth_v(i,j,N+1) = zw_half_nbq(i,j,N)-zw_half_nbq(i,j-1,N)
            coefa_v(i,j,0)    = 0.5 * pn_v(i,j) * real (slip_nbq) 
            coefa_v(i,j,1)    = 0.                    
            coefb_v(i,j,N)    = 0.        
            coefb_v(i,j,N+1)  = 0.5 * pn_v(i,j)  
        enddo
        enddo


!**********************************************************************
!
! MPI NEEDS:
!
! gdepth_u, coefa_u and coefb_u: 
!                we need here ( istru_nh-1,[jstr_nh:jend_nh],[0:N+1] )
!                             ( iendu_nh+1,[jstr_nh:jend_nh],[0:N+1] )               
! gdepth_v, coefa_v and coefb_v: 
!                we need here ( [istr_nh:iend_nh],jstrv_nh-1,[0:N+1] )
!                             ( [istr_nh:iend_nh],jendv_nh+1,[0:N+1] ) 
!  
!**********************************************************************
        
     call grid_exchange()
        
      return
      end subroutine grid_coef_nh

#else
      subroutine grid_coef_nh_empty
      return
      end 
#endif


