#include "cppdefs.h"
#ifdef NBQ

      subroutine  z_thickness_nh(ichoix)

!**********************************************************************
!
!                 Pre-computations for NH / NBQ modes
!
!**********************************************************************

      use module_nh
      use module_nbq
      implicit none

# include "param_F90.h"
# include "grid.h"
# include "ocean3d.h"
# include "nbq.h"

      integer :: i,j,k,kmax,imax,jmax
      integer :: ichoix

#ifdef MPI
#define LOCALLM Lmmpi
#define LOCALMM Mmmpi
#else
#define LOCALLM Lm
#define LOCALMM Mm
#endif    

      kmax=N  
      imax=LOCALLM
      jmax=LOCALMM

      if (ichoix.eq.0 .or. ichoix.eq.1) then  !!CXA simplification
!**********************************************************************
!    Initialisations
!**********************************************************************
!CXA !!!!!! aux indices qui debordent dans toutes les directions. 
!CXA      do k=0,kmax+1
        do k=1,kmax
          do j=0,jmax+1
            do i=0,imax+1
              gdepth_u(i,j,k) = zr_half_nbq(i,j,k,2)-zr_half_nbq(i-1,j  ,k,2)
              gdepth_v(i,j,k) = zr_half_nbq(i,j,k,2)-zr_half_nbq(i  ,j-1,k,2)
              coefa_u(i,j,k)  = 0.25*pm_u(i,j)*                        &
                     (Hzr_half_nbq(i,j,k  )+Hzr_half_nbq(i-1,j,k  ))/  &
                     (Hzw_half_nbq(i,j,k-1)+Hzw_half_nbq(i-1,j,k-1))
              coefb_u(i,j,k)  = 0.25*pm_u(i,j)*                        &
                     (Hzr_half_nbq(i,j,k  )+Hzr_half_nbq(i-1,j,k  ))/  &
                     (Hzw_half_nbq(i,j,k  )+Hzw_half_nbq(i-1,j,k  ))
              coefa_v(i,j,k)  = 0.25*pn_v(i,j)*                        &
                     (Hzr_half_nbq(i,j,k  )+Hzr_half_nbq(i-1,j,k  ))/  &
                     (Hzw_half_nbq(i,j,k-1)+Hzw_half_nbq(i-1,j,k-1))
              coefb_u(i,j,k)  = 0.25*pn_v(i,j)*                        &
                     (Hzr_half_nbq(i,j,k  )+Hzr_half_nbq(i-1,j,k  ))/  &
                     (Hzw_half_nbq(i,j,k  )+Hzw_half_nbq(i-1,j,k  ))
            enddo
          enddo
        enddo

        do i = 0 , imax + 1
          do j = 0 , jmax + 1
!CXA        coefa_u(i,j,0)    = 0.5 / dx_u(i,j) * real (slip_exp)   ! BESOIN ???????
!CXA        coefa_v(i,j,0)    = 0.5 / dy_v(i,j) * real (slip_exp)   ! BESOIN ???????
            coefa_u(i,j,0)    = 0.5 * pm_u(i,j) * real (slip_nbq)   ! BESOIN ???????
            coefa_v(i,j,0)    = 0.5 * pn_v(i,j) * real (slip_nbq)   ! BESOIN ???????
            coefa_u(i,j,1)    = 0. 
            coefa_v(i,j,1)    = 0.
            coefb_u(i,j,kmax) = 0.                                 
            coefb_v(i,j,kmax) = 0.                                 
!CXA        coefb_u(i,j,kmax+1)   = 0.5 / dx_u(i,j)  
!CXA        coefb_v(i,j,kmax+1)   = 0.5 / dy_v(i,j)  
            coefb_u(i,j,kmax+1)   = 0.5 * pm_u(i,j)  
            coefb_v(i,j,kmax+1)   = 0.5 * pn_v(i,j)  
          enddo
        enddo

!**********************************************************************
!CXA surface and bottom coefa and b already treated in initial_nh.F90
!CXA we do not touch anything
!**********************************************************************

!**********************************************************************
!CXA treatment of bottom
!**********************************************************************
        k=0
        do j=0,jmax+1
          do i=0,imax+1
            gdepth_u(i,j,k) = zw_half_nbq(i,j,k)-zw_half_nbq(i-1,j  ,k)
            gdepth_v(i,j,k) = zw_half_nbq(i,j,k)-zw_half_nbq(i  ,j-1,k)
          enddo
        enddo

!**********************************************************************
!CXA treatment of surface
!**********************************************************************
        k=N+1
        do j=0,jmax+1
          do i=0,imax+1
            gdepth_u(i,j,k) = zw_half_nbq(i,j,k-1)-zw_half_nbq(i-1,j  ,k-1)
            gdepth_v(i,j,k) = zw_half_nbq(i,j,k-1)-zw_half_nbq(i  ,j-1,k-1)
          enddo
        enddo

      endif ! ichoix
     
      return
      end subroutine z_thickness_nh

#else
      subroutine z_thickness_nh_empty
      return
      end 
#endif


