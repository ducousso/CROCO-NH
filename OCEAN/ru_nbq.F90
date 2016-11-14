#include "cppdefs.h"
#ifdef NBQ

      subroutine ru_nbq(icall)

!**********************************************************************
!
!                      Various Computations related to
!                            NBQ momentum
!
!> @note <a href="http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm"> Main web documentation </a>
!
! DESCRIPTION: 
!
!> @brief NBQ momentum routine.
!
! REVISION HISTORY:
!
!> @authors
!> @date 2015 January
!> @todo
!
!**********************************************************************

      use module_nh 
      use module_nbq
      implicit none

# include "param_F90.h"
# include "scalars_F90.h"
# include "grid.h"
# include "ocean3d.h"
# include "nbq.h"

      integer :: i,j,k
      integer :: icall,ncp
      real    :: cff


      if (icall.eq.1) then
!
!*******************************************************************
!  Fill external and internal forcing terms
!*******************************************************************
!
        do l_nbq = nequ_nh(1)+1,nequ_nh(6)
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
           dqdmdt_nbq_a(l_nbq)=rho0*(ruint_nbq(i,j,k)+ruext_nbq(i,j,k))
        enddo

        do l_nbq = neqv_nh(1)+1,neqv_nh(6)  
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
           dqdmdt_nbq_a(l_nbq)=rho0*(rvint_nbq(i,j,k)+rvext_nbq(i,j,k))
        enddo

        do l_nbq = neqw_nh(1)+1,neqw_nh(6)
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
           dqdmdt_nbq_a(l_nbq)=rho0*rwint_nbq(i,j,k)
        enddo
       
      elseif (icall.eq.2) then
!
!*******************************************************************
!  Prepare feedback of NBQ rhs terms to external equations
!*******************************************************************
!
        cff=1/(rho0*real(ndtnbq))
!
        rubar_nbq(:,:)=0.
!       do l_nbq = nequ_nh(1)+1,nequ_nh(6)
        do l_nbq = 1,nequ_nh(7)
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
           ru_nbq_ext(i,j,k) = cff*rhssum_nbq_a(l_nbq)*on_u(i,j)*om_u(i,j)
           rubar_nbq(i,j)    = rubar_nbq(i,j)+ru_nbq_ext(i,j,k)
        enddo

# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_u3d_tile (Istru_nh,Iendu_nh,Jstru_nh,Jendu_nh,  &
                                       ru_nbq_ext(START_2D_ARRAY,1))

      call exchange_u2d_tile (Istru_nh,Iendu_nh,Jstru_nh,Jendu_nh,  &
                        rubar_nbq(START_2D_ARRAY))
# endif
        
        rvbar_nbq(:,:)=0.
        do l_nbq = neqv_nh(1)+1,neqv_nh(6)  
            i=l2imom_nh(l_nbq)
            j=l2jmom_nh(l_nbq)
            k=l2kmom_nh(l_nbq)
            rv_nbq_ext(i,j,k) = cff*rhssum_nbq_a(l_nbq)*on_v(i,j)*om_v(i,j)
            rvbar_nbq(i,j)    = rvbar_nbq(i,j)+rv_nbq_ext(i,j,k)
        enddo

        do l_nbq = neqw_nh(1)+1,neqw_nh(6)
            i = l2imom_nh (l_nbq)
            j = l2jmom_nh (l_nbq)
            k = l2kmom_nh (l_nbq)
            rw_nbq_ext(i,j,k) = cff*rhssum_nbq_a(l_nbq)*on_r(i,j)*om_r(i,j)
        enddo

        rhssum_nbq_a(1:neqmom_nh(0)) = 0.

      elseif (icall.eq.6) then
!
!*******************************************************************
!  Increment momentum and density time derivative sigma correction:
!*******************************************************************
!
        call amux(                                                       &
               neqcorrt_nbq                                              &
              ,rhp_nbq_a(1:neqcont_nh,rnrhs_nbq)                         & 
!             ,rhp_nbq_a(1:neqcont_nh,rnrhs_nbq)-rhp_bq_a(1:neqcont_nh,2)  & 
              ,rhs1_nbq (1)                                              &
              ,momvg_nh (1)                                              &
              ,momj_nh  (1)                                              & 
              ,momi_nh  (1)                                              &
                     ) 

#ifdef NBQ_CONS0
        stop 'ru_nbq'
!.......Computes surface correction:
        call amux(                                                       &
               neqcont_nh                                                &
!             ,rhp_nbq_a (1:neqcont_nh,rnrhs_nbq)-rhp_bq_a(1:neqcont_nh,2) &
              ,rhp_nbq_a (1:neqcont_nh,rnrhs_nbq)                        &
              ,rhs1r_nbq                                                 &
              ,corrv_nh  (1)                                             &
              ,corrj_nh  (1)                                             &
              ,corri_nh  (1)                                             &
                      )
    
!.......Surface boundary condition:
          do j=jstrq_nh,jendq_nh
          do i=istrq_nh,iendq_nh
             l_nbq = ijk2lq_nh(i,j,N)
             rhs1r_nbq (l_nbq) = rhs1r_nbq (l_nbq) &
                         + rhp_bq_a(l_nbq,2)*dzdt_nbq(i,j,N) &
                         / Hzr_half_nbq(i,j,N)
         enddo
         enddo
#endif

      elseif (icall.eq.7) then
!
!*******************************************************************
!  Move forward momentum
!*******************************************************************
!
          ncp       = vnnew_nbq
          vnnew_nbq = vnstp_nbq
          vnstp_nbq = vnrhs_nbq
          vnrhs_nbq = ncp

       endif  ! icall

       return
       end
#else
      subroutine ru_nbq_empty 
      return
      end
#endif

