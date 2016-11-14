#include "cppdefs.h"
#ifdef NBQ

      subroutine density_nbq(icall)

!======================================================================
!
!                      Various Computations related to
!                            NBQ density
!
!> @note Main web documentation at:
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm
!
! DESCRIPTION: 
!
!> @brief Density anomaly driver 
!
!> @details 
! - icall=0 vertical averaging density to get the external 
!           mode density (rhpbar_nbq_t).
! - icall=1 internal mode density (rhpio_nbq_t).
! - icall=5 computes the NH pressure gradient for the internal mode.
! - icall=6 computes the divergence of the momentum (div_nbq_a) 
!      + used in the momentum equation (second viscosity term with 
!        gradient operator)
!      + used in the continuity equation
! - icall=7 time incrementation of NBQ mode density anomaly.
!
! REVISION HISTORY:
!
!> @authors
!> @date 2015 January
!> @todo
!
!======================================================================
!
!      use module_principal , only : kount0, iteration3d, rhp_t, rho, mask_t, &
!			    dz_t, iteration2d_max_now, hz_w, iteration2d,    &
!			    imax, jmax, kmax
!     use module_parallele                ! #MPI#
      use module_nh                       ! #NH#
      use module_nbq                      ! #NBQ#
      implicit none
      integer :: icall, i,j ,k, k1,indm_d

# include "param_F90.h"
# include "scalars_F90.h"
# include "work.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "nbq.h"

      integer :: ncp
      real    :: dist_d

      if (icall.eq.2) then
!
!**********************************************************************
!  Transfer density field to i,j,k array 
!  and time filter, ready for external mode
!**********************************************************************
!
        rhobar_nbq(istrq_nh-1:iendq_nh+1,jstrq_nh-1:jendq_nh+1,knew)=0.
        work2d    (:,:     )=0.

#ifdef NBQ_CONS7
        do l_nbq = 1 , neqcont_nh
          i     = l2iq_nh (l_nbq)
          j     = l2jq_nh (l_nbq)
          k     = l2kq_nh (l_nbq)
          rho_nbq_ext(i,j,k)  = 0.5*(rhp_nbq_a(l_nbq,rnstp_nbq)                &
                                    +rhp_nbq_a(l_nbq,rnrhs_nbq)                &
                                    +2.*rho(i,j,k)                             &
                                    )                                          &
                              / Hzr_half_nbq(i,j,k)
          work2d(i,j)         = work2d(i,j)+Hzr_half_nbq(i,j,k)
          rhobar_nbq(i,j,knew)= rhobar_nbq(i,j,knew)                           &
                              + 0.5*(rhp_nbq_a(l_nbq,rnstp_nbq)                &
                                    +rhp_nbq_a(l_nbq,rnrhs_nbq)                &
                                    +2.*rho(i,j,k)                             &
                                    )         
        enddo
#else
        do l_nbq = 1 , neqcont_nh
          i     = l2iq_nh (l_nbq)
          j     = l2jq_nh (l_nbq)
          k     = l2kq_nh (l_nbq)
          rho_nbq_ext(i,j,k)  = 0.5*(rhp_nbq_a(l_nbq,rnstp_nbq)                &
                                    +rhp_nbq_a(l_nbq,rnrhs_nbq)                &
                                    +2.*rho(i,j,k)                             &
                                    )
          work2d(i,j)         = work2d(i,j)+Hzr_half_nbq(i,j,k)
          rhobar_nbq(i,j,knew)= rhobar_nbq(i,j,knew)                           &
                              + rho_nbq_ext(i,j,k)                             &
                                *Hzr_half_nbq(i,j,k)
        enddo
#endif
!       rhobar_nbq = 0.
!       rho_nbq_ext = 0.
 
!
!.......Rho0 added subsequently for added precision 
!
        rho_nbq_ext(:,:,:) = (rho_nbq_ext(:,:,:) + rho0) / rho0
        do j=jstrq_nh-1,jendq_nh+1
        do i=istrq_nh-1,iendq_nh+1
           rhobar_nbq(i,j,knew) = (rhobar_nbq(i,j,knew)/work2d(i,j) + rho0) / rho0
        enddo
        enddo
!       rhobar_nbq = 1.
!       rho_nbq_ext = 1.

#ifdef NBQ_CONSOUT
        call consout_nbq(40)
#endif

      elseif (icall.eq.4) then
!
!**********************************************************************
! Internal Mode Density Storage: NOT USED ANYMORE
!**********************************************************************
!
!.......Termes de l'equation de continuite: RHS(cont)

        return
      
#ifdef NBQ_CONS6
        if (iif.eq.1) then
           rhp_bq_a(:,1)=rhp_bq_a(:,2)
           do l_nbq = 1 , neqcont_nh
              i=l2iq_nh(l_nbq)
              j=l2jq_nh(l_nbq)
              k=l2kq_nh(l_nbq)
#ifdef NBQ_CONS7
              rhp_bq_a(l_nbq,2)=rho(i,j,k)*Hzr_half_nbq(i,j,k)
#else
              rhp_bq_a(l_nbq,2)=rho(i,j,k)
#endif
!             rhp_nbq_a(l_nbq,rnnew_nbq)= rhp_nbq_a(l_nbq,rnnew_nbq) &
!                      + (- rhp_bq_a(l_nbq,1) + rhp_bq_a(l_nbq,2)) / ndtfast
!             rhp_nbq_a(l_nbq,rnrhs_nbq)= rhp_nbq_a(l_nbq,rnrhs_nbq) &
!                      + (- rhp_bq_a(l_nbq,1) + rhp_bq_a(l_nbq,2)) / ndtfast
           enddo
        else ! iif > 1
           do l_nbq = 1 , neqcont_nh
              i=l2iq_nh(l_nbq)
              j=l2jq_nh(l_nbq)
              k=l2kq_nh(l_nbq)
!             rhp_nbq_a(l_nbq,rnnew_nbq)= rhp_nbq_a(l_nbq,rnnew_nbq) &
!                         + (- rhp_bq_a(l_nbq,1) + rhp_bq_a(l_nbq,2)) / ndtfast
!             rhp_nbq_a(l_nbq,rnrhs_nbq)= rhp_nbq_a(l_nbq,rnrhs_nbq) &
!                         + (- rhp_bq_a(l_nbq,1) + rhp_bq_a(l_nbq,2)) / ndtfast
           enddo
        endif
#else
        if (iif.eq.1) then
          rhp_bq_a(:,1)=rhp_bq_a(:,2)
          do l_nbq = 1 , neqcont_nh
             i=l2iq_nh(l_nbq)
             j=l2jq_nh(l_nbq)
             k=l2kq_nh(l_nbq)
#ifdef NBQ_CONS7
!            rhp_bq_a(l_nbq,2)=rho(i,j,k)*Hzr_half_nbq(i,j,k)
             stop 'density_nbq'
#else
             rhp_bq_a(l_nbq,2)=rho(i,j,k)
!            rhp_nbq_a(l_nbq,rnnew_nbq)= rhp_nbq_a(l_nbq,rnnew_nbq) &
!                     - rhp_bq_a(l_nbq,1) + rhp_bq_a(l_nbq,2)
!            rhp_nbq_a(l_nbq,rnrhs_nbq)= rhp_nbq_a(l_nbq,rnrhs_nbq) &
!                     - rhp_bq_a(l_nbq,1) + rhp_bq_a(l_nbq,2)
!            rhp_nbq_a(l_nbq,rnstp_nbq)= rhp_nbq_a(l_nbq,rnstp_nbq) &
!                     - rhp_bq_a(l_nbq,1) + rhp_bq_a(l_nbq,2)
#endif
          enddo
        endif
#endif
 

#ifdef NBQ_CONSOUT
        call consout_nbq(41)
#endif
      elseif (icall.eq.6) then
!
!**********************************************************************
!     Calcul de la divergence
!**********************************************************************
!
            call amux(                                                &
                  neqcont_nh                                          &
                 ,qdm_nbq_a(1,vnrhs_nbq)                              &
                 ,div_nbq_a(1,dnrhs_nbq)                              &
                 ,contv_nh (1)                                        &
                 ,contj_nh (1)                                        &
                 ,conti_nh (1)                                        &
                       )

      elseif (icall.eq.7) then
!
!*******************************************************************
!......Move forward: Masse
!*******************************************************************
!          
         ncp       = rnnew_nbq
         rnnew_nbq = rnstp_nbq
         rnstp_nbq = rnrhs_nbq
         rnrhs_nbq = ncp

         ncp       = dnrhs_nbq
         dnrhs_nbq = dnstp_nbq
         dnstp_nbq = ncp

# ifdef ACOUS
      elseif (icall.eq.10) then
!
!*******************************************************************
!......Acoustic waves: Initialization
!*******************************************************************
          period_exp = 0.025/2.
          for_a_exp  = 2.5
          amp_exp = 1.e-3
          hmax_exp = 128.
          dg_exp = 2.

      elseif (icall.eq.11) then
!
!*******************************************************************
!......Acoustic waves: Forcing
!*******************************************************************
!
        time_nbq = time_nbq + 0.5*dtnbq

        do l_nbq = 1 , neqcont_nh 

          i=l2iq_nh(l_nbq)
          j=l2jq_nh(l_nbq)
          k=l2kq_nh(l_nbq)

          dist_d=sqrt((xr(i,j)-xl/2.)**2+(0.*(yr(i,j)-el/2.))**2       &
                              +(abs(z_r(i,j,k))-hmax_exp/2.)**2)
!         if (dist_d.le.for_a_exp(1)) then
             div_nbq_a(l_nbq,dnrhs_nbq) = div_nbq_a(l_nbq,dnrhs_nbq)   &
                        +amp_exp*sin(2*acos(-1.)*time_nbq/period_exp)  &
                                        *exp(-dist_d**2/for_a_exp**2)
!         endif
        enddo
!         write(6,*) 'ACOUS',div_nbq_a(10,dnrhs_nbq)
# endif /* ACOUS */

      endif  ! icall

        return
        end
#else
        subroutine density_nbq_empty
        return
        end
#endif
