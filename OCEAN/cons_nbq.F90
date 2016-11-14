#include "cppdefs.h"
#ifdef NBQ

      subroutine cons_nbq(icall)

!======================================================================
!
!                      Various Computations related to
!                            NBQ mass conservation
!
!> @note Main web documentation at:
!
! DESCRIPTION: 
!
!
!> @details 
!
! REVISION HISTORY:
!
!> @authors
!> @date 2015 January
!> @todo
!
!
! Associated CPP keys:
!
!             - NBQ_CONS0 (+): NBQ-mode: dzdt (or dzdt2) term added 
!                                        through rhs1r_nbq & CORR matrix
!                              - either dzdt  from column            (CONS2, omega.F)
!                              - or     dzdt2 from surface kine. cdt (CONS3, cons_nbq.F90) 
!                               dzdt(2) is to be changed in ru_nbq.F90 and mat_mom_nh.F90
!                               dzdt(+) dzdt2(-)
!             - NBQ_CONS1 (-): INT-mode: dHzdt term added in omega.F (not with CONS5)
!                                        based on Hz or Hz_Half.
! 
!             - NBQ_CONS2 (+): INT-mode: dzdt  computed in omega.F      (column)
!                                        (==> diag, CONS0)
!             - NBQ_CONS3 (+): INT-mode: dzdt2 computed in cons_nbq.F90 (surface kine. cdt)
!                                        (==> diag, CONS0)
!
!             - NBQ_CONS4 (-): EXT-mode: dzdt added in step2d.F (for tests only)
!                                        based on surface kine. cdt  (CINE matrix)
!                                        ( also available for INT mode )
!             - NBQ_CONS5 (+): INT-mode: Hz corrected in set_depth.F
!             - NBQ_CONS6 (+): NBQ-mode: rho_bq updated in rho_nbq_a
!
!             - NBQ_CONS7 (-): NBQ-mode: flux form of continuity equation
!
!            ( - NBQ_CONSOUT (+)   : several ASCII output to follow conservation...)
!  
!======================================================================

      use module_nh                       ! #NH#
      use module_nbq                      ! #NBQ#
      implicit none
      integer :: icall, i,j ,k, k1
      integer :: indm_d,isum_c,isum2_c

# include "param_F90.h"
# include "scalars_F90.h"
# include "work.h"
# include "grid.h"
# include "ocean3d.h"
# include "nbq.h"

#if defined NBQ_CONSOUT || defined NBQ_CONS5
      real sum_c,sum2_c,sum3_c,sum4_c,sum5_c,sum6_c

      real Zt_avg1(GLOBAL_2D_ARRAY)
      real DU_avg1(GLOBAL_2D_ARRAY,5)
      real DV_avg1(GLOBAL_2D_ARRAY,5)
      real DU_avg2(GLOBAL_2D_ARRAY)
      real DV_avg2(GLOBAL_2D_ARRAY)
      common /ocean_Zt_avg1/Zt_avg1                   &
          /coup_DU_avg1/DU_avg1 /coup_DV_avg1/DV_avg1 &
          /coup_DU_avg2/DU_avg2 /coup_DV_avg2/DV_avg2

      real zeta_new(GLOBAL_2D_ARRAY),  cff

#endif

      if (icall.eq.8) then
#ifdef NBQ_CONS4   
!
!*******************************************************************
!......Compute rho_surf in NBQ mode through CINE matrix
!*******************************************************************
!          
 
         rhscine_nh(:,1)=rhscine_nh(:,2)
  
         call amux(                                                   &
                  neqcont_nh                                          &
                 ,qdm_nbq_a (1,vnrhs_nbq)                             &
                 ,rhscine_nh(1,2)                                     &
                 ,cinev_nh (1)                                        &
                 ,cinej_nh (1)                                        &
                 ,cinei_nh (1)                                       &
                  )

        k=N
        do j=jstrq_nh,jendq_nh
        do i=istrq_nh,iendq_nh
         l_nbq=ijk2lq_nh(i,j,N)
         rhscine_nh(l_nbq,2)=(rhscine_nh(l_nbq,2)+rhs1r_nbq(l_nbq))
        enddo
        enddo

         if (iif.eq.1.and.iic.eq.1)  rhscine_nh(:,1)=rhscine_nh(:,2)
        
      elseif (icall.eq.9) then
!
!*******************************************************************
!......Compute rho_surf in internal mode through CINE matrix
!*******************************************************************
!         
        do l_nbq = nequ_nh(1)+1,nequ_nh(6)
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
     !     rhscine2_nh(l_nbq)=Huon(i,j,k)/0.2*rho0/10.

           rhscine2_nh(l_nbq)=0.5*(Hz(i,j,k)+Hz(i-1,j,k))*u(i,j,k,nnew)*rho0
        enddo
        
        do l_nbq = neqv_nh(1)+1,neqv_nh(6)  
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
     !     rhscine2_nh(l_nbq)=Hvom(i,j,k)/0.2*rho0/10.
           rhscine2_nh(l_nbq)=0.5*(Hz(i,j,k)+Hz(i,j-1,k))*v(i,j,k,nnew)*rho0
        enddo

        do l_nbq = neqw_nh(1)+1,neqw_nh(6)
            i = l2imom_nh (l_nbq)
            j = l2jmom_nh (l_nbq)
            k = l2kmom_nh (l_nbq)
            if (k.gt.0.and.k.lt.N) then
!           if (k.lt.N) then
            rhscine2_nh(l_nbq)=wz(i,j,k,nnew)    &
                  *0.5*(Hz(i,j,k)+Hz(i,j,k+1))*rho0
!           else
!           rhscine2_nh(l_nbq)=wz(i,j,k,nnew)*0.5*Hz(i,j,k)*rho0
!           endif
            endif
        enddo
 
         rhscine_nh(:,1)=rhscine_nh(:,2)
  
         call amux(                                              &
                  neqcont_nh                                     &
                 ,rhscine2_nh(1)                                 &
                 ,rhscine_nh(1,2)                                &
                 ,cinev_nh (1)                                   &
                 ,cinej_nh (1)                                   &
                 ,cinei_nh (1)                                   &
                  )
      
          do l_nbq=1,neqcont_nh
            
           i=l2iq_nh(l_nbq)
           j=l2jq_nh(l_nbq)
           k=l2kq_nh(l_nbq)
           l_nh=ijk2lq_nh(i,j,N)
           if (k.ne.N) then   
            rhscine_nh(l_nbq,2) = rhscine_nh(l_nh,2)             &  
                                /(z_w(i,j,N)-z_w(i,j,0))         &
                                *(z_w(i,j,k)-z_w(i,j,0))   

            dzdt_nbq(i,j,k)= rhscine_nh(l_nh,2)                  &
                                /rho_nbq_avg1(i,j,N)  /rho0      &  
                                /(z_w(i,j,N)-z_w(i,j,0))         &
                                *(z_w(i,j,k)-z_w(i,j,0))  
                        
            else
       !    rhscine_nh(l_nbq,2) = rhscine_nh(l_nh,2)       
            dzdt_nbq(i,j,k)= rhscine_nh(l_nh,2)                  & 
                                /rho_nbq_avg1(i,j,N)  /rho0
           endif
          
          enddo

         if (iif.eq.1.and.iic.eq.1)  rhscine_nh(:,1)=rhscine_nh(:,2)
#endif

#ifdef NBQ_CONS5
      elseif (icall.eq.30) then
!
!*******************************************************************
!......Corrects Hz in set_depth.F
!*******************************************************************
!         
      h2d = 0.

      do j=jstrq_nh,jendq_nh
      do i=istrq_nh,iendq_nh
      do k=1,N,+1
         h2d(i,j)=h2d(i,j)+hz(i,j,k)
      enddo
      enddo
      enddo

      do j=jstrq_nh,jendq_nh
      do i=istrq_nh,iendq_nh
!        rhobar_nbq_avg1(i,j)=rhobar_nbq_avg1(i,j)*Zt_avg1(i,j)/h2d(i,j)
!        rhobar_nbq(i,j,knew)=rhobar_nbq(i,j,knew)*Zt_avg1(i,j)/h2d(i,j)
         do k=1,N,+1
            Hz (i,j,k) = Hz(i,j,k)*Zt_avg1(i,j)/h2d(i,j)
            Hzr(i,j,k) = Hzr(i,j,k)*Zt_avg1(i,j)/h2d(i,j)
            z_w(i,j,k) = z_w(i,j,k)*Zt_avg1(i,j)/h2d(i,j)
            z_r(i,j,k) = z_r(i,j,k)*Zt_avg1(i,j)/h2d(i,j)
!           rho_nbq_avg1(i,j,k)=rho_nbq_avg1(i,j,k)*Zt_avg1(i,j)/h2d(i,j)
!           rho_nbq_avg2(i,j,k)=rho_nbq_avg2(i,j,k)*Zt_avg1(i,j)/h2d(i,j)
!           rho_nbq_ext(i,j,k)=rho_nbq_ext(i,j,k)*Zt_avg1(i,j)/h2d(i,j)
         enddo
         enddo
      enddo
#endif

#ifdef NBQ_CONS3
      elseif (icall.eq.100) then
!
!*******************************************************************
!...... Computes We with surface kinetic condition (1)
!
!    (wz,u)
!    z_w, z_r
!    Hzr
!*******************************************************************
!
         do i=Istru_nh,Iendu_nh
         do j=Jstru_nh,Jendu_nh
         do k=1,N
            u2(i,j,k,nrhs)=Huon(i,j,k)/(on_u(i,j)*0.5*(Hz(i,j,k)+Hz(i-1,j,k)))
         enddo
         enddo
         enddo
         
!.......Computes dzdt with surface and bottom kinematic condition:

        k=N
        do j=jstrq_nh,jendq_nh
        do i=istrq_nh,iendq_nh

           dzdt2_nbq(i,j,N) = wz(i,j,N,nrhs)                                             &   
                 - u2(i  ,j,N,nrhs) * 0.5 * pm_u(i  ,j) * (z_w(i  ,j,N)-z_w(i-1,j,N))    &   
                 - u2(i+1,j,N,nrhs) * 0.5 * pm_u(i+1,j) * (z_w(i+1,j,N)-z_w(i  ,j,N))  

           We3(i,j,0)=0.
           We3(i,j,N)=0.

        enddo
        enddo  

!.......Computes Omega:
        do k=N-1,1,-1
        do j=jstrq_nh,jendq_nh
        do i=istrq_nh,iendq_nh

           dzdt2_nbq(i,j,k) =  dzdt2_nbq(i,j,N) / (z_w(i,j,N)-z_w(i,j,0)) * (z_w(i,j,k)-z_w(i,j,0))

           We3(i,j,k) = wz(i,j,k,nrhs)                                           &  
                    - u2(i  ,j,k+1,nrhs) * 0.25 * pm_u(i,j)                      &
                    *(Hzr(i,j,k+1)+Hzr(i-1,j,k+1))                               &
                    / (z_r(i,j,k+1)-z_r(i  ,j,k)+z_r(i-1,j,k+1)-z_r(i-1,j,k))    &
                    * (z_r(i,j,k+1)-z_r(i-1,j,k+1))                              &

                    - u2(i+1,j,k+1,nrhs) * 0.25 * pm_u(i+1,j)                    &
                    *(Hzr(i+1,j,k+1)+Hzr(i,j,k+1))                               &
                    / (z_r(i+1,j,k+1)-z_r(i+1,j,k)+z_r(i,j,k+1)-z_r(i,j,k))      &
                    * (z_r(i+1,j,k+1)-z_r(i,j,k+1))                              &

                    - u2(i  ,j,k  ,nrhs) * 0.25 * pm_u(i,j)                      &
                    * (Hzr(i,j,k  )+Hzr(i-1,j,k ))                               &
                    / (z_r(i,j,k+1)-z_r(i  ,j,k)+z_r(i-1,j,k+1)-z_r(i-1,j,k))    &
                    * (z_r(i,j,k  )-z_r(i-1,j,k))                                &

                    - u2(i+1,j,k,nrhs) * 0.25 * pm_u(i+1,j)                      &
                    * (Hzr(i+1,j,k  )+Hzr(i,j,k ))                               &
                    / (z_r(i+1,j,k+1)-z_r(i+1,j,k)+z_r(i,j,k+1)-z_r(i,j,k))      &   
                    * (z_r(i+1,j,k  )-z_r(i  ,j,k))                              &

                    - dzdt2_nbq(i,j,k)
        enddo
        enddo  
        enddo  

      elseif (icall.eq.101) then
!
!*******************************************************************
!...... Computes We with surface kinetic condition (2)
!
!    (wz,u)
!    z_w, z_r
!    Hzr
!*******************************************************************
!
         do i=Istru_nh,Iendu_nh
         do j=Jstru_nh,Jendu_nh
         do k=1,N
            u2(i,j,k,nrhs)=Huon(i,j,k)/(on_u(i,j)*0.5*(Hz(i,j,k)+Hz(i-1,j,k)))
         enddo
         enddo
         enddo

!.......Computes dzdt with surface kinematic condition:
 
        k=N
        do j=jstrq_nh,jendq_nh
        do i=istrq_nh,iendq_nh

           dzdt2_nbq(i,j,N) = wz(i,j,N,nrhs)                                             &   
                  - u2(i  ,j,N,nrhs) * 0.5 * pm_u(i  ,j) * (z_w(i  ,j,N)-z_w(i-1,j,N))   &   
                  - u2(i+1,j,N,nrhs) * 0.5 * pm_u(i+1,j) * (z_w(i+1,j,N)-z_w(i  ,j,N)) 

          We3(i,j,0)=0.
          We3(i,j,N)=0.

        enddo
        enddo  

!.......Computes Omega:
        do k=N-1,1,-1
        do j=jstrq_nh,jendq_nh
        do i=istrq_nh,iendq_nh

           dzdt2_nbq(i,j,k) = dzdt2_nbq(i,j,N) / (z_w(i,j,N)-z_w(i,j,0)) * (z_w(i,j,k)-z_w(i,j,0))

           We3(i,j,k) = wz(i,j,k,nrhs)                                           &  

                    - u2(i  ,j,k+1,nrhs) * 0.25 * pm_u(i,j)                      &
                    *(Hzr(i,j,k+1)+Hzr(i-1,j,k+1))                               &
                    / (z_r(i,j,k+1)-z_r(i  ,j,k)+z_r(i-1,j,k+1)-z_r(i-1,j,k))    &
                    * (z_r(i,j,k+1)-z_r(i-1,j,k+1))                              &

                    - u2(i+1,j,k+1,nrhs) * 0.25 * pm_u(i+1,j)                    &
                    *(Hzr(i+1,j,k+1)+Hzr(i,j,k+1))                               &
                    / (z_r(i+1,j,k+1)-z_r(i+1,j,k)+z_r(i,j,k+1)-z_r(i,j,k))      &
                    * (z_r(i+1,j,k+1)-z_r(i,j,k+1))                              &

                    - u2(i  ,j,k,nrhs) * 0.25 * pm_u(i,j)                        &
                    * (Hzr(i,j,k  )+Hzr(i-1,j,k ))                               &
                    / (z_r(i,j,k+1)-z_r(i  ,j,k)+z_r(i-1,j,k+1)-z_r(i-1,j,k))    &
                    * (z_r(i,j,k  )-z_r(i-1,j,k))                                &

                    - u2(i+1,j,k,nrhs) * 0.25 * pm_u(i+1,j)                      &
                    * (Hzr(i+1,j,k  )+Hz(i,j,k ))                                &
                    / (z_r(i+1,j,k+1)-z_r(i+1,j,k)+z_r(i,j,k+1)-z_r(i,j,k))      &   
                    * (z_r(i+1,j,k  )-z_r(i  ,j,k))                              &

                    - dzdt2_nbq(i,j,k)

        enddo
        enddo  
        enddo  

      elseif (icall.eq.102) then
!
!*******************************************************************
!...... Computes We with surface kinetic condition (3)
!
!    (wz,u)
!    z_w, z_r
!    Hzr
!*******************************************************************
!
         do i=Istru_nh,Iendu_nh
         do j=Jstru_nh,Jendu_nh
         do k=1,N
            u2(i,j,k,nrhs)=Huon(i,j,k)/(on_u(i,j)*0.5*(Hz(i,j,k)+Hz(i-1,j,k)))
         enddo
         enddo
         enddo
         

!.......Computes dzdt with surface kinematic condition:

        k=N
        do j=jstrq_nh,jendq_nh
        do i=istrq_nh,iendq_nh

           dzdt2_nbq(i,j,N) = wz(i,j,N,nrhs)                                             &   
                 - u2(i  ,j,N,nrhs) * 0.5 * pm_u(i  ,j) * (z_w(i  ,j,N)-z_w(i-1,j,N))    &   
                 - u2(i+1,j,N,nrhs) * 0.5 * pm_u(i+1,j) * (z_w(i+1,j,N)-z_w(i  ,j,N))

           We3(i,j,0)=0.
           We3(i,j,N)=0.

        enddo
        enddo  

!.......Computes Omega:
        do k=N-1,1,-1
        do j=jstrq_nh,jendq_nh
        do i=istrq_nh,iendq_nh

           dzdt2_nbq(i,j,k) =  dzdt2_nbq(i,j,N) / (z_w(i,j,N)-z_w(i,j,0)) * (z_w(i,j,k)-z_w(i,j,0))

           We3(i,j,k) = wz(i,j,k,nrhs)                                           &  

                    - u2(i  ,j,k+1,nrhs) * 0.25 * pm_u(i,j)                      &
                    *(Hzr(i,j,k+1)+Hzr(i-1,j,k+1))                               &
                    / (z_r(i,j,k+1)-z_r(i  ,j,k)+z_r(i-1,j,k+1)-z_r(i-1,j,k))    &
                    * (z_r(i,j,k+1)-z_r(i-1,j,k+1))                              &

                    - u2(i+1,j,k+1,nrhs) * 0.25 * pm_u(i+1,j)                    &
                    *(Hzr(i+1,j,k+1)+Hzr(i,j,k+1))                               &
                    / (z_r(i+1,j,k+1)-z_r(i+1,j,k)+z_r(i,j,k+1)-z_r(i,j,k))      &
                    * (z_r(i+1,j,k+1)-z_r(i,j,k+1))                              &

                    - u2(i  ,j,k,nrhs) * 0.25 * pm_u(i,j)                        &
                    * (Hzr(i,j,k  )+Hzr(i-1,j,k ))                               &
                    / (z_r(i,j,k+1)-z_r(i  ,j,k)+z_r(i-1,j,k+1)-z_r(i-1,j,k))    &
                    * (z_r(i,j,k  )-z_r(i-1,j,k))                                &

                    - u2(i+1,j,k,nrhs) * 0.25 * pm_u(i+1,j)                      &
                    * (Hzr(i+1,j,k  )+Hzr(i,j,k ))                               &
                    / (z_r(i+1,j,k+1)-z_r(i+1,j,k)+z_r(i,j,k+1)-z_r(i,j,k))      &   
                    * (z_r(i+1,j,k  )-z_r(i  ,j,k))                              &

                    - dzdt2_nbq(i,j,k) 

        enddo
        enddo  
        enddo  
#endif

#ifdef NBQ_CONS2
      elseif (icall.eq.300) then
!
!*******************************************************************
!......Calculations before "Omega computations" (1)
!*******************************************************************
!
        rhos_nbq_int(:,:) = rho_nbq_avg1(:,:,N)
        rhobar_nbq_int=rhobar_nbq_avg1
        if (iic == 1) rhobar_bak1=rhobar_nbq_avg1
        drhobardt=(rhobar_nbq_avg1-rhobar_bak1)/dt
        rhobar_bak1=rhobar_nbq_avg1

        if (iic == 1) Hz_half_bak=Hz_half
        dHzdt=(Hz_half-Hz_half_bak)/dt
        Hz_half_bak =Hz_half

      elseif (icall.eq.301) then
!
!*******************************************************************
!......Calculations before "Omega computations" (2)
!*******************************************************************
!
        rhos_nbq_int(:,:) = rho_nbq_avg2(:,:,N)
        rhobar_nbq_int=rhobar_nbq_avg1
        if (iic == 1) rhobar_bak2=rhobar_nbq_avg1
        drhobardt=(rhobar_nbq_avg1-rhobar_bak2)/dt
        rhobar_bak2=rhobar_nbq_avg1

        dHzdt=(Hz-Hz_bak)/dt

      elseif (icall.eq.302) then
!
!*******************************************************************
!......Calculations before "Omega computations" (3)
!*******************************************************************
!
        rhos_nbq_int(:,:) = rho_nbq_avg2(:,:,N)
        rhobar_nbq_int=rhobar_nbq_avg1
   !    drhobardt=(rhobar_nbq_avg1-rhobar_bak2)/dt
#endif

      endif  ! icall

        return
        end
#else
        subroutine cons_nbq_empty
        return
        end
#endif
