#include "cppdefs.h"
#ifdef NBQ

      subroutine consout_nbq(icall)

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
!             - NBQ_CONSOUT   : several ASCII output to follow conservation...
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

#if defined NBQ_CONSOUT 
      real :: sum_c,sum2_c,sum3_c,sum4_c,sum5_c,sum6_c
      real :: hsum_c

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


#ifdef NBQ_CONSOUT
      if (icall.eq.10) then
!
!*******************************************************************
!......Ascii output (Internal mode 1)
!*******************************************************************
!         
         sum_c=0.
         do j=jstrq_nh,jendq_nh
         do i=istrq_nh,iendq_nh
            sum_c=sum_c+Zt_avg1(i,j)-h(i,j)
         enddo
         enddo 
         open(unit=10,file='conservation_rhoint0.dat',access='append')
            write(10,*) sum_c
         close(10)

      elseif (icall.eq.11) then
!
!*******************************************************************
!......Ascii output (Internal mode 2)
!*******************************************************************
!         
      sum_c=0.
      sum2_c=0.
      sum3_c=0.
      sum4_c=0.
      sum5_c=0.
      sum6_c=0.
       
      do j=jstrq_nh,jendq_nh
      do i=istrq_nh,iendq_nh
      do k=1,N,+1
           if (j.eq.1) then
              sum_c=sum_c+hzr(i,j,k)-h(i,j) / float(N)
              sum2_c=sum2_c+hz(i,j,k)-h(i,j) / float(N)
           endif
           if (j.eq.1.and.i.eq.4) then
              sum3_c=sum3_c+hz(i,j,k)-Zt_avg1(i,j) / float(N)
           endif
           if (j.eq.1.and.i.eq.4) then
              sum4_c=sum4_c+rhobar_nbq_avg1(i,j)*hzr(i,j,k)  &
                     -Zt_avg1(i,j)/float(N)
           endif
      enddo
      enddo
      enddo

      do j=jstrq_nh,jendq_nh
      do i=istrq_nh,iendq_nh
          do k=1,N,+1
           if (j.eq.1.and.i.eq.4) then
             sum5_c=sum5_c+hz(i,j,k)-Zt_avg1(i,j) / float(N)
           endif
           if (j.eq.1) then
             sum6_c=sum6_c+hz(i,j,k)-h(i,j) / float(N)
           endif
          enddo
         enddo
      enddo

      open(unit=10,file='conservation_rhoint1.dat',access='append')
           write(10,*) sum_c
      close(10)
      open(unit=10,file='conservation_rhoint2.dat',access='append')
           write(10,*) sum2_c
      close(10)
      open(unit=10,file='conservation_rhoint3.dat',access='append')
           write(10,*) sum3_c
      close(10)
      open(unit=10,file='conservation_rhoint4.dat',access='append')
           write(10,*) sum4_c
      close(10)
      open(unit=10,file='conservation_rhoint5.dat',access='append')
           write(10,*) sum5_c
      close(10)
      open(unit=10,file='conservation_rhoint6.dat',access='append')
           write(10,*) sum6_c
      close(10)


!      open(unit=10,file='zw.dat')
!      i=4
!      j=1
!      do k=0,N
!         write(10,*) z_w(i,j,k)
!      enddo
!      close(10)

!      open(unit=10,file='hzr.dat')
!      i=4
!      j=1
!      do k=1,N
!         write(10,*) hzr(i,j,k)
!      enddo
!      close(10)

!      open(unit=10,file='hz.dat')
!      i=4
!      j=1
!      do k=1,N
!         write(10,*) hz(i,j,k)
!      enddo
!      close(10)

      elseif (icall.eq.20) then
!
!*******************************************************************
!......Ascii output (External mode 1 )
!*******************************************************************
!         
      sum_c=0.
      do j=jstrq_nh,jendq_nh
      do i=istrq_nh,iendq_nh
         sum_c=sum_c+Zt_avg1(i,j)-h(i,j)
      enddo
      enddo 

      open(unit=10,file='conservation_rhoext3.dat',access='append')
           write(10,*) sum_c   !,Zt_avg1(4,1)
      close(10)

      elseif (icall.eq.40) then
!
!*******************************************************************
!......Ascii outut : Conservation RhoNBQ
!*******************************************************************
!         
        sum_c=0.
        do l_nbq = 1 , neqcont_nh
          i     = l2iq_nh (l_nbq)
          j     = l2jq_nh (l_nbq)
          k     = l2kq_nh (l_nbq)
#ifdef NBQ_CONS7
          sum_c = sum_c + rhp_nbq_a(l_nbq,rnnew_nbq)
#else
          sum_c = sum_c + rhp_nbq_a(l_nbq,rnnew_nbq)*Hzr_half_nbq(i,j,k)
#endif
        enddo
         open(unit=10,file='conservation_rhonbq.dat',access='append')
            write(10,*) sum_c
         close(10)

      elseif (icall.eq.41) then
!
!*******************************************************************
!......Ascii outut : Conservation RhoBQ
!*******************************************************************
!         
        sum_c  = 0.
        hsum_c = 0.

        do l_nbq = 1 , neqcont_nh
           i=l2iq_nh(l_nbq)
           j=l2jq_nh(l_nbq)
           k=l2kq_nh(l_nbq)
#ifdef NBQ_CONS7
           sum_c  = sum_c  +  rhp_bq_a(l_nbq,2)
#else
           sum_c  = sum_c  +  rhp_bq_a(l_nbq,2)*Hzr_half_nbq(i,j,k)
#endif
           hsum_c = hsum_c + Hzr_half_nbq(i,j,k)
        enddo

       open(unit=10,file='conservation_rhobq.dat',access='append')
           write(10,*) sum_c / hsum_c
       close(10)

      elseif (icall.eq.200) then
!
!*******************************************************************
!...... Outputs profils in ASCII (1)
!*******************************************************************
!
        open(unit=10,file='profA_4.dat')
         i=4
         j=1
         do k=1,N
         write(10,*) dzdt2_nbq(i,j,k),wz(i,j,k,nrhs),We(i,j,k),We3(i,j,k)   &          
                      /(pm(i,j)*pn(i,j))
        enddo
        close(10)

        open(unit=10,file='profA_25.dat')
         i=25
         j=1
         do k=1,N
          write(10,*) dzdt2_nbq(i,j,k),wz(i,j,k,nrhs),We(i,j,k),We3(i,j,k)  &          
                      /(pm(i,j)*pn(i,j))
         enddo
        close(10)

        open(unit=10,file='dzdtA.dat',access='append')
         i=4
         j=1
         k=N
         write(10,*) dzdt_nbq(i,j,k),dzdt2_nbq(i,j,k)
        close(10)
        open(unit=10,file='dzdt_test.dat',access='append')
         i=4
         j=1
         k=N
         write(10,*)  wz(i,j,N,nrhs)                                             &   
         ,        - u2(i  ,j,N,nrhs) * 0.5 * pm_u(i  ,j) * (z_w(i  ,j,N)-z_w(i-1,j,N))    &   
          ,       - u2(i+1,j,N,nrhs) * 0.5 * pm_u(i+1,j) * (z_w(i+1,j,N)-z_w(i  ,j,N))
        close(10)
 
        open(unit=10,file='dzdt2_test.dat',access='append')
         i=4
         j=1
         k=N
         write(10,*) We2(i,j,N)*pm(i,j)*pn(i,j)  &
                          , - (z_w(i,j,N)-z_w(i,j,0)),drhobardt(i,j)
        close(10)


        l_nbq=ijk2lq_nh(4,1,N)
        open(unit=10,file='rho.dat',access='append')
         write(10,*) rhobar_nbq(4,1,knew)-1,rhobar_nbq_avg1(4,1)-1.,rhp_nbq_a(l_nbq,2)/rho0
        close(10)


        sum_c  = 0.
        sum2_c = 0.
        sum3_c = 0.
        sum4_c = 0.
        sum5_c = 0.
        isum_c=0
        hsum_c =0.

        do j=jstrq_nh,jendq_nh
        do i=istrq_nh,iendq_nh

           sum3_c=sum3_c+rhobar_nbq_avg1(i,j)-1.
           sum5_c=sum5_c+rhobar_nbq(i,j,knew)-1.
           isum_c=isum_c+1
           do k=1,N
              l_nbq=ijk2lq_nh(i,j,k)
#ifdef NBQ_CONS7
              sum4_c=sum4_c+rhp_nbq_a(l_nbq,2)/rho0
#else
              sum4_c=sum4_c+rhp_nbq_a(l_nbq,2)/rho0*Hzr_half_nbq(i,j,k)
#endif
              hsum_c=hsum_c+Hzr_half_nbq(i,j,k)
           enddo

           k=N
           isum2_c=isum2_c+1
           sum_c=sum_c  +dzdt_nbq(i,j,k)
           sum2_c=sum2_c+dzdt2_nbq(i,j,k)
        
        enddo
        enddo

        open(unit=10,file='dzdtAm.dat',access='append')
         write(10,*) sum_c/float(isum2_c),sum2_c/float(isum2_c)
        close(10)

        open(unit=10,file='rhom.dat',access='append')
         write(10,*) sum5_c/float(isum_c),sum3_c/float(isum_c),sum4_c/hsum_c
        close(10)

      elseif (icall.eq.201) then
!
!*******************************************************************
!...... Outputs profils in ASCII (2)
!*******************************************************************
!
         return
        open(unit=10,file='profB_4.dat')
         i=4
         j=1
         do k=1,N
          write(10,*) dzdt2_nbq(i,j,k),wz(i,j,k,nrhs),We(i,j,k),We3(i,j,k) &          
                       /(pm(i,j)*pn(i,j))
         enddo
        close(10)

        open(unit=10,file='profB_25.dat')
         i=25
         j=1
         do k=1,N
          write(10,*) dzdt2_nbq(i,j,k),wz(i,j,k,nrhs),We(i,j,k),We3(i,j,k) &          
                      /(pm(i,j)*pn(i,j))
         enddo
        close(10)

        open(unit=10,file='dzdtB.dat',access='append')
         i=4
         j=1
         k=N
         write(10,*) dzdt_nbq(i,j,k),dzdt2_nbq(i,j,k)
        close(10)

      elseif (icall.eq.202) then
!
!*******************************************************************
!...... Outputs profils in ASCII (3)
!*******************************************************************
!
         return
        open(unit=10,file='profC_4.dat')
         i=4
         j=1
         do k=1,N
          write(10,*) dzdt2_nbq(i,j,k),wz(i,j,k,nrhs),We(i,j,k),We3(i,j,k) &          
                      /(pm(i,j)*pn(i,j))
         enddo
        close(10)

        open(unit=10,file='profC_25.dat')
         i=25
         j=1
         do k=1,N
          write(10,*) dzdt2_nbq(i,j,k),wz(i,j,k,nrhs),We(i,j,k),We3(i,j,k) &          
                      /(pm(i,j)*pn(i,j))
         enddo
        close(10)

        open(unit=10,file='dzdtC.dat',access='append')
         i=4
         j=1
         k=N
         write(10,*) dzdt_nbq(i,j,k),dzdt2_nbq(i,j,k)
        close(10)

      endif  ! icall
#endif

        return
        end
#else
        subroutine consout_nbq_empty
        return
        end
#endif
