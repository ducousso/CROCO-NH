#include "cppdefs.h"
#ifdef NBQ

      subroutine mat_mom_nh 
!*******************************************************************
!*******************************************************************
!*******************************************************************
!        Matrix MOM for momentum equations: computation 
!
!          updated at every internal mode time step
!
!*******************************************************************
!*******************************************************************
!*******************************************************************

      use module_nh
      use module_nbq
# ifdef TRACETXT
      use module_tracetxt_out
# endif
      implicit none
# include "param_F90.h"
# include "scalars_F90.h"
# include "grid.h"
# include "ocean3d.h"
# include "nbq.h"

      integer          :: i,j,k
      double precision :: a_m,b_m

!*******************************************************************
!     Momentum Equation: X-direction
!******************************************************************* 

#ifdef OBC_WEST
!-------------------------------------------------------------------
!     Boundary condition (istru_nh-1):
!-------------------------------------------------------------------
      do l_nh=nequ_nh(1)+1,nequ_nh(2)
         i = l2imom_nh(l_nh)
         j = l2jmom_nh(l_nh)
         k = l2kmom_nh(l_nh)
         l1_nh = momi_nh   (l_nh) 

!-----------------------------
!........point p(i,j,k):  
!-----------------------------
#ifdef NBQ_CONS7
         momv_nh(l1_nh) = -(0.5*(Hzr_half_nbq(i-1,j,k)+           &
                                     Hzr_half_nbq(i,j,k)))        &
                              / cw_int_nbq / dt / 2.D0            &
                          / Hzr_half_nbq(i,j,k)
#else
         momv_nh(l1_nh) = -(0.5*(Hzr_half_nbq(i-1,j,k)+           &
                                     Hzr_half_nbq(i,j,k)))        &
                              / cw_int_nbq / dt / 2.D0
#endif
         l1_nh = l1_nh + mijk2lq_nh(i,j,k) 
      enddo
#endif

!-------------------------------------------------------------------
!     Inner domain, bottom layer: (i,j,k=1)
!-------------------------------------------------------------------
      do l_nh=nequ_nh(2)+1,nequ_nh(3)
!.......caracteristiques de l'equation:
        i     = l2imom_nh (l_nh)
        j     = l2jmom_nh (l_nh)
        k     = l2kmom_nh (l_nh)
        l1_nh = momi_nh   (l_nh) 

!-----------------------------
!.......precalculs:
!-----------------------------
        a_m  = - (0.5*(Hzr_half_nbq(i-1,j,k)+Hzr_half_nbq(i,j,k)))   &
                                                       * pm_u(i,j)  
        b_m  =  gdepth_u(i,j,k) 

!-----------------------------
!.......point p(i,j,k+1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = b_m * coefb_u(i,j,k)          &
                       / Hzr_half_nbq(i,j,k+1)
#else
        momv_nh(l1_nh) = b_m * coefb_u(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j,k+1)

!-----------------------------
!.......point p(i,j,k):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = ( a_m  - b_m * coefb_u(i,j,k))         &
                       / Hzr_half_nbq(i,j,k)
#else
        momv_nh(l1_nh) =  a_m  - b_m * coefb_u(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j,k)

!-----------------------------
!.......point p(i,j,k-1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = - b_m * coefa_u(i,j,k)         &
                       / Hzr_half_nbq(i,j,k-1)
#else
        momv_nh(l1_nh) = - b_m * coefa_u(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j,k-1)

!-----------------------------
!.......point p(i-1,j,k+1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = b_m * coefb_u(i-1,j,k)      &
                       / Hzr_half_nbq(i-1,j,k+1)
#else
        momv_nh(l1_nh) = b_m * coefb_u(i-1,j,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i-1,j,k+1)

!-----------------------------
!.......point p(i-1,j,k):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = (- a_m  - b_m * coefb_u(i-1,j,k))      &
                       / Hzr_half_nbq(i-1,j,k)
#else
        momv_nh(l1_nh) = - a_m  - b_m * coefb_u(i-1,j,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i-1,j,k)

!-----------------------------
!.......point p(i-1,j,k-1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = - b_m * coefa_u(i-1,j,k)      &
                       / Hzr_half_nbq(i-1,j,k-1)
#else
        momv_nh(l1_nh) = - b_m * coefa_u(i-1,j,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i-1,j,k-1)
      enddo

!-------------------------------------------------------------------
!     Inner domain, inner layers: (i,j, 1<k<N )
!-------------------------------------------------------------------
      do l_nh=nequ_nh(3)+1,nequ_nh(4)
!.......caracteristiques de l'equation:
        i     = l2imom_nh (l_nh)
        j     = l2jmom_nh (l_nh)
        k     = l2kmom_nh (l_nh)
        l1_nh = momi_nh   (l_nh)

!-----------------------------
!.......precalculs:
!-----------------------------
        a_m  = - (0.5*(Hzr_half_nbq(i-1,j,k)+Hzr_half_nbq(i,j,k)))    &
                                                       * pm_u(i,j)  
        b_m  =  gdepth_u(i,j,k) 

!-----------------------------
!.......point p(i,j,k+1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = b_m * coefb_u(i,j,k)     &
                       / Hzr_half_nbq(i,j,k+1)
#else
        momv_nh(l1_nh) = b_m * coefb_u(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j,k+1)

!-----------------------------
!.......point p(i,j,k):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = (  a_m                                    &
                             + b_m * (coefa_u(i,j,k)-coefb_u(i,j,k)))     &
                       / Hzr_half_nbq(i,j,k)
#else
        momv_nh(l1_nh) =   a_m                                    &
                             + b_m * (coefa_u(i,j,k)-coefb_u(i,j,k))
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j,k)

!-----------------------------
!.......point p(i,j,k-1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = - b_m * coefa_u(i,j,k)           &
                       / Hzr_half_nbq(i,j,k-1)
#else
        momv_nh(l1_nh) = - b_m * coefa_u(i,j,k)      
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j,k-1)

!-----------------------------
!.......point p(i-1,j,k+1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = b_m * coefb_u(i-1,j,k)     &
                       / Hzr_half_nbq(i-1,j,k+1)
#else
        momv_nh(l1_nh) = b_m * coefb_u(i-1,j,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i-1,j,k+1)

!-----------------------------
!.......point p(i-1,j,k):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = (- a_m  + b_m *                          &
                            (coefa_u(i-1,j,k) - coefb_u(i-1,j,k)) )    &
                       / Hzr_half_nbq(i-1,j,k)
#else
        momv_nh(l1_nh) = - a_m  + b_m *                          &
                            (coefa_u(i-1,j,k) - coefb_u(i-1,j,k))
#endif
        l1_nh = l1_nh + mijk2lq_nh(i-1,j,k)

!-----------------------------
!.......point p(i-1,j,k-1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = - b_m * coefa_u(i-1,j,k)     &
                       / Hzr_half_nbq(i-1,j,k-1)
#else
        momv_nh(l1_nh) = - b_m * coefa_u(i-1,j,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i-1,j,k-1)
      enddo

!-------------------------------------------------------------------
!     Inner domain, surface layer: (i,j,k=N )
!-------------------------------------------------------------------
      do l_nh=nequ_nh(4)+1,nequ_nh(5)
!.......caracteristiques de l'equation:
        i     = l2imom_nh (l_nh)
        j     = l2jmom_nh (l_nh)
        k     = l2kmom_nh (l_nh)
        l1_nh = momi_nh   (l_nh) 

!-----------------------------
!.......precalculs:
!-----------------------------
        a_m  = - (0.5*(Hzr_half_nbq(i-1,j,k)+Hzr_half_nbq(i,j,k)))     &
                                                       * pm_u(i,j)  
        b_m  =  gdepth_u(i,j,k) 

!-----------------------------
!.......point p(i,j,k+1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = b_m * coefb_u(i,j,k)     &
                       / Hzr_half_nbq(i,j,k+1)
#else
        momv_nh(l1_nh) = b_m * coefb_u(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j,k+1)

!-----------------------------
!.......point p(i,j,k):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = ( a_m +                                           &
                              b_m * coefa_u(i,j,k)                         &
                              - gdepth_u(i,j,k+1) * coefb_u(i,j,k+1) )     &
                       / Hzr_half_nbq(i,j,k)
#else
        momv_nh(l1_nh) =  a_m +                                      &
                              b_m * coefa_u(i,j,k)                   &
                              - gdepth_u(i,j,k+1) * coefb_u(i,j,k+1) 
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j,k)

!-----------------------------
!.......point p(i,j,k-1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = - b_m * coefa_u(i,j,k)     &
                       / Hzr_half_nbq(i,j,k-1)
#else
        momv_nh(l1_nh) = - b_m * coefa_u(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j,k-1)

!-----------------------------
!.......point p(i-1,j,k+1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = b_m * coefb_u(i-1,j,k)     &
                       / Hzr_half_nbq(i-1,j,k+1)
#else
        momv_nh(l1_nh) = b_m * coefb_u(i-1,j,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i-1,j,k+1)

!-----------------------------
!.......point p(i-1,j,k):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) =( - a_m                                      &
                             + b_m * coefa_u(i-1,j,k)               &
                             - gdepth_u(i,j,k+1) * coefb_u(i-1,j,k+1) )    &
                       / Hzr_half_nbq(i-1,j,k)
#else
        momv_nh(l1_nh) = - a_m                                      &
                             + b_m * coefa_u(i-1,j,k)               &
                             - gdepth_u(i,j,k+1) * coefb_u(i-1,j,k+1)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i-1,j,k)

!-----------------------------
!.......point p(i-1,j,k-1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = - b_m * coefa_u(i-1,j,k)
#else
        momv_nh(l1_nh) = - b_m * coefa_u(i-1,j,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i-1,j,k-1)
      enddo

#ifdef OBC_EAST
!-------------------------------------------------------------------
!.....Boundary condition (iendu_nh+1):
!-------------------------------------------------------------------
      do l_nh=nequ_nh(5)+1,nequ_nh(6)
         i = l2imom_nh(l_nh)
         j = l2jmom_nh(l_nh)
         k = l2kmom_nh(l_nh)
         l1_nh = momi_nh(l_nh)

!-----------------------------
!.......point p(i-1,j,k):  
!-----------------------------
#ifdef NBQ_CONS7
         momv_nh(l1_nh) = (0.5*(Hzr_half_nbq(i-1,j,k)+             &
                                     Hzr_half_nbq(i,j,k)))         &
                               / cw_int_nbq / dt / 2.D0     &
                       / Hzr_half_nbq(i-1,j,k)
#else
         momv_nh(l1_nh) = (0.5*(Hzr_half_nbq(i-1,j,k)+             &
                                     Hzr_half_nbq(i,j,k)))         &
                               / cw_int_nbq / dt / 2.D0
#endif
         l1_nh = l1_nh + mijk2lq_nh(i-1,j,k) 
      enddo
#endif

!*******************************************************************
!     Momentum Equation: Y-direction
!******************************************************************* 

#ifdef OBC_SOUTH
!-------------------------------------------------------------------
!     Boundary condition (jstrv_nh-1):
!-------------------------------------------------------------------
      do l_nh=neqv_nh(1)+1,neqv_nh(2)
         i = l2imom_nh(l_nh)
         j = l2jmom_nh(l_nh)
         k = l2kmom_nh(l_nh)
         l1_nh = momi_nh(l_nh)
 
!-----------------------------
!........point p(i,j,k):
!-----------------------------
#ifdef NBQ_CONS7
         momv_nh(l1_nh) = -(0.5*(Hzr_half_nbq(i,j-1,k)+             &
                                     Hzr_half_nbq(i,j,k)))          &
                               / cw_int_nbq / dt / 2.D0        &
                       / Hzr_half_nbq(i,j,k)
#else
         momv_nh(l1_nh) = -(0.5*(Hzr_half_nbq(i,j-1,k)+             &
                                     Hzr_half_nbq(i,j,k)))          &
                               / cw_int_nbq / dt / 2.D0  
#endif
         l1_nh = l1_nh + mijk2lq_nh(i,j,k)
      enddo
#endif

!-------------------------------------------------------------------
!     Inner domain, bottom layer: (i,j,k=1)
!-------------------------------------------------------------------
      do l_nh=neqv_nh(2)+1,neqv_nh(3)
!.......caracteristiques de l'equation:
        i     = l2imom_nh (l_nh)
        j     = l2jmom_nh (l_nh)
        k     = l2kmom_nh (l_nh)
        l1_nh = momi_nh   (l_nh)

!-----------------------------
!.......precalculs:
!-----------------------------
        a_m  = - (0.5*(Hzr_half_nbq(i,j-1,k)+Hzr_half_nbq(i,j,k)))  &
               * pn_v(i,j) 
        b_m  =  gdepth_v(i,j,k) 

!-----------------------------
!.......point p(i,j,k+1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) =  b_m * coefb_v(i,j,k)      &
                       / Hzr_half_nbq(i,j,k+1)
#else
        momv_nh(l1_nh) =  b_m * coefb_v(i,j,k) 
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j,k+1)

!-----------------------------
!.......point p(i,j,k):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = ( a_m - b_m * coefb_v(i,j,k))     &
                       / Hzr_half_nbq(i,j,k)
#else
        momv_nh(l1_nh) =  a_m - b_m * coefb_v(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j,k)

!-----------------------------
!.......point p(i,j,k-1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = - b_m * coefa_v(i,j,k)     &
                       / Hzr_half_nbq(i,j,k-1)
#else
        momv_nh(l1_nh) = - b_m * coefa_v(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j,k-1)

!-----------------------------
!.......point p(i,j-1,k+1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = b_m * coefb_v(i,j-1,k)     &
                       / Hzr_half_nbq(i,j-1,k+1)
#else
        momv_nh(l1_nh) = b_m * coefb_v(i,j-1,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j-1,k+1)

!-----------------------------
!.......point p(i,j-1,k):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = (- a_m - b_m * coefb_v(i,j-1,k))     &
                       / Hzr_half_nbq(i,j-1,k)
#else
        momv_nh(l1_nh) = - a_m - b_m * coefb_v(i,j-1,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j-1,k)

!-----------------------------
!.......point p(i,j-1,k-1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = - b_m * coefa_v(i,j-1,k)     &
                       / Hzr_half_nbq(i,j-1,k-1)
#else
        momv_nh(l1_nh) = - b_m * coefa_v(i,j-1,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j-1,k-1)
      enddo

!-------------------------------------------------------------------
!     Inner domain, inner layers: (i,j, 1<k<N )
!-------------------------------------------------------------------
      do l_nh=neqv_nh(3)+1,neqv_nh(4)
!.......caracteristiques de l'equation:
        i     = l2imom_nh (l_nh)
        j     = l2jmom_nh (l_nh)
        k     = l2kmom_nh (l_nh)
        l1_nh = momi_nh   (l_nh) 

!-----------------------------
!.......precalculs:
!-----------------------------
        a_m  = - (0.5*(Hzr_half_nbq(i,j-1,k)+Hzr_half_nbq(i,j,k)))  &
               * pn_v(i,j) 
        b_m  =  gdepth_v(i,j,k) 

!-----------------------------
!.......point p(i,j,k+1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) =  b_m * coefb_v(i,j,k)      &
                       / Hzr_half_nbq(i,j,k+1)
#else
        momv_nh(l1_nh) =  b_m * coefb_v(i,j,k) 
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j,k+1)

!-----------------------------
!.......point p(i,j,k):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) =  (a_m  + b_m                                &
                              * ( coefa_v(i,j,k) - coefb_v(i,j,k)))     &
                       / Hzr_half_nbq(i,j,k)
#else
        momv_nh(l1_nh) =  a_m  + b_m                                &
                              * ( coefa_v(i,j,k) - coefb_v(i,j,k))
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j,k)

!-----------------------------
!.......point p(i,j,k-1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = - b_m * coefa_v(i,j,k)     &
                       / Hzr_half_nbq(i,j,k-1)
#else
        momv_nh(l1_nh) = - b_m * coefa_v(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j,k-1)

!-----------------------------
!.......point p(i,j-1,k+1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = b_m * coefb_v(i,j-1,k)     &
                       / Hzr_half_nbq(i,j-1,k+1)
#else
        momv_nh(l1_nh) = b_m * coefb_v(i,j-1,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j-1,k+1)

!-----------------------------
!.......point p(i,j-1,k):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = (- a_m + b_m                                &
                            * ( coefa_v(i,j-1,k) - coefb_v(i,j-1,k)))     &
                       / Hzr_half_nbq(i,j-1,k)
#else
        momv_nh(l1_nh) = - a_m + b_m                                &
                            * ( coefa_v(i,j-1,k) - coefb_v(i,j-1,k))
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j-1,k)

!-----------------------------
!.......point p(i,j-1,k-1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = - b_m * coefa_v(i,j-1,k)     &
                       / Hzr_half_nbq(i,j-1,k-1)
#else
        momv_nh(l1_nh) = - b_m * coefa_v(i,j-1,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j-1,k-1)

      enddo

!-------------------------------------------------------------------
!     Inner domain, surface layer: (i,j,k=N )
!-------------------------------------------------------------------
      do l_nh=neqv_nh(4)+1,neqv_nh(5)
!.......caracteristiques de l'equation:
        i     = l2imom_nh (l_nh)
        j     = l2jmom_nh (l_nh)
        k     = l2kmom_nh (l_nh)
        l1_nh = momi_nh   (l_nh)

!-----------------------------
!.......precalculs:
!-----------------------------
        a_m  = - (0.5*(Hzr_half_nbq(i,j-1,k)+Hzr_half_nbq(i,j,k)))  &
               * pn_v(i,j) 
        b_m  =  gdepth_v(i,j,k) 

!-----------------------------
!.......point p(i,j,k+1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) =  b_m * coefb_v(i,j,k)      &
                       / Hzr_half_nbq(i,j,k+1)
#else
        momv_nh(l1_nh) =  b_m * coefb_v(i,j,k) 
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j,k+1)

!-----------------------------
!.......point p(i,j,k):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = ( a_m + b_m * coefa_v(i,j,k)                &
                              - gdepth_v(i,j,k+1) * coefb_v(i,j,k+1))      &
                       / Hzr_half_nbq(i,j,k)
#else
        momv_nh(l1_nh) =  a_m + b_m * coefa_v(i,j,k)                &
                              - gdepth_v(i,j,k+1) * coefb_v(i,j,k+1) 
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j,k)

!-----------------------------
!.......point p(i,j,k-1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = - b_m * coefa_v(i,j,k)     &
                       / Hzr_half_nbq(i,j,k-1)
#else
        momv_nh(l1_nh) = - b_m * coefa_v(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j,k-1)

!-----------------------------
!.......point p(i,j-1,k+1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = b_m * coefb_v(i,j-1,k)     &
                       / Hzr_half_nbq(i,j-1,k+1)
#else
        momv_nh(l1_nh) = b_m * coefb_v(i,j-1,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j-1,k+1)

!-----------------------------
!.......point p(i,j-1,k):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = (- a_m + b_m * coefa_v(i,j-1,k)            &
                             - gdepth_v(i,j,k+1) * coefb_v(i,j-1,k+1) )    &
                       / Hzr_half_nbq(i,j-1,k)
#else
        momv_nh(l1_nh) = - a_m + b_m * coefa_v(i,j-1,k)            &
                             - gdepth_v(i,j,k+1) * coefb_v(i,j-1,k+1)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j-1,k)

!-----------------------------
!.......point p(i,j-1,k-1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh(l1_nh) = - b_m * coefa_v(i,j-1,k)     &
                       / Hzr_half_nbq(i,j-1,k-1)
#else
        momv_nh(l1_nh) = - b_m * coefa_v(i,j-1,k)
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j-1,k-1)

      enddo

#ifdef OBC_NORTH
!-------------------------------------------------------------------
!.....Boundary condition (jendv_nh+1):
!-------------------------------------------------------------------
      do l_nh=neqv_nh(5)+1,neqv_nh(6)
         i = l2imom_nh(l_nh)
         j = l2jmom_nh(l_nh)
         k = l2kmom_nh(l_nh)
         l1_nh = momi_nh   (l_nh)

!-----------------------------
!........point p(i,j-1,k):
!-----------------------------
#ifdef NBQ_CONS7
         momv_nh(l1_nh) = (0.5*(Hzr_half_nbq(i,j-1,k)+            &
                                    Hzr_half_nbq(i,j,k)))         &
                              / cw_int_nbq / dt / 2.D0            &
                       / Hzr_half_nbq(i,j-1,k)
#else
         momv_nh(l1_nh) = (0.5*(Hzr_half_nbq(i,j-1,k)+            &
                                    Hzr_half_nbq(i,j,k)))         &
                              / cw_int_nbq / dt / 2.D0
#endif
         l1_nh = l1_nh + mijk2lq_nh(i,j-1,k) 
      enddo
#endif

      if (ifl_nbq.eq.1) then

      momvg_nh(1:l1_nh) = momv_nh(1:l1_nh)

!*******************************************************************
! Momentum Equation: Z-direction
!******************************************************************* 


!-------------------------------------------------------------------
!     Inner domain, inner and surface layers: (i,j, 0=<k<=N )
!-------------------------------------------------------------------
      do l_nh = neqw_nh(2)+1,neqw_nh(5)

!.......caracteristiques de l'equation:
        i     = l2imom_nh (l_nh)
        j     = l2jmom_nh (l_nh)
        k     = l2kmom_nh (l_nh)
        l1_nh = momi_nh   (l_nh) 

!-----------------------------
!.......point p(i,j,k+1):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh (l1_nh) =  -1. * float(mijk2lq_nh(i,j,k+1))    &
                               * float(mijk2lq_nh(i,j,k))      &
                       / Hzr_half_nbq(i,j,k+1)
        momvg_nh(l1_nh) = ( -1.                                &
           -  Hzw_half_nbq(i,j,k)*g/soundspeed_nbq**2 )     &
              * float(mijk2lq_nh(i,j,k+1))                     &
              * float(mijk2lq_nh(i,j,k))     &
                       / Hzr_half_nbq(i,j,k+1)
#else
        momv_nh (l1_nh) =  -1. * float(mijk2lq_nh(i,j,k+1))    &
                               * float(mijk2lq_nh(i,j,k))
        momvg_nh(l1_nh) = ( -1.                                &
           -  Hzw_half_nbq(i,j,k)*g/soundspeed_nbq**2 )     &
              * float(mijk2lq_nh(i,j,k+1))                     &
              * float(mijk2lq_nh(i,j,k))
#endif
                             
        l1_nh = l1_nh + mijk2lq_nh(i,j,k+1)                    &
                * float(mijk2lq_nh(i,j,k))

!-----------------------------
!.......point p(i,j,k):
!-----------------------------
#ifdef NBQ_CONS7
        momv_nh (l1_nh) =  1. * float(mijk2lq_nh(i,j,k))      &
                       / Hzr_half_nbq(i,j,k)
        momvg_nh(l1_nh) = ( 1.                                &
            - Hzw_half_nbq(i,j,k)*g/soundspeed_nbq**2 )    &
              * float(mijk2lq_nh(i,j,k))                      &
                       / Hzr_half_nbq(i,j,k)
#else
        momv_nh (l1_nh) =  1. * float(mijk2lq_nh(i,j,k)) 
        momvg_nh(l1_nh) = ( 1.                                &
            - Hzw_half_nbq(i,j,k)*g/soundspeed_nbq**2 )    &
              * float(mijk2lq_nh(i,j,k))
#endif
        l1_nh = l1_nh + mijk2lq_nh(i,j,k)  

      enddo

      if (ifl_imp_nbq.eq.1) then
!*******************************************************************
!.......Partie implicite:
!*******************************************************************

!-------------------------------------------------------------------
!     Inner domain, all layers: (i,j,k=0)
!-------------------------------------------------------------------
       
        do l_nh=neqw_nh(2)+1,neqw_nh(5)

           i = l2imom_nh(l_nh)
           j = l2jmom_nh(l_nh)
           k = l2kmom_nh(l_nh)      
           l1_nh = mimpi_nbq(l_nh-neqmom_nh(1)-neqmom_nh(2)) 

!-----------------------------
!........point p(i,j,k+1):
!-----------------------------
#ifdef NBQ_CONS7
           mimpv_nbq(l1_nh)   = - ( soundspeed_nbq**2 * dtnbq - visc2_nbq ) &
                                / Hzr_half_nbq(i,j,k+1)

           stop 'mat_mom_nh'
#else
           mimpv_nbq(l1_nh)   = - ( soundspeed_nbq**2 * dtnbq  - visc2_nbq ) &
                               * float(mijk2lq_nh(i,j,k+1))                  &
                               * float(mijk2lq_nh(i,j,k))

#endif
           l1_nh = l1_nh + mijk2lq_nh(i,j,k+1)                   &
                * float(mijk2lq_nh(i,j,k))

!-----------------------------
!........point p(i,j,k):
!-----------------------------
#ifdef NBQ_CONS7
           mimpv_nbq(l1_nh)   =   ( soundspeed_nbq**2 * dtnbq - visc2_nbq )  &
                       / Hzr_half_nbq(i,j,k)
#else
           mimpv_nbq(l1_nh)   =   ( soundspeed_nbq**2 * dtnbq - visc2_nbq ) &
                                      * float(mijk2lq_nh(i,j,k)) 
#endif

      enddo
      endif

#ifdef NBQ_CONS0
!*******************************************************************
!---->Correction derivee temporelle de rho:
!*******************************************************************

#ifdef NBQ_CONS7
      do l_nh=1,neqcont_nh
       i = l2iq_nh (l_nh)
       j = l2jq_nh (l_nh)
       k = l2kq_nh (l_nh)
       l1_nh = corri_nh(l_nh)

       if (k.lt.N.and.k.gt.1) then
!........Point rh(i,j,k+1)
         corrv_nh(l1_nh) =0.5 *  dzdt_nbq(i,j,k+1) &
                          / Hzr_half_nbq(i,j,k+1)
         l1_nh = l1_nh + mijk2lq_nh(i,j,k+1)
!........Point rh(i,j,k)
         corrv_nh(l1_nh) = 0.5 * ( dzdt_nbq(i,j,k) -  dzdt_nbq(i,j,k) ) &
                          / Hzr_half_nbq(i,j,k)
         l1_nh = l1_nh + mijk2lq_nh(i,j,k)
!........Point rh(i,j,k-1)
         corrv_nh(l1_nh) = - 0.5 *  dzdt_nbq(i,j,k-1) &
                          / Hzr_half_nbq(i,j,k-1)
         l1_nh = l1_nh + mijk2lq_nh(i,j,k-1)
        elseif (k.eq.N) then
!........Point rh(i,j,k)
         corrv_nh(l1_nh) = (  0. *dzdt_nbq(i,j,k) &   ! Surface BC  ==> added in  rh_nbq
                         - 0.5 * dzdt_nbq(i,j,k) )   &
                          / Hzr_half_nbq(i,j,k)
         l1_nh = l1_nh + mijk2lq_nh(i,j,k)
!........Point rh(i,j,k-1)
         corrv_nh(l1_nh) = - 0.5 *  dzdt_nbq(i,j,k-1) &
                          / Hzr_half_nbq(i,j,k-1)
         l1_nh = l1_nh + mijk2lq_nh(i,j,k-1)
       else
!........Point rh(i,j,k+1)
         corrv_nh(l1_nh) = 0.5 *  dzdt_nbq(i,j,k+1) &
                          / Hzr_half_nbq(i,j,k+1)
         l1_nh = l1_nh + mijk2lq_nh(i,j,k+1)
!........Point rh(i,j,k)
         corrv_nh(l1_nh) = 0.5 * dzdt_nbq(i,j,k)  &
                          / Hzr_half_nbq(i,j,k)
         l1_nh = l1_nh + mijk2lq_nh(i,j,k)
       endif


      enddo

#else

      do l_nh=1,neqcont_nh
       i = l2iq_nh (l_nh)
       j = l2jq_nh (l_nh)
       k = l2kq_nh (l_nh)
       l1_nh = corri_nh(l_nh)

       if (k.lt.N.and.k.gt.1) then
!........Point rh(i,j,k+1)
         corrv_nh(l1_nh) =0.5 *  dzdt_nbq(i,j,k) &
                          / Hzr_half_nbq(i,j,k)
         l1_nh = l1_nh + mijk2lq_nh(i,j,k+1)
!........Point rh(i,j,k)
         corrv_nh(l1_nh) = 0.5 * ( dzdt_nbq(i,j,k) -  dzdt_nbq(i,j,k) ) &
                          / Hzr_half_nbq(i,j,k)
         l1_nh = l1_nh + mijk2lq_nh(i,j,k)
!........Point rh(i,j,k-1)
         corrv_nh(l1_nh) = - 0.5 *  dzdt_nbq(i,j,k) &
                          / Hzr_half_nbq(i,j,k)
         l1_nh = l1_nh + mijk2lq_nh(i,j,k-1)
        elseif (k.eq.N) then
!........Point rh(i,j,k)
         corrv_nh(l1_nh) = (  0. *dzdt_nbq(i,j,k) &   ! Surface BC  ==> added in  rh_nbq
                         - 0.5 * dzdt_nbq(i,j,k) )   &
                          / Hzr_half_nbq(i,j,k)
         l1_nh = l1_nh + mijk2lq_nh(i,j,k)
!........Point rh(i,j,k-1)
         corrv_nh(l1_nh) = - 0.5 *  dzdt_nbq(i,j,k) &
                          / Hzr_half_nbq(i,j,k)
         l1_nh = l1_nh + mijk2lq_nh(i,j,k-1)
       else
!........Point rh(i,j,k+1)
         corrv_nh(l1_nh) = 0.5 *  dzdt_nbq(i,j,k) &
                          / Hzr_half_nbq(i,j,k)
         l1_nh = l1_nh + mijk2lq_nh(i,j,k+1)
!........Point rh(i,j,k)
         corrv_nh(l1_nh) = 0.5 * dzdt_nbq(i,j,k)  &
                          / Hzr_half_nbq(i,j,k)
         l1_nh = l1_nh + mijk2lq_nh(i,j,k)
       endif


      enddo
#endif

#endif

      endif

      return
      end subroutine mat_mom_nh
#else
        subroutine mat_mom_nh_empty
        return
        end 
#endif
