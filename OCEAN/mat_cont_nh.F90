#include "cppdefs.h"
#ifdef NBQ

      subroutine mat_cont_nh 
!*******************************************************************
!*******************************************************************
!*******************************************************************
!             Matrix CONT for continuity equation:
!
!           Updated at every internal mode time step
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

      integer :: i,j,k

!*******************************************************************
!     Various Initializations:
!*******************************************************************


!*******************************************************************
!     Updates CONT matrix:
!*******************************************************************

!-------------------------------------------------------------------
!     Inner domain, bottom layer: (i,j,1)
!-------------------------------------------------------------------

      do l_nh = neqq_nh(2)+1,neqq_nh(3)

!......Equation characteristics:
       i = l2iq_nh (l_nh) 
       j = l2jq_nh (l_nh) 
       k = l2kq_nh (l_nh)         
       l1_nh = cont_nnz_nh(l_nh)

!-----------------------------
!....... u(i,j,k):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = (- on_u(i,j)*pm(i,j)*pn(i,j)                  &
                          - coefb_u(i,j,k) * gdepth_u(i,j,k)            &
                          / (0.5*( Hzr_half_nbq(i-1,j,k) +              &
                                    Hzr_half_nbq(i,j,k) ) )  )       
#else
        contv_nh(l1_nh) = (- on_u(i,j)*pm(i,j)*pn(i,j)                  &
                          - coefb_u(i,j,k) * gdepth_u(i,j,k)            &
                          / (0.5*( Hzr_half_nbq(i-1,j,k) +              &
                                    Hzr_half_nbq(i,j,k) ) )  )          &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,1)

!-----------------------------
!....... v(i,j,k):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = (- om_v(i,j)*pm(i,j)*pn(i,j)                  &
                          - coefb_v(i,j,k) * gdepth_v(i,j,k)            &
                          / (0.5*(Hzr_half_nbq(i,j-1,k)+                &
                                  Hzr_half_nbq(i,j,k)))    )      
#else
        contv_nh(l1_nh) = (- om_v(i,j)*pm(i,j)*pn(i,j)                  &
                          - coefb_v(i,j,k) * gdepth_v(i,j,k)            &
                          / (0.5*(Hzr_half_nbq(i,j-1,k)+                &
                                  Hzr_half_nbq(i,j,k)))    )            &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,2)

!-----------------------------
!....... u(i,j,k+1):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = - coefa_u(i,j,k+1) * gdepth_u(i,j,k+1)        &
                          / (0.5*(Hzr_half_nbq(i-1,j,k+1)+              &
                                  Hzr_half_nbq(i,j,k+1)))            
#else
        contv_nh(l1_nh) = - coefa_u(i,j,k+1) * gdepth_u(i,j,k+1)        &
                          / (0.5*(Hzr_half_nbq(i-1,j,k+1)+              &
                                  Hzr_half_nbq(i,j,k+1)))               &
                          / Hzr_half_nbq(i,j,k) 
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k+1,1)

!-----------------------------
!....... v(i,j,k+1):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = - coefa_v(i,j,k+1) * gdepth_v(i,j,k+1)        &
                          / (0.5*(Hzr_half_nbq(i,j-1,k+1)+              &
                                  Hzr_half_nbq(i,j,k+1)))     
#else
        contv_nh(l1_nh) = - coefa_v(i,j,k+1) * gdepth_v(i,j,k+1)        &
                          / (0.5*(Hzr_half_nbq(i,j-1,k+1)+              &
                                  Hzr_half_nbq(i,j,k+1)))               &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k+1,2)

!-----------------------------
!....... u(i+1,j,k):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) =  ( on_u(i+1,j)*pm(i,j)*pn(i,j)                &
                          - coefb_u(i+1,j,k) * gdepth_u(i+1,j,k)        &
                          / (0.5*(Hzr_half_nbq(i,j,k)+                  &
                                  Hzr_half_nbq(i+1,j,k)))   )  
#else
        contv_nh(l1_nh) =  ( on_u(i+1,j)*pm(i,j)*pn(i,j)                &
                          - coefb_u(i+1,j,k) * gdepth_u(i+1,j,k)        &
                          / (0.5*(Hzr_half_nbq(i,j,k)+                  &
                                  Hzr_half_nbq(i+1,j,k)))   )           &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i+1,j,k,1)

!-----------------------------
!....... v(i,j+1,k):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) =  ( om_v(i,j+1)*pm(i,j)*pn(i,j)                &
                          - coefb_v(i,j+1,k) * gdepth_v(i,j+1,k)        &
                          / (0.5*(Hzr_half_nbq(i,j,k)+                  &
                                  Hzr_half_nbq(i,j+1,k)))   )      
#else
        contv_nh(l1_nh) =  ( om_v(i,j+1)*pm(i,j)*pn(i,j)                &
                          - coefb_v(i,j+1,k) * gdepth_v(i,j+1,k)        &
                          / (0.5*(Hzr_half_nbq(i,j,k)+                  &
                                  Hzr_half_nbq(i,j+1,k)))   )           &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j+1,k,2)

!-----------------------------
!....... u(i+1,j,k+1):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = - coefa_u(i+1,j,k+1) * gdepth_u(i+1,j,k+1)    &
                          / (0.5*(Hzr_half_nbq(i,j,k+1)+                &
                                  Hzr_half_nbq(i+1,j,k+1)))        
#else
        contv_nh(l1_nh) = - coefa_u(i+1,j,k+1) * gdepth_u(i+1,j,k+1)    &
                          / (0.5*(Hzr_half_nbq(i,j,k+1)+                &
                                  Hzr_half_nbq(i+1,j,k+1)))             &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i+1,j,k+1,1)

!-----------------------------
!....... v(i,j+1,k+1):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = - coefa_v(i,j+1,k+1) * gdepth_v(i,j+1,k+1)    &
                          / (0.5*(Hzr_half_nbq(i,j,k+1)+                &
                                  Hzr_half_nbq(i,j+1,k+1)))     
#else
        contv_nh(l1_nh) = - coefa_v(i,j+1,k+1) * gdepth_v(i,j+1,k+1)    &
                          / (0.5*(Hzr_half_nbq(i,j,k+1)+                &
                                  Hzr_half_nbq(i,j+1,k+1)))             &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j+1,k+1,2) 

!-----------------------------
!....... w(i,j,k):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = 1. / Hzw_half_nbq(i,j,k)                  
#else
        contv_nh(l1_nh) = 1. / Hzw_half_nbq(i,j,k)                      &
                             / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,3)

!-----------------------------
!....... w(i,j,k-1): 
!-----------------------------

       enddo

!-------------------------------------------------------------------
!     Inner domain, all layers: (i,j,k)
!-------------------------------------------------------------------

      do l_nh = neqq_nh(3)+1 , neqq_nh(4)

!......Equation characteristics:
       i = l2iq_nh (l_nh) 
       j = l2jq_nh (l_nh) 
       k = l2kq_nh (l_nh)      
       l1_nh = cont_nnz_nh(l_nh)

!-----------------------------
!....... u(i,j,k):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) =( - on_u(i,j)*pm(i,j)*pn(i,j)                  &
                          - ( coefb_u(i,j,k) - coefa_u(i,j,k) )         &
                          * gdepth_u(i,j,k)                             &
                            / (0.5*(Hzr_half_nbq(i-1,j,k)+              &
                                    Hzr_half_nbq(i,j,k)))      ) 
#else
        contv_nh(l1_nh) =( - on_u(i,j)*pm(i,j)*pn(i,j)                  &
                          - ( coefb_u(i,j,k) - coefa_u(i,j,k) )         &
                          * gdepth_u(i,j,k)                             &
                            / (0.5*(Hzr_half_nbq(i-1,j,k)+              &
                                    Hzr_half_nbq(i,j,k)))      )        &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,1)

!-----------------------------
!....... v(i,j,k):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) =( - om_v(i,j)*pm(i,j)*pn(i,j)                  &
                          - ( coefb_v(i,j,k) - coefa_v(i,j,k) )         &
                          * gdepth_v(i,j,k)                             &
                             / (0.5*(Hzr_half_nbq(i,j-1,k)+             &
                                     Hzr_half_nbq(i,j,k)))   ) 
#else
        contv_nh(l1_nh) =( - om_v(i,j)*pm(i,j)*pn(i,j)                  &
                          - ( coefb_v(i,j,k) - coefa_v(i,j,k) )         &
                          * gdepth_v(i,j,k)                             &
                             / (0.5*(Hzr_half_nbq(i,j-1,k)+             &
                                     Hzr_half_nbq(i,j,k)))   )          &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,2)

!-----------------------------
!....... u(i,j,k+1):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = - coefa_u(i,j,k+1) * gdepth_u(i,j,k+1)        &
                          / (0.5*(Hzr_half_nbq(i-1,j,k+1)+              &
                                  Hzr_half_nbq(i,j,k+1)))   
#else
        contv_nh(l1_nh) = - coefa_u(i,j,k+1) * gdepth_u(i,j,k+1)        &
                          / (0.5*(Hzr_half_nbq(i-1,j,k+1)+              &
                                  Hzr_half_nbq(i,j,k+1)))               &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k+1,1)

!-----------------------------
!....... v(i,j,k+1):
!-----------------------------  
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = - coefa_v(i,j,k+1) * gdepth_v(i,j,k+1)        &
                          / (0.5*(Hzr_half_nbq(i,j-1,k+1)+              &
                                  Hzr_half_nbq(i,j,k+1))) 
#else
        contv_nh(l1_nh) = - coefa_v(i,j,k+1) * gdepth_v(i,j,k+1)        &
                          / (0.5*(Hzr_half_nbq(i,j-1,k+1)+              &
                                  Hzr_half_nbq(i,j,k+1)))               &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k+1,2)

!-----------------------------
!....... u(i,j,k-1):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = + coefb_u(i,j,k-1) * gdepth_u(i,j,k-1)        &
                          / (0.5*(Hzr_half_nbq(i-1,j,k-1)+              &
                                  Hzr_half_nbq(i,j,k-1)))   
#else
        contv_nh(l1_nh) = + coefb_u(i,j,k-1) * gdepth_u(i,j,k-1)        &
                          / (0.5*(Hzr_half_nbq(i-1,j,k-1)+              &
                                  Hzr_half_nbq(i,j,k-1)))               &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k-1,1)

!-----------------------------
!....... v(i,j,k-1):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = + coefb_v(i,j,k-1) * gdepth_v(i,j,k-1)        &
                          / (0.5*(Hzr_half_nbq(i,j-1,k-1)+              &
                                  Hzr_half_nbq(i,j,k-1)))
#else
        contv_nh(l1_nh) = + coefb_v(i,j,k-1) * gdepth_v(i,j,k-1)        &
                          / (0.5*(Hzr_half_nbq(i,j-1,k-1)+              &
                                  Hzr_half_nbq(i,j,k-1)))               &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k-1,2)

!-----------------------------
!....... u(i+1,j,k):
!-----------------------------   
#ifdef NBQ_CONS7
        contv_nh(l1_nh) =  ( on_u(i+1,j)*pm(i,j)*pn(i,j)                &
                          - ( coefb_u(i+1,j,k) - coefa_u(i+1,j,k) )     &
                          * gdepth_u(i+1,j,k)                           &
                          / (0.5*(Hzr_half_nbq(i,j,k)+                  &
                                  Hzr_half_nbq(i+1,j,k)))       )   
#else
        contv_nh(l1_nh) =  ( on_u(i+1,j)*pm(i,j)*pn(i,j)                &
                          - ( coefb_u(i+1,j,k) - coefa_u(i+1,j,k) )     &
                          * gdepth_u(i+1,j,k)                           &
                          / (0.5*(Hzr_half_nbq(i,j,k)+                  &
                                  Hzr_half_nbq(i+1,j,k)))       )       &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i+1,j,k,1)

!-----------------------------
!....... v(i,j+1,k):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh)  = (  om_v(i,j+1)*pm(i,j)*pn(i,j)               &
                           - ( coefb_v(i,j+1,k) - coefa_v(i,j+1,k) )    &
                           * gdepth_v(i,j+1,k)                          &
                           / (0.5*(Hzr_half_nbq(i,j,k)+                 &
                                   Hzr_half_nbq(i,j+1,k)))   )     
#else
        contv_nh(l1_nh)  = (  om_v(i,j+1)*pm(i,j)*pn(i,j)               &
                           - ( coefb_v(i,j+1,k) - coefa_v(i,j+1,k) )    &
                           * gdepth_v(i,j+1,k)                          &
                           / (0.5*(Hzr_half_nbq(i,j,k)+                 &
                                   Hzr_half_nbq(i,j+1,k)))   )          &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j+1,k,2)

!-----------------------------
!....... u(i+1,j,k+1):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = - coefa_u(i+1,j,k+1) * gdepth_u(i+1,j,k+1)    &
                          / (0.5*(Hzr_half_nbq(i,j,k+1)+                &
                                  Hzr_half_nbq(i+1,j,k+1)))   
#else
        contv_nh(l1_nh) = - coefa_u(i+1,j,k+1) * gdepth_u(i+1,j,k+1)    &
                          / (0.5*(Hzr_half_nbq(i,j,k+1)+                &
                                  Hzr_half_nbq(i+1,j,k+1)))             & 
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i+1,j,k+1,1)

!-----------------------------
!....... v(i,j+1,k+1):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = - coefa_v(i,j+1,k+1) * gdepth_v(i,j+1,k+1)    &
                          / (0.5*(Hzr_half_nbq(i,j,k+1)+                &
                                  Hzr_half_nbq(i,j+1,k+1)))    
#else
        contv_nh(l1_nh) = - coefa_v(i,j+1,k+1) * gdepth_v(i,j+1,k+1)    &
                          / (0.5*(Hzr_half_nbq(i,j,k+1)+                &
                                  Hzr_half_nbq(i,j+1,k+1)))             &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j+1,k+1,2)

!-----------------------------
!....... u(i+1,j,k-1):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = + coefb_u(i+1,j,k-1) * gdepth_u(i+1,j,k-1)    &
                          / (0.5*(Hzr_half_nbq(i,j,k-1)+                &
                                  Hzr_half_nbq(i+1,j,k-1)))      
#else
        contv_nh(l1_nh) = + coefb_u(i+1,j,k-1) * gdepth_u(i+1,j,k-1)    &
                          / (0.5*(Hzr_half_nbq(i,j,k-1)+                &
                                  Hzr_half_nbq(i+1,j,k-1)))             & 
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i+1,j,k-1,1)

!-----------------------------
!....... v(i,j+1,k-1):
!----------------------------- 
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = + coefb_v(i,j+1,k-1) * gdepth_v(i,j+1,k-1)    &
                          / (0.5*(Hzr_half_nbq(i,j,k-1)+                &
                                  Hzr_half_nbq(i,j+1,k-1)))   
#else
        contv_nh(l1_nh) = + coefb_v(i,j+1,k-1) * gdepth_v(i,j+1,k-1)    &
                          / (0.5*(Hzr_half_nbq(i,j,k-1)+                &
                                  Hzr_half_nbq(i,j+1,k-1)))             &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j+1,k-1,2)

!-----------------------------
!....... w(i,j,k):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = 1. / Hzw_half_nbq(i,j,k)                 
#else
        contv_nh(l1_nh) = 1. / Hzw_half_nbq(i,j,k)                      &
                             / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,3)
 
!-----------------------------
!....... w(i,j,k-1):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = -1. / Hzw_half_nbq(i,j,k-1)    
#else
        contv_nh(l1_nh) = -1. / Hzw_half_nbq(i,j,k-1)                   &
                              / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k-1,3)

        enddo

!-------------------------------------------------------------------
!     Inner domain, surface layer: (i,j,N)
!-------------------------------------------------------------------

      do l_nh = neqq_nh(4)+1,neqq_nh(5)  

!......Equation characteristics:
       i = l2iq_nh (l_nh) 
       j = l2jq_nh (l_nh) 
       k = l2kq_nh (l_nh)    
       l1_nh = cont_nnz_nh(l_nh)  
#ifdef NBQ_CONS4
       l2_nh = cinei_nh(l_nh)  
#endif

!-----------------------------
!....... u(i,j,k):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = (- on_u(i,j)*pm(i,j)*pn(i,j)                  &
                          + ( coefa_u(i,j,k) * gdepth_u(i,j,k)          &
                          - coefb_u(i,j,k+1) * gdepth_u(i,j,k+1) )      &
                          / (0.5*(Hzr_half_nbq(i-1,j,k)+                &
                                  Hzr_half_nbq(i,j,k)))        )   
#else
        contv_nh(l1_nh) = (- on_u(i,j)*pm(i,j)*pn(i,j)                  &
                          + ( coefa_u(i,j,k) * gdepth_u(i,j,k)          &
                          - coefb_u(i,j,k+1) * gdepth_u(i,j,k+1) )      &
                          / (0.5*(Hzr_half_nbq(i-1,j,k)+                &
                                  Hzr_half_nbq(i,j,k)))        )        &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,1)
#ifdef NBQ_CONS4
        cinev_nh(l2_nh) = (                                             &
                          + (         &
                          - coefb_u(i,j,k+1) * gdepth_u(i,j,k+1) )      &
                          / (0.5*(Hzr_half_nbq(i-1,j,k)+                &
                                  Hzr_half_nbq(i,j,k)))        )
        l2_nh = l2_nh + mijk2lmom_nh(i,j,k,1)  
#endif
           
!-----------------------------
!....... v(i,j,k):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = (- om_v(i,j)*pm(i,j)*pn(i,j)                  &
                           + ( coefa_v(i,j,k) * gdepth_v(i,j,k)         &
                           - coefb_v(i,j,k+1) * gdepth_v(i,j,k+1) )     &
                           / (0.5*(Hzr_half_nbq(i,j-1,k)+               &
                                   Hzr_half_nbq(i,j,k)))      )   
#else
        contv_nh(l1_nh) = (- om_v(i,j)*pm(i,j)*pn(i,j)                  &
                           + ( coefa_v(i,j,k) * gdepth_v(i,j,k)         &
                           - coefb_v(i,j,k+1) * gdepth_v(i,j,k+1) )     &
                           / (0.5*(Hzr_half_nbq(i,j-1,k)+               &
                                   Hzr_half_nbq(i,j,k)))      )         &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,2)
#ifdef NBQ_CONS4
        cinev_nh(l2_nh) = (                                             &
                           + (                                          &
                           - coefb_v(i,j,k+1) * gdepth_v(i,j,k+1) )     &
                           / (0.5*(Hzr_half_nbq(i,j-1,k)+               &
                                   Hzr_half_nbq(i,j,k)))      )   
        l2_nh = l2_nh + mijk2lmom_nh(i,j,k,2)      
#endif

!-----------------------------
!....... u(i,j,k+1):
!-----------------------------

!-----------------------------
!....... v(i,j,k+1):
!-----------------------------

!-----------------------------
!....... u(i,j,k-1):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = + coefb_u(i,j,k-1) * gdepth_u(i,j,k-1)        &
                          / (0.5*(Hzr_half_nbq(i-1,j,k-1)+              &
                                  Hzr_half_nbq(i,j,k-1)))         
#else
        contv_nh(l1_nh) = + coefb_u(i,j,k-1) * gdepth_u(i,j,k-1)        &
                          / (0.5*(Hzr_half_nbq(i-1,j,k-1)+              &
                                  Hzr_half_nbq(i,j,k-1)))               &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k-1,1)

!-----------------------------
!....... v(i,j,k-1):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = + coefb_v(i,j,k-1) * gdepth_v(i,j,k-1)        &
                          / (0.5*(Hzr_half_nbq(i,j-1,k-1)+              &
                                  Hzr_half_nbq(i,j,k-1)))    
#else
        contv_nh(l1_nh) = + coefb_v(i,j,k-1) * gdepth_v(i,j,k-1)        &
                          / (0.5*(Hzr_half_nbq(i,j-1,k-1)+              &
                                  Hzr_half_nbq(i,j,k-1)))               &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k-1,2)

!-----------------------------
!....... u(i+1,j,k):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = (  on_u(i+1,j)*pm(i,j)*pn(i,j) +              &
                            ( coefa_u(i+1,j,k) * gdepth_u(i+1,j,k)      &
                          - coefb_u(i+1,j,k+1) * gdepth_u(i+1,j,k+1) )  &
                          / (0.5*(Hzr_half_nbq(i,j,k)+                  &
                                  Hzr_half_nbq(i+1,j,k)))     )  
#else
        contv_nh(l1_nh) = (  on_u(i+1,j)*pm(i,j)*pn(i,j) +              &
                            ( coefa_u(i+1,j,k) * gdepth_u(i+1,j,k)      &
                          - coefb_u(i+1,j,k+1) * gdepth_u(i+1,j,k+1) )  &
                          / (0.5*(Hzr_half_nbq(i,j,k)+                  &
                                  Hzr_half_nbq(i+1,j,k)))     )         &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i+1,j,k,1)
#ifdef NBQ_CONS4
        cinev_nh(l2_nh) = (               &
                            (              &
                          - coefb_u(i+1,j,k+1) * gdepth_u(i+1,j,k+1) )  &
                          / (0.5*(Hzr_half_nbq(i,j,k)+                  &
                                  Hzr_half_nbq(i+1,j,k)))     )  
        l2_nh = l2_nh + mijk2lmom_nh(i+1,j,k,1)    
#endif

!-----------------------------
!....... v(i,j+1,k):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = (  om_v(i,j+1)*pm(i,j)*pn(i,j) +              &
                            ( coefa_v(i,j+1,k) * gdepth_v(i,j+1,k)      &
                          - coefb_v(i,j+1,k+1) * gdepth_v(i,j+1,k+1))   &
                          / (0.5*(Hzr_half_nbq(i,j,k)+                  &
                                  Hzr_half_nbq(i,j+1,k)))    )      
#else
        contv_nh(l1_nh) = (  om_v(i,j+1)*pm(i,j)*pn(i,j) +              &
                            ( coefa_v(i,j+1,k) * gdepth_v(i,j+1,k)      &
                          - coefb_v(i,j+1,k+1) * gdepth_v(i,j+1,k+1))   &
                          / (0.5*(Hzr_half_nbq(i,j,k)+                  &
                                  Hzr_half_nbq(i,j+1,k)))    )          &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j+1,k,2)
#ifdef NBQ_CONS4
        cinev_nh(l2_nh) = (  +              &
                            (      &
                          - coefb_v(i,j+1,k+1) * gdepth_v(i,j+1,k+1))   &
                          / (0.5*(Hzr_half_nbq(i,j,k)+                  &
                                  Hzr_half_nbq(i,j+1,k)))    )    
        l2_nh = l2_nh + mijk2lmom_nh(i,j+1,k,2)    
#endif

!-----------------------------
!....... u(i+1,j,k+1):
!-----------------------------

!-----------------------------
!....... v(i,j+1,k+1):
!-----------------------------

!-----------------------------
!....... u(i+1,j,k-1):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = + coefb_u(i+1,j,k-1) * gdepth_u(i+1,j,k-1)    &
                          / (0.5*(Hzr_half_nbq(i,j,k-1)+                &
                                  Hzr_half_nbq(i+1,j,k-1)))     
#else
        contv_nh(l1_nh) = + coefb_u(i+1,j,k-1) * gdepth_u(i+1,j,k-1)    &
                          / (0.5*(Hzr_half_nbq(i,j,k-1)+                &
                                  Hzr_half_nbq(i+1,j,k-1)))             &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i+1,j,k-1,1)

!-----------------------------
!....... v(i,j+1,k-1):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = + coefb_v(i,j+1,k-1) * gdepth_v(i,j+1,k-1)    &
                          / (0.5*(Hzr_half_nbq(i,j,k-1)+                &
                                  Hzr_half_nbq(i,j+1,k-1)))   
#else
        contv_nh(l1_nh) = + coefb_v(i,j+1,k-1) * gdepth_v(i,j+1,k-1)    &
                          / (0.5*(Hzr_half_nbq(i,j,k-1)+                &
                                  Hzr_half_nbq(i,j+1,k-1)))             &
                          / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j+1,k-1,2)

!-----------------------------
!.......point w(i,j,k):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = 1. / Hzw_half_nbq(i,j,k)
#else
        contv_nh(l1_nh) = 1. / Hzw_half_nbq(i,j,k)                      &
                             / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,3)
#ifdef NBQ_CONS4
        cinev_nh(l2_nh) =  1. / Hzw_half_nbq(i,j,k)          
        l2_nh = l2_nh + mijk2lmom_nh(i,j,k,3)   
#endif 

!-----------------------------
!....... w(i,j,k-1):
!-----------------------------
#ifdef NBQ_CONS7
        contv_nh(l1_nh) = -1. / Hzw_half_nbq(i,j,k-1)              
#else
        contv_nh(l1_nh) = -1. / Hzw_half_nbq(i,j,k-1)                   &
                              / Hzr_half_nbq(i,j,k)
#endif
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k-1,3)
        
      enddo

      if (ifl_nbq.eq.1) then
!***********************************************************
!***********************************************************
!     Matrice implicite:    
!          points interieurs
!        & points aux frontieres 
!***********************************************************
!***********************************************************

      l1imp_nbq = 1

      do l_nh = 1,neqcont_nh

       i = l2iq_nh (l_nh)
       j = l2jq_nh (l_nh)
       k = l2kq_nh (l_nh)

!-----------------------------
!.......point w(i,j,k):
!-----------------------------
#ifdef NBQ_CONS7
!      cimpv_nbq(l1imp_nbq) =  -1.d0 / Hzw_half_nbq(i,j,k) / Hzr_half_nbq(i,j,k) * dtnbq 
       cimpv_nbq(l1imp_nbq) =  -1.d0 / Hzw_half_nbq(i,j,k) * dtnbq 
#else
       cimpv_nbq(l1imp_nbq) =  -1.d0 / Hzw_half_nbq(i,j,k) / Hzr_half_nbq(i,j,k) * dtnbq 
#endif
       l1imp_nbq = l1imp_nbq + mijk2lmom_nh(i,j,k,3)

!-----------------------------
!.......point w(i,j,k-1):
!-----------------------------
#ifdef NBQ_CONS7
!      cimpv_nbq(l1imp_nbq) =  1.d0 / Hzw_half_nbq(i,j,k-1) / Hzr_half_nbq(i,j,k) * dtnbq 
       cimpv_nbq(l1imp_nbq) =  1.d0 / Hzw_half_nbq(i,j,k-1) * dtnbq 
#else
       cimpv_nbq(l1imp_nbq) =  1.d0 / Hzw_half_nbq(i,j,k-1) / Hzr_half_nbq(i,j,k) * dtnbq 
#endif
       l1imp_nbq = l1imp_nbq + mijk2lmom_nh(i,j,k-1,3)

      enddo
      endif

      return
      end subroutine mat_cont_nh
#else
      subroutine mat_cont_nh_empty
      return
      end 
#endif
