#include "cppdefs.h"
#ifdef NBQ

      subroutine mat_mom_init_nh 
!*******************************************************************
!*******************************************************************
!*******************************************************************
!      Matrix MOM for momentum equations: initialization
!*******************************************************************
!*******************************************************************
!*******************************************************************

      use module_nh
      use module_nbq       ! #NBQ#

      implicit none

# include "param_F90.h"
# include "scalars_F90.h"
# include "grid.h"
# include "nbq.h"

      integer :: l1_m,i,j,k,i_m

!*******************************************************************
!     Various Initializations:
!*******************************************************************

      nzmimp_nbq   = 1
      nzmom_nh     = 1
      nindkun_nbq  = 0
      neqmimp_nbq = 0
      
#ifdef NBQ_CONS0
      corri_nh = 1
      corrj_nh = 1
      corrv_nh = 0.
#endif

      momi_nh = 1
      momj_nh = 1
      momv_nh = 0.

      mimpi_nbq = 1
      mimpj_nbq = 1
      mimpv_nbq = 0.

!*******************************************************************
!     Momentum Equation: X-direction
!******************************************************************* 
      
#ifdef OBC_NH_WEST
!-------------------------------------------------------------------
!     Boundary condition (istru_nh-1):
!-------------------------------------------------------------------
      do l_nh=nequ_nh(1)+1,nequ_nh(2)
         i = l2imom_nh(l_nh)
         j = l2jmom_nh(l_nh)
         k = l2kmom_nh(l_nh)
         momi_nh  (l_nh)      = nzmom_nh 
         momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k)
         nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j,k) 
      enddo
#endif

!-------------------------------------------------------------------
!     Inner domain, all layers: (i,j,k)
!-------------------------------------------------------------------
      do i_m=0,2
      do l_nh=nequ_nh(2+i_m)+1,nequ_nh(3+i_m)
         i = l2imom_nh(l_nh)
         j = l2jmom_nh(l_nh)
         k = l2kmom_nh(l_nh)
         momi_nh  (l_nh)     = nzmom_nh 

!-----------------------------
!.......point p(i,j,k+1):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k+1)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j,k+1)

!-----------------------------
!.......point p(i,j,k):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j,k)

!-----------------------------
!.......point p(i,j,k-1):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k-1)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j,k-1)

!-----------------------------
!.......point p(i-1,j,k+1):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i-1,j,k+1)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i-1,j,k+1)

!-----------------------------
!.......point p(i-1,j,k):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i-1,j,k)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i-1,j,k)

!-----------------------------
!.......point p(i-1,j,k-1):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i-1,j,k-1)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i-1,j,k-1) 
      enddo 
      enddo 

#ifdef OBC_NH_EAST
!-------------------------------------------------------------------
!.....Boundary condition (iendu_nh+1):
!-------------------------------------------------------------------
      do l_nh=nequ_nh(5)+1,nequ_nh(6)
         i = l2imom_nh(l_nh)
         j = l2jmom_nh(l_nh)
         k = l2kmom_nh(l_nh)
         momi_nh  (l_nh)     = nzmom_nh 
         momj_nh(nzmom_nh)   = ijk2lq_nh (i-1,j,k)
         nzmom_nh            = nzmom_nh + mijk2lq_nh(i-1,j,k) 
      enddo
#else
      momi_nh(nequ_nh(5)+1:nequ_nh(6))=nzmom_nh
#endif

!-------------------------------------------------------------------
!.....MOM matrix structure when points are added at the eastearn boundary!
!-------------------------------------------------------------------
      momi_nh(nequ_nh(6)+1:neqv_nh(1))=nzmom_nh

!*******************************************************************
!     Momentum Equation: Y-direction
!******************************************************************* 

#ifdef OBC_NH_SOUTH
!-------------------------------------------------------------------
!     Boundary condition (jstrv_nh-1):
!-------------------------------------------------------------------
      do l_nh=neqv_nh(1)+1,neqv_nh(2)
         i = l2imom_nh(l_nh)
         j = l2jmom_nh(l_nh)
         k = l2kmom_nh(l_nh)
        momi_nh  (l_nh)     = nzmom_nh
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j,k) 
      enddo
#else
      momi_nh(neqv_nh(1)+1:neqv_nh(2))=nzmom_nh

#endif
    
!-------------------------------------------------------------------
!     Inner domain, all layers: (i,j,k)
!-------------------------------------------------------------------
      do i_m=0,2
      do l_nh=neqv_nh(2+i_m)+1,neqv_nh(3+i_m)
         i = l2imom_nh(l_nh)
         j = l2jmom_nh(l_nh)
         k = l2kmom_nh(l_nh)
         momi_nh  (l_nh)     = nzmom_nh 

!-----------------------------
!.......point p(i,j,k+1):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k+1)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j,k+1) 

!-----------------------------
!.......point p(i,j,k):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j,k) 

!-----------------------------
!.......point p(i,j,k-1):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k-1)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j,k-1) 

!-----------------------------
!.......point p(i,j-1,k+1):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j-1,k+1)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j-1,k+1) 

!-----------------------------
!.......point p(i,j-1,k):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j-1,k)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j-1,k) 

!-----------------------------
!.......point p(i,j-1,k-1):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j-1,k-1)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j-1,k-1) 
       enddo
       enddo
      
#ifdef OBC_NH_NORTH
!-------------------------------------------------------------------
!.....Boundary condition (jendv_nh):
!-------------------------------------------------------------------
      do l_nh=neqv_nh(5)+1,neqv_nh(6)
         i = l2imom_nh(l_nh)
         j = l2jmom_nh(l_nh)
         k = l2kmom_nh(l_nh)
         momi_nh  (l_nh)      = nzmom_nh 
         momj_nh(nzmom_nh)    = ijk2lq_nh (i,j-1,k)
         nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j-1,k) 
       enddo
#else
      momi_nh(neqv_nh(5)+1:neqv_nh(6))=nzmom_nh
#endif

!-------------------------------------------------------------------
!.....MOM matrix structure when points are added at the northern boundary !
!-------------------------------------------------------------------
      momi_nh(neqv_nh(6)+1:neqw_nh(2))=nzmom_nh
      
!*******************************************************************
! Momentum Equation: Z-direction
!******************************************************************* 
      do l_nh=neqw_nh(2)+1,neqw_nh(5)

        i = l2imom_nh(l_nh)
        j = l2jmom_nh(l_nh)
        k = l2kmom_nh(l_nh)
        momi_nh  (l_nh)     = nzmom_nh

!-----------------------------
!.......point p(i,j,k+1):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k+1)
#ifdef NBQ_CONS7
        momv_nh(nzmom_nh)    = -1. * float(mijk2lq_nh(i,j,k+1)) &
                                   * float(mijk2lq_nh(i,j,k))   &
                              / Hzr_half_nbq(i,j,k+1)
#else
        momv_nh(nzmom_nh)    = -1. * float(mijk2lq_nh(i,j,k+1)) &
                                   * float(mijk2lq_nh(i,j,k)) 
#endif
        nzmom_nh             = nzmom_nh  + mijk2lq_nh(i,j,k+1)  &
                                   * float(mijk2lq_nh(i,j,k))

!-----------------------------
!.......point p(i,j,k):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k)
#ifdef NBQ_CONS7
        momv_nh(nzmom_nh)    = 1. * float(mijk2lq_nh(i,j,k))  &
                               / Hzr_half_nbq(i,j,k)
#else
        momv_nh(nzmom_nh)    = 1. * float(mijk2lq_nh(i,j,k))
#endif
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j,k) 

      enddo

!-------------------------------------------------------------------
!.....MOM matrix structure when points are added at the northern boundary !
!-------------------------------------------------------------------
      momi_nh(neqw_nh(5)+1:neqw_nh(7))=nzmom_nh

!.....End of last line:
      neqcorrt_nbq=neqmom_nh(0) 
      momi_nh (neqcorrt_nbq+1) = nzmom_nh 

      if (ifl_imp_nbq.eq.1) then
!*******************************************************************
!.......Partie implicite:
!*******************************************************************

      neqmimp_nbq = 0

!-------------------------------------------------------------------
!     Inner domain, all layers: (i,j,k=0)
!-------------------------------------------------------------------
       
        do l_nh=neqmom_nh(1)+neqmom_nh(2)+1,neqmom_nh(0)

        neqmimp_nbq             = neqmimp_nbq  + 1
        mimpi_nbq (neqmimp_nbq) = nzmimp_nbq
        k = l2kmom_nh(l_nh)      

        if (l_nh.ge.neqw_nh(2)+1.and.l_nh.le.neqw_nh(5).and.k.gt.0) then
           i = l2imom_nh(l_nh)
           j = l2jmom_nh(l_nh)

!-----------------------------
!........point p(i,j,k+1):
!-----------------------------

      !     if (k.ne.N) then
           mimpj_nbq(nzmimp_nbq)   = ijk2lq_nh (i,j,k+1)
           nzmimp_nbq              = nzmimp_nbq + mijk2lq_nh(i,j,k+1)
       !    endif

!-----------------------------
!........point p(i,j,k):
!-----------------------------
           mimpj_nbq(nzmimp_nbq)   = ijk2lq_nh (i,j,k)
           nzmimp_nbq              = nzmimp_nbq + mijk2lq_nh(i,j,k)
       endif

      enddo
      endif

!.....End of last line:
      mimpi_nbq(neqmimp_nbq+1) = nzmimp_nbq   

      

!*******************************************************************
!.....Correction temporelle:
!XA   http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Couplage_Numerique.htm
!     on stocke la matrice qui sert dans l equation pour rho_nbq (M_corr ! dans le document)
!     coherence car c est une matrice qui va etre multipliee par un rho
!******************************************************************* 

#ifdef NBQ_CONS0
      nzcorr_nh = 1

      do l_nh = 1,neqcont_nh
       i = l2iq_nh (l_nh)
       j = l2jq_nh (l_nh)
       k = l2kq_nh (l_nh)

!......caracteristiques de l equation:
       corri_nh  (l_nh) = nzcorr_nh

       if (k.lt.N) then
!......Point rh(i,j,k+1)
          corrj_nh(nzcorr_nh)   = ijk2lq_nh (i,j,k+1)
          nzcorr_nh             = nzcorr_nh + mijk2lq_nh(i,j,k+1)
       endif

!......Point rh(i,j,k)
       corrj_nh(nzcorr_nh)   = ijk2lq_nh (i,j,k)
       nzcorr_nh             = nzcorr_nh + mijk2lq_nh(i,j,k)

!......Point rh(i,j,k-1)
       if (k.gt.1) then
          corrj_nh(nzcorr_nh)   = ijk2lq_nh (i,j,k-1)
          nzcorr_nh             = nzcorr_nh + mijk2lq_nh(i,j,k-1)
       endif

      enddo

!.....End of last line:
      corri_nh  (neqcont_nh+1) = nzcorr_nh
#endif

!.....Tests matrix size:
      if (neqmom_nh(0).gt.nmv_nh) then
         write (6,*) 'nmv_nh trop petit!'
         stop
      endif

      if (nzmom_nh.gt.nmmom_nh) then
         write (6,*) 'nmmom_nh trop petit!'
         stop
      endif

      return
      end subroutine mat_mom_init_nh
#else
      subroutine mat_mom_init_empty
      return
      end 
#endif
