
#include "cppdefs.h"
#ifdef NBQ
      subroutine mat_cont_init_nh 

!*******************************************************************
!*******************************************************************
!*******************************************************************
! Matrix CONT for continuity equation: initialization
!
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Algebrique_Formats_CSR.htm
!
! Matrix construction:
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Pub/NH-NBQ/Html_maps/Schemas_Num_map.html
!
!*******************************************************************
!*******************************************************************
!*******************************************************************

      use module_nh
      use module_nbq
      implicit none

# include "param_F90.h"
# include "scalars_F90.h"
      
      integer :: i,j,k

!*******************************************************************
!     Various Initializations:
!*******************************************************************

!.....Initialize position indices, which will be used
!     as size variable afterwards:
      nzcont_nh     = 1

#ifdef NBQ_CONS4
      nzcine_nh     = 1
#endif

!.....initializations:
      conti_nh    = 1
      cont_nnz_nh = 1
      contj_nh    = 0
      contv_nh    = 0.

#ifdef NBQ_CONS4
      cinei_nh    = 1
      cinej_nh    = 0
      cinev_nh    = 0.
#endif

!*******************************************************************
!     Continuity Equation:
!*******************************************************************

!-------------------------------------------------------------------
!     Inner domain, bottom layer: (i,j,1)
!-------------------------------------------------------------------

      do l_nh = neqq_nh(2)+1,neqq_nh(3)   

!......Equation characteristics:
       i = l2iq_nh (l_nh) 
       j = l2jq_nh (l_nh) 
       k = l2kq_nh (l_nh)      
       conti_nh (l_nh) = nzcont_nh  !! matrix line pointer

!.......u(i,j,k):
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k,1)
         nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j,k,1)

!.......v(i,j,k):
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k,2)
         nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j,k,2)

!.......u(i,j,k+1):
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k+1,1)
         nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j,k+1,1)

!.......v(i,j,k+1):
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k+1,2)
         nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j,k+1,2)

!.......u(i+1,j,k):
         contj_nh(nzcont_nh) = ijk2lmom_nh(i+1,j,k,1)
         nzcont_nh           = nzcont_nh + mijk2lmom_nh(i+1,j,k,1)

!.......v(i,j+1,k):
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j+1,k,2)
         nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j+1,k,2)

!........u(i+1,j,k+1):
         contj_nh(nzcont_nh) = ijk2lmom_nh(i+1,j,k+1,1)
         nzcont_nh           = nzcont_nh + mijk2lmom_nh(i+1,j,k+1,1)

!........v(i,j+1,k+1):
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j+1,k+1,2)
         nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j+1,k+1,2)

!.......w(i,j,k):
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k,3)
         nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j,k,3)

!.......w(i,j,k-1):
!        contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k-1,3)
!        nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j,k-1,3)

!       Last point...
        cont_nnz_nh(l_nh+1)=nzcont_nh

      enddo

!-------------------------------------------------------------------
!     Inner domain, inner layers: (i,j,1<k<N)
!-------------------------------------------------------------------

      do l_nh = neqq_nh(3)+1 , neqq_nh(4)

!......Equation characteristics:
       i = l2iq_nh (l_nh) 
       j = l2jq_nh (l_nh) 
       k = l2kq_nh (l_nh)      
       conti_nh (l_nh) = nzcont_nh  !! matrix line pointer

!.......u(i,j,k):
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k,1)
         nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j,k,1)

!.......v(i,j,k):
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k,2)
         nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j,k,2)

!.......u(i,j,k+1):
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k+1,1)
         nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j,k+1,1)

!.......v(i,j,k+1):
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k+1,2)
         nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j,k+1,2)

!........u(i,j,k-1):
          contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k-1,1)
          nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j,k-1,1)

!........v(i,j,k-1):
          contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k-1,2)
          nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j,k-1,2)

!.......u(i+1,j,k):
         contj_nh(nzcont_nh) = ijk2lmom_nh(i+1,j,k,1)
         nzcont_nh           = nzcont_nh + mijk2lmom_nh(i+1,j,k,1)

!.......v(i,j+1,k):
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j+1,k,2)
         nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j+1,k,2)

!........u(i+1,j,k+1):
          contj_nh(nzcont_nh) = ijk2lmom_nh(i+1,j,k+1,1)
          nzcont_nh           = nzcont_nh + mijk2lmom_nh(i+1,j,k+1,1)

!........v(i,j+1,k+1):
          contj_nh(nzcont_nh) = ijk2lmom_nh(i,j+1,k+1,2)
          nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j+1,k+1,2)

!........u(i+1,j,k-1):
          contj_nh(nzcont_nh) = ijk2lmom_nh(i+1,j,k-1,1)
          nzcont_nh           = nzcont_nh + mijk2lmom_nh(i+1,j,k-1,1)

!........v(i,j+1,k-1):
          contj_nh(nzcont_nh) = ijk2lmom_nh(i,j+1,k-1,2)
          nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j+1,k-1,2)

!.......w(i,j,k):
        contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k,3)
        nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j,k,3)

!.......w(i,j,k-1):
        contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k-1,3)
        nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j,k-1,3)

!       Last point...
        cont_nnz_nh(l_nh+1)=nzcont_nh

      enddo

!-------------------------------------------------------------------
!     Inner domain, surface layer: (i,j,N)
!-------------------------------------------------------------------

      do l_nh = neqq_nh(4)+1,neqq_nh(5)  

!......Equation characteristics:
       i = l2iq_nh (l_nh) 
       j = l2jq_nh (l_nh) 
       k = l2kq_nh (l_nh)      
       conti_nh (l_nh) = nzcont_nh  !! matrix line pointer
#ifdef NBQ_CONS4
       cinei_nh (l_nh) = nzcine_nh  !! matrix line pointer
#endif

!.......u(i,j,k):
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k,1)
         nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j,k,1)
#ifdef NBQ_CONS4
         cinej_nh(nzcine_nh) = ijk2lmom_nh(i,j,k,1)
         nzcine_nh           = nzcine_nh + mijk2lmom_nh(i,j,k,1)
#endif

!.......v(i,j,k):
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k,2)
         nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j,k,2)
#ifdef NBQ_CONS4
         cinej_nh(nzcine_nh) = ijk2lmom_nh(i,j,k,2)
         nzcine_nh           = nzcine_nh + mijk2lmom_nh(i,j,k,2)
#endif

!........u(i,j,k-1):
          contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k-1,1)
          nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j,k-1,1)

!........v(i,j,k-1):
          contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k-1,2)
          nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j,k-1,2)

!.......u(i+1,j,k):
         contj_nh(nzcont_nh) = ijk2lmom_nh(i+1,j,k,1)
         nzcont_nh           = nzcont_nh + mijk2lmom_nh(i+1,j,k,1)
#ifdef NBQ_CONS4
         cinej_nh(nzcine_nh) = ijk2lmom_nh(i+1,j,k,1)
         nzcine_nh           = nzcine_nh + mijk2lmom_nh(i+1,j,k,1)
#endif

!.......v(i,j+1,k):
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j+1,k,2)
         nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j+1,k,2)
#ifdef NBQ_CONS4
         cinej_nh(nzcine_nh) = ijk2lmom_nh(i,j+1,k,2)
         nzcine_nh           = nzcine_nh + mijk2lmom_nh(i,j+1,k,2)
#endif

!........u(i+1,j,k-1):
          contj_nh(nzcont_nh) = ijk2lmom_nh(i+1,j,k-1,1)
          nzcont_nh           = nzcont_nh + mijk2lmom_nh(i+1,j,k-1,1)

!........v(i,j+1,k-1):
          contj_nh(nzcont_nh) = ijk2lmom_nh(i,j+1,k-1,2)
          nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j+1,k-1,2)

!.......w(i,j,k):
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k,3)
         nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j,k,3)
#ifdef NBQ_CONS4
         cinej_nh(nzcine_nh) = ijk2lmom_nh(i,j,k,3)
         nzcine_nh           = nzcine_nh + mijk2lmom_nh(i,j,k,3)
#endif

!.......w(i,j,k-1):
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k-1,3)
         nzcont_nh           = nzcont_nh + mijk2lmom_nh(i,j,k-1,3)

!       Last point...
        cont_nnz_nh(l_nh+1)=nzcont_nh

      enddo

!*******************************************************************
!     Last line treatment:
!*******************************************************************
      conti_nh    (neqq_nh(5)+1:neqq_nh(7)+1) = nzcont_nh
#ifdef NBQ_CONS4
      cinei_nh    (neqq_nh(5)+1:neqq_nh(7)+1) = nzcine_nh
#endif
      cont_nnz_nh (neqq_nh(5)+1:neqq_nh(7)+1) = nzcont_nh

      if (ifl_nbq.eq.1.and.ifl_imp_nbq.eq.1) then
!.......................................
! Matrice schema implicit: 
!  points interieurs et points de frontiere
!.......................................

       nzcimp_nbq     = 1
       cimpj_nbq = 1
       cimpv_nbq = 0.

       do l_nh = 1,neqcont_nh

!......caracteristiques de l'equation:
       i = l2iq_nh (l_nh)
       j = l2jq_nh (l_nh)
       k = l2kq_nh (l_nh)
       cimpi_nbq(l_nh) = nzcimp_nbq

!.......point w(i,j,k):
       cimpj_nbq(nzcimp_nbq) = ijk2lmom_nh(i,j,k,3)-neqmom_nh(1)-neqmom_nh(2)
       nzcimp_nbq            = nzcimp_nbq + mijk2lmom_nh(i,j,k,3)

!.......point w(i,j,k-1):
       cimpj_nbq(nzcimp_nbq) = ijk2lmom_nh(i,j,k-1,3)-neqmom_nh(1)-neqmom_nh(2)
       nzcimp_nbq            = nzcimp_nbq + mijk2lmom_nh(i,j,k-1,3)

       enddo

      endif

!*******************************************************************
!     Last line treatment:
!*******************************************************************
      neqcimp_nbq = neqcont_nh 
      cimpi_nbq(neqcimp_nbq+1) = nzcimp_nbq

      return
      end subroutine mat_cont_init_nh
#else
      subroutine mat_cont_init_nh_empty
      return
      end
#endif
