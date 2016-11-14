#include "cppdefs.h"
#ifdef NBQ

      subroutine nump_nh

!******************************************************************************************
!                   Numbering of Mass Points
!
!******************************************************************************************

      use module_nh
      use module_nbq

      implicit none

# include "param_F90.h"
# include "scalars_F90.h"
# include "grid.h"

#include "def_bounds.h"

      integer ::  i,j,k,nzq_n
      integer ::  jstr_n,jend_n
      integer :: ierr_n,ierrmpi_n

!*******************************************************************
!     Initializations
!*******************************************************************

      nzq_n      = 0
      mijk2lq_nh = 0
      neqq_nh    = 0
      neqcont_nh = 0
 
!******************************************************************************************
! Inner MPI domain:
! cf: http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Algebrique_Numerotation_Base.htm
!******************************************************************************************

#ifdef MPI
!-------------------------------------------------------------------
!     MPI Exchange (WEST):
!-------------------------------------------------------------------
      if (WEST_INTER_NBQ) then
         i        = istr_nh-1

!........Corner (South-West):
         if (SOUTH_INTER_NBQ) then
            jstr_n = jstr_nh-1
         else
            jstr_n = jstr_nh
         endif
!........Corner (North-West):
         if (NORTH_INTER_NBQ) then
            jend_n = jend_nh+1
         else
            jend_n = jend_nh
         endif

         do j=jstr_n,jend_n
         do k=1,N    
#ifdef MASKING
            if (rmask(i,j).ne.0) then
#endif
              nzq_n             = nzq_n + 1           
              ijk2lq_nh(i,j,k)  = nzq_n               
              mijk2lq_nh(i,j,k) = mijk2lq_nh(i,j,k) + 1 
              l2iq_nh  (nzq_n)  = i
              l2jq_nh  (nzq_n)  = j
              l2kq_nh  (nzq_n)  = k
#ifdef MASKING
            endif
#endif
         enddo
         enddo
      endif

!-------------------------------------------------------------------
!     MPI Exchange (SOUTH):
!-------------------------------------------------------------------
      if (SOUTH_INTER_NBQ) then
         j        = jstr_nh-1
         do i=istr_nh,iend_nh
         do k=1,N    
#ifdef MASKING
            if (rmask(i,j).ne.0) then
#endif
              nzq_n             = nzq_n + 1           
              ijk2lq_nh(i,j,k)  = nzq_n               
              mijk2lq_nh(i,j,k) = mijk2lq_nh(i,j,k) + 1 
              l2iq_nh  (nzq_n)  = i
              l2jq_nh  (nzq_n)  = j
              l2kq_nh  (nzq_n)  = k
#ifdef MASKING
            endif
#endif
         enddo
         enddo
      endif
#endif

!-------------------------------------------------------------------
!     WESTERN EDGE
!-------------------------------------------------------------------
# if defined OBC_NBQ && defined OBC_WEST 
#  ifdef MPI
      if (WESTERN_EDGE) then
        i = istr_nh-1

!........Corner (South-West):
        if (SOUTH_INTER_NBQ) then
          jstr_n = jstr_nh-1
        else
          jstr_n = jstr_nh
        endif
!........Corner (North-West):
        if (NORTH_INTER_NBQ) then
          jend_n = jend_nh+1
        else
          jend_n = jend_nh
        endif
#  else
        i = istr_nh-1
        jstr_n = jstr_nh
        jend_n = jend_nh
#  endif /* MPI */
 
        do j=jstr_n,jend_n
        do k=1,N   
#  ifdef MASKING
          if (rmask(i,j).ne.0) then
#  endif
              nzq_n             = nzq_n + 1           
              ijk2lq_nh(i,j,k)  = nzq_n               
              mijk2lq_nh(i,j,k) = mijk2lq_nh(i,j,k) + 1 
              l2iq_nh  (nzq_n)  = i
              l2jq_nh  (nzq_n)  = j
              l2kq_nh  (nzq_n)  = k
#  ifdef MASKING
          endif
#  endif
        enddo
        enddo
#  ifdef MPI
      endif
#  endif
# endif /* OBC_NBQ */

!-------------------------------------------------------------------
!     SOUTHERN EDGE 
!-------------------------------------------------------------------
# if defined OBC_NBQ && !defined NS_PERIODIC && defined OBC_SOUTH
#  ifdef MPI
      if (SOUTHERN_EDGE) then
#  endif
        j = jstr_nh-1

        do i=istr_nh-1,iend_nh+1
        do k=1,N    
#  ifdef MASKING
          if (rmask(i,j).ne.0) then
#  endif
              nzq_n             = nzq_n + 1           
              ijk2lq_nh(i,j,k)  = nzq_n               
              mijk2lq_nh(i,j,k) = mijk2lq_nh(i,j,k) + 1 
              l2iq_nh  (nzq_n)  = i
              l2jq_nh  (nzq_n)  = j
              l2kq_nh  (nzq_n)  = k
#  ifdef MASKING
          endif
#  endif
        enddo
        enddo
#  ifdef MPI
      endif
#  endif
# endif /* OBC_NBQ */

      neqq_nh(1) = nzq_n
      neqq_nh(2) = nzq_n

!-------------------------------------------------------------------
!     Inner domain, bottom layer: (i,j,1)
!-------------------------------------------------------------------
      k = 1 
      do i = istr_nh , iend_nh 
      do j = jstr_nh , jend_nh  
              
#ifdef MASKING
         if (rmask(i,j).ne.0) then
#endif
           nzq_n             = nzq_n + 1           
           ijk2lq_nh(i,j,k)  = nzq_n               
           mijk2lq_nh(i,j,k) = mijk2lq_nh(i,j,k) + 1 
           l2iq_nh  (nzq_n)  = i
           l2jq_nh  (nzq_n)  = j
           l2kq_nh  (nzq_n)  = k
#ifdef MASKING
         endif
#endif

      enddo
      enddo
      neqq_nh(3) = nzq_n

!-------------------------------------------------------------------
!     Inner domain, all layers: (i,j,k)
!-------------------------------------------------------------------
      do k = 2    , N -1
      do i = istr_nh , iend_nh
      do j = jstr_nh , jend_nh   
              
#ifdef MASKING
         if (rmask(i,j).ne.0) then
#endif
           nzq_n             = nzq_n + 1           
           ijk2lq_nh(i,j,k)  = nzq_n               
           mijk2lq_nh(i,j,k) = mijk2lq_nh(i,j,k) + 1 
           l2iq_nh  (nzq_n)  = i
           l2jq_nh  (nzq_n)  = j
           l2kq_nh  (nzq_n)  = k
#ifdef MASKING
         endif
#endif

      enddo
      enddo
      enddo
      neqq_nh(4) = nzq_n

!-------------------------------------------------------------------
!     Inner domain, surface layer: (i,j,N)
!-------------------------------------------------------------------
      k = N 
      do i = istr_nh , iend_nh
      do j = jstr_nh , jend_nh   
              
#ifdef MASKING
         if (rmask(i,j).ne.0) then
#endif
           nzq_n             = nzq_n + 1           
           ijk2lq_nh(i,j,k)  = nzq_n               
           mijk2lq_nh(i,j,k) = mijk2lq_nh(i,j,k) + 1 
           l2iq_nh  (nzq_n)  = i
           l2jq_nh  (nzq_n)  = j
           l2kq_nh  (nzq_n)  = k
#ifdef MASKING
         endif
#endif

      enddo
      enddo
      neqq_nh(5) = nzq_n
      neqq_nh(6) = nzq_n

!-------------------------------------------------------------------
!     EASTERN EDGE
!-------------------------------------------------------------------
# if defined OBC_NBQ && defined OBC_EAST
#  ifdef MPI
      if (EASTERN_EDGE) then
        i = iend_nh+1

!........Corner (South-East):
        if (SOUTH_INTER_NBQ) then
          jstr_n = jstr_nh-1
        else
          jstr_n = jstr_nh
        endif
!........Corner (North-East):
        if (NORTH_INTER_NBQ) then
          jend_n = jend_nh+1
        else
          jend_n = jend_nh
        endif
#  else
        i = iend_nh+1
        jstr_n = jstr_nh
        jend_n = jend_nh
#  endif /* MPI */

        do j=jstr_n,jend_n
        do k=1,N                 
#  ifdef MASKING
          if (rmask(i,j).ne.0) then
#  endif
           nzq_n             = nzq_n + 1           
           ijk2lq_nh(i,j,k)  = nzq_n               
           mijk2lq_nh(i,j,k) = mijk2lq_nh(i,j,k) + 1 
           l2iq_nh  (nzq_n)  = i
           l2jq_nh  (nzq_n)  = j
           l2kq_nh  (nzq_n)  = k
#  ifdef MASKING
          endif
#  endif
        enddo
        enddo
#  ifdef MPI
      endif
#  endif
# endif /* OBC_NBQ */

!-------------------------------------------------------------------
!     NORTHERN EDGE 
!-------------------------------------------------------------------
# if defined OBC_NBQ && !defined NS_PERIODIC && defined OBC_NORTH
#  ifdef MPI
      if (NORTHERN_EDGE) then
#  endif
        j = jend_nh+1

        do i=istr_nh-1,iend_nh+1
        do k=1,N    
#  ifdef MASKING
          if (rmask(i,j).ne.0) then
#  endif
           nzq_n             = nzq_n + 1           
           ijk2lq_nh(i,j,k)  = nzq_n               
           mijk2lq_nh(i,j,k) = mijk2lq_nh(i,j,k) + 1 
           l2iq_nh  (nzq_n)  = i
           l2jq_nh  (nzq_n)  = j
           l2kq_nh  (nzq_n)  = k
#  ifdef MASKING
          endif
#  endif
        enddo
        enddo
#  ifdef MPI
      endif
#  endif
# endif /* OBC_NBQ */


#ifdef MPI
!-------------------------------------------------------------------
!     MPI Exchange (EAST):
!-------------------------------------------------------------------
      if (EAST_INTER_NBQ) then
         i        = iend_nh+1

!........Corner (South-East):
         if (SOUTH_INTER_NBQ) then
            jstr_n = jstr_nh-1
         else
            jstr_n = jstr_nh
         endif
!........Corner (North-East):
         if (NORTH_INTER_NBQ) then
            jend_n = jend_nh+1
         else
            jend_n = jend_nh
         endif

         do j=jstr_n,jend_n
         do k=1,N    
#ifdef MASKING
            if (rmask(i,j).ne.0) then
#endif
              nzq_n             = nzq_n + 1           
              ijk2lq_nh(i,j,k)  = nzq_n               
              mijk2lq_nh(i,j,k) = mijk2lq_nh(i,j,k) + 1 
              l2iq_nh  (nzq_n)  = i
              l2jq_nh  (nzq_n)  = j
              l2kq_nh  (nzq_n)  = k
#ifdef MASKING
            endif
#endif
         enddo
         enddo
      endif

!-------------------------------------------------------------------
!     MPI Exchange (NORTH):
!-------------------------------------------------------------------
      if (NORTH_INTER_NBQ) then
         j        = jend_nh+1
         do i=istr_nh,iend_nh
         do k=1,N    
#ifdef MASKING
            if (rmask(i,j).ne.0) then
#endif
              nzq_n             = nzq_n + 1           
              ijk2lq_nh(i,j,k)  = nzq_n               
              mijk2lq_nh(i,j,k) = mijk2lq_nh(i,j,k) + 1 
              l2iq_nh  (nzq_n)  = i
              l2jq_nh  (nzq_n)  = j
              l2kq_nh  (nzq_n)  = k
#ifdef MASKING
            endif
#endif
         enddo
         enddo
      endif
#endif
      neqq_nh(7) = nzq_n
      neqcont_nh = nzq_n


!*******************************************************************
!*******************************************************************
! Test Grid Definition
!******************************************************************* 
!*******************************************************************
      return
      ierr_n = maxval(mijk2lq_nh)
      if (ierr_n.gt.1) then
# ifdef MPI
         call mpi_finalize(ierrmpi_n)
# endif
         stop 'Grid Error in nump_nh'
      endif

      return

      end subroutine nump_nh
 
#else
        subroutine nump_nh_empty
        return
        end 
#endif
