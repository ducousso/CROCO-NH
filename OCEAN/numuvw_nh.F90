#include "cppdefs.h"
#ifdef NBQ

      subroutine numuvw_nh

!*******************************************************************
!
!                   Numbering of Mass Points
!
!*******************************************************************

      use module_nh
      use module_nbq
      implicit none
# include "param_F90.h"
# include "scalars_F90.h"
# include "grid.h"
# include "def_bounds.h"

      integer   :: i,j,k,nzuvw_n   
      integer   :: jstr_n,jend_n
      integer   :: ierr_n,ierrmpi_n


!*******************************************************************
!*******************************************************************
!     Initializations
!*******************************************************************
!*******************************************************************

      nzuvw_n      = 0
      mijk2lmom_nh = 0
      nequ_nh      = 0
      neqv_nh      = 0
      neqmom_nh    = 0

!*******************************************************************
!*******************************************************************
!     Momentum Equation: X-direction
!******************************************************************* 
!*******************************************************************
# ifdef MPI
!-------------------------------------------------------------------
!     (U) WEST MPI Interface
!-------------------------------------------------------------------
      if (WEST_INTER_NBQ) then
         i = istru_nh-1

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
#  ifdef MASKING
            if (umask(i,j).ne.0) then
#  endif
               nzuvw_n               = nzuvw_n + 1 
               ijk2lmom_nh(i,j,k,1)  = nzuvw_n
               mijk2lmom_nh(i,j,k,1) = mijk2lmom_nh(i,j,k,1) + 1 
               l2imom_nh(nzuvw_n)    = i
               l2jmom_nh(nzuvw_n)    = j
               l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo

      endif

!-------------------------------------------------------------------
!     (U) SOUTH MPI Interface
!-------------------------------------------------------------------
      if (SOUTH_INTER_NBQ) then
         j        = jstr_nh - 1

         do i=istru_nh,iendu_nh
         do k=1,N    
#  ifdef MASKING
            if (umask(i,j).ne.0) then
#  endif
               nzuvw_n               = nzuvw_n + 1 
               ijk2lmom_nh(i,j,k,1)  = nzuvw_n
               mijk2lmom_nh(i,j,k,1) = mijk2lmom_nh(i,j,k,1) + 1 
               l2imom_nh(nzuvw_n)    = i
               l2jmom_nh(nzuvw_n)    = j
               l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo

      endif
# endif /* MPI */

!-------------------------------------------------------------------
!     (U) WESTERN EDGE
!-------------------------------------------------------------------
# if defined OBC_NBQ && !defined OBC_NH && defined OBC_WEST
#  ifdef MPI
      if (WESTERN_EDGE) then
        i = istru_nh-1

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
        i = istru_nh-1
        jstr_n = jstr_nh
        jend_n = jend_nh
#  endif /* MPI */

        do j=jstr_n,jend_n
        do k=1,N    
#  ifdef MASKING
          if (umask(i,j).ne.0) then
#  endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,1)  = nzuvw_n
            mijk2lmom_nh(i,j,k,1) = mijk2lmom_nh(i,j,k,1) + 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
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
!     (U) SOUTHERN EDGE (tangential velocity)
!-------------------------------------------------------------------
# if defined OBC_NBQ && !defined NS_PERIODIC && defined OBC_SOUTH
#  ifdef MPI
      if (SOUTHERN_EDGE) then
#  endif
        j = jstr_nh-1

        do i=istru_nh-1,iendu_nh+1
        do k=1,N    
#  ifdef MASKING
          if (umask(i,j).ne.0) then
#  endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,1)  = nzuvw_n
            mijk2lmom_nh(i,j,k,1) = mijk2lmom_nh(i,j,k,1) + 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
          endif
#  endif
        enddo
        enddo
#  ifdef MPI
      endif
#  endif
# endif /* OBC_NBQ */

      nequ_nh(1) = nzuvw_n

!-------------------------------------------------------------------
!     (U) WESTERN EDGE (MATRIX OBC)
!-------------------------------------------------------------------
# ifdef OBC_NH
#  ifdef MPI
      if (WESTERN_EDGE) then
        i = istru_nh-1

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
        i = istru_nh-1
        jstr_n = jstr_nh
        jend_n = jend_nh
#  endif /* MPI */

        do j=jstr_n,jend_n
        do k=1,N    
#  ifdef MASKING
          if (umask(i,j).ne.0) then
#  endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,1)  = nzuvw_n
            mijk2lmom_nh(i,j,k,1) = mijk2lmom_nh(i,j,k,1) + 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
          endif
#  endif
        enddo
        enddo
#  ifdef MPI
      endif
#  endif
# endif /* OBC_NH */

      nequ_nh(2) = nzuvw_n
      
!-------------------------------------------------------------------
!     (U) Inner domain, bottom layer: (i,j,k=1)
!-------------------------------------------------------------------
      k=1
      do i=istru_nh,iendu_nh
      do j=jstr_nh,jend_nh          
# ifdef MASKING
         if (umask(i,j).ne.0) then
# endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,1)  = nzuvw_n
            mijk2lmom_nh(i,j,k,1) = mijk2lmom_nh(i,j,k,1) + 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
# ifdef MASKING
         endif
# endif
      enddo
      enddo

      nequ_nh(3) = nzuvw_n

!-------------------------------------------------------------------
!     (U) Inner domain, inner layers: (i,j, 1<k<N )
!-------------------------------------------------------------------
      do i=istru_nh,iendu_nh
      do j=jstr_nh,jend_nh
      do k=2,N-1      
# ifdef MASKING
         if (umask(i,j).ne.0) then
# endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,1)  = nzuvw_n
            mijk2lmom_nh(i,j,k,1) = mijk2lmom_nh(i,j,k,1) + 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
# ifdef MASKING
         endif
# endif
      enddo
      enddo
      enddo

      nequ_nh(4) = nzuvw_n

!-------------------------------------------------------------------
!     (U) Inner domain, surface layer: (i,j,k=N )
!-------------------------------------------------------------------
      k=N
      do i=istru_nh,iendu_nh
      do j=jstr_nh,jend_nh           
# ifdef MASKING
         if (umask(i,j).ne.0) then
# endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,1)  = nzuvw_n
            mijk2lmom_nh(i,j,k,1) = mijk2lmom_nh(i,j,k,1) + 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
# ifdef MASKING
         endif
# endif
      enddo
      enddo

      nequ_nh(5) = nzuvw_n

!-------------------------------------------------------------------
!     (U) EASTERN EDGE (MATRIX OBC)
!-------------------------------------------------------------------
# ifdef OBC_NH
#  ifdef MPI
      if (EASTERN_EDGE) then
        i = iendu_nh+1

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
        i = iendu_nh+1
        jstr_n = jstr_nh
        jend_n = jend_nh
#  endif /* MPI */

        do j=jstr_n,jend_n
        do k=1,N                 
#  ifdef MASKING
          if (umask(i,j).ne.0) then
#  endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,1)  = nzuvw_n
            mijk2lmom_nh(i,j,k,1) = mijk2lmom_nh(i,j,k,1) + 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
          endif
#  endif
        enddo
        enddo
#  ifdef MPI
      endif
#  endif
# endif /* OBC_NH */

      nequ_nh(6) = nzuvw_n

!-------------------------------------------------------------------
!     (U) EASTERN EDGE 
!-------------------------------------------------------------------
# if defined OBC_NBQ && ! defined OBC_NH && defined OBC_EAST
#  ifdef MPI
      if (EASTERN_EDGE) then
        i = iendu_nh+1

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
        i = iendu_nh+1
        jstr_n = jstr_nh
        jend_n = jend_nh
#  endif /* MPI */

        do j=jstr_n,jend_n
        do k=1,N                 
#  ifdef MASKING
          if (umask(i,j).ne.0) then
#  endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,1)  = nzuvw_n
            mijk2lmom_nh(i,j,k,1) = mijk2lmom_nh(i,j,k,1) + 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
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
!     (U) NORTHERN EDGE (tangential velocity)
!-------------------------------------------------------------------
# if defined OBC_NBQ && !defined NS_PERIODIC && defined OBC_NORTH
#  ifdef MPI
      if (NORTHERN_EDGE) then
#  endif
        j = jend_nh+1

        do i=istru_nh-1,iendu_nh+1
        do k=1,N    
#  ifdef MASKING
          if (umask(i,j).ne.0) then
#  endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,1)  = nzuvw_n
            mijk2lmom_nh(i,j,k,1) = mijk2lmom_nh(i,j,k,1) + 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
          endif
#  endif
        enddo
        enddo
#  ifdef MPI
      endif
#  endif
# endif /* OBC_NBQ */


# ifdef MPI
!-------------------------------------------------------------------
!     (U) EAST MPI Interface
!-------------------------------------------------------------------
      if (EAST_INTER_NBQ) then
         i = iendu_nh+1

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
#  ifdef MASKING
            if (umask(i,j).ne.0) then
#  endif
               nzuvw_n               = nzuvw_n + 1 
               ijk2lmom_nh(i,j,k,1)  = nzuvw_n
               mijk2lmom_nh(i,j,k,1) = mijk2lmom_nh(i,j,k,1) + 1 
               l2imom_nh(nzuvw_n)    = i
               l2jmom_nh(nzuvw_n)    = j
               l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo
       endif

!-------------------------------------------------------------------
!     (U) NORTH MPI Interface
!-------------------------------------------------------------------
      if (NORTH_INTER_NBQ) then
         j        = jend_nh+1

         do i=istru_nh,iendu_nh
         do k=1,N    
#  ifdef MASKING
            if (umask(i,j).ne.0) then
#  endif
               nzuvw_n               = nzuvw_n + 1 
               ijk2lmom_nh(i,j,k,1)  = nzuvw_n
               mijk2lmom_nh(i,j,k,1) = mijk2lmom_nh(i,j,k,1) + 1 
               l2imom_nh(nzuvw_n)    = i
               l2jmom_nh(nzuvw_n)    = j
               l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo
      endif
# endif /* MPI */

      nequ_nh(7)   = nzuvw_n

!
!.....Number of equations in X-direction:
!
      neqmom_nh(1) = nzuvw_n

!*******************************************************************
!*******************************************************************
!     Momentum Equation: Y-direction
!******************************************************************* 
!*******************************************************************

# ifdef MPI
!-------------------------------------------------------------------
!     (V) WEST MPI Interface
!-------------------------------------------------------------------
      if (WEST_INTER_NBQ) then
         i        = istr_nh - 1

!........Corner (South-West):
         if (SOUTH_INTER_NBQ) then
            jstr_n = jstrv_nh-1
         else
            jstr_n = jstrv_nh
         endif
!........Corner (North-West):
         if (NORTH_INTER_NBQ) then
            jend_n = jendv_nh+1
         else
            jend_n = jendv_nh
         endif

         do j=jstr_n,jend_n
         do k=1,N    
#  ifdef MASKING
            if (vmask(i,j).ne.0) then
#  endif
               nzuvw_n               = nzuvw_n + 1 
               ijk2lmom_nh(i,j,k,2)  = nzuvw_n
               mijk2lmom_nh(i,j,k,2) = mijk2lmom_nh(i,j,k,2) + 1 
               l2imom_nh(nzuvw_n)    = i
               l2jmom_nh(nzuvw_n)    = j
               l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo
      endif

!-------------------------------------------------------------------
!     (V) SOUTH MPI Interface
!-------------------------------------------------------------------
      if (SOUTH_INTER_NBQ) then
         j=jstrv_nh-1

         do i=istr_nh,iend_nh
         do k=1,N 
#  ifdef MASKING
            if (vmask(i,j).ne.0) then
#  endif
               nzuvw_n               = nzuvw_n + 1 
               ijk2lmom_nh(i,j,k,2)  = nzuvw_n
               mijk2lmom_nh(i,j,k,2) = mijk2lmom_nh(i,j,k,2) + 1 
               l2imom_nh(nzuvw_n)    = i
               l2jmom_nh(nzuvw_n)    = j
               l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo

      endif

# endif /* MPI */

!-------------------------------------------------------------------
!     (V) WESTERN EDGE (tangential velocity)
!-------------------------------------------------------------------
# if defined OBC_NBQ && defined OBC_WEST
#  ifdef MPI
      if (WESTERN_EDGE) then
        i = istr_nh-1

!........Corner (South-West):
        if (SOUTH_INTER_NBQ) then
          jstr_n = jstrv_nh-1
        else
          jstr_n = jstrv_nh
        endif
!........Corner (North-West):
        if (NORTH_INTER_NBQ) then
          jend_n = jendv_nh+1
        else
          jend_n = jendv_nh
        endif
#  else
         i = istr_nh-1
         jstr_n = jstrv_nh
         jend_n = jendv_nh
#  endif /* MPI */

         do j=jstr_n,jend_n
         do k=1,N    
#  ifdef MASKING
            if (vmask(i,j).ne.0) then
#  endif
               nzuvw_n               = nzuvw_n + 1 
               ijk2lmom_nh(i,j,k,2)  = nzuvw_n
               mijk2lmom_nh(i,j,k,2) = mijk2lmom_nh(i,j,k,2) + 1 
               l2imom_nh(nzuvw_n)    = i
               l2jmom_nh(nzuvw_n)    = j
               l2kmom_nh(nzuvw_n)    = k
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
!     (V) SOUTHERN EDGE
!-------------------------------------------------------------------
# if defined OBC_NBQ && ! defined OBC_NH && !defined NS_PERIODIC  && defined OBC_SOUTH
#  ifdef MPI
      if (SOUTHERN_EDGE) then
#  endif
        j=jstrv_nh-1

        do i=istr_nh-1,iend_nh+1
        do k=1,N 
#  ifdef MASKING
          if (vmask(i,j).ne.0) then
#  endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,2)  = nzuvw_n
            mijk2lmom_nh(i,j,k,2) = mijk2lmom_nh(i,j,k,2) + 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
          endif
#  endif
        enddo
        enddo
#  ifdef MPI
      endif
#  endif
# endif /* OBC_NBQ */

      neqv_nh(1) = nzuvw_n

!-------------------------------------------------------------------
!     (V) SOUTHERN EDGE (MATRIX OBC)
!-------------------------------------------------------------------
# ifdef OBC_NH
#  ifdef MPI
      if (SOUTHERN_EDGE) then
#  endif
        j=jstrv_nh-1

        do i=istr_nh-1,iend_nh+1
        do k=1,N 
#  ifdef MASKING
          if (vmask(i,j).ne.0) then
#  endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,2)  = nzuvw_n
            mijk2lmom_nh(i,j,k,2) = mijk2lmom_nh(i,j,k,2) + 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
          endif
#  endif
        enddo
        enddo
#  ifdef MPI
      endif
#  endif
# endif /* OBC_NH */

      neqv_nh(2) = nzuvw_n

!-------------------------------------------------------------------
!     (V) Inner domain, bottom layer: (i,j,k=1)
!-------------------------------------------------------------------
      k=1
      do j=jstrv_nh,jendv_nh
      do i=istr_nh,iend_nh
# ifdef MASKING
         if (vmask(i,j).ne.0) then
# endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,2)  = nzuvw_n
            mijk2lmom_nh(i,j,k,2) = mijk2lmom_nh(i,j,k,2) + 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
# ifdef MASKING
         endif
# endif
      enddo
      enddo

      neqv_nh(3) = nzuvw_n

!-------------------------------------------------------------------
!     (V) Inner domain, inner layers: (i,j, 1<k<N )
!-------------------------------------------------------------------
      do j=jstrv_nh,jendv_nh
      do i=istr_nh,iend_nh
      do k=2,N-1 
#ifdef MASKING
         if (vmask(i,j).ne.0) then
#endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,2)  = nzuvw_n
            mijk2lmom_nh(i,j,k,2) = mijk2lmom_nh(i,j,k,2) + 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
#ifdef MASKING
         endif
#endif
      enddo
      enddo
      enddo

      neqv_nh(4) = nzuvw_n

!-------------------------------------------------------------------
!     (V) Inner domain, surface layer: (i,j,k=N )
!-------------------------------------------------------------------
      k=N
      do j=jstrv_nh,jendv_nh
      do i=istr_nh,iend_nh
#ifdef MASKING
         if (vmask(i,j).ne.0) then
#endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,2)  = nzuvw_n
            mijk2lmom_nh(i,j,k,2) = mijk2lmom_nh(i,j,k,2) + 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
#ifdef MASKING
         endif
#endif
      enddo
      enddo

      neqv_nh(5) = nzuvw_n

!-------------------------------------------------------------------
!     (V) NORTHERN EDGE (MATRIX OBC)
!-------------------------------------------------------------------
# ifdef OBC_NH
#  ifdef MPI
      if (NORTHERN_EDGE) then
#  endif
        j=jendv_nh+1

        do i=istr_nh-1,iend_nh+1
        do k=1,N
#  ifdef MASKING
          if (vmask(i,j).ne.0) then
#  endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,2)  = nzuvw_n
            mijk2lmom_nh(i,j,k,2) = mijk2lmom_nh(i,j,k,2) + 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
          endif
#  endif
        enddo
        enddo
#  ifdef MPI
      endif
#  endif
# endif /* OBC_NH */

      neqv_nh(6) = nzuvw_n

!-------------------------------------------------------------------
!     (V) EASTERN EDGE (tangential velocity)
!-------------------------------------------------------------------
# if defined OBC_NBQ  && defined OBC_EAST
#  ifdef MPI
      if (EASTERN_EDGE) then
         i = iend_nh+1

!........Corner (South-East):
         if (SOUTH_INTER_NBQ) then
            jstr_n = jstrv_nh-1
         else
            jstr_n = jstrv_nh
         endif
!........Corner (North-East):
         if (NORTH_INTER_NBQ) then
            jend_n = jendv_nh+1
         else
            jend_n = jendv_nh
         endif
#  else
         i = iend_nh+1
         jstr_n = jstrv_nh
         jend_n = jendv_nh
#  endif /* MPI */

         do j=jstr_n,jend_n
         do k=1,N    
#  ifdef MASKING
            if (vmask(i,j).ne.0) then
#  endif
               nzuvw_n               = nzuvw_n + 1 
               ijk2lmom_nh(i,j,k,2)  = nzuvw_n
               mijk2lmom_nh(i,j,k,2) = mijk2lmom_nh(i,j,k,2) + 1 
               l2imom_nh(nzuvw_n)    = i
               l2jmom_nh(nzuvw_n)    = j
               l2kmom_nh(nzuvw_n)    = k
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
!     (V) NORTHERN EDGE
!-------------------------------------------------------------------
# if defined OBC_NBQ && ! defined OBC_NH && !defined NS_PERIODIC  && defined OBC_NORTH
#  ifdef MPI
      if (NORTHERN_EDGE) then
#  endif
        j=jendv_nh+1

        do i=istr_nh-1,iend_nh+1
        do k=1,N
#  ifdef MASKING
          if (vmask(i,j).ne.0) then
#  endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,2)  = nzuvw_n
            mijk2lmom_nh(i,j,k,2) = mijk2lmom_nh(i,j,k,2) + 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
          endif
#  endif
        enddo
        enddo
#  ifdef MPI
      endif
#  endif
# endif /* OBC_NBQ */


# ifdef MPI
!-------------------------------------------------------------------
!     (V) EAST MPI Interface
!-------------------------------------------------------------------
      if (EAST_INTER_NBQ) then
         i        = iend_nh+1

!........Corner (South-East):
         if (SOUTH_INTER_NBQ) then
            jstr_n = jstrv_nh-1
         else
            jstr_n = jstrv_nh
         endif
!........Corner (North-East):
         if (NORTH_INTER_NBQ) then
            jend_n = jendv_nh+1
         else
            jend_n = jendv_nh
         endif

         do j=jstr_n,jend_n
         do k=1,N    
#  ifdef MASKING
            if (vmask(i,j).ne.0) then
#  endif
               nzuvw_n               = nzuvw_n + 1 
               ijk2lmom_nh(i,j,k,2)  = nzuvw_n
               mijk2lmom_nh(i,j,k,2) = mijk2lmom_nh(i,j,k,2) + 1 
               l2imom_nh(nzuvw_n)    = i
               l2jmom_nh(nzuvw_n)    = j
               l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo

      endif

!-------------------------------------------------------------------
!     (V) NORTH MPI Interface
!-------------------------------------------------------------------
      if (NORTH_INTER_NBQ) then
         j        = jendv_nh + 1

         do i=istr_nh,iend_nh
         do k=1,N

#  ifdef MASKING
            if (vmask(i,j).ne.0) then
#  endif
               nzuvw_n               = nzuvw_n + 1 
               ijk2lmom_nh(i,j,k,2)  = nzuvw_n
               mijk2lmom_nh(i,j,k,2) = mijk2lmom_nh(i,j,k,2) + 1 
               l2imom_nh(nzuvw_n)    = i
               l2jmom_nh(nzuvw_n)    = j
               l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif

         enddo
         enddo

      endif
# endif /* MPI */

      neqv_nh(7)   = nzuvw_n

!
!.....Number of equations in Y-direction:
!
      neqmom_nh(2) = nzuvw_n - neqmom_nh(1)

!*******************************************************************
!*******************************************************************
! Momentum Equation: Z-direction
!******************************************************************* 
!*******************************************************************

# ifdef MPI
!-------------------------------------------------------------------
!     (W) WEST MPI Interface
!-------------------------------------------------------------------
      if (WEST_INTER_NBQ) then
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

         do j=jstr_n,jend_n
         do k=0,N    
#  ifdef MASKING
            if (rmask(i,j).ne.0) then
#  endif
             nzuvw_n               = nzuvw_n + 1 
             ijk2lmom_nh(i,j,k,3)  = nzuvw_n
             mijk2lmom_nh(i,j,k,3) = mijk2lmom_nh(i,j,k,3) + 1 
             l2imom_nh(nzuvw_n)    = i
             l2jmom_nh(nzuvw_n)    = j
             l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo

      endif

!-------------------------------------------------------------------
!     (W) SOUTH MPI Interface
!-------------------------------------------------------------------
      if (SOUTH_INTER_NBQ) then
         j = jstr_nh-1

         do i=istr_nh,iend_nh
         do k=0,N    
#  ifdef MASKING
            if (rmask(i,j).ne.0) then
#  endif
             nzuvw_n               = nzuvw_n + 1 
             ijk2lmom_nh(i,j,k,3)  = nzuvw_n
             mijk2lmom_nh(i,j,k,3) = mijk2lmom_nh(i,j,k,3) + 1 
             l2imom_nh(nzuvw_n)    = i
             l2jmom_nh(nzuvw_n)    = j
             l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo

      endif
# endif /* MPI */

!-------------------------------------------------------------------
!     (W) WESTERN EDGE
!-------------------------------------------------------------------
# if defined OBC_NBQ  && defined OBC_WEST
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
        do k=0,N    
#  ifdef MASKING
          if (rmask(i,j).ne.0) then
#  endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,3)  = nzuvw_n
            mijk2lmom_nh(i,j,k,3) = mijk2lmom_nh(i,j,k,3) + 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
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
!     (W) SOUTHERN EDGE 
!-------------------------------------------------------------------
# if defined OBC_NBQ && !defined NS_PERIODIC  && defined OBC_SOUTH
#  ifdef MPI
      if (SOUTHERN_EDGE) then
#  endif
        j = jstr_nh-1

        do i=istr_nh-1,iend_nh+1
        do k=0,N    
#  ifdef MASKING
          if (rmask(i,j).ne.0) then
#  endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,3)  = nzuvw_n
            mijk2lmom_nh(i,j,k,3) = mijk2lmom_nh(i,j,k,3) + 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
          endif
#  endif
        enddo
        enddo
#  ifdef MPI
      endif
#  endif
# endif /* OBC_NBQ */

      neqw_nh(1)   = nzuvw_n
      neqw_nh(2)   = nzuvw_n
      neqw_nh(3)   = nzuvw_n

!-------------------------------------------------------------------
!     (W) Inner domain: (i,j, 0<=k<=N )
!
!       Caution: do not change the order of the loops,
!                k-loop must be inside
!-------------------------------------------------------------------
      do j=jstr_nh,jend_nh
      do i=istr_nh,iend_nh
      do k=0,N

# ifdef MASKING
        if (rmask(i,j).ne.0) then
# endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,3)  = nzuvw_n
            mijk2lmom_nh(i,j,k,3) = mijk2lmom_nh(i,j,k,3) + 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
# ifdef MASKING
        endif
# endif

      enddo
      enddo
      enddo

      neqw_nh(4)   = nzuvw_n
      neqw_nh(5)   = nzuvw_n
      neqw_nh(6)   = nzuvw_n

!-------------------------------------------------------------------
!     (W) EASTERN EDGE
!-------------------------------------------------------------------
# if defined OBC_NBQ  && defined OBC_EAST
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
        do k=0,N                 
#  ifdef MASKING
          if (rmask(i,j).ne.0) then
#  endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,3)  = nzuvw_n
            mijk2lmom_nh(i,j,k,3) = mijk2lmom_nh(i,j,k,3) + 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
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
!     (W) NORTHERN EDGE 
!-------------------------------------------------------------------
# if defined OBC_NBQ && !defined NS_PERIODIC && defined OBC_NORTH
#  ifdef MPI
      if (NORTHERN_EDGE) then
#  endif
        j = jend_nh+1

        do i=istr_nh-1,iend_nh+1
        do k=0,N    
#  ifdef MASKING
          if (rmask(i,j).ne.0) then
#  endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,3)  = nzuvw_n
            mijk2lmom_nh(i,j,k,3) = mijk2lmom_nh(i,j,k,3) + 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
          endif
#  endif
        enddo
        enddo
#  ifdef MPI
      endif
#  endif
# endif /* OBC_NBQ */


# ifdef MPI
!-------------------------------------------------------------------
!     (W) East MPI Interface
!-------------------------------------------------------------------
      if (EAST_INTER_NBQ) then
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

         do j=jstr_n,jend_n
         do k=0,N    
#  ifdef MASKING
            if (rmask(i,j).ne.0) then
#  endif
             nzuvw_n               = nzuvw_n + 1 
             ijk2lmom_nh(i,j,k,3)  = nzuvw_n
             mijk2lmom_nh(i,j,k,3) = mijk2lmom_nh(i,j,k,3) + 1 
             l2imom_nh(nzuvw_n)    = i
             l2jmom_nh(nzuvw_n)    = j
             l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo

      endif

!-------------------------------------------------------------------
!     (W) NORTH MPI Interface
!-------------------------------------------------------------------
      if (NORTH_INTER_NBQ) then
         j = jend_nh+1

         do i=istr_nh,iend_nh
         do k=0,N    
#  ifdef MASKING
            if (rmask(i,j).ne.0) then
#  endif
             nzuvw_n               = nzuvw_n + 1 
             ijk2lmom_nh(i,j,k,3)  = nzuvw_n
             mijk2lmom_nh(i,j,k,3) = mijk2lmom_nh(i,j,k,3) + 1 
             l2imom_nh(nzuvw_n)    = i
             l2jmom_nh(nzuvw_n)    = j
             l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo

      endif
# endif /* MPI */

      neqw_nh(7)   = nzuvw_n

      neqmom_nh(3) = nzuvw_n -(neqmom_nh(1)+neqmom_nh(2)) 
      neqmom_nh(0) = nzuvw_n


!*******************************************************************
!*******************************************************************
! Test Grid Definition
!******************************************************************* 
!*******************************************************************

      ierr_n = maxval(mijk2lmom_nh)
      return



      if (ierr_n.gt.1) then
# ifdef MPI
         call mpi_finalize(ierrmpi_n)
# endif
         stop 'Grid Error in numuvw_nh'
      endif

      return

      end subroutine numuvw_nh
 
#else
        subroutine numuvw_nh_empty
        return
        end 
#endif
