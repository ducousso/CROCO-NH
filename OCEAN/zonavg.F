! $Id: zonavg.F 972 2012-12-14 17:57:17Z marchesiello $
!
#include "cppdefs.h"
 
#ifdef ZONAL_NUDGING

      subroutine zonavg_2d (istr,iend,jstr,jend,A,Azon)
      implicit none
      integer istr,iend,jstr,jend

# include "param.h"
# include "scalars.h"

      real    A(GLOBAL_2D_ARRAY), Azon(0:Mm+1)
      real*8 Ad(GLOBAL_2D_ARRAY), Azond(0:Mm+1)
      integer i,j

!-- for parallelization (mean of A in xi direction)
#ifdef MPI
      include 'mpif.h'
      integer ierr, lenode
      real*8 allsumd(0:Mm+1,NNODES)
#endif
  
      Ad = A
      
      do j=jstr-1,jend+1
         Azond(j)=sum(Ad(istr:iend,j))
      enddo

#ifdef MPI
      call MPI_ALLGATHER(Azond,(Mm+2),MPI_DOUBLE_PRECISION,
     &                   allsumd,(Mm+2),MPI_DOUBLE_PRECISION,
     &                   MPI_COMM_WORLD,ierr)

      Azond=0.
      do i=1,NP_XI
         lenode=jj*NP_XI + i
         do j=jstr-1,jend+1
            Azond(j)=Azond(j)+allsumd(j,lenode)
         enddo
      enddo
#endif /* MPI */      

      Azon=Azond/float(LLm)
      
!     rewind (80+mynode)
!     write(80+mynode) Azon
!     call crash
      
      return
      end

!===================================================================

      subroutine zonavg_3d (istr,iend,jstr,jend,A,Azon)
      implicit none
      integer istr,iend,jstr,jend

# include "param.h"
# include "scalars.h"

      real    A(GLOBAL_2D_ARRAY,N), Azon(0:Mm+1,N)
      real*8 Ad(GLOBAL_2D_ARRAY,N), Azond(0:Mm+1,N)
      integer i,j,k

!-- for parallelization (mean of A in xi direction)
#ifdef MPI
      include 'mpif.h'
      integer ierr, lenode
      real*8 allsumd(0:Mm+1,N,NNODES)
#endif
  
      Ad = A
      
      do k=1,N
         do j=jstr-1,jend+1
               Azond(j,k)=sum(Ad(istr:iend,j,k))
         enddo
      enddo

#ifdef MPI
      call MPI_ALLGATHER(Azond,(Mm+2)*N,MPI_DOUBLE_PRECISION,
     &                   allsumd,(Mm+2)*N,MPI_DOUBLE_PRECISION,
     &                   MPI_COMM_WORLD,ierr)

      Azond=0.
      do i=1,NP_XI
         lenode=jj*NP_XI + i
         do k=1,N
            do j=jstr-1,jend+1
               Azond(j,k)=Azond(j,k)+allsumd(j,k,lenode)
            enddo
         enddo
      enddo
#endif /* MPI */      

      Azon=Azond/float(LLm)
      
!     rewind (80+mynode)
!     write(80+mynode) Azon
!     call crash
      
#else
      subroutine zonavg_empty
#endif /* ZONAL_NUDGING */
      return
      end



