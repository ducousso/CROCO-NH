! $Id: grid_stiffness.F 1458 2014-02-03 15:01:25Z gcambon $
!
!======================================================================
! CROCO is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! CROCO specific routines (nesting) are under CeCILL-C license.
! 
! CROCO website : http://www.croco-ocean.org
!======================================================================
!
#include "cppdefs.h"
#ifdef SOLVE3D

      subroutine grid_stiffness (tile)
      implicit none
# include "param.h"
      integer tile, trd
C$    integer omp_get_thread_num
# include "compute_tile_bounds.h"
      call grid_stiffness_tile (Istr,Iend,Jstr,Jend)
      return
      end

      subroutine grid_stiffness_tile (Istr,Iend,Jstr,Jend)
!
! Survey three-dimensional grid in order to determine maximum
! grid stiffness ratio:
!
!             z(i,j,k)-z(i-1,j,k)+z(i,j,k-1)-z(i-1,j,k-1)
!      r_x = ---------------------------------------------
!             z(i,j,k)+z(i-1,j,k)-z(i,j,k-1)-z(i-1,j,k-1)
!
! This is done for purely diagnostic purposes does not affect 
! computations.
!
!     implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k, NSUB
      real my_rx0, my_rx1
# ifdef MPI
      include 'mpif.h'
      real*8 buff(2)
      integer size, step, status(MPI_STATUS_SIZE), ierr
# endif
# include "param.h"
# include "scalars.h"
# include "ocean3d.h"
# include "mpi_roms.h"
# ifdef MASKING
#  include "grid.h"
# endif
!
# include "compute_auxiliary_bounds.h"
!
      my_rx0=0.
      my_rx1=0.

      do j=Jstr,Jend
        do i=IstrU,Iend
# ifdef MASKING
          if (umask(i,j).gt.0.) then
# endif
            my_rx0=max(my_rx0, abs( (z_w(i,j,0)-z_w(i-1,j,0))
     &                                   /(z_w(i,j,0)+z_w(i-1,j,0))
     &                                                            ))
            do k=1,N
              my_rx1=max(my_rx1, abs(
     &         (z_w(i,j,k)-z_w(i-1,j,k)+z_w(i,j,k-1)-z_w(i-1,j,k-1))
     &        /(z_w(i,j,k)+z_w(i-1,j,k)-z_w(i,j,k-1)-z_w(i-1,j,k-1))
     &                                                            ))
            enddo
# ifdef MASKING
          endif
# endif
        enddo
      enddo

      do j=JstrV,Jend
        do i=Istr,Iend
# ifdef MASKING
          if (vmask(i,j).gt.0.) then
# endif
            my_rx0=max(my_rx0, abs( (z_w(i,j,0)-z_w(i,j-1,0))
     &                                   /(z_w(i,j,0)+z_w(i,j-1,0))
     &                                                            ))
            do k=1,N
              my_rx1=max(my_rx1, abs(
     &         (z_w(i,j,k)-z_w(i,j-1,k)+z_w(i,j,k-1)-z_w(i,j-1,k-1))
     &        /(z_w(i,j,k)+z_w(i,j-1,k)-z_w(i,j,k-1)-z_w(i,j-1,k-1))
     &                                                            ))
            enddo
# ifdef MASKING
          endif
# endif
        enddo
      enddo

      if (SINGLE_TILE_MODE) then
        NSUB=1
      else
        NSUB=NSUB_X*NSUB_E
      endif
C$OMP CRITICAL (grd_stff_cr_rgn)
        if (tile_count.eq.0) then
          rx0=my_rx0
          rx1=my_rx1
        else
          rx0=max(rx0, my_rx0)
          rx1=max(rx1, my_rx1)
        endif
        tile_count=tile_count+1 
        if (tile_count.eq.NSUB) then
          tile_count=0 
# ifdef MPI
          size=NNODES
  1        step=(size+1)/2
            if (mynode.ge.step .and. mynode.lt.size) then
              buff(1)=rx0
              buff(2)=rx1
              call MPI_Send (buff, 2, MPI_DOUBLE_PRECISION,
     &               mynode-step, 17, MPI_COMM_WORLD,      ierr)
            elseif (mynode .lt. size-step) then
              call MPI_Recv (buff, 2, MPI_DOUBLE_PRECISION,
     &               mynode+step, 17, MPI_COMM_WORLD, status, ierr)
              rx0=max(rx0, buff(1))
              rx1=max(rx1, buff(2))
            endif
           size=step
          if (size.gt.1) goto 1

          buff(1)=rx0
          buff(2)=rx1
          call MPI_Bcast(buff, 2, MPI_DOUBLE_PRECISION,
     &                         0, MPI_COMM_WORLD, ierr)
          rx0=buff(1)
          rx1=buff(2)
# endif
          MPI_master_only write(stdout,'(/1x,A,F12.10,2x,A,F14.10/)')
     &     'Maximum grid stiffness ratios:   rx0 =',rx0, 'rx1 =',rx1
        endif
C$OMP END CRITICAL (grd_stff_cr_rgn)
      return
      end
#else
      subroutine grid_stiffness_empty
      end
#endif  /* SOLVE3D */

