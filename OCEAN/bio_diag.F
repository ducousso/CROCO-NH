! $Id: bio_diag.F 1458 2014-02-03 15:01:25Z gcambon $
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
#ifdef BIOLOGY
      subroutine bio_diag (tile)
      implicit none
# include "param.h"
      integer tile
# include "compute_tile_bounds.h"
      call bio_diag_tile  (Istr,Iend,Jstr,Jend)
      return
      end

      subroutine bio_diag_tile (Istr,Iend,Jstr,Jend)
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
# include "mpi_roms.h"
      integer Istr,Iend,Jstr,Jend, i,j,k,itrc, iocheck,
     &                             trd,omp_get_thread_num
      integer my_itrc(1:4)
!
!
      real cff1, NSUB
      real*QUAD cff, my_sum(0:4)
#ifdef MPI
      include 'mpif.h'
      integer size, step, status(MPI_STATUS_SIZE), ierr
      real*QUAD buff(0:4)
      common /xyz_bio/ buff
#endif

# ifdef BIO_NChlPZD
      my_itrc(1)=iNO3_
      my_itrc(2)=iPhy1
      my_itrc(3)=iZoo1
      my_itrc(4)=iDet1
# elif defined BIO_N2ChlPZD2
      my_itrc(1)=iNO3_
      my_itrc(2)=iPhy2
      my_itrc(3)=iZoo2
      my_itrc(4)=iDet2
# elif defined BIO_BioEBUS
      my_itrc(1)=iNO3_
      my_itrc(2)=iPhy2
      my_itrc(3)=iZoo2
      my_itrc(4)=iDet2
# elif defined PISCES
      my_itrc(1)=iNO3_
      my_itrc(2)=iDIA_
      my_itrc(3)=iZOO_
      my_itrc(4)=iDOC_
# endif

      if (mod(iic-1,ninfo).eq.0) then
        do itrc=0,4
          my_sum(itrc)=QuadZero          ! <-- Reset local sums
        enddo

        do j=Jstr,Jend
         do i=Istr,Iend
# ifdef MASKING
           cff1=rmask(i,j)/(pm(i,j)*pn(i,j))  ! <-- grid box area
# else
           cff1=1./(pm(i,j)*pn(i,j))
# endif
           do k=1,N
            cff=cff1*Hz(i,j,k)       ! <-- volume of grid box(i,j,k) 
            my_sum(0)=my_sum(0)+cff  ! <-- accumulate volume
             do itrc=1,4
              my_sum(itrc)=my_sum(itrc)+cff*t(i,j,k,nstp,my_itrc(itrc))
             enddo
           enddo
         enddo
        enddo

!
! Perform global summation: whoever gets first to the critical region
! resets global sums before global summation starts; after the global
! summation is completed, thread, which is the last one to enter the
! critical region, finalizes the computation of diagnostics and
! prints them out. 
!
        if (SINGLE_TILE_MODE) then
          NSUB=1
        else
          NSUB=NSUB_X*NSUB_E
        endif

C$OMP CRITICAL (bio_cr_rgn)
          if (bio_count.eq.0) then
            do itrc=0,4
              global_sum(itrc)=QuadZero   ! <-- Reset global sums
            enddo
          endif 
                                       !-->  Perform global summation
          do itrc=0,4
            global_sum(itrc)=global_sum(itrc)+my_sum(itrc)
          enddo

          bio_count=bio_count+1             ! This counter identifies
          if (bio_count.eq.NSUB) then      ! the last thread, whoever
            bio_count=0                    ! it is, not always master.
# ifdef MPI
            if (NNODES.gt.1) then         ! Perform global summation 
              size=NNODES                 ! among MPI processes
   10          step=(size+1)/2
                if (mynode.ge.step .and. mynode.lt.size) then
                  do itrc=0,4
                    buff(itrc)=global_sum(itrc)  ! This is MPI_Reduce
                  enddo
                  call MPI_Send (buff, 5, MPI_DOUBLE_PRECISION,
     &                 mynode-step, 17, MPI_COMM_WORLD,      ierr)
                elseif (mynode .lt. size-step) then
                  call MPI_Recv (buff, 5, MPI_DOUBLE_PRECISION,
     &                 mynode+step, 17, MPI_COMM_WORLD, status, ierr)
                  do itrc=0,4
                    global_sum(itrc)=global_sum(itrc)+buff(itrc)
                  enddo
                endif
               size=step
              if (size.gt.1) goto 10
            endif
            if (mynode.eq.0) then
# endif
            trd=omp_get_thread_num()

            cff=1./global_sum(0)
            do itrc=1,4
              global_sum(itrc)=cff*global_sum(itrc)
            enddo
            if (first_time.eq.0) then
              first_time=1
# ifdef BIO_NChlPZD
              write(stdout,1) 'STEP','time[DAYS]','       NO3',
     &                   '     PHYTO','       ZOO','    DETRIT',
     &                   'trd'
# elif defined BIO_N2ChlPZD2
              write(stdout,1) 'STEP','time[DAYS]','       NO3',
     &                   '    PHY2 ','    ZOO2 ', '  DET2',
     &                   'trd'
# elif defined BIO_BioEBUS
              write(stdout,1) 'STEP','time[DAYS]','       NO3',
     &                   '    PHY2 ','    ZOO2 ', '  DET2',
     &                   'trd'
# elif defined PISCES
              write(stdout,1) 'STEP','time[DAYS]','       NO3',
     &                   '      DIA','     ZOO ', '   DOC',
     &                   'trd'
# endif
   1          format(1x,A4,3x,A10,1x,A10,4x,A10,4x,A10,4x,A10,3x,A3)

            endif


            write(stdout,2) iic-1,tdays, 
     &                      global_sum(1),  
     &                      global_sum(2), 
     &                      global_sum(3),  
     &                      global_sum(4) 
     &                    , trd
   2        format(I6, F12.5, 4(1PE14.7), I3)
# ifdef MPI
            endif    ! <-- mynode.eq.0
# endif
          endif
C$OMP END CRITICAL (bio_cr_rgn)
      endif
      return
      end
#else
      subroutine bio_diag_empty
      return
      end
#endif /* BIOLOGY */

