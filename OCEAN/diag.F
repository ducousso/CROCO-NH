! $Id: diag.F 1458 2014-02-03 15:01:25Z gcambon $
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

      subroutine diag (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
#include "param.h"
#include "private_scratch.h"
#include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call diag_tile (Istr,Iend,Jstr,Jend, A2d(1,1,trd),A2d(1,2,trd))
      return
      end

      subroutine diag_tile (Istr,Iend,Jstr,Jend, ke2d,pe2d)
!     implicit none
#include "param.h" 
#include "grid.h"
#ifdef SOLVE3D
# include "ocean3d.h"
#else
# include "ocean2d.h"
#endif
#include "scalars.h"
#include "mpi_roms.h"
      integer Istr,Iend,Jstr,Jend,    i,j,k, NSUB,
     &                                trd, omp_get_thread_num
      real ke2d(PRIVATE_2D_SCRATCH_ARRAY),
     &     pe2d(PRIVATE_2D_SCRATCH_ARRAY)
      real*QUAD cff, my_avgke, my_avgpe, my_volume
      character echar*8
#ifdef MPI
      include 'mpif.h'
      integer size, step, status(MPI_STATUS_SIZE), ierr
      real*QUAD buff(6)
      common /xyz/ buff
#endif

#ifdef ETALON_CHECK
      integer ncheck, nparam, iocheck
      parameter (ncheck=6, nparam=4)
      integer icheck, check_point(ncheck)
      character*60 check_line, etalon_line(ncheck)
      real    A0(nparam), A1(nparam)
      integer P0(nparam), P1(nparam)
#if defined BASIN
#  include "etalon_data.h.Basin"
# elif defined CANYON_A
#  include "etalon_data.h.Canyon_A"
# elif defined CANYON_B
#  include "etalon_data.h.Canyon_B"
# elif defined EQUATOR
#  include "etalon_data.h.Equator"
# elif defined GRAV_ADJ
#  include "etalon_data.h.Grav_adj"
# elif defined INNERSHELF
#  include "etalon_data.h.Innershelf"
# elif defined RIVER
#  include "etalon_data.h.River"
# elif defined SEAMOUNT
#  include "etalon_data.h.Seamount"
# elif defined SHELFRONT
#  include "etalon_data.h.Shelfront"
# elif defined SOLITON
#  include "etalon_data.h.Soliton"
# elif defined OVERFLOW
#  include "etalon_data.h.Overflow"
# elif defined UPWELLING
#  include "etalon_data.h.Upwelling"
# elif defined VORTEX
#  include "etalon_data.h.Vortex"
# elif defined JET
#  include "etalon_data.h.Jet"
!#  if defined AGRIF && defined AGRIF_2WAY
!#      include "etalon_data.h.VortexAgrif2W"
!#  elif defined AGRIF && !defined AGRIF_2WAY
!#       include "etalon_data.h.VortexAgrif"
!#  elif !defined AGRIF 
!#   include "etalon_data.h.Vortex"
!#  endif
# else
#  include "etalon_data.h"
# endif
#endif

!
! Compute and report volume averaged kinetic, potential and total
!-----------------------------------------------------------------
! energy densities for either two- (shallow water) or three-
! dimensional versions of the model.
!
! At first, compute kinetic and potential energies, as well as total
! volume within the tile [subdomain of indices (Istr:Iend,Jstr:Jend)]
! by individual threads. In the case of three dimensions also perform
! verical summation at this stage.
!
      if (mod(iic-1,ninfo).eq.0) then
        do j=Jstr,Jend
#ifdef SOLVE3D
          do i=Istr,Iend
            ke2d(i,j)=0.
            pe2d(i,j)=0.5*g*z_w(i,j,N)*z_w(i,j,N)
                                     !!!zeta(i,j,kstp)*zeta(i,j,kstp)
          enddo
          cff=g/rho0
          do k=N,1,-1
            do i=Istr,Iend
             ke2d(i,j)=ke2d(i,j)+HZR(i,j,k)*0.25*(
     &                               u(i  ,j,k,nstp)*u(i,j,k,nstp)+
     &                               u(i+1,j,k,nstp)*u(i+1,j,k,nstp)+
     &                               v(i,j  ,k,nstp)*v(i,j  ,k,nstp)+
     &                               v(i,j+1,k,nstp)*v(i,j+1,k,nstp))
             pe2d(i,j)=pe2d(i,j)+cff*HZR(i,j,k)*rho(i,j,k)
     &                                       *(z_r(i,j,k)-z_w(i,j,0))
            enddo
          enddo
#else
          cff=0.5*g
          do i=Istr,Iend
            ke2d(i,j)=(zeta(i,j,krhs)+h(i,j))*0.25*(
     &                             ubar(i  ,j,krhs)*ubar(i  ,j,krhs)+
     &                             ubar(i+1,j,krhs)*ubar(i+1,j,krhs)+
     &                             vbar(i,j  ,krhs)*vbar(i,j  ,krhs)+
     &                             vbar(i,j+1,krhs)*vbar(i,j+1,krhs))
            pe2d(i,j)=cff*zeta(i,j,krhs)*zeta(i,j,krhs)
          enddo
#endif /* SOLVE3D */
        enddo
!
! After that integrate horizontally within the subdomain tile. Here,
! in order to reduce the round-off errors, the summation is performed
! in two stages, first the index j is collapsed, then in index i.
! In this order the partial sums consist on much fewer number of
! terms than if it would be the case of a straightforward two-
! dimensional summation. Thus adding numbers which are orders of
! magnitude apart is avoided. Also note that the partial sums are
! stored as quadro precision numbers for the same purpose.
!
        do i=Istr,Iend
          pe2d(i,Jend+1)=0.D0
          pe2d(i,Jstr-1)=0.D0
          ke2d(i,Jstr-1)=0.D0
        enddo
        do j=Jstr,Jend
          do i=Istr,Iend
            cff=1./(pm(i,j)*pn(i,j))
#ifdef SOLVE3D
            pe2d(i,Jend+1)=pe2d(i,Jend+1)+cff*(z_w(i,j,N)-z_w(i,j,0))
#else
            pe2d(i,Jend+1)=pe2d(i,Jend+1)+cff*(zeta(i,j,krhs)+h(i,j))
#endif
            pe2d(i,Jstr-1)=pe2d(i,Jstr-1)+cff*pe2d(i,j)
            ke2d(i,Jstr-1)=ke2d(i,Jstr-1)+cff*ke2d(i,j)

          enddo
        enddo

        my_volume=0.
        my_avgpe=0.
        my_avgke=0.
        do i=Istr,Iend
          my_volume=my_volume+pe2d(i,Jend+1)
          my_avgpe =my_avgpe +pe2d(i,Jstr-1)
          my_avgke =my_avgke +ke2d(i,Jstr-1)
        enddo
        if (SINGLE_TILE_MODE) then
          NSUB=1
        else
          NSUB=NSUB_X*NSUB_E
        endif
!
! Perform global summation: whoever gets first to the critical region
! resets global sums before global summation starts; after the global
! summation is completed, thread, which is the last one to enter the
! critical region, finalizes the computation of diagnostics and
! prints them out. 
!
C$OMP CRITICAL (diag_cr_rgn)
          if (tile_count.eq.0) then
            volume=QuadZero               ! <-- Reset global sums for
            avgke= QuadZero               ! <-- multithreaded (shared
            avgpe= QuadZero               ! <-- memory) summation.
          endif 
          volume=volume+my_volume         ! Perform global 
          avgke =avgke +my_avgke          ! summation among 
          avgpe =avgpe +my_avgpe          ! the threads

          tile_count=tile_count+1         ! This counter identifies
          if (tile_count.eq.NSUB) then    ! the last thread, whoever
            tile_count=0                  ! it is, not always master.
#ifdef MPI
            if (NNODES.gt.1) then         ! Perform global summation 
              size=NNODES                 ! among MPI processes
   1           step=(size+1)/2 
                if (mynode.ge.step .and. mynode.lt.size) then
                  buff(1)=volume
                  buff(2)=avgke           ! This is MPI_Reduce
                  buff(3)=avgpe
                  call MPI_Send (buff,  6, MPI_DOUBLE_PRECISION,
     &                 mynode-step, 17, MPI_COMM_WORLD,      ierr)
                elseif (mynode .lt. size-step) then
                  call MPI_Recv (buff,  6, MPI_DOUBLE_PRECISION,
     &                 mynode+step, 17, MPI_COMM_WORLD, status, ierr)
                  volume=volume+buff(1)
                  avgke=avgke+  buff(2)
                  avgpe=avgpe+  buff(3)
                endif
               size=step
              if (size.gt.1) goto 1
            endif
            if (mynode.eq.0) then
#endif

              avgke=avgke/volume          ! Compute and print global
              avgpe=avgpe/volume          ! diagnostics (last thread
              avgkp=avgke+avgpe           ! of master MPI process)

              if (first_time.eq.0) then
                first_time=1
                write(stdout,2) 'STEP','time[DAYS]','KINETIC_ENRG',
     &                  'POTEN_ENRG','TOTAL_ENRG','NET_VOLUME','trd'
   2            format(1x,A4,3x,A10,1x,A12,4x,A10,4x,A10,4x,A10,3x,A3)
              endif
              trd=omp_get_thread_num()
              write(stdout,3)iic-1,tdays,avgke,avgpe,avgkp,volume,trd
   3          format(I8, F12.5, 1PE16.9, 3(1PE14.7), I3)
!
! Raise may_day_flag to stop computations in the case of blowing up.
! [Criterion for blowing up here is the numerical overflow, so that
! avgkp is 'INF' or 'NAN' (any mix of lover and uppercase letters),
! therefore it is sufficient to check for the presence of letter 'N'.
!
              write(echar,'(1PE8.1)') avgkp
              do i=1,8
               if (echar(i:i).eq.'N' .or. echar(i:i).eq.'n'
     &                      .or. echar(i:i).eq.'*') may_day_flag=1 
              enddo
#ifdef  ETALON_CHECK
              do icheck=1,ncheck
                if (iic-1.eq.check_point(icheck)) then
                  write(check_line,'(1PE15.9,3(1PE14.7))',
     &                iostat=iocheck) avgke,avgpe, avgkp,volume
                  if (check_line .eq. etalon_line(icheck)) then
                    write(stdout,*) 'PASSED_ETALON_CHECK'
                  else
                    read(check_line         ,4,iostat=iocheck)
     &                                   (A1(i), P1(i), i=1,nparam)
                    read(etalon_line(icheck),4,iostat=iocheck)
     &                                   (A0(i), P0(i), i=1,nparam)
   4                format (F11.9,1x,I3,3(F10.7,1x,I3))
                    do i=1,nparam
                      A1(i)=A1(i)-A0(i)*10.**float(P0(i)-P1(i))
                    enddo
                    write(check_line, '(F18.9,3F14.7)',
     &                          iostat=iocheck) (A1(i), i=1,nparam)
                    j=0
                    do i=2,60
                      if (check_line(i-1:i).eq.'0.') then
                        check_line(i:i)=':'
                        j=1
                      elseif (j.eq.1.and.check_line(i:i).eq.'0') then
                        check_line(i:i)='.'
                      else
                        j=0
                      endif
                    enddo
                    write(stdout,'(1x,2A/1x,A)') 'difference:',
     &               check_line, 'ETALON_CHECK: SOMETHING_IS_WRONG'
                  endif
                endif
              enddo
#endif
#ifdef MPI
            endif    ! <-- mynode.eq.0
#endif
/*
            if (ninfo.eq.1 .and. iic.gt.   8) ninfo=2
            if (ninfo.eq.2  .and. iic.gt. 16) ninfo=4
            if (ninfo.eq.4  .and. iic.gt. 32) ninfo=8
            if (ninfo.eq.8  .and. iic.gt. 64) ninfo=16
            if (ninfo.eq.16 .and. iic.gt.128) ninfo=32
            if (ninfo.eq.32 .and. iic.gt.256) ninfo=64
*/
          endif
	  
#if defined MPI && defined DEBUG && !defined AGRIF
      call flush
#endif
	  
C$OMP END CRITICAL (diag_cr_rgn)
      endif
      return
      end

