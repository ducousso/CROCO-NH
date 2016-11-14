! $Id: setup_grid2.F 1458 2014-02-03 15:01:25Z gcambon $
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
      subroutine setup_grid2 (tile)
!     implicit none
      integer tile
#include "param.h"
#include "scalars.h"
#include "mpi_roms.h"
#ifdef MPI
      include 'mpif.h'
#endif
#include "compute_tile_bounds.h"
      call setup_grid2_tile (Istr,Iend,Jstr,Jend)
      return
      end

      subroutine setup_grid2_tile (Istr,Iend,Jstr,Jend)
!
! Setting up curvilinear grid: Compute minimum and maximum depth of
! model topography; minimum andmaximumhorizontal areas of grid boxes
! around h-points; minimum and maximum values of Courant number for
! barotropic mode; combined crossection of all open boundaries. 
!
!     implicit none
      integer Istr,Iend,Jstr,Jend, i,j, NSUB
      real cff, my_hmax, my_grdmax, my_Cu_max
     &        , my_hmin, my_grdmin, my_Cu_min
     &        , my_lonmin, my_lonmax
     &        , my_latmin, my_latmax

      real*QUAD my_volume, my_crss
#include "param.h"
#include "scalars.h"
#include "grid.h"
#include "mpi_roms.h"
#ifdef MPI
      include 'mpif.h'
      integer size, step, status(MPI_STATUS_SIZE), ierr
      real*8 buff(14)
# if QUAD == 16
      real*QUAD buff_vol, buff_crss
      pointer (i_buff11, buff_vol), (i_buff13, buff_crss)
# endif
#endif
!
#include "compute_auxiliary_bounds.h"
!
#ifdef SPHERICAL
      my_lonmin=+1.E+20
      my_lonmax=-1.E+20
      my_latmin=+1.E+20
      my_latmax=-1.E+20
#endif

      my_hmin=+1.E+20              ! Here two-dimensional Courant
      my_hmax=-1.E+20              ! number is defined as
      my_grdmin=+1.E+20            !
      my_grdmax=-1.E+20            !    Cu = c*dt*( 1/dx + 1/dy )
      my_Cu_min=+1.E+20            !
      my_Cu_max=-1.E+20            ! where c=sqrt(g*h) is phase
                                   ! speed for barotropic mode,
      do j=JstrR,JendR             ! and dx,dy are grid spacings
        do i=IstrR,IendR           ! in each direction. 

            my_hmin=min(my_hmin, h(i,j))
            my_hmax=max(my_hmax, h(i,j))
          
#ifdef MASKING
          if (rmask(i,j).gt.0.) then            
#endif          
            cff=1./sqrt(pm(i,j)*pn(i,j))
            my_grdmin=min(my_grdmin, cff)
            my_grdmax=max(my_grdmax, cff)

            cff=dtfast*sqrt( g*max(h(i,j),0.)*( pm(i,j)*pm(i,j)
     &                                 +pn(i,j)*pn(i,j) ))

            my_Cu_min=min(my_Cu_min, cff)
            my_Cu_max=max(my_Cu_max, cff)
#ifdef MASKING
          endif
#endif

#ifdef SPHERICAL
!
! Get minima and maxima for longitude and latitude
! Warning: for openmp we are using the extremma for the global grid 
!          for mpi we are using the values of the local subgrids
! used for online interpolations

            my_lonmin=min(my_lonmin, lonr(i,j))
            my_lonmax=max(my_lonmax, lonr(i,j))
            my_latmin=min(my_latmin, latr(i,j))
            my_latmax=max(my_latmax, latr(i,j))
#endif

        enddo
      enddo
!
! Compute volume of unperturbed grid:
!
      my_volume=QuadZero
      do j=Jstr,Jend
        do i=Istr,Iend
          my_volume=my_volume+h(i,j)/(pm(i,j)*pn(i,j))
        enddo
      enddo
      
!
! Compute integral crossections of all open boundaries. 
!
      my_crss=QuadZero
#ifdef OBC_WEST
      if (WESTERN_EDGE) then
        do j=Jstr,Jend
          my_crss=my_crss+0.5*(h(Istr-1,j)+h(Istr,j))*on_u(Istr,j)
# ifdef MASKING
     &                                                *umask(Istr,j)
# endif
        enddo
      endif
#endif
#ifdef OBC_EAST
      if (EASTERN_EDGE) then
        do j=Jstr,Jend
          my_crss=my_crss+0.5*(h(Iend,j)+h(Iend+1,j))*on_u(Iend+1,j)
# ifdef MASKING
     &                                              *umask(Iend+1,j)
# endif
        enddo
      endif
#endif
#ifdef OBC_SOUTH
      if (SOUTHERN_EDGE) then
        do i=Istr,Iend
          my_crss=my_crss+0.5*(h(i,Jstr)+h(i,Jstr-1))*om_v(i,Jstr)
# ifdef MASKING
     &                                               *vmask(i,Jstr)
# endif
        enddo
      endif
#endif
#ifdef OBC_NORTH
      if (NORTHERN_EDGE) then
        do i=Istr,Iend
          my_crss=my_crss+0.5*(h(i,Jend)+h(i,Jend+1))*om_v(i,Jend+1)
# ifdef MASKING
     &                                              *vmask(i,Jend+1)
# endif
        enddo
      endif
#endif
!
! Thus far everything has ben computed within the tile. Next: combine
! the results to get global min/max and summations. This needs to be
! done in two stages, first among all tiles which belong to the same
! shared memory group (MPI-node); within each MPI-node, then across
! MPI nodes (Reduce--Broadcast sequence).
!
      if (SINGLE_TILE_MODE) then
        NSUB=1
      else
        NSUB=NSUB_X*NSUB_E
      endif
        
C$OMP CRITICAL (grd2_cr_rgn)          ! Global MIN/MAX operations
        hmin=min(hmin, my_hmin)       ! within each shared memory
        hmax=max(hmax, my_hmax)       ! group (an MPI-node). 
#ifdef SPHERICAL
        lonmin=min(lonmin, my_lonmin)
        lonmax=max(lonmax, my_lonmax)
        latmin=min(latmin, my_latmin)
        latmax=max(latmax, my_latmax)
#endif
        grdmin=min(grdmin, my_grdmin)
        grdmax=max(grdmax, my_grdmax)
        Cu_min=min(Cu_min, my_Cu_min)
        Cu_max=max(Cu_max, my_Cu_max) ! Counter tile_count identifies
        volume=volume+my_volume       ! the last thread (within each
        bc_crss=bc_crss+my_crss       ! MPI-process) leaving critical
                                      ! region. This thread (which is
        tile_count=tile_count+1       ! not necessarily master thread 
        if (tile_count.eq.NSUB) then  ! within its MPI-process) is 
          tile_count=0                ! responsible for communication 
# ifdef MPI
#  if QUAD == 16
          i_buff11=loc(buff(11))      ! exchange between MPI-nodes.
          i_buff13=loc(buff(13))      ! Here each real*QUAD number is
#  endif
          size=NNODES                 ! stored as two real*8 words.
  1        step=(size+1)/2
            if (mynode.ge.step .and. mynode.lt.size) then
              buff(1)=hmin
              buff(2)=hmax            ! This is similar MPI_Reduce
              buff(3)=grdmin          ! MIN or MAX operation, except
              buff(4)=grdmax          ! that MIN/MAX and two real*QUAD
              buff(5)=Cu_min          ! summations come together.
              buff(6)=Cu_max
#  ifdef SPHERICAL
              buff(7)=lonmin
              buff(8)=lonmax
              buff(9)=latmin
              buff(10)=latmax       
#  endif
#  if QUAD == 16
              buff_vol=volume
              buff_crss=bc_crss
#  else
              buff(11)=volume
              buff(12)=bc_crss
#  endif
              call MPI_Send (buff,  14, MPI_DOUBLE_PRECISION,
     &             mynode-step, 17, MPI_COMM_WORLD,      ierr)
            elseif (mynode .lt. size-step) then
              call MPI_Recv (buff,  14, MPI_DOUBLE_PRECISION,
     &             mynode+step, 17, MPI_COMM_WORLD, status, ierr)
              hmin=  min(hmin,   buff(1))
              hmax=  max(hmax,   buff(2))
              grdmin=min(grdmin, buff(3))
              grdmax=max(grdmax, buff(4))
              Cu_min=min(Cu_min, buff(5))
              Cu_max=max(Cu_max, buff(6))
#  ifdef SPHERICAL
              lonmin=min(lonmin,buff(7))
              lonmax=max(lonmax,buff(8))
              latmin=min(latmin,buff(9))
              latmax=max(latmax,buff(10))     
#  endif
#  if QUAD == 16
              volume=volume + buff_vol
              bc_crss=bc_crss + buff_crss
#  else
              volume=volume + buff(11) 
               bc_crss=bc_crss + buff(12)
#  endif
            endif
           size=step
          if (size.gt.1) goto 1

          buff(1)=hmin
          buff(2)=hmax
          buff(3)=grdmin
          buff(4)=grdmax
          buff(5)=Cu_min
          buff(6)=Cu_max
#  ifdef SPHERICAL
          buff(7)=lonmin
          buff(8)=lonmax
          buff(9)=latmin
          buff(10)=latmax       
#  endif
#  if QUAD == 16
          buff_vol=volume
          buff_crss=bc_crss
#  else
          buff(11)=volume
          buff(12)=bc_crss
#  endif
          call MPI_Bcast(buff, 14, MPI_DOUBLE_PRECISION,
     &                          0, MPI_COMM_WORLD, ierr)
          hmin=  buff(1)
          hmax=  buff(2)
          grdmin=buff(3)
          grdmax=buff(4)
          Cu_min=buff(5)
          Cu_max=buff(6)
#  ifdef SPHERICAL
          lonmin=buff(7)
          lonmax=buff(8)
          latmin=buff(9)
          latmax=buff(10)       
#  endif
#  if QUAD == 16
          volume=buff_vol
          bc_crss=buff_crss
#  else
          volume=buff(11)
          bc_crss=buff(12)
#  endif
# endif

c**       write(*,'(I4,F12.6,F13.6,2(1x,E12.7),2F12.8)')
c**  &         mynode, hmin,hmax, grdmin,grdmax, Cu_min,Cu_max
c**       write(*,*) mynode, ' crss =',bc_crss, 'vol =',volume

          MPI_master_only write( stdout,
     &        '(/1x,A,8x,A,9x,A,9x,A,9x,A,6x,A)')
     &        'hmin','hmax','grdmin','grdmax','Cu_min','Cu_max'
          MPI_master_only write( stdout,
     &        '(F12.6,F13.6,2(1x,E14.9),2F12.8)')
     &         hmin,  hmax,  grdmin,  grdmax,  Cu_min,  Cu_max
          MPI_master_only write( stdout,'(2(3x,A,1PE27.21)/)')
     &        'volume=', volume, 'open_cross=', bc_crss

#ifdef SPHERICAL
          MPI_master_only write(stdout,
     &        '(5x,A,F6.2,A,F6.2,A,F6.2,A,F6.2/)')
     &        '  lonmin = ', lonmin, '  lonmax = ', lonmax,
     &        '  latmin = ', latmin, '  latmax = ', latmax
#endif

        endif
C$OMP END CRITICAL (grd2_cr_rgn)

      return
      end

