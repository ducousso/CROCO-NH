! $Id: obc_volcons.F 1458 2014-02-03 15:01:25Z gcambon $
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
#ifdef OBC_VOLCONS

      subroutine obc_flux (tile)
      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call obc_flux_tile (Istr,Iend,Jstr,Jend)
      return
      end

      subroutine obc_flux_tile (Istr,Iend,Jstr,Jend)
!
! Compute integral mass flux "obc_flux" accross the open boundaries,
! which is needed to enforce global mass conservation constraint.
! The computation is done by first computing the integral crossection
! of- and fluxes across-  the segments of open boundaries within
! subdomains [tiles], and then making global summation over the
! subdomains. At the end, global integrated flux is divided by the
! global cross-section to compute the correction velocity "ubar_xs".
!
# ifdef AGRIF
      USE Agrif_Util
# endif
      implicit none
# include "param.h"
      integer Istr,Iend,Jstr,Jend,    i,j, NSUB
      real cff
      real*QUAD my_crss,my_flux
# include "grid.h"
# include "ocean2d.h"
# include "scalars.h"
# include "mpi_roms.h"
# ifdef MPI
      include 'mpif.h'
      integer size, step, status(MPI_STATUS_SIZE), ierr
      real*QUAD buff(2)
# endif
!
# include "compute_auxiliary_bounds.h"
!

#ifdef AGRIF
      IF (.NOT.Agrif_Root()) RETURN
#endif
      my_crss=QuadZero  ! Reset partial sums within the segments of open
      my_flux=QuadZero  ! boundaries, then compute partial cross-sections
                    ! and mass fluxes over the subdomains
# ifdef OBC_WEST
      if (WESTERN_EDGE) then
        do j=Jstr,Jend
          cff=0.5*(zeta(Istr-1,j,knew)+zeta(Istr,j,knew)+h(Istr-1,j)
     &                                       +h(Istr,j))*on_u(Istr,j)
#  ifdef MASKING
     &                                                 *umask(Istr,j)
#  endif
          my_crss=my_crss+cff
          my_flux=my_flux+cff*ubar(Istr,j,knew)
        enddo
      endif
# endif
# ifdef OBC_EAST
      if (EASTERN_EDGE) then
        do j=Jstr,Jend
          cff=0.5*(zeta(Iend,j,knew)+zeta(Iend+1,j,knew)+h(Iend,j)
     &                                 +h(Iend+1,j))*on_u(Iend+1,j)
#  ifdef MASKING
     &                                             *umask(Iend+1,j)
#  endif
          my_crss=my_crss+cff
          my_flux=my_flux-cff*ubar(Iend+1,j,knew)
        enddo
      endif
# endif
# ifdef OBC_SOUTH
      if (SOUTHERN_EDGE) then
        do i=Istr,Iend
          cff=0.5*(zeta(i,Jstr,knew)+zeta(i,Jstr-1,knew)+h(i,Jstr)
     &                                   +h(i,Jstr-1))*om_v(i,Jstr)
#  ifdef MASKING
     &                                               *vmask(i,Jstr)
#  endif
          my_crss=my_crss+cff
          my_flux=my_flux+cff*vbar(i,JstrV-1,knew)
        enddo
      endif
# endif
# ifdef OBC_NORTH
      if (NORTHERN_EDGE) then
        do i=Istr,Iend
          cff=0.5*(zeta(i,Jend,knew)+zeta(i,Jend+1,knew)+h(i,Jend)
     &                                 +h(i,Jend+1))*om_v(i,Jend+1)
#  ifdef MASKING
     &                                             *vmask(i,Jend+1)
#  endif
          my_crss=my_crss+cff
          my_flux=my_flux-cff*vbar(i,Jend+1,knew)
        enddo
      endif
# endif

      if (SINGLE_TILE_MODE) then    ! Global summation is performed
        NSUB=1                      ! in two stages, first within
      else                          ! each shared memory (MPI-nodes), 
        NSUB=NSUB_X*NSUB_E          ! then across  MPI nodes (Reduce
      endif                         ! -- Broadcast sequence).

C$OMP CRITICAL (obc_flx_cr_rgn)     ! Counter tile_count identifies
        if (bc_count.eq.0) then     ! the last thread (within each
          bc_flux=QuadZero          ! MPI-process) leaving critical
          bc_crss=QuadZero          ! region. This thread (which is
        endif                       ! not necessarily master thread
        bc_count=bc_count+1         ! within its MPI-process) is
        bc_crss=bc_crss+my_crss     ! responsible for communication
        bc_flux=bc_flux+my_flux     ! exchange between MPI-nodes.

        if (bc_count.eq.NSUB) then
          bc_count=0
# ifdef MPI
          size=NNODES               ! Reduce-broadcast sequence...
  1        step=(size+1)/2
            if (mynode.ge.step .and. mynode.lt.size) then
              buff(1)=bc_crss
              buff(2)=bc_flux
              call MPI_Send (buff,  4, MPI_DOUBLE_PRECISION,
     &             mynode-step, 17, MPI_COMM_WORLD,      ierr)
            elseif (mynode .lt. size-step) then
              call MPI_Recv (buff,  4, MPI_DOUBLE_PRECISION,
     &             mynode+step, 17, MPI_COMM_WORLD, status, ierr)
     
              bc_crss=bc_crss+buff(1)
              bc_flux=bc_flux+buff(2)
            endif
           size=step
          if (size.gt.1) goto 1

          buff(1)=bc_crss
          buff(2)=bc_flux
          call MPI_Bcast(buff, 4, MPI_DOUBLE_PRECISION,
     &                         0, MPI_COMM_WORLD, ierr)
          bc_crss=buff(1)
          bc_flux=buff(2)
# endif
          ubar_xs=bc_flux/bc_crss
        endif
C$OMP END CRITICAL (obc_flx_cr_rgn)
      return
      end



      subroutine set_DUV_bc_tile (Istr,Iend,Jstr,Jend, Drhs,
     &                                              DUon,DVom)
!
! Set vertically integrated mass fluxes DUon and DVom along the open
! boundaries in such a way that the integral volume is conserved.
! This is done by applying ubar_xs correction to the velocities.
!  
# ifdef AGRIF
      USE Agrif_Util
# endif
      implicit none
# include "param.h"
      integer Istr,Iend,Jstr,Jend, i,j
      real Drhs(PRIVATE_2D_SCRATCH_ARRAY),
     &     DUon(PRIVATE_2D_SCRATCH_ARRAY),
     &     DVom(PRIVATE_2D_SCRATCH_ARRAY)
# include "grid.h"
# include "ocean2d.h"
# include "scalars.h"
!
# include "compute_auxiliary_bounds.h"
!
# ifdef AGRIF
       IF (.Not.Agrif_Root()) RETURN
# endif
# ifdef OBC_WEST
      if (WESTERN_EDGE) then
        do j=Jstr-1,JendR
          DUon(istr,j)=DUon(istr,j) - ubar_xs*on_u(istr,j)
     &                         *0.5*(Drhs(istr,j)+Drhs(istr-1,j))
#  ifdef MASKING
     &                                             *umask(istr,j)
#  endif
        enddo
      endif
# endif
# ifdef OBC_EAST
      if (EASTERN_EDGE) then
        do j=Jstr-1,JendR
          DUon(iend+1,j)=DUon(iend+1,j) + ubar_xs*on_u(iend+1,j)
     &                         *0.5*(Drhs(iend+1,j)+Drhs(iend,j))
#  ifdef MASKING
     &                                           *umask(iend+1,j)
#  endif
        enddo
      endif
# endif
# ifdef OBC_SOUTH
      if (SOUTHERN_EDGE) then
        do i=Istr-1,IendR
          DVom(i,jstr)=DVom(i,jstr) - ubar_xs*om_v(i,jstr)
     &                         *0.5*(Drhs(i,jstr)+Drhs(i,jstr-1))
#  ifdef MASKING
     &                                             *vmask(i,jstr)
#  endif
        enddo
      endif
# endif
# ifdef OBC_NORTH
      if (NORTHERN_EDGE) then
        do i=Istr-1,IendR
          DVom(i,jend+1)=DVom(i,jend+1) + ubar_xs*om_v(i,jend+1)
     &                         *0.5*(Drhs(i,jend+1)+Drhs(i,jend))
#  ifdef MASKING
     &                                           *vmask(i,jend+1)
#  endif
        enddo
      endif
# endif
      return
      end
!
!==================================================================
!
      subroutine conserve_mass (tile)
      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call conserve_mass_tile (Istr,Iend,Jstr,Jend)
      return
      end


      subroutine conserve_mass_tile (Istr,Iend,Jstr,Jend)
!
! This subroutine corrects velocities across the open boundaries
! to enforce global mass conservation constraint.
!
# ifdef AGRIF
      USE Agrif_Util
# endif
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "scalars.h"
# ifdef AGRIF
      IF (.Not.Agrif_Root()) RETURN
# endif
# ifdef OBC_WEST
      if (WESTERN_EDGE) then
        do j=Jstr,Jend
          ubar(Istr,j,knew)=(ubar(Istr,j,knew)-ubar_xs)
#  ifdef MASKING
     &                                      *umask(Istr,j)
#  endif
        enddo
      endif
# endif
# ifdef OBC_EAST
      if (EASTERN_EDGE) then
        do j=Jstr,Jend
          ubar(Iend+1,j,knew)=(ubar(Iend+1,j,knew)+ubar_xs)
#  ifdef MASKING
     &                                     *umask(Iend+1,j)
#  endif
        enddo
      endif
# endif
# ifdef OBC_SOUTH
      if (SOUTHERN_EDGE) then
        do i=Istr,Iend
          vbar(i,Jstr,knew)=(vbar(i,Jstr,knew)-ubar_xs)
#  ifdef MASKING
     &                                      *vmask(i,Jstr)
#  endif
        enddo
      endif
# endif
# ifdef OBC_NORTH
      if (NORTHERN_EDGE) then
        do i=Istr,Iend
          vbar(i,Jend+1,knew)=(vbar(i,Jend+1,knew)+ubar_xs)
#  ifdef MASKING
     &                                     *vmask(i,Jend+1)
#  endif
        enddo
      endif
# endif
      return
      end
#else
      subroutine obc_volcons_empty
      end
#endif /* OBC_VOLCONS */


