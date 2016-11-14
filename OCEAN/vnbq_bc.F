! $Id: vnbq_bc.F 1458 2014-02-03 15:01:25Z gcambon $
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
#ifndef CHILD
!
# include "cppdefs.h"
# ifdef NBQ
      subroutine vnbq_bc_tile (Istr,Iend,Jstr,Jend,grad)
#  ifdef AGRIF      
      use AGRIF_Util
      integer Istr,Iend,Jstr,Jend
      real grad(PRIVATE_2D_SCRATCH_ARRAY)
      if (AGRIF_Root()) then
        call vnbq_bc_parent_tile (Istr,Iend,Jstr,Jend,grad)
      else
        call vnbq_bc_child_tile (Istr,Iend,Jstr,Jend,grad)
c        call vnbq_bc_interp_tile(Istr,Iend,Jstr,Jend)
      endif
      return
      end
!
! PARENT
!
      subroutine vnbq_bc_parent_tile (Istr,Iend,Jstr,Jend,grad)
#  endif
!
! Set lateral boundary conditions for V-component momentum
! qdm_nbq_a(V_index,2) for the parent grid.
!
# endif /* NBQ */
#else
# ifdef NBQ
!
! CHILD
!
      subroutine vnbq_bc_child_tile (Istr,Iend,Jstr,Jend,grad)
!
! Set lateral boundary conditions for V-component momentum
! qdm_nbq_a(V_index,2) for the child grid.
!
# endif /* NBQ */
#endif /* CHILD */
#ifdef NBQ
!
! Common Code
!
# include "set_obc_definitions.h"
!
!
      USE module_nh
      USE module_nbq
!
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "climat.h"
# include "scalars.h"
# include "boundary.h"
      integer Istr,Iend,Jstr,Jend, i,j,k,
     &        l_b_j,l_b_jp1,l_bm1_j,l_bm1_jp1,l_bm2_j,
     &        l_b_i,l_b_im1,l_bm1_i,l_bm1_im1,l_bm2_i,
     &        l_c,l_c_im1,l_c_jm1
      real    grad(PRIVATE_2D_SCRATCH_ARRAY), cff,eps,
     &        cx,cy, dft,dfx,dfy, tau,tau_in,tau_out
      parameter (eps=1.E-20)
!
# include "compute_auxiliary_bounds.h"
!
! Interpolations of the parent values to get vnbqbry_east or vnbqclm
!
# ifdef CHILD
      call vnbq_bc_interp_tile(Istr,Iend,Jstr,Jend)
# endif
!
# if defined NBQ_FRC_BRY || defined NBQNUDGING
      tau_in=dt*tauM_in
      tau_out=dt*tauM_out
# endif
!
# define nnew vnnew_nbq
# define nstp vnstp_nbq
!
# ifndef NS_COM_PERIODIC
!
!====================================================================
!                            SOUTHERN BC
!====================================================================
      if (SOUTHERN_EDGE) then
#  ifdef OBC_COM_SOUTH
#   ifdef OBC_COM_NBQORLANSKI
        do k=1,N                            ! Southern edge radiation
          do i=Istr,Iend+1                  ! ======== ==== =========

            l_b_i     = ijk2lmom_nh(i  ,Jstr  ,k,2)
            l_b_im1   = ijk2lmom_nh(i-1,Jstr  ,k,2)
            l_bm1_i   = ijk2lmom_nh(i  ,Jstr+1,k,2)
            l_bm1_im1 = ijk2lmom_nh(i-1,Jstr+1,k,2)

            grad(i,Jstr  )=(qdm_nbq_a(l_b_i  ,nstp)
     &                     -qdm_nbq_a(l_b_im1,nstp))
#    ifdef MASKING
     &                              *pmask(i  ,Jstr)
#    endif
            grad(i,Jstr+1)=(qdm_nbq_a(l_bm1_i  ,nstp)
     &                     -qdm_nbq_a(l_bm1_im1,nstp))
#    ifdef MASKING
     &                                *pmask(i,Jstr+1)
#    endif
          enddo
          do i=Istr,Iend

            l_b_i     = ijk2lmom_nh(i  ,Jstr  ,k,2)
            l_bm1_i   = ijk2lmom_nh(i  ,Jstr+1,k,2)
            l_bm2_i   = ijk2lmom_nh(i  ,Jstr+2,k,2)

            dft=qdm_nbq_a(l_bm1_i,nstp)-qdm_nbq_a(l_bm1_i,nnew)
            dfx=qdm_nbq_a(l_bm1_i,nnew)-qdm_nbq_a(l_bm2_i,nnew)

            if (dfx*dft .lt. 0.) then
              dft=0.                      ! <-- cancel cx, if inflow
#    if defined NBQ_FRC_BRY || defined NBQNUDGING
              tau=tau_in
            else
              tau=tau_out
#    endif
            endif
 
            if (dft*(grad(i,Jstr+1)+grad(i+1,Jstr+1)) .gt. 0.) then
              dfy=grad(i,Jstr+1)
            else
              dfy=grad(i+1,Jstr+1)
            endif
 
#    ifdef OBC_COM_RAD_NORMAL
            dfy=0.
#    endif
            cff=max(dfx*dfx+dfy*dfy, eps)
            cx=dft*dfx
#    ifdef OBC_COM_RAD_NPO
            cy=0.
#    else
            cy=min(cff,max(dft*dfy,-cff))
#    endif
 
            qdm_nbq_a(l_b_i,nnew)=( cff*qdm_nbq_a(l_b_i  ,nstp)
     &                              +cx*qdm_nbq_a(l_bm1_i,nnew)
     &                               -max(cy,0.)*grad(i  ,Jstr)
     &                               -min(cy,0.)*grad(i+1,Jstr)
     &                                               )/(cff+cx)
#    if defined NBQ_FRC_BRY  || defined NBQNUDGING 
            qdm_nbq_a(l_b_i,nnew)=(1.-tau)*qdm_nbq_a(l_b_i,nnew)
#     ifdef NBQ_FRC_BRY
     &                                   +tau*vnbqbry_south(i,k)
#     else     
     &                                    +tau*vnbqclm(i,Jstr,k)
#     endif
#    endif
#    ifdef MASKING
            qdm_nbq_a(l_b_i,nnew)=qdm_nbq_a(l_b_i,nnew)*vmask(i,Jstr)
#    endif
#    ifdef WET_DRY
            qdm_nbq_a(l_b_i,nnew)=qdm_nbq_a(l_b_i,nnew)
     &                                             *vmask_wet(i,Jstr)
#    endif
          enddo
        enddo
#   elif defined OBC_COM_NBQSPECIFIED
!                                           Southern edge Specified BC
!                                           ======== ==== ========= ==
        do k=1,N
          do i=Istr,Iend
            l_b_i     = ijk2lmom_nh(i  ,Jstr  ,k,2)
#    ifdef NBQ_FRC_BRY
            qdm_nbq_a(l_b_i,nnew)=vnbqbry_south(i,k)        ! specified
#    else
            qdm_nbq_a(l_b_i,nnew)=vnbqclm(i,Jstr,k)
#    endif
#    ifdef MASKING
     &                               *vmask(i,Jstr)
#    endif
#    ifdef WET_DRY
     &                           *vmask_wet(i,Jstr)
#    endif
          enddo
        enddo
#   else
!                                           Southern edge gradient BC
!                                           ======== ==== ======== ==
        do k=1,N
          do i=Istr,Iend
            l_b_i     = ijk2lmom_nh(i  ,Jstr  ,k,2)
            l_bm1_i   = ijk2lmom_nh(i  ,Jstr+1,k,2)
            qdm_nbq_a(l_b_i,nnew)=qdm_nbq_a(l_bm1_i,nnew)  ! gradient (default)
#    ifdef MASKING
     &                                     *vmask(i,Jstr)
#    endif
#    ifdef WET_DRY
     &                                 *vmask_wet(i,Jstr)
#    endif
          enddo
        enddo
#   endif
!#  else
!        do k=1,N                                     ! Southern edge closed
!          do i=Istr,Iend                             ! ======== ==== ======
!            l_b_i     = ijk2lmom_nh(i  ,Jstr  ,k,2)  !  (no-flux: default)
!            qdm_nbq_a(l_b_i,nnew)=0.
!          enddo
!        enddo
#  endif              /* OBC_COM_SOUTH */
      endif         !<-- SOUTHERN_EDGE
!
!====================================================================
!                            NORTHERN BC
!====================================================================
      if (NORTHERN_EDGE) then
#  ifdef OBC_COM_NORTH
#   ifdef OBC_COM_NBQORLANSKI
        do k=1,N                            ! Northern edge radiation
          do i=Istr,Iend+1                  ! ======== ==== =========

            l_b_i     = ijk2lmom_nh(i  ,Jend+1,k,2)
            l_b_im1   = ijk2lmom_nh(i-1,Jend+1,k,2)
            l_bm1_i   = ijk2lmom_nh(i  ,Jend  ,k,2)
            l_bm1_im1 = ijk2lmom_nh(i-1,Jend  ,k,2)

            grad(i,Jend  )=(qdm_nbq_a(l_bm1_j  ,nstp)
     &                     -qdm_nbq_a(l_bm1_im1,nstp))
#    ifdef MASKING
     &                                  *pmask(i,Jend)
#    endif
            grad(i,Jend+1)=(qdm_nbq_a(l_b_j  ,nstp)
     &                     -qdm_nbq_a(l_b_im1,nstp))
#    ifdef MASKING
     &                                 *pmask(i,Jend+1)
#    endif
          enddo
          do i=Istr,Iend

            l_b_i     = ijk2lmom_nh(i  ,Jend+1,k,2)
            l_bm1_i   = ijk2lmom_nh(i  ,Jend  ,k,2)
            l_bm2_i   = ijk2lmom_nh(i  ,Jend-1,k,2)

            dft=qdm_nbq_a(l_bm1_i,nstp)-qdm_nbq_a(l_bm1_i,nnew)
            dfx=qdm_nbq_a(l_bm1_i,nnew)-qdm_nbq_a(l_bm2_i,nnew)

            if (dfx*dft .lt. 0.) then
              dft=0.                       ! <-- cancel cx, if inflow
#    if defined NBQ_FRC_BRY || defined NBQNUDGING
              tau=tau_in
            else
              tau=tau_out
#    endif
            endif
 
            if (dft*(grad(i,Jend)+grad(i+1,Jend)) .gt. 0.) then
              dfy=grad(i,Jend)
            else
              dfy=grad(i+1,Jend)
            endif
 
#    ifdef OBC_COM_RAD_NORMAL
            dfy=0.
#    endif
            cff=max(dfx*dfx+dfy*dfy, eps)
            cx=dft*dfx
#    ifdef OBC_COM_RAD_NPO
            cy=0.
#    else
            cy=min(cff,max(dft*dfy,-cff))
#    endif
 
            qdm_nbq_a(l_b_i,nnew)=( cff*qdm_nbq_a(l_b_i  ,nstp)
     &                              +cx*qdm_nbq_a(l_bm1_i,nnew)
     &                             -max(cy,0.)*grad(i  ,Jend+1)
     &                             -min(cy,0.)*grad(i+1,Jend+1)
     &                                               )/(cff+cx)
#    if defined NBQ_FRC_BRY  || defined NBQNUDGING 
            qdm_nbq_a(l_b_i,nnew)=(1.-tau)*qdm_nbq_a(l_b_i,nnew)
#     ifdef NBQ_FRC_BRY
     &                                   +tau*vnbqbry_north(i,k)
#     else     
     &                                  +tau*vnbqclm(i,Jend+1,k)
#     endif
#    endif
#    ifdef MASKING
            qdm_nbq_a(l_b_i,nnew)=qdm_nbq_a(l_b_i,nnew)*vmask(i,Jend+1)
#    endif
#    ifdef WET_DRY
            qdm_nbq_a(l_b_i,nnew)=qdm_nbq_a(l_b_i,nnew)
     &                                             *vmask_wet(i,Jend+1)
#    endif
          enddo
        enddo
!
#   elif defined OBC_COM_NBQSPECIFIED
!                                           Northern edge Specified BC
!                                           ======== ==== ========= ==
        do k=1,N
          do i=Istr,Iend
            l_b_i     = ijk2lmom_nh(i  ,Jend+1,k,2)
#    ifdef NBQ_FRC_BRY
            qdm_nbq_a(l_b_i,nnew)=vnbqbry_north(i,k)      ! specified
#    else
            qdm_nbq_a(l_b_i,nnew)=vnbqclm(i,Jend+1,k)
#    endif
#    ifdef MASKING
     &                               *vmask(i,Jend+1)
#    endif
#    ifdef WET_DRY
     &                           *vmask_wet(i,Jend+1)
#    endif
          enddo
        enddo
#   else
        do k=1,N
          do i=Istr,Iend
!                                           Northern edge gradient BC
!                                           ======== ==== ======== ==
            l_b_i     = ijk2lmom_nh(i  ,Jend+1,k,2)
            l_bm1_i   = ijk2lmom_nh(i  ,Jend  ,k,2)
            qdm_nbq_a(l_b_i,nnew)=qdm_nbq_a(l_bm1_i,nnew) ! gradient (default)
#    ifdef MASKING
     &                                   *vmask(i,Jend+1)
#    endif
#    ifdef WET_DRY
     &                               *vmask_wet(i,Jend+1)
#    endif
          enddo
        enddo
#   endif
!#  else
!        do k=1,N                                    ! Northern edge closed
!          do i=Istr,Iend                            ! ======== ==== ======
!            l_b_i     = ijk2lmom_nh(i  ,Jend+1,k,2) !   (no-flux: default)
!            qdm_nbq_a(l_b_i,nnew)=0.
!          enddo
!        enddo
#  endif
      endif     !<--  NORTHERN_EDGE
# endif          /* !NS_COM_PERIODIC */
 
# ifndef EW_COM_PERIODIC
!
!====================================================================
!                            WESTERN BC
!====================================================================
      if (WESTERN_EDGE) then
#  ifdef OBC_COM_WEST
#   ifdef OBC_COM_NBQORLANSKI
        do k=1,N                             ! Western edge radiation
          do j=JstrV-1,Jend                  ! ======= ==== =========

            l_b_j     = ijk2lmom_nh(Istr-1,j  ,k,2)
            l_b_jp1   = ijk2lmom_nh(Istr-1,j+1,k,2)
            l_bm1_j   = ijk2lmom_nh(Istr  ,j  ,k,2)
            l_bm1_jp1 = ijk2lmom_nh(Istr  ,j+1,k,2)

            grad(Istr-1,j)=qdm_nbq_a(l_b_jp1  ,nstp)
     &                    -qdm_nbq_a(l_b_j    ,nstp)
            grad(Istr  ,j)=qdm_nbq_a(l_bm1_jp1,nstp)
     &                    -qdm_nbq_a(l_bm1_j  ,nstp)
          enddo
          do j=JstrV,Jend

            l_b_j     = ijk2lmom_nh(Istr-1,j  ,k,2)
            l_bm1_j   = ijk2lmom_nh(Istr  ,j  ,k,2)
            l_bm2_j   = ijk2lmom_nh(Istr+1,j  ,k,2)

            dft=qdm_nbq_a(l_bm1_j,nstp)-qdm_nbq_a(l_bm1_j,nnew)
            dfx=qdm_nbq_a(l_bm1_j,nnew)-qdm_nbq_a(l_bm2_j,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                       ! <-- cancel cx, if inflow
#    if defined NBQ_FRC_BRY || defined NBQNUDGING
              tau=tau_in
            else
              tau=tau_out
#    endif
            endif
 
            if (dft*(grad(Istr,j-1)+grad(Istr,j)) .gt. 0.) then
              dfy=grad(Istr,j-1)
            else
              dfy=grad(Istr,j  )
            endif
 
#    ifdef OBC_COM_RAD_NORMAL
            dfy=0.
#    endif
            cff=max(dfx*dfx+dfy*dfy, eps)
            cx=dft*dfx
#    ifdef OBC_COM_RAD_NPO
            cy=0.
#    else
            cy=min(cff,max(dft*dfy,-cff))
#    endif
            qdm_nbq_a(l_b_j,nnew)=( cff*qdm_nbq_a(l_b_j  ,nstp)
     &                              +cx*qdm_nbq_a(l_bm1_j,nnew)
     &                             -max(cy,0.)*grad(Istr-1,j-1)
     &                             -min(cy,0.)*grad(Istr-1,j  )
     &                                               )/(cff+cx)
#    if defined NBQ_FRC_BRY  || defined NBQNUDGING 
            qdm_nbq_a(l_b_j,nnew)=(1.-tau)*qdm_nbq_a(l_b_j,nnew)
#     ifdef NBQ_FRC_BRY
     &                                    +tau*vnbqbry_west(j,k)
#     else
     &                                  +tau*vnbqclm(Istr-1,j,k)
#     endif
#    endif
#    ifdef MASKING
            qdm_nbq_a(l_b_j,nnew)=qdm_nbq_a(l_b_j,nnew)
     &                                             *vmask(Istr-1,j)
#    endif
#    ifdef WET_DRY
            qdm_nbq_a(l_b_j,nnew)=qdm_nbq_a(l_b_j,nnew)
     &                                             *vmask_wet(Istr-1,j)
#    endif
          enddo
        enddo
!
#   elif defined OBC_COM_NBQSPECIFIED
!                                            Western edge Specified BC
!                                            ======= ==== ========= ==
        do k=1,N
          do j=JstrV,Jend
            l_b_j     = ijk2lmom_nh(Istr-1,j  ,k,2)
#    ifdef NBQ_FRC_BRY
            qdm_nbq_a(l_b_j,nnew)=vnbqbry_west(j,k)       ! specified
#    else
            qdm_nbq_a(l_b_j,nnew)=vnbqclm(Istr-1,j,k)
#    endif
#    ifdef MASKING
     &                               *vmask(Istr-1,j)
#    endif
#    ifdef WET_DRY
     &                           *vmask_wet(Istr-1,j)
#    endif
          enddo
        enddo
#   else
!                                            Western edge gradient BC
!                                            ======= ==== ======== ==
        do k=1,N
          do j=JstrV,Jend
            l_b_j     = ijk2lmom_nh(Istr-1,j  ,k,2)
            l_bm1_j   = ijk2lmom_nh(Istr  ,j  ,k,2)
            qdm_nbq_a(l_b_j,nnew)=qdm_nbq_a(l_bm1_j,nnew) ! gradient (default)
#    ifdef MASKING
     &                                   *vmask(Istr-1,j)
#    endif
#    ifdef WET_DRY
     &                               *vmask_wet(Istr-1,j)
#    endif
          enddo
        enddo
#   endif
!#  else
!#   ifdef NS_COM_PERIODIC
!#    define J_RANGE JstrV,Jend
!#   else
!#    define J_RANGE Jstr,JendR
!#   endif
!        do k=1,N                        ! Wall: free-slip (gamma2=+1)
!          do j=J_RANGE                  ! =====   no-slip (gamma2=-1)
!            l_b_j     = ijk2lmom_nh(Istr-1,j  ,k,2)
!            l_bm1_j   = ijk2lmom_nh(Istr  ,j  ,k,2)
!            qdm_nbq_a(l_b_j,nnew)=gamma2*qdm_nbq_a(l_bm1_j,nnew)
!#   ifdef MASKING
!     &                                          *vmask(Istr-1,j)
!#   endif
!          enddo
!        enddo
!#   undef J_RANGE
#  endif
      endif          !<-- WESTERN_EDGE
!
!====================================================================
!                            EASTERN BC
!====================================================================
      if (EASTERN_EDGE) then
#  ifdef OBC_COM_EAST
#   ifdef OBC_COM_NBQORLANSKI
        do k=1,N                             ! Eastern edge radiation
          do j=JstrV-1,Jend                  ! ======= ==== =========

            l_b_j     = ijk2lmom_nh(Iend+1,j  ,k,2)
            l_b_jp1   = ijk2lmom_nh(Iend+1,j+1,k,2)
            l_bm1_j   = ijk2lmom_nh(Iend  ,j  ,k,2)
            l_bm1_jp1 = ijk2lmom_nh(Iend  ,j+1,k,2)

            grad(Iend+1,j)=qdm_nbq_a(l_b_jp1  ,nstp)
     &                    -qdm_nbq_a(l_b_j    ,nstp)
            grad(Iend  ,j)=qdm_nbq_a(l_bm1_jp1,nstp)
     &                    -qdm_nbq_a(l_bm1_j  ,nstp)
          enddo
          do j=JstrV,Jend

            l_b_j     = ijk2lmom_nh(Iend+1,j  ,k,2)
            l_bm1_j   = ijk2lmom_nh(Iend  ,j  ,k,2)
            l_bm2_j   = ijk2lmom_nh(Iend-1,j  ,k,2)

            dft=qdm_nbq_a(l_bm1_j,nstp)-qdm_nbq_a(l_bm1_j,nnew)
            dfx=qdm_nbq_a(l_bm1_j,nnew)-qdm_nbq_a(l_bm2_j,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                       ! <-- cancel cx, if inflow
#    if defined NBQ_FRC_BRY || defined NBQNUDGING
              tau=tau_in
            else
              tau=tau_out
#    endif
            endif
 
            if (dft*(grad(Iend,j-1)+grad(Iend,j)) .gt. 0.) then
              dfy=grad(Iend,j-1)
            else
              dfy=grad(Iend,j  )
            endif
 
#    ifdef OBC_COM_RAD_NORMAL
            dfy=0.
#    endif
            cff=max(dfx*dfx+dfy*dfy, eps)
            cx=dft*dfx
#    ifdef OBC_COM_RAD_NPO
            cy=0.
#    else
            cy=min(cff,max(dft*dfy,-cff))
#    endif
 
            qdm_nbq_a(l_b_j,nnew)=( cff*qdm_nbq_a(l_b_j  ,nstp)
     &                              +cx*qdm_nbq_a(l_bm1_j,nnew)
     &                             -max(cy,0.)*grad(Iend+1,j-1)
     &                             -min(cy,0.)*grad(Iend+1,j  )
     &                                               )/(cff+cx)
#    if defined NBQ_FRC_BRY  || defined NBQNUDGING 
            qdm_nbq_a(l_b_j,nnew)=(1.-tau)*qdm_nbq_a(l_b_j,nnew)
#     ifdef NBQ_FRC_BRY
     &                                    +tau*vnbqbry_east(j,k)
#     else     
     &                                  +tau*vnbqclm(Iend+1,j,k)
#     endif
#    endif
#    ifdef MASKING
            qdm_nbq_a(l_b_j,nnew)=qdm_nbq_a(l_b_j,nnew)
     &                                             *vmask(Iend+1,j)
#    endif
#    ifdef WET_DRY
           qdm_nbq_a(l_b_j,nnew)=qdm_nbq_a(l_b_j,nnew)
     &                                             *vmask_wet(Iend+1,j)
#    endif
          enddo
        enddo
!
#   elif defined OBC_COM_NBQSPECIFIED
!                                            Eastern edge Specified BC
!                                            ======= ==== ========= ==
        do k=1,N
          do j=Jstr,Jend
            l_b_j     = ijk2lmom_nh(Iend+1,j  ,k,2)
#    ifdef NBQ_FRC_BRY
            qdm_nbq_a(l_b_j,nnew)=vnbqbry_east(j,k)       ! specified
#    else
            qdm_nbq_a(l_b_j,nnew)=vnbqclm(Iend+1,j,k)
#    endif
#    ifdef MASKING
     &                               *vmask(Iend+1,j)
#    endif
#    ifdef WET_DRY
     &                           *vmask_wet(Iend+1,j)
#    endif
          enddo
        enddo
#   else
!                                            Eastern edge gradient BC
!                                            ======= ==== ======== ==
        do k=1,N
          do j=Jstr,Jend
            l_b_j     = ijk2lmom_nh(Iend+1,j  ,k,2)
            l_bm1_j   = ijk2lmom_nh(Iend  ,j  ,k,2)
           qdm_nbq_a(l_b_j,nnew)=qdm_nbq_a(l_bm1_j,nnew) ! gradient (default)
#    ifdef MASKING
     &                                  *vmask(Iend+1,j)
#    endif
#    ifdef WET_DRY
     &                              *vmask_wet(Iend+1,j)
#    endif
          enddo
        enddo
#   endif
!#  else
!#   ifdef NS_COM_PERIODIC
!#    define J_RANGE JstrV,Jend
!#   else
!#    define J_RANGE Jstr,JendR
!#   endif
!        do k=1,N                        ! Wall: free-slip (gamma2=+1)
!          do j=J_RANGE                  ! ====    no-slip (gamma2=-1)
!            l_b_j     = ijk2lmom_nh(Iend+1,j  ,k,2)
!            l_bm1_j   = ijk2lmom_nh(Iend  ,j  ,k,2)
!            qdm_nbq_a(l_b_j,nnew)=gamma2*qdm_nbq_a(l_bm1_j,nnew)
!#   ifdef MASKING
!     &                                          *vmask(Iend+1,j)
!#   endif
!          enddo
!        enddo
!#   undef J_RANGE
#  endif
      endif     !<-- EASTERN_EDGE
# endif          /* !EW_COM_PERIODIC */
 
                           ! Corners between adjacent open boundaries
                           ! ======= ======= ======== ==== ==========
 
# if defined OBC_COM_SOUTH && defined OBC_COM_WEST
      if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=1,N
          l_c     = ijk2lmom_nh(Istr-1,Jstr  ,k,2)
          l_c_jm1 = ijk2lmom_nh(Istr-1,Jstr+1,k,2)
          l_c_im1 = ijk2lmom_nh(Istr  ,Jstr  ,k,2)
          qdm_nbq_a(l_c,nnew)=0.5*(qdm_nbq_a(l_c_jm1,nnew)
     &                            +qdm_nbq_a(l_c_im1,nnew))
#  ifdef MASKING
     &                                  *vmask(Istr-1,Jstr)
#  endif
        enddo
      endif
# endif
# if defined OBC_COM_SOUTH && defined OBC_COM_EAST
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=1,N
          l_c     = ijk2lmom_nh(Iend+1,Jstr  ,k,2)
          l_c_jm1 = ijk2lmom_nh(Iend  ,Jstr+1,k,2)
          l_c_im1 = ijk2lmom_nh(Iend  ,Jstr  ,k,2)
          qdm_nbq_a(l_c,nnew)=0.5*(qdm_nbq_a(l_c_jm1,nnew)
     &                            +qdm_nbq_a(l_c_im1,nnew))
#  ifdef MASKING
     &                                  *vmask(Iend+1,Jstr)
#  endif
        enddo
      endif
# endif
# if defined OBC_COM_NORTH && defined OBC_COM_WEST
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        do k=1,N
          l_c     = ijk2lmom_nh(Istr-1,Jend+1,k,2)
          l_c_jm1 = ijk2lmom_nh(Istr-1,Jend  ,k,2)
          l_c_im1 = ijk2lmom_nh(Istr  ,Jend+1,k,2)
          qdm_nbq_a(l_c,nnew)=0.5*(qdm_nbq_a(l_c_jm1,nnew)
     &                            +qdm_nbq_a(l_c_im1,nnew))
#  ifdef MASKING
     &                                *vmask(Istr-1,Jend+1)
#  endif
        enddo
      endif
# endif
# if defined OBC_COM_NORTH && defined OBC_COM_EAST
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        do k=1,N
          l_c     = ijk2lmom_nh(Iend+1,Jend+1,k,2)
          l_c_jm1 = ijk2lmom_nh(Iend+1,Jend  ,k,2)
          l_c_im1 = ijk2lmom_nh(Iend  ,Jend+1,k,2)
          qdm_nbq_a(l_c,nnew)=0.5*(qdm_nbq_a(l_c_jm1,nnew)
     &                            +qdm_nbq_a(l_c_im1,nnew))
#  ifdef MASKING
     &                                *vmask(Iend+1,Jend+1)
#  endif
        enddo
      endif
# endif

# undef nnew
# undef nstp

      return
      end
#else
# ifndef CHILD
      subroutine vnbq_bc_parent_empty
      end
# else
      subroutine vnbq_bc_child_empty
      end
# endif
#endif /* NBQ */
#ifndef CHILD
# define CHILD
# ifdef AGRIF
#  include "vnbq_bc.F"
# endif
# undef CHILD
#endif  /* !CHILD */
 
