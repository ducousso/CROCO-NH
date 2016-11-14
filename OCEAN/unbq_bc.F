! $Id: unbq_bc.F 1458 2014-02-03 15:01:25Z gcambon $
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
      subroutine unbq_bc_tile (Istr,Iend,Jstr,Jend,grad)
#  ifdef AGRIF      
      use AGRIF_Util
      integer Istr,Iend,Jstr,Jend
      real grad(PRIVATE_2D_SCRATCH_ARRAY)
      if (AGRIF_Root()) then
        call unbq_bc_parent_tile (Istr,Iend,Jstr,Jend,grad)
      else
        call unbq_bc_child_tile (Istr,Iend,Jstr,Jend,grad)
      endif
      return
      end
!
! PARENT
!
      subroutine unbq_bc_parent_tile (Istr,Iend,Jstr,Jend,grad)
#  endif
!
! Set lateral boundary conditions for U-component momentum
! qdm_nbq_a(U_index,2) for the parent grid.
!
# endif /* NBQ */
#else
# ifdef NBQ
!
! CHILD
!
      subroutine unbq_bc_child_tile (Istr,Iend,Jstr,Jend,grad)
!
! Set lateral boundary conditions for U-component momentum
! qdm_nbq_a(U_index,2) for the child grid.
!
# endif /* NBQ */
#endif /* CHILD */
#ifdef NBQ
!
! Common Code
!
# include "set_obc_definitions.h"
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
# ifdef MRL_WCI
#  include "forces.h"
# endif
      integer Istr,Iend,Jstr,Jend, i,j,k,
     &        l_b_j,l_b_jm1,l_bm1_j,l_bm1_jm1,l_bm2_j,
     &        l_b_i,l_b_ip1,l_bm1_i,l_bm1_ip1,l_bm2_i,
     &        l_c,l_c_im1,l_c_jm1
      real    grad(PRIVATE_2D_SCRATCH_ARRAY)
      real    cff,eps,
     &        cx,cy, dft,dfx,dfy, tau,tau_in,tau_out
      parameter (eps=1.E-20)
!
# include "compute_auxiliary_bounds.h"
!
! Interpolations of the parent values to get unbqbry_east or unbqclm
!
# ifdef CHILD
      call unbq_bc_interp_tile(Istr,Iend,Jstr,Jend)
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
# ifndef EW_COM_PERIODIC
!
!====================================================================
!                            WESTERN BC
!====================================================================
      if (WESTERN_EDGE) then
#  ifdef OBC_COM_WEST
#   ifdef OBC_COM_NBQORLANSKI
        do k=1,N                             ! Western edge radiation
          do j=Jstr,Jend+1                   ! ======= ==== =========

            l_b_j     = ijk2lmom_nh(Istr  ,j  ,k,1)
            l_b_jm1   = ijk2lmom_nh(Istr  ,j-1,k,1)
            l_bm1_j   = ijk2lmom_nh(Istr+1,j  ,k,1)
            l_bm1_jm1 = ijk2lmom_nh(Istr+1,j-1,k,1)

            grad(Istr  ,j)=(qdm_nbq_a(l_b_j  ,nstp)
     &                     -qdm_nbq_a(l_b_jm1,nstp))
#    ifdef MASKING
     &                                              *pmask(Istr  ,j)
#    endif
            grad(Istr+1,j)=(qdm_nbq_a(l_bm1_j  ,nstp)
     &                     -qdm_nbq_a(l_bm1_jm1,nstp))
#    ifdef MASKING
     &                                              *pmask(Istr+1,j)
#    endif
          enddo
          do j=Jstr,Jend

            l_b_j     = ijk2lmom_nh(Istr  ,j,k,1)
            l_bm1_j   = ijk2lmom_nh(Istr+1,j,k,1)
            l_bm2_j   = ijk2lmom_nh(Istr+2,j,k,1)

            dft=qdm_nbq_a(l_bm1_j ,nstp)-qdm_nbq_a(l_bm1_j,nnew)
            dfx=qdm_nbq_a(l_bm1_j ,nnew)-qdm_nbq_a(l_bm2_j,nnew)

            if (dfx*dft .lt. 0.) then
              dft=0.                       ! <-- cancel cx, if inflow
#    if defined NBQ_FRC_BRY || defined NBQNUDGING
              tau=tau_in
            else
              tau=tau_out
#    endif
            endif
 
            if (dft*(grad(Istr+1,j)+grad(Istr+1,j+1)) .gt. 0.) then
              dfy=grad(Istr+1,j)
            else
              dfy=grad(Istr+1,j+1)
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
     &                               -max(cy,0.)*grad(Istr,j  )
     &                               -min(cy,0.)*grad(Istr,j+1)
     &                                               )/(cff+cx)
#    if defined NBQ_FRC_BRY  || defined NBQNUDGING 
            qdm_nbq_a(l_b_j,nnew)=(1.-tau)*qdm_nbq_a(l_b_j,nnew)
#     ifdef NBQ_FRC_BRY
     &                                    +tau*unbqbry_west(j,k)
#     else     
     &                                    +tau*unbqclm(Istr,j,k)
#     endif
#    endif
#    ifdef MASKING
            qdm_nbq_a(l_b_j,nnew)=qdm_nbq_a(l_b_j,nnew)*umask(Istr,j)
#    endif
#    ifdef WET_DRY
            qdm_nbq_a(l_b_j,nnew)=qdm_nbq_a(l_b_j,nnew)
     &                                              *umask_wet(Istr,j)
#    endif
          enddo
        enddo
!
#   elif defined OBC_COM_NBQSPECIFIED
!                                            Western edge Specified BC
!                                            ======= ==== ========= ==
        do k=1,N
          do j=Jstr,Jend
            l_b_j     = ijk2lmom_nh(Istr  ,j  ,k,1)
#    ifdef NBQ_FRC_BRY
            qdm_nbq_a(l_b_j,nnew)=unbqbry_west(j,k)         ! specified
#    else
            qdm_nbq_a(l_b_j,nnew)=unbqclm(Istr,j,k)
#    endif
#    ifdef MASKING
     &                               *umask(Istr,j)
#    endif
#    ifdef WET_DRY
     &                           *umask_wet(Istr,j)
#    endif
          enddo
        enddo
#   else
!                                            Western edge gradient BC
!                                            ======= ==== ======== ==
        do k=1,N
          do j=Jstr,Jend
            l_b_j     = ijk2lmom_nh(Istr  ,j  ,k,1)
            l_bm1_j   = ijk2lmom_nh(Istr+1,j  ,k,1)
            qdm_nbq_a(l_b_j,nnew)=qdm_nbq_a(l_bm1_j,nnew) ! Gradient: default
#    ifdef MASKING
     &                                     *umask(Istr,j)
#    endif
#    ifdef WET_DRY
     &                                 *umask_wet(Istr,j)
#    endif
          enddo
        enddo
#   endif
!
!#  else           /* alternative to open */
!        do k=1,N                                      ! Western edge closed
!          do j=jstr,jend                              ! ======= ==== ======
!            l_b_j     = ijk2lmom_nh(Istr  ,j  ,k,1)   !  (no-flux: default)
!            qdm_nbq_a(l_b_j,nnew)=0. 
!          enddo
!        enddo
#  endif              /* OBC_COM_WEST */
      endif         !<-- WESTERN_EDGE
!
!====================================================================
!                            EASTERN BC
!====================================================================
      if (EASTERN_EDGE) then
#  ifdef OBC_COM_EAST
#   ifdef OBC_COM_NBQORLANSKI
        do k=1,N                             ! Eastern edge radiation
          do j=Jstr,Jend+1                   ! ======= ==== =========

            l_b_j       = ijk2lmom_nh(Iend+1,j  ,k,1)
            l_b_jm1     = ijk2lmom_nh(Iend+1,j-1,k,1)
            l_bm1_j     = ijk2lmom_nh(Iend  ,j  ,k,1)
            l_bm1_jm1   = ijk2lmom_nh(Iend  ,j-1,k,1)

            grad(Iend  ,j)=(qdm_nbq_a(l_bm1_j  ,nstp)
     &                     -qdm_nbq_a(l_bm1_jm1,nstp))
#    ifdef MASKING
     &                                              *pmask(Iend  ,j)
#    endif
            grad(Iend+1,j)=(qdm_nbq_a(l_b_j  ,nstp)
     &                     -qdm_nbq_a(l_b_jm1,nstp))
#    ifdef MASKING
     &                                              *pmask(Iend+1,j)
#    endif
          enddo
          do j=Jstr,Jend

            l_b_j     = ijk2lmom_nh(Iend+1,j,k,1)
            l_bm1_j   = ijk2lmom_nh(Iend  ,j,k,1)
            l_bm2_j   = ijk2lmom_nh(Iend-1,j,k,1)

            dft=qdm_nbq_a(l_bm1_j ,nstp)-qdm_nbq_a(l_bm1_j,nnew)
            dfx=qdm_nbq_a(l_bm1_j ,nnew)-qdm_nbq_a(l_bm2_j,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                      ! <-- cancel cx, if inflow
#    if defined NBQ_FRC_BRY || defined NBQNUDGING
              tau=tau_in
            else
              tau=tau_out
#    endif
            endif
 
            if (dft*(grad(Iend,j)+grad(Iend,j+1)) .gt. 0.) then
              dfy=grad(Iend,j)
            else
              dfy=grad(Iend,j+1)
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
     &                             -max(cy,0.)*grad(Iend+1,j  )
     &                             -min(cy,0.)*grad(Iend+1,j+1)
     &                                               )/(cff+cx)
#    if defined NBQ_FRC_BRY  || defined NBQNUDGING 
            qdm_nbq_a(l_b_j,nnew)=(1.-tau)*qdm_nbq_a(l_b_j,nnew)
#     ifdef NBQ_FRC_BRY
     &                                    +tau*unbqbry_east(j,k)
#     else     
     &                                  +tau*unbqclm(Iend+1,j,k)
#     endif
#    endif
#    ifdef MASKING
            qdm_nbq_a(l_b_j,nnew)=qdm_nbq_a(l_b_j,nnew)
     &                                             *umask(Iend+1,j)
#    endif
#    ifdef WET_DRY
            qdm_nbq_a(l_b_j,nnew)=qdm_nbq_a(l_b_j,nnew)
     &                                             *umask_wet(Iend+1,j)
#    endif
          enddo
        enddo
!
#   elif defined OBC_COM_NBQSPECIFIED
!                                            Eastern edge Specified BC
!                                            ======= ==== ========= ==
        do k=1,N
          do j=Jstr,Jend
            l_b_j     = ijk2lmom_nh(Iend+1,j,k,1)
#    ifdef NBQ_FRC_BRY
            qdm_nbq_a(l_b_j,nnew)=unbqbry_east(j,k)       ! specified
#    else
            qdm_nbq_a(l_b_j,nnew)=unbqclm(Iend+1,j,k)
#    endif
#    ifdef MASKING
     &                               *umask(Iend+1,j)
#    endif
#    ifdef WET_DRY
     &                           *umask_wet(Iend+1,j)
#    endif
          enddo
        enddo
#   else
!                                            Eastern edge gradient BC
!                                            ======= ==== ======== ==
        do k=1,N
          do j=Jstr,Jend
            l_b_j     = ijk2lmom_nh(Iend+1,j,k,1)
            l_bm1_j   = ijk2lmom_nh(Iend  ,j,k,1)
            qdm_nbq_a(l_b_j,nnew)=qdm_nbq_a(l_bm1_j,nnew)  ! gradient (default)
#    ifdef MASKING
     &                                   *umask(Iend+1,j)
#    endif
#    ifdef WET_DRY
     &                               *umask_wet(Iend+1,j)
#    endif
          enddo
        enddo
#   endif
!
!#  else
!        do k=1,N                                     ! Eastern edge closed
!          do j=jstr,jend                             ! ======= ==== ======
!             l_b_j     = ijk2lmom_nh(Iend+1,j,k,1)   !  (no-flux: default)
!             qdm_nbq_a(l_b_j,nnew)=0.               
!          enddo
!        enddo
#  endif            /* OBC_COM_EAST */
      endif         !<-- EASTERN_EDGE
# endif             /* !EW_COM_PERIODIC */
 
# ifndef NS_COM_PERIODIC
!
!====================================================================
!                            SOUTHERN BC
!====================================================================
      if (SOUTHERN_EDGE) then
#  ifdef OBC_COM_SOUTH
#   ifdef OBC_COM_NBQORLANSKI
        do k=1,N                            ! Southern edge radiation
          do i=IstrU-1,Iend                 ! ======== ==== =========

            l_b_i     = ijk2lmom_nh(i  ,Jstr-1,k,1)
            l_b_ip1   = ijk2lmom_nh(i+1,Jstr-1,k,1)
            l_bm1_i   = ijk2lmom_nh(i  ,Jstr  ,k,1)
            l_bm1_ip1 = ijk2lmom_nh(i+1,Jstr  ,k,1)

            grad(i,Jstr-1)=qdm_nbq_a(l_b_ip1  ,nstp)
     &                    -qdm_nbq_a(l_b_i    ,nstp)
            grad(i,Jstr  )=qdm_nbq_a(l_bm1_ip1,nstp)
     &                    -qdm_nbq_a(l_bm1_i  ,nstp)
          enddo
          do i=IstrU,Iend

            l_b_i     = ijk2lmom_nh(i  ,Jstr-1,k,1)
            l_bm1_i   = ijk2lmom_nh(i  ,Jstr  ,k,1)
            l_bm2_i   = ijk2lmom_nh(i  ,Jstr+1,k,1)

            dft=qdm_nbq_a(l_bm1_i ,nstp)-qdm_nbq_a(l_bm1_i,nnew)
            dfx=qdm_nbq_a(l_bm1_i ,nnew)-qdm_nbq_a(l_bm2_i,nnew)

            if (dfx*dft .lt. 0.) then
              dft=0.                       ! <-- cancel cx, if inflow
#    if defined NBQ_FRC_BRY || defined NBQNUDGING
              tau=tau_in
            else
              tau=tau_out
#    endif
            endif
 
            if (dft*(grad(i-1,Jstr)+grad(i,Jstr)) .gt. 0.) then
              dfy=grad(i-1,Jstr)
            else
              dfy=grad(i  ,Jstr)
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
     &                             -max(cy,0.)*grad(i-1,Jstr-1)
     &                             -min(cy,0.)*grad(i  ,Jstr-1)
     &                                               )/(cff+cx)
#    if defined NBQ_FRC_BRY  || defined NBQNUDGING 
           qdm_nbq_a(l_b_i,nnew)=(1.-tau)*qdm_nbq_a(l_b_i,nnew)
#     ifdef NBQ_FRC_BRY
     &                                  +tau*unbqbry_south(i,k)
#     else     
     &                                 +tau*unbqclm(i,Jstr-1,k)
#     endif
#    endif
#    ifdef MASKING
            qdm_nbq_a(l_b_i,nnew)=qdm_nbq_a(l_b_i,nnew)*umask(i,Jstr-1)
#    endif
#    ifdef WET_DRY
            unbq(i,Jstr-1,k,nnew)=unbq(i,Jstr-1,k,nnew)
     &                                             *umask_wet(i,Jstr-1)
#    endif
          enddo
        enddo
!
#   elif defined OBC_COM_NBQSPECIFIED
!                                           Southern edge Specified BC
!                                           ======== ==== ========= ==
        do k=1,N
          do i=IstrU,Iend
            l_b_i     = ijk2lmom_nh(i  ,Jstr-1,k,1)
#    ifdef NBQ_FRC_BRY
            qdm_nbq_a(l_b_i,nnew)=unbqbry_south(i,k)      ! specified
#    else
            qdm_nbq_a(l_b_i,nnew)=unbqclm(i,Jstr-1,k)
#    endif
#    ifdef MASKING
     &                               *umask(i,Jstr-1)
#    endif
#    ifdef WET_DRY
     &                           *umask_wet(i,Jstr-1)
#    endif
          enddo
        enddo
#   else
!                                           Southern edge gradient BC
!                                           ======== ==== ======== ==
        do k=1,N
          do i=IstrU,Iend
            l_b_i     = ijk2lmom_nh(i  ,Jstr-1,k,1)
            l_bm1_i   = ijk2lmom_nh(i  ,Jstr  ,k,1)
            qdm_nbq_a(l_b_i,nnew)=qdm_nbq_a(l_bm1_i,nnew) ! gradient (default)
#    ifdef MASKING
     &                                   *umask(i,Jstr-1)
#    endif
#    ifdef WET_DRY
     &                               *umask_wet(i,Jstr-1)
#    endif
          enddo
        enddo
#   endif
!#  else
!#   ifdef EW_COM_PERIODIC
!#    define I_RANGE IstrU,Iend
!#   else
!#    define I_RANGE Istr,IendR
!#   endif
!        do k=1,N                        ! Wall: free-slip (gamma2=+1)
!          do i=I_RANGE                  ! =====   no-slip (gamma2=-1)
!            l_b_i     = ijk2lmom_nh(i  ,Jstr-1,k,1)
!            l_bm1_i   = ijk2lmom_nh(i  ,Jstr  ,k,1)
!            qdm_nbq_a(l_b_i,nnew)=gamma2*qdm_nbq_a(l_bm1_i,nnew)
!#   ifdef MASKING
!     &                                          *umask(i,Jstr-1)
!#   endif
!          enddo
!        enddo
!#   undef I_RANGE
#  endif
      endif              !<-- SOUTHERN_EDGE
!
!====================================================================
!                            NORTHERN BC
!====================================================================
      if (NORTHERN_EDGE) then
#  ifdef OBC_COM_NORTH
#   ifdef OBC_COM_NBQORLANSKI
        do k=1,N                            ! Northern edge radiation
          do i=IstrU-1,Iend                 ! ======== ==== =========

            l_b_i     = ijk2lmom_nh(i  ,Jend+1,k,1)
            l_b_ip1   = ijk2lmom_nh(i+1,Jend+1,k,1)
            l_bm1_i   = ijk2lmom_nh(i  ,Jend  ,k,1)
            l_bm1_ip1 = ijk2lmom_nh(i+1,Jend  ,k,1)

            grad(i,Jend+1)=qdm_nbq_a(l_b_ip1  ,nstp)
     &                    -qdm_nbq_a(l_b_i    ,nstp)
            grad(i,Jend  )=qdm_nbq_a(l_bm1_ip1,nstp)
     &                    -qdm_nbq_a(l_bm1_i  ,nstp)

          enddo
          do i=IstrU,Iend

            l_b_i     = ijk2lmom_nh(i  ,Jend+1,k,1)
            l_bm1_i   = ijk2lmom_nh(i  ,Jend  ,k,1)
            l_bm2_i   = ijk2lmom_nh(i  ,Jend-1,k,1)

            dft=qdm_nbq_a(l_bm1_i ,nstp)-qdm_nbq_a(l_bm1_i,nnew)
            dfx=qdm_nbq_a(l_bm1_i ,nnew)-qdm_nbq_a(l_bm2_i,nnew)

            if (dfx*dft .lt. 0.) then
              dft=0.                                 ! <-- INFLOW
#    if defined NBQ_FRC_BRY || defined NBQNUDGING
              tau=tau_in
            else
              tau=tau_out
#    endif
            endif
 
            if (dft*(grad(i-1,Jend)+grad(i,Jend)) .gt. 0.) then
              dfy=grad(i-1,Jend)
            else
              dfy=grad(i  ,Jend)
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
     &                             -max(cy,0.)*grad(i-1,Jend+1)
     &                             -min(cy,0.)*grad(i  ,Jend+1)
     &                                               )/(cff+cx)
#    if defined NBQ_FRC_BRY  || defined NBQNUDGING 
            qdm_nbq_a(l_b_i,nnew)=(1.-tau)*qdm_nbq_a(l_b_i,nnew)
#     ifdef NBQ_FRC_BRY
     &                                   +tau*unbqbry_north(i,k)
#     else     
     &                                  +tau*unbqclm(i,Jend+1,k)
#     endif
#    endif
#    ifdef MASKING
            qdm_nbq_a(l_b_i,nnew)=qdm_nbq_a(l_b_i,nnew)*umask(i,Jend+1)
#    endif
#    ifdef WET_DRY
            qdm_nbq_a(l_b_i,nnew)=qdm_nbq_a(l_b_i,nnew)
     &                                             *umask_wet(i,Jend+1)
#    endif
          enddo
        enddo
!
#   elif defined OBC_COM_NBQSPECIFIED
!                                           Northern edge Specified BC
!                                           ======== ==== ========= ==
        do k=1,N
          do i=IstrU,Iend
            l_b_i     = ijk2lmom_nh(i  ,Jend+1,k,1)
#    ifdef NBQ_FRC_BRY
            qdm_nbq_a(l_b_i,nnew)=unbqbry_north(i,k)      ! specified
#    else
            qdm_nbq_a(l_b_i,nnew)=unbqclm(i,Jend+1,k)
#    endif
#    ifdef MASKING
     &                               *umask(i,Jend+1)
#    endif
#    ifdef WET_DRY
     &                           *umask_wet(i,Jend+1)
#    endif
          enddo
        enddo
#   else
!                                           Northern edge gradient BC
!                                           ======== ==== ======== ==
        do k=1,N
          do i=IstrU,Iend
            l_b_i     = ijk2lmom_nh(i  ,Jend+1,k,1)
            l_bm1_i   = ijk2lmom_nh(i  ,Jend  ,k,1)
            qdm_nbq_a(l_b_i,nnew)=qdm_nbq_a(l_bm1_i,nnew)  ! gradient (default)
#    ifdef MASKING
     &                                   *umask(i,Jend+1)
#    endif
#    ifdef WET_DRY
     &                               *umask_wet(i,Jend+1)
#    endif
          enddo
        enddo
#   endif
!#  else
!#   ifdef EW_COM_PERIODIC
!#    define I_RANGE IstrU,Iend
!#   else
!#    define I_RANGE Istr,IendR
!#   endif
!        do k=1,N                        ! Wall: free-slip (gamma2=+1)
!          do i=I_RANGE                  ! =====   no-slip (gamma2=-1)
!            l_b_i     = ijk2lmom_nh(i  ,Jend+1,k,1)
!            l_bm1_i   = ijk2lmom_nh(i  ,Jend  ,k,1)
!            qdm_nbq_a(l_b_i,nnew)=gamma2*qdm_nbq_a(l_bm1_i,nnew)
!#   ifdef MASKING
!     &                                          *umask(i,Jend+1)
!#   endif
!          enddo
!        enddo
!#   undef I_RANGE
#  endif
      endif   !<-- NORTHERN_EDGE
# endif        /* !NS_COM_PERIODIC */
 
                           ! Corners between adjacent open boundaries
                           ! ======= ======= ======== ==== ==========
 
# if defined OBC_COM_SOUTH && defined OBC_COM_WEST
      if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=1,N
          l_c     = ijk2lmom_nh(Istr  ,Jstr-1,k,1)
          l_c_im1 = ijk2lmom_nh(Istr+1,Jstr-1,k,1)
          l_c_jm1 = ijk2lmom_nh(Istr  ,Jstr  ,k,1)
          qdm_nbq_a(l_c,nnew)=0.5*(qdm_nbq_a(l_c_im1,nnew)
     &                            +qdm_nbq_a(l_c_jm1,nnew))
#  ifdef MASKING
     &                                  *umask(Istr,Jstr-1)
#  endif
        enddo
      endif
# endif
# if defined OBC_COM_SOUTH && defined OBC_COM_EAST
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=1,N
          l_c     = ijk2lmom_nh(Iend+1,Jstr-1,k,1)
          l_c_im1 = ijk2lmom_nh(Iend  ,Jstr-1,k,1)
          l_c_jm1 = ijk2lmom_nh(Iend+1,Jstr  ,k,1)
          qdm_nbq_a(l_c,nnew)=0.5*(qdm_nbq_a(l_c_im1,nnew)
     &                            +qdm_nbq_a(l_c_jm1,nnew))
#  ifdef MASKING
     &                                *umask(Iend+1,Jstr-1)
#  endif
        enddo
      endif
# endif
# if defined OBC_COM_NORTH && defined OBC_COM_WEST
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        do k=1,N
          l_c     = ijk2lmom_nh(Istr  ,Jend+1,k,1)
          l_c_im1 = ijk2lmom_nh(Istr+1,Jend+1,k,1)
          l_c_jm1 = ijk2lmom_nh(Istr  ,Jend  ,k,1)
          qdm_nbq_a(l_c,nnew)=0.5*(qdm_nbq_a(l_c_im1,nnew)
     &                            +qdm_nbq_a(l_c_jm1,nnew))
#  ifdef MASKING
     &                                  *umask(Istr,Jend+1)
#  endif
        enddo
      endif
# endif
# if defined OBC_COM_NORTH && defined OBC_COM_EAST
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        do k=1,N
          l_c     = ijk2lmom_nh(Iend+1,Jend+1,k,1)
          l_c_im1 = ijk2lmom_nh(Iend  ,Jend+1,k,1)
          l_c_jm1 = ijk2lmom_nh(Iend+1,Jend  ,k,1)
          qdm_nbq_a(l_c,nnew)=0.5*(qdm_nbq_a(l_c_im1,nnew)
     &                            +qdm_nbq_a(l_c_jm1,nnew))
#  ifdef MASKING
     &                                *umask(Iend+1,Jend+1)
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
      subroutine unbq_bc_parent_empty
      end
# else
      subroutine unbq_bc_child_empty
      end
# endif
#endif /* NBQ */
#ifndef CHILD
# define CHILD
# ifdef AGRIF
#  include "unbq_bc.F"
# endif
# undef CHILD
#endif  /* !CHILD */
