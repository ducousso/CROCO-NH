! $Id: v3dbc.F 1458 2014-02-03 15:01:25Z gcambon $
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
# ifdef SOLVE3D
      subroutine v3dbc_tile (Istr,Iend,Jstr,Jend,grad)
#  ifdef AGRIF      
      use AGRIF_Util
      integer Istr,Iend,Jstr,Jend
      real grad(PRIVATE_2D_SCRATCH_ARRAY)
      if (AGRIF_Root()) then
        call v3dbc_parent_tile (Istr,Iend,Jstr,Jend,grad)
      else
        call v3dbc_child_tile (Istr,Iend,Jstr,Jend,grad)
c        call v3dbc_interp_tile(Istr,Iend,Jstr,Jend)
      endif
      return
      end
!
! PARENT
!
      subroutine v3dbc_parent_tile (Istr,Iend,Jstr,Jend,grad)
#  endif
!
! Set lateral boundary conditions for ETA-component velocity
! v(:,:,:,nnew) for the parent grid.
!
# endif /* SOLVE3D */
#else
# ifdef SOLVE3D
!
! CHILD
!
      subroutine v3dbc_child_tile (Istr,Iend,Jstr,Jend,grad)
!
! Set lateral boundary conditions for ETA-component velocity
! v(:,:,:,nnew) for the child grid.
!
# endif /* SOLVE3D */
#endif /* CHILD */
#ifdef SOLVE3D
!
! Common Code
!
# include "set_obc_definitions.h"
!
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "climat.h"
# include "scalars.h"
# include "boundary.h"
      integer Istr,Iend,Jstr,Jend, i,j,k
      real grad(PRIVATE_2D_SCRATCH_ARRAY), cff,eps,
     &      cx,cy, dft,dfx,dfy, tau,tau_in,tau_out
      parameter (eps=1.E-20)
!
# include "compute_auxiliary_bounds.h"
!
! Interpolations of the parent values to get vbry_east or vclm
!
# ifdef CHILD
      call v3dbc_interp_tile(Istr,Iend,Jstr,Jend)
# endif
!
# if defined M3_FRC_BRY || defined M3CLIMATOLOGY
      tau_in=dt*tauM_in
      tau_out=dt*tauM_out
# endif
!
# ifndef NS_COM_PERIODIC
!
!====================================================================
!                            SOUTHERN BC
!====================================================================
      if (SOUTHERN_EDGE) then
#  ifdef OBC_COM_SOUTH
#   ifdef OBC_COM_M3ORLANSKI
        do k=1,N                            ! Southern edge radiation
          do i=Istr,Iend+1                  ! ======== ==== =========
            grad(i,Jstr  )=(v(i,Jstr  ,k,nstp)-v(i-1,Jstr  ,k,nstp))
#    ifdef MASKING
     &                                                *pmask(i,Jstr)
#    endif
            grad(i,Jstr+1)=(v(i,Jstr+1,k,nstp)-v(i-1,Jstr+1,k,nstp))
#    ifdef MASKING
     &                                              *pmask(i,Jstr+1)
#    endif
          enddo
          do i=Istr,Iend
            dft=v(i,Jstr+1,k,nstp)-v(i,Jstr+1,k,nnew)
            dfx=v(i,Jstr+1,k,nnew)-v(i,Jstr+2,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                      ! <-- cancel cx, if inflow
#    if defined M3_FRC_BRY || defined M3CLIMATOLOGY
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
 
            v(i,Jstr,k,nnew)=( cff*v(i,Jstr,k,nstp)
     &                        +cx*v(i,Jstr+1,k,nnew)
     &                    -max(cy,0.)*grad(i  ,Jstr)
     &                    -min(cy,0.)*grad(i+1,Jstr)
     &                                   )/(cff+cx)
#    if defined M3_FRC_BRY  || defined M3CLIMATOLOGY 
            v(i,Jstr,k,nnew)=(1.-tau)*v(i,Jstr,k,nnew)+tau*
#     ifdef M3_FRC_BRY
     &                                    vbry_south(i,k)
#     else     
     &                                     vclm(i,Jstr,k)
#     endif
#    endif
#    ifdef MASKING
            v(i,Jstr,k,nnew)=v(i,Jstr,k,nnew)*vmask(i,Jstr)
#    endif
#    ifdef WET_DRY
            v(i,Jstr,k,nnew)=v(i,Jstr,k,nnew)*vmask_wet(i,Jstr)
#    endif
          enddo
        enddo
#   elif defined OBC_COM_M3SPECIFIED
!                                           Southern edge Specified BC
!                                           ======== ==== ========= ==
        do k=1,N
          do i=Istr,Iend
#    ifdef M3_FRC_BRY
            v(i,Jstr,k,nnew)=vbry_south(i,k)        ! specified
#    else
            v(i,Jstr,k,nnew)=vclm(i,Jstr,k)
#    endif
#    ifdef MASKING
     &                       *vmask(i,Jstr)
#    endif
#    ifdef WET_DRY
     &                       *vmask_wet(i,Jstr)
#    endif
          enddo
        enddo
#   else
!                                           Southern edge gradient BC
!                                           ======== ==== ======== ==
        do k=1,N
          do i=Istr,Iend
            v(i,Jstr,k,nnew)=v(i,Jstr+1,k,nnew)  ! gradient (default)
#    ifdef MASKING
     &                       *vmask(i,Jstr)
#    endif
#    ifdef WET_DRY
     &                       *vmask_wet(i,Jstr)
#    endif
          enddo
        enddo
#   endif
#  else
        do k=1,N                               ! Southern edge closed
          do i=Istr,Iend                       ! ======== ==== ======
            v(i,Jstr,k,nnew)=0.                !  (no-flux: default)
          enddo
        enddo
#  endif              /* OBC_COM_SOUTH */
      endif         !<-- SOUTHERN_EDGE
!
!====================================================================
!                            NORTHERN BC
!====================================================================
      if (NORTHERN_EDGE) then
#  ifdef OBC_COM_NORTH
#   ifdef OBC_COM_M3ORLANSKI
        do k=1,N                            ! Northern edge radiation
          do i=Istr,Iend+1                  ! ======== ==== =========
            grad(i,Jend  )=(v(i,Jend  ,k,nstp)-v(i-1,Jend  ,k,nstp))
#    ifdef MASKING
     &                                                *pmask(i,Jend)
#    endif
            grad(i,Jend+1)=(v(i,Jend+1,k,nstp)-v(i-1,Jend+1,k,nstp))
#    ifdef MASKING
     &                                              *pmask(i,Jend+1)
#    endif
          enddo
          do i=Istr,Iend
            dft=v(i,Jend,k,nstp)-v(i,Jend  ,k,nnew)
            dfx=v(i,Jend,k,nnew)-v(i,Jend-1,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                       ! <-- cancel cx, if inflow
#    if defined M3_FRC_BRY || defined M3CLIMATOLOGY
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
 
            v(i,Jend+1,k,nnew)=( cff*v(i,Jend+1,k,nstp)
     &                              +cx*v(i,Jend,k,nnew)
     &                      -max(cy,0.)*grad(i  ,Jend+1)
     &                      -min(cy,0.)*grad(i+1,Jend+1)
     &                                      )/(cff+cx)
#    if defined M3_FRC_BRY  || defined M3CLIMATOLOGY 
            v(i,Jend+1,k,nnew)=(1.-tau)*v(i,Jend+1,k,nnew)+tau*
#     ifdef M3_FRC_BRY
     &                                         vbry_north(i,k)
#     else     
     &                                        vclm(i,Jend+1,k)
#     endif
#    endif
#    ifdef MASKING
            v(i,Jend+1,k,nnew)=v(i,Jend+1,k,nnew)*vmask(i,Jend+1)
#    endif
#    ifdef WET_DRY
            v(i,Jend+1,k,nnew)=v(i,Jend+1,k,nnew)*vmask_wet(i,Jend+1)
#    endif
          enddo
        enddo
!
#   elif defined OBC_COM_M3SPECIFIED
!                                           Northern edge Specified BC
!                                           ======== ==== ========= ==
        do k=1,N
          do i=Istr,Iend
#    ifdef M3_FRC_BRY
            v(i,Jend+1,k,nnew)=vbry_north(i,k)      ! specified
#    else
            v(i,Jend+1,k,nnew)=vclm(i,Jend+1,k)
#    endif
#    ifdef MASKING
     &                         *vmask(i,Jend+1)
#    endif
#    ifdef WET_DRY
     &                         *vmask_wet(i,Jend+1)
#    endif
          enddo
        enddo
#   else
        do k=1,N
          do i=Istr,Iend
!                                           Northern edge gradient BC
!                                           ======== ==== ======== ==
            v(i,Jend+1,k,nnew)=v(i,Jend,k,nnew)  ! gradient (default)
#    ifdef MASKING
     &                         *vmask(i,Jend+1)
#    endif
#    ifdef WET_DRY
     &                         *vmask_wet(i,Jend+1)
#    endif
          enddo
        enddo
#   endif
#  else
        do k=1,N                               ! Northern edge closed
          do i=Istr,Iend                       ! ======== ==== ======
            v(i,Jend+1,k,nnew)=0.              !   (no-flux: default)
          enddo
        enddo
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
#   ifdef OBC_COM_M3ORLANSKI
        do k=1,N                             ! Western edge radiation
          do j=JstrV-1,Jend                  ! ======= ==== =========
            grad(Istr-1,j)=v(Istr-1,j+1,k,nstp)-v(Istr-1,j,k,nstp)
            grad(Istr  ,j)=v(Istr  ,j+1,k,nstp)-v(Istr  ,j,k,nstp)
          enddo
          do j=JstrV,Jend
            dft=v(Istr,j,k,nstp)-v(Istr  ,j,k,nnew)
            dfx=v(Istr,j,k,nnew)-v(Istr+1,j,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                       ! <-- cancel cx, if inflow
#    if defined M3_FRC_BRY || defined M3CLIMATOLOGY
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
 
            v(Istr-1,j,k,nnew)=( cff*v(Istr-1,j,k,nstp)
     &                              +cx*v(Istr,j,k,nnew)
     &                      -max(cy,0.)*grad(Istr-1,j-1)
     &                      -min(cy,0.)*grad(Istr-1,j  )
     &                                       )/(cff+cx)
#    if defined M3_FRC_BRY  || defined M3CLIMATOLOGY 
            v(Istr-1,j,k,nnew)=(1.-tau)*v(Istr-1,j,k,nnew)
#     ifdef M3_FRC_BRY
     &                                 +tau*vbry_west(j,k)
#     else     
     &                               +tau*vclm(Istr-1,j,k)
#     endif
#    endif
#    ifdef MASKING
            v(Istr-1,j,k,nnew)=v(Istr-1,j,k,nnew)*vmask(Istr-1,j)
#    endif
#    ifdef WET_DRY
            v(Istr-1,j,k,nnew)=v(Istr-1,j,k,nnew)*vmask_wet(Istr-1,j)
#    endif
          enddo
        enddo
!
#   elif defined OBC_COM_M3SPECIFIED
!                                            Western edge Specified BC
!                                            ======= ==== ========= ==
        do k=1,N
          do j=JstrV,Jend
#    ifdef M3_FRC_BRY
            v(Istr-1,j,k,nnew)=vbry_west(j,k)       ! specified
#    else
            v(Istr-1,j,k,nnew)=vclm(Istr-1,j,k)
#    endif
#    ifdef MASKING
     &                         *vmask(Istr-1,j)
#    endif
#    ifdef WET_DRY
     &                         *vmask_wet(Istr-1,j)
#    endif
          enddo
        enddo
#   else
!                                            Western edge gradient BC
!                                            ======= ==== ======== ==
        do k=1,N
          do j=JstrV,Jend
            v(Istr-1,j,k,nnew)=v(Istr,j,k,nnew)  ! gradient (default)
#    ifdef MASKING
     &                         *vmask(Istr-1,j)
#    endif
#    ifdef WET_DRY
     &                         *vmask_wet(Istr-1,j)
#    endif
          enddo
        enddo
#   endif
#  else
#   ifdef NS_COM_PERIODIC
#    define J_RANGE JstrV,Jend
#   else
#    define J_RANGE Jstr,JendR
#   endif
        do k=1,N                        ! Wall: free-slip (gamma2=+1)
          do j=J_RANGE                  ! =====   no-slip (gamma2=-1)
            v(Istr-1,j,k,nnew)=gamma2*v(Istr,j,k,nnew)
#   ifdef MASKING
     &                                *vmask(Istr-1,j)
#   endif
          enddo
        enddo
#   undef J_RANGE
#  endif
      endif          !<-- WESTERN_EDGE
!
!====================================================================
!                            EASTERN BC
!====================================================================
      if (EASTERN_EDGE) then
#  ifdef OBC_COM_EAST
#   ifdef OBC_COM_M3ORLANSKI
        do k=1,N                             ! Eastern edge radiation
          do j=JstrV-1,Jend                  ! ======= ==== =========
            grad(Iend  ,j)=v(Iend  ,j+1,k,nstp)-v(Iend  ,j,k,nstp)
            grad(Iend+1,j)=v(Iend+1,j+1,k,nstp)-v(Iend+1,j,k,nstp)
          enddo
          do j=JstrV,Jend
            dft=v(Iend,j,k,nstp)-v(Iend  ,j,k,nnew)
            dfx=v(Iend,j,k,nnew)-v(Iend-1,j,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                       ! <-- cancel cx, if inflow
#    if defined M3_FRC_BRY || defined M3CLIMATOLOGY
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
 
            v(Iend+1,j,k,nnew)=( cff*v(Iend+1,j,k,nstp)
     &                              +cx*v(Iend,j,k,nnew)
     &                      -max(cy,0.)*grad(Iend+1,j-1)
     &                      -min(cy,0.)*grad(Iend+1,j  )
     &                                       )/(cff+cx)
#    if defined M3_FRC_BRY  || defined M3CLIMATOLOGY 
            v(Iend+1,j,k,nnew)=(1.-tau)*v(Iend+1,j,k,nnew)
#     ifdef M3_FRC_BRY
     &                                 +tau*vbry_east(j,k)
#     else     
     &                               +tau*vclm(Iend+1,j,k)
#     endif
#    endif
#    ifdef MASKING
            v(Iend+1,j,k,nnew)=v(Iend+1,j,k,nnew)*vmask(Iend+1,j)
#    endif
#    ifdef WET_DRY
            v(Iend+1,j,k,nnew)=v(Iend+1,j,k,nnew)*vmask_wet(Iend+1,j)
#    endif
          enddo
        enddo
!
#   elif defined OBC_COM_M3SPECIFIED
!                                            Eastern edge Specified BC
!                                            ======= ==== ========= ==
        do k=1,N
          do j=Jstr,Jend
#    ifdef M3_FRC_BRY
            v(Iend+1,j,k,nnew)=vbry_east(j,k)       ! specified
#    else
            v(Iend+1,j,k,nnew)=vclm(Iend+1,j,k)
#    endif
#    ifdef MASKING
     &                         *vmask(Iend+1,j)
#    endif
#    ifdef WET_DRY
     &                         *vmask_wet(Iend+1,j)
#    endif
          enddo
        enddo
#   else
!                                            Eastern edge gradient BC
!                                            ======= ==== ======== ==
        do k=1,N
          do j=Jstr,Jend
            v(Iend+1,j,k,nnew)=v(Iend,j,k,nnew)  ! gradient (default)
#    ifdef MASKING
     &                         *vmask(Iend+1,j)
#    endif
#    ifdef WET_DRY
     &                         *vmask_wet(Iend+1,j)
#    endif
          enddo
        enddo
#   endif
#  else
#   ifdef NS_COM_PERIODIC
#    define J_RANGE JstrV,Jend
#   else
#    define J_RANGE Jstr,JendR
#   endif
        do k=1,N                        ! Wall: free-slip (gamma2=+1)
          do j=J_RANGE                  ! ====    no-slip (gamma2=-1)
            v(Iend+1,j,k,nnew)=gamma2*v(Iend,j,k,nnew)
#   ifdef MASKING
     &                                *vmask(Iend+1,j)
#   endif
          enddo
        enddo
#   undef J_RANGE
#  endif
      endif     !<-- EASTERN_EDGE
# endif          /* !EW_COM_PERIODIC */
 
                           ! Corners between adjacent open boundaries
                           ! ======= ======= ======== ==== ==========
 
# if defined OBC_COM_SOUTH && defined OBC_COM_WEST
      if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=1,N
          v(Istr-1,Jstr,k,nnew)=0.5*( v(Istr-1,Jstr+1,k,nnew)
     &                               +v(Istr  ,Jstr  ,k,nnew))
#  ifdef MASKING
     &                          *vmask(Istr-1,Jstr)
#  endif
        enddo
      endif
# endif
# if defined OBC_COM_SOUTH && defined OBC_COM_EAST
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=1,N
          v(Iend+1,Jstr,k,nnew)=0.5*( v(Iend+1,Jstr+1,k,nnew)
     &                               +v(Iend  ,Jstr  ,k,nnew))
#  ifdef MASKING
     &                          *vmask(Iend+1,Jstr)
#  endif
        enddo
      endif
# endif
# if defined OBC_COM_NORTH && defined OBC_COM_WEST
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        do k=1,N
          v(Istr-1,Jend+1,k,nnew)=0.5*( v(Istr-1,Jend,k,nnew)
     &                                 +v(Istr,Jend+1,k,nnew))
#  ifdef MASKING
     &                            *vmask(Istr-1,Jend+1)
#  endif
        enddo
      endif
# endif
# if defined OBC_COM_NORTH && defined OBC_COM_EAST
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        do k=1,N
          v(Iend+1,Jend+1,k,nnew)=0.5*( v(Iend+1,Jend,k,nnew)
     &                                 +v(Iend,Jend+1,k,nnew))
#  ifdef MASKING
     &                            *vmask(Iend+1,Jend+1)
#  endif
        enddo
      endif
# endif
      return
      end
#else
      subroutine v3dbc_empty
      end
#endif /* SOLVE3D */
#ifndef CHILD
# define CHILD
# ifdef AGRIF
#  include "v3dbc.F"
# endif
# undef CHILD
#endif  /* !CHILD */
 
