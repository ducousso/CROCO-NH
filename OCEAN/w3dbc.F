! $Id:$
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
      subroutine w3dbc_tile(Istr,Iend,Jstr,Jend,grad)
#  ifdef AGRIF      
      use Agrif_Util
      integer Istr,Iend,Jstr,Jend
      real grad(PRIVATE_2D_SCRATCH_ARRAY)
      if (Agrif_Root()) then
        call w3dbc_parent_tile(Istr,Iend,Jstr,Jend,grad)
      else
        call w3dbc_child_tile(Istr,Iend,Jstr,Jend,grad)
      endif
      return
      end
!
! PARENT
!
      subroutine w3dbc_parent_tile(Istr,Iend,Jstr,Jend,grad)
#  endif
!
! Set lateral boundary conditions for field wz(:,:,:,nnew)
! for the parent grid.
!
# endif /* NBQ */
#else
# ifdef NBQ
!
! CHILD
!
      subroutine w3dbc_child_tile(Istr,Iend,Jstr,Jend,grad)
!
! Set lateral boundary conditions for field wz(:,:,:,nnew)
! for the child grid.
!
# endif /* NBQ */
#endif /* CHILD */
#ifdef NBQ
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
      integer Istr,Iend,Jstr,Jend,i,j,k
      real    grad(PRIVATE_2D_SCRATCH_ARRAY)
      real    eps, cff,
     &        cx,cy, dft,dfx,dfy, tau,tau_in,tau_out
      parameter (eps=1.E-20)
!
# include "compute_auxiliary_bounds.h"
!
! Interpolations of the parent values to get wbry_east or wclm
!
# if defined W_FRC_BRY || defined WCLIMATOLOGY
      tau_in=dt*tauM_in
      tau_out=dt*tauM_out
# endif
!
# ifndef EW_COM_PERIODIC
!
!====================================================================
!                            WESTERN BC
!====================================================================
      if (WESTERN_EDGE) then
#  ifdef OBC_COM_WEST
#   ifdef OBC_COM_M3ORLANSKI
!                                           Western edge radiation BC
!                                           ======= ==== ========= ==
        do k=1,N
          do j=Jstr,Jend+1
            grad(Istr-1,j)=( wz(Istr-1,j  ,k,nstp)
     &                      -wz(Istr-1,j-1,k,nstp))
#    ifdef MASKING
     &                                 *vmask(Istr-1,j)
#    endif
            grad(Istr  ,j)=( wz(Istr  ,j  ,k,nstp)
     &                      -wz(Istr  ,j-1,k,nstp))
#    ifdef MASKING
     &                                   *vmask(Istr,j)
#    endif
          enddo
          do j=Jstr,Jend
            dft=wz(Istr,j,k,nstp)-wz(Istr  ,j,k,nnew)
            dfx=wz(Istr,j,k,nnew)-wz(Istr+1,j,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                  ! <-- cancel, if inflow 
#    if defined W_FRC_BRY || defined WCLIMATOLOGY
              tau=tau_in
            else
              tau=tau_out
#    endif
            endif
 
            if (dft*(grad(Istr,j)+grad(Istr,j+1)) .gt. 0.) then
              dfy=grad(Istr,j)
            else
              dfy=grad(Istr,j+1)
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
            wz(Istr-1,j,k,nnew)=( cff*wz(Istr-1,j  ,k,nstp)
     &                            +cx*wz(Istr  ,j  ,k,nnew)
     &                  -max(cy,0.)*grad(Istr-1,j  )
     &                  -min(cy,0.)*grad(Istr-1,j+1)
     &                                                 )/(cff+cx)
#    if defined W_FRC_BRY || defined WCLIMATOLOGY
            wz(Istr-1,j,k,nnew)=(1.-tau)*wz(Istr-1,j,k,nnew)
#     ifdef W_FRC_BRY
     &                                  +tau*wbry_west(j,k)     
#     else
     &                                  +tau*wclm(Istr-1,j,k)
#     endif
#    endif
#    ifdef MASKING
            wz(Istr-1,j,k,nnew)=wz(Istr-1,j,k,nnew)*rmask(Istr-1,j)
#    endif
#    ifdef WET_DRY
            wz(Istr-1,j,k,nnew)=wz(Istr-1,j,k,nnew)*rmask_wet(Istr-1,j)
#    endif
          enddo
        enddo
#   elif defined OBC_COM_WSPECIFIED
!                                            Western edge Specified BC
!                                            ======= ==== ========= ==
        do k=1,N
          do j=Jstr,Jend
#    ifdef W_FRC_BRY
            wz(Istr-1,j,k,nnew)=wbry_west(j,k)
#    else
            wz(Istr-1,j,k,nnew)=wclm(Istr-1,j,k)
#    endif
#    ifdef MASKING
     &                         *rmask(Istr-1,j)
#    endif
#    ifdef WET_DRY
     &                         *rmask_wet(Istr-1,j)
#    endif
         enddo
        enddo
#   else
        do k=1,N
          do j=Jstr,Jend
!                                            Western edge gradient BC
!                                            ======= ==== ======== ==
            wz(Istr-1,j,k,nnew)=wz(Istr,j,k,nnew)
#    ifdef MASKING
     &                         *rmask(Istr-1,j)
#    endif
#    ifdef WET_DRY
     &                         *rmask_wet(Istr-1,j)
#    endif
          enddo
        enddo
#   endif
!
#  else           /* alternative to open */
        do k=1,N                               ! Western edge closed
          do j=jstr,jend                       ! ======= ==== ======
            wz(Istr-1,j,k,nnew)=0.             !  (no-flux: default)
          enddo
        enddo

#  endif        /* OBC_COM_WEST */
      endif     ! <-- WESTERN_EDGE
!
!====================================================================
!                            EASTERN BC
!====================================================================
      if (EASTERN_EDGE) then
#  ifdef OBC_COM_EAST
#   ifdef OBC_COM_M3ORLANSKI
!                                           Eastern edge radiation BC
!                                           ======= ==== ========= ==
        do k=1,N
          do j=Jstr,Jend+1
           grad(Iend  ,j)=( wz(Iend  ,j  ,k,nstp)
     &                     -wz(Iend  ,j-1,k,nstp))
#    ifdef MASKING
     &                                  *vmask(Iend  ,j)
#    endif
           grad(Iend+1,j)=( wz(Iend+1,j  ,k,nstp)
     &                     -wz(Iend+1,j-1,k,nstp))
#    ifdef MASKING
     &                                  *vmask(Iend+1,j)
#    endif
          enddo
          do j=Jstr,Jend
            dft=wz(Iend,j,k,nstp)-wz(Iend  ,j,k,nnew)
            dfx=wz(Iend,j,k,nnew)-wz(Iend-1,j,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                  ! <-- cancel, if inflow 
#    if defined W_FRC_BRY || defined WCLIMATOLOGY
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
 
            wz(Iend+1,j,k,nnew)=( cff*wz(Iend+1,j  ,k,nstp)
     &                            +cx*wz(Iend  ,j  ,k,nnew)
     &                  -max(cy,0.)*grad(Iend+1,j  )
     &                  -min(cy,0.)*grad(Iend+1,j+1)
     &                                                 )/(cff+cx)
#    if defined W_FRC_BRY || defined WCLIMATOLOGY
            wz(Iend+1,j,k,nnew)=(1.-tau)*wz(Iend+1,j,k,nnew)
#     ifdef W_FRC_BRY
     &                                  +tau*wbry_east(j,k)
#     else
     &                                  +tau*wclm(Iend+1,j,k)
#     endif
#    endif
#    ifdef MASKING
            wz(Iend+1,j,k,nnew)=wz(Iend+1,j,k,nnew)*rmask(Iend+1,j)
#    endif
#    ifdef WET_DRY
            wz(Iend+1,j,k,nnew)=wz(Iend+1,j,k,nnew)*rmask_wet(Iend+1,j)
#    endif
          enddo
        enddo
#   elif defined OBC_COM_WSPECIFIED
!                                            Eastern edge Specified BC
!                                            ======= ==== ========= ==
        do k=1,N
          do j=Jstr,Jend
#    ifdef W_FRC_BRY
            wz(Iend+1,j,k,nnew)=wbry_east(j,k) 
#    else
            wz(Iend+1,j,k,nnew)=wclm(Iend+1,j,k)
#    endif
#    ifdef MASKING
     &                         *rmask(Iend+1,j)
#    endif
#    ifdef WET_DRY
     &                         *rmask_wet(Iend+1,j)
#    endif
          enddo
        enddo
#   else 
!                                            Eastern edge gradient BC
!                                            ======= ==== ======== ==
        do k=1,N
          do j=Jstr,Jend
            wz(Iend+1,j,k,nnew)=wz(Iend,j,k,nnew)
#    ifdef MASKING
     &                         *rmask(Iend+1,j)
#    endif
#    ifdef WET_DRY
     &                         *rmask_wet(Iend+1,j)
#    endif
          enddo
        enddo
#   endif
!
#  else           /* alternative to open */
        do k=1,N                                ! Eastern edge closed
          do j=jstr,jend                        ! ======= ==== ======
            wz(Iend+1,j,k,nnew)=0.              !  (no-flux: default)
          enddo
        enddo
#  endif       /* OBC_COM_EAST */
      endif    ! <-- EASTERN_EDGE
# endif        /* !EW_COM_PERIODIC */

# ifndef NS_COM_PERIODIC
!
!====================================================================
!                            SOUTHERN BC
!====================================================================
      if (SOUTHERN_EDGE) then
#  ifdef OBC_COM_SOUTH
#   ifdef OBC_COM_M3ORLANSKI
!                                          Southern edge radiation BC
!                                          ======== ==== ========= ==
        do k=1,N
          do i=Istr,Iend+1
            grad(i,Jstr  )=( wz(i  ,Jstr  ,k,nstp)
     &                      -wz(i-1,Jstr  ,k,nstp))
#    ifdef MASKING
     &                                   *umask(i,Jstr  )
#    endif
            grad(i,Jstr-1)=( wz(i  ,Jstr-1,k,nstp)
     &                      -wz(i-1,Jstr-1,k,nstp))
#    ifdef MASKING
     &                                   *umask(i,Jstr-1)
#    endif
          enddo
          do i=Istr,Iend
            dft=wz(i,Jstr,k,nstp)-wz(i,Jstr  ,k,nnew)
            dfx=wz(i,Jstr,k,nnew)-wz(i,Jstr+1,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                   ! <-- cancel, if inflow 
#    if defined W_FRC_BRY || defined WCLIMATOLOGY
              tau=tau_in
            else
              tau=tau_out
#    endif
            endif
 
            if (dft*(grad(i,Jstr)+grad(i+1,Jstr)) .gt. 0.) then
              dfy=grad(i,Jstr)
            else
              dfy=grad(i+1,Jstr)
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
 
            wz(i,Jstr-1,k,nnew)=( cff*wz(i  ,Jstr-1,k,nstp)
     &                            +cx*wz(i  ,Jstr  ,k,nnew)
     &                  -max(cy,0.)*grad(i  ,Jstr-1)
     &                  -min(cy,0.)*grad(i+1,Jstr-1)
     &                                                 )/(cff+cx)
#    if defined W_FRC_BRY || defined WCLIMATOLOGY
            wz(i,Jstr-1,k,nnew)=(1.-tau)*wz(i,Jstr-1,k,nnew)
#     ifdef W_FRC_BRY
     &                                  +tau*wbry_south(i,k)
#     else
     &                                  +tau*wclm(i,Jstr-1,k)
#     endif
#    endif
#    ifdef MASKING
            wz(i,Jstr-1,k,nnew)=wz(i,Jstr-1,k,nnew)*rmask(i,Jstr-1)
#    endif
#    ifdef WET_DRY
            wz(i,Jstr-1,k,nnew)=wz(i,Jstr-1,k,nnew)*rmask_wet(i,Jstr-1)
#    endif
          enddo
        enddo
#   elif defined OBC_COM_WSPECIFIED
!                                           Southern edge Specified BC
!                                           ======== ==== ========= ==
        do k=1,N
          do i=Istr,Iend
#    ifdef W_FRC_BRY
            wz(i,Jstr-1,k,nnew)=wbry_south(i,k)
#    else
            wz(i,Jstr-1,k,nnew)=wclm(i,Jstr-1,k)
#    endif
#    ifdef MASKING
     &                         *rmask(i,Jstr-1)
#    endif
#    ifdef WET_DRY
     &                         *rmask_wet(i,Jstr-1)
#    endif
          enddo
        enddo
#   else
!                                           Southern edge gradient BC
!                                           ======== ==== ======== ==
        do k=1,N
          do i=Istr,Iend
            wz(i,Jstr-1,k,nnew)=wz(i,Jstr,k,nnew)
#    ifdef MASKING
     &                         *rmask(i,Jstr-1)
#    endif
#    ifdef WET_DRY
     &                         *rmask_wet(i,Jstr-1)
#    endif
          enddo
        enddo
#   endif
!
#  else           /* alternative to open */
        do k=1,N                               ! Southern edge closed
          do i=Istr,Iend                       ! ======== ==== ======
            wz(i,Jstr-1,k,nnew)=0.
          enddo
        enddo
#  endif        /* OBC_COM_SOUTH */
      endif    ! <-- SOUTHERN_EDGE
!
!====================================================================
!                            NORTHERN BC
!====================================================================
      if (NORTHERN_EDGE) then
#  ifdef OBC_COM_NORTH
#   ifdef OBC_COM_M3ORLANSKI
!                                          Northern edge radiation BC
!                                          ======== ==== ========= ==
        do k=1,N
          do i=Istr,Iend+1
            grad(i,Jend  )=( wz(i  ,Jend  ,k,nstp)
     &                      -wz(i-1,Jend  ,k,nstp))
#    ifdef MASKING
     &                                   *umask(i,Jend  )
#    endif
            grad(i,Jend+1)=( wz(i  ,Jend+1,k,nstp)
     &                      -wz(i-1,Jend+1,k,nstp))
#    ifdef MASKING
     &                                   *umask(i,Jend+1)
#    endif
          enddo
          do i=Istr,Iend
            dft=wz(i,Jend,k,nstp)-wz(i,Jend  ,k,nnew)
            dfx=wz(i,Jend,k,nnew)-wz(i,Jend-1,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                          ! <-- cancel, if inflow 
#    if defined W_FRC_BRY || defined WCLIMATOLOGY
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
            wz(i,Jend+1,k,nnew)=( cff*wz(i  ,Jend+1,k,nstp)
     &                            +cx*wz(i  ,Jend  ,k,nnew)
     &                  -max(cy,0.)*grad(i  ,Jend+1)
     &                  -min(cy,0.)*grad(i+1,Jend+1)
     &                                                 )/(cff+cx)
#    if defined W_FRC_BRY || defined WCLIMATOLOGY
            wz(i,Jend+1,k,nnew)=(1.-tau)*wz(i,Jend+1,k,nnew)
#     ifdef W_FRC_BRY
     &                                   +tau*wbry_north(i,k)
#     else
     &                                   +tau*wclm(i,Jend+1,k)
#     endif
#    endif
#    ifdef MASKING
            wz(i,Jend+1,k,nnew)=wz(i,Jend+1,k,nnew)*rmask(i,Jend+1)
#    endif
#    ifdef WET_DRY
            wz(i,Jend+1,k,nnew)=wz(i,Jend+1,k,nnew)*rmask_wet(i,Jend+1)
#    endif
          enddo
        enddo
#   elif defined OBC_COM_WSPECIFIED
!                                           Northern edge Specified BC
!                                           ======== ==== ========= ==
        do k=1,N
          do i=Istr,Iend
#    ifdef W_FRC_BRY
            wz(i,Jend+1,k,nnew)=wbry_north(i,k)
#    else
            wz(i,Jend+1,k,nnew)=wclm(i,Jend+1,k)
#    endif
#    ifdef MASKING
     &                         *rmask(i,Jend+1)
#    endif
#    ifdef WET_DRY
     &                         *rmask_wet(i,Jend+1)
#    endif
          enddo
        enddo
#   else
!                                           Northern edge gradient BC
!                                           ======== ==== ======== ==
        do k=1,N
          do i=Istr,Iend
            wz(i,Jend+1,k,nnew)=wz(i,Jend,k,nnew)
#    ifdef MASKING
     &                         *rmask(i,Jend+1)
#    endif
#    ifdef WET_DRY
     &                         *rmask_wet(i,Jend+1)
#    endif
          enddo
        enddo
#   endif
!
#  else           /* alternative to open */
        do k=1,N                               ! Northern edge closed
          do i=Istr,Iend                       ! ======== ==== ======
            wz(i,Jend+1,k,nnew)=0.
          enddo
        enddo
#  endif       /* OBC_COM_NORTH */
      endif    ! <-- NORTHERN_EDGE
# endif        /* ! NS_COM_PERIODIC */

                           ! Corners between adjacent open boundaries
                           ! ======= ======= ======== ==== ==========

# if defined OBC_COM_SOUTH && defined OBC_COM_WEST
      if (SOUTHERN_EDGE .and. WESTERN_EDGE) then
        do k=1,N
          wz(Istr-1,Jstr-1,k,nnew)=0.5*
     &                          ( wz(Istr  ,Jstr-1,k,nnew)
     &                           +wz(Istr-1,Jstr  ,k,nnew))
#   ifdef MASKING
     &                                *rmask(Istr-1,Jstr-1)
#   endif
        enddo
      endif
# endif
# if defined OBC_COM_SOUTH && defined OBC_COM_EAST
      if (SOUTHERN_EDGE .and. EASTERN_EDGE) then
        do k=1,N
          wz(Iend+1,Jstr-1,k,nnew)=0.5*
     &                          (wz(Iend  ,Jstr-1,k,nnew)
     &                          +wz(Iend+1,Jstr  ,k,nnew))
#   ifdef MASKING
     &                               *rmask(Iend+1,Jstr-1)
#   endif
        enddo
      endif
# endif
# if defined OBC_COM_NORTH && defined OBC_COM_WEST
      if (NORTHERN_EDGE .and. WESTERN_EDGE) then
        do k=1,N
          wz(Istr-1,Jend+1,k,nnew)=0.5*
     &                          ( wz(Istr  ,Jend+1,k,nnew)
     &                           +wz(Istr-1,Jend  ,k,nnew))
#   ifdef MASKING
     &                                *rmask(Istr-1,Jend+1)
#   endif
        enddo
      endif
# endif
# if defined OBC_COM_NORTH && defined OBC_COM_EAST
      if (NORTHERN_EDGE .and. EASTERN_EDGE) then
        do k=1,N
          wz(Iend+1,Jend+1,k,nnew)=0.5*
     &                          ( wz(Iend  ,Jend+1,k,nnew)
     &                           +wz(Iend+1,Jend  ,k,nnew))
#   ifdef MASKING
     &                                *rmask(Iend+1,Jend+1)
#   endif
        enddo
      endif
# endif
      return
      end
#else
      subroutine w3dbc_empty
      end
#endif /* NBQ */
