! $Id: t3dbc.F 1458 2014-02-03 15:01:25Z gcambon $
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
      subroutine t3dbc_tile(Istr,Iend,Jstr,Jend,indx,itrc,grad)
#  ifdef AGRIF      
      use Agrif_Util
      integer Istr,Iend,Jstr,Jend,indx,itrc
      real grad(PRIVATE_2D_SCRATCH_ARRAY)
      if (Agrif_Root()) then
        call t3dbc_parent_tile(Istr,Iend,Jstr,Jend,indx,itrc,grad)
      else
        call t3dbc_child_tile(Istr,Iend,Jstr,Jend,indx,itrc,grad)
c        call t3dbc_interp_tile(Istr,Iend,Jstr,Jend,indx,itrc)
      endif
      return
      end
!
! PARENT
!
      subroutine t3dbc_parent_tile(Istr,Iend,Jstr,Jend,indx,itrc,grad)
#  endif
!
! Set lateral boundary conditions for tracer field t(:,:,:,indx,itrc)
! for the parent grid.
!
# endif /* SOLVE3D */
#else
# ifdef SOLVE3D
!
! CHILD
!
      subroutine t3dbc_child_tile(Istr,Iend,Jstr,Jend,indx,itrc,grad)
!
! Set lateral boundary conditions for tracer field t(:,:,:,indx,itrc)
! for the child grid.
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
      integer Istr,Iend,Jstr,Jend,indx,itrc,i,j,k
      real    grad(PRIVATE_2D_SCRATCH_ARRAY)
      real    eps, cff,
     &        cx,cy, dft,dfx,dfy, tau,tau_in,tau_out
      parameter (eps=1.E-20)
!
# include "compute_auxiliary_bounds.h"
!
! Interpolations of the parent values to get tbry_east or tclm
!
# ifdef CHILD
      call t3dbc_interp_tile(Istr,Iend,Jstr,Jend,indx,itrc)
# endif
!
# if defined T_FRC_BRY || defined TCLIMATOLOGY
      tau_in=dt*tauT_in
      tau_out=dt*tauT_out
# endif
!
# ifndef EW_COM_PERIODIC
!
!====================================================================
!                            WESTERN BC
!====================================================================
      if (WESTERN_EDGE) then
#  if defined OBC_COM_WEST && defined OBC_COM_TORLANSKI
!                                           Western edge radiation BC
!                                           ======= ==== ========= ==
        do k=1,N
          do j=Jstr,Jend+1
            grad(Istr-1,j)=( t(Istr-1,j  ,k,nstp,itrc)
     &                      -t(Istr-1,j-1,k,nstp,itrc))
#   ifdef MASKING
     &                                 *vmask(Istr-1,j)
#   endif
            grad(Istr  ,j)=( t(Istr  ,j  ,k,nstp,itrc)
     &                      -t(Istr  ,j-1,k,nstp,itrc))
#   ifdef MASKING
     &                                   *vmask(Istr,j)
#   endif
          enddo
          do j=Jstr,Jend
            dft=t(Istr,j,k,nstp,itrc)-t(Istr  ,j,k,indx,itrc)
            dfx=t(Istr,j,k,indx,itrc)-t(Istr+1,j,k,indx,itrc)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                  ! <-- cancel, if inflow 
#   if defined T_FRC_BRY || defined TCLIMATOLOGY
              tau=tau_in
            else
              tau=tau_out
#   endif
            endif
 
            if (dft*(grad(Istr,j)+grad(Istr,j+1)) .gt. 0.) then
              dfy=grad(Istr,j)
            else
              dfy=grad(Istr,j+1)
            endif
 
#   ifdef OBC_COM_RAD_NORMAL
            dfy=0.
#   endif
            cff=max(dfx*dfx+dfy*dfy, eps)
            cx=dft*dfx
#   ifdef OBC_COM_RAD_NPO
            cy=0.
#   else
            cy=min(cff,max(dft*dfy,-cff))
#   endif
            t(Istr-1,j,k,indx,itrc)=( cff*t(Istr-1,j,k,nstp,itrc)
     &                                   +cx*t(Istr,j,k,indx,itrc)
     &                                -max(cy,0.)*grad(Istr-1,j  )
     &                                -min(cy,0.)*grad(Istr-1,j+1)
     &                                                 )/(cff+cx)
#   if defined T_FRC_BRY || defined TCLIMATOLOGY
            t(Istr-1,j,k,indx,itrc)=(1.-tau)*t(Istr-1,j,k,indx,itrc)
#    ifdef T_FRC_BRY
     &                                    +tau*tbry_west(j,k,itrc)     
#    else
     &                                    +tau*tclm(Istr-1,j,k,itrc)
#    endif
#   endif
#   ifdef MASKING
            t(Istr-1,j,k,indx,itrc)=t(Istr-1,j,k,indx,itrc)
     &                                      *rmask(Istr-1,j)
#   endif
          enddo
        enddo
#  elif defined OBC_COM_WEST && defined OBC_COM_TUPWIND
!                                               Western edge Upwind BC
!                                               ======= ==== ====== ==
        do k=1,N
          do j=Jstr,Jend
            if (u(Istr+1,j,k,nstp).lt.0.) then
              t(Istr-1,j,k,indx,itrc)=t(Istr,j,k,indx,itrc)
            else
#   ifdef T_FRC_BRY
              t(Istr-1,j,k,indx,itrc)=tbry_west(j,k,itrc)
#   else
              t(Istr-1,j,k,indx,itrc)=tclm(Istr-1,j,k,itrc)
#   endif
           endif
#   ifdef MASKING
              t(Istr-1,j,k,indx,itrc)=t(Istr-1,j,k,indx,itrc)
     &                                       *rmask(Istr-1,j)
#   endif
         enddo
        enddo
#  elif defined OBC_COM_WEST && defined OBC_COM_TSPECIFIED
!                                            Western edge Specified BC
!                                            ======= ==== ========= ==
        do k=1,N
          do j=Jstr,Jend
#   ifdef T_FRC_BRY
              t(Istr-1,j,k,indx,itrc)=tbry_west(j,k,itrc)
#   else
              t(Istr-1,j,k,indx,itrc)=tclm(Istr-1,j,k,itrc)
#   endif
#   ifdef MASKING
     &                                       *rmask(Istr-1,j)
#   endif
         enddo
        enddo
#  else
        do k=1,N
          do j=Jstr,Jend
!                                            Western edge gradient BC
!                                            ======= ==== ======== ==
            t(Istr-1,j,k,indx,itrc)=t(Istr,j,k,indx,itrc)
#   ifdef MASKING
     &                              *rmask(Istr-1,j)
#   endif
          enddo
        enddo
#  endif
      endif     ! <-- WESTERN_EDGE
!
!====================================================================
!                            EASTERN BC
!====================================================================
      if (EASTERN_EDGE) then
#  if defined OBC_COM_EAST && defined OBC_COM_TORLANSKI
!                                           Eastern edge radiation BC
!                                           ======= ==== ========= ==
        do k=1,N
          do j=Jstr,Jend+1
           grad(Iend  ,j)=( t(Iend  ,j  ,k,nstp,itrc)
     &                     -t(Iend  ,j-1,k,nstp,itrc))
#   ifdef MASKING
     &                                  *vmask(Iend,j)
#   endif
           grad(Iend+1,j)=( t(Iend+1,j  ,k,nstp,itrc)
     &                     -t(Iend+1,j-1,k,nstp,itrc))
#   ifdef MASKING
     &                                *vmask(Iend+1,j)
#   endif
          enddo
          do j=Jstr,Jend
            dft=t(Iend,j,k,nstp,itrc)-t(Iend  ,j,k,indx,itrc)
            dfx=t(Iend,j,k,indx,itrc)-t(Iend-1,j,k,indx,itrc)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                  ! <-- cancel, if inflow 
#   if defined T_FRC_BRY || defined TCLIMATOLOGY
              tau=tau_in
            else
              tau=tau_out
#   endif
            endif
 
            if (dft*(grad(Iend,j)+grad(Iend,j+1)) .gt. 0.) then
              dfy=grad(Iend,j)
            else
              dfy=grad(Iend,j+1)
            endif
 
#   ifdef OBC_COM_RAD_NORMAL
            dfy=0.
#   endif
            cff=max(dfx*dfx+dfy*dfy, eps)
            cx=dft*dfx
#   ifdef OBC_COM_RAD_NPO
            cy=0.
#   else
            cy=min(cff,max(dft*dfy,-cff))
#   endif
 
            t(Iend+1,j,k,indx,itrc)=( cff*t(Iend+1,j,k,nstp,itrc)
     &                                   +cx*t(Iend,j,k,indx,itrc)
     &                                -max(cy,0.)*grad(Iend+1,j  )
     &                                -min(cy,0.)*grad(Iend+1,j+1)
     &                                                 )/(cff+cx)
#   if defined T_FRC_BRY || defined TCLIMATOLOGY
            t(Iend+1,j,k,indx,itrc)=(1.-tau)*t(Iend+1,j,k,indx,itrc)
#    ifdef T_FRC_BRY
     &                                      +tau*tbry_east(j,k,itrc)
#    else
     &                                    +tau*tclm(Iend+1,j,k,itrc)
#    endif
#   endif
#   ifdef MASKING
            t(Iend+1,j,k,indx,itrc)=t(Iend+1,j,k,indx,itrc)
     &                                     *rmask(Iend+1,j)
#   endif
          enddo
        enddo
#  elif defined OBC_COM_EAST && defined OBC_COM_TUPWIND
!                                            Eastern edge Upwind BC
!                                            ======= ==== ====== ==
        do k=1,N
          do j=Jstr,Jend
            if (u(Iend-1,j,k,nstp).gt.0.) then
              t(Iend+1,j,k,indx,itrc)=t(Iend,j,k,indx,itrc)
            else
#   ifdef T_FRC_BRY
              t(Iend+1,j,k,indx,itrc)=tbry_east(j,k,itrc)
#   else
              t(Iend+1,j,k,indx,itrc)=tclm(Iend+1,j,k,itrc)
#   endif
            endif
#   ifdef MASKING
              t(Iend+1,j,k,indx,itrc)=t(Iend+1,j,k,indx,itrc)
     &                                       *rmask(Iend+1,j)
#   endif
          enddo
        enddo
#  elif defined OBC_COM_EAST && defined OBC_COM_TSPECIFIED
!                                            Eastern edge Specified BC
!                                            ======= ==== ========= ==
        do k=1,N
          do j=Jstr,Jend
#   ifdef T_FRC_BRY
            t(Iend+1,j,k,indx,itrc)=tbry_east(j,k,itrc) 
#   else
            t(Iend+1,j,k,indx,itrc)=tclm(Iend+1,j,k,itrc)
#   endif
#   ifdef MASKING
     &                                   *rmask(Iend+1,j)
#   endif
          enddo
        enddo
#  else 
!                                            Eastern edge gradient BC
!                                            ======= ==== ======== ==
        do k=1,N
          do j=Jstr,Jend
            t(Iend+1,j,k,indx,itrc)=t(Iend,j,k,indx,itrc)
#   ifdef MASKING
     &                                   *rmask(Iend+1,j)
#   endif
          enddo
        enddo
#  endif
      endif    ! <-- EASTERN_EDGE
# endif        /* !EW_COM_PERIODIC */

# ifndef NS_COM_PERIODIC
!
!====================================================================
!                            SOUTHERN BC
!====================================================================
      if (SOUTHERN_EDGE) then
#  if defined OBC_COM_SOUTH && defined OBC_COM_TORLANSKI
!                                          Southern edge radiation BC
!                                          ======== ==== ========= ==
        do k=1,N
          do i=Istr,Iend+1
            grad(i,Jstr  )=( t(i  ,Jstr  ,k,nstp,itrc)
     &                      -t(i-1,Jstr  ,k,nstp,itrc))
#   ifdef MASKING
     &                                   *umask(i,Jstr)
#   endif
            grad(i,Jstr-1)=( t(i  ,Jstr-1,k,nstp,itrc)
     &                      -t(i-1,Jstr-1,k,nstp,itrc))
#   ifdef MASKING
     &                                *umask(i,Jstr-1)
#   endif
          enddo
          do i=Istr,Iend
            dft=t(i,Jstr,k,nstp,itrc)-t(i,Jstr  ,k,indx,itrc)
            dfx=t(i,Jstr,k,indx,itrc)-t(i,Jstr+1,k,indx,itrc)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                   ! <-- cancel, if inflow 
#   if defined T_FRC_BRY || defined TCLIMATOLOGY
              tau=tau_in
            else
              tau=tau_out
#   endif
            endif
 
            if (dft*(grad(i,Jstr)+grad(i+1,Jstr)) .gt. 0.) then
              dfy=grad(i,Jstr)
            else
              dfy=grad(i+1,Jstr)
            endif
 
#   ifdef OBC_COM_RAD_NORMAL
            dfy=0.
#   endif
            cff=max(dfx*dfx+dfy*dfy, eps)
            cx=dft*dfx
#   ifdef OBC_COM_RAD_NPO
            cy=0.
#   else
            cy=min(cff,max(dft*dfy,-cff))
#   endif
 
            t(i,Jstr-1,k,indx,itrc)=( cff*t(i,Jstr-1,k,nstp,itrc)
     &                                   +cx*t(i,Jstr,k,indx,itrc)
     &                                -max(cy,0.)*grad(i  ,Jstr-1)
     &                                -min(cy,0.)*grad(i+1,Jstr-1)
     &                                                 )/(cff+cx)
#   if defined T_FRC_BRY || defined TCLIMATOLOGY
            t(i,Jstr-1,k,indx,itrc)=(1.-tau)*t(i,Jstr-1,k,indx,itrc)
#    ifdef T_FRC_BRY
     &                                     +tau*tbry_south(i,k,itrc)
#    else
     &                                    +tau*tclm(i,Jstr-1,k,itrc)
#    endif
#   endif
#   ifdef MASKING
            t(i,Jstr-1,k,indx,itrc)=t(i,Jstr-1,k,indx,itrc)
     &                                     *rmask(i,Jstr-1)
#   endif
          enddo
        enddo
#  elif defined OBC_COM_SOUTH && defined OBC_COM_TUPWIND
!                                           Southern edge Upwind BC
!                                           ======== ==== ====== ==
        do k=1,N
          do i=Istr,Iend
            if (v(i,Jstr+1,k,nstp).lt.0.) then
              t(i,Jstr-1,k,indx,itrc)=t(i,Jstr,k,indx,itrc)
            else
#   ifdef T_FRC_BRY
              t(i,Jstr-1,k,indx,itrc)=tbry_south(i,k,itrc)
#   else
              t(i,Jstr-1,k,indx,itrc)=tclm(i,Jstr-1,k,itrc)
#   endif
            endif
#   ifdef MASKING
              t(i,Jstr-1,k,indx,itrc)=t(i,Jstr-1,k,indx,itrc)
     &                                       *rmask(i,Jstr-1)
#   endif
          enddo
        enddo
#  elif defined OBC_COM_SOUTH && defined OBC_COM_TSPECIFIED
!                                           Southern edge Specified BC
!                                           ======== ==== ========= ==
        do k=1,N
          do i=Istr,Iend
#   ifdef T_FRC_BRY
            t(i,Jstr-1,k,indx,itrc)=tbry_south(i,k,itrc)
#   else
            t(i,Jstr-1,k,indx,itrc)=tclm(i,Jstr-1,k,itrc)
#   endif
#   ifdef MASKING
     &                                   *rmask(i,Jstr-1)
#   endif
          enddo
        enddo
#  else
!                                           Southern edge gradient BC
!                                           ======== ==== ======== ==
        do k=1,N
          do i=Istr,Iend
            t(i,Jstr-1,k,indx,itrc)=t(i,Jstr,k,indx,itrc)
#   ifdef MASKING
     &                                   *rmask(i,Jstr-1)
#   endif
          enddo
        enddo
#  endif
      endif    ! <-- SOUTHERN_EDGE
!
!====================================================================
!                            NORTHERN BC
!====================================================================
      if (NORTHERN_EDGE) then
#  if defined OBC_COM_NORTH && defined OBC_COM_TORLANSKI
!                                          Northern edge radiation BC
!                                          ======== ==== ========= ==
        do k=1,N
          do i=Istr,Iend+1
            grad(i,Jend  )=( t(i  ,Jend  ,k,nstp,itrc)
     &                      -t(i-1,Jend  ,k,nstp,itrc))
#   ifdef MASKING
     &                                   *umask(i,Jend)
#   endif
            grad(i,Jend+1)=( t(i  ,Jend+1,k,nstp,itrc)
     &                      -t(i-1,Jend+1,k,nstp,itrc))
#   ifdef MASKING
     &                                 *umask(i,Jend+1)
#   endif
          enddo
          do i=Istr,Iend
            dft=t(i,Jend,k,nstp,itrc)-t(i,Jend  ,k,indx,itrc)
            dfx=t(i,Jend,k,indx,itrc)-t(i,Jend-1,k,indx,itrc)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                          ! <-- cancel, if inflow 
#   if defined T_FRC_BRY || defined TCLIMATOLOGY
              tau=tau_in
            else
              tau=tau_out
#   endif
            endif
 
            if (dft*(grad(i,Jend)+grad(i+1,Jend)) .gt. 0.) then
              dfy=grad(i,Jend)
            else
              dfy=grad(i+1,Jend)
            endif
 
#   ifdef OBC_COM_RAD_NORMAL
            dfy=0.
#   endif
            cff=max(dfx*dfx+dfy*dfy, eps)
            cx=dft*dfx
#   ifdef OBC_COM_RAD_NPO
            cy=0.
#   else
            cy=min(cff,max(dft*dfy,-cff))
#   endif
            t(i,Jend+1,k,indx,itrc)=( cff*t(i,Jend+1,k,nstp,itrc)
     &                                 +cx*t(i,Jend  ,k,indx,itrc)
     &                                -max(cy,0.)*grad(i  ,Jend+1)
     &                                -min(cy,0.)*grad(i+1,Jend+1)
     &                                                 )/(cff+cx)
#   if defined T_FRC_BRY || defined TCLIMATOLOGY
            t(i,Jend+1,k,indx,itrc)=(1.-tau)*t(i,Jend+1,k,indx,itrc)
#    ifdef T_FRC_BRY
     &                                     +tau*tbry_north(i,k,itrc)
#    else
     &                                    +tau*tclm(i,Jend+1,k,itrc)
#    endif
#   endif
#   ifdef MASKING
            t(i,Jend+1,k,indx,itrc)=t(i,Jend+1,k,indx,itrc)
     &                                     *rmask(i,Jend+1)
#   endif
          enddo
        enddo
#  elif defined OBC_COM_NORTH && defined OBC_COM_TUPWIND
!                                           Northern edge Upwind BC
!                                           ======== ==== ====== ==
        do k=1,N
          do i=Istr,Iend
            if (v(i,Jend-1,k,nstp).gt.0.) then
              t(i,Jend+1,k,indx,itrc)=t(i,Jend,k,indx,itrc)
            else
#   ifdef T_FRC_BRY
              t(i,Jend+1,k,indx,itrc)=tbry_north(i,k,itrc)
#   else
              t(i,Jend+1,k,indx,itrc)=tclm(i,Jend+1,k,itrc)
#   endif
            endif
#   ifdef MASKING
              t(i,Jend+1,k,indx,itrc)=t(i,Jend+1,k,indx,itrc)
     &                                       *rmask(i,Jend+1)
#   endif
          enddo
        enddo
#  elif defined OBC_COM_NORTH && defined OBC_COM_TSPECIFIED
!                                           Northern edge Specified BC
!                                           ======== ==== ========= ==
        do k=1,N
          do i=Istr,Iend
#   ifdef T_FRC_BRY
            t(i,Jend+1,k,indx,itrc)=tbry_north(i,k,itrc)
#   else
            t(i,Jend+1,k,indx,itrc)=tclm(i,Jend+1,k,itrc)
#   endif
#   ifdef MASKING
     &                                   *rmask(i,Jend+1)
#   endif
          enddo
        enddo
#  else
!                                           Northern edge gradient BC
!                                           ======== ==== ======== ==
        do k=1,N
          do i=Istr,Iend
            t(i,Jend+1,k,indx,itrc)=t(i,Jend,k,indx,itrc)
#   ifdef MASKING
     &                                   *rmask(i,Jend+1)
#   endif
          enddo
        enddo
#  endif
      endif    ! <-- NORTHERN_EDGE
# endif /* ! NS_COM_PERIODIC */

                           ! Corners between adjacent open boundaries
                           ! ======= ======= ======== ==== ==========

# if defined OBC_COM_SOUTH && defined OBC_COM_WEST
      if (SOUTHERN_EDGE .and. WESTERN_EDGE) then
        do k=1,N
          t(Istr-1,Jstr-1,k,indx,itrc)=0.5*
     &                          ( t(Istr,Jstr-1,k,indx,itrc)
     &                           +t(Istr-1,Jstr,k,indx,itrc))
#   ifdef MASKING
     &                                 *rmask(Istr-1,Jstr-1)
#   endif
        enddo
      endif
# endif
# if defined OBC_COM_SOUTH && defined OBC_COM_EAST
      if (SOUTHERN_EDGE .and. EASTERN_EDGE) then
        do k=1,N
          t(Iend+1,Jstr-1,k,indx,itrc)=0.5*
     &                          (t(Iend,Jstr-1,k,indx,itrc)
     &                          +t(Iend+1,Jstr,k,indx,itrc))
#   ifdef MASKING
     &                                 *rmask(Iend+1,Jstr-1)
#   endif
        enddo
      endif
# endif
# if defined OBC_COM_NORTH && defined OBC_COM_WEST
      if (NORTHERN_EDGE .and. WESTERN_EDGE) then
        do k=1,N
          t(Istr-1,Jend+1,k,indx,itrc)=0.5*
     &                          ( t(Istr,Jend+1,k,indx,itrc)
     &                           +t(Istr-1,Jend,k,indx,itrc))
#   ifdef MASKING
     &                                 *rmask(Istr-1,Jend+1)
#   endif
        enddo
      endif
# endif
# if defined OBC_COM_NORTH && defined OBC_COM_EAST
      if (NORTHERN_EDGE .and. EASTERN_EDGE) then
        do k=1,N
          t(Iend+1,Jend+1,k,indx,itrc)=0.5*
     &                          ( t(Iend,Jend+1,k,indx,itrc)
     &                           +t(Iend+1,Jend,k,indx,itrc))
#   ifdef MASKING
     &                                 *rmask(Iend+1,Jend+1)
#   endif
        enddo
      endif
# endif
      return
      end
#else
      subroutine t3dbc_empty
      end
#endif /* SOLVE3D */
#ifndef CHILD
# define CHILD
# ifdef AGRIF
#  include "t3dbc.F"
# endif
# undef CHILD
#endif  /* !CHILD */
