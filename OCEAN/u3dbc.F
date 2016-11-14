! $Id: u3dbc.F 1458 2014-02-03 15:01:25Z gcambon $
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
      subroutine u3dbc_tile (Istr,Iend,Jstr,Jend,grad)
#  ifdef AGRIF      
      use AGRIF_Util
      integer Istr,Iend,Jstr,Jend
      real grad(PRIVATE_2D_SCRATCH_ARRAY)
      if (AGRIF_Root()) then
        call u3dbc_parent_tile (Istr,Iend,Jstr,Jend,grad)
      else
        call u3dbc_child_tile (Istr,Iend,Jstr,Jend,grad)
      endif
      return
      end
!
! PARENT
!
      subroutine u3dbc_parent_tile (Istr,Iend,Jstr,Jend,grad)
#  endif
!
! Set lateral boundary conditions for XI-component velocity
! u(:,:,:,nnew) for the parent grid.
!
# endif /* SOLVE3D */
#else
# ifdef SOLVE3D
!
! CHILD
!
      subroutine u3dbc_child_tile (Istr,Iend,Jstr,Jend,grad)
!
! Set lateral boundary conditions for XI-component velocity
! u(:,:,:,nnew) for the child grid.
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
# ifdef MRL_WCI
#  include "forces.h"
# endif
      integer Istr,Iend,Jstr,Jend, i,j,k
      real    grad(PRIVATE_2D_SCRATCH_ARRAY)
      real    cff,eps,
     &        cx,cy, dft,dfx,dfy, tau,tau_in,tau_out
      parameter (eps=1.E-20)
!
# include "compute_auxiliary_bounds.h"
!
! Interpolations of the parent values to get ubry_east or uclm
!
# ifdef CHILD
      call u3dbc_interp_tile(Istr,Iend,Jstr,Jend)
# endif
!
# if defined M3_FRC_BRY || defined M3CLIMATOLOGY
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
        do k=1,N                             ! Western edge radiation
          do j=Jstr,Jend+1                   ! ======= ==== =========
            grad(Istr  ,j)=(u(Istr  ,j,k,nstp)-u(Istr  ,j-1,k,nstp))
#    ifdef MASKING
     &                                                *pmask(Istr,j)
#    endif
            grad(Istr+1,j)=(u(Istr+1,j,k,nstp)-u(Istr+1,j-1,k,nstp))
#    ifdef MASKING
     &                                              *pmask(Istr+1,j)
#    endif
          enddo
          do j=Jstr,Jend
            dft=u(Istr+1,j,k,nstp)-u(Istr+1,j,k,nnew)
            dfx=u(Istr+1,j,k,nnew)-u(Istr+2,j,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                       ! <-- cancel cx, if inflow
#    if defined M3_FRC_BRY || defined M3CLIMATOLOGY
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
 
            u(Istr,j,k,nnew)=( cff*u(Istr,j,k,nstp)
     &                        +cx*u(Istr+1,j,k,nnew)
     &                    -max(cy,0.)*grad(Istr,j  )
     &                    -min(cy,0.)*grad(Istr,j+1)
     &                                   )/(cff+cx)
#    if defined M3_FRC_BRY  || defined M3CLIMATOLOGY 
            u(Istr,j,k,nnew)=(1.-tau)*u(Istr,j,k,nnew)+tau*
#     ifdef M3_FRC_BRY
     &                                      ubry_west(j,k)
#     else     
     &                                      uclm(Istr,j,k)
#     endif
#    endif
#    ifdef MASKING
            u(Istr,j,k,nnew)=u(Istr,j,k,nnew)*umask(Istr,j)
#    endif
#    ifdef WET_DRY
            u(Istr,j,k,nnew)=u(Istr,j,k,nnew)*umask_wet(Istr,j)
#    endif
          enddo
        enddo
!
#   elif defined OBC_COM_M3SPECIFIED
!                                            Western edge Specified BC
!                                            ======= ==== ========= ==
        do k=1,N
          do j=Jstr,Jend
#    ifdef M3_FRC_BRY
            u(Istr,j,k,nnew)=ubry_west(j,k)         ! specified
#    else
            u(Istr,j,k,nnew)=uclm(Istr,j,k)
#    endif
#    ifdef MASKING
     &                       *umask(Istr,j)
#    endif
#    ifdef WET_DRY
     &                       *umask_wet(Istr,j)
#    endif
          enddo
        enddo
#   else
!                                            Western edge gradient BC
!                                            ======= ==== ======== ==
        do k=1,N
          do j=Jstr,Jend
            u(Istr,j,k,nnew)=u(Istr+1,j,k,nnew)  ! Gradient: default
#    ifdef MASKING
     &                       *umask(Istr,j)
#    endif
#    ifdef WET_DRY
     &                       *umask_wet(Istr,j)
#    endif
          enddo
        enddo
#   endif
!
#   ifdef MRL_WCI000
        do k=1,N                               ! Western edge WEC
          do j=jstr,jend                       ! ======= ==== ===
            u(istr,j,k,nnew)=u(istr,j,k,nnew)
     &                       -ust(istr,j,k)    ! no Lagrangian flux
#    ifdef MASKING
     &                       *umask(istr,j)
#    endif
          enddo
        enddo
#   endif
!
#  else           /* alternative to open */
        do k=1,N                               ! Western edge closed
          do j=jstr,jend                       ! ======= ==== ======
#   ifdef MRL_WCI
            u(istr,j,k,nnew)=-ust(istr,j,k)    ! no Lagrangian flux
#    ifdef MASKING
     &                       *umask(istr,j)
#    endif
#   else
            u(istr,j,k,nnew)=0.                !  (no-flux: default)
#   endif
          enddo
        enddo
#  endif              /* OBC_COM_WEST */
      endif         !<-- WESTERN_EDGE
!
!====================================================================
!                            EASTERN BC
!====================================================================
      if (EASTERN_EDGE) then
#  ifdef OBC_COM_EAST
#   ifdef OBC_COM_M3ORLANSKI
        do k=1,N                             ! Eastern edge radiation
          do j=Jstr,Jend+1                   ! ======= ==== =========
            grad(Iend  ,j)=(u(Iend  ,j,k,nstp)-u(Iend  ,j-1,k,nstp))
#    ifdef MASKING
     &                                                *pmask(Iend,j)
#    endif
            grad(Iend+1,j)=(u(Iend+1,j,k,nstp)-u(Iend+1,j-1,k,nstp))
#    ifdef MASKING
     &                                              *pmask(Iend+1,j)
#    endif
          enddo
          do j=Jstr,Jend
            dft=u(Iend,j,k,nstp)-u(Iend  ,j,k,nnew)
            dfx=u(Iend,j,k,nnew)-u(Iend-1,j,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                      ! <-- cancel cx, if inflow
#    if defined M3_FRC_BRY || defined M3CLIMATOLOGY
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
 
            u(Iend+1,j,k,nnew)=( cff*u(Iend+1,j,k,nstp)
     &                              +cx*u(Iend,j,k,nnew)
     &                      -max(cy,0.)*grad(Iend+1,j  )
     &                      -min(cy,0.)*grad(Iend+1,j+1)
     &                                       )/(cff+cx)
#    if defined M3_FRC_BRY  || defined M3CLIMATOLOGY 
            u(Iend+1,j,k,nnew)=(1.-tau)*u(Iend+1,j,k,nnew)+tau*
#     ifdef M3_FRC_BRY
     &                                          ubry_east(j,k)
#     else     
     &                                        uclm(Iend+1,j,k)
#     endif
#    endif
#    ifdef MASKING
            u(Iend+1,j,k,nnew)=u(Iend+1,j,k,nnew)*umask(Iend+1,j)
#    endif
#    ifdef WET_DRY
            u(Iend+1,j,k,nnew)=u(Iend+1,j,k,nnew)*umask_wet(Iend+1,j)
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
            u(Iend+1,j,k,nnew)=ubry_east(j,k)       ! specified
#    else
            u(Iend+1,j,k,nnew)=uclm(Iend+1,j,k)
#    endif
#    ifdef MASKING
     &                         *umask(Iend+1,j)
#    endif
#    ifdef WET_DRY
     &                         *umask_wet(Iend+1,j)
#    endif
          enddo
        enddo
#   else
!                                            Eastern edge gradient BC
!                                            ======= ==== ======== ==
        do k=1,N
          do j=Jstr,Jend
            u(Iend+1,j,k,nnew)=u(Iend,j,k,nnew)  ! gradient (default)
#    ifdef MASKING
     &                         *umask(Iend+1,j)
#    endif
#    ifdef WET_DRY
     &                         *umask_wet(Iend+1,j)
#    endif
          enddo
        enddo
#   endif
!
#   ifdef MRL_WCI000
        do k=1,N                                ! Eastern edge WEC
          do j=jstr,jend                        ! ======= ==== ===
            u(iend+1,j,k,nnew)=u(iend+1,j,k,nnew)
     &                         -ust(iend+1,j,k) ! no Lagrangian flux
#    ifdef MASKING
     &                         *umask(iend+1,j)
#    endif
          enddo
        enddo
#   endif
!
#  else
        do k=1,N                                ! Eastern edge closed
          do j=jstr,jend                        ! ======= ==== ======
#   ifdef MRL_WCI
            u(iend+1,j,k,nnew)=-ust(iend+1,j,k) ! no Lagrangian flux
#    ifdef MASKING
     &                         *umask(iend+1,j)
#    endif
#   else
            u(iend+1,j,k,nnew)=0.               !  (no-flux: default)
#   endif
          enddo
        enddo
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
#   ifdef OBC_COM_M3ORLANSKI
        do k=1,N                            ! Southern edge radiation
          do i=IstrU-1,Iend                 ! ======== ==== =========
            grad(i,Jstr-1)=u(i+1,Jstr-1,k,nstp)-u(i,Jstr-1,k,nstp)
            grad(i,Jstr  )=u(i+1,Jstr  ,k,nstp)-u(i,Jstr  ,k,nstp)
          enddo
          do i=IstrU,Iend
            dft=u(i,Jstr,k,nstp)-u(i,Jstr  ,k,nnew)
            dfx=u(i,Jstr,k,nnew)-u(i,Jstr+1,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                       ! <-- cancel cx, if inflow
#    if defined M3_FRC_BRY || defined M3CLIMATOLOGY
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
            u(i,Jstr-1,k,nnew)=( cff*u(i,Jstr-1,k,nstp)
     &                              +cx*u(i,Jstr,k,nnew)
     &                      -max(cy,0.)*grad(i-1,Jstr-1)
     &                      -min(cy,0.)*grad(i  ,Jstr-1)
     &                                       )/(cff+cx)
#    if defined M3_FRC_BRY  || defined M3CLIMATOLOGY 
           u(i,Jstr-1,k,nnew)=(1.-tau)*u(i,Jstr-1,k,nnew)
#     ifdef M3_FRC_BRY
     &                               +tau*ubry_south(i,k)
#     else     
     &                              +tau*uclm(i,Jstr-1,k)
#     endif
#    endif
#    ifdef MASKING
            u(i,Jstr-1,k,nnew)=u(i,Jstr-1,k,nnew)*umask(i,Jstr-1)
#    endif
#    ifdef WET_DRY
            u(i,Jstr-1,k,nnew)=u(i,Jstr-1,k,nnew)*umask_wet(i,Jstr-1)
#    endif
          enddo
        enddo
!
#   elif defined OBC_COM_M3SPECIFIED
!                                           Southern edge Specified BC
!                                           ======== ==== ========= ==
        do k=1,N
          do i=IstrU,Iend
#    ifdef M3_FRC_BRY
            u(i,Jstr-1,k,nnew)=ubry_south(i,k)      ! specified
#    else
            u(i,Jstr-1,k,nnew)=uclm(i,Jstr-1,k)
#    endif
#    ifdef MASKING
     &                         *umask(i,Jstr-1)
#    endif
#    ifdef WET_DRY
     &                         *umask_wet(i,Jstr-1)
#    endif
          enddo
        enddo
#   else
!                                           Southern edge gradient BC
!                                           ======== ==== ======== ==
        do k=1,N
          do i=IstrU,Iend
            u(i,Jstr-1,k,nnew)=u(i,Jstr,k,nnew)  ! gradient (default)
#    ifdef MASKING
     &                         *umask(i,Jstr-1)
#    endif
#    ifdef WET_DRY
     &                         *umask_wet(i,Jstr-1)
#    endif
          enddo
        enddo
#   endif
#  else
#   ifdef EW_COM_PERIODIC
#    define I_RANGE IstrU,Iend
#   else
#    define I_RANGE Istr,IendR
#   endif
        do k=1,N                        ! Wall: free-slip (gamma2=+1)
          do i=I_RANGE                  ! =====   no-slip (gamma2=-1)
            u(i,Jstr-1,k,nnew)=gamma2*u(i,Jstr,k,nnew)
#   ifdef MASKING
     &                                *umask(i,Jstr-1)
#   endif
          enddo
        enddo
#   undef I_RANGE
#  endif
      endif              !<-- SOUTHERN_EDGE
!
!====================================================================
!                            NORTHERN BC
!====================================================================
      if (NORTHERN_EDGE) then
#  ifdef OBC_COM_NORTH
#   ifdef OBC_COM_M3ORLANSKI
        do k=1,N                            ! Northern edge radiation
          do i=IstrU-1,Iend                 ! ======== ==== =========
            grad(i,Jend  )=u(i+1,Jend  ,k,nstp)-u(i,Jend  ,k,nstp)
            grad(i,Jend+1)=u(i+1,Jend+1,k,nstp)-u(i,Jend+1,k,nstp)
          enddo
          do i=IstrU,Iend
            dft=u(i,Jend,k,nstp)-u(i,Jend  ,k,nnew)
            dfx=u(i,Jend,k,nnew)-u(i,Jend-1,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                                 ! <-- INFLOW
#    if defined M3_FRC_BRY || defined M3CLIMATOLOGY
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
 
            u(i,Jend+1,k,nnew)=( cff*u(i,Jend+1,k,nstp)
     &                              +cx*u(i,Jend,k,nnew)
     &                      -max(cy,0.)*grad(i-1,Jend+1)
     &                      -min(cy,0.)*grad(i  ,Jend+1)
     &                                       )/(cff+cx)
#    if defined M3_FRC_BRY  || defined M3CLIMATOLOGY 
            u(i,Jend+1,k,nnew)=(1.-tau)*u(i,Jend+1,k,nnew)
#     ifdef M3_FRC_BRY
     &                                +tau*ubry_north(i,k)
#     else     
     &                               +tau*uclm(i,Jend+1,k)
#     endif
#    endif
#    ifdef MASKING
            u(i,Jend+1,k,nnew)=u(i,Jend+1,k,nnew)*umask(i,Jend+1)
#    endif
#    ifdef WET_DRY
            u(i,Jend+1,k,nnew)=u(i,Jend+1,k,nnew)*umask_wet(i,Jend+1)
#    endif
          enddo
        enddo
!
#   elif defined OBC_COM_M3SPECIFIED
!                                           Northern edge Specified BC
!                                           ======== ==== ========= ==
        do k=1,N
          do i=IstrU,Iend
#    ifdef M3_FRC_BRY
            u(i,Jend+1,k,nnew)=ubry_north(i,k)      ! specified
#    else
            u(i,Jend+1,k,nnew)=uclm(i,Jend+1,k)
#    endif
#    ifdef MASKING
     &                         *umask(i,Jend+1)
#    endif
#    ifdef WET_DRY
     &                         *umask_wet(i,Jend+1)
#    endif
          enddo
        enddo
#   else
!                                           Northern edge gradient BC
!                                           ======== ==== ======== ==
        do k=1,N
          do i=IstrU,Iend
            u(i,Jend+1,k,nnew)=u(i,Jend,k,nnew)  ! gradient (default)
#    ifdef MASKING
     &                         *umask(i,Jend+1)
#    endif
#    ifdef WET_DRY
     &                         *umask_wet(i,Jend+1)
#    endif
          enddo
        enddo
#   endif
#  else
#   ifdef EW_COM_PERIODIC
#    define I_RANGE IstrU,Iend
#   else
#    define I_RANGE Istr,IendR
#   endif
        do k=1,N                        ! Wall: free-slip (gamma2=+1)
          do i=I_RANGE                  ! =====   no-slip (gamma2=-1)
            u(i,Jend+1,k,nnew)=gamma2*u(i,Jend,k,nnew)
#   ifdef MASKING
     &                                *umask(i,Jend+1)
#   endif
          enddo
        enddo
#   undef I_RANGE
#  endif
      endif   !<-- NORTHERN_EDGE
# endif        /* !NS_COM_PERIODIC */
 
                           ! Corners between adjacent open boundaries
                           ! ======= ======= ======== ==== ==========
 
# if defined OBC_COM_SOUTH && defined OBC_COM_WEST
      if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=1,N
          u(Istr,Jstr-1,k,nnew)=0.5*( u(Istr+1,Jstr-1,k,nnew)
     &                               +u(Istr  ,Jstr  ,k,nnew))
#  ifdef MASKING
     &                          *umask(Istr,Jstr-1)
#  endif
        enddo
      endif
# endif
# if defined OBC_COM_SOUTH && defined OBC_COM_EAST
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=1,N
          u(Iend+1,Jstr-1,k,nnew)=0.5*( u(Iend,Jstr-1,k,nnew)
     &                                 +u(Iend+1,Jstr,k,nnew))
#  ifdef MASKING
     &                            *umask(Iend+1,Jstr-1)
#  endif
        enddo
      endif
# endif
# if defined OBC_COM_NORTH && defined OBC_COM_WEST
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        do k=1,N
          u(Istr,Jend+1,k,nnew)=0.5*( u(Istr+1,Jend+1,k,nnew)
     &                               +u(Istr  ,Jend  ,k,nnew))
#  ifdef MASKING
     &                          *umask(Istr,Jend+1)
#  endif
        enddo
      endif
# endif
# if defined OBC_COM_NORTH && defined OBC_COM_EAST
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        do k=1,N
          u(Iend+1,Jend+1,k,nnew)=0.5*( u(Iend,Jend+1,k,nnew)
     &                                 +u(Iend+1,Jend,k,nnew))
#  ifdef MASKING
     &                            *umask(Iend+1,Jend+1)
#  endif
        enddo
      endif
# endif
      return
      end
#else
      subroutine u3dbc_empty
      end
#endif /* SOLVE3D */
#ifndef CHILD
# define CHILD
# ifdef AGRIF
#  include "u3dbc.F"
# endif
# undef CHILD
#endif  /* !CHILD */
