! $Id: gls_bc.h 1160 2013-06-11 09:48:32Z gcambon $
!
!======================================================================
! CROCO is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! CROCO specific routines (nesting) are under CeCILL-C license.
! 
! CROCO website : http://www.croco-ocean.org/
!
!======================================================================
!#include "cppdefs.h"
!#ifdef SOLVE3D
! 
!      subroutine glsbc_tile (istr,iend,jstr,jend, grad)
!
! Set lateral boundary conditions for GLS model field (tke & gls)
!
!      implicit none
!      integer istr,iend,jstr,jend, i,j,k
!      real grad(PRIVATE_2D_SCRATCH_ARRAY), cff,
!     &       cx,cy, dft,dfx,dfy, tau,tau_in,tau_out
!# include "param.h"
!# include "grid.h"
!# include "ocean3d.h"
!# include "scalars.h"
!# include "boundary.h"
!
!# include "compute_auxiliary_bounds.h"
!
# if defined OBC_TORLANSKI && (defined T_FRC_BRY || defined TCLIMATOLOGY)
      tau_in=dt*tauT_in
      tau_out=dt*tauT_out
# endif

# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
#  if defined OBC_WEST && defined OBC_TORLANSKI
        do k=1,N
          do j=jstr,jend+1
            grad(istr-1,j)=( tmp(istr-1,j  ,k,nstp)
     &                      -tmp(istr-1,j-1,k,nstp))
#   ifdef MASKING
     &                              *vmask(istr-1,j)
#   endif
            grad(istr  ,j)=( tmp(istr  ,j  ,k,nstp)
     &                      -tmp(istr  ,j-1,k,nstp))
#   ifdef MASKING
     &                                *vmask(istr,j)
#   endif
          enddo
          do j=jstr,jend
            cx=-dt*u(istr,j,k,nrhs)*pm(istr-1,j)
            cy=0.5*dt*(v(istr-1,j,k,nrhs)+v(istr-1,j+1,k,nrhs))
     &                                            *pn(istr-1,j)
            if (cx.gt.0.) then
              tau=0.
            else
              tau=-cx
              cx=0.
            endif

            tmp(istr-1,j,k,nnew)=(1.-cx)*( tmp(istr-1,j,k,nstp)
     &                                   -max(cy,0.)*grad(istr-1,j  )
     &                                   -min(cy,0.)*grad(istr-1,j+1)
     &                                                              )
     &                                  +cx*(  tmp(istr,j,k,nstp)
     &                                     -max(cy,0.)*grad(istr,j  )
     &                                     -min(cy,0.)*grad(istr,j+1)
     &                                                              )
#   ifdef MASKING
            tmp(istr-1,j,k,nnew)=tmp(istr-1,j,k,nnew)
     &                               *rmask(istr-1,j)
#   endif
          enddo
        enddo
#  else
        do k=1,N
          do j=jstr,jend
            tmp(istr-1,j,k,nnew)=tmp(istr,j,k,nnew)
#   ifdef MASKING
     &                             *rmask(istr-1,j)
#   endif
          enddo
        enddo
#  endif
      endif     ! <-- WESTERN_EDGE

      if (EASTERN_EDGE) then
#  if defined OBC_EAST && defined OBC_TORLANSKI
!
!                                        !  Eastern edge radiation BC
        do k=1,N                         !  ======= ==== ========= ==
          do j=jstr,jend+1
           grad(iend  ,j)=( tmp(iend  ,j  ,k,nstp)
     &                     -tmp(iend  ,j-1,k,nstp))
#   ifdef MASKING
     &                               *vmask(iend,j)
#   endif
           grad(iend+1,j)=( tmp(iend+1,j  ,k,nstp)
     &                     -tmp(iend+1,j-1,k,nstp))
#   ifdef MASKING
     &                             *vmask(iend+1,j)
#   endif
          enddo
          do j=jstr,jend
            cx=dt*u(iend+1,j,k,nrhs)*pm(iend+1,j)
            cy=0.5*dt*(v(iend+1,j,k,nrhs)+v(iend+1,j+1,k,nrhs))
     &                                            *pn(iend+1,j)
            if (cx.gt.0.) then
              tau=0.
            else
              tau=-cx
              cx=0.
            endif
            tmp(iend+1,j,k,nnew)=(1.-cx)*( tmp(iend+1,j,k,nstp)
     &                                   -max(cy,0.)*grad(iend+1,j  )
     &                                   -min(cy,0.)*grad(iend+1,j+1)
     &                                                              )
     &                                  +cx*(  tmp(iend,j,k,nnew)
     &                                     -max(cy,0.)*grad(iend,j  )
     &                                     -min(cy,0.)*grad(iend,j+1)
     &                                                              )
#   ifdef MASKING
            tmp(iend+1,j,k,nnew)=tmp(iend+1,j,k,nnew)
     &                               *rmask(iend+1,j)
#   endif
          enddo
        enddo
#  else
        do k=1,N
          do j=jstr,jend
            tmp(iend+1,j,k,nnew)=tmp(iend,j,k,nnew)
#   ifdef MASKING
     &                             *rmask(iend+1,j)
#   endif
          enddo
        enddo
#  endif
      endif    ! <-- EASTERN_EDGE
# endif        /* !EW_PERIODIC */


 
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
#  if defined OBC_SOUTH && defined OBC_TORLANSKI
        do k=1,N
          do i=istr,iend+1
            grad(i,jstr  )=( tmp(i  ,jstr  ,k,nstp)
     &                      -tmp(i-1,jstr  ,k,nstp))
#   ifdef MASKING
     &                                *umask(i,jstr)
#   endif
            grad(i,jstr-1)=( tmp(i  ,jstr-1,k,nstp)
     &                      -tmp(i-1,jstr-1,k,nstp))
#   ifdef MASKING
     &                              *umask(i,jstr-1)
#   endif
          enddo
          do i=istr,iend
            cx=-dt*v(i,jstr,k,nrhs)*pn(i,jstr-1)
            cy=0.5*dt*(u(i,jstr-1,k,nrhs)+u(i+1,jstr-1,k,nrhs))
     &                                            *pm(i,jstr-1)
            if (cx.gt.0.) then
              tau=0.
            else
              tau=-cx
              cx=0.
            endif
            tmp(i,jstr-1,k,nnew)=(1.-cx)*( tmp(i,jstr-1,k,nstp)
     &                                   -max(cy,0.)*grad(i  ,jstr-1)
     &                                   -min(cy,0.)*grad(i+1,jstr-1)
     &                                                              )
     &                                  +cx*(  tmp(i,jstr,k,nstp)
     &                                     -max(cy,0.)*grad(i  ,jstr)
     &                                     -min(cy,0.)*grad(i+1,jstr)
     &                                                              )
#   ifdef MASKING
            tmp(i,jstr-1,k,nnew)=tmp(i,jstr-1,k,nnew)
     &                               *rmask(i,jstr-1)
#   endif
          enddo
        enddo
#  else
        do k=1,N
          do i=istr,iend
            tmp(i,jstr-1,k,nnew)=tmp(i,jstr,k,nnew)
#   ifdef MASKING
     &                             *rmask(i,jstr-1)
#   endif
          enddo
        enddo
#  endif
      endif    ! <-- SOUTHERN_EDGE


      if (NORTHERN_EDGE) then
#  if defined OBC_NORTH && defined OBC_TORLANSKI
        do k=1,N
          do i=istr,iend+1
            grad(i,jend  )=( tmp(i  ,jend  ,k,nstp)
     &                      -tmp(i-1,jend  ,k,nstp))
#   ifdef MASKING
     &                                *umask(i,jend)
#   endif
            grad(i,jend+1)=( tmp(i  ,jend+1,k,nstp)
     &                      -tmp(i-1,jend+1,k,nstp))
#   ifdef MASKING
     &                              *umask(i,jend+1)
#   endif
          enddo
          do i=istr,iend
            cx=dt*v(i,jend+1,k,nrhs)*pn(i,jend+1)
            cy=0.5*dt*(u(i,jend+1,k,nrhs)+u(i+1,jend+1,k,nrhs))
     &                                            *pm(i,jend+1)
            if (cx.gt.0.) then
              tau=0.
            else
              tau=-cx
              cx=0.
            endif
            tmp(i,jend+1,k,nnew)=(1.-cx)*( tmp(i,jend+1,k,nstp)
     &                                   -max(cy,0.)*grad(i  ,jend+1)
     &                                   -min(cy,0.)*grad(i+1,jend+1)
     &                                                              )
     &                                  +cx*(  tmp(i,jend,k,nnew)
     &                                     -max(cy,0.)*grad(i  ,jend)
     &                                     -min(cy,0.)*grad(i+1,jend)
     &                                                              )
#   ifdef MASKING
            tmp(i,jend+1,k,nnew)=tmp(i,jend+1,k,nnew)
     &                               *rmask(i,jend+1)
#   endif
          enddo
        enddo
#  else
        do k=1,N
          do i=istr,iend
            tmp(i,jend+1,k,nnew)=tmp(i,jend,k,nnew)
#   ifdef MASKING
     &                             *rmask(i,jend+1)
#   endif
          enddo
        enddo
#  endif
      endif    ! <-- NORTHERN_EDGE
# endif /* ! NS_PERIODIC */
!
! Corner points between adjacent boundaries. Note that because
! boundary conditions for tracers are of gradient nature -- either
! Neumann, or radiation, the corner points have to be set in all
! cases, even if the adjacent boundaries are closed. This differs
! from setting boundaries for velocities, where there is anisotropy
! associated with normal and tangential components. In the case when 
! one/or both points near the corner is/are masked, the use of masked
! points is avoided. 

# ifndef EW_PERIODIC
      if (SOUTHERN_EDGE .and. WESTERN_EDGE) then
#  ifdef MASKING
        cff=rmask(istr,jstr-1)+rmask(istr-1,jstr)
        if (cff.gt.0.) then
          cff=1./cff
          do k=1,N
            tmp(istr-1,jstr-1,k,nnew)=cff*(
     &              rmask(istr,jstr-1)*tmp(istr,jstr-1,k,nnew)
     &             +rmask(istr-1,jstr)*tmp(istr-1,jstr,k,nnew))
          enddo
        else
          do k=1,N
            tmp(istr-1,jstr-1,k,nnew)=0.
          enddo
        endif
#  else
        do k=1,N
          tmp(istr-1,jstr-1,k,nnew)=0.5*( tmp(istr,jstr-1,k,nnew)
     &                                   +tmp(istr-1,jstr,k,nnew))
        enddo
#  endif
      endif

      if (SOUTHERN_EDGE .and. EASTERN_EDGE) then
#  ifdef MASKING
        cff=rmask(iend,jstr-1)+rmask(iend+1,jstr)
        if (cff.gt.0.) then
          cff=1./cff
          do k=1,N
            tmp(iend+1,jstr-1,k,nnew)=cff*(
     &              rmask(iend,jstr-1)*tmp(iend,jstr-1,k,nnew)
     &             +rmask(iend+1,jstr)*tmp(iend+1,jstr,k,nnew))
          enddo
        else
          do k=1,N
            tmp(iend+1,jstr-1,k,nnew)=0.
          enddo
        endif
#  else
        do k=1,N
          tmp(iend+1,jstr-1,k,nnew)=0.5*(tmp(iend,jstr-1,k,nnew)
     &                                  +tmp(iend+1,jstr,k,nnew))
        enddo
#  endif
      endif
# endif

# ifndef NS_PERIODIC
      if (NORTHERN_EDGE .and. WESTERN_EDGE) then
#  ifdef MASKING
        cff=rmask(istr,jend+1)+rmask(istr-1,jend)
        if (cff.gt.0.) then
          cff=1./cff
          do k=1,N
            tmp(istr-1,jend+1,k,nnew)=cff*(
     &              rmask(istr,jend+1)*tmp(istr,jend+1,k,nnew)
     &             +rmask(istr-1,jend)*tmp(istr-1,jend,k,nnew))
          enddo
        else
          do k=1,N
            tmp(istr-1,jend+1,k,nnew)=0.
          enddo
        endif
#  else
        do k=1,N
          tmp(istr-1,jend+1,k,nnew)=0.5*( tmp(istr,jend+1,k,nnew)
     &                                   +tmp(istr-1,jend,k,nnew))
        enddo
#  endif
      endif

      if (NORTHERN_EDGE .and. EASTERN_EDGE) then
#  ifdef MASKING
        cff=rmask(iend,jend+1)+rmask(iend+1,jend)
        if (cff.gt.0.) then
          cff=1./cff
          do k=1,N
            tmp(iend+1,jend+1,k,nnew)=cff*(
     &              rmask(iend,jend+1)*tmp(iend,jend+1,k,nnew)
     &             +rmask(iend+1,jend)*tmp(iend+1,jend,k,nnew))
          enddo
        else
          do k=1,N
            tmp(iend+1,jend+1,k,nnew)=0.
          enddo
        endif
#  else
        do k=1,N
          tmp(iend+1,jend+1,k,nnew)=0.5*( tmp(iend,jend+1,k,nnew)
     &                                   +tmp(iend+1,jend,k,nnew))
        enddo
#  endif
      endif
# endif
!
! update Kv first vertically and then horizontally
!
# ifdef KV_UPDATE
      do j=jstr,jend    ! Update surface and bottom Akt
        do i=istr,iend
          Akt(i,j,0,itemp)=2.*Akt(i,j,1,itemp)-Akt(i,j,2,itemp)
          Akt(i,j,N,itemp)=2.*Akt(i,j,N-1,itemp)-Akt(i,j,N-2,itemp)
!csh extrapolate at surface w points
          Akv(i,j,0)=2.*Akv(i,j,1)-Akv(i,j,2)
          Akv(i,j,N)=2.*Akv(i,j,N-1)-Akv(i,j,N-2)
# ifdef SALINITY
          Akt(i,j,0,isalt)=2.*Akt(i,j,1,isalt)-Akt(i,j,2,isalt)
          Akt(i,j,N,isalt)=2.*Akt(i,j,N-1,isalt)-Akt(i,j,N-2,isalt)
# endif
        enddo
      enddo

      do j=jstr,jend    ! Apply the background small viscosity
        do i=istr,iend
          do k=0,N
            Akt(i,j,k,itemp)=max(Akt(i,j,k,itemp),Akt_bak(itemp))
#  ifdef SALINITY
            Akt(i,j,k,isalt)=max(Akt(i,j,k,isalt),Akt_bak(isalt))
#  endif
          enddo
        enddo
      enddo

# define k0 0
# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        do j=jstr,jend
          do k=k0,N
            Akv(istr-1,j,k)=Akv(istr,j,k)
            Akk(istr-1,j,k)=Akk(istr,j,k)
            Akp(istr-1,j,k)=Akp(istr,j,k)
            Akt(istr-1,j,k,itemp)=Akt(istr,j,k,itemp)
#  ifdef SALINITY
            Akt(istr-1,j,k,isalt)=Akt(istr,j,k,isalt)
#  endif
          enddo
        enddo
      endif
      if (EASTERN_EDGE) then
        do j=jstr,jend
          do k=k0,N
            Akv(iend+1,j,k)=Akv(iend,j,k)
            Akk(iend+1,j,k)=Akk(iend,j,k)
            Akp(iend+1,j,k)=Akp(iend,j,k)
            Akt(iend+1,j,k,itemp)=Akt(iend,j,k,itemp)
#  ifdef SALINITY
            Akt(iend+1,j,k,isalt)=Akt(iend,j,k,isalt)
#  endif
          enddo
        enddo
      endif
# endif
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do i=istr,iend
          do k=k0,N
            Akv(i,jstr-1,k)=Akv(i,jstr,k)
            Akk(i,jstr-1,k)=Akk(i,jstr,k)
            Akp(i,jstr-1,k)=Akp(i,jstr,k)
            Akt(i,jstr-1,k,itemp)=Akt(i,jstr,k,itemp)
#  ifdef SALINITY
            Akt(i,jstr-1,k,isalt)=Akt(i,jstr,k,isalt)
#  endif
          enddo
        enddo
      endif
      if (NORTHERN_EDGE) then
        do i=istr,iend
          do k=k0,N
            Akv(i,jend+1,k)=Akv(i,jend,k)
            Akk(i,jend+1,k)=Akk(i,jend,k)
            Akp(i,jend+1,k)=Akp(i,jend,k)
            Akt(i,jend+1,k,itemp)=Akt(i,jend,k,itemp)
#  ifdef SALINITY
            Akt(i,jend+1,k,isalt)=Akt(i,jend,k,isalt)
#  endif
          enddo
        enddo
      endif
#  ifndef EW_PERIODIC
      if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=k0,N
          Akv(istr-1,jstr-1,k)=Akv(istr,jstr,k)
          Akk(istr-1,jstr-1,k)=Akk(istr,jstr,k)
          Akp(istr-1,jstr-1,k)=Akp(istr,jstr,k)
          Akt(istr-1,jstr-1,k,itemp)=Akt(istr,jstr,k,itemp)
#  ifdef SALINITY
          Akt(istr-1,jstr-1,k,isalt)=Akt(istr,jstr,k,isalt)
#  endif
        enddo
      endif
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        do k=k0,N
          Akv(istr-1,jend+1,k)=Akv(istr,jend,k)
          Akk(istr-1,jend+1,k)=Akk(istr,jend,k)
          Akp(istr-1,jend+1,k)=Akp(istr,jend,k)
          Akt(istr-1,jend+1,k,itemp)=Akt(istr,jend,k,itemp)
#  ifdef SALINITY
          Akt(istr-1,jend+1,k,isalt)=Akt(istr,jend,k,isalt)
#  endif
        enddo
      endif
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=k0,N
          Akv(iend+1,jstr-1,k)=Akv(iend,jstr,k)
          Akk(iend+1,jstr-1,k)=Akk(iend,jstr,k)
          Akp(iend+1,jstr-1,k)=Akp(iend,jstr,k)
          Akt(iend+1,jstr-1,k,itemp)=Akt(iend,jstr,k,itemp)
#  ifdef SALINITY
          Akt(iend+1,jstr-1,k,isalt)=Akt(iend,jstr,k,isalt)
#  endif
        enddo
      endif
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        do k=k0,N
          Akv(iend+1,jend+1,k)=Akv(iend,jend,k)
          Akk(iend+1,jend+1,k)=Akk(iend,jend,k)
          Akp(iend+1,jend+1,k)=Akp(iend,jend,k)
          Akt(iend+1,jend+1,k,itemp)=Akt(iend,jend,k,itemp)
#  ifdef SALINITY
          Akt(iend+1,jend+1,k,isalt)=Akt(iend,jend,k,isalt)
#  endif
        enddo
      endif
#  endif
# endif
# endif  /* KV_UPDATE */

!      return
!      end
!#else
!      subroutine glsbc_empty
!      end
!#endif /* SOLVE3D */
