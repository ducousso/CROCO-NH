! $Id: hmix_coef.F 1618 2014-12-18 14:39:51Z rblod $
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
#if defined  VIS_COEF_3D || (!defined SOLVE3D && defined UV_VIS_SMAGO)

      subroutine hvisc_coef (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call hvisc_coef_tile (Istr,Iend,Jstr,Jend,A2d(1,1,trd))
      return
      end
!
      subroutine hvisc_coef_tile (Istr,Iend,Jstr,Jend, grd_scale)
!
!==================================================================
!
! Compute horizontal viscosity
!
! Patrick Marchesiello and Pierrick Penven, IRD 2007
!
!==================================================================
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k,itrc
      real grd_scale(PRIVATE_2D_SCRATCH_ARRAY)
      real cff, surf, defrate, horcon, cff_peclet
      parameter (horcon=0.1)
# include "param.h"
# include "grid.h"
# include "mixing.h"
# include "ocean3d.h"
# include "ocean2d.h"
# include "scalars.h"
#ifdef SWASH
      real dZdt,dZdt0,Bbr,Dbr
#endif
!
# include "compute_auxiliary_bounds.h"
!
# if defined UV_VIS2 && defined UV_VIS_SMAGO
!
!---------------------------------------------------------------
! Smagorinsky viscosity at RHO points
!
!  The POM formulation is used:
!    A = C*DX*DY*DEFRATE
!        with DEFRATE=sqrt[du/dx^2 +dvdy^2 + 0.5(dvdx+dudy)^2]
!        the deformation rate
!
!---------------------------------------------------------------
!
! Smagorinsky viscosity at RHO points
!
      do k=1,N
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            defrate=sqrt( ((u(i+1,j,k,nrhs)-u(i,j,k,nrhs))*pm(i,j))**2
     &                   +((v(i,j+1,k,nrhs)-v(i,j,k,nrhs))*pn(i,j))**2
     &               +0.5*(0.25*pn(i,j)*(
     &                     u(i,j+1,k,nrhs)+u(i+1,j+1,k,nrhs)
     &                    -u(i,j-1,k,nrhs)-u(i+1,j-1,k,nrhs))
     &                   + 0.25*pm(i,j)*(
     &                     v(i+1,j,k,nrhs)+v(i+1,j+1,k,nrhs)
     &                    -v(i-1,j,k,nrhs)-v(i-1,j+1,k,nrhs)) )**2)
            surf=om_r(i,j)*on_r(i,j)
            visc3d_r(i,j,k)=horcon*surf*defrate
          enddo
        enddo
      enddo
!
! Add Wave breaking dissipation in wave-resolved cases
!
#  ifdef SWASH
      do j=JstrV-1,Jend
        do i=IstrU-1,Iend
          dZdt=abs(zeta(i,j,knew)-zeta(i,j,kstp))/dtfast
#   ifdef NBQ
          cff=zeta(i,j,knew)
          Dbr=10.*om_r(i,j)*on_r(i,j)
#   else
          cff=z_w(i,j,N)-z_w(i,j,0)
          Dbr=10.*om_r(i,j)*on_r(i,j)
#   endif
          dZdt0=0.01*sqrt(g*cff)
          Bbr=max(0.,min(1.,dZdt/dZdt0-1.))
          do k=1,N
            visc3d_r(i,j,k)=max(0.1/horcon*visc3d_r(i,j,k),
     &                                    Dbr*Bbr*cff*dZdt)
!     &                               *1000*max(pm(i,j)*pm(i,j)*
!     &                              (z_r(i+1,j,k)-z_r(i-1,j,k)-
!     &                               z_w(i+1,j,0)+z_w(i-1,j,0))**2,
!     &                                         pn(i,j)*pn(i,j)*
!     &                               (z_r(i,j+1,k)-z_r(i,j-1,k)-
!     &                               z_w(i,j+1,0)+z_w(i,j-1,0))**2))
          enddo
        enddo
      enddo
#  endif
!
!------------------------------------------------------------
! Boundary conditions
!------------------------------------------------------------
!
      if (WESTERN_EDGE) then
        do k=1,N
          do j=JstrV-1,Jend
           visc3d_r(Istr-1,j,k)=visc3d_r(Istr,j,k)
          enddo
        enddo
      endif
      if (EASTERN_EDGE) then
        do k=1,N
          do j=JstrV-1,Jend
            visc3d_r(Iend+1,j,k)=visc3d_r(Iend,j,k)
          enddo
        enddo
      endif
      if (SOUTHERN_EDGE) then
        do k=1,N
          do i=IstrU-1,Iend
            visc3d_r(i,Jstr-1,k)=visc3d_r(i,Jstr,k)
          enddo
        enddo
      endif
      if (NORTHERN_EDGE) then
        do k=1,N
          do i=IstrU-1,Iend
            visc3d_r(i,Jend+1,k)=visc3d_r(i,Jend,k)
          enddo
        enddo
      endif
!
! Corners
!
      if (SOUTHERN_EDGE .and. WESTERN_EDGE) then
        do k=1,N
          visc3d_r(Istr-1,Jstr-1,k)=0.5*
     &                          ( visc3d_r(Istr,Jstr-1,k)
     &                           +visc3d_r(Istr-1,Jstr,k))
        enddo
      endif
      if (SOUTHERN_EDGE .and. EASTERN_EDGE) then
        do k=1,N
          visc3d_r(Iend+1,Jstr-1,k)=0.5*
     &                          (visc3d_r(Iend,Jstr-1,k)
     &                          +visc3d_r(Iend+1,Jstr,k))
        enddo
      endif
      if (NORTHERN_EDGE .and. WESTERN_EDGE) then
        do k=1,N
          visc3d_r(Istr-1,Jend+1,k)=0.5*
     &                          ( visc3d_r(Istr,Jend+1,k)
     &                           +visc3d_r(Istr-1,Jend,k))
        enddo
      endif
      if (NORTHERN_EDGE .and. EASTERN_EDGE) then
        do k=1,N
          visc3d_r(Iend+1,Jend+1,k)=0.5*
     &                          ( visc3d_r(Iend,Jend+1,k)
     &                           +visc3d_r(Iend+1,Jend,k))
        enddo
      endif
!
!  Viscosity at PSI points
!
      do k=1,N
        do j=Jstr,JendR
          do i=Istr,IendR
            visc3d_p(i,j,k)=0.25*
     &                  ( visc3d_r(i,j  ,k)+visc3d_r(i-1,j  ,k)
     &                   +visc3d_r(i,j-1,k)+visc3d_r(i-1,j-1,k))
          enddo
        enddo
      enddo

#  if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_p3d_tile (Istr,Iend,Jstr,Jend,visc3d_p)
#  endif

# endif /* UV_VIS_SMAGO  */
#else
      subroutine hvisc_coef_empty
#endif
      return
      end

!
!===================================================================
!                          DIFFUSIVITY
!===================================================================
!
#ifdef DIF_COEF_3D

      subroutine hdiff_coef (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call hdiff_coef_tile (Istr,Iend,Jstr,Jend,A2d(1,1,trd))
      return
      end
!
      subroutine hdiff_coef_tile (Istr,Iend,Jstr,Jend,grd_scale)
!
!==================================================================
!
! Compute horizontal diffusivity coefficients
!
! Patrick Marchesiello and Pierrick Penven, IRD 2007
!
!==================================================================
!

      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k,itrc
      real grd_scale(PRIVATE_2D_SCRATCH_ARRAY)
      real cff, surf, defrate, horcon
# ifdef TS_HADV_C6
      parameter (horcon=1./20.)
# else
      parameter (horcon=1./12.)
# endif
# include "param.h"
# include "grid.h"
# include "mixing.h"
# include "ocean3d.h"
# include "scalars.h"
!
# include "compute_auxiliary_bounds.h"
!
# ifdef TS_DIF_SMAGO
!---------------------------------------------------------------
!  Smagorinsky diffusion coefficient
!
!  For Laplacian diffusion, the POM formulation is used:
!    A = CA*DX*DY*DEFRATE
!        with DEFRATE=sqrt[du/dx^2 +dvdy^2 + 0.5(dvdx+dudy)^2]
!        DEFRATE is the deformation rate
!
!---------------------------------------------------------------
!
      do k=1,N
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            defrate=sqrt( ((u(i+1,j,k,nrhs)-u(i,j,k,nrhs))*pm(i,j))**2
     &                   +((v(i,j+1,k,nrhs)-v(i,j,k,nrhs))*pn(i,j))**2
     &               +0.5*(0.25*pn(i,j)*(
     &                     u(i,j+1,k,nrhs)+u(i+1,j+1,k,nrhs)
     &                    -u(i,j-1,k,nrhs)-u(i+1,j-1,k,nrhs))
     &                   + 0.25*pm(i,j)*(
     &                     v(i+1,j,k,nrhs)+v(i+1,j+1,k,nrhs)
     &                    -v(i-1,j,k,nrhs)-v(i-1,j+1,k,nrhs)) )**2)
            surf=om_r(i,j)*on_r(i,j)
            diff3d_r(i,j,k)=horcon*surf*defrate
          enddo
        enddo
      enddo
!
! Boundary conditions
!
      if (WESTERN_EDGE) then
        do k=1,N
          do j=JstrV-1,Jend
           diff3d_r(Istr-1,j,k)=diff3d_r(Istr,j,k)
          enddo
        enddo
      endif
      if (EASTERN_EDGE) then
        do k=1,N
          do j=JstrV-1,Jend
            diff3d_r(Iend+1,j,k)=diff3d_r(Iend,j,k)
          enddo
        enddo
      endif
      if (SOUTHERN_EDGE) then
        do k=1,N
          do i=IstrU-1,Iend
            diff3d_r(i,Jstr-1,k)=diff3d_r(i,Jstr,k)
          enddo
        enddo
      endif
      if (NORTHERN_EDGE) then
        do k=1,N
          do i=IstrU-1,Iend
            diff3d_r(i,Jend+1,k)=diff3d_r(i,Jend,k)
          enddo
        enddo
      endif

# endif /* TS_DIF_SMAGO */

# if defined TS_HADV_RSUP3 || defined TS_HADV_RSUP5

!--------------------------------------------------------------------
! Tracer mixing coefficient for use in rotated split 3rd order upstream 
! biased advection scheme: B = 1/12 * abs(u) * grd_scale**3
!
! Marchesiello et al. (Ocean Modelling, 2009)
!--------------------------------------------------------------------
!
      do k=1,N
        do j=Jstr-1,Jend+1
          do i=IstrU-1,Iend+1
            diff3d_u(i,j,k)=horcon*abs(u(i,j,k,nrhs))*om_u(i,j)**3
          enddo
        enddo
        do j=JstrV-1,Jend+1
          do i=Istr-1,Iend+1
            diff3d_v(i,j,k)=horcon*abs(v(i,j,k,nrhs))*on_v(i,j)**3
          enddo
        enddo
      enddo
# endif /* TS_HADV_RSUP3 */

# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
#  ifdef TS_DIF_SMAGO
      call exchange_r3d_tile (Istr,Iend,Jstr,Jend,diff3d_r)
#  endif
#  if defined TS_HADV_RSUP3 || defined TS_HADV_RSUP5
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,diff3d_u)
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend,diff3d_v)
#  endif
# endif

#else
      subroutine hdiff_coef_empty
#endif
      return
      end

