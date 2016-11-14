! $Id: lmd_vmix.F 1556 2014-06-19 07:42:15Z marchesiello $
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
#ifdef LMD_MIXING
!
      subroutine lmd_vmix (tile)
!
!--------------------------------------------------------------------
!  This subroutine computes vertical mixing coefficients for momentum
!  and tracers at the ocean interior using the Large, McWilliams and
!  Doney (1994) mixing scheme.
!
!  On Output:
!     Kv       vertical viscosity coefficient [m^2/s].
!     Kt       vertical diffusion coefficient for potential
!                 temperature [m^2/s].
!     Ks       vertical diffusion coefficient for salinity [m^2/s].
!
!  Reference:
!
!    Large, W.G., J.C. McWilliams, and S.C. Doney, 1994: A Review
!    and model with a nonlocal boundary layer parameterization,
!    Reviews of Geophysics, 32,363-403.
!--------------------------------------------------------------------
!
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call  lmd_vmix_tile (Istr,Iend,Jstr,Jend,
     &                     A3d(1, 1,trd), A3d(1, 2,trd), A3d(1, 3,trd),
     &                                                   A3d(1, 4,trd))
# ifdef LMD_SKPP
#  ifdef LMD_SKPP2005
      call lmd_skpp_tile  (Istr,Iend,Jstr,Jend, 
     &                     A3d(1, 1,trd), A3d(1, 2,trd), A3d(1, 3,trd),
     &                     A2d(1, 1,trd), A2d(1, 2,trd), A2d(1, 3,trd),
     &                     A2d(1, 4,trd), A2d(1, 5,trd), A2d(1, 6,trd),
     &                     A2d(1, 7,trd), A2d(1, 8,trd), A2d(1, 9,trd),
     &                     A2d(1,10,trd), A2d(1,11,trd), A2d(1,12,trd),
     &                     A2d(1,13,trd), A2d(1,14,trd), A2d(1,15,trd),
     &                                    A3d(1, 4,trd),    B2d(1,trd))
#  else
      call lmd_skpp_tile (Istr,Iend,Jstr,Jend,
     &                    A3d(1, 1,trd), A3d(1, 2,trd), A3d(1, 3,trd),  
     &                    A2d(1, 1,trd), A2d(1, 2,trd), A2d(1, 3,trd), 
     &                    A2d(1, 4,trd), A2d(1, 5,trd), A2d(1, 6,trd), 
     &                    A2d(1, 7,trd), A2d(1, 8,trd), A2d(1, 9,trd),
     &                    A2d(1,10,trd), A2d(1,11,trd), A2d(1,12,trd),
     &                    A2d(1,13,trd), A3d(1, 4,trd),    B2d(1,trd)) 
#  endif
# endif
# if defined LMD_BKPP 
C$OMP BARRIER
#  ifdef LMD_BKPP2005
      call lmd_bkpp_tile (Istr,Iend,Jstr,Jend,
     &                    A3d(1, 1,trd), A3d(1, 2,trd), A3d(1, 3,trd),
     &                    A2d(1, 5,trd), A2d(1, 6,trd), A2d(1, 7,trd),
     &                    A2d(1, 8,trd), A2d(1, 9,trd), A2d(1,10,trd),
     &                    A2d(1,11,trd), A2d(1,12,trd), A2d(1,13,trd),
     &                    A2d(1,14,trd), A2d(1,15,trd), A2d(1,16,trd))
#  else
      call lmd_bkpp_tile (Istr,Iend,Jstr,Jend,
     &                    A3d(1, 1,trd), A3d(1, 2,trd), A3d(1, 3,trd),
     &                    A2d(1, 5,trd), A2d(1, 6,trd), A2d(1, 7,trd),
     &                    A2d(1, 8,trd), A2d(1, 9,trd), A2d(1,10,trd),
     &                    A2d(1,12,trd), A2d(1,13,trd), A2d(1,14,trd), 
     &                    A2d(1,15,trd), A2d(1,16,trd), A3d(1, 4,trd))
#  endif
# endif
      call lmd_finalize_tile (Istr,Iend,Jstr,Jend, 
     &                    A3d(1, 1,trd), A3d(1, 2,trd), A3d(1, 3,trd))

      return
      end
!
!--------------------------------------------------------------------
      subroutine lmd_vmix_tile (Istr,Iend,Jstr,Jend, Kv,Kt,Ks,Rig)
!--------------------------------------------------------------------
!
      implicit none
# include "param.h"
      integer Istr,Iend,Jstr,Jend, i,j,k
      real Rig(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &      Kv(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &      Kt(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &      Ks(PRIVATE_2D_SCRATCH_ARRAY,0:N)
# include "grid.h"
# include "ocean3d.h"
# include "mixing.h"
# include "scalars.h"
# include "forces.h"
      real eps, lmd_iwm,lmd_iws, nu_sx,nu_sxc, cff
      parameter (eps=1.E-14)

      real lmd_Ri0,  lmd_nuwm,  lmd_nuws, lmd_nu0m,  lmd_nu0s,
     &     lmd_nu0c, lmd_nu,   lmd_Rrho0, lmd_nuf,  
     &     lmd_fdd,  lmd_tdd1,  lmd_tdd2, lmd_tdd3,  lmd_sdd1, 
     &     lmd_sdd2, lmd_sdd3
      parameter (
     &     lmd_Ri0=0.7,      ! Critical gradient Richardson number
                             ! below which turbulent mixing occurs.
                             !
     &     lmd_nuwm=1.0e-4,  ! Interior viscosity and diffusivity 
     &     lmd_nuws=0.1e-4,  ! due to wave breaking, [m^2/s] 
                             !
     &     lmd_nu0m=50.e-4,  ! Maximum interior viscosity and
     &     lmd_nu0s=50.e-4,  ! diffusivity due to shear
                             ! instability  [m^2/s];
                             !
     &     lmd_nu0c=0.1,     ! Maximum interior convective 
                             ! viscosity and  diffusivity due
                             ! to shear instability, [m^2/s];
                             !
     &     lmd_nu=1.5e-6,    ! Molecular viscosity [m^2/s];
                             !
                             ! Value of double-diffusive density 
     &     lmd_Rrho0=1.9,    ! ratio where diffusivity goes to 
                             ! zero in salt fingering.
     &     lmd_nuf=10.0e-4,  ! Scaling factors for double diffusion
     &     lmd_fdd=0.7,      ! coefficient in salt fingering.
                             !
     &     lmd_tdd1=0.909,   !
     &     lmd_tdd2=4.6,     ! Double diffusion constants 
     &     lmd_tdd3=0.54,    ! for temperature (Marmorino and 
     &     lmd_sdd1=0.15,    ! Caldwell, 1976) and salinity 
     &     lmd_sdd2=1.85,    ! (Fedorov, 1988).
     &     lmd_sdd3=0.85)    !
     
# undef LMD_NUW_GARGETT
# ifdef LMD_RIMIX
#  define RI_HSMOOTH
#  define RI_VSMOOTH
      real Ri, ratio, dudz,dvdz,shear2
# endif
# ifdef LMD_BOTEK
      real Kvbotek, Kvbotmax, lmd_cekman, hekman
      parameter(lmd_cekman = 0.3,  ! Coefficient for bottom Ekman
                                   ! thickness (Soulsby, 1983)
     &          Kvbotmax   = 0.01) ! Maximum bottom viscosity
# endif
# ifdef LMD_DDMIX
      real Rrho, ddDS, ddDT, nu_dds, nu_ddt, alfaobeta, Tt, Ts, Tp

      real A0,A1,A2,A3,A4, B0,B1, C0, D0,D1,D2, E0,F0,G0,H0, Smean
      parameter(A0=+0.665157E-01, A1=+0.170907E-01, A2=-0.203814E-03,
     &          A3=+0.298357E-05, A4=-0.255019E-07, B0=+0.378110E-02,
     &          B1=-0.846960E-04, C0=-0.678662E-05, D0=+0.380374E-04,
     &          D1=-0.933746E-06, D2=+0.791325E-08, E0=-0.164759E-06,
     &          F0=-0.251520E-11, G0=+0.512857E-12, H0=-0.302285E-13,
     &          Smean=35.0)
# endif

# ifndef EW_PERIODIC
      integer imin,imax
# endif
# ifndef NS_PERIODIC
      integer jmin,jmax
# endif
# define tind nstp

# ifdef LMD_RIMIX
!
! Compute Richardson number.
!-------------------------------------------------------------------
! Compute horizontal velocity shear squared,  shear2=(du/dz)^2+
! +(dv/dz)^2, at horizontal RHO-points and vertical W-points.
!
! After that compute gradient Richardson number, bounded by
! a very large negative value, at horizontal RHO-points and
! vertical W-points.
!
#  ifdef EW_PERIODIC
#   define I_EXT_RANGE Istr-1,Iend+1
#  else
      if (WESTERN_EDGE) then
        imin=Istr
      else
        imin=Istr-1
      endif
      if (EASTERN_EDGE) then
        imax=Iend
      else
        imax=Iend+1
      endif
#   define I_EXT_RANGE imin,imax
#  endif
#  ifdef NS_PERIODIC
#   define J_EXT_RANGE Jstr-1,Jend+1
#  else
      if (SOUTHERN_EDGE) then
        jmin=Jstr
      else
        jmin=Jstr-1
      endif
      if (NORTHERN_EDGE) then
        jmax=Jend
      else
        jmax=Jend+1
      endif
#   define J_EXT_RANGE jmin,jmax
#  endif

      do k=1,N-1
        do j=J_EXT_RANGE
          do i=I_EXT_RANGE
            cff=0.5/(z_r(i,j,k+1)-z_r(i,j,k))
            dudz=cff*(u(i  ,j,k+1,tind)-u(i  ,j,k,tind)+
     &                u(i+1,j,k+1,tind)-u(i+1,j,k,tind))
            dvdz=cff*(v(i,j  ,k+1,tind)-v(i,j  ,k,tind)+
     &                v(i,j+1,k+1,tind)-v(i,j+1,k,tind))
            shear2=dudz*dudz+dvdz*dvdz
            Rig(i,j,k)=bvf(i,j,k)/max(shear2, 1.E-10)
          enddo
        enddo

#  ifdef RI_HSMOOTH
!
!    Smooth Ri horizontally
!
#   ifndef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=J_EXT_RANGE
            Rig(Istr-1,j,k)=Rig(Istr,j,k)
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=J_EXT_RANGE
            Rig(Iend+1,j,k)=Rig(Iend,j,k)
          enddo
        endif
#   endif
#   ifndef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=I_EXT_RANGE
            Rig(i,Jstr-1,k)=Rig(i,Jstr,k)
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=I_EXT_RANGE
            Rig(i,Jend+1,k)=Rig(i,Jend,k)
          enddo
        endif
#    ifndef EW_PERIODIC
        if (WESTERN_EDGE.and.SOUTHERN_EDGE) then
          Rig(Istr-1,Jstr-1,k)=Rig(Istr,Jstr,k)
        endif
        if (WESTERN_EDGE.and.NORTHERN_EDGE) then
          Rig(Istr-1,Jend+1,k)=Rig(Istr,Jend,k)
        endif
        if (EASTERN_EDGE.and.SOUTHERN_EDGE) then
          Rig(Iend+1,Jstr-1,k)=Rig(Iend,Jstr,k)
        endif
        if (EASTERN_EDGE.and.NORTHERN_EDGE) then
          Rig(Iend+1,Jend+1,k)=Rig(Iend,Jend,k)
        endif
#    endif
#   endif
#   undef I_EXT_RANGE
#   undef J_EXT_RANGE
!
!    Smooth Rig horizontally [Array Rig(:,:,0) is used
!                            as scratch workspace here]
      do j=Jstr,Jend+1
        do i=Istr,Iend+1
          Rig(i,j,0)=0.25*(Rig(i,j  ,k)+Rig(i-1,j  ,k)
     &                    +Rig(i,j-1,k)+Rig(i-1,j-1,k))
#   ifdef MASKING
     &                                      *pmask2(i,j)     
#   endif
        enddo
      enddo
      do j=Jstr,Jend
        do i=Istr,Iend
#   ifdef MASKING
          cff=0.25*(pmask2(i,j)   +pmask2(i+1,j)
     &             +pmask2(i,j+1) +pmask2(i+1,j+1))
#   else
          cff=1.
#   endif
          Rig(i,j,k)=(1-cff)*Rig(i,j,k)+
     &                0.25*(Rig(i,j  ,0)+Rig(i+1,j  ,0)
     &                     +Rig(i,j+1,0)+Rig(i+1,j+1,0))
#   ifdef MASKING
          Rig(i,j,k)=Rig(i,j,k)*rmask(i,j)
#   endif
        enddo
      enddo                     !--> discard  Rig(:,:,0)
#   endif /* RI_HSMOOTH */
      enddo  !--> k loop

#  ifdef RI_VSMOOTH
!
!  Smooth Rig vertically at the interior points.
!
      do k=N-2,2,-1
        do j=Jstr,Jend
          do i=Istr,Iend
            Rig(i,j,k)=0.25*Rig(i,j,k-1)+
     &                 0.50*Rig(i,j,k  )+
     &                 0.25*Rig(i,j,k+1)
          enddo
        enddo
      enddo
#  endif /* RI_VSMOOTH */
# endif /* LMD_RIMIX */

      do k=1,N-1
!
! Compute "interior" viscosities and diffusivities everywhere as 
! the superposition of three processes: local Richardson number
! instability due to resolved vertical shear, internal wave
! breaking, and double diffusion.
!
        do j=Jstr,Jend
          do i=Istr,Iend
!
! Compute shear instability mixing.
!------------------------------------------------------
! Insure that local gradient Richardson number is positive.
! Then compute interior diffusivity due to shear instability
! mixing (lmd_Ri0=0.7).
!
# ifdef LMD_RIMIX
            Ri=max(0.,Rig(i,j,k))
            ratio=min(1.,Ri/lmd_Ri0)
            nu_sx=1.-ratio*ratio
            nu_sx=nu_sx*nu_sx*nu_sx
# else
            nu_sx=0.
# endif

!
!  Compute interior diffusivity due to wave breaking 
!  (choose constant values as in Large et al. or 
!   Gargett and Holloway parameterization)
!
# ifdef LMD_NUW_GARGETT
           cff=1./SQRT(MAX(bvf(i,j,k),1.e-7))
           lmd_iwm=1.e-6*cff
           lmd_iws=1.e-7*cff
# else
           lmd_iwm=lmd_nuwm
           lmd_iws=lmd_nuws
# endif
!
! Compute interior convective diffusivity due to
! static instability mixing.
!
# if defined LMD_CONVEC && !defined LMD_SKPP && !defined LMD_BKPP
            if (bvf(i,j,k).ge.0.) then
              nu_sxc=0.
            else
              nu_sxc=1.
            endif
# else
            nu_sxc=0.
# endif

!
! Sum contributions due to internal wave breaking, shear
! instability and convective diffusivity due to shear
! instability.
!
            Kv(i,j,k)=lmd_iwm+lmd_nu0m*nu_sx+lmd_nu0c*nu_sxc
            Kt(i,j,k)=lmd_iws+lmd_nu0s*nu_sx+lmd_nu0c*nu_sxc
            Ks(i,j,k)=Kt(i,j,k)
          enddo
        enddo

# ifdef LMD_DDMIX
!
! Compute double-diffusive mixing.
!-----------------------------------------
! It can occur when vertical gradient of density is stable but the
! vertical gradient of salinity (salt figering) or temperature
! (diffusive convection) is unstable.
!
! Compute the ratio of thermal expansion and saline contraction
! coefficients at horizontal and vertical W-points.
!
! Compute double-diffusive density ratio, Rrho.
!
        do j=Jstr,Jend
          do i=Istr,Iend
            Tt=0.5*(t(i,j,k,tind,itemp)+t(i,j,k+1,tind,itemp))
            Ts=0.5*(t(i,j,k,tind,isalt)+t(i,j,k+1,tind,isalt))
     &                                                 -Smean
            Tp=-z_w(i,j,k)

            alfaobeta=A0+Tt*(A1+Tt*(A2+Tt*(A3+Tt*A4)))
     &                  +Ts*(B0+Tt*B1+Ts*C0)
     &                  +Tp*(D0+Tt*(D1+Tt*D2)+Ts*E0
     &                      +Tp*(Ts*F0+Tt*Tt*G0+Tp*H0))

            ddDT=t(i,j,k+1,tind,itemp)-t(i,j,k,tind,itemp)
            ddDS=t(i,j,k+1,tind,isalt)-t(i,j,k,tind,isalt)
            ddDS=sign(1.,ddDS)*max(abs(ddDS),eps)
            Rrho=alfaobeta*ddDT/ddDS

!
! Salt fingering case.
!--------------------------
!
            if (Rrho.gt.1. .and. ddDS.gt.0.) then
!
! Compute interior diffusivity for double diffusive mixing
! of salinity. Upper bound "Rrho" by "Rrho0"; (lmd_Rrho0=1.9,
! lmd_nuf=0.001).
!
              Rrho=min(Rrho,lmd_Rrho0)
              nu_dds=1.-((Rrho-1.)/(lmd_Rrho0-1.))**2
              nu_dds=lmd_nuf*nu_dds*nu_dds*nu_dds
!
! Compute interior diffusivity for double diffusive mixing
! of temperature (lmd_fdd=0.7).
!
              nu_ddt=lmd_fdd*nu_dds
!
! Diffusive convection case.
!-------------------------------
!
            elseif (Rrho.lt.1. .and. Rrho.gt.0. .and.
     &                               ddDS.lt.0.) then
!
! Compute interior diffusivity for double diffusive mixing of
! temperature (Marmorino and Caldwell, 1976); (lmd_nu=1.5e-6,
! lmd_tdd1=0.909, lmd_tdd2=4.6, lmd_tdd3=0.54).
!
              nu_ddt=lmd_nu*lmd_tdd1*
     &               exp(lmd_tdd2*exp(-lmd_tdd3*((1./Rrho)-1.)))
!
! Compute interior diffusivity for double diffusive mixing 
! of salinity (lmd_sdd1=0.15, lmd_sdd2=1.85, lmd_sdd3=0.85).
!
              if (Rrho.lt.0.5) then
                nu_dds=nu_ddt*lmd_sdd1*Rrho
              else
                nu_dds=nu_ddt*(lmd_sdd2*Rrho-lmd_sdd3)
              endif
            else
              nu_ddt=0.
              nu_dds=0.
            endif
!
! Add double diffusion contribution to temperature and salinity
! mixing coefficients.
! 
            Kt(i,j,k)=Kt(i,j,k)+nu_ddt
            Ks(i,j,k)=Ks(i,j,k)+nu_dds
          enddo
        enddo
# endif /* LMD_DDMIX */
      enddo     ! <-- k

# if defined LMD_BOTEK && !defined LMD_BKPP
!
! Add Bottom Ekman layer parameterization
!
      do j=Jstr,Jend
        do i=Istr,Iend
          ustar(i,j)=sqrt(sqrt( (0.5*(bustr(i,j)+bustr(i+1,j)))**2
     &                         +(0.5*(bvstr(i,j)+bvstr(i,j+1)))**2))
        enddo
      enddo
      do k=1,N-1
        do j=Jstr,Jend
          do i=Istr,Iend
             hekman=lmd_cekman*ustar(i,j)/max(abs(f(i,j)),eps)
             hekman=min(hekman,z_w(i,j,N)-z_w(i,j,0))
             Kvbotek=min(Kvbotmax,max(0.,vonKar*ustar(i,j)*
     &                                (z_w(i,j,k)-z_w(i,j,0))*4.*
     &             (hekman+z_w(i,j,0)-z_w(i,j,k))/(hekman*hekman) ))
             Kv(i,j,k)=Kv(i,j,k)+Kvbotek
             Kt(i,j,k)=Kt(i,j,k)+Kvbotek
#  ifdef SALINITY
             Ks(i,j,k)=Ks(i,j,k)+Kvbotek
#  endif
          enddo
        enddo
      enddo
# endif /* LMD_BOTEK */

# if defined LMD_SKPP || defined LMD_BKPP
!
! Pad out surface and bottom values for lmd_blmix calculations.
! The interior values used here may not be the best values to
! use for the padding.
!
      do j=Jstr,Jend
        do i=Istr,Iend
          Kv(i,j,N)=Kv(i,j,N-1)
          Ks(i,j,N)=Ks(i,j,N-1)
          Kt(i,j,N)=Kt(i,j,N-1)
          Kv(i,j,0)=Kv(i,j,  1)
          Ks(i,j,0)=Ks(i,j,  1)
          Kt(i,j,0)=Kt(i,j,  1)
        enddo
      enddo
# endif /* LMD_SKPP || LMD_BKPP */

      return
      end
!
!====================================================================
!
!   Subroutine lmd_finalize
!
!====================================================================
!
      subroutine lmd_finalize_tile (istr,iend,jstr,jend, Kv,Kt,Ks)
!
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "mixing.h"
# include "forces.h"
# include "scalars.h"
      integer istr,iend,jstr,jend, i,j,k
      real  Kv(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &      Kt(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &      Ks(PRIVATE_2D_SCRATCH_ARRAY,0:N)
# ifdef MASKING
#  define SWITCH *
# else
#  define SWITCH !
# endif
!
!------------------------------------------------
!  Finalize: copy into shared arrays 
!------------------------------------------------
!
      do j=jstr,jend
        do i=istr,iend
          do k=1,N-1
            Akv(i,j,k)       =Kv(i,j,k) SWITCH rmask(i,j)
            Akt(i,j,k,itemp) =Kt(i,j,k) SWITCH rmask(i,j)
# ifdef SALINITY
            Akt(i,j,k,isalt) =Ks(i,j,k) SWITCH rmask(i,j)
# endif
          enddo

          Akv(i,j,N)       =max(1.5*Kv(i,j,N-1)-0.5*Kv(i,j,N-2),0.)
          Akt(i,j,N,itemp) =max(1.5*Kt(i,j,N-1)-0.5*Kt(i,j,N-2),0.)
# ifdef SALINITY
          Akt(i,j,N,isalt) =max(1.5*Ks(i,j,N-1)-0.5*Ks(i,j,N-2),0.)
# endif
!
!------------------------------------------------
!  Apply wave effects
!------------------------------------------------
!
# ifdef MRL_WCI
          do k=1,N
            Akv(i,j,k)       =Akv(i,j,k)+Akb(i,j,k)
            Akt(i,j,k,itemp) =Akt(i,j,k,itemp)+Akb(i,j,k)+Akw(i,j,k)
#  ifdef SALINITY
            Akt(i,j,k,isalt) =Akt(i,j,k,isalt)+Akb(i,j,k)+Akw(i,j,k)
#  endif
          enddo
# endif
          Akv(i,j,0)       =0.
          Akt(i,j,0,itemp) =0.
# ifdef SALINITY
          Akt(i,j,0,isalt) =0.
# endif
        enddo
      enddo
!
!------------------------------------------------
!  Apply boundary conditions
!------------------------------------------------
!
# define k0 0 
# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        do j=jstr,jend
          do k=k0,N
            Akv(istr-1,j,k)=Akv(istr,j,k)
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
          Akt(istr-1,jstr-1,k,itemp)=Akt(istr,jstr,k,itemp)
#  ifdef SALINITY
          Akt(istr-1,jstr-1,k,isalt)=Akt(istr,jstr,k,isalt)
#  endif
        enddo
      endif
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        do k=k0,N
          Akv(istr-1,jend+1,k)=Akv(istr,jend,k)
          Akt(istr-1,jend+1,k,itemp)=Akt(istr,jend,k,itemp)
#  ifdef SALINITY
          Akt(istr-1,jend+1,k,isalt)=Akt(istr,jend,k,isalt)
#  endif
        enddo
      endif
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=k0,N
          Akv(iend+1,jstr-1,k)=Akv(iend,jstr,k)
          Akt(iend+1,jstr-1,k,itemp)=Akt(iend,jstr,k,itemp)
#  ifdef SALINITY
          Akt(iend+1,jstr-1,k,isalt)=Akt(iend,jstr,k,isalt)
#  endif
        enddo
      endif
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        do k=k0,N
          Akv(iend+1,jend+1,k)=Akv(iend,jend,k)
          Akt(iend+1,jend+1,k,itemp)=Akt(iend,jend,k,itemp)
#  ifdef SALINITY
          Akt(iend+1,jend+1,k,isalt)=Akt(iend,jend,k,isalt)
#  endif
        enddo
      endif
#  endif
# endif

# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_w3d_tile (istr,iend,jstr,jend, Akv)
      call exchange_w3d_tile (istr,iend,jstr,jend,
     &                        Akt(START_2D_ARRAY,0,itemp))
#  ifdef SALINITY
      call exchange_w3d_tile (istr,iend,jstr,jend,
     &                        Akt(START_2D_ARRAY,0,isalt))
#  endif
# endif

#else
      subroutine lmd_vmix_empty
#endif /* LMD_MIXING */
      return    
      end

