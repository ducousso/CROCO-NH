! $Id: gls_mixing.F 1524 2014-04-14 17:00:06Z gcambon $
!
!======================================================================
! CROCO is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! CROCO specific routines (nesting) are under CeCILL-C license.
! 
! CROCO website : http://www.croco-ocean.org/
!======================================================================
!
! Umlauf and Burchard (2003) have come up with a generic two-equation
! turbulence closure scheme which can be tuned to behave like several
! of the traditional schemes, including that of Mellor and Yamada 2.5.
! This is known as the Generic Length Scale, or GLS vertical mixing
! scheme and was introduced to ROMS by Warner et al. (2005). 
! Warner's implementation contains 4 schemes: 
!           k-kl (~ MY2.5), k-epsilon, k-omega, k-gen
!
! References: 
! 
! Umlauf, L. and H. Burchard, 2001:  A generic length-scale 
!   Equation for geophysical turbulence models. 
!
! Warner, J. C., C. R. Sherwood, H. G. Arango, and R. P. Signell,
!   2005: Performance of four turbulence closure methods implemented
!   using a generic length scale method, Ocean Modelling, 8, 81-113
!
! This file contains the computations of GLS turbulent closure
! at corrector step. It was adapted from the RUTGERS code 
! by S. Herbette and P. Marchesiello, 2013
!
!======================================================================
!
#include "cppdefs.h"
#if defined SOLVE3D && defined GLS_MIXING
# undef  SPLINE_SHEAR
# define HORIZ_SMOOTH
# undef  GLS_DIF2
 
!**********************************************************************
      subroutine step3d_gls (tile)
!
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "ocean3d.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call step3d_gls_tile (Istr, Iend, Jstr, Jend,
     &                 A3d(1, 1,trd), A3d(1, 2,trd), 
     &                 A2d(1, 1,trd), A2d(1, 2,trd),  
     &                 A2d(1, 3,trd), A2d(1, 4,trd), A2d(1, 5,trd),  
     &                 A2d(1, 6,trd), A2d(1, 7,trd), A2d(1, 8,trd), 
     &                 A2d(1, 9,trd), A2d(1,10,trd), A2d(1,11,trd), 
     &                 A2d(1,12,trd), A2d(1,13,trd))
      return
      end
!
!**********************************************************************
      subroutine step3d_gls_tile (Istr, Iend, Jstr, Jend, 
     &                 shear2, buoy2, 
     &                 WORK, CF, 
     &                 wrk1, wrk2, wrk3,
     &                 wrk4, wrk5, wrk6,
     &                 wrk7, wrk8, wrk9,
     &                 wrk10, wrk11)
!                                                                      !
!=======================================================================
!                                                                      !
!  This routine perfoms the corrector step for turbulent kinetic       !
!  energy and generic length scale prognostic variables, tke and       !
!  gls.                                                                !
!                                                                      !
!=======================================================================
!
      implicit none
# include "param.h"
      integer Istr, Iend, Jstr, Jend
      integer i, j, k, imin, imax, jmin, jmax
      real cff1, cff2, cff3, cff, eps, epsil, ustar2
      real shear2(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &     buoy2(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &     WORK(PRIVATE_2D_SCRATCH_ARRAY),
     &     CF(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     wrk1(PRIVATE_2D_SCRATCH_ARRAY),
     &     wrk2(PRIVATE_2D_SCRATCH_ARRAY),
     &     wrk3(PRIVATE_2D_SCRATCH_ARRAY),
     &     wrk4(PRIVATE_2D_SCRATCH_ARRAY),
     &     wrk5(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     wrk6(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     wrk7(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     wrk8(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     wrk9(PRIVATE_1D_SCRATCH_ARRAY),
     &     wrk10(PRIVATE_1D_SCRATCH_ARRAY),
     &     wrk11(PRIVATE_1D_SCRATCH_ARRAY)
      real Zos_min, Zob_min, Gh, Gm, Kprod, Ls_unlmt, Ls_lmt, Pprod, 
     &     Sh, Sm, cmu_fac1, cmu_fac2, cmu_fac3, cmu_fac4, L_sft,
     &     gls_c3, gls_exp1, gls_fac1, gls_fac2, gls_fac3, cb_wallE,
     &     gls_fac4, gls_fac5, gls_fac6, ql, sqrt2, strat2,
     &     tke_exp1, tke_exp2, tke_exp3, tke_exp4, wall_fac, ogls_sigp,
     &     sig_eff, gls_sigp_cb 
      real cx, cy, dft, dfx, dfy, tau, tau_in, tau_out
      parameter (eps=1.E-10, epsil=1.E-16)
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "forces.h"
# include "mixing.h"
# include "scalars.h"
# include "coupling.h"
!
# ifdef MASKING
#  define SWITCH *
# else
#  define SWITCH !
# endif
!
# include "compute_auxiliary_bounds.h"
!
!-----------------------------------------------------------------------
!  Compute several constants.
!-----------------------------------------------------------------------
!
      Zos_min=max(Zos,0.0001) ! Zos: surface roughness
      Zob_min=max(Zob,0.0001)
!
# if defined CRAIG_BANNER || defined TKE_WAVEDISS
#  ifdef GLS_KKL
      cb_wallE=1.25
#  else
      cb_wallE=1.
#  endif
      L_sft=vonKar
      cff1=sqrt(1.5*gls_sigk)*gls_cmu0/L_sft
      gls_sigp_cb=L_sft**2/(gls_cmu0**2*gls_c2*cb_wallE)
     &            *(gls_n**2-cff1*gls_n/3.*(4.*gls_m+1.)
     &                      +cff1**2*gls_m/9.*(2.+4.*gls_m))
# else
      L_sft=vonKar
      gls_sigp_cb=gls_sigp
# endif
      ogls_sigp=1./gls_sigp_cb
      sqrt2=sqrt(2.)
      cmu_fac1=gls_cmu0**(-gls_p/gls_n)
      cmu_fac2=gls_cmu0**(3.+gls_p/gls_n)
      cmu_fac3=1./gls_cmu0**2
      cmu_fac4=(1.5*gls_sigk)**(1./3.)/(gls_cmu0**(4./3.))
      gls_fac1=gls_n*gls_cmu0**(gls_p+1.)
      gls_fac2=gls_cmu0**gls_p*gls_n*vonKar**gls_n
      gls_fac3=gls_cmu0**(gls_p)*gls_n
      gls_fac4=gls_cmu0**gls_p*(vonKar*Zob_min)**gls_n
      gls_fac5=0.56**(0.5*gls_n)*gls_cmu0**gls_p
      gls_fac6=8./gls_cmu0**6.
!
      gls_exp1=1./gls_n
      tke_exp1=gls_m/gls_n
      tke_exp2=0.5+gls_m/gls_n
      tke_exp3=0.5+gls_m
      tke_exp4=gls_m+0.5*gls_n
!
!-----------------------------------------------------------------------
!  Compute vertical velocity shear at W-points.
!-----------------------------------------------------------------------
!
# ifdef EW_PERIODIC
#  define I_EXT_RANGE Istr-1,Iend+1
# else
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
#  define I_EXT_RANGE imin,imax
# endif
# ifdef NS_PERIODIC
#  define J_EXT_RANGE Jstr-1,Jend+1
# else
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
#  define J_EXT_RANGE jmin,jmax
# endif      

# define dU wrk5
# define dV wrk6
# ifdef SPLINE_SHEAR
      do j=J_EXT_RANGE
        do i=I_EXT_RANGE
          CF(i,0)=0.
          dU(i,0)=0.
          dV(i,0)=0.
        enddo
        do k=1,N-1
          do i=I_EXT_RANGE
            cff=1./(2.*Hz(i,j,k+1)+Hz(i,j,k)*(2.-CF(i,k-1)))
            CF(i,k)=cff*Hz(i,j,k+1)
            dU(i,k)=cff*( 3.*( u(i  ,j,k+1,nstp)-u(i,  j,k,nstp)
     &                        +u(i+1,j,k+1,nstp)-u(i+1,j,k,nstp) )
     &                                      -Hz(i,j,k)*dU(i,k-1) )
            dV(i,k)=cff*( 3.*( v(i,j  ,k+1,nstp)-v(i,j  ,k,nstp)
     &                        +v(i,j+1,k+1,nstp)-v(i,j+1,k,nstp) )
     &                                      -Hz(i,j,k)*dV(i,k-1) )
          enddo
        enddo
        do i=I_EXT_RANGE
          dU(i,N)=0.
          dV(i,N)=0.
        enddo
        do k=N-1,1,-1
          do i=I_EXT_RANGE
            dU(i,k)=dU(i,k)-CF(i,k)*dU(i,k+1)
            dV(i,k)=dV(i,k)-CF(i,k)*dV(i,k+1)
          enddo
        enddo
        do k=1,N-1
          do i=I_EXT_RANGE
            shear2(i,j,k)=dU(i,k)*dU(i,k)+dV(i,k)*dV(i,k)
          enddo
        enddo
      enddo
# else
      do k=1,N-1
        do j=J_EXT_RANGE
          do i=I_EXT_RANGE
            cff=0.5/(z_r(i,j,k+1)-z_r(i,j,k))
            shear2(i,j,k)=(cff*(u(i  ,j,k+1,nstp)-u(i  ,j,k,nstp)
     &                         +u(i+1,j,k+1,nstp)-u(i+1,j,k,nstp)))**2
     &                   +(cff*(v(i,j  ,k+1,nstp)-v(i,j  ,k,nstp)
     &                         +v(i,j+1,k+1,nstp)-v(i,j+1,k,nstp)))**2
          enddo
        enddo
      enddo           ! <-- discard dU,dV,CF
# endif
# undef dU
# undef dV

! Load Brunt-Vaisala frequency.
      do k=1,N-1
        do j=Jstr-1,Jend+1
          do i=Istr-1,Iend+1
            buoy2(i,j,k)=bvf(i,j,k)
          enddo
        enddo
      enddo
!
!-----------------------------------------------------------------------
!  Smooth horizontally buoyancy and shear.  Use buoy2(:,:,0) and
!  shear2(:,:,0) as scratch utility array.
!-----------------------------------------------------------------------
!
!csh 
# ifdef HORIZ_SMOOTH
      do k=1,N-1
#   define hwrk shear2
#   include "gls_smooth.h"
#   undef hwrk
#   define hwrk buoy2
#   include "gls_smooth.h"
#   undef hwrk
      enddo
# endif

# undef I_EXT_RANGE
# undef J_EXT_RANGE

! ========== === ======== =========
! horizontal and vertical advection
! ========== === ======== =========

# define FX wrk1
# define FE wrk2
# define FC wrk5
!
!-----------------------------------------------------------------------
! Horizontal advection for tke
!-----------------------------------------------------------------------
!
# define tmp tke
      do k=1,N
# include "gls_horiz_advection.h"
        do j=Jstr,Jend
          do i=Istr,Iend
           tmp(i,j,k,nnew)=Hz_bak(i,j,k)*tmp(i,j,k,nstp) -dt*pm(i,j)
     &                 *pn(i,j)*(FX(i+1,j)-FX(i,j)+FE(i,j+1)-FE(i,j))
          enddo
        enddo
      enddo
# undef tmp
!
!-----------------------------------------------------------------------
! Horizontal advection for gls
!-----------------------------------------------------------------------
!
# define tmp gls
      do k=1,N
# include "gls_horiz_advection.h"
        do j=Jstr,Jend
          do i=Istr,Iend
            tmp(i,j,k,nnew)=Hz_bak(i,j,k)*tmp(i,j,k,nstp) -dt*pm(i,j)
     &                 *pn(i,j)*(FX(i+1,j)-FX(i,j)+FE(i,j+1)-FE(i,j))
          enddo
        enddo
      enddo                     !-->  discard FX,FE
# undef tmp

      do j=Jstr,Jend
!
!-----------------------------------------------------------------------
! Vertical advection for tke
!-----------------------------------------------------------------------
!
# define tmp tke
# include "gls_vert_advection.h"
        do k=1,N
          do i=Istr,Iend
            tmp(i,j,k,nnew)=tmp(i,j,k,nnew)-dt*pm(i,j)*pn(i,j)
     &                                    *(FC(i,k)-FC(i,k-1)) 
          enddo
        enddo
# undef tmp
!
!-----------------------------------------------------------------------
! Vertical advection for gls
!-----------------------------------------------------------------------
!
# define tmp gls
# include "gls_vert_advection.h"
        do k=1,N
          do i=Istr,Iend
            tmp(i,j,k,nnew)=tmp(i,j,k,nnew)-dt*pm(i,j)*pn(i,j)
     &                                    *(FC(i,k)-FC(i,k-1))
          enddo
        enddo   ! Notice at this moment tke and gls are multipled by Hz.
# undef tmp

# undef FX
# undef FE
# undef FC

      enddo
!
!-----------------------------------------------------------------------
!  Add in horizontal Laplacian diffusion along constant S-surfaces.
!-----------------------------------------------------------------------
!
# ifdef GLS_DIF2
!
! csh: GLS_DIF2 not tested, probably some bugs with MPI
!
#  define FXK wrk1
#  define FXP wrk2
#  define FEK wrk3
#  define FEP wrk4
      cff=gls_diff2  ! lateral diffusion coef. [m2/s]
      do k=1,N
       do j=Jstr,Jend
         do i=Istr,Iend+1
           FXK(i,j)=cff*pmon_u(i,j)*(tke(i,j,k,nrhs)-tke(i-1,j,k,nrhs))
           FXP(i,j)=cff*pmon_u(i,j)*(gls(i,j,k,nrhs)-gls(i-1,j,k,nrhs))
#  ifdef MASKING
           FXK(i,j)=FXK(i,j)*umask(i,j)
           FXP(i,j)=FXP(i,j)*umask(i,j)
#  endif
          enddo
        enddo
        do j=Jstr,Jend+1
          do i=Istr,Iend
            FEK(i,j)=cff*pnom_v(i,j)*(tke(i,j,k,nrhs)-tke(i,j-1,k,nrhs))
            FEP(i,j)=cff*pnom_v(i,j)*(gls(i,j,k,nrhs)-gls(i,j-1,k,nrhs))
#  ifdef MASKING
            FEK(i,j)=FEK(i,j)*vmask(i,j)
            FEP(i,j)=FEP(i,j)*vmask(i,j)
#  endif
          enddo
        enddo
        do j=Jstr,Jend
          do i=Istr,Iend
            tke(i,j,k,nnew)=tke(i,j,k,nnew)+dt*pm(i,j)*pn(i,j)
     &                        *(FXK(i+1,j)-FXK(i,j)+FEK(i,j+1)-FEK(i,j))
            gls(i,j,k,nnew)=gls(i,j,k,nnew)+dt*pm(i,j)*pn(i,j)
     &                        *(FXP(i+1,j)-FXP(i,j)+FEP(i,j+1)-FEP(i,j))
          enddo
        enddo
      enddo
#  undef FXK
#  undef FXP
#  undef FEK
#  undef FEP  
# endif /* GLS_DIF2 */

!
!----------------------------------------------------------------------
!  Compute vertical mixing, turbulent production and turbulent
!  dissipation terms.
!----------------------------------------------------------------------
!
# ifdef EW_PERIODIC
#  define I_EXT_RANGE Istr-1,Iend+1
# else
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
#  define I_EXT_RANGE imin,imax
# endif
# ifdef NS_PERIODIC
#  define J_EXT_RANGE Jstr-1,Jend+1
# else
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
#  define J_EXT_RANGE jmin,jmax
# endif      

      do j=J_EXT_RANGE
!
!  Set term for vertical mixing of turbulent fields.
!
# define FCK wrk5
# define FCP wrk6
# define BCK wrk7
# define BCP wrk8
        cff=-0.5*dt
        do i=Istr,Iend
          do k=2,N-1
            FCK(i,k)=cff*(Akk(i,j,k)+Akk(i,j,k-1))/Hz(i,j,k)
            FCP(i,k)=cff*(Akp(i,j,k)+Akp(i,j,k-1))/Hz(i,j,k)
            CF(i,k)=0.
          enddo
          FCP(i,1)=0.
          FCP(i,N)=0.
          FCK(i,1)=0.
          FCK(i,N)=0.
        enddo
!
!  Compute production and dissipation terms.
!
        do i=Istr,Iend
          do k=1,N-1
!
!  Compute shear and bouyant production of turbulent energy (m3/s3)
!  at W-points (ignore small negative values of buoyancy).
!
            strat2=buoy2(i,j,k)
            if (strat2.gt.0.) then
              gls_c3=gls_c3m
            else
              gls_c3=gls_c3p
            endif
            Kprod=shear2(i,j,k)*(Akv(i,j,k)-Akv_bak)
     &           -strat2*(Akt(i,j,k,itemp)-Akt_bak(itemp))
            Pprod=gls_c1*shear2(i,j,k)*(Akv(i,j,k)-Akv_bak)
     &           -gls_c3*strat2*(Akt(i,j,k,itemp)-Akt_bak(itemp))
!
!  If negative production terms, then add buoyancy to dissipation terms
!  (BCK and BCP) below, using "cff1" and "cff2" as the on/off switch.
!
            cff1=1.
            if (Kprod.lt.0.) then
             Kprod=Kprod+strat2*(Akt(i,j,k,itemp)-Akt_bak(itemp))
             cff1=0.
            endif
            cff2=1.
            if (Pprod.lt.0.) then
             Pprod=Pprod+gls_c3*strat2*(Akt(i,j,k,itemp)-Akt_bak(itemp))
             cff2=0.
            endif
!
!  Time-step shear and buoyancy production terms.
!
            cff=0.5*(Hz(i,j,k)+Hz(i,j,k+1))
            tke(i,j,k,nnew)=tke(i,j,k,nnew)+dt*cff*Kprod
            gls(i,j,k,nnew)=gls(i,j,k,nnew)+dt*cff*Pprod
     &                                  *gls(i,j,k,nstp)
     &                    /max(tke(i,j,k,nstp),gls_Kmin)
!
!  Compute dissipation of turbulent energy (m3/s3).
!
# ifdef GLS_KKL
#   undef  KKL_WALL_PARA1
#   define KKL_WALL_PARA2
#   undef  KKL_WALL_TRI
#   undef  KKL_WALL_LINEAR
!
!  Parabolic wall function,  L = ds db / (ds + db).
!
#   ifdef KKL_WALL_PARA1
            wall_fac=1. +gls_E2/(vonKar*vonKar)
     &               *(gls(i,j,k,nstp)**( gls_exp1)*cmu_fac1
     &                *tke(i,j,k,nstp)**(-tke_exp1)
     &               *(1./(z_w(i,j,N)-z_w(i,j,k))
     &                +1./(z_w(i,j,k)-z_w(i,j,0))))**2
!
!  Parabolic wall function + free surface correction (default)
!
#   elif defined KKL_WALL_PARA2
            wall_fac=1. +gls_E2/(vonKar*vonKar)
     &               *(gls(i,j,k,nstp)**( gls_exp1)*cmu_fac1
     &                *tke(i,j,k,nstp)**(-tke_exp1)
     &                *(1./ (z_w(i,j,k)-z_w(i,j,0))))**2
     &               +0.25/(vonKar*vonKar)
     &               *(gls(i,j,k,nstp)**( gls_exp1)*cmu_fac1
     &                *tke(i,j,k,nstp)**(-tke_exp1)
     &                *(1./ (z_w(i,j,N)-z_w(i,j,k))))**2
!
!  Triangular wall function, L = min (ds, db).
!
#   elif defined KKL_WALL_TRI
            wall_fac=1. +gls_E2/(vonKar*vonKar)
     &                *(gls(i,j,k,nstp)**( gls_exp1)*cmu_fac1
     &                 *tke(i,j,k,nstp)**(-tke_exp1)
     &                 *(1./min((z_w(i,j,N)-z_w(i,j,k)),
     &                          (z_w(i,j,k)-z_w(i,j,0)))))**2
!
!  Linear wall function for , L = ds (=dist to surface).
!
#   elif defined KKL_WALL_LINEAR
            wall_fac=1. +gls_E2/(vonKar*vonKar)
     &                *(gls(i,j,k,nstp)**( gls_exp1)*cmu_fac1
     &                 *tke(i,j,k,nstp)**(-tke_exp1)
     &                *(1./ (z_w(i,j,N)-z_w(i,j,k))))**2
#   else
            write(*,*)'Wall function error for GLS_KKL: gls_mixing.F'
#   endif
# else
            wall_fac=1.
# endif
            BCK(i,k)=cff*(1. +dt
     &                   *gls(i,j,k,nstp)**(-gls_exp1)*cmu_fac2
     &                   *tke(i,j,k,nstp)**( tke_exp2)
     &                   +dt*(1.-cff1)*strat2
     &                   *(Akt(i,j,k,itemp)-Akt_bak(itemp))
     &                   /tke(i,j,k,nstp))-FCK(i,k)-FCK(i,k+1)
            BCP(i,k)=cff*(1. +dt*gls_c2*wall_fac
     &                   *gls(i,j,k,nstp)**(-gls_exp1)*cmu_fac2
     &                   *tke(i,j,k,nstp)**( tke_exp2)
     &                   +dt*(1.-cff2)*gls_c3*strat2
     &                   *(Akt(i,j,k,itemp)-Akt_bak(itemp))
     &                   /tke(i,j,k,nstp))-FCP(i,k)-FCP(i,k+1)
          enddo
        enddo
!
!-----------------------------------------------------------------------
!  Time-step dissipation and vertical diffusion terms implicitly.
!-----------------------------------------------------------------------
!
!  Set Dirichlet surface and bottom boundary conditions. Compute
!  surface roughness from wind stress (Charnok) and set Craig and
!  Banner wave breaking surface flux, if appropriate.
!
# define Zos_eff wrk9
        do i=I_EXT_RANGE
# if defined CRAIG_BANNER
          tke(i,j,N,nnew)=max( gls_Kmin, cmu_fac4*0.5*
     &                              sqrt((sustr(i,j)+sustr(i+1,j))**2
     &                                  +(svstr(i,j)+svstr(i,j+1))**2)
     &                                       *crgban_cw**(2./3.) )
# elif defined TKE_WAVEDISS
          tke(i,j,N,nnew)=max( gls_Kmin, cmu_fac4
     &                       *(sz_alpha*wdsp(i,j)*rho0)**(2./3.) )
# else
          tke(i,j,N,nnew)=max( gls_Kmin, cmu_fac3*0.5
     &                           *sqrt((sustr(i,j)+sustr(i+1,j))**2 
     &                                +(svstr(i,j)+svstr(i,j+1))**2) )
# endif
          tke(i,j,0,nnew)=MAX(cmu_fac3*0.5*                          
     &                        SQRT((bustr(i,j)+bustr(i+1,j))**2+     
     &                             (bvstr(i,j)+bvstr(i,j+1))**2),    
     &                        gls_Kmin)

# if defined CHARNOK
          Zos_eff(i)=max( Zos_min, charnok_alpha/g*0.5*
     &                            sqrt((sustr(i,j)+sustr(i+1,j))**2
     &                                +(svstr(i,j)+svstr(i,j+1))**2) )
# elif defined ZOS_HSIG
          Zos_eff(i)=max(Zos_min, zos_hsig_alpha*whrm(i,j)*sqrt2)
# else
          Zos_eff(i)=Zos_min
# endif
          gls(i,j,N,nnew)=max( gls_Pmin, gls_cmu0**gls_p
     &          *tke(i,j,N,nnew)**gls_m*(L_sft*Zos_eff(i))**gls_n )
          gls(i,j,0,nnew)=max( gls_Pmin,
     &                            gls_fac4*tke(i,j,0,nnew)**gls_m )
        enddo
!
!  Solve tri-diagonal system for turbulent kinetic energy.
!
# define tke_fluxt wrk10
# define tke_fluxb wrk11
        do i=I_EXT_RANGE
# if defined CRAIG_BANNER
          tke_fluxt(i)=dt*crgban_cw
     &              *( 0.5*sqrt((sustr(i,j)+sustr(i+1,j))**2
     &                           +(svstr(i,j)+svstr(i,j+1))**2) )**1.5
# elif defined TKE_WAVEDISS
          tke_fluxt(i)=dt*sz_alpha*wdsp(i,j)*rho0
# else
          tke_fluxt(i)=0.
# endif
          tke_fluxb(i)=0.
          cff=1./BCK(i,N-1)
          CF(i,N-1)=cff*FCK(i,N-1)
          tke(i,j,N-1,nnew)=cff*(tke(i,j,N-1,nnew)+tke_fluxt(i))
        enddo
        do i=Istr,Iend
          do k=N-2,1,-1
            cff=1./(BCK(i,k)-CF(i,k+1)*FCK(i,k+1))
            CF(i,k)=cff*FCK(i,k)
            tke(i,j,k,nnew)=cff*( tke(i,j,k,nnew)
     &                          -FCK(i,k+1)*tke(i,j,k+1,nnew) )
          enddo
          tke(i,j,1,nnew)=tke(i,j,1,nnew)-cff*tke_fluxb(i)
        enddo
        do k=2,N-1
          do i=Istr,Iend
            tke(i,j,k,nnew)=tke(i,j,k,nnew)-CF(i,k)*tke(i,j,k-1,nnew)
          enddo
        enddo             ! <-- discard FCK, BCK, tke_fluxt, tke_fluxb
# undef tke_fluxt
# undef tke_fluxb
# define gls_fluxt wrk10
# define gls_fluxb wrk11
!
!  Solve tri-diagonal system for generic statistical field.
!
        do i=I_EXT_RANGE
          cff=0.5*(tke(i,j,N,nnew)+tke(i,j,N-1,nnew))
          gls_fluxt(i)=dt*gls_fac3*cff**gls_m*L_sft**(gls_n)
     &                        *(Zos_eff(i)+0.5*Hz(i,j,N))**(gls_n-1.)
     &                                   *0.5*(Akp(i,j,N)+Akp(i,j,N-1))
# ifdef CRAIG_BANNER
          gls_fluxt(i)=gls_fluxt(i)-dt*gls_m*(gls_cmu0**gls_p)
     &                   *cff**(gls_m-1.)*((Zos_eff(i)+0.5*Hz(i,j,N))
     &                      *L_sft)**gls_n*gls_sigk*ogls_sigp*crgban_cw
     &                          *(0.5*sqrt((sustr(i,j)+sustr(i+1,j))**2
     &                             +(svstr(i,j)+svstr(i,j+1))**2))**1.5
# elif defined TKE_WAVEDISS
          gls_fluxt(i)=gls_fluxt(i)-dt*gls_m*(gls_cmu0**gls_p)
     &                   *cff**(gls_m-1.)*((Zos_eff(i)+0.5*Hz(i,j,N))
     &                                *L_sft)**gls_n*gls_sigk*ogls_sigp
     &                                         *sz_alpha*wdsp(i,j)*rho0
# endif
          cff=0.5*(tke(i,j,0,nnew)+tke(i,j,1,nnew))
          gls_fluxb(i)=dt*gls_fac2*(cff**gls_m)*(0.5*Hz(i,j,1)+Zob_min)
     &                       **(gls_n-1.)*0.5*(Akp(i,j,0)+Akp(i,j,1))
          cff=1./BCP(i,N-1)
          CF(i,N-1)=cff*FCP(i,N-1)
          gls(i,j,N-1,nnew)=cff*(gls(i,j,N-1,nnew)-gls_fluxt(i))
        enddo
        do i=Istr,Iend
          do k=N-2,1,-1
            cff=1./(BCP(i,k)-CF(i,k+1)*FCP(i,k+1))
            CF(i,k)=cff*FCP(i,k)
            gls(i,j,k,nnew)=cff*(gls(i,j,k,nnew)
     &                          -FCP(i,k+1)*gls(i,j,k+1,nnew))
          enddo
          gls(i,j,1,nnew)=gls(i,j,1,nnew)-cff*gls_fluxb(i)
        enddo             ! <-- discard FCP, BCP, gls_fluxt, gls_fluxb
        do k=2,N-1
          do i=Istr,Iend
            gls(i,j,k,nnew)=gls(i,j,k,nnew)-CF(i,k)*gls(i,j,k-1,nnew)
          enddo
        enddo             ! <-- discard CF
# undef gls_fluxt
# undef gls_fluxb
# undef FCK
# undef FCP
# undef BCK
# undef BCP
!
!---------------------------------------------------------------------
!  Compute vertical mixing coefficients (m2/s).
!---------------------------------------------------------------------
!
        do i=Istr,Iend
          do k=1,N-1
!
!  Compute turbulent length scale (m).
!
            tke(i,j,k,nnew)=max(tke(i,j,k,nnew),gls_Kmin)
            gls(i,j,k,nnew)=max(gls(i,j,k,nnew),gls_Pmin)
            if (gls_n.ge.0.) then
              gls(i,j,k,nnew)=min( gls(i,j,k,nnew),gls_fac5
     &                               *tke(i,j,k,nnew)**(tke_exp4)
     &            *(sqrt(max(0.,buoy2(i,j,k)))+eps)**(-gls_n) )
            else
              gls(i,j,k,nnew)=max( gls(i,j,k,nnew),gls_fac5
     &                               *tke(i,j,k,nnew)**(tke_exp4)
     &            *(sqrt(max(0.,buoy2(i,j,k)))+eps)**(-gls_n) )
            endif
            Ls_unlmt=max( gls(i,j,k,nnew)**( gls_exp1)*cmu_fac1
     &                   *tke(i,j,k,nnew)**(-tke_exp1), eps )
            if (buoy2(i,j,k).gt.0.) then
              Ls_lmt=min( Ls_unlmt, sqrt(0.56*tke(i,j,k,nnew)
     &                           /(max(0.,buoy2(i,j,k))+eps)) )
            else
              Ls_lmt=Ls_unlmt
            endif
!
! Recompute gls based on limited length scale
!
            gls(i,j,k,nnew)=max( gls_Pmin, gls_cmu0**gls_p
     &                    *tke(i,j,k,nnew)**gls_m*Ls_lmt**gls_n )
!
!  Compute nondimensional stability functions for tracers (Sh) and
!  momentum (Sm).
!
            Gh=min(gls_Gh0,-buoy2(i,j,k)*Ls_lmt*Ls_lmt
     &                         /(2.*tke(i,j,k,nnew)))
            Gh=min(Gh,Gh-(Gh-gls_Ghcri)**2
     &                    /(Gh+gls_Gh0-2.*gls_Ghcri))
            Gh=max(Gh,gls_Ghmin)
# if defined CANUTO_A || defined CANUTO_B
!
!  Compute shear number.
!
            Gm=(gls_b0/gls_fac6-gls_b1*Gh+gls_b3*gls_fac6*(Gh**2))
     &                                /(gls_b2-gls_b4*gls_fac6*Gh)
            Gm=min( Gm, shear2(i,j,k)*Ls_lmt*Ls_lmt
     &                                   /(2.*tke(i,j,k,nnew)) )
!
!  Compute stability functions
!
            cff=gls_b0-gls_b1*gls_fac6*Gh+gls_b2*gls_fac6*Gm
     &         +gls_b3*gls_fac6**2*Gh**2-gls_b4*gls_fac6**2*Gh*Gm
     &                                  +gls_b5*gls_fac6**2*Gm*Gm
            Sm=(gls_s0-gls_s1*gls_fac6*Gh+gls_s2*gls_fac6*Gm)/cff
            Sh=(gls_s4-gls_s5*gls_fac6*Gh+gls_s6*gls_fac6*Gm)/cff
            Sm=max(Sm,0.)
            Sh=max(Sh,0.)
!
!  Relate Canuto stability to ROMS notation
!
            Sm=Sm*sqrt2/gls_cmu0**3
            Sh=Sh*sqrt2/gls_cmu0**3
# elif defined KANTHA_CLAYSON
            cff=1.-my_Sh2*Gh
            Sh=my_Sh1/cff
            Sm=(my_B1pm1o3+my_Sm4*Sh*Gh)/(1.-my_Sm2*Gh)
# else  /* Galperin */
            cff=1.D0-my_Sh2*Gh
            Sh=my_Sh1/cff
            Sm=(my_Sm3+Sh*Gh*my_Sm4)/(1.-my_Sm2*Gh)
# endif
!
!  Compute vertical mixing (m2/s) coefficients of momentum and
!  tracers.  Average ql over the two timesteps rather than using
!  the new Lscale and just averaging tke.
!
            ql=sqrt2*0.5*(Ls_lmt*sqrt(tke(i,j,k,nnew))
     &                      +Lscale(i,j,k)*sqrt(tke(i,j,k,nstp)))
            Akv(i,j,k)=Akv_bak+Sm*ql
            Akt(i,j,k,itemp)=Akt_bak(itemp)+Sh*ql
# ifdef SALINITY
            Akt(i,j,k,isalt)=Akt_bak(isalt)+Sh*ql
# endif
!
!  Compute vertical mixing (m2/s) coefficents of turbulent kinetic
!  energy and generic statistical field.
!
            Akk(i,j,k)=Akk_bak+Sm*ql/gls_sigk
# if defined CRAIG_BANNER || defined TKE_WAVEDISS
!
!  If wave breaking, modify surface boundary condition for
!  gls diffusivity Schmidt number.
!
            Pprod=gls_c1*shear2(i,j,k)*Akv(i,j,k)
            cff=cmu_fac2*tke(i,j,k,nnew)**(1.5+tke_exp1)
     &                   *gls(i,j,k,nnew)**(-1./gls_n)
            cff2=min(Pprod/cff, 1.)
            sig_eff=cff2*gls_sigp+(1.-cff2)*gls_sigp_cb
            Akp(i,j,k)=Akp_bak+Sm*ql/sig_eff
# else
            Akp(i,j,k)=Akp_bak+Sm*ql*ogls_sigp
# endif
!
!  Save limited length scale.
!
            Lscale(i,j,k)=Ls_lmt
          enddo
!
!  Compute vertical mixing coefficients at the surface and bottom.
!
          Akv(i,j,N)=Akv_bak+L_sft*Zos_eff(i)*gls_cmu0
     &                        *sqrt(tke(i,j,N,nnew))
          Akv(i,j,0)=Akv_bak+vonKar*Zob_min*gls_cmu0
     &                        *sqrt(tke(i,j,0,nnew))
          Akk(i,j,N)=Akk_bak+Akv(i,j,N)/gls_sigk
          Akk(i,j,0)=Akk_bak+Akv(i,j,0)/gls_sigk
          Akp(i,j,N)=Akp_bak+Akv(i,j,N)/gls_sigp
          Akp(i,j,0)=Akp_bak+Akv(i,j,0)/gls_sigp
!
          Akt(i,j,N,itemp)=Akt_bak(itemp)
          Akt(i,j,0,itemp)=Akt_bak(itemp)
# ifdef SALINITY
          Akt(i,j,N,isalt)=Akt_bak(isalt)
          Akt(i,j,0,isalt)=Akt_bak(isalt)
# endif
        enddo
      enddo            !<-- end j-loop from computing vertical advection

# undef Zos_eff
!
!-----------------------------------------------------------------------
!  Set lateral boundary conditions.
!-----------------------------------------------------------------------
!
! ATT Verif boundary condition ??
!
# define grad WORK
# define tmp tke
# undef  KV_UPDATE
# include "gls_bc.h"
# undef tmp
# define tmp gls
# define KV_UPDATE
# include "gls_bc.h"
# undef tmp
# undef grad

!
! Exchange computational margines and/or periodic boundaries:
!--------- ------------- -------- ------ -------- ----------- 
!
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend,
     &                        tke(START_2D_ARRAY,0,nnew))
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend,
     &                        gls(START_2D_ARRAY,0,nnew))
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend, Akv)
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend, Akk)
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend, Akp)
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend,
     &                        Akt(START_2D_ARRAY,0,itemp))
#   ifdef SALINITY
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend,
     &                        Akt(START_2D_ARRAY,0,isalt))
#   endif
#  endif
      return
      end

!*******************************************************************************
      subroutine init_gls (ierr)
!
!-----------------------------------------------------------------------
!
      implicit none
      integer ierr
#include "param.h"
#include "scalars.h"
#include "mixing.h"
!
!-----------------------------------------------------------------------
!  Background eddy viscosity for TKE and GLS (must move to read_inp.F).
!-----------------------------------------------------------------------
!
      Akt_bak(itemp)=1.E-6
# ifdef SALINITY
      Akt_bak(isalt)=5.E-6
# endif
      Akk_bak=1.E-6
      Akp_bak=1.E-6
      gls_diff2=1.0    ! horizontal diffusion coefficient [m2/s]
!
!-----------------------------------------------------------------------
!  Generic Length Scale parameters.
!-----------------------------------------------------------------------
!
!    gls_Gh0
!    gls_Ghcri
!    gls_Ghmin
!    gls_Kmin      Minimum value of specific turbulent kinetic energy.
!    gls_Pmin      Minimum Value of dissipation.
!    gls_cmu0      Stability coefficient (non-dimensional).
!    gls_c1        Shear production coefficient (non-dimensional).
!    gls_c2        Dissipation coefficient (non-dimensional).
!    gls_c3m       Buoyancy production coefficient (minus).
!    gls_c3p       Buoyancy production coefficient (plus).
!    gls_E2
!    gls_m         Turbulent kinetic energy exponent (non-dimensional).
!    gls_n         Turbulent length scale exponent (non-dimensional).
!    gls_p         Stability exponent (non-dimensional).
!    gls_sigk      Constant Schmidt number (non-dimensional) for
!                    turbulent kinetic energy diffusivity.
!    gls_sigp      Constant Schmidt number (non-dimensional) for
!                    turbulent generic statistical field, "psi".
!

# ifdef GLS_KEPSILON
      gls_p = 3.         ! Stability exponent
      gls_m = 1.5        ! TKE exponent
      gls_n = -1.0       ! Turbulent length scale exponent
      gls_Kmin = 7.6E-6  ! Min. specific turbulent kinetic energy
      gls_Pmin = 1.0E-12 ! Min. dissipation
      gls_cmu0 = 0.5477  ! Stability coefficient
      gls_c1 = 1.44      ! Shear production coefficient
      gls_c2 = 1.92      ! Dissipation coefficient
      gls_c3m = -0.4     ! Buoyancy production coefficient (minus)
      gls_c3p = 1.0D     ! Buoyancy production coefficient (plus)
      gls_sigk = 1.0     ! Const. Schmidt number for TKE Kv
      gls_sigp = 1.3     ! Const. Schmidt num. statistical field
# elif defined GLS_KKL
      gls_p = 0.0        ! a.k.a. Mellor-Yamada Level 2.5
      gls_m = 1.0
      gls_n = 1.0
      gls_Kmin = 5.0E-6
      gls_Pmin = 6.0E-6
      gls_cmu0 = 0.5544
      gls_c1 = 0.9
      gls_c2 = 0.52
      gls_c3m = 2.5
      gls_c3p = 1.0
      gls_sigk = 1.96
      gls_sigp = 1.96
# elif defined GLS_KOMEGA
      gls_p =-1.0
      gls_m = 0.5
      gls_n =-1.0
      gls_Kmin = 7.6E-6
      gls_Pmin = 1.0E-12
      gls_cmu0 = 0.5477
      gls_c1 = 0.555
      gls_c2 = 0.833
      gls_c3m = -0.6
      gls_c3p = 1.0
      gls_sigk = 2.0
      gls_sigp = 2.0
# elif defined GLS_GEN
      gls_p = 2.0
      gls_m = 1.0
      gls_n =-0.67
      gls_Kmin = 1.0E-8
      gls_Pmin = 1.0E-8
      gls_cmu0 = 0.5544
      gls_c1 = 1.0
      gls_c2 = 1.22
      gls_c3m = 0.1
      gls_c3p = 1.0
      gls_sigk = 0.8
      gls_sigp = 1.07
# else
      write(*,*) 'CPP configuration error: gls_mixing.F'
# endif

# ifdef CANUTO_A
      gls_Gh0 = 0.0329         ! 0.0329 GOTM, 0.0673 Burchard
      gls_Ghcri = 0.03
      gls_L1 = 0.107
      gls_L2 = 0.0032
      gls_L3 = 0.0864
      gls_L4 = 0.12
      gls_L5 = 11.9
      gls_L6 = 0.4
      gls_L7 = 0.0
      gls_L8 = 0.48
# elif defined CANUTO_B
      gls_Gh0 = 0.0444         ! 0.044 GOTM, 0.0673 Burchard
      gls_Ghcri = 0.0414
      gls_L1 = 0.127
      gls_L2 = 0.00336
      gls_L3 = 0.0906
      gls_L4 = 0.101
      gls_L5 = 11.2
      gls_L6 = 0.4
      gls_L7 = 0.0
      gls_L8 = 0.318
# else
      gls_Gh0 = 0.028
      gls_Ghcri = 0.02
# endif
      gls_Ghmin = -0.28
      gls_E2 = 1.33
!
!------------------------------------------------------------------------------
! Constants used in the various formulation of surface flux boundary
! conditions for the GLS vertical turbulence closure in terms of
! Charnok surface roughness (CHARNOK_ALPHA), roughness from wave
! amplitude (zos_hsig_alpha), wave dissipation (SZ_ALPHA), and
! Craig and Banner wave breaking (CRGBAN_CW).
!
!  CHARNOK_ALPHA   Charnok surface roughness,
!                    Zos:   (charnok_alpha * u_star**2) / g
!  ZOS_HSIG_ALPHA  Roughness from wave amplitude,
!                    Zos:   zos_hsig_alpha * Hsig
!  SZ_ALPHA        Surface flux from wave dissipation,
!                    flux:  dt * sz_alpha * Wave_dissip
!  CRGBAN_CW       Surface flux due to Craig and Banner wave breaking,
!                    flux:  dt * crgban_cw * u_star**3
!  where Wave_dissip = epsilon^b * rho0
!------------------------------------------------------------------------------
!
      Zos=0.01
      charnok_alpha = 1400.
      zos_hsig_alpha = 0.5
      sz_alpha = 0.25
      crgban_cw = 100.
!
!-----------------------------------------------------------------------
!  Mellor-Yamada (1982) Level 2.5 vertical mixing variables.
!-----------------------------------------------------------------------
!
      my_A1 = 0.92
      my_A2 = 0.74
      my_B1 = 16.6
      my_B2 = 10.1
      my_C1 = 0.08
      my_C2 = 0.7
      my_C3 = 0.2
      my_E1 = 1.8
      my_E2 = 1.33
      my_Gh0 = 0.0233
      my_Sq = 0.2
      my_dtfac = 0.05
      my_lmax = 0.53
      my_qmin = 1.0E-8

# if defined CANUTO_A || defined CANUTO_B
!
! Compute parameters for Canuto et al. (2001) stability functions.
!   (Canuto, V.M., Cheng, H.Y., and Dubovikov, M.S., 2001: Ocean
!    turbulence. Part I: One-point closure model - momentum and
!    heat vertical diffusivities, JPO, 1413-1426).
!
      gls_s0=3.0/2.0*gls_L1*gls_L5**2
      gls_s1=-gls_L4*(gls_L6+gls_L7)+2.0*gls_L4*gls_L5*
     &        (gls_L1-1.0/3.0*gls_L2-gls_L3)+3.0/2.0*
     &        gls_L1*gls_L5*gls_L8
      gls_s2=-3.0/8.0*gls_L1*(gls_L6**2-gls_L7**2)
      gls_s4=2.0*gls_L5
      gls_s5=2.0*gls_L4
      gls_s6=2.0/3.0*gls_L5*(3.0*gls_L3**2-gls_L2**2)-
     &       1.0/2.0*gls_L5*gls_L1*(3.0*gls_L3-gls_L2)+
     &       3.0/4.0*gls_L1*(gls_L6-gls_L7)
      gls_b0=3.0*gls_L5**2
      gls_b1=gls_L5*(7.0*gls_L4+3.0*gls_L8)
      gls_b2=gls_L5**2*(3.0*gls_L3**2-gls_L2**2)-
     &       3.0/4.0*(gls_L6**2-gls_L7**2)
      gls_b3=gls_L4*(4.0*gls_L4+3.0*gls_L8)
      gls_b5=1.0/4.0*(gls_L2**2-3.0*gls_L3**2)*
     &       (gls_L6**2-gls_L7**2)
      gls_b4=gls_L4*(gls_L2*gls_L6-3.0*gls_L3*gls_L7-
     &       gls_L5*(gls_L2**2-gls_L3**2))+gls_L5*gls_L8*
     &       (3.0*gls_L3**2-gls_L2**2)
# endif
!
!  Coefficients used to compute stability functions for tracer and
!  momentum.
!
      my_B1p2o3=my_B1**(2.0/3.0)
      my_B1pm1o3=1.0/(my_B1**(1.0/3.0))
      my_E1o2=0.5*my_E1
      my_Sm1=my_A1*my_A2*((my_B2-3.0*my_A2)*
     &       (1.0-6.0*my_A1/my_B1)-3.0*my_C1*(my_B2+6.0*my_A1))
      my_Sm2=9.0*my_A1*my_A2
      my_Sh1=my_A2*(1.0-6.0*my_A1/my_B1)
# ifdef KANTHA_CLAYSON
      my_Sh2=3.0*my_A2*(6.0*my_A1+my_B2*(1.0-my_C3))
      my_Sm4=18.0*my_A1*my_A1+9.0*my_A1*my_A2*(1.0-my_C2)
# else
      my_Sh2=3.0*my_A2*(6.0*my_A1+my_B2)
      my_Sm3=my_A1*(1.0-3.0*my_C1-6.0*my_A1/my_B1)
      my_Sm4=18.0*my_A1*my_A1+9.0*my_A1*my_A2
# endif
      return
      end

#else
      subroutine step3d_gls_empty
      return
      end
#endif  /* SOLVE3D & GLS_MIXING */