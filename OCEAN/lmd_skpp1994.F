! $Id: lmd_skpp1994.F 1545 2014-06-05 13:08:22Z penven $
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

#if defined LMD_SKPP   
      subroutine lmd_skpp_tile (Istr,Iend,Jstr,Jend, Kv,Kt,Ks,
     &                                my_hbl, Bo,Bosol, Bflux,
     &                     Gm1,dGm1dS, Gt1,dGt1dS, Gs1,dGs1dS,
     &                                    wrk1,wrk2,wrk3, Rib,
     &                                                 my_kbl)
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "forces.h"
# include "mixing.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j,k, ka,ku,ksave
      integer imin,imax,jmin,jmax
      integer my_kbl(PRIVATE_2D_SCRATCH_ARRAY)
      real    Kv(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &        Kt(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &        Ks(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &        my_hbl(PRIVATE_2D_SCRATCH_ARRAY),
     &            Bo(PRIVATE_2D_SCRATCH_ARRAY),
     &         Bosol(PRIVATE_2D_SCRATCH_ARRAY),
     &         Bflux(PRIVATE_2D_SCRATCH_ARRAY),
     &           Gm1(PRIVATE_2D_SCRATCH_ARRAY),
     &        dGm1dS(PRIVATE_2D_SCRATCH_ARRAY),
     &           Gt1(PRIVATE_2D_SCRATCH_ARRAY),
     &        dGt1dS(PRIVATE_2D_SCRATCH_ARRAY),
     &           Gs1(PRIVATE_2D_SCRATCH_ARRAY),
     &        dGs1dS(PRIVATE_2D_SCRATCH_ARRAY),
     &          wrk1(PRIVATE_2D_SCRATCH_ARRAY),
     &          wrk2(PRIVATE_2D_SCRATCH_ARRAY),
     &          wrk3(PRIVATE_2D_SCRATCH_ARRAY),
     &          Rib(PRIVATE_2D_SCRATCH_ARRAY,2)

      real Vtc,    hekman,  hmonob, dVsq,   Vtsq,
     &     sl_dpth,sl_dnew, sig,    Kv_bl,  Kt_bl,  Ks_bl,
     &     cff,    lmd_a1, dKv_bl, dKt_bl, dKs_bl,
     &     cff_up, lmd_a2, Gm,     Gt,     Gs,
     &     cff_dn, Ritop,   lmd_a3
# define tind nstp
      real zbl
      real eps
      parameter (eps=1.E-20)
      real lmd_nubl, lmd_cs, lmd_Cv, Ric, lmd_betaT, lmd_epsilon,
     &     lmd_cekman, lmd_cmonob, lmd_Cstar, lmd_Cg, lmd_nu0c,
     &     ustarb 
      parameter (
     &   lmd_nubl=0.01,    ! Maximum allowed boundary layer
                           ! viscosity and diffusivity [m^2/s].
                           !                       
     &   lmd_cs=98.96,     ! see parameter associated with turbulent
                           !     velocity scales in lmd_wscale.F
                           !
     &   lmd_Cv=1.8,       ! Ratio of interior Brunt-Vaisala
                           !     frequency "N" to that at the
                           !           entrainment depth "he".
                           !
     &   Ric=0.3,          ! Critical bulk Richardson number.
                           !
!# if defined DIURNAL_SRFLUX || defined BULK_FLUX
!     &   Ric=0.15,        ! Critical bulk Richardson number. (must be decreased
!     &                    ! in case of a diurnal cycle, see McWilliams et. al, 2009, jpo)
!#else
!     &   Ric=0.45,
!#endif
     &   lmd_betaT=-0.2,   ! Ratio of entrainment flux to 
                           !       to surface buoyancy flux.
                           !
     &   lmd_epsilon=0.1,  ! Nondimensional extent of the
                           !                surface layer.
                           !
     &   lmd_cekman=0.7,   ! Constant used in the computation
                           !           of Ekman depth.
                           !
     &   lmd_cmonob=1.,    ! Constant used in the computaion
                           ! Monin-Obukhov depth.
                           !
     &   lmd_Cstar=10.,    ! Proportionality coefficient 
                           ! parameterizing nonlocal transport.
                           !
     &   lmd_nu0c=0.1      ! Maximum interior convective
                           ! viscosity and  diffusivity due
                           ! to shear instability, [m^2/s];
     &                                                           )

# undef LMD_SKPP_MONOB
# define HBL_SMOOTH

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
!
!  Initialize relevant parameters.
!
      lmd_Cg=lmd_Cstar*vonKar*(lmd_cs*vonKar*lmd_epsilon)**(1./3.)
      Vtc=lmd_Cv*sqrt(-lmd_betaT)/( sqrt(lmd_cs*lmd_epsilon)
     &                                        *Ric*vonKar*vonKar )
!
!  Compute turbulent friction velocity [m/s] "ustar" from wind stress
!  at RHO-points.
!
      do j=J_EXT_RANGE
        do i=I_EXT_RANGE
          ustar(i,j)=sqrt(sqrt( (0.5*(sustr(i,j)+sustr(i+1,j)))**2
     &                         +(0.5*(svstr(i,j)+svstr(i,j+1)))**2))
        enddo
      enddo
!
!  Compute thermal expansion coefficient "alpha" [kg/m^3/decC] and
!  saline contraction coefficient "beta" [kg/m^3/PSU] at the surface.
!
# define alpha wrk1
# define  beta wrk2
      call alfabeta_tile (Istr,Iend,Jstr,Jend, alpha,beta)
!
!  Compute surface turbulent buoyancy forcing "Bo" [m^2/s^3].
!  Remove incoming solar shortwave radiation because this
!  contribution is included in  surface radiative buoyancy
!  forcing "Bosol".
!
      do j=J_EXT_RANGE
        do i=I_EXT_RANGE
          Bo(i,j)=g*( alpha(i,j)*(stflx(i,j,itemp)-srflx(i,j))
# ifdef SALINITY
     &                              -beta(i,j)*stflx(i,j,isalt)
# endif
     &                                                        )
          Bosol(i,j)=g*alpha(i,j)*srflx(i,j)
        enddo
      enddo  !--> discard alpha,beta; keep Bo,Bosol to the very end. 
# undef beta
# undef alpha
!
!----------------------------------------------------------------
!  Compute bulk Richardson number "Rib" and then find depth of the
!  oceanic planetary boundary layer "hbl", such that Rib(hbl)=Ric.
!----------------------------------------------------------------
!
!  Set indices for array "Rib", the bulk Richardson number.
!
      ka=1
      ku=2
!
!  Intialize boundary layer depth "hbl" and index "kbl" of first grid
!  level below "hbl" to maximum (bottomed out) values.
!
        do j=J_EXT_RANGE
          do i=I_EXT_RANGE
          my_hbl(i,j)=z_w(i,j,N)-z_r(i,j,1)
          my_kbl(i,j)=1
          Rib(i,j,ka)=0.
        enddo
      enddo
!
!  Find bulk Richardson number at every grid level until > Ric.
!
      do k=N-1,1,-1
!
!  Compute fraction of the solar shortwave flux "swdk" penetrating
!  to grid level depth (at vertical RHO-points).
!  Then compute total surface buoyancy flux "Bflux" as the sum of
!  contributions from surface turbulent buoyancy forcing "Bo"
!  and radiative flux down to boundary layer depth [Bosol*(1-swdk)].
!
# define zgrid wrk1
# define swdk wrk2
        do j=J_EXT_RANGE
          do i=I_EXT_RANGE
            zgrid(i,j)=z_r(i,j,k)-z_w(i,j,N)
          enddo
        enddo
        call lmd_swfrac_ER_tile (Istr,Iend,Jstr,Jend, 1.,zgrid,swdk)
        do j=J_EXT_RANGE
          do i=I_EXT_RANGE
            Bflux(i,j)=Bo(i,j)+Bosol(i,j)*(1.-swdk(i,j))
          enddo
        enddo       !--> discard zgrid,swdk
# undef swdk
# undef zgrid
!
!  Compute nondimensional vertical coordinate "sigma".
!  Compute turbulent velocity scales (wm,ws) at "sigma".
!
# define sigma wrk1
# define wm wrk3
# define ws wrk2
        do j=J_EXT_RANGE
          do i=I_EXT_RANGE
            sl_dpth=1.
            sigma(i,j)=min(z_w(i,j,N)-z_r(i,j,k),sl_dpth)
          enddo
        enddo       !--> discard Bflux (will be recomputed later)

        call lmd_wscale_ER_tile (Istr,Iend,Jstr,Jend, Bflux,
     &                                       sigma,wm,ws)
# undef sigma
# undef wm
!                               !--> discard sigma, wm (not used)
!
!  Compute bulk Richardson number "Rib" 
!---------------------------------------
!
!                  [Br - B(d)] * d
!     Rib(d) = ----------------------- ;       Rib(hbl)=Ric
!              |Vr - V(d)|^2 + Vt(d)^2
!
!  To do so, first compute numerator of bluk Richardson number,
!  Ritop=(Br-B)*d, where Br is the near-surface reference buoyancy,
!  B is the mean buoyancy as function of d, and d is the distance
!  coordinate from the boundary.
!
!  Then compute the square of velocity shear relative to reference
!  velocities, dVsq=|Vr-V|^2, at horizontal and vertical RHO-points.
!
!  Then compute Vtsq
!
        cff=g/rho0
        do j=J_EXT_RANGE
          do i=I_EXT_RANGE
            Ritop=-cff*(rho1(i,j,N)-rho1(i,j,k))
     &                           *(z_r(i,j,N)-z_r(i,j,k))

            dVsq=0.25*( (u(i  ,j,N,tind)-u(i  ,j,k,tind)+
     &                   u(i+1,j,N,tind)-u(i+1,j,k,tind))**2
     &                 +(v(i,j  ,N,tind)-v(i,j  ,k,tind)+
     &                   v(i,j+1,N,tind)-v(i,j+1,k,tind))**2)

            Vtsq=Vtc*(z_r(i,j,N)-z_r(i,j,k))*ws(i,j)
     &           *sqrt(max(0.,0.5*(bvf(i,j,k)+bvf(i,j,k-1))))

            Rib(i,j,ku)=Ritop/(dVsq+Vtsq+eps)
          enddo
        enddo   !--> discard ws
# undef ws
!
!  Linearly interpolate to find "hbl" where Rib=Ric.
!
        do j=J_EXT_RANGE
          do i=I_EXT_RANGE
            if (my_kbl(i,j).eq.1 .and. Rib(i,j,ku).gt.Ric) then
              zbl=z_r(i,j,k+1)-(z_r(i,j,k+1)-z_r(i,j,k))*
     &              (Ric-Rib(i,j,ka))/(Rib(i,j,ku)-Rib(i,j,ka))
              my_hbl(i,j)=z_w(i,j,N)-zbl
              my_kbl(i,j)=k
            endif
          enddo
        enddo
        ksave=ka
        ka=ku
        ku=ksave
      enddo    !<-- k    !--> discard Rr,Zr,Ur,Vr,Rib
!
!  Find stability and buoyancy forcing "Bflux" at "hbl".
!
# define zgrid wrk1
# define swdk_hbl wrk2
      do j=J_EXT_RANGE
        do i=I_EXT_RANGE
          zgrid(i,j)=-my_hbl(i,j)
        enddo
      enddo
      call lmd_swfrac_ER_tile (Istr,Iend,Jstr,Jend, 1.,zgrid,swdk_hbl)
      do j=J_EXT_RANGE
        do i=I_EXT_RANGE
          Bflux(i,j)=Bo(i,j)+Bosol(i,j)*(1.-swdk_hbl(i,j))
        enddo
      enddo      !--> discard zgrid, swdk_hbl
# undef zgrid
# undef swdk_hbl
!
!  Correct "hbl" with physically limiting cases (Ekman depth
!  and Monin-Obukhov depth).
!
      do j=J_EXT_RANGE
        do i=I_EXT_RANGE
          if (Bflux(i,j).gt.0.) then
            hekman=lmd_cekman*ustar(i,j)/max(abs(f(i,j)),eps)
            my_hbl(i,j)=min(my_hbl(i,j),hekman)
# ifdef LMD_SKPP_MONOB
            hmonob=lmd_cmonob*ustar(i,j)*ustar(i,j)*ustar(i,j)
     &                                    /(vonKar*Bflux(i,j)) 
            my_hbl(i,j)=min(my_hbl(i,j),hmonob)
# endif
          endif
          !my_kbl(i,j)=1
        enddo
      enddo

#  ifdef HBL_SMOOTH
!
!    Smooth HBL horizontally
!
#   ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        do j=J_EXT_RANGE
          my_hbl(Istr-1,j)=my_hbl(Istr,j)
        enddo
      endif
      if (EASTERN_EDGE) then
        do j=J_EXT_RANGE
          my_hbl(Iend+1,j)=my_hbl(Iend,j)
        enddo
      endif
#   endif
#   ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do i=I_EXT_RANGE
          my_hbl(i,Jstr-1)=my_hbl(i,Jstr)
        enddo
      endif
      if (NORTHERN_EDGE) then
        do i=I_EXT_RANGE
          my_hbl(i,Jend+1)=my_hbl(i,Jend)
        enddo
      endif
#    ifndef EW_PERIODIC
      if (WESTERN_EDGE.and.SOUTHERN_EDGE) then
        my_hbl(Istr-1,Jstr-1)=my_hbl(Istr,Jstr)
      endif
      if (WESTERN_EDGE.and.NORTHERN_EDGE) then
        my_hbl(Istr-1,Jend+1)=my_hbl(Istr,Jend)
      endif
      if (EASTERN_EDGE.and.SOUTHERN_EDGE) then
        my_hbl(Iend+1,Jstr-1)=my_hbl(Iend,Jstr)
      endif
      if (EASTERN_EDGE.and.NORTHERN_EDGE) then
        my_hbl(Iend+1,Jend+1)=my_hbl(Iend,Jend)
      endif
#    endif
#   endif
#   undef I_EXT_RANGE
#   undef J_EXT_RANGE
      do j=Jstr,Jend+1
        do i=Istr,Iend+1
          wrk1(i,j)=0.25*(my_hbl(i,j)  +my_hbl(i-1,j)
     &                   +my_hbl(i,j-1)+my_hbl(i-1,j-1))
#   ifdef MASKING
     &                   *pmask2(i,j)
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
          hbl(i,j)=(1-cff)*my_hbl(i,j)+
     &              0.25*(wrk1(i,j)  +wrk1(i+1,j)
     &                   +wrk1(i,j+1)+wrk1(i+1,j+1))
          hbl(i,j)=min(hbl(i,j),z_w(i,j,N)-z_w(i,j,0))
#   ifdef MASKING
          hbl(i,j)=hbl(i,j)*rmask(i,j)
#   endif
        enddo
      enddo

#  else
      do j=Jstr,Jend
        do i=Istr,Iend
           hbl(i,j)=my_hbl(i,j)
#   ifdef MASKING
     &                         *rmask(i,j)
#   endif
        enddo
      enddo
#  endif /* HBL_SMOOTH */

!
!  Find new boundary layer index "kbl".
!
      do j=Jstr,Jend
        do i=Istr,Iend
            kbl(i,j)=1
        enddo
      enddo
      do k=1,N-1
        do j=Jstr,Jend
          do i=Istr,Iend
            if (z_w(i,j,N)-z_r(i,j,k).gt.hbl(i,j)) then
              kbl(i,j)=k
            endif
          enddo
        enddo
      enddo
!
!  Find stability and buoyancy forcing for final "hbl" values.
!
# define zgrid wrk1
# define swdk_hbl wrk2
      do j=Jstr,Jend
        do i=Istr,Iend
          zgrid(i,j)=-hbl(i,j)
        enddo
      enddo
      call lmd_swfrac_tile (Istr,Iend,Jstr,Jend, 1.,zgrid,swdk_hbl)
      do j=Jstr,Jend
        do i=Istr,Iend
          Bflux(i,j)=Bo(i,j)+Bosol(i,j)*(1.-swdk_hbl(i,j))
# ifdef MASKING
          Bflux(i,j)=Bflux(i,j)*rmask(i,j)
# endif
        enddo
      enddo    !--> discard zgrid, swdk_hbl
# undef swdk_hbl
# undef zgrid

# define sigma wrk1
# define wm wrk3
# define ws wrk2
!
!  Compute tubulent velocity scales (wm,ws) at "hbl".

      do j=Jstr,Jend
        do i=Istr,Iend
          sigma(i,j)=hbl(i,j)*lmd_epsilon
        enddo
      enddo
      call lmd_wscale_tile (Istr,Iend,Jstr,Jend,
     &                          Bflux,sigma,wm,ws)
#undef sigma
!
!-----------------------------------------------------------------
!  Compute nondimensional shape function Gx(sigma) at "hbl"
!  (sigma=1) in terms of interior diffusivities (Gx1) and
!  its vertical derivative (dGx1dS) via interpolation.
!-----------------------------------------------------------------
!
      do j=Jstr,Jend
        do i=Istr,Iend
          zbl=z_w(i,j,N)-hbl(i,j)
          k=kbl(i,j)
          if (zbl.gt.z_w(i,j,k)) k=k+1
          cff=1./(z_w(i,j,k)-z_w(i,j,k-1))
          cff_up=cff*(zbl-z_w(i,j,k-1))
          cff_dn=cff*(z_w(i,j,k)-zbl)

          if(k.eq.1) then
!
!  If the surface boundary layer extends to the bottom, assume that
!  the neutral boundary layer similarity theory holds at the bottom.
!  Kz = vonKar*ustarb*z (z is height above the bottom)
!
            kbl(i,j)=0
            ustarb=SQRT(SQRT((0.5*(bustr(i,j)+bustr(i+1,j)))**2+
     &                       (0.5*(bvstr(i,j)+bvstr(i,j+1)))**2))

            dKv_bl=vonKar*ustarb
            Kv_bl=dKv_bl*(zbl-z_w(i,j,0))
            dKt_bl=vonKar*ustarb
            Kt_bl=dKt_bl*(zbl-z_w(i,j,0))
# ifdef SALINITY
            dKs_bl=vonKar*ustarb
            Ks_bl=dKs_bl*(zbl-z_w(i,j,0))
# endif /* SALINITY */
          else
            Kv_bl=cff_up*Kv(i,j,k)+cff_dn*Kv(i,j,k-1)
            dKv_bl=cff*(Kv(i,j,k)-Kv(i,j,k-1))

            Kt_bl=cff_up*Kt(i,j,k)+cff_dn*Kt(i,j,k-1)
            dKt_bl=cff*(Kt(i,j,k)-Kt(i,j,k-1))
# ifdef SALINITY
            Ks_bl=cff_up*Ks(i,j,k)+cff_dn*Ks(i,j,k-1)
            dKs_bl=cff*(Ks(i,j,k)-Ks(i,j,k-1))
# endif /* SALINITY */
          endif

          Gm1(i,j)=Kv_bl/(hbl(i,j)*wm(i,j)+eps)
          dGm1dS(i,j)=min(0.,-dKv_bl/(wm(i,j)+eps))

          Gt1(i,j)=Kt_bl/(hbl(i,j)*ws(i,j)+eps)
          dGt1dS(i,j)=min(0.,-dKt_bl/(ws(i,j)+eps))

# ifdef SALINITY
          Gs1(i,j)=Ks_bl/(hbl(i,j)*ws(i,j)+eps)
          dGs1dS(i,j)=min(0.,-dKs_bl/(ws(i,j)+eps))
# endif /* SALINITY */
        enddo
      enddo
#undef wm
#undef ws
!
!-----------------------------------------------------------------
!  Compute boundary layer mixing coefficients.
!-----------------------------------------------------------------
!
!  Compute turbulent velocity scales at vertical W-points.
!
#define sigma wrk1
#define wm wrk3
#define ws wrk2
      do k=1,N-1
       do j=Jstr,Jend
         do i=Istr,Iend
            sl_dnew=hbl(i,j)*lmd_epsilon
            sigma(i,j)=min(z_w(i,j,N)-z_w(i,j,k),sl_dnew)
          enddo
        enddo
        call lmd_wscale_tile (Istr,Iend,Jstr,Jend,
     &                             Bflux,sigma,wm,ws)
!
        do j=Jstr,Jend
          do i=Istr,Iend
            if (k.gt.kbl(i,j)) then
!
!  Set polynomial coefficients for shape function.
!
              sig=(z_w(i,j,N)-z_w(i,j,k))/(hbl(i,j)+eps)
# ifdef MASKING
              sig=sig*rmask(i,j)
# endif
              lmd_a1=sig-2.
              lmd_a2=3.-2.*sig
              lmd_a3=sig-1.
!
!  Compute nondimensional shape functions.
!
              Gm=lmd_a1+lmd_a2*Gm1(i,j)+lmd_a3*dGm1dS(i,j)
              Gt=lmd_a1+lmd_a2*Gt1(i,j)+lmd_a3*dGt1dS(i,j)
# ifdef SALINITY
              Gs=lmd_a1+lmd_a2*Gs1(i,j)+lmd_a3*dGs1dS(i,j)
# endif
!
!  Compute boundary layer mixing coefficients, combine them
!  with interior mixing coefficients.
!
              Kv(i,j,k)=hbl(i,j)*wm(i,j)*sig*(1.+sig*Gm)
              Kt(i,j,k)=hbl(i,j)*ws(i,j)*sig*(1.+sig*Gt)
# ifdef SALINITY
              Ks(i,j,k)=hbl(i,j)*ws(i,j)*sig*(1.+sig*Gs)
# endif
# ifdef LMD_NONLOCAL
!
!  Compute boundary layer nonlocal transport [m/s^2]
!
              if (Bflux(i,j).le.0.) then
                ghats(i,j,k)=lmd_Cg * sig*(1.-sig)**2
              else
                ghats(i,j,k)=0.
              endif
# endif

            else                !<-- k < kbl(i,j)

# ifdef LMD_NONLOCAL
              ghats(i,j,k)=0.
# endif
# if defined LMD_CONVEC && !defined LMD_BKPP
!
!  Add convective adjustment
!
             if (bvf(i,j,k).lt.0.) then
               Kv(i,j,k)=Kv(i,j,k)+lmd_nu0c
               Kt(i,j,k)=Kt(i,j,k)+lmd_nu0c
               Ks(i,j,k)=Ks(i,j,k)+lmd_nu0c
             endif
# endif
           endif                !<-- k < kbl(i,j)
          enddo
        enddo
#undef ws
#undef wm
#undef sigma
      enddo                     !<-- k loop

      do j=Jstr,Jend
        do i=Istr,Iend
          kbl(i,j)=max(1,kbl(i,j))
        enddo
      enddo

#else
      subroutine lmd_skpp_empty
#endif /* LMD_SKPP */
      return
      end
