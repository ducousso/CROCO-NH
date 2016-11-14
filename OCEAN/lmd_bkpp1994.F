! $Id: lmd_bkpp1994.F 1526 2014-04-16 14:09:11Z marchesiello $
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
#if defined LMD_BKPP 
# define HBBL_SMOOTH
      subroutine lmd_bkpp_tile (Istr,Iend,Jstr,Jend, Kv,Kt,Ks,
     &                     Gm1,dGm1dS, Gt1,dGt1dS, Gs1,dGs1dS,
     &                        wm,ws, my_hbbl,my_kbbl,wrk, Rib)
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "forces.h"
# include "mixing.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j,k, ka,ku,ksave,
     &        imin,imax,jmin,jmax
      real    Kv(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &        Kt(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &        Ks(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &           Gm1(PRIVATE_2D_SCRATCH_ARRAY),
     &        dGm1dS(PRIVATE_2D_SCRATCH_ARRAY),
     &           Gt1(PRIVATE_2D_SCRATCH_ARRAY),
     &        dGt1dS(PRIVATE_2D_SCRATCH_ARRAY),
     &           Gs1(PRIVATE_2D_SCRATCH_ARRAY),
     &        dGs1dS(PRIVATE_2D_SCRATCH_ARRAY),
     &            wm(PRIVATE_2D_SCRATCH_ARRAY),
     &            ws(PRIVATE_2D_SCRATCH_ARRAY),
     &       my_hbbl(PRIVATE_2D_SCRATCH_ARRAY),
     &       my_kbbl(PRIVATE_2D_SCRATCH_ARRAY),
     &           wrk(PRIVATE_2D_SCRATCH_ARRAY),
     &         Rib(PRIVATE_2D_SCRATCH_ARRAY,2)
# define tind nstp
      real Vtc,    hekman,  dVsq,   Vtsq,
     &     sig,    Kv_bl,   Kt_bl,  Ks_bl,
     &     cff,    lmd_a1,  dKv_bl, dKt_bl, dKs_bl,
     &     cff_up, lmd_a2,  Gm,     Gt,     Gs,
     &     cff_dn, Ritop,   lmd_a3,
     &     zbl,    zsbl,    eps
      parameter (eps=1.E-20)
      real lmd_cs, lmd_Cv, Ric, lmd_betaT, lmd_epsilon,
     &     lmd_cekman, lmd_nu0c,
     &     Kv0, Kt0, Ks0
      parameter (
     &   lmd_cs=98.96,     ! see parameter associated with turbulent
                           ! velocity scales in lmd_wscale.F
                           !
     &   lmd_Cv=1.8,       ! Ratio of interior Brunt-Vaisala
                           ! frequency "N" to that at the
                           ! entrainment depth "he".
                           !
     &   Ric=0.3,          ! Critical bulk Richardson number.
                           !
     &   lmd_betaT=-0.2,   ! Ratio of entrainment flux to 
                           ! to surface buoyancy flux.
                           !
     &   lmd_epsilon=0.1,  ! Nondimensional extent of the
                           ! bottom layer.
                           !
     &   lmd_cekman=0.7,   ! Constant used in the computation
                           ! of Ekman depth.
                           !
     &   lmd_nu0c=0.1      ! Maximum interior convective
                           ! viscosity and  diffusivity due
                           ! to shear instability, [m^2/s];
     &                                                           )

# ifdef HBBL_SMOOTH
#  ifdef EW_PERIODIC
      imin=istr-1
      imax=iend+1
#  else
      if (WESTERN_EDGE) then
        imin=istr
      else
        imin=istr-1
      endif
      if (EASTERN_EDGE) then
        imax=iend
      else
        imax=iend+1
      endif
#  endif
#  ifdef NS_PERIODIC
      jmin=jstr-1
      jmax=jend+1
#  else
      if (SOUTHERN_EDGE) then
        jmin=jstr
      else
        jmin=jstr-1
      endif
      if (NORTHERN_EDGE) then
        jmax=jend
      else
        jmax=jend+1
      endif
#  endif
#  define I_EXT_RANGE imin,imax
#  define J_EXT_RANGE jmin,jmax
# else
#  define I_EXT_RANGE istr,iend
#  define J_EXT_RANGE jstr,jend
# endif

!
! Initialize relevant parameters
!
      Vtc=lmd_Cv*sqrt(-lmd_betaT)/( sqrt(lmd_cs*lmd_epsilon)
     &                                        *Ric*vonKar*vonKar )
!
!  Compute turbulent friction velocity [m/s] "ustar" from bottom stress
!  at RHO-points. (Bottom stress includes wave effects if activated in BBL)
!
      do j=J_EXT_RANGE
        do i=I_EXT_RANGE
          ustar(i,j)=sqrt(sqrt( (0.5*(bustr(i,j)+bustr(i+1,j)))**2
     &                         +(0.5*(bvstr(i,j)+bvstr(i,j+1)))**2))
        enddo
      enddo
!
!  Compute turbulent velocity scales (wm,ws) for zero bottom flux
!
      do j=J_EXT_RANGE
        do i=I_EXT_RANGE
          wm(i,j)=vonKar*ustar(i,j)
          ws(i,j)=wm(i,j)          
        enddo
      enddo
!
!----------------------------------------------------------------
!  Compute bulk Richardson number "Rib" and then find depth of the
!  oceanic planetary boundary layer "hbbl", such that Rib(hbbl)=Ric.
!----------------------------------------------------------------
!
!  Set indices for array "Rib", the bulk Richardson number.
!
      ka=1 
      ku=2
!
!  Intialize boundary layer depth "hbbl" and index "kbbl" of first grid
!  level above "hbbl" to maximum values.
!
      do j=J_EXT_RANGE
        do i=I_EXT_RANGE
          my_hbbl(i,j)=z_r(i,j,N)-z_w(i,j,0)
          my_kbbl(i,j)=N
          Rib(i,j,ku)=0.
        enddo
      enddo
!
!  Find bulk Richardson number Rib at every grid level until > Ric.
!
!                  [Br - B(d)] * d
!     Rib(d) = ----------------------- ;       Rib(hbbl)=Ric
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
      do k=2,N
        cff=g/rho0
        do j=J_EXT_RANGE
          do i=I_EXT_RANGE
            Ritop=-cff*(rho1(i,j,k)-rho1(i,j,1))
     &                           *(z_r(i,j,k)-z_r(i,j,1))

            dVsq=0.25*( (u(i  ,j,k,tind)-u(i  ,j,1,tind)+
     &                   u(i+1,j,k,tind)-u(i+1,j,1,tind))**2
     &                 +(v(i,j  ,k,tind)-v(i,j  ,1,tind)+
     &                   v(i,j+1,k,tind)-v(i,j+1,1,tind))**2)

            Vtsq=Vtc*(z_r(i,j,k)-z_r(i,j,1))*ws(i,j)
     &          *sqrt(max(0.,0.5*(bvf(i,j,k)+bvf(i,j,k-1))))

            Rib(i,j,ka)=Ritop/(dVsq+Vtsq+eps)
          enddo
        enddo   !--> discard ws
!
!  Linearly interpolate to find "hbbl" where Rib=Ric.
!
        do j=J_EXT_RANGE
          do i=I_EXT_RANGE

            if (my_kbbl(i,j).eq.N .and. Rib(i,j,ka).gt.Ric) then
              zbl=z_r(i,j,k)-(z_r(i,j,k)-z_r(i,j,k-1))*
     &           (Ric-Rib(i,j,ka))/(Rib(i,j,ku)-Rib(i,j,ka))
              my_hbbl(i,j)=zbl-z_w(i,j,0)
              my_kbbl(i,j)=k
            endif
          enddo
        enddo
        ksave=ka
        ka=ku
        ku=ksave
      enddo    !<-- k    !--> discard Rr,Zr,Ur,Vr,Rib
!
!  Correct "hbbl" with physically limiting case (Ekman depth)
!
      do j=J_EXT_RANGE
        do i=I_EXT_RANGE
          hekman=lmd_cekman*ustar(i,j)/max(abs(f(i,j)),1.e-6)
          my_hbbl(i,j)=min(my_hbbl(i,j),hekman)
        enddo
      enddo

!
!    Smooth HBL horizontally
!
# ifdef HBBL_SMOOTH
#  define hwrk my_hbbl
#  include "kpp_smooth.h"
#  undef hwrk
# endif
# undef I_EXT_RANGE
# undef J_EXT_RANGE
      do j=Jstr,Jend
        do i=Istr,Iend
          hbbl(i,j)=min(my_hbbl(i,j),z_w(i,j,N)-z_w(i,j,0))
# ifdef MASKING
     &                                          *rmask(i,j)
# endif
          kbbl(i,j)=N
        enddo
      enddo

!
!  Find new boundary layer index "kbbl".
!
      do k=N,1,-1
        do j=Jstr,Jend
          do i=Istr,Iend
            if (z_r(i,j,k)-z_w(i,j,0).gt.hbbl(i,j)) then
              kbbl(i,j)=k
            endif
          enddo
        enddo
      enddo
!
!-----------------------------------------------------------------
!  Compute nondimensional shape function Gx(sigma) at "hbbl"
!  (sigma=1) in terms of interior diffusivities (Gx1) and
!  its vertical derivative (dGx1dS) via interpolation.
!-----------------------------------------------------------------
!
      do j=Jstr,Jend
        do i=Istr,Iend
          zbl=z_w(i,j,0)+hbbl(i,j)
          k=kbbl(i,j)
          if (zbl.lt.z_w(i,j,k-1)) k=k-1
          cff=1./(z_w(i,j,k)-z_w(i,j,k-1))
          cff_up=cff*(zbl-z_w(i,j,k-1))
          cff_dn=cff*(z_w(i,j,k)-zbl)

          Kv_bl=cff_up*Kv(i,j,k)+cff_dn*Kv(i,j,k-1)
          dKv_bl=-cff*(Kv(i,j,k)-Kv(i,j,k-1))
          Gm1(i,j)=Kv_bl/(hbbl(i,j)*wm(i,j)+eps)
          dGm1dS(i,j)=min(0.,dKv_bl/(wm(i,j)+eps))

          Kt_bl=cff_up*Kt(i,j,k)+cff_dn*Kt(i,j,k-1)
          dKt_bl=-cff*(Kt(i,j,k)-Kt(i,j,k-1))
          Gt1(i,j)=Kt_bl/(hbbl(i,j)*ws(i,j)+eps)
          dGt1dS(i,j)=min(0.,dKt_bl/(ws(i,j)+eps))

# ifdef SALINITY
          Ks_bl=cff_up*Ks(i,j,k)+cff_dn*Ks(i,j,k-1)
          dKs_bl=-cff*(Ks(i,j,k)-Ks(i,j,k-1))
          Gs1(i,j)=Ks_bl/(hbbl(i,j)*ws(i,j)+eps)
          dGs1dS(i,j)=min(0.,dKs_bl/(ws(i,j)+eps))
# endif /* SALINITY */
        enddo
      enddo
!
!-----------------------------------------------------------------
!  Compute boundary layer mixing coefficients.
!-----------------------------------------------------------------
!
      do k=1,N-1
!
!  Compute turbulent velocity scales at vertical W-points.
!
        do j=Jstr,Jend
          do i=Istr,Iend
            if (k.lt.kbbl(i,j)) then
!
!  Set polynomial coefficients for shape function.
!
              sig=min((z_w(i,j,k)-z_w(i,j,0))/(hbbl(i,j)+eps),1.)
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
              Kv0=hbbl(i,j)*wm(i,j)*sig*(1.+sig*Gm)
              Kt0=hbbl(i,j)*ws(i,j)*sig*(1.+sig*Gt)
# ifdef SALINITY
              Ks0=hbbl(i,j)*ws(i,j)*sig*(1.+sig*Gs)
# endif

# ifdef LMD_SKPP
!
! If BBL reaches into SBL, take the sum (or max) of surface and bottom values 
!
#  ifdef LMD_SKPP2005
              zsbl=z_w(i,j,N)-hbls(i,j,3-nstp)
#  else
              zsbl=z_w(i,j,N)-hbl(i,j)
#  endif
              if (z_w(i,j,k).gt.zsbl) then
                Kv0=max(Kv0,Kv(i,j,k))
                Kt0=max(Kt0,Kt(i,j,k))
#  ifdef SALINITY
                Ks0=max(Ks0,Ks(i,j,k))
#  endif
              endif
# endif /* LMD_SKPP */

              Kv(i,j,k)=Kv0
              Kt(i,j,k)=Kt0
#  ifdef SALINITY
              Ks(i,j,k)=Ks0
#  endif

            else  !<-- k > kbbl(i,j)

# ifdef  LMD_CONVEC
!
!  Add convective adjustment
!
              if (bvf(i,j,k).lt.0.) then
#  ifdef LMD_SKPP

#   ifdef LMD_SKPP2005                
             zsbl=z_w(i,j,N)-hbls(i,j,3-nstp)
#   else
             zsbl=z_w(i,j,N)-hbl(i,j)
#   endif                
                
                if (z_w(i,j,k).lt.zsbl) then
#  endif
                  Kv(i,j,k)=Kv(i,j,k)+lmd_nu0c
                  Kt(i,j,k)=Kt(i,j,k)+lmd_nu0c
                  Ks(i,j,k)=Ks(i,j,k)+lmd_nu0c
#  ifdef LMD_SKPP
                endif
#  endif
              endif
# endif

            endif  !<-- k > kbbl(i,j)

          enddo
        enddo
# undef ws
# undef wm
      enddo ! <-- k

!
!----------------------------------------------------------------------
!  Extend Boundary values
!----------------------------------------------------------------------
!
# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        do j=jstr,jend
          hbbl(istr-1,j)=hbbl(istr,j)
        enddo
      endif
      if (EASTERN_EDGE) then
        do j=jstr,jend
          hbbl(iend+1,j)=hbbl(iend,j)
        enddo
      endif
# endif
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do i=istr,iend
          hbbl(i,jstr-1)=hbbl(i,jstr)
        enddo
      endif
      if (NORTHERN_EDGE) then
        do i=istr,iend
          hbbl(i,jend+1)=hbbl(i,jend)
        enddo
      endif
#  ifndef EW_PERIODIC
      if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
        hbbl(istr-1,jstr-1)=hbbl(istr,jstr)
      endif
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        hbbl(istr-1,jend+1)=hbbl(istr,jend)
      endif
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        hbbl(iend+1,jstr-1)=hbbl(iend,jstr)
      endif
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        hbbl(iend+1,jend+1)=hbbl(iend,jend)
      endif
#  endif
# endif

# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,hbbl(START_2D_ARRAY))
# endif

#else
      subroutine lmd_bkpp_empty
#endif /* LMD_BKPP */
      return
      end
