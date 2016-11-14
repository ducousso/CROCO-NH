! $Id: lmd_bkpp2005.F 1533 2014-04-18 16:41:24Z marchesiello $
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
#if defined SOLVE3D && defined LMD_BKPP
# define HBBL_SMOOTH
# define OLD_BKPP

      subroutine lmd_bkpp_tile (istr,iend,jstr,jend, Kv,Kt,Ks,
     &                                 ws,wm, my_hbbl,wrk, Cr, 
     &             Gm1,dGm1dS, Gt1,dGt1dS, Gs1,dGs1dS, my_kbbl)
!
!=================================================================================
!  This subroutine computes vertical mixing coefficients for momentum
!  and tracers at the bottom using a K-Profile Parameterization and 
!  following the IF-less KPP scheme of A. Shchepetkin.
!
!  hbbl is defined as the first non-zero depth at which Cr(z) = 0, with
!
!          |zeta           du^2      N^2(z')                    
!  Cr(z) = |       Kern* ( ------ - -------- - C_Ek f^2 ) dz' 
!          |-z              dz'^2      Ric                      
!
!  References:
!
!    Large, W.G., J.C. McWilliams, and S.C. Doney, 1994: A Review
!    and model with a nonlocal boundary layer parameterization,
!    Reviews of Geophysics, 32,363-403.
!
!    Shchepetkin, A.F., 2005. If-less KPP. ROMS/TOMS Workshop: Adjoint
!    Modeling and Applications, La. Jolla, CA, October 24
!    <http://www.myroms.org/Workshops/ROMS2005/Nonlinear/AlexanderShchepetkin.pdf>
!
!  * April 2014, P. marchesiello: implementation in CROCO
!
!==================================================================================
!
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "forces.h"
# include "mixing.h"
# include "scalars.h"
# include "coupling.h"
# define tind nstp

      integer istr,iend,jstr,jend, i,j,k
# ifdef HBBL_SMOOTH
     &      , imin,imax,jmin,jmax
# endif 
      real Kv(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &     Kt(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &     Ks(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &     ws(PRIVATE_2D_SCRATCH_ARRAY), zscale,
     &     wm(PRIVATE_2D_SCRATCH_ARRAY),
     &my_hbbl(PRIVATE_2D_SCRATCH_ARRAY),
     &    wrk(PRIVATE_2D_SCRATCH_ARRAY),
     &     Cr(PRIVATE_1D_SCRATCH_ARRAY,0:N), z_bl, zsbl,
     &    Gm1(PRIVATE_1D_SCRATCH_ARRAY),   Av_bl,
     & dGm1dS(PRIVATE_1D_SCRATCH_ARRAY),  dAv_bl,
     &    Gt1(PRIVATE_1D_SCRATCH_ARRAY),   At_bl, a1, 
     & dGt1dS(PRIVATE_1D_SCRATCH_ARRAY),  dAt_bl, a2,
     &    Gs1(PRIVATE_1D_SCRATCH_ARRAY),   As_bl, a3,
     & dGs1dS(PRIVATE_1D_SCRATCH_ARRAY),  dAs_bl

      integer my_kbbl(PRIVATE_1D_SCRATCH_ARRAY)
      real Kern, sigma, cff,cff1, cff_up,cff_dn, lmd_nu0c, Ricr, 
     &     Ri_inv, C_Ek, eps, invrho, Kv0, Kt0, Ks0
!======================================================================
      parameter (
     &   lmd_nu0c=0.1,    ! convective adjustment for viscosity
                          !              and diffusivity [m^2/s].
                          !
     &   Ricr=0.45,       ! Critical bulk Richardson number (0.3).
                          !
     &   Ri_inv=1./Ricr,  !
                          !
     &   C_Ek=258.,       ! constant for computating stabilization term 
                          ! due to Coriolis force (Ekman depth limit).
     &   eps=1.E-20 )
!======================================================================
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
!-----------------------------------------------------------------
!  Initialize parameters
!-----------------------------------------------------------------
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
!=============================
      do j=J_EXT_RANGE                        !<-- start j-loop
!============================= 
!
!  Compute turbulent velocity scales (wm,ws) for zero bottom flux
!  and initialize boundary layer depth index "kbl" to the maximum values.
!

        do i=I_EXT_RANGE
          wm(i,j)=vonKar*ustar(i,j)
          ws(i,j)=wm(i,j)
          my_kbbl(i)=N
          Cr(i,0)=0.
        enddo
!
!-----------------------------------------------------------------
!  Compute the Integral
!-----------------------------------------------------------------
!
        do k=1,N-1,+1
          do i=I_EXT_RANGE
            zscale=z_r(i,j,k)-z_w(i,j,0)
            Kern=zscale/(zscale+Zob)

            Cr(i,k)=Cr(i,k-1) + Kern*(
     &                 0.5*( ( u(i  ,j,k+1,tind)-u(i  ,j,k,tind)
     &                        +u(i+1,j,k+1,tind)-u(i+1,j,k,tind) )**2
     &                      +( v(i,j  ,k+1,tind)-v(i,j  ,k,tind)
     &                        +v(i,j+1,k+1,tind)-v(i,j+1,k,tind) )**2
     &                      )/(Hz(i,j,k)+Hz(i,j,k+1))

     &               -0.5*(Hz(i,j,k)+Hz(i,j,k+1))*( Ri_inv*bvf(i,j,k)

     &                                            +C_Ek*f(i,j)*f(i,j)
     &                                                             ))
          enddo
        enddo
        do i=I_EXT_RANGE
          Cr(i,N)=2.*Cr(i,N-1) -Cr(i,N-2)  ! defined at rho-points
        enddo

        do k=1,N,+1
          do i=I_EXT_RANGE
            if (my_kbbl(i).eq.N .and. Cr(i,k).lt.0.) my_kbbl(i)=k
          enddo
        enddo   !<-- k, discard
!
!-----------------------------------------------------------------
! Linear interpolation to find hbl
!-----------------------------------------------------------------
!
        do i=I_EXT_RANGE
          my_hbbl(i,j)=z_w(i,j,N)-z_w(i,j,0)
          if (my_kbbl(i).lt.N) then
            k=my_kbbl(i)
            if (k.eq.1) then
              my_hbbl(i,j)=z_r(i,j,1)-z_w(i,j,0)
            else
              my_hbbl(i,j)=( z_r(i,j,k)*Cr(i,k-1)-z_r(i,j,k-1)*Cr(i,k)
     &                            )/(Cr(i,k-1)-Cr(i,k)) -z_w(i,j,0)
            endif
          endif
        enddo     !--> discard kbl

!=============================
      enddo                        !<-- terminate j-loop
!=============================  
!
!-----------------------------------------------------------------
!    HBBL Smoothing
!-----------------------------------------------------------------
!
#  ifdef HBBL_SMOOTH
#   define hwrk my_hbbl
#   include "kpp_smooth.h"
#   undef hwrk
#  endif
#  undef I_EXT_RANGE
#  undef J_EXT_RANGE

      do j=Jstr,Jend
        do i=Istr,Iend
          hbbl(i,j)=min(my_hbbl(i,j),z_w(i,j,N)-z_w(i,j,0))
#  ifdef MASKING
     &                                          *rmask(i,j)
#  endif
          kbbl(i,j)=N               
        enddo
        do k=N-1,1,-1     ! find new boundary layer index "kbbl".
          do i=Istr,Iend
            if (z_r(i,j,k)-z_w(i,j,0).gt.hbbl(i,j)) kbbl(i,j)=k
          enddo
        enddo
!
!-----------------------------------------------------------------
! Compute nondimensional shape function coefficients Gx( ) by
! matching values and vertical derivatives of interior mixing
! coefficients at hbbl (sigma=1).
!-----------------------------------------------------------------
# ifdef OLD_BKPP
        do i=Istr,Iend
          k=kbbl(i,j)
          z_bl=z_w(i,j,0)+hbbl(i,j)
          if (z_bl.lt.z_w(i,j,k-1)) k=k-1
          cff=1./(z_w(i,j,k)-z_w(i,j,k-1))
          cff_up=cff*(z_bl -z_w(i,j,k-1))
          cff_dn=cff*(z_w(i,j,k)   -z_bl)

          Av_bl=cff_up*Kv(i,j,k)+cff_dn*Kv(i,j,k-1)
          dAv_bl=cff * (Kv(i,j,k)  -   Kv(i,j,k-1))
          Gm1(i)=Av_bl/(hbbl(i,j)*wm(i,j)+eps)
          dGm1dS(i)=min(0., -dAv_bl/(wm(i,j)+eps))

          At_bl=cff_up*Kt(i,j,k)+cff_dn*Kt(i,j,k-1)
          dAt_bl=cff * (Kt(i,j,k)  -   Kt(i,j,k-1))
          Gt1(i)=At_bl/(hbbl(i,j)*ws(i,j)+eps)
          dGt1dS(i)=min(0., -dAt_bl/(ws(i,j)+eps))

#  ifdef SALINITY
          As_bl=cff_up*Ks(i,j,k)+cff_dn*Ks(i,j,k-1)
          dAs_bl=cff * (Ks(i,j,k)  -   Ks(i,j,k-1))
          Gs1(i)=As_bl/(hbbl(i,j)*ws(i,j)+eps)
          dGs1dS(i)=min(0., -dAs_bl/(ws(i,j)+eps))
#  endif
        enddo
# endif
!
!-----------------------------------------------------------------
! Compute boundary layer mixing coefficients.
!-----------------------------------------------------------------
!
        do i=Istr,Iend
          do k=1,N-1
            if (k.lt.kbbl(i,j)) then
# ifdef OLD_BKPP
              sigma=min((z_w(i,j,k)-z_w(i,j,0))/(hbbl(i,j)+eps),1.)
              a1=sigma-2.
              a2=3.-2.*sigma
              a3=sigma-1.

              Kv0 =wm(i,j)*hbbl(i,j)*( sigma*( 1.+sigma*(
     &                            a1+a2*Gm1(i)+a3*dGm1dS(i) )))
              Kt0 =ws(i,j)*hbbl(i,j)*( sigma*( 1.+sigma*(
     &                            a1+a2*Gt1(i)+a3*dGt1dS(i) )))
#  ifdef SALINITY
              Ks0 =ws(i,j)*hbbl(i,j)*( sigma*( 1.+sigma*(
     &                            a1+a2*Gs1(i)+a3*dGs1dS(i) ))) 
#  endif
# else
              sigma=(z_w(i,j,k)-z_w(i,j,0))/(hbbl(i,j)+eps)
              if (sigma.lt.1.) then
                cff=sigma*(1.-sigma)**2
              else
                cff=0.
              endif
              Kv0=cff*wm(i,j)*hbbl(i,j)
              Kt0=cff*ws(i,j)*hbbl(i,j)
#  ifdef SALINITY
              Ks0=cff*ws(i,j)*hbbl(i,j)
#  endif
# endif

# ifdef LMD_SKPP
!
!-----------------------------------------------------------------
! If BBL reaches into SBL, take max of surface and bottom values. 
! If wave breaking exists, its contribution will be further added 
! in lmd_vmix.F.
!-----------------------------------------------------------------
!
#  ifdef LMD_SKPP2005
              zsbl=z_w(i,j,N)-hbls(i,j,3-nstp)
#  else
              zsbl=z_w(i,j,N)-hbl(i,j)
#  endif
              if (z_w(i,j,k).gt.zsbl) then
                Kv0=max(Kv(i,j,k),Kv0)
                Kt0=max(Kt(i,j,k),Kt0)
#  ifdef SALINITY
                Ks0=max(Ks(i,j,k),Ks0)
#  endif
              endif
# endif   /* LMD_KPP */
              Kv(i,j,k)=Kv0
              Kt(i,j,k)=Kt0
#  ifdef SALINITY
              Ks(i,j,k)=Ks0
#  endif

# ifdef LMD_CONVEC
!
!-----------------------------------------------------------------
!  Add convective adjustment
!-----------------------------------------------------------------
!
            else  !<-- k > kbbl(i,j)

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
          enddo    !<-- k
        enddo      !<-- i
      enddo        !<-- j
!
!----------------------------------------------------------------------
!  Extend boundary values
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
