! $Id: lmd_skpp2005.F 1526 2014-04-16 14:09:11Z marchesiello $
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

      subroutine lmd_skpp_tile (Istr,Iend,Jstr,Jend, Kv,Kt,Ks,
     &                                       my_hbl, Bo,Bosol,
     &                     Gm1,dGm1dS, Gt1,dGt1dS, Gs1,dGs1dS,
     &                          Bfsfc_bl,Cr,FC,wrk1,wrk2,wrk3,
     &                                       swr_frac, my_kbl)
!
!=================================================================================
!  This subroutine computes vertical mixing coefficients for momentum
!  and tracers at the surface using a K-Profile Parameterization with a 
!  non-local transport term defined throughout the surface boundary layer.
!
!  hbl is defined as the first non-zero depth at which Cr(z) = 0, with
!
!          |zeta           du^2      N^2(z')                      Vt^2
!  Cr(z) = |       Kern* ( ------ - -------- - C_Ek f^2 ) dz' + ---------
!          |-z              dz'^2      Ric                       z - zeta
!
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
!  * Jan 2012, F. Lemarié: implements KPP2005 in CROCO
!  * Feb 2012, P. Marchesiello: Adds bottom condition of K profile in case
!              hbl reaches the bottom
!
!==================================================================================
!
# define  HBL_SMOOTH
# define  LIMIT_UNSTABLE_ONLY

      implicit none
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "forces.h"
# include "mixing.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j,k, khbl
      integer imin,imax,jmin,jmax
      integer my_kbl(PRIVATE_1D_SCRATCH_ARRAY     )
      real    Kv    (PRIVATE_2D_SCRATCH_ARRAY,0:N ),
     &        Kt    (PRIVATE_2D_SCRATCH_ARRAY,0:N ),
     &        Ks    (PRIVATE_2D_SCRATCH_ARRAY,0:N ),
     &      swr_frac(PRIVATE_2D_SCRATCH_ARRAY,0:N ),
     &        my_hbl(PRIVATE_2D_SCRATCH_ARRAY     ),
     &        Bo    (PRIVATE_2D_SCRATCH_ARRAY     ),
     &        Bosol (PRIVATE_2D_SCRATCH_ARRAY     ),
     &           Gm1(PRIVATE_1D_SCRATCH_ARRAY     ),
     &        dGm1dS(PRIVATE_1D_SCRATCH_ARRAY     ),
     &           Gt1(PRIVATE_1D_SCRATCH_ARRAY     ),
     &        dGt1dS(PRIVATE_1D_SCRATCH_ARRAY     ),
     &           Gs1(PRIVATE_1D_SCRATCH_ARRAY     ),
     &        dGs1dS(PRIVATE_1D_SCRATCH_ARRAY     ),
     &      Bfsfc_bl(PRIVATE_1D_SCRATCH_ARRAY     ),
     &        Cr    (PRIVATE_1D_SCRATCH_ARRAY,0:N ),
     &        FC    (PRIVATE_1D_SCRATCH_ARRAY,0:N ),
     &          wrk1(PRIVATE_1D_SCRATCH_ARRAY,0:N ),
     &          wrk2(PRIVATE_1D_SCRATCH_ARRAY,0:N ),
     &          wrk3(PRIVATE_2D_SCRATCH_ARRAY     )

      real eps
      parameter (eps=1.E-20)

      real ustar3,Bfsfc,zscale,zetahat, ws,wm,z_bl
      real Av_bl,dAv_bl, f1,At_bl,dAt_bl,As_bl,dAs_bl
      real Kern, Vtc, Vtsq,  sigma, cff,cff1, cff_up,cff_dn
# define tind nstp
      real zeta_m, a_m, c_m, zeta_s, a_s,c_s,r2,r3,r4
      real a1,a2,a3
      real nu0c, Cv, Ric, Ri_inv, 
     &     betaT,epssfc,Cstar,Cg,C_Ek
      real ustarb
!======================================================================
      parameter (
     &   Cv=1.8,       ! Ratio of interior Brunt-Vaisala
                       !     frequency "N" to that at the entrainment depth "he".
     &   Ric=0.3,      ! Critical bulk Richardson number
     &   Ri_inv=1./Ric, 
     &   betaT=-0.2,   ! Ratio of entrainment flux to 
                       !       to surface buoyancy flux.
     &   epssfc=0.1,   ! Nondimensional extent of the
                       !                surface layer.
     &   Cstar=10.,    ! Proportionality coefficient 
                       ! parameterizing nonlocal transport.
     &   nu0c=0.1,     ! Maximum interior convective
                       ! viscosity and  diffusivity due
                       ! to shear instability, [m^2/s];     
     &   C_Ek=258.,    ! constant for computating stabilization term 
                       ! due to Coriolis force (Ekman depth limit).
     &   zeta_m=-0.2,  ! Maximum stability parameters "zeta"
     &   a_m=1.257,    ! value of the 1/3 power law regime of
     &   c_m=8.360,    ! flux profile for momentum and tracers
     &   zeta_s=-1.0,  ! and coefficients of flux profile for
     &   a_s=-28.86,   ! momentum and tracers in their 1/3-power law regime;
     &   c_s=98.96,
     &   r2=0.5, r3=1./3., r4=0.25
     &                                                           )
!======================================================================
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
!======================================================================
!  Initialize parameters
!
      Cg=Cstar*vonKar*(c_s*vonKar*epssfc)**r3      
      Vtc= Cv * sqrt(-betaT/(c_s*epssfc)) / (Ric*vonKar**2)

!  Compute thermal expansion coefficient "alpha" [kg/m^3/decC] and
!  saline contraction coefficient "beta" [kg/m^3/PSU] at the surface.

# define alpha Bosol
# define  beta Bo
      call alfabeta_tile (Istr,Iend,Jstr,Jend, alpha,beta)

!  Compute surface turbulent buoyancy forcing "Bo" [m^2/s^3].
!  Remove incoming solar shortwave radiation because this
!  contribution is included in  surface radiative buoyancy
!  forcing "Bosol". Compute turbulent friction velocity [m/s] "ustar"
!
      do j=J_EXT_RANGE
        do i=I_EXT_RANGE
          Bo(i,j)=g*( alpha(i,j)*(stflx(i,j,itemp)-srflx(i,j))
# ifdef SALINITY
     &                              -beta(i,j)*stflx(i,j,isalt)
# endif
     &                                                        )
          Bosol(i,j)=g*alpha(i,j)*srflx(i,j)

          ustar(i,j)=sqrt(sqrt( (0.5*(sustr(i,j)+sustr(i+1,j)))**2
     &                         +(0.5*(svstr(i,j)+svstr(i,j+1)))**2))

        enddo
      enddo 
# undef beta
# undef alpha
!========================================================================
! Compute fraction of the solar shortwave flux "swr_frac"
! penetrating to grid level depth (at vertical w-points).
!
!?? Because swr_frac is also computed in step3d_t.F this could be defined 
!?? as a 3D global variable
!
!== BEGIN ugly piece of code 

#  define wrk1 wrk3
#  define wrk2 my_hbl
      do k=0,N-1
        do j=J_EXT_RANGE
          do i=I_EXT_RANGE
            wrk1(i,j)=z_w(i,j,k)-z_w(i,j,N)
          enddo
        enddo
        call lmd_swfrac_ER_tile (Istr,Iend,Jstr,Jend,1.,wrk1,wrk2)
        do j=J_EXT_RANGE
          do i=I_EXT_RANGE
            swr_frac(i,j,k)=wrk2(i,j)
          enddo
        enddo
      enddo
#  undef wrk1
#  undef wrk2
      do j=J_EXT_RANGE
        do i=I_EXT_RANGE
          swr_frac(i,j,N)=1.
        enddo
      enddo

!== END ugly piece of code
!---------------------------------------------------------------     

!======================================================================
      do j=J_EXT_RANGE
!======================================================================                                       
        do i=I_EXT_RANGE
          my_hbl(i,j)  = hbls(i,j,tind)
          my_kbl(i)    = 0
          FC(i,N)      = 0.
          Cr(i,N)      = 0. 
          Cr(i,0)      = 0.
        enddo
#  define du wrk1
#  define dv wrk2 
        do k=1,N-1
         do i=I_EXT_RANGE
          cff=1./(Hz(i,j,k)+Hz(i,j,k+1))
          du(i,k)=cff*( u(i,j,k+1,tind)+u(i+1,j,k+1,tind)
     &                 -u(i,j,k  ,tind)-u(i+1,j,k  ,tind))
          dv(i,k)=cff*( v(i,j,k+1,tind)+v(i,j+1,k+1,tind)
     &                 -v(i,j,k  ,tind)-v(i,j+1,k  ,tind))
         enddo
        enddo
        do i=I_EXT_RANGE
          du(i,N)=du(i,N-1)
          dv(i,N)=dv(i,N-1)
          du(i,0)=du(i,  1)
          dv(i,0)=dv(i,  1)
        enddo

        do k=N,1,-1
          do i=I_EXT_RANGE
            zscale = z_w(i,j,N)-z_w(i,j,k-1)
            Kern   = zscale/(zscale+epssfc*my_hbl(i,j))
            Bfsfc  = Bo(i,j) +Bosol(i,j)*(1.-swr_frac(i,j,k-1)) 
#include "lmd_wscale_ws_only.h" 
!===================================================
!++  harmonic averaging to compute bvf at rho points
!===================================================
            cff=bvf(i,j,k)*bvf(i,j,k-1)
            if (cff.gt.0.D0) then
              cff=cff/(bvf(i,j,k)+bvf(i,j,k-1))
            else
              cff=0.D0
            endif
!=======================
!++ Compute the Integral
!=======================
            FC(i,k-1)=FC(i,k) + Kern*Hz(i,j,k)*
     &       ( 0.375*( du(i,k)**2 + du(i,k-1)**2
     &               + dv(i,k)**2 + dv(i,k-1)**2 )
     &       + 0.25 *( du(i,k-1)*du(i,k)
     &               + dv(i,k-1)*dv(i,k) )
     &       - Ri_inv*( cff + 
     &              0.25*(bvf(i,j,k)+bvf(i,j,k-1)) )
     &       - C_Ek*f(i,j)*f(i,j) )

            Vtsq=Vtc*ws*sqrt(max(0., bvf(i,j,k-1)))

            Cr(i,k-1)=FC(i,k-1) +Vtsq
            if (my_kbl(i).eq.0 .and. Cr(i,k-1).lt.0.) 
     &                          my_kbl(i)=k
          enddo
        enddo
#  undef dv
#  undef du
!===================================
!++ Linear interpolation to find hbl
!===================================
        do i=I_EXT_RANGE

          if (my_kbl(i).gt.0) then
            k=my_kbl(i)
            my_hbl(i,j)=z_w(i,j,N)-( z_w(i,j,k-1)*Cr(i,k)
     &                              -z_w(i,j,k)*Cr(i,k-1)
     &                              )/(Cr(i,k)-Cr(i,k-1))
          else
            my_hbl(i,j)=z_w(i,j,N)-z_w(i,j,0)
          endif
        enddo
!======================================================================
      enddo  !<-- terminate j-loop
!======================================================================  



!================================
!    HBL Smoothing
!================================

#  ifdef HBL_SMOOTH
#   define hwrk my_hbl
#   define wrk wrk3
#   include "kpp_smooth.h"
#   undef hwrk
#   undef wrk
#  endif
#  undef I_EXT_RANGE
#  undef J_EXT_RANGE


!====================================================================== 
      do j=Jstr,Jend
!====================================================================== 
        do i=istr,iend
          kbl(i,j)=N             !<-- initialize search
        enddo
        do k=N-1,1,-1       ! find new boundary layer index "kbl".
          do i=istr,iend
            my_hbl(i,j)=min(my_hbl(i,j),z_w(i,j,N)-z_w(i,j,0))
#  ifdef MASKING
     &                                             *rmask(i,j)
#  endif
            if (z_w(i,j,k) .gt. z_w(i,j,N)-my_hbl(i,j)) kbl(i,j)=k
          enddo
        enddo
        
        do i=istr,iend
          k=kbl(i,j)
          z_bl=z_w(i,j,N)-my_hbl(i,j)
          zscale=my_hbl(i,j)

          if (swr_frac(i,j,k-1).gt. 0.) then
            Bfsfc=Bo(i,j) +Bosol(i,j)*( 1. -swr_frac(i,j,k-1)
     &              *swr_frac(i,j,k)*(z_w(i,j,k)-z_w(i,j,k-1))
     &               /( swr_frac(i,j,k  )*(z_w(i,j,k)   -z_bl)
     &                 +swr_frac(i,j,k-1)*(z_bl -z_w(i,j,k-1))
     &                                                      ))
          else
            Bfsfc=Bo(i,j)+Bosol(i,j)
          endif

#include "lmd_wscale_wm_and_ws.h"

# ifdef LIMIT_UNSTABLE_ONLY
          f1=5.0 * max(0., Bfsfc) * vonKar/(ustar(i,j)**4+eps)
# else
          f1=0.
# endif
          cff=1./(z_w(i,j,k)-z_w(i,j,k-1))
          cff_up=cff*(z_bl -z_w(i,j,k-1))
          cff_dn=cff*(z_w(i,j,k)   -z_bl)
!
!  If the surface boundary layer extends to the bottom, assume that
!  the neutral boundary layer similarity theory holds at the bottom.
!  Kz = vonKar*ustarb*z (z is height above the bottom)
!
         if(k.eq.1) then
!            kbl(i,j)=0
            ustarb=SQRT(SQRT((0.5*(bustr(i,j)+bustr(i+1,j)))**2+
     &                       (0.5*(bvstr(i,j)+bvstr(i,j+1)))**2))

            dAv_bl=vonKar*ustarb
            Av_bl=dAv_bl*(z_bl-z_w(i,j,0))
            dAt_bl=vonKar*ustarb
            At_bl=dAt_bl*(z_bl-z_w(i,j,0))
# ifdef SALINITY
            dAs_bl=vonKar*ustarb
            As_bl=dAs_bl*(z_bl-z_w(i,j,0))
# endif /* SALINITY */
          else
            Av_bl=cff_up*Kv(i,j,k)+cff_dn*Kv(i,j,k-1)
            dAv_bl=cff * (Kv(i,j,k)  -   Kv(i,j,k-1))
            At_bl=cff_up*Kt(i,j,k)+cff_dn*Kt(i,j,k-1)
            dAt_bl=cff * (Kt(i,j,k)  -   Kt(i,j,k-1))
# ifdef SALINITY
            As_bl=cff_up*Ks(i,j,k)+cff_dn*Ks(i,j,k-1)
            dAs_bl=cff * (Ks(i,j,k)  -   Ks(i,j,k-1))
# endif
          endif
          Gm1(i)=Av_bl/(my_hbl(i,j)*wm+eps)
          dGm1dS(i)=min(0., Av_bl*f1-dAv_bl/(wm+eps))
          Gt1(i)=At_bl/(my_hbl(i,j)*ws+eps)
          dGt1dS(i)=min(0., At_bl*f1-dAt_bl/(ws+eps))
# ifdef SALINITY
          Gs1(i)=As_bl/(my_hbl(i,j)*ws+eps)
          dGs1dS(i)=min(0., As_bl*f1-dAs_bl/(ws+eps))
# endif
          Bfsfc_bl(i)=Bfsfc
        enddo !<== i-loop

!
! Compute boundary layer mixing coefficients.
!--------- -------- ----- ------ -------------
! Compute turbulent velocity scales at vertical W-points.
!
        do i=istr,iend
          
          khbl=kbl(i,j)
          
          do k=N-1,khbl,-1                   !-- in the mixed layer
            Bfsfc=Bfsfc_bl(i)
            zscale=z_w(i,j,N)-z_w(i,j,k)

#include "lmd_wscale_wm_and_ws.h"
!
! Compute vertical mixing coefficients
!
            sigma=(z_w(i,j,N)-z_w(i,j,k))/max(my_hbl(i,j),eps)

            a1=sigma-2.
            a2=3.-2.*sigma
            a3=sigma-1.

            if (sigma.lt.0.07D0) then
              cff=0.5*(sigma-0.07D0)**2/0.07D0
            else
              cff=0.D0
            endif
            
            Kv(i,j,k)=wm*my_hbl(i,j)*( cff + sigma*( 1.+sigma*(
     &               a1+a2*Gm1(i)+a3*dGm1dS(i) )))

            Kt(i,j,k)=ws*my_hbl(i,j)*( cff + sigma*( 1.+sigma*(
     &               a1+a2*Gt1(i)+a3*dGt1dS(i) )))
# ifdef SALINITY
            Ks(i,j,k)=ws*my_hbl(i,j)*( cff + sigma*( 1.+sigma*(
     &               a1+a2*Gs1(i)+a3*dGs1dS(i) ))) 
# endif
# ifdef LMD_NONLOCAL
            if (Bfsfc .lt. 0.) then
              ghats(i,j,k)=Cg * sigma*(1.-sigma)**2
            else
              ghats(i,j,k)=0.
            endif
# endif
          enddo
          
          do k=khbl-1,1,-1                    !-- below mixed layer
# ifdef LMD_NONLOCAL
            ghats(i,j,k)=0.
# endif 

# if  defined LMD_CONVEC && !defined LMD_BKPP
!
!  Add convective adjustment (done in LMD_BKPP if defined)
!

            if (bvf(i,j,k).lt.0.) then
              Kv(i,j,k)=Kv(i,j,k) + nu0c
              Kt(i,j,k)=Kt(i,j,k) + nu0c
#  ifdef SALINITY
              Ks(i,j,k)=Ks(i,j,k) + nu0c
#  endif
            endif
# endif
          enddo
        
        enddo
      enddo    !<-- j-loop

!
!--------------------------------------------------------------------
!  Copy "hbl" into its shared array and pad out ghost points
!  at lateral-sideboundaries.
!--------------------------------------------------------------------
!
      do j=jstr,jend
        do i=istr,iend
          hbls(i,j,3-nstp)=my_hbl(i,j)
        enddo
      enddo

# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        do j=jstr,jend
          hbls(istr-1,j,3-nstp)=hbls(istr,j,3-nstp)     
          kbl (istr-1,j)       =kbl(istr,j)               
        enddo
      endif
      if (EASTERN_EDGE) then
        do j=jstr,jend
          hbls(iend+1,j,3-nstp)=hbls(iend,j,3-nstp)     
          kbl(iend+1,j)=kbl(iend,j)                  
        enddo
      endif
# endif
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do i=istr,iend
          hbls(i,jstr-1,3-nstp)=hbls(i,jstr,3-nstp)   
          kbl(i,jstr-1)=kbl(i,jstr)                   
        enddo
      endif
      if (NORTHERN_EDGE) then
        do i=istr,iend
          hbls(i,jend+1,3-nstp)=hbls(i,jend,3-nstp) 
          kbl(i,jend+1)=kbl(i,jend)                              
        enddo
      endif
#  ifndef EW_PERIODIC
      if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
        hbls(istr-1,jstr-1,3-nstp)=hbls(istr,jstr,3-nstp)
        kbl(istr-1,jstr-1)=kbl(istr,jstr)        
      endif
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        hbls(istr-1,jend+1,3-nstp)=hbls(istr,jend,3-nstp)
        kbl(istr-1,jend+1)=kbl(istr,jend)        
      endif
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        hbls(iend+1,jstr-1,3-nstp)=hbls(iend,jstr,3-nstp)
        kbl(iend+1,jstr-1)=kbl(iend,jstr)        
      endif
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        hbls(iend+1,jend+1,3-nstp)=hbls(iend,jend,3-nstp)
        kbl(iend+1,jend+1)=kbl(iend,jend)        
      endif
#  endif
# endif

# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_r2d_tile (istr,iend,jstr,jend,
     &                     hbls(START_2D_ARRAY,3-nstp))
# endif
      return
      end
