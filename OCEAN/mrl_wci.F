! $Id$
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
#if defined MRL_WCI
      subroutine mrl_wci (tile)
      implicit none
      integer  tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call mrl_wci_tile (istr,iend,jstr,jend,
# ifdef SOLVE3D
     &            A3d(1,1,trd),A3d(1,2,trd),A3d(1,3,trd),
# endif
     &            A2d(1,1,trd),A2d(1,2,trd),A2d(1,3,trd),A2d(1,4,trd),
     &            A2d(1,5,trd),A2d(1,6,trd),
     &            A2d(1,7,trd),A2d(1,8,trd),A2d(1,9,trd),
     &            A2d(1,10,trd),A2d(1,11,trd),
     &            A2d(1,12,trd),A2d(1,13,trd))
      return
      end

      subroutine mrl_wci_tile (istr,iend,jstr,jend, 
# ifdef SOLVE3D
     &                                                   wrk1,wrk2,wrk3,
# endif
     &           wh,fr,kw,brk,stk,Dstp,act,kD,inv_d,inv_f,frc,ebrk,erol)
!
!--------------------------------------------------------------------
!   Evaluating wave-averaged terms and Stokes drift based on MRL04
!     (McWilliams, Restrepo & Lane, 2004, JFM, 511, pp.135-178)
!    
!   inputs: R.M.S. wave height (m);
!           peak wave freqiuency (rad/s);
!           mean wave direction: wdrx & wdre (non dimensional);
!           breaking dissipation   (\epsilon_b / \rho, m3/s3);
!           roller dissipation     (\epsilon_r / \rho, m3/s3); and
!           frictional dissipation (\epsilon_d / \rho, m3/s3)
!
!   Note that if wkb_wwave.F is used, all the dissipation terms are
!   divided by wave frequency, sigma (2pi/T).
!
!   REFERENCE
!   ---------
!   Uchiyama, Y., McWilliams, J.C. and Shchepetkin, A.F. (2010): 
!      Wave-current interaction in an oceanic circulation model with a 
!      vortex force formalism: Application to the surf zone.
!      Ocean Modelling Vol. 34:1-2, pp.16-35.
!
!--------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "forces.h"
# include "grid.h"
# include "scalars.h"
# include "ocean2d.h"
# ifdef SOLVE3D
#  include "ocean3d.h"
#  ifdef WAVE_STREAMING
#   include "mixing.h"
#  endif
# endif
# ifdef ANA_BRY
#  include "boundary.h"
# endif
# ifdef WKB_WWAVE
#  include "wkb_wwave.h"
# endif
      integer istr,iend,jstr,jend,i,j,imin,jmin 
      real    cff,cff1,cff2,cff3,cff4,inv_g,khd,kh,umag,
     &   eps,wave_ramp,wramp2,kbrk,fb,fb0,fb1,fb2,inv_fbs,
     &   bconst,tauc,tauw,ka_f00,hz0,z_tide,
     &   dtinv,inv_zb,a_brk,a_kv,c1o3,c4o3,inv_k,dd,ust_ker,intfb,
     &   fbsrf,fn1,fn2,khmax,bz1,bz2,beta,abot,delta,
     &   a_frc,kfrc,Hrms,
     &             wh(PRIVATE_2D_SCRATCH_ARRAY),
     &             fr(PRIVATE_2D_SCRATCH_ARRAY),
     &             kw(PRIVATE_2D_SCRATCH_ARRAY),
     &            brk(PRIVATE_2D_SCRATCH_ARRAY),
     &            stk(PRIVATE_2D_SCRATCH_ARRAY),
     &           Dstp(PRIVATE_2D_SCRATCH_ARRAY),
     &            act(PRIVATE_2D_SCRATCH_ARRAY),
     &             kD(PRIVATE_2D_SCRATCH_ARRAY),
     &          inv_d(PRIVATE_2D_SCRATCH_ARRAY),
     &          inv_f(PRIVATE_2D_SCRATCH_ARRAY),
     &            frc(PRIVATE_2D_SCRATCH_ARRAY),
     &           ebrk(PRIVATE_2D_SCRATCH_ARRAY),
     &           erol(PRIVATE_2D_SCRATCH_ARRAY)
# ifdef SOLVE3D
      integer  k,kk 
      real     wrk1(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &         wrk2(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &         wrk3(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &         wrk4(0:N),wrk5(0:N),wrk6(0:N), kvsurf
# endif
      parameter ( eps=1.e-10, 
     &            bconst=0.03D0,               ! breaking contribution to KPP
     &            a_kv=1.2D0,                  ! breaking scale for eddy visc.
     &            a_brk=0.2D0,                ! breaking scale for body force
     &            a_frc=3.D0,                 ! friction scale for body force
     &            khmax=20.D0,                ! deep-water limit for k x dep
     &            c1o3=0.3333333333333333,    ! 1/3
     &            c4o3=1.3333333333333333     ! 4/3
     &           )
# ifdef WAVE_OFFLINE
      real roller_fraction
#  ifdef WAVE_ROLLER
      parameter ( roller_fraction=1.0)
#  else
      parameter ( roller_fraction=0.0)
#  endif
#  define wkb_roller roller_fraction
# endif
#include "compute_auxiliary_bounds.h"

! explicit wavenumber estimator

# undef  KH_SOULSBY
# define KH_HUNT

! vertical distribution function F_Kv(z) for eddy viscosity

# undef  FKV_FUNC1
# define FKV_FUNC2
# undef  FKV_FUNC3

! vertical distribution function F_B(z) for breaking accerelation

# undef  FB_FUNC0
# undef  FB_FUNC1
# undef  FB_FUNC2
# define FB_FUNC3
# undef  FB_WSCALE

! vertical distribution function F_B(z) for bottom streaming

# undef  FF_FUNC1
# undef  FF_FUNC2
# define FF_FUNC3

! initial ramping coefficients

# if defined WAVE_RAMP
#  ifdef STOKESEKMAN
      wave_ramp = tanh(3.0*dt*sec2day*float(iic-ntstart))
#  elif defined DUCK94
      wave_ramp = tanh(48.0*dt*sec2day*float(iic-ntstart))
#  elif defined RIP_TOPO_2D
      wave_ramp = tanh(192.0*dt*sec2day*float(iic-ntstart))
#  else
      wave_ramp = tanh(12.0*dt*sec2day*float(iic-ntstart))
#  endif
# else
      wave_ramp = 1.0
# endif
      if (iic.eq.0) wave_ramp=1.0
      wramp2= wave_ramp**2
!
! Evaluate 2DH wave-current interaction variables.
! ================================================
!
! 2DH Stokes velocities, breaking, roller and bottom-friction 
! dissipation terms, defined at horizontal rho-points.
!
      inv_g = 1.0/g
      do j=jstr-1,jend+1
        do i=istr-1,iend+1
# ifdef SOLVE3D
          Dstp(i,j)=h(i,j)+z_w(i,j,N)
# else
          Dstp(i,j)=h(i,j)+zeta(i,j,knew)
# endif          
          inv_d(i,j)=1.0/Dstp(i,j)

# ifdef WKB_WWAVE
!
! Wave parameters from WKB model
!
          fr(i,j) =frq(i,j,wnew)
          wh(i,j) =wave_ramp*hrm(i,j,wnew)
          kw(i,j) =max(wvn(i,j,wnew),eps)
#  ifndef WAVE_ROLLER
          cff = wramp2*wsb(i,j,wnew)
          ebrk(i,j)=cff*fr(i,j)                  ! epsilon_b (m3/s3)
#  else
          ebrk(i,j)=wramp2*wsb(i,j,wnew)*fr(i,j) ! epsilon_b (m3/s3)
          erol(i,j)=wramp2*wsr(i,j,wnew)*fr(i,j) ! epsilon_r (m3/s3)
          cff =wramp2*( wsr(i,j,wnew)+           ! for breaking term
     &           (1.-wkb_roller)*wsb(i,j,wnew) ) ! primary + roller
#  endif
          brk(i,j) =cff*kw(i,j)                  ! 2DH breaking term
          frc(i,j) =wramp2*wfc(i,j,wnew)*kw(i,j) ! 2DH friction term
#  ifdef MRL_CEW
          kD(i,j) =kw(i,j)*Dstp(i,j)
#  else
!          kD(i,j) =kw(i,j)*(h(i,j)+wkb_tide)
      !rb    kD(i,j) =kw(i,j)*h(i,j)
          kD(i,j) =kw(i,j)*Dstp(i,j)
#  endif
#  ifndef WAVE_ROLLER
          act(i,j) =wramp2*max(wac(i,j,wnew),0.)         ! wave action density
#  else
          act(i,j) =wramp2*(max(wac(i,j,wnew),0.)
     &                     +max(war(i,j,wnew),0.))
#  endif

# else    /* ifndef WKB_WWAVE */

          fr(i,j) =wfrq(i,j)
          wh(i,j) =wave_ramp*whrm(i,j)
          inv_f(i,j) =1.0/max(fr(i,j),eps)
          khd = Dstp(i,j)*(fr(i,j)**2)*inv_g
#  ifdef KH_SOULSBY
          if (khd.ge.1.0D0) then        ! explicit wavenumber estimator
           kh=khd
          else
           kh=sqrt(khd)
          endif
          do k=1,3
            cff = tanh(kh)
            kh = kh-(kh*cff-khd)/max(cff+kh*(1.0-cff**2),eps)
          enddo
#  elif defined KH_HUNT
          kh = sqrt( khd*khd + khd/(1.0 + khd*(0.6666666666
     &               +khd*(0.3555555555 + khd*(0.1608465608
     &               +khd*(0.0632098765 + khd*(0.0217540484
     &                            +khd*0.0065407983)))))) )
#  endif
          kD(i,j) =kh
          kw(i,j) =kh*inv_d(i,j)

#  if defined ANA_WWAVE || defined OW_COUPLING
#   ifdef WAVE_BREAK_TG86
          cff=3.0/16.0*sqrt(pi)*g*(0.8**3)/(0.4**4)/(2.0*pi)
          Eb(i,j)=cff/(Dstp(i,j)**5)*(Hrms**7)  ! ep_b/rho/sigma
          ebrk(i,j) =wramp2*Eb(i,j)
#   elif defined WAVE_BREAK_TG86A
          cff=3.0/16.0*sqrt(pi)*g*(0.8**3)/(0.4**2)/(2.0*pi)
          cff1=Hrms/(0.4*Dstp(i,j))
          Eb(i,j)=cff/(Dstp(i,j)**3)*(Hrms**5)*
     &                                    (1.0-(1.0+cff1**2)**(-2.5))
#   elif defined WAVE_BREAK_CT93
          cff=3.0/16.0*sqrt(pi)*g*(0.8**3)/(2.0*pi)
          cff1=Hrms/(0.4*Dstp(i,j))
          Eb(i,j)=cff/Dstp(i,j)*(Hrms**3)
     &              *( 1.0+tanh(8.0*(cff1-1.0)) )*
     &               ( 1.0-(1.0+cff1**2)**(-2.5) )
#   endif  
#  endif  /* ANA_WWAVE */
          ebrk(i,j) =wramp2*Eb(i,j)              ! epsilon_b (m3/s3)
#  ifdef WAVE_ROLLER
          erol(i,j) =wramp2*0.5762*Dstp(i,j)*    ! epsilon_r (m3/s3)
     &               whrm(i,j)*wvqb(i,j)*        ! 0.5762=0.06*sinb*g^2
     &               kw(i,j)*inv_f(i,j)          ! Nairn et al (1991)
          cff =(1.-wkb_roller)*ebrk(i,j)+erol(i,j)
#  else
          cff =ebrk(i,j)
#  endif
          frc(i,j) =wramp2*wved(i,j)*kw(i,j)
     &                           *inv_f(i,j)     ! 2DH friction term
          brk(i,j) =cff*kw(i,j)*inv_f(i,j)       ! 2DH breaking term
          act(i,j) =inv_f(i,j)*( 
     &                      0.125*g*(wh(i,j)**2) ! wave action density
#  ifdef WAVE_ROLLER
     &          +wramp2*0.03*g*Dstp(i,j)*wh(i,j)*wvqb(i,j)
#  endif
     &                                                   )

# endif   /* ifdef WKB_WWAVE */

          stk(i,j) =act(i,j)*kw(i,j)*inv_d(i,j)  ! depth-averaged Stokes
# ifdef MASKING
           kw(i,j) = kw(i,j)*rmask(i,j)
           kD(i,j) = kD(i,j)*rmask(i,j)
          act(i,j) =act(i,j)*rmask(i,j)
          stk(i,j) =stk(i,j)*rmask(i,j)
          brk(i,j) =brk(i,j)*rmask(i,j)
          frc(i,j) =frc(i,j)*rmask(i,j)
# endif   /* MASKING */
# ifdef WET_DRY
           kw(i,j) = kw(i,j)*rmask_wet(i,j)
           kD(i,j) = kD(i,j)*rmask_wet(i,j)
          act(i,j) =act(i,j)*rmask_wet(i,j)
          stk(i,j) =stk(i,j)*rmask_wet(i,j)
          brk(i,j) =brk(i,j)*rmask_wet(i,j)
          frc(i,j) =frc(i,j)*rmask_wet(i,j)
# endif   /* WET_DRY */
        enddo
      enddo              ! <-- discard inv_d (ifndef BBL_F00)

!
! sup : quasi-static sea-level response, set-up. no interaction.
! ==============================================================
!
      do j=jstrR,jendR
        do i=istrR,iendR
          sup(i,j) =-0.125*(wh(i,j)**2)*kw(i,j)
     &              /max(sinh(2.*min(kD(i,j),khmax)),eps)
#  ifdef MASKING
     &                                        *rmask(i,j)
#  endif
#  ifdef WET_DRY
     &                                        *rmask_wet(i,j)
#  endif
        enddo
      enddo
!
! Dissipation terms : copy into shared arrays
! ===========================================
!
      do j=jstrR,jendR
        do i=istrR,iendR
          wdsp(i,j)=ebrk(i,j)         ! epsilon_b (m3/s3)
# ifdef WAVE_ROLLER
          rdsp(i,j)=erol(i,j)         ! epsilon_r (m3/s3)
# endif
# ifdef WKB_WWAVE
          cff = wramp2*wfc(i,j,wnew)
# elif defined WAVE_OFFLINE 
          cff = wramp2*wved(i,j)*inv_f(i,j)
# endif
          wdrg(i,j)=cff*fr(i,j)       ! epsilon_d (m3/s3) 
          wbst(i,j)=cff*kw(i,j)       ! e_d stress (m2/s2) for KPP
# ifdef MASKING
          wdsp(i,j)=wdsp(i,j)*rmask(i,j)
          wbst(i,j)=wbst(i,j)*rmask(i,j)
          wdrg(i,j)=wdrg(i,j)*rmask(i,j)
#  ifdef WAVE_ROLLER
          rdsp(i,j)=rdsp(i,j)*rmask(i,j)
#  endif
# endif
# ifdef WET_DRY
          wdsp(i,j)=wdsp(i,j)*rmask_wet(i,j)
          wbst(i,j)=wbst(i,j)*rmask_wet(i,j)
          wdrg(i,j)=wdrg(i,j)*rmask_wet(i,j)
#  ifdef WAVE_ROLLER
          rdsp(i,j)=rdsp(i,j)*rmask_wet(i,j)
#  endif
# endif
        enddo
      enddo
# ifndef WKB_WWAVE
#  undef inv_f
# endif
!
! 2DH depth-averaged Stokes drift and breaking acceleration
! =========================================================
!
      do j=jstrR,jendR
        do i=istr,iendR
          ust2d(i,j)  =0.5*(stk(i-1,j)*wdrx(i-1,j)+stk(i,j)*wdrx(i,j))
          brk2dx(i,j) =0.5*(brk(i-1,j)*wdrx(i-1,j)+brk(i,j)*wdrx(i,j))
          frc2dx(i,j) =0.5*(frc(i-1,j)*wdrx(i-1,j)+frc(i,j)*wdrx(i,j))
# ifdef MASKING
          ust2d(i,j)  = ust2d(i,j)*umask(i,j)
          brk2dx(i,j) =brk2dx(i,j)*umask(i,j)
          frc2dx(i,j) =frc2dx(i,j)*umask(i,j)
# endif
# ifdef WET_DRY
! no mask wet for ustokes here 
          brk2dx(i,j) =brk2dx(i,j)*umask_wet(i,j)
          frc2dx(i,j) =frc2dx(i,j)*umask_wet(i,j)
# endif
        enddo
      enddo
      do j=jstr,jendR
        do i=istrR,iendR
          vst2d(i,j)  =0.5*(stk(i,j-1)*wdre(i,j-1)+stk(i,j)*wdre(i,j))
          brk2de(i,j) =0.5*(brk(i,j-1)*wdre(i,j-1)+brk(i,j)*wdre(i,j))
          frc2de(i,j) =0.5*(frc(i,j-1)*wdre(i,j-1)+frc(i,j)*wdre(i,j))
# ifdef MASKING
          vst2d(i,j)  = vst2d(i,j)*vmask(i,j)
          brk2de(i,j) =brk2de(i,j)*vmask(i,j)
          frc2de(i,j) =frc2de(i,j)*vmask(i,j)
# endif
# ifdef WET_DRY
! no mask wet for vstokes here 
          brk2de(i,j) =brk2de(i,j)*vmask_wet(i,j)
          frc2de(i,j) =frc2de(i,j)*vmask_wet(i,j)
# endif
        enddo
      enddo       ! <-- discard stk

# ifdef SOLVE3D
!
! Conservative 3D wave-current interaction variables.
! ===================================================
!
! We estimate cell-averaged 3D Stokes drift velocity (ust_r) by
! presuming that roller Stokes drift has the same z-dependency as
! the primary waves.  Analytical volume-averaging operation is
! applied to 3D Stokes drift velocity such that:
!
!   u^st = 0.5*g*A^2/frq*k/dz/(1-exp(-4*k*D))*
!         ( exp(2*k*(z_up +h-D)) -exp(-2*k*(z_up +h+D))
!          -exp(2*k*(z_low+h-D)) +exp(-2*k*(z_low+h+D)) )
!
! in which sinh, cosh, and tanh functions in u^st are expanded to
! exponential function so as not to be singular when kD gets very
! large.  Here 0.5*g*A^2/frq is equivalent to wave action density.
! The similar rule is also adapted in computation of CalP, Kapsrf,
! and Akw terms (see the code below).
!
#  define ust_r wrk1
#  define inv_dz wrk2
#  define inv_ex inv_f
!
! 3D depth-dependent Stokes drift velocities
! ==========================================
!
      do j=jstr-1,jend+1
        do i=istr-1,iend+1
          inv_ex(i,j)=1.0/max(1.0-exp(-4.*kD(i,j)),eps)
          ust_ker=act(i,j)*kw(i,j)*inv_ex(i,j)
          cff1 = exp( 2.*kw(i,j)*(z_w(i,j,0)+h(i,j)-Dstp(i,j)))
     &          -exp(-2.*kw(i,j)*(z_w(i,j,0)+h(i,j)+Dstp(i,j)))
          do k=1,N,+1              ! <-- irreversible
            inv_dz(i,j,k)=1./Hz(i,j,k)
            cff2 = exp( 2.*kw(i,j)*(z_w(i,j,k)+h(i,j)-Dstp(i,j)))
     &            -exp(-2.*kw(i,j)*(z_w(i,j,k)+h(i,j)+Dstp(i,j)))
            ust_r(i,j,k) =ust_ker*inv_dz(i,j,k)*( cff2-cff1 )
            cff1=cff2
          enddo
        enddo
      enddo                 ! <-- keep ust_r, inv_dz, &inv_ex
!
! Horizontal interpolation of ust_r to u- & v- points.
! *** Notice that mask should NOT be applied.
!
      do k=1,N
        do j=jstrR,jendR
          do i=istr,iendR
            ust(i,j,k)=0.5*( ust_r(i-1,j,k)*wdrx(i-1,j)
     &                        +ust_r(i,j,k)*wdrx(i,j) )
          enddo
        enddo
        do j=jstr,jendR
          do i=istrR,iendR
            vst(i,j,k)=0.5*( ust_r(i,j-1,k)*wdre(i,j-1)
     &                        +ust_r(i,j,k)*wdre(i,j) )
          enddo
        enddo
      enddo
!
! calP & Kapsrf: surface pressure & Bernoulli head at rho-point
! =============================================================
!
#  define kv wrk4
#  define dkvdz wrk5
#  define d2kv  wrk6
#  define actp umag
      do j=jstr,jend        ! kv (k dot v) at rho-point
        do i=istr,iend      ! and its 1st & 2nd derivertives
          do k=1,N          ! at rho-point
            kv(k) =0.5*kw(i,j)*(
     &         wdrx(i,j)* ( u(i,j,k,nstp)+u(i+1,j,k,nstp) )
     &       + wdre(i,j)* ( v(i,j,k,nstp)+v(i,j+1,k,nstp) ) )
          enddo
          kvsurf =1.5*kv(N)-0.5*kv(N-1) ! extrapolate to surface
          do k=1,N-1
            dkvdz(k) =2.0*(kv(k+1)-kv(k))/(Hz(i,j,k+1)+Hz(i,j,k))
          enddo
          dkvdz(0) = dkvdz(1)    !2.*dkvdz(1)-dkvdz(2) ! severe!
          dkvdz(N) = dkvdz(N-1)  !2.*dkvdz(N-1)-dkvdz(N-2)
          do k=1,N
            d2kv(k) =dkvdz(k)-dkvdz(k-1) ! d^2kv/dz^2 x Hz
          enddo
          cff3 = 0.0
          do k=1,N
            dd   = z_r(i,j,k)-z_w(i,j,N)
            cff3 = cff3 + d2kv(k)*(
     &             exp( 2.*kw(i,j)*(dd-Dstp(i,j)))
     &           + exp(-2.*kw(i,j)*(dd+Dstp(i,j))) )
          enddo
          cff1 =-2.0*exp(-2.*kD(i,j))*inv_ex(i,j)*dkvdz(N)
          cff2 = dkvdz(0)/max(tanh(2.*kD(i,j)),eps)
          cff3 = cff3*inv_ex(i,j)
          cff4 =-2.0*kw(i,j)*kvsurf
          actp =0.125*g*(wh(i,j)**2)/max(fr(i,j),eps)
          calP(i,j) = actp*inv_g*tanh(kD(i,j))
     &                    *( cff1+cff2+cff3+cff4 )
#  ifdef MASKING
     &                                 *rmask(i,j)
#  endif
#  ifdef WET_DRY
     &                                 *rmask_wet(i,j)
#  endif
          cff=0.0
          do k=1,N
            cff = cff +  d2kv(k)*
     &         ( exp( 2.*kw(i,j)*(z_w(i,j,N)-z_r(i,j,k)-Dstp(i,j)))
     &          -exp(-2.*kw(i,j)*(z_w(i,j,N)-z_r(i,j,k)+Dstp(i,j))) )
          enddo
#  define KAPSRF_SAFE
#  ifdef KAPSRF_SAFE
          Kapsrf(i,j) = ( 0.5*Kapsrf(i,j)  ! time-filter to suppress noises
     &                   +0.5*cff*actp*inv_ex(i,j) )
#  else
          Kapsrf(i,j) = cff*actp*inv_ex(i,j)
#  endif
#  ifdef MASKING
     &                           *rmask(i,j)
#  endif
#  ifdef WET_DRY
     &                           *rmask_wet(i,j)
#  endif
        enddo
      enddo      ! <-- discard kv, dkvdz, d2kv, & inv_ex
#  undef kv
#  undef dkvdz
#  undef d2kv
#  undef inv_ex
!
! kvf : vertical vortex force term (K term) at rho-point
! ======================================================
!
#  define kvr wrk3
#  define kvfx stk
#  define kvfy inv_f
      if (WESTERN_EDGE) then
        imin=IstrU-1
      else
        imin=IstrU
      endif
      if (SOUTHERN_EDGE) then
        jmin=JstrV-1
      else
        jmin=JstrV
      endif
      do k=1,N-1
        do j=JstrV-1,jend+1             ! du/dz & dv/dz at horz u- &
          do i=IstrU-1,iend+1           ! v- & vert w-point
            kvfx(i,j) = 2.0*(u(i,j,k+1,nstp)-u(i,j,k,nstp))
     &         /(z_r(i-1,j,k+1)+z_r(i,j,k+1)-z_r(i-1,j,k)-z_r(i,j,k))
            kvfy(i,j) = 2.0*(v(i,j,k+1,nstp)-v(i,j,k,nstp))
     &         /(z_r(i,j-1,k+1)+z_r(i,j,k+1)-z_r(i,j-1,k)-z_r(i,j,k))
          enddo
        enddo
        do j=jmin,jend
          do i=imin,iend     ! K term at horz rho- & vert w-point
            kvr(i,j,k) = 0.25*(ust_r(i,j,k)+ust_r(i,j,k+1))*
     &                   ( wdrx(i,j)*(kvfx(i,j)+kvfx(i+1,j))
     &                    +wdre(i,j)*(kvfy(i,j)+kvfy(i,j+1)) )
          enddo
        enddo
      enddo
      do j=jmin,jend                 ! bottom & top B.Cs.
        do i=imin,iend
          kvr(i,j,0)=0.D0
          kvr(i,j,N)=2.*kvr(i,j,N-1)-kvr(i,j,N-2)
        enddo
      enddo
      do k=1,N                       ! move to vert rho-point
        do j=jmin,jend               ! copy into a shared array
          do i=imin,iend
            kvf(i,j,k)=0.5*( kvr(i,j,k)+kvr(i,j,k-1) )
#  ifdef MASKING
     &                                     *rmask(i,j)
#  endif
#  ifdef WET_DRY
     &                                     *rmask_wet(i,j)
#  endif
          enddo
        enddo
      enddo           ! <-- discard ust_r, kvfx, kvfy, kvr
#  undef ust_r
#  undef kvr
#  undef kvfx
#  undef kvfy
!
! Akw: primary wave-induced additional diffusivity for tracers at w-point
! =======================================================================
!
      cff=wave_ramp/dt
      do j=jstrR,jendR
        do i=istrR,iendR
          cff2 = 0.25*wh(i,j)/max(1.-exp(-2.*kD(i,j)),eps)
          do k=0,N
            cff1 = cff2*( exp( kw(i,j)*(z_w(i,j,k)+h(i,j)-Dstp(i,j)))
     &                   -exp(-kw(i,j)*(z_w(i,j,k)+h(i,j)+Dstp(i,j))) )
            cff3 = cff1**2
            Akw(i,j,k) = cff*(cff3-E_pre(i,j,k))
#  ifdef MASKING
     &                               *rmask(i,j)
#  endif
#  ifdef WET_DRY
     &                               *rmask_wet(i,j)
#  endif
            E_pre(i,j,k) = cff3   ! store this for the next time step.
          enddo
        enddo
      enddo
!
! Non conservative wave-current interaction variables.
! ====================================================
!
! We estimate cell-averaged 3D breaking acceleration (brk_r),
! breaking-enhaced vertical eddy viscosity (Akb), and bottom
! wave streaming-induced acceleration (frc_r).  brk_r, Akb, and
! frc_r are assumed to follow an arbitrary vertical shape function,
! fb (or fkv, fwd for Akb & frc_r) with a surface/bottom scale,
! where kb^-1 = a_brk Hrms (for fb), or kb^-1 = a_kv Hrms (for fkv),
! or kb^-1 = a_frc x delta (for fwd), where Hrms is rms wave height
! and delta is turbulent wave boundary layer thickness.  a_brk,
! a_kv and a_frc are considered O(1) parameters which determine
! vertical penetration of breaking-driven momenta, additional eddy
! kinetic energy by breaking, and streaming-induced momenta near
! the bed.  fb (fkv and fwd) is designed with three (plus one 
! special case for fb) different ways:
!
!  FUNC1: gb~1-tanh(kb(\zeta-z))^4 : Warner et al (2008)
!  FUNC2: gb~1-tanh(kb(\zeta-z))^2 : modified from the above
!  FUNC3: gb~cosh(kb(z+h))         : analogous to primary wave
!  FUNC0: gb~1                     : vertically uniform (Fb only)
!
! where fb(z) = gb(z) / [\int^D gb(z') dz'] = 1, thus vertical
! integral of fb is normalized.  Then volume-averaging operation is 
! applied.  Exactly the same procedure is taken for fkv (not show 
! here), while slightly different consideration is taken for fwd 
! (see below).  In practice, vertical integrals of gb should have 
! the following forms:
!
!   1) FUNC1
!
!                      4      3e^{4kbz}+3e^{2kbz}+2
!   \int gb(z') dz' = ---*-------------------------------
!                     3kb  e^{6kbz}+3e^{4kbz}+3e^{2kbz}+1
!
!                      4  3e^{-2kbz}+3e^{-4kbz}+2e^{-6kbz}
!                   = ---*---------------------------------
!                     3kb 1+3e^{-2kbz}+3e^{-4kbz}+e^{-6kbz}
!
!   2) FUNC2
!
!                      2     1         2  e^{-2kbz}
!   \int gb(z') dz' = --*---------- = --*-----------
!                     kb e^{2kbz}+1   kb 1+e^{-2kbz}
!
!   3) FUNC3
!
!   \int gb(z') dz' = [sinh(kb*z)]/kb
!
!   and then for FUNC3
!
!           kb       1     z2
!   fb = ----------*--*\int  cosh[kb(z+h)] dz
!        sinh(kb*D) dz     z1
!
!                1                                       z2
!      = -----------------*[e^{kb(z+h-D)}-e^{-kb(z+h+D)}]
!        dz*(1-e^{-2kb*D})                               z1
!
! in which hyperbolic functions (sinh, cosh, and tanh) in fb
! are expanded to exponential function so as not to be singular
! when kD is very large.
!
! Additionally, we can introduce gb=1 (vertically uniform function)
! with FB_FUNC0 for experimental purposes.  This option can be
! chosen only for fb since it may be unappropriate to fkv and fwd.
!
! Note that the above expressions are introduced in order to
! avoid the integral being Inf or NaN when kb^-1 -> 0.
!
#  ifdef LMD_MIXING
#   define kb0 wrk4
#   define vs  cff1
#   define kvb cff2
!
! Akb: Eddy viscosity due to depth-induced wave breaking
! ======================================================
!
      do j=jstr-1,jend+1
        do i=istr-1,iend+1
          inv_k =min(max(a_kv*wh(i,j),eps),Dstp(i,j))
          kbrk  =1.0/inv_k         ! vertical scale for breaking
#   ifdef FKV_FUNC1
          fn1   =3.*(exp(-2.*kbrk*Dstp(i,j))+exp(-4.*kbrk*Dstp(i,j)))
          fn2   =exp(-6.*kbrk*Dstp(i,j))
          fb1   =c4o3*inv_k*(fn1+2.*fn2)/(1.+fn1+fn2)
          intfb =c4o3*inv_k - fb1  ! intfb = \int^D gb(z') dz'
          fb0   =1.D0/intfb        ! 1/[\int^D gb(z') dz']
#   elif defined FKV_FUNC2
          fn1   =exp(-2.*kbrk*Dstp(i,j))
          fb1   =2.*inv_k*fn1/(1.+fn1)
          intfb =inv_k - fb1
          fb0   =1.D0/intfb
#   elif defined FKV_FUNC3
          fb1   =0.0               ! fb1 = \int gb dz at z = -h
          fb0   =1.D0/(1.-exp(-2.*kbrk*Dstp(i,j)))
#   endif
#   ifndef WAVE_ROLLER
          vs    =ebrk(i,j)**c1o3   ! representative velocity scale
#   else
          vs    =((1.-wkb_roller)*ebrk(i,j)+erol(i,j))**c1o3
#   endif
          kvb   =bconst*vs*wh(i,j) ! depth-averaged Akb
          do k=1,N,+1              ! <-- irreversible
#   ifdef FKV_FUNC1
            dd  =z_w(i,j,N)-z_w(i,j,k)
            fn1 =3.*(exp(-2.*kbrk*dd)+exp(-4.*kbrk*dd))
            fn2 =exp(-6.*kbrk*dd)
            fb2 =c4o3*inv_k*(fn1+2.*fn2)/(1.+fn1+fn2)
#   elif defined FKV_FUNC2
            fn1 =exp(-2.*kbrk*(z_w(i,j,N)-z_w(i,j,k)))
            fb2 =2.*inv_k*fn1/(1.+fn1)
#   elif defined FKV_FUNC3
            fb2 =exp( kbrk*(z_w(i,j,k)+h(i,j)-Dstp(i,j)))
     &          -exp(-kbrk*(z_w(i,j,k)+h(i,j)+Dstp(i,j)))
#   endif
            fb =fb0*inv_dz(i,j,k)*(fb2-fb1)
            kb0(k) = kvb*fb*Dstp(i,j)
            fb1=fb2     ! recursive procedure
          enddo
          do k=1,N-1
            Akb(i,j,k)=0.5*(kb0(k)+kb0(k+1))
          enddo
          Akb(i,j,0)=0.D0
          Akb(i,j,N)=max(1.5*kb0(N)-0.5*kb0(N-1),0.0)
#   ifdef MASKING
          do k=0,N
            Akb(i,j,k)=Akb(i,j,k)*rmask(i,j)
          enddo
#   endif
#   ifdef WET_DRY
          do k=0,N
            Akb(i,j,k)=Akb(i,j,k)*rmask_wet(i,j)
          enddo
#   endif
        enddo
      enddo        ! <-- discard kb0
#   undef kb0
#  endif    /* LMD_MIXING */

#  ifndef WAVE_SFC_BREAK
#   define brk_r wrk3
!
! 3D breaking acceleration term defined as a body force.
! =====================================================
!
      do j=jstr-1,jend+1
        do i=istr-1,iend+1
#   ifdef FB_FUNC0
          do k=1,N                 ! vertically uniform case
            brk_r(i,j,k) = brk(i,j)*inv_d(i,j)
          enddo                    ! <-- discard inv_d
#   else
#    ifdef FB_WSCALE
          inv_k =1.0/max(2.0*kw(i,j),eps)  ! Stokes scale
#    else
          inv_k =min(max(a_brk*wh(i,j),eps),Dstp(i,j))
#    endif
          kbrk  =1.0/inv_k         ! vertical scale for breaking
#    ifdef FB_FUNC1
          fn1   =3.*(exp(-2.*kbrk*Dstp(i,j))+exp(-4.*kbrk*Dstp(i,j)))
          fn2   =exp(-6.*kbrk*Dstp(i,j))
          fb1   =c4o3*inv_k*(fn1+2.*fn2)/(1.+fn1+fn2)
          intfb =c4o3*inv_k - fb1  ! intfb = \int^D gb(z') dz'
          fb0   =1.D0/intfb        ! 1/[\int^D gb(z') dz']
#    elif defined FB_FUNC2
          fn1   =exp(-2.*kbrk*Dstp(i,j))
          fb1   =2.*inv_k*fn1/(1.+fn1)
          intfb =inv_k - fb1
          fb0   =1.D0/intfb
#    elif defined FB_FUNC3
          fb1   =0.0               ! fb1 = \int gb dz at z = -h
          fb0   =1.D0/(1.-exp(-2.*kbrk*Dstp(i,j)))
#    endif

          do k=1,N,+1              ! <-- irreversible
#    ifdef FB_FUNC1
            dd  =z_w(i,j,N)-z_w(i,j,k)
            fn1 =3.*(exp(-2.*kbrk*dd)+exp(-4.*kbrk*dd))
            fn2 =exp(-6.*kbrk*dd)
            fb2 =c4o3*inv_k*(fn1+2.*fn2)/(1.+fn1+fn2)
#    elif defined FB_FUNC2
            fn1 =exp(-2.*kbrk*(z_w(i,j,N)-z_w(i,j,k)))
            fb2 =2.*inv_k*fn1/(1.+fn1)
#    elif defined FB_FUNC3
            fb2 =exp( kbrk*(z_w(i,j,k)+h(i,j)-Dstp(i,j)))
     &          -exp(-kbrk*(z_w(i,j,k)+h(i,j)+Dstp(i,j)))
#    endif
            fb =fb0*inv_dz(i,j,k)*(fb2-fb1)
            brk_r(i,j,k) = fb*brk(i,j)
            fb1=fb2     ! recursive procedure
          enddo
#   endif       /* ifdef FB_FUNC0 */
        enddo
      enddo

      do k=1,N
        do j=jstrR,jendR
          do i=istr,iendR
            brk3dx(i,j,k)=0.5*( brk_r(i-1,j,k)*wdrx(i-1,j)
     &                           +brk_r(i,j,k)*wdrx(i,j) )
#   ifdef MASKING
     &                                         *umask(i,j)
#   endif
#   ifdef WET_DRY
     &                                         *umask_wet(i,j)
#   endif
          enddo
        enddo

        do j=jstr,jendR
          do i=istrR,iendR
            brk3de(i,j,k)=0.5*( brk_r(i,j-1,k)*wdre(i,j-1)
     &                           +brk_r(i,j,k)*wdre(i,j) )
#   ifdef MASKING
     &                                         *vmask(i,j)
#   endif
#   ifdef WET_DRY
     &                                         *vmask_wet(i,j)
#   endif
          enddo
        enddo
      enddo       ! <-- discard brk_r, but keep ust_r

#   undef brk_r
#  endif        /* !WAVE_SFC_BREAK */
#  undef inv_d
!
! 3D bottom streaming acceleration term defined as a body force.
! ==============================================================
!
!  Convert wave-induced bottom streaming as a grid-averaged body
!  force. Similarly to the breaking acceleration, we can invoke one
!  of the following three kernal functions:
!
!   FUNC1: fb~1-tanh(kf(h+z))^4 : Warner et al (2008)
!   FUNC2: fb~1-tanh(kf(h+z))^2 : modified from the above
!   FUNC3: fb~cosh(kf(zeta-z))  : analogous to primary wave
!
!  Notice that for now the z-coordinate is inverted, and thus defined
!  from the bottom.  The length scale, k_wd^-1, is referenced to the
!  turbulent wave bottom boundary layer thickness, delta, which is
!  evaluated by Eq (2.45) in Fredsoe & Deigaard (Mechanics of Caostal
!  Sediment Transport, 1992, p.25).  There is also another option to
!  identify the laminar bottom streaming by Longuet-Higgins (1953),
!  which is required to reproduce Xu & Bowen (1994) and Lentz et al.
!  (2008).
!
# define LH53_STREAMING
#  ifdef WAVE_STREAMING
#   ifdef WAVE_BODY_STREAMING
#    undef LH53_STREAMING
#   endif
#   define frc_r wrk3
#   define frc2d stk
      do j=jstr-1,jend+1
        do i=istr-1,iend+1
#   ifndef LH53_STREAMING
          abot  =wh(i,j)/max(2.D0*sinh(min(kD(i,j),khmax)),eps)
          delta =0.160455*((abot/Zob)**0.83)*Zob  ! BBL thickness
          inv_k =min(max(a_frc*delta,eps),Dstp(i,j))
          kfrc  =1.0/inv_k         ! vertical scale for wave friction
#    ifdef FF_FUNC1
          fn1   =3.*(exp(-2.*kfrc*Dstp(i,j))+exp(-4.*kfrc*Dstp(i,j)))
          fn2   =exp(-6.*kfrc*Dstp(i,j))
          fb1   =c4o3*inv_k        ! fb1: at z=-h (z_w at k=0)
          fb2   =c4o3*inv_k*(fn1+2.*fn2)/(1.+fn1+fn2)
          intfb =fb2-fb1           ! intfb = \int^D gb(z') dz'
          fb0   =1.D0/intfb        ! 1/[\int^D gb(z') dz']
#    elif defined FF_FUNC2
          fn1   =exp(-2.*kfrc*Dstp(i,j))
          fb1   =inv_k
          fb2   =2.*inv_k*fn1/(1.+fn1)
          intfb =fb2-fb1
          fb0   =1.D0/intfb
#    elif defined FF_FUNC3
          fb1   =1.0-exp(-2.*kfrc*Dstp(i,j))
          fb0   =1.D0/(exp(-2.*kfrc*Dstp(i,j))-1.0)
#    endif
          do k=1,N,+1              ! <-- irreversible
#    ifdef FF_FUNC1
            dd  =h(i,j)+z_w(i,j,k)
            fn1 =3.*(exp(-2.*kfrc*dd)+exp(-4.*kfrc*dd))
            fn2 =exp(-6.*kfrc*dd)
            fb2 =c4o3*inv_k*(fn1+2.*fn2)/(1.+fn1+fn2)
#    elif defined FF_FUNC2
            fn1 =exp(-2.*kfrc*(h(i,j)+z_w(i,j,k)))
            fb2 =2.*inv_k*fn1/(1.+fn1)
#    elif defined FF_FUNC3
            fb2 =exp( kfrc*(z_w(i,j,N)-z_w(i,j,k)-Dstp(i,j)))
     &          -exp(-kfrc*(z_w(i,j,N)-z_w(i,j,k)+Dstp(i,j)))
#    endif
            fb =fb0*inv_dz(i,j,k)*(fb2-fb1)
            frc_r(i,j,k) = fb*frc(i,j)
            fb1=fb2
          enddo
#   else  /* LH53_STREAMING */
          frc2d(i,j)=0.0
          beta=sqrt(fr(i,j)/(Akv(i,j,0)+Akv(i,j,1)))
          cff =wramp2*(wh(i,j)**2)*(fr(i,j)**2)*kw(i,j)
     &         /max(16.0*(sinh(min(kD(i,j),khmax))**2)*beta,eps)
          bz1 =beta*(h(i,j)+z_w(i,j,0))
          cff1=2.*exp(-bz1)*(bz1*sin(bz1)+cos(bz1))-1.-exp(-2.*bz1)
          cff3=1.D0 !min(1.0, pi/beta/Hz(i,j,1)) ! <-- unresolved viscous layer
          do k=1,N,+1              ! <-- irreversible
            bz2 =beta*(h(i,j)+z_w(i,j,k))
            cff2=2.*exp(-bz2)*(bz2*sin(bz2)+cos(bz2))-1.-exp(-2.*bz2)
            frc_r(i,j,k)=-cff*(cff2-cff1)*inv_dz(i,j,k)*cff3
            cff1=cff2
            cff3=1.0
            frc2d(i,j) = frc2d(i,j) + frc_r(i,j,k)*Hz(i,j,k)
          enddo
#   endif  /* !LH53_STREAMING */
        enddo
      enddo
      do j=jstrR,jendR
        do i=istr,iendR
#   ifdef LH53_STREAMING
          frc2dx(i,j)=0.5*( frc2d(i-1,j)*wdrx(i-1,j)
     &                       +frc2d(i,j)*wdrx(i,j) )
#    ifdef MASKING
     &                                   *umask(i,j)
#    endif
#    ifdef WET_DRY
     &                                   *umask_wet(i,j)
#    endif
#   else
          do k=1,N
            frc3dx(i,j,k)=0.5*( frc_r(i-1,j,k)*wdrx(i-1,j)
     &                           +frc_r(i,j,k)*wdrx(i,j) )
#    ifdef MASKING
     &                                         *umask(i,j)
#    endif
#    ifdef WET_DRY
     &                                         *umask_wet(i,j)
#    endif
          enddo
#   endif
        enddo
      enddo
      do j=jstr,jendR
        do i=istrR,iendR
#   ifdef LH53_STREAMING
          frc2de(i,j)=0.5*( frc2d(i,j-1)*wdre(i,j-1)
     &                       +frc2d(i,j)*wdre(i,j) )
#    ifdef MASKING
     &                                   *vmask(i,j)
#    endif
#    ifdef WET_DRY
     &                                   *vmask_wet(i,j)
#    endif
#   else
          do k=1,N
            frc3de(i,j,k)=0.5*( frc_r(i,j-1,k)*wdre(i,j-1)
     &                           +frc_r(i,j,k)*wdre(i,j) )
#    ifdef MASKING
     &                                         *vmask(i,j)
#    endif
#    ifdef WET_DRY
     &                                         *vmask_wet(i,j)
#    endif
          enddo
#   endif
        enddo
      enddo       ! <-- discard frc_r, frc2d
#   undef frc_r
#   undef frc2d
#  endif   /* WAVE_STREAMING */
#  undef inv_dz

# endif       /* SOLVE3D */

!
! Open boundary condition (valid only for idealized situations)
! =============================================================
!
# ifdef ANA_BRY
#  ifndef SHOREFACE
      z_tide=0.0 !wkb_tide
#  else
      z_tide=0.0
#  endif
#  if defined OBC_WEST && (defined Z_FRC_BRY || defined M2_FRC_BRY \
                                               || defined M3_FRC_BRY)
      if (WESTERN_EDGE) then
        do j=jstrR,jendR
#   ifdef Z_FRC_BRY
          zetabry_west(j)=z_tide + sup(istrR,j)
#   endif
#   ifdef M2_FRC_BRY
#    ifdef SHOREFACE
          ubarbry_west(j)=ubar(istr,j,knew)
#    else
          ubarbry_west(j)=-ust2d(istrR,j)    ! Stokes drift (xi)
#    endif
          vbarbry_west(j)=vbar(istr,j,knew) ! Stokes drift (eta)
#   endif
#   if defined SOLVE3D && defined M3_FRC_BRY
          do k=1,N
            ubry_west(j,k)=u(istr,j,k,nnew)
c            ubry_west(j,k)=-ust(istrR,j,k)
            vbry_west(j,k)=v(istr,j,k,nnew)
          enddo
#   endif
        enddo
      endif
#  endif  /* OBC_WEST */
#  if defined OBC_EAST && (defined Z_FRC_BRY || defined M2_FRC_BRY \
                                               || defined M3_FRC_BRY)
      if (EASTERN_EDGE) then
        do j=jstrR,jendR
#   ifdef Z_FRC_BRY
          zetabry_east(j)=z_tide + sup(iendR,j)
#   endif
#   ifdef M2_FRC_BRY
c         ubarbry_east(j)=ubar(iend,j,knew) ! Neumann
          ubarbry_east(j)=-ust2d(iend,j)  ! Stokes drift (xi)
          vbarbry_east(j)=vbar(iend,j,knew) !-vst2d(iendR,j)  ! Stokes drift (eta)
#   endif
#   if defined SOLVE3D && defined M3_FRC_BRY
          do k=1,N
            ubry_east(j,k)=u(iend,j,k,nnew)
c           ubry_east(j,k)=-ust(iendR,j,k)
            vbry_east(j,k)=v(iend,j,k,nnew)
          enddo
#   endif
        enddo
      endif
#  endif  /* OBC_EAST */
# endif  /* ANA_BRY */

# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r2d_tile(istr,iend,jstr,jend,sup)
      call exchange_u2d_tile(istr,iend,jstr,jend,ust2d)
      call exchange_v2d_tile(istr,iend,jstr,jend,vst2d)
      call exchange_u2d_tile(istr,iend,jstr,jend,brk2dx)
      call exchange_v2d_tile(istr,iend,jstr,jend,brk2de)
      call exchange_u2d_tile(istr,iend,jstr,jend,frc2dx)
      call exchange_v2d_tile(istr,iend,jstr,jend,frc2de)
      call exchange_r2d_tile(istr,iend,jstr,jend,wdsp)
      call exchange_r2d_tile(istr,iend,jstr,jend,wdrg)
      call exchange_r2d_tile(istr,iend,jstr,jend,wbst)
#  ifdef WAVE_ROLLER
      call exchange_r2d_tile(istr,iend,jstr,jend,rdsp)
#  endif
#  ifdef SOLVE3D
      call exchange_r2d_tile(istr,iend,jstr,jend,Kapsrf)
      call exchange_r2d_tile(istr,iend,jstr,jend,calP)
      call exchange_u3d_tile(istr,iend,jstr,jend,ust)
      call exchange_v3d_tile(istr,iend,jstr,jend,vst)
      call exchange_r3d_tile(istr,iend,jstr,jend,wst)
      call exchange_r3d_tile(istr,iend,jstr,jend,kvf) 
      call exchange_w3d_tile(istr,iend,jstr,jend,Akb)
      call exchange_w3d_tile(istr,iend,jstr,jend,Akw)
      call exchange_w3d_tile(istr,iend,jstr,jend,E_pre)
#   ifndef WAVE_SFC_BREAK
      call exchange_u3d_tile (istr,iend,jstr,jend,brk3dx)
      call exchange_v3d_tile (istr,iend,jstr,jend,brk3de)
#   endif
#   if defined WAVE_STREAMING  && defined WAVE_BODY_STREAMING
      call exchange_u3d_tile (istr,iend,jstr,jend,frc3dx)
      call exchange_v3d_tile (istr,iend,jstr,jend,frc3de)
#   endif
#  endif
# endif

      return
      end
!
!======================================================================
!
!                         WSTOKES
!
!======================================================================
!
# ifdef SOLVE3D
!
! Vertical Stokes drift velocity at rho-point
! ===========================================
! Note "wst" is not used in prognostic computation, but only 
! for output. 
!
      subroutine wstokes (tile)
      implicit none
# include "param.h"
      integer tile, trd, omp_get_thread_num
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call wstokes_tile (istr,iend,jstr,jend, A2d(1,1,trd))
      return
      end

      subroutine wstokes_tile (istr,iend,jstr,jend,Wrk)
      implicit none
# include "param.h"
      integer istr,iend,jstr,jend, i,j,k
      real, dimension(PRIVATE_1D_SCRATCH_ARRAY,0:N) :: Wrk

# include "grid.h"
# include "ocean3d.h"
# include "forces.h"
# include "scalars.h"

# include "compute_auxiliary_bounds.h"

      do j=jstr,jend
        do i=istr,iend
          Wrk(i,0)=0.
        enddo
        do k=1,N,+1        !--> recursive
          do i=istr,iend
            Wrk(i,k)=Wrk(i,k-1)-0.5*pm(i,j)*pn(i,j)*(
     &        (Hz(i+1,j,k)+Hz(i,j,k))*on_u(i+1,j)*ust(i+1,j,k)
     &           -(Hz(i,j,k)+Hz(i-1,j,k))*on_u(i,j)*ust(i,j,k)
     &       +(Hz(i,j+1,k)+Hz(i,j,k))*om_v(i,j+1)*vst(i,j+1,k)
     &           -(Hz(i,j,k)+Hz(i,j-1,k))*om_v(i,j)*vst(i,j,k) )
          enddo
        enddo
        do i=istr,iend
          wst(i,j,N)=+0.375*Wrk(i,N) +0.75*Wrk(i,N-1)
     &                                -0.125*Wrk(i,N-2)
        enddo
        do k=N-1,2,-1
          do i=istr,iend
            wst(i,j,k)=+0.5625*(Wrk(i,k  )+Wrk(i,k-1))
     &                  -0.0625*(Wrk(i,k+1)+Wrk(i,k-2))
          enddo
        enddo
        do i=istr,iend
          wst(i,j,  1)= -0.125*Wrk(i,2) +0.75*Wrk(i,1)
     &                                  +0.375*Wrk(i,0)
        enddo
      enddo

# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then                       ! Set lateral
        do k=1,N                                   ! boundary
          do j=jstr,jend                           ! conditions
            wst(istr-1,j,k)=wst(istr,j,k)
          enddo
        enddo
      endif
      if (EASTERN_EDGE) then
        do k=1,N
          do j=jstr,jend
            wst(iend+1,j,k)=wst(iend,j,k)
          enddo
        enddo
      endif
# endif
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do k=1,N
          do i=istr,iend
            wst(i,jstr-1,k)=wst(i,jstr,k)
          enddo
        enddo
      endif
      if (NORTHERN_EDGE) then
        do k=1,N
          do i=istr,iend
            wst(i,jend+1,k)=wst(i,jend,k)
          enddo
        enddo
      endif
#  ifndef EW_PERIODIC
      if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=1,N
          wst(istr-1,jstr-1,k)=wst(istr,jstr,k)
        enddo
      endif
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        do k=1,N
          wst(istr-1,jend+1,k)=wst(istr,jend,k)
        enddo
      endif
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=1,N
          wst(iend+1, jstr-1,k)=wst(iend,jstr,k)
        enddo
      endif
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        do k=1,N
          wst(iend+1,jend+1,k)=wst(iend,jend,k)
        enddo
      endif
#  endif
# endif
      return
      end
# endif  /* SOLVE3D */

#else
      subroutine mrl_wci_empty
      end
#endif

