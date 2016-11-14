#include "cppdefs.h"

#ifdef WKB_WWAVE
# define WKB_AB2
# undef  WKB_AB3
# undef  WKB_AB2CN2
# ifdef WKB_TIME_FILTER
#  define wmod wcew
# else
#  define wmod wint
# endif

# define ADD_DIFF

      subroutine wkb_wwave (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call wkb_wwave_tile (istr,iend,jstr,jend, 
     &                     A2d(1,1,trd), A2d(1,2,trd), A2d(1,3,trd), 
     &                     A2d(1,4,trd), A2d(1,5,trd), A2d(1,6,trd), 
     &                     A2d(1,7,trd), A2d(1,8,trd))
      return
      end
 
      subroutine wkb_wwave_tile (istr,iend,jstr,jend, 
     &                     cgx,cge, acx,ace, Dstp, rkx,rke,rwac)
!
!======================================================================
!  WKB ray equation + action-density conservation equations for
!  primary waves and rollers are solved with 2nd-order Adams-Bashforth
!  time stepping algorithm.  Also AB3 and AB2-Crank-Nicolson predictor-
!  corrector steppings may be chosen.  For AB2-CN2 you need to modify 
!  main.F to execute this subroutine twice during one barotropic time
!  step.  The breaking dissipation term is modeled according to Thornton
!  and Guza (1983, JGR), or Church and Thornton (1993, Coastal Eng.).
!  Wave-friction is also modeled based on Rayleigh wave-height PDF as 
!  appeared in TG83. 
!
!  Ref: Uchiyama, McWilliams and Shchepetkin (2010, Ocean Modelling).
!  
!  Adapted to CROCO by R. Benshila & P. Marchesiello, 2013
!======================================================================
!
      implicit none
# include "param.h"
      integer istr,iend,jstr,jend, i, j, k, wprv, wbak, wold, 
     &        imin,imax,jmin,jmax, prc
      real, dimension(PRIVATE_2D_SCRATCH_ARRAY) :: cgx,cge,acx,ace,
     &                                             Dstp,rkx,rke,rwac
      real, dimension(3) :: wkb_bry
      real sbc, kh, kk, nw, cg, a0, ax, ax1, ax2, ay, ay1, ay2,
     &     cosw, sinw, cw, dtinv, cxu, cev, orbital, peg,
     &     inv_k, khn, cff, cff0, cff1, cff2, w1, w2, w3,
     &     dtwave, cfrc, inv_f, urms, abot, fw, c_roller
      real wamp, wh, cfrq, cdir, Btg, gamw, khd, kw, kr, ks,
     &     ho, dd, co, cgo, dsup
      real, parameter :: eps=1.e-10, 
     &                   khmax=20.D0     ! deep-water limit for k*h
#if defined MPI 
      include 'mpif.h'
# include "mpi_roms.h"
      integer ierr
#endif
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "wkb_wwave.h"
# include "forces.h"
# if defined BBL || defined BBL_S95 || defined BBL_F00 || defined BBL_QUAD2D
#  include "coupling.h"
# endif
# include "boundary.h"
# include "scalars.h"
# include "compute_auxiliary_bounds.h"
#ifdef MASKING
# define SWITCH *
#else
# define SWITCH !
#endif
!
! Sort out time-stepping parameters: For now, there are 3 choices,
! =================================  1) AB2, 2) AB3, and 3) AB2-CN2.
! The option AB3 requires shorter a time step (nominally CFL<0.5) 
! and modifying WKB_TIME = 4 in wkb_wwave.h (see below). For 3),
! you need to double wave steps in main.F (not implemented).
!
      dtwave=dtfast
      wprv=wstp
# ifdef WKB_AB2
      if (iwave.eq.1) then              ! Option 1: Adams-Bashforth2
        wbak=wstp                       ! The first wave step is solved
        w1=1.                           ! with forward Euler scheme.
        w2=0.
      else
        wbak=wstp-1
        if (wbak.lt.1) wbak=wstp
        w1=1.5
        w2=-0.5
      endif
# elif defined WKB_AB3
      if (iwave.eq.1) then              ! Option 2: Adams-Bashforth3
        wbak=wstp                       ! WKB_TIME = 4 is required in
        wold=wstp                       ! wkb_wwave.h. Slightly unstable
        w1=1.                           ! with dtfast. The first two
        w2=0.                           ! wave steps are with forward
        w3=0.                           ! Euler and AB2 schemes.
      elseif (iwave.eq.2) then
        wbak=wstp-1
        if (wbak.lt.1) wbak=wstp
        wold=wbak-1
        if (wold.lt.1) wold=wstp
        w1=1.5
        w2=-0.5
        w3=0.0
      else
        wbak=wstp-1
        if (wbak.lt.1) wbak=wstp
        wold=wbak-1
        if (wold.lt.1) wold=wstp
        w1=23.0/12.0
        w2=-16./12.
        w3=5./12.
      endif
# elif defined WKB_AB2CN2
      if (iwave.eq.1) then              ! Option 3: Predictor-corrector
        wbak=wstp                       ! stepping with AB2 and Crank-
        w1=1.                           ! Nicolson2 schemes. The first two
        w2=0.                           ! wave steps are with forward
      else                              ! Euler and AB2 schemes.
        if (mod(iwave,2).eq.0) then
          wbak=wstp-1
          if (wbak.lt.1) wbak=wstp
          w1=1.5
          w2=-0.5
        else
          wbak=wstp-1
          if (wbak.lt.1) wbak=wstp
          wprv=wbak
          w1=0.5
          w2=0.5
        endif
      endif
# endif
!
! Initializing parameters for breaking and roller models
!
# ifdef WAVE_BREAK_TG86
      sbc=3.0/16.0*sqrt(pi)*g*(wkb_btg**3)/(wkb_gam**4)/(2.0*pi)
# elif defined WAVE_BREAK_TG86A
      sbc=3.0/16.0*sqrt(pi)*g*(wkb_btg**3)/(wkb_gam**2)/(2.0*pi)
# elif defined WAVE_BREAK_CT93
      sbc=3.0/16.0*sqrt(pi)*g*(wkb_btg**3)/(2.0*pi)
# elif defined WAVE_BREAK_R93
      sbc=0.25*g*(wkb_btg**3)/(2.0*pi)
# else
      sbc=3.0/16.0*sqrt(pi)*g*(wkb_btg**3)/(2.0*pi)
# endif
# ifdef WAVE_ROLLER
      c_roller=g*wkb_rsb  ! for roller dissipation term, epsilon_r
# endif
# ifdef WAVE_FRICTION
      cfrc = 0.5/sqrt(pi)
# endif
      peg=8.0/g
!
!----------------------------------------------------------------------
!  Initialisation of WKB model variables
!----------------------------------------------------------------------
!
      if (iwave.eq.1) then
!
# define ANA_INI_WKB
# ifdef ANA_INI_WKB

#  if defined AGRIF
        if( AGRIF_Root() ) then
#  endif

#  ifdef WKB_OBC_WEST
        ho=h(1,1)+zeta(1,1,knew)         !+wkb_tide       ! offshore depth
#   ifdef MPI
        call MPI_Bcast(ho, 1, MPI_DOUBLE_PRECISION,
     &                     0, MPI_COMM_WORLD, ierr)
#   endif
#  endif
#  ifdef WKB_OBC_EAST
        ho=h(LLm,1)+zeta(LLm,1,knew)      !+wkb_tide       ! offshore depth
#   ifdef MPI
        call MPI_Bcast(ho, 1, MPI_DOUBLE_PRECISION,
     &                     NP_XI-1, MPI_COMM_WORLD, ierr)
#   endif
#  endif
#  ifdef WKB_OBC_NORTH
        ho=h(LLm,MMm)+zeta(LLm,MMm,knew)  !+wkb_tide       ! offshore depth
#   ifdef MPI
        call MPI_Bcast(ho, 1, MPI_DOUBLE_PRECISION,
     &                     NPP-1, MPI_COMM_WORLD, ierr)!faux
#   endif
#  endif
#  ifdef WKB_OBC_SOUTH
        ho=h(1,1)+zeta(1,1,knew)          !+wkb_tide       ! offshore depth
#   ifdef MPI
        call MPI_Bcast(ho, 1, MPI_DOUBLE_PRECISION,
     &                     0, MPI_COMM_WORLD, ierr)!faux
#   endif
#  endif
#  ifdef ANA_BRY_WKB 
        wamp = wkb_amp               ! wave amplitude (m)
        cfrq = 2.0*pi/wkb_prd        ! peak wave frequency (rad/s)
        cdir = wkb_ang*deg2rad       ! wave direction rad
#  else
#   ifdef WKB_OBC_WEST
         wkb_bry(1)=wacbry_west(1) 
         wkb_bry(2)=wkxbry_west(1)
         wkb_bry(3)=wkebry_west(1)
         prc = 0
#   endif
#   ifdef WKB_OBC_EAST
         wkb_bry(1)=wacbry_east(1) 
         wkb_bry(2)=wkxbry_east(1)
         wkb_bry(3)=wkebry_east(1)
         prc = NP_XI-1
#   endif
#   ifdef MPI
        call MPI_Bcast(wkb_bry, 3, MPI_DOUBLE_PRECISION,
     &                     prc, MPI_COMM_WORLD, ierr)
#   endif
        cff1=sqrt(wkb_bry(2)**2+wkb_bry(3)**2)
        inv_k =1.0/cff1
        khn =cff1*(ho+zeta(1,1,knew))
        cfrq =sqrt(g*cff1*tanh(khn))
        cosw = inv_k*wkb_bry(2)          
        sinw = inv_k*wkb_bry(3)           
        cdir=ATAN2(sinw,cosw)
        wamp=sqrt(2*wkb_bry(1)/g*cfrq)
#  endif /* ANA_BRY_WKB */
        Btg  = wkb_btg               ! B parameter
        gamw = wkb_gam               ! gamma paramemer (Hrms/h ratio)

        khd = ho*cfrq*cfrq/g
        kh  = sqrt(    khd*khd + khd/(1.0 + khd*(0.6666666666
     &                 +khd*(0.3555555555 + khd*(0.1608465608
     &                 +khd*(0.0632098765 + khd*(0.0217540484
     &                               +khd*0.0065407983)))))) )
        co=sqrt(g/kh*ho*tanh(kh))
        cgo=co*0.5*(1.+2.*kh/sinh(2.*kh))
        do j=jstrR,jendR
          do i=istrR,iendR
            Dstp(i,j)=h(i,j)+zeta(i,j,knew)
            dsup=0.0
            do k=1,4
              dd = Dstp(i,j) + dsup         !+ wkb_tide
              khd = dd*cfrq*cfrq/g
              kh = sqrt(    khd*khd + khd/(1.0 + khd*(0.6666666666
     &                      +khd*(0.3555555555 + khd*(0.1608465608
     &                      +khd*(0.0632098765 + khd*(0.0217540484
     &                                      +khd*0.0065407983)))))) )
              kw=kh/max(dd,eps)
              cw=sqrt(g/kw*tanh(kh))
              nw=0.5*(1.+2.*kh/sinh(2.*kh))  ! n=Cg/C
              ks=sqrt(cgo/(2.*nw*cw))        ! shoaling coefficient
              kr=sqrt(cos(cdir)/cos(cdir))   ! refraction coefficient
              cosw=cos(cdir)
              sinw=sin(cdir)
              cff1=gamw*dd
              cff2=2.0*wamp*ks*kr
              wh = min(cff1,cff2)
              dsup=-0.125*(wh**2)*kw/sinh(2.*kw*dd) ! wave set-up
            enddo
            do k=1,2
              hrm(i,j,k)=wh*2.0 SWITCH rmask(i,j)
              frq(i,j,k)=cfrq
              wcg(i,j,k)=nw*cw/kw
              wac(i,j,k)=0.125*g*(hrm(i,j,k)**2)/frq(i,j,k)
              wkx(i,j,k)=kw*cosw
              wke(i,j,k)=kw*sinw
              wsb(i,j,k)=sbc/(Dstp(i,j)**5)*(hrm(i,j,k)**7)
              wvn(i,j,k)=kw
#  ifdef WAVE_ROLLER
              war(i,j,k)=0.0
              wsr(i,j,k)=0.0
              wcr(i,j,k)=cw/kw
#  endif
            enddo
            wdrx(i,j)=cosw      ! cosine wave direction (xi)
            wdre(i,j)=sinw      ! sine wave direction (eta)
            sup(i,j)=dsup       ! wave setup
            wdsp(i,j)=0.0       ! sbc/(h(i,j)**5)*(hrm(i,j,k)**7)/cfrq 
                                ! S (\ep_b/rho, wave dissipation)
          enddo
        enddo

!
!  Boundary fields
!
#  ifdef ANA_BRY_WKB 
#   ifdef WKB_OBC_EAST
        if (EASTERN_EDGE) then
          do j=jstr-1,jend+1
            wacbry_east(j)=wac(iendR,j,1)
            wkxbry_east(j)=wkx(iendR,j,1)
            wkebry_east(j)=wke(iendR,j,1)
          enddo
        endif
#   endif  /* WKB_OBC_EAST */
#   ifdef WKB_OBC_WEST
        if (WESTERN_EDGE) then
          do j=jstr-1,jend+1
            wacbry_west(j)=wac(istr-1,j,1)
            wkxbry_west(j)=wkx(istr-1,j,1)
            wkebry_west(j)=wke(istr-1,j,1)
          enddo
        endif
#   endif  /* WKB_OBC_WEST */
#   ifdef WKB_OBC_SOUTH
        if (SOUTHERN_EDGE) then
          do i=istr-1,iend+1
            wacbry_south(j)=wac(i,jstr-1,1)
            wkxbry_south(j)=wkx(i,jstr-1,1)
            wkebry_south(j)=wke(i,jstr-1,1)
          enddo
        endif
#   endif  /* WKB_OBC_SOUTH */
#   ifdef WKB_OBC_NORTH
        if (NORTHERN_EDGE) then
          do i=istr-1,iend+1
            wacbry_north(i)=wac(i,jendR,1)
            wkxbry_north(i)=wkx(i,jendR,1)
            wkebry_north(i)=wke(i,jendR,1)
          enddo
        endif
#   endif  /* WKB_OBC_NORTH */
#  endif /* ANA_BRY_WKB */
#  ifdef AGRIF
        endif
#  endif

# else /* !ANA_INI_WKB */

        do j=jstrR,jendR
          do i=istrR,iendR
            kk=max(sqrt(wkx(i,j,wstp)**2+wke(i,j,wstp)**2),eps)
            kh=min(kk*h(i,j),khmax)
            nw=0.5*(1.0+2.0*kh/max(sinh(2.0*kh),eps))
            wvn(i,j,wstp)=kk
            frq(i,j,wstp)=sqrt(g*kk*tanh(kh))
            wcg(i,j,wstp)=frq(i,j,wstp)/max(kk**2,eps)*nw  ! cg/k
            hrm(i,j,wstp)=sqrt(peg*max(wac(i,j,wstp),0.0)*frq(i,j,wstp))
#  ifdef WAVE_BREAK_TG86
            wsb(i,j,wstp)=sbc/(h(i,j)**5)*(hrm(i,j,wstp)**7) ! ep_b/rho/sigma
#  elif defined WAVE_BREAK_TG86A
            cff1=hrm(i,j,wstp)/(wkb_gam*h(i,j))
            wsb(i,j,wstp)=sbc/(h(i,j)**3)*(hrm(i,j,wstp)**5)
     &                           *(1.0-(1.0+cff1**2)**(-2.5))
#  elif defined WAVE_BREAK_CT93
            cff1=hrm(i,j,wstp)/(wkb_gam*h(i,j))
            wsb(i,j,wstp)=sbc/h(i,j)*(hrm(i,j,wstp)**3)
     &      *( 1.0+tanh(8.0*(cff1-1.0)) )*( 1.0-(1.0+cff1**2)**(-2.5) )
#  elif defined WAVE_BREAK_R93
            cff1=hrm(i,j,wstp)/(wkb_gam*h(i,j))
            wsb(i,j,wstp)=sbc/h(i,j)*hrm(i,j,wstp)**3*cff1**4
     &       *(1 -exp (-cff1**5 ) )
#  endif
#  ifdef WAVE_ROLLER
            wcr(i,j,wstp)=frq(i,j,wstp)/max(kk**2,eps)  ! c/k
            wsr(i,j,wstp)=c_roller*war(i,j,wstp)*kk     ! ep_r/rho/sigma
     &                      /max(frq(i,j,wstp)**2,eps)
#  endif
          enddo
        enddo
# endif /* ANA_INI_WKB */
!
# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
        do k=1,2
          call exchange_r2d_tile (istr,iend,jstr,jend,
     &                            wkx(START_2D_ARRAY,k))
          call exchange_r2d_tile (istr,iend,jstr,jend,
     &                            wke(START_2D_ARRAY,k))
          call exchange_r2d_tile (istr,iend,jstr,jend,
     &                            wac(START_2D_ARRAY,k))
          call exchange_r2d_tile (istr,iend,jstr,jend,
     &                            hrm(START_2D_ARRAY,k))
          call exchange_r2d_tile (istr,iend,jstr,jend,
     &                            frq(START_2D_ARRAY,k))
          call exchange_r2d_tile (istr,iend,jstr,jend,
     &                            wcg(START_2D_ARRAY,k))
          call exchange_r2d_tile (istr,iend,jstr,jend,
     &                        wsb(START_2D_ARRAY,k))
          call exchange_r2d_tile (istr,iend,jstr,jend,
     &                            wvn(START_2D_ARRAY,k))
#  ifdef WAVE_ROLLER
          call exchange_r2d_tile (istr,iend,jstr,jend,
     &                            war(START_2D_ARRAY,k))
          call exchange_r2d_tile (istr,iend,jstr,jend,
     &                            wcr(START_2D_ARRAY,k))
          call exchange_r2d_tile (istr,iend,jstr,jend,
     &                            wsr(START_2D_ARRAY,k))
#  endif
        end do
# endif
C$OMP BARRIER

      endif  ! <-- iwave=1
!
! ---------------------   End initialisation ------------------------------
!
!
! Prepair for multiple-time step algorithms
!
      do j=jstr-1,jend+1
        do i=istr-1,iend+1
# ifdef WKB_AB3
          rkx(i,j) =w1*wkx(i,j,wstp)+w2*wkx(i,j,wbak)+w3*wkx(i,j,wold)
          rke(i,j) =w1*wke(i,j,wstp)+w2*wke(i,j,wbak)+w3*wke(i,j,wold)
          rwac(i,j)=w1*wac(i,j,wstp)+w2*wac(i,j,wbak)+w3*wac(i,j,wold)
# else
          rkx(i,j) =w1*wkx(i,j,wstp)+w2*wkx(i,j,wbak)
          rke(i,j) =w1*wke(i,j,wstp)+w2*wke(i,j,wbak)
          rwac(i,j)=w1*wac(i,j,wstp)+w2*wac(i,j,wbak)
# endif
        enddo
      enddo
C$OMP BARRIER

      do j=jstr-1,jend+1
        do i=istr-1,iend+1
# ifdef MRL_CEW
#  if defined WKB_KZ_FILTER || defined WKB_TIME_FILTER
          Dstp(i,j)=max(h(i,j)+zwave(i,j,wmod),eps)
#  else
          Dstp(i,j)=h(i,j)+zeta(i,j,knew)
#  endif
# else
          Dstp(i,j)=h(i,j)+zeta(i,j,knew)
# endif
          cgx(i,j)=wcg(i,j,wstp)*rkx(i,j)
          cge(i,j)=wcg(i,j,wstp)*rke(i,j)
        enddo
      enddo

# define kvx acx
# define kue ace
# ifdef MRL_CEW
      do j=jstr,jend+1        ! 1/n*du/dx & 1/m*dv/de at psi-point
        do i=istr,iend+1
          kvx(i,j)=on_p(i,j)*(vwave(i,j,wmod)-vwave(i-1,j,wmod))
          kue(i,j)=om_p(i,j)*(uwave(i,j,wmod)-uwave(i,j-1,wmod))
        enddo
      enddo
# endif
!
!---------------------------------------------------------------------
! Solve primary wavenumber equation
!---------------------------------------------------------------------
!
      cff=10*0.5                          ! smoothing coefficient
!
      do j=jstr,jend
        do i=istr,iend
# ifdef MRL_CEW
          cxu=cgx(i,j) +0.5*(uwave(i+1,j,wmod)+uwave(i,j,wmod))
          cev=cge(i,j) +0.5*(vwave(i,j+1,wmod)+vwave(i,j,wmod))
# else
          cxu=cgx(i,j)
          cev=cge(i,j)
# endif
          ax1=cxu+abs(cxu)                ! 1st order upwind scheme
          ax2=cxu-abs(cxu)
          ay1=cev+abs(cev)
          ay2=cev-abs(cev)
          kh =min(wvn(i,j,wstp)*Dstp(i,j),khmax)
          a0 =frq(i,j,wstp)*wvn(i,j,wstp)/max(sinh(2.0*kh),eps)
          wkx(i,j,wnew) =wkx(i,j,wprv) -dtwave*pm(i,j)*pn(i,j)
     &                *( a0*0.5*on_r(i,j)*(Dstp(i+1,j)-Dstp(i-1,j))
     &               +0.5*( ax1*on_u(i  ,j)*(rkx(i  ,j)-rkx(i-1,j))
     &                     +ax2*on_u(i+1,j)*(rkx(i+1,j)-rkx(i  ,j))
     &                     +ay1*om_v(i,j  )*(rkx(i,j  )-rkx(i,j-1))
     &                     +ay2*om_v(i,j+1)*(rkx(i,j+1)-rkx(i,j  )) ) 
# ifdef MRL_CEW
     &                                          +rkx(i,j)*on_r(i,j)
     &                         *(uwave(i+1,j,wmod)-uwave(i,j,wmod))
     &                                               +0.25*rke(i,j)
     &               *(kvx(i,j)+kvx(i+1,j)+kvx(i,j+1)+kvx(i+1,j+1))
# endif
# ifdef ADD_DIFF
     &           -cff*on_r(i,j)*(rkx(i+1,j)-2.*rkx(i,j)+rkx(i-1,j))
     &           -cff*om_r(i,j)*(rkx(i,j+1)-2.*rkx(i,j)+rkx(i,j-1))
# endif
     &                                                            )
          wke(i,j,wnew) =wke(i,j,wprv) -dtwave*pm(i,j)*pn(i,j)
     &                *( a0*0.5*om_r(i,j)*(Dstp(i,j+1)-Dstp(i,j-1))
     &               +0.5*( ax1*on_u(i  ,j)*(rke(i  ,j)-rke(i-1,j))
     &                     +ax2*on_u(i+1,j)*(rke(i+1,j)-rke(i  ,j))
     &                     +ay1*om_v(i,j  )*(rke(i,j  )-rke(i,j-1))
     &                     +ay2*om_v(i,j+1)*(rke(i,j+1)-rke(i,j  )) )
# ifdef MRL_CEW
     &                                          +rke(i,j)*om_r(i,j)
     &                         *(vwave(i,j+1,wmod)-vwave(i,j,wmod))
     &                                               +0.25*rkx(i,j)
     &               *(kue(i,j)+kue(i+1,j)+kue(i,j+1)+kue(i+1,j+1))
# endif
# ifdef ADD_DIFF
     &           -cff*on_r(i,j)*(rke(i+1,j)-2.*rke(i,j)+rke(i-1,j))
     &           -cff*om_r(i,j)*(rke(i,j+1)-2.*rke(i,j)+rke(i,j-1))
# endif
     &                                                            )
# ifdef MASKING
          wkx(i,j,wnew) =wkx(i,j,wnew)*rmask(i,j)
          wke(i,j,wnew) =wke(i,j,wnew)*rmask(i,j)
# endif
        enddo
      enddo                ! <-- discard kvx,kue
# undef kvx
# undef kue
!
!---------------------------------------------------------------------
! Solve primary action balance equation
!---------------------------------------------------------------------
!
      do j=jstr-1,jend                    ! 1st order upwind scheme
        do i=istr-1,iend
# ifdef MRL_CEW
          cxu=0.5*(cgx(i+1,j)+cgx(i,j))+uwave(i+1,j,wmod)
          cev=0.5*(cge(i,j+1)+cge(i,j))+vwave(i,j+1,wmod)
# else
          cxu=0.5*(cgx(i+1,j)+cgx(i,j))
          cev=0.5*(cge(i,j+1)+cge(i,j))
# endif
          acx(i,j)=0.5*on_u(i+1,j)*( (cxu-abs(cxu))*rwac(i+1,j)
     &                              +(cxu+abs(cxu))*rwac(i,j) )
          ace(i,j)=0.5*om_v(i,j+1)*( (cev-abs(cev))*rwac(i,j+1)
     &                              +(cev+abs(cev))*rwac(i,j) )
        enddo
      enddo           ! <--- discard cgx,cge,rwac
!
      do j=jstr,jend
        do i=istr,iend
          wac(i,j,wnew) =wac(i,j,wprv)-dtwave*( pm(i,j)*pn(i,j)
     &                 *(acx(i,j)-acx(i-1,j)+ace(i,j)-ace(i,j-1)
# ifdef ADD_DIFF
     &                           -cff*on_r(i,j)*(wac(i+1,j,wprv)
     &                        -2.*wac(i,j,wprv)+wac(i-1,j,wprv))
     &                           -cff*om_r(i,j)*(wac(i,j+1,wprv)
     &                        -2.*wac(i,j,wprv)+wac(i,j-1,wprv))
# endif
     &                          ) +wsb(i,j,wstp)+wfc(i,j,wstp) )
# ifdef MASKING
          wac(i,j,wnew) =wac(i,j,wnew)*rmask(i,j)
# endif
        enddo
      enddo           ! <--- discard acx,ace
!
!----------------------------------------------------------------------
! Solve roller action balance equation
!---------------------------------------------------------------------
!
# ifdef WAVE_ROLLER
#  define rwar rwac
      do j=jstr-1,jend+1                  ! time stepping
        do i=istr-1,iend+1
#  ifdef WKB_AB3
          rwar(i,j)=w1*war(i,j,wstp)+w2*war(i,j,wbak)+w3*war(i,j,wold)
#  else
          rwar(i,j)=w1*war(i,j,wstp)+w2*war(i,j,wbak)
#  endif
        enddo
      enddo
      do j=jstr-1,jend+1                  ! phase velocity (not Cg)
        do i=istr-1,iend+1
          cgx(i,j)=wcr(i,j,wstp)*rkx(i,j)
          cge(i,j)=wcr(i,j,wstp)*rke(i,j)
        enddo
      enddo           ! <--- discard rkx,rke

      do j=jstr-1,jend                    ! 1st order upwind scheme
        do i=istr-1,iend
#  ifdef MRL_CEW
          cxu=0.5*(cgx(i+1,j)+cgx(i,j))+uwave(i+1,j,wmod)
          cev=0.5*(cge(i,j+1)+cge(i,j))+vwave(i,j+1,wmod)
#  else
          cxu=0.5*(cgx(i+1,j)+cgx(i,j))
          cev=0.5*(cge(i,j+1)+cge(i,j))
#  endif
          acx(i,j)=0.5*on_u(i+1,j)*( (cxu-abs(cxu))*rwar(i+1,j)
     &                              +(cxu+abs(cxu))*rwar(i,j) )
          ace(i,j)=0.5*om_v(i,j+1)*( (cev-abs(cev))*rwar(i,j+1)
     &                              +(cev+abs(cev))*rwar(i,j) )
        enddo
      enddo           ! <--- discard cgx,cge,rwar

      do j=jstr,jend
        do i=istr,iend
          war(i,j,wnew) =war(i,j,wprv)-dtwave*( pm(i,j)*pn(i,j)
     &               *(acx(i,j)-acx(i-1,j)+ace(i,j)-ace(i,j-1))
     &                -wkb_roller*wsb(i,j,wstp)+wsr(i,j,wstp) )
#  ifdef MASKING
          war(i,j,wnew) =war(i,j,wnew)*rmask(i,j)
#  endif
        enddo
      enddo           ! <--- discard acx,ace
#  undef rwar
# endif    /* WAVE_ROLLER */
!
!---------------------------------------------------------------------
! Set boundary conditions for the WKB wave quantities
! For now, only gradient and clamped BCs are available.
!---------------------------------------------------------------------
!
      call wkbbc_tile(Istr,Iend,Jstr,Jend)
!
!---------------------------------------------------------------------
! Estimate wave-associated variables for MRL_WCI/BBL routines
!---------------------------------------------------------------------
!
      do j=jstrR,jendR
        do i=istrR,iendR
          wvn(i,j,wnew)=max(sqrt(wkx(i,j,wnew)**2+wke(i,j,wnew)**2),eps)
          inv_k =1.0/wvn(i,j,wnew)
          khn =min(wvn(i,j,wnew)*Dstp(i,j),khmax)
          nw =0.5*(1.0+2.0*khn/max(sinh(2.0*khn),eps))
          frq(i,j,wnew) =sqrt(g*wvn(i,j,wnew)*tanh(khn))
          wcg(i,j,wnew) =frq(i,j,wnew)*(inv_k**2)*nw  ! cg/k
          hrm(i,j,wnew) =sqrt(peg*max(wac(i,j,wnew),0.0)*frq(i,j,wnew))
# ifdef WAVE_BREAK_TG86
          wsb(i,j,wnew) =sbc*((1.0/Dstp(i,j))**5)*(hrm(i,j,wnew)**7) ! ep_b/rho/sigma
# elif defined WAVE_BREAK_TG86A
          cff1=hrm(i,j,wnew)/(wkb_gam*Dstp(i,j))
          wsb(i,j,wnew)=sbc/(Dstp(i,j)**3)*(hrm(i,j,wnew)**5)
     &                            *(1.0-(1.0+peg**2)**(-2.5))
# elif defined WAVE_BREAK_CT93
          cff1=hrm(i,j,wnew)/(wkb_gam*Dstp(i,j))
          wsb(i,j,wnew) =sbc/Dstp(i,j)*(hrm(i,j,wnew)**3)
     &      *( 1.0+tanh(8.0*(cff1-1.0)) )*( 1.0-(1.0+cff1**2)**(-2.5) )

#  elif defined WAVE_BREAK_R93
            cff1=hrm(i,j,wnew)/(wkb_gam*Dstp(i,j))
            wsb(i,j,wstp)=sbc/Dstp(i,j)*hrm(i,j,wnew)**3*cff1**4
     &       *(1 -exp (-cff1**5 ) )
# else
          cff1=hrm(i,j,wnew)/(wkb_gam*Dstp(i,j))
          wsb(i,j,wnew) =sbc/Dstp(i,j)*(hrm(i,j,wnew)**3)
     &      *( 1.0+tanh(8.0*(cff1-1.0)) )*( 1.0-(1.0+cff1**2)**(-2.5) )
# endif
# ifdef WAVE_ROLLER
          wcr(i,j,wnew) = frq(i,j,wnew)*(inv_k**2)   ! c/k
          wsr(i,j,wnew) = c_roller*war(i,j,wnew)     ! ep_r/rho/sigma
     &                    *wvn(i,j,wnew)/max(frq(i,j,wnew)**2,eps)
# endif
# if defined BBL || defined MRL_WCI
          cosw = inv_k*wkx(i,j,wnew)            ! cosine wave direction (xi)
          sinw = inv_k*wke(i,j,wnew)            ! sine wave direction (eta)
#  ifdef BBL
!          wfrq(i,j) = frq(i,j,wnew)             ! used in M.Blaas BBL
!          orbital   = 0.5*wfrq(i,j)
!     &        *hrm(i,j,wnew)/max(sinh(khn),eps) ! orbital velocity magnitude
!          uorb(i,j) = orbital*cosw              ! orbital velocity (xi)
!          vorb(i,j) = orbital*sinw              ! orbital velocity (eta)
          inv_f= 1.0/max(frq(i,j,wnew),eps)
          Pwave(i,j)=2.*pi*inv_f
          Awave(i,j)=hrm(i,j,wnew)/2.
          Dwave(i,j)=ATAN2(sinw,cosw)
          if (cosw.ne.0.0) then
            Dwave(i,j)=ATAN2(sinw,cosw)
          else
            Dwave(i,j)=(pi/2.0)*SIGN(1.0,sinw)
          endif
#  endif
#  ifdef MRL_WCI
          wdrx(i,j) = cosw                      ! cosine wave direction (xi)
          wdre(i,j) = sinw                      ! sine wave direction (eta)
#  endif
# endif
# ifdef WAVE_FRICTION
          inv_f= 1.0/max(frq(i,j,wnew),eps)
          urms = 0.5*frq(i,j,wnew)*hrm(i,j,wnew)/max(sinh(khn),eps)
          abot = inv_f*urms
          fw   = 1.39*(Zob/max(abot,eps))**0.52 ! Soulsby (1995)
          wfc(i,j,wnew) = cfrc*fw*inv_f*urms**3 ! ep_d/rho/sigma
# else
          wfc(i,j,wnew) = 0.0
# endif
        enddo
      enddo

# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r2d_tile (istr,iend,jstr,jend,
     &                        wkx(START_2D_ARRAY,wnew))
      call exchange_r2d_tile (istr,iend,jstr,jend,
     &                        wke(START_2D_ARRAY,wnew))
      call exchange_r2d_tile (istr,iend,jstr,jend,
     &                        wac(START_2D_ARRAY,wnew))
      call exchange_r2d_tile (istr,iend,jstr,jend,
     &                        hrm(START_2D_ARRAY,wnew))
      call exchange_r2d_tile (istr,iend,jstr,jend,
     &                        frq(START_2D_ARRAY,wnew))
      call exchange_r2d_tile (istr,iend,jstr,jend,
     &                        wcg(START_2D_ARRAY,wnew))
      call exchange_r2d_tile (istr,iend,jstr,jend,
     &                        wsb(START_2D_ARRAY,wnew))
      call exchange_r2d_tile (istr,iend,jstr,jend,
     &                        wvn(START_2D_ARRAY,wnew))
#  ifdef WAVE_ROLLER
      call exchange_r2d_tile (istr,iend,jstr,jend,
     &                        war(START_2D_ARRAY,wnew))
      call exchange_r2d_tile (istr,iend,jstr,jend,
     &                        wcr(START_2D_ARRAY,wnew))
      call exchange_r2d_tile (istr,iend,jstr,jend,
     &                        wsr(START_2D_ARRAY,wnew))
#  endif
#  ifdef WAVE_FRICTION
      call exchange_r2d_tile (istr,iend,jstr,jend,
     &                        wfc(START_2D_ARRAY,wnew))
#  endif
#  ifdef BBL
!      call exchange_r2d_tile (istr,iend,jstr,jend,wfrq(START_2D_ARRAY))
!      call exchange_r2d_tile (istr,iend,jstr,jend,uorb(START_2D_ARRAY))
!      call exchange_r2d_tile (istr,iend,jstr,jend,vorb(START_2D_ARRAY))
      call exchange_r2d_tile (istr,iend,jstr,jend,Awave(START_2D_ARRAY))
      call exchange_r2d_tile (istr,iend,jstr,jend,Pwave(START_2D_ARRAY))
      call exchange_r2d_tile (istr,iend,jstr,jend,Dwave(START_2D_ARRAY))
#  endif
#  ifdef MRL_WCI
      call exchange_r2d_tile (istr,iend,jstr,jend,wdrx(START_2D_ARRAY))
      call exchange_r2d_tile (istr,iend,jstr,jend,wdre(START_2D_ARRAY))
#  endif
# endif

# undef wmod

      return
      end
!
!======================================================================
!
!                         WKB_CEW_PREP
!
!======================================================================
!
# ifdef MRL_CEW
      subroutine wkb_cew_prep (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call wkb_cew_prep_tile (istr,iend,jstr,jend,
     &                        A2d(1,1,trd),A2d(1,2,trd),A2d(1,3,trd))
      return
      end

      subroutine wkb_cew_prep_tile (istr,iend,jstr,jend,wrk1,wrk2,wrk3)
      implicit none
#include "param.h"
      integer istr,iend,jstr,jend, i,j,k
      real cff,cff1,cff3, cff4, wlen, dep,uave, vave, udep, vdep
      real, dimension(PRIVATE_2D_SCRATCH_ARRAY) :: wrk1,wrk2,wrk3
# include "grid.h"
# include "ocean2d.h"
# include "wkb_wwave.h"
# include "scalars.h"
# ifdef SOLVE3D
#  include "ocean3d.h"
# endif
# include "compute_auxiliary_bounds.h"

# ifdef WKB_TIME_FILTER
      if (iic.eq.0.or.iic.eq.ntstart.or.mod(iic-ntstart,cewavg).eq.1) then
        cff =1.0
        cff1=0.0
      elseif (mod(iic-ntstart,cewavg).gt.1) then
        cff =1.0
        cff1=1.0
      elseif (mod(iic-ntstart,cewavg).eq.0) then
        cff=1./float(cewavg)
        cff1=1.0
      endif
# endif
      do j=jstrR,jendR
        do i=istrR,iendR
# ifdef SOLVE3D
          wlen=pi/wvn(i,j,wnew)  ! L/2 = (2*pi/k)/2
          if (h(i,j)+zeta(i,j,knew).le.wlen) then
            wrk1(i,j)=ubar(i,j,knew)
            wrk2(i,j)=vbar(i,j,knew)
          else
            uave=0.D0
            vave=0.D0
            udep=0.D0
            vdep=0.D0
            do k=N,1,-1
              dep = zeta(i,j,knew)-z_w(i,j,k)
              if (k.eq.N.or.dep.le.wlen) then
                cff3=0.5*(Hz(i,j,k)+Hz(i-1,j,k))
                cff4=0.5*(Hz(i,j,k)+Hz(i,j-1,k))
                uave=uave+cff3*u(i,j,k,nrhs)
                vave=vave+cff4*v(i,j,k,nrhs)
                udep=udep+cff3
                vdep=vdep+cff4
              endif
            enddo
            wrk1(i,j)=uave/udep
            wrk2(i,j)=vave/vdep
          endif
          wrk3(i,j)=zeta(i,j,knew)
# else
          wrk1(i,j)=ubar(i,j,knew)
          wrk2(i,j)=vbar(i,j,knew)
          wrk3(i,j)=zeta(i,j,knew)
# endif
# ifdef WKB_TIME_FILTER
          uwave(i,j,wavg)=cff*(cff1*uwave(i,j,wavg)+wrk1(i,j))
          vwave(i,j,wavg)=cff*(cff1*vwave(i,j,wavg)+wrk2(i,j))
          zwave(i,j,wavg)=cff*(cff1*zwave(i,j,wavg)+wrk3(i,j))
# else
          uwave(i,j,wavg)=wrk1(i,j)
          vwave(i,j,wavg)=wrk2(i,j)
          zwave(i,j,wavg)=wrk3(i,j)
# endif
        enddo
      enddo

# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
#  ifdef WKB_TIME_FILTER
      if (iic.eq.0.or.mod(iic-ntstart,cewavg).eq.0) then
#  endif
      call exchange_r2d_tile(istr,iend,jstr,jend,
     &                       uwave(START_2D_ARRAY,wavg))
      call exchange_r2d_tile(istr,iend,jstr,jend,
     &                       vwave(START_2D_ARRAY,wavg))
      call exchange_r2d_tile(istr,iend,jstr,jend,
     &                       zwave(START_2D_ARRAY,wavg))
#  ifdef WKB_TIME_FILTER
      endif
#  endif
# endif

      return
      end
!
!======================================================================
!
!                         WKB_UVFIELD
!
!======================================================================
!
      subroutine wkb_uvfield (tile,linterp)
      implicit none
      integer tile, linterp, trd, omp_get_thread_num
#include "param.h"
#include "private_scratch.h"
#include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call wkb_uvfield_tile (istr,iend,jstr,jend, linterp, 
     &            A2d(1, 1, trd), A2d(1, 2, trd), A2d(1, 3, trd), 
     &            A2d(1, 4, trd), A2d(1, 5, trd), A2d(1, 6, trd), 
     &            A2d(1, 7, trd), A2d(1, 8, trd), A2d(1, 9, trd), 
     &            A2d(1,10, trd),  A2d(1,11, trd), A2d(1,12,trd))
      return
      end

      subroutine wkb_uvfield_tile (istr,iend,jstr,jend, linterp,
     &           wrk1, wrk2, wrk3, FX,FE,FE1, GX,GE,GE1, ZX,ZE,ZE1)
      implicit none
#include "param.h"
      integer istr,iend,jstr,jend, i,j,k, imin,imax,jmin,jmax, 
     &        wold,linterp
      real cff, cff1, cff2
      real, dimension(PRIVATE_2D_SCRATCH_ARRAY) :: wrk1,wrk2,wrk3,
     &                              FX,FE,FE1,GX,GE,GE1,ZX,ZE,ZE1
#include "wkb_wwave.h"
#include "grid.h"
#include "scalars.h"
#include "compute_auxiliary_bounds.h"

# ifdef WKB_TIME_FILTER
      if (iic.eq.0.or.mod(iic-ntstart,cewavg).eq.0) then
# endif

        wold=wint-1
        if (wold.lt.1) wold=2

        imin=istr-1
        imax=iend+1
        jmin=jstr-1
        jmax=jend+1
# ifndef EW_PERIODIC
        if (WESTERN_EDGE) imin=istr
        if (EASTERN_EDGE) imax=iend
# endif
# ifndef NS_PERIODIC
        if (SOUTHERN_EDGE) jmin=jstr
        if (NORTHERN_EDGE) jmax=jend
# endif

# ifdef WKB_KZ_FILTER
!
! Kolmogorov-Zurbenko filter with Gaussian/Laplacian kernel
! ===============================================================
! This code provides smoothing filter on current velocity and zeta
! for WKB model with CEW as current field should evolve slowly on
! waves in the MRL asymptotic regime.  Sometimes it is recommended
! to iterate the following sequence interp_max (times) defined in 
! wkb_wwave.h (Kolmogorov-Zurbenko filter).
!
! 1) cff = 1/8, 1/4      2) cff = 1/12, 3/16    3) cff = 0, 1/8
!
!    1/16  1/8  1/16       1/32   1/8  1/32            1/8
!    1/8   1/4  1/8         1/8   3/8  1/8        1/8  1/4  1/8
!    1/16  1/8  1/16       1/32   1/8  1/32            1/8
!
!  2D 1-2-1-Hanning           isotropic             5-point
!   window smoother           Laplacian             Laplacian
!  (Gaussian filter)
!
        cff=1.D0/8.D0
        cff1=1.D0/4.D0
c        cff=1.D0/12.D0
c        cff1=3.D0/16.D0

        if (linterp.eq.1) then
          do j=jmin,jmax
            do i=imin,imax
              wrk1(i,j)=uwave(i,j,wavg)
              wrk2(i,j)=vwave(i,j,wavg)
              wrk3(i,j)=zwave(i,j,wavg)
            enddo
          enddo
        else
          do j=jmin,jmax
            do i=imin,imax
              wrk1(i,j)=uwave(i,j,wold)
              wrk2(i,j)=vwave(i,j,wold)
              wrk3(i,j)=zwave(i,j,wold)
            enddo
          enddo
        endif

#  ifndef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=jmin,jmax
            wrk1(istr-1,j)=wrk1(istr,j)
            wrk2(istr-1,j)=wrk2(istr,j)
            wrk3(istr-1,j)=wrk3(istr,j)
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=jmin,jmax
            wrk1(iend+1,j)=wrk1(iend,j)
            wrk2(iend+1,j)=wrk2(iend,j)
            wrk3(iend+1,j)=wrk3(iend,j)
          enddo
        endif
#  endif
#  ifndef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=imin,imax
            wrk1(i,jstr-1)=wrk1(i,jstr)
            wrk2(i,jstr-1)=wrk2(i,jstr)
            wrk3(i,jstr-1)=wrk3(i,jstr)
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=imin,imax
            wrk1(i,jend+1)=wrk1(i,jend)
            wrk2(i,jend+1)=wrk2(i,jend)
            wrk3(i,jend+1)=wrk3(i,jend)
          enddo
        endif
#   ifndef EW_PERIODIC
        if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
          wrk1(istr-1,jstr-1)=wrk1(istr,jstr)
          wrk2(istr-1,jstr-1)=wrk2(istr,jstr)
          wrk3(istr-1,jstr-1)=wrk3(istr,jstr)
        endif
        if (WESTERN_EDGE .and. NORTHERN_EDGE) then
          wrk1(istr-1,jend+1)=wrk1(istr,jend)
          wrk2(istr-1,jend+1)=wrk2(istr,jend)
          wrk3(istr-1,jend+1)=wrk3(istr,jend)
        endif
        if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
          wrk1(iend+1,jstr-1)=wrk1(iend,jstr)
          wrk2(iend+1,jstr-1)=wrk2(iend,jstr)
          wrk3(iend+1,jstr-1)=wrk3(iend,jstr)
        endif
        if (EASTERN_EDGE .and. NORTHERN_EDGE) then
          wrk1(iend+1,jend+1)=wrk1(iend,jend)
          wrk2(iend+1,jend+1)=wrk2(iend,jend)
          wrk3(iend+1,jend+1)=wrk3(iend,jend)
        endif
#   endif
#  endif
        do j=jstr-1,jend+1
          do i=istr,iend+1
            FX(i,j)=(wrk1(i,j)-wrk1(i-1,j))
#  ifdef MASKING
     &                        *rmask(i-1,j)
#  endif
            GX(i,j)=(wrk2(i,j)-wrk2(i-1,j))
#  ifdef MASKING
     &                        *pmask(i-1,j)
#  endif
            ZX(i,j)=(wrk3(i,j)-wrk3(i-1,j))
#  ifdef MASKING
     &                          *umask(i,j)
#  endif
          enddo
        enddo
        do j=jstr,jend+1
          do i=istr-1,iend+1
            FE(i,j)=(wrk1(i,j)-wrk1(i,j-1))
#  ifdef MASKING
     &                          *pmask(i,j)
#  endif
            GE(i,j)=(wrk2(i,j)-wrk2(i,j-1))
#  ifdef MASKING
     &                        *rmask(i,j-1)
#  endif
            ZE(i,j)=(wrk3(i,j)-wrk3(i,j-1))
#  ifdef MASKING
     &                          *vmask(i,j)
#  endif
          enddo
        enddo
        do j=jstr,jend+1
          do i=istr,iend
            FE1(i,j)=FE(i,j)+cff*( FX(i+1,j)+FX(i,j-1)
     &                            -FX(i,j)-FX(i+1,j-1))
            GE1(i,j)=GE(i,j)+cff*( GX(i+1,j)+GX(i,j-1)
     &                            -GX(i,j)-GX(i+1,j-1))
            ZE1(i,j)=ZE(i,j)+cff*( ZX(i+1,j)+ZX(i,j-1)
     &                            -ZX(i,j)-ZX(i+1,j-1))
          enddo
        enddo
        do j=jstr,jend
          do i=istr,iend+1
            FX(i,j)=FX(i,j)+cff*( FE(i,j+1)+FE(i-1,j)
     &                           -FE(i,j)-FE(i-1,j+1))
            GX(i,j)=GX(i,j)+cff*( GE(i,j+1)+GE(i-1,j)
     &                           -GE(i,j)-GE(i-1,j+1))
            ZX(i,j)=ZX(i,j)+cff*( ZE(i,j+1)+ZE(i-1,j)
     &                           -ZE(i,j)-ZE(i-1,j+1))
          enddo
        enddo
        do j=jstr,jend
          do i=istr,iend
            wrk1(i,j)=wrk1(i,j)+cff1*( FX(i+1,j)-FX(i,j)
     &                             +FE1(i,j+1)-FE1(i,j))
#  ifdef MASKING
            wrk1(i,j)=wrk1(i,j)*umask(i,j)
#  endif
            wrk2(i,j)=wrk2(i,j)+cff1*( GX(i+1,j)-GX(i,j)
     &                            +GE1(i,j+1)-GE1(i,j))
#  ifdef MASKING
            wrk2(i,j)=wrk2(i,j)*vmask(i,j)
#  endif
            wrk3(i,j)=wrk3(i,j)+cff1*( ZX(i+1,j)-ZX(i,j)
     &                            +ZE1(i,j+1)-ZE1(i,j))
#  ifdef MASKING
            wrk3(i,j)=wrk3(i,j)*rmask(i,j)
#  endif
          enddo
        enddo
        do j=jstr,jend            ! copy into shared arrays
          do i=istr,iend
            uwave(i,j,wint)=wrk1(i,j)
            vwave(i,j,wint)=wrk2(i,j)
            zwave(i,j,wint)=wrk3(i,j)
          enddo
        enddo
#  ifndef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=jstr,jend
            uwave(istr-1,j,wint)=uwave(istr,j,wint)
            vwave(istr-1,j,wint)=vwave(istr,j,wint)
            zwave(istr-1,j,wint)=zwave(istr,j,wint)
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=jstr,jend
            uwave(iend+1,j,wint)=uwave(iend,j,wint)
            vwave(iend+1,j,wint)=vwave(iend,j,wint)
            zwave(iend+1,j,wint)=zwave(iend,j,wint)
          enddo
        endif
#  endif
#  ifndef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=istr,iend
            uwave(i,jstr-1,wint)=uwave(i,jstr,wint)
            vwave(i,jstr-1,wint)=vwave(i,jstr,wint)
            zwave(i,jstr-1,wint)=zwave(i,jstr,wint)
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=istr,iend
            uwave(i,jend+1,wint)=uwave(i,jend,wint)
            vwave(i,jend+1,wint)=vwave(i,jend,wint)
            zwave(i,jend+1,wint)=zwave(i,jend,wint)
          enddo
        endif
#   ifndef EW_PERIODIC
        if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
          uwave(istr-1,jstr-1,wint)=uwave(istr,jstr,wint)
          vwave(istr-1,jstr-1,wint)=vwave(istr,jstr,wint)
          zwave(istr-1,jstr-1,wint)=zwave(istr,jstr,wint)
        endif
        if (WESTERN_EDGE .and. NORTHERN_EDGE) then
          uwave(istr-1,jend+1,wint)=uwave(istr,jend,wint)
          vwave(istr-1,jend+1,wint)=vwave(istr,jend,wint)
          zwave(istr-1,jend+1,wint)=zwave(istr,jend,wint)
        endif
        if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
          uwave(iend+1,jstr-1,wint)=uwave(iend,jstr,wint)
          vwave(iend+1,jstr-1,wint)=vwave(iend,jstr,wint)
          zwave(iend+1,jstr-1,wint)=zwave(iend,jstr,wint)
        endif
        if (EASTERN_EDGE .and. NORTHERN_EDGE) then
          uwave(iend+1,jend+1,wint)=uwave(iend,jend,wint)
          vwave(iend+1,jend+1,wint)=vwave(iend,jend,wint)
          zwave(iend+1,jend+1,wint)=zwave(iend,jend,wint)
        endif
#   endif
#  endif

# else    /* WKB_KZ_FILTER */

        do j=jstrR,jendR
          do i=istrR,iendR
            uwave(i,j,wint)=uwave(i,j,wavg)
            vwave(i,j,wint)=vwave(i,j,wavg)
            zwave(i,j,wint)=zwave(i,j,wavg)
#  ifdef MASKING
            uwave(i,j,wint)=uwave(i,j,wint)*umask(i,j)
            vwave(i,j,wint)=vwave(i,j,wint)*vmask(i,j)
            zwave(i,j,wint)=zwave(i,j,wint)*rmask(i,j)
#  endif
          enddo
        enddo

# endif /* WKB_KZ_FILTER */

# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
        call exchange_r2d_tile(istr,iend,jstr,jend,
     &                         uwave(START_2D_ARRAY,wint))
        call exchange_r2d_tile(istr,iend,jstr,jend,
     &                         vwave(START_2D_ARRAY,wint))
        call exchange_r2d_tile(istr,iend,jstr,jend,
     &                         zwave(START_2D_ARRAY,wint))
# endif

# ifdef WKB_TIME_FILTER
      endif
# endif

      return
      end
!
!======================================================================
!
!                         WKB_CEW_FINALIZE
!
!======================================================================
!
      subroutine wkb_cew_finalize (tile)
      implicit none
      integer tile
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      call wkb_cew_finalize_tile (istr,iend,jstr,jend)
      return
      end

      subroutine wkb_cew_finalize_tile (istr,iend,jstr,jend)
      implicit none
# include "param.h"
      integer istr,iend,jstr,jend, i,j
# ifdef WKB_TIME_FILTER
      integer wstp1, wstp2
      real cff,cff1,cff2
#  include "wkb_wwave.h"
#  include "scalars.h"
#  include "compute_auxiliary_bounds.h"
      wstp1=4
      wstp2=5
      if (iic.eq.0.or.iic.eq.ntstart) then
        do j=jstrR,jendR
          do i=istrR,iendR
            uwave(i,j,wstp1)=uwave(i,j,wint)
            vwave(i,j,wstp1)=vwave(i,j,wint)
            zwave(i,j,wstp1)=zwave(i,j,wint)
            uwave(i,j,wstp2)=uwave(i,j,wint)
            vwave(i,j,wstp2)=vwave(i,j,wint)
            zwave(i,j,wstp2)=zwave(i,j,wint)
          enddo
        enddo
      elseif (mod(iic-ntstart,cewavg).eq.0) then
        do j=jstrR,jendR
          do i=istrR,iendR
            uwave(i,j,wstp1)=uwave(i,j,wstp2)
            vwave(i,j,wstp1)=vwave(i,j,wstp2)
            zwave(i,j,wstp1)=zwave(i,j,wstp2)
            uwave(i,j,wstp2)=uwave(i,j,wint)
            vwave(i,j,wstp2)=vwave(i,j,wint)
            zwave(i,j,wstp2)=zwave(i,j,wint)
          enddo
        enddo
      endif
      cff =1.D0/dble(cewavg)
#  ifdef SOLVE3D
      cff2=cff*(dble(mod(iic-ntstart,cewavg))+dble(iwave-1)/ndtfast)
#  else
      cff2=cff*dble(mod(iic-ntstart,cewavg))
#  endif
      if (iic.eq.0) cff2=0.D0
      cff1=1.D0-cff2
      do j=jstrR,jendR
        do i=istrR,iendR
          uwave(i,j,wcew)=cff1*uwave(i,j,wstp1)+cff2*uwave(i,j,wstp2)
          vwave(i,j,wcew)=cff1*vwave(i,j,wstp1)+cff2*vwave(i,j,wstp2)
          zwave(i,j,wcew)=cff1*zwave(i,j,wstp1)+cff2*zwave(i,j,wstp2)
        enddo
      enddo

# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
        call exchange_r2d_tile(istr,iend,jstr,jend,
     &                         uwave(START_2D_ARRAY,wcew))
        call exchange_r2d_tile(istr,iend,jstr,jend,
     &                         vwave(START_2D_ARRAY,wcew))
        call exchange_r2d_tile(istr,iend,jstr,jend,
     &                         zwave(START_2D_ARRAY,wcew))
# endif

# endif  /* WKB_TIME_FILTER */
      return
      end
# endif  /* MRL_CEW */
!
!======================================================================
!
!                         WKB_DIAG
!
!======================================================================
!
      subroutine wkb_diag (tile)
      implicit none
      integer tile
#include "param.h"
#include "private_scratch.h"
#include "compute_tile_bounds.h"
      call wkb_diag_tile (istr,iend,jstr,jend)
      return
      end

      subroutine wkb_diag_tile (istr,iend,jstr,jend)
      implicit none
      integer istr,iend,jstr,jend, i,j,k, nsubs,ie
#include "param.h"
      real, dimension(2) :: my_ww, buff
      integer max_check_line
      parameter (max_check_line=128)
      character check_line*(max_check_line), tstring*18
#ifdef MPI
      include 'mpif.h'
      integer status(MPI_STATUS_SIZE), ierr
#     include "mpi_roms.h"
#endif
#include "grid.h"
#include "wkb_wwave.h"
#include "scalars.h"
#include "compute_auxiliary_bounds.h"
!
! Diagnose difference of wave action density and modulus wavenumber
! between two neighboring time steps to ensure the steady state.
! First, compute thread-integrated deviations, my_wac and my_wkn.
!
      if (mod(iwave-1,winfo) .eq. 0) then
        my_ww(1) = maxval(abs(wac(:,:,wnew)-wac(:,:,wstp)))
        my_ww(2) = maxval(abs(wvn(:,:,wnew)-wvn(:,:,wstp)))
        if (SINGLE_TILE_MODE) then
          nsubs=1
        else
          nsubs=NSUB_X*NSUB_E
        endif
!
! Compute global summation for OMP and MPI situations
!
C$OMP CRITICAL (diag_cr_rgn)
!        if (tile_count.eq.0) then       ! Initialize global sums
          av_wac=my_ww(1)               ! for multithreaded shared
          av_wkn=my_ww(2)               ! memory summation.
!        else                            ! Perform global summation
!          av_wac=av_wac+my_ww(1)        ! among the threads within
!          av_wkn=av_wkn+my_ww(2)        ! each MPI process.
!        endif

        tile_count=tile_count+1         ! This counter identifies
                                        ! the last thread, whoever
        if (tile_count.eq.nsubs) then   ! it is, not always master.
          tile_count=0
#ifdef MPI
          if (NNODES.gt.1) then         ! Perform global max
            call MPI_allreduce(my_ww,buff,2,
     &              MPI_DOUBLE_PRECISION, MPI_MAX,MPI_COMM_WORLD,ierr)
          av_wac = buff(1)
          av_wkn = buff(2)
          endif
          if (mynode.eq.0) then               ! Compute diagnostics 
#endif
            if (first_time.eq.0) then
              first_time=1
              write(*,'(5x,A,1x,A,3x,A,3x,A,2x,A)')
     &         'STEP','time[DAYS]','DIFF_ACTION','DIFF_WAVENUMBER'
!C$   &                                                      ,'trd'
            endif
            write(check_line,'(2(1PE16.10,X),I3)') av_wac, av_wkn
!C$   &                                                   , proc(2)
            ie=max_check_line
            do while (check_line(ie:ie).eq.' ' .and. ie.gt.0)
              ie=ie-1
            enddo                                 ! Suppress FORTRAN
            i=0                                   ! floating point Es
            do while (i.lt.ie)                    ! to shorten the
              i=i+1                               ! diagnostic line.
              if (check_line(i:i).eq.'E' .or.
     &            check_line(i:i).eq.'e') then
                check_line(i:ie-1)=check_line(i+1:ie)
                check_line(ie:ie)=' '
                ie=ie-1
              elseif (ichar(check_line(i:i)).lt.48 .or.
     &                ichar(check_line(i:i)).gt.57) then
                if (check_line(i:i).ne.' ' .and.
     &              check_line(i:i).ne.'+'  .and. ! Set may_day_flag
     &              check_line(i:i).ne.'-'  .and. ! to terminate the
     &              check_line(i:i).ne.'.') then  ! run in the case
                  may_day_flag=1                  ! of floating point
                endif                             ! exception
              endif
            enddo

            write(tstring,'(F18.8)') real(iwave-1)*dtfast*sec2day
            i=1
            do while (i.lt.18 .and. tstring(i:i).eq.' ')
              i=i+1
            enddo
            write(*,'(I9,1x,A,1x,A)') iwave-1, tstring(i:i+9),
     &                                       check_line(1:ie)

#ifdef MPI
          endif    ! <-- mynode.eq.0

          call MPI_Bcast(may_day_flag, 1, MPI_INTEGER,
     &                       0, MPI_COMM_WORLD, ierr)
#endif

          if (winfo.eq. 1   .and.  iwave.gt.   8) winfo=2
          if (winfo.eq. 2   .and.  iwave.gt.  16) winfo=4
          if (winfo.eq. 4   .and.  iwave.gt.  64) winfo=8
          if (winfo.eq. 8   .and.  iwave.gt. 256) winfo=16
          if (winfo.eq.16   .and.  iwave.gt. 512) winfo=32

        endif  ! <-- tile_count.eq.nsubs
C$OMP END CRITICAL (diag_cr_rgn)
      endif              ! <-- mod(iwave-1,winfo).eq.0
      return
      end
#else
      subroutine wkb_wwave_empty
      return
      end
#endif
 
