! $Id: bbl.F 1588 2014-08-04 16:26:01Z marchesiello $
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
#ifdef BBL
      subroutine bblm (tile)
!====================================================================!
!  Compute bottom stresses for combined waves & currents             !
!  using the parametric approximation by Soulsby 1997:               !
!  t_cw = t_c[1+1.2(t_w/(t_c+t_w))^3.2]                              !
!  in which                                                          !
!     t_cw = the combined wave-averaged stress (in current dir)      !
!     t_c  = stress due to currents if waves would be absent         ! 
!     t_w  = amplitude of stress due to waves without currents       !
!  and                                                               !
!     t_cw_max = SQRT([t_cw+t_w cos(phi_cw)]^2 + [t_w sin(phi_cw)]^2)!
!     in which                                                       !
!       t_cw_max ~ the maximum combined wave-averaged stress         !
!       phi_cw   = the angle between current and waves               !
!--------------------------------------------------------------------!
! References:
!
!  - Dyer 1986, Coastal & Estuarine Sediment Dynamics, Wiley, 342 pp.
!  - Harris & Wiberg 2001, Comp. & Geosci. 27, 675-690
!  - Li & Amos 2001, Comp. & Geosci. 27, 619-645
!  - Soulsby 1997, Dynamics of Marine Sands, Telford  Publ., 249 pp.
!  - Soulsby 1995, Bed shear-stresses due to combined waves and currents,
!    in: Stive et al: Advances in Coastal Morphodynamics, Wiley, 4.20-4.23
!  - Wiberg & Harris 1994, J. Geophys. Res. 99(C4), 775-789
!
! First Implementation: Meinte Blaas 2002
! Changes: P. Marchesiello & R. Benshila 2013-2014:
!  --> WKB wave model; bottom stress stability limit; 2D application
!
!====================================================================!
      implicit none
      integer  tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
!
# include "compute_tile_bounds.h"
!
      trd=omp_get_thread_num()
      call bblm_tile (Istr,Iend,Jstr,Jend,
     &                  A2d(1, 1,trd),A2d(1, 2,trd),A2d(1, 3,trd),
     &                  A2d(1, 4,trd),A2d(1, 5,trd),A2d(1, 6,trd),
     &                  A2d(1, 7,trd),A2d(1, 8,trd),A2d(1, 9,trd))
      return
      end

!********************************************************************!
      SUBROUTINE bblm_tile (Istr,Iend,Jstr,Jend,Ub,Zr,Ur,Vr,Umag,
     &                      tauc,tauw,taucw,taucwmax)
!********************************************************************!
!  output: bu/vstr = tauc = t_cw 
!     effective wave-averaged bottom stress
!     (after applying apparent roughness)  
!     bu/vstrw = tauw = t_cw_max
!     maximum skin-frictional bottom stress
!     rheight = ripple height (m)
!     rlength = ripple length (m)
!                                
!  input: Awave, Pwave, Dwave, h, Zob, d50, u,v
!         NB! Dwave is assumed to be direction waves are coming from
!             positive clockwise wrt North      
!********************************************************************!
      implicit none
# include "param.h"
# include "bbl.h"
# include "forces.h"
# include "grid.h"
# ifdef SOLVE3D
#  include "ocean3d.h"
# else
#  include "ocean2d.h"
# endif
# include "scalars.h"
# include "sediment.h"

      logical
     &        SAND, SILT

      integer
     &        Iend, Istr, Jend, Jstr, i, ised, j

      real
     &  Ab, anglec, anglew,cff, cff1, cff2, d50,
     &  Fwave, Kbh, Kbh2, Kdh, phic, phicw, 
     &  rhbio, rhbiomax, rhgt, rhmax, rhmin, rhfac, 
     &  rlbio, rlen, rlmin, rhosed, rhow,
     &  tau_bf, tau_c, tau_cb, tau_ex, tau_up,tau_w,
     &  tau_wb, tau_cs, tau_cw, tau_cwb, tau_cws, tau_en,
     &  thetw, twopi, Ucur, Vcur, Ubc, visk, wset, 
     &  znot, znotc, znot_bl, znot_rip
      parameter (Ubc = 0.01)               

      real
     &            tauc(PRIVATE_2D_SCRATCH_ARRAY),
     &            tauw(PRIVATE_2D_SCRATCH_ARRAY),
     &           taucw(PRIVATE_2D_SCRATCH_ARRAY),
     &        taucwmax(PRIVATE_2D_SCRATCH_ARRAY),
     &              Ub(PRIVATE_2D_SCRATCH_ARRAY),
     &            Umag(PRIVATE_2D_SCRATCH_ARRAY),
     &              Ur(PRIVATE_2D_SCRATCH_ARRAY),
     &              Vr(PRIVATE_2D_SCRATCH_ARRAY),
     &              Zr(PRIVATE_2D_SCRATCH_ARRAY)
     
      real
     &        K1, K2, K3, K4, K5, K6
      parameter (K1=0.6666666666, K2=0.3555555555,
     &           K3=0.1608465608, K4=0.0632098765,
     &           K5=0.0217540484, K6=0.0065407983)

      real
     &           scf1, scf2, scf3, scf4, scf5
      parameter (scf1 = 0.5 * 1.39, scf2 = 0.52, 
     &           scf3 = 2.0 - scf2, scf4 = 1.2,
     &           scf5 = 3.2)

# include "compute_auxiliary_bounds.h"

!
!---------------------------------------------------------------------
!  Initalize to default values + set b(u,v)str to old time level value.
!---------------------------------------------------------------------
!
      twopi=2.0*pi
      rhbiomax = 6.0d-3 ! maximum biogenic ripple height [m]  
      rhmin = 1.0d-3    ! user-defined minimum ripple height [m]  
      rlmin = 1.0d-2    ! user-defined minimum ripple length [m]  
      rhfac = 1.0d0/EXP(4.11d0)

      do j=JstrV-1,Jend
        do i=IstrU-1,Iend
          tauc(i,j)=0.0       ! [m^2/s^2]
          taucw(i,j)=0.0
          taucwmax(i,j)=0.0
       enddo
      enddo
!---------------------------------------------------------------------
!  Set currents above bed.
!---------------------------------------------------------------------
      DO j=JstrV-1,Jend+1
        DO i=IstrU-1,Iend+1
# ifdef SOLVE3D
          Zr(i,j)=max(z_r(i,j,1)-z_w(i,j,0),Zob+1.E-4)
          Ur(i,j)=u(i,j,1,nrhs)
          Vr(i,j)=v(i,j,1,nrhs)
# else
          Zr(i,j)=0.5*(h(i,j)+zeta(i,j,kstp))
          Ur(i,j)=ubar(i,j,kstp)
          Vr(i,j)=vbar(i,j,kstp)
# endif
        enddo
      enddo
    
! start main j,i loop

      DO j=JstrV-1,Jend
        DO i=IstrU-1,Iend
          rhbio = 0.0d0
          rlbio = 0.0d0
          rlen = Lripple(i,j)
          rhgt = Hripple(i,j)
# ifdef SOLVE3D
          rhow = rho(i,j,1)+1000.0
          visk = 1.3e-3/rhow         ! kinem. viscosity
# else
          visk = 1.3e-3/rho0
# endif
!---------------------------------------------------------------------
!  Compute bed wave orbital velocity (m/s) and excursion amplitude
!  (m) from wind-induced waves.  Use Dean & Dalrymple 1991 6th degree
!  polynomial expr. to approximate wave number on shoaling water
!---------------------------------------------------------------------
          Fwave=twopi/Pwave(i,j)
          Kdh=(h(i,j)+z_w(i,j,N))*Fwave*Fwave/g
          Kbh2=Kdh*Kdh+
     &         Kdh/(1.0+Kdh*(K1+Kdh*(K2+Kdh*(K3+Kdh*(K4+
     &              Kdh*(K5+K6*Kdh))))))
          Kbh = SQRT(Kbh2)

!  Compute bed wave orbital velocity and excursion amplitude.
!
          Ab=Awave(i,j)/SINH(Kbh)
          Ub(i,j)=Fwave*Ab
# ifdef MASKING
     &                                   *rmask(i,j)
# endif
!
!  Compute bottom current magnitude at RHO-points.
!
          Ucur=0.5*(Ur(i,j)+Ur(i+1,j))
          Vcur=0.5*(Vr(i,j)+Vr(i,j+1))
          Umag(i,j)=SQRT(Ucur*Ucur+Vcur*Vcur)
# ifdef MASKING
     &                                   *rmask(i,j)
# endif
!
!  Compute angle between currents and waves (radians)
!
!         if Dwave is dir waves travel TO N:
!         phicw=pi/2.0 +Dwave(i,j)-angler(i,j)-phic
!
          if (Ucur.ne.0.0) then
            phic=ATAN2(Vcur,Ucur)
          else
            phic=(pi/2.0)*SIGN(1.0,Vcur)
          endif
# ifdef WKB_WWAVE
          phicw=Dwave(i,j)-phic
# else
          phicw=(3.0*pi)/2.0 -Dwave(i,j)-phic
# endif
# ifdef CURVGRID
     &                                  -angler(i,j)
# endif
!---------------------------------------------------------------------
!  Determine skin roughness from sediment size and
!  set default logarithmic profile consistent with current-only case
!  
! Establish local median grain size for all calculations in this subroutine 
! and determine local values of critical stresses.
! Since most parameterizations have been derived ignoring multiple grain sizes, 
! we apply this single d50 also in the case of mixed beds. 
!---------------------------------------------------------------------
#if defined ANA_BSEDIM || defined SEDIMENT

          d50 = Ssize(i,j)             ! [m]
          tau_cb = taucb(i,j)          ! [m^2/s^2]
# ifdef ANA_BSEDIM
     &      /rhow
# endif
          wset = w_set(i,j)            ! [m/s]
          rhosed = Sdens(i,j)/rhow     ! [ ] relative density
          
# endif /* ANA_BSEDIM || SEDIMENT */ 

! critical stress for transition to sheet flow  (Li&Amos'01, eq.11)
          tau_up = 0.172*(rhosed-1.)*g*(d50**0.624) ! [m^2/s^2]

! critical stress for initiation to suspended load (Bagnold '66|Li&Amos'01, eq.9)
!         tau_crs = max(0.64*wset*wset,ustr_shields)              ! [m^2/s^2]

! threshold stress for break off (Grant & Madsen,1982)   
          tau_bf = 0.79*(visk**(-0.6))*(((rhosed-1.)*g)**0.3)
     &                                  *(d50**0.9)*tau_cb        ! [m^2/s^2]

          znotc= d50/12.0
          znot = MAX(Zob,znotc)
!---------------------------------------------------------------------
!  set tauc acc. to current-only case (max znot & skin friction) [m/s]
!---------------------------------------------------------------------
          cff1 = vonKar/LOG(Zr(i,j)/znot)
          cff2 = MIN(Cdb_max,MAX(Cdb_min,cff1*cff1))
          tauc(i,j)= cff2*Umag(i,j)*Umag(i,j)        ! [m^2/s^2] 

          cff1 = vonKar/LOG(Zr(i,j)/znotc)
          tau_cs = cff1*cff1*Umag(i,j)*Umag(i,j)     ! [m^2/s^2] 
 
!---------------------------------------------------------------------
! If significant waves (Ub > Ubc= 0.01 m/s):
!  Wave-current interaction case according to Soulsby 1995.  
!  Otherwise: tauw = tauc for sediment purposes
!---------------------------------------------------------------------

          if(Ub(i,j).gt.Ubc) then 

! Determine skin stresses for pure waves and combined flow (m^2/s^2)
! using Soulsby approximation of the wave friction factor
!  fw = 2*scf1*(znotc/Ab)**scf2;  tauw = fw*Ub^2/2
 
            tau_w = scf1*((znotc*Fwave)**scf2)*(Ub(i,j)**scf3)
 
!  combined wave-current skin stress 
            tau_cw= tau_cs*
     &          (1.+scf4*((tau_w/(tau_w+tau_cs))**scf5))
 
!  Maximum of combined wave-current skin stress component for sediment
            tau_cws = SQRT((tau_cw+tau_w*COS(phicw))**2
     &                          + (tau_w*SIN(phicw))**2) 
            
!            tauw(i,j)=tau_cws             ! [m^2/s^2]
            taucwmax(i,j)=tau_cws
            tauw(i,j)=tau_w                       
!  combined wave-current stress using znot
            tau_w = scf1*((znot*Fwave)**scf2)*(Ub(i,j)**scf3)
    
!  combined wave-current stress 
            tau_cw= tauc(i,j)*
     &          (1.+scf4*((tau_w/(tau_w+tauc(i,j)))**scf5))

# ifdef Z0_BL
!---------------------------------------------------------------------
!  Compute bedload roughness for ripple predictor & sediment purposes.
!  At high transport stages, friction depends on thickness of bedload layer.
!  Shear stress due to combined grain & bedload roughness is to be used 
!  to predict ripples and onset of suspension (Li & Amos 2001)
!---------------------------------------------------------------------

            tau_ex=max((tau_cws-tau_cb),0.0)
            cff=(1./((rhosed-1.)*g*d50))
            znot_bl=17.4*d50*(cff*tau_ex)**0.75
            znotc = znotc + znot_bl

!---------------------------------------------------------------------
!  Compute stresses for sediment purposes [m^2/s^2], 
!  using grain + bedload roughness.
!---------------------------------------------------------------------
            cff1=vonKar/LOG(Zr(i,j)/znotc)
            tau_c = cff1*cff1*Umag(i,j)*Umag(i,j)    
            tau_wb = scf1*((znotc*Fwave)**scf2)*(Ub(i,j)**scf3)
            tau_cw= tau_c*(1.+scf4*((tau_wb/(tau_wb+tau_c))**scf5))
        
!  Maximum of combined wave-current stress component for sediment purposes

            tau_cwb = SQRT((tau_cw+tau_wb*COS(phicw))**2 +
     &                            (tau_wb*SIN(phicw))**2)
            taucwmax(i,j)=tau_cwb
            tauw(i,j)=tau_wb
#  ifdef Z0_RIP        
!---------------------------------------------------------------------
!  Determine bedform roughness  ripple height (m)
!  and ripple length (m) for sandy beds.
!
!  Use structure according to Li & Amos 2001
!  See Li & Amos 2001 for further references
!---------------------------------------------------------------------

! check median grain diameter

             if (d50.ge.0.063d-3) then
    
!enhanced skin stress if pre-exisiting ripples (Nielsen, 1986)
              rhmax=0.8*rlen/pi
              rhgt=max(min(rhmax,rhgt),rhmin)
          
              tau_en=max(tau_cws,tau_cws*(rlen/(rlen-pi*rhgt))**2) 
          
              if ((tau_cws.lt.tau_cb).and.(tau_en.ge.tau_cb)) then 
! local transp.
                rhgt = (19.6*(SQRT(tau_cws/tau_cb))+20.9)*d50
                rlen = rhgt/0.12
              else 
     &          if ((tau_cws.ge.tau_cb).and.(tau_cwb.lt.tau_bf)) then 
! bed load in eq. range
                  rhgt = (22.15*(SQRT(tau_cwb/tau_cb))+6.38)*d50
                  rlen = rhgt/0.12
              else 
     &          if ((tau_cwb.ge.tau_bf).and.(tau_cwb.lt.tau_up)) then 
! break off regime
                  rlen = 535.*d50
                  rhgt = 0.15*rlen*(SQRT(tau_up)-SQRT(tau_cwb))/
     &                             (SQRT(tau_up)-SQRT(tau_bf )) 
              else if  (tau_cwb.ge.tau_up) then 
! sheet flow, plane bed
                rlen = 0.0
                rhgt = 0.0
              else 
! tau_en < tau_cb: no transport, pre-existing conditions
                rhgt=Hripple(i,j) 
                rlen=Lripple(i,j) 
              endif !tau_cws
            endif !d50
#  endif /* Z0_BL */
# endif /* Z0_RIP */       

# ifdef Z0_BIO
!---------------------------------------------------------------------
!  Determine (biogenic) bedform roughness  ripple height (m)
!  and ripple length (m) for silty beds 
!  using Harris & Wiberg 2001.
!---------------------------------------------------------------------
!   rlbio -> user defined or input field...
            if (d50.lt.0.063d-3) then
              rlbio = 0.1d0      ! 10 cm default biogenic r.length (Wheatcroft 1994) 
              thetw = tau_cws*(1./((rhosed-1.)*g*d50))
              rhbio = (thetw**(-1.67d0))*rlbio*rhfac
              rhgt = min(rhbio,rhbiomax)
              rlen = rlbio
            endif
# endif /* Z0_BIO */

# if defined Z0_RIP || defined Z0_BIO
!ripple roughness using Grant & Madsen (1982) roughness length

            znot_rip = 0.92*rhgt*rhgt/(max(rlen,rlmin))
            znotc = znotc+znot_rip
!---------------------------------------------------------------------
! Compute bottom stress components based on total roughness [m/s]^2
!---------------------------------------------------------------------
            cff1  = vonKar/LOG(Zr(i,j)/znotc)
            tau_c = cff1*cff1*Umag(i,j)*Umag(i,j)  
            tau_w = scf1*((znotc*Fwave)**scf2)*(Ub(i,j)**scf3)
            tau_cw= tau_c*(1.+scf4*((tau_w/(tau_w+tau_c))**scf5))

# endif /* Z0_RIP || Z0_BIO */
!---------------------------------------------------------------------
!  Compute effective bottom shear velocity [m/s]
!  relevant for flow and eddy-diffusivities/viscosity
!---------------------------------------------------------------------

            taucw(i,j)=tau_cw
            tauw(i,j)=tau_w
          
          else ! (Ub < Ubc)   

! If current-only: tauw = tauc(skin) for use in sediment.F (ifdef BBL)
! tauc for current still depending on roughness due to current ripples (ifdef Z0_RIP)
            
            taucwmax(i,j)=tauc(i,j)
            tauw(i,j)=0.0

# ifdef Z0_RIP    
            if(tau_cs.gt.tau_up) then
              rhgt=0.0
              rlen=0.0
            else if(tau_cs.lt.tau_cb) then
              rhgt=Hripple(i,j) 
              rlen=Lripple(i,j) 
            else
              rlen=1000.*d50               ! Yalin (1964)
              rhgt=7.4*(rlen/100.)**1.19   ! Allen (1970)
            endif
 
            znotc = znot + 0.92*rhgt*rhgt/(max(rlen,rlmin))

            cff1=vonKar/LOG(Zr(i,j)/znotc)
            cff2=MIN(Cdb_max,MAX(Cdb_min,cff1*cff1))
            taucw(i,j)=cff2*Umag(i,j)*Umag(i,j)
# else
            taucw(i,j)=tauc(i,j)
# endif
          endif 
!---------------------------------------------------------------------
!  Load variables for output purposes.
!---------------------------------------------------------------------

          Abed(i,j)   = Ab
# ifdef MASKING
     &                                   *rmask(i,j)
# endif
          Hripple(i,j)= rhgt
# ifdef MASKING
     &                                   *rmask(i,j)
# endif
          Lripple(i,j)= rlen
# ifdef MASKING
     &                                   *rmask(i,j)
# endif
         Zbnot(i,j)  = znot
# ifdef MASKING
     &                                   *rmask(i,j)
# endif
         Zbapp(i,j) = znotc
# ifdef MASKING
     &                                   *rmask(i,j)
# endif
       enddo
      enddo  ! end main loop

!---------------------------------------------------------------------
!  Compute kinematic bottom stress components for flow
!  due to combined current and wind-induced waves.
!---------------------------------------------------------------------
!
      do j=Jstr,Jend
        do i=IstrU,Iend
          cff1=0.5*(taucw(i-1,j)+taucw(i,j))
          anglec=Ur(i,j)/(0.5*(Umag(i-1,j)+Umag(i,j)+1.e-10))
          bustr(i,j)=cff1*anglec
# ifdef MASKING
     &                                   *umask(i,j)
# endif
        enddo
      enddo

      do j=JstrV,Jend
        do i=Istr,Iend
          cff1=0.5*(taucw(i,j-1)+taucw(i,j))
          anglec=Vr(i,j)/(0.5*(Umag(i,j-1)+Umag(i,j)+1.e-10))
          bvstr(i,j)=cff1*anglec
# ifdef MASKING
     &                                   *vmask(i,j)
# endif
        enddo
      enddo

      do j=Jstr,Jend
        do i=Istr,Iend
          Ucur=0.5*(Ur(i,j)+Ur(i+1,j))
          Vcur=0.5*(Vr(i,j)+Vr(i,j+1))

          anglec=Ucur/(Umag(i,j)+1.e-10)
# ifdef WKB_WWAVE
          anglew=cos(Dwave(i,j)
# else
          anglew=cos(1.5*pi-Dwave(i,j)
# endif
# ifdef CURVGRID
     &     -angler(i,j)
# endif          
     &     )
          bustrc(i,j)=taucw(i,j)*anglec
          bustrw(i,j)=tauw(i,j)*anglew
          bustrcwmax(i,j)=taucwmax(i,j)*anglew
!          Ubot(i,j)=Ub(i,j)*anglew
!          Ur(i,j)=Ucur
!
          anglec=Vcur/(Umag(i,j)+1.e-10)
# ifdef WKB_WWAVE
          anglew=sin(Dwave(i,j)
# else
          anglew=sin(1.5*pi-Dwave(i,j)
# endif
# ifdef CURVGRID
     &     -angler(i,j)
# endif          
     &     )
          bvstrc(i,j)=taucw(i,j)*anglec
          bvstrw(i,j)=tauw(i,j)*anglew
          bvstrcwmax(i,j)=taucwmax(i,j)*anglew
!          Vbot(i,j)=Ub(i,j)*anglew
!          Vr(i,j)=Vcur

        enddo
      enddo

# ifdef LIMIT_BSTRESS
!---------------------------------------------------------------------
!  From J. Warner's code:
!  Set limiting factor for bottom stress. The bottom stress is adjusted
!  to not change the direction of momentum.  It only should slow down
!  to zero.  The value of 0.75 is arbitrary limitation assigment.
!---------------------------------------------------------------------
      cff=0.75/dt
      do j=Jstr,Jend
        do i=IstrU,Iend
          cff1=cff*0.5*(Zr(i-1,j)+Zr(i,j))
          bustr(i,j)=SIGN(1.0, bustr(i,j))* 
     &               MIN(ABS(bustr(i,j)),
     &                   ABS(Ur(i,j))*cff1)
        enddo
      enddo
      do j=JstrV,Jend
        do i=Istr,Iend
          cff1=cff*0.5*(Zr(i,j-1)+Zr(i,j))
          bvstr(i,j)=SIGN(1.0, bvstr(i,j))*
     &               MIN(ABS(bvstr(i,j)),
     &                   ABS(Vr(i,j))*cff1)
        enddo
      enddo
# endif

!---------------------------------------------------------------------
! Set boundary conditions
!---------------------------------------------------------------------
# ifndef EW_PERIODIC
      IF (EASTERN_EDGE) THEN
        DO j=Jstr,Jend
          bustr(Iend+1,j )=bustr(Iend,j)
        END DO
        DO j=JstrV,Jend
          bvstr(Iend+1,j )=bvstr(Iend,j)
        END DO
        !
        DO j=Jstr,Jend
          bustrc(Iend+1,j )=bustrc(Iend,j)
          bustrw(Iend+1,j )=bustrw(Iend,j)
          bustrcwmax(Iend+1,j )=bustrcwmax(Iend,j)
          bvstrc(Iend+1,j )=bvstrc(Iend,j)
          bvstrw(Iend+1,j )=bvstrw(Iend,j)
          bvstrcwmax(Iend+1,j )=bvstrcwmax(Iend,j)
        END DO
      END IF
      IF (WESTERN_EDGE) THEN
        DO j=Jstr,Jend
          bustr(IstrU-1,j)=bustr(IstrU,j)
        END DO
        DO j=JstrV,Jend
          bvstr(Istr-1,j)=bvstr(Istr,j)
        END DO
        DO j=Jstr,Jend
          bustrc(Istr-1,j)=bustrc(Istr,j)
          bustrw(Istr-1,j)=bustrw(Istr,j)
          bustrcwmax(Istr-1,j)=bustrcwmax(Istr,j)
          bvstrc(Istr-1,j)=bvstrc(Istr,j)
          bvstrw(Istr-1,j)=bvstrw(Istr,j)
          bvstrcwmax(Istr-1,j)=bvstrcwmax(Istr,j)
        END DO        
      END IF
# endif
# ifndef NS_PERIODIC
      IF (NORTHERN_EDGE) THEN
        DO i=IstrU,Iend
          bustr(i,Jend+1) =bustr(i,Jend)
        END DO
        DO i=Istr,Iend
          bvstr(i,Jend+1) =bvstr(i,Jend)
        END DO
        DO i=Istr,Iend
          bustrc(i,Jend+1) =bustrc(i,Jend)
          bustrw(i,Jend+1) =bustrw(i,Jend)
          bustrcwmax(i,Jend+1) =bustrcwmax(i,Jend)
          bvstrc(i,Jend+1) =bvstrc(i,Jend)
          bvstrw(i,Jend+1) =bvstrw(i,Jend)
          bvstrcwmax(i,Jend+1) =bvstrcwmax(i,Jend)
        END DO
      END IF
      IF (SOUTHERN_EDGE) THEN
        DO i=IstrU,Iend
          bustr(i,Jstr-1)=bustr(i,Jstr)
        END DO
        DO i=Istr,Iend
          bvstr(i,JstrV-1)=bvstr(i,JstrV)
        END DO
        DO i=Istr,Iend
          bustrc(i,Jstr-1)=bustrc(i,Jstr)
          bustrw(i,Jstr-1)=bustrw(i,Jstr)
          bustrcwmax(i,Jstr-1)=bustrcwmax(i,Jstr)
          bvstrc(i,Jstr-1)=bvstrc(i,Jstr)
          bvstrw(i,Jstr-1)=bvstrw(i,Jstr)
          bvstrcwmax(i,Jstr-1)=bvstrcwmax(i,Jstr)
        END DO
      END IF
# endif
# if !defined EW_PERIODIC && !defined NS_PERIODIC
      IF (SOUTHERN_EDGE.and.WESTERN_EDGE) THEN
        bustr(Istr,Jstr-1)=0.5*(bustr(Istr+1,Jstr-1)+bustr(Istr,Jstr))
        bvstr(Istr-1,Jstr)=0.5*(bvstr(Istr,Jstr)+bvstr(Istr-1,Jstr+1))
        bustrc(Istr-1,Jstr-1)=0.5*(bustrc(Istr,Jstr-1)+bustrc(Istr-1,Jstr))
        bvstrc(Istr-1,Jstr-1)=0.5*(bvstrc(Istr,Jstr-1)+bvstrc(Istr-1,Jstr))
        bustrw(Istr-1,Jstr-1)=0.5*(bustrw(Istr,Jstr-1)+bustrw(Istr-1,Jstr))
        bvstrw(Istr-1,Jstr-1)=0.5*(bvstrw(Istr,Jstr-1)+bvstrw(Istr-1,Jstr))
        bustrcwmax(Istr-1,Jstr-1)=0.5*(bustrcwmax(Istr,Jstr-1)
     &   +bustrcwmax(Istr-1,Jstr))
        bvstrcwmax(Istr-1,Jstr-1)=0.5*(bvstrcwmax(Istr,Jstr-1)
     &   +bvstrcwmax(Istr-1,Jstr))
      END IF
      IF (SOUTHERN_EDGE.and.EASTERN_EDGE) THEN
        bustr(Iend+1,Jstr-1)=0.5*(bustr(Iend+1,Jstr)+bustr(Iend,Jstr-1))
        bvstr(Iend+1,Jstr)=0.5*(bvstr(Iend+1,Jstr+1)+bvstr(Iend,Jstr))
        bustrc(Iend+1,Jstr-1)=0.5*(bustrc(Iend+1,Jstr)+bustrc(Iend,Jstr-1))
        bvstrc(Iend+1,Jstr-1)=0.5*(bvstrc(Iend+1,Jstr)+bvstrc(Iend,Jstr-1))
        bustrw(Iend+1,Jstr-1)=0.5*(bustrw(Iend+1,Jstr)+bustrw(Iend,Jstr-1))
        bvstrw(Iend+1,Jstr-1)=0.5*(bvstrw(Iend+1,Jstr)+bvstrw(Iend,Jstr-1))
        bustrcwmax(Iend+1,Jstr-1)=0.5*(bustrcwmax(Iend+1,Jstr)
     &   +bustrcwmax(Iend,Jstr-1))
        bvstrcwmax(Iend+1,Jstr-1)=0.5*(bvstrcwmax(Iend+1,Jstr)
     &   +bvstrcwmax(Iend,Jstr-1))
      END IF
      IF (NORTHERN_EDGE.and.WESTERN_EDGE) THEN
        bustr(Istr,Jend+1)=0.5*(bustr(Istr,Jend)+bustr(Istr+1,Jend+1))
        bvstr(Istr-1,Jend+1)=0.5*(bvstr(Istr-1,Jend)+bvstr(Istr,Jend+1))
        bustrc(Istr-1,Jend+1)=0.5*(bustrc(Istr-1,Jend)+bustrc(Istr,Jend+1))
        bvstrc(Istr-1,Jend+1)=0.5*(bvstrc(Istr-1,Jend)+bvstrc(Istr,Jend+1))
        bustrw(Istr-1,Jend+1)=0.5*(bustrw(Istr-1,Jend)+bustrw(Istr,Jend+1))
        bvstrw(Istr-1,Jend+1)=0.5*(bvstrw(Istr-1,Jend)+bvstrw(Istr,Jend+1))
        bustrcwmax(Istr-1,Jend+1)=0.5*(bustrcwmax(Istr-1,Jend)
     &   +bustrcwmax(Istr,Jend+1))
        bvstrcwmax(Istr-1,Jend+1)=0.5*(bvstrcwmax(Istr-1,Jend)
     &   +bvstrcwmax(Istr,Jend+1))
      END IF
      IF (NORTHERN_EDGE.and.EASTERN_EDGE) THEN
        bustr(Iend+1,Jend+1)=0.5*(bustr(Iend+1,Jend)+bustr(Iend,Jend+1))
        bvstr(Iend+1,Jend+1)=0.5*(bvstr(Iend+1,Jend)+bvstr(Iend,Jend+1))
        bustrc(Iend+1,Jend+1)=0.5*(bustrc(Iend+1,Jend)+bustrc(Iend,Jend+1))
        bvstrc(Iend+1,Jend+1)=0.5*(bvstrc(Iend+1,Jend)+bvstrc(Iend,Jend+1))
        bustrw(Iend+1,Jend+1)=0.5*(bustrw(Iend+1,Jend)+bustrw(Iend,Jend+1))
        bvstrw(Iend+1,Jend+1)=0.5*(bvstrw(Iend+1,Jend)+bvstrw(Iend,Jend+1))
        bustrcwmax(Iend+1,Jend+1)=0.5*(bustrcwmax(Iend+1,Jend)
     &   +bustrcwmax(Iend,Jend+1))
        bvstrcwmax(Iend+1,Jend+1)=0.5*(bvstrcwmax(Iend+1,Jend)
     &   +bvstrcwmax(Iend,Jend+1))
      END IF
# endif
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,Abed(START_2D_ARRAY))
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,Hripple(START_2D_ARRAY))
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,Lripple(START_2D_ARRAY))
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,Zbnot(START_2D_ARRAY))
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,Zbapp(START_2D_ARRAY))
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,bustr(START_2D_ARRAY))
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,bvstr(START_2D_ARRAY))
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,bustrc(START_2D_ARRAY))
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,bvstrc(START_2D_ARRAY))
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,bustrw(START_2D_ARRAY))
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,bvstrw(START_2D_ARRAY))
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,bustrcwmax(START_2D_ARRAY))
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,bvstrcwmax(START_2D_ARRAY))
# endif

      return
      end
!====================================================================!
#else
      subroutine bblm_empty
      end
#endif

