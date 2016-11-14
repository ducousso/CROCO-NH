!
!======================================================================
! CROCO is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al)
! and Rutgers University (Arango et al) are under MIT/X style license.
! CROCO specific routines (nesting) are under CeCILL-C license.
!
! CROCO website : http://www.croco-ocean.org/
!
! These Sediment Dynamics routines are based on the Community Sediment 
! Transport Model (CSTM) as implemented in ROMS by J. Warner (USGS)
!
! Adapted from John Warner's code to CROCO by R. Benshila, CNRS 2015
! -- modifs: P. Marchesiello, Dec 2015
!
!======================================================================
!
#include "cppdefs.h"
#ifdef SEDIMENT

# ifdef BEDLOAD
#  define bnew nnew
# else
#  define bnew nstp      
# endif  /* BEDLOAD */
# ifndef BBL_MODEL
#  define BSTRESS_UPWIND
# endif
!
# ifdef MOVING_BATHY
#  define MORPH_FAC *morph_fac
# else
#  define MORPH_FAC 
# endif  /* MOVING_BATHY */
!
# undef  LINEAR_CONTINUATION /* for settling scheme */
# undef  NEUMANN
!
      subroutine sediment (tile)
!
!=======================================================================
!                                                                      !
!  This routine it is the main driver for the sediment-transport       !
!  model. Currently, it includes calls to the following routines:      !
!                                                                      !
!  * Vertical settling of sediment in the water column.                !
!  * Erosive and depositional flux interactions of sediment            !
!    between water column and the bed.                                 !
!  * Transport of multiple grain sizes.                                !
!  * Bed layer stratigraphy.                                           !
!  * Bed morphology.                                                   !
!  * Bedload based on Meyer Peter Mueller.                             !
!  * Bedload based on Soulsby combined waves + currents                !
!    (p166 Soulsby 1997)                                               !
!  * Bedload slope term options: Nemeth et al, 2006, Coastal           !
!    Engineering, v 53, p 265-275; Lesser et al, 2004, Coastal         !
!    Engineering, v 51, p 883-915.                                     !
!                                                                      !
!  * Seawater/sediment vertical level distribution:                    !
!                                                                      !
!         W-level  RHO-level                                           !
!                                                                      !
!            N     _________                                           !
!                 |         |                                          !
!                 |    N    |                                          !
!          N-1    |_________|  S                                       !
!                 |         |  E                                       !
!                 |   N-1   |  A                                       !
!            2    |_________|  W                                       !
!                 |         |  A                                       !
!                 |    2    |  T                                       !
!            1    |_________|  E                                       !
!                 |         |  R                                       !
!                 |    1    |                                          !
!            0    |_________|_____ bathymetry                          !
!                 |/////////|                                          !
!                 |    1    |                                          !
!            1    |_________|  S                                       !
!                 |         |  E                                       !
!                 |    2    |  D                                       !
!            2    |_________|  I                                       !
!                 |         |  M                                       !
!                 |  NLAY-1 |  E                                       !
!        Nbed-1   |_________|  N                                       !
!                 |         |  T                                       !
!                 |  NLAY   |                                          !
!         Nbed    |_________|                                          !
!                                                                      !
!  References:                                                         !
!                                                                      !
!  Warner, J.C., C.R. Sherwood, R.P. Signell, C.K. Harris, and H.G.    !
!    Arango, 2008:  Development of a three-dimensional,  regional,     !
!    coupled wave, current, and sediment-transport model, Computers    !
!    & Geosciences, 34, 1284-1306.                                     !
!                                                                      !
!=======================================================================

      implicit none
# include "param.h"
# include "scalars.h"
# include "sediment.h"
# include "private_scratch.h"
      integer tile, trd, omp_get_thread_num
# include "compute_tile_bounds.h"
!
      trd=omp_get_thread_num()
# ifdef BEDLOAD
      call sed_bedload_tile (Istr,Iend,Jstr,Jend,
     &                       A2d(1,1,trd), A2d(1,2,trd), 
     &                       A2d(1,3,trd), A2d(1,4,trd),
     &                       A2d(1,5,trd)     
#  ifdef BEDLOAD_MPM
     &                     , A2d(1,6,trd),A2d(1,7,trd)
     &                     , A2d(1,8,trd),A2d(1,9,trd)     
#  endif
#  ifdef BEDLOAD_SOULSBY
     &                     , A2d(1,6,trd),A2d(1,7,trd)     
     &                     , A2d(1,8,trd),A2d(1,9,trd)     
#  endif      
     &  )
C$OMP BARRIER
# endif /* BEDLOAD */
!
# ifdef SUSPLOAD
      call sed_settling_tile (Istr,Iend,Jstr,Jend, 
     &                        A2d(1,1,trd), A2d(1,2,trd),
     &                        A2d(1,3,trd), A2d(1,4,trd),
     &                        A2d(1,5,trd), A2d(1,6,trd),
     &                        A2d(1,7,trd), A2d(1,8,trd),
     &                        A2d(1,9,trd),
     &                        B2d(1,trd) )
C$OMP BARRIER
      call sed_fluxes_tile (Istr,Iend,Jstr,Jend,A2d(1,1,trd))
C$OMP BARRIER
# endif /* SUSPLOAD */
!
      call sed_bed_tile (Istr,Iend,Jstr,Jend,
     &                   A2d(1,1,trd), A2d(1,2,trd) ,
     &                   A2d(1,3,trd), A2d(1,4,trd) )
C$OMP BARRIER
!
      call sed_surf_tile (Istr,Iend,Jstr,Jend)     
C$OMP BARRIER
!
      return
      end
      
# ifdef BEDLOAD
!======================================================================
!                                                                      !
!  This routine computes sediment bedload transport using the Meyer-   !
!  Peter and Muller (1948) formulation  for unidirectional flow and    !
!  Souksby and Damgaard (2005) algorithm that accounts for combined    !
!  effect of currents and waves.                                       !
!                                                                      !
!  References:                                                         !
!                                                                      !
!  Meyer-Peter, E. and R. Muller, 1948: Formulas for bedload transport !
!    In: Report on the 2nd Meeting International Association Hydraulic !
!    Research, Stockholm, Sweden, pp 39-64.                            !
!                                                                      !
!  Soulsby, R.L. and J.S. Damgaard, 2005: Bedload sediment transport   !
!    in coastal waters, Coastal Engineering, 52 (8), 673-689.          !
!                                                                      !
!  Warner, J.C., C.R. Sherwood, R.P. Signell, C.K. Harris, and H.G.    !
!    Arango, 2008:  Development of a three-dimensional,  regional,     !
!    coupled wave, current, and sediment-transport model, Computers    !
!    & Geosciences, 34, 1284-1306.                                     !
!                                                                      !
!======================================================================
!
      subroutine sed_bedload_tile (Istr, Iend, Jstr, Jend,
     &                             tau_w,FX,FE,FX_r,FE_r
#  ifdef BEDLOAD_MPM
     &                            ,tau_wX,tau_wE
     &                            ,angleu,anglev
#  endif
#  ifdef BEDLOAD_SOULSBY
     &                            ,phic,phicw,tau_wav,tau_mean 
#  endif      
     &                            )
!
      implicit none
      integer Istr,Iend,Jstr,Jend
      real tau_w(PRIVATE_2D_SCRATCH_ARRAY)
      real FX(PRIVATE_2D_SCRATCH_ARRAY)
      real FE(PRIVATE_2D_SCRATCH_ARRAY)
      real FX_r(PRIVATE_2D_SCRATCH_ARRAY)
      real FE_r(PRIVATE_2D_SCRATCH_ARRAY)
#  ifdef BEDLOAD_MPM
      real tau_wX(PRIVATE_2D_SCRATCH_ARRAY)
      real tau_wE(PRIVATE_2D_SCRATCH_ARRAY)
      real angleu(PRIVATE_2D_SCRATCH_ARRAY)
      real anglev(PRIVATE_2D_SCRATCH_ARRAY)
#  endif
#  ifdef BEDLOAD_SOULSBY
      real phic(PRIVATE_2D_SCRATCH_ARRAY)
      real phicw(PRIVATE_2D_SCRATCH_ARRAY)
      real tau_wav(PRIVATE_2D_SCRATCH_ARRAY)
      real tau_mean(PRIVATE_2D_SCRATCH_ARRAY)
#  endif
      integer i, j, k, ised
      real eps 
      real cff, cff1, cff2, cff3, cff4, cff5
      real a_slopex, a_slopey, sed_angle
      real bedld, bedld_mass, dzdx, dzdy
      real smgd, smgdr, osmgd, Umag
      real Ua, Ra, phi, Clim
#  ifdef BEDLOAD_SOULSBY
      real theta_mean, theta_wav, w_asym
      real theta_max, theta_max1, theta_max2
      real phi_x1, phi_x2, phi_x, phi_y, Dstp
      real bedld_x, bedld_y, tau_cur, waven, wavec
#  endif
      real kdmax,Kbh,Kbh2,Kdh,Fwave
      real rhs_bed, bed_change
      integer imin,imax,jmin,jmax
      parameter (eps = 1.e-20)
      parameter (kdmax = 100.)
      real K1, K2, K3, K4, K5, K6
      parameter (K1=0.6666666666, K2=0.3555555555,
     &           K3=0.1608465608, K4=0.0632098765,
     &           K5=0.0217540484, K6=0.0065407983)
#  include "param.h"
#  include "scalars.h"
#  include "grid.h"
#  include "ocean3d.h"
#  include "sediment.h"
#  include "forces.h"
#  ifdef WKB_WWAVE
#   include "wkb_wwave.h"
#  endif
#  include "bbl.h"
#  include "compute_auxiliary_bounds.h"

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
#  endif /* EW_PERIODIC */
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
#  endif /* NS_PERIODIC */
!
!-----------------------------------------------------------------------
! Compute maximum bottom stress for MPM bedload
!-----------------------------------------------------------------------
!
#  ifdef BEDLOAD_MPM 
#   ifdef BBL
      do j=Jstr-1,Jend+1
        do i=Istr-1,Iend+1
          tau_w(i,j)=sqrt( bustrcwmax(i,j)**2
     &                   + bvstrcwmax(i,j)**2)
       enddo
      enddo
#   else
      do j=J_EXT_RANGE
        do i=I_EXT_RANGE
#    ifdef BSTRESS_UPWIND
            cff1=0.5*(1.0+SIGN(1.0,bustr(i+1,j)))
            cff2=0.5*(1.0-SIGN(1.0,bustr(i+1,j)))
            cff3=0.5*(1.0+SIGN(1.0,bustr(i  ,j)))
            cff4=0.5*(1.0-SIGN(1.0,bustr(i  ,j)))
            tau_wX(i,j)=cff3*(cff1*bustr(i,j)+                          
     &                  cff2*0.5*(bustr(i,j)+bustr(i+1,j)))+         
     &                  cff4*(cff2*bustr(i+1,j)+                        
     &                  cff1*0.5*(bustr(i,j)+bustr(i+1,j)))
            cff1=0.5*(1.0+SIGN(1.0,bvstr(i,j+1)))
            cff2=0.5*(1.0-SIGN(1.0,bvstr(i,j+1)))
            cff3=0.5*(1.0+SIGN(1.0,bvstr(i,j)))
            cff4=0.5*(1.0-SIGN(1.0,bvstr(i,j)))
            tau_wE(i,j)=cff3*(cff1*bvstr(i,j)+                          
     &                  cff2*0.5*(bvstr(i,j)+bvstr(i,j+1)))+         
     &                  cff4*(cff2*bvstr(i,j+1)+                        
     &                  cff1*0.5*(bvstr(i,j)+bvstr(i,j+1)))
#    endif
            tau_w(i,j)=0.5*SQRT((bustr(i,j)+bustr(i+1,j))*             
     &                          (bustr(i,j)+bustr(i+1,j))+             
     &                          (bvstr(i,j)+bvstr(i,j+1))*             
     &                          (bvstr(i,j)+bvstr(i,j+1)))
       enddo
      enddo
#   endif /* BBL */
#  endif /* BEDLOAD_MPM */
!
!-----------------------------------------------------------------------
!  Compute bedload sediment transport.
!-----------------------------------------------------------------------
!
! Compute some constant bed slope parameters.
!
      sed_angle=DTAN(33.0*pi/180.0)
!
!  Compute angle between currents and waves (radians).
!
      do j=J_EXT_RANGE
       do i=I_EXT_RANGE
#  ifdef BEDLOAD_SOULSBY
!
! Compute angle between currents and waves, measure CCW from current
! direction toward wave vector.
!
          if (bustrc(i,j).eq.0.0) then
            phic(i,j)=0.5*pi*SIGN(1.0,bvstrc(i,j))
          else
            phic(i,j)=ATAN2(bvstrc(i,j),bustrc(i,j))
          endif
#   ifdef WKB_WWAVE
          phicw(i,j)=Dwave(i,j)-phic(i,j)
#   else
          phicw(i,j)=1.5*pi-Dwave(i,j)-phic(i,j)
#   endif
#   ifdef CURVGRID
     &                              -angler(i,j)
#   endif
!
! Compute stress components at rho points.
!
          tau_cur=SQRT(bustrc(i,j)*bustrc(i,j)+  
     &                 bvstrc(i,j)*bvstrc(i,j))
          tau_wav(i,j)=SQRT(bustrw(i,j)*bustrw(i,j)+ 
     &                      bvstrw(i,j)*bvstrw(i,j))
          tau_mean(i,j)=tau_cur*(1.0+1.2*((tau_wav(i,j)/  
     &                  (tau_cur+tau_wav(i,j)+eps))**3.2))
!
#  elif defined BEDLOAD_MPM && !defined BSTRESS_UPWIND
          cff1=0.5*(bustr(i,j)+bustr(i+1,j))
          cff2=0.5*(bvstr(i,j)+bvstr(i,j+1))
          Umag=SQRT(cff1*cff1+cff2*cff2)+eps
          angleu(i,j)=cff1/Umag
          anglev(i,j)=cff2/Umag
#  endif /* BEDLOAD_SOULSBY or BEDLOAD_MPM */
        enddo
      enddo
!
      do ised=1,NST
        smgd=(Srho(ised)/rho0-1.0)*g*Sd(ised)
        osmgd=1.0/smgd 
        smgdr=SQRT(smgd)*Sd(ised)*Srho(ised)
!
        do j=J_EXT_RANGE
          do i=I_EXT_RANGE
#  ifdef BEDLOAD_SOULSBY
!
! Compute wave asymmetry factor, based on Fredosoe and Deigaard.
!
            Dstp=z_w(i,j,N)+h(i,j)  
#   ifdef WKB_WWAVE
            waven=max(wvn(i,j,wnew),eps)
#   else
            Fwave=2*pi/Pwave(i,j)
            Kdh=Dstp*Fwave*Fwave/g
            Kbh2=Kdh*Kdh+
     &      Kdh/(1.0+Kdh*(K1+Kdh*(K2+Kdh*(K3+Kdh*(K4+
     &      Kdh*(K5+K6*Kdh))))))
            waven = SQRT(Kbh2)/Dstp           
#   endif
            wavec=SQRT(g/waven*tanh(waven*Dstp))
            cff4=MIN(waven*Dstp,kdmax)
            cff1=-0.1875*wavec*(waven*Dstp)**2/(SINH(cff4))**4
            cff2=0.125*g*(2*Awave(i,j))**2/(wavec*Dstp+eps)
            cff3=pi*2*Max(Awave(i,j),eps)/(Pwave(i,j)*SINH(cff4)+eps)
!
! Compute wave asymmetry factor, based on the note of Soulsby
!
            cff1=MIN(0.375*(2*Awave(i,j)/Dstp)*         
     &                    ((waven*Dstp)/(SINH(cff4))**3),0.15)
            w_asym=2.0_r8*cff1/(1.0+cff1**2.)
!
! Compute nondimensional stresses.
!
            theta_wav=tau_wav(i,j)*osmgd+eps
            theta_mean=tau_mean(i,j)*osmgd
!
            cff1=theta_wav*(1.0+w_asym)
            cff2=theta_wav*(1.0-w_asym)
            theta_max1=SQRT((theta_mean+                       
     &                       cff1*COS(phicw(i,j)))**2+    
     &                      (cff1*SIN(phicw(i,j)))**2)
            theta_max2=SQRT((theta_mean+                 
     &                       cff2*COS(phicw(i,j)+pi))**2+ 
     &                      (cff2*SIN(phicw(i,j)+pi))**2)
            theta_max=MAX(theta_max1,theta_max2)
!
! Motion initiation factor.
!
            cff3=0.5*(1.0+SIGN(1.0,                            
     &                   theta_max/(tau_ce(ised)*osmgd)-1.0))
!
! Compute bed loads in parallel and perpendicular direction of currents
!
            phi_x1=12.*SQRT(theta_mean)*                    
     &             MAX((theta_mean-tau_ce(ised)*osmgd),0.)
            phi_x2=12.*(0.9534+0.1907*COS(2.*phicw(i,j)))* 
     &             SQRT(theta_wav)*theta_mean+                  
     &             12.*(0.229*w_asym*theta_wav**1.5*  
     &                      COS(phicw(i,j)))
!           phi_x=MAX(phi_x1,phi_x2) !  <- original
            IF (ABS(phi_x2).gt.phi_x1) THEN
              phi_x=phi_x2
            ELSE
              phi_x=phi_x1
            END IF
            bedld_x=phi_x*smgdr*cff3
!
            cff5=theta_wav**1.5+1.5*(theta_mean**1.5)
            phi_y=12.0*0.1907*theta_wav*theta_wav*       
     &            (theta_mean*SIN(2.0*phicw(i,j))+1.2*w_asym*  
     &            SIN(phicw(i,j)))/cff5*cff3
            bedld_y=phi_y*smgdr
!
! Partition bedld into xi and eta directions, still at rho points.
! FX_r and FE_r have dimensions of kg.
!
            FX_r(i,j)=(bedld_x*COS(phic(i,j))-bedld_y*SIN(phic(i,j)))* 
     &                                                    on_r(i,j)*dt
            FE_r(i,j)=(bedld_x*SIN(phic(i,j))+bedld_y*COS(phic(i,j)))*
     &                                                    om_r(i,j)*dt
#  elif defined BEDLOAD_MPM
#   ifdef BSTRESS_UPWIND
!
! Magnitude of bed load at rho points. Meyer-Peter Muller formulation.
! bedld has dimensions of kg m-1 s-1. Use partitions of stress 
! from upwind direction, still at rho points.
! FX_r and FE_r have dimensions of kg.
!
            bedld=8.0*(MAX((ABS(tau_wX(i,j))*osmgd-0.047),0.0)**1.5)
     &                                   *smgdr*IGN(1.0,tau_wX(i,j))
            FX_r(i,j)=bedld*on_r(i,j)*dt
            bedld=8.0*(MAX((ABS(tau_wE(i,j))*osmgd-0.047),0.0)**1.5)
     &                                   *smgdr*IGN(1.0,tau_wE(i,j))
            FE_r(i,j)=bedld*om_r(i,j)*dt
#   else
!
! Magnitude of bed load at rho points. Meyer-Peter Muller formulation.
! (BEDLD has dimensions of kg m-1 s-1).
!
            bedld=8.0*(MAX((tau_w(i,j)*osmgd-0.047),0.0)**1.5)*smgdr
!
! Partition bedld into xi and eta directions, still at rho points.
! (FX_r and FE_r have dimensions of kg).
!
            FX_r(i,j)=angleu(i,j)*bedld*on_r(i,j)*dt(ng)
            FE_r(i,j)=anglev(i,j)*bedld*om_r(i,j)*dt(ng)
#   endif /* BSTRESS_UPWIND */
#  endif /* BEDLOAD_SOULSBY or BEDLOAD_MPM */
!
! Correct for along-direction slope. Limit slope to 0.9*sed angle.
!
            cff1=0.5*(1.0+SIGN(1.0,FX_r(i,j)))
            cff2=0.5*(1.0-SIGN(1.0,FX_r(i,j)))
            cff3=0.5*(1.0+SIGN(1.0,FE_r(i,j)))
            cff4=0.5*(1.0-SIGN(1.0,FE_r(i,j)))
#  ifdef SLOPE_NEMETH
            dzdx=(h(i+1,j)-h(i  ,j))/om_u(i+1,j)*cff1+       
     &           (h(i-1,j)-h(i  ,j))/om_u(i  ,j)*cff2
            dzdy=(h(i,j+1)-h(i,j  ))/on_v(i,j+1)*cff3+      
     &           (h(i,j-1)-h(i,j  ))/on_v(i  ,j)*cff4

            a_slopex=1.7*dzdx
            a_slopey=1.7*dzdy
!
! Add contriubiton of bed slope to bed load transport fluxes.
!
            FX_r(i,j)=FX_r(i,j)*(1.0+a_slopex)
            FE_r(i,j)=FE_r(i,j)*(1.0+a_slopey)
!
#  elif defined SLOPE_LESSER
            dzdx=MIN(((h(i+1,j)-h(i  ,j))/om_u(i+1,j)*cff1+      
     &                (h(i  ,j)-h(i-1,j))/om_u(i  ,j)*cff2),0.52)* 
     &                SIGN(1.0,FX_r(i,j))
            dzdy=MIN(((h(i,j+1)-h(i,j  ))/on_v(i,j+1)*cff3+      
     &                (h(i,j  )-h(i,j-1))/on_v(i  ,j)*cff4),0.52)* 
     &                SIGN(1.0,FE_r(i,j))
            cff=ATAN(dzdx)
            a_slopex=sed_angle/(COS(cff)*(sed_angle-dzdx))
            cff=ATAN(dzdy)
            a_slopey=sed_angle/(COS(cff)*(sed_angle-dzdy))
!
! Add contribution of bed slope to bed load transport fluxes.
!
            FX_r(i,j)=FX_r(i,j)*a_slopex
            FE_r(i,j)=FE_r(i,j)*a_slopey
#  endif /* SLOPE_NEMETH */
!
#  ifdef MOVING_BATHY
!
! Apply morphology factor.
!
            FX_r(i,j)=FX_r(i,j)*morph_fac
            FE_r(i,j)=FE_r(i,j)*morph_fac
#  endif
!
! Apply bedload transport rate coefficient. Also limit
! bedload to the fraction of each sediment class.
!
            FX_r(i,j)=FX_r(i,j)*bedload_coeff*bed_frac(i,j,1,ised)
            FE_r(i,j)=FE_r(i,j)*bedload_coeff*bed_frac(i,j,1,ised)
!
! Limit bed load to available bed mass.
!
            bedld_mass=ABS(FX_r(i,j))+ABS(FE_r(i,j))+eps
            FX_r(i,j)=MIN(ABS(FX_r(i,j)),                      
     &                    bed_mass(i,j,1,nstp,ised)*           
     &                    om_r(i,j)*on_r(i,j)*ABS(FX_r(i,j))/    
     &                    bedld_mass)*                        
     &                    SIGN(1.0,FX_r(i,j))
            FE_r(i,j)=MIN(ABS(FE_r(i,j)),                    
     &                    bed_mass(i,j,1,nstp,ised)*          
     &                    om_r(i,j)*on_r(i,j)*ABS(FE_r(i,j))/     
     &                    bedld_mass)*             
     &                    SIGN(1.0,FE_r(i,j))
#  ifdef MASKING
            FX_r(i,j)=FX_r(i,j)*rmask(i,j)
            FE_r(i,j)=FE_r(i,j)*rmask(i,j)
#  endif
#  ifdef WET_DRY
            FX_r(i,j)=FX_r(i,j)*rmask_wet(i,j)
            FE_r(i,j)=FE_r(i,j)*rmask_wet(i,j)
#  endif
          enddo
        enddo

#  ifndef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=J_EXT_RANGE
            FX_r(Istr-1,j)=FX_r(Istr,j)
            FE_r(Istr-1,j)=FE_r(Istr,j)
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=J_EXT_RANGE
            FX_r(Iend+1,j)=FX_r(Iend,j)
            FE_r(Iend+1,j)=FE_r(Iend,j)
          enddo
        endif
#  endif
#  ifndef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=I_EXT_RANGE
            FX_r(i,Jstr-1)=FX_r(i,Jstr)
            FE_r(i,Jstr-1)=FE_r(i,Jstr)
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=I_EXT_RANGE
            FX_r(i,Jend+1)=FX_r(i,Jend)
            FE_r(i,Jend+1)=FE_r(i,Jend)
          enddo
        endif
#  endif

#  if !defined EW_PERIODIC && !defined NS_PERIODIC
        if ((SOUTHERN_EDGE).and.(WESTERN_EDGE)) then
          FX_r(Istr-1,Jstr-1)=0.5*(FX_r(Istr  ,Jstr-1)+     
     &                                FX_r(Istr-1,Jstr  ))
          FE_r(Istr-1,Jstr-1)=0.5*(FE_r(Istr  ,Jstr-1)+       
     &                                FE_r(Istr-1,Jstr  ))
        endif
        if ((SOUTHERN_EDGE).and.(EASTERN_EDGE)) then
          FX_r(Iend+1,Jstr-1)=0.5*(FX_r(Iend  ,Jstr-1)+      
     &                                FX_r(Iend+1,Jstr  ))
          FE_r(Iend+1,Jstr-1)=0.5*(FE_r(Iend  ,Jstr-1)+       
     &                                FE_r(Iend+1,Jstr  ))
        endif
        if ((NORTHERN_EDGE).and.(WESTERN_EDGE)) then
          FX_r(Istr-1,Jend+1)=0.5*(FX_r(Istr-1,Jend  )+       
     &                                FX_r(Istr  ,Jend+1))
          FE_r(Istr-1,Jend+1)=0.5*(FE_r(Istr-1,Jend  )+      
     &                                FE_r(Istr  ,Jend+1))
        endif
        if ((NORTHERN_EDGE).and.(EASTERN_EDGE)) then
          FX_r(Iend+1,Jend+1)=0.5*(FX_r(Iend+1,Jend  )+      
     &                                FX_r(Iend  ,Jend+1))
          FE_r(Iend+1,Jend+1)=0.5*(FE_r(Iend+1,Jend  )+    
     &                                FE_r(Iend  ,Jend+1))
        endif
#  endif

#  undef I_EXT_RANGE
#  undef J_EXT_RANGE
!
! Compute face fluxes at u and v points before taking divergence.
!
        do j=Jstr,Jend
          do i=Istr,Iend+1
            cff1=0.5*(1.0+SIGN(1.0,FX_r(i,j)))
            cff2=0.5*(1.0-SIGN(1.0,FX_r(i,j)))
            FX(i,j)=0.5*(1.0+SIGN(1.0,FX_r(i-1,j)))*     
     &              (cff1*FX_r(i-1,j)+                    
     &               cff2*0.5*(FX_r(i-1,j)+FX_r(i,j)))+   
     &              0.5*(1.0-SIGN(1.0,FX_r(i-1,j)))*    
     &              (cff2*FX_r(i  ,j)+                   
     &               cff1*0.5*(FX_r(i-1,j)+FX_r(i,j)))
#  ifdef MASKING
            FX(i,j)=FX(i,j)*umask(i,j)
#  endif
#  ifdef WET_DRY
            FX(i,j)=FX(i,j)*umask_wet(i,j)
#  endif
#  ifdef SLOPE_KIRWAN
            dzdx=(h(i,j)-h(i-1  ,j))/om_u(i,j)
            a_slopex=(MAX(0.0,abs(dzdx)-0.01)
     &               *SIGN(1.0,dzdx)*0.1)
     &               *om_r(i,j)*dt
#   ifdef MOVING_BATHY
            a_slopex=a_slopex*morph_fac
#   endif
            FX(i,j)=FX(i,j)+a_slopex
#   ifdef MASKING
            FX(i,j)=FX(i,j)*umask(i,j)
#   endif
#  endif /* SLOPE_KIRWAN */
          enddo
        enddo
        do j=Jstr,Jend+1
          do i=Istr,Iend
            cff1=0.5*(1.0+SIGN(1.0,FE_r(i,j)))
            cff2=0.5*(1.0-SIGN(1.0,FE_r(i,j)))
            FE(i,j)=0.5*(1.0+SIGN(1.0,FE_r(i,j-1)))*     
     &              (cff1*FE_r(i,j-1)+                      
     &               cff2*0.5*(FE_r(i,j-1)+FE_r(i,j)))+       
     &              0.5*(1.0-SIGN(1.0,FE_r(i,j-1)))*      
     &              (cff2*FE_r(i  ,j)+                    
     &               cff1*0.5*(FE_r(i,j-1)+FE_r(i,j)))
#  ifdef MASKING
            FE(i,j)=FE(i,j)*vmask(i,j)
#  endif
#  ifdef WET_DRY
            FE(i,j)=FE(i,j)*vmask_wet(i,j)
#  endif
#  ifdef SLOPE_KIRWAN
            dzdy=(h(i,j)-h(i  ,j-1))/on_v(i,j)
            a_slopey=(MAX(0.0,abs(dzdy)-0.01
     &               *SIGN(1.0,dzdy)*0.1)
     &               *on_r(i,j)*dt
#   ifdef MOVING_BATHY
            a_slopey=a_slopey*morph_fac
#   endif
            FE(i,j)=FE(i,j)+a_slopey
#   ifdef MASKING
            FE(i,j)=FE(i,j)*vmask(i,j)
#   endif
#  endif /* SLOPE_KIRWAN */
          enddo
        enddo
!
! Limit fluxes to prevent bottom from breaking thru water surface.
!
        DO j=Jstr,Jend
          DO i=Istr,Iend
!
! Total thickness available.
!
#  ifdef WET_DRY
            Dstp=MAX(z_w(i,j,N)-z_w(i,j,0)-Dcrit(i,j),0.)
#  else
            Dstp=MAX(z_w(i,j,N)-z_w(i,j,0)-hmin,0.)
#  endif
!
! Thickness change that wants to occur.
!
            rhs_bed=(FX(i+1,j)-FX(i,j)+                          
     &               FE(i,j+1)-FE(i,j))*pm(i,j)*pn(i,j)
            bed_change=rhs_bed/(Srho(ised)*(1.0-bed_poros(i,j,1)))
!
! Limit that change to be less than available.
!
            cff=MAX(bed_change-Dstp,0.0)
            cff1=cff/ABS(bed_change+eps)
            FX(i+1,j  )=FX(i+1,j  )*(1.0-cff1)
            FX(i  ,j  )=FX(i  ,j  )*(1.0-cff1)
            FE(i  ,j+1)=FE(i  ,j+1)*(1.0-cff1)
            FE(i  ,j  )=FE(i  ,j  )*(1.0-cff1)
          END DO
        END DO
!
! Apply boundary conditions
!
#  ifndef EW_PERIODIC
#   ifndef OBC_WEST
        if (WESTERN_EDGE) then
          do j=Jstr-1,Jend+1
            FX(Istr,j)=0.
          enddo
        endif
#   endif
#   ifndef OBC_EAST
        if (EASTERN_EDGE) then
          do j=Jstr-1,Jend+1
            FX(Iend+1,j)=0.
          enddo
        endif
#   endif
#  endif
#  ifndef NS_PERIODIC
#   ifndef OBC_SOUTH
        if (SOUTHERN_EDGE) then
          do i=Istr-1,Iend+1
            FE(i,Jstr)=0.
          enddo
        endif
#   endif
#   ifndef OBC_NORTH
        if (NORTHERN_EDGE) then
          do i=Istr-1,Iend+1
            FE(i,Jend+1)=0.
          enddo
        endif
#   endif
#  endif
!
!  Determine flux divergence and evaluate change in bed properties.
!
        do j=Jstr,Jend
          do i=Istr,Iend
            cff=(FX(i+1,j)-FX(i,j)+                           
     &           FE(i,j+1)-FE(i,j))*pm(i,j)*pn(i,j)
#  define DIFF_BED
#  ifdef DIFF_BED
     &                    -dt*pm(i,j)*pn(i,j)*1.e-2 MORPH_FAC
     &                                       *(0.5*on_r(i,j)*(
     &                            bed_mass(i+1,j,1,nstp,ised)
     &                        -2.*bed_mass(i  ,j,1,nstp,ised)
     &                           +bed_mass(i-1,j,1,nstp,ised))
#   ifdef WET_DRY
     &       *rmask_wet(i-1,j)*rmask_wet(i,j)*rmask_wet(i+1,j)
#   endif
     &                                        +0.5*om_r(i,j)*(
     &                            bed_mass(i,j+1,1,nstp,ised)
     &                        -2.*bed_mass(i,j  ,1,nstp,ised)
     &                           +bed_mass(i,j-1,1,nstp,ised))
#   ifdef WET_DRY
     &       *rmask_wet(i,j-1)*rmask_wet(i,j)*rmask_wet(i,j+1)
#   endif
     &                                                       )
#  endif /* DIFF_BED */
            bed_mass(i,j,1,nnew,ised)=MAX(bed_mass(i,j,1,nstp,ised)-   
     &                                    cff,0.0)
#  if !defined SUSPLOAD
            do k=2,NLAY
              bed_mass(i,j,k,nnew,ised)=bed_mass(i,j,k,nstp,ised)
            enddo
#  endif
            bed_thick(i,j,1)=MAX(bed_thick(i,j,1)-            
     &                           cff/(Srho(ised)*              
     &                           (1.0-bed_poros(i,j,1))), 0.0)
          enddo
        enddo
!
!-----------------------------------------------------------------------
!  Output bedload fluxes.
!-----------------------------------------------------------------------
!
        cff=0.5/dt
        do j=JstrR,JendR
          do i=Istr,IendR
            bedldu(i,j,ised)=FX(i,j)*(pn(i-1,j)+pn(i,j))*cff
          enddo
        enddo
        do j=Jstr,JendR
          do i=IstrR,IendR
            bedldv(i,j,ised)=FE(i,j)*(pm(i,j-1)+pm(i,j))*cff
          enddo
        enddo
!
      enddo  ! ised
!
!-----------------------------------------------------------------------
!  Update mean surface properties.
!-----------------------------------------------------------------------
!
      do j=Jstr,Jend
        do i=Istr,Iend
          cff3=0.0
          DO ised=1,NST
            cff3=cff3+bed_mass(i,j,1,nnew,ised)
          END DO
          IF (cff3.eq.0.0) THEN
            cff3=eps
          END IF
          DO ised=1,NST
            bed_frac(i,j,1,ised)=bed_mass(i,j,1,nnew,ised)/cff3
          END DO
!
          cff1=1.
          cff2=1.
          cff3=1.
          cff4=1.
          do ised=1,NST
            cff1=cff1*tau_ce(ised)**bed_frac(i,j,1,ised)
            cff2=cff2*Sd(ised)**bed_frac(i,j,1,ised)
            cff3=cff3*(wsed(ised)+eps)**bed_frac(i,j,1,ised)
            cff4=cff4*Srho(ised)**bed_frac(i,j,1,ised)
          enddo
          taucb(i,j)=cff1
          Ssize(i,j)=MIN(cff2,Zob)
          w_set(i,j)=cff3
          Sdens(i,j)=MAX(cff4,1050.)
        enddo
      enddo
!
! Set boundary conditions
!
# ifndef EW_PERIODIC
      if (EASTERN_EDGE) then
        do j=Jstr,Jend
           taucb(Iend+1,j)=taucb(Iend,j)
           Ssize(Iend+1,j)=Ssize(Iend,j)
           w_set(Iend+1,j)=w_set(Iend,j)
           Sdens(Iend+1,j)=Sdens(Iend,j)
        enddo
      endif
      if (WESTERN_EDGE) then
        do j=Jstr,Jend
           taucb(Istr-1,j)=taucb(Istr,j)
           Ssize(Istr-1,j)=Ssize(Istr,j)
           w_set(Istr-1,j)=w_set(Istr,j)
           Sdens(Istr-1,j)=Sdens(Istr,j)
        enddo
      endif
# endif
# ifndef NS_PERIODIC
      if (NORTHERN_EDGE) then
        do i=Istr,Iend
          taucb  (i,Jend+1)=taucb(i,Jend)
          Ssize  (i,Jend+1)=Ssize(i,Jend)
          w_set  (i,Jend+1)=w_set(i,Jend)
          Sdens  (i,Jend+1)=Sdens(i,Jend)
        enddo
      endif
      if (SOUTHERN_EDGE) then
        do i=Istr,Iend
          taucb  (i,Jstr-1)=taucb(i,Jstr)
          Ssize  (i,Jstr-1)=Ssize(i,Jstr)
          w_set  (i,Jstr-1)=w_set(i,Jstr)
          Sdens  (i,Jstr-1)=Sdens(i,Jstr)
        enddo
      endif
# endif
# if !defined EW_PERIODIC && !defined NS_PERIODIC
      if (SOUTHERN_EDGE.and.WESTERN_EDGE) then
        taucb(Istr-1,Jstr-1)=
     &  0.5*(taucb(Istr,Jstr-1)
     &      +taucb(Istr-1,Jstr))
        Ssize(Istr-1,Jstr-1)=
     &  0.5*(Ssize(Istr,Jstr-1)
     &      +Ssize(Istr-1,Jstr))
        w_set(Istr-1,Jstr-1)=
     &  0.5*(w_set(Istr,Jstr-1)
     &      +w_set(Istr-1,Jstr))
        Sdens(Istr-1,Jstr-1)=
     &  0.5*(Sdens(Istr,Jstr-1)
     &      +Sdens(Istr-1,Jstr))
      endif
      if (SOUTHERN_EDGE.and.EASTERN_EDGE) then
        taucb(Iend+1,Jstr-1)=
     &  0.5*(taucb(Iend,Jstr-1)
     &      +taucb(Iend+1,Jstr))
        Ssize(Iend+1,Jstr-1)=
     &  0.5*(Ssize(Iend,Jstr-1)
     &      +Ssize(Iend+1,Jstr))
        w_set(Iend+1,Jstr-1)=
     &  0.5*(w_set(Iend,Jstr-1)
     &      +w_set(Iend+1,Jstr))
        Sdens(Iend+1,Jstr-1)=
     &  0.5*(Sdens(Iend,Jstr-1)
     &      +Sdens(Iend+1,Jstr))
      endif
      if (NORTHERN_EDGE.and.WESTERN_EDGE) then
        taucb(Istr-1,Jend+1)=
     &  0.5*(taucb(Istr,Jend+1)
     &      +taucb(Istr-1,Jend))
        Ssize(Istr-1,Jend+1)=
     &  0.5*(Ssize(Istr,Jend+1)
     &      +Ssize(Istr-1,Jend))
        w_set(Istr-1,Jend+1)=
     &  0.5*(w_set(Istr,Jend+1)
     &      +w_set(Istr-1,Jend))
        Sdens(Istr-1,Jend+1)=
     &  0.5*(Sdens(Istr,Jend+1)
     &      +Sdens(Istr-1,Jend))
      endif
      if (NORTHERN_EDGE.and.EASTERN_EDGE) then
        taucb(Iend+1,Jend+1)=
     &   0.5*(taucb(Iend,Jend+1)
     &       +taucb(Iend+1,Jend))
        Ssize(Iend+1,Jend+1)=
     &   0.5*(Ssize(Iend,Jend+1)
     &       +Ssize(Iend+1,Jend))
        w_set(Iend+1,Jend+1)=
     &   0.5*(w_set(Iend,Jend+1)
     &       +w_set(Iend+1,Jend))
        Sdens(Iend+1,Jend+1)=
     &   0.5*(Sdens(Iend,Jend+1)
     &       +Sdens(Iend+1,Jend))
      endif
# endif

# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
         call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 taucb(START_2D_ARRAY))
         call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 Ssize(START_2D_ARRAY))
         call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 w_set(START_2D_ARRAY))
         call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 Sdens(START_2D_ARRAY))
# endif
!
!-----------------------------------------------------------------------
! Set boundary conditions
!-----------------------------------------------------------------------
!
#  ifndef EW_PERIODIC
      if (EASTERN_EDGE) then
        do j=Jstr,Jend
           do k=1,NLAY
             do ised=1,NST
                bed_mass(Iend+1,j,k,nnew,ised)=
     &              bed_mass(Iend,j,k,nnew,ised)
                bed_frac(Iend+1,j,k,ised)=
     &              bed_frac(Iend,j,k,ised)
             enddo
           enddo
           bed_thick(Iend+1,j,1)=bed_thick(Iend,j,1)
         enddo
      endif
      if (WESTERN_EDGE) then
        do j=Jstr,Jend
          do k=1,NLAY
            do ised=1,NST
                bed_mass(Istr-1,j,k,nnew,ised)=
     &              bed_mass(Istr,j,k,nnew,ised)
                bed_frac(Istr-1,j,k,ised)=
     &              bed_frac(Istr,j,k,ised)
             enddo
           enddo
          bed_thick(Istr-1,j,1)=bed_thick(Istr,j,1)
        enddo
      endif
#  endif
#  ifndef NS_PERIODIC
      if (NORTHERN_EDGE) then
        do i=Istr,Iend
          do k=1,NLAY
            do ised=1,NST
               bed_mass(i,Jend+1,k,nnew,ised)=
     &              bed_mass(i,Jend,k,nnew,ised)
               bed_frac(i,Jend+1,k,ised)=
     &              bed_frac(i,Jend,k,ised)
            enddo
          enddo
          bed_thick(i,Jend+1,1)=bed_thick(i,Jend,1)
        enddo
      endif
      if (SOUTHERN_EDGE) then
        do i=Istr,Iend
          do k=1,NLAY
            do ised=1,NST
               bed_mass(i,Jstr-1,k,nnew,ised)=
     &              bed_mass(i,Jstr,k,nnew,ised)
               bed_frac(i,Jstr-1,k,ised)=
     &              bed_frac(i,Jstr,k,ised)
            enddo
          enddo
          bed_thick(i,Jstr-1,1)=bed_thick(i,Jstr,1)
        enddo
      endif
#  endif
#  if !defined EW_PERIODIC && !defined NS_PERIODIC
      if (SOUTHERN_EDGE.and.WESTERN_EDGE) then
        bed_mass(Istr-1,Jstr-1,:,nnew,:)=
     &  0.5*(bed_mass(Istr,Jstr-1,:,nnew,:)
     &      +bed_mass(Istr-1,Jstr,:,nnew,:))
        bed_frac(Istr-1,Jstr-1,:,:)=
     &  0.5*(bed_frac(Istr,Jstr-1,:,:)
     &      +bed_frac(Istr-1,Jstr,:,:))
        bed_thick(Istr-1,Jstr-1,1)=
     &  0.5*(bed_thick(Istr,Jstr-1,1)
     &      +bed_thick(Istr-1,Jstr,1))
      endif
      if (SOUTHERN_EDGE.and.EASTERN_EDGE) then
        bed_mass(Iend+1,Jstr-1,:,nnew,:)=
     &  0.5*(bed_mass(Iend,Jstr-1,:,nnew,:)
     &      +bed_mass(Iend+1,Jstr,:,nnew,:))
        bed_frac(Iend+1,Jstr-1,:,:)=
     &  0.5*(bed_frac(Iend,Jstr-1,:,:)
     &      +bed_frac(Iend+1,Jstr,:,:))
        bed_thick(Iend+1,Jstr-1,:)=
     &  0.5*(bed_thick(Iend,Jstr-1,1)
     &      +bed_thick(Iend+1,Jstr,1))
      endif
      if (NORTHERN_EDGE.and.WESTERN_EDGE) then
        bed_mass(Istr-1,Jend+1,:,nnew,:)=
     &  0.5*(bed_mass(Istr,Jend+1,:,nnew,:)
     &      +bed_mass(Istr-1,Jend,:,nnew,:))
        bed_frac(Istr-1,Jend+1,:,:)=
     &  0.5*(bed_frac(Istr,Jend+1,:,:)
     &      +bed_frac(Istr-1,Jend,:,:))
        bed_thick(Istr-1,Jend+1,1)=
     &  0.5*(bed_thick(Istr,Jend+1,1)
     &      +bed_thick(Istr-1,Jend,1))
      endif
      if (NORTHERN_EDGE.and.EASTERN_EDGE) then
        bed_mass(Iend+1,Jend+1,:,nnew,:)=
     &   0.5*(bed_mass(Iend,Jend+1,:,nnew,:)
     &       +bed_mass(Iend+1,Jend,:,nnew,:))
        bed_frac(Iend+1,Jend+1,:,:)=
     &   0.5*(bed_Frac(Iend,Jend+1,:,:)
     &       +bed_Frac(Iend+1,Jend,:,:))
        bed_thick(Iend+1,Jend+1,:)=
     &   0.5*(bed_thick(Iend,Jend+1,1)
     &       +bed_thick(Iend+1,Jend,1))
      endif
#  endif

#  if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      do ised=1,NST
        do k=1,NLAY
          call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 bed_mass(START_2D_ARRAY,k,nnew,ised))
          call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 bed_frac(START_2D_ARRAY,k,ised))
        enddo 
        call exchange_u2d_tile (Istr,Iend,Jstr,Jend,
     &                 bedldu(START_2D_ARRAY,ised))
        call exchange_v2d_tile (Istr,Iend,Jstr,Jend,
     &                 bedldv(START_2D_ARRAY,ised))
      enddo
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 bed_thick(START_2D_ARRAY,1))
#  endif

      return 
      end
# endif /* BEDLOAD */
      
# ifdef SUSPLOAD
!======================================================================
!                                                                      !
!  This routine computes the vertical settling (sinking) of suspended  !
!  sediment via a semi-Lagrangian advective flux algorithm. It uses a  !
!  parabolic,  vertical reconstructuion of the suspended  sediment in  !
!  the water column with PPT/WENO constraints to avoid oscillations.   !
!                                                                      !
!  References:                                                         !
!                                                                      !
!  Colella, P. and P. Woodward, 1984: The piecewise parabolic method   !
!    (PPM) for gas-dynamical simulations, J. Comp. Phys., 54, 174-201. !
!                                                                      !
!  Liu, X.D., S. Osher, and T. Chan, 1994: Weighted essentially        !
!    nonoscillatory shemes, J. Comp. Phys., 115, 200-212.              !
!                                                                      !
!  Warner, J.C., C.R. Sherwood, R.P. Signell, C.K. Harris, and H.G.    !
!    Arango, 2008:  Development of a three-dimensional,  regional,     !
!    coupled wave, current, and sediment-transport model, Computers    !
!    & Geosciences, 34, 1284-1306.                                     !
!                                                                      !
!=======================================================================
!
      subroutine sed_settling_tile (Istr,Iend,Jstr,Jend,
     &                              Hz_inv,Hz_inv2,Hz_inv3,qc,
     &                              FC,qR,qL,WR,WL,ksource) 
!
      implicit none
#  include "param.h"
#  include "ocean3d.h"
#  include "sediment.h"
#  include "scalars.h"
#  include "grid.h"
   
      integer Istr,Iend,Jstr,Jend
      real Hz_inv(PRIVATE_1D_SCRATCH_ARRAY,N),  
     &    Hz_inv2(PRIVATE_1D_SCRATCH_ARRAY,N), 
     &    Hz_inv3(PRIVATE_1D_SCRATCH_ARRAY,N),  
     &         qc(PRIVATE_1D_SCRATCH_ARRAY,N),  
     &         FC(PRIVATE_1D_SCRATCH_ARRAY,0:N),  
     &         qR(PRIVATE_1D_SCRATCH_ARRAY,N), 
     &         qL(PRIVATE_1D_SCRATCH_ARRAY,N),  
     &         WR(PRIVATE_1D_SCRATCH_ARRAY,N), 
     &         WL(PRIVATE_1D_SCRATCH_ARRAY,N)
      integer  ksource(PRIVATE_1D_SCRATCH_ARRAY,N),
     &         ised,indx,i,j,k,ks  
      real dltR, dltL,cff,cffR,cffL,cu

      do j=Jstr,Jend                        ! Auxiliary step: save   
        do k=1,N                            ! inverses of grid box 
          do i=Istr,Iend                    ! heights to avoid
            Hz_inv(i,k)=1./Hz(i,j,k)        ! repeated divisions
          enddo
        enddo
        do k=1,N-1
          do i=Istr,Iend
            Hz_inv2(i,k)=1./(Hz(i,j,k)+Hz(i,j,k+1))
          enddo
        enddo
        do k=2,N-1
          do i=Istr,Iend
            Hz_inv3(i,k)=1./(Hz(i,j,k-1)+Hz(i,j,k)+Hz(i,j,k+1))
          enddo
        enddo

!     
! Vertical sinking of suspended particles:   Copy concentration of
!========= ======= == ========= ==========   suspended sediment into
! scratch array "qc" (q-central, restrict it to be positive) which
! are hereafter interpreted as a set of grid-box averaged values for
! concentration. then reconstruct vertical profile of "qc" in terms
! of a set of parabolic segments within each grid box; and, finally,
! compute semi-Lagrangian flux due to sinking.
!
        do ised=1,NST
          indx=itrc_sed+ised-1
          do k=1,N
            do i=Istr,Iend
              qc(i,k)=t(i,j,k,nnew,indx)
            enddo
          enddo

          do k=N-1,1,-1
            do i=Istr,Iend
              FC(i,k)=(qc(i,k+1)-qc(i,k))*Hz_inv2(i,k)
            enddo
          enddo
          do k=2,N-1
            do i=Istr,Iend
              dltR=Hz(i,j,k)*FC(i,k)
              dltL=Hz(i,j,k)*FC(i,k-1)
              cff=Hz(i,j,k-1)+2.*Hz(i,j,k)+Hz(i,j,k+1)
              cffR=cff*FC(i,k)
              cffL=cff*FC(i,k-1)            ! Apply PPM monotonicity
                                            ! constraint to prevent
              if (dltR*dltL .le. 0.) then   ! oscillation within the
                dltR=0.                     ! grid box
                dltL=0.
              elseif (abs(dltR) .gt. abs(cffL)) then
                dltR=cffL
              elseif (abs(dltL) .gt. abs(cffR)) then
                dltL=cffR
              endif                         ! Compute right and left
                                            ! side values qR,qL of 
              cff=(dltR-dltL)*Hz_inv3(i,k)  ! parabolic segments
              dltR=dltR-cff*Hz(i,j,k+1)     ! within grid box Hz(k)
              dltL=dltL+cff*Hz(i,j,k-1)     ! (WR,WL are measures of 
              qR(i,k)=qc(i,k)+dltR          ! quadratic variations).
              qL(i,k)=qc(i,k)-dltL
              WR(i,k)=( 2.*dltR-dltL )**2   ! NOTE: Although each 
              WL(i,k)=( dltR-2.*dltL )**2   ! parabolic segment is
            enddo                           ! monotone within its
          enddo          !--> discard FC    ! grid box, monotonicity 
                                            ! of the whole profile is
          cff=1.0E-14                       ! not guaranteed, because 
          do k=2,N-2                        ! qL(k+1)-qR(k) may still 
            do i=Istr,Iend                  ! have different sign 
              dltL=max(WL(i,k),   cff)      ! than qc(k+1)-qc(k)...
              dltR=max(WR(i,k+1), cff)
              qR(i,k)=(dltR*qR(i,k)+dltL*qL(i,k+1))/(dltR+dltL)
              qL(i,k+1)=qR(i,k)
            enddo                           !   ...this possibility
          enddo      !--> discard WR,WL     ! is excluded, after qL
                                            ! and qR are reconciled
          do i=Istr,Iend                    ! using WENO procedure.
            FC(i,N)=0.     !<-- no-flux BC
#  if defined LINEAR_CONTINUATION
            qL(i,N)=qR(i,N-1)
            qR(i,N)=2.*qc(i,N)-qL(i,N)
#  elif defined NEUMANN
            qL(i,N)=qR(i,N-1)
            qR(i,N)=1.5*qc(i,N)-0.5*qL(i,N)
#  else
            qR(i,N)=qc(i,N)                 ! Strictly monotone
            qL(i,N)=qc(i,N)                 ! version as the default:
            qR(i,N-1)=qc(i,N)               ! distributions at top...
#  endif
#  if defined LINEAR_CONTINUATION 
            qR(i,1)=qL(i,2)
            qL(i,1)=2.*qc(i,1)-qR(i,1)
#  elif defined NEUMANN
            qR(i,1)=qL(i,2)
            qL(i,1)=1.5*qc(i,1)-0.5*qR(i,1)
#  else  
            qL(i,2)=qc(i,1)                 !     ...and bottom grid
            qR(i,1)=qc(i,1)                 ! boxes re assumed to be
            qL(i,1)=qc(i,1)                 ! piecewise constant.
#  endif
          enddo

          do k=1,N                          ! Since the reconciled 
            do i=Istr,Iend                  ! interfacial values may
              dltR=qR(i,k)-qc(i,k)          ! cause non-monotonic
              dltL=qc(i,k)-qL(i,k)          ! behavior of parabolic
              cffR=2.*dltR                  ! segments inside grid
              cffL=2.*dltL                  ! box apply monotonicity
                                            ! constraint again.
              if (dltR*dltL .lt. 0.) then
                dltR=0.
                dltL=0.
              elseif (abs(dltR) .gt. abs(cffL)) then
                dltR=cffL
              elseif (abs(dltL) .gt. abs(cffR)) then
                dltL=cffR
              endif

              qR(i,k)=qc(i,k)+dltR
              qL(i,k)=qc(i,k)-dltL
            enddo
          enddo     !--> discard everything, except qR,qL
!
! After this moment reconstruction is considered complete. The next
! stage is to compute vertical advective fluxes FC. It is expected
! that sinking may occurs relatively fast, the algorithm is designed
! to be free of CFL criterion, which is achieved by allowing
! integration bounds for semi-Lagrangian advective flux to use as
! many grid boxes in upstream direction as necessary.
!
          cff=dt*abs(Wsed(ised))          ! In the two code segments
          do k=1,N                        ! WL is z-coordinate of the
            do i=Istr,Iend                ! departure point for grid
              FC(i,k-1)=0.                ! box interface z_w with
              WL(i,k)=z_w(i,j,k-1)+cff    ! the same indices;
#  ifdef WET_DRY
     &                *rmask_wet(i,j)
#  endif
              WR(i,k)=Hz(i,j,k)*qc(i,k)   ! FC is finite volume flux;
              ksource(i,k)=k              ! ksource(:,k) is index of
            enddo                         ! vertical grid box which
          enddo                           ! contains the departure
          do k=1,N                        ! point (restricted by N);
            do ks=k,N-1                   ! During the search: also
              do i=Istr,Iend
                if (WL(i,k) .gt. z_w(i,j,ks)) then
                  ksource(i,k)=ks+1
                  FC(i,k-1)=FC(i,k-1)+WR(i,ks)
                endif
              enddo                       ! add in content of whole
            enddo                         ! grid boxes participating
          enddo       !--> discard WR     ! in FC.

          do k=1,N                        ! Finalize computation of
            do i=Istr,Iend                ! flux: add fractional part
              ks=ksource(i,k)
              cu=min(1.,(WL(i,k)-z_w(i,j,ks-1))*Hz_inv(i,ks))
              FC(i,k-1)=FC(i,k-1) + Hz(i,j,ks)*cu*( qL(i,ks)
     &                             +cu*( 0.5*(qR(i,ks)-qL(i,ks))
     &             -(1.5-cu)*(qR(i,ks)+qL(i,ks)-2.*qc(i,ks))  ))
            enddo
          enddo
          do i=Istr,Iend
            do k=1,N,+1
               t(i,j,k,nnew,indx)=qc(i,k)
     &                   +(FC(i,k)-FC(i,k-1))*Hz_inv(i,k) 
            enddo   
            settling_flux(i,j,ised)=FC(i,0)
          enddo   
        enddo   
      enddo     ! j   lood    

!
!  Exchange for output purpose
!
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      do ised=1,NST
        call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                          settling_flux(START_2D_ARRAY,ised))
      enddo
#  endif

      return
      end
!
!=======================================================================
!                                                                      !
!  This computes sediment erosion flux and resuspended load.           !
!                                                                      !
!  References:                                                         !
!                                                                      !
!  Warner, J.C., C.R. Sherwood, R.P. Signell, C.K. Harris, and H.G.    !
!    Arango, 2008:  Development of a three-dimensional,  regional,     !
!    coupled wave, current, and sediment-transport model, Computers    !
!    & Geosciences, 34, 1284-1306.                                     !
!                                                                      !
!=======================================================================
!
      subroutine sed_fluxes_tile (Istr,Iend,Jstr,Jend, tau_w) 
!
      implicit none
#  include "param.h"
#  include "grid.h"
#  include "bbl.h"
#  include "forces.h"
#  include "sediment.h"
#  include "ocean3d.h"
#  include "scalars.h"
      integer Istr,Iend,Jstr,Jend
      integer i,j,ised
      integer indx
      integer imin,jmin,imax,jmax
      real cff,cff1,cff2,cff3,cff4,tau_water,Zr
      real tau_w(PRIVATE_2D_SCRATCH_ARRAY)

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
!
!-----------------------------------------------------------------------
!  Compute bottom skin-friction stress due to combined maximum wave 
!  and current interaction
!-----------------------------------------------------------------------
!
#  ifdef BBL
      do j=Jstr-1,Jend+1
        do i=Istr-1,Iend+1
          tau_w(i,j)=sqrt( bustrcwmax(i,j)**2
     &                    +bvstrcwmax(i,j)**2)
#  else
      do j=J_EXT_RANGE
        do i=I_EXT_RANGE
          tau_w(i,j)=0.5*sqrt( (bustr(i,j)+bustr(i+1,j))**2
     &                        +(bvstr(i,j)+bvstr(i,j+1))**2)
#  endif
#  ifdef WET_DRY
          tau_w(i,j)=tau_w(i,j)*rmask_wet(i,j)
#  endif
        enddo
      enddo
!
!-----------------------------------------------------------------------
!  Compute erosion flux and resuspended load
!-----------------------------------------------------------------------
!
      do j=jstr,jend
        do ised=1,NST
          do i=istr,iend
          indx=itrc_sed+ised-1     
! 
!  Calculate critical shear stress in Pa
!  => change for COHESIVE bed here
!
            cff=1./tau_ce(ised)
!
!  Compute erosion, ero_flux (kg/m2).
!
            cff1=(1.0-bed_poros(i,j,1))*bed_frac(i,j,1,ised)
            cff2=dt*Erate(ised)*cff1
            cff3=Srho(ised)*cff1
            cff4=bed_mass(i,j,1,bnew,ised)
            ero_flux(i,j,ised)=                                     
     &            MIN(MAX(0.,cff2*(cff*tau_w(i,j)-1.)),      
     &                MIN(cff3*bot_thick(i,j) MORPH_FAC ,cff4)+
     &                settling_flux(i,j,ised))
!
!  Update global tracer variables (m Tunits for nnew indx, Tuints for 3)
!  for erosive flux.
!                        
            t(i,j,1,nnew,indx)=t(i,j,1,nnew,indx)+ero_flux(i,j,ised)
     &                                                 *1./Hz(i,j,1)
          enddo  ! i loop
        enddo    ! ised loop 
      enddo      ! j loop 

!
!  Exchange for output purpose
!
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      do ised=1,NST
        call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                          ero_flux(START_2D_ARRAY,ised))
      enddo
#  endif

      return
      end
# endif /* SUSPLOAD */

!
!=======================================================================
!                                                                      !
!  This routine computes sediment bed layer stratigraphy.              !
!                                                                      !
!  Warner, J.C., C.R. Sherwood, R.P. Signell, C.K. Harris, and H.G.    !
!    Arango, 2008:  Development of a three-dimensional,  regional,     !
!    coupled wave, current, and sediment-transport model, Computers    !
!    & Geosciences, 34, 1284-1306.                                     !
!                                                                      !
!=======================================================================
!
      subroutine sed_bed_tile (Istr,Iend,Jstr,Jend,
     &                         tau_w,dep_mass,Awrk1,Awrk2)
!
      implicit none
# include "param.h"
# include "bbl.h"
# include "forces.h"
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
# include "sediment.h"
      integer Istr,Iend,Jstr,Jend
      integer i,j,k,ks,ised,Ksed, NLAYm1
      real tau_w(PRIVATE_2D_SCRATCH_ARRAY)
      real dep_mass(PRIVATE_1D_SCRATCH_ARRAY,NST)
      real Awrk1(PRIVATE_2D_SCRATCH_ARRAY)
      real Awrk2(PRIVATE_2D_SCRATCH_ARRAY)
      real eps,cff,cff1,cff2,cff3
      real thck_avail,thck_to_add,newlayer_thick
      integer imin,imax,jmin,jmax
      parameter ( eps=1.D-20,newlayer_thick=0.01)

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
!-----------------------------------------------------------------------
!  Compute bottom skin-friction stress due to combined maximum wave 
!  and current interaction
!-----------------------------------------------------------------------
!
# ifdef BBL
      do j=Jstr-1,Jend+1
        do i=Istr-1,Iend+1
          tau_w(i,j)=sqrt( bustrcwmax(i,j)**2
     &                    +bvstrcwmax(i,j)**2)
# else
      do j=J_EXT_RANGE
        do i=I_EXT_RANGE
          tau_w(i,j)=0.5*sqrt( (bustr(i,j)+bustr(i+1,j))**2
     &                        +(bvstr(i,j)+bvstr(i,j+1))**2)
# endif
!
!-----------------------------------------------------------------------
!  Calculate active layer thickness
!     --> Harris and Wiberg 1997: za = k1 (tau-tc)rho + k2 Sd50
!-----------------------------------------------------------------------
!
          bot_thick(i,j)=MAX(newlayer_thick,            
     &                       0.007*(tau_w(i,j)-taucb(i,j))*rho0)+   
     &                       6.*Ssize(i,j)
        enddo
      enddo
!
!-----------------------------------------------------------------------
!  Update bed properties according to ero_flux and dep_flux.
!-----------------------------------------------------------------------
!
# ifdef SUSPLOAD
!
      NLAYm1=max(NLAY-1,1)  ! for NLAY=1 case
!
      do j=Jstr,Jend
        do ised=1,NST
!
!  The deposition and resuspension of sediment on the bottom "bed"
!  is due to precipitation flux, already computed, and the
!  resuspension (erosion, hence called ero_flux). The resuspension is
!  applied to the bottom-most grid box value qc(:,1) so the total mass
!  is conserved. Restrict "ero_flux" so that "bed" cannot go negative
!  after both fluxes are applied.
!
          do i=Istr,Iend
            dep_mass(i,ised)=0.0
!
! Apply morphology factor.
!
#  ifdef MOVING_BATHY
            ero_flux(i,j,ised)=ero_flux(i,j,ised)*morph_fac
            settling_flux(i,j,ised)=settling_flux(i,j,ised)*morph_fac
#  endif
            if ( settling_flux(i,j,ised)-ero_flux(i,j,ised)
     &                                            .gt. 0. ) then
!
!  If first time step of deposit, then store deposit material in
!  temporary array, dep_mass.
! 
              if ( time .gt. (bed_age(i,j,1)+1.1*dt) .and.
     &             bed_thick(i,j,1) .gt. newlayer_thick ) then
                dep_mass(i,ised)=settling_flux(i,j,ised)-          
     &                                ero_flux(i,j,ised)
              endif
              bed_age(i,j,1)=time
            endif
!
!  Update bed mass arrays.
!
            bed_mass(i,j,1,nnew,ised)=MAX(bed_mass(i,j,1,bnew,ised)- 
     &                                   (ero_flux(i,j,ised)-        
     &                                    settling_flux(i,j,ised)),  
     &                                    0.) 
            do k=2,NLAY
              bed_mass(i,j,k,nnew,ised)=bed_mass(i,j,k,nstp,ised)
            enddo
          enddo   ! i loop
        enddo     ! ised loop
!
!  If first time step of deposit, create new layer and combine bottom
!  two bed layers.
!
        do i=Istr,Iend
          cff=0.
!
!  Determine if deposition ocurred here.
!
          if (NLAY.gt.1) then
!
            do ised=1,NST
              cff=cff+dep_mass(i,ised)
     &                     /(Srho(ised)*
     &                     (1.0-bed_poros(i,j,1)))
            enddo
!
!            if (cff.gt.0.0) then
            if (cff.gt.1.e-7 MORPH_FAC) then
!     
!  Combine bottom layers.
!
              bed_poros(i,j,NLAY)=0.5*(bed_poros(i,j,NLAYm1)+        
     &                                 bed_poros(i,j,NLAY))
              bed_age  (i,j,NLAY)=0.5*(bed_age  (i,j,NLAYm1)+     
     &                                 bed_age  (i,j,NLAY))
              do ised=1,NST
                bed_mass(i,j,NLAY,nnew,ised)=                           
     &                             bed_mass(i,j,NLAYm1,nnew,ised)+      
     &                             bed_mass(i,j,NLAY  ,nnew,ised)
              enddo
!
!  Push layers down.
!
              do k=NLAY-1,2,-1
                bed_poros(i,j,k)=bed_poros(i,j,k-1)
                bed_age  (i,j,k)=bed_age  (i,j,k-1)
                do ised =1,NST
                  bed_mass(i,j,k,nnew,ised)=bed_mass(i,j,k-1,nnew,ised)
                enddo
              enddo
!
!  Set new top layer parameters.
!
              do ised=1,NST
                bed_mass(i,j,2,nnew,ised)=MAX(bed_mass(i,j,2,nnew,ised)-
     &                                        dep_mass(i,ised),0.)
                bed_mass(i,j,1,nnew,ised)=dep_mass(i,ised)
              enddo
            endif
          endif  ! NLAY=1
!
!  Recalculate thickness and fractions for all layers.
!
          do k=1,NLAY
            cff3=0.
            do ised=1,NST
              cff3=cff3+bed_mass(i,j,k,nnew,ised)
            enddo
            if (cff3.eq.0.) then
              cff3=eps
            endif
            bed_thick(i,j,k)=0.
            do ised=1,NST
              bed_frac(i,j,k,ised)=bed_mass(i,j,k,nnew,ised)/cff3
              bed_thick(i,j,k)=MAX(bed_thick(i,j,k)+                    
     &                         bed_mass(i,j,k,nnew,ised)/               
     &                         (Srho(ised)*(1.-bed_poros(i,j,k))),0.)
            enddo  ! ised loop 
          enddo    ! k loop   
        enddo      ! i loop
      enddo        ! j loop
!
# endif /* SUSPLOAD */
!
!  Ensure top bed layer thickness is greater or equal than active layer
!  thickness. If need to add sed to top layer, then entrain from lower
!  levels. Create new layers at bottom to maintain Nlay
!
      do j=Jstr,Jend
        do i=Istr,Iend
          if (bot_thick(i,j).gt.bed_thick(i,j,1)) then
            if (NLAY.eq.1) then
              bot_thick(i,j)=bed_thick(i,j,1)
            else
              thck_to_add=bot_thick(i,j)-bed_thick(i,j,1)
              thck_avail=0.
              Ksed=1       ! initialize
              do k=2,NLAY
                if (thck_avail.lt.thck_to_add) then
                  thck_avail=thck_avail+bed_thick(i,j,k)
                  Ksed=k
                endif
              enddo
!
!  Catch here if there was not enough bed material.
!
              if (thck_avail.lt.thck_to_add) then
                bot_thick(i,j)=bed_thick(i,j,1)+thck_avail
                thck_to_add=thck_avail
              endif
!
!  Update bed mass of top layer and fractional layer.
!
              cff2=MAX(thck_avail-thck_to_add,0.)/            
     &             MAX(bed_thick(i,j,Ksed),eps)
              do ised=1,NST
                cff1=0.
                do k=1,Ksed
                  cff1=cff1+bed_mass(i,j,k,nnew,ised)
                enddo
                cff3=cff2*bed_mass(i,j,Ksed,nnew,ised)
                bed_mass(i,j,1   ,nnew,ised)=cff1-cff3
                bed_mass(i,j,Ksed,nnew,ised)=cff3
              enddo
!
!  Update thickness of fractional layer ksource_sed
!
              bed_thick(i,j,Ksed)=MAX(thck_avail-thck_to_add,0.)
!
!  Upate bed fraction of top layer.
!
              cff3=0.
              do ised=1,NST
                cff3=cff3+bed_mass(i,j,1,nnew,ised)
              enddo
              if (cff3.eq.0.) then
                cff3=eps
              endif
              do ised=1,NST
                bed_frac(i,j,1,ised)=bed_mass(i,j,1,nnew,ised)/cff3
              enddo
!
!  Upate bed thickness of top layer.
!
              bed_thick(i,j,1)=bot_thick(i,j)
!
!  Pull all layers closer to the surface.
!
              do k=Ksed,NLAY
                ks=Ksed-2
                bed_thick(i,j,k-ks)=bed_thick(i,j,k)
                bed_poros(i,j,k-ks)=bed_poros(i,j,k)
                bed_age  (i,j,k-ks)=bed_age  (i,j,k)
                do ised=1,NST
                  bed_frac(i,j,k-ks,ised)=bed_frac(i,j,k,ised)
                  bed_mass(i,j,k-ks,nnew,ised)=bed_mass(i,j,k,nnew,ised)
                enddo
              enddo
!
!  Add new layers onto the bottom. Split what was in the bottom layer to
!  fill these new empty cells. ("ks" is the number of new layers).
!
              ks=Ksed-2
              cff=1./REAL(ks+1)
              do k=NLAY,NLAY-ks,-1
                bed_thick(i,j,k)=bed_thick(i,j,NLAY-ks)*cff
                bed_age  (i,j,k)=bed_age  (i,j,NLAY-ks)
                do ised=1,NST
                  bed_frac(i,j,k,ised)=bed_frac(i,j,NLAY-ks,ised)
                  bed_mass(i,j,k,nnew,ised)=                          
     &                             bed_mass(i,j,NLAY-ks,nnew,ised)*cff
                enddo   ! ised loop
              enddo     ! k loop
            endif       ! NLAY > 1
          endif         ! increase top bed layer
        enddo           ! i loop
      enddo             ! j loop
!
! Set boundary conditions
!
# ifndef EW_PERIODIC
      if (EASTERN_EDGE) then
        do j=Jstr,Jend
          do k=1,NLAY
            do ised=1,NST
            bed_mass(Iend+1,j,k,nnew,ised)=
     &                       bed_mass(Iend,j,k,nnew,ised)
            bed_frac(Iend+1,j,k,ised)=
     &                        bed_frac(Iend,j,k,ised)
             enddo
             bed_thick(Iend+1,j,k)=bed_thick(Iend,j,k)
             bed_poros(Iend+1,j,k)=bed_poros(Iend,j,k)
             bed_age(Iend+1,j,k)=bed_age(Iend,j,k)
           enddo
           bot_thick(Iend+1,j)=bot_thick(Iend,j)
         enddo
        endif
      if (WESTERN_EDGE) then
        do j=Jstr,Jend
          do k=1,NLAY
            do ised=1,NST
            bed_mass(Istr-1,j,k,nnew,ised)=
     &                       bed_mass(Istr,j,k,nnew,ised)
            bed_frac(Istr-1,j,k,ised)=
     &                        bed_frac(Istr,j,k,ised)
             enddo
             bed_thick(Istr-1,j,k)=bed_thick(Istr,j,k)
             bed_poros(Istr-1,j,k)=bed_poros(Istr,j,k)
             bed_age(Istr-1,j,k)=bed_age(Istr,j,k)
           enddo
           bot_thick(Istr-1,j)=bot_thick(Istr,j)
        enddo
      endif
# endif
# ifndef NS_PERIODIC
      if (NORTHERN_EDGE) then
        do i=Istr,Iend
          do k=1,NLAY
            do ised=1,NST
              bed_mass (i,Jend+1,k,nnew,ised)=
     &                            bed_mass(i,Jend,k,nnew,ised)
              bed_frac (i,Jend+1,k,ised)=
     &                            bed_frac(i,Jend,k,ised)
            enddo
            bed_thick(i,Jend+1,k)=bed_thick(i,Jend,k)
            bed_poros(i,Jend+1,k)=bed_poros(i,Jend,k)
            bed_age  (i,Jend+1,k)=bed_age(i,Jend,k)
          enddo
          bot_thick  (i,Jend+1)=bot_thick(i,Jend)
        enddo
      endif
      if (SOUTHERN_EDGE) then
        do i=Istr,Iend
          do k=1,NLAY
            do ised=1,NST
              bed_mass (i,Jstr-1,k,nnew,ised)=
     &                            bed_mass(i,Jstr,k,nnew,ised)
              bed_frac (i,Jstr-1,k,ised)=
     &                            bed_frac(i,Jstr,k,ised)
            enddo
            bed_thick(i,Jstr-1,k)=bed_thick(i,Jstr,k)
            bed_poros(i,Jstr-1,k)=bed_poros(i,Jstr,k)
            bed_age  (i,Jstr-1,k)=bed_age(i,Jend,k)
          enddo
          bot_thick  (i,Jstr-1)=bot_thick(i,Jstr)
        enddo
      endif
# endif
# if !defined EW_PERIODIC && !defined NS_PERIODIC
      if (SOUTHERN_EDGE.and.WESTERN_EDGE) then
        bed_mass(Istr-1,Jstr-1,:,nnew,:)=
     &  0.5*(bed_mass(Istr,Jstr-1,:,nnew,:)
     &      +bed_mass(Istr-1,Jstr,:,nnew,:))
        bed_frac(Istr-1,Jstr-1,:,:)=
     &  0.5*(bed_frac(Istr,Jstr-1,:,:)
     &      +bed_frac(Istr-1,Jstr,:,:))
        bed_thick(Istr-1,Jstr-1,:)=
     &  0.5*(bed_thick(Istr,Jstr-1,:)
     &      +bed_thick(Istr-1,Jstr,:))
        bed_poros(Istr-1,Jstr-1,:)=
     &  0.5*(bed_poros(Istr,Jstr-1,:)
     &      +bed_poros(Istr-1,Jstr,:))
        bed_age(Istr-1,Jstr-1,:)=
     &  0.5*(bed_age(Istr,Jstr-1,:)
     &      +bed_age(Istr-1,Jstr,:))
      endif
      if (SOUTHERN_EDGE.and.EASTERN_EDGE) then
        bed_mass(Iend+1,Jstr-1,:,nnew,:)=
     &  0.5*(bed_mass(Iend,Jstr-1,:,nnew,:)
     &      +bed_mass(Iend+1,Jstr,:,nnew,:))
        bed_frac(Iend+1,Jstr-1,:,:)=
     &  0.5*(bed_frac(Iend,Jstr-1,:,:)
     &      +bed_frac(Iend+1,Jstr,:,:))
        bed_thick(Iend+1,Jstr-1,:)=
     &  0.5*(bed_thick(Iend,Jstr-1,:)
     &      +bed_thick(Iend+1,Jstr,:))
        bed_poros(Iend+1,Jstr-1,:)=
     &  0.5*(bed_poros(Iend,Jstr-1,:)
     &      +bed_poros(Iend+1,Jstr,:))
        bed_age(Iend+1,Jstr-1,:)=
     &  0.5*(bed_age(Iend,Jstr-1,:)
     &      +bed_age(Iend+1,Jstr,:))
      endif
      if (NORTHERN_EDGE.and.WESTERN_EDGE) then
        bed_mass(Istr-1,Jend+1,:,nnew,:)=
     &  0.5*(bed_mass(Istr,Jend+1,:,nnew,:)
     &      +bed_mass(Istr-1,Jend,:,nnew,:))
        bed_frac(Istr-1,Jend+1,:,:)=
     &  0.5*(bed_frac(Istr,Jend+1,:,:)
     &      +bed_frac(Istr-1,Jend,:,:))
        bed_thick(Istr-1,Jend+1,:)=
     &  0.5*(bed_thick(Istr,Jend+1,:)
     &      +bed_thick(Istr-1,Jend,:))
        bed_poros(Istr-1,Jend+1,:)=
     &  0.5*(bed_poros(Istr,Jend+1,:)
     &      +bed_poros(Istr-1,Jend,:))
        bed_age(Istr-1,Jend+1,:)=
     &  0.5*(bed_age(Istr,Jend+1,:)
     &      +bed_age(Istr-1,Jend,:))
      endif
      if (NORTHERN_EDGE.and.EASTERN_EDGE) then
        bed_mass(Iend+1,Jend+1,:,nnew,:)=
     &   0.5*(bed_mass(Iend,Jend+1,:,nnew,:)
     &       +bed_mass(Iend+1,Jend,:,nnew,:))
        bed_frac(Iend+1,Jend+1,:,:)=
     &   0.5*(bed_frac(Iend,Jend+1,:,:)
     &       +bed_frac(Iend+1,Jend,:,:))
        bed_thick(Iend+1,Jend+1,:)=
     &   0.5*(bed_thick(Iend,Jend+1,:)
     &       +bed_thick(Iend+1,Jend,:))
        bed_poros(Iend+1,Jend+1,:)=
     &   0.5*(bed_poros(Iend,Jend+1,:)
     &       +bed_poros(Iend+1,Jend,:))
        bed_age(Iend+1,Jend+1,:)=
     &   0.5*(bed_age(Iend,Jend+1,:)
     &       +bed_age(Iend+1,Jend,:))
      endif
# endif

# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      do ised=1,NST
        do k=1,NLAY
          call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 bed_frac(START_2D_ARRAY,k,ised))
          call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 bed_mass(START_2D_ARRAY,k,nnew,ised))
         enddo 
      enddo
      do k=1,NLAY
         call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 bed_poros(START_2D_ARRAY,k))
         call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 bed_thick(START_2D_ARRAY,k))
         call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 bed_age(START_2D_ARRAY,k))
      enddo   
         call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 bot_thick(START_2D_ARRAY))
# endif
!
!-----------------------------------------------------------------------
!  Store total bed thickness
!-----------------------------------------------------------------------
!
# ifdef MOVING_BATHY
      do j=Jstr,Jend
        do i=Istr,Iend
          bed_thick_tot(i,j,nnew)=0.
          do k=1,NLAY
            bed_thick_tot(i,j,nnew)=bed_thick_tot(i,j,nnew)+
     &                              bed_thick(i,j,k)
          enddo
        enddo
      enddo
!
! Set boundary conditions
!
#  ifndef EW_PERIODIC
      if (EASTERN_EDGE) then
        do j=Jstr,Jend
          bed_thick_tot(Iend+1,j,nnew)=bed_thick_tot(Iend,j,nnew)
         enddo
      endif
      if (WESTERN_EDGE) then
        do j=Jstr,Jend
          bed_thick_tot(Istr-1,j,nnew)=bed_thick_tot(Istr,j,nnew)
        enddo
      endif
#  endif
#  ifndef NS_PERIODIC
      if (NORTHERN_EDGE) then
        do i=Istr,Iend
          bed_thick_tot(i,Jend+1,nnew)=bed_thick_tot(i,Jend,nnew)
        enddo
      endif
      if (SOUTHERN_EDGE) then
        do i=Istr,Iend
          bed_thick_tot(i,Jstr-1,nnew)=bed_thick_tot(i,Jstr,nnew)
        enddo
      endif
#  endif
#  if !defined EW_PERIODIC && !defined NS_PERIODIC
      if (SOUTHERN_EDGE.and.WESTERN_EDGE) then
        bed_thick_tot(Istr-1,Jstr-1,nnew)=
     &  0.5*(bed_thick_tot(Istr,Jstr-1,nnew)
     &      +bed_thick_tot(Istr-1,Jstr,nnew))
      endif
      if (SOUTHERN_EDGE.and.EASTERN_EDGE) then
        bed_thick_tot(Iend+1,Jstr-1,nnew)=
     &  0.5*(bed_thick_tot(Iend,Jstr-1,nnew)
     &      +bed_thick_tot(Iend+1,Jstr,nnew))
      endif
      if (NORTHERN_EDGE.and.WESTERN_EDGE) then
        bed_thick_tot(Istr-1,Jend+1,nnew)=
     &  0.5*(bed_thick_tot(Istr,Jend+1,nnew)
     &      +bed_thick_tot(Istr-1,Jend,nnew))
      endif
      if (NORTHERN_EDGE.and.EASTERN_EDGE) then
        bed_thick_tot(Iend+1,Jend+1,nnew)=
     &   0.5*(bed_thick_tot(Iend,Jend+1,nnew)
     &       +bed_thick_tot(Iend+1,Jend,nnew))
      endif
#  endif
!
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
       call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                         bed_thick_tot(START_2D_ARRAY,nnew))
#  endif
!
!-----------------------------------------------------------------------
!  Store bed evolution
!-----------------------------------------------------------------------
!
       dh(:,:)=bed_thick_tot(:,:,nstp)-bed_thick_tot(:,:,nnew)
!
# endif /* MOVING_BATHY*/

      return
      end
!
!=======================================================================
!                                                                      !
!  This routine computed sediment surface layer (sediment-water        !
!  interface) properties.                                              !
!                                                                      !
!  References:                                                         !
!                                                                      !
!  Warner, J.C., C.R. Sherwood, R.P. Signell, C.K. Harris, and H.G.    !
!    Arango, 2008:  Development of a three-dimensional,  regional,     !
!    coupled wave, current, and sediment-transport model, Computers    !
!    & Geosciences, 34, 1284-1306.                                     !
!                                                                      !
!=======================================================================
!
      subroutine sed_surf_tile (Istr,Iend,Jstr,Jend)
!
      implicit none
# include "param.h"
# include "bbl.h"
# include "grid.h"
# include "scalars.h"
# include "sediment.h"
      integer Istr,Iend,Jstr,Jend
      integer i,j,ised
      real cff1,cff2,cff3,cff4,eps
      parameter (eps=1.D-20)
!
# include "compute_auxiliary_bounds.h"

!
!  Update mean surface properties.
!  Sd50 must be positive definite, due to BBL routines.
!  Srho must be >1000, due to (s-1) in BBL routines
!
      do j=Jstr-1,Jend+1
        do i=Istr-1,Iend+1
          cff1=1.
          cff2=1.
          cff3=1.
          cff4=1.
          do ised=1,NST
            cff1=cff1*tau_ce(ised)**bed_frac(i,j,1,ised)
            cff2=cff2*Sd(ised)**bed_frac(i,j,1,ised)
            cff3=cff3*(wsed(ised)+eps)**bed_frac(i,j,1,ised)
            cff4=cff4*Srho(ised)**bed_frac(i,j,1,ised)
          enddo
          taucb(i,j)=cff1
          Ssize(i,j)=MIN(cff2,Zob)
          w_set(i,j)=cff3
          Sdens(i,j)=MAX(cff4,1050.)
        enddo
      enddo
!
! Set boundary conditions
!
# ifndef EW_PERIODIC
      if (EASTERN_EDGE) then
        do j=Jstr,Jend
           taucb(Iend+1,j)=taucb(Iend,j)
           Ssize(Iend+1,j)=Ssize(Iend,j)
           w_set(Iend+1,j)=w_set(Iend,j)
           Sdens(Iend+1,j)=Sdens(Iend,j)
        enddo
      endif
      if (WESTERN_EDGE) then
        do j=Jstr,Jend
           taucb(Istr-1,j)=taucb(Istr,j)
           Ssize(Istr-1,j)=Ssize(Istr,j)
           w_set(Istr-1,j)=w_set(Istr,j)
           Sdens(Istr-1,j)=Sdens(Istr,j)
        enddo
      endif
# endif
# ifndef NS_PERIODIC
      if (NORTHERN_EDGE) then
        do i=Istr,Iend
          taucb  (i,Jend+1)=taucb(i,Jend)
          Ssize  (i,Jend+1)=Ssize(i,Jend)
          w_set  (i,Jend+1)=w_set(i,Jend)
          Sdens  (i,Jend+1)=Sdens(i,Jend)
        enddo
      endif
      if (SOUTHERN_EDGE) then
        do i=Istr,Iend
          taucb  (i,Jstr-1)=taucb(i,Jstr)
          Ssize  (i,Jstr-1)=Ssize(i,Jstr)
          w_set  (i,Jstr-1)=w_set(i,Jstr)
          Sdens  (i,Jstr-1)=Sdens(i,Jstr)
        enddo
      endif
# endif
# if !defined EW_PERIODIC && !defined NS_PERIODIC
      if (SOUTHERN_EDGE.and.WESTERN_EDGE) then
        taucb(Istr-1,Jstr-1)=
     &  0.5*(taucb(Istr,Jstr-1)
     &      +taucb(Istr-1,Jstr))
        Ssize(Istr-1,Jstr-1)=
     &  0.5*(Ssize(Istr,Jstr-1)
     &      +Ssize(Istr-1,Jstr))
        w_set(Istr-1,Jstr-1)=
     &  0.5*(w_set(Istr,Jstr-1)
     &      +w_set(Istr-1,Jstr))
        Sdens(Istr-1,Jstr-1)=
     &  0.5*(Sdens(Istr,Jstr-1)
     &      +Sdens(Istr-1,Jstr))
      endif
      if (SOUTHERN_EDGE.and.EASTERN_EDGE) then
        taucb(Iend+1,Jstr-1)=
     &  0.5*(taucb(Iend,Jstr-1)
     &      +taucb(Iend+1,Jstr))
        Ssize(Iend+1,Jstr-1)=
     &  0.5*(Ssize(Iend,Jstr-1)
     &      +Ssize(Iend+1,Jstr))
        w_set(Iend+1,Jstr-1)=
     &  0.5*(w_set(Iend,Jstr-1)
     &      +w_set(Iend+1,Jstr))
        Sdens(Iend+1,Jstr-1)=
     &  0.5*(Sdens(Iend,Jstr-1)
     &      +Sdens(Iend+1,Jstr))
      endif
      if (NORTHERN_EDGE.and.WESTERN_EDGE) then
        taucb(Istr-1,Jend+1)=
     &  0.5*(taucb(Istr,Jend+1)
     &      +taucb(Istr-1,Jend))
        Ssize(Istr-1,Jend+1)=
     &  0.5*(Ssize(Istr,Jend+1)
     &      +Ssize(Istr-1,Jend))
        w_set(Istr-1,Jend+1)=
     &  0.5*(w_set(Istr,Jend+1)
     &      +w_set(Istr-1,Jend))
        Sdens(Istr-1,Jend+1)=
     &  0.5*(Sdens(Istr,Jend+1)
     &      +Sdens(Istr-1,Jend))
      endif
      if (NORTHERN_EDGE.and.EASTERN_EDGE) then
        taucb(Iend+1,Jend+1)=
     &   0.5*(taucb(Iend,Jend+1)
     &       +taucb(Iend+1,Jend))
        Ssize(Iend+1,Jend+1)=
     &   0.5*(Ssize(Iend,Jend+1)
     &       +Ssize(Iend+1,Jend))
        w_set(Iend+1,Jend+1)=
     &   0.5*(w_set(Iend,Jend+1)
     &       +w_set(Iend+1,Jend))
        Sdens(Iend+1,Jend+1)=
     &   0.5*(Sdens(Iend,Jend+1)
     &       +Sdens(Iend+1,Jend))
      endif
# endif

# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
         call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 taucb(START_2D_ARRAY))
         call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 Ssize(START_2D_ARRAY))
         call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 w_set(START_2D_ARRAY))
         call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                 Sdens(START_2D_ARRAY))
# endif

      return
      end

#else
      subroutine sediment_empty
      end
                 
#endif /* SEDIMENT*/
      
#ifdef SEDIMENT
!====================================================================
!
!  This routine reads in and processes sediment parameters and
!  some initial data from sediments.in file.
!
!=====================================================================
!
       subroutine init_sediment
!
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "ncscrum.h"
# include "scalars.h"
# include "sediment.h"
!
      integer Nclass, i, icard, iunit, lstr, nl
      integer lenstr
      parameter (iunit=50)
!
!---------------------------------------------------------------------
!  Read in initial float locations.
!---------------------------------------------------------------------
!
      lstr=lenstr(sedname)

      open(iunit,file=sedname(1:lstr),form='formatted',
     &  status='old', err=195)
!
!  Read input parameters according to their input card number.
!
      icard=0
      do while (icard.lt.99)
!
!  Read in sediment case title.
!
        if (icard.eq.1) then
          read(iunit,'(a)',err=60) Stitle
          lstr=lenstr(Stitle)
          write(stdout,10) Stitle(1:lstr)
  10      format(1x,'(',a,')',/)
!
!  Read in parameteramd initial values per size class.
!
        elseif (icard.eq.2) then
          i=0
          Nclass=0
          do i=1,NST
           read(iunit,*,err=40) Sd(i), Csed(i), Srho(i), Wsed(i),
     &                     Erate(i), tau_ce(i), tau_cd(i), 
     &                     (Bfr(nl,i),nl=1,NLAY)
     
           MPI_master_only write(stdout,'(/A,2x,i2)')
     &     'Sediment parameters for grain-size class:', i
     
           MPI_master_only write(stdout,'(7(/f10.5,2x,A))')
     &     Sd(i),    'Sd      Median sediment grain diameter (mm).',
     &     Csed(i),  'Csed    Analyt. init. of sed. conc. (mg/l).',
     &     Srho(i),  'Srho    Sediment grain density (kg/m3).',
     &     Wsed(i),  'Wsed    Particle settling velocity (mm/s).',
     &     Erate(i), 'Erate   Surface erosion rate (kg/(m2s)).',
     &     tau_ce(i),'tau_ce  Critical shear for erosion (N/m2).',
     &     tau_cd(i),'tau_cd  Critical shear for deposition (N/m2).'
          
           do nl=1,NLAY
            MPI_master_only write(stdout,'(f10.5,2x,A,2x,i2)')
     &      Bfr(nl,i), 'bed_frac   Volume fraction, layer:',nl
           enddo
 
           Sd(i)=Sd(i)/1000.         ! mm --> m
           Wsed(i)=Wsed(i)/1000.     ! mm/s --> m/s
           tau_ce(i)=tau_ce(i)/rho0  ! N/m2 --> m2/s2
           tau_cd(i)=tau_cd(i)/rho0  ! N/m2 --> m2/s2
           Nclass=Nclass+1
          enddo

        elseif (icard.eq.3) then
           MPI_master_only write(stdout,'(/A)')
     &     'Bed parameters for all bed levels'

          read(iunit,*,err=60) (Bthk(nl),nl=1,NLAY)
           do nl=1,NLAY
            MPI_master_only write(stdout,'(f10.5,2x,A,2x,i2,2x,A)')
     &      Bthk(nl), 'Bthk   Init. bed thickness, layer:',nl,'(m).'
           enddo

        elseif (icard.eq.4) then

          read(iunit,*,err=60) (Bpor(nl),nl=1,NLAY)
           do nl=1,NLAY
            MPI_master_only write(stdout,'(f10.5,2x,A,2x,i2,2x,A)')
     &      Bpor(nl), 'Bpor   Init. bed porosity, layer:',nl,'(m).'
           enddo

        elseif (icard.eq.5) then
          read(iunit,*,err=60) Hrip                     
            MPI_master_only write(stdout,'(f10.5,2x,A)')
     &      Hrip, 'Hrip   Init. ripple roughness (m).'
        
        elseif (icard.eq.6) then
          read(iunit,*,err=60) Lrip                     
            MPI_master_only write(stdout,'(f10.5,2x,A)')
     &      Lrip, 'Lrip   Init. riple height (m).'

        elseif (icard.eq.7) then
          read(iunit,*,err=60) bedload_coeff                     
            MPI_master_only write(stdout,'(f10.5,2x,A)')
     &      bedload_coeff, 'bedload_coeff   Bedload rate.'

        elseif (icard.eq.8) then
          read(iunit,*,err=60) morph_fac                     
            MPI_master_only write(stdout,'(f10.5,2x,A)')
     &      morph_fac, 'morph_fac   Mophological factor.'

        endif ! icard 
!
!  Read until last input card ID.
!
        read(iunit,*,err=60) icard
      enddo
      goto 90
!
!  Error while reading input parameters.
!
  40  write(stdout,50) icard, i, Nclass, sedname
  50  format(/,' INIT_SEDIMENTS - error reading variables in card: ',
     &             i2, ', entry: ',i3,/,15x,
     &             'nclass:',i3, 'input script: ',a)
  
  60  write(stdout,80) icard, sedname
  80  format(/,' INIT_SEDIMENTS - error while reading input card: ',
     &       i2,15x,'in input script: ',a)

 195  write(stdout,205) sedname
 205  format(/,'sediment inputfile ',A,/, 
     &   ' not found sediment initialization', ' from restart file')

  90  close(iunit)
      write(stdout,100) Nclass
 100  format(/,
     &       'Number of size classes in sediment computation:'i2)

      return
      end
#else       
      subroutine init_sediment_empty
      end
#endif
