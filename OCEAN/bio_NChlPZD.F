! $Id: bio_NChlPZD.F 1458 2014-02-03 15:01:25Z gcambon $
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
#if defined BIOLOGY && defined BIO_NChlPZD

      subroutine biology_tile (Istr,Iend,Jstr,Jend)
!
! Compute biological forcing functions
!
! In this particular implementation there is 4 compartments:
! NO3, PHYTOplankton, ZOOplanknton, DETritus.
!
      implicit none
      integer Istr,Iend,Jstr,Jend
#include "param.h"
#include "grid.h"
#include "ocean3d.h"
#include "ocean2d.h"
#include "diagnostics.h"
#include "scalars.h"
#include "forces.h"
#include "mixing.h"

      real kwater, palpha, kChla, CN_Phyt, theta_m, opc,
     &     K_NO3, mu_P_D, gmax, beta, K_Phyt,
     &     mu_Z_A, mu_Z_D, mu_D_N,
     &     wPhyt, wDet
#ifdef OXYGEN
     &   , CN_Z
#endif
      integer ITERMAX
      integer nsink
#ifdef DIAGNOSTICS_BIO

      real trend_no3,trend_phy,trend_zoo,trend_det,somme
      real bilan_no3,bilan_phy,bilan_zoo,bilan_det, sinking_loss, 
     &     trend_total
      integer l, iflux
      real ThisVSinkFlux(N, NumVSinkTerms),  ! [mmol m-2 s-1], upward flux is positive
     &     ThisFlux(N, NumFluxTerms)
# ifdef OXYGEN
      real ThisGasExcFlux(NumGasExcTerms), trend_o2, bilan_o2
# endif
#endif
      parameter (
     &  ITERMAX = 3,      ! number of small implicit time step
     &  nsink   = NumVSinkTerms + 1, ! add'lly: Chlorophyll
!
! Parameters as in Table 1; Fasham et al. [JMR, 48, 591-639, 1990]
!
     &  kwater  = 0.04,    ! light attenuation due to sea water  [m-1]
                           ! range:(0.04<==>0.04];    units:[m-1]
     &  palpha  = 1.0,     ! initial slope of the P-I curve
                           ! range:(1.00<==>1.00);       [(W m-2 d)-1]
     &  kChla  = 0.024,    ! light attenuation by Chlorophyl  
                           !                         [(m^2 mg Chla)-1]
     &  CN_Phyt= 6.625,    ! C:N ratio for phytoplankton
                           !                       [mMol C (mMol N)-1]
#ifdef OXYGEN
     &  CN_Z   = 6.625,    ! C:N ratio for zoo
                           ! range:(4.<==>6.);     [mol-C (mol-N)-1]
#endif
     &  theta_m= 0.0535,    ! max Cellular Chlorophyll to Carbon Ratio
                           ! range:(0.015<==>0.072);    [mg Chla/mg C]
     &  K_NO3   = 1./0.5,  ! inverse half-saturation for Phytoplankton
                           ! range:(1./.0 <==> 1./.9);[1/(mmol-N m-3)]
     &  mu_P_D  = 0.03,    ! Phyto mortality to Det rate        [d-1]
     &  gmax    = 0.9,     ! maximum Zooplankton growth rate     [d-1]
     &  beta    = 0.75,    ! Zooplankton assimilation efficiency of 
                           !                       Phytoplankton [n.d.]
     &  K_Phyt  = 1.0,     ! Zooplankton half-saturation constant
                           ! for ingestion of phyto [d-1]
     &  mu_Z_A  = 0.10,    ! Zooplankton specific excretion rate [d-1]
     &  mu_Z_D  = 0.10,    ! Zooplankton mortality to Detritus   [d-1]
     &  mu_D_N  = 0.05,    ! Detrital remineralization to NO3 rate
                           ! range:( <==>   );        units:[d-1]
     &  wPhyt   = 0.5,     ! sinking velocities for Phytoplankton [m.d-1]
     &  wDet    = 5.0  )   !    ''       ''      '' Detritus
!
      integer i,j,k, ITER, iB
      real    NO3(N), Phyt(N), Zoo(N), Det(N), Chla(N), 
     &        aJ(N),FC(0:N),
     &        PAR, PARsup, attn, Vp, Epp, cu, aL,aR, dtdays, L_NO3,
     &        E_NO3,cff,cff0,cff1,cff2,cff6,
     &        SB(N,nsink),dSB(0:N,nsink),wSB(nsink)
# ifdef OXYGEN
     &      , O2(N), tem(N), sal(N), den(N)
     &      , O2satu_loc, Kv_O2_loc
     &      , eos80, u10_loc, Sc
     
#define OCMIP_OXYGENSAT

#  ifdef OCMIP_OXYGENSAT
     &       , o2sato   ! OCMIP function, calculates O2 saturation
#  else /* OCMIP_OXYGENSAT */
     &       , satpc    ! oxygen saturation in % (calculated, but unused)
     &       , AOU      ! Apparent oxygen utilization (calc., but unused)
#  endif /* OCMIP_OXYGENSAT */
# endif /* OXYGEN */
# if defined OXYGEN || defined DIAGNOSTICS_BIO
     &      , dtsec     ! length of time step in seconds (for gas exchange)
# endif
# if defined DIAGNOSTICS_BIO
     &      , LastVSinkFlux,ColumnMassOld(NumVSinkTerms)
     &      , ColumnMassNew(NumVSinkTerms)
# endif /* DIAGNOSTICS_BIO */
!
# include "compute_auxiliary_bounds.h"
!
      dtdays=dt/(24.*3600.*float(ITERMAX))  ! time step as fraction of day.
# if defined DIAGNOSTICS_BIO || defined OXYGEN
      dtsec = dt / float(ITERMAX)           ! time step in seconds
# endif /* DIAGNOSTICS_BIO || OXYGEN */
!
!
! Since the following solver is iterative to achieve implicit
! discretization of the biological interaction, two time slices are
! required, BIO where BIO is understood as vector of
! biological state variables: BIO=[NO3,Phyt,Zoo,Det]. Assume
! that the iterations converge, the newly obtained state variables
! satisfy equations
!
!           BIO = BIO + dtdays * rhs(BIO)
! 
! where rhs(BIO) is the vector of biological r.h.s. computed at
! the new time step. During the iterative procedure a series of
! fractional time steps is performed in a chained mode (splitting
! by different biological conversion processes) in sequence NO3 -- 
! Phyt -- Zoo -- Det, that is the main food chain. In all 
! stages the concentration of the component being consumed is
! treated in fully implicit manner, so that the algorithm guarantees
! non-negative values, no matter how strong is the concentration of
! active consuming component (Phyto or Zoo).
!
! The overall algorithm, as well as any stage of it is formulated
! in conservative form (except explicit sinking) in sense that the
! sum of concentration of all five components is conserved.
!

/*
#  ifdef EW_PERIODIC
#   define I_RANGE Istr,Iend
#  else
#   define I_RANGE IstrR,IendR
#  endif
#  ifdef NS_PERIODIC
#   define J_RANGE Jstr,Jend
#  else
#   define J_RANGE JstrR,JendR
#  endif
*/
#  define I_RANGE Istr,Iend
#  define J_RANGE Jstr,Jend



      do j=J_RANGE
        do i=I_RANGE
#ifdef DIAGNOSTICS_BIO
! Reset the biogeochemical fluxes. This is necessary because the
! biological routine uses multiple. time steps for each physical time
! step.
          do k=1,N
            do l=1,NumFluxTerms
              bioFlux(i,j,k,l) = 0.0
            enddo
          end do
          do k=0,N
            do l=1,NumVSinkTerms
              bioVSink(i,j,k,l) = 0.0
            enddo
          enddo
# ifdef OXYGEN
          do l=1,NumGasExcTerms
            GasExcFlux(i,j,l) = 0.0
          enddo
# endif
#endif /* DIAGNOSTICS_BIO */
!
! Extract biological variables from tracer arrays; place them into
! scratch variables; restrict their values to be positive definite.
!

      do k=1,N
        NO3(k) =max(t(i,j,k,nnew,iNO3_)  ,0.)   ! Nitrate
        Phyt(k)=max(t(i,j,k,nnew,iPhy1)  ,0.)   ! Phytoplankton
        Chla(k)=max(t(i,j,k,nnew,iChla)  ,0.)   ! Chlor a
        Zoo(k) =max(t(i,j,k,nnew,iZoo1)  ,0.)   ! Zooplankton
        Det(k) =max(t(i,j,k,nnew,iDet1)  ,0.)   ! Detritus
!
        if (Phyt(k) .gt. 0.001 .and. Chla(k) .gt. 0.001) then
           theta(i,j,k) = Chla(k)/(Phyt(k)*CN_Phyt*12.)      ! Chla/Phyt ratio
           if (theta(i,j,k).gt.theta_m) theta(i,j,k)=theta_m ! [mg Chla(mg C)-1
        else                                     ! [mg Chla (mg C)-1]
           theta(i,j,k) = theta_m
        endif
# ifdef OXYGEN
        tem(k) =max(t(i,j,k,nnew,itemp),0.)      ! temperature; [deg. C]
        sal(k) =max(t(i,j,k,nnew,isalt),0.)      ! salinity; [PSU]
#  ifndef OCMIP_OXYGENSAT
        den(k) =1000.+ rho1(i,j,k)   		 ! potential density; [kg m-3]
#  endif
        O2(k)  =max(t(i,j,k,nnew,iO2),0.)        ! Oxygen;  [mmol O2 m-3]
# endif 
      enddo

      DO ITER=1,ITERMAX      !--> Start internal iterations to achieve
                             !    nonlinear backward-implicit solution.

        PAR=srflx(i,j)*rho0*Cp*0.43
        opc=0.01*PAR

        if (PAR.gt.0.) then
!
!   *** SUN IS UP ***
!
! Calulate aJ: Set Photosynthetically Available Radiation (PAR) at
! surface from solar radiation x 0.43. Then, within each grid box
! compute attenuation coefficient based on the concentration of
! Phytoplankton inside the grid box, and attenuate PAR from surface
! down (thus, PAR at certain depth depends on the whole distribution
! of Phytoplankton above). To compute aJ, one needs PAR somewhat in
! the middle of the gridbox, so that attenuation "attn" corresponds
! to half of the grid box height, while PAR is multiplied by it
! twice: once to get it in the middle of grid-box and once the
! compute on trhe lower grid-box interface;
!
          do k=N,1,-1     !<-- irreversible

            attn=exp(-0.5*(kwater+kChla*Chla(k))*
     &               (z_w(i,j,k)-z_w(i,j,k-1)))

            PARsup=PAR*attn
            Vp=0.59*(1.066**t(i,j,k,nnew,itemp))   ! From Eppley
!            Vp=0.8356*(1.066**t(i,j,k,nnew,itemp))   ! a.b^cT=µ_max=3.0
            cff0=PARsup*palpha*theta(i,j,k)           ! for diatoms & 
            Epp=Vp/sqrt(Vp*Vp+cff0*cff0)             ! 2.0 for flagelates
            aJ(k)=Epp*cff0
!
!           theta adaptation
            L_NO3=K_NO3*NO3(k)/(1+K_NO3*NO3(k))
            cff=dtdays*Epp*cff0*L_NO3
            theta(i,j,k)=(theta(i,j,k)+theta_m*Epp*cff*L_NO3)/(1.+cff)
! (1) NO3 uptake by Phyto
!
            E_NO3=K_NO3/(1+K_NO3*NO3(k)) ! Parker 1993 Ecol Mod. 66 113-120
            cff=dtdays*aJ(k)*Phyt(k)*E_NO3
            NO3(k)=NO3(k)/(1.+cff)
#ifdef DIAGNOSTICS_BIO
            ThisFlux(k, NFlux_NewProd) = cff*NO3(k)
!
# ifdef OXYGEN
!           production of O2 by phyto growth
            ThisFlux(k, OGain_NewProd) =
     &           ThisFlux(k, NFlux_NewProd) * (CN_Phyt + 2.)
# endif /* OXYGEN */
#endif /* DIAGNOSTICS_BIO */
# ifdef OXYGEN
            O2(k) = O2(k) + cff*NO3(k)*(CN_Phyt + 2.)
# endif
            Phyt(k)=Phyt(k)+cff*NO3(k)

            PAR=PARsup*attn
!      Calcul of the euphotic depth
!
            if (PARsup.ge.opc) then
              if (PAR.ge.opc) then
                hel(i,j)=-z_w(i,j,k-1)
              else
                hel(i,j)=-z_r(i,j,k)
              endif
            endif
!
          enddo
!
        else
#ifdef DIAGNOSTICS_BIO
          do k = N, 1, -1
            ThisFlux(k, NFlux_NewProd) = 0.0
# ifdef OXYGEN
            ThisFlux(k, OGain_NewProd) = 0.0
# endif /* OXYGEN */
          enddo
#endif /* DIAGNOSTICS_BIO */
!#ifdef AVERAGES
!! if the sun is down, set the logical variable "sun_down"
!! to true for not taking into account this time step in the averaging
!          if ((ZEROTH_TILE).and.(srflx(Istr,Jstr).eq.0.)) then
!            sun_down=.true.
!          endif
!#endif
          hel(i,j)=0.0
        endif
!
! (1) Phytoplankton grazing by Zooplankton to Zoo and Detr
! (2) Phytoplankton mortality to Detr (mu_P_D)
!
        do k=1,N
          cff1=dtdays*gmax*Zoo(k)/(K_Phyt+Phyt(k))
          cff2=dtdays*mu_P_D
          Phyt(k)=Phyt(k)/(1.+cff1+cff2)
          Zoo(k)=Zoo(k)+Phyt(k)*cff1*beta
#ifdef DIAGNOSTICS_BIO
          ThisFlux(k, NFlux_Grazing)=Phyt(k)*cff1*beta
          ThisFlux(k, NFlux_SlopFeed) = Phyt(k) * cff1 * (1.-beta)
          ThisFlux(k, NFlux_Pmort) = Phyt(k) * cff2
#endif /* DIAGNOSTICS_BIO */
          Det(k)=Det(k)+Phyt(k)*(cff1*(1.-beta)+cff2)
!
! (1) Zoo excretion to NO3  (rate mu_Z_A)
! (2) Zoo mortality to Det (rate mu_Z_D)
!
          cff1=dtdays*mu_Z_A
          cff2=dtdays*mu_Z_D*Zoo(k)
          Zoo(k)=Zoo(k)/(1.+cff1+cff2)
#ifdef DIAGNOSTICS_BIO
          ThisFlux(k, NFlux_Zmetab)=cff1*Zoo(k)
          ThisFlux(k, NFlux_Zmort)=cff2*Zoo(k)
# ifdef OXYGEN
!         Zoo uptake of O2 (rate t_Zbmet + R_C)
!         there is no control yet for assuring non-negative Oxygen
!         values!
          ThisFlux(k, OLoss_Zmetab) =
     &         ThisFlux(k, NFlux_Zmetab) * CN_Z
# endif /* OXYGEN */
#endif /* DIAGNOSTICS_BIO */
#ifdef OXYGEN 
          O2(k)  = O2(k) - cff1 * Zoo(k) * CN_Z
#endif
          NO3(k)=NO3(k)+Zoo(k)*cff1
          Det(k)=Det(k)+Zoo(k)*cff2
!
! (1) Det remineralization to N03
!
          cff1=dtdays*mu_D_N
          Det(k)=Det(k)/(1.+cff1)
#ifdef DIAGNOSTICS_BIO
          ThisFlux(k, NFlux_ReminD)=Det(k)*cff1
# ifdef OXYGEN
!         Loss of O2 in Det/Det remineralization
          ThisFlux(k, OLoss_ReminD)=
     &             ThisFlux(k,NFlux_ReminD) * (CN_Phyt+2.)
# endif /* OXYGEN */
#endif /* DIAGNOSTICS_BIO */
#ifdef OXYGEN
          O2(k) = O2(k) - Det(k) * cff1 * (CN_Phyt+2.)
#endif
          NO3(k)=NO3(k)+Det(k)*cff1
        enddo
!
#ifdef OXYGEN
#  ifdef OCMIP_OXYGEN_SC
!*********************************************************************
!  alternative formulation (Sc will be slightly smaller up to about 35
!  C)
!  Computes the Schmidt number of oxygen in seawater using the
!  formulation proposed by Keeling et al. (1998, Global Biogeochem.
!  Cycles, 12, 141-163).  Input is temperature in deg C.
!
        Sc = 1638.0 - 81.83*tem(N) + 1.483*(tem(N)**2) -
     &       0.008004*(tem(N)**3)
!*********************************************************************
#  else /* OCMIP_OXYGEN_SC */
!       calculate the Schmidt number for O2 in sea water [Wanninkhof,
!       1992]
        Sc=1953.4 - 128.0*tem(N) + 3.9918*(tem(N)**2) -
     &     0.050091*(tem(N)**3)
#  endif /* OCMIP_OXYGEN_SC */
!
!       calculate the wind speed from the surface stress values
        u10_loc = sqrt(sqrt( (0.5*(sustr(i,j)+sustr(i+1,j)))**2
     &                         +(0.5*(svstr(i,j)+svstr(i,j+1)))**2)
     &       * rho0 * 550.)   ! 550 = 1 / (1.3 * 0.0014) (=rho_air * CD)
!       calculate the gas transfer coef for O2
        Kv_O2_loc=0.31*u10_loc*u10_loc*sqrt(660./Sc)/(100.*3600.) 
!  denominator: convert Kv from [cm/h] to [m/s]
!       calculate the saturation oxygen level
#  ifdef OCMIP_OXYGENSAT
        O2satu_loc = o2sato(tem(N), sal(N))
#  else /* OCMIP_OXYGENSAT */
        call O2sato(O2(N),tem(N),sal(N),den(N),O2satu_loc,satpc,AOU)
#  endif /* OCMIP_OXYGENSAT */
!       air-sea flux of O2
!       abs(z_w(i,j,N-1))==> volume of upper layer
#  ifdef DIAGNOSTICS_BIO
        ThisGasExcFlux(OFlux_GasExc) = Kv_O2_loc * (O2satu_loc - O2(N))
     &                        * dtsec / ( z_w(i,j,N) - z_w(i,j,N-1) )
!       ThisGasExcFlux is positive if ocean takes up O2 from the
!       atmosphere
#  endif
        O2(N) = O2(N) + Kv_O2_loc * (O2satu_loc - O2(N))
     &                        * dtsec / ( z_w(i,j,N) - z_w(i,j,N-1) )  
#endif /* OXYGEN */
!
! Vertical sinking: Vertical advection algorithm based on monotonic,
! continuous conservative parabolic splines.
!
        do k=1,N
          SB(k,1)=theta(i,j,k)*Phyt(k)*CN_Phyt*12.
          SB(k,2)=Phyt(k)
          SB(k,3)=Det(k)
        enddo
        wSB(1)=wPhyt
        wSB(2)=wPhyt
        wSB(3)=wDet

        do iB=1,nsink

! Part (i): Construct parabolic splines: compute vertical derivatives
! of the fields SB. The derivatives are located at W-points;
! Neumann boundary conditions are assumed on top and bottom.
!
          dSB(0,iB)=0.
          FC(0)=0.
          cff6=6.
          do k=1,N-1
            cff=1./(2.*Hz(i,j,k+1)+Hz(i,j,k)*(2.-FC(k-1)))
            FC(k)=cff*Hz(i,j,k+1)
            dSB(k,iB)=cff*(cff6*(SB(k+1,iB)-SB(k,iB))
     &                             -Hz(i,j,k)*dSB(k-1,iB))
          enddo
          dSB(N,iB)=0.
          do k=N-1,1,-1     !<-- irreversible
            dSB(k,iB)=dSB(k,iB)-FC(k)*dSB(k+1,iB)
          enddo
!
! Part (ii): Convert dSB [which are now vertical derivatives
! of fields SB at the grid box interfaces] into field values
! at these interfaces, assuming parabolic profiles within each grid
! box. Restrict these values to lie between bounds determined from
! box-averaged values of grid boxes adjscent from above and below.
! (This restriction is part of PPM-like monotonization procedure.)
!
          cff=1./3.
          dSB(0,iB)=SB(1,iB) !-cff*Hz(1)*(dSB(0,iB)+0.5*dSB(1,iB))
          dSB(N,iB)=SB(N,iB) !+cff*Hz(N)*(dSB(N,iB)+0.5*dSB(N-1,iB))
          do k=2,N          !<-- irreversible
            dSB(k-1,iB)=SB(k,iB)
     &                 -cff*Hz(i,j,k)*(0.5*dSB(k,iB)+dSB(k-1,iB))
            dSB(k-1,iB)=max(dSB(k-1,iB),min(SB(k-1,iB),SB(k,iB)))
            dSB(k-1,iB)=min(dSB(k-1,iB),max(SB(k-1,iB),SB(k,iB)))
          enddo
!  
! Part (iii): Convert dSB into flux-integrated values,
! complete PPM flux limiting. This procedure starts from assigning
! Left and Right (aR,aL) values of the interpolating parabolae, then
! monotonicity conditions are checked and aL,aR are modified to fit.
! Overall, from this moment and further on it follows Colella--
! --Woodward, 1984 bombmaking code almost exactly.
!
          do k=1,N           !<-- irreversible
            FC(k)=dtdays/Hz(i,j,k)
            aR=dSB(k,iB)
            aL=dSB(k-1,iB)
            cff1=(aR-aL)*6.*(SB(k,iB)-.5*(aR+aL))
            cff2=(aR-aL)**2
            if ((aR-SB(k,iB))*(SB(k,iB)-aL).lt.0.) then
              aL=SB(k,iB)
              aR=SB(k,iB)
            elseif (cff1.gt.cff2) then
              aL=3.*SB(k,iB)-2.*aR
            elseif (cff1.lt.-cff2) then
              aR=3.*SB(k,iB)-2.*aL
            endif
            cu=wSB(iB)*FC(k)
            dSB(k-1,iB)=SB(k,iB)-(1.-cu)*(.5*(aR-aL)-(.5*(aR+aL)
     &                                   -SB(k,iB) )*(1.-2.*cu))
          enddo
          dSB(N,iB)=0.   ! Set no-flux boundary conditions at top.
!
! Apply fluxes:
!
          do k=1,N
            SB(k,iB)=SB(k,iB)+wSB(iB)*FC(k)*(dSB(k,iB)-dSB(k-1,iB))
          enddo
        enddo  ! <-- iB

#ifdef DIAGNOSTICS_BIO
        do iflux = 1, NumVSinkTerms
          ColumnMassOld(iflux) = 0.0
          ColumnMassNew(iflux) = 0.0
        end do
#endif /* DIAGNOSTICS_BIO */

        do k=1,N
          theta(i,j,k)= SB(k,1)/(SB(k,2)*CN_Phyt*12.+1.E-20)
          if (theta(i,j,k).gt.theta_m) theta(i,j,k)=theta_m
#ifdef DIAGNOSTICS_BIO
! ColumnMassOld and ColumnMassNew are needed to compute the sinking flux
! into the sediment
          ColumnMassOld(1)=ColumnMassOld(1)
     &                    +Phyt(k)
          ThisVSinkFlux(k, NFlux_VSinkP1)=Phyt(k)-SB(k,2)
#endif /* DIAGNOSTICS_BIO */
          Phyt(k) = SB(k,2)
#ifdef DIAGNOSTICS_BIO
          ColumnMassNew(1)=ColumnMassNew(1)
     &                    +Phyt(k)
#endif /* DIAGNOSTICS_BIO */
          ! detritus
#ifdef DIAGNOSTICS_BIO
          ColumnMassOld(2)=ColumnMassOld(2)
     &                    +Det(k)
          ThisVSinkFlux(k, NFlux_VSinkD1)=Det(k)-SB(k,3)
#endif /* DIAGNOSTICS_BIO */
          Det(k) = SB(k,3)
#ifdef DIAGNOSTICS_BIO
          ColumnMassNew(2)=ColumnMassNew(2)
     &                    +Det(k)
#endif /* DIAGNOSTICS_BIO */
        enddo
!
#ifdef DIAGNOSTICS_BIO
! Transfer fluxes to global arrays at the end of each biological time step
! for computational efficiency, divide now by dtsec to get the correct units
        do iflux = 1, NumFluxTerms
          do k = 1, N
            bioFlux(i,j,k,iflux) = ( bioFlux(i,j,k,iflux) +
     &                               ThisFlux(k, iflux) / dt )
# ifdef MASKING
     &             * rmask(i,j)
# endif /* MASKING */
          end do
        end do
        do iflux = 1, NumVSinkTerms
! Compute the vertical sinking flux into the sediment by comparing
! previous and current mass in this (i,j) column
! The flux is positive if upward, so usually it will be
! negative, i.e. into the sediment.
          LastVSinkFlux = ( ColumnMassNew(iflux) -
     &                      ColumnMassOld(iflux) )
          bioVSink(i,j,0,iflux) = (  bioVSink(i,j,0,iflux) +
     &                               LastVSinkFlux / dt )
# ifdef MASKING
     &          * rmask(i,j)
# endif /* MASKING */
          do k = 1, N
            LastVSinkFlux = LastVSinkFlux +
     &                      ThisVSinkFlux(k,iflux)
            bioVSink(i,j,k,iflux) =(  bioVSink(i,j,k,iflux) +
     &                                LastVSinkFlux / dt )
# ifdef MASKING
     &          * rmask(i,j)
# endif /* MASKING */
          end do
        end do
# ifdef OXYGEN
! ThisGasExcFlux is already in the correct units [mmol s^-1]
        do iflux = 1, NumGasExcTerms
           GasExcFlux(i,j,iflux) = ( GasExcFlux(i,j,iflux) +
     &                               ThisGasExcFlux(iflux) / dt )
#  ifdef MASKING
     &          * rmask(i,j)
#  endif /* MASKING */
        end do
# endif
#endif /* DIAGNOSTICS_BIO */
!
      ENDDO  ! <-- ITER
!
! Write back
!
!          print*,'N=',N
#undef DEBUG_BIO
# if defined DIAGNOSTICS_BIO && defined DEBUG_BIO
!
      k=N
      if ((i.eq.13).and.(j.eq.15)) then
        bilan_no3 = bioFlux(i,j,k,NFlux_Zmetab)
     &           + bioFlux(i,j,k,NFlux_ReminD)
     &           - bioFlux(i,j,k,NFlux_NewProd)
!
        bilan_phy = bioFlux(i,j,k,NFlux_NewProd)
     &           - bioFlux(i,j,k,NFlux_Pmort)
     &           - bioFlux(i,j,k,NFlux_Grazing)
     &           - bioFlux(i,j,k,NFlux_SlopFeed)
     &           - ( bioVSink(i,j,k,NFlux_VSinkP1) 
     &               - bioVSink(i,j,k-1,NFlux_VSinkP1) )
!
        bilan_zoo = bioFlux(i,j,k,NFlux_Grazing) 
     &            - bioFlux(i,j,k,NFlux_Zmetab)
     &            - bioFlux(i,j,k,NFlux_Zmort)
!
        bilan_det = bioFlux(i,j,k,NFlux_SlopFeed)
     &            + bioFlux(i,j,k,NFlux_Zmort)
     &            + bioFlux(i,j,k,NFlux_Pmort)
     &            - bioFlux(i,j,k,NFlux_ReminD)
     &            - ( bioVSink(i,j,k,NFlux_VSinkD1) 
     &                - bioVSink(i,j,k-1,NFlux_VSinkD1) )
#   ifdef OXYGEN
        bilan_o2 = bioFlux(i,j,k, OGain_NewProd)
     &           - bioFlux(i,j,k, OLoss_Zmetab)
     &           - bioFlux(i,j,k, OLoss_ReminD)
        if (k.eq.N) then
           bilan_o2 =  bilan_o2 + GasExcFlux(i,j, OFlux_GasExc)
        endif                     
        trend_o2 = ( (min(t(i,j,k,nnew,iO2),0.) +O2(k))
     &               -   t(i,j,k,nnew,iO2) ) / dt
#   endif
        somme = bilan_no3+bilan_phy+bilan_zoo+bilan_det
        trend_no3 = ( (min(t(i,j,k,nnew,iNO3_),0.) +NO3(k))
     &               - t(i,j,k,nnew,iNO3_) ) / dt
        trend_phy = ( (min(t(i,j,k,nnew,iPhy1),0.) +Phyt(k))
     &               - t(i,j,k,nnew,iPhy1) )/ dt
        trend_zoo = ( (min(t(i,j,k,nnew,iZoo1),0.) +Zoo(k))
     &               - t(i,j,k,nnew,iZoo1) )/ dt
        trend_det = ( (min(t(i,j,k,nnew,iDet1),0.) +Det(k))
     &       - t(i,j,k,nnew,iDet1) )/ dt
!
        trend_total =  trend_no3 + trend_phy + trend_zoo + trend_det
!
        sinking_loss = - ( bioVSink(i,j,k,NFlux_VSinkD1) 
     &                     - bioVSink(i,j,k-1,NFlux_VSinkD1) )
     &                 - ( bioVSink(i,j,k,NFlux_VSinkP1) 
     &                     - bioVSink(i,j,k-1,NFlux_VSinkP1) )
! 
        print*, '=================='
        print*, 'i=',i,' j=',j,' k=',k
        print*, 'somme SMS(of N)=         ',somme
        print*, 'trend_total of N=        ',trend_total
        print*, 'Sinking_loss of N=       ',sinking_loss
        print*, 'These three values have to be the same!'
        print*, 'Error for N = trend_total-somme=',trend_total-somme
        print*, "-----------------"
        print*, 'bilan_no3 - trend_no3=', bilan_no3-trend_no3
        print*, 'bilan_phy-trend_phy=  ', bilan_phy-trend_phy
        print*, 'bilan_zoo-trend_zoo=  ', bilan_zoo-trend_zoo
        print*, 'bilan_det-trend_det=  ', bilan_det-trend_det
        print*, '-----------------'
        print*, 'bioFlux(i,j,k,NFlux_NewProd)= ', 
     &                               bioFlux(i,j,k,NFlux_NewProd)
        print*, 'bioFlux(i,j,k,NFlux_Grazing)= ', 
     &                               bioFlux(i,j,k,NFlux_Grazing)
        print*, 'bioFlux(i,j,k,NFlux_SlopFeed)=', 
     &                               bioFlux(i,j,k,NFlux_SlopFeed)
        print*, 'bioFlux(i,j,k,NFlux_Pmort)=   ', 
     &                               bioFlux(i,j,k,NFlux_Pmort)
        print*, 'bioFlux(i,j,k,NFlux_Zmetab)=  ', 
     &                               bioFlux(i,j,k,NFlux_Zmetab)
        print*, 'bioFlux(i,j,k,NFlux_Zmort)=   ', 
     &                               bioFlux(i,j,k,NFlux_Zmort)
        print*, 'bioFlux(i,j,k,NFlux_ReminD)=  ', 
     &                               bioFlux(i,j,k,NFlux_ReminD)
        print*, 'bioVSink(i,j,k,NFlux_VSinkP1)=', 
     &                                bioVSink(i,j,k,NFlux_VSinkP1)
        print*, 'bioVSink(i,j,k,NFlux_VSinkD1)=', 
     &                                bioVSink(i,j,k,NFlux_VSinkD1)
#    ifdef OXYGEN
        print*, '-----------------'
        print*, 'Error for O2 = trend_O2-SMS(O2)=',trend_o2-bilan_o2
        print*, 'bioFlux(i,j,k,OGain_NewProd)=',
     &                               bioFlux(i,j,k,OGain_NewProd)
        print*, 'bioFlux(i,j,k,OLoss_Zmetab)= ',
     &                               bioFlux(i,j,k,OLoss_Zmetab)
        print*, 'bioFlux(i,j,k,OLoss_ReminD)= ',
     &                               bioFlux(i,j,k,OLoss_ReminD)
        if (k.eq.N) then
        print*, 'GasExcFlux(i,j,OFlux_GasExc)= ',
     &                                GasExcFlux(i,j,OFlux_GasExc)
        endif 
#    endif
        print*, '=================='
      endif
# endif

          do k=1,N
            t(i,j,k,nnew,iNO3_)=min(t(i,j,k,nnew,iNO3_),0.) +NO3(k)
            t(i,j,k,nnew,iPhy1)=min(t(i,j,k,nnew,iPhy1),0.) +Phyt(k)
            t(i,j,k,nnew,iZoo1)=min(t(i,j,k,nnew,iZoo1),0.) +Zoo(k)
            t(i,j,k,nnew,iDet1)=min(t(i,j,k,nnew,iDet1),0.) +Det(k)
            t(i,j,k,nnew,iChla)=min(t(i,j,k,nnew,iChla),0.) +
     &                              CN_Phyt*12.*Phyt(k)*theta(i,j,k)
# ifdef OXYGEN
            t(i,j,k,nnew,iO2)  =min(t(i,j,k,nnew,iO2),0.)   +O2(k)
# endif
          enddo
# ifdef OXYGEN
          O2satu(i,j) = O2satu_loc
          Kv_O2(i,j) = Kv_O2_loc
          u10(i,j) = u10_loc
!         if (i==10.and.j==10) then 
!            write (*,*) 'O2satu(10,10)=',O2satu(10,10)
!            write (*,*) 'Kv_O2(10,10)=',KV_O2(10,10)
!            write (*,*) 'u10(10,10)=',u10(10,10)
!          endif
# endif /* OXYGEN */
        enddo
      enddo


#else
      subroutine biology_empty ()
#endif
      return
      end
