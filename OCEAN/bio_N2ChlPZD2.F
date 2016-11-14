! $Id: bio_N2ChlPZD2.F 1458 2014-02-03 15:01:25Z gcambon $
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
#if defined BIOLOGY && defined BIO_N2ChlPZD2

      subroutine biology_tile (Istr,Iend,Jstr,Jend)
!
! Compute biological forcing functions as defined by the
! Fasham et al. [JMR, 48, 591-639, 1990]
!
! In this particular implementation there is 7 compartments:
! NO3, NH4, Chlorophyl, PHYTOplankton, ZOOplanknton, Small Detritus, 
! and Large Detritus.
!
      implicit none
      integer Istr,Iend,Jstr,Jend
#include "param.h"
#include "grid.h"
#include "ocean3d.h"
#include "scalars.h"
#include "forces.h"
#include "diagnostics.h"
      real kwater, kChla, palpha,
     &     K_NO3, K_NH4, mu_P_Sd, mu_Agg, gmax, K_Phyt,
     &     beta, mu_Sd_A, mu_Z_A, mu_Z_Sd, mu_A_N, mu_Ld_A, 
     &     CN_Phyt, theta_m, chla_C, wPhyt, wSDet, wLDet
#ifdef DIAGNOSTICS_BIO
!     gc  
      integer l, iflux
      real ThisVSinkFlux(N, NumVSinkTerms) ! [mmol m-2 s-1], upward flux is positive
     &     , ThisFlux(N, NumFluxTerms)
     &     , somme, bilan_nh4, bilan_phy, bilan_zoo
     &     , bilan_det1, bilan_no3, bilan_det2
      real trend_no3,trend_nh4,trend_phy,trend_zoo,trend_det1
     &     , trend_det2, sinking_loss, trend_total
#endif
      integer ITERMAX
      integer nsink
      parameter (
     &  ITERMAX = 3,      ! number of small implicit time steps
     &  nsink   = NumVSinkTerms + 1, !-->  add'lly: Chlorophyll
!
! Parameters as in Table 1; Fasham et al. [JMR, 48, 591-639, 1990]
!
     &  kwater  = 0.04,   ! light attenuation due to sea water  [m-1]
     &  kChla   = 0.024,  ! light attenuation by Chlorophyl  
                          !                         [(m^2 mg Chla)-1]
     &  palpha  = 1.0,    ! initial slope of the P-I curve
                          !                             [(W m-2 d)-1]
     &  CN_Phyt = 6.625,  ! C:N ratio for phytoplankton
                          !                       [mMol C (mMol N)-1]
     &  theta_m =0.053478,! maximum Cellular Chlorophyll to Carbon
                          !                      Ratio [mg Chla/mg C]
     &  chla_C  = 1.3538, ! mass balance for chla molecule, 893.5/660.
                          !                        [mg Chla (mg C)-1]
     &  K_NO3   = 1./.75, ! inverse half-saturation for Phytoplankton
                          !         NO3 uptake       [1/(mMol N m-3)]
     &  K_NH4   = 1./.5,  ! inverse half-saturation for Phytoplankton
                          !         NH4 uptake       [1/(mMol N m-3)]
     &  mu_A_N  = 0.05,   ! Oxidation of NH4 to NO3 (Nitrification) [d-1]
     &  mu_P_Sd = 0.072,  ! Phyto mortality to SDet rate        [d-1]
     &  gmax    = 0.6,    ! maximum Zooplankton growth rate     [d-1]
     &  beta    = 0.75,   ! Zooplankton assimilation efficiency of
                          !                       Phytoplankton [n.d.]
     &  K_Phyt  = 1.0,    ! Zooplankton half-saturation constant
     &                    !                       for ingestion [d-1]
     &  mu_Z_A  = 0.10,   ! Zooplankton specific excretion rate [d-1]
     &  mu_Z_Sd = 0.025,  ! Zooplankton mortality to Detritus   [d-1] 
     &  mu_Sd_A = 0.03,   ! Small Detrital breakdown to NH4 rate [d-1]
     &  mu_Agg  = 0.005,  ! Specific (per unit Phyto+SDet) aggregation
                          !  rate:   SDet+Phyt ==> LDet [1/(mMol N * d)]
     &  mu_Ld_A = 0.01,   ! Large Detrital recycling to NH4 rate [d-1]
     &  wPhyt   = 0.5,    ! sinking velocities for Phytoplankton [m.d-1]
     &  wSDet   = 1.0,    !                        Small Detritus
     &  wLDet   =10.0  )  !                        Large Detritus
!
      integer i,j,k, ITER, iB
      real    NO3(N), NH4(N), Phyt(N), Zoo(N), 
     &        SDet(N), LDet(N), theta(N), 
     &        aJ(N),FC(0:N),
     &        PAR, attn, Vp, Epp, Q, cu, aL,aR, dtdays,
     &        cff,cff1,cff2,cff6,
     &        SB(N,4),dSB(0:N,4),wSB(4)
# if defined DIAGNOSTICS_BIO
     &      , m, dtsec        ! length of time step in seconds (for gas exchange)
     &      , LastVSinkFlux,ColumnMassOld(NumVSinkTerms)
     &      , ColumnMassNew(NumVSinkTerms)
# endif /* DIAGNOSTICS_BIO */
!
# include "compute_auxiliary_bounds.h"
!
      dtdays=dt/(24.*3600.*float(ITERMAX))  ! time step as fraction of day.
# ifdef DIAGNOSTICS_BIO
      dtsec = dt / float(ITERMAX)           ! time step in seconds
# endif /* DIAGNOSTICS_BIO */
!
!
! Since the following solver is iterative to achieve implicit
! discretization of the biological interaction, two time slices are
! required, BIO where BIO is understood as vector of
! biological state variables: BIO=[NO3,NH4,Phyt,Zoo,SDet]. Assume
! that the iterations converge, the newly obtained state variables
! satisfy equations
!
!           BIO = BIO + dtdays * rhs(BIO)
! 
! where rhs(BIO) is the vector of biological r.h.s. computed at
! the new time step. During the iterative procedure a series of
! fractional time steps is performed in a chained mode (splitting
! by different biological conversion processes) in sequence NO3 -- 
! NH4 -- Phyt -- Zoo -- SDet, that is the main food chain. In all 
! stages the concentration of the component being consumed is
! treated in fully implicit manner, so that the algorithm guarantees
! non-negative values, no matter how strong is the concentration of
! active consuming component (Phyto or Zoo).
!
! The overall algorithm, as well as any stage of it is formulated
! in conservative form (except explicit sinking) in sense that the
! sum of concentration of all five components is conserved.
!
!#  ifdef EW_PERIODIC
!#   define I_RANGE Istr,Iend
!#  else
!#   define I_RANGE IstrR,IendR
!#  endif
!#  ifdef NS_PERIODIC
!#   define J_RANGE Jstr,Jend
!#  else
!#   define J_RANGE JstrR,JendR
!#  endif
#  define I_RANGE Istr,Iend
#  define J_RANGE Jstr,Jend

      do j=J_RANGE
        do i=I_RANGE
# ifdef DIAGNOSTICS_BIO
! Reset the biogeochemical fluxes. This is necessary because the 
! biological routine uses multiple. time steps for each physical time step.
          do k=1,N
            do l=1,NumFluxTerms
              bioFlux(i,j,k,l) = 0.0
            enddo
            do l=1,NumVSinkTerms
              bioVSink(i,j,k,l) = 0.0
            enddo
          enddo
# endif /* DIAGNOSTICS_BIO */
!
! Extract biological variables from tracer arrays; place them into
! scratch variables; restrict their values to be positive definite.
!
      do k=1,N
        NO3(k) =max(t(i,j,k,nnew,iNO3_)  ,0.)  ! Nitrate
        NH4(k) =max(t(i,j,k,nnew,iNH4_)  ,0.)  ! Ammonium
        Phyt(k)=max(t(i,j,k,nnew,iPhy1)  ,0.)  ! Phytoplankton
        Zoo(k) =max(t(i,j,k,nnew,iZoo1)  ,0.)  ! Zooplankton
        SDet(k)=max(t(i,j,k,nnew,iDet1)  ,0.)  ! Small Detritus
        LDet(k)=max(t(i,j,k,nnew,iDet2)  ,0.)  ! Large Detritus
        theta(k)=max(t(i,j,k,nnew,iChla) ,0.)  ! Chla/Phyto ratio
     &        /(Phyt(k)*CN_Phyt*12. +1.E-20)
      enddo


      DO ITER=1,ITERMAX      !--> Start internal iterations to achieve
                             !    nonlinear backward-implicit solution.

        PAR=srflx(i,j)*rho0*Cp*0.43

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

            attn=exp(-0.5*(kwater+kChla *
     &               theta(k)*Phyt(k)*CN_Phyt*12.+1.e-20) *
     &               (z_w(i,j,k)-z_w(i,j,k-1)))

            PAR=PAR*attn
            Vp=0.59*(1.066**t(i,j,k,nnew,itemp))   ! From Eppley
            cff=PAR*palpha*theta(k)        
            Epp=Vp/sqrt(Vp*Vp+cff*cff)
            aJ(k)=Epp*cff

            cff=K_NO3*NO3(k)+K_NH4*NH4(k)
            Q=cff/(1.+cff)

            cff=dtdays*aJ(k)*Q
            theta(k)=(theta(k)+theta_m*Chla_c*Epp*Q*cff)/(1.+cff)
            PAR=PAR*attn
          enddo
!
! (1) NO3 uptake by Phyto
!
          do k=1,N
            cff1=dtdays*Phyt(k)*aJ(k)*K_NO3
     &               /(1.+K_NO3*NO3(k)+K_NH4*NH4(k))
            NO3(k)=NO3(k)/(1.+cff1)
#ifdef DIAGNOSTICS_BIO
            ThisFlux(k, NFlux_NewProd) = cff1*NO3(k)
#endif /* DIAGNOSTICS_BIO */
            Phyt(k)=Phyt(k)+cff1*NO3(k) 
          enddo
!
! (1) NH4 uptake by Phyto
! (2) nitrification of NH4 ==> NO3
!
          do k=1,N
            cff1=dtdays*Phyt(k)*aJ(k)*K_NH4
     &               /(1.+K_NO3*NO3(k)+K_NH4*NH4(k))
            cff2=dtdays*mu_A_N
            NH4(k)=NH4(k)/(1.+cff1+cff2)
#ifdef DIAGNOSTICS_BIO
            ThisFlux(k, NFlux_RegProd) = cff1*NH4(k)
            ThisFlux(k, NFlux_Nitrific)= cff2*NH4(k)
#endif /* DIAGNOSTICS_BIO */
            Phyt(k)=Phyt(k)+NH4(k)*cff1
            NO3(k)=NO3(k)+NH4(k)*cff2
          enddo
!
        else
!
!   *** SUN IS DOWN ***
!
! (1) nitrification of NH4 ==> NO3
!
          do k=1,N
            cff1=dtdays*mu_A_N
            NH4(k)=NH4(k)/(1.+cff1)
            NO3(k)=NO3(k)+NH4(k)*cff1
#ifdef DIAGNOSTICS_BIO
            ThisFlux(k, NFlux_NewProd) = 0.0
            ThisFlux(k, NFlux_RegProd) = 0.0
            ThisFlux(k, NFlux_Nitrific)=cff1*NH4(k)
#endif /* DIAGNOSTICS_BIO */
          enddo
!
        endif
!
! (1) Phytoplankton grazing by Zooplankton to Zoo and SDet
! (2) Phytoplankton mortality to SDet (mu_P_Sd)
!
        do k=1,N
          cff1=dtdays*gmax*Zoo(k)/(K_Phyt+Phyt(k))
          cff2=dtdays*mu_P_Sd
          Phyt(k)=Phyt(k)/(1.+cff1+cff2)
          Zoo(k)=Zoo(k)+Phyt(k)*cff1*beta
#ifdef DIAGNOSTICS_BIO
          ThisFlux(k, NFlux_Grazing)=Phyt(k)*cff1*beta
          ThisFlux(k, NFlux_SlopFeed) = Phyt(k) * cff1 * (1.-beta)
          ThisFlux(k, NFlux_Pmort) = Phyt(k) * cff2
#endif /* DIAGNOSTICS_BIO */
          SDet(k)=SDet(k)+Phyt(k)*(cff1*(1.-beta)+cff2)
        enddo
!
! (1) Zoo excretion to NH4  (rate mu_Z_A)
! (2) Zoo mortality to SDet (rate mu_Z_Sd)
!
        do k=1,N
          cff1=dtdays*mu_Z_A
          cff2=dtdays*mu_Z_Sd
          Zoo(k)=Zoo(k)/(1.+cff1+cff2)
#ifdef DIAGNOSTICS_BIO
          ThisFlux(k, NFlux_Zmetab)=cff1*Zoo(k)
          ThisFlux(k, NFlux_Zmort)=cff2*Zoo(k)
#endif /* DIAGNOSTICS_BIO */
          NH4(k)=NH4(k)+Zoo(k)*cff1
          SDet(k)=SDet(k)+Zoo(k)*cff2
        enddo
!
! (1) Coagulation of Phyt+SDet to LDet
!
        do k=1,N
          cff1=dtdays*mu_Agg*(SDet(k)+PHyt(k))
          Phyt(k)=Phyt(k)/(1.+cff1)
          SDet(k)=SDet(k)/(1.+cff1)
          LDet(k)=LDet(k)+(Phyt(k)+SDet(k))*cff1
#ifdef DIAGNOSTICS_BIO
          ThisFlux(k, NFlux_CoagPhy)= cff1 * Phyt(k)
          ThisFlux(k, NFlux_CoagSDet)= cff1 * SDet(k)
#endif /* DIAGNOSTICS_BIO */
        enddo
!
! (1) SDet breakdown to NH4
!
        do k=1,N
          cff1=dtdays*mu_Sd_A
          SDet(k)=SDet(k)/(1.+cff1)
#ifdef DIAGNOSTICS_BIO
          ThisFlux(k, NFlux_ReminD1)=SDet(k)*cff1
#endif /* DIAGNOSTICS_BIO */
          NH4(k)=NH4(k)+SDet(k)*cff1
        enddo
!
! (1) LDet recycling to NH4 (remineralization)
!
        do k=1,N
          cff1=dtdays*mu_Ld_A
          LDet(k)=LDet(k)/(1.+cff1)
#ifdef DIAGNOSTICS_BIO
          ThisFlux(k, NFlux_ReminD2)=LDet(k)*cff1
#endif /* DIAGNOSTICS_BIO */
          NH4(k)=NH4(k)+LDet(k)*cff1
        enddo

!
! Vertical sinking: Vertical advection algorithm based on monotonic,
! continuous conservative parabolic splines.
!
        do k=1,N
          SB(k,1)=theta(k)*Phyt(k)*CN_Phyt*12.
          SB(k,2)=Phyt(k)
          SB(k,3)=SDet(k)
          SB(k,4)=LDet(k)
        enddo
        wSB(1)=wPhyt
        wSB(2)=wPhyt
        wSB(3)=wSDet
        wSB(4)=wLDet

        do iB=1,4

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
        enddo
#endif /* DIAGNOSTICS_BIO */
        do k=1,N
          theta(k)=SB(k,1)/(SB(k,2)*CN_Phyt*12.+1.E-20)
#ifdef DIAGNOSTICS_BIO
! ColumnMassOld and ColumnMassNew are needed to compute the sinking flux
! into the sediment
!       
!         Phyto
          ColumnMassOld(1)=ColumnMassOld(1) + Phyt(k)
          ThisVSinkFlux(k, NFlux_VSinkP1)=Phyt(k)-SB(k,2)
#endif /* DIAGNOSTICS_BIO */
          Phyt(k) =SB(k,2)
#ifdef DIAGNOSTICS_BIO
          ColumnMassNew(1) = ColumnMassNew(1)+Phyt(k)
#endif /* DIAGNOSTICS_BIO */
!
!         Small detritus
#ifdef DIAGNOSTICS_BIO
          ColumnMassOld(2) = ColumnMassOld(2)+SDet(k)
          ThisVSinkFlux(k, NFlux_VSinkD1) = SDet(k)-SB(k,3)
#endif /* DIAGNOSTICS_BIO */
          SDet(k) =SB(k,3)
#ifdef DIAGNOSTICS_BIO
          ColumnMassNew(2) = ColumnMassNew(2)+SDet(k)
#endif /* DIAGNOSTICS_BIO */
!
!         Large detritus
#ifdef DIAGNOSTICS_BIO
          ColumnMassOld(3) = ColumnMassOld(3)+LDet(k)
          ThisVSinkFlux(k, NFlux_VSinkD2) = LDet(k)-SB(k,4)
#endif /* DIAGNOSTICS_BIO */
          LDet(k) =SB(k,4)
#ifdef DIAGNOSTICS_BIO
          ColumnMassNew(3) = ColumnMassNew(3)+LDet(k)
#endif /* DIAGNOSTICS_BIO */
        enddo
!
!
#ifdef DIAGNOSTICS_BIO
! Transfer fluxes to global arrays at the end of each biological time step
! for computational efficiency, divide now by dtsec to get the correct units
        do iflux = 1, NumFluxTerms
           do k = 1, N
! biological Flux in mmolN/s
              bioFlux(i,j,k,iflux) =  bioFlux(i,j,k,iflux) +
     &                                ThisFlux(k, iflux) / dt
# ifdef MASKING
     &             * rmask(i,j)
# endif /* MASKING */
           end do
        end do
        do iflux = 1, NumVSinkTerms

! Compute the vertical sinking flux into the sediment by comparing
! previous and current mass in this (i,j) column
! The flux is positive if upward, so usually it will be negative, i.e.
! into the sediment.
           LastVSinkFlux = ( ColumnMassNew(iflux) -
     &                       ColumnMassOld(iflux) )
           bioVSink(i,j,0,iflux) = (  bioVSink(i,j,0,iflux) +
     &                                LastVSinkFlux / dt )
#  ifdef MASKING
     &          * rmask(i,j)
#  endif /* MASKING */
           do k = 1, N
              LastVSinkFlux = LastVSinkFlux +
     &                        ThisVSinkFlux(k,iflux)

              bioVSink(i,j,k,iflux) =(  bioVSink(i,j,k,iflux) +
     &                                  LastVSinkFlux / dt )
#  ifdef MASKING
     &             * rmask(i,j)
#  endif /* MASKING */
           end do
         enddo
#endif /* DIAGNOSTICS_BIO */ 
!! 
      ENDDO  ! <-- ITER

!
! Write back
!
!
#undef DEBUG_BIO
# if defined DIAGNOSTICS_BIO && defined DEBUG_BIO
!
      k=N
      if ((i.eq.13) .and. (j.eq.15)) then

        bilan_no3 = bioFlux(i,j,k,NFlux_Nitrific)
     &           -bioFlux(i,j,k,NFlux_NewProd)
!
        bilan_nh4 = bioFlux(i,j,k,NFlux_ReminD1)
     &           +bioFlux(i,j,k,NFlux_ReminD2)
     &           +bioFlux(i,j,k,NFlux_Zmetab)
     &           -bioFlux(i,j,k,NFlux_RegProd)
     &           -bioFlux(i,j,k,NFlux_Nitrific)
!
        bilan_phy = bioFlux(i,j,k,NFlux_NewProd)
     &            +bioFlux(i,j,k,NFlux_RegProd)
     &            -bioFlux(i,j,k,NFlux_Grazing)/beta
     &            -bioFlux(i,j,k,NFlux_Pmort)
     &            -bioFlux(i,j,k,NFlux_CoagPhy)
     &            + ( bioVSink(i,j,k-1,NFlux_VSinkP1)
     &                - bioVSink(i,j,k,NFlux_VSinkP1) )
!
        bilan_zoo = bioFlux(i,j,k,NFlux_Grazing)
     &            -bioFlux(i,j,k,NFlux_Zmetab)
     &            -bioFlux(i,j,k,NFlux_Zmort)
!
        bilan_det1 = bioFlux(i,j,k,NFlux_Pmort)
     &            +bioFlux(i,j,k,NFlux_SlopFeed)
     &            +bioFlux(i,j,k,NFlux_Zmort)
     &            -bioFlux(i,j,k,NFlux_ReminD1)
     &            -bioFlux(i,j,k,NFlux_CoagSDet)
     &            + ( bioVSink(i,j,k-1,NFlux_VSinkD1)
     &                - bioVSink(i,j,k,NFlux_VSinkD1) )
!
        bilan_det2 = -bioFlux(i,j,k,NFlux_ReminD2)
     &             +bioFlux(i,j,k,NFlux_CoagPhy)
     &             +bioFlux(i,j,k,NFlux_CoagSDet)
     &             + ( bioVSink(i,j,k-1,NFlux_VSinkD2)
     &                 - bioVSink(i,j,k,NFlux_VSinkD2) )
!     
        somme = bilan_no3+bilan_nh4+bilan_phy+bilan_zoo
     &          +bilan_det1+bilan_det2
!
        trend_no3 = ( (min(t(i,j,k,nnew,iNO3_),0.) +NO3(k))
     &              - t(i,j,k,nnew,iNO3_) ) / dt
        trend_nh4 = ( (min(t(i,j,k,nnew,iNH4_),0.) +NH4(k))
     &               - t(i,j,k,nnew,iNH4_) ) / dt
        trend_phy = ( (min(t(i,j,k,nnew,iPhy1),0.) +Phyt(k))
     &       - t(i,j,k,nnew,iPhy1) )/ dt
        trend_zoo = ( (min(t(i,j,k,nnew,iZoo1),0.) +Zoo(k))
     &       - t(i,j,k,nnew,iZoo1) )/ dt
        trend_det1 = ( (min(t(i,j,k,nnew,iDet1),0.) +SDet(k))
     &       - t(i,j,k,nnew,iDet1) )/ dt
        trend_det2 = ( (min(t(i,j,k,nnew,iDet2),0.) +LDet(k))
     &       - t(i,j,k,nnew,iDet2) )/ dt
!--
        trend_total =  trend_no3 + trend_nh4 + trend_phy +
     &       trend_zoo + trend_det1 + trend_det2
!--
        sinking_loss = + ( bioVSink(i,j,k-1,NFlux_VSinkD2) - 
     &                                bioVSink(i,j,k,NFlux_VSinkD2) )
     &       + ( bioVSink(i,j,k-1,NFlux_VSinkD1) - 
     &                                bioVSink(i,j,k,NFlux_VSinkD1) )
     &       + ( bioVSink(i,j,k-1,NFlux_VSinkP1) - 
     &                                bioVSink(i,j,k,NFlux_VSinkP1) )
!-- 
        print*, '=================='   
        print*, 'i=',i,' j=',j,' k=',k
        print*, 'somme SMS(of N)=         ',somme
        print*, 'trend_total of N=        ',trend_total
        print*, 'Sinking_loss of N=       ',sinking_loss
        print*, 'These three values have to be the same!'
        print*, 'Error for N = trend_total-somme=',trend_total-somme
        print*, "-----------------"
        print*, 'bilan_no3 - trend_no3=  ', bilan_no3-trend_no3
        print*, 'bilan_nh4 - trend_nh4=  ', bilan_nh4-trend_nh4
        print*, 'bilan_phy-trend_phy=    ', bilan_phy-trend_phy
        print*, 'bilan_zoo-trend_zoo=    ', bilan_zoo-trend_zoo
        print*, 'bilan_det1-trend_det1=  ', bilan_det1-trend_det1
        print*, 'bilan_det2-trend_det2=  ', bilan_det2-trend_det2
        print*, "-----------------"
        print*, 'trend_no3= ', trend_no3
        print*, 'trend_nh4= ', trend_nh4
        print*, 'trend_phy= ', trend_phy
        print*, 'trend_zoo= ', trend_zoo
        print*, 'trend_det1=', trend_det1 
        print*, 'trend_det2=', trend_det2
        print*, "-----------------"
        print*, 'bilan_no3= ', bilan_no3
        print*, 'bilan_nh4= ', bilan_nh4
        print*, 'bilan_phy= ', bilan_phy
        print*, 'bilan_zoo= ', bilan_zoo
        print*, 'bilan_det1=', bilan_det1 
        print*, 'bilan_det2=', bilan_det2
        print*, "-----------------"
        print*, 'bioVSinkP1=', bioVSink(i,j,k,NFlux_VSinkP1)
        print*, 'bioVSinkD1=', bioVSink(i,j,k,NFlux_VSinkD1)
        print*, 'bioVSinkD2=', bioVSink(i,j,k,NFlux_VSinkD2)
        print*, "=================="
        endif
#endif /* DIAGNOSTICS_BIO */
          do k=1,N
           t(i,j,k,nnew,iNO3_)=min(t(i,j,k,nnew,iNO3_),0.) +NO3(k)
           t(i,j,k,nnew,iNH4_)=min(t(i,j,k,nnew,iNH4_),0.) +NH4(k)
           t(i,j,k,nnew,iPhy1)=min(t(i,j,k,nnew,iPhy1),0.) +Phyt(k)
           t(i,j,k,nnew,iZoo1)=min(t(i,j,k,nnew,iZoo1),0.) +Zoo(k)
           t(i,j,k,nnew,iDet1)=min(t(i,j,k,nnew,iDet1),0.) +SDet(k)
           t(i,j,k,nnew,iDet2)=min(t(i,j,k,nnew,iDet2),0.) +LDet(k)
           t(i,j,k,nnew,iChla)=min(t(i,j,k,nnew,iChla),0.) +
     &                                 CN_Phyt*12.*Phyt(k)*theta(k)
          enddo
        enddo
      enddo

#else
      subroutine biology_empty ()
#endif
      return
      end
