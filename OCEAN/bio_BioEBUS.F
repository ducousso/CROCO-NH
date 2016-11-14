! $Id: bio_BioEBUS.F 1427 2014-01-17 10:58:03Z gcambon $
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
!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
! **********************************************************
! ***********************  BioEBUS  ************************
! Biogeochemical model for Eastern Boundary Upweling Systems
!
! BioEBUS routine is under CeCILL-C licence N°: 06572-01
!
! **********************************************************
! Elodie GUTKNECHT : 2007 - 2011
! Isabelle DADOU (LEGOS, Toulouse, France)
!
!
! if you have any question on BioEBUS model, please contact:
! Elodie Gutknecht (elodie.gutknecht@legos.obs-mip.fr)
! Isabelle Dadou (isabelle.dadou@legos.obs-mip.fr)
!
! References to cite for any publication and communication using BioEBUS model:
!
! Gutknecht, E., Dadou, I., Le Vu, B., Cambon, G., Sudre, J., Garçon, V., 
! Machu, E., Rixen, T., Kock, A., Flohr, A., Paulmier, A., and Lavik, G.: 
! Coupled physical/biogeochemical modeling including O2-dependent processes 
! in the Eastern Boundary Upwelling Systems: application in the Benguela, 
! Biogeosciences, 10, 3559-3591, doi:10.5194/bg-10-3559-2013, 2013.
!
!
! Gutknecht, E., Dadou, I., Marchesiello, P., Cambon, G., Le Vu, B., Sudre, J., 
! Garçon, V., Machu, E., Rixen, T., Kock, A., Flohr, A., Paulmier, A., and Lavik, G.: 
! Nitrogen transfers off Walvis Bay: a 3-D coupled physical/biogeochemical 
! modeling approach in the Namibian upwelling system, 
! Biogeosciences, 10, 4117-4135, doi:10.5194/bg-10-4117-2013, 2013.
!
!
! based on NChlPZD, N2ChlPZD2, and N2P2Z2D2 models
! new tracers: NO2, DON, O2, N2O
! + air-sea O2, N2O fluxes
! + biogeochemical fluxes
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------


#include "cppdefs.h"

#ifdef BIOLOGY

      subroutine biology_tile (Istr,Iend,Jstr,Jend)

! Compute biological forcing functions as defined by the
! Fasham et al. [JMR, 48, 591-639, 1990]
! Huret et al (2005, Continant shelf research)
! Kone et al (2005, Global Biogeochemical Cycles)
! Yakushev et al., (2007)
!
! In this particular implementation there is 11 compartments:
! NO3, NO2, NH4, small phyto (SPhy), large phyto (LPhy),
! small zoo (SZoo), large zoo (LZoo),
! small detritus (SDet), large detritus (LDet),
! dissolved organic nitrogen (DON), dissolved oxygen (O2)
! and there is 1 optional compartment:
! nitrous oxide (N2O)


      implicit none

      integer Istr,Iend,Jstr,Jend

# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "diagnostics.h"
# include "scalars.h"
# include "forces.h"
# include "mixing.h"
# include "cste_bio_coastal.h"


      integer    nsink
      parameter (nsink   = NumVSinkTerms + 1) ! add'lly: Chlorophyll

      integer    i, j, k, ITER, iB, itrc
      real       dtdays

      real       NO3(N), NO2(N), NH4(N), SPhy(N), LPhy(N)
     &         , SZoo(N), LZoo(N), SDet(N), LDet(N), DON(N)
     &         , O2(N), FluxO2
# ifdef NITROUS_OXIDE
     &         , N2O(N), function_O2
     &         , Scn2o, aKwn2o, N2O_eq, N2Oeq, FluxN2O
# endif

      real       aJ_1(N), aJ_2(N), FC(0:N), Ft_1(N), Ft_2(N)
     &         , PAR, opc, PARsup, attn, Vp1, Vp2, Epp1, Epp2, cu
     &         , aL, aR, cff, cff1, cff2, cff3, cff4, cff5, cff6
     &         , cff7, cff8, cff9, cff10, cffa, cffb, nitrif(N)
     &         , SB(N,nsink), dSB(0:N,nsink), wSB(nsink)
     &         , Fox, RemD1_O2, RemD2_O2, RemDON_O2, Fdnox
     &         , FdnNO3, FdnNO2, Denitr1, Denitr2
     &         , zdet1nh4, zdet2nh4, zdonnh4
      real       o2sat(N),o2sato,Sco2,aKwo2
      real       patm,P0,rhoo(N),wind_loc,rho_air,CD


# ifdef DIAGNOSTICS_BIO
      real       ThisVSinkFlux(N, NumVSinkTerms)  ! Upward flux is positive
     &         , ThisFlux(N, NumFluxTerms)    
     &         , ThisGasExcFlux(NumGasExcTerms)   
      real       budget_NO3 ,budget_NO2 ,budget_NH4 ,budget_SPhy,budget_LPhy
     &         , budget_SZoo,budget_LZoo,budget_SDet,budget_LDet,budget_DON
     &         , budget_O2  ,budget_N
#  ifdef NITROUS_OXIDE
     &         , budget_nitrous_oxide, trend_N2O
#  endif 
      real       trend_NO3 ,trend_NO2 ,trend_NH4 ,trend_SPhy,trend_LPhy
     &         , trend_SZoo,trend_LZoo,trend_SDet,trend_LDet,trend_DON
     &         , trend_O2  ,trend_N
      real       N_loss,sinking
      integer    iflux, l
      real       dtsec    
     &         , LastVSinkFlux, ColumnMassOld(0:NumVSinkTerms) 
     &         , ColumnMassNew(0:NumVSinkTerms)
# endif


# include "compute_auxiliary_bounds.h"

! dt: physical time step (s)

      dtdays= dt / (24.*3600.*float(ITERMAX))  ! biological time step (d)

# ifdef DIAGNOSTICS_BIO
      dtsec = dt / float(ITERMAX)              ! biological time step (s)
# endif /* DIAGNOSTICS_BIO */
!
!
! Since the following solver is iterative to achieve implicit
! discretization of the biological interaction, two time slices are
! required, BIO where BIO is understood as vector of
! biological state variables: BIO=[NO3,NO2,NH4,SPhy,LPhy,SZoo,LZoo,SDet,LDet,DON].
! Assume that the iterations converge, the newly obtained state variables
! satisfy equations
!
!           BIO = BIO + dtdays * rhs(BIO)
!
! where rhs(BIO) is the vector of biological r.h.s. computed at
! the new time step. During the iterative procedure a series of
! fractional time steps is performed in a chained mode (splitting
! by different biological conversion processes) in sequence NO3 -- NO2 --
! NH4 -- SPhy -- LPhy -- SZoo -- LZoo -- SDet -- LDet -- DON, 
! that is the main food chain. In all stages the concentration of the component 
! being consumed is treated in fully implicit manner, so that the algorithm guarantees
! non-negative values, no matter how strong is the concentration of
! active consuming component (Phyto or Zoo).
!
! The overall algorithm, as well as any stage of it is formulated
! in conservative form (except explicit sinking) in sense that the
! sum of concentration of all components is conserved.
!

/*
# ifdef EW_PERIODIC
#  define I_RANGE Istr,Iend
# else
#  define I_RANGE IstrR,IendR
# endif

# ifdef NS_PERIODIC
#  define J_RANGE Jstr,Jend
# else
#  define J_RANGE JstrR,JendR
# endif
*/
# define I_RANGE Istr,Iend
# define J_RANGE Jstr,Jend


! COEFFICIENT USED FOR GAZ TRANSFERT VELOVITY 
!-------------------------------------------------------------------------------------
# undef LISS_MERLIVAT  		/*Liss and Merlivat (1986)*/
# define WANNINKHOF    		/*Wanninkhof (1992)*/


# ifdef NITROUS_OXIDE
! PARAMETERIZATION USED FOR N2O PRODUCTION 
!-------------------------------------------------------------------------------------
#  undef NEVISON  		/*Nevison et al. (2003)*/
#  define SUNTHARALINGAM    	/*Suntharalingam et al. (2000,2012)*/
# endif



      patm = 1.0                ! (atm), use mean value for atmospheric pressure.
                                ! in bulk_flux.F: patm = 1013.0 mbar = 1 atm
      P0 = 1.0                  ! (atm)
      rho_air = 1.3             ! air density (kg.m-3)
      CD = 0.0014               ! drag coefficient ( )


      do j=J_RANGE
        do i=I_RANGE



!--------------------------------------------------------------------
! calculate the wind speed (m.s-1) from the surface stress values
! sustr and svstr  (m2.s-2)

        wind_loc = sqrt(sqrt( (0.5*(sustr(i,j)+sustr(i+1,j)))**2
     &                       +(0.5*(svstr(i,j)+svstr(i,j+1)))**2)
     &                       *rho0/(rho_air*CD))

!--------------------------------------------------------------------


# ifdef DIAGNOSTICS_BIO
! Reset the biogeochemical fluxes. This is necessary because the 
! biological routine uses multiple time steps for each physical time step.
          do k=1,N
            do l=1,NumFluxTerms
              ThisFlux(k,l) = 0.0
              bioFlux(i,j,k,l) = 0.0
            enddo
            do l=1,NumVSinkTerms
              ThisVSinkFlux(k,l) = 0.0
            enddo
          enddo
          do k=0,N
            do l=1,NumVSinkTerms
              bioVSink(i,j,k,l) = 0.0
            enddo
          enddo
          do l=1,NumGasExcTerms
            ThisGasExcFlux(l) = 0.0
            GasExcFlux(i,j,l) = 0.0
          enddo
# endif /* DIAGNOSTICS_BIO */

# ifdef MASKING
        if (rmask(i,j) .eq. 1) then
# endif /* MASKING */
!
! Extract biological variables from tracer arrays; place them into
! scratch variables; restrict their values to be positive definite.


      do k=1,N

        NO3(k)   = max(t(i,j,k,nnew,iNO3_),0.)  ! Nitrate
        NO2(k)   = max(t(i,j,k,nnew,iNO2_),0.)  ! Nitrite
        NH4(k)   = max(t(i,j,k,nnew,iNH4_),0.)  ! Ammonium
        SPhy(k)  = max(t(i,j,k,nnew,iPhy1),0.)  ! Small Phytoplankton
        LPhy(k)  = max(t(i,j,k,nnew,iPhy2),0.)  ! Large Phytoplankton
        SZoo(k)  = max(t(i,j,k,nnew,iZoo1),0.)  ! Small Zooplankton
        LZoo(k)  = max(t(i,j,k,nnew,iZoo2),0.)  ! Large Zooplankton
        SDet(k)  = max(t(i,j,k,nnew,iDet1),0.)  ! Small Detritus
        LDet(k)  = max(t(i,j,k,nnew,iDet2),0.)  ! Large Detritus
        DON(k)   = max(t(i,j,k,nnew,iDON),0.)   ! Dissolved Organic Nitrogen
        O2(k)    = max(t(i,j,k,nnew,iO2),0.)  ! Dissolved Oxygen
# ifdef NITROUS_OXIDE
        N2O(k)   = max(t(i,j,k,nnew,iN2O),0.)   ! Nitrous oxide
# endif

      enddo


      DO ITER=1,ITERMAX      !--> Start internal iterations to achieve
                             !    nonlinear backward-implicit solution.

        PAR=srflx(i,j)*rho0*Cp*0.43                                     ![W/m2]
        opc=0.01*PAR                                                    ![W/m2]

! srflx (°C m/s)
! rho0 (kg/m3)
! Cp (J/kg/°C)
! 0.43: part of the light used to photosynthesis



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
! compute on the lower grid-box interface;
!


          do k=N,1,-1     !<-- irreversible

! Light intensity

            attn=exp(-0.5*(kwater+kChla*
     &               theta_m*CN_Phyt*(SPhy(k)+LPhy(k))*12.+1.e-20)
     &                   *(z_w(i,j,k)-z_w(i,j,k-1)))                    ![ ]


            PAR=PAR*attn                                                ![W/m2]
            PARsup=PAR                                                  ![W/m2]


! Temperature effect (From Eppley, 1972)

            Vp1=abio1*(bbio**(cbio*t(i,j,k,nnew,itemp)))                ![d-1]
            Vp2=abio2*(bbio**(cbio*t(i,j,k,nnew,itemp)))                ![d-1]


! Light and temperature Limitations

            cff1=PAR*palpha1                                            ![d-1] 
            cff2=PAR*palpha2                                            ![d-1] 

            Epp1=Vp1/sqrt(Vp1*Vp1+cff1*cff1)                            ![ ]
            Epp2=Vp2/sqrt(Vp2*Vp2+cff2*cff2)                            ![ ] 

            aJ_1(k)=Epp1*cff1                                           ![d-1] 
            aJ_2(k)=Epp2*cff2                                           ![d-1]

# ifdef DIAGNOSTICS_BIO
            ThisFlux(k,NFlux_lightlimitP1)= aJ_1(k)/Vp1                 ![ ]
            ThisFlux(k,NFlux_lightlimitP2)= aJ_2(k)/Vp2                 ![ ]
            ThisFlux(k,NFlux_templimitP1)= Vp1                          ![d-1]
            ThisFlux(k,NFlux_templimitP2)= Vp2                          ![d-1]
# endif



! ====================================================================
! 1. NO3, NO2 and NH4 uptake by SPhy and LPhy 
!    Nutrient limitation terms:--> Yakushev et al (2007) 
!    Exudation of SPhy and LPhy to DON
! ====================================================================

! NO3 uptake by SPhy and exudation
! ====================================================================

             cff=exp(-K_psi*NH4(k))/(K1_NO32+NO3(k)+NO2(k))             !(mmolN/m3)-1
             cff1=dtdays*aJ_1(k)*SPhy(k)*cff                            ![ ]
             NO3(k)=NO3(k)/(1.+cff1)                                    !mmolN/m3
# ifdef DIAGNOSTICS_BIO
             ThisFlux(k,NFlux_ProdNO3P1)= cff1*NO3(k)                   !mmolN/m3
             ThisFlux(k,NFlux_NO3limitP1)=NO3(k)*cff                    ![ ]
# endif
             SPhy(k)=SPhy(k)+cff1*NO3(k)*(1.-epsilon1)                  !mmolN/m3
             DON(k)=DON(k)+cff1*NO3(k)*epsilon1                         !mmolN/m3
             O2(k)=O2(k)+ro2n*cff1*NO3(k)                               !mmolO2/m3


! NO3 uptake by LPhy and exudation
! ====================================================================

             cff=exp(-K_psi*NH4(k))/(K2_NO32+NO3(k)+NO2(k))             !(mmolN/m3)-1
             cff2=dtdays*aJ_2(k)*LPhy(k)*cff                            ![ ] 
             NO3(k)=NO3(k)/(1.+cff2)                                    !mmolN/m3
# ifdef DIAGNOSTICS_BIO
             ThisFlux(k,NFlux_ProdNO3P2)= cff2*NO3(k)                   !mmolN/m3  
             ThisFlux(k,NFlux_NO3limitP2)=NO3(k)*cff                    ![ ]
# endif
             LPhy(k)=LPhy(k)+cff2*NO3(k)*(1.-epsilon2)                  !mmolN/m3
             DON(k)=DON(k)+cff2*NO3(k)*epsilon2                         !mmolN/m3
             O2(k)=O2(k)+ro2n*cff2*NO3(k)                               !mmolO2/m3


! NO2 uptake by SPhy and exudation
! ====================================================================

             cff=exp(-K_psi*NH4(k))/(K1_NO32+NO3(k)+NO2(k))             !(mmolN/m3)-1
             cff1=dtdays*aJ_1(k)*SPhy(k)*cff                            ![ ]
             NO2(k)=NO2(k)/(1.+cff1)                                    !mmolN/m3
# ifdef DIAGNOSTICS_BIO
             ThisFlux(k,NFlux_ProdNO2P1)= cff1*NO2(k)                   !mmolN/m3   
             ThisFlux(k,NFlux_NO2limitP1)=NO2(k)*cff                    ![ ]
# endif
             SPhy(k)=SPhy(k)+cff1*NO2(k)*(1.-epsilon1)                  !mmolN/m3
             DON(k)=DON(k)+cff1*NO2(k)*epsilon1                         !mmolN/m3
             O2(k)=O2(k)+ro2n*cff1*NO2(k)                               !mmolO2/m3


! NO2 uptake by LPhy and exudation
! ====================================================================

             cff=exp(-K_psi*NH4(k))/(K2_NO32+NO3(k)+NO2(k))             !(mmolN/m3)-1
             cff2=dtdays*aJ_2(k)*LPhy(k)*cff                            ![ ]
             NO2(k)=NO2(k)/(1.+cff2)                                    !mmolN/m3
# ifdef DIAGNOSTICS_BIO 
             ThisFlux(k,NFlux_ProdNO2P2)= cff2*NO2(k)                   !mmolN/m3 
             ThisFlux(k,NFlux_NO2limitP2)=NO2(k)*cff                    ![ ]
# endif
             LPhy(k)=LPhy(k)+cff2*NO2(k)*(1.-epsilon2)                  !mmolN/m3
             DON(k)=DON(k)+cff2*NO2(k)*epsilon2                         !mmolN/m3
             O2(k)=O2(k)+ro2n*cff2*NO2(k)                               !mmolO2/m3


! NH4 uptake by SPhy and exudation
! nitrification of NH4 ==> NO2 ==> NO3
! ====================================================================

             cff=1./(K1_NH4+NH4(k))                                     !(mmolN/m3)-1
             cff1=dtdays*aJ_1(k)*SPhy(k)*cff                            ![ ]
             cffa=dtdays*K_NH4_NO2*O2(k)/(O2(k)+O2nf)                   ![ ]
             cffb=dtdays*K_NO2_NO3*O2(k)/(O2(k)+O2nf)                   ![ ]

             NH4(k)=NH4(k)/(1.+cff1+cffa)                               !mmolN/m3
             NO2(k)=(NO2(k)+cffa*NH4(k))/(1.+cffb)                      !mmolN/m3
             NO3(k)=NO3(k)+cffb*NO2(k)                                  !mmolN/m3
# ifdef DIAGNOSTICS_BIO
             ThisFlux(k,NFlux_Nitrif1)= cffa*NH4(k)                     !mmolN/m3
             ThisFlux(k,NFlux_Nitrif2)= cffb*NO2(k)                     !mmolN/m3
             ThisFlux(k,NFlux_ProdNH4P1)= cff1*NH4(k)                   !mmolN/m3
             ThisFlux(k,NFlux_NH4limitP1)= NH4(k)*cff                   ![ ]
# endif
             nitrif(k)=1.5*cffa*NH4(k)+0.5*cffb*NO2(k)                  !mmolO2/m3
             SPhy(k)=SPhy(k)+cff1*NH4(k)*(1.-epsilon1)                  !mmolN/m3
             DON(k)=DON(k)+cff1*NH4(k)*epsilon1                         !mmolN/m3
             O2(k)=O2(k)+ro2n*cff1*NH4(k)-nitrif(k)                     !mmolO2/m3


! NH4 uptake by LPhy and exudation
! ====================================================================

             cff=1./(K2_NH4+NH4(k))                                     !(mmolN/m3)-1
             cff2=dtdays*aJ_2(k)*LPhy(k)*cff                            ![ ]

             NH4(k)=NH4(k)/(1.+cff2)                                    !mmolN/m3
# ifdef DIAGNOSTICS_BIO
             ThisFlux(k,NFlux_ProdNH4P2)= cff2*NH4(k)                   !mmolN/m3
             ThisFlux(k,NFlux_NH4limitP2)= NH4(k)*cff                   ![ ]
# endif
             LPhy(k)=LPhy(k)+cff2*NH4(k)*(1.-epsilon2)                  !mmolN/m3
             DON(k)=DON(k)+cff2*NH4(k)*epsilon2                         !mmolN/m3
             O2(k)=O2(k)+ro2n*cff2*NH4(k)                               !mmolO2/m3


            PAR=PAR*attn                                                ![W/m2]	

! Calculation of the euphotic depth 
! (z_w is negative and hel is positive)

            if (PARsup.ge.opc) then
              if (PAR.ge.opc) then
                hel(i,j)=-z_w(i,j,k-1)                                  ![m]
              else
                hel(i,j)=-z_r(i,j,k)                                    ![m]
              endif
            endif

          enddo


        else

!
! *** SUN IS DOWN ***
!
          do k = N, 1, -1

# ifdef DIAGNOSTICS_BIO
             ThisFlux(k,NFlux_lightlimitP1)= 0.
             ThisFlux(k,NFlux_lightlimitP2)= 0.
             ThisFlux(k,NFlux_templimitP1)= 0.
             ThisFlux(k,NFlux_templimitP2)= 0.
             ThisFlux(k,NFlux_NO3limitP1)= 0.
             ThisFlux(k,NFlux_NO3limitP2)= 0.
             ThisFlux(k,NFlux_NO2limitP1)= 0.
             ThisFlux(k,NFlux_NO2limitP2)= 0.
             ThisFlux(k,NFlux_NH4limitP1)= 0.
             ThisFlux(k,NFlux_NH4limitP2)= 0.
             ThisFlux(k,NFlux_ProdNO3P1)= 0.
             ThisFlux(k,NFlux_ProdNO3P2)= 0.
             ThisFlux(k,NFlux_ProdNO2P1)= 0.
             ThisFlux(k,NFlux_ProdNO2P2)= 0.
             ThisFlux(k,NFlux_Nitrif1)= 0.
             ThisFlux(k,NFlux_Nitrif2)= 0.
             ThisFlux(k,NFlux_ProdNH4P1)= 0.
             ThisFlux(k,NFlux_ProdNH4P2)= 0.
# endif


! ====================================================================
! 2. Dark Nitrification of NH4 ==>  NO2 ==> NO3
! ====================================================================

             cff1=dtdays*K_NH4_NO2*O2(k)/(O2(k)+O2nf)                   ![ ]
             cff2=dtdays*K_NO2_NO3*O2(k)/(O2(k)+O2nf)                   ![ ]

             NH4(k)=NH4(k)/(1.+cff1)                                    !mmolN/m3
             NO2(k)=(NO2(k)+cff1*NH4(k))/(1.+cff2)                      !mmolN/m3
# ifdef DIAGNOSTICS_BIO
             ThisFlux(k,NFlux_Nitrif1)= cff1*NH4(k)                     !mmolN/m3
             ThisFlux(k,NFlux_Nitrif2)= cff2*NO2(k)                     !mmolN/m3
# endif
             NO3(k)=NO3(k)+cff2*NO2(k)                                  !mmolN/m3
             nitrif(k)=1.5*cff1*NH4(k)+0.5*cff2*NO2(k)                  !mmolO2/m3
             O2(k)=O2(k)-nitrif(k)                                      !mmolO2/m3

        enddo


          hel(i,j)=0.0


        endif   ! (PAR.gt.0.)



        do k=1,N


! ====================================================================
! 3. Grazing by SZoo and LZoo and fecal pellets (non assimilated)
!    SPhy and LPhy mortality to SDet 
! ====================================================================


! ---> Ft_1:Total Food available for SZoo
! ---> Ft_2:Total Food available for LZoo

          Ft_1(k)=e_11*SPhy(k)+e_12*LPhy(k)                             !mmolN/m3
          Ft_2(k)=e_21*SPhy(k)+e_22*LPhy(k)+e_zz*SZoo(k)                !mmolN/m3

! (1) SPhy grazing by SZoo to SZoo and SDet
! (2) LPhy grazing by SZoo to SZoo and SDet
! (a) SPhy mortality to SDet (mu_P1_Sd)
! (b) LPhy mortality to SDet (mu_P2_Ld)

          cff1=dtdays*gmax1*e_11*SZoo(k)/(k_Zoo1+Ft_1(k))               ![ ]
          cff2=dtdays*gmax1*e_12*SZoo(k)/(k_Zoo1+Ft_1(k))               ![ ]
          cffa=dtdays*mu_P1_Sd                                          ![ ]
          cffb=dtdays*mu_P2_Sd                                          ![ ]
          SPhy(k)=SPhy(k)/(1.+cff1+cffa)                                !mmolN/m3
          LPhy(k)=LPhy(k)/(1.+cff2+cffb)                                !mmolN/m3
# ifdef DIAGNOSTICS_BIO
          ThisFlux(k,NFlux_P1Z1Grazing) = cff1*SPhy(k)                  !mmolN/m3
          ThisFlux(k,NFlux_P2Z1Grazing) = cff2*LPhy(k)                  !mmolN/m3
          ThisFlux(k,NFlux_P1mort) = cffa*SPhy(k)                       !mmolN/m3
          ThisFlux(k,NFlux_P2mort) = cffb*LPhy(k)                       !mmolN/m3
# endif
          SZoo(k)=SZoo(k)+(SPhy(k)*cff1+LPhy(k)*cff2)*beta1             !mmolN/m3
          SDet(k)=SDet(k)+SPhy(k)*(cff1*(1.-beta1)+cffa)+
     &                    LPhy(k)*(cff2*(1.-beta1)+cffb)                !mmolN/m3


! (1) SPhy grazing by LZoo to LZoo and LDet
! (2) LPhy grazing by LZoo to LZoo and LDet
! (3) SZoo grazing by LZoo to LZoo and LDet

          cff1=dtdays*gmax2*e_21*LZoo(k)/(k_Zoo2+Ft_2(k))               ![ ]
          cff2=dtdays*gmax2*e_22*LZoo(k)/(k_Zoo2+Ft_2(k))               ![ ]
          cff3=dtdays*gmax2*e_zz*LZoo(k)/(k_Zoo2+Ft_2(k))               ![ ]

          SPhy(k)=SPhy(k)/(1.+cff1)                                     !mmolN/m3
          LPhy(k)=LPhy(k)/(1.+cff2)                                     !mmolN/m3
          SZoo(k)=SZoo(k)/(1.+cff3)                                     !mmolN/m3
# ifdef DIAGNOSTICS_BIO
          ThisFlux(k,NFlux_P1Z2Grazing) = cff1*SPhy(k)                  !mmolN/m3
          ThisFlux(k,NFlux_P2Z2Grazing) = cff2*LPhy(k)                  !mmolN/m3
          ThisFlux(k,NFlux_Z1Z2Grazing) = cff3*SZoo(k)                  !mmolN/m3
# endif
          LZoo(k)=LZoo(k)+(SPhy(k)*cff1+LPhy(k)*cff2+
     &                                 SZoo(k)*cff3)*beta2              !mmolN/m3
          LDet(k)=LDet(k)+(SPhy(k)*cff1+LPhy(k)*cff2+
     &                            SZoo(k)*cff3)*(1.-beta2)              !mmolN/m3



! ====================================================================
! 4. SZoo and LZoo excretion to NH4 and DON 
!    SZoo mortality to SDet
!    LZoo mortality to LDet
! ====================================================================


! (1) SZoo excretion to NH4 and DON  (rate gamma_Z1_ADON)
! (2) SZoo mortality to SDet (rate mu_Z1_Sd)

          cff1=dtdays*gamma_Z1_ADON                                     ![ ]
          cff2=dtdays*mu_Z1_Sd*SZoo(k)*SZoo(k)                          ![ ]
          SZoo(k)=SZoo(k)/(1.+cff1)-cff2                                !mmolN/m3
# ifdef DIAGNOSTICS_BIO
          ThisFlux(k,NFlux_Z1metab) = SZoo(k)*cff1                      !mmolN/m3
          ThisFlux(k,NFlux_Z1mort) = cff2                               !mmolN/m3
# endif
          NH4(k)=NH4(k)+SZoo(k)*cff1*f2_Z1_NH4                          !mmolN/m3
          DON(k)=DON(k)+SZoo(k)*cff1*(1.-f2_Z1_NH4)                     !mmolN/m3
          SDet(k)=SDet(k)+cff2                                          !mmolN/m3
          O2(k)=O2(k)-ro2n*cff1*SZoo(k)*f2_Z1_NH4                       !mmolO2/m3


! (1) LZoo excretion to NH4 and DON (rate gamma_Z2_ADON)
! (2) LZoo mortality to LDet (rate mu_Z2_Ld)

          cff1=dtdays*gamma_Z2_ADON                                     ![ ]
          cff2=dtdays*mu_Z2_Ld*LZoo(k)*LZoo(k)                          ![ ]
          LZoo(k)=LZoo(k)/(1.+cff1)-cff2                                !mmolN/m3
# ifdef DIAGNOSTICS_BIO
          ThisFlux(k,NFlux_Z2metab) = LZoo(k)*cff1                      !mmolN/m3
          ThisFlux(k,NFlux_Z2mort) = cff2                               !mmolN/m3
# endif
          NH4(k)=NH4(k)+LZoo(k)*cff1*f2_Z2_NH4                          !mmolN/m3
          DON(k)=DON(k)+LZoo(k)*cff1*(1.-f2_Z2_NH4)                     !mmolN/m3
          LDet(k)=LDet(k)+cff2                                          !mmolN/m3
          O2(k)=O2(k)-ro2n*cff1*LZoo(k)*f2_Z2_NH4                       !mmolO2/m3



! ====================================================================
! 5. Remineralization of SDet, LDet and DON to NH4 
!    Oxic and Suboxic Remimeralization (Yakushev et al, 2007)
!    Hydrolysis of SDet and LDet to DON
! ====================================================================

! Decomposition of SDet, LDet ans DON in oxic condition

          IF (O2(k).LE.O2ox) THEN
             Fox = 0.                                                   ![ ]
          ELSE 
             Fox = (O2(k)-O2ox)/(O2(k)-O2ox+Kox)                        ![ ]
          END IF	  
          RemD1_O2 = exp(Ktox*t(i,j,k,nnew,itemp))*K_Sd_NH4*Fox         !d-1
          RemD2_O2 = exp(Ktox*t(i,j,k,nnew,itemp))*K_Ld_NH4*Fox         !d-1
          RemDON_O2 = exp(Ktox*t(i,j,k,nnew,itemp))*K_DON_NH4*Fox       !d-1

! Decomposition of SDet, LDet and DON in suboxic condition (denitrification)
! (if O2(k) < O2denitr , NO3(k) > aNO3mi and NO2(k) > aNO2mi)

          IF ((O2(k).GT.O2denitr).OR.(O2(k).LT.0.)) THEN
             Fdnox = 0.                                                 ![ ]
          ELSE
             Fdnox = 1. - O2(k)/(25.*(O2denitr+1.-O2(k)))                   ![ ]
          END IF
          IF (NO3(k).LE.aNO3mi) THEN
             FdnNO3 = 0.                                                ![ ]
          ELSE
             FdnNO3 = (NO3(k)-aNO3mi)/(NO3(k)-aNO3mi+1.)                ![ ]
          END IF
          IF (NO2(k).LE.aNO2mi) THEN
             FdnNO2 = 0.                                                ![ ]
          ELSE
             FdnNO2 = (NO2(k)-aNO2mi)/(NO2(k)-aNO2mi+1.)                ![ ]
          END IF
          Denitr1 = K_NO3_NO2*Fdnox*FdnNO3                              !d-1
          Denitr2 = K_NO2_N2O*Fdnox*FdnNO2                              !d-1


          cff1=dtdays*mu_Sd_DON         !hydrolysis of SDet             ![ ]
          cff2=dtdays*RemD1_O2          !oxic remin of SDet             ![ ]
          cff3=dtdays*Denitr1           !denitr1                        ![ ]
          cff4=dtdays*Denitr2           !denitr2                        ![ ]
          cff6=dtdays*mu_Ld_DON         !hydrolysis of LDet             ![ ]
          cff7=dtdays*RemD2_O2          !oxic remin of LDet             ![ ]
          cff9=dtdays*RemDON_O2         !oxic remin of DON              ![ ]



          SDet(k)=SDet(k)/(1.+cff1+cff2+cff3*0.5+cff4*0.75)             !mmolN/m3   

# ifdef DIAGNOSTICS_BIO
          ThisFlux(k,NFlux_HydrolD1) = cff1*Sdet(k)                     !mmolN/m3
          ThisFlux(k,NFlux_ReminOxyD1) = cff2*Sdet(k)                   !mmolN/m3
          ThisFlux(k,NFlux_Denitr1D1) = cff3*0.5*Sdet(k)                !mmolN/m3
          ThisFlux(k,NFlux_Denitr2D1) = cff4*0.75*Sdet(k)               !mmolN/m3
# endif

          LDet(k)=LDet(k)/(1.+cff6+cff7+cff3*0.5+cff4*0.75)             !mmolN/m3   

# ifdef DIAGNOSTICS_BIO
          ThisFlux(k,NFlux_HydrolD2) = cff6*Ldet(k)                     !mmolN/m3
          ThisFlux(k,NFlux_ReminOxyD2) = cff7*Ldet(k)                   !mmolN/m3
          ThisFlux(k,NFlux_Denitr1D2) = cff3*0.5*Ldet(k)                !mmolN/m3
          ThisFlux(k,NFlux_Denitr2D2) = cff4*0.75*Ldet(k)               !mmolN/m3
# endif

          DON(k)=(DON(k)+cff1*SDet(k)+cff6*LDet(k))/
     &                   (1.+cff9+cff3*0.5+cff4*0.75)                   !mmolN/m3   

# ifdef DIAGNOSTICS_BIO
          ThisFlux(k,NFlux_ReminOxyDON) = cff9*DON(k)                   !mmolN/m3
          ThisFlux(k,NFlux_Denitr1DON) = cff3*0.5*DON(k)                !mmolN/m3
          ThisFlux(k,NFlux_Denitr2DON) = cff4*0.75*DON(k)               !mmolN/m3
# endif
 
          NH4(k)=NH4(k)+cff2*SDet(k)+cff7*LDet(k)+cff9*DON(k)
     &                 +(cff3*0.5)*(Sdet(k)+Ldet(k)+DON(k))
     &                 +(cff4*0.75)*(Sdet(k)+Ldet(k)+DON(k))
     
          NO3(k)=NO3(k)-cff3*(SDet(k)+LDet(k)+DON(k))                   !mmolN/m3
          NO2(k)=NO2(k)+(cff3-cff4)*(SDet(k)+LDet(k)+DON(k))            !mmolN/m3

          O2(k)=O2(k)-ro2n*(cff2*SDet(k)+cff7*LDet(k)+cff9*DON(k))      !mmolO2/m3
 



! ====================================================================
! 6. Anammox (if O2(k) < O2anam)
!    Yakushev et al (2007)
! ====================================================================

          IF ((O2(k).GT.O2anam).OR.(O2(k).LT.0.)) THEN
             cff1 = 0.
             cff2 = 0.
          ELSE 
             cff1 = NH4(k)*K_anam*dtdays*K_convert                      ![ ]
             cff2 = NO2(k)*K_anam*dtdays*K_convert                      ![ ]
          END IF

          NO2(k)=NO2(k)/(1.+cff1)                                       !mmolN/m3
          NH4(k)=NH4(k)/(1.+cff2)                                       !mmolN/m3

# ifdef DIAGNOSTICS_BIO
          ThisFlux(k,NFlux_NO2anammox) = cff1*NO2(k)                    !mmolN/m3
          ThisFlux(k,NFlux_NH4anammox) = cff2*NH4(k)                    !mmolN/m3
# endif

        enddo


       do k=1,N

! ====================================================================
! 7. O2 Flux at ocean-atmosphere interface 
! ====================================================================

        o2sat(k)=o2sato(t(i,j,k,nnew,itemp),t(i,j,k,nnew,isalt))
        AOU(i,j,k)=o2sat(k)-O2(k)                                       !mmolO2/m3

        IF (k.EQ.N) THEN


! Schmidt Number
!---------------------------------------

! Keeling et al. (1998)
          Sco2=1638.0-81.83*t(i,j,k,nnew,itemp)+1.483*
     &      (t(i,j,k,nnew,itemp)**2)-0.008004*(t(i,j,k,nnew,itemp)**3)  ![ ]

! Wanninkhof (1992)
!          Sco2=1953.4-128.0*t(i,j,k,nnew,itemp)+3.9918*
!     &     (t(i,j,k,nnew,itemp)**2)-0.050091*(t(i,j,k,nnew,itemp)**3)  ![ ]


! Gas Transfert velocitiy
!---------------------------------------

# ifdef LISS_MERLIVAT  /*Liss and Merlivat (1986)*/
          if (wind_loc.LT.3.6) then
            aKwo2=(0.17*wind_loc)*(Sco2/595.)**(-2./3.)                 !cm/h
          else if (wind_loc.LT.13.) then
            aKwo2=(2.85*wind_loc-9.65)*(Sco2/595.)**(-0.5)              !cm/h
          else
            aKwo2=(5.9*wind_loc-49.3)*(Sco2/595.)**(-0.5)               !cm/h
          end if
# elif defined WANNINKHOF /*Wanninkhof (1992)*/
          aKwo2=(0.31*wind_loc**2)*((Sco2/660.)**(-0.5))                !cm/h
# endif
        
!  conversion ( aKwo2 : cm/h ----> m/d )
          aKwo2=aKwo2*0.24                                              !m/d

          FluxO2 = aKwo2*(o2sat(k)*patm/P0-O2(k))                       !mmolO2/m2/d

          O2(k)=O2(k)+dtdays*FluxO2/(z_w(i,j,k)-z_w(i,j,k-1))           !mmolO2/m3

# ifdef DIAGNOSTICS_BIO
          ThisGasExcFlux(O2Flux_GasExc) = dtdays*FluxO2
     &                                  /(z_w(i,j,k)-z_w(i,j,k-1))      !mmolO2/m3
!ThisGasExcFlux is positive if ocean takes up O2 from the atmosphere
# endif

         END IF



! ====================================================================
! 8. N2O 
! ====================================================================

# ifdef NITROUS_OXIDE

#  ifdef NEVISON  /*Nevison et al. (2003)*/

        if((O2(k).gt.4.).and.(z_w(i,j,k).lt.(-hel(i,j))))then
          cff=(16./170.)*((0.26/O2(k))-0.0004)*exp(z_w(i,j,k)/3000.)    !mmolN2O/mmolO2
        else
          cff=0.                                                        !mmolN2O/mmolO2	
        endif
        N2O(k)=N2O(k)+nitrif(k)*cff					!mmolN2O/m3

#   ifdef DIAGNOSTICS_BIO
          ThisFlux(k,NFlux_paramN2O) = nitrif(k)*cff            	!mmolN2O/m3
#   endif

#  elif defined SUNTHARALINGAM  /*Suntharalingam et al. (2000,2012)*/

        if(z_w(i,j,k).lt.(-hel(i,j)))then
!  normalized representation of the functional dependance on oxygen distribution
          if (O2(k).lt.O2max) then
             function_O2=O2(k)/O2max					![ ] 	
          else
             function_O2=exp(-k_O2*(O2(k)-O2max)/O2max)			![ ]
          endif
          cff=alpha_N2O*nitrif(k)+beta_N2O*nitrif(k)*function_O2    	!mmolN2O/m3
        else
          cff=0.                                                        !mmolN2O/m3	
        endif

          N2O(k)=N2O(k)+cff                                   		!mmolN2O/m3

#   ifdef DIAGNOSTICS_BIO
          ThisFlux(k,NFlux_paramN2O) = cff                   		!mmolN2O/m3
#   endif

#  endif

# endif



! ====================================================================
! 9. N2O Flux at ocean-atmosphere interface 
! ====================================================================

# ifdef NITROUS_OXIDE

        IF (k.EQ.N) THEN

! Schmidt number 
!---------------------------------------
! Wanninkhof (1992) >> Wilke and Chang (1955)

          Scn2o=2301.1-151.1*t(i,j,k,nnew,itemp)+4.7364*
     &      t(i,j,k,nnew,itemp)**2-0.059431*t(i,j,k,nnew,itemp)**3      ![ ]

! Gas Transfert velocity
!---------------------------------------

#  ifdef LISS_MERLIVAT /*Liss and Merlivat (1986)*/
          if (wind_loc.LT.3.6) then
            aKwn2o=(0.17*wind_loc)*(Scn2o/595.)**(-2./3.)               !cm/h
          else if (wind_loc.LT.13.) then
            aKwn2o=(2.85*wind_loc-9.65)*(Scn2o/595.)**(-0.5)            !cm/h
          else
            aKwn2o=(5.9*wind_loc-49.3)*(Scn2o/595.)**(-0.5)             !cm/h
          end if
#  elif defined WANNINKHOF /*Wanninkhof (1992)*/
          aKwn2o=(0.31*wind_loc**2)*((Scn2o/660.)**(-0.5))              !cm/h
#  endif

!  conversion ( aKwn2o : cm/h ----> m/d )
          aKwn2o=aKwn2o*0.24                                            !m/d

          N2Oeq=N2O_eq(t(i,j,k,nnew,itemp),t(i,j,k,nnew,isalt),
     & N2O_atm,P0)                                                      !mmolN2O/m3

          FluxN2O = aKwn2o*(N2Oeq-N2O(k))                               !mmolN2O/m2/d

          N2O(k)=N2O(k)+dtdays*FluxN2O/(z_w(i,j,k)-z_w(i,j,k-1))        !mmolN2O/m3

#  ifdef DIAGNOSTICS_BIO
          ThisGasExcFlux(N2OFlux_GasExc) = dtdays*FluxN2O
     &                                  /(z_w(i,j,k)-z_w(i,j,k-1))      !mmolN2O/m3
!  ThisGasExcFlux is positive if ocean takes up N2O from the atmosphere
#  endif

        END IF

# endif


        enddo



! ====================================================================
! ====================================================================


! Vertical sinking: Vertical advection algorithm based on monotonic,
! continuous conservative parabolic splines.
!
        do k=1,N
          SB(k,1)=theta_m*LPhy(k)*CN_Phyt*12.
          SB(k,2)=LPhy(k)
          SB(k,3)=SDet(k)
          SB(k,4)=LDet(k)
        enddo
        wSB(1)=wLPhy
        wSB(2)=wLPhy
        wSB(3)=wSDet
        wSB(4)=wLDet
 
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
 

! Apply fluxes:
!
          do k=1,N
            SB(k,iB)=SB(k,iB)+wSB(iB)*FC(k)*(dSB(k,iB)-dSB(k-1,iB))
          enddo

        enddo  ! <-- iB

# ifdef DIAGNOSTICS_BIO
        do iflux = 1, NumVSinkTerms
          ColumnMassOld(iflux) = 0.0
          ColumnMassNew(iflux) = 0.0
        enddo
# endif /* DIAGNOSTICS_BIO */

        do k=1,N

# ifdef DIAGNOSTICS_BIO
! ColumnMassOld and ColumnMassNew are needed to compute the sinking flux
! into the sediment
         ! Large Phytoplankton
          ColumnMassOld(1) = ColumnMassOld(1)+LPhy(k)                   !mmolN/m3
          ThisVSinkFlux(k,NFlux_VSinkP2) = LPhy(k)-SB(k,2)              !mmolN/m3
# endif /* DIAGNOSTICS_BIO */
          LPhy(k) =SB(k,2)                                              !mmolN/m3
# ifdef DIAGNOSTICS_BIO
          ColumnMassNew(1)=ColumnMassNew(1)+LPhy(k)                     !mmolN/m3
# endif /* DIAGNOSTICS_BIO */

          ! Small detritus
# ifdef DIAGNOSTICS_BIO
          ColumnMassOld(2) = ColumnMassOld(2)+SDet(k)                   !mmolN/m3
          ThisVSinkFlux(k,NFlux_VSinkD1) = SDet(k)-SB(k,3)              !mmolN/m3
# endif /* DIAGNOSTICS_BIO */
          SDet(k) =SB(k,3)                                              !mmolN/m3
# ifdef DIAGNOSTICS_BIO
          ColumnMassNew(2) = ColumnMassNew(2)+SDet(k)                   !mmolN/m3
# endif /* DIAGNOSTICS_BIO */
          
            ! Large detritus
# ifdef DIAGNOSTICS_BIO
          ColumnMassOld(3) = ColumnMassOld(3)+LDet(k)                   !mmolN/m3
          ThisVSinkFlux(k,NFlux_VSinkD2) = LDet(k)-SB(k,4)              !mmolN/m3
# endif /* DIAGNOSTICS_BIO */
          LDet(k) =SB(k,4)                                              !mmolN/m3
# ifdef DIAGNOSTICS_BIO
          ColumnMassNew(3) = ColumnMassNew(3)+LDet(k)                   !mmolN/m3
# endif /* DIAGNOSTICS_BIO */

        enddo

!
# ifdef DIAGNOSTICS_BIO
! --------------------------------------------------------------------------------
! Transfer fluxes to global arrays at the end of each biological time step
! for computational efficiency, divide now by dtsec to get the correct units
        do iflux = 1, 10
           do k = 1, N
              bioFlux(i,j,k,iflux) = bioFlux(i,j,k,iflux) 
     &             + ThisFlux(k,iflux) / float(ITERMAX)
#  ifdef MASKING
     &             * rmask(i,j)
#  endif /* MASKING */
           end do
        end do
        do iflux = 11, NumFluxTerms
           do k = 1, N
              bioFlux(i,j,k,iflux) = bioFlux(i,j,k,iflux) 
     &             + ThisFlux(k,iflux) / dt                             !mmol/m3/s
#  ifdef MASKING
     &             * rmask(i,j)
#  endif /* MASKING */
           end do
        end do
! --------------------------------------------------------------------------------
! Compute the vertical sinking flux into the sediment by comparing
! previous and current mass in this (i,j) column
! The flux is positive if upward, so usually it will be negative, i.e.
! into the sediment.
        do iflux = 1, NumVSinkTerms
           LastVSinkFlux = ( ColumnMassNew(iflux) -
     &                       ColumnMassOld(iflux) )                     !mmolN/m3/s
           bioVSink(i,j,0,iflux) =  bioVSink(i,j,0,iflux)
     &                              + LastVSinkFlux / dt                !mmolN/m3/s
#  ifdef MASKING
     &          * rmask(i,j)
#  endif /* MASKING */
           do k = 1, N
              LastVSinkFlux = LastVSinkFlux +
     &                        ThisVSinkFlux(k,iflux)                    !mmolN/m3/s 

              bioVSink(i,j,k,iflux) =  bioVSink(i,j,k,iflux)
     &                                + LastVSinkFlux / dt              !mmolN/m3/s  
#  ifdef MASKING
     &             * rmask(i,j)
#  endif /* MASKING */
           end do
         enddo
! -------------------------------------------------------------------------------- 
! ThisGasExcFlux mmol(O2, N2O)/m3 
        do iflux = 1, NumGasExcTerms
           GasExcFlux(i,j,iflux) =  GasExcFlux(i,j,iflux) +
     &                             ThisGasExcFlux(iflux) / dt           ! mmol(O2, N2O)/m3/s 
#  ifdef MASKING
     &          * rmask(i,j)
#  endif /* MASKING */
        end do

# endif /* DIAGNOSTICS_BIO */  


      ENDDO  ! <-- ITER
           

! Write back
!

# undef DEBUG_BIO
# if defined DEBUG_BIO && defined DIAGNOSTICS_BIO

        if ((i.eq.50) .and. (j.eq.50)) then

          do k=1,N 
        
! sources - sinks  (SMS: mmol/m3/s)
!-----------------------------------------------------------------
          call budget_N_O2(i,j,k,budget_NO3,budget_NO2,budget_NH4
     &                    ,budget_SPhy,budget_LPhy,budget_SZoo,budget_LZoo
     &                    ,budget_SDet,budget_LDet,budget_DON
     &                    ,budget_O2)
   
          budget_N=budget_NO3+budget_NO2+budget_NH4+
     &             budget_SPhy+budget_LPhy+budget_SZoo+budget_LZoo+
     &             budget_SDet+budget_LDet+budget_DON

! trend  (mmol/m3/s)
!-----------------------------------------------------------------
         trend_NO3  = (( min(t(i,j,k,nnew,iNO3_),0.) + NO3(k) ) 
     &                     - t(i,j,k,nnew,iNO3_)) / dt 
         trend_NO2  = (( min(t(i,j,k,nnew,iNO2_),0.) + NO2(k) ) 
     &                     - t(i,j,k,nnew,iNO2_)) / dt 
         trend_NH4  = (( min(t(i,j,k,nnew,iNH4_),0.) + NH4(k) ) 
     &                     - t(i,j,k,nnew,iNH4_)) / dt 
         trend_SPhy = (( min(t(i,j,k,nnew,iPhy1),0.) + SPhy(k) ) 
     &                     - t(i,j,k,nnew,iPhy1)) / dt 
         trend_LPhy = (( min(t(i,j,k,nnew,iPhy2),0.) + LPhy(k) ) 
     &                     - t(i,j,k,nnew,iPhy2)) / dt 
         trend_SZoo = (( min(t(i,j,k,nnew,iZoo1),0.) + SZoo(k) ) 
     &                     - t(i,j,k,nnew,iZoo1)) / dt 
         trend_LZoo = (( min(t(i,j,k,nnew,iZoo2),0.) + LZoo(k) ) 
     &                     - t(i,j,k,nnew,iZoo2)) / dt 
         trend_SDet = (( min(t(i,j,k,nnew,iDet1),0.) + SDet(k) ) 
     &                     - t(i,j,k,nnew,iDet1)) / dt 
         trend_LDet = (( min(t(i,j,k,nnew,iDet2),0.) + LDet(k) ) 
     &                     - t(i,j,k,nnew,iDet2)) / dt 
         trend_DON  = (( min(t(i,j,k,nnew,iDON),0.)  + DON(k) ) 
     &                     - t(i,j,k,nnew,iDON)) / dt 

         trend_N    = trend_NO3+trend_NO2+trend_NH4+
     &                trend_SPhy+trend_LPhy+trend_SZoo+trend_LZoo+
     &                trend_SDet+trend_LDet+trend_DON
     
         trend_O2   = (( min(t(i,j,k,nnew,iO2),0.) + O2(k) ) 
     &                     - t(i,j,k,nnew,iO2)) / dt 

! N loss (mmol/m3/s)
!-----------------------------------------------------------------
          N_loss   = - ( bioFlux(i,j,k,NFlux_Denitr2D1)/0.75+
     &                   bioFlux(i,j,k,NFlux_Denitr2D2)/0.75+ 
     &                   bioFlux(i,j,k,NFlux_Denitr2DON)/0.75+
     &                   bioFlux(i,j,k,NFlux_NO2anammox) + 
     &                   bioFlux(i,j,k,NFlux_NH4anammox) )
     
! vertical sinking (mmol/m3/s)
!-----------------------------------------------------------------
          sinking   =  ( bioVSink(i,j,k-1,NFlux_VSinkP2)
     &                  -bioVSink(i,j,k,NFlux_VSinkP2) )
     &               + ( bioVSink(i,j,k-1,NFlux_VSinkD1)
     &                  -bioVSink(i,j,k,NFlux_VSinkD1) )
     &               + ( bioVSink(i,j,k-1,NFlux_VSinkD2)
     &                  -bioVSink(i,j,k,NFlux_VSinkD2) )

! error = trend - SMS (mmol/m3/s)
!-----------------------------------------------------------------
        write(*,*)'====================================='
        write(*,*)'trend-SMS(NO3) ',k,trend_NO3 -budget_NO3
        write(*,*)'trend-SMS(NO2) ',k,trend_NO2 -budget_NO2
        write(*,*)'trend-SMS(NH4) ',k,trend_NH4 -budget_NH4
        write(*,*)'trend-SMS(SPhy)',k,trend_SPhy-budget_SPhy
        write(*,*)'trend-SMS(LPhy)',k,trend_LPhy-budget_LPhy
        write(*,*)'trend-SMS(SZoo)',k,trend_SZoo-budget_SZoo
        write(*,*)'trend-SMS(LZoo)',k,trend_LZoo-budget_LZoo
        write(*,*)'trend-SMS(SDet)',k,trend_SDet-budget_SDet
        write(*,*)'trend-SMS(LDet)',k,trend_LDet-budget_LDet
        write(*,*)'trend-SMS(DON) ',k,trend_DON -budget_DON
        write(*,*)'-------------------------------------'
        write(*,*)'trend (N total)                :',k,trend_N
        write(*,*)'SMS (N total)                  :',k,budget_N
        write(*,*)'N loss + vertical sinking      :',k,N_loss+sinking
        write(*,*)'The three have to be the same !!'
        write(*,*)'-------------------------------------'
        write(*,*)'error (N total) = trend - SMS :', 
     &                                               k,trend_N- budget_N
        write(*,*)'-------------------------------------'
        write(*,*)'-------------------------------------'
        write(*,*)'error (O2)      = trend - SMS :',
     &                                               k,trend_O2-budget_O2


#  if defined NITROUS_OXIDE
 
!  sources - sinks  (SMS: mmol/m3/s)
!-----------------------------------------------------------------
          call budget_N2O(i,j,k,budget_Nitrous_oxide)

!  trend  (mmol/m3/s)
!-----------------------------------------------------------------
         trend_N2O  = (( min(t(i,j,k,nnew,iN2O),0.) + N2O(k) ) 
     &                     - t(i,j,k,nnew,iN2O)) / dt 

        write(*,*)'-------------------------------------'
        write(*,*)'-------------------------------------'
        write(*,*)'error (N2O)      = trend - SMS :',
     &                                k,trend_N2O-budget_Nitrous_oxide
        write(*,*)'====================================='
	 
#  endif 


       enddo

       endif
       

# endif /* DIAGNOSTICS_BIO */

 
          do k=1,N
           t(i,j,k,nnew,iNO3_)=min(t(i,j,k,nnew,iNO3_),0.) + NO3(k)
           t(i,j,k,nnew,iNO2_)=min(t(i,j,k,nnew,iNO2_),0.) + NO2(k)
           t(i,j,k,nnew,iNH4_)=min(t(i,j,k,nnew,iNH4_),0.) + NH4(k)
           t(i,j,k,nnew,iPhy1)=min(t(i,j,k,nnew,iPhy1),0.) + SPhy(k)
           t(i,j,k,nnew,iPhy2)=min(t(i,j,k,nnew,iPhy2),0.) + LPhy(k)
           t(i,j,k,nnew,iZoo1)=min(t(i,j,k,nnew,iZoo1),0.) + SZoo(k)
           t(i,j,k,nnew,iZoo2)=min(t(i,j,k,nnew,iZoo2),0.) + LZoo(k)
           t(i,j,k,nnew,iDet1)=min(t(i,j,k,nnew,iDet1),0.) + SDet(k)
           t(i,j,k,nnew,iDet2)=min(t(i,j,k,nnew,iDet2),0.) + LDet(k)
           t(i,j,k,nnew,iDON) =min(t(i,j,k,nnew,iDON),0.)  + DON(k)
           t(i,j,k,nnew,iO2)=min(t(i,j,k,nnew,iO2),0.) + O2(k)
# ifdef NITROUS_OXIDE
           t(i,j,k,nnew,iN2O)=min(t(i,j,k,nnew,iN2O),0.) + N2O(k)
# endif
          enddo

# ifdef MASKING
        endif  ! rmask if
# endif /* MASKING */


          wind10(i,j) = wind_loc


        enddo   !<----------i=I_RANGE
      enddo     !<----------j=J_RANGE



!         write(*,*)'====================================='
!!         write(*,*)'O2GasExcFlux ',GasExcFlux(50,50,1)
 !        write(*,*)'====================================='

      return

      end





# ifdef NITROUS_OXIDE

       function N2O_eq(T,S,N2O_atm,P)

! ====================================================================
! -------------------  function N2O_eq(T,S,N2O_atm,P)-----------------
! ====================================================================

! N2O_eq [mmol/m^3] is the N2O concentration in equilibrium with moist
! air at one atmosphere total pressure 

       implicit none
!
! Computes the N2O equilibrium concentration at 1 atm total pressure
! in mmol/m^3 given the temperature (T, in deg C) and the salinity (S).

! From "Nitrogen in the Marine Environment", chapter 2: Gaseous nitrogen
! compounds (NO, N2O, N2, NH3) in the ocean from H. Bange
! Bunsen solubility from Weiss and Price (1980)

! F is in mol L-1 atm-1
! T is the temperature in K
! S for salinity
! N2O_atm is the dry mole fraction of atmospheric N2O
! P is the ambient pressure

! N2O_eq IS DEFINED BETWEEN O(deg C) <= T <= 40(deg C) AND
! 0 <= S <= 40 
!
! CHECK VALUE:  T = 25. deg C
!               S = 35. 
!               N2O_atm = 318 ppb 
! N2O_eq = 6.38 nmol L-1
!
! CHECK VALUE:  T = 5. deg C
!               S = 35. 
!               N2O_atm = 318 ppb 
! N2O_eq = 12.42 nmol L-1

       real N2O_eq, N2O_atm, T, S, P
       real A1, A2, A3, A4 
       real B1, B2, B3
       real TK, TK1, TK2, TK3, lnF, F

       data A1/ -165.8806 /, A2/ 222.8743  /,
     $      A3/ 92.0792   /, A4/ -1.48425  /
       data B1/-0.056235  /, B2/0.031619   /,
     $      B3/-0.004847  /

       TK  = 273.15+T
       TK1 = 100./TK
       TK2 = TK/100.
       TK3 = (TK2)**2

       lnF =   A1 + A2*TK1 + A3*log(TK2) + A4*TK3
     $     +  S*(B1 + B2*(TK2) + B3*TK3)

       F = exp(lnF)                                                     !mol/L/atm
       
       N2O_eq = F * N2O_atm * P                                         !nmol/L = 10-3 mmol/m^3
       N2O_eq = N2O_eq *0.001                                           !mmol/m^3

       return

       end

# endif




# ifdef DIAGNOSTICS_BIO

      subroutine budget_N_O2(i,j,k,budget_NO3,budget_NO2,budget_NH4,
     & budget_SPhy,budget_LPhy,budget_SZoo,budget_LZoo,budget_SDet,
     & budget_LDet,budget_DON,budget_O2)

! ====================================================================
! BUDGET FOR EACH VARIABLE
! ====================================================================	

# include "param.h"
# include "diagnostics.h"
# include "cste_bio_coastal.h"

       budget_NO3=-bioFlux(i,j,k,NFlux_ProdNO3P1)
     &           -bioFlux(i,j,k,NFlux_ProdNO3P2)
     &           +bioFlux(i,j,k,NFlux_Nitrif2)
     &           -bioFlux(i,j,k,NFlux_Denitr1D1)/0.5
     &           -bioFlux(i,j,k,NFlux_Denitr1D2)/0.5
     &           -bioFlux(i,j,k,NFlux_Denitr1DON)/0.5

       budget_NO2=-bioFlux(i,j,k,NFlux_ProdNO2P1)
     &           -bioFlux(i,j,k,NFlux_ProdNO2P2)
     &           +bioFlux(i,j,k,NFlux_Nitrif1)
     &           -bioFlux(i,j,k,NFlux_Nitrif2)
     &           +bioFlux(i,j,k,NFlux_Denitr1D1)/0.5
     &           +bioFlux(i,j,k,NFlux_Denitr1D2)/0.5
     &           +bioFlux(i,j,k,NFlux_Denitr1DON)/0.5
     &           -bioFlux(i,j,k,NFlux_Denitr2D1)/0.75
     &           -bioFlux(i,j,k,NFlux_Denitr2D2)/0.75
     &           -bioFlux(i,j,k,NFlux_Denitr2DON)/0.75
     &           -bioFlux(i,j,k,NFlux_NO2anammox)

       budget_NH4=-bioFlux(i,j,k,NFlux_ProdNH4P1)
     &           -bioFlux(i,j,k,NFlux_ProdNH4P2)
     &           -bioFlux(i,j,k,NFlux_Nitrif1)
     &           +f2_Z1_NH4*bioFlux(i,j,k,NFlux_Z1metab)
     &           +f2_Z2_NH4*bioFlux(i,j,k,NFlux_Z2metab)
     &           +bioFlux(i,j,k,NFlux_ReminOxyD1)
     &           +bioFlux(i,j,k,NFlux_ReminOxyD2)
     &           +bioFlux(i,j,k,NFlux_ReminOxyDON)
     &           +bioFlux(i,j,k,NFlux_Denitr1D1)
     &           +bioFlux(i,j,k,NFlux_Denitr1D2)
     &           +bioFlux(i,j,k,NFlux_Denitr1DON)
     &           +bioFlux(i,j,k,NFlux_Denitr2D1)
     &           +bioFlux(i,j,k,NFlux_Denitr2D2)
     &           +bioFlux(i,j,k,NFlux_Denitr2DON)
     &           -bioFlux(i,j,k,NFlux_NH4anammox) 
             
       budget_SPhy=+(1.-epsilon1)*(bioFlux(i,j,k,NFlux_ProdNO3P1)
     &           +bioFlux(i,j,k,NFlux_ProdNO2P1)
     &           +bioFlux(i,j,k,NFlux_ProdNH4P1))
     &           -bioFlux(i,j,k,NFlux_P1Z1Grazing)
     &           -bioFlux(i,j,k,NFlux_P1mort)
     &           -bioFlux(i,j,k,NFlux_P1Z2Grazing)
     
       budget_LPhy=+(1.-epsilon2)*(bioFlux(i,j,k,NFlux_ProdNO3P2)
     &           +bioFlux(i,j,k,NFlux_ProdNO2P2)
     &           +bioFlux(i,j,k,NFlux_ProdNH4P2))	
     &           -bioFlux(i,j,k,NFlux_P2Z1Grazing)
     &           -bioFlux(i,j,k,NFlux_P2mort)   
     &           -bioFlux(i,j,k,NFlux_P2Z2Grazing)
     &           + ( bioVSink(i,j,k-1,NFlux_VSinkP2)
     &              -bioVSink(i,j,k,NFlux_VSinkP2) ) !(bioVSink<0 if downward)
          
       budget_SZoo=+beta1*(bioFlux(i,j,k,NFlux_P1Z1Grazing)
     &            +bioFlux(i,j,k,NFlux_P2Z1Grazing))
     &            -bioFlux(i,j,k,NFlux_Z1Z2Grazing)
     &            -bioFlux(i,j,k,NFlux_Z1metab)
     &            -bioFlux(i,j,k,NFlux_Z1mort)
     
       budget_LZoo=+beta2*(bioFlux(i,j,k,NFlux_P1Z2Grazing)
     &            +bioFlux(i,j,k,NFlux_P2Z2Grazing)
     &            +bioFlux(i,j,k,NFlux_Z1Z2Grazing))
     &            -bioFlux(i,j,k,NFlux_Z2metab)
     &            -bioFlux(i,j,k,NFlux_Z2mort)
            
       budget_SDet=+(1.-beta1)*(bioFlux(i,j,k,NFlux_P1Z1Grazing)
     &            +bioFlux(i,j,k,NFlux_P2Z1Grazing))
     &            +bioFlux(i,j,k,NFlux_P1mort)
     &            +bioFlux(i,j,k,NFlux_P2mort)
     &            +bioFlux(i,j,k,NFlux_Z1mort)
     &            -bioFlux(i,j,k,NFlux_HydrolD1) 
     &            -bioFlux(i,j,k,NFlux_ReminOxyD1) 
     &            -bioFlux(i,j,k,NFlux_Denitr1D1) 
     &            -bioFlux(i,j,k,NFlux_Denitr2D1)
     &            + ( bioVSink(i,j,k-1,NFlux_VSinkD1)   
     &               -bioVSink(i,j,k,NFlux_VSinkD1) )    
    
       budget_LDet=+(1.-beta2)*(bioFlux(i,j,k,NFlux_P1Z2Grazing)
     &            +bioFlux(i,j,k,NFlux_P2Z2Grazing)
     &            +bioFlux(i,j,k,NFlux_Z1Z2Grazing))
     &            +bioFlux(i,j,k,NFlux_Z2mort)
     &            -bioFlux(i,j,k,NFlux_HydrolD2) 
     &            -bioFlux(i,j,k,NFlux_ReminOxyD2)
     &            -bioFlux(i,j,k,NFlux_Denitr1D2) 
     &            -bioFlux(i,j,k,NFlux_Denitr2D2)
     &            + ( bioVSink(i,j,k-1,NFlux_VSinkD2)  
     &               -bioVSink(i,j,k,NFlux_VSinkD2) )   
     
       budget_DON=+epsilon1*(bioFlux(i,j,k,NFlux_ProdNO3P1)
     &            +bioFlux(i,j,k,NFlux_ProdNO2P1)
     &            +bioFlux(i,j,k,NFlux_ProdNH4P1))
     &            +epsilon2*(bioFlux(i,j,k,NFlux_ProdNO3P2)
     &            +bioFlux(i,j,k,NFlux_ProdNO2P2)
     &            +bioFlux(i,j,k,NFlux_ProdNH4P2))	
     &            +(1.-f2_Z1_NH4)*bioFlux(i,j,k,NFlux_Z1metab)
     &            +(1.-f2_Z2_NH4)*bioFlux(i,j,k,NFlux_Z2metab)
     &            +bioFlux(i,j,k,NFlux_HydrolD1)
     &            +bioFlux(i,j,k,NFlux_HydrolD2)     
     &            -bioFlux(i,j,k,NFlux_ReminOxyDON)
     &            -bioFlux(i,j,k,NFlux_Denitr1DON) 
     &            -bioFlux(i,j,k,NFlux_Denitr2DON) 
     
       budget_O2=+ro2n*(bioFlux(i,j,k,NFlux_ProdNO3P1)
     &            +bioFlux(i,j,k,NFlux_ProdNO3P2)
     &            +bioFlux(i,j,k,NFlux_ProdNO2P1)
     &            +bioFlux(i,j,k,NFlux_ProdNO2P2)
     &            +bioFlux(i,j,k,NFlux_ProdNH4P1)
     &            +bioFlux(i,j,k,NFlux_ProdNH4P2)
     &            -f2_Z1_NH4*bioFlux(i,j,k,NFlux_Z1metab)
     &            -f2_Z2_NH4*bioFlux(i,j,k,NFlux_Z2metab)
     &            -bioFlux(i,j,k,NFlux_ReminOxyD1)
     &            -bioFlux(i,j,k,NFlux_ReminOxyD2)
     &            -bioFlux(i,j,k,NFlux_ReminOxyDON))
     &            -1.5*bioFlux(i,j,k,NFlux_Nitrif1)
     &            -0.5*bioFlux(i,j,k,NFlux_Nitrif2)
         if (k.eq.N) then
          budget_O2=budget_O2+GasExcFlux(i,j,O2Flux_GasExc)
         endif

      return
      end

# endif 




# if defined DIAGNOSTICS_BIO && defined NITROUS_OXIDE

      subroutine budget_N2O(i,j,k,budget_nitrous_oxide)

! ====================================================================
! BUDGET FOR N2O
! ====================================================================	

# include "param.h"
# include "diagnostics.h"
# include "cste_bio_coastal.h"

          budget_nitrous_oxide=+bioFlux(i,j,k,NFlux_paramN2O)
          if (k.eq.N) then	  
           budget_nitrous_oxide=budget_nitrous_oxide
     &                        +GasExcFlux(i,j,N2OFlux_GasExc)
          endif 

      return
      end

# endif 




#else /* BIOLOGY */

       subroutine biology_empty ()
       end

#endif /* BIOLOGY */

