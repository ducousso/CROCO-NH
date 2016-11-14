! $Id: cste_bio_coastal.h 1353 2013-10-11 16:15:58Z gcambon $
!======================================================================
! CROCO is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! CROCO specific routines (nesting) are under CeCILL-C license.
! 
! CROCO website : http://www.croco-ocean.org
!======================================================================
!
      real    kwater, kChla, palpha1, palpha2,
     &        abio1, abio2, bbio, cbio,
     &        K_psi, K1_NO32, K2_NO32, K1_NH4, K2_NH4, 
     &        epsilon1, epsilon2, K_NH4_NO2, K_NO2_NO3, 
     &        O2nf, mu_P1_Sd, mu_P2_Sd, gmax1, gmax2, 
     &        K_Zoo1, K_Zoo2, beta1, beta2,
     &        e_11, e_12, e_21, e_22, e_ZZ,                
     &        gamma_Z1_ADON, gamma_Z2_ADON, mu_Z1_Sd, mu_Z2_Ld,  
     &        f2_Z1_NH4, f2_Z2_NH4, O2ox, Kox, Ktox, 
     &        K_Sd_NH4, K_Ld_NH4, K_DON_NH4, 
     &        O2denitr, O2anam, aNO3mi, aNO2mi, K_NO3_NO2, K_NO2_N2O,
     &        mu_Sd_DON, mu_Ld_DON, K_anam, K_convert,
     &        CN_Phyt, wLPhy, wSDet, wLDet,
     &        ro2n
     &    ,   theta_m
#ifdef NITROUS_OXIDE
     &    ,   N2O_atm, alpha_N2O, beta_N2O, k_O2, O2max
#endif
            
      integer ITERMAX
     

      parameter (
     &  ITERMAX    = 3      ! number of small implicit time steps
!
!
! Parameters as in Table 2 of Gutknecht et al. (2013) 
!
! Gutknecht, E., I. Dadou, B. Le Vu, G. Cambon, J. Sudre, V. Garçon, E. Machu, T. Rixen, 
! A. Kock, A. Flohr, A. Paulmier, and G. Lavik, Coupled physical/biogeochemical modeling 
! including O2-dependent processes in the Eastern Boundary Upwelling Systems: application 
! in the Benguela, Biogeosciences, 10, 3559-3591, doi:10.5194/bg-10-3559-2013, 2013.
!
!-------------------------------------------------------------------------------------------------------
! Phytoplankton
!-------------------------------------------------------------------------------------------------------
     &  , palpha1    = 0.025  ! Initial slope of P-I curve for SPhy                    [(W m-2)-1 d-1]
     &  , palpha2    = 0.04   ! Initial slope of P-I curve for LPhy                    [(W m-2)-1 d-1]
     &  , kwater     = 0.04   ! light attenuation coefficient due to pure water        [m-1]
     &  , kChla      = 0.024  ! light attenuation coefficient by phytoplankton         [m2 (mgChla)-1]
     &  , theta_m    = 0.02   ! Chl/Carbon ratio                                       [mgChla (mgC)-1]
     &  , abio1      = 0.557  !	SPhy maximum growth rate at 0°C                        [d-1]
     &  , abio2      = 0.6    ! LPhy maximum growth rate at 0°C                        [d-1]
     &  , bbio       = 1.066  !                                                        [ ]
     &  , cbio       = 1.     !                                                        [°C-1]
     &  , mu_P1_Sd   = 0.027  ! Mortality rate of SPhy                                 [d-1]
     &  , mu_P2_Sd   = 0.030  ! Mortality rate of LPhy                                 [d-1]  
     &  , epsilon1   = 0.05  ! Exudation fraction of primary production (by SPhy)     [d-1]
     &  , epsilon2   = 0.05  ! Exudation fraction of primary production (by LPhy)     [d-1]
     &  , K_psi      = 1.46   ! Strength of NH4 inhibition of NO3 uptake constant      [(mmolN m-3)-1]
     &  , K1_NH4     = 0.5    ! Half-saturation constant for uptake of NH4 by SPhy     [mmolN m-3]
     &  , K2_NH4     = 1.     ! Half-saturation constant for uptake of NH4 by LPhy     [mmolN m-3]
     &  , K1_NO32    = 0.5    ! Half-saturation constant for uptake of NO3+NO2 by SPhy [mmolN m-3]
     &  , K2_NO32    = 2.     ! Half-saturation constant for uptake of NO3+NO2 by LPhy [mmolN m-3]
     &  , CN_Phyt    = 106./16.! C:N ratio for phytoplankton                            [molC (molN)-1]
     &  , ro2n	   = 170./16. ! O2:N ratio (Anderson and Sarmiento, 1994)              [molO2 molN-1]
     &  , wLPhy      = 0.5    ! Sinking velocity of LPhy                               [m d-1]
!-------------------------------------------------------------------------------------------------------
! Zooplankton
!-------------------------------------------------------------------------------------------------------
     &  , beta1      = 0.75   ! Assimilation efficiency of SZoo                        [ ]
     &  , beta2      = 0.70   ! Assimilation efficiency of LZoo                        [ ] 
     &  , gmax1      = 0.9    ! Maximum grazing rate of SZoo                           [d-1]
     &  , gmax2      = 1.2    ! Maximum grazing rate of LZoo                           [d-1]
     &  , e_11       = 0.7    ! Preference of SZoo for SPhy                            [ ]
     &  , e_12       = 0.3    ! Preference of SZoo for LPhy                            [ ]
     &  , e_21       = 0.26   ! Preference of LZoo for SPhy                            [ ]
     &  , e_22       = 0.53   ! Preference of LZoo for LPhy                            [ ]
     &  , e_zz       = 0.21   ! Preference of LZoo for SZoo                            [ ]
     &  , K_Zoo1     = 1.5    ! Half-saturation constant for ingestion by SZoo         [mmolN m-3]
     &  , K_Zoo2     = 4.0    ! Half-saturation constant for ingestion by LZoo         [mmolN m-3]
     &  , mu_Z1_Sd   = 0.025  ! Mortality rate of SZoo                                 [(mmolN m-3)-1 d-1]
     &  , mu_Z2_Ld   = 0.05   ! Mortality rate of LZoo                                 [(mmolN m-3)-1 d-1] 
     &  , gamma_Z1_ADON = 0.05! Excretion rate of SZoo                                 [d-1]
     &  , gamma_Z2_ADON = 0.05! Excretion rate of LZoo                                 [d-1]
     &  , f2_Z1_NH4  = 0.75   ! Non Organic fraction of SZoo excretion                 [ ]
     &  , f2_Z2_NH4  = 0.75   ! Non Organic fraction of LZoo excretion                 [ ]
!-------------------------------------------------------------------------------------------------------
! Detritus
!-------------------------------------------------------------------------------------------------------
     &  , mu_Sd_DON  = 0.12   ! Hydrolysis rate of SDet                                [d-1]
     &  , mu_Ld_DON  = 0.08   ! Hydrolysis rate of LDet                                [d-1]
     &  , wSDet      = 1.0    ! Sinking velocity of SDet                               [m d-1]
     &  , wLDet      = 40.0   ! Sinking velocity of LDet                               [m d-1]
!-------------------------------------------------------------------------------------------------------
! Decomposition in oxic conditions
!-------------------------------------------------------------------------------------------------------
     &  , K_DON_NH4  = 0.006  ! Decomposition rate of DON                              [d-1]
     &  , K_Sd_NH4   = 0.014  ! Decomposition rate of SDet                             [d-1]
     &  , K_Ld_NH4   = 0.014  ! Decomposition rate of LDet                             [d-1]
     &  , Ktox       = 0.15   ! Temperature parameter                                  [°C-1]
     &  , O2ox       = 0.     ! Oxygen parameter                                       [mmolO2 m-3] 
     &  , Kox        = 15.    ! Half-saturation constant                               [mmolO2 m-3]
!-------------------------------------------------------------------------------------------------------
! Denitrification
!-------------------------------------------------------------------------------------------------------
     &  , K_NO3_NO2  = 1.2    ! Rate of 1st stage of denitrification                   [d-1]
     &  , K_NO2_N2O  = 2.     ! Rate of 2nd stage of denitrification                   [d-1]
     &  , O2denitr       = 25.! Oxygen parameter                                       [mmolO2 m-3] 
     &  , aNO3mi     = 0.001  ! NO3 parameter                                          [mmolN m-3] 
     &  , aNO2mi     = 0.0001 ! NO2 parameter                                          [mmolN m-3] 
!-------------------------------------------------------------------------------------------------------
! Nitrification
!-------------------------------------------------------------------------------------------------------
     &  , K_NH4_NO2  = 0.9    ! Rate of 1st stage of nitrification                     [d-1]     
     &  , K_NO2_NO3  = 2.5    ! Rate of 2nd stage of nitrification                     [d-1]
     &  , O2nf       = 1.     ! Oxygen parameter                                       [mmolO2 m-3] 
!-------------------------------------------------------------------------------------------------------
! Anammox
!-------------------------------------------------------------------------------------------------------
     &  , O2anam       = 25.  ! Oxygen parameter                                       [mmolO2 m-3] 
     &  , K_anam     = 0.3    ! Anammox constant                                       [d-1]
     &  , K_convert  = 1.     ! Constant for conversion                                [(mmolN m-3)-1]
!-------------------------------------------------------------------------------------------------------
! N2O formulation
!-------------------------------------------------------------------------------------------------------
#ifdef NITROUS_OXIDE   /*Suntharalingam et al. (2000,2012)*/
     &  , alpha_N2O  = 0.75e-4! Scalar multiplier                                      [molN2O molN-1]
     &  , beta_N2O   = 0.03   ! Scalar multiplier                                      [molN2O molN-1]
     &  , k_O2       = 0.1    !                                                        [ ]
     &  , O2max      = 1.     !                                                        [mmolO2 m-3]
     &  , N2O_atm    = 318.0  ! Dry mole fraction of atmospheric N2O                   [ppb]
#endif     
     &  )



