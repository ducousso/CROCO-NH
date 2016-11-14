#include "cppdefs.h"

MODULE par_pisces
   !!======================================================================
   !!                        ***  par_pisces  ***
   !! TOP :   set the PISCES parameters
   !!======================================================================
   !! History :   2.0  !  2007-12  (C. Ethe, G. Madec)  revised architecture
   !!----------------------------------------------------------------------
   !! NEMO/TOP 2.0 , LOCEAN-IPSL (2007) 
   !! $Id: par_pisces.F90 1152 2008-06-26 14:11:13Z rblod $ 
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
   IMPLICIT NONE
   PUBLIC

   INTEGER, PUBLIC, PARAMETER ::   jp_l      = 0      !: cumulative number of already defined TRC
   INTEGER, PUBLIC, PARAMETER ::   jp_l_2d   = 0   !:
   INTEGER, PUBLIC, PARAMETER ::   jp_l_3d   = 0   !:
   INTEGER, PUBLIC, PARAMETER ::   jp_l_trd  = 0  !:

#if defined key_pisces  &&  defined key_kriest
   !!---------------------------------------------------------------------
   !!   'key_pisces' & 'key_kriest'                 PISCES bio-model + ???
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_pisces     = .TRUE.  !: PISCES flag 
   LOGICAL, PUBLIC, PARAMETER ::   lk_kriest     = .TRUE.  !: Kriest flag 
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces     =  23     !: number of passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces_2d  =  13     !: additional 2d output ('key_trc_diaadd')
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces_3d  =  18     !: additional 3d output ('key_trc_diaadd')
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces_trd =   1     !: number of sms trends for PISCES

   ! assign an index in trc arrays for each LOBSTER prognostic variables
   !    WARNING: be carefull about the order when reading the restart
        !   !!gm  this warning should be obsolet with IOM
   INTEGER, PUBLIC, PARAMETER ::   jpdic = jp_l +  1    !: dissolved inoganic carbon concentration 
   INTEGER, PUBLIC, PARAMETER ::   jptal = jp_l +  2    !: total alkalinity 
   INTEGER, PUBLIC, PARAMETER ::   jpoxy = jp_l +  3    !: oxygen carbon concentration 
   INTEGER, PUBLIC, PARAMETER ::   jpcal = jp_l +  4    !: calcite  concentration 
   INTEGER, PUBLIC, PARAMETER ::   jppo4 = jp_l +  5    !: phosphate concentration 
   INTEGER, PUBLIC, PARAMETER ::   jppoc = jp_l +  6    !: small particulate organic phosphate concentration
   INTEGER, PUBLIC, PARAMETER ::   jpsil = jp_l +  7    !: silicate concentration
   INTEGER, PUBLIC, PARAMETER ::   jpphy = jp_l +  8    !: phytoplancton concentration 
   INTEGER, PUBLIC, PARAMETER ::   jpzoo = jp_l +  9    !: zooplancton concentration
   INTEGER, PUBLIC, PARAMETER ::   jpdoc = jp_l + 10    !: dissolved organic carbon concentration 
   INTEGER, PUBLIC, PARAMETER ::   jpdia = jp_l + 11    !: Diatoms Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpmes = jp_l + 12    !: Mesozooplankton Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpbsi = jp_l + 13    !: (big) Silicate Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpfer = jp_l + 14    !: Iron Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpnum = jp_l + 15    !: Big iron particles Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpsfe = jp_l + 16    !: number of particulate organic phosphate concentration
   INTEGER, PUBLIC, PARAMETER ::   jpdfe = jp_l + 17    !: Diatoms iron Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpdsi = jp_l + 18    !: Diatoms Silicate Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpnfe = jp_l + 19    !: Nano iron Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpnch = jp_l + 20    !: Nano Chlorophyll Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpdch = jp_l + 21    !: Diatoms Chlorophyll Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpno3 = jp_l + 22    !: Nitrates Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpnh4 = jp_l + 23    !: Ammonium Concentration

#elif defined key_pisces
   !!---------------------------------------------------------------------
   !!   'key_pisces'   :                         standard PISCES bio-model
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_pisces     = .TRUE.  !: PISCES flag 
   LOGICAL, PUBLIC, PARAMETER ::   lk_kriest     = .FALSE. !: Kriest flag 
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces     = 24      !: number of PISCES passive tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces_2d  = 11      !: additional 2d output ('key_trc_diaadd')
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces_3d  = 16      !: additional 3d output ('key_trc_diaadd')
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces_trd =  1      !: number of sms trends for PISCES

   ! assign an index in trc arrays for each LOBSTER prognostic variables
   !    WARNING: be carefull about the order when reading the restart
        !   !!gm  this warning should be obsolet with IOM
   INTEGER, PUBLIC, PARAMETER ::   jpdic = jp_l +  1    !: dissolved inoganic carbon concentration 
   INTEGER, PUBLIC, PARAMETER ::   jptal = jp_l +  2    !: total alkalinity 
   INTEGER, PUBLIC, PARAMETER ::   jpoxy = jp_l +  3    !: oxygen carbon concentration 
   INTEGER, PUBLIC, PARAMETER ::   jpcal = jp_l +  4    !: calcite  concentration 
   INTEGER, PUBLIC, PARAMETER ::   jppo4 = jp_l +  5    !: phosphate concentration 
   INTEGER, PUBLIC, PARAMETER ::   jppoc = jp_l +  6    !: small particulate organic phosphate concentration
   INTEGER, PUBLIC, PARAMETER ::   jpsil = jp_l +  7    !: silicate concentration
   INTEGER, PUBLIC, PARAMETER ::   jpphy = jp_l +  8    !: phytoplancton concentration 
   INTEGER, PUBLIC, PARAMETER ::   jpzoo = jp_l +  9    !: zooplancton concentration
   INTEGER, PUBLIC, PARAMETER ::   jpdoc = jp_l + 10    !: dissolved organic carbon concentration 
   INTEGER, PUBLIC, PARAMETER ::   jpdia = jp_l + 11    !: Diatoms Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpmes = jp_l + 12    !: Mesozooplankton Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpbsi = jp_l + 13    !: (big) Silicate Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpfer = jp_l + 14    !: Iron Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpbfe = jp_l + 15    !: Big iron particles Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpgoc = jp_l + 16    !: big particulate organic phosphate concentration
   INTEGER, PUBLIC, PARAMETER ::   jpsfe = jp_l + 17    !: Small iron particles Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpdfe = jp_l + 18    !: Diatoms iron Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpdsi = jp_l + 19    !: Diatoms Silicate Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpnfe = jp_l + 20    !: Nano iron Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpnch = jp_l + 21    !: Nano Chlorophyll Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpdch = jp_l + 22    !: Diatoms Chlorophyll Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpno3 = jp_l + 23    !: Nitrates Concentration
   INTEGER, PUBLIC, PARAMETER ::   jpnh4 = jp_l + 24    !: Ammonium Concentration

   INTEGER, PUBLIC, PARAMETER ::   jp_flxco2  = jp_l_2d + 1    !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_flxo2   = jp_l_2d + 2    !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_kgco2   = jp_l_2d + 3    !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_dpco2   = jp_l_2d + 4    !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_sinkco2 = jp_l_2d + 5    !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_sinkfer = jp_l_2d + 6    !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_sinksil = jp_l_2d + 7    !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_sinkcal = jp_l_2d + 8    !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_heup    = jp_l_2d + 9    !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_irondep = jp_l_2d + 10    !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_nitrpot = jp_l_2d + 11   !: 

   INTEGER, PUBLIC, PARAMETER ::   jp_hi      = jp_l_3d + 1   !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_co3     = jp_l_3d + 2   !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_co3sat  = jp_l_3d + 3   !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_etot    = jp_l_3d + 4   !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_pphy    = jp_l_3d + 5   !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_pphy2   = jp_l_3d + 6   !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_pnew    = jp_l_3d + 7   !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_pnew2   = jp_l_3d + 8   !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_pbsi    = jp_l_3d + 9   !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_pfed    = jp_l_3d + 10   !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_pfen    = jp_l_3d + 11   !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_pnewo2  = jp_l_3d + 12   !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_prego2  = jp_l_3d + 13   !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_grazt   = jp_l_3d + 14   !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_nitrifo2 = jp_l_3d + 15   !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_remino2 = jp_l_3d + 15   !: 
   INTEGER, PUBLIC, PARAMETER ::   jp_nfixo2  = jp_l_3d + 16   !: 
#else
   !!---------------------------------------------------------------------
   !!   Default                                   No CFC geochemical model
   !!---------------------------------------------------------------------
   LOGICAL, PUBLIC, PARAMETER ::   lk_pisces     = .FALSE.  !: CFC flag 
   LOGICAL, PUBLIC, PARAMETER ::   lk_kriest     = .FALSE.  !: Kriest flag 
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces     =  0       !: No CFC tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces_2d  =  0       !: No CFC additional 2d output arrays 
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces_3d  =  0       !: No CFC additional 3d output arrays 
   INTEGER, PUBLIC, PARAMETER ::   jp_pisces_trd =  0       !: number of sms trends for PISCES
#endif

   ! Starting/ending PISCES do-loop indices (N.B. no PISCES : jpl_pcs < jpf_pcs the do-loop are never done)
   INTEGER, PUBLIC, PARAMETER ::   jptra       = jp_pisces                  !: First index of PISCES tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs0     = jp_l + 1                  !: First index of PISCES tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs1     = jp_l + jp_pisces          !: Last  index of PISCES tracers
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs0_2d  = jp_l_2d + 1               !: First index of 2D diag
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs1_2d  = jp_l_2d + jp_pisces_2d    !: Last  index of 2D diag
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs0_3d  = jp_l_3d + 1               !: First index of 3D diag
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs1_3d  = jp_l_3d + jp_pisces_3d    !: Last  index of 3d diag
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs0_trd = jp_l_trd + 1              !: First index of bio diag
   INTEGER, PUBLIC, PARAMETER ::   jp_pcs1_trd = jp_l_trd + jp_pisces_trd  !: Last  index of bio diag

   !!======================================================================
END MODULE par_pisces
