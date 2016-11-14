#include "cppdefs.h"

MODULE trcwri_pisces
   !!======================================================================
   !!                       *** MODULE trcwri ***
   !!    PISCES :   Output of PISCES tracers
   !!======================================================================
   !! History :   1.0  !  2009-05 (C. Ethe)  Original code
   !!----------------------------------------------------------------------
#if defined key_iomput && defined key_pisces 
   !!----------------------------------------------------------------------
   !! trc_wri_pisces   :  outputs of concentration fields
   !!----------------------------------------------------------------------
   USE sms_pisces  ! PISCES variables
   USE trc         ! passive tracers common variables 

   IMPLICIT NONE
   PRIVATE

   PUBLIC trc_wri_pisces 

   !! * Substitutions
#include "ocean2pisces.h90"


CONTAINS

   SUBROUTINE trc_wri_pisces
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE trc_wri_trc  ***
      !!
      !! ** Purpose :   output passive tracers fields 
      !!---------------------------------------------------------------------
      CHARACTER (len=20)   :: cltra, cltras
      REAL(wp)             :: zrfact
      INTEGER              :: ji, jj, jk, jn
      !!---------------------------------------------------------------------
 
      DO jn = jp_pcs0, jp_pcs1
         zrfact = 1.0
         cltra = TRIM( ctrcnm(jn) )                  ! short title for tracer
         CALL iom_put( cltra, trn(:,:,:,jn) * zrfact )
      END DO
# if defined key_trc_diaadd
      CALL iom_put( "Cflx"    , trc2d(:,:,jp_flxco2)   )
      CALL iom_put( "Oflx"    , trc2d(:,:,jp_flxo2)    )
      CALL iom_put( "Kg"      , trc2d(:,:,jp_kgco2)    )
      CALL iom_put( "Dpco2"   , trc2d(:,:,jp_dpco2)    )
      CALL iom_put( "EPC100"  , trc2d(:,:,jp_sinkco2)  )
      CALL iom_put( "EPFE100" , trc2d(:,:,jp_sinkfer)  )
      CALL iom_put( "EPSI100" , trc2d(:,:,jp_sinksil)  )
      CALL iom_put( "EPCAL100", trc2d(:,:,jp_sinkcal)  )
      CALL iom_put( "Heup"    , trc2d(:,:,jp_heup)     )
      CALL iom_put( "Nfix"    , trc2d(:,:,jp_nfix)     )
      CALL iom_put( "Irondep" , trc2d(:,:,jp_irondep)  ) 
      !
      CALL iom_put( "PH"      , trc3d(:,:,:,jp_hi)     )
      CALL iom_put( "CO3"     , trc3d(:,:,:,jp_co3)    )
      CALL iom_put( "CO3sat"  , trc3d(:,:,:,jp_co3sat) )
      CALL iom_put( "PAR"     , trc3d(:,:,:,jp_etot)   )
      CALL iom_put( "PPPHY"   , trc3d(:,:,:,jp_pphy)   )
      CALL iom_put( "PPPHY2"  , trc3d(:,:,:,jp_pphy2)  )
      CALL iom_put( "PPNEWN"  , trc3d(:,:,:,jp_pnew)   )
      CALL iom_put( "PPNEWD"  , trc3d(:,:,:,jp_pnew2)  )
      CALL iom_put( "PBSi"    , trc3d(:,:,:,jp_pbsi)   )
      CALL iom_put( "PFeN"    , trc3d(:,:,:,jp_pfen)   )
      CALL iom_put( "PFeD"    , trc3d(:,:,:,jp_pfed)   )
      CALL iom_put( "PPNEWo2" , trc3d(:,:,:,jp_pnewo2) )
      CALL iom_put( "PPRego2" , trc3d(:,:,:,jp_prego2) )
      CALL iom_put( "GRAZ"    , trc3d(:,:,:,jp_grazt)  )
      CALL iom_put( "Nito2"   , trc3d(:,:,:,jp_nitro2) )
      CALL iom_put( "Nremino2", trc3d(:,:,:,jp_remino2))
      CALL iom_put( "Nfixo2"  , trc3d(:,:,:,jp_nfixo2) )
#endif
      !
   END SUBROUTINE trc_wri_pisces
#else
   !!----------------------------------------------------------------------
   !!  Dummy module :                                     No passive tracer
   !!----------------------------------------------------------------------
   PUBLIC trc_wri_pisces
CONTAINS
   SUBROUTINE trc_wri_pisces                     ! Empty routine  
   END SUBROUTINE trc_wri_pisces
#endif

   !!----------------------------------------------------------------------
   !! NEMO/TOP 3.3 , NEMO Consortium (2010)
   !! $Id: trcwri_pisces.F90 3160 2011-11-20 14:27:18Z cetlod $ 
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!======================================================================
END MODULE trcwri_pisces
