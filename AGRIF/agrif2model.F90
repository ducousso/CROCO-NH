#if defined key_agrif
   !!----------------------------------------------------------------------
   !! NEMO/NST 3.3 , NEMO Consortium (2010)
   !! $Id: agrif2model.F90 3680 2012-11-27 14:42:24Z rblod $
   !! Software governed by the CeCILL licence (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
   SUBROUTINE Agrif2Model
      !!---------------------------------------------
      !!   *** ROUTINE Agrif2Model ***
      !!--------------------------------------------- 
   END SUBROUTINE Agrif2model

   SUBROUTINE Agrif_Set_numberofcells(Agrif_Gr)
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_Set_numberofcells ***
      !!--------------------------------------------- 
      USE Agrif_Grids
      IMPLICIT NONE

      Type(Agrif_Grid), Pointer :: Agrif_Gr

      IF ( associated(Agrif_Curgrid) )THEN
#include "SetNumberofcells.h"
      ENDIF

   END SUBROUTINE Agrif_Set_numberofcells

   SUBROUTINE Agrif_Get_numberofcells(Agrif_Gr)
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_Get_numberofcells ***
      !!--------------------------------------------- 
      USE Agrif_Grids
      IMPLICIT NONE

      Type(Agrif_Grid), Pointer :: Agrif_Gr

    if ( associated(Agrif_Curgrid) ) then
#include "GetNumberofcells.h"
    endif

   END SUBROUTINE Agrif_Get_numberofcells

   SUBROUTINE Agrif_Allocationcalls(Agrif_Gr)
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_Allocationscalls ***
      !!--------------------------------------------- 
      USE Agrif_Grids 
#include "include_use_Alloc_agrif.h"
      IMPLICIT NONE

      Type(Agrif_Grid), Pointer :: Agrif_Gr

#include "allocations_calls_agrif.h"

   END SUBROUTINE Agrif_Allocationcalls

   SUBROUTINE Agrif_probdim_modtype_def()
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_probdim_modtype_def ***
      !!--------------------------------------------- 
      USE Agrif_Types
      IMPLICIT NONE

#include "modtype_agrif.h"
#include "probdim_agrif.h"
#include "keys_agrif.h"

      Return

   END SUBROUTINE Agrif_probdim_modtype_def

   SUBROUTINE Agrif_clustering_def()
      !!---------------------------------------------
      !!   *** ROUTINE Agrif_clustering_def ***
      !!--------------------------------------------- 
      IMPLICIT NONE

      Return

   END SUBROUTINE Agrif_clustering_def

#else
   SUBROUTINE Agrif2Model
      !!---------------------------------------------
      !!   *** ROUTINE Agrif2Model ***
      !!--------------------------------------------- 
      WRITE(*,*) 'Impossible to bet here'
   END SUBROUTINE Agrif2model
#endif
