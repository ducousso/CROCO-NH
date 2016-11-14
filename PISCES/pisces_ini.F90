#include "cppdefs.h"

MODULE pisces_ini
   !!----------------------------------------------------------------------
#if defined key_pisces
   !!----------------------------------------------------------------------
   !!   'key_pisces'                                       PISCES bio-model
   !!----------------------------------------------------------------------
   USE ocean2pisces
   USE trcini_pisces

   !!* Substitution
#  include "ocean2pisces.h90"


   IMPLICIT NONE
   PRIVATE

   PUBLIC   pisces_ini_tile   ! called by trcini.F90 module

CONTAINS

    SUBROUTINE pisces_ini_tile( tile )

       INTEGER :: tile

#include "compute_tile_bounds.h"
      
       CALL ocean_2_pisces(Istr,Iend,Jstr,Jend)
       CALL trc_ini_pisces

    END SUBROUTINE pisces_ini_tile
 
#else
   !!----------------------------------------------------------------------
   !!   Dummy module                            No PISCES biochemical model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE pisces_ini_tile
   END SUBROUTINE pisces_ini_tile
#endif

   !!======================================================================
END MODULE pisces_ini
