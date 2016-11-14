! $Id: cpl_prism_init.F 1159 2013-06-11 09:47:49Z gcambon $
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
       SUBROUTINE cpl_prism_init
#include "cppdefs.h"
#if defined OA_COUPLING || defined OW_COUPLING
#ifdef OA_MCT
          USE mod_prism            ! OASIS3 prism module
#else
          USE mod_prism_proto      ! OASIS3 prism module
#endif
          IMPLICIT NONE

#include "cppdefs.h"
#include "param.h" 
!         INTEGER :: ocean_grid_comm, comp_ierror,comp_id
!         CHARACTER(len=6)   :: comp_name
#include "mpi_roms.h"

      !!-------------------------------------------------------------------
      !!             ***  ROUTINE cpl_prism_init  ***
      !!
      !! ** Purpose :   Initialize coupled mode communication for ocean
      !!                exchange between WRF and COUPLER. (OASIS3 software)
      !!
      !! ** Method  :   OASIS3 MPI communication
      !!--------------------------------------------------------------------

      !------------------------------------------------------------------
      ! Initialize the PRISM system for the application
      !------------------------------------------------------------------

      CALL prism_init_comp_proto ( comp_id, comp_name, comp_ierror)
      IF (comp_ierror .NE. 0) THEN
        CALL prism_abort_proto(comp_id, 
     &                         'cpl_prism_ini', 'init_comp_proto')
      END IF

      !------------------------------------------------------------------
      ! Get an MPI communicator for ROMS/AGRIF local communication
      !------------------------------------------------------------------

      CALL prism_get_localcomm_proto ( ocean_grid_comm, comp_ierror )
      IF (comp_ierror .NE. 0) THEN
        CALL prism_abort_proto(comp_id, 'cpl_prism_ini', 
     &                         'get_localcomm_proto')
      END IF
#else 
!---------------------------------------------------------------------------------------
! Dummy subroutine                                                           NO Coupling
!---------------------------------------------------------------------------------------
#endif 
      END SUBROUTINE cpl_prism_init

