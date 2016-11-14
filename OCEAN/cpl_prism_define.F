! $Id: cpl_prism_define.F 1274 2013-09-16 09:24:57Z gcambon $
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
      SUBROUTINE cpl_prism_define
#include "cppdefs.h"
!     IMPLICIT NONE
#if defined OA_COUPLING || defined OW_COUPLING
#ifdef OA_MCT
      USE mod_prism             ! OASIS3 prism module
      INCLUDE 'mpif.h'
#else 
      USE mod_prism_def_partition_proto ! OASIS3 prism module for partitioning
      USE mod_prism_grids_writing ! OASIS3 prism module for writing grid files
#endif

#include "param.h"
#include "grid.h"
#include "scalars.h"
#include "mpi_roms.h"
      integer i, j, ji, i0, j0
      integer nproc, myproc, error_flg
      integer start_x, start_y
      real delta_lon, delta_lat

!     !-------------------------------------------------------------------
!     !             ***  ROUTINE cpl_prism_define  ***
!     !
!     ! ** Purpose :   Define grid and field information for ocean
!     !                exchange between ROMS and COUPLER. (OASIS3-MCT software)
!     !
!     ! ** Method  :   OASIS3 MPI communication
!     !--------------------------------------------------------------------

!------------------------------------------------------------------
!     Set the ROMS grid definition to PRISM system
!------------------------------------------------------------------

      start_x =  iminmpi
      start_y =  jminmpi
      
!     Set the ROMS partition definition to PRISM system
!------------------------------------------------------------------

      oasis_ig_paral(1) = 2     ! Box partition
      oasis_ig_paral(2) = (LLm)*(start_y-1) + (start_x-1)
      oasis_ig_paral(3) = Lmmpi
      oasis_ig_paral(4) = Mmmpi
      oasis_ig_paral(5) = LLm


      CALL prism_def_partition_proto(oasis_part_id, 
     &     oasis_ig_paral, comp_ierror)
      IF ( comp_ierror /= PRISM_Ok ) THEN
        CALL prism_abort_proto ( comp_id, 'cpl_prism_grid',
     &       'Failure in prism_def_partition')
      END IF

!------------------------------------------------------------------
!     Set the WRF exchange variables definition to PRISM system
!------------------------------------------------------------------

      oasis_var_nodims(1) = 2   ! Rank of the field array is 2
      oasis_var_nodims(2) = 1   ! Bundles always 1 for OASIS3

      oasis_var_type = PRISM_Real

      oasis_var_shape(1) = 1
      oasis_var_shape(2) = Lmmpi !Lm+2+padd_X + 1  !Lm
      oasis_var_shape(3) = 1
      oasis_var_shape(4) = Mmmpi !Mm+2+padd_E + 1  !Mm
!
      call cpl_prism_getlistvals(nmaxfld,srcv_clname,ssnd_clname,
     &   krcv,ksnd,oasis_runtime)
!      
!     ... Announce send variables.
      DO ji = 1, ksnd
          MPI_master_only write(*,*) 
     &    'CPL-ROMS: ',ssnd_clname(ji),' will be sent'
        CALL prism_def_var_proto ( ssnd_nid(ji),ssnd_clname(ji),
     &       oasis_part_id, oasis_var_nodims,
     &       PRISM_Out, oasis_var_shape,
     &       oasis_var_type, comp_ierror)
          MPI_master_only write(*,*) 
     &    'CPL-ROMS: ',ssnd_clname(ji),ssnd_nid(ji)
        IF ( comp_ierror /= PRISM_Ok ) THEN
          CALL prism_abort_proto ( srcv_nid(ji),'cpl_prism_define',
     &         'Failure in prism_def_var')
        ENDIF
      END DO

!     ... Announce receive variables.
      DO ji = 1, krcv
          MPI_master_only write(*,*) 
     &    'CPL-ROMS: ',srcv_clname(ji),' will be received'
        CALL prism_def_var_proto ( srcv_nid(ji),srcv_clname(ji),
     &       oasis_part_id, oasis_var_nodims, 
     &       PRISM_In, oasis_var_shape, 
     &       oasis_var_type, comp_ierror)
          MPI_master_only write(*,*) 
     &    'CPL-ROMS: ',srcv_clname(ji),srcv_nid(ji)         
        IF ( comp_ierror /= PRISM_Ok ) THEN
          CALL prism_abort_proto ( srcv_nid(ji),'cpl_prism_define', 
     &         'Failure in prism_def_var')
        ENDIF
      END DO

!------------------------------------------------------------------
!     End of definition phase
!------------------------------------------------------------------
      CALL MPI_Barrier( MPI_COMM_WORLD, comp_ierror ) 
#ifdef AGRIF
       if (.not.Agrif_Root()) then
       if (Agrif_fixed() == Agrif_Nb_Fine_Grids()) then
#endif
          MPI_master_only write(*,*) 
     &    'CPL-ROMS: ENDDEF'
         CALL prism_enddef_proto( comp_ierror )
         IF ( comp_ierror /= PRISM_Ok ) THEN
           CALL prism_abort_proto ( comp_id, 'cpl_prism_enddef', 
     &       'Failure in prism_enddef')
         ENDIF
#ifdef AGRIF
      endif
      endif
#endif

#else 
!---------------------------------------------------------------------------------------
!     Dummy subroutine                                                           NO Coupling
!---------------------------------------------------------------------------------------
#endif 

      END SUBROUTINE cpl_prism_define


