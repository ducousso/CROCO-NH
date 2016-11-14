! $Id: online.h 1458 2014-02-03 15:01:25Z gcambon $
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
!  This is include file "online.h"
!------------------------------------------------------------------------------
!  This file contains all the declarations regarding the online NCEP/CFSR tool:
!------------------------------------------------------------------------------
#ifdef ONLINE
      ! ------------------------ !
      ! Bulk number of variables !
      ! ------------------------ !
      integer nblkvrs
      parameter(nblkvrs=9)
      ! --------------------- !
      ! Bulk variable''s names !
      ! --------------------- !
      character*250 pathbulk, bulkfilename(nblkvrs), 
     &              blk_vname(4, nblkvrs)
      ! ---------------------- !
      ! Bulk variable''s tables !
      ! ---------------------- !
      real    dum_array(GLOBAL_2D_ARRAY,2,nblkvrs)
      integer itbulkO(nblkvrs), bulk_varid(nblkvrs),  
     &        ncidbulkO(nblkvrs)
      ! ------------------------------- !
      ! Bulk variable''s time parameters !
      ! ------------------------------- !
      integer bulkyearnum(nblkvrs), bulkmonthnum(nblkvrs),
     &        yearnum, monthnum, yearend, monthend, 
     &        recordsperday
      integer bulk_recO(nblkvrs), bulk_ncycleO(nblkvrs),
     &        bulk_tidO(nblkvrs), ntbulkO(nblkvrs)
      real    bulk_timeO(2,nblkvrs), bulk_cycleO(nblkvrs)
      logical newbulk(nblkvrs)
      ! -------------------------- !
      ! Bulk variable''s dimensions !
      ! -------------------------- !
      integer NX0(nblkvrs), NY0(nblkvrs)

      common /bulkonline_real/
     &        dum_array, 
     &        bulk_timeO, bulk_cycleO

      common /bulkonline_integer/
     &        itbulkO, bulk_varid, ncidbulkO,
     &        bulkyearnum, bulkmonthnum, yearnum, monthnum, 
     &        yearend, monthend, recordsperday,
     &        bulk_recO, bulk_ncycleO, bulk_tidO, ntbulkO,
     &        NX0, NY0

      common /bulkonline_character/
     &        pathbulk, bulkfilename, blk_vname

      common /bulkonline_logical/
     &        newbulk

#endif

