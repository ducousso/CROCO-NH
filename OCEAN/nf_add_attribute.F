! $Id: nf_add_attribute.F 1458 2014-02-03 15:01:25Z gcambon $
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
!----------------------------------------------------------------
! Add attribute if any, begining at index indx = 5, to 11 for now
! but it can be updated easily
!====
!  5 :  the standard_name 
!  6 :  coordinates 
!  7 :  axis
!  8 :  _FillValue
!  9 :  positive for vertical axis
! 10 :  time origin
! 11 :  calendar for time
!==
! 12 :  
! 13 :  
!---------------------------------------------------------------
#include "cppdefs.h"
      subroutine nf_add_attribute(ncid, vid, indx, i, nf_type, ierr)
      implicit none
      integer ncid,vid,lvar,indx,ierr,lenstr,i, nf_type
#include "param.h"
#include "scalars.h"
#include "ncscrum.h"
#include "netcdf.inc"
 
      character*70 text
      
      text=vname(i,indx)
      lvar=lenstr(text)
      if (LEN_TRIM(text).gt.0) then
         ierr=nf_put_att_text (ncid, vid, 'standard_name',
     &        lvar, text(1:lvar))
      endif
      
      text=vname(i+1,indx)
      lvar=lenstr(text)
      if (LEN_TRIM(text).gt.0) then
         ierr=nf_put_att_text (ncid, vid, 'coordinates',
     &        lvar, text(1:lvar))
      endif
      
      text=vname(i+2,indx)
      lvar=lenstr(text)
      if (LEN_TRIM(text).gt.0) then
         ierr=nf_put_att_text (ncid, vid, 'axis',
     &        lvar, text(1:lvar))
      endif
      
      text=vname(i+4,indx)
      lvar=lenstr(text)
      if (LEN_TRIM(text).gt.0) then
         ierr=nf_put_att_text (ncid, vid, 'positive',
     &        lvar, text(1:lvar))
      endif
      text=vname(i+5,indx)
      lvar=lenstr(text)
      if (LEN_TRIM(text).gt.0) then
         ierr=nf_put_att_text (ncid, vid, 'time_origin',
     &        lvar, text(1:lvar))
      endif

      text=vname(i+6,indx)
      lvar=lenstr(text)
      if (LEN_TRIM(text).gt.0) then
         ierr=nf_put_att_text (ncid, vid, 'calendar',
     &        lvar, text(1:lvar))
      endif
#ifdef FILLVAL
      text=vname(i+3,indx)
      if (TRIM(text) /= '.NO.') then
      ierr= nf_put_att_FTYPE (ncid, vid,'_FillValue', nf_type,
     &     1, spval)
      endif
#endif      
      return
      end
      
