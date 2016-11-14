! $Id: lenstr.F 1458 2014-02-03 15:01:25Z gcambon $
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
      integer function lenstr (string)
!
! Returns the character position of the last nonblank character in
! input string after removing the leading blank characters, if any.
! Algorithm: Find length of input character string using intrinsic
! function "len", then find positions of  the first "is" and the
! last "ie" nonblank characters. Define the length of the string
! based on these bounds. In the case of there are any leading blank
! characters, shift the whole string to the beginning.
!
      implicit none
      integer is, ie
      character*(*) string
      ie=len(string)
   1   if (string(ie:ie).eq.' ') then
         ie=ie-1
         if (ie.gt.0) goto 1
       endif
      is=0
   2   is=is+1
       if (string(is:is).eq.' ' .and. is.lt.ie) goto 2
      lenstr=ie-is+1
      if (is.gt.1) string=string(is:ie)
      return
      end
