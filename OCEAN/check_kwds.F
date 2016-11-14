! $Id: check_kwds.F 1458 2014-02-03 15:01:25Z gcambon $
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
#include "cppdefs.h"
                                           ! Find place in the string
      subroutine cancel_kwd (keyword,ierr) ! Coptions(1:max_opt_size)
      implicit none                        ! which matches the input
#include "strings.h"
      character*(*) keyword                ! keyword and turn it to
      integer ierr, is,i,ie, lenkw,lenstr  ! to blank. Complain about
                                           ! error, if not found.
      lenkw=lenstr(keyword)
      is=0
   1   is=is+1
       if (Coptions(is:is).eq.' ' .and. is.lt.max_opt_size) goto 1

      ie=is
   2   ie=ie+1
       if (Coptions(ie:ie).ne.' ' .and. ie.lt.max_opt_size) goto 2

      if (lenkw.eq.ie-is .and. Coptions(is:ie-1).eq.keyword) then
c**     write(*,'(1x,A,1x,A)') 'Recognized: ', Coptions(is:ie-1)
        do i=is,ie-1 
          Coptions(i:i)=' '
        enddo
        return
      elseif (is.lt.max_opt_size) then
        is=ie
        goto 1
      endif
      write(*,'(2(/1x,A,1x,A,1x,A)/)') 'CANCEL_KW ERROR:',
     &        'Can not cancel keyword:',  keyword(1:lenkw),
     &        'Check SCRUM/ROMS input script for possible',
     &        'duplicated keyword.'
      ierr=ierr+1 
      return
      end 

                                      ! Check that keyword string is
      subroutine check_kwds (ierr)    ! empty, and if it is not print 
      implicit none                   ! out remaining keyword(s) as 
#include "strings.h"
      integer ierr, is,ie             ! an error message and increase
                                      ! error counter ierr.
      is=0
   1   is=is+1
       if (is .gt. max_opt_size) return !--> eventually
       if (Coptions(is:is) .eq. ' ') goto 1

      ie=is
   2   ie=ie+1
       if (Coptions(ie:ie).ne.' ' .and. ie.lt.max_opt_size) goto 2

      ierr=ierr+1
      write(*,'(/2(1x,A)/)') 'ERROR: keyword not found:',
     &                                  Coptions(is:ie-1)  
      is=ie
      goto 1
      end

