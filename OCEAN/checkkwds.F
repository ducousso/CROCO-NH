! $Id: checkkwds.F 1458 2014-02-03 15:01:25Z gcambon $
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
      program checkkwds
!
! Reads file "read_inp.F" as an input text file and creates file
! "setup_kwds.F" which contains subroutine setup_kwds which is
! adjoint to subroutine read_inp in the following sense:
!
!  1. CPP-control stucture of "setup_kwds.F" is identical to that
!     of "read_inp.F"; and
!
!  2. within this CPP-control stucture, for each code statement
!     of "read_inp.F",
!
!       if/elseif (keyword(1:kwlen).eq.'keyword_string') then
!
!     it creates a code segment in "setup_kwds.F" which records this
!     particular 'keyword_string' into a character array Coptions.
!
! Later, when "setup_kwds.F" is compiled and executed within the
! SCRUM/ROMS code, it creates a set of keywords which should be 
! present in the startup file in order to guarantee proper
! initialization of the model: "setup_kwds" along with two other
! subroutines, "cancel_kwd" and "check_kwds" provides a safety 
! mechanism, which verifies that all keyword-blocks required by
! the particular model configuration are present in the startup
! file.
!
! This safety  mechanism works as fillows: "setup_kwds" is called
! in the beginning of "read_inp" (before reading the startup file)
! and crates a character string which contains the set of keywords
! required by the model (depending on CPP-switches); then, every
! time when a keyword-block in identified and read from the startup
! file, call to "cancel_kwd" is made to remove (blank out) that
! keyword from the string. After the last reading of the startup file
! is complete, call to "check_kwds" is made to verify that the 
! the whole keyword is blank; if not, print an error message about
! what keywords were not cancelled and terminate the model run. 
!
! Created by Alexander Shchepetkin <alex@atmos.ucla.edu>, June 2000.
!
      implicit none
      integer input,iout, maxstring, nwords
      parameter (input=11, iout=12, maxstring=80, nwords=16)
      character string*80, backslash*1
      logical cont_switch, end_of_file
      integer is,i,ie, kwlen, lstring, iocheck, count
      backslash=char(92)
!
! Open output file and create subprogram title and declarations.
!
      write(*,'(/1x,A,1x,A/)') 'This is CHECKKWDS: Creating',
     &                        'new version of "setup_kwds.F".' 
      open(iout,file='setup_kwds.F', form='formatted')
      write(iout,'(A/,/6x,A/3(/A,1x,A)/A,8x,A/A,1x,A/A,39x,A)')
     &  '#include "cppdefs.h"',    'subroutine setup_kwds (ierr)',
     &  '!!!!!! WARNING: THIS IS A MACHINE GENERATED',
     &                                   'CODE, DO NOT EDIT! !!!!!!',
     &  '!!!!!! This file needs to be updated only if',
     &                                    'new keywords were !!!!!!',
     &  '!!!!!! introduced into "read_inp.F".',
     &                            'To create or refresh this !!!!!!',
     &  '!!!!!! file use compile and execute "checkkwds.F" as an',
     &  '!!!!!!', '!!!!!! independent program, or use',
     &                          'commands "make checkkwds"   !!!!!!',
     &  '!!!!!! or "make depend".',                         '!!!!!!'
      write(iout,'(2(/6x,A), 5(/A) /6x,A /8x,A, 2(/6x,A))')
     &  'implicit none', 'integer ierr, is,ie', '#include "param.h"', 
     &  '#include "strings.h"', '#ifdef MPI','# include "scalars.h"',
     &  '#endif',   'do is=1,max_opt_size',  'Coptions(is:is)='' ''',
     &  'enddo',    'is=1'
!
! Open input file and process it line-by-line: copy all CPP-command
! structure of the input file into the subprogram to be created.
!
      open(input,file='read_inp.F',status='old',form='formatted')
      count=0                           !
      cont_switch=.false.               ! Set string to blank, then
      end_of_file=.false.               ! read in a line from input 
   1   count=count+1                    ! file. Find position of the 
        do i=1,maxstring                ! last nonblank character. 
         string(i:i)=' '                ! Reject all lines, which...
        enddo                           ! 
        read(input,'(A)',iostat=iocheck,end=2) string
        goto 3                          !
   2    end_of_file=.true.              ! are blank (lstring=0), or  
   3    lstring=maxstring               !
   4    if (string(lstring:lstring).eq.' ') then
          lstring=lstring-1             ! are fortran comment lines, 
          if (lstring.gt.0) goto 4      ! or CPP #include commands.
        endif                           !
        if (lstring.eq.0 .and. end_of_file) goto 11
        if (lstring.eq.0       .or. string(1:1).eq.'!' .or.
     &      string(1:1).eq.'C' .or. string(1:1).eq.'c') goto 1
!
! Copy CPP-command lines other than  #include into the target file.
! Also set CPP-command continuation switch to copy the next line. 
!
        if (string(1:1).eq.'#') then
         is=2
   5      if (string(is:is).ne.'i' .and. is.lt.lstring-6) then
            is=is+1
            goto 5
          elseif (string(is:is+6).eq.'include') then
            goto 1
          endif
        endif 
        if (string(1:1).eq.'#' .or. cont_switch) then
          write(iout,'(A)') string(1:lstring)
          if (string(lstring:lstring).eq.backslash) then
            cont_switch=.true.
          else
            cont_switch=.false.
          endif
          goto 1
        endif
!
! Recognize keywords and create corresponding code in the subprogram.
!
        is=7
   6     if (string(is:is).ne.'k' .and. is.lt.lstring-6) then
           is=is+1
           goto 6
         elseif (string(is:is+6).eq.'keyword') then
           is=is+10
   7       if (string(is:is).ne.'k' .and. is.lt.lstring-4) then
             is=is+1
             goto 7
           elseif (string(is:is+4).eq.'kwlen') then
             is=is+6
   8         if (string(is:is).ne.'.' .and. is.lt.lstring-4) then 
               is=is+1
               goto 8
             elseif (string(is:is+3).eq. '.eq.') then
               is=is+4
   9           i=ichar(string(is:is))
               if (i.ne.39 .and. is.lt.lstring) then
                 is=is+1
                 goto 9
               elseif (i.eq.39) then
                 ie=is+1 
  10             i=ichar(string(ie:ie)) 
                 if (i.ne.39 .and. ie.lt.lstring) then
                   ie=ie+1
                   goto 10
                 elseif (i.eq.39) then
                   kwlen=ie-is-1
                   write(iout,'(6x,A,I2/6x,A/6x,A,A/6x,A/6x,A)')
     &                      'ie=is +', kwlen,
     &                      'if (ie.ge.max_opt_size) goto 99',
     &                      'Coptions(is:ie)=',     string(is:ie),
     &                      'Coptions(ie+1:ie+1)='' ''', 'is=ie+2'
                 endif
               endif
             endif
           endif
         endif
       goto 1
  11  close(input)
!
! Finalize the subprogram:
!
      write(iout,'(6x,A)') 'return'
      write(iout,'(2x,A/5x,A1,2x,A,1x,A/5x,A1,2x,A,1x,A)')
     &   '99  MPI_master_only write(stdout,''(/1x,A,A/14x,A)'')',
     &   '&', '''SETUP_KWDS ERROR: Unsufficient size of string',
     &     'Coptions'',', '&', '''in file "strings.h".'',',
     &                   '''Increase the size it and recompile.'''
      write(iout,'(6x,A,2(/6x,A))') 'ierr=ierr+1', 'return', 'end'
      close(iout)
      stop
      end 
