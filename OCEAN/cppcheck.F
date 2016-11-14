! $Id: cppcheck.F 1458 2014-02-03 15:01:25Z gcambon $
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
      program cppcheck
!
! PURPOSE: Scan all existing CPP-switches in file "cppdefs.h" and
! automatomatically generate file check_switches1.F which contains
! subroutine check_switches1. When later this file is compiled and
! executed a part of SCRUM/ROMS model, it creates log of activated
! CPP-switches.
!
! Algorithm: this program reads line-by-line file "cppdefs.h" and
! creates catalog of CPP-switches found there. It does not matter,
! whether switches are in defined or undefined status, either way
! they are put into catalog. For the purpose of this algorithm
! CPP-switch (CPP-macro name) is a word which follows a command 
! (reserved word) of C-preprocessor, such as "ifdef", "define" etc.   
! Conversely, a word which follows another word which is not a
! CPP-command is not considered as a CPP-macro name.
!
! For the purpopse of preceeding paragraph "word" means a consecutive
! string of nonblank and nonspecial characters (i.e. letters, digits
! and ! underscore '_').
!
! The algorithm works as follows:
!---- --------- ----- -- --------
! 0. reset catalog: arrays of names of CPP-switches and their sizes;
! 1. read line from the file; ignore all lines which do not have #;
! 2. find non-trivial length of that line;
! 3. set all symbols within C-style comments to blank (' ');  
! 4. set all special characters to blank;
! 5. identify words within the modified line;
! 6. identify words which are CPP commands and CPP-macro names. 
! 7. for each CPP-macro name encountered in (6) check whether it
!     already listed in the catalog, and if not, place it there. 
! 8. Once catalog is complete, generate code for checkdefs.
!
! Created by Alexander Shchepetkin <alex@atmos.ucla.edu> on May 2000.
!
      implicit none
      integer input,iout, maxstring, lstring
      parameter (input=11, iout=12, maxstring=80)
      character*80 string
      integer nwords                    , nswitches,nexample
     &                                  , max_switches
      parameter (nwords=16              , max_switches=1024)
      integer istart(nwords), is        , size(max_switches)
     &       ,  iend(nwords), ie,ln     , line(max_switches)
      logical macro(nwords)             , example
      character*32                        switch(max_switches) 
      integer count, iocheck, i,k,n
      logical end_of_file, comment, word, command, new

      write(*,'(/1x,A,1x,A/)') 'This is CPPCHECK: Creating',
     &                   'new version of check_switches1.F.'
      example=.true.
      nexample=0                        !
      nswitches=0                       ! Initialize catalog.
      do i=1,max_switches               ! reset/blank out arrays
        size(i)=0                       ! of sizes and names.
        switch(i)='                                '
      enddo   !!!  12345678901234567890123456789012

!      open(input,file='cppdefs.h',status='old',form='formatted')
      open(input,file='mergcpp.txt',status='old',form='formatted')

      count=0     ! <-- reset counter of lines within the file.
      end_of_file=.false.               !
   1   count=count+1                    ! Read line from input file.
        do i=1,maxstring                ! Ignore all lines, which do
         string(i:i)=' '                ! not start with #.
        enddo                           !
        read(input,'(A)',iostat=iocheck,end=2) string
        if (string(1:1).ne.'#') goto 1  !
        goto 3                          ! Find length of the string,
   2    end_of_file=.true.              ! which is equal to position
                                        ! of the most right nonblank 
   3    lstring=maxstring+1             ! character. 
   4     lstring=lstring-1              !
         if ((string(lstring:lstring).eq.' ').AND.(lstring.GT.1)) goto 4
!
! Suppress C-style comments and special characters.
!
        n=0       ! <-- reset counter of comments within the string.
        comment=.false.
        i=1
   5     i=i+1
          if (.not.comment .and. string(i:i+1).eq.'/*') then
            comment=.true.
            n=n+1
            istart(n)=i
          elseif (comment  .and. string(i:i+1).eq.'*/') then
            comment=.false.
            iend(n)=i+1
          endif 
         if (i+1.lt.lstring) goto 5     ! 
        if (comment) then               ! If string ends as an open
          lstring=istart(n)-1           ! comment, restrict lstring  
          n=n-1                         ! and disregard all symbols 
        endif                           ! one right right from it. 
        do k=1,n                        !
          do i=istart(k),iend(k)        !
            string(i:i)=' '             ! blank out C-style comments
          enddo                         !
        enddo                           ! Suppress special characters
        do i=1,lstring
c*          if (string(i:i).eq.'(' .or. string(i:i).eq.')' .or.
c*     &        string(i:i).eq.'&' .or. string(i:i).eq.'|'  .or.
c*     &        string(i:i).eq.'!' .or. ichar(string(i:i)).eq.9)
c*     &      string(i:i)=' '           ! Character 9 is TaB symbol.

          k=ichar(string(i:i))
          if (k.lt.48 .or. (k.gt.57 .and. k.lt.65) .or. (k.gt.90
     &    .and. k.lt.95) .or. k.eq.96 .or. k.gt.122) string(i:i)=' '
        enddo
!
! Identify words within the string, find starting and ending
! characters of each word. Since all special characters have
! been removed, at this point word is a sequence of non-blank
! characters.
!
        n=0       ! <-- reset counter of words within the string.
        word=.false.
        i=1
   6     i=i+1
          if (string(i:i).ne.' ' .and. .not.word) then
            word=.true.
            n=n+1
            istart(n)=i
          elseif (string(i:i).eq.' ' .and.  word) then
            word=.false.
            iend(n)=i-1
          endif
         if (i.lt.lstring) goto 6
        if (word) iend(n)=i

c**     write(*,'(/,I4,I4,/)') count, n ! Print out words.
c**     do k=1,n
c**       write(*,'(10x,80A1)') (string(i:i), i=istart(k),iend(k))
c**     enddo                           !
                                        ! Identify CPP-commands (i.e.
        command=.false.                 ! reserved words) and CPP- 
        do k=1,n                        ! macro names among the words 
          macro(k)=.false.              ! of the line. Cancel example
          is=istart(k)                  ! switch when encounter first
          ie=iend(k)                    ! conditional CPP-command. 
          ln=ie-is+1                    !

          if (ln.eq.6 .and. string(is:ie).eq.'define') then
            command=.true.
          elseif (ln.eq.5 .and. string(is:ie).eq.'undef') then
            command=.true.
          elseif (ln.eq.2 .and. string(is:ie).eq.'if') then
            command=.true.
            example=.false.
          elseif (ln.eq.5 .and. string(is:ie).eq.'ifdef') then
            command=.true.
            example=.false.
          elseif (ln.eq.7 .and. string(is:ie).eq.'defined') then
            command=.true.
            example=.false.
          elseif (ln.eq.4 .and. string(is:ie).eq.'elif') then
            command=.true.
            example=.false.
          elseif (ln.eq.4 .and. string(is:ie).eq.'else') then

          elseif (ln.eq.5 .and. string(is:ie).eq.'endif') then

          elseif (ln.eq.7 .and. string(is:ie).eq.'include') then

          elseif (command) then
            command=.false.
            macro(k)=.true.
c**       elseif (string(istart(1):iend(1)) .ne. 'include') then
c**         write (*,'(6x,A,1x,A,1x,I4,A1/8x,A)') 'CPPCHECK ERROR:',
c**  &      'Unknown CPP-command on line', count, ':', string(is:ie)
          endif
        enddo
c**     write(*,'(/,I4,I4,/)') count, n ! Print out CPP-macro names.
c**     do k=1,n
c**       if (macro(k)) then
c**         write(*,'(10x,80A1)') (string(i:i),i=istart(k),iend(k))
c**       endif
c**     enddo
                                        !
        do k=1,n                        ! Scan catalog of previously
          if (macro(k)) then            ! discovered switches to find
            is=istart(k)                ! match with CPP-macro names
            ie=iend(k)                  ! found in the present line. 
            ln=ie-is+1                  ! If no match is found, add
            new=.true.                  ! the new switch to the
            do i=1,nswitches            ! catalog.
              if (ln.eq.size(i)) then   !
                if (string(is:ie).eq.switch(i)(1:ln)) new=.false.
              endif
            enddo
            if (new) then
              nswitches=nswitches+1
              size(nswitches)=ln
              switch(nswitches)(1:ln)=string(is:ie)
              line(nswitches)=count
              if (example) nexample=nexample+1
            endif                       !
          endif                         ! CPP-switches found prior
        enddo                           ! to the first conditional
      if (.not.end_of_file) goto 1      ! CPP-command correspond to 
      close(unit=input)                   ! predefined examples.

c**   write(*,'(/,I4,/)') nswitches     ! Print out catalog.
c**    do i=1,nswitches
c**      ln=size(i)
c**     write(*,'(10x,I4,I4,2x,A)') line(i), ln, switch(i)(1:ln)
c**   enddo
!
! Generate CPP-checking subroutine. 
!
      open (unit=iout,file='check_switches1.F',form='formatted')
      write(iout,'(A/)')  '#include "cppdefs.h"'
      write(iout,'(/6x,A/)') 'subroutine check_switches1 (ierr)'
      write(iout,'(4(A,1x,A/),A,14x,A/A,1x,A,3x,A/A,14x,A/A,22x,A)')
     &  '!!!!!! WARNING: THIS IS A MACHINE GENERATED',
     &                                   'CODE, DO NOT EDIT! !!!!!!',
     &  '!!!!!! This file needs to be updated only if',
     &                                    'new CPP-switches  !!!!!!',
     &  '!!!!!! were introduced into "cppdefs.h".',
     &                                ' NO ACTION IS NEEDED  !!!!!!',
     &  '!!!!!! if changes in "cppdefs.h" are limited',
     &                                    'to activation or  !!!!!!',
     &  '!!!!!! deactivation of previously known switches.','!!!!!!', 
     &  '!!!!!! To refresh this file compile and execute',
     &                                      '"cppcheck.F"', '!!!!!!',
     &  '!!!!!! as an independent program, or use commands','!!!!!!',
     &  '!!!!!! "make checkdefs" or "make depend".',        '!!!!!!' 
      write(iout,'(A,20x,I3,1x,A/A,23x,I3,1x,A)')
     &  '!!!!!! Number of Configuration Choices:',nexample, '!!!!!!',
     &  '!!!!!! Total number of CPP-switches:', nswitches,  '!!!!!!'
      write(iout,'(2(/6x,A), 5(/A) /6x,A /5x,A1,6x,A, 5(/6x,A))')
     &  'implicit none',        'integer ierr, is,ie, iexample', 
     &  '#include "param.h"',   '#include "strings.h"', 
     &  '#ifdef MPI',           '# include "scalars.h"',    '#endif',
     &  'MPI_master_only write(stdout,''(/1x,A/)'')',       '&',
     &  '''Activated C-preprocessing Options:''',
     &  'do is=1,max_opt_size', '  Coptions(is:is)='' ''',  'enddo',
     &                                       'iexample=0',  'is=1'
      do i=1,nswitches
        ln=size(i)
        write(iout,'(A,1x,A)') '#ifdef', switch(i)(1:ln)
        if (i.le.nexample) write(iout,'(6x,A)') 'iexample=iexample+1'
        write(iout,'(6x,A,1x,A1,A,A1)')
     &         'MPI_master_only write(stdout,''(10x,A)'')',
     &                       '''', switch(i)(1:ln), ''''
        write(iout,'(6x,A7,I2/6x,A/6x,A,A,A1/6x,A/6x,A/A)')
     &       'ie=is +', ln-1, 'if (ie.ge.max_opt_size) goto 99', 
     &       'Coptions(is:ie)=''', switch(i)(1:ln), '''',
     &       'Coptions(ie+1:ie+1)='' ''', 'is=ie+2', '#endif'
      enddo
      write(iout,'(6x,A/6x,A/8x,A/5x,A1,1x,A/8x,A/6x,A)')
     &     'MPI_master_only write(stdout,''(/)'')',
     &     'if (iexample.eq.0) then',
     &     'MPI_master_only write(stdout,''(1x,A)'')', '&',
     &   '''ERROR in "cppdefs.h": no configuration is specified.''',
     &     'ierr=ierr+1',  'elseif (iexample.gt.1) then'
      write(iout,'(8x,A/5x,A1,1x,A/8x,A/6x,A/6x,A)')
     &     'MPI_master_only write(stdout,''(1x,A)'')', '&',
     &   '''ERROR: more than one configuration in "cppdefs.h".''',
     &     'ierr=ierr+1', 'endif', 'return'
      write(iout,'(2x,A/5x,A1,2x,A,1x,A/5x,A1,2x,A,1x,A)')
     &   '99  MPI_master_only write(stdout,''(/1x,A,A/14x,A)'')',
     &   '&', '''CHECKDEFS -- ERROR: Unsufficient size of string',
     &     'Coptions'',', '&', '''in file "strings.h".'',',
     &                   '''Increase the size it and recompile.''' 
      write(iout,'(6x,A,2(/6x,A))') 'ierr=ierr+1', 'return', 'end'
      close(unit=iout)
      stop
      end
