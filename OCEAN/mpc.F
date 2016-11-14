! $Id: mpc.F 1458 2014-02-03 15:01:25Z gcambon $
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
      program mpc
!
! Multifunctional precompiling processor designed to work between
! CPP and FORTRAN compiler.
!-------------------------------------------------------------------
! Objectives:
!------------ 
! (1) Eliminate blank lines left by CPP and fortran style comments,
!     but keep compiler directives like C$DOACROSS in place.
!
! (2) Usually Fortran syntax does not prohibit the practice when a
!     quotation is opened on one line and closes on the next line.
!     Doing so, however, may be considered as unsafe and sloppy from
!     stylistical point of view; mpc enforces the rule when every
!     string inside quotation mark must be closed within the line. 
!   --> Optional:: activated by TRAP_SINGLE_QUOTES CPP-switch.
!
! (3) Convert _default_ size fortran real type declarations into
!     real*8.  Any mixture of lower and upper cases latters is
!     allowed in the word real. 
!     Explicitly specified size declaration, such as real*4
!     or real*16 will be left unchanged.
!   --> Optional:: activated by REAL_TO_REAL8 CPP-switch.
!
! (4) Enforce double precision accuracy for the real type constants
!     appearing anywhere in the program, for example, 1. --> 1.D0;
!     .1 --> .1D0;    9.81 --> 9.81D0;   .5e-8 --> .5D-8; 
!     1.2e+14 --> 1.2D+14 etc.
!   --> Safe. Will distinguish real type constants from look alike
!     combinations like, say format descriptors F8.4 or e12.5; or
!     logical expressions like 1.eq.i etc. Will fold line of the
!     code automatically according to fortran syntax rules, when
!     this change causes line to increase beyond 72 characters. 
!   --> Optional: activated by DOUBLE_CONST CPP-switch.
!
!
! (5) Convert _default_ size fortran integer type declarations
!     into integer*4. Explicitly specified size declaration, such
!     as integer*2 or integer*8 will be left unchanged.
!   --> Optional: activated when _both_  INT_TO_INT4 _and_
!                 REAL_TO_REAL8 CPP-switches are defined.
!
!
! (6) Convert parallelized loops over subdomains (tiles) 
!
!          do tile=0,NSUB_X*NSUB_E-1
!
!     into two nested loops. The outer is loop over parallel threads
!     (to be parallelized), and the inner is over tiles to be
!     processed by the particular thread:
!
!          do trd=0,numthreads-1
!            do tile=forward_sweep
!     or
!          do trd=0,numthreads-1
!            do tile=backward_sweep
!
!     in such a way the inner loops in the subsequent parallel
!     regions are reversed, so that each thread "zig-zags" across
!     the tiles it is processing. Automatically append SHARED/PRIVATE
!     lists with the newly introduced variables.
!   --> Optional: activated by ZIG_ZAG CPP-switch. 
!
! More detailed description for each option is available below.
!
! In the case when after they were modified, lines of the fortran
! program became longer than the prescribed width of 72 characters, 
! they are automatically folded by mpc according to the fortran
! syntax rules. 
!
! Usage: 
!-------
! Similar to CPP
!                  mpc source.file target.file
!             or
!                  mpc source.file > target.file
!             or
!                  mpc < source.file > target.file
!
! mpc is smart enough to recognize how many arguments are given,
! TWO, ONE or NONE and act accordingly. If two arguments are present,
! the first one is input file name, while the second is output, if
! only one is present, it is input; output goes to standard output;
! if NONE is present, mpc expects input from standard input, while
! output goes to standard output. The last option allows mpc to work
! as receiver in pipe with CPP:
!
!                /lib/cpp -P file.F | mpc > file.f          
!
! WARNING: Insofar as manifestations of functional deficiencies are
! agreed upon by any and all concerned parties to be imperceivable,
! and are so stipulated, it is incumbent upon said heretofore
! mentioned parties to exercise the deferment of otherwise pertinent
! maintenance procedures.
!
! And therefore, all manifestations of functional deficiencies, such
! as bugs, evidence of nonrobust behaviour or any other problems of
! any kind shall be reported to Alexander Shchepetkin, (310)-206-9381
! or alex@atmos.ucla.edu.
!
#if defined Linux && !defined __IFC
# define INT_TO_INT4
# define REAL_TO_REAL8
# define DOUBLE_CONST
#else
# undef INT_TO_INT4
# undef REAL_TO_REAL8
# undef DOUBLE_CONST
#endif

#undef REAL_TO_REAL8
#undef KIND_STYLE
#define RUTGERS
#define TRAP_SINGLE_QUOTES
#define ZIG_ZAG
#define ZIG_ZAG_OLD

#ifdef __IFC
# define F77_TO_F95
# define integer INTEGER(kind=4)
#else
# define integer INTEGER*4
#endif

      implicit none
      integer max_length, max_filename
      parameter(max_length=80, max_filename=32)
      character file_in*(max_filename), file_out*(max_filename),
     &          symbol(2*max_length),   buffr(max_length),
     &          scratch*(max_length),   quote,     double_quote,
     &          type, type_none, char_type, int_type, real_type
      parameter (type_none=' ', char_type='1',
     &           int_type='4',  real_type='8')
      integer last_arg, iargc,iin,iout, iocheck, line, istrt,
     &         length, i,j,k,m, is,ie, isft, ks1,ks2,ks3,ks4,ks5
      logical not_end_of_file, directive, dir_switch, lswtch
#ifdef DOUBLE_CONST
      integer ndots, indx(max_length)
#endif
#ifdef ZIG_ZAG_OLD
      logical par_region, lnest
      integer ip1,ip2, is1,is2
      lnest=.false.
      par_region=.false.
#endif
      k=0
      dir_switch=.false.
      quote=char(39)
      double_quote=char(34)

      file_in='                                '
      file_out='                                '
      last_arg=iargc()
      if (last_arg.gt.0) then
        iin=11
        call getarg(1,file_in)
        open(iin, file=file_in, form='formatted', status='old')
      else
        iin=5       !<-- read from standard input
      endif
      if (last_arg.eq.2) then
        iout=12
        call getarg(2,file_out)
        open(iout, file=file_out, form='formatted', status='unknown')
      else
        iout=6        !--> write to standard output
      endif

      not_end_of_file=.true.
      line=0
  1    line=line+1
        length=0                ! Reset the string length and blank
        directive=.false.
        do i=1,max_length       ! out the string itself. Then read
          symbol(i)=' '         ! in a new string.
        enddo
        read(iin,'(80A1)',iostat=iocheck,end=2)
     &                 (symbol(i), i=1,max_length)
        goto 3
   2    not_end_of_file=.false.
        if (symbol(1).eq.'#') goto 1 
   3    if (symbol(1).eq.'C' .or. symbol(1).eq.'c' .or.
     &                            symbol(1).eq.'!') then
          if (symbol(2).eq.'$' .or. symbol(3).eq.'$' .or.
     &        symbol(4).eq.'$' .or. symbol(5).eq.'$') then
            if ((symbol(2).eq.'$').and.
     &        ((symbol(3).eq.'A').or.(symbol(3).eq.'a'))) then
           directive = .false.  ! this is the special !$AGRIF or C$AGRIF directive
           do i=1,max_length
            if (symbol(i).ne.' ') length = i
           enddo
                      write(iout,'(72A1)') (symbol(i),i=1,length)
               goto 1       
           else
            directive=.true.
            endif
          else                  ! if the first symbol indicates that 
            goto 1              ! this line is a fortran comment, but
          endif                 ! a dollar sign is present somewhere
        endif                   ! in positions 2,3,4,5, this line is
                                ! a directive and it should be pro-
        lswtch=.false.          ! cessed further. 
        do i=1,max_length
          if (symbol(i).eq.quote) then
            lswtch=.not.lswtch
          elseif (symbol(i).eq.'!' .and. .not.lswtch) then
            goto 4
          endif
          if (symbol(i).ne.' ') length=i
        enddo
   4    continue
#ifdef TRAP_SINGLE_QUOTES
        if (lswtch) write(iout,'(/6x,A,I4,1x,A/)')
     &    'ERROR: unmatched quote on line', line,'in .i file.'
#endif
        if (length.NE.0) then
        do while (length.gt.1 .and. symbol(length).eq.' ')
          length=length-1
        enddo
        endif
        istrt=7
        do while (istrt.lt.length .and. symbol(istrt).eq.' ')
          istrt=istrt+1 
        enddo
!
! Recognize REAL or real and turn it into REAL*8 or real*8,
! but do not convert REAL*4 or real*4 into real*8.
!
        i=istrt
        type=type_none
#if defined REAL_TO_REAL8 || defined F77_TO_F95
        if (symbol(i  ).eq.'R' .or. symbol(i  ).eq.'r') then
         if (symbol(i+1).eq.'E' .or. symbol(i+1).eq.'e') then
          if (symbol(i+2).eq.'A' .or. symbol(i+2).eq.'a') then
           if (symbol(i+3).eq.'L' .or. symbol(i+3).eq.'l') then
             i=i+3
             type=real_type
           endif
          endif
         endif
        endif
#endif
#if defined INT_TO_INT4 || defined F77_TO_F95
        if (symbol(i  ).eq.'I' .or. symbol(i  ).eq.'i') then
         if (symbol(i+1).eq.'N' .or. symbol(i+1).eq.'n') then
          if (symbol(i+2).eq.'T' .or. symbol(i+2).eq.'t') then
           if (symbol(i+3).eq.'E' .or. symbol(i+3).eq.'e') then
            if (symbol(i+4).eq.'G' .or. symbol(i+4).eq.'g') then
             if (symbol(i+5).eq.'E' .or. symbol(i+5).eq.'e') then
              if (symbol(i+6).eq.'R' .or. symbol(i+6).eq.'r') then
                i=i+6
                type=int_type
              endif
             endif
            endif
           endif
          endif
         endif
        endif
#endif
#ifdef F77_TO_F95
        if (symbol(i  ).eq.'C' .or. symbol(i  ).eq.'c') then
         if (symbol(i+1).eq.'H' .or. symbol(i+1).eq.'h') then
          if (symbol(i+2).eq.'A' .or. symbol(i+2).eq.'a') then
           if (symbol(i+3).eq.'R' .or. symbol(i+3).eq.'r') then
            if (symbol(i+4).eq.'A' .or. symbol(i+4).eq.'a') then
             if (symbol(i+5).eq.'C' .or. symbol(i+5).eq.'c') then
              if (symbol(i+6).eq.'T' .or. symbol(i+6).eq.'t') then
               if (symbol(i+7).eq.'E' .or. symbol(i+7).eq.'e') then
                if (symbol(i+8).eq.'R' .or. symbol(i+8).eq.'r') then
                  i=i+8
                  type=char_type
                endif
               endif
              endif
             endif
            endif                       ! After this moment "i" is
           endif                        ! index of the last character
          endif                         ! of FORTRAN type declaration
         endif                          ! which may be etiter "real",
        endif                           ! "integer", or "character"..
#endif
        if (type.ne.type_none) then     ! ...and j is index of the
          j=i+1
          do while (j.lt.length .and. symbol(j).eq.' ')
            j=j+1
          enddo                         ! first non-blanc symbol 
                                        ! after type declaration.
!
! Once it is eatablished that the line contains an "obsolecent" from
! Intel IFC compiler point of view style of explicit size declaration
! like real*16 or character*128, change it to FORTRAN 95 style,
!
!                    real*16 --> real(16)
!              character*128 --> character(len=128)
!
! which is then accepted by the compiler without warning message.
! To do so, first extract the size itself and save it into "buffr"
! (strip outer braces if any), and set initial value "isft" to be
! the number of deleted characters (negative), which is asterick
! '*' itself and all blank spaces occurring between FORTRAN type
! specifier and size specifier.
!
          if (symbol(j).eq.'*') then
#ifdef F77_TO_F95
            is=j+1
            do while (symbol(is).eq.' ' .and. is.lt.length)
              is=is+1
            enddo
            ie=is
            do while (symbol(ie+1).ne.' ' .and. 
     &                symbol(ie+1).ne.',' .and. ie.lt.length)
              ie=ie+1
            enddo
            isft=i+1-is
            if (symbol(is).eq.'(' .and. symbol(ie).eq.')') then
              is=is+1
              ie=ie-1
              isft=isft-2
            endif
            do k=1,ie-is+1
              buffr(k)=symbol(k+is-1)
            enddo                        ! Finally, modify the line
                                         ! of code: add the number of
            if (type.eq.char_type) then  ! newly inserted characters
              isft=isft+6                ! to "isft", then move the 
              do k=length,ie+1,-1        ! rest of the line to the 
                symbol(k+isft)=symbol(k) ! right in order to make 
              enddo                      ! room for the insertion, 
              length=length+isft         ! adjust the length of the
              symbol(i+1)='('            ! line accordingly, then 
              symbol(i+2)='l'            ! insert the new-style size 
              symbol(i+3)='e'            ! specification. 
              symbol(i+4)='n'
              symbol(i+5)='='
              do k=1,ie-is+1
                symbol(i+k+5)=buffr(k)
              enddo
              symbol(i+ie-is+7)=')'
            elseif (type.eq.int_type .or. type.eq.real_type) then
# ifdef KIND_STYLE
              isft=isft+7
# else
              isft=isft+2
# endif
              do k=length,ie+1,-1
                symbol(k+isft)=symbol(k)
              enddo
              length=length+isft
# ifdef KIND_STYLE
              symbol(i+1)='('
              symbol(i+2)='k'
              symbol(i+3)='i'
              symbol(i+4)='n'
              symbol(i+5)='d'
              symbol(i+6)='='
              do k=1,ie-is+1
                symbol(i+k+6)=buffr(k)
              enddo
              symbol(i+ie-is+8)=')'
# else
              symbol(i+1)='('
              do k=1,ie-is+1
                symbol(i+k+1)=buffr(k)
              enddo
              symbol(i+ie-is+3)=')'
# endif
            endif
#endif
!
! The following code segment deals with declaration of default-size
! real and integer type variables:  if so directed, insert explicit
! specification of the size.  Once again, two versions, F77 and F90
! are provided. The logical condition here is explained as follows:
! at first, reject declaration where the first symbol after type
! declaration is opening bracket (this is possible only if it is
! already a F90/95 style declaration with explicit size specification
! ==> no further action is required). If the symbol is not opening
! bracket, then three possibilities may occur: (i) it is a comma,
! separating type and F90 attribute (like pointer, dimension, etc.,);
! (ii) it is still a blanc character, which means that the line
! contains just one word, say REAL, and contunues on the next line
! (typical situation in RUTGERS code); or (iii) there symbols other
! that '*' (already considered above) and '(', separated by at least
! one blank space: these are etther variable names, or F90 double
! colon :: separater. In the all three cases (i -- iii) the line is
! considered as default type declaration, and should be processed.
!
          elseif (symbol(j).ne.'(' .and. (symbol(j).eq.' ' .or.
     &                      j.gt.i+1 .or. symbol(j).eq.',')) then

#if defined REAL_TO_REAL8 && defined INT_TO_INT4
            if (type.eq.int_type .or. type.eq.real_type) then
#elif defined REAL_TO_REAL8
            if (type.eq.real_type) then
#elif defined INT_TO_INT4
            if (type.eq.int_type) then
#endif
#if defined REAL_TO_REAL8 || defined INT_TO_INT4
# ifdef F77_TO_F95
#  ifdef KIND_STYLE
              isft=8
#  else
              isft=3
#  endif
# else
              isft=2
# endif
              do k=length,j,-1           ! Move the rest of the line
                symbol(k+isft)=symbol(k) ! isft symbols to the right
              enddo                      ! to make room for explicit
              do k=j,j+isft-1            ! declaration of type size  
                symbol(k)=' '            ! and increase lenght of
              enddo                      ! the line accordingly, then
              length=length+isft         ! insert size declaration.
# ifdef F77_TO_F95
#  ifdef KIND_STYLE
              symbol(i+1)='('
              symbol(i+2)='k'
              symbol(i+3)='i'
              symbol(i+4)='n'
              symbol(i+5)='d'
              symbol(i+6)='='
              symbol(i+7)=type
              symbol(i+8)=')'
#  else
              symbol(i+1)='('
              symbol(i+2)=type
              symbol(i+3)=')'
#  endif
# else
              symbol(i+1)='*'
              symbol(i+2)=type
# endif
            endif
#endif
          endif
        endif

#ifdef DOUBLE_CONST
!
! Recognize numerical constants of real type in the text of the
! program and convert them into double precision constants, for
! example, 1. --> 1.D0;  .1 --> .1D0;  9.81 --> 9.81D0;
! .5e-8 --> .5D-8;  1.2e+14 --> 1.2D+14 etc.
!
! Algorithm:
!-----------
! (1) Form list of indices of all dots within the line, except dots
!     which occur within quotations ' ... '. To do so, local logical
!     switch "lswtch" is used as a masking switch, it turns OFF when
!     meeting a quotation mark, when entering a region between quotes
!     and turns back ON again, when exiting.
!
! For each dot character in the list, starting from the last one,
! and moving from the right to the left: 
!
! (2) Scan the characters adjacent from the _left_ to the dot in
!     order to find the first non-blank character which is not a
!     digit (for this purpose digits are ASCII symbols with numbers
!     within the range (48:57) inclusive). This search is terminated
!     if either
!               a non-blank non-digital symbol is found
!     or
!               the 7th position (the starting position for the
!               fixed format fortran statements) has been reached.
!
!     During this search also save "is", which is position of the
!     leftmost digit among digits adjacent to the dot on the left
!     side (position of the dot itself, if there are no adjacent
!     digits on the left of it). 
!
! (3) Check weather this symbol is _NOT_ a letter, that is excluding
!     ASCII characters with numbers within the ranges of numbers
!     (65:90) or (97:122) inclusive, _OR_ weather the 7th position
!     has been reached (in this case it does not matter what the
!     symbol is). If either condition is true, continue processing,
!     otherwise terminate it.  
!
!     It should be noted that in a legal fortran code a constant
!     expression may be preceded by a mathematical operation symbol,
!     bracket, comma, dot, etc; but _NOT_ with a letter. If it
!     happens, the potential candidate for the numerical constant is
!     actually a format statement descriptor, like E16.8, and not a
!     constant of real type. These are rejected at this moment.
!
! (4) Once condition (3) holds, scan the characters adjacent on the
!     right to the dot in order to find the first non-blanc character
!     which is non a digit. This search is limited by the length of
!     the line in the case when no such symbol is found (if it is the
!     case, it is then interpreted as a blanc symbol). During this
!     search also save "ie", which is position of the last digit
!     among digits adjacent to the dot on the right (it is set to the
!     position of the dot itself, if no adjacent digits are present).
!     Along with the previously saved "is" [see (2) above], "ie"
!     forms a logical switch "is.lt.ie", indicating that there is at
!     least one digit adjacent to the dot, so further processing is
!     required.
!
! (5) Once this symbol is found, if any, or the search was terminated
!     (in this case index m is equal to lenght+1, so that that symbol
!     is blank, or ! (fortran comment), this symbol may be
!     ether
!          'e', 'E', 'd' or 'D', so that it likely belongs to the
!          constant itself. In this case scan to the right, to verify
!          that this symbol is '+', '-' or a digit. If so, the
!          expression is a floating point real type constant to be
!          converted into double precision.
!     or
!          an underscore symbol '_', which may be associated with a
!          FORTRAN 90/95-style real-valued constant with explict type
!          (kind), e.g., 1.2_4 (in which case no it is accepted "as
!          is" and no further action is taken); or a RUTGERS-style
!          constant -- e.g., 1.0_r8 or, 1.23_e8+18 -- in this case it
!          is converted into standard double presision syntax -->
!          1.0D0 and 1.23D+18. 
!     or 
!          any other character. In this case verify that it is not a
!          letter. (In a legal fortran code a constant expression may
!          be followed by a mathematical/logical operation, bracket,
!          coma, etc, but NEVER a letter. If so, the expression is a
!          fixed point real valued constant to be converted into
!          double precision. Move the rest of the line two positions
!          to the right to make room and paste "D0" immediately after
!          its last digit.
!
! NOTE: if there are more than one real constant within the line of
! the code, the order of processing is from the right to the left.
! This is done because in the case when 'D0' is pasted to the
! constant as in the step (5), the second case, the tail of the
! line is shifted to the right. Processing them in the forward order 
! will also cause shift of the dots to be processed later. So that 
! the indices indx(j+1:ndots) are no longer consistent with the
! actual position of the dots, if the dot indx(j) was found to be  
! a fixed point real type constant as defined in (5), second case.
!
! Limitations:  1. Real-valued constants should not be continued
!-------------     to the next line of fortran code.
!
        ndots=0                                          ! Step (1)
        lswtch=.true.
        do i=7,length
          if (symbol(i).eq.quote) then
            lswtch=.not.lswtch
          elseif (symbol(i).eq.'.' .and. lswtch) then
            ndots=ndots+1
            indx(ndots)=i
          endif
        enddo
        do j=ndots,1,-1 ! <-- REVERSED
          m=indx(j)                                      ! Step (2)
          is=indx(j)
          lswtch=.true.
          do while (lswtch .and. m.gt.7)
            m=m-1
            k=ichar(symbol(m))
            if (k.ge.48 .and. k.le.57) then
              is=m
            elseif (symbol(m).ne.' ') then
              lswtch=.false.
            endif
          enddo

          if (lswtch .or. k.lt.65 .or. (k.gt.90. and.    ! Cond.(3)
     &                    k.lt.97) .or. k.gt.122) then
            m=indx(j)
            ie=indx(j)                                   ! Step (4)
            lswtch=.true.
            do while (lswtch .and. m.lt.length)
              m=m+1
              k=ichar(symbol(m))
              if (k.ge.48 .and. k.le.57) then 
                ie=m
              elseif (symbol(m).ne.' ') then
                lswtch=.false.
              endif
            enddo
            if (lswtch) m=m+1

            if (is.lt.ie) then                           ! Step (5)
              k=ichar(symbol(m))
              if (symbol(m).eq.'e' .or. symbol(m).eq.'E' .or.
     &            symbol(m).eq.'d' .or. symbol(m).eq.'D') then
                i=m+1
                do while (symbol(i).eq.' ' .and. i.lt.length)
                  i=i+1
                enddo
                k=ichar(symbol(i)) 
                if (k.eq.43 .or. k.eq.45 .or.
     &              (k.ge.48 .and. k.le.57)) symbol(m)='D' 
              elseif (symbol(m).eq.'_') then
!
! Rutgers compatibility mode: the following code segment searches
! for suffices _r8 and _e8 attached to real valued constants and
! conversts it into standard fortran double precision constants,
! e.g., 1.0_r8 --> 1.0D0; 2.4_e8+3 --> 2.4D+3.
!
! NOTE: there is still possibility for FORTRAN 90/95 real valued
! constant format with explicit type, such that 1.0_4 or 2.3_8.
! These are left unchanged.
!
# ifdef RUTGERS
                if (symbol(m+2) .eq. '8') then
                  if (symbol(m+1) .eq. 'r') then
                    symbol(m  )='D'
                    symbol(m+1)='0'
                    length=length-1
                    do i=m+2,length
                      symbol(i)=symbol(i+1)
                    enddo
                  elseif (symbol(m+1) .eq. 'e') then
                    symbol(m)='D'
                    length=length-2
                    do i=m+1,length
                      symbol(i)=symbol(i+2)
                    enddo
                  endif
                endif
# endif
              elseif (k.lt.65 .or. (k.gt.90. and.
     &                k.lt.97) .or. k.gt.122) then
                do while (symbol(m-1).eq.' ')
                  m=m-1
                enddo
                do i=length,m,-1
                  symbol(i+2)=symbol(i)
                enddo
                symbol(m)='D'
                symbol(m+1)='0'
                length=length+2
              endif
            endif
          endif
        enddo
#endif


#ifdef ZIG_ZAG
!
! Recognize and transform parallel loops:
!---------- --- --------- -------- ------
! The following code segment rearranges direction of consecutive
! 'do tile=my_first,my_last' loops into zig-zag order. This version
! is designed to work in conjuction with the OpenMP version of main.F
! (or any other code), so that no parallel directive is required in
! front of the loop in order for the rearrangement to occur: it is
! simpy triggered by the occurrence of 'do tile=my_***' with possible
! blank spaces between 'tile', '=' and 'my_***', where 'my_***' may
! be anything like handcoded 'my_first,my_last' or 'my_tile_range'. 
!
! Compared to the straightforward sequence, this measure eliminates
! secondary cache misses, because after passing a synchronization
! point, each thread proceeds with the same tile it was processing
! just before the synchronization point. It also reduces the
! probability of mutual cache_line invalidation (by factor of two)
! in a multiprocessor machine, if multiprocessing is allowed for the
! subdomains adjacent in the XI direction. This is because after a
! subdomain has been processed by a processor and a synchronization
! point has been reached, all cache_lines which are going across the
! subdivision partitioning are coherent with the cache of _that_
! processor (and invalid with respect to the one working on the
! adjacent subdomain). And it is _that_ processor (and not the
! adjacent one), who proceeds with this subdomain, and therefore
! enjoys all its cache_lines coherent with its cache at this moment.
!
        i=istrt
        if (symbol(i).eq.'d' .and. symbol(i+1).eq.'o') then
          i=i+2
          do while (symbol(i).eq.' ' .and. i.lt.length)
            i=i+1
          enddo
          if (symbol(i  ).eq.'t' .and. symbol(i+1).eq.'i' .and.
     &        symbol(i+2).eq.'l' .and. symbol(i+3).eq.'e') then
            i=i+4
            do while (symbol(i).eq.' ' .and. i.lt.length)
              i=i+1
            enddo
            if (symbol(i).eq.'=') then
              i=i+1
              do while (symbol(i).eq.' ' .and. i.lt.length)
                i=i+1
              enddo
     
              if (symbol(i).eq.'m' .and. symbol(i+1).eq.'y' .and.
     &                                   symbol(i+2).eq.'_') then
                dir_switch=.not.dir_switch
                if (dir_switch) then
                  scratch='my_first,my_last,+1'
                else
                  scratch='my_last,my_first,-1'
                endif
                do j=1,19
                  symbol(i+j-1)=scratch(j:j)
                enddo
                length=i+18
              endif
            endif
          endif
        endif
#endif
#ifdef ZIG_ZAG_OLD
!
! Similarly as above, but designed to work for version of main.F,
! where parallel directives are inserted in front of each parallel 
! loop over subdomains "tiles", i.e., loops like
!
!      C$DOACROSS LOCAL (tile)
!          do tile=0,NSUB_X*NSUB_E-1
!
! are converted into two nested loops. The outer is loop over
! parallel threads (to be parallelized), and the inner is over
! tiles to be processed by the particular thread:
!
!          do trd=0,numthreads-1
!            do tile=forward_sweep
! or 
!          do trd=0,numthreads-1
!            do tile=backward_sweep
!
! in such a way the inner loops in the subsequent parallel regions
! are reversed, so that each thread zig-zags across the tiles it is
! processing.
!                             ! If the line is a parallel directive
        if (directive) then   ! containing attribute LOCAL(..) or 
          ip1=0               ! PRIVATE(..), and perhaps, SHARE(..),
          ip2=0               ! identify indices of the first and
          is1=0               ! the last symbol inside each bracket,
          is2=0               ! which are then used below to append
          i=1                                        ! either list. 
          do while (i.lt.length)
            if (symbol(i  ).eq.'L' .or. symbol(i  ).eq.'l') then
              if (symbol(i+1).eq.'O' .or. symbol(i+1).eq.'o') then
               if (symbol(i+2).eq.'C' .or. symbol(i+2).eq.'c') then
                if (symbol(i+3).eq.'A' .or. symbol(i+3).eq.'a') then
                 if (symbol(i+4).eq.'L' .or. symbol(i+4).eq.'l') then
                   i=i+5
                   do while (symbol(i).ne.'(' .and. i.lt.length)
                     i=i+1
                   enddo
                   if (symbol(i).eq.'(') then
                     ip1=i+1
                     do while (symbol(i).ne.')' .and. i.lt.length)
                       i=i+1
                     enddo
                     if (symbol(i).eq.')') ip2=i-1
                   endif
                 endif
                endif
               endif
              endif
            elseif(symbol(i).eq.'P' .or. symbol(i  ).eq.'p') then
              if (symbol(i+1).eq.'R' .or. symbol(i+1).eq.'r') then
               if (symbol(i+2).eq.'I' .or. symbol(i+2).eq.'i') then
                if (symbol(i+3).eq.'V' .or. symbol(i+3).eq.'v') then
                 if (symbol(i+4).eq.'A' .or. symbol(i+4).eq.'a') then
                  if (symbol(i+5).eq.'T'.or. symbol(i+5).eq.'t') then
                   if (symbol(i+6).eq.'E'.or.symbol(i+6).eq.'e') then
                     i=i+5
                     do while (symbol(i).ne.'(' .and. i.lt.length)
                       i=i+1
                     enddo
                     if (symbol(i).eq.'(') then
                       ip1=i+1
                       do while (symbol(i).ne.')' .and. i.lt.length)
                         i=i+1
                       enddo
                       if (symbol(i).eq.')') ip2=i-1
                     endif
                   endif
                  endif
                 endif
                endif
               endif
              endif
            elseif(symbol(i).eq.'S' .or. symbol(i  ).eq.'s') then
              if (symbol(i+1) .eq.'H' .or. symbol(i+1).eq.'h') then
               if (symbol(i+2) .eq.'A' .or. symbol(i+2).eq.'a') then
                if (symbol(i+3) .eq.'R'.or. symbol(i+3).eq.'r') then
                 if (symbol(i+4).eq.'E'.or. symbol(i+4).eq.'e') then
                   i=i+5
                   do while (symbol(i).ne.'(' .and. i.lt.length)
                     i=i+1
                   enddo
                   if (symbol(i).eq.'(') then
                     is1=i+1
                     do while (symbol(i).ne.')' .and. i.lt.length)
                       i=i+1
                     enddo
                     if (symbol(i).eq.')') is2=i-1
                   endif
                 endif    ! Skip to the beginning of the next word
                endif     ! for further checking. NOTE that 'PRIVATE'
               endif      ! in context of 'DO PRIVATE' is recognized 
              endif       ! (as it should be), but 'PRIVATE' within 
            endif         ! 'THREADPRIVATE' is disregarded because 
                          !  it not a complete word.
            i=i+1
            do while (symbol(i).ne.' ' .and. symbol(i).ne.',')
              i=i+1
            enddo
            do while (symbol(i).eq.' ' .and. i.lt.length)
              i=i+1
            enddo
          enddo

          if (ip1.gt.0 .and. ip2.eq.0) write(iout,'(A)')
     &      '### ERROR: No closing bracket in local/private list.' 
          if (is1.gt.0 .and. is2.eq.0) write(iout,'(A)')
     &             '### ERROR: No closing bracket in shared list.'
!
! It should be noted that a valid DOACROSS / DO PARALEL /DO ALL
! directive must have at least one valiable in its LOCAL/PRIVATE
! list to privatize the index of do-loop which follows it. Hence, 
! the presence of non-empty local list triggers further processing 
! in the following code segment: paste additional variables "trd"
! and subs into private list and "numthreads" into shared, in order
! to allow loop rearrangement. There are three possibilities here:
! SHARE attribute goes beforeLOCAL; vise versa; and and SHARE
! attribute is absent.
!                                           ! Save the whole string 
          if (ip1.gt.0 .and. ip2.gt.0) then ! into buffer before
            do i=1,length                   ! start messing with it.
              buffr(i)=symbol(i)
            enddo
            if (ip1.gt.is2 .and. is2.ge.is1 .and. is1.gt.0) then
              scratch = 'numthreads, '
              do i=1,12
                symbol(is1+i-1)=scratch(i:i)
              enddo
              do i=is1,ip1-1
                symbol(i+12)=buffr(i)
              enddo
              scratch = 'trd,subs, '
              do i=1,10
                symbol(ip1+i+11)=scratch(i:i)
              enddo
              do i=ip1,length
                symbol(i+22)=buffr(i)
              enddo
              length=length+22
            else
              scratch = 'trd,subs, '
              do i=1,10
                symbol(ip1+i-1)=scratch(i:i)
              enddo
              if (is1.gt.0 .and. is2.gt.0) then
                do i=ip1,is1-1
                  symbol(i+10)=buffr(i)
                enddo
                scratch = 'numthreads, '
                do i=1,12
                  symbol(is1+i+9)=scratch(i:i)
                enddo
                do i=is1,length
                  symbol(i+22)=buffr(i) 
                enddo
                length=length+22
              else
                do i=ip1,ip2+1
                  symbol(i+10)=buffr(i)
                enddo
                scratch = ', SHARED(numthreads)'
                do i=1,20
                  symbol(ip2+i+11)=scratch(i:i)
                enddo
                do i=ip2+2,length
                  symbol(i+30)=buffr(i)
                enddo
                length=length+30
              endif
            endif
!
! Write out the modified compiler directive, which is obviously
! longer than the original one, check its length and if it exceeds
! 72 symbols, split it into two. For the estetic purposes the split
! goes along a natural divider symbol, such as a intentionally
! placed blank character or a comma.
!
            if (length.le.72) then
              write(iout,'(72A1)') (symbol(i),i=1,length)
            else
              ks1=72
              do while (symbol(ks1).ne.' ' .and. ks1.gt.0)
                ks1=ks1-1
              enddo
              ks2=72
              do while (symbol(ks2).ne.',' .and. ks2.gt.0)
                ks2=ks2-1
              enddo
              if (ks1.gt.54) then
                k=ks1
              elseif (ks2.gt.6) then
                k=ks2
              else 
                write(iout,*) 'MPC ERROR: Cannot split directive.'
              endif
              write(iout,'(72A1)') (symbol(i),i=1,k)
# ifdef CRAY 
              m=6
# else
              m=3
# endif
              symbol(m)='&'
              do i=m+1,k
                symbol(i)=' '
              enddo
              m=length-k
              do i=k+1,length
                symbol(i-m)=symbol(i)
              enddo
              write(iout,'(72A1)') (symbol(i),i=1,k)
            endif
            par_region=.true.
            goto 1
          endif
        endif
!
! Once a parallel region is detected and the compiler directive above
! was modified, transform the parallel loop over subdomains (tiles)
! into a set of nested loops over threads (outer loop) and subdomains
! within the work zone of each thread (inner loop), in such a way
! that the direction of the inner loop is always reversed with respect
! to the direction of similar loop in the previous parallel region
! (zig-zag sequence).  
!
        if (par_region) then
          is=7
          do while (symbol(is).eq.' ' .and. is.lt.length)
            is=is+1
          enddo
          if (symbol(is).eq.'d' .and. symbol(is+1).eq.'o') then
            is=is+2
            do while (symbol(is).eq.' ' .and. is.lt.length)
              is=is+1
            enddo
            if (symbol(is ).eq.'t' .and. symbol(is+1).eq.'i' .and.
     &          symbol(is+2).eq.'l' .and. symbol(is+3).eq.'e' .and.
     &                                   symbol(is+4).eq.'=') then
              scratch='0,NSUB_X*NSUB_E-1'
              lswtch=.true.
              do i=1,17 
                if (symbol(is+4+i).ne.scratch(i:i)) lswtch=.false.
              enddo
              if (lswtch) then
                lnest=.true.
                write(iout,'(7x,A/8x,A)') 'do trd=0,numthreads-1',
     &                             'subs=NSUB_X*NSUB_E/numthreads'
                dir_switch=.not.dir_switch
                if (dir_switch) then
                  write(iout,'(9x,A)')
     &               'do tile=subs*trd,subs*(trd+1)-1,+1'
                else
                  write(iout,'(9x,A)')
     &               'do tile=subs*(trd+1)-1,subs*trd,-1'
                endif
                goto 1
              endif
            endif
          elseif (lnest) then
            if (symbol(is).eq.'e' .and. symbol(is+1).eq.'n' .and.
     &          symbol(is+2).eq.'d' .and. symbol(is+3).eq.'d' .and.
     &                                     symbol(is+4).eq.'o') then
              lnest=.false.
              par_region=.false.
              write(iout,'(9x,A/7x,A)') 'enddo', 'enddo'
              goto 1
            endif
          endif
        endif
#endif
!
! Write the modified line of code into the output file.
!------ --- -------- ---- -- ---- ---- --- ------ -----
! Because its length after modification may exceed the standard of
! 72 symbols, it may be necessary to split the line into two. In this
! case several attempts are made to find a good (from estetic point
! of view) splitting point. First attempt is made by searching for
! the first blank symbol starting from position 72 and moving to the
! left; if such is not found, then the search starts from the
! beginning and attempt is made to split along a comma; if this is
! not successful, an assignment operator is being searched; then
! a mathematical operation. 
!
        if (length.gt.6 .and. length.le.72) then
           write(iout,'(72A1)') (symbol(i),i=1,length)
        elseif (length.gt.72) then

          ks1=0        ! Find appropriate places in the line where it
          ks2=0        ! may be split into two. This is just a purely
          ks3=0        ! estetic matter: line split may be done if
          ks4=0        ! there is a natural divider, such as a blank
          ks5=0        ! haracter in the middle; comma or a symbol of
                       ! of mathematical operation.
          do k=7,72
            if (symbol(k).eq.' ') then
              ks1=k
            elseif (symbol(k).eq.',') then
              ks2=k
            elseif (symbol(k).eq.'=') then
              ks3=k
            elseif (symbol(k).eq.'/') then
              ks4=k 
            elseif (symbol(k).eq.'+' .or.
     &              symbol(k).eq.'-'  .or.
     &              symbol(k).ne.'*') then
              ks5=k
            endif
          enddo
            
          if (length-ks1.gt.66) ks1=0
          if (length-ks2.gt.66) ks2=0
          if (length-ks3.gt.66) ks3=0
          if (length-ks4.gt.66) ks4=0
          if (length-ks5.gt.66) ks5=0
!
! Making final decision about the line split: there is nothing
! special here, it is just a matter of estetics to decide which of
! the possible breaking points (if more than one are available) is
! the most appropriate; the logical sequence below is designed as
! hierarchy of preferences. 
!
          if (ks1.gt.34) then
            k=ks1
          elseif (ks4.gt.6) then
            k=ks4-1
          elseif (ks2.gt.54) then
            k=ks2
          elseif (ks3.gt.60) then
            k=ks3
          elseif (ks5.gt.6) then
            k=ks5-1
          else
            write(iout,*) 'MPC ERROR: Cannot split line'
          endif
!
! Write out the line. First write the first part of the line; then
! create a continuation line resetting the starting symbols to blank,
! and them moving the tail to the left in such a way that its end
! will be in the position 72. Also put a continuation character into
! position 6. 
!
          write(iout,'(72A1)') (symbol(i),i=1,k) 
          do i=1,k
            symbol(i)=' '
          enddo
          symbol(6)='&'
          m=length-72
          do i=k+1,length
            symbol(i-m)=symbol(i)
          enddo
          write(iout,'(72A1)') (symbol(i),i=1,72)
        endif
       if (not_end_of_file) go to 1
      if (iout.gt.6) close(iout)
      if (iin.gt.5) close(iin)
      stop
      end
       
