! $Id: insert_node.F 1458 2014-02-03 15:01:25Z gcambon $
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
#ifdef TIME_INDEX
# ifdef EXTRACT_INDEX
      subroutine extract_time_index (name, lstr, indx, ierr)
# else
      subroutine   insert_time_index (name, lstr, indx, ierr)
# endif
#else
# include "cppdefs.h"
 
      subroutine insert_node (name, lstr, node, nnodes, ierr)
#endif
!
! Insert MPI-node number "node" into character string "name" and
! adjust the lenght of the string "lstr" to accommodate the inserted
! number. The node number is coded using as many digits as necessary
! to accommodate numbers from 0 to nnodes-1, which is determined
! internally in this subprogram. The number is placed just before
! the suffix, if suffix is present, e.g.: '.nc', or in the end of
! the name, if it does not have suffix.
!
! input:  name    character string
!         lstr    length of the string
!       indx/node Time Index/MPI-node number (MPI-rank, within
!                    the from 0 to nnodes-1) to be inserted into
!                    the string.
!         nnodes  total number of MPI nodes, the same as parameter
!                    NNODES in "param.h", but since this routine is
!                    also used in "partit.F", it is passed as an
!                    argument, rather than parameter in include file;
!                    in this code nnodes is used only to determine
!                    how many digits are needed to accomodate all
!                    possible MPI node numbers.
!         ierr    error counter, NORMALLY MUST BE TO 0 BY CALLER;
!
! output: name    modified string with node number inserted.
!         lstr    length of the modified string
!         ierr    the same as at entry, if no error occurs,
!                        otherwise increased by one.
!
      implicit none
      character*(*) name, sffx*16
      integer lstr, ierr, i,j,k, lsffx, digits, power, ndots, idot(3)
#ifdef TIME_INDEX
     &                                        , indx
#else
     &                                        , node,  nnodes
#endif
      parameter (digits=4)
      logical leading_dots
      ndots=0                            ! Determine how many dots
      leading_dots=.true.                ! are present inthe string.
      do i=1,lstr                        ! Do not allow more than 3
        if (name(i:i).eq.'.') then       ! dots: signal about error,
         if (.not.leading_dots) then     ! if it occurs, however, do
            if (ndots.lt.3) then         ! not count leading dots,
              ndots=ndots+1              ! so that that names like
              idot(ndots)=i              ! "../grid.00.nc" are OK.
            else
              write(*,'(/1x,4A/)') 'INSERT_NODE/INDEX ERROR: too ',
     &             'many dots in file name ''', name(1:lstr), '''.'
              ierr=ierr+1
              return
            endif
          endif
        else
          leading_dots=.false.
        endif
      enddo                              ! Determine whether the
                                         ! name has suffix: the last
      lsffx=0                            ! segment of the name is
      if (ndots.gt.0) then               ! considered to be a suffix,
        i=idot(ndots)+1                  ! if it has least one
   1    k=ichar(name(i:i))-48            ! character, which is not
        if ((k.lt.0 .or. k.gt.9) .and.   ! a digit or UNIX wildcard.
     &    name(i:i).ne.'*' .and. name(i:i).ne.'?') then
          lsffx=lstr-idot(ndots)+1
        elseif (i.lt.lstr) then          ! If suffix is present, it
          i=i+1                          ! will be saved and added
          goto 1                         ! to the name later.
        endif
      endif
 
      do j=1,ndots-1                     ! Inspect segments between
        i=idot(j)+1                      ! the dots. These segments
   2    k=ichar(name(i:i))-48            ! may contain only digits
        if (k.lt.0 .or. k.gt.9) then
          if (name(i:i).ne.'*' .and. name(i:i).ne. '?') then
            write(*,'(/1x,2A/20x,3A/)') 'INSERT_NODE/INDEX ERROR: ',
     &                    'a non-digital character found in index ',
     &                    'segment of name ''',  name(1:lstr), '''.'
            ierr=ierr+1
          endif                          ! or UNIX wild cards '*'
        elseif (i.lt.idot(j+1)-1) then   ! and '?'. These segments
          i=i+1                          ! are used to store Time
          goto 2                         ! Index or MPI-node number.
        endif                            ! Set error signal, if
      enddo                              ! an illegal symbol found
      if (ierr.ne.0) return              ! in this area.
 
 
#ifdef EXTRACT_INDEX
      if (ndots.eq.1 .and. lsffx.eq.0) then
        i=idot(1)+1
        j=lstr                           ! Read digital segment in
      elseif (ndots.gt.1) then           ! the file name which
        i=idot(1)+1                      ! contains time index (that 
        j=idot(2)-1                      ! is the leftmost segment
      else                               ! which has length of at
        i=0                              ! least equal to the setting
        j=0                              ! of parameter "digit". 
      endif
      indx=0
      if (j-i+1.ge.digits) then
        do k=i,j
          indx=10*indx + ichar(name(k:k))-48
        enddo
      endif
#else
!
! Determine where to put Time Index and/or MPI-node number (rank).
! Since the string name may or may not contain digital segments,
! three possibilities exist:
!
! (1) there are no digital segments: either there are no dots
!     (hence no suffix), or there is only one dot (which separates
!     the suffix from the root name). In this case an ne digital
!     segment is created for either Time Index or MPI-node (rank).
!
! (2) only ONE digital segment exist. In this case it has to be
!     determined whether it is to be used as Time Index or MPI-node.
!     The determination is made based upon the length of the segment:
!
!     --> if the segment length is greater or equal than parameter
!     "digits" specified above, then it will be interpreted as the
!     place to store time index. (if MPI-node needs to be inserted,
!     a new digital segment adjacentl to the right from the existing
!     one will be created in this case.)
!
!     --> if, in the other hand, the segment length is smaller than
!     "digits", then it will be interpreted as the MPI-node number.
!     A a new digital segment adjacentl to the LEFT from the existing
!     one will be created to place Time Index.
!
! (3) There are already TWO digital segments in string "name". The
!     left one will be used for time index, the right for MPI-node.
!
! In the code segment below, "i" is the starting dot of digital
! segment to be inserted,  while "j" is the starting dot of the tail
! of the string "name", i.e. name(j:lstr) contains either suffix of
! the string "name" (including starting dot), or the right digital
! segment (if there is one, and there is no suffix); or both segment
! and suffix.
!
      if (ndots.eq.0) then
        i=lstr+1
        j=lstr+1
        name(i:i)='.'
      else
# ifdef TIME_INDEX
        i=idot(1)
        if (ndots.eq.1) then
          if (lsffx.gt.0 .or. lstr-idot(1).lt.digits) then
            j=idot(1)
          else
            j=lstr+1
          endif
        elseif (ndots.eq.2 .and. idot(2)-idot(1).le.digits) then
          j=idot(1)
        else
          j=idot(2)
        endif
# else
        if (ndots.eq.1) then
          i=idot(1)
        elseif (ndots.eq.2) then
          if (idot(2)-idot(1).le.digits) then
            i=idot(1)
          else
            i=idot(2)
          endif
        else
          i=idot(ndots-1)
        endif
        if (lsffx.gt.0) then
          j=idot(ndots)
        else
          j=lstr+1
        endif
# endif
      endif
      lsffx=lstr+1-j
      if (lsffx.gt.0) sffx(1:lsffx)=name(j:lstr)
 
!
! Load Time Index or MPI-node (rank) into temporal variable "k".
! This variable will be written into digital segment. Also specify
! maximum allowed number, which sets the number of digits in the
! segment.
!
 
# ifdef TIME_INDEX
      k=indx
      power=10**digits
# else
      k=node
      power=10                           ! Determine how many digits
   3  if (nnodes.gt.power) then          ! are needed to accommodate
        power=10*power                   ! the largest possible MPI-
        goto 3                           ! node number (rank).
      endif
      if (power .ge. 10**digits) then
        write(*,'(/1x,2A/6x,2A/6x,A/)')  'INSERT_NODE/INDEX ERROR: ',
     &   'Possible ambiguity between MPI-node segment',    'length ',
     &   'and time index segment length. To fix: increase parameter',
     &   '''digits'' in file "insert_node.F" and recompile.'
        ierr=ierr+1
        return
      endif
# endif
   4  power=power/10
       i=i+1                             ! Insert time index or
       j=k/power                         ! MPI node number (rank)
       name(i:i)=char(48+j)              ! into the string, then
       k=k-j*power                       ! attach suffix, if any.
       if (power.gt.1) goto 4
      if (lsffx.gt.0) name(i+1:i+lsffx)=sffx(1:lsffx)
      lstr=i+lsffx
#endif  /* EXTRACT_INDEX */
      return
      end
 
c--#define TEST_INSERT
 
#ifndef TIME_INDEX
# define TIME_INDEX
# include "insert_node.F"
#else
# ifndef EXTRACT_INDEX
#  define EXTRACT_INDEX
#  include "insert_node.F"
# else
#  ifdef TEST_INSERT
      implicit none                      ! Testing program
      character*64 hisname               ! for insert_node
      integer lstr, i, nnodes, ierr
 
      hisname='his_00_*/.*'
      hisname='../dir/root_name.000.nc'
      hisname='../dir/root_name.*.*.nc'
      hisname='../dir/root_name.0'
 
      hisname='../dir/root_name.123.3459'
 
      nnodes=10
 
      lstr=1
      do while (lstr.lt.64 .and. hisname(lstr:lstr).ne.' ')
        lstr=lstr+1
      enddo
      if (hisname(lstr:lstr).eq.' ') lstr=lstr-1 
 
#   ifndef EXTRACT_INDEX
      do i=0,nnodes-1
        call insert_node (hisname, lstr, i, nnodes, ierr)
c        call insert_time_index (hisname, lstr, i, ierr)
        write(*,'(I4,1x,A,1x,I2)') i, hisname(1:lstr), lstr
      enddo
#   else
      call extract_time_index (hisname, lstr, i, ierr)
      write(*,'(1x,A,I8)') hisname(1:lstr), i
#   endif
      stop
      end
#  endif
# endif
#endif
 
 
 
