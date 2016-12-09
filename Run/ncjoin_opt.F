#ifndef EXCLUDE_MAIN
# define EXCLUDE_MAIN

      program ncjoin


! Generic netCDF assembly tool: reads partitioned netCDF files and
! assembles them into a file corresponding to the whole physical grid.
! This operation is inverse to partit.
!
! This code uses self-inclusion in order to generate several similar
! subroutines from the same source code. CPP-macro EXCLUDE_MAIN makes
! main program appear only once after CPP is executed: it is assumed 
! to be undefined initially.  All other CPP-switches below are user
! configurable. 
!
! CPP-switch "FAST_MODE" activates mode in which partitioned arrays
! are assembled into a big array in operating memory, and the whole
! data is written into the disk as a one long record. If not defined,
! the code relies on the netCDF capability to write rectangular
! sub-arrays via nf_put_vara_XXX routines.  Because in hardware terms
! this results in writing multiple small records (with record length
! equal to the first fortran dimension of sub-array), it is
! considerably slower.
! 
! CPP-switch "KEEP_CLOSED" forces regime in which to keep no more
! than one partitioned file for MPI-nodes 1:nnnodes-1 be opened at
! any given time (file belonging to MPI-node=0 is kept permanently
! open, so does the target file). This regime is needed to avoid
! netCDF internal memory limitation associated with simultaneous
! opening too many files.



#define FAST_MODE
#define KEEP_CLOSED
c--#define VERBOSE
#define AUTORENICE


      implicit none
#include "netcdf.inc"
      integer maxdims, maxvars, maxnodes
      parameter (maxdims=32, maxvars=64, maxnodes=2048)
      character ncname0*64, ncname(0:maxnodes-1)*64, string*80
      character root*64, root_bak*64, sffx*8, sffx_bak*8
      character dimname(maxdims)*32, varname(maxvars)*32

      logical complete, digit, var_mask
      integer nargs,    nnodes,  id_xi_rho,  code_size,
     &        arg,      node,    id_xi_u,    code_size_bak,    
     &        iargc,  id_eta_rho, lbak,
     &        ierr,   id_eta_v,   unlimdimid, 
     &        ndims, nvars,    ngatts,  tsize,      varatts, i,j,k,
     &        is,ie, ntest, nctarg, lvar,  size, size1, rec,  lstr,
     &        size_XI, size_ETA, size_S, lenstr

      integer XI_rho, ETA_rho

      integer xi_start(0:maxnodes-1), eta_start(0:maxnodes-1)
      logical western_edge(0:maxnodes-1), eastern_edge(0:maxnodes-1),
     &       southern_edge(0:maxnodes-1), northern_edge(0:maxnodes-1)

      integer ncid(0:maxnodes-1),  varid(maxvars),  vartype(maxvars),
     &        vardims(maxvars),    dimid(maxdims),  ldim(maxdims),
     &        ibuff(maxdims),    dimids(maxdims,maxvars),
     &        dimsize(maxdims,0:maxnodes),  start(maxdims),
     &        count(maxdims),  start1(maxdims),
     &        part_type(maxvars)

      logical series(maxvars)
      common /partit_int/ xi_start, eta_start,
     &                    ncid,  varid,  vartype, vardims,   dimid,
     &                    ldim,  ibuff,  dimids,  dimsize,   start,
     &                    count, start1,   part_type, series,
     &   western_edge, eastern_edge, southern_edge, northern_edge


#ifdef FAST_MODE
      integer count1(maxdims)
      common /partit_int1/ count1
#endif

      integer part_xi, part_eta, part_2d 
      parameter (part_XI=1, part_ETA=2, part_2D=3)

      integer max_buff_size, alloc_buff_size
      real*8, allocatable, dimension(:) :: buff
#ifdef FAST_MODE
      integer max_bfr_out, alloc_bfr_out
      real*8, allocatable, dimension(:) :: bfr_out
#endif


#ifdef AUTORENICE
      integer getpid, pid            ! Sometimes it makes sense to
      character*32 cmd               ! run ncjoin in the background
      pid=getpid()                   ! mode with lowered priority to
      write(cmd,'(I8)') pid          ! not interfere with running MPI
      lstr=lenstr(cmd)               ! job. This code segment catches
      cmd(11:lstr+10)=cmd(1:lstr)    ! pid of its own process and
      lstr=lstr+10                   ! executes re-nice command.
      cmd(1:10)='renice 19 '
      write(*,'(/3A/)') 'Autorenice: executing ''',cmd(1:lstr),'''.'
      call system(cmd(1:lstr))
#endif
                                     ! initialize sizes of buffer 
      max_buff_size=0                ! arrays to be allocated. Here
      alloc_buff_size=0              ! "max_*" means needed size, and
#ifdef FAST_MODE
      max_bfr_out=0                  ! "alloc_*" is size of actually
      alloc_bfr_out=0                ! allocated array. 
#endif

      nargs=iargc()
      arg=0

!
! Extract a set of files which cover the whole physical grid.
!
  1   nnodes=-1                  ! Reset variables which define
      do node=0,maxnodes-1       ! complete set of partitioned files.
        ncid(node)=-1            ! These variables will receave 
        xi_start(node)=-1        ! meaningful values by extracting
        eta_start(node)=-1       ! from netCDF files and they are
      enddo                      ! needed to verify completeness
      root_bak(1:1)=' '          ! of the set.
      sffx_bak(1:1)=' '
      code_size_bak=-1

 
  2    arg=arg+1
        call getarg(arg,ncname0)
        lstr=lenstr(ncname0)

        if (ntest.gt.0) then
          ierr=nf_close(ntest)
          ntest=-1
        endif
        ierr=nf_open (ncname0(1:lstr), nf_nowrite, ntest)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_att (ntest, nf_global, 
     &                  'partition_ucla', i, lvar)
          if (ierr .eq. nf_noerr) then
            if (i.eq.nf_int .and. lvar.eq.4) then
              ierr=nf_get_att_int (ntest,nf_global,
     &                  'partition_ucla',ibuff)
              if (ierr .eq. nf_noerr) then
                ierr=nf_close(ntest)
                ntest=-1
#define ntest illegal

                if (ibuff(2).gt.maxnodes) then
                  write(*,'(/4x,A,I4,1x,A/11x,2A/11x,A,I4,A/)')
     &                  '### ERROR: Number of requested MPI-nodes =',
     &                  nnodes, 'exceeds maximum allowed.', 'Adjust',
     &                  'parameter "maxnodes" in file "ncjoin.F" ',
     &                  'and recompile.',     'Currently maxnodes =',
     &                                                maxnodes,  '.'
                  stop
                elseif (nnodes.eq.-1) then
                  nnodes=ibuff(2)
                elseif (nnodes.ne.ibuff(2)) then
                  write(*,'(/1x,2A,I5,1x,4A/12x,2A,I5/)')
     &             'WARNING: Number of MPI nodes stated in global ',
     &             'attribute "partition"', ibuff(2),  'in netCDF ',
     &             'file ''', ncname0(1:lstr),''' ', 'contradicts ',
     &             'that in the previous file, nnodes =',  nnodes 
                  arg=arg-1
                  goto 5
                endif

                node=ibuff(1)
                if (xi_start(node).eq.-1 .and.
     &              eta_start(node).eq.-1) then
                  xi_start(node)=ibuff(3)
                  eta_start(node)=ibuff(4)
                  ncname(node)=ncname0
                  if (ncid(node).eq.-1) then  
                    ierr=nf_open (ncname(node),nf_nowrite,ncid(node))
                  else
                    write(*,'(/1x,2A,I4,1x,A)') '### ERROR: netCDF ',
     &                     'ID for file corresponding to MPI-node =',
     &                                   node,  'is already in use.'
                    stop
                  endif
!
! Lexical analysis of the file name: It is assumed that name of the
! file consists of root name (eg.: "history"); integer number which
! contains MPI node number (eg.: 03) and; suffix (eg.: ".nc").
! Files which belong to the same set normally have the same (1) root
! and (2) suffix names; the same (3) number of digits in the MPI node
! number segment in the filename and; (4) MPI node number from the
! file name should match the number determined from global attribute
! "partition".
!                                           ! Determine positions of
                  digit=.false.             ! starting and ending
                  is=0                      ! characters of MPI node
                  ie=0                      ! segment (is:ie)  
                  i=lstr+1
                  do while (is.eq.0 .and. i.gt.1)
                    i=i-1
                    if (ncname0(i:i).ge.'0' .and.
     &                  ncname0(i:i).le.'9') then 
                      if (.not.digit) then
                        if (i.lt.lstr) then
                          if (ncname0(i+1:i+1).eq.'.') then
                            ie=i
                            digit=.true.    ! check that node
                          endif             ! segment and suffix
                        else                ! are separated by '.'
                          ie=i
                          digit=.true.      ! no-suffix case
                        endif
                      endif
                    elseif (digit .and. ncname0(i:i).eq.'.') then
                      digit=.false.
                      is=i+1 
                    endif
                  enddo

                  if (is.gt.0 .and. ie.ge.is) then
                    root=ncname0(1:is-1)
                    if (ie.lt.lstr) then       ! Extract common part
                      sffx=ncname0(ie+1:lstr)  ! of file names, MPI
                    else                       ! node number and
                      sffx(1:1)=' '            ! suffix (if any).
                    endif
                    k=0
                    do i=is,ie
                      k=10*k + ichar(ncname0(i:i))-48
                    enddo
                    code_size=ie-is+1
                  else
                    write(*,'(/1x,3A)')         '### ERROR: Cannot ',
     &                 'determine MPI node number from file name ''',
     &                                        ncname0(1:lstr), '''.'
                  endif
# ifdef VERBOSE
                  write(*,'(1x,3A,I3,1x,A,I4,1x,A,I4,3x,A,2I4)')
     &           'fname = ''', ncname0(1:lstr), ''' code_size =',
     &                code_size,  'code =',  k,   'node =', node,
     &               'i,jSW =',  xi_start(node), eta_start(node)
# endif
!
! Checking consistency of root name with previously found.
!
                  ierr=nf_noerr
                  if (root_bak(1:1).eq.' ') then
                    root_bak=root
                  else
                    lvar=lenstr(root)
                    lbak=lenstr(root_bak)
                    if (lvar.ne.lbak .or. root.ne.root_bak) then
                      ierr=ierr+1
                      write(*,'(/8x,6A/17x,3A/)') 'WARNING: file ''',
     &                   ncname0(1:lstr),   ''' has different root ',
     &                   'name ''',      root(1:lvar),     ''' than',
     &                   'previously found root name ''',
     &                   root_bak(1:lbak),   ''' from the same set.'
                    endif
                  endif
!
! Checking consistency of suffix with previously found..
!
                  if (sffx_bak(1:1).eq.' ') then
                    sffx_bak=sffx
                  else
                    lvar=lenstr(sffx)
                    lbak=lenstr(sffx_bak)
                    if (lvar.ne.lbak .or. sffx.ne.sffx_bak) then
                      ierr=ierr+1
                      write(*,'(/8x,7A/17x,3A/)') 'WARNING: ',
     &                  'file ''',     ncname0(1:lstr),   ''' has ',
     &                  'different suffix name ''',    sffx(1:lvar),
     &                  ''' than','previously found suffix name ''',
     &                  sffx_bak(1:lbak),   ''' from the same set.'
                    endif
                  endif
!
! Checking consistency of length of node number segment
!
                  if (code_size_bak.eq.-1) then
                    code_size_bak=code_size
                  elseif (code_size .ne. code_size_bak) then
                    ierr=ierr+1
                    write(*,'(/8x,A,I2,1x,A/17x,3A,I2,A/)')
     &              'WARNING: number of digits in MPI node segment',
     &               code_size,'in file name', '''',ncname0(1:lstr),
     &              ''' is different than previously determined',
     &              code_size_bak, '.'
                  endif
 
 
!
! Checking consistency of node number with the file name.
!
                  if (k.ne.node) then
                    ierr=ierr+1
                    write(*,'(/8x,3A,I3/17x,2A/17x,A,I3,A/)')
     &                   'WARNING: file ''', ncname0(1:lstr),
     &                   ''' belongs to different MPI node',   node,
     &                   '(as determined from its global attribute',
     &                   '''partition'')', 'than node', k,
     &                   ' determined from to the file name.'
                  endif
!
! Stop, if something is wrong.
!
                  if (ierr.ne.nf_noerr) goto 97
                else
c                  write(*,'(/8x,3A/17x,A,I2,A,I2,A/)')
c   &              'WARNING: netCDF file ''',       ncname0(1:lstr),
c   &              ''' corresponds to subdomain','inode,jnode =', i,
c   &              ',', j, ' which already defined in another file.'
                  arg=arg-1
                  goto 5
                endif
              else
                write(*,'(/8x,2A/17x,4A/)') 'WARNING: Cannot ',
     &                'aquire global attribute ''partition'' from ',
     &                'netCDF file ''',  ncname0(1:lstr),    '''. ',
     &                'This file is ignored.'

              endif
            else
              write(*,'(/1x,2A/12x,3A/)')            '### WARNING: ',
     &                    'incompatible type and/or size of global ',
     &                   'attribute ''partition'' in netCDF file ''',
     &                  ncname0(1:lstr), '''. This file is ignored.'
            endif
          else
            write(*,'(/8x,3A/17x,A/)')     '### WARNING: netCDF file ''',
     &              ncname0(1:lstr), ''' is not a partitioned file,',
     &                                       'This file is ignored.'
          endif
        else
          write(*,'(/1x,A,1x,3A/14x,A)') '### WARNING: Cannot open ',
     &   'netCDF file ''', ncname0(1:lstr), '''.', nf_strerror(ierr)
        endif

!
! Verify, whether ncname,ncid(0:nnodes-1) comprises a complete
! set of partitioned files, if not, keep searching.
!
   5    complete=.true.
        do node=0,nnodes-1
          if (ncid(node).lt.0) complete=.false.
        enddo
        if (.not.complete .and. arg.lt.nargs) goto 2

!
! Once a complete set is identified, print it.
!

        if (complete) then
          lvar=lenstr(ncname(0))
          write(*,'(2(1x,A,I4),1x,A,2x,A,2I5)') 'Processing set of ',
     &                         nnodes, 'files', 0, ncname(0)(1:lvar),
     &                          'i,jSW =', xi_start(0), eta_start(0) 
          do node=1,nnodes-1
            if (node.lt.16 .or. (nnodes.gt.16 .and. node.eq.nnodes-1
     &                                                       )) then
              write(*,'(29x,I4,1x,A,2x,A,2I5)') node,
     &                              ncname(node)(1:lvar), 'i,jSW =',
     &                              xi_start(node), eta_start(node)
            elseif (nnodes.gt.16 .and. node.lt.18) then
              write(*,'(24x,A)') '.................................'
            endif
          enddo

#ifdef KEEP_CLOSED
          do node=1,nnodes-1
            ncid(node)=-1
          enddo
#endif
          write(*,*)
        elseif (arg.lt.nargs) then
          goto 1
        else
          write(*,*) 'stop at 375'
          stop
        endif



!
!      *****    *********    ******   *******    *********
!    ***   ***  *  ***  *   **  ***   ***   ***  *  ***  *
!    ***           ***     **   ***   ***   ***     *** 
!      *****       ***    ***   ***   ***   **      ***
!          ***     ***    *********   ******        ***
!    ***   ***     ***    ***   ***   ***  **       ***
!      *****       ***    ***   ***   ***   ***     ***
!
!
! After this moment a set of files recorded as [ncname(0:nnodes-1),
! xi_start(0:nnodes-1), eta_start(0:nnodes-1), and ncid(0:nnodes-1)]
! should comprise a complete set, however in the case of KEEP_CLOSED
! the state of all files is "closed" and all netCDF IDs are reset
! to -1.
!

  




!
! General inquiry into the file which belongs to subdomain node=0

        lstr=lenstr(ncname(0))
#ifdef KEEP_CLOSED
        ierr=nf_open (ncname(0)(1:lstr), nf_nowrite, ncid(0))
        if (ierr.eq.nf_noerr) then
#endif
          ierr=nf_inq (ncid(0), ndims, nvars, ngatts, unlimdimid)
          if (ierr .ne. nf_noerr) then
            write(*,'(/1x,4A/12x,A/)')     '### ERROR: Cannot make ',
     &                         'general inquiry into netCDF file ''',
     &                   ncname(0)(1:lstr), '''.', nf_strerror(ierr)
            goto 97
          elseif (ndims .gt. maxdims) then
            write(*,'(/8x,2A,I4,1x,4A/15x,A,1x,A/)')   '### ERROR: ',
     &                  'number of dimensions', ndims,  'in netCDF ',
     &        'file ''',  ncname(0)(1:lstr), '''',  'exceeds limit.',
     &        'Increase parameter maxdims in file "partit.F".'
            goto 97
          elseif (nvars .gt. maxvars) then
            write(*,'(/8x,2A,I4,1x,4A/15x,A,1x,A/)')   '### ERROR: ',
     &        'number of variables',  nvars,  'in netCDF', 'file ''',
     &                    ncname(0)(1:lstr), '''',  'exceeds limit.',
     &        'Increase parameter maxvars in file "partit.F".'
            goto 97
          endif
!
! Verify that ndims, nvars, ngatts, unlimdimid are the same for
! all of the nodes.
!
          do node=1,nnodes-1
#ifdef KEEP_CLOSED
            ierr=nf_open (ncname(node), nf_nowrite, ncid(node)) 
#endif
   
            ierr=nf_inq (ncid(node), ibuff(1), ibuff(2),
     &                               ibuff(3), ibuff(4))
            if (ierr .ne. nf_noerr) then
              write(*,'(/1x,4A/12x,A/)')   '### ERROR: Cannot ',
     &                   'make general inquiry into netCDF file ''',
     &               ncname(node)(1:lstr), '''.', nf_strerror(ierr)
              goto 97
            elseif (ibuff(1) .ne. ndims) then
              write(*,'(/8x,3A/15x,3A/)') 'ERROR: netCDF file ''',
     &               ncname(node)(1:lstr), ''' has different number',
     &              'of dimensions than ''', ncname(0)(1:lstr), '''.'
              goto 97
            elseif (ibuff(2) .ne. nvars) then
              write(*,'(/8x,3A/15x,3A/)') 'ERROR: netCDF file ''',
     &               ncname(node)(1:lstr), ''' has different number',
     &              'of variables than ''',  ncname(0)(1:lstr), '''.'
              goto 97
            elseif (ibuff(3) .ne. ngatts) then
              write(*,'(/8x,3A/15x,3A/)') 'ERROR: netCDF file ''',
     &               ncname(node)(1:lstr), ''' has different number',
     &              'of global attributes than ''',
     &                                       ncname(0)(1:lstr), '''.'
              goto 97
            elseif (ibuff(4) .ne. unlimdimid) then
              write(*,'(/8x,3A/15x,3A/)') 'ERROR: netCDF file ''',
     &               ncname(node)(1:lstr), ''' has different ID',
     &              'for unlimited dimension than ''',
     &                                       ncname(0)(1:lstr), '''.'
              goto 97
            endif
#ifdef KEEP_CLOSED
            ierr=nf_close(ncid(node))
            ncid(node)=-1
#endif
          enddo
!
! Check dimensions (IDs, names and sizes) for consistency across
! the subdomains.
!
          do node=0,nnodes-1
#ifdef KEEP_CLOSED
            if (node.gt.0) ierr=nf_open (ncname(node)(1:lstr),
     &                                  nf_nowrite, ncid(node))
            if (ierr.eq.nf_noerr) then
#endif
            do i=1,ndims
              ierr=nf_inq_dim (ncid(node), i, string,dimsize(i,node))
              if (ierr .eq. nf_noerr) then
                lvar=lenstr(string)
                if (node.eq.0) then
                  ldim(i)=lvar
                  dimname(i)=string(1:lvar)
                elseif (lvar.ne.ldim(i) .or. string(1:lvar).ne.
     &                               dimname(i)(1:ldim(i)) )then
                  write(*,'(/1x,A,I3,3A/12x,6A/12x,3A/)')
     &              '### ERROR: Name of dimension with with #',    i,
     &              ', ''',   string(1:lvar),  ''' from netCDF file',
     &              '''', ncname(node)(1:lstr), ''' does not match ',
     &              'name ''',  dimname(i)(1:ldim(i)),
     &              ''' with the same ID', 'in netCDF file ''',
     &               ncname(0)(1:lstr),    '''.'
                  goto 97
                endif
              else
                write(*,'(/1x,2A,I3/12x,3A/12x,A)')    '### ERROR: ',
     &               'Cannot determine name and size of dimension #',
     &                i,  'in netCDF file ''',  ncname(node)(1:lstr),
     &                                      '''.', nf_strerror(ierr)
                goto 97
              endif
            enddo
#ifdef KEEP_CLOSED
             if (node.gt.0) then
               ierr=nf_close(ncid(node))
               ncid(node)=-1
              endif
            else
              lstr=lenstr(ncname(node))
              write(*,'(/1x,A,1x,3A/14x,A)')    '### ERROR: Cannot ',
     &            'open netCDF file ''', ncname(node)(1:lstr), '''.',
     &                                             nf_strerror(ierr)
              goto 97
            endif
#endif
          enddo
!
! Determine sizes of dimensions for combined file: For partitionable
! dimensions 'xi_rho', 'xi_u', 'eta_rho' and 'eta_v' determine the
! extent of the physical grid in each direction as the maximum over
! all subdomains of the dimension of each partial file combined with
! its starting index, "xi_start" or "eta_start". This is straghtforward,
! for RHO-points, but for U- and V-dimensions it requires to take
! into account the fact that the subdomains adjacent to eastern or
! southern edge have one point less than the corresponding RHO-
! dimension. Consequently, all subsequent subdomains receive
! one-point shift.  For all other dimensions, verify that the sizes
! are the same for all nodes.  Also find size of unlimited dimension,
! if it is present.  Note that variable "tsize" is set to its default
! value 1 (meaning one record), and may or may not be overwritten by
! the actual size of unlimited dimension (if it exists). If the
! unlimited dimension does not exist, it retains its value of 1 so
! that the loop over records is still executed, but only once.
!


          id_xi_rho=0
          id_eta_rho=0
          id_xi_rho=0
          id_eta_v=0

          XI_rho=0
          ETA_rho=0

          size_XI=1
          size_ETA=1
          size_S=1
          tsize=1


          do i=1,ndims
            dimsize(i,nnodes)=0
            lvar=lenstr(dimname(i))
            if (lvar.eq.6 .and. dimname(i)(1:lvar).eq.'xi_rho') then
              id_xi_rho=i
              do node=0,nnodes-1
                dimsize(i,nnodes)=max( dimsize(i,nnodes),
     &                 dimsize(i,node) +xi_start(node)-1 )
                size_XI=max(size_XI,dimsize(i,node))
                XI_rho=max(XI_rho, dimsize(i,nnodes))
              enddo
           
            elseif (lvar.eq.4 .and.dimname(i)(1:lvar).eq.'xi_u') then
              id_xi_u=i
              do node=0,nnodes-1
                if (xi_start(node).gt.1) then
                  dimsize(i,nnodes)=max( dimsize(i,nnodes),
     &                   dimsize(i,node) +xi_start(node)-2 )
                else
                  dimsize(i,nnodes)=max( dimsize(i,nnodes),
     &                                    dimsize(i,node) )
                endif
                size_XI=max(size_XI,dimsize(i,node))
                XI_rho=max(XI_rho, dimsize(i,nnodes)+1)
              enddo

            elseif (lvar.eq.7 .and. dimname(i)(1:lvar) .eq.
     &                                      'eta_rho') then
              id_eta_rho=i
              do node=0,nnodes-1
                dimsize(i,nnodes)=max( dimsize(i,nnodes),
     &                dimsize(i,node) +eta_start(node)-1 )
                size_ETA=max(size_ETA,dimsize(i,node))
                ETA_rho=max(ETA_rho, dimsize(i,nnodes))
              enddo

            elseif (lvar.eq.5.and.dimname(i)(1:lvar).eq.'eta_v') then
              id_eta_v=i
              do node=0,nnodes-1
                if (eta_start(node).gt.1) then
                  dimsize(i,nnodes)=max( dimsize(i,nnodes),
     &                   dimsize(i,node) +eta_start(node)-2 )
                else
                  dimsize(i,nnodes)=max( dimsize(i,nnodes),
     &                                     dimsize(i,node))
                endif
                size_ETA=max(size_ETA,dimsize(i,node))
                ETA_rho=max(ETA_rho, dimsize(i,nnodes)+1)
              enddo

            else
              dimsize(i,nnodes)=dimsize(i,0)
              do node=1,nnodes-1
                if (dimsize(i,0).ne.dimsize(i,node)) then
                  write(*,'(/1x,A,I3,3A,I4,1x,A/12x,4A/12x,3A,I4/)')
     &                '### ERROR: Nonpartitioned dimension #',     i,
     &                ', named ''', dimname(i)(1:lvar), ''', size =',
     &                 dimsize(i,node),    'in ',   'netCDF file ''',
     &                 ncname(node)(1:lstr),     ''' has different ',
     &                'size than similar',  'dimension from file ''',
     &                 ncname(0)(1:lstr),     ''', which has size =',
     &                                                  dimsize(i,0)
                  goto 97
                endif
              enddo
              if (lvar.eq.5 .and. dimname(i)(1:lvar).eq.'s_rho') then
                size_S=max(size_S, dimsize(i,0))
              elseif (lvar.eq.3.and.dimname(i)(1:lvar).eq.'s_w') then
                size_S=max(size_S, dimsize(i,0))
              endif
            endif
            if (i.eq. unlimdimid) then
              tsize=dimsize(i,nnodes)
              dimsize(i,nnodes)=nf_unlimited
            endif
          enddo ! <-- i loop over dimensions


#ifdef VERBOSE
          write(*,*) 'Identifying presense of boundary edges:'
#endif
          do node=0,nnodes-1
            western_edge(node)=.true.
            eastern_edge(node)=.true.
            southern_edge(node)=.true.
            northern_edge(node)=.true.

            if (xi_start(node).gt.1) then
              western_edge(node)=.false.
            endif
            if (id_xi_rho.gt.0) then
              if ( xi_start(node)+dimsize(id_xi_rho,node)
     &           .lt.XI_rho ) eastern_edge(node)=.false. 
            endif
            if (id_xi_u.gt.0) then
              if ( xi_start(node)+dimsize(id_xi_u,node)
     &           .lt.XI_rho ) eastern_edge(node)=.false.
            endif
            if (eta_start(node).gt.1) then
              southern_edge(node)=.false.
            endif
            if (id_eta_rho.gt.0) then
              if ( eta_start(node)+dimsize(id_eta_rho,node)
     &           .lt.ETA_rho ) northern_edge(node)=.false.
            endif
            if (id_eta_v.gt.0) then
              if ( eta_start(node)+dimsize(id_eta_v,node)
     &           .lt.ETA_rho ) northern_edge(node)=.false.
            endif

#ifdef VERBOSE
            if (node.eq.0) then
              write(*,'(8x,A,I4,4(2x,A,L1))') 'node =', node,
     &        'WST=',western_edge(node),  'EST=',eastern_edge(node),
     &        'SOU=',southern_edge(node), 'NOR=',northern_edge(node)
            else
              write(*,'(14x,I4,4(6x,L1))') node,
     &                     western_edge(node),  eastern_edge(node),
     &                     southern_edge(node), northern_edge(node)
            endif
#endif
          enddo





!
! set buffer size for needed to accommodate data for the largest
! possible MPI-subdomain (i.e., partial file).
! 
          max_buff_size=size_XI*size_ETA*size_S
!
! Create combined netCDF file:   Once the completeness of the set of
!------- -------- ------ -----   partial files have been established
! and dimensions survive consistency check, create the combined file,
! define its dimensions and copy global attributes.
!
          i=lenstr(root_bak)
          if (sffx_bak(1:1).ne.' ') then
            j=lenstr(sffx_bak)
            if (root_bak(i:i).eq.'.' .and. sffx_bak(1:1).eq.'.') then
              ncname0=root_bak(1:i)/ /sffx_bak(2:j)
            else
              ncname0=root_bak(1:i)/ /sffx_bak(1:j)
            endif
          else
            ncname0=root_bak(1:i)
          endif
          lstr=lenstr(ncname0)
c**       ierr=nf_create (ncname0(1:lstr), nf_clobber, nctarg)

          ierr=nf_create (ncname0(1:lstr), nf_64bit_offset, nctarg)

          if (ierr .eq. nf_noerr) then
            write(*,'(1x,3A)')  'Created netCDF file ''',
     &                            ncname0(1:lstr), '''.'
          else
            write(*,'(/1x,4A/12x,A/)')   '### ERROR: Cannot create ',
     &                      'netCDF file ''', ncname0(1:lstr), '''.',
     &                                             nf_strerror(ierr)
            goto 97
          endif
!
! Define dimensions: also compute the size of buffer needed to
! accommodate the largest array.
!
# ifdef VERBOSE
          write(*,'(1x,A,3x,A,1x,A,1x,A)') 'Dimensions:', 'id',
     &                                           'size', 'name'
# endif
          size_XI=1
          size_ETA=1
          size_S=1

          do i=1,ndims
            lvar=lenstr(dimname(i))
            ierr=nf_def_dim (nctarg, dimname(i)(1:lvar),
     &                     dimsize(i,nnodes), dimid(i))
            if (ierr .eq. nf_noerr) then
              if (dimid(i) .eq. i) then
# ifdef VERBOSE
                write(*,'(14x,I3,I5,1x,3A)') dimid(i), 
     &                               dimsize(i,nnodes), '''',
     &                               dimname(i)(1:lvar), ''''
# endif
                if (dimname(i)(1:3) .eq. 'xi_') then
                  size_XI=max(size_XI, dimsize(i,nnodes))
                elseif (dimname(i)(1:4) .eq. 'eta_') then
                  size_ETA=max(size_ETA, dimsize(i,nnodes))
                elseif (dimname(i)(1:5) .eq. 's_rho' .or.
     &                  dimname(i)(1:3) .eq. 's_w') then
                  size_S=max(size_S, dimsize(i,nnodes))
                endif
              else
                write(*,'(/1x,A,I3,1x,5A/12x,2A,I3,A/)')
     &               '### ERROR: ID =', dimid(i), 'for dimension ''',
     &                         dimname(i)(1:lvar), ''' from file ''',
     &                    ncname0(1:lstr), '''', 'differs from the ',
     &                                       'original ID =', i, '.'
                goto 97
              endif
            else
              write(*,'(/1x,4A/12x,A,I4,A/12x,A/)')    '### ERROR: ',
     &             'Cannot define dimension ''',  dimname(i)(1:lvar),
     &             '''.',  'netCDF error status code =',  ierr,  ',',
     &                                             nf_strerror(ierr)
              goto 97
            endif 
          enddo ! <-- i loop over dimensions
# ifdef FAST_MODE
          max_bfr_out=size_XI*size_ETA*size_S 
# endif

!
! Copy all global attributes, except 'partition'.
!
# ifdef VERBOSE
          write(*,'(1x,A)') 'Copying global attributes:'
# endif
          do i=1,ngatts
            ierr=nf_inq_attname (ncid(0), nf_global, i, string)
            if (ierr. eq. nf_noerr) then
              lvar=lenstr(string)
              if (string(1:lvar) .ne. 'partition') then
                ierr=nf_copy_att (ncid(0), nf_global, string(1:lvar),
     &                                            nctarg, nf_global)
                if (ierr .ne. nf_noerr) then
                  write(*,'(/8x,4A/15x,3A/)')  'ERROR: Cannot ',
     &            'copy global attribute ''', string(1:lvar), '''',
     &            'into netCDF file ''',   ncname0(1:lstr),   '''.'
                  goto 97
                endif
# ifdef VERBOSE
                write(*,'(20x,3A)') '''', string(1:lvar), ''''
# endif      
              endif
            else
              lstr=lenstr(ncname(0))
              write(*,'(/1x,2A,I3/12x,3A/12x,A)')     '### ERROR: ',
     &                'Cannot determine name of global attribute #',
     &                 i, 'from netCDF file ''',  ncname(0)(1:lstr),
     &                                    '''.',  nf_strerror(ierr)
              goto 97
            endif
          enddo
!
! Define variables and copy their attributes.
!
# ifdef VERBOSE
          write(*,'(1x,2A)') 'Variables, their dimensions and ',
     &                                           'attributes:'
# endif
          do i=1,nvars
            ierr=nf_inq_var (ncid(0),   i, varname(i),  vartype(i),
     &                        vardims(i), dimids(1,i),   varatts)
            if (ierr .eq. nf_noerr) then
              lvar=lenstr(varname(i))
              ierr=nf_def_var (nctarg, varname(i)(1:lvar),vartype(i),
     &                            vardims(i), dimids(1,i), varid(i))
              if (ierr .eq. nf_noerr) then
# ifdef VERBOSE
                write(*,'(8x,3A,2x,A,8I3)') '''', varname(i)(1:lvar),
     &              ''',', 'dimids =', (dimids(j,i), j=1,vardims(i))
# endif
                do j=1,varatts
                  ierr=nf_inq_attname (ncid(0), varid(i), j, string)
                  if (ierr .eq. nf_noerr) then
                    lvar=lenstr(string)
                    ierr=nf_copy_att (ncid(0), i, string(1:lvar),
     &                                          nctarg, varid(i))
                    if (ierr. ne. nf_noerr) then
                      write(*,'(/8x,2A,I3,3A/15x,3A/)') 'ERROR: ',
     &                  'Cannot copy attribute with ID =',   j,
     &                  ' for variable ''', varname(i)(1:lvar), '''',
     &                  'into netCDF file ''', ncname0(1:lstr), '''.'
                      goto 97
                    endif
# ifdef VERBOSE
                     write(*,'(16x,3A)') '''', string(1:lvar), ''''
# endif
                  else
                    write(*,'(/1x,2A,I3/12x,3A/12x,A/)')
     &                 '### ERROR: Cannot get name of attribute #',
     &                  j,  'for variable ''',  varname(i)(1:lvar),
     &                                    '''.', nf_strerror(ierr) 
                    goto 97
                  endif
                enddo
              else
                write(*,'(/8x,4A/)') 'ERROR: Cannot define ',
     &                  'variable ''', varname(i)(1:lvar), '''.'
                goto 97
              endif
            else
              lstr=lenstr(ncname(0))
              write(*,'(/8x,2A/15x,A,I3,1x,3A/)')  'ERROR: Cannot ',
     &            'determine name, type and attributes for variable',
     &            'with ID =', i, 'from netCDF file ''',
     &                                      ncname(0)(1:lstr),  '''.'
              goto 97
            endif
!
! Determine whether partitionable dimensions or unlimited dimension
! are present for this variable.
!
            series(i)=.false.
            part_type(i)=0
            do j=1,vardims(i)
              if (dimids(j,i).eq.id_xi_rho .or.
     &            dimids(j,i).eq.id_xi_u) then
                part_type(i)=part_type(i)+1
              elseif (dimids(j,i).eq.id_eta_rho .or.
     &                dimids(j,i).eq.id_eta_v) then
                part_type(i)=part_type(i)+2
              elseif (dimids(j,i).eq.unlimdimid) then
                series(i)=.true.
              endif
            enddo
          enddo       ! <-- i, variable ID.

!
! Leave definition mode
!
          ierr=nf_enddef (nctarg)
# ifdef VERBOSE
          write(*,'(1x,A)') 'Leaving definition mode.'
# endif


#ifdef KEEP_CLOSED
        else
          write(*,'(/1x,4A/14x,A/)')       '### ERROR: Cannot open ',
     &                    'netCDF file ''', ncname(0)(1:lstr), '''.',
     &                                             nf_strerror(ierr)
          goto 97
        endif
#endif



!
!
!     **     *     ***  *******    ***  *********  ********
!      *    ***   ***   ***   ***  ***  *  ***  *  ***    *
!       *   ***   ***   ***   ***  ***     ***     ***
!       *  *** * ***    ***   **   ***     ***     ******
!        * **  * **     ******     ***     ***     ***
!        ***    ***     ***  **    ***     ***     ***    *
!         *     **      ***   ***  ***     ***     ********
!
!
! Allocate necessary buffer arrays: here "max_*_size" is needed size
! of buffer array, as determined by processing of current file, while
! "alloc_*_size" is size of array already allocated. Both are
! initialized to zero at the beginning.  Basically this anticipates
! the possibility of gradual increase of needed size of buffer array,
! so that if it is allocated at this moment, but insufficient, it is
! deallocated first. 
!
# ifdef FAST_MODE
        if (max_bfr_out .gt. alloc_bfr_out) then
          if (allocated(bfr_out)) deallocate(bfr_out)
        endif
# endif
        if (max_buff_size .gt. alloc_buff_size) then
          if (allocated(buff)) deallocate(buff)
          allocate(buff(max_buff_size))
          alloc_buff_size=max_buff_size
          write(*,*) 'allocated "buff" with max_buff_size =',
     &                                         max_buff_size
        endif
# ifdef FAST_MODE
        if (max_bfr_out .gt. alloc_bfr_out) then
          allocate(bfr_out(max_bfr_out))
          alloc_bfr_out=max_bfr_out
          write(*,*) 'allocated "bfr_out" with ',
     &        'max_bfr_out =', max_bfr_out
        endif
# endif
!
! Transfer variables into combined file.
!

        do rec=1,tsize
          if (tsize.gt.1) write(*,'(1x,A,I8,1x,A,I8,1x,A)')
     &     'Processing record', rec, 'out of', tsize,  '...'
          do i=1,nvars
            if (rec.eq.1 .or. series(i)) then
              if (part_type(i).eq.0 .and. .not.series(i)) then
!
! Scalar (zero-dimensional) variables:
!
                lvar=lenstr(varname(i))
                write(*,'(8x,3A)') 'Copy scalar variable: ''',
     &                            varname(i)(1:lvar), '''...'

                if (vartype(i) .eq. nf_char) then
                  ierr=nf_get_var_text  (ncid(0), i, buff)
                elseif (vartype(i) .eq. nf_int) then
                  ierr=nf_get_var_int    (ncid(0), i, buff)
                elseif (vartype(i) .eq. nf_float) then
                  ierr=nf_get_var_real   (ncid(0), i, buff)
                elseif (vartype(i) .eq. nf_double) then
                  ierr=nf_get_var_double (ncid(0), i, buff)
                else
                  lvar=lenstr(varname(i))
                  write(*,'(/8x,4A/)') '### ERROR: scalar variable ',
     &              '''', varname(i)(1:lvar), ''' has unknown type.'
                  goto 97
                endif
                if (ierr .eq. nf_noerr) then
                  if (vartype(i) .eq. nf_char) then
                    ierr=nf_put_var_text  (nctarg, varid(i), buff)
                  elseif (vartype(i) .eq. nf_int) then
                    ierr=nf_put_var_int    (nctarg, varid(i), buff)
                  elseif (vartype(i) .eq. nf_float) then
                    ierr=nf_put_var_real   (nctarg ,varid(i), buff)
                  elseif (vartype(i) .eq. nf_double) then
                    ierr=nf_put_var_double (nctarg, varid(i), buff)
                  endif
                  if (ierr .ne. nf_noerr) then
                    lvar=lenstr(varname(i))
                    write(*,'(/1x,4A/12x,3A/12x,A)') '### ERROR: ',
     &                           'Cannot write scalar variable ''',
     &                        varname(i)(1:lvar), ''' into netCDF',
     &                        'file ''',   ncname0(1:lstr),  '''.',
     &                                           nf_strerror(ierr)
                    goto 97
                  endif
                else
                  lvar=lenstr(varname(i))
                  write(*,'(/1x,4A/12x,A/)')  '### ERROR: Cannot ',
     &               'read scalar variable ''', varname(i)(1:lvar),
     &                                    '''.', nf_strerror(ierr)
                  goto 97
                endif
              elseif (part_type(i).eq.0) then
!
! Non-partitionable array.
!
                lvar=lenstr(varname(i))
                write(*,'(8x,3A)') 'Copy non-partitioned array: ''',
     &                                  varname(i)(1:lvar), '''...'
                size=1
                do j=1,vardims(i)
                  if (dimids(j,i).eq.unlimdimid) then
                    start(j)=rec
                    count(j)=1
                  else
                    start(j)=1
                    count(j)=dimsize(dimids(j,i),0)
                  endif
                  size=size*count(j)
                enddo
                if (vartype(i) .eq. nf_char) then
                  size=size*1
                elseif (vartype(i) .eq. nf_int) then
                  size=size*4
                elseif (vartype(i) .eq. nf_float) then
                  size=size*4
                elseif (vartype(i) .eq. nf_double) then
                  size=size*8
                else
                  lvar=lenstr(varname(i))
                  write(*,'(/8x,3A/)') '### ERROR: variable ''',
     &               varname(i)(1:lvar), ''' has unknown type.'
                  goto 97
                endif
                if (size .gt. 8*max_buff_size) then
                  if (allocated(buff)) deallocate(buff)
                  max_buff_size=(size+7)/8
                  allocate(buff(max_buff_size))
                  write(*,*) 'allocated "buff" with max_buff_size =',
     &                                              max_buff_size
                endif
 
                if (vartype(i) .eq. nf_char) then
                  ierr=nf_get_vara_text   (ncid(0), i, start,
     &                                           count, buff)
                elseif (vartype(i) .eq. nf_int) then
                  ierr=nf_get_vara_int    (ncid(0), i, start,
     &                                           count, buff)
                elseif (vartype(i) .eq. nf_float) then
                  ierr=nf_get_vara_real   (ncid(0), i, start,
     &                                           count, buff)
                elseif (vartype(i) .eq. nf_double) then
                  ierr=nf_get_vara_double (ncid(0), i, start,
     &                                           count, buff)
                endif
                if (ierr .eq. nf_noerr) then
                  if (vartype(i) .eq. nf_char) then
                    ierr=nf_put_vara_text   (nctarg, varid(i),
     &                                         start, count, buff)
                  elseif (vartype(i) .eq. nf_int) then
                    ierr=nf_put_vara_int    (nctarg, varid(i),
     &                                         start, count, buff)
                  elseif (vartype(i) .eq. nf_float) then
                    ierr=nf_put_vara_real   (nctarg, varid(i),
     &                                         start, count, buff)
                  elseif (vartype(i) .eq. nf_double) then
                    ierr=nf_put_vara_double (nctarg, varid(i),
     &                                         start, count, buff)
                  endif
                  if (ierr .ne. nf_noerr) then
                    lvar=lenstr(varname(i))
                    write(*,'(/8x,4A,I3/15x,3A,I3/)')  '### ERROR: ',
     &               'Cannot write variable ''',  varname(i)(1:lvar),
     &               ''' for time record',rec, 'into netCDF file ''',
     &               ncname0(1:lstr),'''. netCDF error code =', ierr
                    goto 97
                  endif
                else
                  lvar=lenstr(varname(i))
                  write(*,'(/8x,4A,I3,A/15x,A,I4/)')  '### ERROR: ',
     &               'Cannot read variable ''',  varname(i)(1:lvar),
     &               ''' for time record', rec, '.',
     &                           'netCDF error status code =', ierr
                  goto 97
                endif
              elseif (part_type(i).gt.0) then

!
! Partitioned array:
!
                lvar=lenstr(varname(i))
                write(*,'(8x,2A,I3,1x,3A)') 'Assembly partitioned ',
     &                     'array type', part_type(i),  'name = ''',
     &                                     varname(i)(1:lvar), ''''
                do node=0,nnodes-1
                  var_mask=.true.
                  if (part_type(i).eq.1 .and. lvar.gt.6) then 
                    if (varname(i)(lvar-5:lvar).eq.'_south' 
     &                        .and. southern_edge(node)) then
                      var_mask=.true.
# ifdef VERBOSE
                      write(*,'(3x,A,I4,1x,4A)')  'node =', node,
     &               'identified XI-partitioned southern ',
     &               'boundary array ''', varname(i)(1:lvar), ''''
# endif
                    elseif (varname(i)(lvar-5:lvar).eq.'_north'
     &                           .and. northern_edge(node)) then
                      var_mask=.true.
# ifdef VERBOSE
                      write(*,'(3x,A,I4,1x,4A)')   'node =', node,
     &               'identified XI-partitioned northern ',
     &               'boundary array ''', varname(i)(1:lvar), ''''
# endif
                    endif
                  elseif (part_type(i).eq.2 .and. lvar.gt.5) then
                    if (varname(i)(lvar-4:lvar).eq.'_west'
     &                                .and. western_edge(node)) then
                      var_mask=.true.
# ifdef VERBOSE
                      write(*,'(3x,A,I4,1x,4A)')     'node =', node,
     &               'identified ETA-partitioned western boundary ',
     &               'array ''', varname(i)(1:lvar), ''''
# endif
                    elseif (varname(i)(lvar-4:lvar).eq.'_east'
     &                          .and. eastern_edge(node)) then
                      var_mask=.true.
# ifdef VERBOSE
                      write(*,'(3x,A,I4,1x,4A)')     'node =', node,
     &               'identified ETA-partitioned eastern boundary ',
     &               'array ''', varname(i)(1:lvar), ''''
# endif
                    endif
                  elseif (part_type(i).eq.3) then
                    var_mask=.true.
# ifdef VERBOSE
                    write(*,'(3x,A,I4,1x,4A)')   'node =', node,
     &                     'identified 2D-partitioned array ''',
     &                      varname(i)(1:lvar), ''''
# endif
                  endif
                  if (var_mask) then
                    size=1
                    size1=1
                    do j=1,vardims(i)
                      k=dimids(j,i)
                      if (k.eq.id_xi_rho  .or. k.eq.id_xi_u  .or.
     &                    k.eq.id_eta_rho .or. k.eq.id_eta_v) then
                        start(j)=1
                        count(j)=dimsize(k,node)

                        if (k.eq.id_xi_rho) then
                          start1(j)=xi_start(node)
#ifdef FAST_MODE
                          count1(j)=XI_rho
#endif
                        elseif (k.eq.id_xi_u) then
                          start1(j)=max(xi_start(node)-1,1)
#ifdef FAST_MODE
                          count1(j)=XI_rho-1
#endif
                        elseif (k.eq.id_eta_rho) then
                          start1(j)=eta_start(node)
#ifdef FAST_MODE
                          count1(j)=ETA_rho
#endif
                        elseif (k.eq.id_eta_v) then
                          start1(j)=max(eta_start(node)-1,1)
#ifdef FAST_MODE
                          count1(j)=ETA_rho-1
#endif
                        endif

                      elseif (k.eq.unlimdimid) then
                        start(j)=rec
                        count(j)=1
                        start1(j)=rec
#ifdef FAST_MODE
                        count1(j)=1
#endif
                      else
                        start(j)=1
                        count(j)=dimsize(k,nnodes)
                        start1(j)=1
#ifdef FAST_MODE
                        count1(j)=count(j)
#endif
                      endif
                      size=size*count(j)
#ifdef FAST_MODE
                      size1=size1*count1(j)
#else
                      size1=size*count(j)
#endif
                    enddo


                    if (vartype(i) .eq. nf_char) then
                      size=size*1
                      size1=size1*1
                    elseif (vartype(i) .eq. nf_int) then
                      size=size*4
                      size1=size1*4
                    elseif (vartype(i) .eq. nf_float) then
                      size=size*4
                      size1=size1*4
                    elseif (vartype(i) .eq. nf_double) then
                      size=size*8
                      size1=size1*8
                    else
                      lvar=lenstr(varname(i))
                      write(*,'(/8x,4A/)') '### ERROR: variable ''',
     &                   varname(i)(1:lvar), ''' has unknown type.'
                      goto 97
                    endif

                    if (size .gt. 8*max_buff_size) then
                      if (allocated(buff)) deallocate(buff)
                      max_buff_size=(size+7)/8
                      allocate(buff(max_buff_size))
                      write(*,*) 'allocated "buff" with ',
     &                   'max_buff_size =', max_buff_size
                    endif




c?                  if (max(size,size1) .gt. 8*max_buff_size) then
c?                    write(*,'(/8x,A,3(/15x,A,I10,1x,A)/)')
c?   &              'ERROR: insufficient buffer size in "ncjoin.F":',
c?   &              'requested:',     max(size,size1),      'Bytes,',
c?   &              'available:',   8*max_buff_size, 'Bytes.',
c?   &              'Increase parameter max_buff_size and recompile.'
c?                    goto 97
c?                  endif




#ifdef KEEP_CLOSED
                    if (node.gt.0) ierr=nf_open (ncname(node),
     &                                       nf_nowrite, ncid(node))
                    if (ierr.eq.nf_noerr) then
#endif

# ifdef VERBOSE
                      write(*,'(3x,A,4(I4,1x,A),$)')  'node =',node,
     &               'ncid=',ncid(node), 'xi_start =',xi_start(node),
     &                  'eta_start =',eta_start(node),  'reading...'
# endif

                      if (vartype(i) .eq. nf_char) then
                        ierr=nf_get_vara_text  (ncid(node), i, start,
     &                                                   count, buff)
                      elseif (vartype(i) .eq. nf_int) then
                        ierr=nf_get_vara_int   (ncid(node), i, start,
     &                                                   count, buff)
                      elseif (vartype(i) .eq. nf_float) then
                        ierr=nf_get_vara_real  (ncid(node), i, start,
     &                                                   count, buff)
                      elseif (vartype(i) .eq. nf_double) then
                        ierr=nf_get_vara_double(ncid(node), i, start,
     &                                                   count, buff)
                      endif
#ifdef KEEP_CLOSED
                    if (node.gt.0) then
                      ierr=nf_close(ncid(node))
                      ncid(node)=-1
                    endif
                  else
                    lstr=lenstr(ncname(node))
                    write(*,'(/1x,A,1x,3A/14x,A)')     '### ERROR: ',
     &                                  'Cannot open netCDF file ''',
     &                ncname(node)(1:lstr), '''.', nf_strerror(ierr)
                    goto 97
                  endif
#endif
# ifdef VERBOSE
                    write(*,'(1x,A)')  'copying ...'
# endif
!
! In the code segment below there are two strategies, resulting in
! equivalent outcome, but different performance: CPP-switch FAST_MODE
! activated code in which the partitioned array is first assembled
! into the intermediate buffer array "bfr_out" horizontal dimensions
! of which correspond to the whole physical grid. After that, the
! whole array is defined, it is written once as a single record [this
! occurs immediately after the reading of the last node]. Note that
! all values of "count1(k)" [except for "k" corresponding to
! unlimited dimension] are equal to the actual dimensions of the
! variable in netCDF file, while all all "start(k)" [except unlimited
! dimension] are equal to 1.
!
! Alternatively, partial rectangular blocks are written immediately
! after they read from the partial netCDF files, relying on sub-array
! writing capability of "nf_put_vara_XXX". This results physical
! write into a large number of small records of size count(1).
!
                    if (ierr .eq. nf_noerr) then
# ifdef FAST_MODE

                      if (size1 .gt. 8*max_bfr_out) then
                        if (allocated(bfr_out)) deallocate(bfr_out)
                        max_bfr_out=(size1+7)/8
                        allocate(bfr_out(max_bfr_out))
                        write(*,*) 'allocated "bfr_out" with ',
     &                     'max_bfr_out =', max_bfr_out 
                      endif


                      if (vartype(i) .eq. nf_char) then
                        call assembly_text (buff,count, bfr_out,
     &                                  start1,count1, vardims(i))
                      elseif (vartype(i) .eq. nf_int) then
                        call assembly_int  (buff,count, bfr_out,
     &                                  start1,count1, vardims(i))
                      elseif (vartype(i) .eq. nf_float) then
                        call assembly_real (buff,count, bfr_out,
     &                                  start1,count1, vardims(i))
                      elseif (vartype(i) .eq. nf_double) then
                        call assembly_double (buff,count, bfr_out,
     &                                  start1,count1, vardims(i))
                      endif
                      if (node .eq. nnodes-1) then
                        if (vartype(i) .eq. nf_char) then
                          ierr=nf_put_vara_text   (nctarg, varid(i),
     &                                      start, count1, bfr_out)
                        elseif (vartype(i) .eq. nf_int) then
                          ierr=nf_put_vara_int    (nctarg, varid(i),
     &                                      start, count1, bfr_out)
                        elseif (vartype(i) .eq. nf_float) then
                          ierr=nf_put_vara_real   (nctarg, varid(i),
     &                                      start, count1, bfr_out)
                        elseif (vartype(i) .eq. nf_double) then
                          ierr=nf_put_vara_double (nctarg, varid(i),
     &                                      start, count1, bfr_out)
                        endif
# else
                        if (vartype(i) .eq. nf_char) then
                          ierr=nf_put_vara_text   (nctarg, varid(i),
     &                                         start1, count, buff)
                        elseif (vartype(i) .eq. nf_int) then
                          ierr=nf_put_vara_int    (nctarg, varid(i),
     &                                         start1, count, buff)
                        elseif (vartype(i) .eq. nf_float) then
                          ierr=nf_put_vara_real   (nctarg, varid(i),
     &                                         start1, count, buff)
                        elseif (vartype(i) .eq. nf_double) then
                          ierr=nf_put_vara_double (nctarg, varid(i),
     &                                         start1, count, buff)
                        endif
# endif
                        if (ierr .ne. nf_noerr) then
                          lvar=lenstr(varname(i))
                          write(*,'(/8x,3A,I3/15x,3A,I3/)')
     &                      '### ERROR: Cannot write variable ''',
     &                      varname(i)(1:lvar), ''' for time record',
     &                      rec,    'into file ''',  ncname0(1:lstr),
     &                      '''. netCDF error status code =',  ierr
                          goto 97
                         endif
# ifdef FAST_MODE
                      endif  !<--  node.eq.nnodes-1
# endif
                    else
                      lvar=lenstr(varname(i))
                      lstr=lenstr(ncname(node))
                      write(*,'(/1x,4A,I3/12x,3A,I4/12x,A/)') '### ',
     &                    'ERROR: Cannot read variable ''',
     &                     varname(i)(1:lvar),  ''' for time record',
     &                     rec, 'from file ''', ncname(node)(1:lstr),
     &                    '''; netCDF error code =', ierr,
     &                                             nf_strerror(ierr)
                      goto 97
                    endif
                  endif  ! <-- var_mask 
                enddo       ! <-- node=0,nnodes-1
              endif
            endif
          enddo    !<--- i=1,nvars, loop over variables
          ierr=nf_sync (nctarg)
        enddo    !<--- rec, loop over records


!                                     ! At this moment open/closed
! Close all files                     ! status of partial files
!                                     ! depends on the state of CPP
  97    ierr=nf_close (nctarg)        ! switch KEEP_CLOSED. If the
        nctarg=-1                     ! switch is defined, then only
        do node=0,nnodes-1            ! node=0 file is expected to
          if (ncid(node).gt.-1) then  ! be opened here. Otherwise
            ierr=nf_close(ncid(node)) ! the entire set is opened and
            ncid(node)=-1             ! needs to be closed.  Either
          endif                       ! way, as ncid(node).eq/ne.-1
        enddo                         ! is used as flag indicating  
                                      ! status of each file.

       if (arg .lt. nargs)  goto 1  !--> next set of partial files.
      stop
      end

#endif   /* EXCLUDE_MAIN */
!
! The following four subroutines differ only by name and type of
! the first and the third argument, but their inner codes are
! identically the same. Use self-inclusion to generate multiple
! versions. 
!
#ifdef FAST_MODE
# if FUNC_TYPE == 3
      subroutine assembly_double (buff,count, bfr_out,start1,count1,
     &                                                      vardims)
      implicit none
      real*8 buff(*), bfr_out(*)
# elif FUNC_TYPE == 2 
      subroutine assembly_real (buff,count, bfr_out,start1,count1,
     &                                                    vardims)
      implicit none
      real*4 buff(*), bfr_out(*)
# elif FUNC_TYPE == 1 
      subroutine assembly_int (buff,count, bfr_out,start1,count1,
     &                                                   vardims)
      implicit none
      integer buff(*), bfr_out(*)
# else
      subroutine assembly_text (buff,count, bfr_out,start1,count1,
     &                                                    vardims)
      implicit none
      character buff(*), bfr_out(*)
# endif
      integer vardims, ndims
      integer count(vardims), start1(vardims), count1(vardims)
      integer i,        istr,imax,imax1,  j,js,js1, jstr,jmax,jmax1,
     &        k,ks,ks1, kstr,kmax,kmax1,  l,ls,ls1, lstr,lmax,lmax1
!
! Check whether the last dimension is unlimited dimension and if so,
! ignore it, since records corresponding to the different indices
! along unlimited dimension are always written one-by-one.
!
      if (count1(vardims).eq.1) then
        ndims=vardims-1 
      else
        ndims=vardims
      endif
      imax=count(1)              ! WARNING: This code is restricted
      istr=start1(1)             ! for partitioned arrays to have
      imax1=count1(1)            ! no more that 4 dimensions, not
      if (ndims.gt.1) then       ! counting unlimited dimension.
        jmax=count(2)
        jstr=start1(2)           ! Furthermore, it is assumed that
        jmax1=count1(2)          ! unlimited dimension is always
      else                       ! the last one [this is the standard
        jstr=1                   ! practice in ROMS, however netCDF
        jmax=1                   ! is not restricted to do so.
        jmax1=1
      endif
      if (ndims.gt.2) then
        kmax=count(3)
        kstr=start1(3)
        kmax1=count1(3)
      else
        kstr=1
        kmax=1
        kmax1=1
      endif
      if (ndims.gt.3) then
        lmax=count(4)
        lstr=start1(4)
        lmax1=count1(4)
      else
        lstr=1
        lmax=1
        lmax1=1
      endif
      if (ndims.gt.4) then
        write(*,'(/1x,2A/12x,A/)') '### ERROR: Exceeding limit of ',
     &                         '4 dimensions for partitioned array',
     &                      '[unlimited dimension does not count].'
        stop
      endif

c*    write(*,'(1x,A,2I3,3(3x,A,4I4))') 'ndims,vardims =', ndims,
c*   & vardims, 'imax1,jmax1,kmax1,lmax1 =', imax1,jmax1,kmax1,lmax1,
c*   &                  'imax,jmax,kmax,lmax =', imax,jmax,kmax,lmax,
c*   &                  'istr,jstr,kstr,lstr =', istr,jstr,kstr,lstr
  
      do l=1,lmax
        ls=l-1
        ls1=l+lstr-2
        do k=1,kmax
          ks=k-1 +ls*kmax
          ks1=k+kstr-2 + ls1*kmax1
          do j=1,jmax
            js=j-1 +ks*jmax
            js1=j+jstr-2 + ks1*jmax1
            do i=1,imax
              bfr_out(i+istr-1 + js1*imax1)=buff(i + js*imax)
            enddo
          enddo
        enddo
      enddo
      return
      end


# if FUNC_TYPE == 3
#  undef FUNC_TYPE
# elif FUNC_TYPE == 2
#  undef FUNC_TYPE
#  define FUNC_TYPE 3
# elif FUNC_TYPE == 1 
#  undef FUNC_TYPE
#  define FUNC_TYPE 2 
# else
#  define FUNC_TYPE 1
# endif
# ifdef FUNC_TYPE
#  include "ncjoin.F"
# endif
#endif /* FAST_MODE */
