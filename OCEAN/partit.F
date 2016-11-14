! $Id: partit.F 1458 2014-02-03 15:01:25Z gcambon $
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
      program partit 
!
! Generic netCDF partitioning tool: reads netCDF files corresonding
! to the whole physical grid and prepares multiple files which hold
! data corresponding to different subdomains. These files can be
! then read in parallel by different MPI processes. 
!
! Usage:  partit NP_XI NP_ETA ncname1 ... ncnameN
! ------  where NP_XI  number of subdomains along XI-direction
!               NP_ETA number of subdomains along ETA-direction
!               ncname1 ... ncnameN  names of netCDF files
!
! Non-partitionable objects of netCDF files, such as scalar variables
! and attributes (both global and attributes to variables) are copied
! redundantly into the partitioned files, while partitionable array
! data is subdivided into subdomains and distributed among the
! partitioned files in such a manner that all files contain
! individual data without any overlap or redundantly stored data.
!
! The partitioning algorithm works as follows: The partitionable
! dimensions ('xi_rho', 'xi_u', 'eta_rho' and 'eta_v') are identified
! by name, then then their values are read and compared in pairs to
! detect if any of the directions have periodicity. It is assumed
! that ghost points corresponding to physical boundaries are stored
! in the file, but computational margins (including periodic margins) 
! are not. Consequently, if xi_rho and xi_u are equal to each other, 
! XI-direction is periodic, and if they differ by one, it is not.
! ETA-direction is treated similarly. Once periodicity type is
! determined, the internal number of internal points in each
! direction (i.e. excluding ghost points corresponding to physical
! boundaries) id divided by the number of subdomains in that
! direction and then physical boundary points are attached to
! subdomains which are adjacent to the boundaries. This results in
! slightly different dimension sizes of netCDF files corresponding
! to diffeent subdomains.
!
! Once all dimensions are sorted out, data corresponding to
! subdomains is extracted from the source file and copied into
! partial files.
!
      implicit none
      integer stdout, max_buff_size, maxdims, maxvars, maxnodes
      parameter (stdout=6,           max_buff_size=300*300*30,
     &           maxdims=40,         maxvars=64,     maxnodes=512)
      character*120 ncname0, ncname(0:maxnodes-1), string 
      character*32 dimname(maxdims), varname(maxvars)
      logical XiPeriodic, EtaPeriodic, part_switch(maxvars),
     &                                           series(maxvars)
      real*8 buff(max_buff_size)
      integer narg,  NP_XI, NNODES,  xi_rho,  id_xi_rho,  id_xi_psi,
     &        arg,  NP_ETA,   node,  xi_u,    id_xi_u,    id_xi_v,
     &        iargc, subXI,     ii,  eta_rho, id_eta_rho, id_eta_psi,
     &        ierr, subETA,     jj,  eta_v,   id_eta_v,   id_eta_u,
     &        ndims, nvars, ngatts,  tsize,   unlimdimid, varatts,
     &        i,j,k,  lstr,   lvar,  lenstr,  rec,  size, ncid0, 
     &  ncid(0:maxnodes-1), dimid(maxdims),   dimsize(maxdims),
     &      varid(maxvars), vartype(maxvars), vardims(maxvars),
     &      start(maxdims), count(maxdims),   start1(maxdims),
     &      dimids(maxdims,maxvars),          ibuff(maxdims)
      common /partit_main/  buff,  ncid,  dimid,  dimsize, varid,
     &    vartype, vardims, start, count, start1, dimids,  ibuff
      integer LLm, MMm
      integer chunk_size_X, margin_X
      integer chunk_size_E, margin_E      
      integer istrmpi, jstrmpi
      integer iendmpi, jendmpi
      integer Lmmpi, Mmmpi
      
#include "netcdf.inc"
!
! Check how many arguments are given, complain about the error,
! if too few, otherwise extract NP_X and NP_E from the first two
! arguments.
!
      narg=iargc()
      if (narg .lt. 3) then
        write(stdout,'(/1x,A,1x,A/32x,A/)') 'Usage of partit',
     &    'should be:', 'partit NP_X NP_E ncname1 ... ncnameN'
        stop
      endif

      call getarg(1,string)
      lvar=lenstr(string)
      NP_XI=0
      do i=1,lvar
        j=ichar(string(i:i))-48
        if (j.ge.0 .and. j.le.9) then
          NP_XI=10*NP_XI+j
        else
          write(stdout,'(/8x,2(A,1x),2A/)') 'ERROR: illegal first',
     &     'argument', string(1:lvar), ', must be an integer number.'
          stop
        endif
      enddo

      call getarg(2,string)
      lvar=lenstr(string)
      NP_ETA=0
      do i=1,lvar
        j=ichar(string(i:i))-48
        if (j.ge.0 .and. j.le.9) then
          NP_ETA=10*NP_ETA+j
        else
          write(stdout,'(/8x,2(A,1x),2A/)') 'ERROR: illegal second',
     &      'argument', string(1:lvar), ', must be an integer number.'
          stop
        endif
      enddo
      NNODES=NP_XI*NP_ETA
      if (NNODES.gt.maxnodes) then
        write(stdout,'(/8x,A,1x,A,I3,1x,A/15x,A/)') 'ERROR:',
     &      'requested number of nodes',NNODES,'exceeds limit.',
     &      'Increase parameter maxnodes in file "partit.F".'
        stop
      endif
      write(stdout,'(/4x,2(4x,A,I3)/)') 'NP_XI =',  NP_XI,
     &                                  'NP_ETA =', NP_ETA
!
! Process netCDF files: open, determine if it is already
! a partitioned file, then make general inquiry. Complain
! about error if it already partitioned, or if number of
! variables and/or dimensions exceeds specified limits.
!
      do arg=3,narg
        call getarg(arg,ncname0)
        lstr=lenstr(ncname0)
        ierr=nf_open (ncname0(1:lstr), nf_nowrite, ncid0)
        if (ierr .ne. nf_noerr) then
          write(stdout,'(/8x,A,1x,A,1x,A,A/)') 'ERROR: Cannot',
     &                  'open netCDF file', ncname0(1:lstr),'.'
          goto 97     !--> next file
        endif
        ierr=nf_inq_att (ncid0, nf_global, 'partition', i,j)
        if (ierr .eq. nf_noerr) then
          write(stdout,'(/8x,3A/17x,A/)') 'WARNING: netCDF file ''',
     &                 ncname0(1:lstr), ''' is already partitioned',
     &                 'file. It cannot be partitioned any further.'
          goto 97     !--> next file
        endif

        write(stdout,'(8x,3A)') 'Processing netCDF file ''',
     &                               ncname0(1:lstr), '''...'

        ierr=nf_inq (ncid0, ndims, nvars, ngatts, unlimdimid)
        if (ierr .ne. nf_noerr) then
           write(stdout,'(/8x,A,1x,A/15x,A,1x,A,A/)') 'ERROR:',
     &        'Cannot determine number of dimensions, variables',
     &        'and attributes in netCDF file',ncname0(1:lstr),'.'
          goto 97     !--> next file
        elseif (ndims .gt. maxdims) then
          write(stdout,'(/8x,A,I4,1x,4A/15x,A,1x,A/)')
     &        'ERROR: number of dimensions', ndims,  'in netCDF',
     &        'file ''', ncname0(1:lstr), '''', 'exceeds limit.',
     &        'Increase parameter maxdims in file "partit.F".'
          goto 97     !--> next file
         elseif (nvars .gt. maxvars) then
          write(stdout,'(/8x,A,I4,1x,4A/15x,A,1x,A/)')
     &        'ERROR: number of variables',  nvars,  'in netCDF',
     &        'file ''', ncname0(1:lstr), '''', 'exceeds limit.',
     &        'Increase parameter maxvars in file "partit.F".'
          goto 97     !--> next file
        endif
!
! Sort out dimensions: For each dimension find and save its name and
! size. Then check whether all partitionable dimensions (identified
! by names 'xi_rho', 'xi_u', 'eta_rho' and 'eta_v')  are present and
! save their IDs and sizes. 
!
        tsize=1      ! <-- default value.
        do i=1,ndims
          ierr=nf_inq_dimname (ncid0, i, dimname(i))
          if (ierr .ne. nf_noerr) then
             write(stdout,'(/8x,A,I3/15x,3A/)')
     &           'ERROR: Cannot determine name for dimension ID =',
     &            i,  'in netCDF file ''',  ncname0(1:lstr),  '''.' 
             goto 97     !--> next file
          endif
          ierr=nf_inq_dimlen  (ncid0, i, dimsize(i))
          if (ierr .ne. nf_noerr) then
             lvar=lenstr(dimname(i))
             write(stdout,'(/8x,A,A,A/15x,3A/)')
     &            'ERROR: Cannot determine length of dimension ''',
     &             dimname(i)(1:lvar),  '''',  'in netCDF file ''',
     &                                       ncname0(1:lstr), '''.'
             goto 97     !--> next file
          endif
          if (i.eq. unlimdimid) then
            tsize=dimsize(i)
            dimsize(i)=nf_unlimited
          endif
        enddo
!
! Determine IDs and sizes of partitionable dimensions, 'xi_rho',
! 'xi_u', 'eta_rho' and 'eta_v'. Also save IDs of obsolete dimensions
! 'xi_psi', 'xi_v', 'eta_psi' and and 'eta_u'. These are used to
! readress obsolete dimensions according to the rules:
!
        id_xi_rho=0                     ! xi_psi  --> xi_u
        id_xi_u=0                       ! xi_v    --> xi_rho
        id_eta_rho=0                    ! eta_psi --> eta_v
        id_eta_v=0                      ! eta_u   --> eta_rho 
        id_xi_psi=0
        id_xi_v=0
        id_eta_psi=0
        id_eta_u=0
        do i=1,ndims
          lvar=lenstr(dimname(i))
          if (lvar.eq.6 .and. dimname(i)(1:lvar).eq.'xi_rho') then
            id_xi_rho=i
            xi_rho=dimsize(i)
          elseif (lvar.eq.4 .and. dimname(i)(1:lvar).eq.'xi_u') then
            id_xi_u=i
            xi_u=dimsize(i)
          elseif (lvar.eq.7.and.dimname(i)(1:lvar).eq.'eta_rho') then
            id_eta_rho=i
            eta_rho=dimsize(i)
          elseif (lvar.eq.5 .and. dimname(i)(1:lvar).eq.'eta_v') then
            id_eta_v=i
            eta_v=dimsize(i)
          elseif (lvar.eq.6 .and.dimname(i)(1:lvar).eq.'xi_psi') then
            id_xi_psi=i
          elseif (lvar.eq.4 .and. dimname(i)(1:lvar).eq.'xi_v') then
            id_xi_v=i
          elseif (lvar.eq.7.and.dimname(i)(1:lvar).eq.'eta_psi') then
            id_eta_psi=i
          elseif (lvar.eq.5 .and. dimname(i)(1:lvar).eq.'eta_u') then
            id_eta_u=i
          endif
c**       write(*,'(I3,1x,A,T16,I3)') i,dimname(i)(1:lvar),dimsize(i)
        enddo
        if (id_xi_rho.eq.0  .or. id_xi_u.eq.0 .or.
     &      id_eta_rho.eq.0 .or. id_eta_v.eq.0) then
          write(stdout,'(/8x,2A/15x,3A/)') 'ERROR: not all ',
     &            'partitionable dimensions are found',
     &            'in netCDF file ''', ncname0(1:lstr), '''.'
          goto 97     !--> next file 
        endif
!
! Determine subdomain dimensions. Here "subXI" and "subETA" are
! the nimbers of internal grid points within each subdomains (that
! is, excluding physical boundary points and computational margins). 
! The number of internal points in either direction for the the
! whole computational domain must be divisible by NP_XI and NP_ETA  
! respectively. If it cannot be divided, complain about the error
! and exit.
!
        LLm = xi_rho - 2
        MMm = eta_rho - 2
        if (xi_rho .eq. xi_u) then
          XiPeriodic=.true.
          subXI=xi_rho/NP_XI
          if (subXI*NP_XI .ne. xi_rho) then
            write(stdout,'(/8x,A,1x,A,I4,1x,A/15x,A,I3,1x,A/)')
     &         'ERROR: Cannot partition XI-direction:', 'xi_rho =',
     &          xi_rho,  'is',  'not divisible by NP_XI =',  NP_XI,
     &         'in XI-periodic case.'

            goto 97
          endif
        elseif (xi_rho .eq. xi_u+1) then 
          XiPeriodic=.false.
          subXI=(xi_rho-2)/NP_XI
          chunk_size_X=(LLm+NP_XI-1)/NP_XI
          margin_X=(NP_XI*chunk_size_X-LLm)/2
          
C          if (subXI*NP_XI .ne. xi_rho-2) then
C            write(stdout,'(/8x,A,1x,A,I4,1x,A/15x,A,I3,1x,A/)')
C     &         'ERROR: Cannot partition XI-direction:', 'xi_rho-2 =',
C     &          xi_rho-2,  'is',  'not divisible by NP_XI =',  NP_XI,
C     &         'in nonperiodic XI case.'
C            goto 97
C          endif
        else
          write(stdout,'(/8x,2A/)') 'ERROR: inconsistent ',
     &                'dimensions ''xi_rho'' and ''xi_u''.' 
          goto 97     !--> next file
        endif

        if (eta_rho .eq. eta_v) then
          EtaPeriodic=.true.
          subETA=eta_rho/NP_ETA
      
          if (subETA*NP_ETA .ne. eta_rho) then
            write(stdout,'(/8x,A,1x,A,I4,1x,A/15x,A,I3,1x,A/)')
     &         'ERROR: Cannot partition ETA-direction:', 'eta_rho =',
     &          eta_rho,  'is',  'not divisible by NP_ETA =', NP_ETA,
     &         'in ETA-periodic case.'

            goto 97
          endif
        elseif (eta_rho .eq. eta_v+1) then
          EtaPeriodic=.false.
          subETA=(eta_rho-2)/NP_ETA
          
          chunk_size_E=(MMm+NP_ETA-1)/NP_ETA
          margin_E=(NP_ETA*chunk_size_E-MMm)/2          

C          if (subETA*NP_ETA .ne. eta_rho-2) then
C            write(stdout,'(/8x,A,1x,A,I4,1x,A/15x,A,I3,1x,A/)')
C     &        'ERROR: Cannot partition ETA-direction:','eta_rho-2 =',
C     &         eta_rho-2, 'is',  'not divisible by NP_ETA =', NP_ETA,
C     &        'in nonperiodic ETA case.'
C            goto 97
C          endif
        else
          write(stdout,'(/8x,A,1x,A/)') 'ERROR: inconsistent',
     &                 'dimensions ''eta_rho'' and ''eta_v''.'
          goto 97     !--> next file
        endif
!
! Create partitioned files.
! ====== =========== ======
!
        do node=0,NNODES-1
          lstr=lenstr(ncname0)
          ncname(node)=ncname0
          ierr=0
          call insert_node (ncname(node), lstr, node, NNODES, ierr)
          if (ierr. ne. 0) goto 97     !--> next file
!          ierr=nf_create (ncname(node)(1:lstr),nf_clobber,ncid(node))
          ierr=nf_create (ncname(node)(1:lstr),nf_64bit_offset,ncid(node))
          if (ierr .eq. nf_noerr) then
            write(stdout,'(12x,3A)') 'Created partitioned file ''',
     &                                  ncname(node)(1:lstr), '''.' 
          else
            write(stdout,'(/8x,A,1x,3A/)') 'ERROR: cannot create',
     &              'netCDF file ''', ncname(node)(1:lstr), '''.'
            goto 97     !--> next file
          endif
!
! Define dimensions of partitioned files.
!
          jj=node/NP_XI
          ii=node-jj*NP_XI
          
      istrmpi=1+ii*chunk_size_X-margin_X
      iendmpi=istrmpi+chunk_size_X-1
      istrmpi=max(istrmpi,1)
      iendmpi=min(iendmpi,LLm)

      jstrmpi=1+jj*chunk_size_E-margin_E
      jendmpi=jstrmpi+chunk_size_E-1
      jstrmpi=max(jstrmpi,1)
      jendmpi=min(jendmpi,MMm)     
 
      Lmmpi=iendmpi-istrmpi+1
      Mmmpi=jendmpi-jstrmpi+1
      
          do i=1,ndims
            size=dimsize(i)
            if (i .eq. id_xi_rho) then
              size=Lmmpi
              if (.not.XiPeriodic) then
                if (ii.eq.0      ) size=size+1
                if (ii.eq.NP_XI-1) size=size+1
              endif
            elseif (i .eq. id_xi_u) then
              size=Lmmpi
              if (.not.XiPeriodic) then
                if (ii.eq.NP_XI-1) size=size+1 
              endif
            elseif (i .eq. id_eta_rho) then
              size=Mmmpi
              if (.not.EtaPeriodic) then
                if (jj.eq.0       ) size=size+1 
                if (jj.eq.NP_ETA-1) size=size+1 
              endif
            elseif (i .eq. id_eta_v) then
              size=Mmmpi
              if (.not.EtaPeriodic) then
                if (jj.eq.NP_ETA-1) size=size+1 
              endif
            endif
            if (i.ne.id_xi_psi   .and.  i.ne.id_xi_v  .and. 
     &          i.ne.id_eta_psi  .and.  i.ne.id_eta_u)  then
              lvar=lenstr(dimname(i))
              ierr=nf_def_dim (ncid(node), dimname(i)(1:lvar),
     &                                         size, dimid(i))
              if (ierr .ne. nf_noerr) then
                write(stdout,'(/8x,4A/15x,A,I4,A)') 'ERROR: ',
     &            'Cannot define dimension ''', dimname(i)(1:lvar),
     &            '''.',    'netCDF ettor status =',       ierr, '.'
              endif
c**           write(*,'(2I3,4x,2I3,I4,1x,A)') i, dimid(i), ii,
c**  &                            jj, size, dimname(i)(1:lvar)
            else
              dimid(i)=0
            endif
          enddo
!
! WARNING!!! ...After this moment array dimid(1:ndims) contains
! the set of NEW dimension IDs. Since the four dimensions, 'xi_psi',
! 'eta_psi', 'xi_v' and 'eta_u' have been eliminated, dimid(i) does
! not correspond to the set of dimension IDs of the original file
! [which would be just dimid(i)=i], but it is rather different.
! Array dimid(1:ndims) will be used later to remap old dimension
! IDs into new ones, see the remapping procedure approximately 80
! lines below.
!
! Put global attribute 'partition' which identifies subdomain
! within the processor grid individually for each file.
!
          ibuff(1)=ii
          ibuff(2)=jj
          ibuff(3)=NP_XI
          ibuff(4)=NP_ETA
          ierr=nf_put_att_int (ncid(node), nf_global, 'partition',
     &                                           nf_int, 4, ibuff)
        enddo
!
! Copy global attributes
!
        do i=1,ngatts 
          ierr=nf_inq_attname (ncid0, nf_global, i, string) 
          if (ierr. eq. nf_noerr) then
            lvar=lenstr(string)
            do node=0,NNODES-1
              ierr=nf_copy_att (ncid0, nf_global, string(1:lvar),
     &                                     ncid(node), nf_global)
              if (ierr. ne. nf_noerr) then
                lstr=lenstr(ncname(node))
                write(stdout,'(/8x,4A/15x,3A/)')  'ERROR: Cannot ',
     &            'copy global attribute ''', string(1:lvar), '''',
     &            'into netCDF file ''', ncname(node)(1:lstr),'''.'
                goto 97
              endif
            enddo
          else
            lstr=lenstr(ncname(0))
            write(stdout,'(/8x,2A,I3/15x,3A/)') 'ERROR: Cannot ',
     &         'determine mame of global attribute with ID =', i,
     &         'from netCDF file ''',    ncname0(1:lstr),   '''.'
            goto 97
          endif
        enddo
!
! Define variables and their attributes.
!
        do i=1,nvars
          ierr=nf_inq_var (ncid0,   i, varname(i),  vartype(i),
     &                      vardims(i), dimids(1,i),   varatts)
!
! Readress obsolete dimensions, if any:
!
          do j=1,vardims(i)
            if (dimids(j,i).eq.id_xi_psi) then
              dimids(j,i)=id_xi_u
            elseif (dimids(j,i).eq.id_xi_v) then
              dimids(j,i)=id_xi_rho
            elseif (dimids(j,i).eq.id_eta_psi) then
              dimids(j,i)=id_eta_v
            elseif (dimids(j,i).eq.id_eta_u) then
              dimids(j,i)=id_eta_rho
            endif
          enddo
!
! Determine whether partitionable dimensions or unlimited dimension
! are present for this variable. 
!
          series(i)=.false.
          part_switch(i)=.false.
          do j=1,vardims(i)
            if (dimids(j,i).eq.id_xi_rho .or.
     &          dimids(j,i).eq.id_xi_u    .or.
     &          dimids(j,i).eq.id_eta_rho .or.
     &          dimids(j,i).eq.id_eta_v) then
              part_switch(i)=.true. 
            elseif (dimids(j,i).eq.unlimdimid) then
              series(i)=.true. 
            endif
          enddo
!
! WARNING: Since dimids(1:vardims(i),i) contains dimension IDs
! corresponding to the set of IDs of the ORIGINAL file, and since
! some of the original dimensions were eliminated (merged), the
! set of dimension IDs in the NEW definitions is obtained by 
! inverse mapping of dimids(j,i) onto ibuff(j) using dimid(k) as
! a mapping array.  
!
          do j=1,vardims(i)
            do k=1,ndims
              if (dimids(j,i).eq.k) ibuff(j)=dimid(k)
            enddo
          enddo
c**       write(*,*) 'old_dimids:', (dimids(j,i),j=1,vardims(i))
c**       write(*,*) 'new_dimids:',    (ibuff(j),j=1,vardims(i))

          lvar=lenstr(varname(i))
          do node=0,NNODES-1
            ierr=nf_def_var (ncid(node), varname(i)(1:lvar),
     &              vartype(i), vardims(i), ibuff, varid(i))
          enddo
c**       write(stdout,'(I3,1x,A,T20,I3,1x,L1,1x,L1)') i,
c**  &                        varname(i)(1:lvar), vardims(i),
c**  &                            part_switch(i),  series(i)
          do j=1,varatts
            ierr=nf_inq_attname (ncid0, varid(i), j, string)
            lvar=lenstr(string)
            do node=0,NNODES-1
            ierr=nf_copy_att (ncid0, i, string(1:lvar),
     &                            ncid(node), varid(i))
            enddo
          enddo
        enddo
!
! Leave definition mode
!
        do node=0,NNODES-1
c          ierr=nf_set_fill (ncid(node), nf_nofill, i)
          ierr=nf_enddef(ncid(node))
        enddo
!
! Transfer variables into newly created files. 
!
        do rec=1,tsize
          if (tsize.gt.1) write(stdout,'(16x,A,I4,A)')
     &                 'Processing record', rec, '...'
          do i=1,nvars
            if (series(i) .or. rec.eq.1) then
              if (.not.part_switch(i) .and. .not.series(i)) then
!
! Scalar (zero-dimensional) variables:
!
                if (vartype(i) .eq. nf_char) then
                  ierr=nf_get_var_text (ncid0, i, buff)
                elseif (vartype(i) .eq. nf_int) then
                  ierr=nf_get_var_int    (ncid0, i, buff)
                elseif (vartype(i) .eq. nf_real) then
                  ierr=nf_get_var_real   (ncid0, i, buff)
                elseif (vartype(i) .eq. nf_double) then
                  ierr=nf_get_var_double (ncid0, i, buff)
                else
                  lvar=lenstr(varname(i))
                  write(stdout,'(/8x,4A/)') 'ERROR: scalar variable',
     &              ' ''', varname(i)(1:lvar), ''' has unknown type.'
                  goto 97
                endif
                if (ierr .eq. nf_noerr) then
                  do node=0,NNODES-1
                    if (vartype(i) .eq. nf_char) then
                     ierr=nf_put_var_text (ncid(node),varid(i),buff)
                    elseif (vartype(i) .eq. nf_int) then
                     ierr=nf_put_var_int   (ncid(node),varid(i),buff)
                    elseif (vartype(i) .eq. nf_real) then
                     ierr=nf_put_var_real  (ncid(node),varid(i),buff)
                    elseif (vartype(i) .eq. nf_double) then
                     ierr=nf_put_var_double(ncid(node),varid(i),buff)
                    endif
                    if (ierr .ne. nf_noerr) then
                      lvar=lenstr(varname(i))
                      lstr=lenstr(ncname(node))
                      write(stdout,'(/8x,3A/15x,3A,I4,A/)')
     &                    'ERROR: Cannot write scalar variable ''',
     &                     varname(i)(1:lvar), ''' into netCDF file',
     &                    '''',  ncname(node)(1:lstr), 
     &                    '''.     netCDF error code =',  ierr,   '.'
                      goto 97
                    endif
                  enddo
                else
                  lvar=lenstr(varname(i))
                  write(stdout,'(/8x,4A/)') 'ERROR: Cannot read ',
     &             'scalar variable ''', varname(i)(1:lvar), '''.'
                  goto 97
                endif
              elseif (.not.part_switch(i)) then
!
! Non-partitionable array.
!
                size=1
                do j=1,vardims(i) 
                  if (dimids(j,i).eq.unlimdimid) then
                    start(j)=rec
                    count(j)=1
                  else
                    start(j)=1
                    count(j)=dimsize(dimids(j,i))
                  endif 
                  size=size*count(j)
                enddo
                if (vartype(i) .eq. nf_char) then
                  size=size*1
                elseif (vartype(i) .eq. nf_int) then
                  size=size*4
                elseif (vartype(i) .eq. nf_real) then
                  size=size*4
                elseif (vartype(i) .eq. nf_double) then
                  size=size*8
                else
                  lvar=lenstr(varname(i))
                  write(stdout,'(/8x,3A/)') 'ERROR: variable ''',
     &                 varname(i)(1:lvar), ''' has unknown type.'
                  goto 97
                endif
                if (size .gt. 8*max_buff_size) then
                  write(stdout,'(/8x,A,3(/15x,A,I10,1x,A)/)')
     &              'ERROR: unsufficient buffer size in "partit.F":',
     &              'requested:',         size,      'Bytes,',
     &              'available:',   8*max_buff_size, 'Bytes.',
     &              'Increase parameter max_buff_size and recompile.'
                  goto 97
                endif

                if (vartype(i) .eq. nf_char) then
                  ierr=nf_get_vara_text   (ncid0, i, start,
     &                                          count, buff)
                elseif (vartype(i) .eq. nf_int) then
                  ierr=nf_get_vara_int    (ncid0, i, start,
     &                                         count, buff)
                elseif (vartype(i) .eq. nf_real) then
                  ierr=nf_get_vara_real   (ncid0, i, start,
     &                                         count, buff)
                elseif (vartype(i) .eq. nf_double) then
                  ierr=nf_get_vara_double (ncid0, i, start,
     &                                         count, buff)
                endif
                if (ierr .eq. nf_noerr) then
                  do node=0,NNODES-1
                    if (vartype(i) .eq. nf_char) then
                      ierr=nf_put_vara_text   (ncid(node), varid(i),
     &                                           start, count, buff)
                    elseif (vartype(i) .eq. nf_int) then
                      ierr=nf_put_vara_int    (ncid(node), varid(i),
     &                                           start, count, buff)
                    elseif (vartype(i) .eq. nf_real) then
                      ierr=nf_put_vara_real   (ncid(node), varid(i),
     &                                           start, count, buff)
                    elseif (vartype(i) .eq. nf_double) then
                      ierr=nf_put_vara_double (ncid(node), varid(i),
     &                                           start, count, buff)
                    endif
                    if (ierr .ne. nf_noerr) then
                      lvar=lenstr(varname(i))
                      lstr=lenstr(ncname(node))
                      write(stdout,'(/8x,3A,I3/15x,3A,I4,A/)')
     &                  'ERROR: Cannot write variable ''',
     &                   varname(i)(1:lvar),''' for time record',rec,
     &                  'into netCDF file ''',  ncname(node)(1:lstr),
     &                  '''. netCDF error code =', ierr, '.'
                      goto 97
                    endif
                  enddo
                else
                  lstr=lenstr(ncname0)
                  lvar=lenstr(varname(i))
                  write(stdout,'(/8x,4A,I3/15x,3A,I4/)') 'ERROR: ',
     &              'Cannot read variable ''',  varname(i)(1:lvar),
     &              ''' for time record',rec,'from netCDF file ''',
     &              ncname0(1:lstr),'''. netCDF error code =',ierr
                  goto 97
                endif
              elseif (part_switch(i)) then
!
! Partitioned array: 
!
                do node=0,NNODES-1
                  jj=node/NP_XI
                  ii=node-jj*NP_XI
                  
      istrmpi=1+ii*chunk_size_X-margin_X
      iendmpi=istrmpi+chunk_size_X-1
      istrmpi=max(istrmpi,1)
      iendmpi=min(iendmpi,LLm)

      jstrmpi=1+jj*chunk_size_E-margin_E
      jendmpi=jstrmpi+chunk_size_E-1
      jstrmpi=max(jstrmpi,1)
      jendmpi=min(jendmpi,MMm)     
 
      Lmmpi=iendmpi-istrmpi+1
      Mmmpi=jendmpi-jstrmpi+1
      
                  size=1
                  do j=1,vardims(i)
                    if (dimids(j,i).eq.id_xi_rho) then
                      start(j)=istrmpi
                      count(j)=Lmmpi
                      if (.not.XiPeriodic) then
                        if (ii.gt.0      ) start(j)=start(j)+1
                        if (ii.eq.0      ) count(j)=count(j)+1
                        if (ii.eq.NP_XI-1) count(j)=count(j)+1
                      endif
                      start1(j)=1

                    elseif (dimids(j,i).eq.id_xi_u) then
                      start(j)=istrmpi
                      count(j)=Lmmpi
                      if (.not.XiPeriodic) then
                        if (ii.eq.NP_XI-1) count(j)=count(j)+1
                      endif
                      start1(j)=1

                    elseif (dimids(j,i).eq.id_eta_rho) then
                      start(j)=jstrmpi
                      count(j)=Mmmpi
                      if (.not.EtaPeriodic) then
                        if (jj.gt.0       ) start(j)=start(j)+1
                        if (jj.eq.0       ) count(j)=count(j)+1
                        if (jj.eq.NP_ETA-1) count(j)=count(j)+1
                      endif
                      start1(j)=1

                    elseif (dimids(j,i).eq.id_eta_v) then
                      start(j)=jstrmpi
                      count(j)=Mmmpi
                      if (.not.EtaPeriodic) then
                        if (jj.eq.NP_ETA-1) count(j)=count(j)+1
                      endif
                      start1(j)=1

                    elseif (dimids(j,i).eq.unlimdimid) then
                      start(j)=rec
                      count(j)=1
                      start1(j)=rec
                    else
                      start(j)=1
                      count(j)=dimsize(dimids(j,i))
                      start1(j)=1
                    endif
                    size=size*count(j)
                  enddo
c**               write(*,*) 'dimids:', (dimids(j,i),j=1,vardims(i))
c**               write(*,*) ' start:',    (start(j),j=1,vardims(i))
c**               write(*,*) ' count:',    (count(j),j=1,vardims(i))


                  if (vartype(i) .eq. nf_char) then
                    size=size*1
                  elseif (vartype(i) .eq. nf_int) then
                    size=size*4
                  elseif (vartype(i) .eq. nf_real) then
                    size=size*4
                  elseif (vartype(i) .eq. nf_double) then
                    size=size*8
                  else
                    lvar=lenstr(varname(i))
                    write(stdout,'(/8x,4A/)') 'ERROR: variable ''',
     &                   varname(i)(1:lvar), ''' has unknown type.'
                    goto 97
                  endif
                  if (size .gt. 8*max_buff_size) then
                    write(stdout,'(/8x,A,3(/15x,A,I10,1x,A)/)')
     &              'ERROR: unsufficient buffer size in "partit.F":',
     &              'requested:',         size,      'Bytes,',
     &              'available:',   8*max_buff_size, 'Bytes.',
     &              'Increase parameter max_buff_size and recompile.'
                    goto 97
                  endif

                  if (vartype(i) .eq. nf_char) then
                    ierr=nf_get_vara_text   (ncid0, i, start,
     &                                           count, buff)
                  elseif (vartype(i) .eq. nf_int) then
                    ierr=nf_get_vara_int    (ncid0, i, start,
     &                                           count, buff)
                  elseif (vartype(i) .eq. nf_real) then
                    ierr=nf_get_vara_real   (ncid0, i, start,
     &                                           count, buff)
                  elseif (vartype(i) .eq. nf_double) then
                    ierr=nf_get_vara_double (ncid0, i, start,
     &                                           count, buff)
                  endif

                  if (ierr .eq. nf_noerr) then
                    if (vartype(i) .eq. nf_char) then
                      ierr=nf_put_vara_text   (ncid(node), varid(i),
     &                                          start1, count, buff)
                    elseif (vartype(i) .eq. nf_int) then
                      ierr=nf_put_vara_int    (ncid(node), varid(i),
     &                                          start1, count, buff)
                    elseif (vartype(i) .eq. nf_real) then
                      ierr=nf_put_vara_real   (ncid(node), varid(i),
     &                                          start1, count, buff)
                    elseif (vartype(i) .eq. nf_double) then
                      ierr=nf_put_vara_double (ncid(node), varid(i),
     &                                          start1, count, buff)
                    endif
                    if (ierr .ne. nf_noerr) then
                      lvar=lenstr(varname(i))
                      lstr=lenstr(ncname(node))
                      write(stdout,'(/8x,3A,I3/15x,3A,I4,A/)')
     &                  'ERROR: Cannot write partitioned array ''',
     &                   varname(i)(1:lvar),''' for time record',rec,
     &                  'into netCDF file ''',  ncname(node)(1:lstr),
     &                  '''. netCDF error code =', ierr, '.'
                      goto 97
                    endif
                  else
                    lstr=lenstr(ncname0)
                    lvar=lenstr(varname(i))
                    write(stdout,'(/8x,3A,I3/15x,3A,I4,A/)')
     &                  'ERROR: Cannot read partitioned array ''',
     &                   varname(i)(1:lvar),   ''' for time record',
     &                   rec, 'from netCDF file ''',ncname0(1:lstr),
     &                  '''. netCDF error code =',   ierr,    '.'

                    goto 97
                  endif
                enddo       ! <-- node=0,NNODES-1
              endif
            endif       ! <--series(i) .or. rec.eq.1
          enddo       ! <-- i=1,nvars
        enddo       ! <-- rec=1,tsize
!
! Close all netCDF files
!
  97    ierr=nf_close (ncid0)
        do node=0,NNODES-1
         ierr=nf_close (ncid(node))
        enddo
      enddo
      stop
      end

