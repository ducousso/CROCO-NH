! $Id: ncjoin.F 1458 2014-02-03 15:01:25Z gcambon $
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
      program ncjoin 
!
! Generic netCDF assembly tool: reads partitioned netCDF files and
! assembles them into file corresponding to the whole corresonding
! to the whole physical grid. This operation is inverse to partit.
!
      implicit none
      integer stdout, maxdims, maxvars, maxnodes
      parameter (stdout=6, maxdims=16, maxvars=64, maxnodes=512)
      character*120 ncname0, ncname(0:maxnodes-1), string
      character root*64, root_bak*64, sffx*8, sffx_bak*8
      character*32 dimname(maxdims), varname(maxvars)
      logical complete, digits, part_switch(maxvars),series(maxvars)
      integer narg, arg, iargc, NNODES, NP_XI, NP_ETA, i,j,k,m, ierr,
     &   lstr, lvar, lbak, lenstr, code_size, code_size_bak, node,
     &   ii(0:maxnodes), jj(0:maxnodes),  ncid0,  ncid(0:maxnodes),
     &   dimsize(maxdims,0:maxnodes), dimid(maxdims), varid(maxvars),
     &   vartype(maxvars), vardims(maxvars), dimids(maxdims,maxvars),
     &   ndims, nvars, ngatts, tsize, unlimdimid, varatts, size,
     &   id_xi_rho, id_xi_u, id_eta_rho, id_eta_v, rec,
     &   ibuff(maxdims),start(maxdims),count(maxdims),start1(maxdims)

      integer max_buff_size
      parameter (max_buff_size=300*300*30)
      real*8 buff(max_buff_size)
#include "netcdf.inc"



      narg=iargc()
      arg=0
!
! Extract a set of files which cover the whole physical grid.
!
  1   NP_XI=-1                ! Reset variables which define
      NP_ETA=-1               ! partitioning policy: NP_XI,NP_ETA 
      do node=0,maxnodes-1    ! which are the dimensions of the 
        ii(node)=-1           ! processor grid as well as ii,jj 
        jj(node)=-1           ! (node) which are indices of node 
        ncid(node)=-1         ! on the processor grid. All these 
      enddo                   ! variables will be extracted from 
      root_bak(1:1)=' '       ! netCDF files and completness of 
      sffx_bak(1:1)=' '       ! all components will be verified. 
      code_size_bak=-1
  
  2    arg=arg+1
        call getarg(arg,ncname0)
        lstr=lenstr(ncname0)
        ierr=nf_open (ncname0(1:lstr), nf_nowrite, ncid0)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_att (ncid0, nf_global, 'partition', i,lvar)
          if (ierr .eq. nf_noerr) then
            if (i.eq.nf_int .and. lvar.eq.4) then
              ierr=nf_get_att_int (ncid0,nf_global,'partition',ibuff)
              if (ierr .eq. nf_noerr) then
                if (NP_XI.eq.-1 .and. NP_ETA.eq.-1) then
                  NP_XI =ibuff(3)
                  NP_ETA=ibuff(4)
                elseif (NP_XI  .ne. ibuff(3)  .or.
     &                  NP_ETA .ne. ibuff(4)) then 
                  write(stdout,'(/8x,4A/2(17x,A,I2,A,I2,A/))')
     &                 'WARNING: Partitioning structure of netCDF',
     &                 'file ''', ncname0(1:lstr), ''',',
     &                 'NP_XI,NP_ETA =', ibuff(3),  ',',  ibuff(4),
     &                 ' conflicts with previously discovered',
     &                 'NP_XI,NP_ETA =',  NP_XI,  ',',  NP_ETA, '.'
                  arg=arg-1
                  goto 5
                endif
                i=ibuff(1)
                j=ibuff(2)
                node=i+j*NP_XI
                if (ii(node).eq.-1 .and. jj(node).eq.-1) then
                  ii(node)=i
                  jj(node)=j
                  ncid(node)=ncid0
                  ncname(node)=ncname0
!
! Lexical analysis of the file name: It is assumed that name of the
! file consists of root name (eg.: "history"); integer number which
! contains MPI node number (eg.: 03) and; suffix (eg.: ".nc").
! Files which belong to the same set normally have the same (1) root
! and (2) suffix names; the same (3) number of digits in the MPI node
! number segment in the filename and; (4) MPI node number from the
! file name should match the number determined from global attribute
! 'partition'. 
!
                  root(1:1)=' '               ! Split file name into 
                  digits=.true.               ! root, MPI node number
                  i=lstr                      ! and suffix, if any.
                  j=0
   3              k=ichar(ncname0(i:i))
                  if (ncname0(i:i).eq.'.' .and. .not.digits) then
                    digits=.true.
                    j=i
                    sffx=ncname0(j:lstr)
                  elseif (ncname0(i:i).eq.'.' .and. digits) then
                    root=ncname0(1:i-1)
                  elseif (k.lt.48 .or. k.gt.57) then
                    digits=.false.
                  endif  
                  if (root(1:1).eq.' ' .and. i.gt.1) then
                    i=i-1
                    goto 3
                  endif

                  if (j.gt.0) then            ! Setting suffix name,
                    sffx=ncname0(j:lstr)      ! if there is a suffix.
                  else                        !
                    sffx(1:1)=' '
                  endif
 
                  code_size=max(0,j-i-1)      ! Determine MPI node
                  k=0                         ! number according to 
   4              i=i+1                       ! the file name. 
                  if (i.lt.j) then
                    k=10*k+ichar(ncname0(i:i))-48
                    goto 4
                  endif
c**               write(*,'(2I3,1x,A)') code_size, k, ncname0(1:lstr)
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
                      write(stdout,'(/8x,7A/17x,3A/)') 'WARNING: ',
     &                  'file ''',    ncname0(1:lstr),   ''' has ',
     &                  'different root name ''',     root(1:lvar),
     &                  ''' than', 'previously found root name ''',
     &                  root_bak(1:lbak),   ''' from the same set.'
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
                      write(stdout,'(/8x,7A/17x,3A/)') 'WARNING: ',
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
                    write(stdout,'(/8x,A,I2,1x,A/17x,3A,I2,A/)')
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
                    write(stdout,'(/8x,3A,I3/17x,2A/17x,A,I3,A/)')
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
                  write(stdout,'(/8x,3A/17x,A,I2,A,I2,A/)')
     &               'WARNING: netCDF file ''',    ncname0(1:lstr),
     &               ''' corresponds to subdomain','ii,jj =',i,',',
     &               j,  ' which already defined in another file.'
                  arg=arg-1
                  goto 5
                endif
              else 
                write(stdout,'(/8x,2A/17x,4A/)') 'WARNING: Cannot ',
     &                'aquire global attribute ''partition'' from ',
     &                'netCDF file ''',  ncname0(1:lstr),    '''. ',
     &                'This file is ignored.'
              endif
            else
              write(stdout,'(/8x,2A/17x,3A/)') 'WARNING: ',
     &          'incompatible type and/or size of global attribute',
     &          '''partition'' in netCDF file ''',  ncname0(1:lstr),
     &          '''. This file is ignored.'
            endif
          else
            write(stdout,'(/8x,3A/17x,A/)')'WARNING: netCDF file ''',
     &              ncname0(1:lstr), ''' is not a partitioned file,',
     &                                        'This file is ignored.'
          endif 
        else
          write(stdout,'(/8x,A,1x,3A/)') 'WARNING: Cannot open',
     &                  'netCDF file ''', ncname0(1:lstr), '''.'
        endif
        if (arg.lt.narg) goto 2
!
! Verify, whether ncname,ncid(0:NP_XI*NP_ETA-1) comprizes a complete
! set of partitioned files.
!
   5    complete=.true.
        do node=0,NP_XI*NP_ETA-1 
          if (ncid(node).lt.0) complete=.false.
        enddo
        if (complete) then
          lvar=lenstr(ncname(0))
          write(stdout,'(4x,2A,I2,1x,A,2(3x,A,I2))') 'Processing ',
     &       'set of files', 0, ncname(0)(1:lvar), 'NP_XI =', NP_XI,
     &                                            'NP_ETA =', NP_ETA
          do node=1,NP_XI*NP_ETA-1
           write(stdout,'(25x,I4,1x,A)')node, ncname(node)(1:lvar)
          enddo
          write(stdout,*)
        elseif (arg.lt.narg) then
          goto 1
        else
          stop 
        endif

        NNODES=NP_XI*NP_ETA
!
! General inquiry into the file which belongs to subdomain ii,jj=0,0
!
        lstr=lenstr(ncname(0))
        ierr=nf_inq (ncid(0), ndims, nvars, ngatts, unlimdimid)
        if (ierr .ne. nf_noerr) then
          write(stdout,'(/8x,2A/15x,3A/)')   'ERROR: Cannot ',
     &            'determine number of dimensions, variables',
     &            'and attributes in netCDF file ''', 
     &                              ncname(0)(1:lstr),   '''.'
          goto 97
        elseif (ndims .gt. maxdims) then
          write(stdout,'(/8x,A,I4,1x,4A/15x,A,1x,A/)')
     &        'ERROR: number of dimensions', ndims,  'in netCDF',
     &        'file ''', ncname(0)(1:lstr), '''', 'exceeds limit.',
     &        'Increase parameter maxdims in file "partit.F".'
          goto 97
         elseif (nvars .gt. maxvars) then
          write(stdout,'(/8x,A,I4,1x,4A/15x,A,1x,A/)')
     &        'ERROR: number of variables',  nvars,  'in netCDF',
     &        'file ''', ncname(0)(1:lstr), '''', 'exceeds limit.',
     &        'Increase parameter maxvars in file "partit.F".'
          goto 97
        endif
!
! Verify that ndims, nvars, ngatts, unlimdimid are the same for
! all of the nodes.
!
        do node=1,NNODES-1
          ierr=nf_inq (ncid(node), ibuff(1), ibuff(2),
     &                             ibuff(3), ibuff(4))
          if (ierr .ne. nf_noerr) then
            write(stdout,'(/8x,2A/15x,3A/)')   'ERROR: Cannot ',
     &              'determine number of dimensions, variables',
     &              'and attributes in netCDF file ''', 
     &                             ncname(node)(1:lstr),   '''.'
            goto 97
          elseif (ibuff(1) .ne. ndims) then
            write(stdout,'(/8x,3A/15x,3A/)') 'ERROR: netCDF file ''',
     &               ncname(node)(1:lstr), ''' has different number',
     &              'of dimensions than ''', ncname(0)(1:lstr), '''.'
            goto 97
          elseif (ibuff(2) .ne. nvars) then
            write(stdout,'(/8x,3A/15x,3A/)') 'ERROR: netCDF file ''',
     &               ncname(node)(1:lstr), ''' has different number',
     &              'of variables than ''',  ncname(0)(1:lstr), '''.'
            goto 97
          elseif (ibuff(3) .ne. ngatts) then
            write(stdout,'(/8x,3A/15x,3A/)') 'ERROR: netCDF file ''',
     &               ncname(node)(1:lstr), ''' has different number',
     &              'of global attributes than ''',
     &                                       ncname(0)(1:lstr), '''.'
            goto 97
          elseif (ibuff(4) .ne. unlimdimid) then
            write(stdout,'(/8x,3A/15x,3A/)') 'ERROR: netCDF file ''',
     &               ncname(node)(1:lstr), ''' has different ID',
     &              'for unlimited dimension than ''',
     &                                       ncname(0)(1:lstr), '''.'
            goto 97
          endif
        enddo 
!
! Check dimensions (IDs, names and sizes) for consistency accross
! the subdomains:
!
        tsize=1 !--> will remain 1, if no unlimited dimension exists.
        do i=1,ndims
          do node=0,NNODES-1
            ierr=nf_inq_dimname (ncid(node), i, string)
            if (ierr .ne. nf_noerr) then
              write(stdout,'(/8x,2A,I3/15x,3A/)')     'ERROR: ',
     &          'Cannot determine name for dimension ID =',   i,
     &          'in netCDF file ''', ncname(node)(1:lstr), '''.'
              goto 97
            else
              j=lenstr(string) 
              if (node.eq.0) then
                lvar=j
                dimname(i)=string(1:j)
              elseif (lvar.ne.j .or. dimname(i)(1:lvar)  .ne.
     &                                      string(1:j)) then
                write(stdout,'(/8x,4A,I3/3(15x,3A/))') 'ERROR: ',
     &            'name ''',string(1:j),''' of dimension with ID =',
     &            i, 'in netCDF file ''',    ncname(node)(1:lstr),
     &            ''' does not match',   'dimension name ''',
     &            dimname(i)(1:lvar), ''' with the same ID in',
     &            'netCDF file ''',   ncname(0)(1:lstr),    '''.'
                  goto 97
              endif
            endif
            ierr=nf_inq_dimlen  (ncid(node), i, dimsize(i,node))
            if (ierr .ne. nf_noerr) then
              write(stdout,'(/8x,A,1x,3A/15x,3A/)')  'ERROR: Cannot', 
     &          'determine size of dimension ''', dimname(i)(1:lvar),
     &          '''', 'in netCDF file ''', ncname(node)(1:lstr),'''.'
              goto 97
            endif
          enddo
!
! Set dimension size for the combined file the same as similar
! dimension for subdomain of MPI node 0. Then for partitionable
! dimensions 'xi_rho', 'xi_u', 'eta_rho' and 'eta_v' scan files
! for subdomains adjacent in corresponding directions (XI- for
! 'xi_rho', 'xi_u' and ETA-direction for 'eta_rho', 'eta_v') and
! add up their dimension sizes.
! For all other dimensions, verify that the sizes are the same
! for all nodes. Also find size of unlimited dimension.
!
          dimsize(i,NNODES)=dimsize(i,0)
          if (lvar.eq.6 .and. dimname(i)(1:lvar).eq.'xi_rho') then 
            id_xi_rho=i
            do node=1,NP_XI-1
              dimsize(i,NNODES)=dimsize(i,NNODES)+dimsize(i,node)
            enddo
          elseif (lvar.eq.4 .and. dimname(i)(1:lvar).eq.'xi_u') then
            id_xi_u=i
            do node=1,NP_XI-1
              dimsize(i,NNODES)=dimsize(i,NNODES)+dimsize(i,node)
            enddo
          elseif (lvar.eq.7.and.dimname(i)(1:lvar).eq.'eta_rho') then 
            id_eta_rho=i
            do node=NP_XI, NP_XI*NP_ETA-1, NP_XI
              dimsize(i,NNODES)=dimsize(i,NNODES)+dimsize(i,node)
            enddo
          elseif (lvar.eq.5 .and. dimname(i)(1:lvar).eq.'eta_v') then
            id_eta_v=i
            do node=NP_XI, NP_XI*NP_ETA-1, NP_XI
              dimsize(i,NNODES)=dimsize(i,NNODES)+dimsize(i,node)
            enddo
          else
            do node=1,NNODES-1
              if (dimsize(i,0).ne.dimsize(i,node)) then
                write(stdout,'(/8x,A,I4,3A,I3/15x,3A/15x,A,I4,3A/)')
     &           'ERROR: Size',  dimsize(i,node), ' of dimension ''',
     &            dimname(i)(1:lvar),   ''' with ID =',  i,
     &           'in netCDF file ''',   ncname(node)(1:lstr),
     &           ''' does not match',  'similar size',  dimsize(i,0),
     &           ' from file file ''',  ncname(0)(1:lstr),  '''.'
               goto 97
              endif
            enddo
          endif
          if (i.eq. unlimdimid) then
            tsize=dimsize(i,NNODES)
            dimsize(i,NNODES)=nf_unlimited
          endif
        enddo ! <-- i loop over dimensions
!                              
! Create combined netCDF file:  Once the completeness of the set of
! ====== ======== ====== =====  partial files have been established
!                               and dimensions survive consistency
!                               check, create the combined file,
!                               define its dimensions and copy
!                               global attributes.
        i=lenstr(root_bak)
        j=lenstr(sffx_bak)
        ncname0=root_bak(1:i)/ /sffx_bak(1:j)
        lstr=lenstr(ncname0)
!       to enable ncjoin with large netcdf files : 
!       use nf_64bit_offset instead of the original nf_clobber
!       ierr=nf_create ( ncname0(1:lstr), nf_clobber, ncid0)
        ierr=nf_create ( ncname0(1:lstr), nf_64bit_offset, ncid0)
        if (ierr .eq. nf_noerr) then
          write(stdout,'(8x,3A)')  'Created netCDF file ''',
     &                                ncname0(1:lstr), '''.'
        else
          write(stdout,'(/8x,4A/)') 'ERROR: Cannot create ',
     &              'netCDF file ''', ncname0(1:lstr), '''.'
          goto 97
        endif
!
! Define dimensions
!
        do i=1,ndims
          lvar=lenstr(dimname(i))
          ierr=nf_def_dim (ncid0, dimname(i)(1:lvar),
     &                     dimsize(i,NNODES), dimid(i))
          if (ierr .ne. nf_noerr) then
            write(stdout,'(/8x,4A/15x,A,I4,A/)') 'ERROR: Cannot ',
     &           'define dimension ''', dimname(i)(1:lvar), '''.', 
     &           'netCDF ettor status =', ierr, '.'
            goto 97
          elseif (dimid(i).ne.i) then
            write(stdout,'(/8x,4A/15x,A/)') 'ERROR: dimension ',
     &        'ID from file ''', ncname0(1:lstr),  ''' differs',
     &        'from the original ID.'
            goto 97
          endif
c**       write(*,'(2I3,I5,1x,A)') i, dimid(i), dimsize(i,NNODES), 
c**  &                                          dimname(i)(1:lvar)
        enddo ! <-- i loop over dimensions
!
! Copy global attributes (except 'partition').
!
        do i=1,ngatts
          ierr=nf_inq_attname (ncid(0), nf_global, i, string)
          if (ierr. eq. nf_noerr) then
            lvar=lenstr(string)
            if (string(1:lvar) .ne. 'partition') then 
              ierr=nf_copy_att (ncid(0), nf_global, string(1:lvar),
     &                                            ncid0, nf_global)
              if (ierr. ne. nf_noerr) then
                write(stdout,'(/8x,4A/15x,3A/)')  'ERROR: Cannot ',
     &            'copy global attribute ''', string(1:lvar), '''',
     &            'into netCDF file ''',   ncname0(1:lstr),   '''.'
               goto 97
              endif
            endif 
          else
            lstr=lenstr(ncname(0))
            write(stdout,'(/8x,2A,I3/15x,3A/)') 'ERROR: Cannot ',
     &         'determine mame of global attribute with ID =', i,
     &         'from netCDF file ''',   ncname(0)(1:lstr),  '''.'
            goto 97
          endif
        enddo
!
! Define variables and copy their attributes.
!
        do i=1,nvars
          ierr=nf_inq_var (ncid(0),   i, varname(i),  vartype(i),
     &                        vardims(i), dimids(1,i),   varatts)
          if (ierr .eq. nf_noerr) then
            lvar=lenstr(varname(i))
            ierr=nf_def_var (ncid0, varname(i)(1:lvar), vartype(i),
     &                           vardims(i), dimids(1,i), varid(i))
            if (ierr .eq. nf_noerr) then
              do j=1,varatts
                ierr=nf_inq_attname (ncid(0), varid(i), j, string)
                if (ierr .eq. nf_noerr) then
                  lvar=lenstr(string)
                  ierr=nf_copy_att (ncid(0), i, string(1:lvar),
     &                                           ncid0, varid(i))
                  if (ierr. ne. nf_noerr) then
                    write(stdout,'(/8x,2A,I3,3A/15x,3A/)') 'ERROR: ',
     &                  'Cannot copy attribute with ID =',   j,
     &                  ' for variable ''', varname(i)(1:lvar), '''',
     &                  'into netCDF file ''', ncname0(1:lstr), '''.'
                    goto 97
                  endif
                else
                  write(stdout,'(/8x,2A,I3/15x,3A)')     'ERROR: ',
     &                 'Cannot acquire attribute with ID =',  j,
     &                 'for variable ''', varname(i)(1:lvar), '''.'
                  goto 97
                endif
              enddo
            else
              write(stdout,'(/8x,4A/)') 'ERROR: Cannot define ',
     &                  'variable ''', varname(i)(1:lvar), '''.'
              goto 97
            endif
          else
            lstr=lenstr(ncname(0))
            write(stdout,'(/8x,2A/15x,A,I3,1x,3A/)')'ERROR: Cannot ',
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
        enddo       ! <-- i, variable ID.
!
! Leave definition mode
!
        ierr=nf_enddef (ncid0)
!
! Transfer variables into newly created file.
!
        do rec=1,tsize
          if (tsize.gt.1) write(stdout,'(16x,A,I5,1x,A)')
     &                     'Processing record', rec, '...'
          do i=1,nvars
            if (series(i) .or. rec.eq.1) then
              if (.not.part_switch(i) .and. .not.series(i)) then
!
! Scalar (zero-dimensional) variables:
!
c**             write(*,*) 'Scalar variable ', varname(i)
                if (vartype(i) .eq. nf_char) then
                  ierr=nf_get_var_text  (ncid(0), i, buff)
                elseif (vartype(i) .eq. nf_int) then
                  ierr=nf_get_var_int    (ncid(0), i, buff)
                elseif (vartype(i) .eq. nf_real) then
                  ierr=nf_get_var_real   (ncid(0), i, buff)
                elseif (vartype(i) .eq. nf_double) then
                  ierr=nf_get_var_double (ncid(0), i, buff)
                else
                  lvar=lenstr(varname(i))
                  write(stdout,'(/8x,4A/)') 'ERROR: scalar variable',
     &              ' ''', varname(i)(1:lvar), ''' has unknown type.'
                  goto 97
                endif
                if (ierr .eq. nf_noerr) then
                  if (vartype(i) .eq. nf_char) then
                    ierr=nf_put_var_text  (ncid0, varid(i), buff)
                  elseif (vartype(i) .eq. nf_int) then
                    ierr=nf_put_var_int    (ncid0, varid(i), buff)
                  elseif (vartype(i) .eq. nf_real) then
                    ierr=nf_put_var_real   (ncid0 ,varid(i), buff)
                  elseif (vartype(i) .eq. nf_double) then
                    ierr=nf_put_var_double (ncid0, varid(i), buff)
                  endif
                  if (ierr .ne. nf_noerr) then
                    lvar=lenstr(varname(i))
                    write(stdout,'(/8x,4A/15x,4A,I4,A/)') 'ERROR: ',
     &                        'Cannot write scalar variable ''',
     &                         varname(i)(1:lvar), ''' into netCDF',
     &                        'file ''', ncname0(1:lstr), '''.  ',
     &                        'netCDF error status code =', ierr,'.'
                    goto 97
                  endif
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
c**             write(*,*) 'Non-partitionable array ', varname(i)
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
     &             'ERROR: unsufficient buffer size in "ncjoin.F":',
     &             'requested:',         size,      'Bytes,',
     &             'available:',   8*max_buff_size, 'Bytes.',
     &             'Increase parameter max_buff_size and recompile.'
                  goto 97
                endif

               if (vartype(i) .eq. nf_char) then
                  ierr=nf_get_vara_text   (ncid(0), i, start,
     &                                           count, buff)
                elseif (vartype(i) .eq. nf_int) then
                  ierr=nf_get_vara_int    (ncid(0), i, start,
     &                                           count, buff)
                elseif (vartype(i) .eq. nf_real) then
                  ierr=nf_get_vara_real   (ncid(0), i, start,
     &                                           count, buff)
                elseif (vartype(i) .eq. nf_double) then
                  ierr=nf_get_vara_double (ncid(0), i, start,
     &                                           count, buff)
                endif
                if (ierr .eq. nf_noerr) then
                  if (vartype(i) .eq. nf_char) then
                    ierr=nf_put_vara_text   (ncid0, varid(i),
     &                                         start, count, buff)
                  elseif (vartype(i) .eq. nf_int) then
                    ierr=nf_put_vara_int    (ncid0, varid(i),
     &                                         start, count, buff)
                  elseif (vartype(i) .eq. nf_real) then
                    ierr=nf_put_vara_real   (ncid0, varid(i),
     &                                         start, count, buff)
                  elseif (vartype(i) .eq. nf_double) then
                    ierr=nf_put_vara_double (ncid0, varid(i),
     &                                         start, count, buff)
                  endif
                  if (ierr .ne. nf_noerr) then
                    lvar=lenstr(varname(i))
                    write(stdout,'(/8x,4A,I3/15x,3A,I3/)') 'ERROR: ',
     &               'Cannot write variable ''',  varname(i)(1:lvar),
     &               ''' for time record',rec, 'into netCDF file ''',
     &               ncname0(1:lstr),'''. netCDF error code =', ierr
                    goto 97
                  endif
                else
                  lvar=lenstr(varname(i))
                  write(stdout,'(/8x,4A,I3,A/15x,A,I4/)') 'ERROR: ',
     &               'Cannot read variable ''',  varname(i)(1:lvar),
     &               ''' for time record', rec, '.',
     &                           'netCDF error status code =', ierr
                  goto 97
                endif
              elseif (part_switch(i)) then
!
! Partitioned array:
!
c**             write(*,*) 'Partitioned array ', varname(i)
                do node=0,NNODES-1
                  size=1
                  do j=1,vardims(i)
                    k=dimids(j,i)
                    if (k.eq.id_xi_rho .or. k.eq.id_xi_u) then
                      start(j)=1
                      count(j)=dimsize(k,node)
                      start1(j)=1
                      do m=0,ii(node)-1
                        start1(j)=start1(j)+dimsize(k,m)
                      enddo

                    elseif (k.eq.id_eta_rho .or. k.eq.id_eta_v) then
                      start(j)=1
                      count(j)=dimsize(k,node)
                      start1(j)=1
                      do m=0,jj(node)-1
                        start1(j)=start1(j)+dimsize(k,m*NP_XI)
                      enddo

                    elseif (k.eq.unlimdimid) then
                      start(j)=rec
                      count(j)=1
                      start1(j)=rec
                    else
                      start(j)=1
                      count(j)=dimsize(k,nnodes)
                      start1(j)=1
                    endif
                    size=size*count(j)
c**                 if (k.eq.id_eta_rho) write(*,'(A,1x, 6I6)')
c**  &                 varname(i)(1:lvar), node,ii(node),jj(node),
C**  &                            j, start1(j), dimsize(k,nnodes)
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
                    write(stdout,'(/8x,4A/)') 'ERROR: variable ''',
     &                   varname(i)(1:lvar), ''' has unknown type.'
                    goto 97
                  endif
                  if (size .gt. 8*max_buff_size) then
                    write(stdout,'(/8x,A,3(/15x,A,I10,1x,A)/)')
     &             'ERROR: unsufficient buffer size in "ncjoin.F":',
     &             'requested:',         size,      'Bytes,',
     &             'available:',   8*max_buff_size, 'Bytes.',
     &             'Increase parameter max_buff_size and recompile.'
                    goto 97
                  endif

                  if (vartype(i) .eq. nf_char) then
                    ierr=nf_get_vara_text   (ncid(node), i, start,
     &                                                count, buff)
                  elseif (vartype(i) .eq. nf_int) then
                    ierr=nf_get_vara_int    (ncid(node), i, start,
     &                                                count, buff)
                  elseif (vartype(i) .eq. nf_real) then
                    ierr=nf_get_vara_real   (ncid(node), i, start,
     &                                                count, buff)
                  elseif (vartype(i) .eq. nf_double) then
                    ierr=nf_get_vara_double (ncid(node), i, start,
     &                                                count, buff)
                  endif
                  if (ierr .eq. nf_noerr) then
                    if (vartype(i) .eq. nf_char) then
                      ierr=nf_put_vara_text   (ncid0, varid(i),
     &                                        start1, count, buff)
                    elseif (vartype(i) .eq. nf_int) then
                      ierr=nf_put_vara_int    (ncid0, varid(i),
     &                                        start1, count, buff)
                    elseif (vartype(i) .eq. nf_real) then
                      ierr=nf_put_vara_real   (ncid0, varid(i),
     &                                        start1, count, buff)
                    elseif (vartype(i) .eq. nf_double) then
                      ierr=nf_put_vara_double (ncid0, varid(i),
     &                                        start1, count, buff)
                    endif
                    if (ierr .ne. nf_noerr) then
                      lvar=lenstr(varname(i))
                      write(stdout,'(/8x,3A,I3/15x,3A,I3/)')
     &                  'ERROR: Cannot write variable ''',
     &                   varname(i)(1:lvar), ''' for time record',
     &                   rec,    'into file ''',  ncname0(1:lstr),
     &                   '''. netCDF error status code =',  ierr
                      goto 97
                    endif
                  else
                    lvar=lenstr(varname(i))
                    lstr=lenstr(ncname(node))
                    write(stdout,'(/8x,4A,I3/15x,3A,I4/)') 'ERROR: ',
     &                 'Cannot read variable ''', varname(i)(1:lvar),
     &                ''' for time record',    rec,   'from file ''',
     &                ncname(node)(1:lstr),
     &                               '''. netCDF error code =', ierr
                    goto 97
                  endif


                enddo       ! <-- node=0,NNODES-1
              endif
            endif
          enddo
        enddo
!
! Close all files
!
  97    ierr=nf_close (ncid0) 
        do node=0,NNODES-1
          ierr=nf_close (ncid(node))
        enddo
       if (arg .lt. narg)  goto 1  !--> next set of partial files.
      stop
      end

