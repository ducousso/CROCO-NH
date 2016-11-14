! $Id: ncrename.F 1458 2014-02-03 15:01:25Z gcambon $
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
      program ncrename
!
! Generic tool to rename object in netCDF file. Object may be
! either a variable, dimension, global attribute or an attribute
! to a variable. In the last case, if more that one variable has
! attribute with name 'name', attributes to ALL variables will be
! renamed.  
!
! Usage:  ncrename needs 3 arguments:  ncrename ncname name newname
!-------  where
!                'ncname' is name of netCDF file
!                'name'   is name of object to be renamed
!                'newname' is the new name of that object
!
      implicit none
      logical found
      character*80 ncname, varname, newname,name
      integer stdout, n, ierr, ncid, varid,  nvars
     &           , lstr, lvar, lnew, lenstr, iargc
      parameter (stdout=6)
#include "netcdf.inc"

      n=iargc()
      if (n.ne.3) then
        write(stdout,'(/1x,A,1x,A/32x,A/)') 'Usage of ncrename', 
     &             'should be:', 'ncrename file.nc name newname' 
        stop
      endif

      call getarg(1,ncname) 
      call getarg(2,varname)
      call getarg(3,newname)

      lstr=lenstr(ncname)
      lvar=lenstr(varname)
      lnew=lenstr(newname)
      found=.false.
!
! Open netCDF file and switch to redefinition mode.
!
      lstr=lenstr(ncname)
      ierr=nf_open (ncname(1:lstr), nf_write, ncid)
      if (ierr .ne. nf_noerr) then
        write(stdout,'(/8x,A,1x,A,A/)') 'Cannot open netCDF file',
     &                                          ncname(1:lstr),'.'
        goto 100 
      endif
      ierr=nf_redef (ncid)
      if (ierr .ne. nf_noerr) then
        write(stdout,'(/8x,A,1x,A,1x,A,A/)') 'Cannot switch to',
     &    'redefinition mode for netCDF file', ncname(1:lstr),'.'
        goto 99
      endif
!
! Check, whether object to be renamed is a variable and if it is,
! rename it as requested.
!
      ierr=nf_inq_varid (ncid, varname(1:lvar), varid)
      if (ierr .ne. nf_noerr) goto 1 
      ierr=nf_rename_var (ncid, varid, newname(1:lnew))
      if (ierr .eq. nf_noerr) then
        write(stdout,'(/8x,5A/)') 'Renamed variable ''',
     &  varname(1:lvar), ''' into ''', newname(1:lnew),'''.'
        found=.true.
      else
        write(stdout,'(/8x,6A/8x,A,I3/)') 'Cannot rename ',
     &     'variable ''',  varname(1:lvar), ''' into ''',
     &     newname(1:lnew),'''.', 'netCDF error status =',i err
        if (ierr. eq. nf_enameinuse) then
          write(stdout,'(8x,A)') 'This name is already in use.'
        endif
      endif
      goto 98
!
! Check, whether object to be renamed is a dimension and if it is,
! rename it as requested.
!
  1   ierr=nf_inq_dimid (ncid, varname(1:lvar), varid)
      if (ierr .ne. nf_noerr) goto 2 
      ierr=nf_rename_dim (ncid, varid, newname(1:lnew))
      if (ierr .eq. nf_noerr) then
        write(stdout,'(/8x,5A/)') 'Renamed dimension ''',
     &    varname(1:lvar), ''' into ''', newname(1:lnew), '''.'
        found=.true.
      else
        write(stdout,'(/8x,6A/8x,A,I3/)') 'Cannot rename ',
     &   'dimension ''', varname(1:lvar), ''' into ''',
     &    newname(1:lnew), '''.', 'netCDF error status =', ierr
        if (ierr. eq. nf_enameinuse) then
          write(stdout,'(8x,A/)') 'This name is already in use.'
        endif
      endif
      goto 98
!
! Check, whether object to be renamed exists as a global attribute
! or attribute to a variable, and if so, rename it as requested.
!
  2   ierr=nf_rename_att (ncid, nf_global, varname(1:lvar),
     &                                     newname(1:lnew))
      if (ierr .eq. nf_noerr) then
        write(stdout,'(/8x,5A/)') 'Renamed global attribute ''',
     &     varname(1:lvar), ''' into ''', newname(1:lnew), '''.'
        found=.true.
        goto 98
      else
        ierr=nf_inq_nvars (ncid, nvars)
        if (ierr .eq. nf_noerr) then
          do varid=1,nvars
            ierr=nf_rename_att (ncid, varid, varname(1:lvar),
     &                                       newname(1:lnew))  
            if (ierr .eq. nf_noerr) then
              ierr=nf_inq_varname (ncid, varid, name)
              n=lenstr (name) 
              write(stdout,'(/8x,7A/)') 'Renamed attribute ''',
     &          varname(1:lvar), ''' into ''', newname(1:lnew),
     &                   ''' for variable ''', name(1:n), '''.'
              found=.true.
            endif
          enddo
        else
          write(stdout,'(/8x,5A/)') 'Cannot determine number of ',
     &            'variables in netCDF file ', ncname(1:lstr), '.' 
        endif
      endif

      if (.not.found) write(stdout,'(/8x,6A/)') 'Cannot find ',
     &      'object ''', varname(1:lvar), ''' in netCDF file ',
     &                                      ncname(1:lstr), '.'

  98  ierr=nf_enddef (ncid) 
      if (ierr. ne. nf_noerr) then 
        write(stdout,'(/8x,A,1x,A,1x,A,A/)') 'Cannot switch',
     &    'to data mode for netCDF file', ncname(1:lstr),'.'
      endif
  99  ierr=nf_close (ncid)
 100  stop
      end

