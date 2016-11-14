! $Id: set_cycle.F 1458 2014-02-03 15:01:25Z gcambon $
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

      subroutine set_cycle (ncid, varid, ntime, cycle_length,
     &                                        icycle, trecord)
!
! Determine whether there is time periodicuty (cycling) for a
! forcing variable in input NetCDF file and set initial time
! record index "trecord" and cycling index "icycle" depending
! on the current model.
!
! Input:  ncid          netCDF file ID.
!         varid         netCDF field time variable ID.
!
! Output: ntime         Size of time dimension.
!         cycle_length  Length of field time cycle [seconds].
!         icycle        Cycling index (integer),
!         trecord       Time record index (integer).
!
!
!         i n p u t     o u t p u t      i n t e r n a l
!
      implicit none
      real              cycle_length,    tstart,  tend,     cff
      integer ncid,     icycle,          tshift,  vartype,  lvar,
     &        varid,    trecord,         irec,    nvatts,   latt, 
     &                  ntime,           i,ierr,  nvdims,   ldim,
     &                                   size,    vdims(5), lenstr 
      character*16 varname, dimname, attname

#include "param.h"
#include "scalars.h"
#include "netcdf.inc"
!
! Check if more than one time record is available; if so, check if
! time cycling attribute is present and if so, then read in time
! cycle_length. In the case when only one record is available,
! set negative cycle_length.
!
      ierr=nf_inq_var (ncid, varid, varname, vartype,
     &                       nvdims,  vdims,  nvatts)
      if (ierr .eq. nf_noerr) then
        lvar=lenstr(varname)
        ntime=0
        do i=1,nvdims
          ierr=nf_inq_dim (ncid, vdims(i), dimname, size)
          if (ierr .eq. nf_noerr) then
            ldim=lenstr(dimname)
            if (dimname(ldim-4:ldim) .eq. '_time') then
              if (ntime.eq.0) then
                ntime=size
              else
                write (stdout,'(/1x,4A/18x,A)') 'SET_CYCLE ERROR: ',
     &                     'variable ''', varname(1:lvar), ''' has',
     &                     'more than one _time dimension.'
                goto 99                                   !--> ERROR
              endif
            endif
          else
            write(stdout,'(/1x,4A/)') 'SET_CYCLE ERROR while ',
     &                  'inquiring dimensions for variable ''',
     &                                   varname(1:lvar), '''.'
            goto 99                                       !--> ERROR
          endif
        enddo
        if (ntime.gt.1) then
          cycle_length=0.
          do i=1,nvatts
            ierr=nf_inq_attname (ncid, varid, i, attname)
            if (ierr .eq. nf_noerr) then
              latt=lenstr(attname)
              if (attname(1:latt) .eq. 'cycle_length') then
                ierr=nf_get_att_FTYPE (ncid, varid, attname(1:latt),
     &                                                 cycle_length)
                if (ierr .eq. nf_noerr) then
                  cycle_length=cycle_length*day2sec
                else
                  write(stdout,'(/1x,4A/)') 'SET_CYCLE ERROR while ',
     &                 'reading attribute ''', attname(1:latt), '''.'
                  goto 99                                 !--> ERROR
                endif
              endif
            else
              write(stdout,'(/1x,4A/)') 'SET_CYCLE ERROR while ',
     &                    'inquiring attributes for variable ''',
     &                                     varname(1:lvar), '''.'
              goto 99                                     !--> ERROR
            endif
          enddo
        else
          cycle_length=-1.
        endif
      else
        write(stdout,'(/1x,2A,I4/18x,A,I4/)') 'SET_CYCLE ERROR: ',
     &           'Cannot inquire about variable with ID =', varid,
     &           'in input file; netCDF error code =', ierr
        goto 99                                           !--> ERROR
      endif
!
!  Search for starting time record field and time-index.
!
      ierr=nf_get_var1_FTYPE(ncid, varid, 1, tstart)
      if (ierr .ne. nf_noerr) goto 10                     !--> ERROR
      tstart=tstart*day2sec

      if (cycle_length.gt.0.) then
        cff=time-tstart
        icycle=int(abs(cff)/cycle_length)
        if (cff.lt.0.) icycle=-1-icycle
        tstart=tstart+icycle*cycle_length
      else
        icycle=0
      endif

      tshift=icycle 
      trecord=0
      i=1
  1    irec=i+1
        if (cycle_length.gt.0. .and. irec.gt.ntime) then
          irec=1
          tshift=tshift+1
        endif
        if (irec.le.ntime) then 
          ierr=nf_get_var1_FTYPE(ncid, varid, irec, tend)
          if (ierr .ne. nf_noerr) goto 10                 !--> ERROR
          tend=tend*day2sec+tshift*cycle_length
          if (tstart.le.time .and. time.lt.tend) then
            trecord=i
          elseif (irec.ne.1) then
            i=irec
            tstart=tend
            goto 1
          endif
        endif
!
! If record index is found, retard it by one. This is needed
! to compensate the first call to advance_cycle from the routine
! whict calls set_cycle.
!
      if (trecord.ne.0) then
        trecord=trecord-1
        if (trecord.lt.1 .and. cycle_length.gt.0.) then
          trecord=trecord+ntime
          icycle=icycle-1 
        endif
        return                                    !--> NORMAL RETURN
      endif
!
! Sort out reasons for error, if any. 
!
      if (cycle_length.gt.0.) then
        write(stdout,2) varname(1:lvar)
  2     format(/1x, 'SET_CYCLE ERROR: Algorithm failure',
     &                ' while processing variable: ',A,'.'/)
      else
        write(stdout,3) varname(1:lvar), tdays, tend*sec2day  
  3     format(/1x, 'SET_CYCLE ERROR: non-cycling regime,',
     &         ' but model time exeeds'  /18x,  'time of the',
     &         ' last available data record for variable: ',A/
     &         18x, 'TDAYS = ', G12.4, 2x, 'TLAST = ', G12.4/)

      endif
      goto 99
  10  write(stdout,'(1x,4A)') 'SET_CYCLE ERROR while reading ',
     &                    'variable ''', varname(1:lvar), '''.'
  99  may_day_flag=2
      return                             !-->  EXIT
      end


      integer function advance_cycle (cycle_length, ntime,
     &                                      icycle, trecord)
      implicit none
      real cycle_length
      integer ntime, icycle, trecord, ierr 

      ierr=0
      trecord=trecord+1
      if (trecord.gt.ntime) then
        if (cycle_length.gt.0.) then
          trecord=1
          icycle=icycle+1
        else
          ierr=1                                         !--> ERROR
        endif
      endif
      advance_cycle=ierr
      return
      end

