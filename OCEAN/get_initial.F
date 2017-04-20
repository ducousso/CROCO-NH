! $Id: get_initial.F 1458 2014-02-03 15:01:25Z gcambon $
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
                              ! Read initial conditions for the
      subroutine get_initial  ! primitive variables from NetCDF 
                              ! initialization file.
      implicit none
# include "param.h"
# include "scalars.h"
# include "ncscrum.h"
# include "ocean2d.h"
# include "ocean3d.h"
#ifdef SEDIMENT
# include "sediment.h"
#endif
#ifdef BBL
# include "bbl.h"
#endif
#if defined OA_COUPLING || defined OW_COUPLING
# include "mpi_roms.h"
#endif
#ifdef NBQ
# include "grid.h"
#endif

      real time_scale
      integer itrc
#if defined SEDIMENT || defined BBL
     &      ,  indxWrk
#endif
      integer ncid, indx, varid,  ierr, lstr, lvar, latt, lenstr,
     &        start(2), count(2), ibuff(4),   nf_fread, checkdims
      character units*64
#include "netcdf.inc"
!
! Open initial conditions netCDF file for reading. Check that all
! spatial dimensions in that file are consistent with the model
! arrays, determine how many time records are available in the file
! and set record from which the dada will be read.
!
! The record is set as follows: (1) if only one time record is
! available in the file, then that record is used REGARDLESS of
! value of nrrec supplied from the parameter file; (2) if the
! file has multiple records and nrrec is positive, then nrrec is
! used, provided that nrrec is within the available records; 
! (3) if the file has multiple records and nrrec<0, then THE LAST 
! available record is used.
!
      if (may_day_flag.ne.0) return      !-->  EXIT
      lstr=lenstr(ininame)
      ierr=nf_open(ininame(1:lstr), nf_nowrite, ncid)
      if (ierr .eq. nf_noerr) then
        ierr=checkdims (ncid, ininame, lstr, indx)
        if (ierr. ne. nf_noerr) then
          goto 99
        elseif (indx.eq.0) then
          indx=1
        elseif (indx.gt.0 .and. nrrec.gt.0 .and. nrrec.le.indx) then
          indx=nrrec
        elseif (indx.gt.0 .and. nrrec.gt.indx) then
          write(stdout,'(/1x,A,I4,A/16x,A,I4,A/16x,3A/)')
     &            'GET_INITIAL ERROR: requested restart time record',
     &             nrrec, ' exceeds',  'number of available records', 
     &             indx,'in netCDF file', '''',ininame(1:lstr),'''.'
          goto 99                                        !--> ERROR
        endif
      else
        write(stdout,'(/1x,2A/15x,3A)') 'GET_INITIAL ERROR: Cannot ',
     &               'open netCDF file', '''', ininame(1:lstr) ,'''.'
        goto 99                                           !--> ERROR
      endif
!
! Read in evolving model variables:
! ---- -- -------- ----- ----------
!
! Time: find netCDF id, read value, read attribute 'units'
! and set starting time index and time clock in days.
!
      lvar=lenstr(vname(1,indxTime))
      ierr=nf_inq_varid (ncid, vname(1,indxTime)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_get_var1_FTYPE (ncid, varid, indx, time)
        if (ierr .eq. nf_noerr) then
          ierr=nf_get_att_text(ncid, varid, 'units', units)
          if (ierr .eq. nf_noerr) then
            latt=lenstr(units)
            if (units(1:6).eq.'second') then
               time_scale=1.
            elseif (units(1:3).eq.'day') then
              time_scale=day2sec
            else
              write (stdout,'(/1x,4A/8x,3A/)') 'GET_INITIAL ',
     &              'ERROR: unknown units of for variable ''', 
     &               vname(1,indxTime)(1:lvar), '''',
     &              'in netCDF file ''', ininame(1:lstr),'''.' 
              goto 99                                    !--> ERROR
            endif
          else
            write (stdout,'(/1x,2A/8x,5A/)') 'GET_INITIAL ERROR: ',
     &             'cannot read attribute ''units'' for variable',
     &             '''', vname(1,indxTime)(1:lvar),
     &             ''' in netCDF file ''',  ininame(1:lstr), '''.'
            goto 99                                       !--> ERROR
          endif
        else
          write(stdout,2) vname(1,indxTime)(1:lvar), indx,
     &                                        ininame(1:lstr)
          goto 99                                         !--> ERROR
        endif
      else
        write(stdout,1) vname(1,indxTime)(1:lvar),ininame(1:lstr)
        goto 99                                           !--> ERROR
      endif

      time=time*time_scale
      tdays=time*sec2day

      ierr=nf_inq_varid (ncid, 'time_step', varid)
      if (ierr .eq. nf_noerr) then
        start(1)=1
        start(2)=indx
        count(1)=4
        count(2)=1
        ierr=nf_get_vara_int (ncid, varid, start, count, ibuff)
        if (ierr .eq. nf_noerr) then
          ntstart=ibuff(1)
          nrecrst=ibuff(2)
          nrechis=ibuff(3)
#ifdef AVERAGES
          nrecavg=ibuff(4)
#endif
        MPI_master_only write(stdout,
     &     '(6x,A,G12.4,A,I2,A,I6,A,I3,A,I3,A)')
     &     'GET_INITIAL: Restarted from day =', tdays, ' rec =',
     &      indx, '(', ntstart, ',', nrecrst, ',', nrechis, ').'

        else
         MPI_master_only write(stdout,'(/1x,2A/)') 
     &                            'GET_INITIAL ERROR: Cannot ',
     &                            'read time and record indices.'
          goto 99                                         !--> ERROR
        endif
      else
        ntstart=1
        nrecrst=0
        nrechis=0
#ifdef AVERAGES
        nrecavg=0
#endif
        MPI_master_only write(stdout,'(6x,2A,G12.4,1x,A,I4)') 
     &          'GET_INITIAL -- ',
     &          'Processing data for time =', tdays, 'record =', indx
      endif
      if (ntstart.lt.1) ntstart=1
      ntimes=ntstart+ntimes-1
!
! Free-surface.
!
      lvar=lenstr(vname(1,indxZ))
      ierr=nf_inq_varid (ncid, vname(1,indxZ)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fread (zeta(START_2D_ARRAY,1), ncid, varid,
     &                                         indx, r2dvar)
        if (ierr .ne. nf_noerr) then
          write(stdout,2) vname(1,indxZ)(1:lvar),indx,ininame(1:lstr)
          goto 99                                         !--> ERROR
        endif
      else
        write(stdout,1) vname(1,indxZ)(1:lvar), ininame(1:lstr)
        goto 99                                           !--> ERROR
      endif
#ifdef NBQ     
      zeta(:,:,1) = zeta(:,:,1) + h(:,:)  ! Change zeta definition
                                          ! from ssh to total depth
#endif
!
! 2D momentum component in the XI-direction.
!
      lvar=lenstr(vname(1,indxUb))
      ierr=nf_inq_varid (ncid, vname(1,indxUb)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fread (ubar(START_2D_ARRAY,1), ncid, varid,
     &                                         indx, u2dvar)
        if (ierr .ne. nf_noerr) then
          write(stdout,2) vname(1,indxUb)(1:lvar), indx,
     &                                      ininame(1:lstr)
          goto 99                                        !--> ERROR
        endif
      else
        write(stdout,1) vname(1,indxUb)(1:lvar), ininame(1:lstr)
        goto 99                                           !--> ERROR
      endif
!
! 2D momentum component in the ETA-direction.
!
      lvar=lenstr(vname(1,indxVb))
      ierr=nf_inq_varid (ncid, vname(1,indxVb)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fread (vbar(START_2D_ARRAY,1), ncid, varid,
     &                                         indx, v2dvar)
        if (ierr .ne. nf_noerr) then
          write(stdout,2) vname(1,indxVb)(1:lvar), indx,
     &                                       ininame(1:lstr)
          goto 99                                          !--> ERROR
        endif
      else
        write(stdout,1) vname(1,indxVb)(1:lvar), ininame(1:lstr)
        goto 99                                           !--> ERROR
      endif
#ifdef SOLVE3D
!
! 3D momentum component in the XI-direction.
!
      lvar=lenstr(vname(1,indxU))
      ierr=nf_inq_varid (ncid, vname(1,indxU)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fread (u(START_2D_ARRAY,1,1), ncid, varid,
     &                                        indx, u3dvar)
        if (ierr .ne. nf_noerr) then
          write(stdout,2) vname(1,indxU)(1:lvar), indx,
     &                                     ininame(1:lstr)
          goto 99                                         !--> ERROR
        endif
      else
        write(stdout,1) vname(1,indxU)(1:lvar), ininame(1:lstr)
        goto 99                                           !--> ERROR
      endif
!
! 3D momentum component in the ETA-direction.
!
      lvar=lenstr(vname(1,indxV))
      ierr=nf_inq_varid (ncid, vname(1,indxV)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fread (v(START_2D_ARRAY,1,1), ncid, varid,
     &                                        indx, v3dvar)
        if (ierr .ne. nf_noerr) then
          write(stdout,2) vname(1,indxV)(1:lvar), indx,
     &                                       ininame(1:lstr)
          goto 99                                        !--> ERROR
        endif
      else
        write(stdout,1) vname(1,indxV)(1:lvar), ininame(1:lstr)
        goto 99                                           !--> ERROR
      endif
!
! Tracer variables.
!
      do itrc=1,NT
        got_tini(itrc)=.true.
        lvar=lenstr(vname(1,indxT+itrc-1))
        ierr=nf_inq_varid (ncid, vname(1,indxT+itrc-1)(1:lvar),
     &                                                    varid)
        if (ierr .eq. nf_noerr) then
          ierr=nf_fread (t(START_2D_ARRAY,1,1,itrc), ncid,  varid,
     &                                               indx, r3dvar)

          if (ierr .ne. nf_noerr) then
            write(stdout,2) vname(1,indxT+itrc-1)(1:lvar), indx,
     &                                              ininame(1:lstr)
            goto 99                                       !--> ERROR
          endif
        else
          write(stdout,3) vname(1,indxT+itrc-1)(1:lvar),
     &                                    ininame(1:lstr)
          got_tini(itrc)=.false.
        endif
      enddo
#ifdef SEDIMENT
!
! Bed thickness
!
      got_inised(1)=.true.
      lvar=lenstr(vname(1,indxBTHK))
      ierr=nf_inq_varid (ncid, vname(1,indxBTHK)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fread (bed_thick, ncid, varid,
     &                                        indx, b3dvar)
        if (ierr .ne. nf_noerr) then
          write(stdout,2) vname(1,indxBTHK)(1:lvar), indx,
     &                                       ininame(1:lstr)
          goto 99                                        !--> ERROR
        endif
      else
        write(stdout,3) vname(1,indxBTHK)(1:lvar), ininame(1:lstr)
        got_inised(1)=.false.
      endif
!
! Bed porosity
!
      got_inised(2)=.true.
      lvar=lenstr(vname(1,indxBPOR))
      ierr=nf_inq_varid (ncid, vname(1,indxBPOR)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fread (bed_poros, ncid, varid,
     &                                        indx, b3dvar)
        if (ierr .ne. nf_noerr) then
          write(stdout,2) vname(1,indxBPOR)(1:lvar), indx,
     &                                       ininame(1:lstr)
          goto 99                                        !--> ERROR
        endif
      else
        write(stdout,3) vname(1,indxBPOR)(1:lvar), ininame(1:lstr)
        got_inised(2)=.false.
      endif
!
! Bed sediment fractions
!
      got_inised(3)=.true.
      do itrc=1,NST
         indxWrk=indxBFRA(1)+itrc-1
         lvar=lenstr(vname(1,indxWrk))
      ierr=nf_inq_varid (ncid, vname(1,indxWrk)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fread (bed_frac(START_2D_ARRAY,1,itrc), ncid, varid,
     &                                        indx, b3dvar)
        if (ierr .ne. nf_noerr) then
          write(stdout,2) vname(1,indxWrk)(1:lvar), indx,
     &                                       ininame(1:lstr)
          goto 99                                        !--> ERROR
        endif
      else
        write(stdout,3) vname(1,indxWrk)(1:lvar), ininame(1:lstr)
        got_inised(3)=.false.
      endif
      enddo
#endif /* SEDIMENT */
# ifdef BBL
!
! Ripple height
!
      got_inibed(1)=.true.
      lvar=lenstr(vname(1,indxHrip))
      ierr=nf_inq_varid (ncid, vname(1,indxHrip)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fread (Hripple, ncid, varid,
     &                                        indx, r2dvar)
        if (ierr .ne. nf_noerr) then
          write(stdout,2) vname(1,indxHrip)(1:lvar), indx,
     &                                       ininame(1:lstr)
          goto 99                                        !--> ERROR
        endif
      else
        write(stdout,3) vname(1,indxWrk)(1:lvar), ininame(1:lstr)
        got_inibed(1)=.false.
      endif
!
! Ripple length
!
      got_inibed(2)=.true.
      lvar=lenstr(vname(1,indxLrip))
      ierr=nf_inq_varid (ncid, vname(1,indxLrip)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fread (Lripple, ncid, varid,
     &                                        indx, r2dvar)
        if (ierr .ne. nf_noerr) then
          write(stdout,2) vname(1,indxLrip)(1:lvar), indx,
     &                                       ininame(1:lstr)
          goto 99                                        !--> ERROR
        endif
      else
        write(stdout,3) vname(1,indxWrk)(1:lvar), ininame(1:lstr)
        got_inibed(2)=.false.
      endif
# endif /* BBL */
!
!  Close input NetCDF file.
!
      ierr=nf_close(ncid)
#endif

#if defined OA_COUPLING || defined OW_COUPLING
      call cpl_prism_define
      oasis_time = 0
      MPI_master_only write(*,*)'CPL-ROMS: OASIS_TIME',oasis_time
#endif

  1   format(/1x,'GET_INITIAL - unable to find variable:',    1x,A,
     &                            /15x,'in input NetCDF file:',1x,A/)
  2   format(/1x,'GET_INITIAL - error while reading variable:',1x, A,
     &    2x,'at time record =',i4/15x,'in input NetCDF file:',1x,A/)
  3   format(/1x,'GET_INITIAL - unable to find variable:',    1x,A,
     &                            /15x,'in input NetCDF file:',1x,A,
     &    1x,'-> analytical value'/)
      return
  99  may_day_flag=2
      return
      end
