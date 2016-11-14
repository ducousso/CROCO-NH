! $Id: get_psource.F 1428 2014-01-17 11:03:36Z gcambon $
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
! Marine HERRMANN (IRD/LEGOS) : 2013
!
#include "cppdefs.h"
#if defined PSOURCE_NCFILE

      subroutine get_psource
!
!  Read in point or grided sea river runoff discharge 
!  the appropriate time from runoff NetCDF file.
!
# define PSOURCE_DATA
      implicit none
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "netcdf.inc"
# include "ncscrum.h"
# include "sources.h"

      real cff
      integer is,i,ierr, lstr,lvar,lenstr, nf_fread, advance_cycle,
     &        itrc
      integer s(2),c(2)
      
!
! Initialization: Inquire about the contents of forcing NetCDF file:
!================  variables and dimensions. Check for consistency.
!
      if (may_day_flag.ne.0) return      !-->  EXIT
      if (itqbar.eq.0 .or. iic.eq.0) then
        lstr=lenstr(qbarname)

c*        call opencdf (qbarname,N)
c*        if (may_day_flag.ne.0) return    !-->  EXIT
!
! If not opened yet, open forcing NetCDF file for reading.
! Find and save IDs for relevant variables
!
        if (ncidqbar.eq.-1) then
          ierr=nf_open(qbarname(1:lstr), nf_nowrite, ncidqbar)
          if (ierr. ne. nf_noerr) goto 4                  !--> ERROR
        endif

        ierr=nf_inq_varid (ncidqbar, 'qbar_time', qbar_tid)
        if (ierr .ne. nf_noerr) then
          write(stdout,3) 'qbar_time', qbarname(1:lstr)
          goto 99                                         !--> ERROR
        endif
!
! River flow
! 
        lvar=lenstr(vname(1,indxQBAR))
        ierr=nf_inq_varid (ncidqbar, vname(1,indxQBAR)(1:lvar), qbar_id)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidqbar, qbar_id, i) 
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxQBAR)(1:lvar), qbarname(1:lstr)
          goto 99                                         !--> ERROR
        endif
!     
! Determine whether there is cycling to reuse the input data and
! find cycling period "qbar_cycle", set initial cycling index
! "qbar_ncycle" and record index "qbar_rec".
! Set initial value for time index "itqbar" and both time record
! bounds to large negative artificial values, so that it will
! trigger the logic in reading part below.
!
! Flow
!
         call set_cycle (ncidqbar,   qbar_tid,    ntqbar,
     &                  qbar_cycle, qbar_ncycle, qbar_rec)
        if (may_day_flag.ne.0) return    !-->  EXIT
        itqbar=2
        qbar_time(1)=-1.E+20
        qbar_time(2)=-1.E+20
        
      endif                     ! iic.eq.0
!
! Reading data from the runoff file:  Get out, if model time is
!======== ==== ==== === ======= =====  already within the interval
! set by the past and future data times. Otherwise flip the time
! index, increment record and cyclin indices and read a new portion
! of data. Repeat it until model time is between the two times from
! data.
!
 1    i=3-itqbar
      cff=time+0.5*dt
      if (qbar_time(i).le.cff .and. cff.lt.qbar_time(itqbar)) return
      ierr=advance_cycle (qbar_cycle, ntqbar, qbar_ncycle, qbar_rec)
      if (ierr .ne. 0) goto 7   !--> ERROR

      ierr=nf_get_var1_FTYPE (ncidqbar, qbar_tid, qbar_rec, cff)

      if (ierr .ne. nf_noerr) then
        write(stdout,6) 'qbar_time', qbar_rec
        goto 99                 !--> ERROR
      endif
      qbar_time(i)=cff*day2sec+qbar_cycle*qbar_ncycle
      if (qbar_time(itqbar).eq.-1.E+20) qbar_time(itqbar)=qbar_time(i)

      s=(/ qbar_rec,1 /)
      c=(/ 1,Nsrc /)

      ierr=nf_get_vara_FTYPE(ncidqbar,qbar_id,s,c,qbarg(1:Nsrc,i))
CR      write(*,*)'====='
CR      write(*,*)'cff',cff,' qbar_rec',qbar_rec,' qbarg(:)',qbarg(1,:)
      if (ierr .ne. nf_noerr) then
        write(stdout,6) 'QBAR', qbar_rec
        goto 99                 !--> ERROR
      endif     

      itqbar=i
      MPI_master_only write(stdout,'(6x,A,1x,A,1x,g12.4,1x,I4)') 
     &     'GET_PSOURCE --',
     &     'Read Run-off flow fields      for time =', cff 

!      OPEN(UNIT=114,file='roms_runoff.out',FORM='FORMATTED',
!     &      access='append',status='OLD')
!      WRITE(114,FMT=110)cff,qbar_rec,(qbarg(is,i),is=1,Nsrc)
!110	FORMAT(g12.4,' ',I4,5(' ',F10.3)/)
!      CLOSE(114)

#ifdef MPI
     &                                                      , mynode
#endif
      if (ntqbar.gt.1) goto 1
      if (ntqbar.eq.1) return     
!
! Sort out error messages: The following portion of the code is
!===== === ===== ========= not accessed unless something goes wrong.
!
  3   format(/,' GET_PSOURCE - ERROR: unable to find forcing variable',
     &                   ': ',a,/,11x,'in runoff NetCDF file: ',a)
  4   write(stdout,5) qbarname(1:lstr)
  5   format(/,' GET_PSOURCE - ERROR: unable to open runoff NetCDF ',
     &                                                  'file: ',a)
      goto 99
  6   format(/,' GET_PSOURCE - ERROR while reading variable: ',a,2x,
     &                                      ' at TIME index = ',i4)
  7   write(stdout,8) qbar_rec, ntqbar, qbarname(1:lstr), tdays,
     &                                      qbar_time(itqbar)*sec2day
  8   format(/,' GET_PSOURCE - ERROR: requested time record ',I4,
     &       1x,'exeeds the last available',  /,  11x,'record ',I4,
     &       1x,'in runoff NetCDF file: ', a, /,   11x,'TDAYS = ',
     &                g12.4,2x,'last available QBAR_TIME = ',g12.4)
  99  may_day_flag=2
  
      return
      end

      subroutine set_psource_tile(Istr,Iend,Jstr,Jend)
!
! Set-up river runoff discharge data for current tile.
!
      implicit none
      integer Istr,Iend,Jstr,Jend,i, it1,it2
      real cff, cff1,cff2
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "sources.h"
# include "compute_extended_bounds.h"
!
CR      write(*,*)'Enter set_psource_tile...'

      it1=3-itqbar
      it2=itqbar
      cff=time+0.5*dt
      cff1=qbar_time(it2)-cff
      cff2=cff-qbar_time(it1)
!
!  Load time invariant QBAR data.
!
      if (qbar_cycle.lt.0.) then
        if (FIRST_TIME_STEP) then
          do i=1,Nsrc
            Qbar(i)=qbarg(i,itqbar)
          enddo
        endif
!
!  Time-interpolate QBAR from point data.
!  Check that for the next time step [when time=time+dt] time+dt 
!  is still between qbar_time(it1) and qbar_time(it2); and if not,
!  set synchro_flag top signal that the new forcing data should be
!  read from the netCDF input file (master thread only).
!
      elseif (cff1.ge.0. .and. cff2.ge.0.) then
        if (ZEROTH_TILE .and. cff1.lt.dt) synchro_flag=.TRUE.
        cff=1./(cff1+cff2)
        cff1=cff1*cff
        cff2=cff2*cff
        do i=1,Nsrc
          Qbar(i)=cff1*qbarg(i,it1)+cff2*qbarg(i,it2)
CR          write(*,*)'i=',i,' -- Qbar(i)=',Qbar(i)
        enddo
!     
!  Unable to set-up QBAR:
!     Complain about the error and signal to quit.      
!     
      else
        if (ZEROTH_TILE) then
          write(stdout,1) 'qbar_time', tdays, qbar_time(it2)*sec2day
  1       format(/,' SET_PSOURCE - current model time exceeds ending',
     &           1x,'value for variable: ',a,/,11x,'TDAYS = ',g12.4,
     &           2x,'TEND = ',g12.4)
          may_day_flag=2
        endif
      endif

      return

      end
#else
      subroutine get_psource_empty
      return
      end
#endif /* PSOURCE_NCFILE */
