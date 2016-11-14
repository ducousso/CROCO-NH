! $Id: get_sss.F 1458 2014-02-03 15:01:25Z gcambon $
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
#if defined SALINITY && defined SFLX_CORR && !defined ANA_SSS

      subroutine get_sss
!
!  Read in point or grided sea surface salinity used to correct
!  the salinity fluxes at the appropriate
!  time from forcing NetCDF file.
!
!  These forcing fields are used when salt flux correction is activated:
!
!    SSSFLX_model ~ SSS*(E-P) + CST * (SSS_model - SSS)
!
!  we use DQDSST for CST.... 
!

# define SSS_DATA
      implicit none
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "netcdf.inc"
# include "ncscrum.h"
      real cff
      integer i,ierr, lstr,lvar,lenstr, nf_fread, advance_cycle
!
! Initialization: Inquire about the contents of forcing NetCDF file:
!================  variables and dimensions. Check for consistency.
!
      if (may_day_flag.ne.0) return      !-->  EXIT
      if (itsss.eq.0 .or. iic.eq.0) then
        lstr=lenstr(frcname)
!
! If not opened yet, open forcing NetCDF file for reading.
! Find and save IDs for relevant variables, determine whether
! SSS is a field or scalar value.
!
        if (ncidfrc.eq.-1) then
          ierr=nf_open(frcname(1:lstr), nf_nowrite, ncidfrc)
          if (ierr. ne. nf_noerr) goto 4                  !--> ERROR
        endif

        ierr=nf_inq_varid (ncidfrc, 'sss_time', sss_tid)
        if (ierr .ne. nf_noerr) then
          write(stdout,3) 'sss_time', frcname(1:lstr)
          goto 99                                         !--> ERROR
        endif

        lvar=lenstr(vname(1,indxSSS))
        ierr=nf_inq_varid (ncidfrc, vname(1,indxSSS)(1:lvar), sss_id)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidfrc, sss_id, i) 
          if (ierr. eq. nf_noerr) then
            if (i.gt.1) then
              lsssgrd=1
            else
              lsssgrd=0
            endif
          endif
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxSSS)(1:lvar), frcname(1:lstr)
          goto 99                                         !--> ERROR
        endif

# if defined SFLX_CORR && !defined QCORRECTION
        lvar=lenstr(vname(1,indxdQdSST))
        ierr=nf_inq_varid (ncidfrc, vname(1,indxdQdSST)(1:lvar),
     &                                                    dqdt_id)
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxdQdSST)(1:lvar),frcname(1:lstr)
          goto 99                                         !--> ERROR
        endif
# endif
!
! Determine whether there is cycling to reuse the input data and
! find cycling period "sss_cycle", set initial cycling index
! "sss_ncycle" and record index "sss_rec".
! Set initial value for time index "itsss" and both time record
! bounds to large negative artificial values, so that it will
! trigger the logic in reading part below.
! Also set scale factor to convert input dQdSST from Watts/m2/Celsius
! to meter/second.
!
        call set_cycle (ncidfrc,   sss_tid,    ntsss,
     &                  sss_cycle, sss_ncycle, sss_rec)
        if (may_day_flag.ne.0) return    !-->  EXIT
        itsss=2
        sss_time(1)=-1.E+20
        sss_time(2)=-1.E+20
# if defined SFLX_CORR && !defined QCORRECTION
        scldqdt=1./(rho0*Cp)
# endif
      endif
!
! Reading data from the forcing file:  Get out, if model time is
!======== ==== ==== === ======= =====  already within the interval
! set by the past and future data times. Otherwise flip the time
! index, increment record and cyclin indices and read a new portion
! of data. Repeat it until model time is between the two times from
! data.
!
  1   i=3-itsss
       cff=time+0.5*dt
        if (sss_time(i).le.cff .and. cff.lt.sss_time(itsss)) return
        ierr=advance_cycle (sss_cycle, ntsss, sss_ncycle, sss_rec)
        if (ierr .ne. 0) goto 7                           !--> ERROR

        ierr=nf_get_var1_FTYPE (ncidfrc, sss_tid, sss_rec, cff)
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'sss_time', sss_rec
          goto 99                                         !--> ERROR
        endif
        sss_time(i)=cff*day2sec+sss_cycle*sss_ncycle
        if (sss_time(itsss).eq.-1.E+20) sss_time(itsss)=sss_time(i)

        if (lsssgrd.eq.1) then
          ierr=nf_fread (sssg(START_2D_ARRAY,i), ncidfrc, sss_id,
     &                                           sss_rec, r2dvar)
        else
          ierr=nf_get_var1_FTYPE (ncidfrc, sss_id, sss_rec, sssp(i))
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'SSS', sss_rec
          goto 99                                         !--> ERROR
        endif

# if defined SFLX_CORR && !defined QCORRECTION
        if (lsssgrd.eq.1) then
          ierr=nf_fread (dqdtg(START_2D_ARRAY,i), ncidfrc, dqdt_id,
     &                                            sss_rec,  r2dvar)
        else
          ierr=nf_get_var1_FTYPE (ncidfrc,dqdt_id,sss_rec,dqdtp(i))
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'dQdSST', sss_rec
          goto 99                                         !--> ERROR
        endif
# endif
       itsss=i
# if defined SFLX_CORR && !defined QCORRECTION
      MPI_master_only write(stdout,'(6x,A,1x,A,1x,g12.4,1x,I4)') 
     &              'GET_SSS    --',
     &              'Read SSS and dQdSST fields     for time =', cff
# else
      MPI_master_only write(stdout,'(6x,A,16x,A,1x,g12.4,1x,I4)') 
     &                  'GET_SSS    -- Read SSS fields',
     &                                     'for time =', cff
# endif
#ifdef MPI
     &                                                 , mynode
#endif
      if (ntsss.gt.1) goto 1
      if (ntsss.eq.1) return
!
! Sort out error messages: The following portion of the code is
!===== === ===== ========= not accessed unless something goes wrong.
!
  3   format(/,' GET_SSS - ERROR: unable to find forcing variable',
     &                   ': ',a,/,11x,'in forcing NetCDF file: ',a)
  4   write(stdout,5) frcname(1:lstr)
  5   format(/,' GET_SSS - ERROR: unable to open forcing NetCDF ',
     &                                                  'file: ',a)
      goto 99
  6   format(/,' GET_SSS - ERROR while reading variable: ',a,2x,
     &                                      ' at TIME index = ',i4)
  7   write(stdout,8) sss_rec, ntsss, frcname(1:lstr), tdays,
     &                                      sss_time(itsss)*sec2day
  8   format(/,' GET_SSS - ERROR: requested time record ',I4,
     &       1x,'exeeds the last available',  /,  11x,'record ',I4,
     &       1x,'in forcing NetCDF file: ', a, /,   11x,'TDAYS = ',
     &                g12.4,2x,'last available SSS_TIME = ',g12.4)
  99  may_day_flag=2
      return
      end



      subroutine set_sss_tile (Istr,Iend,Jstr,Jend)
!
! Set-up sea surface salinity data for current tile.
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j, it1,it2
      real cff, cff1,cff2, cff3,cff4, val1, val2
# define SSS_DATA
# define SST_DATA /* PIERRICK */
# include "param.h"
# include "forces.h"
# include "scalars.h"
!
# include "compute_extended_bounds.h"
!
      it1=3-itsss
      it2=itsss
      cff=time+0.5*dt
      cff1=sss_time(it2)-cff
      cff2=cff-sss_time(it1)
!
!  Load time invariant SSS data.
!
      if (sss_cycle.lt.0.) then
        if (FIRST_TIME_STEP) then
          if (lsssgrd.eq.1) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                sss(i,j)=sssg(i,j,itsss)
# if defined SFLX_CORR && !defined QCORRECTION
                dqdt(i,j)=scldqdt*dqdtg(i,j,itsss)
# endif
              enddo
            enddo
          else
            val1=sssp(itsss)
# if defined SFLX_CORR && !defined QCORRECTION
            val2=scldqdt*dqdtp(itsss)
# endif
            do j=JstrR,JendR
              do i=IstrR,IendR
                sss(i,j)=val1
# if defined SFLX_CORR && !defined QCORRECTION
                dqdt(i,j)=val2
# endif
              enddo
            enddo
          endif
        endif
!
!  Time-interpolate SSS from grided or point data.
!  Check that for the next time step [when time=time+dt] time+dt 
!  is still between sss_time(it1) and sss_time(it2); and if not,
!  set synchro_flag top signal that the new forcing data should be
!  read from the netCDF input file (master thread only).
!
      elseif (cff1.ge.0. .and. cff2.ge.0.) then
        if (ZEROTH_TILE .and. cff1.lt.dt) synchro_flag=.TRUE.
# if defined SFLX_CORR && !defined QCORRECTION
        cff=scldqdt/(cff1+cff2)
        cff3=cff1*cff
        cff4=cff2*cff
# endif
        cff=1./(cff1+cff2)
        cff1=cff1*cff
        cff2=cff2*cff
        if (lsssgrd.eq.1) then
          do j=JstrR,JendR
            do i=IstrR,IendR
              sss(i,j)=cff1*sssg(i,j,it1)+cff2*sssg(i,j,it2)
# if defined SFLX_CORR && !defined QCORRECTION
              dqdt(i,j)=cff3*dqdtg(i,j,it1)+cff4*dqdtg(i,j,it2)
# endif
            enddo
          enddo
        else
          val1=cff1*sssp(it1)+cff2*sssp(it2)
# if defined SFLX_CORR && !defined QCORRECTION
          val2=cff3*dqdtp(it1)+cff4*dqdtp(it2)
# endif
          do j=JstrR,JendR
            do i=IstrR,IendR
              sss(i,j)=val1
# if defined SFLX_CORR && !defined QCORRECTION
              dqdt(i,j)=val2
# endif
            enddo
          enddo
        endif
!
!  Unable to set-up SSS:
!  Complain about the error and signal to quit.      
!
      else
        if (ZEROTH_TILE) then
          write(stdout,1) 'sss_time', tdays, sss_time(it2)*sec2day
  1       format(/,' SET_SSS - current model time exceeds ending',
     &           1x,'value for variable: ',a,/,11x,'TDAYS = ',g12.4,
     &           2x,'TEND = ',g12.4)
          may_day_flag=2
        endif
      endif
      return
      end
#else
      subroutine get_sss_empty
      return
      end
#endif /* SALINITY && SFLX_CORR && !defined ANA_SSS */
