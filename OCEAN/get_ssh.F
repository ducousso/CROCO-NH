! $Id: get_ssh.F 1458 2014-02-03 15:01:25Z gcambon $
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
#if defined ZCLIMATOLOGY && !defined ANA_SSH
                                         ! Read sea surface height
      subroutine get_ssh                 ! at appropriate time from
                                         ! climatology NetCDF file.
# define SSH_DATA
      implicit none
# include "param.h"
# include "scalars.h"
# include "ncscrum.h"
# include "climat.h"
#include "netcdf.inc"
      real cff
      integer i, lstr,lvar, lenstr,
     &      ierr, nf_fread, advance_cycle
!
! Initialization: Inquire about the contents of forcing NetCDF file:
!================  variables and dimensions. Check for consistency.
!
      if (may_day_flag.ne.0) return      !-->  EXIT
      if (itssh.eq.0 .or. iic.eq.0) then
        lstr=lenstr(clmname)
!
! If not opened yet, open climatology NetCDF file for reading.
! Check for availability of SSH in input netCDF file and save
! their IDs. Signal to terminate, if they are not found.
!
        if (ncidclm.eq.-1) then
          ierr=nf_open(clmname(1:lstr), nf_nowrite, ncidclm)
          if (ierr .ne. NF_NOERR)  goto 4                  !--> ERROR 
        endif

        ierr=nf_inq_varid (ncidclm, 'ssh_time', ssh_tid)
        if (ierr .ne. nf_noerr) then
          write(stdout,3) 'ssh_time', clmname(1:lstr)
          goto 99                                         !--> ERROR
        endif

        lvar=lenstr(vname(1,indxSSH))
        ierr=nf_inq_varid (ncidclm, vname(1,indxSSH)(1:lvar), ssh_id)
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxSSH)(1:lvar), clmname(1:lstr)
          goto 99                                         !--> ERROR
        endif
!
! Determine whether there is cycling to reuse the input data and
! find cycling period "ssh_cycle", set initial cycling index
! "ssh_ncycle" and record index "ssh_rec".
! Set initial value for time index "itssh" and both time record
! bounds to large negative artificial values, so that it will
! trigger the logic in reading part below.
!
        call set_cycle (ncidclm,   ssh_tid,    ntssh,
     &                  ssh_cycle, ssh_ncycle, ssh_rec)
        if (may_day_flag.ne.0) return    !-->  EXIT
        itssh=2
        ssh_time(1)=-1.E+20
        ssh_time(2)=-1.E+20
      endif
!
! Reading data from the forcing file:  Get out, if model time is
!======== ==== ==== === ======= =====  already within the interval
! set by the past and future data times. Otherwise flip the time
! index, increment record and cyclin indices and read a new portion
! of data. Repeat it until model time is between the two times from
! data.
!
  1   i=3-itssh
       cff=time+0.5*dt
        if (ssh_time(i).le.cff .and. cff.lt.ssh_time(itssh)) return
        ierr=advance_cycle (ssh_cycle, ntssh, ssh_ncycle, ssh_rec)
        if (ierr.ne.0) goto 7                           !--> ERROR

        ierr=nf_get_var1_FTYPE(ncidclm, ssh_tid, ssh_rec, cff)
        if (ierr.ne.NF_NOERR) then
          write(stdout,6) 'ssh_time', ssh_rec
          goto 99                                         !--> ERROR
        endif
        ssh_time(i)=cff*day2sec+ssh_cycle*ssh_ncycle
        if (ssh_time(itssh).eq.-1.E+20) ssh_time(itssh)=ssh_time(i)

        ierr=nf_fread(sshg(START_2D_ARRAY,i), ncidclm, ssh_id,
     &                                          ssh_rec, r2dvar)
        if (ierr.ne.NF_NOERR) then
          write(stdout,6) 'SSH', ssh_rec
          goto 99                                         !--> ERROR
        endif
        MPI_master_only write(stdout,2) cff
        itssh=i
      if (ntssh.gt.1) goto 1
      if (ntssh.eq.1) return

  2   format(6x,'GET_SSH     - Read SSH climatology',11x,
     &                                        'for time = ',g12.4)
!
! Sort out error messages: The following portion of the code is
!===== === ===== ========= not accessed unless something goes wrong.
!
  3   format(/,' GET_SSH - unable to find forcing variable: ',a,
     &       /,15x,'in forcing NetCDF file: ',a)

  4   write(stdout,5) clmname(1:lstr)
  5   format(/,' GET_SSH - unable to open forcing NetCDF file: ',a)
      goto 99

  6   format(/,' GET_SSH - ERROR while reading variable: ',a,2x,
     &                                      ' at TIME index = ',i4)
  7   write(stdout,8) ssh_rec, ntssh, frcname(1:lstr), tdays,
     &                                      ssh_time(itssh)*sec2day
  8   format(/,' GET_SSH - ERROR: requested time record ',I4,
     &       1x,'exeeds the last available',  /,  11x,'record ',I4,
     &       1x,'in forcing NetCDF file: ', a, /,   11x,'TDAYS = ',
     &                g12.4,2x,'last available SSH_TIME = ',g12.4)
  99  may_day_flag=2
      return
      end

      subroutine set_ssh (tile)
      implicit none
      integer tile
# include "param.h"
#ifdef  ALLOW_SINGLE_BLOCK_MODE
C$    integer  trd, omp_get_thread_num
#endif
# include "compute_tile_bounds.h"
      call set_ssh_tile (Istr,Iend,Jstr,Jend)
      return
      end

      subroutine set_ssh_tile (Istr,Iend,Jstr,Jend)
!
! Set-up sea surface height climatology for current tile.
!
# define SSH_DATA
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j, it1,it2
      real cff,cff1,cff2
# include "param.h"
# include "grid.h"
# include "climat.h"
# include "scalars.h"
!
# include "compute_extended_bounds.h"
!
      it1=3-itssh
      it2=itssh
      cff=time+0.5*dt
      cff1=ssh_time(it2)-cff
      cff2=cff-ssh_time(it1)
!
! Load time invariant sea surface height.
!
      if (ssh_cycle.lt.0.) then
        if (iic.eq.0) then
          do j=JstrR,JendR
            do i=IstrR,IendR
              ssh(i,j)=sshg(i,j,itssh)
            enddo
          enddo
        endif
!
! Time-interpolate sea surface height.
! Check that for the next time step [when time=time+dt] time+dt 
! is still between srf_tintrp(it1) and srf_tintrp(it2); and if not,
! set synchro_flag top signal that the new forcing data should be
! read from the netCDF input file (master thread only).
!
      elseif (cff1.ge.0. .and. cff2.ge.0.) then
        if (ZEROTH_TILE .and. cff1.lt.dt) synchro_flag=.TRUE.
        cff=1./(cff1+cff2)
        cff1=cff1*cff
        cff2=cff2*cff
        do j=JstrR,JendR
          do i=IstrR,IendR
            ssh(i,j)=cff1*sshg(i,j,it1)+cff2*sshg(i,j,it2)
# ifdef NBQ
     &                                             +h(i,j)
# endif
          enddo
        enddo
!
! Unable to set-up sea surface height:
! Complain about the error and signal to quit (ONE THREAD ONLY).
!
      else
        if (ZEROTH_TILE) then
          write(stdout,1) 'ssh_time', tdays, ssh_time(it2)*sec2day
  1       format(/,' SET_SSH - current model time exceeds ending',
     &           1x,'value for variable: ',a,/,11x,'TDAYS = ',g12.4,
     &           2x,'TEND = ',g12.4)
          may_day_flag=2
        endif
      endif

# ifdef WET_DRY
      do j=JstrR,JendR
        do i=IstrR,IendR
           if (ssh(i,j) .lt. Dcrit(i,j)-h(i,j)) then
               ssh(i,j)=Dcrit(i,j)-h(i,j)
           endif
        enddo
      enddo
# endif

      return
      end
#else
      subroutine get_ssh_empty
      end
#endif /* ZNUDGING && !defined ANA_SSH */

