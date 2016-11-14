! $Id: get_bry_wkb.F 1514 2014-04-06 09:22:29Z rblod $
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
#if !defined ANA_BRY_WKB && defined WKB_WWAVE

      subroutine get_bry_wkb        ! Read side boundary forcing 
      implicit none                 ! fields from boundary file

# define wac_west 'cdir_west'
# include "param.h"
# include "scalars.h"
# include "ncscrum.h"
# include "netcdf.inc"
# include "boundary.h"
      real cff
      integer  lstr,lvar,lenstr, ierr, ierr_all
      integer  nf_read_bry_EW, nf_read_bry_NS 
!
! Initialization: Check, whether boundary forcing filefor WKB model is
!================ already opened, and if not, open it.  Find and save 
! netCDF IDs for relevant variables.  Determine whether there is cycling 
! to reuse the input data and find cycling period "brywkb_cycle", set
! initial cycling index "brywkb_ncycle" and record index "brywkb_rec",
! time index "itbrywkb" and both time record bounds to large negative
! artificial values, so that it will trigger the logic in reading
! part below.
! Note that the frequency may be different than for other bry files
!
      ierr=nf_noerr
      lstr=lenstr(brywkb_file)
      if (iic.eq.0 ) then
        if (brywkb_id.eq.-1) then
          ierr=nf_open (brywkb_file(1:lstr), nf_nowrite, brywkb_id)
          if (ierr.ne.nf_noerr) write(stdout,'(/1x,4A/)') 'ERROR ',
     &               'in get_bry_wkb: can not open netCDF file ''',
     &                                   brywkb_file(1:lstr),   '''.'
        endif
        if (ierr.eq.nf_noerr) then
          ierr_all=0 
          ierr=nf_inq_varid (brywkb_id, 'brywkb_time',  brywkb_time_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'brywkb_time'
          ierr_all=ierr_all+ierr

# ifdef WKB_OBC_WEST
          ierr=nf_inq_varid (brywkb_id, 'wac_west', wacbry_west_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'wac_west'
          ierr_all=ierr_all+ierr
          ierr=nf_inq_varid (brywkb_id, 'wkx_west', wkxbry_west_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'wkx_west'
          ierr_all=ierr_all+ierr
          ierr=nf_inq_varid (brywkb_id, 'wke_west', wkebry_west_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'wke_west'
          ierr_all=ierr_all+ierr
# endif

# ifdef WKB_OBC_EAST
          ierr=nf_inq_varid (brywkb_id, 'wac_east', wacbry_east_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'wac_east'
          ierr_all=ierr_all+ierr
          ierr=nf_inq_varid (brywkb_id, 'wkx_east', wkxbry_east_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'wkx_east'
          ierr_all=ierr_all+ierr
          ierr=nf_inq_varid (brywkb_id, 'wke_east', wkebry_east_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'wke_east'
          ierr_all=ierr_all+ierr
# endif

# ifdef WKB_OBC_SOUTH
          ierr=nf_inq_varid (brywkb_id, 'wac_south', wacbry_south_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'wac_south'
          ierr_all=ierr_all+ierr
          ierr=nf_inq_varid (brywkb_id, 'wkx_south', wkxbry_south_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'wkx_south'
          ierr_all=ierr_all+ierr
          ierr=nf_inq_varid (brywkb_id, 'wke_south', wkebry_south_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'wke_south'
          ierr_all=ierr_all+ierr
# endif

# ifdef WKB_OBC_NORTH
          ierr=nf_inq_varid (brywkb_id, 'wac_north', wacbry_north_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'wac_north'
          ierr_all=ierr_all+ierr
          ierr=nf_inq_varid (brywkb_id, 'wkx_north', wkxbry_north_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'wkx_north'
          ierr_all=ierr_all+ierr
          ierr=nf_inq_varid (brywkb_id, 'wke_north', wkebry_north_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'wke_north'
          ierr_all=ierr_all+ierr
# endif
          ierr=ierr_all
          if (ierr.eq.nf_noerr) then
            call set_cycle (brywkb_id, brywkb_time_id, ntbrywkb,
     &           brywkb_cycle, brywkb_ncycle, brywkb_rec)
            itbrywkb=1
            brywkb_time(1)=-1.E+20
            brywkb_time(2)=-1.E+20
          else
            write(stdout,'(8x,4A)') 'ERROR(s) occur while examining',
     &          ' content of netCDF file ''', brywkb_file(1:lstr), '''.'
          endif
        endif
      endif
  1   format(' ERROR in get_bry_wkb: cannot find variable ''',A,'''')
!
! Read data from the file:  Check if model time is bounded by past
!===== ==== ==== === =====  and future data times: if not, increment
! record and cycling indices, flip time index and read a new portion
! of data. Repeat until model time falls between the two data times.
!
      do while (brywkb_time(itbrywkb).lt.time+.5*dt .and. ierr.eq.nf_noerr)
        call advance_cycle (brywkb_cycle,ntbrywkb,brywkb_ncycle,brywkb_rec)
        if (ierr.eq.nf_noerr) then
          ierr=nf_get_var1_FTYPE (brywkb_id, brywkb_time_id, brywkb_rec, cff)
          if (ierr.eq.nf_noerr) then
            itbrywkb=min(3-itbrywkb,ntbrywkb)
            brywkb_time(itbrywkb)=cff*day2sec + brywkb_cycle*brywkb_ncycle
            ierr_all=0
#ifdef WKB_OBC_WEST
            ierr=nf_read_bry_EW (wacbry_west_dt(START_1D_ARRAYETA,itbrywkb
     &          ), brywkb_id,wacbry_west_id, brywkb_rec, r2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'wac_west'
            ierr_all=ierr_all+ierr
            ierr=nf_read_bry_EW (wkxbry_west_dt(START_1D_ARRAYETA,itbrywkb
     &          ), brywkb_id,wkxbry_west_id, brywkb_rec, r2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'wkx_west'
            ierr_all=ierr_all+ierr
            ierr=nf_read_bry_EW (wkebry_west_dt(START_1D_ARRAYETA,itbrywkb
     &          ), brywkb_id,wkebry_west_id, brywkb_rec, r2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'wke_west'
            ierr_all=ierr_all+ierr
# endif

# ifdef WKB_OBC_EAST
            ierr=nf_read_bry_EW (wacbry_east_dt(START_1D_ARRAYETA,itbrywkb
     &          ), brywkb_id,wacbry_east_id, brywkb_rec, r2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'wac_east'
            ierr_all=ierr_all+ierr
            ierr=nf_read_bry_EW (wkxbry_east_dt(START_1D_ARRAYETA,itbrywkb
     &          ), brywkb_id,wkxbry_east_id, brywkb_rec, r2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'wkx_east'
            ierr_all=ierr_all+ierr
            ierr=nf_read_bry_EW (wkebry_east_dt(START_1D_ARRAYETA,itbrywkb
     &          ), brywkb_id,wkebry_east_id, brywkb_rec, r2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'wke_east'
            ierr_all=ierr_all+ierr
# endif

# ifdef WKB_OBC_SOUTH
            ierr=nf_read_bry_NS (wacbry_south_dt(START_1D_ARRAYXI,itbrywkb
     &          ), brywkb_id,wacbry_south_id, brywkb_rec,  r2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'wac_south'
            ierr_all=ierr_all+ierr
            ierr=nf_read_bry_NS (wkxbry_south_dt(START_1D_ARRAYXI,itbrywkb
     &          ), brywkb_id,wkxbry_south_id, brywkb_rec,  r2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'wkx_south'
            ierr_all=ierr_all+ierr
            ierr=nf_read_bry_NS (wkebry_south_dt(START_1D_ARRAYXI,itbrywkb
     &          ), brywkb_id,wkebry_south_id, brywkb_rec,  r2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'wke_south'
            ierr_all=ierr_all+ierr
# endif

# ifdef WKB_OBC_NORTH
            ierr=nf_read_bry_NS (wacbry_north_dt(START_1D_ARRAYXI,itbrywkb
     &          ), brywkb_id,wacbry_north_id, brywkb_rec, r2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'wac_north'
            ierr_all=ierr_all+ierr
            ierr=nf_read_bry_NS (wkxbry_north_dt(START_1D_ARRAYXI,itbrywkb
     &          ), brywkb_id,wkxbry_north_id, brywkb_rec, r2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'wkx_north'
            ierr_all=ierr_all+ierr
            ierr=nf_read_bry_NS (wkebry_north_dt(START_1D_ARRAYXI,itbrywkb
     &          ), brywkb_id,wkebry_north_id, brywkb_rec, r2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'wke_north'
            ierr_all=ierr_all+ierr
# endif
            ierr=ierr_all
            if (ierr.eq.0) then
              write(stdout,'(6x,A,9x,A,1x,G12.4,1x,I4)')
     &       'GET_BRY_WKB    -- Read wave boundary data',
     &                                 'for time =', cff 
#ifdef MPI
     &                                             , mynode
#endif
            else
              write(stdout,'(1x,2A,I4/8x,3A)')
     &           'ERROR(s) occur while trying to read record ',
     &            brywkb_rec,  'in file ''',brywkb_file(1:lstr),'''.'
            endif 
          else
            write(stdout,2) 'brywkb_time'
          endif
        else
           write(stdout,'(/1x,A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')
     &    'ERROR in get_bry_all: requested time record ',  brywkb_rec,
     &    'exeeds the last record', ntbrywkb,  'available in netCDF ',
     &    'file ''',   brywkb_file(1:lstr),  '''',  'tdays = ',tdays,
     &    '  but the last available  brywkb_time =',
     &                                   brywkb_time(itbrywkb)*sec2day
        endif
      enddo
  2   format(' ERROR in get_bry: cannot read variable ''',A,'''')
      if (ierr.ne.nf_noerr) may_day_flag=2
      return
      end 

      subroutine set_bry_wkb (tile)
      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call set_bry_wkb_tile (Istr,Iend,Jstr,Jend)
      return
      end


      subroutine set_bry_wkb_tile (Istr,Iend,Jstr,Jend)
!
!--------------------------------------------------------------------
!  Set-up all boundary forcing fields
!--------------------------------------------------------------------
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j, it1,it2
      real cff, cff1,cff2
# include "param.h"
# include "scalars.h"
# include "boundary.h"
!
# include "compute_extended_bounds.h"

      it1=3-itbrywkb
      it2=itbrywkb
      cff=time+0.5*dt
      cff1=brywkb_time(it2)-cff
      cff2=cff-brywkb_time(it1)
      if (ZEROTH_TILE .and. cff1.lt.dt) synchro_flag=.true.

      if (cff1.ge.0. .and. cff2.ge.0.) then
        cff=1./(cff1+cff2)                        ! interpolate
        cff1=cff1*cff                             ! boundary values 
        cff2=cff2*cff                             ! in time 

# ifdef WKB_OBC_WEST
        if (WESTERN_EDGE) then
          do j=JstrR,JendR
            wacbry_west(j)=cff1*wacbry_west_dt(j,it1)
     &                    +cff2*wacbry_west_dt(j,it2)
            wkxbry_west(j)=cff1*wkxbry_west_dt(j,it1)
     &                    +cff2*wkxbry_west_dt(j,it2)
            wkebry_west(j)=cff1*wkebry_west_dt(j,it1)
     &                    +cff2*wkebry_west_dt(j,it2)
          enddo
        endif  
# endif

# ifdef WKB_OBC_EAST
        if (EASTERN_EDGE) then
          do j=JstrR,JendR
            wacbry_east(j)=cff1*wacbry_east_dt(j,it1)
     &                    +cff2*wacbry_east_dt(j,it2)
            wkxbry_east(j)=cff1*wkxbry_east_dt(j,it1)
     &                    +cff2*wkxbry_east_dt(j,it2)
            wkebry_east(j)=cff1*wkebry_east_dt(j,it1)
     &                    +cff2*wkebry_east_dt(j,it2)
          enddo
        endif
# endif

# ifdef WKB_OBC_SOUTH
        if (SOUTHERN_EDGE) then
          do i=IstrR,IendR
            wacbry_south(i)=cff1*wacbry_south_dt(i,it1)
     &                     +cff2*wacbry_south_dt(i,it2)
            wkxbry_south(i)=cff1*wkxbry_south_dt(i,it1)
     &                     +cff2*wkxbry_south_dt(i,it2)
            wkebry_south(i)=cff1*wkebry_south_dt(i,it1)
     &                     +cff2*wkebry_south_dt(i,it2)
          enddo
        endif
# endif

# ifdef WKB_OBC_NORTH
        if (NORTHERN_EDGE) then
          do i=IstrR,IendR
            wacbry_north(i)=cff1*wacbry_north_dt(i,it1)
     &                     +cff2*wacbry_north_dt(i,it2)
            wkxbry_north(i)=cff1*wkxbry_north_dt(i,it1)
     &                     +cff2*wkxbry_north_dt(i,it2)
            wkebry_north(i)=cff1*wkebry_north_dt(i,it1)
     &                     +cff2*wkebry_north_dt(i,it2)
          enddo
        endif
# endif
      elseif (ZEROTH_TILE) then
        write(stdout,'(/2(1x,A)/3(1x,A,F16.10)/)')
     &   'SET_BRY_WKB_TILE - current model time is out of bounds of',
     &   '''brywkb_time''.',     'BRY_WKB_TSTART=', brywkb_time(it1)*sec2day,
     &   'TDAYS=',  tdays,    'BRY_WKB_TEND=', brywkb_time(it2)*sec2day
        may_day_flag=2
      endif
      return
      end
#else
      subroutine get_bry_wkb_empty
      end
#endif

