! $Id: get_bry.F 1584 2014-07-15 12:27:13Z gcambon $
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
#if !defined ANA_BRY && (defined T_FRC_BRY || defined M2_FRC_BRY || \
                         defined M3_FRC_BRY || defined Z_FRC_BRY )

      subroutine get_bry            ! Read side boundary forcing 
      implicit none                 ! fields from boundary file
# include "param.h"
# include "scalars.h"
# include "ncscrum.h"
# include "netcdf.inc"
# include "boundary.h"
      real cff
      integer  lstr,lvar,lenstr, ierr, ierr_all, itrc
      integer  nf_read_bry_EW, nf_read_bry_NS 
!
! Initialization: Check, whether boundary forcing file is already
!================ opened, and if not, open it.  Find and save netCDF
! IDs for relevant variables.  Determine whether there is cycling to
! reuse the input data and find cycling period "bry_cycle", set
! initial cycling index "bry_ncycle" and record index "bry_rec",
! time index "itbry" and both time record bounds to large negative
! artificial values, so that it will trigger the logic in reading
! part below.
!
      ierr=nf_noerr
      lstr=lenstr(bry_file)
      if (iic.eq.0 ) then
        if (bry_id.eq.-1) then
          ierr=nf_open (bry_file(1:lstr), nf_nowrite, bry_id)
          if (ierr.ne.nf_noerr) write(stdout,'(/1x,4A/)') 'ERROR ',
     &               'in get_all_bry: can not open netCDF file ''',
     &                                   bry_file(1:lstr),   '''.'
        endif
        if (ierr.eq.nf_noerr) then
          ierr_all=0 
          ierr=nf_inq_varid (bry_id, 'bry_time',  bry_time_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'bry_time'
          ierr_all=ierr_all+ierr

# ifdef OBC_WEST
#  ifdef Z_FRC_BRY
          ierr=nf_inq_varid (bry_id, 'zeta_west', zetabry_west_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'zeta_west'
          ierr_all=ierr_all+ierr
#  endif
#  ifdef M2_FRC_BRY
          ierr=nf_inq_varid (bry_id, 'ubar_west', ubarbry_west_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'ubar_west'
          ierr_all=ierr_all+ierr
          ierr=nf_inq_varid (bry_id, 'vbar_west', vbarbry_west_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'vbar_west'
          ierr_all=ierr_all+ierr
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
          ierr=nf_inq_varid (bry_id,    'u_west', ubry_west_id)
          if (ierr.ne.nf_noerr)  write(stdout,1) 'u_west'
          ierr_all=ierr_all+ierr
          ierr=nf_inq_varid (bry_id,    'v_west', vbry_west_id)
          if (ierr.ne.nf_noerr)  write(stdout,1) 'v_west'
          ierr_all=ierr_all+ierr
#   endif
#   ifdef T_FRC_BRY
          ierr=nf_inq_varid (bry_id, 'temp_west', 
     &         tbry_west_id(itemp))
          if (ierr.ne.nf_noerr) write(stdout,1) 'temp_west'
          ierr_all=ierr_all+ierr
#    ifdef SALINITY
          ierr=nf_inq_varid (bry_id, 'salt_west', 
     &         tbry_west_id(isalt))
          if (ierr.ne.nf_noerr) write(stdout,1) 'salt_west'
          ierr_all=ierr_all+ierr
#    endif
#   endif
#  endif
# endif

# ifdef OBC_EAST
#  ifdef Z_FRC_BRY
          ierr=nf_inq_varid (bry_id, 'zeta_east', zetabry_east_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'zeta_east'
          ierr_all=ierr_all+ierr
#  endif
#  ifdef M2_FRC_BRY
          ierr=nf_inq_varid (bry_id, 'ubar_east', ubarbry_east_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'ubar_east'
          ierr_all=ierr_all+ierr
          ierr=nf_inq_varid (bry_id, 'vbar_east', vbarbry_east_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'vbar_east'
          ierr_all=ierr_all+ierr
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
          ierr=nf_inq_varid (bry_id,    'u_east',   ubry_east_id)
          if (ierr.ne.nf_noerr)  write(stdout,1)   'u_east'
          ierr_all=ierr_all+ierr
          ierr=nf_inq_varid (bry_id,    'v_east',   vbry_east_id)
          if (ierr.ne.nf_noerr)  write(stdout,1)   'v_east'
          ierr_all=ierr_all+ierr
#   endif
#   ifdef T_FRC_BRY
          ierr=nf_inq_varid (bry_id, 'temp_east', 
     &         tbry_east_id(itemp))
          if (ierr.ne.nf_noerr) write(stdout,1) 'temp_east'
          ierr_all=ierr_all+ierr
#    ifdef SALINITY
          ierr=nf_inq_varid (bry_id, 'salt_east', 
     &         tbry_east_id(isalt))
          if (ierr.ne.nf_noerr) write(stdout,1) 'salt_east'
          ierr_all=ierr_all+ierr
#    endif
#   endif
#  endif
# endif

# ifdef OBC_SOUTH
#  ifdef Z_FRC_BRY
          ierr=nf_inq_varid (bry_id, 'zeta_south', zetabry_south_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'zeta_south'
          ierr_all=ierr_all+ierr
#  endif
#  ifdef M2_FRC_BRY
          ierr=nf_inq_varid (bry_id, 'ubar_south', ubarbry_south_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'ubar_south'
          ierr_all=ierr_all+ierr
          ierr=nf_inq_varid (bry_id, 'vbar_south', vbarbry_south_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'vbar_south'
          ierr_all=ierr_all+ierr
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
          ierr=nf_inq_varid (bry_id,    'u_south',  ubry_south_id)
          if (ierr.ne.nf_noerr) write(stdout,1)    'u_south'
          ierr_all=ierr_all+ierr
          ierr=nf_inq_varid (bry_id,    'v_south',  vbry_south_id)
          if (ierr.ne.nf_noerr) write(stdout,1)    'v_south'
          ierr_all=ierr_all+ierr
#   endif
#   ifdef T_FRC_BRY
          ierr=nf_inq_varid (bry_id, 'temp_south', 
     &         tbry_south_id(itemp))
          if (ierr.ne.nf_noerr) write(stdout,1) 'temp_south'
          ierr_all=ierr_all+ierr
#    ifdef SALINITY
          ierr=nf_inq_varid (bry_id, 'salt_south', 
     &         tbry_south_id(isalt))
          if (ierr.ne.nf_noerr) write(stdout,1) 'salt_south'
          ierr_all=ierr_all+ierr
#    endif
#   endif
#  endif
# endif

# ifdef OBC_NORTH
#  ifdef Z_FRC_BRY
          ierr=nf_inq_varid (bry_id, 'zeta_north', zetabry_north_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'zeta_north'
          ierr_all=ierr_all+ierr
#  endif
#  ifdef M2_FRC_BRY
          ierr=nf_inq_varid (bry_id, 'ubar_north', ubarbry_north_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'ubar_north'
          ierr_all=ierr_all+ierr
          ierr=nf_inq_varid (bry_id, 'vbar_north', vbarbry_north_id)
          if (ierr.ne.nf_noerr) write(stdout,1) 'vbar_north'
          ierr_all=ierr_all+ierr
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
          ierr=nf_inq_varid (bry_id,    'u_north',  ubry_north_id)
          if (ierr.ne.nf_noerr) write(stdout,1)    'u_north'
          ierr_all=ierr_all+ierr
          ierr=nf_inq_varid (bry_id,    'v_north',  vbry_north_id)
          if (ierr.ne.nf_noerr)   write(stdout,1)  'v_north'
          ierr_all=ierr_all+ierr
#   endif
#   ifdef T_FRC_BRY
          ierr=nf_inq_varid (bry_id, 'temp_north', 
     &         tbry_north_id(itemp))
          if (ierr.ne.nf_noerr) write(stdout,1) 'temp_north'
          ierr_all=ierr_all+ierr
#    ifdef SALINITY
          ierr=nf_inq_varid (bry_id, 'salt_north', 
     &         tbry_north_id(isalt))
          if (ierr.ne.nf_noerr) write(stdout,1) 'salt_north'
          ierr_all=ierr_all+ierr
#    endif
#   endif
#  endif
# endif
          ierr=ierr_all
          if (ierr.eq.nf_noerr) then
            call set_cycle (bry_id, bry_time_id, ntbry,
     &           bry_cycle, bry_ncycle, bry_rec)
            itbry=1
            bry_time(1)=-1.E+20
            bry_time(2)=-1.E+20
          else
            write(stdout,'(8x,4A)') 'ERROR(s) occur while examining',
     &          ' content of netCDF file ''', bry_file(1:lstr), '''.'
          endif
        endif
      endif
  1   format(' ERROR in get_all_bry: cannot find variable ''',A,'''')
!
! Read data from the file:  Check if model time is bounded by past
!===== ==== ==== === =====  and future data times: if not, increment
! record and cycling indices, flip time index and read a new portion
! of data. Repeat until model time falls between the two data times.
!
      do while (bry_time(itbry).lt.time+.5*dt .and. ierr.eq.nf_noerr)
        call advance_cycle (bry_cycle,ntbry,bry_ncycle,bry_rec)
        if (ierr.eq.nf_noerr) then
          ierr=nf_get_var1_FTYPE (bry_id, bry_time_id, bry_rec, cff)
          if (ierr.eq.nf_noerr) then
            itbry=min(3-itbry,ntbry)
            bry_time(itbry)=cff*day2sec + bry_cycle*bry_ncycle
            ierr_all=0
#ifdef OBC_WEST
#  ifdef Z_FRC_BRY
            ierr=nf_read_bry_EW (zetabry_west_dt(START_1D_ARRAYETA,itbry
     &          ), bry_id,zetabry_west_id, bry_rec, r2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'zeta_west'
            ierr_all=ierr_all+ierr
#  endif
#  ifdef M2_FRC_BRY
            ierr=nf_read_bry_EW (ubarbry_west_dt(START_1D_ARRAYETA,itbry
     &          ), bry_id,ubarbry_west_id, bry_rec, u2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'ubar_west'
            ierr_all=ierr_all+ierr
            ierr=nf_read_bry_EW (vbarbry_west_dt(START_1D_ARRAYETA,itbry
     &          ), bry_id,vbarbry_west_id, bry_rec, v2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'vbar_west'
            ierr_all=ierr_all+ierr
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
            ierr=nf_read_bry_EW (ubry_west_dt(START_1D_ARRAYETA,1,itbry)
     &          , bry_id,ubry_west_id,  bry_rec,  u3dvar)
            if (ierr.ne.nf_noerr)  write(stdout,2)  'u_west'
            ierr_all=ierr_all+ierr
            ierr=nf_read_bry_EW (vbry_west_dt(START_1D_ARRAYETA,1,itbry)
     &          , bry_id,vbry_west_id,  bry_rec,  v3dvar)
            if (ierr.ne.nf_noerr)  write(stdout,2)  'v_west'
            ierr_all=ierr_all+ierr
#   endif
#   ifdef T_FRC_BRY
            do itrc=1,isalt
              ierr=nf_read_bry_EW (tbry_west_dt(START_1D_ARRAYETA,1
     &            ,itbry,itrc),bry_id,tbry_west_id(itrc),  bry_rec,
     &            r3dvar)
              if (ierr.ne.nf_noerr)  write(stdout,2)  'trc_west'
              ierr_all=ierr_all+ierr
            enddo
#   endif
#  endif
# endif

# ifdef OBC_EAST
#  ifdef Z_FRC_BRY
            ierr=nf_read_bry_EW (zetabry_east_dt(START_1D_ARRAYETA,itbry
     &          ), bry_id,zetabry_east_id, bry_rec, r2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'zeta_east'
            ierr_all=ierr_all+ierr
#  endif
#  ifdef M2_FRC_BRY
            ierr=nf_read_bry_EW (ubarbry_east_dt(START_1D_ARRAYETA,itbry
     &          ), bry_id,ubarbry_east_id, bry_rec, u2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'ubar_east'
            ierr_all=ierr_all+ierr
            ierr=nf_read_bry_EW (vbarbry_east_dt(START_1D_ARRAYETA,itbry
     &          ), bry_id,vbarbry_east_id, bry_rec, v2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'vbar_east'
            ierr_all=ierr_all+ierr
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
            ierr=nf_read_bry_EW (ubry_east_dt(START_1D_ARRAYETA,1,itbry)
     &          , bry_id,ubry_east_id,  bry_rec,  u3dvar)
            if (ierr.ne.nf_noerr)  write(stdout,2)  'u_east'
            ierr_all=ierr_all+ierr
            ierr=nf_read_bry_EW (vbry_east_dt(START_1D_ARRAYETA,1,itbry)
     &          , bry_id,vbry_east_id,  bry_rec,  v3dvar)
            if (ierr.ne.nf_noerr)  write(stdout,2)  'v_east'
            ierr_all=ierr_all+ierr
#   endif
#   ifdef T_FRC_BRY
            do itrc=1,isalt
              ierr=nf_read_bry_EW (tbry_east_dt(START_1D_ARRAYETA,1
     &            ,itbry,itrc),bry_id,tbry_east_id(itrc),  bry_rec,
     &            r3dvar)
              if (ierr.ne.nf_noerr)  write(stdout,2)  'trc_east'
              ierr_all=ierr_all+ierr
            enddo
#   endif
#  endif
# endif

# ifdef OBC_SOUTH
#  ifdef Z_FRC_BRY
            ierr=nf_read_bry_NS (zetabry_south_dt(START_1D_ARRAYXI,itbry
     &          ), bry_id,zetabry_south_id, bry_rec,  r2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'zeta_south'
            ierr_all=ierr_all+ierr
#  endif
#  ifdef M2_FRC_BRY
            ierr=nf_read_bry_NS (ubarbry_south_dt(START_1D_ARRAYXI,itbry
     &          ), bry_id,ubarbry_south_id, bry_rec, u2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'ubar_south'
            ierr_all=ierr_all+ierr
            ierr=nf_read_bry_NS (vbarbry_south_dt(START_1D_ARRAYXI,itbry
     &          ), bry_id,vbarbry_south_id, bry_rec, v2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'vbar_south'
            ierr_all=ierr_all+ierr
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
            ierr=nf_read_bry_NS (ubry_south_dt(START_1D_ARRAYXI,1,itbry)
     &          , bry_id,ubry_south_id,  bry_rec,  u3dvar)
            if (ierr.ne.nf_noerr)  write(stdout,2)  'u_south'
            ierr_all=ierr_all+ierr

            ierr=nf_read_bry_NS (vbry_south_dt(START_1D_ARRAYXI,1,itbry)
     &          , bry_id,vbry_south_id,  bry_rec,  v3dvar)
            if (ierr.ne.nf_noerr)  write(stdout,2)  'v_south'
            ierr_all=ierr_all+ierr
#   endif
#   ifdef T_FRC_BRY
            do itrc=1,isalt
              ierr=nf_read_bry_NS (tbry_south_dt(START_1D_ARRAYXI,1
     &            ,itbry,itrc),bry_id,tbry_south_id(itrc),  bry_rec,
     &            r3dvar)
              if (ierr.ne.nf_noerr) write(stdout,2) 'trc_south'
              ierr_all=ierr_all+ierr
            enddo
#   endif
#  endif
# endif

# ifdef OBC_NORTH
#  ifdef Z_FRC_BRY
            ierr=nf_read_bry_NS (zetabry_north_dt(START_1D_ARRAYXI,itbry
     &          ), bry_id,zetabry_north_id, bry_rec, r2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'zeta_north'
            ierr_all=ierr_all+ierr
#  endif
#  ifdef M2_FRC_BRY
            ierr=nf_read_bry_NS (ubarbry_north_dt(START_1D_ARRAYXI,itbry
     &          ), bry_id,ubarbry_north_id, bry_rec, u2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'ubar_north'
            ierr_all=ierr_all+ierr
            ierr=nf_read_bry_NS (vbarbry_north_dt(START_1D_ARRAYXI,itbry
     &          ), bry_id,vbarbry_north_id, bry_rec, v2dvar)
            if (ierr.ne.nf_noerr) write(stdout,2) 'vbar_north'
            ierr_all=ierr_all+ierr
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
            ierr=nf_read_bry_NS (ubry_north_dt(START_1D_ARRAYXI,1,itbry)
     &          , bry_id,ubry_north_id,  bry_rec,  u3dvar)
            if (ierr.ne.nf_noerr)  write(stdout,2)  'u_north'
            ierr_all=ierr_all+ierr
            ierr=nf_read_bry_NS (vbry_north_dt(START_1D_ARRAYXI,1,itbry)
     &          , bry_id,vbry_north_id,  bry_rec,  v3dvar)
            if (ierr.ne.nf_noerr)  write(stdout,2)  'v_north'
            ierr_all=ierr_all+ierr
#   endif
#   ifdef T_FRC_BRY
            do itrc=1,isalt
              ierr=nf_read_bry_NS (tbry_north_dt(START_1D_ARRAYXI,1
     &            ,itbry,itrc),bry_id,tbry_north_id(itrc),  bry_rec,
     &            r3dvar)
              if (ierr.ne.nf_noerr)  write(stdout,2)  'trc_north'
              ierr_all=ierr_all+ierr
            enddo
#   endif
#  endif
# endif
            ierr=ierr_all
            if (ierr.eq.0) then
              MPI_master_only write(stdout,'(6x,A,9x,A,1x,G12.4,1x,I4)')
     &       'GET_BRY    -- Read all boundary data',
     &                                 'for time =', cff 
#ifdef MPI
     &                                             , mynode
#endif
            else
              MPI_master_only write(stdout,'(1x,2A,I4/8x,3A)')
     &           'ERROR(s) occur while trying to read record ',
     &            bry_rec,  'in file ''',bry_file(1:lstr),'''.'
            endif 
          else
            MPI_master_only write(stdout,2) 'bry_time'
          endif
        else
         MPI_master_only write(stdout,
     &    '(/1x,A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')
     &    'ERROR in get_bry_all: requested time record ',  bry_rec,
     &    'exeeds the last record', ntbry,  'available in netCDF ',
     &    'file ''',   bry_file(1:lstr),  '''',  'tdays = ', tdays,
     &    '  but the last available  bry_time =',
     &                                   bry_time(itbry)*sec2day
        endif
      enddo
  2   format(' ERROR in get_bry: cannot read variable ''',A,'''')
      if (ierr.ne.nf_noerr) may_day_flag=2
      return
      end 

      subroutine set_bry (tile)
      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call set_bry_tile (Istr,Iend,Jstr,Jend)
      return
      end


      subroutine set_bry_tile (Istr,Iend,Jstr,Jend)
!
!--------------------------------------------------------------------
!  Set-up all boundary forcing fields
!--------------------------------------------------------------------
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k, it1,it2, itrc
      real cff, cff1,cff2
# include "param.h"
# include "grid.h"
# include "scalars.h"
# include "boundary.h"
!
# include "compute_extended_bounds.h"

      it1=3-itbry
      it2=itbry
      cff=time+0.5*dt
      cff1=bry_time(it2)-cff
      cff2=cff-bry_time(it1)
      if (ZEROTH_TILE .and. cff1.lt.dt) synchro_flag=.true.

      if (cff1.ge.0. .and. cff2.ge.0.) then
        cff=1./(cff1+cff2)                        ! interpolate
        cff1=cff1*cff                             ! boundary values 
        cff2=cff2*cff                             ! in time 

# ifdef OBC_WEST
        if (WESTERN_EDGE) then
#  ifdef Z_FRC_BRY
          do j=JstrR,JendR
            zetabry_west(j)=cff1*zetabry_west_dt(j,it1)
     &                     +cff2*zetabry_west_dt(j,it2)
#   ifdef NBQ
     &                                      +h(IstrR,j)
#   endif
          enddo
#  endif
#  ifdef M2_FRC_BRY
          do j=JstrR,JendR
            ubarbry_west(j)=cff1*ubarbry_west_dt(j,it1)
     &                     +cff2*ubarbry_west_dt(j,it2)
            vbarbry_west(j)=cff1*vbarbry_west_dt(j,it1)
     &                     +cff2*vbarbry_west_dt(j,it2)
          enddo
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
          do k=1,N
            do j=JstrR,JendR
              ubry_west(j,k)=cff1*ubry_west_dt(j,k,it1)
     &                      +cff2*ubry_west_dt(j,k,it2)
              vbry_west(j,k)=cff1*vbry_west_dt(j,k,it1)
     &                      +cff2*vbry_west_dt(j,k,it2)
            enddo
          enddo
#   endif
#   ifdef T_FRC_BRY
          do itrc=1,isalt
            do k=1,N
              do j=JstrR,JendR
                tbry_west(j,k,itrc)=cff1*tbry_west_dt(j,k,it1,itrc)
     &                             +cff2*tbry_west_dt(j,k,it2,itrc)
              enddo
            enddo
          enddo
#   endif
#  endif
        endif
# endif

# ifdef OBC_EAST
        if (EASTERN_EDGE) then
#  ifdef Z_FRC_BRY
          do j=JstrR,JendR
            zetabry_east(j)=cff1*zetabry_east_dt(j,it1)
     &                     +cff2*zetabry_east_dt(j,it2)
#   ifdef NBQ
     &                                      +h(IendR,j)
#   endif
          enddo
#  endif
#  ifdef M2_FRC_BRY
          do j=JstrR,JendR
            ubarbry_east(j)=cff1*ubarbry_east_dt(j,it1)
     &                     +cff2*ubarbry_east_dt(j,it2)
            vbarbry_east(j)=cff1*vbarbry_east_dt(j,it1)
     &                     +cff2*vbarbry_east_dt(j,it2)
          enddo
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
          do k=1,N
            do j=JstrR,JendR
              ubry_east(j,k)=cff1*ubry_east_dt(j,k,it1)
     &                      +cff2*ubry_east_dt(j,k,it2)
              vbry_east(j,k)=cff1*vbry_east_dt(j,k,it1)
     &                      +cff2*vbry_east_dt(j,k,it2)
            enddo
          enddo
#   endif
#   ifdef T_FRC_BRY
          do itrc=1,isalt
            do k=1,N
              do j=JstrR,JendR
                tbry_east(j,k,itrc)=cff1*tbry_east_dt(j,k,it1,itrc)
     &                             +cff2*tbry_east_dt(j,k,it2,itrc)
              enddo
            enddo
          enddo
#   endif
#  endif
        endif
# endif

# ifdef OBC_SOUTH
        if (SOUTHERN_EDGE) then
#  ifdef Z_FRC_BRY
          do i=IstrR,IendR
            zetabry_south(i)=cff1*zetabry_south_dt(i,it1)
     &                      +cff2*zetabry_south_dt(i,it2)
#   ifdef NBQ
     &                                        +h(i,JstrR)
#   endif
          enddo
#  endif
#  ifdef M2_FRC_BRY
          do i=IstrR,IendR
            ubarbry_south(i)=cff1*ubarbry_south_dt(i,it1)
     &                      +cff2*ubarbry_south_dt(i,it2)
            vbarbry_south(i)=cff1*vbarbry_south_dt(i,it1)
     &                      +cff2*vbarbry_south_dt(i,it2)
          enddo
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
          do k=1,N
            do i=IstrR,IendR
              ubry_south(i,k)=cff1*ubry_south_dt(i,k,it1)
     &                       +cff2*ubry_south_dt(i,k,it2)
              vbry_south(i,k)=cff1*vbry_south_dt(i,k,it1)
     &                       +cff2*vbry_south_dt(i,k,it2)
            enddo
          enddo
#   endif
#   ifdef T_FRC_BRY
          do itrc=1,isalt
            do k=1,N
              do i=IstrR,IendR
                tbry_south(i,k,itrc)=cff1*tbry_south_dt(i,k,it1,itrc)
     &                              +cff2*tbry_south_dt(i,k,it2,itrc)
              enddo
            enddo
          enddo
#   endif
#  endif
        endif
# endif

# ifdef OBC_NORTH
        if (NORTHERN_EDGE) then
#  ifdef Z_FRC_BRY
          do i=IstrR,IendR
            zetabry_north(i)=cff1*zetabry_north_dt(i,it1)
     &                      +cff2*zetabry_north_dt(i,it2)
#   ifdef NBQ
     &                                        +h(i,JendR)
#   endif
          enddo
#  endif
#  ifdef M2_FRC_BRY
          do i=IstrR,IendR
            ubarbry_north(i)=cff1*ubarbry_north_dt(i,it1)
     &                      +cff2*ubarbry_north_dt(i,it2)
            vbarbry_north(i)=cff1*vbarbry_north_dt(i,it1)
     &                      +cff2*vbarbry_north_dt(i,it2)
          enddo
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
          do k=1,N
            do i=IstrR,IendR
              ubry_north(i,k)=cff1*ubry_north_dt(i,k,it1)
     &                       +cff2*ubry_north_dt(i,k,it2)
              vbry_north(i,k)=cff1*vbry_north_dt(i,k,it1)
     &                       +cff2*vbry_north_dt(i,k,it2)
            enddo
          enddo
#   endif
#   ifdef T_FRC_BRY
          do itrc=1,isalt
            do k=1,N
              do i=IstrR,IendR
                tbry_north(i,k,itrc)=cff1*tbry_north_dt(i,k,it1,itrc)
     &                              +cff2*tbry_north_dt(i,k,it2,itrc)
              enddo
            enddo
          enddo
#   endif
#  endif
        endif
# endif
      elseif (ZEROTH_TILE) then
        MPI_master_only write(stdout,'(/2(1x,A)/3(1x,A,F16.10)/)')
     &   'SET_BRY_ALL_TILE - current model time is out of bounds of',
     &   '''bry_time''.',     'BRY_TSTART=',   bry_time(it1)*sec2day,
     &   'TDAYS=',  tdays,    'BRY_TEND=',     bry_time(it2)*sec2day
        may_day_flag=2
      endif
      return
      end
#else
      subroutine get_bry_empty
      end
#endif

