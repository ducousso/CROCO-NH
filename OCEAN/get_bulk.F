! $Id: get_bulk.F 1564 2014-06-24 17:39:21Z gcambon $
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
#if defined BULK_FLUX

      subroutine get_bulk
!
!  Read in wind speed and surface air temperature
!
!

      implicit none
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "netcdf.inc"
# include "ncscrum.h"
      real    cff
      integer i,ierr, lstr,lvar,lenstr, nf_fread, advance_cycle
!
! Initialization: Inquire about the contents of forcing NetCDF file:
!================  variables and dimensions. Check for consistency.
!
      if (may_day_flag.ne.0) return      !-->  EXIT
      if (itbulk.eq.0 .or. iic.eq.0) then
        lstr=lenstr(bulkname)
c*        call opencdf (bulkname,N)
c*        if (may_day_flag.ne.0) return    !-->  EXIT
!
! If not opened yet, open forcing NetCDF file for reading.
! Find and save IDs for relevant variables, determine whether
! SST is a field or scalar value.
!
        if (ncidbulk.eq.-1) then
          ierr=nf_open(bulkname(1:lstr), nf_nowrite, ncidbulk)
          if (ierr. ne. nf_noerr) goto 4                  !--> ERROR
        endif

        ierr=nf_inq_varid (ncidbulk, 'bulk_time', bulk_tid)
        if (ierr .ne. nf_noerr) then
          write(stdout,3) 'bulk_time', bulkname(1:lstr)
          goto 99                                         !--> ERROR
        endif

        lvar=lenstr(vname(1,indxWSPD))
        ierr=nf_inq_varid (ncidbulk, vname(1,indxWSPD)(1:lvar), wspd_id)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidbulk, wspd_id, i) 
          if (ierr. eq. nf_noerr) then
            if (i.gt.1) then
              lwspdgrd=1
            else
              lwspdgrd=0
            endif
          endif
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxWSPD)(1:lvar), bulkname(1:lstr)
          goto 99                                         !--> ERROR
        endif

        lvar=lenstr(vname(1,indxTAIR))
        ierr=nf_inq_varid (ncidbulk, vname(1,indxTAIR)(1:lvar),tair_id)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidbulk, tair_id, i)
          if (ierr. eq. nf_noerr) then
            if (i.gt.1) then
              ltairgrd=1
            else
              ltairgrd=0
            endif
          endif
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxTAIR)(1:lvar),bulkname(1:lstr)
          goto 99                                         !--> ERROR
        endif

        lvar=lenstr(vname(1,indxRHUM))
        ierr=nf_inq_varid (ncidbulk, vname(1,indxRHUM)(1:lvar),rhum_id)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidbulk, rhum_id, i)
          if (ierr. eq. nf_noerr) then
            if (i.gt.1) then
              lrhumgrd=1
            else
              lrhumgrd=0
            endif
          endif
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxRHUM)(1:lvar),bulkname(1:lstr)
          goto 99                                         !--> ERROR
        endif

        lvar=lenstr(vname(1,indxRADLW))
        ierr=nf_inq_varid (ncidbulk,vname(1,indxRADLW)(1:lvar),radlw_id)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidbulk, radlw_id, i)
          if (ierr. eq. nf_noerr) then
            if (i.gt.1) then
              lradlwgrd=1
            else
              lradlwgrd=0
            endif
          endif
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxRADLW)(1:lvar),bulkname(1:lstr)
          goto 99                                         !--> ERROR
        endif

        lvar=lenstr(vname(1,indxShflx_rsw))
        ierr=nf_inq_varid (ncidbulk,vname(1,indxShflx_rsw)
     &                                                (1:lvar),radsw_id)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidbulk, radsw_id, i)
          if (ierr. eq. nf_noerr) then
            if (i.gt.1) then
              lradswgrd=1
            else
              lradswgrd=0
            endif
          endif
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxShflx_rsw)(1:lvar),
     &                                                bulkname(1:lstr)
          goto 99                                        !--> ERROR
        endif

# ifdef DIURNAL_INPUT_SRFLX
        lvar=lenstr(vname(1,indxShflx_rswbio))
        ierr=nf_inq_varid (ncidbulk,vname(1,indxShflx_rswbio)(1:lvar)
     &                                              ,radswbio_id)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidbulk, radswbio_id, i)
          if (ierr. eq. nf_noerr) then
            if (i.gt.1) then
              lradswbiogrd=1
            else
              lradswbiogrd=0
            endif
          endif
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxShflx_rswbio)(1:lvar),
     &                                              bulkname(1:lstr)
          goto 99                                         !--> ERROR
        endif
# endif /* DIURNAL_INPUT_SRFLX */

# ifdef BULK_EP
        lvar=lenstr(vname(1,indxPRATE))
        ierr=nf_inq_varid (ncidbulk,vname(1,indxPRATE)(1:lvar),prate_id)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidbulk, prate_id, i)
          if (ierr. eq. nf_noerr) then
            if (i.gt.1) then
              lprategrd=1
            else
              lprategrd=0
            endif
          endif
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxPRATE)(1:lvar),bulkname(1:lstr)
          goto 99                                         !--> ERROR
        endif
# endif /* BULK_EP */
# ifdef BULK_SMFLUX
#  ifdef BULK_SM_UPDATE
        lvar=lenstr(vname(1,indxUWND))
        ierr=nf_inq_varid (ncidbulk,vname(1,indxUWND)(1:lvar),uwnd_id)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidbulk, uwnd_id, i)
          if (ierr. eq. nf_noerr) then
            if (i.gt.1) then
              luwndgrd=1
            else
              luwndgrd=0
            endif
          endif
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxUWND)(1:lvar),bulkname(1:lstr)
          goto 99                                         !--> ERROR
        endif

        lvar=lenstr(vname(1,indxVWND))
        ierr=nf_inq_varid (ncidbulk,vname(1,indxVWND)(1:lvar),vwnd_id)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidbulk, vwnd_id, i)
          if (ierr. eq. nf_noerr) then
            if (i.gt.1) then
              lvwndgrd=1
            else
              lvwndgrd=0
            endif
          endif
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxVWND)(1:lvar),bulkname(1:lstr)
          goto 99                                         !--> ERROR
        endif

# else
        lvar=lenstr(vname(1,indxSUSTR))
        ierr=nf_inq_varid (ncidbulk,vname(1,indxSUSTR)(1:lvar),susid)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidbulk, susid, i)
          if (ierr. eq. nf_noerr) then
            if (i.gt.1) then
              lsusgrd=1
            else
              lsusgrd=0
            endif
          endif
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxSUSTR)(1:lvar),bulkname(1:lstr)
          goto 99                                         !--> ERROR
        endif

        lvar=lenstr(vname(1,indxSVSTR))
        ierr=nf_inq_varid (ncidbulk,vname(1,indxSVSTR)(1:lvar),svsid)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidbulk, svsid, i)
          if (ierr. eq. nf_noerr) then
            if (i.gt.1) then
              lsvsgrd=1
            else
              lsvsgrd=0
            endif
          endif
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxSVSTR)(1:lvar),bulkname(1:lstr)
          goto 99                                         !--> ERROR
        endif
#  endif /* BULK_SM_UPDATE */
# endif /* BULK_SMFLUX */
!
! Determine whether there is cycling to reuse the input data and
! find cycling period "bulk_cycle", set initial cycling index
! "wspd_ncycle" and record index "wspd_rec".
! Set initial value for time index "itbulk" and both time record
! bounds to large negative artificial values, so that it will
! trigger the logic in reading part below.
! Also set scale factor to convert input dQdSST from Watts/m2/Celsius
! to meter/second.
!
        call set_cycle (ncidbulk,   bulk_tid,    ntbulk,
     &                  bulk_cycle, bulk_ncycle, bulk_rec)
        if (may_day_flag.ne.0) return    !-->  EXIT
        itbulk=2
        bulk_time(1)=-1.E+20
        bulk_time(2)=-1.E+20
        srf_scale=1./(rho0*Cp)
# ifdef BULK_EP
        stf_scale(isalt)=0.01/86400.
# endif
# if (defined BULK_SMFLUX &&  !defined BULK_SM_UPDATE)
        sms_scale=1./rho0
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
  1   i=3-itbulk
       cff=time+0.5*dt
        if (bulk_time(i).le.cff .and. cff.lt.bulk_time(itbulk)) 
     &            return
        ierr=advance_cycle (bulk_cycle,ntbulk,bulk_ncycle,bulk_rec)
        if (ierr .ne. 0) goto 7                           !--> ERROR

        ierr=nf_get_var1_FTYPE (ncidbulk, bulk_tid, bulk_rec, cff)
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'bulk_time', bulk_rec
          goto 99                                         !--> ERROR
        endif
        bulk_time(i)=cff*day2sec+bulk_cycle*bulk_ncycle
        if (bulk_time(itbulk).eq.-1.E+20) 
     &          bulk_time(itbulk)=bulk_time(i)
!
! wspd
!
        if (lwspdgrd.eq.1) then
          ierr=nf_fread (wspdg(START_2D_ARRAY,i), ncidbulk, wspd_id,
     &                                           bulk_rec, r2dvar)
        else
          ierr=nf_get_var1_FTYPE (ncidbulk, wspd_id, bulk_rec, wspdp(i))
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'WSPD', bulk_rec
          goto 99                                         !--> ERROR
        endif
!
! tair
!
        if (ltairgrd.eq.1) then
          ierr=nf_fread (tairg(START_2D_ARRAY,i), ncidbulk, tair_id,
     &                                            bulk_rec,  r2dvar)
        else
          ierr=nf_get_var1_FTYPE (ncidbulk,tair_id,bulk_rec,tairp(i))
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'TAIR', bulk_rec
          goto 99                                         !--> ERROR
        endif
!
! rhum
!
        if (lrhumgrd.eq.1) then
          ierr=nf_fread (rhumg(START_2D_ARRAY,i), ncidbulk, rhum_id,
     &                                            bulk_rec,  r2dvar)
        else
          ierr=nf_get_var1_FTYPE (ncidbulk,rhum_id,bulk_rec,rhump(i))
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'RHUM', bulk_rec
          goto 99                                         !--> ERROR
        endif
!
! radlw
!
        if (lradlwgrd.eq.1) then
          ierr=nf_fread (radlwg(START_2D_ARRAY,i), ncidbulk, radlw_id,
     &                                            bulk_rec,  r2dvar)
        else
          ierr=nf_get_var1_FTYPE (ncidbulk,radlw_id,bulk_rec,radlwp(i))
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'RADLW', bulk_rec
          goto 99                                         !--> ERROR
        endif
!
! radsw
!
        if (lradswgrd.eq.1) then
          ierr=nf_fread (radswg(START_2D_ARRAY,i), ncidbulk, radsw_id,
     &                                            bulk_rec,  r2dvar)
        else
          ierr=nf_get_var1_FTYPE (ncidbulk,radsw_id,bulk_rec,radswp(i))
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'RADSW', bulk_rec
          goto 99                                         !--> ERROR
        endif
!
! radswbio
!
# ifdef DIURNAL_INPUT_SRFLX
        if (lradswbiogrd.eq.1) then
          ierr=nf_fread (radswbiog(START_2D_ARRAY,i), ncidbulk,
     &                        radswbio_id, bulk_rec,  r2dvar)
        else
          ierr=nf_get_var1_FTYPE (ncidbulk,radswbio_id,
     &                        bulk_rec, radswbiop(i))
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'RADSWBIO', bulk_rec
          goto 99                                         !--> ERROR
        endif
# endif /* DIURNAL_INPUT_SRFLX */
!
! prate
!
# ifdef BULK_EP
        if (lprategrd.eq.1) then
          ierr=nf_fread (prateg(START_2D_ARRAY,i), ncidbulk, prate_id,
     &                                            bulk_rec,  r2dvar)
        else
          ierr=nf_get_var1_FTYPE (ncidbulk,prate_id,bulk_rec,pratep(i))
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'PRATE', bulk_rec
          goto 99                                         !--> ERROR
        endif
# endif
# ifdef BULK_SMFLUX
#  ifdef BULK_SM_UPDATE
!
! uwnd
!
        if (luwndgrd.eq.1) then
          ierr=nf_fread(uwndg(START_2D_ARRAY,i), ncidbulk, uwnd_id,
     &                                              bulk_rec, u2dvar)
        else
          ierr=nf_get_var1_FTYPE(ncidbulk,uwnd_id,bulk_rec,uwndp(i))
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'uwnd', bulk_rec
          goto 99                                         !--> ERROR
        endif
!
! vwnd
!
        if (lvwndgrd.eq.1) then
          ierr=nf_fread(vwndg(START_2D_ARRAY,i), ncidbulk, vwnd_id,
     &                                              bulk_rec, v2dvar)
        else
          ierr=nf_get_var1_FTYPE(ncidbulk,vwnd_id,bulk_rec,vwndp(i))
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'vwnd', bulk_rec
          goto 99                                         !--> ERROR
        endif
# else
!
! sustr
!
        if (lsusgrd.eq.1) then
          ierr=nf_fread(sustrg(START_2D_ARRAY,i), ncidbulk, susid,
     &                                              bulk_rec, u2dvar)
        else
          ierr=nf_get_var1_FTYPE(ncidbulk,susid,bulk_rec,sustrp(i))
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'sustr', bulk_rec
          goto 99                                         !--> ERROR
        endif
!
! svstr
!
        if (lsvsgrd.eq.1) then
          ierr=nf_fread(svstrg(START_2D_ARRAY,i), ncidbulk, svsid,
     &                                              bulk_rec, v2dvar)
        else
          ierr=nf_get_var1_FTYPE(ncidbulk,svsid,bulk_rec,svstrp(i))
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'svstr', bulk_rec
          goto 99                                         !--> ERROR
        endif
#  endif /* BULK_SM_UPDATE */
# endif /* BULK_SMFLUX */
       itbulk=i
       MPI_master_only write(stdout,'(6x,A,1x,A,1x,g12.4,1x,I4)') 
     &              'GET_BULK   --',
     &              'Read fields for bulk formula   for time =', cff 
# ifdef MPI
     &                                                      , mynode
# endif
      if (ntbulk.gt.1) goto 1
      if (ntbulk.eq.1) return
!
! Sort out error messages: The following portion of the code is
!===== === ===== ========= not accessed unless something goes wrong.
!
  3   format(/,' GET_BULK - ERROR: unable to find forcing variable',
     &                   ': ',a,/,11x,'in forcing NetCDF file: ',a)
  4   write(stdout,5) bulkname(1:lstr)
  5   format(/,' GET_BULK - ERROR: unable to open forcing NetCDF ',
     &                                                  'file: ',a)
      goto 99
  6   format(/,' GET_BULK - ERROR while reading variable: ',a,2x,
     &                                      ' at TIME index = ',i4)
  7   write(stdout,8) bulk_rec, ntbulk, bulkname(1:lstr), tdays,
     &                                     bulk_time(itbulk)*sec2day
  8   format(/,' GET_BULK - ERROR: requested time record ',I4,
     &       1x,'exeeds the last available',  /,  11x,'record ',I4,
     &       1x,'in forcing NetCDF file: ', a, /,   11x,'TDAYS = ',
     &               g12.4,2x,'last available BULK_TIME = ',g12.4)
  99  may_day_flag=2
      return
      end


      subroutine set_bulk_tile (Istr,Iend,Jstr,Jend)
!
! Set-up bulk data for current tile.
!
!!!! gc # define BULK_DATA
!!!! gc # ifdef BULK_WSTR
!!!! gc #  define SMFLUX_DATA
!!!! gc # endif
      implicit none
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "grid.h"
      integer Istr,Iend,Jstr,Jend, i,j, it1,it2
      real cff,cff1,cff2, cff3,cff4
# ifdef BULK_EP
      real cff5,cff6
# endif
# ifndef BULK_SM_UPDATE
      real cff7,cff8
# endif
      real val1,val2,val3,val4,val5,val6,val7,val8
# ifdef DIURNAL_INPUT_SRFLX
      real val55
# endif /* DIURNAL_INPUT_SRFLX */
!
# include "compute_extended_bounds.h"
!
      it1=3-itbulk
      it2=itbulk
      cff=time+0.5*dt
      cff1=bulk_time(it2)-cff
      cff2=cff-bulk_time(it1)
!
!  Load time invariant 
!
      if (bulk_cycle.lt.0.) then
        if (FIRST_TIME_STEP) then
          if (ltairgrd.eq.1) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                wspd(i,j)=wspdg(i,j,itbulk)
                tair(i,j)=tairg(i,j,itbulk)
                rhum(i,j)=rhumg(i,j,itbulk)
                radlw(i,j)=srf_scale*radlwg(i,j,itbulk)
                radsw(i,j)=srf_scale*radswg(i,j,itbulk)
                srflx(i,j)=radsw(i,j)
# ifdef DIURNAL_INPUT_SRFLX
                radswbio(i,j)=srf_scale*radswbiog(i,j,itbulk)
                srflxbio(i,j)=radswbio(i,j)
# endif /* DIURNAL_INPUT_SRFLX */
# ifdef BULK_EP
                prate(i,j)=stf_scale(isalt)*prateg(i,j,itbulk)
# endif
# ifdef BULK_SMFLUX
#  ifdef BULK_SM_UPDATE
                uwnd(i,j)=uwndg(i,j,itbulk)
                vwnd(i,j)=vwndg(i,j,itbulk)
#  else
                sustr(i,j)=sms_scale*sustrg(i,j,itbulk)
                svstr(i,j)=sms_scale*svstrg(i,j,itbulk)
#  endif /* BULK_SM_UPDATE */
# endif /* BULK_SMFLUX */
              enddo
            enddo
          else
            val1=wspdp(itbulk)
            val2=tairp(itbulk)
            val3=rhump(itbulk)
            val4=srf_scale*radlwp(itbulk)
            val5=srf_scale*radswp(itbulk)
# ifdef DIURNAL_INPUT_SRFLX
            val55=srf_scale*radswbiop(itbulk)
# endif /* DIURNAL_INPUT_SRFLX */
# ifdef BULK_EP
            val6=stf_scale(isalt)*pratep(itbulk)
# endif
# ifdef BULK_SMFLUX
#  ifdef BULK_SM_UPDATE
            val7=uwndp(itbulk)
            val8=vwndp(itbulk)
#  else
            val7=sms_scale*sustrp(itbulk)
            val8=sms_scale*svstrp(itbulk)
#  endif /* BULK_SM_UPDATE */
# endif /* BULK_SMFLUX */
            do j=JstrR,JendR
              do i=IstrR,IendR
                wspd(i,j)=val1
                tair(i,j)=val2
                rhum(i,j)=val3
                radlw(i,j)=val4
                radsw(i,j)=val5
                srflx(i,j)=val5
# ifdef DIURNAL_INPUT_SRFLX
                radswbio(i,j)=val55
                srflxbio(i,j)=val55
# endif /* DIURNAL_INPUT_SRFLX */
# ifdef BULK_EP
                prate(i,j)=val6
# endif
# ifdef BULK_SMFLUX
#  ifdef BULK_SM_UPDATE
                uwnd(i,j)=val7
                vwnd(i,j)=val8
#  else
                sustr(i,j)=val7
                svstr(i,j)=val8
#  endif /* BULK_SM_UPDATE */
# endif /* BULK_SMFLUX */
              enddo
            enddo
          endif
        endif
!
!  Time-interpolate SST and dQdSST from grided or point data.
!  Check that for the next time step [when time=time+dt] time+dt 
!  is still between wspd_time(it1) and wspd_time(it2); and if not,
!  set synchro_flag top signal that the new forcing data should be
!  read from the netCDF input file (master thread only).
!
      elseif (cff1.ge.0. .and. cff2.ge.0.) then
        if (ZEROTH_TILE .and. cff1.lt.dt) synchro_flag=.TRUE.
                                    !note cff order maters
        cff=srf_scale/(cff1+cff2)
        cff3=cff1*cff
        cff4=cff2*cff
# ifdef BULK_EP
        cff=stf_scale(isalt)/(cff1+cff2)
        cff5=cff1*cff
        cff6=cff2*cff
# endif
# ifdef BULK_SMFLUX
#  ifndef BULK_SM_UPDATE 
        cff=sms_scale/(cff1+cff2)
        cff7=cff1*cff
        cff8=cff2*cff
#  endif 
# endif 
        cff=1./(cff1+cff2)
        cff1=cff1*cff
        cff2=cff2*cff

        if (ltairgrd.eq.1) then
          do j=JstrR,JendR
            do i=IstrR,IendR
              wspd(i,j)=cff1*wspdg(i,j,it1)+cff2*wspdg(i,j,it2)
              tair(i,j)=cff1*tairg(i,j,it1)+cff2*tairg(i,j,it2)
              rhum(i,j)=cff1*rhumg(i,j,it1)+cff2*rhumg(i,j,it2)
              radlw(i,j)=cff3*radlwg(i,j,it1)+cff4*radlwg(i,j,it2)
              radsw(i,j)=cff3*radswg(i,j,it1)+cff4*radswg(i,j,it2)
              srflx(i,j)=radsw(i,j)
# ifdef DIURNAL_INPUT_SRFLX
              radswbio(i,j)=cff3*radswbiog(i,j,it1)
     &                     +cff4*radswbiog(i,j,it2)
              srflxbio(i,j)=radswbio(i,j)
# endif /* DIURNAL_INPUT_SRFLX */
# ifdef BULK_EP
              prate(i,j)=cff5*prateg(i,j,it1)+cff6*prateg(i,j,it2)
# endif
# ifdef BULK_SMFLUX
#  ifdef BULK_SM_UPDATE
              uwnd(i,j)=cff1*uwndg(i,j,it1)+cff2*uwndg(i,j,it2)
              vwnd(i,j)=cff1*vwndg(i,j,it1)+cff2*vwndg(i,j,it2)
#  else 
              sustr(i,j)=cff7*sustrg(i,j,it1)+cff8*sustrg(i,j,it2)
              svstr(i,j)=cff7*svstrg(i,j,it1)+cff8*svstrg(i,j,it2)
#  endif /* BULK_SM_UPDATE */
# endif
            enddo
          enddo
        else
          val1=cff1*wspdp(it1)+cff2*wspdp(it2)
          val2=cff1*tairp(it1)+cff2*tairp(it2)
          val3=cff1*rhump(it1)+cff2*rhump(it2)
          val4=cff3*radlwp(it1)+cff4*radlwp(it2)
          val5=cff3*radswp(it1)+cff4*radswp(it2)
# ifdef DIURNAL_INPUT_SRFLX
          val55=cff3*radswbiop(it1)+cff4*radswbiop(it2)
# endif /* DIURNAL_INPUT_SRFLX */
# ifdef BULK_EP
          val6=cff5*pratep(it1)+cff6*pratep(it2)
# endif
# ifdef BULK_SMFLUX
#  ifdef BULK_SM_UPDATE
          val7=cff1*uwndp(it1)+cff2*uwndp(it2)
          val8=cff1*vwndp(it1)+cff2*vwndp(it2)
#  else 
          val7=cff7*sustrp(it1)+cff8*sustrp(it2)
          val8=cff7*svstrp(it1)+cff8*svstrp(it2)
#  endif
# endif
          do j=JstrR,JendR
            do i=IstrR,IendR
              wspd(i,j)=val1
              tair(i,j)=val2
              rhum(i,j)=val3
              radlw(i,j)=val4
              radsw(i,j)=val5
              srflx(i,j)=val5
# ifdef DIURNAL_INPUT_SRFLX
              radswbio(i,j)=val55
              srflxbio(i,j)=val55
# endif /* DIURNAL_INPUT_SRFLX */
# ifdef BULK_EP
              prate(i,j)=val6
# endif
# ifdef BULK_SMFLUX
#  ifdef BULK_SM_UPDATE
              uwnd(i,j)=val7
              vwnd(i,j)=val8
#  else
              sustr(i,j)=val7
              svstr(i,j)=val8
#  endif /* BULK_SM_UPDATE */
# endif /* BULK_SMFLUX */
            enddo
          enddo
        endif
!
!  Unable to set-up SST and dQdSST:
!  Complain about the error and signal to quit.      
!
      else
        if (ZEROTH_TILE) then
          write(stdout,1) 'bulk_time',tdays,bulk_time(it2)*sec2day
  1       format(/,' SET_BULK - current model time exceeds ending',
     &           1x,'value for variable: ',a,/,11x,'TDAYS = ',g12.4,
     &           2x,'TEND = ',g12.4)
          may_day_flag=2
        endif
      endif
      return
      end
#else
      subroutine get_bulk_empty
      return
      end
#endif /* BULK_FLUX */
