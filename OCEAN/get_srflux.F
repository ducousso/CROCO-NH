! $Id: get_srflux.F 1570 2014-07-01 10:20:21Z gcambon $
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
#if defined SOLVE3D && !defined ANA_SRFLUX

                              ! Read point or grided shortwave
      subroutine get_srflux   ! radiation flux at the appropriate
                              ! time from forcing NetCDF file.
# define SRFLUX_DATA
      implicit none
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "ncscrum.h"
      real cff
      integer i,ierr, lstr,lvar, lenstr, nf_fread, advance_cycle
#include "netcdf.inc"
!
! Initialization: Inquire about the contents of forcing NetCDF file:
!================  variables and dimensions. Check for consistency.
!
      if (may_day_flag.ne.0) return      !-->  EXIT
      if (itsrf.eq.0 .or. iic.eq.0) then
        lstr=lenstr(frcname)
        if (may_day_flag.ne.0) return    !-->  EXIT
!
! If not opened yet, open forcing NetCDF file for reading.
! Find and save IDs for relevant variables, determine whether
! surface radiation flux is a field or scalar value. 
!
        if (ncidfrc.eq.-1) then
          ierr=nf_open(frcname(1:lstr), nf_nowrite, ncidfrc)
          if (ierr .ne. nf_noerr) goto 4                !--> ERROR 
        endif

        ierr=nf_inq_varid (ncidfrc, 'srf_time', srf_tid)
        if (ierr .ne. nf_noerr) then
          write(stdout,3) 'srf_time', frcname(1:lstr)
          goto 99                                         !--> ERROR
        endif
!
! srflx
!
        lvar=lenstr(vname(1,indxShflx_rsw))
        ierr=nf_inq_varid (ncidfrc,vname(1,indxShflx_rsw)
     &                                               (1:lvar),srf_id)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidfrc, srf_id, i)
          if (ierr. eq. nf_noerr) then
            if (i.gt.1) then
              lsrfgrd=1
            else
              lsrfgrd=0
            endif
          endif
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxShflx_rsw)(1:lvar),frcname(1:lstr)
          goto 99                                         !--> ERROR
        endif
# ifdef DIURNAL_INPUT_SRFLX
!
! srflxbio : for PISCES daily averaged short-wave (solar) radiation )
!
        lvar=lenstr(vname(1,indxShflx_rswbio))
        ierr=nf_inq_varid (ncidfrc,vname(1,indxShflx_rswbio)(1:lvar)
     &                                              ,srfbio_id)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidfrc, srfbio_id, i)
          if (ierr. eq. nf_noerr) then
            if (i.gt.1) then
              lsrfbiogrd=1
            else
              lsrfbiogrd=0
            endif
          endif
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxShflx_rswbio)(1:lvar),
     &                                               frcname(1:lstr)
          goto 99                                         !--> ERROR
        endif
# endif /* DIURNAL_INPUT_SRFLX */
!
! Determine whether there is cycling to reuse the input data
! and find cycling period "srf_cycle", set initial cycling
! index "srf_ncycle" and record index "srf_rec".
! Set initial value for time index "itsrf" and both time record
! bounds to large negative artificial values, so that it will
! trigger the logic in reading part below.
! Also set scale factor to convert input flux to model units:
! convert from Watts meter-2 to Celsius meter second-1.
!
        call set_cycle (ncidfrc,   srf_tid,    ntsrf,
     &                  srf_cycle, srf_ncycle, srf_rec)
        if (may_day_flag.ne.0) return    !-->  EXIT
        itsrf=2
        srf_time(1)=-1.E+20
        srf_time(2)=-1.E+20
        srf_scale=1./(rho0*Cp)
      endif                   !<-- itsrf.eq.0 .or. iic.eq.0

!
! Reading data from the forcing file:  Get out, if model time is
!======== ==== ==== === ======= =====  already within the interval
! set by the past and future data times. Otherwise flip the time
! index, increment record and cyclin indices and read a new portion
! of data. Repeat it until model time is between the two times from
! data.
!
  1   i=3-itsrf
       cff=time+0.5*dt
        if (srf_time(i).le.cff .and. cff.lt.srf_time(itsrf)) return
        ierr=advance_cycle (srf_cycle, ntsrf, srf_ncycle, srf_rec)
        if (ierr.ne.0) goto 7                           !--> ERROR

        ierr=nf_get_var1_FTYPE(ncidfrc, srf_tid, srf_rec, cff)
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'srf_time', srf_rec 
          goto 99                                         !--> ERROR
        endif
        srf_time(i)=cff*day2sec+srf_cycle*srf_ncycle
        if (srf_time(itsrf).eq.-1.E+20) srf_time(itsrf)=srf_time(i)
!
! srflx
!
        if (lsrfgrd.eq.1) then
          ierr=nf_fread (srflxg(START_2D_ARRAY,i), ncidfrc, srf_id,
     &                                             srf_rec, r2dvar)
        else
          ierr=nf_get_var1_FTYPE (ncidfrc,srf_id,srf_rec,srflxp(i))
        endif

        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'srflux', srf_rec
          goto 99                                         !--> ERROR
        endif 

# ifdef DIURNAL_INPUT_SRFLX
!
! srflxbio (=radswbio with bulk) for PISCES daily averaged short-wave (solar) radiation)
!     
        if (lsrfbiogrd.eq.1) then
          ierr=nf_fread (srflxbiog(START_2D_ARRAY,i), ncidfrc,
     &                        srfbio_id, srf_rec,  r2dvar)
        else
          ierr=nf_get_var1_FTYPE (ncidfrc,srfbio_id,
     &                            srf_rec ,srflxbiop(i))
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'srflux_bio', srf_rec
          goto 99                                         !--> ERROR
        endif
# endif /* DIURNAL_INPUT_SRFLX */

       itsrf=i
       MPI_master_only write(stdout,'(6x,A,1x,A,1x,g12.4,1x,I4)') 
     &           'GET_SRFLUX --',
     &           'Read solar shortwave radiation for time =', cff
#ifdef MPI
     &                                                      , mynode
#endif
      if (ntsrf.gt.1) goto 1
      if (ntsrf.eq.1) return 
!
! Sort out error messages: The following portion of the code is
!===== === ===== ========= not accessed unless something goes wrong.
!
  3   format(/,' GET_SRFLUX - unable to find forcing variable: ',a,
     &                          /,15x,'in forcing NetCDF file: ',a)
  4   write(stdout,5) frcname(1:lstr)
  5   format(/,' GET_SRFLUX - unable to open forcing NetCDF file:',
     &                                                        1x,a)
      goto 99

  6   format(/,' GET_SRFLUX - error while reading variable: ',a,2x,
     &                                      ' at TIME index = ',i4)
  7   write(stdout,8) srf_rec, ntsrf, frcname(1:lstr), tdays,
     &                                srf_time(itsrf)*sec2day
  8   format(/,' GET_SRFLUX - ERROR: requested time record ',I4,
     &       1x,'exeeds the last available', /,14x,'record ',I4,
     &       1x,'in forcing NetCDF file: ',a, /,14x,'TDAYS = ',
     &       g12.4,2x,'last available SRF_TIME = ',g12.4)

  99  may_day_flag=2
      return
      end

      subroutine set_srflux_tile (Istr,Iend,Jstr,Jend)
!
!--------------------------------------------------------------------
!  Set-up shortwave radiation flux for current tile.
!--------------------------------------------------------------------
!
# define SRFLUX_DATA
      implicit none
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "grid.h"
      integer Istr,Iend,Jstr,Jend, i,j, it1,it2
      real cff, cff1, cff2, srf
#ifdef DIURNAL_INPUT_SRFLX
      real srfbio
#endif
!
# include "compute_extended_bounds.h"
!
! Set coefficients for interpolation. Check that for the next time
! step [when time=time+dt] both weights will still be positive, and
! if not, set synchro_flag to signal that new data should be read
! from an appropriate netCDF input file (master thread only).
! After that either load time-invariant data, or interpolate in time
! or complain about error and signal to quit, if interpolation is
! needed, but not possible.
!
!
      it1=3-itsrf
      it2=itsrf
      cff=time+0.5*dt
      cff1=srf_time(it2)-cff
      cff2=cff-srf_time(it1)
      if (ZEROTH_TILE.and. cff1.lt.dt) synchro_flag=.TRUE.
!
! Load time-invariant shortwave radiation flux.
! Time interpolation is not performed in this case.
!
      if (srf_cycle.lt.0.) then
        if (FIRST_TIME_STEP) then
          if (lsrfgrd.eq.1) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                srflx(i,j)=srf_scale*srflxg(i,j,itsrf)
# ifdef DIURNAL_INPUT_SRFLX
                srflxbio(i,j)=srf_scale*srflxbiog(i,j,itsrf)
# endif
              enddo
            enddo
          else
            srf=srf_scale*srflxp(itsrf)
# ifdef DIURNAL_INPUT_SRFLX
            srflxbio=srf_scale*srflxbiop(itsrf)
# endif
            do j=JstrR,JendR
              do i=IstrR,IendR
                srflx(i,j)=srf
# ifdef DIURNAL_INPUT_SRFLX
                srflxbio(i,j)=srfbio
# endif
              enddo
            enddo
          endif
        endif
!
! Time-interpolate shortwave radiation flux from grided or point
! data. Check that for the next time step [when time=time+dt]
! time+dt is still between srf_time(it1) and srf_time(it2);
! and if not, set synchro_flag to signal that the new forcing data
! should be read from the netCDF input file (master thread only).
!
      elseif (cff1.ge.0. .and. cff2.ge.0.) then
        cff=srf_scale/(cff1+cff2)
        cff1=cff1*cff
        cff2=cff2*cff
        if (lsrfgrd.eq.1) then
          do j=JstrR,JendR
            do i=IstrR,IendR
              srflx(i,j)=cff1*srflxg(i,j,it1)+cff2*srflxg(i,j,it2)
# ifdef DIURNAL_INPUT_SRFLX
              srflxbio(i,j)=cff1*srflxbiog(i,j,it1)
     &                                    +cff2*srflxbiog(i,j,it2)
# endif
            enddo
          enddo
        else
          srf=cff1*srflxp(it1)+cff2*srflxp(it2)
# ifdef DIURNAL_INPUT_SRFLX
          srfbio=cff1*srflxbiop(it1)
     &                        +cff2*srflxbiop(it2)
# endif
            do j=JstrR,JendR
              do i=IstrR,IendR
              srflx(i,j)=srf
# ifdef DIURNAL_INPUT_SRFLX
              srflxbio(i,j)=srfbio
# endif
            enddo
          enddo
        endif
!
! Unable to set-up shortwave radiation flux:
! Complain about the error and signal to quit (ONE THREAD ONLY).
!
      else
        if (ZEROTH_TILE) then
          write(stdout,1) 'srf_time', tdays, srf_time(it2)*sec2day  
  1       format(/,' SET_SRFLUX_TILE - current model time exceeds',
     &             ' ending value for variable: ',a,/,14x,'TDAYS = ',
     &                                      g12.4,2x,'TEND = ',g12.4)
          may_day_flag=2
        endif
      endif

      return
      end
#else
      subroutine get_srflux_empty
      return
      end
#endif /* SOLVE3D && !ANA_SRFLUX */
!--------------------------------------------------------------------
#if defined SOLVE3D && defined ANA_DIURNAL_SW
      subroutine ana_diurnal_sw_tile (Istr,Iend,Jstr,Jend)
!
!--------------------------------------------------------------------
!
!  DIURNAL CYCLE - USED IN BOTH PHYSICAL AND ECOSYSTEM MODELS
!  Patrick Marchesiello - 99
!
!  Modulate average daily insolation to get diurnal cycle
!  by:  pi*( cos(h)*cos(d)*cos(phi)+   sin(d)sin(phi))/
!          (sin(h0)*cos(d)*cos(phi)+h0*sin(d)sin(phi))
!  h, d, phi are hour, declination, latitude angles
!  h0 is hour angle at sunset and sunrise
!
!--------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "grid.h"
      integer Istr,Iend,Jstr,Jend, i,j
      real cff,cff1,cff2, 
     &     hour,cos_h,phi,h0,dec,cos_dec,sin_dec,tan_dec
!
# include "compute_extended_bounds.h"
!
! Initialize fixed parameters
!
      if (FIRST_TIME_STEP) then
        do j=JstrR,JendR
          do i=IstrR,IendR
            phi=latr(i,j)*deg2rad
            cos_phi(i,j)=cos(phi)
            sin_phi(i,j)=sin(phi)
            tan_phi(i,j)=tan(phi)
          enddo
        enddo
      endif
!
! Compute local hour angle (radians)
!
      hour=2.*pi*(tdays+.5-int(tdays+.5))
!
! Compute declination angle
!
      dec=-0.406*cos(deg2rad*(tdays-
     &               int(tdays*day2year)*year2day))
      cos_dec=cos(dec)
      sin_dec=sin(dec)
      tan_dec=sin_dec/cos_dec
!
! Compute daily insolation coefficient
! Modulate srflx and update stflx
!  --> use Greenwich hour angle: 
!      cos_h=1 at solar noon in Greenwich
!
      do j=JstrR,JendR
        do i=IstrR,IendR
          cos_h=cos(hour-lonr(i,j)*deg2rad)
          h0=acos(-tan_phi(i,j)*tan_dec)
          cff1=cos_dec*cos_phi(i,j)
          cff2=sin_dec*sin_phi(i,j)
          cff=pi*(cos_h*cff1+cff2)/(sin(h0)*cff1+h0*cff2)
            
          stflx(i,j,itemp)=stflx(i,j,itemp)-srflx(i,j)
          srflx(i,j)=max(0.,cff*srflx(i,j))
          stflx(i,j,itemp)=stflx(i,j,itemp)+srflx(i,j)

# ifdef BULK_FLUX
!
!  Correct fluxes for diagnostics
!
          shflx_rsw(i,j)=srflx(i,j)
# endif

        enddo
      enddo

      return
      end
#else
      subroutine diurnal_srflux_empty
      return
      end
#endif /* SOLVE3D && ANA_DIURNAL_SW */
