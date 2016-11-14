! $Id: online_get_bulk.F 1458 2014-02-03 15:01:25Z gcambon $
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
!
!  This is the "online_get_bulk.F" script
!------------------------------------------------------------------------------
!  This file contains the subfunctions enabling the online extraction of the 
!  forcing from a NCEP/CFSR dataset. A spatial and time interpolation are applied 
!  to the extracted data in order to adapt these to the considered simulation 
!  domain as well as the associated MPI/OPENMP discretisation (if defined MPI/
!  OPENMP).
!------------------------------------------------------------------------------
#include "cppdefs.h"
#if defined BULK_FLUX && defined ONLINE 

!*******************************************************************************
      subroutine get_bulk_online
!------------------------------------------------------------------------------
!  This subfunction enables the online extraction of data from a forcing netcdf 
!  NCEP/CFSR dataset. It complete the extraction with a spatial interpolation and 
!  a recalculation of the forcing fields to generate the bulk forcing (tairg, 
!  rhumg, prateg, radlwg, radswg, wspdg, uwndg, vwndg) on the simulation domain.
!------------------------------------------------------------------------------
!  The main steps of this extraction are:
!  - Opening of the relevant NCEP/CFSR netcdf file used for the forcing of the 
!    considered variable.
!  - Extraction from this netcdf of the data corresponding with the simulation 
!    domain.
!  - Interpolation of this extracted dataset on the grid of the model (in serial 
!    as well as parallel).
!  - Transformation of the various interpolated dataset into roms bulk forcings:
!    tairg; rhumg; prateg; radlwg; radswg; wspdg; uwndg; vwndg.
!
!  In this subroutine the aformentioned steps are completed for every bulk forcing 
!  bulkfilename(X) and are repeated when necessary during the time evolution of 
!  the simulation. 
!
      implicit none
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "netcdf.inc"
# include "grid.h"
# include "online.h"
      integer blkvar_id, imin, imax, jmin, jmax, strt3(3), cnt3(3)
      integer blk_dimlonid, blk_lonid, blk_dimlatid, blk_latid
      integer iblk, ierr, lvar, lenstr, i, j, NX, NY, tile, subs, trd
      logical firstiter
      real bulkstart, bulkend
      real daycfsr, timestp, gap
      real(kind=4), dimension(:), allocatable :: lon0, lat0
      real(kind=8), dimension(:), allocatable :: lon, lat    
      real(kind=8), dimension(:,:), allocatable :: var
      character*16 dimname
!
! Time of the current model step
!
      timestp=time+0.5*dt
!
! Initialisation
!
      firstiter=.false.
!
!===== == === === ==== ========= 
! Loop on all the CFSR variables
!===== == === === ==== =========  
!
      blkvar_id = 0
  10  blkvar_id = blkvar_id+1
!
! Do not process #7 :  upward longwave is obsolete
!
        if (blkvar_id.eq.7) goto 10
#  ifdef ERA_ECMWF
!
! In the case of ERA the net short wave is used
! Net Short wave - 5 downward short wave not used
!
        if (blkvar_id.eq.4) goto 10
#  endif
!
! -> End of loop on blkvar_id : goto 10
!
        if (blkvar_id.gt.nblkvrs) return
!
! Open a new CFSR file if necessary
!
        if (newbulk(blkvar_id)) then
!
          newbulk(blkvar_id)=.false.
!
! Get the file name
!
          call get_bulknetcdf_names(blkvar_id)
!
! Open the file
!
          lvar=lenstr(bulkfilename(blkvar_id))
      print*, 'Open :',bulkfilename(blkvar_id)(1:lvar)
          ierr=nf_open(bulkfilename(blkvar_id)(1:lvar), nf_nowrite, 
     &                 ncidbulkO(blkvar_id)) 
          if (ierr. ne. nf_noerr) goto 4            !--> ERROR
!
! Extract the CFSR variable index
!
          lvar=lenstr(blk_vname(1, blkvar_id))
          ierr=nf_inq_varid(ncidbulkO(blkvar_id), 
     &                      blk_vname(1,blkvar_id)(1:lvar),
     &                      bulk_varid(blkvar_id))
!
! Read Time dimension
! 
          ierr=nf_inq_varid(ncidbulkO(blkvar_id),'time', 
     &                      bulk_tidO(blkvar_id))
          if (ierr .ne. nf_noerr) goto 99           !--> ERROR
!
! Extract the number of records
!
          ierr=nf_inq_dim(ncidbulkO(blkvar_id), bulk_tidO(blkvar_id),  
     &                    dimname, ntbulkO(blkvar_id))
!
! Read Longitude dimension
! 
          ierr=nf_inq_dimid(ncidbulkO(blkvar_id),'lon',blk_dimlonid)
          if (ierr. ne. nf_noerr) goto 99           !--> ERROR
          ierr=nf_inq_dimlen(ncidbulkO(blkvar_id),blk_dimlonid,
     &                       NX0(blkvar_id))
!
! Read Latitude dimension
! 
          ierr=nf_inq_dimid(ncidbulkO(blkvar_id),'lat',blk_dimlatid)
          if (ierr. ne. nf_noerr) goto 99           !--> ERROR
          ierr=nf_inq_dimlen(ncidbulkO(blkvar_id),blk_dimlatid,
     &                       NY0(blkvar_id))
!
!======= === =============== ======= == === ========== === =============
! Define the caracteristics, limits, of the extraction and interpolation
!======= === =============== ======= == === ========== === =============
!
! First time record
!
          ierr=nf_get_var1_FTYPE(ncidbulkO(blkvar_id), bulk_tidO(blkvar_id), 
     &                           1, bulkstart)
!
! Last time record
          ierr=nf_get_var1_FTYPE(ncidbulkO(blkvar_id), bulk_tidO(blkvar_id), 
     &                           ntbulkO(blkvar_id),bulkend)
!
! Length of time vector in seconds
!
          bulk_cycleO(blkvar_id)=day2sec*(bulkend-bulkstart)
!
! Check if this is the first interation
!
          firstiter=(itbulkO(blkvar_id).eq.0)
!
! bulk forcing parameters
!
          srf_scale=1./(rho0*Cp)
# ifdef BULK_EP
          stf_scale(isalt)=0.01/86400.
# endif
# if (defined BULK_SMFLUX &&  !defined BULK_SM_UPDATE)
          sms_scale=1./rho0
# endif
!
! Leap parameter initialisation for time interpolation
!
          itbulkO(blkvar_id)=2

        endif
!
!======== ==== ==== === ======= ======= === ============== 
! Reading data from the forcing dataset and interpolation:  
!======== ==== ==== === ======= ======= === ============== 
! Leap 1->2->1->2->... to keep two forcing fields for time interpolation 
!
  1     iblk=3-itbulkO(blkvar_id)
!
!===== = ==== ==== === === ======== ===== == =========
! Open a CFSR file for the previous month if necessary
!===== = ==== ==== === === ======== ===== == =========
!
! (i.e. when the current CFSR time limits  
!  are after the current model time) 
!
          if ((firstiter).and.(timestp.le.(bulkstart*day2sec))) then
!
! Open a new CFSR file
!            
            newbulk(blkvar_id)=.true.
            bulk_recO(blkvar_id)=0
!
! Caracteristics (Year, month), of the CFSR file for the previous month
!
            bulkmonthnum(blkvar_id)=bulkmonthnum(blkvar_id)-1
            if(bulkmonthnum(blkvar_id).le.0) then
              bulkyearnum(blkvar_id)=bulkyearnum(blkvar_id)-1
              bulkmonthnum(blkvar_id)=12
            endif
!
! Message for the opening of the CFSR file for the previous month
!
            lvar=lenstr(blk_vname(1,blkvar_id))
            MPI_master_only write(stdout,
     &      '(6x,A,1x,A,1x,I4,1x,A,1x,I2,1x,A,1x,A)') 'ONLINE_BULK --',  
     &      'for temporal consistency open CFSR file of year',   
     &      bulkyearnum(blkvar_id),
     &      'month',  bulkmonthnum(blkvar_id), 'for',   
     &      blk_vname(1,blkvar_id)(1:lvar)

            blkvar_id=blkvar_id-1
            firstiter=.false.   
            goto 10

          endif
!
!===== ====== ========== == == ==== == =========== 
! Pass NETCDF extraction if no need of extraction:
!===== ====== ========== == == ==== == =========== 
! OR GO STRAIGHT to temporal interpolation if the current step is between 
! already extracted time steps
!
          if ((bulk_timeO(iblk,blkvar_id).le.timestp).and.
     &       (timestp.lt.bulk_timeO(itbulkO(blkvar_id),blkvar_id))) goto 10
!
!=========== == ====== ========
! Extraction of NETCDF records:
!=========== == ====== ========
! OR LEAP if the extration of a new time step is necessary
! Load new netcdf if new forcing
!
          if (newbulk(blkvar_id)) then
            blkvar_id=blkvar_id-1
            goto 10
          endif                      
!
! Record index to extract in the forcing netcdf file
!
          bulk_recO(blkvar_id)=bulk_recO(blkvar_id)+1
!
!=========== == =====
! Extraction of Time:
!=========== == =====
! Time (days) associated with the record index
!
          ierr=nf_get_var1_FTYPE(ncidbulkO(blkvar_id),bulk_tidO(blkvar_id),
     &                           bulk_recO(blkvar_id),daycfsr)
          if (ierr .ne. nf_noerr) goto 99
!
! Computation of the time in seconds for time interpolation
!
          bulk_timeO(iblk,blkvar_id)=daycfsr*day2sec
!
! Initialize bulk_timeO(itbulkO(blkvar_id),blkvar_id)
!
          if (bulk_timeO(itbulkO(blkvar_id),blkvar_id).eq.(-1.E+20))
     &      bulk_timeO(itbulkO(blkvar_id),blkvar_id)=
     &      bulk_timeO(iblk,blkvar_id)
!
! ---------------------------------------------------------------
! Evaluation of the caracteristics of the next record to extract:
! ---------------------------------------------------------------
! When at the last record of a netcdf forcing, determination of the
! caracteristics (Year, month), of the next file to open.
!
          if(bulk_recO(blkvar_id).eq.ntbulkO(blkvar_id)) then
!
! Open a new CFSR file
!
            newbulk(blkvar_id)=.true.
!
! Caracteristics of the record
!
            bulkmonthnum(blkvar_id)=bulkmonthnum(blkvar_id)+1
            if(bulkmonthnum(blkvar_id).gt.12) then
              bulkyearnum(blkvar_id)=bulkyearnum(blkvar_id)+1
              bulkmonthnum(blkvar_id)=1
            endif
          endif  
!
! Jump from record to record without completing the 2D interpolation 
! in order to spare computation time when the initial field is
! researched into a netcdf file
!
          gap=day2sec/recordsperday
          if ((timestp-bulk_timeO(iblk,blkvar_id).gt.2*gap).and.
     &        (timestp-bulk_timeO(itbulkO(blkvar_id),
     &                            blkvar_id).gt.2*gap)) then     
!
! Leap incrementation and go to to the next record without extraction
!
            itbulkO(blkvar_id)=iblk 
            goto 1
          endif
!
! ========== == === == ======== === === ======== == ===== ========
! Extraction of the 2D forcings for the variable of index blkvar_id:
! ========== == === == ======== === === ======== == ===== ========
!
! Get CFSR longitude 
!
          allocate(lon0(1:NX0(blkvar_id)))
          ierr=nf_inq_varid (ncidbulkO(blkvar_id), 'lon', blk_lonid)
          if (ierr. ne. nf_noerr) goto 99
          ierr=nf_get_var_real(ncidbulkO(blkvar_id),blk_lonid,lon0)
          if (ierr. ne. nf_noerr) goto 99
!
! Get CFSR latitude 
!
          allocate(lat0(1:NY0(blkvar_id)))
          ierr=nf_inq_varid (ncidbulkO(blkvar_id), 'lat', blk_latid)
          if (ierr. ne. nf_noerr) goto 99
          ierr=nf_get_var_real(ncidbulkO(blkvar_id),blk_latid,lat0)
          if (ierr. ne. nf_noerr) goto 99
!
! Get a subgrid agreing with the limits of the model domain
!
! lonmin<lon0<lonmax
!
          imin=99999
          imax=-99999
          do i=1,NX0(blkvar_id)
            if (lon0(i).lt.lonmin) imin=i
          enddo
          do i=NX0(blkvar_id),1,-1
            if (lon0(i).gt.lonmax) imax=i
          enddo
          if (imin.le.1) goto 12                    !--> ERROR
          if (imax.ge.NX0(blkvar_id)) goto 14       !--> ERROR
!
! latmin<lat0<latmax
!
          jmin=99999
          jmax=-99999
          do j=1,NY0(blkvar_id)
            if (lat0(j).lt.latmin) jmin=j
          enddo
          do j=NY0(blkvar_id),1,-1
            if (lat0(j).gt.latmax) jmax=j
          enddo
          if (jmin.ge.NY0(blkvar_id)) goto 16       !--> ERROR
          if (jmax.le.1) goto 18                    !--> ERROR
!
! Extend the subgrid limit of 1 grid point
!
          imin=imin-1
          if (imin.lt.1) imin=imin+1
          imax=imax+1
          if (imax.gt.NX0(blkvar_id)) imax=imax-1
          jmin=jmin-1
          if (jmin.lt.1) jmin=jmin+1
          jmax=jmax+1
          if (jmax.gt.NY0(blkvar_id)) jmax=jmax+1
!
! New CFSR subgrid caracteristics
! Size: NX x NY
!
          NX=1+imax-imin
          NY=1+jmax-jmin
!
! Allocate lon and lat for CFSR data
!
          allocate(lon(1:NX))
          allocate(lat(1:NY))
          lon=lon0(imin:imax)
          lat=lat0(jmin:jmax)
!
! ---------------------------------------------------------------------
! Extract CFSR data for the subgrid
! ---------------------------------------------------------------------
! Limits specification 
! start index (imin, jmin, record)
!
          strt3(1)=imin
          strt3(2)=jmin
          strt3(3)=bulk_recO(blkvar_id)
!
! size index (Nx in lon, NY in lat, 1 record)
!
          cnt3(1)=NX
          cnt3(2)=NY
          cnt3(3)=1
!
! Allocate table for the CFSR data
!
          allocate(var(1:NX,1:NY))
!
! Extract CFSR data
!
          ierr=nf_get_vara_FTYPE(ncidbulkO(blkvar_id),
     &                          bulk_varid(blkvar_id),strt3,cnt3,var)
          if (ierr. ne. nf_noerr) goto 99           !--> ERROR
!
!============== == === ====== ====== == === ===== =====
! Interpolation of the NETCDF record on the local grid:
!============== == === ====== ====== == === ===== =====
! Interpolation on the simulation grid of the forcings and computation 
! of the roms bulk forcings tairg; rhumg; prateg; radlwg; radswg; 
! wspdg; uwndg; vwndg.
!
!
!-----C$OMP PARALLEL DO PRIVATE(tile)
!          do tile=0,NSUB_X*NSUB_E-1    ! Parallel interpolation
!            call interpolate_bulk_online (NX,NY,lon,lat,var,blkvar_id,i,tile)
!          enddo
!
! For the moment we don't know how this can be parallelized 
! inside the C$OMP MASTER loop of step.F (2012/05/10)
!
!-----
!
!
          call interpolate_bulk_online(NX,NY,lon,lat,var,
     &                                 blkvar_id,iblk)
!
!
!-----
!
          if(bulk_recO(blkvar_id).eq.ntbulkO(blkvar_id)) then
             bulk_recO(blkvar_id)=0
!
! Error if no more available datasets
!
          if(((bulkmonthnum(blkvar_id).gt.monthend).and.
     &          (bulkyearnum(blkvar_id).ge.yearend)).or.
     &         ((bulkmonthnum(blkvar_id).eq.1).and.
     &          (bulkyearnum(blkvar_id).gt.yearend))) then
                 goto 9                !--> ERROR
            endif
          endif  
!
! Free memory
!
          deallocate(lon0)
          deallocate(lat0)
          deallocate(lon)
          deallocate(lat)
          deallocate(var)
!
! === ==== ======= === ==== == ======= ======== 
! GET BULK message and loop to another blkvar_id:
! === ==== ======= === ==== == ======= ======== 
!
          if (blkvar_id.eq.nblkvrs) then
            MPI_master_only write(stdout,'(6x,A,1x,A,1x,g12.4,1x,I4)') 
     &              'ONLINE_BULK --',
     &              'Read CFSR for time =', daycfsr  
          endif
!
! Leap incrementation
!
          itbulkO(blkvar_id)=iblk  
!
! Back to the beginning of the extraction loop
!
        if (ntbulkO(blkvar_id).gt.1) goto 1

!
! Sort out error messages: The following portion of the code is
!===== === ===== ========= not accessed unless something goes wrong.
!
  3   format(/,' ONLINE_GET_BULK - ERROR: unable to find forcing variable',
     &                   ': ',a,/,11x,'in forcing NetCDF file: ',a)
  4   lvar=lenstr(bulkfilename(blkvar_id))
      write(stdout,5) bulkfilename(blkvar_id)(1:lvar)
  5   format(/,' ONLINE_GET_BULK - ',
     &         'ERROR: unable to open forcing NetCDF ',
     &                                                  'file: ',a)
      goto 99
  6   format(/,' ONLINE_GET_BULK - ',
     &         'ERROR while reading variable: ',a,2x,
     &                                      ' at TIME index = ',i4)
  7   lvar=lenstr(bulkfilename(blkvar_id))
      write(stdout,8) bulk_recO(blkvar_id), ntbulkO(blkvar_id), 
     &                bulkfilename(blkvar_id)(1:lvar), tdays,
     &                bulk_timeO(itbulkO,blkvar_id)*sec2day
  8   format(/,' ONLINE_GET_BULK - ',
     &         'ERROR: requested time record ',I4,
     &       1x,'exeeds the last available',  /,  11x,'record ',I4,
     &       1x,'in forcing NetCDF file: ', a, /,   11x,'TDAYS = ',
     &               g12.4,2x,'last available bulk_timeO = ',g12.4)
      goto 99
  9   write(stdout,11) bulkyearnum(blkvar_id), bulkmonthnum(blkvar_id),
     &       blk_vname(1,blkvar_id)
  11  format(/,' ONLINE_GET_BULK - ',
     &         'ERROR: The dataset for the year ',I4,          
     &       1x,'month ',I2,1x,'is missing: ',a)
      goto 99
  12  lvar=lenstr(bulkfilename(blkvar_id))
      write(stdout,13) bulkfilename(blkvar_id)(1:lvar)
  13  format(/,' ONLINE_GET_BULK - ',
     &         'ERROR: Min longitude too small compared to forcing', 
     &                                                                a)
      goto 99
  14  lvar=lenstr(bulkfilename(blkvar_id))
      write(stdout,15) bulkfilename(blkvar_id)(1:lvar)
  15  format(/,' ONLINE_GET_BULK - ',
     &         'ERROR: Max longitude too large compared to forcing', 
     &                                                                a)
      goto 99
  16  lvar=lenstr(bulkfilename(blkvar_id))
      write(stdout,17) bulkfilename(blkvar_id)(1:lvar)
  17  format(/,' ONLINE_GET_BULK - ',
     &         'ERROR: Min latitude too small compared to forcing', 
     &                                                                a)
      goto 99
  18  lvar=lenstr(bulkfilename(blkvar_id))
      write(stdout,19) bulkfilename(blkvar_id)(1:lvar)
  19  format(/,' ONLINE_GET_BULK - ',
     &         'ERROR: Max latitude too large compared to forcing', 
     &                                                                a)
  99  may_day_flag=2
      return
      end

#endif /* BULK_FLUX && ONLINE */
