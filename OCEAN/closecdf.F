! $Id: closecdf.F 1586 2014-07-30 14:57:11Z marchesiello $
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
                                !
      subroutine closecdf       ! Close output netCDF files. 
                                !
      implicit none
#include "param.h"
#include "scalars.h"
#include "ncscrum.h"
#ifdef FLOATS
# include "ncscrum_floats.h"
#endif
#ifdef STATIONS
# include "nc_sta.h"
#endif
#include "netcdf.inc"
      integer ierr 
      if (ncidrst.ne.-1) ierr=nf_close(ncidrst)
      if (ncidhis.ne.-1) ierr=nf_close(ncidhis)
#ifdef AVERAGES
      if (ncidavg.ne.-1) ierr=nf_close(ncidavg)
#endif
#ifdef FLOATS
      if (ncidflt.ne.-1) ierr=nf_close(ncidflt)
#endif
#if defined DIAGNOSTICS_TS
      if (nciddia.ne.-1) ierr=nf_close(nciddia)
# ifdef AVERAGES
      if (nciddia_avg.ne.-1) ierr=nf_close(nciddia_avg)
# endif
#endif
#if defined DIAGNOSTICS_UV
      if (nciddiaM.ne.-1) ierr=nf_close(nciddiaM)
# ifdef AVERAGES
      if (nciddiaM_avg.ne.-1) ierr=nf_close(nciddiaM_avg)
# endif
#endif
#if defined DIAGNOSTICS_BIO
      if (nciddiabio.ne.-1) ierr=nf_close(nciddiabio)
# ifdef AVERAGES
      if (nciddiabio_avg.ne.-1) ierr=nf_close(nciddiabio_avg)
# endif
#endif
#ifdef STATIONS
      if (ncidsta.ne.-1) ierr=nf_close(ncidsta)
#endif

      MPI_master_only write(stdout,'(/1x,2A,I5,3(/8x,A,I5)/)') 
     &   'MAIN - '
     &  ,'number of records written into history  file(s):', nrechis
     &  ,'number of records written into restart  file(s):', nrecrst
#ifdef AVERAGES
     &  ,'number of records written into averages file(s):', nrecavg
#endif
#ifdef STATIONS
     &  ,'number of records written into station  file(s):', nrecsta
#endif /* STATIONS */
#ifdef FLOATS
     &  ,'number of records written into floats   file(s):', nrecflt
#endif /* FLOATS */
#if defined DIAGNOSTICS_TS
     &  ,'number of records written into diags_t  file(s):', nrecdia
# ifdef AVERAGES
     &  ,'number of records written into avg diag_t  file(s):',
     &                                                      nrecdia_avg
# endif
#endif /*DIAGNOSTICS_UV*/
#if defined DIAGNOSTICS_UV
     &  ,'number of records written into diags_u  file(s):', nrecdiaM
# ifdef AVERAGES
     &  ,'number of records written into avg diags_u file(s):',
     &                                                      nrecdiaM_avg
# endif
#endif /* DIAGNOSTICS_UV */

      if (may_day_flag.eq.0) then
        MPI_master_only write(stdout,'(/1x,A/)')
     &           'MAIN: DONE'
      elseif (may_day_flag.eq.1) then
        MPI_master_only write(stdout,'(/1x,A/)')
     &           'MAIN: Abnormal termination: BLOWUP'
      elseif (may_day_flag.eq.2) then
        MPI_master_only write(stdout,'(/1x,A/)')
     &           'ERROR: Abnormal termination: netCDF INPUT'
      elseif (may_day_flag.eq.3) then
        MPI_master_only write(stdout,'(/1x,A/)')
     &           'ERROR: Abnormal termination: netCDF OUTPUT'
      elseif (may_day_flag.eq.4) then
        MPI_master_only write(stdout,'(/1x,A/)')
     &           'ERROR: Cannot open netCDF file'
      elseif (may_day_flag.eq.5) then
        MPI_master_only write(stdout,'(/1x,A/)')
     &           'ERROR: READ_INP: Error in input file'
      elseif (may_day_flag.eq.6) then
        MPI_master_only write(stdout,'(/1x,A/)')
     &           'ERROR: READ_INP: An input file is not available'
      elseif (may_day_flag.eq.7) then
        MPI_master_only write(stdout,'(/1x,A/)')
     &           'ERROR: KPP algoritm failure'
      endif

      return
      end
