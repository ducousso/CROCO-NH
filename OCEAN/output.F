! $Id: output.F 1586 2014-07-30 14:57:11Z marchesiello $
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
      subroutine output
!
!=======================================================================
!                                                                      !
! Output block: write restart/history files.
!                                                                      !
!=======================================================================
!
#ifdef AGRIF
      use Agrif_Util
#endif
      implicit none
      integer ilc
#include "param.h"
#include "scalars.h"
#include "ncscrum.h"
#ifdef FLOATS
# include "floats.h"
# include "ncscrum_floats.h"
#endif
#ifdef STATIONS
# include "sta.h"
# include "nc_sta.h"
#endif
!
                integer ntrds,trd,range,
     &        my_first,my_last, tile, my_iif

      integer omp_get_num_threads, omp_get_thread_num
      ilc=1+iic-ntstart   ! number of time step since restart
!
!
! Conditions for writing output
!
      if ( iic.gt.ntstart .and. ( mod(ilc-1,nrst).eq.0 .or.
     &         (mod(ilc-1,nwrt).eq.0 .and. wrthis(indxTime))
#ifdef AVERAGES
     &    .or. (mod(ilc-1,navg).eq.0 .and. wrtavg(indxTime))
#endif
#if defined DIAGNOSTICS_TS
     &    .or. (mod(ilc-1,nwrtdia).eq.0 .and. wrtdia3D(NT+1))
# ifdef AVERAGES
     &    .or. (mod(ilc-1,nwrtdia_avg).eq.0 .and. wrtdia3D_avg(NT+1))
# endif
# if defined DIAGNOSTICS_TS_MLD
     &    .or. (mod(ilc-1,nwrtdia).eq.0 .and. wrtdia2D(NT+1))
#  ifdef AVERAGES
     &    .or. (mod(ilc-1,nwrtdia_avg).eq.0 .and. wrtdia2D_avg(NT+1))
#  endif
# endif
#endif
#if defined DIAGNOSTICS_UV
     &    .or. (mod(ilc-1,nwrtdiaM).eq.0 .and. wrtdiaM(3))
# ifdef AVERAGES
     &    .or. (mod(ilc-1,nwrtdiaM_avg).eq.0 .and. wrtdiaM_avg(3))
# endif
#endif
#ifdef DIAGNOSTICS_BIO
     &    .or. (mod(ilc-1,nwrtdiabio).eq.0 .and. 
     &    (      wrtdiabioFlux(NumFluxTerms+1)
     &      .or. wrtdiabioVSink(NumVSinkTerms+1)
     &      .or. wrtdiabioGasExc(NumGasExcTerms+1) ) )

# ifdef AVERAGES
     &    .or. (mod(ilc-1,nwrtdiabio_avg).eq.0 .and. 
     &    (      wrtdiabioFlux_avg(NumFluxTerms+1)
     &      .or. wrtdiabioVSink_avg(NumVSinkTerms+1)
     &      .or. wrtdiabioGasExc_avg(NumGasExcTerms+1) ))
# endif
#endif /*DIAGNOSTICS_BIO*/
#ifdef FLOATS
# ifdef AGRIF
     &    .or. (Agrif_Root() .and. mod(ilc-1,nflt).eq.0
# else
     &    .or. (mod(ilc-1,nflt).eq.0
# endif
     &                             .and. nfloats.gt.0)
#endif
#ifdef STATIONS
# ifdef AGRIF
     &    .or. (Agrif_Root() .and. mod(ilc-1,nsta).eq.0
# else
     &    .or. (mod(ilc-1,nsta).eq.0
# endif
     &                             .and. nstas0.gt.0)
#endif
     &                                               )) then
!
! Get record number
!
        if (mod(ilc-1,nrst).eq.0) nrecrst=nrecrst+1
        if (mod(ilc-1,nwrt).eq.0) nrechis=nrechis+1
#ifdef AVERAGES
        if (mod(ilc-1,navg).eq.0) nrecavg=nrecavg+1
#endif
#if defined DIAGNOSTICS_TS
        if (mod(ilc-1,nwrtdia).eq.0) nrecdia=nrecdia+1
# ifdef AVERAGES
        if (mod(ilc-1,nwrtdia_avg).eq.0) nrecdia_avg=nrecdia_avg+1
# endif
#endif
#if defined DIAGNOSTICS_UV
        if (mod(ilc-1,nwrtdiaM).eq.0) nrecdiaM=nrecdiaM+1
# ifdef AVERAGES
        if (mod(ilc-1,nwrtdiaM_avg).eq.0) nrecdiaM_avg=nrecdiaM_avg+1
# endif
#endif
#ifdef DIAGNOSTICS_BIO
        if (mod(ilc-1,nwrtdiabio).eq.0) nrecdiabio=nrecdiabio+1
# ifdef AVERAGES
        if (mod(ilc-1,nwrtdiabio_avg).eq.0)
     &                          nrecdiabio_avg=nrecdiabio_avg+1
# endif
#endif /*DIAGNOSTICS_BIO*/
#ifdef FLOATS
# ifdef AGRIF
        if (Agrif_Root() .and. mod(ilc-1,nflt).eq.0)
# else
        if (mod(ilc-1,nflt).eq.0)
# endif
     &     nrecflt=nrecflt+1
#endif
#ifdef STATIONS
# ifdef AGRIF
        if (Agrif_Root() .and. mod(ilc-1,nsta).eq.0)
# else
        if (mod(ilc-1,nsta).eq.0)
# endif
     &     nrecsta=nrecsta+1
#endif
!
! Call output routines
!
!        write(*,*) "wrtdia3D ",wrtdia3D
!        write(*,*) "wrtdia2D ",wrtdia2D
!        write(*,*) "wrtdia_avg3D ",wrtdia3D_avg
!        write(*,*) "wrtdia_avg2D ",wrtdia2D_avg
!!
!        write(*,*) "wrtdiaM ",wrtdiaM
!        write(*,*) "wrtdiaM_avg ",wrtdiaM_avg
!
        if (mod(ilc-1,nrst).eq.0) call wrt_rst
#ifdef XIOS
       ntrds=omp_get_num_threads()
      trd=omp_get_thread_num()
      range=(NSUB_X*NSUB_E+ntrds-1)/ntrds
      my_first=trd*range
      my_last=min(my_first + range-1, NSUB_X*NSUB_E-1)
      
      do tile=my_first,my_last
          call send_xios_diags(tile)
      end do
#else
        if (mod(ilc-1,nwrt).eq.0 .and. wrthis(indxTime)) call wrt_his
#  ifdef AVERAGES
        if (mod(ilc-1,navg).eq.0 .and. wrtavg(indxTime)) call wrt_avg
#  endif
#endif
#if defined DIAGNOSTICS_TS
        if (mod(ilc-1,nwrtdia).eq.0 .and. (wrtdia3D(NT+1) .or.
     &      wrtdia2D(NT+1)))                              
     &                                                 call wrt_diags
# ifdef AVERAGES
        if (ldefdia_avg .and. mod(ilc-1,nwrtdia_avg).eq.0 
     &     .and. (wrtdia3D_avg(NT+1) .or. wrtdia2D_avg(NT+1))) 
     &                                             call wrt_diags_avg
# endif
#endif

#if defined DIAGNOSTICS_UV
        if (mod(ilc-1,nwrtdiaM).eq.0 .and. wrtdiaM(3)) 
     &                                              call wrt_diagsM
# ifdef AVERAGES
        if (ldefdiaM_avg .and. mod(ilc-1,nwrtdiaM_avg).eq.0
     &                 .and. wrtdiaM_avg(3))        call wrt_diagsM_avg
# endif
#endif
#if defined DIAGNOSTICS_BIO && ! defined XIOS
        if (mod(ilc-1,nwrtdiabio).eq.0 .and. 
     &       (      wrtdiabioFlux(NumFluxTerms+1)
     &       .or. wrtdiabioVSink(NumVSinkTerms+1)
     &       .or. wrtdiabioGasExc(NumGasExcTerms+1) ) ) then
          call wrt_bio_diags
        endif
# ifdef AVERAGES
        if (ldefdiabio_avg .and. mod(ilc-1,nwrtdiabio_avg).eq.0.and.   
     &       (      wrtdiabioFlux_avg(NumFluxTerms+1)
     &       .or. wrtdiabioVSink_avg(NumVSinkTerms+1)
     &       .or. wrtdiabioGasExc_avg(NumGasExcTerms+1) ) ) then
          call wrt_bio_diags_avg
        endif
# endif
#endif
#ifdef FLOATS
# ifdef AGRIF
        if (Agrif_Root()) then
# endif
          if (mod(ilc-1,nflt).eq.0
     &                .and.nfloats.gt.0) call wrt_floats
          diagfloats=.false.
# ifdef AGRIF
        endif
# endif
#endif
#ifdef STATIONS
# ifdef AGRIF
        if (Agrif_Root()) then
# endif
          if (mod(ilc-1,nsta).eq.0
     &                  .and.nstas0.gt.0) call wrt_sta
          diagsta=.false.
# ifdef AGRIF
        endif
# endif
#endif
!
      endif      ! conditions for writing output
!
#ifdef FLOATS
!
! Flag for diagnostic computation (writing at next time step)
!
# ifdef AGRIF
      if (Agrif_Root() .and. mod(ilc,nflt).eq.0) then
# else
      if (mod(ilc,nflt).eq.0) then
# endif
        diagfloats=.true.
      endif
#endif
#ifdef STATIONS
# ifdef AGRIF
      if (Agrif_Root() .and. mod(ilc,nsta).eq.0) then
# else
      if (mod(ilc,nsta).eq.0) then
# endif
        diagsta=.true.
      endif
#endif

      return
      end

