! $Id: read_inp.F 1615 2014-12-17 13:27:07Z rblod $
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
                                        ! Read/report model input
      subroutine read_inp (ierr)        ! parameters  from  startup 
                                        ! script file using keywords
                                        ! to recognize variables.
!     implicit none
#include "param.h"
#include "scalars.h"
#include "ncscrum.h"
#include "sediment.h"
#ifdef FLOATS
# include "ncscrum_floats.h"
#endif
#ifdef STATIONS
# include "nc_sta.h"
#endif
#if defined PSOURCE || defined PSOURCE_NCFILE
# include "sources.h"
#endif
#ifdef WKB_WWAVE
# include "wkb_wwave.h"
#endif
#ifdef ONLINE
# include "online.h"
#endif
#ifdef MPI
      include 'mpif.h'
#endif
#ifdef NBQ
# include "nbq.h"
#endif
#include "mpi_roms.h"
      integer kwsize, testunit, input
      parameter (kwsize=32, testunit=40, input=15)
      character end_signal*3, keyword*32, fname*64
      parameter (end_signal='end')
      integer ierr, iargc, is,ie, kwlen, lstr, lenstr
#ifdef SOLVE3D
     &                                       , itrc
#endif
      logical dumboolean
!
! Use pre-set default startup filename for known applications,
! or get it as an argument from command line via iargc-getarg

! (override default). NOTE: The usage of the executable should
! be either 
!           croco
!        or
!           croco startup_file_name
!
! WITHOUT the UNIX redirection (<): croco<startup_file like it
! used to be.
!
#if defined BASIN
      fname='TEST_CASES/croco.in.Basin'
#elif defined CANYON_A
      fname='TEST_CASES/croco.in.Canyon_A'
#elif defined CANYON_B
      fname='TEST_CASES/croco.in.Canyon_B'
#elif defined EQUATOR
      fname='TEST_CASES/croco.in.Equator'
#elif defined GRAV_ADJ
      fname='TEST_CASES/croco.in.Grav_adj'
#elif defined INNERSHELF
      fname='TEST_CASES/croco.in.Innershelf'
#elif defined INTERNAL
      fname='TEST_CASES/croco.in.Internal'
#elif defined OVERFLOW
      fname='TEST_CASES/croco.in.Overflow'
#elif defined RIVER
      fname='TEST_CASES/croco.in.River'
#elif defined SEAMOUNT
      fname='TEST_CASES/croco.in.Seamount'
#elif defined SHELFRONT
      fname='TEST_CASES/croco.in.Shelfront'
#elif defined SOLITON
      fname='TEST_CASES/croco.in.Soliton'
#elif defined UPWELLING
      fname='TEST_CASES/croco.in.Upwelling'
#elif defined VORTEX
      fname='TEST_CASES/croco.in.Vortex'
#elif defined JET
      fname='TEST_CASES/croco.in.Jet'
#elif defined SHOREFACE
      fname='TEST_CASES/croco.in.ShoreFace'
#elif defined RIP
      fname='TEST_CASES/croco.in.Rip'
#elif defined THACKER
      fname='TEST_CASES/croco.in.Thacker'
#elif defined TANK
# ifdef NBQ
      fname='TEST_CASES/croco.in.Tank'
# else
      fname='TEST_CASES/croco.in.Tank_nbq'
# endif
#elif defined REGIONAL
      fname='croco.in'
#else
      fname='no_startup_file'
#endif
#ifdef MPI
      if (mynode.eq.0 .and. iargc().GT.0) call getarg(1,fname) 
      call MPI_Bcast(fname,64,MPI_BYTE, 0, MPI_COMM_WORLD,ierr)
#else
      if (iargc().eq.1) call getarg(1,fname)
#endif
!
! if child grid, use an input name: fname.1, .2, .3, ...
!
#ifdef AGRIF
      if (.Not.Agrif_Root()) then
        lstr=lenstr(fname)
#ifdef AGRIF_ADAPTIVE
        fname=fname(1:lstr) / / '.1'  
#else        
        fname=fname(1:lstr) / / '.' / / Agrif_Cfixed()
#endif 
        lstr=lenstr(fname)
      endif
#endif
!
      wrthis(indxTime)=.false.
#ifdef AVERAGES
      wrtavg(indxTime)=.false.
#endif

#ifdef DIAGNOSTICS_TS
      wrtdia3D(NT+1)=.false.
# ifdef AVERAGES
      wrtdia3D_avg(NT+1)=.false.
# endif
# ifdef DIAGNOSTICS_TS_MLD
      wrtdia2D(NT+1)=.false.
#  ifdef AVERAGES
      wrtdia2D_avg(NT+1)=.false.
#  endif
# endif
#endif
#ifdef DIAGNOSTICS_UV
      wrtdiaM(3)=.false.
#ifdef AVERAGES
      wrtdiaM_avg(3)=.false.
#endif
#endif
#ifdef DIAGNOSTICS_BIO
      wrtdiabioFlux(NumFluxTerms+1)=.false.
      wrtdiabioVSink(NumVSinkTerms+1)=.false.
      wrtdiabioGasExc(NumGasExcTerms+1)=.false.
#ifdef AVERAGES
      wrtdiabioFlux_avg(NumFluxTerms+1)=.false.
      wrtdiabioVSink_avg(NumVSinkTerms+1)=.false.
      wrtdiabioGasExc_avg(NumGasExcTerms+1)=.false.   
#endif
#endif
!
! Read in keyword: keep trying, until keyword is found.
! ==== == ======== ==== ======= ===== ======= == ======
!
      ierr=0    ! <-- reset error counter
      call setup_kwds (ierr)
      open (input,file=fname,status='old',form='formatted',err=97)
   1  keyword='                                '
      read(input,'(A)',err=1,end=99) keyword
      if (ichar(keyword(1:1)).eq.33) goto 1
      is=1
   2  if (is.eq.kwsize) then
        goto 1
      elseif (keyword(is:is).eq.' ') then
        is=is+1
        goto 2
      endif
      ie=is
   3  if (keyword(ie:ie).eq.':') then
        keyword(ie:ie)=' '
        goto 4           !--> recognized keyword.               
      elseif (keyword(ie:ie).ne.' ' .and. ie.lt.kwsize) then 
        ie=ie+1
        goto 3
      endif
      goto 1
   4  kwlen=ie-is
      if (is.gt.1) keyword(1:kwlen)=keyword(is:is+kwlen-1)
!
! Read input parameters according to the keyword:
! ==== ===== ========== ========= == === ========
!
! Title
!
      if (keyword(1:kwlen).eq.'title') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,'(A)',err=95) title
        lstr=lenstr(title)
        MPI_master_only write(stdout,'(/1x,A)') title(1:lstr)
!
#ifdef START_DATE
!
! Start date
!
      elseif (keyword(1:kwlen).eq.'start_date') then
         call cancel_kwd (keyword(1:kwlen), ierr)
         read(input,'(A)',err=95) start_date
         lstr=lenstr(start_date)
         MPI_master_only write(stdout,'(/2x,A,1x,A/)') 
     &   'Start date:', start_date(1:lstr)
#endif
!     
! Time-stepping parameters
!
      elseif (keyword(1:kwlen).eq.'time_stepping') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) ntimes,dt,ndtfast, ninfo
        MPI_master_only write(stdout,
     & '(I10,2x,A,1x,A /F10.2,2x,A,2(/I10,2x,A,1x,A)/F10.4,2x,A)')
     &    ntimes,  'ntimes   Total number of timesteps for',
     &                                            '3D equations.',
     &    dt,      'dt       Timestep [sec] for 3D equations',
     &    ndtfast, 'ndtfast  Number of 2D timesteps within each',
     &                                                 '3D step.',
     &    ninfo,   'ninfo    Number of timesteps between',
     &                                     'runtime diagnostics.'
        dtfast=dt/float(ndtfast)     ! set barotropic time step.

        if (NWEIGHT.lt.(2*ndtfast-1)) then
          write(stdout,'(a,i3)')
     &    ' Error - Number of 2D timesteps (2*ndtfast-1): ',
     &    2*ndtfast-1
          write(stdout,'(a,i3)')
     &    '           exceeds barotopic weight dimension: ',NWEIGHT
          goto 95
        endif

#ifdef NBQ
!     
! NBQ Time-stepping parameters
!
      elseif (keyword(1:kwlen).eq.'time_stepping_nbq') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) ndtnbq, csound_nbq, visc2_nbq
        MPI_master_only write(stdout,
     &    '(I10,2x,A,1x,A,/F10.2,2x,A,/1pe10.3,2x,A,1x,A/)')
     &   ndtnbq,    'ndtnbq      Number of NBQ timesteps within each',
     &                                                 '2D step.',
     &   csound_nbq,'csound_nbq  Sound wave celerity.',
     &   visc2_nbq, 'visc2_nbq   Second viscosity coefficient for',
     &                                      'compressible fluids.'

        dtnbq=dtfast/float(ndtnbq)     ! set nbq time step.
#endif

#ifdef SOLVE3D
!
! Vertical S-coordinates parameters.
!
      elseif (keyword(1:kwlen).eq.'S-coord') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) theta_s, theta_b, Tcline
        MPI_master_only write(stdout,
     &                        '(3(1pe10.3,2x,A,1x,A/),32x,A)')
     &    theta_s, 'theta_s  S-coordinate surface control',
     &                                               'parameter.',
     &    theta_b, 'theta_b  S-coordinate bottom control',
     &                                               'parameter.',
     &    Tcline,  'Tcline   S-coordinate surface/bottom layer',
     &  'width used in', 'vertical coordinate stretching, meters.'
#endif
!
! Initial conditions file name. Check its availability (in the case
! of analytical initial conditions and nrrec=0 initial conditions are
! created internally and no file is needed).
!
      elseif (keyword(1:kwlen).eq.'initial') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) nrrec
#ifdef ANA_INITIAL
        if (nrrec.gt.0) then 
#endif
          read(input,'(A)',err=95) fname
          lstr=lenstr(fname)
#if defined MPI && defined PARALLEL_FILES
          call insert_node (fname, lstr, mynode, NNODES, ierr)
#endif
          open (testunit, file=fname(1:lstr), status='old', err=97)
          close(testunit)
          ininame=fname(1:lstr)
          MPI_master_only write(stdout,'(1x,A,2x,A,4x,A,I3)')
     &     'Initial State File:', ininame(1:lstr), 'Record:',nrrec
#ifdef ANA_INITIAL
        endif
#endif
#ifndef ANA_GRID
!
! Grid file name. Check its availability.
!
      elseif (keyword(1:kwlen).eq.'grid') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,'(A)',err=95) fname 
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        open(testunit,file=fname(1:lstr), status='old', err=97)
        close(testunit)
        grdname=fname(1:lstr)
        MPI_master_only write(stdout,'(10x,A,2x,A)')
     &                   'Grid File:', grdname(1:lstr)
#endif

#if (!defined BULK_FLUX  && !defined ANA_SMFLUX) \
        || (defined TIDES) \
        || (defined MRL_WCI     && !defined ANA_WWAVE   && \
                                   !defined WKB_WWAVE && \
                                   !defined OW_COUPLING) \
        || (defined SOLVE3D     && (\
                                  (!defined ANA_STFLUX  && !defined BULK_FLUX) \
        ||                         !defined ANA_BTFLUX   \
        || (defined BBL         && !defined ANA_BSEDIM  && !defined SEDIMENT) \
        || (defined BBL         && !defined ANA_WWAVE   && \
                                   !defined WKB_WWAVE && \
                                   !defined OW_COUPLING) \
        || (defined QCORRECTION && !defined ANA_SST)   \
        || (defined SALINITY    && !defined ANA_SSFLUX  && \
                                   !defined BULK_FLUX   && !defined BULK_EP) \
        || (defined SALINITY    &&  defined QCORRECTION && \
                                    defined SFLX_CORR   && !defined ANA_SSS) \
        || (defined LMD_SKPP    && !defined ANA_SRFLUX  && !defined BULK_FLUX) \
        || (defined BULK_FLUX   && \
                                  (!defined BULK_SMFLUX || !defined BULK_EP)) \
                                   )\
           )\
        && !defined OA_COUPLING
!
!
! Forcing file name. Check its availability.
!
      elseif (keyword(1:kwlen).eq.'forcing') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,'(A)',err=95) fname
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        open (testunit, file=fname(1:lstr), status='old', err=97)
        close(testunit)
        frcname=fname(1:lstr)
        MPI_master_only write(stdout,'(2x,A,2x,A)')
     &             'Forcing Data File:', frcname(1:lstr)
#endif
!
! Biology forcing: iron dust deposition. 
!
#if defined BIOLOGY && defined PISCES
      elseif (keyword(1:kwlen).eq.'biology') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,'(A)',err=95) fname
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
c        open (testunit, file=fname(1:lstr), status='old', err=97)
c        close(testunit)
        bioname=fname(1:lstr)
        MPI_master_only write(stdout,'(2x,A,2x,A)')
     &             'Biology Forcing Data File:', bioname(1:lstr)
#endif
!
! Bulk file name. Check its availability.
!
#if defined BULK_FLUX
# ifndef ONLINE /* ONLINE FORCING */
        elseif (keyword(1:kwlen).eq.'bulk_forcing') then
          call cancel_kwd (keyword(1:kwlen), ierr)
          read(input,'(A)',err=95) fname
          lstr=lenstr(fname)
#  if defined MPI && defined PARALLEL_FILES
          call insert_node (fname, lstr, mynode, NNODES, ierr)
#  endif
          open (testunit, file=fname(1:lstr), status='old', err=97)
          close(testunit)
          bulkname=fname(1:lstr)
          MPI_master_only write(stdout,'(2x,A,2x,A)')
     &               '   Bulk Data File:', bulkname(1:lstr)
# endif  /* ONLINE FORCING */
#endif
#if (defined TCLIMATOLOGY  && !defined ANA_TCLIMA) || \
      (defined ZCLIMATOLOGY  && !defined ANA_SSH) || \
      (defined M2CLIMATOLOGY && !defined ANA_M2CLIMA) || \
      (defined M3CLIMATOLOGY && !defined ANA_M3CLIMA)
!
! Climatology file name. Check availability.
!
      elseif (keyword(1:kwlen).eq.'climatology') then
        call cancel_kwd (keyword(1:kwlen), ierr)
# ifdef AGRIF
       if (Agrif_Root()) then
# endif
        read(input,'(A)',err=95) fname 
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        open (testunit, file=fname(1:lstr), status='old', err=97)
        close(testunit)
        clmname=fname(1:lstr)
        MPI_master_only write(stdout,'(3x,A,2x,A)')
     &            'Climatology File:', clmname(1:lstr)
# ifdef AGRIF
       endif
# endif
#endif
#if !defined ANA_BRY && defined FRC_BRY
!
! Boundary file name. Check availability.
!
        elseif (keyword(1:kwlen).eq.'boundary') then
          call cancel_kwd (keyword(1:kwlen), ierr)
#  ifdef AGRIF
       if (Agrif_Root()) then
#  endif
          read(input,'(A)',err=95) fname
          lstr=lenstr(fname)
#  if defined MPI && defined PARALLEL_FILES
          call insert_node (fname, lstr, mynode, NNODES, ierr)
#  endif
          open (testunit, file=fname(1:lstr), status='old', err=97)
          close(testunit)
          bry_file=fname(1:lstr)
          MPI_master_only write(stdout,'(6x,A,2x,A)')
     &          'Boundary File:', bry_file(1:lstr)
#  ifdef AGRIF
       endif
#  endif
#endif
#if defined WKB_WWAVE && !defined ANA_BRY_WKB 
!
! WKB boundary file name. Check availability.
!
        elseif (keyword(1:kwlen).eq.'wkb_boundary') then
          call cancel_kwd (keyword(1:kwlen), ierr)
#  ifdef AGRIF
       if (Agrif_Root()) then
#  endif
          read(input,'(A)',err=95) fname
          lstr=lenstr(fname)
#  if defined MPI && defined PARALLEL_FILES
          call insert_node (fname, lstr, mynode, NNODES, ierr)
#  endif
          open (testunit, file=fname(1:lstr), status='old', err=97)
          close(testunit)
          brywkb_file=fname(1:lstr)
          MPI_master_only write(stdout,'(6x,A,2x,A)')
     &          'WKB Boundary File:', brywkb_file(1:lstr)
#  ifdef AGRIF
       endif
#  endif
#endif
!
! Restart file name.
!
      elseif (keyword(1:kwlen).eq.'restart') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) nrst, nrpfrst
        read(input,'(A)',err=95)  fname 
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        rstname=fname(1:lstr)
        MPI_master_only write(stdout,
     &             '(7x,A,2x,A,4x,A,I6,4x,A,I4)')
     &             'Restart File:', rstname(1:lstr),
     &             'nrst =', nrst, 'rec/file: ', nrpfrst 
!
! History file name.
!
      elseif (keyword(1:kwlen).eq.'history') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) ldefhis, nwrt, nrpfhis
        read(input,'(A)',err=95) fname
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        hisname=fname(1:lstr)
        MPI_master_only write(stdout,
     &             '(7x,A,2x,A,2x,A,1x,L1,2x,A,I4,2x,A,I3)')
     &       'History File:', hisname(1:lstr),  'Create new:',
     &       ldefhis, 'nwrt =', nwrt, 'rec/file =', nrpfhis
#ifdef AVERAGES
!
! Averages file name.
!
      elseif (keyword(1:kwlen).eq.'averages') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) ntsavg, navg, nrpfavg
        read(input,'(A)',err=95) fname
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        avgname=fname(1:lstr)
        MPI_master_only write(stdout,
     &         '(2(I10,2x,A,1x,A/32x,A/),6x,A,2x,A,1x,A,I3)')
     &      ntsavg, 'ntsavg      Starting timestep for the',
     &         'accumulation of output', 'time-averaged data.',
     &      navg,   'navg        Number of timesteps between',
     &     'writing of time-averaged','data into averages file.',
     &     'Averages File:', avgname(1:lstr),
     &     'rec/file =', nrpfavg
#endif

#if defined DIAGNOSTICS_TS
!
! Diagnostics file name.
!
      elseif (keyword(1:kwlen).eq.'diagnostics') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) ldefdia, nwrtdia, nrpfdia
        read(input,'(A)',err=95) fname
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        dianame=fname(1:lstr)
        MPI_master_only write(stdout,
     &    '(9x,A,2x,A/,32x,A,1x,L1,2x,A,I4,2x,A,I3)')
     &    'Tracer Diag File:',dianame(1:lstr),'Create new:',
     &    ldefdia,'nwrt =',nwrtdia,'rec/file =',nrpfdia
!
# ifdef AVERAGES
      elseif (keyword(1:kwlen).eq.'diag_avg') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) ldefdia_avg, ntsdia_avg, nwrtdia_avg,
     &                                                nrpfdia_avg
        read(input,'(A)',err=95) fname
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        dianame_avg=fname(1:lstr)
        MPI_master_only write(stdout,
     &    '(5x,A,2x,A/,32x,A,1x,L1,2x,A,I4,2x,A,I3,/32x,A,I10)')
     &    'Tracer AVG Diag File:',dianame_avg(1:lstr),'Create new:',
     &    ldefdia_avg,'nwrt =',nwrtdia_avg,'rec/file =',nrpfdia_avg,
     &    'Starting timestep = ',ntsdia_avg
# endif
#endif /* DIAGOSTICS_TS */
#if defined DIAGNOSTICS_UV
!
! Diagnostics Momentum file name.
!
      elseif (keyword(1:kwlen).eq.'diagnosticsM') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) ldefdiaM, nwrtdiaM, nrpfdiaM
        read(input,'(A)',err=95) fname
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        dianameM=fname(1:lstr)
        MPI_master_only write(stdout,
     &    '(5x,A,2x,A/,32x,A,1x,L1,2x,A,I4,2x,A,I3)')
     &    'Momentum Diag File:', dianameM(1:lstr),  'Create new:',
     &    ldefdiaM, 'nwrt =', nwrtdiaM, 'rec/file =', nrpfdiaM
!
# ifdef AVERAGES
      elseif (keyword(1:kwlen).eq.'diagM_avg') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) ldefdiaM_avg, ntsdiaM_avg, nwrtdiaM_avg,
     &                                                  nrpfdiaM_avg
        read(input,'(A)',err=95) fname
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        dianameM_avg=fname(1:lstr)
        MPI_master_only write(stdout,
     &    '(5x,A,2x,A/,32x,A,1x,L1,2x,A,I4,2x,A,I3,/32x,A,I10)')
     &    'Momentum AVG Diag File:',dianameM_avg(1:lstr),'Create new:',
     &    ldefdiaM_avg,'nwrt =',nwrtdiaM_avg,'rec/file =',nrpfdiaM_avg,
     &    'Starting timestep = ',ntsdiaM_avg

# endif
#endif /*DIAGNOSTICS_UV */
#ifdef DIAGNOSTICS_BIO
!
! Diagnostics Biology file name.
!
      elseif (keyword(1:kwlen).eq.'diagnostics_bio') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) ldefdiabio, nwrtdiabio, nrpfdiabio
        read(input,'(A)',err=95) fname
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        dianamebio=fname(1:lstr)
        MPI_master_only write(stdout,
     &    '(8x,A,2x,A/,32x,A,1x,L1,2x,A,I4,2x,A,I3)') 
     &    'Biology Diag File:', dianamebio(1:lstr),  'Create new:',
     &    ldefdiabio, 'nwrt =', nwrtdiabio, 'rec/file =', nrpfdiabio
!
# ifdef AVERAGES
      elseif (keyword(1:kwlen).eq.'diagbio_avg') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) ldefdiabio_avg, ntsdiabio_avg,
     &                       nwrtdiabio_avg, nrpfdiabio_avg
        read(input,'(A)',err=95) fname
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        dianamebio_avg=fname(1:lstr)
        MPI_master_only write(stdout,
     &    '(4x,A,2x,A/,32x,A,1x,L1,2x,A,I4,2x,A,I3,/32x,A,I10)')
     &    'Biology AVG Diag File:',dianamebio_avg(1:lstr),'Create new:',
     &    ldefdiabio_avg,'nwrt =',nwrtdiabio_avg,'rec/file =',
     &    nrpfdiabio_avg,'Starting timestep = ',ntsdiabio_avg
# endif
#endif /* DIAGNOSTICS_BIO */

#ifdef FLOATS
!
! Floats file name.
!
      elseif (keyword(1:kwlen).eq.'floats') then
        call cancel_kwd (keyword(1:kwlen), ierr)
#ifdef AGRIF
        if (Agrif_Root()) then
#endif
          read(input,*,err=95) ldefflt, nflt, nrpfflt
          read(input,'(A)',err=95) fposnam
          read(input,'(A)',err=95) fname
          lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
          call insert_node (fposnam, lstr, mynode, NNODES, ierr)
# endif
          fltname=fname(1:lstr)
          MPI_master_only write(stdout,
     &              '(9x,A,2x,A,2x,A,1x,L1,2x,A,I4,2x,A,I3)')
     &        'Float File:',fltname(1:lstr),  'Create new:',
     &        ldefflt, 'nflt =', nflt, 'rec/file =', nrpfflt
#ifdef AGRIF
        else
          ldefflt=Agrif_Parent(ldefflt)
          nflt=Agrif_Parent(nflt)
          nrpfflt=Agrif_Parent(nrpfflt)
        endif
#endif
#endif /* FLOATS */

#ifdef STATIONS
!
! Stations file name.
!
      elseif (keyword(1:kwlen).eq.'stations') then
        call cancel_kwd (keyword(1:kwlen), ierr)
# ifdef AGRIF
        if (Agrif_Root()) then
# endif
          read(input,*,err=95) ldefsta, nsta, nrpfsta
          read(input,'(A)',err=95) staposname
          read(input,'(A)',err=95) fname
          lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
          call insert_node (staposname, lstr, mynode, NNODES, ierr)
# endif
          staname=fname(1:lstr)
          MPI_master_only write(stdout,
     &              '(9x,A,2x,A,2x,A,1x,L1,2x,A,I4,2x,A,I3)')
     &        'Station File:',staname(1:lstr),  'Create new:',
     &        ldefsta, 'nsta =', nsta, 'rec/file =', nrpfsta
# ifdef AGRIF
        else
          ldefsta=Agrif_Parent(ldefsta)
          nsta=Agrif_Parent(nsta)
          nrpfsta=Agrif_Parent(nrpfsta)
        endif
# endif
#endif /* STATIONS */

#ifdef ASSIMILATION
!
! Assimilation input/output file names.
!
      elseif (keyword(1:kwlen).eq.'assimilation') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,'(A)',err=95) aparnam
        read(input,'(A)',err=95) assname
        fname=aparnam
        lstr=lenstr(aparnam)
        open (testunit,file=aparnam(1:lstr),status='old',err=97)
        close(testunit)
        MPI_master_only write(stdout,'(1x,A,2x,A)')
     &         'Assimilation Parameters File:', aparnam(1:lstr)
        fname=assname
        lstr=lenstr(assname)
        open (testunit,file=assname(1:lstr),status='old',err=97)
        close(testunit)
        MPI_master_only write(stdout,'(12x,A,2x,A)')
     &                    'Assimilation File:', assname(1:lstr)
#endif
!
! Switches for fields to be saved into history file.
!
      elseif (keyword(1:kwlen).eq.'primary_history_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) wrthis(indxZ),  wrthis(indxUb)
     &                                       ,  wrthis(indxVb)
#ifdef SOLVE3D
     &                    ,  wrthis(indxU),  wrthis(indxV)
     &                    , (wrthis(itrc), itrc=indxT,indxT+NT-1) 
#endif
        if ( wrthis(indxZ) .or. wrthis(indxUb) .or. wrthis(indxVb)
#ifdef SOLVE3D
     &                        .or. wrthis(indxU) .or. wrthis(indxV)
#endif
     &     ) wrthis(indxTime)=.true.

        MPI_master_only write(stdout,'(/1x,A,5(/6x,l1,2x,A,1x,A))')
     &    'Fields to be saved in history file: (T/F)'
     &    , wrthis(indxZ),  'write zeta ', 'free-surface.'
     &    , wrthis(indxUb), 'write UBAR ', '2D U-momentum component.'
     &    , wrthis(indxVb), 'write VBAR ', '2D V-momentum component.'
#ifdef SOLVE3D
     &    , wrthis(indxU),  'write U    ', '3D U-momentum component.'
     &    , wrthis(indxV),  'write V    ', '3D V-momentum component.'
        do itrc=1,NT
          if (wrthis(indxT+itrc-1)) wrthis(indxTime)=.true.
          MPI_master_only write(stdout, '(6x,L1,2x,A,I2,A,I2,A)')
     &                     wrthis(indxT+itrc-1), 'write T(', itrc,
     &                              ')  Tracer of index', itrc,'.'
        enddo
#endif

#if !defined SOLVE3D && defined RIP
      elseif (keyword(1:kwlen).eq.'auxiliary_history_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95)
     &                                             wrthis(indxBostr)
     &                                          ,  wrthis(indxWstr)
     &                                          ,  wrthis(indxUWstr)
     &                                          ,  wrthis(indxVWstr)
#elif defined SOLVE3D
      elseif (keyword(1:kwlen).eq.'auxiliary_history_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95)
     &                                             wrthis(indxR)
     &                                          ,  wrthis(indxO)
     &                                          ,  wrthis(indxW)
     &                                          ,  wrthis(indxAkv)
     &                                          ,  wrthis(indxAkt)
# ifdef SALINITY
     &                                          ,  wrthis(indxAks)
# else 
     &                                          ,  dumboolean
# endif
# ifdef VIS_COEF_3D
     &                                          ,  wrthis(indxVisc)
# else 
     &                                          ,  dumboolean
# endif
# ifdef DIF_COEF_3D
     &                                          ,  wrthis(indxDiff)
# else 
     &                                          ,  dumboolean
# endif
# ifdef LMD_SKPP
     &                                          ,  wrthis(indxHbl)
# else 
     &                                          ,  dumboolean
# endif
# ifdef LMD_BKPP
     &                                          ,  wrthis(indxHbbl)
# else 
     &                                          ,  dumboolean
# endif
     &                                          ,  wrthis(indxBostr)
     &                                          ,  wrthis(indxWstr)
     &                                          ,  wrthis(indxUWstr)
     &                                          ,  wrthis(indxVWstr)
     &                                          ,  wrthis(indxShflx)
# ifdef SALINITY
     &                                          ,  wrthis(indxSwflx)
# else 
     &                                          ,  dumboolean
# endif
     &                                          ,  wrthis(indxShflx_rsw)
# ifdef BULK_FLUX
     &                                          , wrthis(indxShflx_rlw)
     &                                          , wrthis(indxShflx_lat)
     &                                          , wrthis(indxShflx_sen)
# else 
     &                                          ,  dumboolean
     &                                          ,  dumboolean
     &                                          ,  dumboolean
# endif
# ifdef MOVING_BATHY
     &                                          , wrthis(indxHm)
# else 
     &                                          ,  dumboolean
# endif
# if defined BIOLOGY && !defined PISCES
     &                                          ,  wrthis(indxHel)
#  ifdef BIO_NChlPZD
     &                                          ,  wrthis(indxChC)
#   ifdef OXYGEN
     &                                          ,  wrthis(indxU10)
     &                                          ,  wrthis(indxKvO2)
     &                                          ,  wrthis(indxO2sat)
#   endif
#  elif defined BIO_BioEBUS
     &                                          ,  wrthis(indxAOU)
     &                                          ,  wrthis(indxWIND10)
#  endif
# endif

        if ( wrthis(indxR) 
     &                                        .or. wrthis(indxO) 
     &                                        .or. wrthis(indxW)
     &                                        .or. wrthis(indxAkv)
     &                                        .or. wrthis(indxAkt)
# ifdef SALINITY
     &                                        .or. wrthis(indxAks)
# endif
# ifdef VIS_COEF_3D
     &                                        .or. wrthis(indxVisc)
# endif
# ifdef DIF_COEF_3D
     &                                        .or. wrthis(indxDiff)
# endif
# ifdef LMD_SKPP
     &                                        .or. wrthis(indxHbl)
# endif
# ifdef LMD_BKPP
     &                                        .or. wrthis(indxHbbl)
# endif
     &                                        .or. wrthis(indxBostr)
     &                                        .or. wrthis(indxWstr)
     &                                        .or. wrthis(indxUWstr)
     &                                        .or. wrthis(indxVWstr)
     &                                        .or. wrthis(indxShflx)
# ifdef SALINITY
     &                                        .or. wrthis(indxSwflx)
# endif
     &                                        .or. wrthis(indxShflx_rsw)
# if defined BULK_FLUX
     &                                        .or. wrthis(indxShflx_rlw)
     &                                        .or. wrthis(indxShflx_lat)
     &                                        .or. wrthis(indxShflx_sen)
# endif
# if defined BIOLOGY && !defined PISCES
     &                                        .or. wrthis(indxHel)
#  ifdef BIO_NChlPZD
     &                                        .or. wrthis(indxChC)
#   ifdef OXYGEN
     &                                        .or. wrthis(indxU10)
     &                                        .or. wrthis(indxKvO2)
     &                                        .or. wrthis(indxO2sat)
#   endif
#  elif defined BIO_BioEBUS
     &                                        .or. wrthis(indxAOU)
     &                                        .or. wrthis(indxWIND10)
#  endif
# endif
     &     ) wrthis(indxTime)=.true.


        MPI_master_only write(stdout,'(8(/6x,l1,2x,A,1x,A))')
     &    wrthis(indxR),    'write RHO  ', 'Density anomaly.'
     &  , wrthis(indxO),    'write Omega', 'Omega vertical velocity.'
     &  , wrthis(indxW),    'write W    ', 'True vertical velocity.'
     &  , wrthis(indxAkv),  'write Akv  ', 'Vertical viscosity.'
     &  , wrthis(indxAkt),  'write Akt  ',
     &                      'Vertical diffusivity for temperature.'
# ifdef SALINITY
     &  , wrthis(indxAks),  'write Aks  ',
     &                      'Vertical diffusivity for salinity.'
# endif
# ifdef VIS_COEF_3D
     &  , wrthis(indxVisc),  'write Visc3d', 'Horizontal viscosity.'
# endif
# ifdef DIF_COEF_3D
     &  , wrthis(indxDiff),  'write Visc3d', 'Horizontal diffusivity.'
# endif
# ifdef LMD_SKPP
     &  , wrthis(indxHbl),  'write Hbl  ',
     &                      'Depth of KPP-model boundary layer.'
# endif
# ifdef LMD_BKPP
     &  , wrthis(indxHbbl), 'write Hbbl  ',
     &                      'Depth of bottom planetary boundary layer.'
# endif
# ifdef BULK_FLUX
     &  , wrthis(indxShflx_rlw), 'write shflx_rlw [W/m2]', 
     &                                 'Long Wave heat flux.'
     &  , wrthis(indxShflx_lat), 'write shflx_lat [W/m2]',
     &                                 'Latent heat flux.'
     &  , wrthis(indxShflx_sen), 'write shflx_sen [W/m2]',
     &                                 'Sensible heat flux'
# endif
# ifdef MOVING_BATY
     &  , wrthis(indxHm), 'write Hm [m]',
     &                                 'Time evolving bathymetry'
# endif 
# if defined BIOLOGY && !defined PISCES
     &  , wrthis(indxHel),   'write Hel  ',
     &                       'Depth of the euphotic layer'
#  ifdef BIO_NChlPZD
     &  , wrthis(indxChC),   'write ChC  ',
     &                       'Chlorophyll to Carbon ratio'
#   ifdef OXYGEN
     &  , wrthis(indxU10),   'write u10 ',
     &                       'Wind speed at 10 m height'
     &  , wrthis(indxKvO2),  'write Kv_O2 ',
     &                       'Gas transfer coefficient for O2'
     &  , wrthis(indxO2sat), 'write O2sat ',
     &                       'Saturation concentration of O2'
#   endif
#  elif defined BIO_BioEBUS 
     &  , wrthis(indxAOU), 'write AOU ' 
     &  ,                  'Apparent Oxygen Utilization.'
     &  , wrthis(indxWIND10), 'write wind10 ', 'wind speed at 10 m.'  
#  endif
# endif /* BIOLOGY */
     &  , wrthis(indxBostr), 'write Bostr', 'Bottom Stress.'
     &  , wrthis(indxWstr),  'write Wstress', 'Wind Stress.'
     &  , wrthis(indxUWstr), 'write U-Wstress comp.', 'U-Wind Stress.'
     &  , wrthis(indxVWstr), 'write V-Wstress comp.', 'V-Wind Stress.'
     &  , wrthis(indxShflx), 'write Shflx [W/m2]', 
     &                       'Surface net heat flux'
# ifdef SALINITY
     &  , wrthis(indxSwflx), 'write Swflx [cm/day]', 
     &                       'Surface freshwater flux (E-P)'
# endif
     &  , wrthis(indxShflx_rsw),'write Shflx_rsw [W/m2]', 
     &                          'Short-wave surface radiation'

!
! Switches for GLS fields to be saved into history file.
!
# ifdef GLS_MIXING
      elseif (keyword(1:kwlen).eq.'gls_history_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) wrthis(indxAkk),  wrthis(indxAkp)
     &                    ,  wrthis(indxTke),  wrthis(indxGls)
     &                    ,  wrthis(indxLsc)
        if ( wrthis(indxAkk) .or. wrthis(indxAkp) .or. wrthis(indxTke)
     &                       .or. wrthis(indxGls) .or. wrthis(indxLsc)
     &     ) wrthis(indxTime)=.true.

        MPI_master_only write(stdout,'(/1x,A,5(/6x,l1,2x,A,1x,A))')
     &    'Fields to be saved in history file: (T/F)'
     &   , wrthis(indxAkk), 'write Akk ', 'vertical diffusion for TKE.'
     &   , wrthis(indxAkp), 'write Akp ', 'vertical diffusion for GLS.'
     &   , wrthis(indxTke), 'write TKE ', 'turbulent kinetic energy.  '
     &   , wrthis(indxGls), 'write GLS ', 'generic length scale.'
     &   , wrthis(indxLsc), 'write Lscale ', 
     &                                  'vertical mixing length scale.'
# endif
#endif /* SOLVE3D */

#ifdef AVERAGES
!
! Switches for fields to be saved into averages file.
!
      elseif (keyword(1:kwlen).eq.'primary_averages') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) wrtavg(indxZ),  wrtavg(indxUb)
     &                                    ,  wrtavg(indxVb)
# ifdef SOLVE3D
     &                    ,  wrtavg(indxU),  wrtavg(indxV)
     &                    , (wrtavg(itrc), itrc=indxT,indxT+NT-1)
# endif
        if ( wrtavg(indxZ) .or. wrtavg(indxUb) .or. wrtavg(indxVb)
# ifdef SOLVE3D
     &                     .or. wrtavg(indxU)  .or. wrtavg(indxV)
# endif
     &     ) wrtavg(indxTime)=.true.

        MPI_master_only write(stdout,'(/1x,A,5(/6x,l1,2x,A,1x,A))')
     &  'Fields to be saved in averages file: (T/F)'
     &  , wrtavg(indxZ),  'write zeta ', 'free-surface.'
     &  , wrtavg(indxUb), 'write UBAR ', '2D U-momentum component.'
     &  , wrtavg(indxVb), 'write VBAR ', '2D V-momentum component.'
# ifdef SOLVE3D
     &  , wrtavg(indxU),  'write U    ', '3D U-momentum component.'
     &  , wrtavg(indxV),  'write V    ', '3D V-momentum component.'
        do itrc=1,NT
          if (wrtavg(indxT+itrc-1)) wrtavg(indxTime)=.true.
          MPI_master_only write(stdout,
     &                     '(6x,L1,2x,A,I2,A,2x,A,I2,A)')
     &                      wrtavg(indxT+itrc-1), 'write T(',
     &                      itrc,')', 'Tracer of index', itrc,'.'
        enddo

      elseif (keyword(1:kwlen).eq.'auxiliary_averages') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) wrtavg(indxR), wrtavg(indxO)
     &        ,  wrtavg(indxW),  wrtavg(indxAkv),  wrtavg(indxAkt)
# ifdef SALINITY
     &                                          ,  wrtavg(indxAks)
# else 
     &                                          ,  dumboolean
# endif
# ifdef VIS_COEF_3D
     &                                          ,  wrtavg(indxVisc)
# else 
     &                                          ,  dumboolean
# endif
# ifdef DIF_COEF_3D
     &                                          ,  wrtavg(indxDiff)
# else 
     &                                          ,  dumboolean
# endif
# ifdef LMD_SKPP
     &                                          ,  wrtavg(indxHbl)
# else 
     &                                          ,  dumboolean
# endif
# ifdef LMD_BKPP
     &                                          ,  wrtavg(indxHbbl)
# else 
     &                                          ,  dumboolean
# endif
     &                                          ,  wrtavg(indxBostr)
     &                                          ,  wrtavg(indxWstr)
     &                                          ,  wrtavg(indxUWstr)
     &                                          ,  wrtavg(indxVWstr)
     &                                          ,  wrtavg(indxShflx)
# ifdef SALINITY
     &                                          ,  wrtavg(indxSwflx)
# else 
     &                                          ,  dumboolean
# endif
     &                                          ,  wrtavg(indxShflx_rsw)
# ifdef BULK_FLUX
     &                                          , wrtavg(indxShflx_rlw)
     &                                          , wrtavg(indxShflx_lat)
     &                                          , wrtavg(indxShflx_sen)
# else 
     &                                          ,  dumboolean
     &                                          ,  dumboolean
     &                                          ,  dumboolean
# endif
# ifdef MOVING_BATHY
     &                                          , wrtavg(indxHm)
# else 
     &                                          ,  dumboolean
# endif
# if defined BIOLOGY && !defined PISCES
     &                                          ,  wrtavg(indxHel)
#  ifdef BIO_NChlPZD
     &                                          ,  wrtavg(indxChC)
#   ifdef OXYGEN
     &                                          ,  wrtavg(indxU10)
     &                                          ,  wrtavg(indxKvO2)
     &                                          ,  wrtavg(indxO2sat)
#   endif
#  elif defined BIO_BioEBUS
     &                                          ,  wrtavg(indxAOU)
     &                                          ,  wrtavg(indxWIND10)
#  endif
# endif

        if ( wrtavg(indxR) .or. wrtavg(indxO) .or. wrtavg(indxW)
     &                   .or. wrtavg(indxAkv) .or. wrtavg(indxAkt)
#  ifdef SALINITY
     &                                        .or. wrtavg(indxAks)
#  endif
# ifdef VIS_COEF_3D
     &                                        .or. wrtavg(indxVisc)
# endif
# ifdef DIF_COEF_3D
     &                                        .or. wrtavg(indxDiff)
# endif
#  ifdef LMD_SKPP
     &                                        .or. wrtavg(indxHbl)
#  endif
#  ifdef LMD_BKPP
     &                                        .or. wrtavg(indxHbbl)
#  endif
     &                                        .or. wrtavg(indxBostr)
     &                                        .or. wrtavg(indxWstr)
     &                                        .or. wrtavg(indxUWstr)
     &                                        .or. wrtavg(indxVWstr)
     &                                        .or. wrtavg(indxShflx)
#  ifdef SALINITY
     &                                        .or. wrtavg(indxSwflx)
#  endif
     &                                        .or. wrtavg(indxShflx_rsw)
     &     
#  ifdef BULK_FLUX
     &                                        .or. wrtavg(indxShflx_rlw)
     &                                        .or. wrtavg(indxShflx_lat)
     &                                        .or. wrtavg(indxShflx_sen)
#  endif 
#  if defined BIOLOGY && !defined PISCES
     &                                        .or. wrtavg(indxHel)
#   ifdef BIO_NChlPZD
     &                                        .or. wrtavg(indxChC)
#    ifdef OXYGEN
     &                                        .or. wrtavg(indxU10)
     &                                        .or. wrtavg(indxKvO2)
     &                                        .or. wrtavg(indxO2sat)
#    endif 
#   elif defined BIO_BioEBUS
     &                                     .or. wrtavg(indxAOU)
     &                                     .or. wrtavg(indxWIND10)
#   endif
#  endif  /* BIOLOGY */
     &     ) wrtavg(indxTime)=.true.



        MPI_master_only write(stdout,'(8(/6x,l1,2x,A,1x,A))')
     &    wrtavg(indxR),    'write RHO  ', 'Density anomaly'
     &  , wrtavg(indxO),    'write Omega', 'Omega vertical velocity.'
     &  , wrtavg(indxW),    'write W    ', 'True vertical velocity.'
     &  , wrtavg(indxAkv),  'write Akv  ', 'Vertical viscosity'
     &  , wrtavg(indxAkt),  'write Akt  ',
     &                      'Vertical diffusivity for temperature.'
#  ifdef SALINITY
     &  , wrtavg(indxAks),  'write Aks  ',
     &                         'Vertical diffusivity for salinity.'
#  endif
#  ifdef VIS_COEF_3D
     &  , wrtavg(indxVisc),'write visc3d', 'Horizontal viscosity'
#  endif
#  ifdef DIF_COEF_3D
     &  , wrtavg(indxDiff),'write diff3d', 'Horizontal diffusivity'
#  endif
#  ifdef LMD_SKPP
     &  , wrtavg(indxHbl),  'write Hbl  ',
     &                          'Depth of KPP-model boundary layer'
#  endif
#  ifdef LMD_BKPP
     &  , wrtavg(indxHbbl),  'write Hbbl  ',
     &                    'Depth of the bottom planetary boundary layer'
#  endif
#  ifdef BULK_FLUX
     &  , wrtavg(indxShflx_rlw), 'write shflx_rlw [W/m2]', 
     &                                 'Long Wave heat flux.'
     &  , wrtavg(indxShflx_lat), 'write shflx_lat[W/m2] ',
     &                                 'Latente heat flux.'
     &  , wrtavg(indxShflx_sen), 'write shflx_sen [W/m2]',
     &                                 'Sensible heat flux.'
#  endif
# ifdef MOVING_BATHY
     &  , wrtavg(indxHm), 'write Hm [m]',
     &                                 'Time evolving bathymetry.'
# endif
#  if defined BIOLOGY && !defined PISCES
     &  , wrtavg(indxHel),'write Hel  ',
     &                          'Depth of the euphotic layer'
#   ifdef BIO_NChlPZD
     &  , wrtavg(indxChC),'write ChC  ',
     &                          'Chlorophyll to Carbon ratio'
#    ifdef OXYGEN
     &  , wrtavg(indxU10),'write u10 ',
     &         'Wind speed at 10 m height'
     &  , wrtavg(indxKvO2),'write Kv_O2 ',
     &         'Gas transfer coefficient for O2'
     &  , wrtavg(indxO2sat),'write O2sat ',
     &         'Saturation concentration of O2'
#    endif

#   elif defined BIO_BioEBUS
     &  , wrtavg(indxAOU), 'write AOU '
     &  ,                      'Apparent Oxygen Utilization.'
     &  , wrtavg(indxWIND10), 'write wind10 ', 'wind speed at 10 m.'
#   endif
#  endif   /* BIOLOGY */
     &  , wrtavg(indxBostr),'write Bostr', 'Bottom Stress.'
     &  , wrtavg(indxWstr), 'write Wstr', 'Wind Stress.'
     &  , wrtavg(indxUWstr),'write U-Wstress comp.', 'U-Wind Stress.'
     &  , wrtavg(indxVWstr),'write V-Wstress comp.', 'V-Wind Stress.'
     &  , wrtavg(indxShflx),'write Shflx [W/m2]', 
     &                      'Surface net heat flux.'
#  ifdef SALINITY
     &  , wrtavg(indxSwflx),'write Swflx [cm/day]', 
     &                      'Surface freshwater flux (E-P)'
#  endif
     &  , wrtavg(indxShflx_rsw),'write Shflx_rsw [W/m2]', 
     &                      'Short-wave surface radiation.'

!
! Switches for GLS fields to be saved into history file.
!
#  ifdef GLS_MIXING
      elseif (keyword(1:kwlen).eq.'gls_averages') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) wrtavg(indxAkk),  wrtavg(indxAkp)
     &                    ,  wrtavg(indxTke),  wrtavg(indxGls)
     &                    ,  wrtavg(indxLsc)
        if ( wrtavg(indxAkk) .or. wrtavg(indxAkp) .or. wrtavg(indxTke)
     &                       .or. wrtavg(indxGls) .or. wrtavg(indxLsc)
     &     ) wrtavg(indxTime)=.true.

        MPI_master_only write(stdout,'(/1x,A,5(/6x,l1,2x,A,1x,A))')
     &    'Fields to be saved in average file: (T/F)'
     &   , wrtavg(indxAkk), 'write Akk ', 'vertical diffusion for TKE.'
     &   , wrtavg(indxAkp), 'write Akp ', 'vertical diffusion for GLS.'
     &   , wrtavg(indxTke), 'write TKE ', 'turbulent kinetic energy.  '
     &   , wrtavg(indxGls), 'write GLS ', 'generic length scale.'
     &   , wrtavg(indxLsc), 'write Lscale ', 
     &                                  'vertical mixing length scale.'
#  endif

# endif /* SOLVE3D */
#endif /* AVERAGES */


#ifdef FLOATS
!
! Switches for fields to be saved into floats output file.
!
      elseif (keyword(1:kwlen).eq.'float_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
#ifdef AGRIF
        if (Agrif_Root()) then
#endif
          read(input,*,err=95) wrtflt(indxfltGrd), 
     &       wrtflt(indxfltTemp), wrtflt(indxfltSalt), 
     &       wrtflt(indxfltRho), wrtflt(indxfltVel)
           MPI_master_only write(stdout,'(/1x,A,5(/6x,l1,2x,A))')
     &      'Fields to be saved in floats output file: (T/F)'
     &     , wrtflt(indxfltGrd),   'write Grid location variables'
     &     , wrtflt(indxfltTemp),  'write temperature.'
     &     , wrtflt(indxfltSalt),  'write salinity.'
     &     , wrtflt(indxfltRho),   'write density.'
     &     , wrtflt(indxfltVel),   'write mean float velocity' 
#ifdef AGRIF
        endif
#endif
#endif /* FLOATS */

#if defined DIAGNOSTICS_TS
!
! Switches for fields to be saved into tracer diagnostics file.
!
      elseif (keyword(1:kwlen).eq.'diag3D_history_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95)  (wrtdia3D(itrc), itrc=1, NT)
        do itrc=1,NT
          if (wrtdia3D(itrc)) wrtdia3D(NT+1)=.true.
          MPI_master_only write(stdout, '(6x,L1,2x,A,2x,A,I2,A,I4)')
     &        wrtdia3D(itrc), 
     &        'write Tracer equation terms ',
     &        ' Tracer of index', itrc,'/',NT
        enddo
# ifdef DIAGNOSTICS_TS_MLD
      elseif (keyword(1:kwlen).eq.'diag2D_history_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95)  (wrtdia2D(itrc), itrc=1, NT)
        do itrc=1,NT
          if (wrtdia2D(itrc)) wrtdia2D(NT+1)=.true.
          MPI_master_only write(stdout, '(6x,L1,2x,A,2x,A,I2,A,I4)')
     &        wrtdia2D(itrc), 
     &        'write Hbl Integrated Tracer equation terms ',
     &        ' Tracer of index', itrc,'/',NT
        enddo
# endif  /* DIAGNOSTICS_TS_MLD */

# ifdef AVERAGES
!
! Switches for fields to be saved into tracer diagnostics average file.
!
      elseif (keyword(1:kwlen).eq.'diag3D_average_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) (wrtdia3D_avg(itrc),itrc=1,NT)
        do itrc=1,NT
          if (wrtdia3D_avg(itrc)) wrtdia3D_avg(NT+1)=.true.
          MPI_master_only write(stdout, '(6x,L1,2x,A,2x,A,I2,A,I4)')
     &        wrtdia3D_avg(itrc), 
     &        'write Avg Tracer equation terms ',
     &        ' Tracer of index', itrc,'/',NT
        enddo
!
#  ifdef DIAGNOSTICS_TS_MLD
      elseif (keyword(1:kwlen).eq.'diag2D_average_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) (wrtdia2D_avg(itrc),itrc=1,NT)
        do itrc=1,NT
          if (wrtdia2D_avg(itrc)) wrtdia2D_avg(NT+1)=.true.
          MPI_master_only write(stdout, '(6x,L1,2x,A,2x,A,I2,A,I4)')
     &        wrtdia2D_avg(itrc), 
     &        'write Avg Hbl Integrated  Tracer equation terms ',
     &        ' Tracer of index', itrc,'/',NT
        enddo
#  endif  /* DIAGNOSTICS_TS_MLD */
# endif  /* AVERAGES */

#endif  /* DIAGNOSTICS_TS */


#if defined DIAGNOSTICS_UV
!
! Switches for fields to be saved into momentum diagnostics file.
!
!!          wrtdiaM=.true.
      elseif (keyword(1:kwlen).eq.'diagM_history_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95)  (wrtdiaM(itrc), itrc=1,2)
         do itrc=1,2
           if (wrtdiaM(itrc)) wrtdiaM(3)=.true.
           MPI_master_only write(stdout, '(6x,L1,2x,A,2x,A,I2)')
     &         wrtdiaM(itrc), 
     &         'write momentum equation terms ',
     &         ' Momentum of index', itrc
        enddo

# ifdef AVERAGES
!!         wrtdiaM_avg=.true.
      elseif (keyword(1:kwlen).eq.'diagM_average_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) (wrtdiaM_avg(itrc),itrc=1,2)  
        do itrc=1,2
          if (wrtdiaM(itrc)) wrtdiaM_avg(3)=.true.
          MPI_master_only write(stdout, '(6x,L1,2x,A,2x,A,I2)')
     &        wrtdiaM_avg(itrc), 
     &        'write averaged momentum  equation terms ',
     &        ' Momentum of index', itrc
       enddo
# endif /* AVERAGES  */

#endif /*DIAGNOSTICS_UV */

#ifdef DIAGNOSTICS_BIO
!
! Switches for fields to be saved into biology diagnostics file.
!
!          wrtdiabio=.true.
      elseif (keyword(1:kwlen).eq.'diagbioFlux_history_fields') then
!
CR       write(*,*)'NumFluxTerms=',NumFluxTerms
CR        write(*,*)'NumVSinkTerms=',NumVSinkTerms
CR        write(*,*)'NumGasExcTerms=',NumGasExcTerms
!
        call cancel_kwd (keyword(1:kwlen), ierr)
!     ==
        read(input,*,err=95) (wrtdiabioFlux(iflux),iflux=1,NumFluxTerms)
        do iflux=1,NumFluxTerms
          if (wrtdiabioFlux(iflux)) 
     &         wrtdiabioFlux(NumFluxTerms+1)=.true.
          MPI_master_only write(stdout, '(6x,L1,2x,A,2x,A,I2)')
     &         wrtdiabioFlux(iflux), 
     &         'write equation terms ',
     &         'Flux term of index (his)', iflux
        enddo
!     ==
      elseif (keyword(1:kwlen).eq.'diagbioVSink_history_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) (wrtdiabioVSink(iflux),
     &       iflux=1,NumVSinkTerms)
        do iflux=1,NumVSinkTerms
          if (wrtdiabioVSink(iflux)) 
     &         wrtdiabioVSink(NumVSinkTerms+1)=.true.
          MPI_master_only write(stdout, '(6x,L1,2x,A,2x,A,I2)')
     &         wrtdiabioVSink(iflux), 
     &         'write equation terms ',
     &         'VSink term of index (his)', iflux
        enddo
!     == 
      elseif (keyword(1:kwlen).eq.'diagbioGasExc_history_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) (wrtdiabioGasExc(iflux),
     &                                       iflux=1,NumGasExcTerms)
        do iflux=1,NumGasExcTerms
          if (wrtdiabioGasExc(iflux)) 
     &         wrtdiabioGasExc(NumGasExcTerms+1)=.true.
          MPI_master_only write(stdout, '(6x,L1,2x,A,2x,A,I2)')
     &         wrtdiabioGasExc(iflux), 
     &         'write equation terms ',
     &         'GasExc term of index (his)', iflux
        enddo      
! === === ===
# ifdef AVERAGES
!         wrtdiabio_avg=.true.
        write(*,*)'=============='
      elseif (keyword(1:kwlen).eq.'diagbioFlux_average_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
!     ==
        read(input,*,err=95) (wrtdiabioFlux_avg(iflux),
     &       iflux=1,NumFluxTerms)
        do iflux=1,NumFluxTerms
          if (wrtdiabioFlux_avg(iflux)) 
     &         wrtdiabioFlux_avg(NumFluxTerms+1)=.true.
          MPI_master_only write(stdout, '(6x,L1,2x,A,2x,A,I2)')
     &         wrtdiabioFlux_avg(iflux), 
     &         'write equation terms ',
     &         'Flux term of index (avg)', iflux
        enddo
!     ==
      elseif (keyword(1:kwlen).eq.'diagbioVSink_average_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) (wrtdiabioVSink_avg(iflux),
     &                              iflux=1,NumVSinkTerms)
        do iflux=1,NumVSinkTerms
          if (wrtdiabioVSink_avg(iflux)) 
     &         wrtdiabioVSink_avg(NumVSinkTerms+1)=.true.
          MPI_master_only write(stdout, '(6x,L1,2x,A,2x,A,I2)')
     &         wrtdiabioVSink_avg(iflux), 
     &         'write equation terms ',
     &         'VSink term of index (avg)', iflux
        enddo
!     == 
      elseif (keyword(1:kwlen).eq.'diagbioGasExc_average_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)   
        read(input,*,err=95) (wrtdiabioGasExc_avg(iflux),
     &                                  iflux=1,NumGasExcTerms)
        do iflux=1,NumGasExcTerms
          if (wrtdiabioGasExc_avg(iflux)) 
     &         wrtdiabioGasExc_avg(NumGasExcTerms+1)=.true.
          MPI_master_only write(stdout, '(6x,L1,2x,A,2x,A,I2)')
     &         wrtdiabioGasExc_avg(iflux), 
     &         'write equation terms ',
     &         'GasExc term of index (avg)', iflux
        enddo      
# endif  /* AVERAGES */
#endif /* DIAGNOSTICS_BIO */

#ifdef STATIONS
!
! Switches for fields to be saved into stations output file.
!
      elseif (keyword(1:kwlen).eq.'station_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
# ifdef AGRIF
        if (Agrif_Root()) then
# endif
          read(input,*,err=95) wrtsta(indxstaGrd),
     &       wrtsta(indxstaTemp), wrtsta(indxstaSalt),
     &       wrtsta(indxstaRho), wrtsta(indxstaVel)
           MPI_master_only write(stdout,'(/1x,A,5(/6x,l1,2x,A))')
     &      'Fields to be saved in stations output  (T/F)'
     &     , wrtsta(indxstaGrd),   'write Grid location variables'
     &     , wrtsta(indxstaTemp),  'write temperature.'
     &     , wrtsta(indxstaSalt),  'write salinity.'
     &     , wrtsta(indxstaRho),   'write density.'
     &     , wrtsta(indxstaVel),   'write mean station velocity'
# ifdef AGRIF
        endif
# endif
#endif /* STATIONS */

!
! Boussinesq Approximation mean density.
!
      elseif (keyword(1:kwlen).eq.'rho0') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) rho0
        MPI_master_only write(stdout,'(F10.4,2x,A,1x,A)')
     &        rho0, 'rho0     Boussinesq approximation',
     &                           'mean density, kg/m3.'
#if defined UV_VIS2 || defined UV_VIS4
!
! Horizontal viscosity coefficients.
!
      elseif (keyword(1:kwlen).eq.'lateral_visc') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) visc2, visc4
#endif
#if defined UV_VIS2 || defined SPONGE_VIS2
        MPI_master_only write(stdout,9) visc2
   9    format(1pe10.3,2x,'visc2    Horizontal Laplacian ',
     &       'mixing coefficient [m2/s]',/,32x,'for momentum.')
#endif
#ifdef UV_VIS4
        MPI_master_only write(stdout,10) visc4
  10    format(1pe10.3,2x,'visc4    Horizontal biharmonic ',
     &       'mixing coefficient [m4/s]',/,32x,'for momentum.')
#endif
!
! Bottom drag coefficients.
!
      elseif (keyword(1:kwlen).eq.'bottom_drag') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) rdrg, rdrg2, Zob, Cdb_min, Cdb_max
        MPI_master_only write(stdout,'(5(1pe10.3,2x,A/))')
     &     rdrg, 'rdrg     Linear bottom drag coefficient (m/si).',
     &    rdrg2, 'rdrg2    Quadratic bottom drag coefficient.',
     &      Zob, 'Zob      Bottom roughness for logarithmic law (m).',
     &  Cdb_min, 'Cdb_min  Minimum bottom drag coefficient.',
     &  Cdb_max, 'Cdb_max  Maximum bottom drag coefficient.'
!
! Lateral boundary slipperness.
!
      elseif (keyword(1:kwlen).eq.'gamma2') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) gamma2
        MPI_master_only write(stdout,'(f10.2,2x,A,1x,A)')
     &     gamma2, 'gamma2   Slipperiness parameter:',
     &                     'free-slip +1, or no-slip -1.'
#ifdef SOLVE3D
# if defined TS_DIF2 || defined SPONGE_DIF2
!
! Horizontal Laplacian mixing coefficients for tracers.
!
      elseif (keyword(1:kwlen).eq.'tracer_diff2') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) (tnu2(itrc),itrc=1,NT)
        do itrc=1,NT
          MPI_master_only write(stdout,7) tnu2(itrc), itrc, itrc
   7      format(1pe10.3,'  tnu2(',i2,')  Horizontal Laplacian '
     &     ,'mixing coefficient (m2/s)',/,32x,'for tracer ',i2,'.')
        enddo
# endif
# ifdef TS_DIF4
!
! Horizontal biharmonic mixing coefficients for tracer.
!
      elseif (keyword(1:kwlen).eq.'tracer_diff4') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) (tnu4(itrc),itrc=1,NT)
        do itrc=1,NT
          MPI_master_only write(stdout,8) tnu4(itrc), itrc, itrc
   8      format(1pe10.3,'  tnu4(',i2,')  Horizontal biharmonic'
     &    ,' mixing coefficient [m4/s]',/,32x,'for tracer ',i2,'.')
        enddo

# endif
# if !defined LMD_MIXING && !defined BVF_MIXING && !defined GLS_MIXING
!
! Background vertical viscosity and mixing coefficients for tracers.
!
      elseif (keyword(1:kwlen).eq.'vertical_mixing') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) Akv_bak,(Akt_bak(itrc),itrc=1,NT)
        MPI_master_only write(stdout,'(1pe10.3,2x,A,1x,A)')
     &      Akv_bak, 'Akv_bak    Background vertical viscosity',
     &                                     'coefficient, m2/s.'
        do itrc=1,NT
          MPI_master_only write(stdout,
     &           '(1pe10.3,2x,A,I2,A,1x,A/32x,A,I2,A)')
     &            Akt_bak(itrc), 'Akt_bak(', itrc, ')',
     &           'Background vertical mixing coefficient, m2/s,',
     &                                  'for tracer ', itrc, '.' 
        enddo
# endif
# ifdef BODYFORCE
      elseif (keyword(1:kwlen).eq.'bodyforce') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) levsfrc,levbfrc
        if (levsfrc.lt.1 .or. levsfrc.gt.N) then
          MPI_master_only write(stdout,19) 'LEVSFRC = ',levsfrc
  19      format(' READ_INP - Illegal bodyforce level, ',A,i4)
          ierr=ierr+1
        endif
        if (levbfrc.lt.1 .or. levbfrc.gt.N) then
          MPI_master_only write(stdout,19) 'LEVBFRC = ',levbfrc
          ierr=ierr+1
        endif
        MPI_master_only write(stdout,20) levsfrc, levbfrc
  20    format(4x,i6,2x,'levsfrc     ',
     &           'Deepest level to apply surface stress as a ',
     &           'bodyforce.',/,
     &         4x,i6,2x,'levbfrc     ',
     &           'Shallowest level to apply bottom stress as a ',
     &           'bodyforce.')
# endif
#endif
#if  (defined SPONGE && !defined SPONGE_GRID) 
!
! Parameters for sponge layers
!
!  if SPONGE_GRID is defined, they are set generically
!  in set_nudgcof routine
!
      elseif (keyword(1:kwlen).eq.'sponge') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) x_sponge, v_sponge
        MPI_master_only write(stdout,'(1pe10.2,2x,A,1x,A)')
     &     x_sponge,'x_sponge Thickness of sponge',
     &     'and/or nudging layer (m)'
        MPI_master_only write(stdout,'(f10.2,2x,A)')
     &     v_sponge,'v_sponge Viscosity in sponge layer (m2/s)'
!
#elif  (defined SPONGE && defined SPONGE_GRID) 
!
!  if SPONGE_GRID is defined, they are set generically
!  in set_nudgcof routine
!
      elseif (keyword(1:kwlen).eq.'sponge') then
         call cancel_kwd (keyword(1:kwlen), ierr)
         MPI_master_only write(stdout,'(/,1x,A,/,25x,A,/)')
     &   'SPONGE_GRID is defined: parameters for sponge layers',
     &   'are set generically in set_nudgcof.F routine'
#endif
!
#if  defined T_FRC_BRY     || defined M2_FRC_BRY    || \
     defined M3_FRC_BRY    || defined Z_FRC_BRY     || \
     defined TCLIMATOLOGY  || defined M2CLIMATOLOGY || \
     defined M3CLIMATOLOGY || defined ZCLIMATOLOGY
!
! Nudging parameters for OBC and nudging layers 
! (converted from [days] to [sec^-1]
!
      elseif (keyword(1:kwlen).eq.'nudg_cof') then
        call cancel_kwd (keyword(1:kwlen), ierr)
# if  defined AGRIF && !defined AGRIF_OBC_M2ORLANSKI && \
     !defined AGRIF_OBC_M3ORLANSKI && !defined AGRIF_OBC_TORLANSKI
        if (Agrif_Root()) then
# endif
          read(input,*,err=95) tauT_in,tauT_out,tauM_in,tauM_out
          tauT_in =1./(tauT_in *86400.)
          tauT_out=1./(tauT_out*86400.)
          tauM_in =1./(tauM_in *86400.)
          tauM_out=1./(tauM_out*86400.)
          MPI_master_only write(stdout,'(1pe10.3,2x,A)') 
     &        tauT_in,'tauT_in  Nudging coefficients [sec^-1]'
          MPI_master_only write(stdout,'(1pe10.3,2x,A)') 
     &       tauT_out,'tauT_out Nudging coefficients [sec^-1]'
          MPI_master_only write(stdout,'(1pe10.3,2x,A)') 
     &        tauM_in,'tauM_in  Nudging coefficients [sec^-1]'
          MPI_master_only write(stdout,'(1pe10.3,2x,A/)') 
     &       tauM_out,'tauM_out Nudging coefficients [sec^-1]'
# if defined AGRIF && !defined AGRIF_OBC_M2ORLANSKI && \
     !defined AGRIF_OBC_M3ORLANSKI && !defined AGRIF_OBC_TORLANSKI
        endif
# endif
#endif
#ifdef SOLVE3D
# ifndef NONLIN_EOS
!
! Parameters for linear equations of state.
!
      elseif (keyword(1:kwlen).eq.'lin_EOS_cff') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) R0, T0, S0, Tcoef, Scoef
        MPI_master_only write(stdout,'(5(f10.4,2x,A,1x,A/))')
     &       T0, 'T0       Background value for potential',
     &                                     'temperature (Celsius).',
     &       S0, 'S0       Background salinity (PSU),', 'constant.',
     &       R0, 'R0       Background density (kg/m3) used in',
     &                                                'linear EOS.',
     &    Tcoef, 'Tcoef    Thermal expansion coefficient',
     &                                           '(kg/m3/Celsius).',
     &    Scoef, 'Scoef    Saline contraction coefficient',
     &                                                '(kg/m3/PSU).'
# endif
# ifdef SEDIMENT
!
! Sediments input file name.
!
      elseif (keyword(1:kwlen).eq.'sediments') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,'(A)',err=95) sedname
        lstr=lenstr(sedname)
    
        MPI_master_only write(stdout,
     &              '(/9x,A,2x,A)')
     &        'Sediment input file:',sedname

      elseif (keyword(1:kwlen).eq.'sediment_history_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) 
     &                   (wrthis(itrc), itrc=indxSed,indxSed+NST+1
#  ifdef SUSPLOAD
     &                                       +2*NST
#  endif
#  ifdef BEDLOAD
     &                                       +2*NST 
#  endif     
     &  ) 
#  ifdef AVERAGES
        do itrc=indxSed,indxSed+NST+1
#  ifdef SUSPLOAD
     &                                       +2*NST
#  endif
#  ifdef BEDLOAD
     &                                       +2*NST 
#  endif     
          wrtavg(itrc)=wrthis(itrc)
        enddo
#  endif
        MPI_master_only write(stdout,'(2(/6x,L1,2x,A,1x,A))')
     &    wrthis(indxBTHK), 'write bed_thick ', 
     &                            'thickness of sediment bed layer.'
     &  , wrthis(indxBPOR), 'write bed_poros ', 
     &                             'porosity of sediment bed layer.'
        do itrc=1,NST
            MPI_master_only write(stdout, '(6x,L1,2x,A,I2,A,I2,A)')
     &          wrthis(indxBFRA(itrc)), 'write bed_frac(',itrc,
     &                   ') Sediment fraction of index ',itrc,'.'
        enddo
#  ifdef SUSPLOAD
        do itrc=1,NST
            MPI_master_only write(stdout, '(6x,L1,2x,A,I2,A,I2,A)')
     &          wrthis(indxDFLX(itrc)), 'write erosion flux(',itrc,
     &                   ') for sediment of index ',itrc,'.'
        enddo
        do itrc=1,NST
            MPI_master_only write(stdout, '(6x,L1,2x,A,I2,A,I2,A)')
     &          wrthis(indxEFLX(itrc)), 'write deposition flux(',itrc,
     &                   ') for sediment of index ',itrc,'.'
        enddo
#  endif
#  ifdef BEDLOAD
        do itrc=1,NST
            MPI_master_only write(stdout, '(6x,L1,2x,A,I2,A,I2,A)')
     &          wrthis(indxBDLU(itrc)), 'write xi-bedload flux(',itrc,
     &                   ') for sediment of index ',itrc,'.'
        enddo
        do itrc=1,NST
            MPI_master_only write(stdout, '(6x,L1,2x,A,I2,A,I2,A)')
     &          wrthis(indxBDLU(itrc)), 'write eta-bedload flux(',itrc,
     &                   ') for sediment of index ',itrc,'.'
        enddo
#  endif
# endif /* SEDIMENT */
#endif
#ifdef BBL
      elseif (keyword(1:kwlen).eq.'bbl_history_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) 
     &                    (wrthis(itrc), itrc=indxBBL,indxBBL+5) 

        MPI_master_only write(stdout,'(6(/6x,L1,2x,A,1x,A))')
     &    wrthis(indxAbed), 'write Abed ', 
     &                            'bed wave excursion amplitude.'
     &  , wrthis(indxHrip), 'write Hripple ', 
     &                                       'Bed ripple height.'
     &  , wrthis(indxLrip), 'write Lripple ', 
     &                                       'Bed ripple length.'
     &  , wrthis(indxZbnot), 'write Zbnot ', 
     &                              'Physical bottom roughness.'
     &  , wrthis(indxZbapp), 'write Zbapp ', 
     &                              'Apparent bottom roughness.'
     &  , wrthis(indxBostrw), 'write Bostrw ', 
     &                              'Wave-induced bottom stress.'

#endif /* BBL */
#ifdef MRL_WCI
!
! MRL wave-current interaction variables history fields
!
        elseif (keyword(1:kwlen).eq.'wci_history_fields') then
          call cancel_kwd (keyword(1:kwlen), ierr)
          read(input,*,err=95) wrthis(indxSUP),  wrthis(indxUST2D)
     &                                        ,  wrthis(indxVST2D)
# ifdef SOLVE3D
     &         , wrthis(indxUST), wrthis(indxVST), wrthis(indxwST)
     &         , wrthis(indxAKB), wrthis(indxAKW), wrthis(indxKVF)
     &                        , wrthis(indxCALP), wrthis(indxKAPS)
# endif
        MPI_master_only write(stdout,'(/1x,A,3(/6x,l1,2x,A))')
     &    'Fields to be saved in MRL-WCI 2D history  (T/F)',
     &    wrthis(indxSUP),
     &    'write SUP    quasi-static sea-level [m]',
     &    wrthis(indxUST2D),
     &    'write UST2D  depth-averaged  XI-Stokes velocity [m/s]',
     &    wrthis(indxVST2D),
     &    'write VST2D  depth-averaged ETA-Stokes velocity [m/s]'
# ifdef SOLVE3D
        MPI_master_only write(stdout,'(/1x,A,8(/6x,l1,2x,A))')
     &    'Fields to be saved in MRL-WCI 3D history  (T/F)',
     &    wrthis(indxUST),
     &    'write UST  3D  XI-Stokes velocity [m/s]',
     &    wrthis(indxVST),
     &    'write VST  3D ETA-Stokes velocity [m/s]',
     &    wrthis(indxWST),
     &    'write WST  3D vertical Stokes velocity [m/s]',
     &    wrthis(indxAkb),
     &    'write Akb  eddy viscosity due to wave breaking [m2/s]',
     &    wrthis(indxAkw),
     &    'write Akw  eddy diffusivity due to primary waves [m2/s]',
     &    wrthis(indxKVF),
     &    'write KVF  vertical vortex force [m/s2]',
     &    wrthis(indxCALP),
     &    'write CALP surface pressure correction [m]',
     &    wrthis(indxKAPS),
     &    'write KAPS surface Bernoulli head [m]'
# endif
# ifdef AVERAGES
!
! MRL_WCI average field follows its history field.
!
          wrtavg(indxSUP)=wrthis(indxSUP)
          wrtavg(indxUST2D)=wrthis(indxUST2D)
          wrtavg(indxVST2D)=wrthis(indxVST2D)
# ifdef SOLVE3D
          wrtavg(indxUST)=wrthis(indxUST)
          wrtavg(indxVST)=wrthis(indxVST)
          wrtavg(indxWST)=wrthis(indxWST)
          wrtavg(indxAkb)=wrthis(indxAkb)
          wrtavg(indxAkw)=wrthis(indxAkw)
          wrtavg(indxKVF)=wrthis(indxKVF)
          wrtavg(indxCALP)=wrthis(indxCALP)
          wrtavg(indxKAPS)=wrthis(indxKAPS)
# endif
# endif /* AVERAGES */
#endif /* MRL_WCI */

#ifdef OW_COUPLING
!
! coupled wave history fields
!
      elseif (keyword(1:kwlen).eq.'wkb_history_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) (wrthis(i), i=indxHRM,indxHRM+8)
        MPI_master_only write(stdout,'(/1x,A,9(/6x,l1,2x,A))')
     &    'Fields to be saved in wave history  (T/F)',
     &    wrthis(indxHRM), 'write WHRM  RMS wave height (m)',
     &    wrthis(indxFRQ), 'write WFRQ  intrinsic frequency (rad/s)',
     &    wrthis(indxWAC), 'write WAC  primary wave action (m3/s)',
     &    wrthis(indxWKX), 'write WDRX  cosine of wave direction',
     &    wrthis(indxWKE), 'write WDRE  sine of wave direction',
     &    wrthis(indxEPB), 'write WDSP  breaking dissipation (m3/s3)',
     &    wrthis(indxEPD), 'write WDRG  frictional dissipation (m3/s3)',
     &    wrthis(indxWAR), 'write WAR  roller wave action (m3/s)',
     &    wrthis(indxEPR),
     &              'write RDSP  roller dissipation (m3/s3): [0,1]'

#endif /* OW_COUPLING */
 
#ifdef WKB_WWAVE
!
! WKB wave history fields
!
      elseif (keyword(1:kwlen).eq.'wkb_history_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) (wrthis(i), i=indxHRM,indxHRM+8)
        MPI_master_only write(stdout,'(/1x,A,9(/6x,l1,2x,A))')
     &    'Fields to be saved in WKB wave history  (T/F)',
     &    wrthis(indxHRM), 'write HRM  wave height (m)',
     &    wrthis(indxFRQ), 'write FRQ  intrinsic frequency (rad/s)',
     &    wrthis(indxWAC), 'write WAC  primary wave action (m3/s)',
     &    wrthis(indxWKX), 'write WKX  wavenumber  xi-dir component',
     &    wrthis(indxWKE), 'write WKE  wavenumber eta-dir component',
     &    wrthis(indxEPB), 'write EPB  breaking dissipation (m3/s3)',
     &    wrthis(indxEPD), 'write EPD  frictional dissipation (m3/s3)',
     &    wrthis(indxWAR), 'write WAR  roller wave action (m3/s)',
     &    wrthis(indxEPR), 
     &              'write EPR  roller dissipation (m3/s3): [0,1]'
# ifdef AVERAGES
!
! WKB wave average field follows its history field.
!
#  ifndef WAVE_ROLLER
        do i=1,7
#  else
        do i=1,9
#  endif
          wrtavg(indxHRM+i-1)=wrthis(indxHRM+i-1)
        enddo
# endif
!
! WKB primary waves and empirical breaking model parameters
!
      elseif (keyword(1:kwlen).eq.'wkb_wwave') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) wkb_amp,wkb_ang,wkb_prd,wkb_tide,wkb_btg,
     &                       wkb_gam
        MPI_master_only write(stdout,
     &             '(/1x,A,4(/3x,f10.4,2x,A),/3x,A,2(/3x,f10.4,2x,A))')
     &    'Primary waves and breaking parameters for WKB wave model',
     &   wkb_amp,   'wkb_amp    offshore wave amplitude [m]',
     &   wkb_ang,   'wkb_ang    offshore wave angle [deg]',
     &   wkb_prd,   'wkb_prd    offshore wave period [s]',
     &   wkb_tide,  'wkb_tide   constant offshore water level [m]',
#  ifdef WAVE_BREAK_TG86
     &  'breaking model (Thornton & Guza, 1983a) is specifield.',
#  elif defined WAVE_BREAK_TG86A
     &  'breaking model (Thornton & Guza, 1983b) is specifield.',
#  elif defined WAVE_BREAK_CT93
     &  'breaking model (Church & Thornton, 1993) is specifield.',
#  else
     &  'breaking model (Church & Thornton, 1993) is default.',
#  endif
     &   wkb_btg,   'wkb_btg    B parameter',
     &   wkb_gam,   'wkb_gam    gamma paramemer (Hrms/h ratio)'

# ifdef WAVE_ROLLER
!
! Svendsen's surface roller model parameters
!
      elseif (keyword(1:kwlen).eq.'wkb_roller') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) wkb_rsb, wkb_roller
        MPI_master_only write(stdout,'(/1x,A,2(/3x,f10.4,2x,A))')
     &  'Svenden (1984) surface roller model parameters.',
     &   wkb_rsb,   'wkb_rsb     sin(beta) roller dissipation',
     &   wkb_roller,'wkb_roller  breaking contrib to roller: [0,1]'
# endif
#endif  /* WKB_WWAVE */

#ifdef ANA_PSOURCE
!
! Set-up point Sources/Sink number (Nsrc), direction (Dsrc), I- and
! J-grid locations (Isrc,Jsrc), and logical switch for type of tracer
! to apply (Lsrc). Currently, the direction can be along XI-direction
! (Dsrc = 0) or along ETA-direction (Dsrc > 0).  The mass sources are
! located at U- or V-points so the grid locations should range from
! 1 =< Isrc =< L  and  1 =< Jsrc =< M.
!
# ifdef PSOURCE_NCFILE
      elseif (keyword(1:kwlen).eq.'psource_ncfile') then
        call cancel_kwd (keyword(1:kwlen), ierr)
! river runoff file name. Check its availability.
        read(input,'(A)',err=95) fname
        lstr=lenstr(fname)
        open (testunit, file=fname(1:lstr), status='old', err=97)
        close(testunit)
        qbarname=fname(1:lstr)
        MPI_master_only write(stdout,'(2x,A,2x,A)')
     &             'Runoff Data File:', qbarname(1:lstr)
        read(input,*,err=95) Nsrc
        MPI_master_only write(stdout,'(/6x,i6,2x,A,1x,A)')
     &                               Nsrc, 'Number of point sources'
        do is=1,Nsrc
#  ifdef PSOURCE_NCFILE_TS
          read(input,*,err=95) Isrc(is),Jsrc(is),Dsrc(is),qbardir(is),
     &                                    (Lsrc(is,itrc), itrc=1,30)
#  else
          read(input,*,err=95) Isrc(is),Jsrc(is),Dsrc(is),qbardir(is),
     &                                     (Lsrc(is,itrc), itrc=1,30),
     &                                     (Tsrc0(is,itrc), itrc=1,NT)
#  endif /* PSOURCE_NCFILE_TS  */ 

        MPI_master_only write(stdout,'(3(/6x,i6,2x,A),(/6x,F6.0,2x,A))')
     &    Isrc(is), 'I point source indice'
     &  , Jsrc(is), 'J point source indice'
     &  , Dsrc(is), 'Orientation of point source flow'
!         MPI_master_only write(stdout,'(/6x,F6.0,2x,A)')
     &  , qbardir(is), 'Direction of point source flow'
#  ifdef MPI
        if (iminmpi.LE.Isrc(is) .AND. Isrc(is).LE.imaxmpi .AND.
     &       jminmpi.LE.Jsrc(is) .AND. Jsrc(is).LE.jmaxmpi) then
          Isrc_mpi(is,mynode)=Isrc(is)-iminmpi+1
          Jsrc_mpi(is,mynode)=Jsrc(is)-jminmpi+1
        else
          Isrc_mpi(is,mynode)=-1
          Jsrc_mpi(is,mynode)=-1
        endif
#  endif
        do itrc=1,NT
          MPI_master_only write(stdout,
     &         '(6x,L1,2x,A,I2,A,2x,A,I2,A)')
     &         Lsrc(is,itrc), 'write Lsrc(',
     &         itrc,')', 'Tracer of index', itrc,'.'
        enddo
#  ifndef PSOURCE_NCFILE_TS
        do itrc=1,NT
          MPI_master_only write(stdout,
     &         '(6x,1pe10.3,2x,A,I2,A,2x,A,I2,A)')
     &         Tsrc0(is,itrc), 'write Tsrc(',
     &         itrc,')', 'Tracer of index', itrc,'.'
        enddo
#  endif
      enddo
# else   
!<- PSOURCE_NCFILE 
      elseif (keyword(1:kwlen).eq.'psource') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        
        read(input,*,err=95) Nsrc
        MPI_master_only write(stdout,'(/6x,i6,2x,A,1x,A)')
     &                               Nsrc, 'Number of point sources'
        do is=1,Nsrc
          read(input,*,err=95) Isrc(is), Jsrc(is), Dsrc(is), Qbar(is),
     &                                     (Lsrc(is,itrc), itrc=1,NT),
     &                                    (Tsrc0(is,itrc), itrc=1,NT)
          MPI_master_only write(stdout,'(3(/6x,i6,2x,A))')
     &    Isrc(is), 'I point source indice'
     &  , Jsrc(is), 'J point source indice'
     &  , Dsrc(is), 'Direction of point source flow'
          MPI_master_only write(stdout,'(1pe10.3,2x,A)')
     &    Qbar(is), 'Total transport at point source'
#  ifdef MPI
          write(*,*)'Isrc(is)=',Isrc(is)
          write(*,*)'Jsrc(is)=',Jsrc(is)
          if (iminmpi.LE.Isrc(is) .AND. Isrc(is).LE.imaxmpi .AND.
     &        jminmpi.LE.Jsrc(is) .AND. Jsrc(is).LE.jmaxmpi) then
           Isrc_mpi(is,mynode)=Isrc(is)-iminmpi+1
           Jsrc_mpi(is,mynode)=Jsrc(is)-jminmpi+1
          else
           Isrc_mpi(is,mynode)=-1
           Jsrc_mpi(is,mynode)=-1
          endif
#  endif
          do itrc=1,NT
            MPI_master_only write(stdout,
     &                     '(6x,L1,2x,A,I2,A,2x,A,I2,A)')
     &                      Lsrc(is,itrc), 'write Lsrc(',
     &                      itrc,')', 'Tracer of index', itrc,'.'
          enddo
          do itrc=1,NT
            MPI_master_only write(stdout,
     &                     '(6x,1pe10.3,2x,A,I2,A,2x,A,I2,A)')
     &                      Tsrc0(is,itrc), 'write Tsrc(',
     &                      itrc,')', 'Tracer of index', itrc,'.'
          enddo
        enddo
# endif   /*  define PSOURCE_NCFILE  */
#endif /* ANA_PSOURCE */
!
! Online forcing with CFSR
!
#ifdef ONLINE   /* JG Online */
      elseif (keyword(1:kwlen).eq.'online') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) yearnum, monthnum, recordsperday, yearend, 
     &                       monthend
        read(input,'(A)',err=95)  fname 
        lstr=lenstr(fname)
        pathbulk=fname(1:lstr)
        MPI_master_only write(stdout,
     &         '(7x,A,1x,I4,A,1x,I2,A)')
     &         'Online forcing: first forcing, year',yearnum,
     &         ', month', monthnum,'.'
        MPI_master_only write(stdout,
     &         '(7x,A,1x,I4,A,1x,I2,A)')
     &         'Online forcing: last forcing year',yearend,
     &         ', month', monthend,'.'
        MPI_master_only write(stdout,
     &         '(7x,A,A,1x,A4,1x,I1,A)')
     &         'Online forcing: datasets in ',fname(1:lstr),
     &         'with',recordsperday, ' records per day.'
#endif /* ONLINE */

      else
        MPI_master_only write(stdout,'(/3(1x,A)/)')
     &                  'WARNING: Unrecognized keyword:',
     &                   keyword(1:kwlen),' --> DISREGARDED.'
      endif
      if (keyword(1:kwlen) .eq. end_signal) goto 99
      goto 1
!
! Error while reading input parameters.
!
  95  write(stdout,'(/1x,4A/)') 'READ_INP ERROR while reading block',
     &                    ' with keyword ''', keyword(1:kwlen), '''.' 
      ierr=ierr+1
      goto 99 
  97  write(stdout,'(/1x,4A/)') 'READ_INP ERROR: Cannot find input ',
     &                                'file ''', fname(1:lstr), '''.'
      ierr=ierr+1
  99  close (input) 
!
! Check that all keywords were canceled
! Complain if some of them are left
!
      if (ierr.eq.0) then
        call check_kwds (ierr)
!
! Check CPP-switches for consistency. This operation is split into
! two stages because the first subroutine, "check_switches1", is
! generated by special program cppcheck (file cppcheck.F) by
! examining and documention all available switches in cppdefs.h.
! This subroutine creates log of all switches defined in "cppdefs.h",
! as well as traps multiply defined global configurations (project
! switches, such as REGIONAL, etc).
! The second routine, "check_switches2" is hand written and it
! contains traps for mutually exclussive definition of all other
! CPP-switches (i.e. those which are NOT project selection switches,
! for example, it traps multiply defined vertical mixing schemes or
! lateral boundary conditions).
!
! Both codes are written in transparent mode: they assumed that error
! variable (ierr) is initialized at entry and they add 1 for each
! error discovered. 
!
        call check_srcs
        call check_switches1 (ierr)
        call check_switches2 (ierr)
      endif
      if (ierr.ne.0) then
        write(stdout,'(/1x,2A,I3,1x,A/)') 'READ_INP ERROR: ',
     & 'A total of', ierr, 'configuration errors discovered.'
        return
      endif
#ifdef MPI
!      call MPI_Barrier (MPI_COMM_WORLD, ierr)
#endif
      return
      end

c
