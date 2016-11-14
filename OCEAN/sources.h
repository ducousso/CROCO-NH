! $Id: sources.h 1225 2013-07-11 15:33:27Z gcambon $
#if defined PSOURCE || defined PSOURCE_NCFILE
!
! Nsrc       Number of point Sources/Sinks.
! Dsrc       Direction of point Sources/Sinks:  0 = along XI-;
!                                               1 = along ETA-direction.
! Isrc,Jsrc  i,j-grid location of point Sources/Sinks,
!              0 <= Isrc <= Lm+1;   0 =<- Jsrc <= Mm+1.
! Lsrc       Logical switch indicating which tracer field to apply
!                                         the point Sources/Sinks.
! Qsrc       Mass transport profile (m3/s) of point Sources/Sinks.
! Qbar       Vertically integrated Qsrc (m3/s) of point
! QbarG      Latest two-time snapshots of vertically integrated
!              mass transport (m3/s) of point Sources/Sinks.
! Tsrc       Tracer (tracer units) point Sources/Sinks.
! TsrcG      Latest two-time snapshots of tracer (tracer units)
!              point Sources/Sinks.
! Qshape     Nondimensional shape function to distribute mass
!             ass point Sources/Sinks vertically.
!
      real Qbar(Msrc)
      common /sources_Qbar/ Qbar

      real Qsrc(Msrc,N)
      common /source_Qsrc/ Qsrc

      real Qshape(Msrc,N)
      common /source_Qshape/ Qshape

      real Tsrc(Msrc,N,NT)
      common /source_Tsrc/ Tsrc

      real Tsrc0(Msrc,NT)
      common /source_Tsrc0/ Tsrc0

      real lasrc(Msrc)
      common /source_lasrc/ lasrc

      real losrc(Msrc)
      common /source_losrc/ losrc

      integer Nsrc
      common /source_Nsrc/ Nsrc

      integer Dsrc(Msrc)
      common /source_Dsrc/ Dsrc

      integer Isrc(Msrc)
      common /source_Isrc/ Isrc

      integer Jsrc(Msrc)
      common /source_Jsrc/ Jsrc

      logical Lsrc(Msrc,30)
      common /source_Lsrc/ Lsrc

#ifdef PSOURCE_NCFILE
!
!  qbarg  |  Two-time-level grided data for river runoff [m3/s].
!  tqbar     Time of river runoff data.
!
      real qbarg(Msrc,2)
      common /qbardat_qbarg/qbarg
      real    qbar_time(2)
      real    qbar_cycle
      integer itqbar, qbar_ncycle, qbar_rec,  qbar_tid,  qbar_id
      common /qbardat1/ qbar_time
      common /qbardat2/ qbar_cycle
      common /qbardat3/ itqbar, qbar_ncycle, qbar_rec, qbar_tid, qbar_id

      real qbardir(Msrc)
      common /source_qbardir/ qbardir

# ifdef PSOURCE_NCFILE_TS
      real tsrcg(Msrc,2,NT)
      common /tsrcdat_tsrcg/tsrcg
      real    tsrc_time(2,NT)
      real    tsrc_cycle(NT)
      integer ittsrc(NT), tsrc_ncycle(NT), tsrc_rec(NT),  tsrc_tid(NT),  
     &        tsrc_id(NT)
      common /tsrcdat1/ tsrc_time
      common /tsrcdat2/ tsrc_cycle
      common /tsrcdat3/ ittsrc, tsrc_ncycle, tsrc_rec, tsrc_tid, tsrc_id

      logical got_tsrc(NT)
      common /tsrc_logical/ got_tsrc

# endif
#endif /* PSOURCE_NCFILE */

# ifdef MPI
      integer Isrc_mpi(Msrc,0:NNODES-1)
      common /source_Isrc_mpi/ Isrc_mpi
      integer Jsrc_mpi(Msrc,0:NNODES-1)
      common /source_Jsrc_mpi/ Jsrc_mpi
# endif

#endif

