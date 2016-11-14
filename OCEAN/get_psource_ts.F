!     $Id: get_psource_ts.F 1470 2014-02-10 17:32:26Z gcambon $
!     
!======================================================================
!     CROCO is a branch of ROMS developped at IRD and INRIA, in France
!     The two other branches from UCLA (Shchepetkin et al) 
!     and Rutgers University (Arango et al) are under MIT/X style license.
!     CROCO specific routines (nesting) are under CeCILL-C license.
!     
!     CROCO website : http://www.croco-ocean.org
!======================================================================
!     
#include "cppdefs.h"
#if defined PSOURCE_NCFILE_TS
      subroutine get_psource_ts
!     
!     Read in point or grided sea river runoff tracer 
!     concentration the appropriate time from runoff NetCDF file.
!     
      implicit none
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "netcdf.inc"
# include "ncscrum.h"
# include "sources.h"
      real cff
      integer is,i,ierr, lstr,lvar,lenstr, nf_fread, advance_cycle,
     &     itrc
      integer s(2),c(2)
!     #define CR
!     
!     Initialization: Inquire about the contents of forcing NetCDF file:
!================variables and dimensions. Check for consistency.
!     
      if (may_day_flag.ne.0) return !-->  EXIT
      if (itqbar.eq.0 .or. iic.eq.0) then
        lstr=lenstr(qbarname)
c     *        call opencdf (qbarname,N)
c     *        if (may_day_flag.ne.0) return    !-->  EXIT
!     
!     If not opened yet, open forcing NetCDF file for reading.
!     Find and save IDs for relevant variables
!     
        if (ncidqbar.eq.-1) then
          ierr=nf_open(qbarname(1:lstr), nf_nowrite, ncidqbar)
          if (ierr. ne. nf_noerr) goto 4 !--> ERROR
        endif

        do itrc=1,NT
          got_tsrc(itrc)=.FALSE.
          if (itrc.eq.itemp) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (ncidqbar,'temp_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'temp_src_time', qbarname(1:lstr)
c              goto 99           !--> ERROR
            endif
#ifdef SALINITY
          elseif (itrc.eq.isalt) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (ncidqbar,'salt_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'salt_src_time', qbarname(1:lstr)
c              goto 99           !--> ERROR
            endif
#endif /* SALINITY  */ 
#ifdef BIOLOGY
# if (defined BIO_NChlPZD || defined BIO_N2ChlPZD2)
          elseif (itrc.eq.iNO3_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (ncidqbar,'no3_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'no3_src_time', qbarname(1:lstr)
c     goto 99           !--> ERROR
            endif
!     
          elseif (itrc.eq.iPhy1) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (ncidqbar,'phyto_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'phyto_src_time', qbarname(1:lstr)
c     goto 99           !--> ERROR
            endif
!     
          elseif (itrc.eq.iZoo1) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (ncidqbar,'zoo_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'zoo_src_time', qbarname(1:lstr)
c     goto 99           !--> ERROR
            endif
!     
          elseif (itrc.eq.iChla) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (ncidqbar,'chla_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'chla_src_time', qbarname(1:lstr)
c     goto 99           !--> ERROR            
            endif
!     
#  ifdef OXYGEN
          elseif (itrc.eq.iO2) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (ncidqbar, 'o2_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'o2_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
#  endif
# endif /* BIO_NChlPZD || BIO_N2ChlPZD */
!     
# ifdef BIO_NChlPZD
          elseif (itrc.eq.iDet1) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (ncidqbar,'det_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'det_src_time', qbarname(1:lstr)
c     goto 99           !--> ERROR
            endif
# endif
!     
# ifdef BIO_N2ChlPZD2
          elseif (itrc.eq.iNH4_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'nh4_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'nh4_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iDet1) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'det1_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'det1_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif             
          elseif (itrc.eq.iDet2) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'det2_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'det2_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
# endif
# ifdef BIO_BioEBUS
          elseif (itrc.eq.iNO3_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (ncidqbar,'no3_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'no3_src_time', qbarname(1:lstr)
c     goto 99           !--> ERROR
            endif
          elseif (itrc.eq.iNO2_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (ncidqbar,'no2_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'no2_src_time', qbarname(1:lstr)
c     goto 99           !--> ERROR
            endif
          elseif (itrc.eq.iNH4_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'nh4_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'nh4_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iPhy1) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (ncidqbar,'sphyto_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'sphyto_src_time', qbarname(1:lstr)
c     goto 99           !--> ERROR
            endif
          elseif (itrc.eq.iPhy2) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (ncidqbar,'lphyto_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'lphyto_src_time', qbarname(1:lstr)
c     goto 99           !--> ERROR
            endif
          elseif (itrc.eq.iZoo1) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (ncidqbar,'szoo_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'szoo_src_time', qbarname(1:lstr)
c     goto 99           !--> ERROR
            endif
          elseif (itrc.eq.iZoo2) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (ncidqbar,'lzoo_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'lzoo_src_time', qbarname(1:lstr)
c     goto 99           !--> ERROR
            endif
          elseif (itrc.eq.iDet1) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'det1_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'det1_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif             
          elseif (itrc.eq.iDet2) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'det2_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'det2_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iDON) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'don_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'don_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iO2) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'o2_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'o2_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
#   ifdef NITROUS_OXIDE
          elseif (itrc.eq.iN2O) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'n2o_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'n2o_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
#   endif
# endif /* BIO_BioEBUS */
# ifdef PISCES
          elseif (itrc.eq.iNO3_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (ncidqbar,'no3_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'no3_src_time', qbarname(1:lstr)
c     goto 99           !--> ERROR
            endif
          elseif (itrc.eq.iDIC_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'dic_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'dic_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iTAL_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'talk_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'talk_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iOXY_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'o2_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'o2_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iCAL_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'cal_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'cal_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iPO4_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'po4_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'po4_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iPOC_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'poc_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'poc_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iSIL_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'si_src_time', tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'si_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iPHY_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'phy_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'phy_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iZOO_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'zoo_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'zoo_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iDOC_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'doc_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'doc_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iDIA_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'dia_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'dia_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iMES_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'mes_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'mes_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iBSI_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'bsi_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'bsi_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iFER_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'fer_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'fer_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iBFE_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'bfe_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'bfe_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iGOC_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'goc_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'goc_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iSFE_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'sfe_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'sfe_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iDFE_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'dfe_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'dfe_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iDSI_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'dsi_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'dsi_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iNFE_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'nfe_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'nfe_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iNCH_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'nch_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'nch_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iDCH_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'dch_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'dch_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iNO3_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'no3_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'no3_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iNH4_) then
            got_tsrc(itrc)=.true.
            ierr=nf_inq_varid (tsrc_id, 'nh4_src_time', 
     &           tsrc_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tsrc(itrc)=.false.
              write(stdout,3) 'nh4_src_time', qbarname(1:lstr)
c     goto 99                                     !--> ERROR
            endif
            
#endif   /* PISCES */              
#endif /* BIOLOGY  */
          endif

!     Read in Netcdf file the concentration
C     R          write(*,*)'==============='
C     R          write(*,*)'ITRC=',itrc
C     R          write(*,*)'indxTsrc+itrc-1=',indxTsrc+itrc-1
C     R          write(*,*)'vname(...)=',vname(1,indxTsrc+itrc-1)
          if (got_tsrc(itrc)) then
            lvar=lenstr(vname(1,indxTsrc+itrc-1))
            ierr=nf_inq_varid (ncidqbar,vname(1,indxTsrc+itrc-1)
     &           (1:lvar),tsrc_id(itrc))
            if (ierr .eq. nf_noerr) then
              ierr=nf_inq_varndims (ncidqbar,tsrc_id(itrc), i) 
            endif
            if (ierr .ne. nf_noerr) then
              write(stdout,3) vname(1,indxTsrc+itrc-1)(1:lvar), 
     &             qbarname(1:lstr)
              goto 99           !--> ERROR
            endif
!     
!     
!     Determine whether there is cycling to reuse the input data and
!     find cycling period "qbar_cycle", set initial cycling index
!     "qbar_ncycle" and record index "qbar_rec".
!     Set initial value for time index "itqbar" and both time record
!     bounds to large negative artificial values, so that it will
!     trigger the logic in reading part below.
!     
            call set_cycle (ncidqbar,tsrc_tid(itrc),
     &           nttsrc(itrc),
     &           tsrc_cycle(itrc), tsrc_ncycle(itrc),
     &           tsrc_rec(itrc))
            if (may_day_flag.ne.0) return !-->  EXIT
            ittsrc(itrc)=2
            tsrc_time(1,itrc)=-1.E+20
            tsrc_time(2,itrc)=-1.E+20
          endif                 ! got_tsrc(itrc)
        enddo                   ! itrc
      endif                     ! iic.eq.0
!     
!     Reading data from the runoff file:  Get out, if model time is
!================ === ======= =====  already within the interval
!     set by the past and future data times. Otherwise flip the time
!     index, increment record and cyclin indices and read a new portion
!     of data. Repeat it until model time is between the two times from
!     data.
!     
      do itrc=1,NT
C     R          write(*,*)'==========='
C     R          write(*,*)'itrc=',itrc
C     R          write(*,*)'got_tsrc(itrc)=',got_tsrc(itrc)
C     R          write(*,*)'vname(...)=',vname(1,indxTsrc+itrc-1)
        if (got_tsrc(itrc)) then
 1        i=3-ittsrc(itrc)
          cff=time+0.5*dt
          if (tsrc_time(i,itrc).le.cff .and. 
     &         cff.lt.tsrc_time(ittsrc(itrc),itrc)) goto 10
          ierr=advance_cycle(tsrc_cycle(itrc), nttsrc(itrc), 
     &         tsrc_ncycle(itrc),tsrc_rec(itrc))
          if (ierr .ne. 0) then
            write(stdout,7) tsrc_rec(itrc), nttsrc(itrc), 
     &           qbarname(1:lstr), tdays,
     &           tsrc_time(ittsrc(itrc),itrc)*sec2day 
            goto 99             !--> ERROR
          endif
          
          ierr=nf_get_var1_FTYPE (ncidqbar, tsrc_tid(itrc),
     &         tsrc_rec(itrc),  cff)
          
          if (ierr .ne. nf_noerr) then
            write(stdout,6) 'Xtsrc_time', tsrc_rec(itrc)
            goto 99             !--> ERROR   ??
          endif
          tsrc_time(i,itrc)=cff*day2sec+tsrc_cycle(itrc)*
     &         tsrc_ncycle(itrc)
          if (tsrc_time(ittsrc(itrc),itrc).eq.-1.E+20) then
            tsrc_time(ittsrc(itrc),itrc)=tsrc_time(i,itrc)
          endif
          
          s=(/ tsrc_rec(itrc),1 /)
          c=(/ 1,Nsrc /)
          ierr=nf_get_vara_FTYPE(ncidqbar,tsrc_id(itrc),s,c,
     &         tsrcg(1:Nsrc,i,itrc))
          
          ittsrc(itrc)=i
          MPI_master_only write(stdout,
     &         '(6x,A,2x,I2,1x,A,1x,g12.4,1x,I4)') 
     &         'GET_PSOURCE_TS -- Read conc fields of tracer ',itrc,
     &         'for time =', cff  
#ifdef MPI
     &         , mynode
#endif
          if (nttsrc(itrc).gt.1) goto 1
 10       continue    
        endif                   !got_tsrc(itrc)
      enddo                     !itrc
      return
!     Sort out error messages: The following portion of the code is
!======================not accessed unless something goes wrong.
      
 3    format(/,' GET_PSOURCE_TS - unable to find forcing variable: '
     &     a,/,11x,'in runoff NetCDF file: ',a,          
     &     '--> analytical value (in analytical.F) ')
 4    write(stdout,5) qbarname(1:lstr)   
 5    format(/,' GET_PSOURCE_TS - ERROR: unable ' 
     &     'to open runoff NetCDF file: ',a)
      goto 99
 6    format(/,' GET_PSOURCE_TS - ERROR while reading variable: ',
     &     a,2x,' at TIME index = ',i4)
 7    format(/,' GET_PSOURCE_TS - ERROR: requested time record ',I4,
     &     1x,'exeeds the last available',  /,  11x,'record ',I4,
     &     1x,'in runoff NetCDF file: ', a, /,   11x,'TDAYS = ',
     &     g12.4,2x,'last available QBAR_TIME = ',g12.4)
      
 99   may_day_flag=2
      return
      end
      
      subroutine set_psource_ts_tile(Istr,Iend,Jstr,Jend)
!     
!     Set-up river runoff concentration data for current tile.
!     
      implicit none
      integer itrc,Istr,Iend,Jstr,Jend,i, it1,it2
      real cff, cff1,cff2
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "sources.h"
# include "compute_extended_bounds.h"
!     
C     R      write(*,*)'Enter set_psource_ts_tile...'
      do itrc=1,NT
        if (got_tsrc(itrc)) then
          it1=3-ittsrc(itrc)
          it2=ittsrc(itrc)
          cff=time+0.5*dt
          cff1=tsrc_time(it2,itrc)-cff
          cff2=cff-tsrc_time(it1,itrc)
!     
!     Load time invariant QBAR data.
!     
          if (tsrc_cycle(itrc).lt.0.) then
            if (FIRST_TIME_STEP) then
              do i=1,Nsrc
                Tsrc0(i,itrc)=tsrcg(i,ittsrc(itrc),itrc)
              enddo
            endif
!     
!     Time-interpolate QBAR from point data.
!     Check that for the next time step [when time=time+dt] time+dt 
!     is still between qbar_time(it1) and qbar_time(it2); and if not,
!     set synchro_flag top signal that the new forcing data should be
!     read from the netCDF input file (master thread only).
!     
          elseif (cff1.ge.0. .and. cff2.ge.0.) then
            if (ZEROTH_TILE .and. cff1.lt.dt) synchro_flag=.TRUE.
            cff=1./(cff1+cff2)
            cff1=cff1*cff
            cff2=cff2*cff
            do i=1,Nsrc
              Tsrc0(i,itrc)=cff1*tsrcg(i,it1,itrc)
     &             + cff2*tsrcg(i,it2,itrc)
C     R              write(*,*)'i=',i,' itrc=',itrc,
C     R    &                  'Tsrc0(i,itrc)=',Tsrc0(i,itrc)
            enddo
!     
!     Unable to set-up Runoff Conc.:
!     Complain about the error and signal to quit.      
!     
          elseif (ZEROTH_TILE) then
            write(stdout,1) 'qbar_time', tdays, qbar_time(it2)*sec2day
 1          format(/,' SET_PSOURCE_TS -- ',
     &           ' current model time exceeds ending',
     &           1x,'value for variable: ',a,/,11x,'TDAYS = ',g12.4,
     &           2x,'TEND = ',g12.4)
            may_day_flag=2
          endif
        endif                   !got_tsrc
      enddo                     !itrc
      
      return
      end
#else
      subroutine get_psource_ts_empty
      return
      end
#endif /* PSOURCE_NCFILE_TS */
      
