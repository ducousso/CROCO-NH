! $Id: get_tclima.F 1458 2014-02-03 15:01:25Z gcambon $
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
#if defined TCLIMATOLOGY && !defined ANA_TCLIMA

                             ! Read tracer(s) climatology
      subroutine get_tclima  ! fields from climatology file
                             ! at appropriate time.
# define TCLIMA_DATA
      implicit none
#include "param.h"
#include "scalars.h"
#include "ncscrum.h"
#include "climat.h"
      real cff
      integer i,itrc, lstr,lvar,lenstr, ierr, nf_fread, advance_cycle
#include "netcdf.inc"
!
! Initialization: Inquire about the contents of climatological file:
!================ variables and dimensions. Check for consistency.
!
      if (may_day_flag.ne.0) return      !-->  EXIT
      if (iic.eq.0 ) then
        lstr=lenstr(clmname)
!
! If not opened yet, open climatological file for reading.
! Check for availability of tracer climatology foelds in input
! netCDF file and save their IDs. Signal to terminate, if not found.
!
        if (ncidclm.eq.-1) then
          ierr=nf_open (clmname(1:lstr), nf_nowrite, ncidclm)
          if (ierr .ne. nf_noerr) goto 4                !--> ERROR
        endif

        do itrc=1,NT

          got_tclm(itrc)=.false.

          if (itrc.eq.itemp) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'tclm_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'tclm_time', clmname(1:lstr)
              goto 99                                     !--> ERROR
            endif
# ifdef SALINITY
          elseif (itrc.eq.isalt) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'sclm_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'sclm_time', clmname(1:lstr)
              goto 99                                     !--> ERROR
            endif
# endif
# ifdef BIOLOGY
# ifdef PISCES
          elseif (itrc.eq.iDIC_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'dic_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'dic_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iTAL_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'talk_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'talk_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iOXY_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'o2_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'o2_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iCAL_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'cal_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'cal_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iPO4_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'po4_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'po4_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iPOC_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'poc_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'poc_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iSIL_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'si_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'si_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iPHY_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'phy_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'phy_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iZOO_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'zoo_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'zoo_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iDOC_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'doc_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'doc_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iDIA_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'dia_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'dia_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iMES_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'mes_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'mes_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iBSI_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'bsi_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'bsi_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iFER_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'fer_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'fer_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iBFE_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'bfe_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'bfe_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iGOC_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'goc_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'goc_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iSFE_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'sfe_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'sfe_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iDFE_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'dfe_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'dfe_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iDSI_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'dsi_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'dsi_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iNFE_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'nfe_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'nfe_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iNCH_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'nch_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'nch_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iDCH_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'dch_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'dch_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iNO3_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'no3_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'no3_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iNH4_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'nh4_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'nh4_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif

#  elif defined BIO_NChlPZD

          elseif (itrc.eq.iNO3_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'no3_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'no3_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iPhy1) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'phyto_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'phyto_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iZoo1) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'zoo_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'zoo_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iChla) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'chla_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'chla_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iDet1) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'det_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'det_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
#   ifdef OXYGEN
          elseif (itrc.eq.iO2) then
           got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'o2_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'o2_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
#   endif
#  elif defined BIO_N2ChlPZD2
          elseif (itrc.eq.iNO3_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'no3_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'no3_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iNH4_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'nh4_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'nh4_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iPhy1) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'phyto_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'phyto_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iZoo1) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'zoo_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'zoo_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iChla) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'chla_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'chla_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iDet1) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'sdet_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'sdet_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iDet2) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'ldet_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'ldet_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
#  elif defined BIO_BioEBUS 

          elseif (itrc.eq.iNO3_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'no3_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'no3_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iNO2_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'no2_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'no2_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif	    
          elseif (itrc.eq.iNH4_) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'nh4_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'nh4_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iPhy1) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'sphyto_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'sphyto_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iPhy2) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'lphyto_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'lphyto_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iZoo1) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'szoo_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'szoo_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iZoo2) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'lzoo_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'lzoo_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iDet1) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'sdet_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'sdet_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iDet2) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'ldet_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'ldet_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iDON) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'don_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'don_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
          elseif (itrc.eq.iO2) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'o2_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'o2_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif	    
#    ifdef NITROUS_OXIDE
         elseif (itrc.eq.iN2O) then
            got_tclm(itrc)=.true.
            ierr=nf_inq_varid (ncidclm, 'n2o_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              got_tclm(itrc)=.false.
              write(stdout,3) 'n2o_time', clmname(1:lstr)
c              goto 99                                     !--> ERROR
            endif
#     endif

#  endif
# endif /* BIOLOGY */
          endif

          if (got_tclm(itrc)) then
            lvar=lenstr(vname(1,indxT+itrc-1))
            ierr=nf_inq_varid(ncidclm, vname(1,indxT+itrc-1)(1:lvar),
     &                                                tclm_id(itrc))
            if (ierr .ne. nf_noerr) then
              write(stdout,3) vname(1,indxT+itrc-1)(1:lvar),
     &                                     clmname(1:lstr)
              goto 99                                      !--> ERROR
            endif
!
! Determine whether there is cycling to reuse the input data
! and find cycling period "tclm_cycle", set initial cycling
! index "tclm_ncycle" and record index "tclm_rec".
! Set initial value for time index "itclm" and both time record
! bounds to large negative artificial values, so that it will
! trigger the logic in reading part below.
!
            call set_cycle (ncidclm, tclm_tid(itrc), nttclm(itrc),
     &         tclm_cycle(itrc), tclm_ncycle(itrc), tclm_rec(itrc))

            if (may_day_flag.ne.0) return    !-->  EXIT
            ittclm(itrc)=2
            tclm_time(1,itrc)=-1.E+20
            tclm_time(2,itrc)=-1.E+20
          endif   ! got_tclm(itrc)
        enddo   ! itrc
      endif   ! iic.eq.0
!
! Read in tracer climatology: Check, if if model time is within the 
!===== == ====== ============ bounds set by the past and future data
! times. If not, flip the time index, increment record and cycling
! indices and read a new portion of data: climatology time
! coordinate and tracer climatology field. Check and read again,
! until model time is between the two time bounds.
!
      do itrc=1,NT
       if (got_tclm(itrc)) then
 10     i=3-ittclm(itrc)
         cff=time+0.5*dt
         if (tclm_time(i,itrc).le.cff .and.
     &     cff.lt.tclm_time(ittclm(itrc),itrc)) goto 1
         ierr=advance_cycle (tclm_cycle(itrc),  nttclm(itrc),
     &                       tclm_ncycle(itrc), tclm_rec(itrc))
         if (ierr.ne.0) then
           write(stdout,7) tclm_rec(itrc), nttclm(itrc),
     &                     clmname(1:lstr), tdays,
     &                     tclm_time(ittclm(itrc),itrc)*sec2day
           goto 99                                        !--> ERROR
         endif

         ierr=nf_get_var1_FTYPE(ncidclm, tclm_tid(itrc),
     &                                tclm_rec(itrc), cff)
         if (ierr.ne.NF_NOERR) then
           write(stdout,6) 'Xclm_time', tclm_rec(itrc)
           goto 99                                        !--> ERROR
         endif
         tclm_time(i,itrc)=cff*day2sec+tclm_cycle(itrc)
     &                                *tclm_ncycle(itrc)
         if (tclm_time(ittclm(itrc),itrc).eq.-1.E+20)
     &       tclm_time(ittclm(itrc),itrc)=tclm_time(i,itrc)

         ierr=nf_fread (tclima(START_2D_ARRAY,1,i,itrc),
     &                              ncidclm, tclm_id(itrc),
     &                              tclm_rec(itrc), r3dvar)
         if (ierr.ne.NF_NOERR) then
           lvar=lenstr(vname(1,indxT+itrc-1))
           write(stdout,6) vname(1,indxT+itrc-1)(1:lvar),
     &                                      tclm_rec(itrc)
           goto 99                                        !--> ERROR
         endif
        ittclm(itrc)=i
        MPI_master_only write(stdout,'(6x,A,2x,I2,1x,A,1x,g12.4,1x,I4)')
     &             'GET_TCLIMA -- Read climatology of tracer', itrc,
     &                                             'for time =', cff
#ifdef MPI
     &                                                      , mynode
#endif
        if (nttclm(itrc).gt.1) goto 10
  1     continue    
       endif ! got_tclm(itrc)
      enddo !itrc
      return
!
! Sort out error messages: The following portion of the code is
!===== === ===== ========= not accessed unless something goes wrong.
!
  3   format(/,' GET_TCLIMA - unable to find climatology variable: ',
     &       a,/,15x,'in climatology NetCDF file: ',a)
  4   write(stdout,5) clmname(1:lstr)
  5   format(/,' GET_TCLIMA - unable to open climatology',
     &         1x,'NetCDF file: ',a)
      goto 99
  6   format(/,' GET_TCLIMA - ERROR while reading variable: ',a,2x,
     &       ' at TIME index = ',i4)

  7   format(/,' GET_TCLIMA - ERROR: requested time record ',I4,
     &       1x,'exeeds the last available', /,14x,'record ',I4,
     &       1x,'in climatology file: ',a, /,14x,'TDAYS = ',
     &       g12.4,2x,'last available TCLM_TIME = ',g12.4)

  99  may_day_flag=2
      return
      end

      subroutine set_tclima (tile)
      implicit none
      integer tile
# include "param.h"
#ifdef  ALLOW_SINGLE_BLOCK_MODE
C$    integer  trd, omp_get_thread_num
#endif
# include "compute_tile_bounds.h"
      call set_tclima_tile (Istr,Iend,Jstr,Jend)
      return
      end

      subroutine set_tclima_tile (Istr,Iend,Jstr,Jend)
!
!--------------------------------------------------------------------
!  Set-up tracer climatology for current tile.
!--------------------------------------------------------------------
!
# define TCLIMA_DATA
      implicit none
# include "param.h"
# include "ocean3d.h"
# include "scalars.h"
# include "climat.h"
      integer Istr,Iend,Jstr,Jend, itrc,i,j,k, it1,it2
      real    cff, cff1, cff2
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
      do itrc=1,NT
       if (got_tclm(itrc)) then
        it1=3-ittclm(itrc)
        it2=ittclm(itrc)
        cff=time+0.5*dt
        cff1=tclm_time(it2,itrc)-cff
        cff2=cff-tclm_time(it1,itrc)
        if (ZEROTH_TILE .and. cff1.lt.dt) synchro_flag=.TRUE.

        if (tclm_cycle(itrc).lt.0.) then            ! Load time-
          if (iic.eq.0) then                        ! invariant
            do k=1,N                                ! tracer
              do j=JstrR,JendR                      ! climatology.
                do i=IstrR,IendR
                  tclm(i,j,k,itrc)=tclima(i,j,k,ittclm(itrc),itrc)
                enddo
              enddo
            enddo
          endif
        elseif (cff1.ge.0. .and. cff2.ge.0.) then
          cff=1./(cff1+cff2)                        ! Interpolate
          cff1=cff1*cff                             ! tracer 
          cff2=cff2*cff                             ! climatology 
          do k=1,N                                  ! climatology
            do j=JstrR,JendR                        ! in time.
              do i=IstrR,IendR
                tclm(i,j,k,itrc)=cff1*tclima(i,j,k,it1,itrc)
     &                          +cff2*tclima(i,j,k,it2,itrc)
              enddo
            enddo
          enddo
        elseif (ZEROTH_TILE) then
          write(stdout,'(/1x,2A/3(1x,A,F16.10)/)')
     &            'SET_TCLIMA_TILE - current model time is outside ',
     &            'bounds of ''tclm_time''.', 'TCLM_TSTART=',
     &             tclm_time(it1,itrc)*sec2day,     'TDAYS=',  tdays,
     &            'TCLM_TEND=',   tclm_time(it2,itrc)*sec2day
          may_day_flag=2
        endif
       endif  !got_tclm
      enddo  !itrc

      return
      end
#else
      subroutine get_tclima_empty 
      return
      end
#endif /* TCLIMATOLOGY && !ANA_TCLIMA */
