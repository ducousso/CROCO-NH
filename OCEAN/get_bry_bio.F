! $Id: get_bry_bio.F 1585 2014-07-17 14:42:34Z gcambon $ 
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
#if defined SOLVE3D && defined T_FRC_BRY && defined BIOLOGY 

      subroutine get_bry_bio    ! Read side boundary forcing 
      implicit none             ! fields from boundary file
# include "param.h"
# include "scalars.h"
# include "ncscrum.h"
# include "netcdf.inc"
# include "boundary.h"
      real cff
      integer  lstr,lvar,lvar2,lenstr, ierr, ierr_all, itrc
      integer  nf_read_bry_EW, nf_read_bry_NS 
      character*60 text
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
        do itrc=1,NT
          got_tbry(itrc)=.FALSE.
        enddo
        if (bry_id.eq.-1) then
          ierr=nf_open (bry_file(1:lstr), nf_nowrite, bry_id)
          if (ierr.ne.nf_noerr) write(stdout,'(/1x,4A/)') 'ERROR ',
     &      'in get_all_bry: can not open netCDF file ''',
     &      bry_file(1:lstr),   '''.'
        endif
        if (ierr.eq.nf_noerr) then
          ierr_all=0 
          do itrc=1,NT
            if (itrc.eq.itemp) then
              got_tbry(itemp)=.true.
# ifdef SALINITY
              got_tbry(isalt)=.true.
# endif
              ierr=nf_inq_varid (bry_id, 'bry_time',  bry_time_id)
              if (ierr.ne.nf_noerr) then
                got_tbry(itemp)=.false.
               ! ierr_all=ierr_all+ierr
# ifdef SALINITY
                got_tbry(isalt)=.false.
# endif
                write(stdout,3) 'bry_time', bry_file(1:lstr)
C     goto 99      !--> ERROR
              endif
# ifdef PISCES
            elseif (itrc.eq.iDIC_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'dic_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'dic_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iTAL_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'talk_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'talk_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iOXY_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'o2_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'o2_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iCAL_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'cal_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'cal_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iPO4_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'po4_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'po4_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iPOC_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'poc_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'poc_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iSIL_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'si_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'si_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iPHY_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'phy_time', bry_tid(itrc))
              ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'phy_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iZOO_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'zoo_time', bry_tid(itrc))
            !  ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'zoo_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iDOC_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'doc_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'doc_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iDIA_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'dia_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'dia_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iMES_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'mes_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'mes_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iBSI_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'bsi_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'bsi_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iFER_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'fer_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'fer_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iBFE_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'bfe_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'bfe_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iGOC_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'goc_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'goc_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iSFE_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'sfe_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'sfe_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iDFE_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'dfe_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'dfe_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iDSI_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'dsi_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'dsi_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iNFE_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'nfe_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'nfe_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iNCH_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'nch_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'nch_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iDCH_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'dch_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'dch_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iNO3_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'no3_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'no3_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iNH4_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'nh4_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'nh4_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif

# elif (defined BIO_NChlPZD || defined BIO_N2ChlPZD2)
            elseif (itrc.eq.iNO3_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'no3_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'NO3', bry_file(1:lstr)
c              goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iPhy1) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'phyto_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'PHYTO', bry_file(1:lstr)
c              goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iZoo1) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'zoo_time', bry_tid(itrc))
            !  ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'ZOO', bry_file(1:lstr)
c              goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iChla) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'chla_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'CHLA', bry_file(1:lstr)
c              goto 99                                     !--> ERROR
              endif
#   if !defined BIO_N2ChlPZD2         
            elseif (itrc.eq.iDet1) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'det_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'DET', bry_file(1:lstr)
c              goto 99                                     !--> ERROR
              endif
#   endif          
#   ifdef BIO_N2ChlPZD2
            elseif (itrc.eq.iNH4_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'nh4_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'NH4', bry_file(1:lstr)
c              goto 99                                     !--> ERROR
              endif
              elseif (itrc.eq.iDet1) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'det1_time', bry_tid(itrc))
             !  ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'SDET', bry_file(1:lstr)
c              goto 99                                     !--> ERROR
              endif             
            elseif (itrc.eq.iDet2) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'det2_time', bry_tid(itrc))
            !  ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'LDET', bry_file(1:lstr)
c              goto 99                                     !--> ERROR
              endif
#   endif
#   ifdef OXYGEN
            elseif (itrc.eq.iO2) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'o2_time', bry_tid(itrc))
            !  ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'O2', bry_file(1:lstr)
c              goto 99                                     !--> ERROR
              endif
#   endif   
#  elif defined BIO_BioEBUS
           elseif (itrc.eq.iNO3_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'no3_time', bry_tid(itrc))
            !  ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'no3_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iNH4_) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'nh4_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'nh4_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iPhy1) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'sphyto_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'sphyto_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iPhy2) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'lphyto_time', bry_tid(itrc))
            !  ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'lphyto_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iZoo1) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'szoo_time', bry_tid(itrc))
            !  ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'szoo_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iZoo2) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'lzoo_time', bry_tid(itrc))
            !  ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'lzoo_time', bry_file(1:lstr)
c              goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iDet1) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'sdet_time', bry_tid(itrc))
            !  ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'sdet_time', bry_file(1:lstr)
c              goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iDet2) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'ldet_time', bry_tid(itrc))
            !  ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'ldet_time', bry_file(1:lstr)
c              goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iDON) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'don_time', bry_tid(itrc))
              ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'don_time', bry_file(1:lstr)
c              goto 99                                     !--> ERROR
              endif
            elseif (itrc.eq.iO2) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'o2_time', bry_tid(itrc))
             ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'o2_time', bry_file(1:lstr)
c              goto 99  
              endif
#    ifdef NITROUS_OXIDE
            elseif (itrc.eq.iN2O) then
              got_tbry(itrc)=.true.
              ierr=nf_inq_varid (bry_id, 'n2o_time', bry_tid(itrc))
              ! ierr_all=ierr_all+ierr
              if (ierr .ne. nf_noerr) then
                got_tbry(itrc)=.false.
                write(stdout,3) 'n2o_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
              endif
#    endif
#endif     /* PISCES or (BIO_NChlPZD || BIO_N2ChlPZ2) or BIO_BioEBUS */           
            endif   !condition on itrc value
          enddo   ! loop on itrc
!
! Initialisation ierr_all
          ierr_all=0  

!     --
!     nf_noerr is  0
!     if (ierr_all.ne.nf_noerr) write(stdout,'(/1x,4A/)') 'ERROR ',
!     &                   'in get_all_bry TIME ''',bry_file(1:lstr),   '''.'             
!     --

#  ifdef OBC_WEST
          do itrc=3,NT
            if (got_tbry(itrc)) then
              lvar=lenstr(vname(1,indxT+itrc-1))
              text=vname(1,indxT+itrc-1)(1:lvar) / /'_west'
              lvar2=lenstr(text)
              ierr=nf_inq_varid(bry_id,text(1:lvar2),tbry_west_id(itrc))
              if (ierr .ne. nf_noerr) then
                write(stdout,3) text(1:lvar2),
     &               bry_file(1:lstr)
c                goto 99         !--> ERROR
              endif
            endif
          enddo
#  endif /* OBC_WEST */
          
#  ifdef OBC_EAST
          do itrc=3,NT
            if (got_tbry(itrc)) then
              lvar=lenstr(vname(1,indxT+itrc-1))
              text=vname(1,indxT+itrc-1)(1:lvar) / /'_east'
              lvar2=lenstr(text)
              ierr=nf_inq_varid(bry_id,text(1:lvar2),tbry_east_id(itrc))
              if (ierr .ne. nf_noerr) then
                write(stdout,3) text(1:lvar2),
     &            bry_file(1:lstr)
c                goto 99         !--> ERROR
              endif
            endif
          enddo
#  endif /* OBC_EAST */

#  ifdef OBC_SOUTH
          do itrc=3,NT
            if (got_tbry(itrc)) then
              lvar=lenstr(vname(1,indxT+itrc-1))
              text=vname(1,indxT+itrc-1)(1:lvar) / /'_south'
              lvar2=lenstr(text)
              ierr=nf_inq_varid(bry_id,text(1:lvar2),
     &                                 tbry_south_id(itrc))
              if (ierr .ne. nf_noerr) then
                write(stdout,3) text(1:lvar2),
     &            bry_file(1:lstr)
c                goto 99         !--> ERROR
              endif
            endif
          enddo
#  endif /* OBC_SOUTH */

#  ifdef OBC_NORTH
          do itrc=3,NT
            if (got_tbry(itrc)) then
              lvar=lenstr(vname(1,indxT+itrc-1))
              text=vname(1,indxT+itrc-1)(1:lvar) / /'_north'
              lvar2=lenstr(text)
              ierr=nf_inq_varid(bry_id,text(1:lvar2),
     &                                 tbry_north_id(itrc))
              if (ierr .ne. nf_noerr) then
                write(stdout,3) text(1:lvar2),
     &            bry_file(1:lstr)
c                goto 99         !--> ERROR
              endif
            endif
          enddo
#  endif /* OBC_NORTH */
!  
          ierr=ierr_all
          if (ierr.eq.nf_noerr) then
            do itrc=3,NT
              if(got_tbry(itrc)) then	
                call set_cycle (bry_id, bry_tid(itrc), 
     &            ntbry1(itrc),bry_cycle1(itrc),
     $            bry_ncycle1(itrc),bry_rec1(itrc))
                bry_time1(1,itrc)=-1.E+20
                bry_time1(2,itrc)=-1.E+20
                itbry1(itrc)=1
              endif	      
            enddo
          else
          MPI_master_only  write(stdout,'(8x,4A)') 
     &        'ERROR(s) occur while examining',
     &        ' content of netCDF file ''', bry_file(1:lstr), '''.'
          endif
        endif                   ! test on ierr.no_err 
      endif                     ! test on iic
 1    format(' ERROR in get_all_bry: cannot find variable ''',A,'''') !

! Read data from the file:  Check if model time is bounded by past
!===== ==== ==== === =====  and future data times: if not, increment
! record and cycling indices, flip time index and read a new portion
! of data. Repeat until model time falls between the two data times.
!     
      do itrc=3,NT              ! loop on biology
        if(got_tbry(itrc)) then
          do while (bry_time1(itbry1(itrc),itrc).lt.time+0.5*dt .and. 
     &      ierr.eq.nf_noerr)
!            write(*,*)'============================================='
!            write(*,*)'itrc=',itrc
!            write(*,*)'bry_time1(itbry1(itrc),itrc)',
!     &                                 bry_time1(itbry1(itrc),itrc)
!            write(*,*)'time+0.5*dt',time+0.5*dt
!            write(*,*)'============================================='
            call advance_cycle (bry_cycle1(itrc),ntbry1(itrc),
     &        bry_ncycle1(itrc),bry_rec1(itrc))
            if (ierr.eq.nf_noerr) then
              ierr=nf_get_var1_FTYPE 
     &          (bry_id, bry_tid(itrc), bry_rec1(itrc), cff)
              if (ierr.eq.nf_noerr) then
                itbry1(itrc)=min(3-itbry1(itrc),ntbry1(itrc))
                bry_time1(itbry1(itrc),itrc)=cff*day2sec + 
     &            bry_cycle1(itrc)*bry_ncycle1(itrc)
!
                ierr_all=0
!
# ifdef OBC_WEST

                ierr=nf_read_bry_EW (tbry_west_dt(START_1D_ARRAYETA,1
     &            ,itbry1(itrc),itrc),bry_id,tbry_west_id(itrc),
     &            bry_rec1(itrc), r3dvar)
                if (ierr.ne.nf_noerr)  write(stdout,2)  'trc_west'
                ierr_all=ierr_all+ierr
# endif
# ifdef OBC_EAST

                ierr=nf_read_bry_EW (tbry_east_dt(START_1D_ARRAYETA,1
     &            ,itbry1(itrc),itrc),bry_id,tbry_east_id(itrc),
     &            bry_rec1(itrc), r3dvar)
                if (ierr.ne.nf_noerr)  write(stdout,2)  'trc_east'
                ierr_all=ierr_all+ierr
# endif
# ifdef OBC_SOUTH
                ierr=nf_read_bry_NS (tbry_south_dt(START_1D_ARRAYXI,1
     &            ,itbry1(itrc),itrc),bry_id,tbry_south_id(itrc),
     &            bry_rec1(itrc), r3dvar)
                if (ierr.ne.nf_noerr)  write(stdout,2)  'trc_south'
                ierr_all=ierr_all+ierr
# endif
# ifdef OBC_NORTH
                ierr=nf_read_bry_NS (tbry_north_dt( START_1D_ARRAYXI,1
     &            ,itbry1(itrc),itrc),bry_id,tbry_north_id(itrc),
     &            bry_rec1(itrc), r3dvar)
                if (ierr.ne.nf_noerr)  write(stdout,2)  'trc_north'
                ierr_all=ierr_all+ierr
# endif

                ierr=ierr_all
                if (ierr.eq.0) then
                  MPI_master_only  write(stdout,
     &              '(6x,A,9x,A,1x,G12.4,1x,I4)')
     &              'GET_BRY_BIO    -- Read all boundary data',
     &              'for time =', cff 
#ifdef MPI
     &              , mynode
#endif
                else
                  MPI_master_only write(stdout,
     &              '(1x,2A,I4/8x,3A)')
     &              'ERROR(s) occur while trying to read record ',
     &              bry_rec,  'in file ''',bry_file(1:lstr),'''.'
                endif 
              else
                write(stdout,2) 'bry_time'
              endif
            else
              MPI_master_only write(stdout,
     &          '(/1x,A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')
     &          'ERROR in get_bry_bio: requested time record ',  bry_rec
     $          ,'exeeds the last record', ntbry,
     $          'available in netCDF '
     $          ,'file ''',   bry_file(1:lstr),  '''',  'tdays = ',
     $          tdays,'  but the last available  bry_time ='
     $          ,bry_time(itbry1(itrc))*sec2day
            endif

          enddo                 !do while
        endif                   !got_tbry(itrc)
      enddo                     ! itrc
      

 2    format(' ERROR in get_bry_bio: cannot read variable ''',A,'''')
 3    format(/,' GET_BRY_BIO - unable to find variable: ',
     &  a,/,15x,'in input NetCDF file: ',a, 1x,'-> analytical value'/)
      if (ierr.ne.nf_noerr) may_day_flag=2
      return
      end 

      subroutine set_bry_bio (tile)
      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call set_bry_bio_tile (Istr,Iend,Jstr,Jend)
      return
      end


      subroutine set_bry_bio_tile (Istr,Iend,Jstr,Jend)
!
!--------------------------------------------------------------------
!  Set-up biology boundary forcing fields
!--------------------------------------------------------------------
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k, it1,it2, itrc
      real cff, cff1,cff2
# include "param.h"
# include "scalars.h"
# include "boundary.h"
!
# include "compute_extended_bounds.h"

      do itrc=3,NT
        if(got_tbry(itrc)) then
          it1=3-itbry1(itrc)
          it2=itbry1(itrc)
          cff=time+0.5*dt
          cff1=bry_time1(it2,itrc)-cff
          cff2=cff-bry_time1(it1,itrc)
!            write(*,*)'========='
!            write(*,*)'it1=',it1
!            write(*,*)'it2=',it2
!            write(*,*)'cff=',time
!            write(*,*)'cff+0.5*dt=',time+0.5*dt
!            write(*,*)'bry_time1(it1,3)',bry_time1(it1,3)
!            write(*,*)'bry_time1(it2,3)',bry_time1(it2,3)
!            write(*,*)'cff1=',cff1
!            write(*,*)'cff2=',cff2 
!            write(*,*)'========='
          if (ZEROTH_TILE .and. cff1.lt.dt) synchro_flag=.true.
          
          if (cff1.ge.0. .and. cff2.ge.0.) then
            cff=1./(cff1+cff2)  ! interpolate
            cff1=cff1*cff       ! boundary values 
            cff2=cff2*cff       ! in time 

# ifdef OBC_WEST
            if (WESTERN_EDGE) then
              do k=1,N
                do j=JstrR,JendR
                  tbry_west(j,k,itrc)=cff1*tbry_west_dt(j,k,it1,itrc)
     &              +cff2*tbry_west_dt(j,k,it2,itrc)
                enddo
              enddo
            endif
# endif

# ifdef OBC_EAST
            if (EASTERN_EDGE) then
              do k=1,N
                do j=JstrR,JendR
                  tbry_east(j,k,itrc)=cff1*tbry_east_dt(j,k,it1,itrc)
     &              +cff2*tbry_east_dt(j,k,it2,itrc)
                enddo
              enddo
            endif
# endif


# ifdef OBC_SOUTH
            if (SOUTHERN_EDGE) then
              do k=1,N
                do j=IstrR,IendR
                  tbry_south(j,k,itrc)=cff1*tbry_south_dt(j,k,it1,itrc)
     &              +cff2*tbry_south_dt(j,k,it2,itrc)
                enddo
              enddo
            endif
# endif


# ifdef OBC_NORTH
            if (NORTHERN_EDGE) then
              do k=1,N
                do j=IstrR,IendR
                  tbry_north(j,k,itrc)=cff1*tbry_north_dt(j,k,it1,itrc)
     &              +cff2*tbry_north_dt(j,k,it2,itrc)
                enddo
              enddo
            endif
# endif

          elseif (ZEROTH_TILE) then
           MPI_master_only  write(stdout,
     &      '(/2(1x,A)/3(1x,A,F16.10)/)')
     &      'SET_BRY_ALL_TILE - current model time is out of bounds of',
     &      '''bry_time''.',     'BRY_TSTART=',   bry_time(it1)*sec2day,
     &      'TDAYS=',  tdays,    'BRY_TEND=',     bry_time(it2)*sec2day
            may_day_flag=2
          endif
        endif
       enddo

       return
       end
#else
      subroutine get_bry_bio_empty
      end
#endif

