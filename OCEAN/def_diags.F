! $Id: def_diags.F 1571 2014-07-01 12:38:05Z gcambon $
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
#ifndef AVRH
# include "cppdefs.h"
#endif
#if defined  DIAGNOSTICS_TS 
# ifndef AVRH                  
      subroutine def_diags(ncid,total_rec,ierr) 
# else
      subroutine def_diags_avg(ncid,total_rec,ierr)
# endif
!
!---------------------------------------------------------------
! Create diag data NetCDF file:
! Define its variables, dimensions, and attributes
!---------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "mixing.h"
# include "ncscrum.h"
# include "scalars.h"
# include "strings.h"
# include "diagnostics.h"
# include "netcdf.inc"
      logical create_new_file, res
      integer ncid, total_rec, ierr, rec, lstr,lvar,lenstr, timedim
     &      , r2dgrd(3),u2dgrd(3),v2dgrd(3),auxil(2),checkdims 
# ifdef SOLVE3D
     &      , r3dgrd(4),u3dgrd(4),v3dgrd(4), w3dgrd(4), itrc
# endif
!
# ifndef AVRH
!
#  define ncname dianame
#  define rec_per_file nrpfdia
#  define wrt3D wrtdia3D
#  define vidTime diaTime
#  define vidTime2 diaTime2
#  define vidTstep diaTstep
!
#  define vidTXadv diaTXadv
#  define vidTYadv diaTYadv
#  define vidTVadv diaTVadv
#  define vidTHmix diaTHmix
#  define vidTVmix diaTVmix
#  define vidTForc diaTForc
#  define vidTrate diaTrate
!
#ifdef DIAGNOSTICS_TS_MLD
#  define wrt2D wrtdia2D
#  define vidTXadv_mld diaTXadv_mld
#  define vidTYadv_mld diaTYadv_mld
#  define vidTVadv_mld diaTVadv_mld
#  define vidTHmix_mld diaTHmix_mld
#  define vidTVmix_mld diaTVmix_mld
#  define vidTForc_mld diaTForc_mld
#  define vidTrate_mld diaTrate_mld
#  define vidTentr_mld diaTentr_mld
#endif
!
# else /* AVRH */
!
#  define ncname dianame_avg
#  define rec_per_file nrpfdia_avg
#  define wrt3D wrtdia3D_avg
#  define vidTime diaTime_avg
#  define vidTime2 diaTime2_avg
#  define vidTstep diaTstep_avg
!
#  define vidTXadv diaTXadv_avg
#  define vidTYadv diaTYadv_avg
#  define vidTVadv diaTVadv_avg
#  define vidTHmix diaTHmix_avg
#  define vidTVmix diaTVmix_avg
#  define vidTForc diaTForc_avg
#  define vidTrate diaTrate_avg
!
#ifdef DIAGNOSTICS_TS_MLD
#  define wrt2D wrtdia2D_avg
#  define vidTXadv_mld diaTXadv_mld_avg
#  define vidTYadv_mld diaTYadv_mld_avg
#  define vidTVadv_mld diaTVadv_mld_avg
#  define vidTHmix_mld diaTHmix_mld_avg
#  define vidTVmix_mld diaTVmix_mld_avg
#  define vidTForc_mld diaTForc_mld_avg
#  define vidTrate_mld diaTrate_mld_avg
#  define vidTentr_mld diaTentr_mld_avg
#endif
!
        character*60 text
!
# endif /* AVRH */
!
      if (may_day_flag.ne.0) return      !-->  EXIT
!
      ierr=0
      lstr=lenstr(ncname)
      if (rec_per_file.gt.0) then
        lvar=total_rec-(1+mod(total_rec-1, rec_per_file))
        call insert_time_index (ncname, lstr, lvar, ierr)
        if (ierr .ne. 0) goto 99
      endif
!
!  Create a new diagnostics data file.
!-------------------------------------
!
#ifndef AVRH
      create_new_file=ldefdia
#else
      create_new_file=ldefdia_avg
#endif
!
      if (ncid.ne.-1) create_new_file=.false.
#if defined MPI & !defined PARALLEL_FILES
      if (mynode.gt.0) create_new_file=.false.
#endif
!
 10   if (create_new_file) then
        lstr=lenstr(ncname)
        ierr=nf_create(ncname(1:lstr),NF_CLOBBER,ncid)
        if (ierr.ne.nf_noerr) then
          write(stdout,11) ncname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
!
! Put global attributes.
! --- ------ -----------
!
        call put_global_atts (ncid, ierr)
        if (ierr.ne.nf_noerr) then
          write(stdout,11) ncname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
!
! Define dimensions of staggered fields.
! ------ ---------- -- --------- -------
!
        ierr=nf_def_dim (ncid, 'xi_rho',   xi_rho,   r2dgrd(1))
        ierr=nf_def_dim (ncid, 'xi_u',     xi_u,     u2dgrd(1))
        ierr=nf_def_dim (ncid, 'eta_rho',  eta_rho,  r2dgrd(2))
        ierr=nf_def_dim (ncid, 'eta_v',    eta_v,    v2dgrd(2))
#ifdef SOLVE3D
        ierr=nf_def_dim (ncid, 's_rho',    N,        r3dgrd(3))
        ierr=nf_def_dim (ncid, 's_w',      N+1,      w3dgrd(3))
#endif
        ierr=nf_def_dim (ncid, 'time', nf_unlimited, timedim)
        ierr=nf_def_dim (ncid, 'auxil',    4,        auxil(1))
!
        auxil(2)=timedim
        r2dgrd(3)=timedim           ! Free surface
!
        u2dgrd(2)=r2dgrd(2)         ! 2D UBAR-type
        u2dgrd(3)=timedim
!
        v2dgrd(1)=r2dgrd(1)         ! 2D VBAR-type
        v2dgrd(3)=timedim
!
#ifdef SOLVE3D
        r3dgrd(1)=r2dgrd(1)         !
        r3dgrd(2)=r2dgrd(2)         ! 3D RHO-type
        r3dgrd(4)=timedim           !
!
        u3dgrd(1)=u2dgrd(1)         !
        u3dgrd(2)=r2dgrd(2)         ! 3D U-type
        u3dgrd(3)=r3dgrd(3)         !
        u3dgrd(4)=timedim
!
        v3dgrd(1)=r2dgrd(1)         !
        v3dgrd(2)=v2dgrd(2)         ! 3D V-type
        v3dgrd(3)=r3dgrd(3)         !
        v3dgrd(4)=timedim
#endif
!
#if (defined PUT_GRID_INTO_HISTORY && !defined AVRH)\
 || (defined PUT_GRID_INTO_AVERAGES && defined AVRH)
!
! Define grid variables :
! ------ ---- ---------
!
      if (total_rec.le.1) then
# ifdef SOLVE3D
         call def_grid_3d(ncid, r2dgrd, u2dgrd, v2dgrd 
     &        ,r3dgrd, w3dgrd)
# else
         call def_grid_2d(ncid, r2dgrd, u2dgrd, v2dgrd)
# endif
      endif
#endif
!
! Define running parameters :
!--------------------------
!
! Define variables (time step number and time record indices)
! and their attributes
!
        ierr=nf_def_var (ncid, 'time_step', nf_int, 2, auxil,
     &                                                 vidTstep)
        ierr=nf_put_att_text (ncid, diaTstep, 'long_name', 48,
     &       'time step and record numbers from initialization')
!
! Time.
!
        lvar=lenstr(vname(1,indxTime))
        ierr=nf_def_var (ncid, vname(1,indxTime)(1:lvar),
     &                            NF_FOUT, 1, timedim, vidTime)
!
#ifndef AVRH
        lvar=lenstr(vname(2,indxTime))
        ierr=nf_put_att_text (ncid, vidTime, 'long_name', 
     &       lvar, vname(2,indxTime)(1:lvar))
#else
        text='avg'/ /vname(2,indxTime)
        lvar=lenstr(text)
        ierr=nf_put_att_text (ncid, vidTime, 'long_name', 
     &       lvar, text(1:lvar))
#endif
!
        lvar=lenstr(vname(2,indxTime))
        ierr=nf_put_att_text (ncid, vidTime, 'long_name', lvar,
     &                                vname(2,indxTime)(1:lvar))
!
        lvar=lenstr(vname(3,indxTime))
        ierr=nf_put_att_text (ncid, vidTime, 'units',  lvar,
     &                                vname(3,indxTime)(1:lvar))
!
        lvar=lenstr(vname(4,indxTime))
        ierr=nf_put_att_text (ncid, vidTime, 'field',  lvar,
     &                                vname(4,indxTime)(1:lvar))

        call nf_add_attribute(ncid, vidTime, indxTime, 5, 
     &       NF_FOUT, ierr)
!
! Time2.
!
        lvar=lenstr(vname(1,indxTime2))
        ierr=nf_def_var (ncid, vname(1,indxTime2)(1:lvar),
     &                            NF_FOUT, 1, timedim, vidTime2)
!
#ifndef AVRH
        lvar=lenstr(vname(2,indxTime2))
        ierr=nf_put_att_text (ncid, vidTime2, 'long_name', 
     &       lvar, vname(2,indxTime2)(1:lvar))
#else
        text='avg'/ /vname(2,indxTime2)
        lvar=lenstr(text)
        ierr=nf_put_att_text (ncid, vidTime2, 'long_name', 
     &       lvar, text(1:lvar))
#endif
!
        lvar=lenstr(vname(2,indxTime2))
        ierr=nf_put_att_text (ncid, vidTime2, 'long_name', lvar,
     &                                vname(2,indxTime2)(1:lvar))
!
        lvar=lenstr(vname(3,indxTime2))
        ierr=nf_put_att_text (ncid, vidTime2, 'units',  lvar,
     &                                vname(3,indxTime2)(1:lvar))
!
        lvar=lenstr(vname(4,indxTime2))
        ierr=nf_put_att_text (ncid, vidTime2, 'field',  lvar,
     &                                vname(4,indxTime2)(1:lvar))

        call nf_add_attribute(ncid, vidTime2, indxTime2, 5, 
     &       NF_FOUT, ierr)
!
! Tracer diagnostics variables :
! ------ ----------- ---------
!
!
        do itrc=1,NT
          if (wrt3D(itrc)) then
!
! indxTXadv
!
          lvar=lenstr(vname(1,indxTXadv+itrc-1))
          ierr=nf_def_var (ncid, vname(1,indxTXadv+itrc-1)(1:lvar),
     &          NF_FOUT, 4, r3dgrd, vidTXadv(itrc))
# ifndef AVRH
          lvar=lenstr(vname(2,indxTXadv+itrc-1))
          ierr=nf_put_att_text (ncid, vidTXadv(itrc), 'long_name',
     &          lvar, vname(2,indxTXadv+itrc-1)(1:lvar))
# else
          text='averaged '/ /vname(2,indxTXadv+itrc-1)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidTXadv(itrc), 'long_name', 
     &          lvar, text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxTXadv+itrc-1))
          ierr=nf_put_att_text (ncid, vidTXadv(itrc), 'units', lvar,
     &          vname(3,indxTXadv+itrc-1)(1:lvar))
          lvar=lenstr(vname(4,indxTXadv+itrc-1))
          ierr=nf_put_att_text (ncid,vidTXadv(itrc), 'field',
     &          lvar, vname(4,indxTXadv+itrc-1)(1:lvar))

       call nf_add_attribute(ncid,vidTXadv(itrc),indxTXadv+itrc-1,5, 
     &       NF_FOUT, ierr)
! 
! indxTYadv
!
           lvar=lenstr(vname(1,indxTYadv+itrc-1))
           ierr=nf_def_var (ncid, vname(1,indxTYadv+itrc-1)(1:lvar),
     &                           NF_FOUT, 4, r3dgrd, vidTYadv(itrc))
# ifndef AVRH
           lvar=lenstr(vname(2,indxTYadv+itrc-1))
           ierr=nf_put_att_text (ncid, vidTYadv(itrc), 'long_name',
     &                     lvar, vname(2,indxTYadv+itrc-1)(1:lvar))
# else
           text='averaged '/ /vname(2,indxTYadv+itrc-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidTYadv(itrc), 'long_name', 
     &       lvar, text(1:lvar))
# endif
           lvar=lenstr(vname(3,indxTYadv+itrc-1))
           ierr=nf_put_att_text (ncid, vidTYadv(itrc), 'units', lvar,
     &                             vname(3,indxTYadv+itrc-1)(1:lvar))
           lvar=lenstr(vname(4,indxTYadv+itrc-1))
           ierr=nf_put_att_text (ncid,vidTYadv(itrc), 'field',
     &                       lvar, vname(4,indxTYadv+itrc-1)(1:lvar))

       call nf_add_attribute(ncid,vidTYadv(itrc),indxTYadv+itrc-1,5, 
     &       NF_FOUT, ierr)
!
! indxTVadv
!
           lvar=lenstr(vname(1,indxTVadv+itrc-1))
           ierr=nf_def_var (ncid, vname(1,indxTVadv+itrc-1)(1:lvar),
     &                           NF_FOUT, 4, r3dgrd, vidTVadv(itrc))
# ifndef AVRH
           lvar=lenstr(vname(2,indxTVadv+itrc-1))
           ierr=nf_put_att_text (ncid, vidTVadv(itrc), 'long_name',
     &                     lvar, vname(2,indxTVadv+itrc-1)(1:lvar))
# else
           text='averaged '/ /vname(2,indxTVadv+itrc-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidTVadv(itrc), 'long_name', 
     &       lvar, text(1:lvar))
# endif
           lvar=lenstr(vname(3,indxTVadv+itrc-1))
           ierr=nf_put_att_text (ncid, vidTVadv(itrc), 'units', lvar,
     &                             vname(3,indxTVadv+itrc-1)(1:lvar))
           lvar=lenstr(vname(4,indxTVadv+itrc-1))
           ierr=nf_put_att_text (ncid,vidTVadv(itrc), 'field',
     &                       lvar, vname(4,indxTVadv+itrc-1)(1:lvar))

       call nf_add_attribute(ncid,vidTVadv(itrc),indxTVadv+itrc-1,5, 
     &       NF_FOUT, ierr)
!
! indxTHmix
!
           lvar=lenstr(vname(1,indxTHmix+itrc-1))
           ierr=nf_def_var (ncid, vname(1,indxTHmix+itrc-1)(1:lvar),
     &                            NF_FOUT, 4, r3dgrd, vidTHmix(itrc))
# ifndef AVRH
           lvar=lenstr(vname(2,indxTHmix+itrc-1))
           ierr=nf_put_att_text (ncid, vidTHmix(itrc), 'long_name',
     &                       lvar, vname(2,indxTHmix+itrc-1)(1:lvar))
# else
           text='averaged '/ /vname(2,indxTHmix+itrc-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidTHmix(itrc), 'long_name',
     &                                            lvar, text(1:lvar))
# endif
           lvar=lenstr(vname(3,indxTHmix+itrc-1))
           ierr=nf_put_att_text (ncid, diaTHmix(itrc), 'units', lvar,
     &                             vname(3,indxTHmix+itrc-1)(1:lvar))
           lvar=lenstr(vname(4,indxTHmix+itrc-1))
           ierr=nf_put_att_text (ncid,vidTHmix(itrc), 'field',
     &                       lvar, vname(4,indxTHmix+itrc-1)(1:lvar))

       call nf_add_attribute(ncid,vidTHmix(itrc),indxTHmix+itrc-1,5, 
     &       NF_FOUT, ierr)
!
!indxTVmix
!
           lvar=lenstr(vname(1,indxTVmix+itrc-1))
           ierr=nf_def_var (ncid, vname(1,indxTVmix+itrc-1)(1:lvar),
     &                             NF_FOUT, 4, r3dgrd, vidTVmix(itrc))
# ifndef AVRH
           lvar=lenstr(vname(2,indxTVmix+itrc-1))
           ierr=nf_put_att_text (ncid, vidTVmix(itrc), 'long_name',
     &                     lvar, vname(2,indxTVmix+itrc-1)(1:lvar))
# else
           text='averaged '/ /vname(2,indxTVmix+itrc-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidTVmix(itrc), 'long_name', 
     &       lvar, text(1:lvar))
# endif
           lvar=lenstr(vname(3,indxTVmix+itrc-1))
           ierr=nf_put_att_text (ncid, diaTVmix(itrc), 'units', lvar,
     &                             vname(3,indxTVmix+itrc-1)(1:lvar))
           lvar=lenstr(vname(4,indxTVmix+itrc-1))
           ierr=nf_put_att_text (ncid,vidTVmix(itrc), 'field',
     &                       lvar, vname(4,indxTVmix+itrc-1)(1:lvar))

       call nf_add_attribute(ncid,vidTVmix(itrc),indxTVmix+itrc-1,5, 
     &       NF_FOUT, ierr)
!
! indxTForc - Solar radiation and nudging body forces
!
           lvar=lenstr(vname(1,indxTForc+itrc-1))
           ierr=nf_def_var (ncid,vname(1,indxTForc+itrc-1)(1:lvar),
     &                        NF_FOUT, 4, r3dgrd, vidTForc(itrc))
# ifndef AVRH
           lvar=lenstr(vname(2,indxTForc+itrc-1))
           ierr=nf_put_att_text (ncid, vidTForc(itrc), 'long_name', 
     &                      lvar, vname(2,indxTForc+itrc-1)(1:lvar))
# else
           text='averaged '/ /vname(2,indxTForc+itrc-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidTForc(itrc), 'long_name', 
     &          lvar, text(1:lvar))
# endif
           lvar=lenstr(vname(3,indxTForc+itrc-1))
           ierr=nf_put_att_text (ncid, vidTForc(itrc),'units',     
     &                     lvar, vname(3,indxTForc+itrc-1)(1:lvar))
           lvar=lenstr(vname(4,indxTForc+itrc-1))
           ierr=nf_put_att_text (ncid, vidTForc(itrc),'field',     
     &                     lvar, vname(4,indxTForc+itrc-1)(1:lvar))

       call nf_add_attribute(ncid,vidTForc(itrc),indxTForc+itrc-1,5, 
     &       NF_FOUT, ierr)
!
! Trate indxTrate
!
           lvar=lenstr(vname(1,indxTrate+itrc-1))
           ierr=nf_def_var (ncid, vname(1,indxTrate+itrc-1)(1:lvar),
     &                           NF_FOUT, 4, r3dgrd, vidTrate(itrc))
# ifndef AVRH
           lvar=lenstr(vname(2,indxTrate+itrc-1))
           ierr=nf_put_att_text (ncid, vidTrate(itrc), 'long_name', 
     &                     lvar, vname(2,indxTrate+itrc-1)(1:lvar))
# else
           text='averaged '/ /vname(2,indxTrate+itrc-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidTrate(itrc), 'long_name', 
     &                                          lvar, text(1:lvar))
# endif
           lvar=lenstr(vname(3,indxTrate+itrc-1))
           ierr=nf_put_att_text (ncid, vidTrate(itrc),'units',
     &                     lvar, vname(3,indxTrate+itrc-1)(1:lvar))
           lvar=lenstr(vname(4,indxTrate+itrc-1))
           ierr=nf_put_att_text (ncid, vidTrate(itrc),'field',
     &                     lvar, vname(4,indxTrate+itrc-1)(1:lvar))

       call nf_add_attribute(ncid,vidTrate(itrc),indxTrate+itrc-1,5, 
     &       NF_FOUT, ierr)
!
         endif
!
#ifdef DIAGNOSTICS_TS_MLD
         
         if (wrt2D(itrc)) then
!
! indxTXadv on MLD
!
           lvar=lenstr(vname(1,indxTXadv_mld+itrc-1))
           ierr=nf_def_var (ncid, vname(1,indxTXadv_mld+itrc-1)(1:lvar),
     &                       NF_FOUT, 3, r2dgrd, vidTXadv_mld(itrc))
# ifndef AVRH
           lvar=lenstr(vname(2,indxTXadv_mld+itrc-1))
           ierr=nf_put_att_text (ncid, vidTXadv_mld(itrc), 'long_name',
     &                     lvar, vname(2,indxTXadv_mld+itrc-1)(1:lvar))
# else
           text='averaged '/ /vname(2,indxTXadv_mld+itrc-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidTXadv_mld(itrc), 'long_name', 
     &       lvar, text(1:lvar))
# endif
           lvar=lenstr(vname(3,indxTXadv_mld+itrc-1))
           ierr=nf_put_att_text (ncid,vidTXadv_mld(itrc), 'units', lvar,
     &                            vname(3,indxTXadv_mld+itrc-1)(1:lvar))
           lvar=lenstr(vname(4,indxTXadv_mld+itrc-1))
           ierr=nf_put_att_text (ncid,vidTXadv_mld(itrc), 'field',
     &                      lvar, vname(4,indxTXadv_mld+itrc-1)(1:lvar))

       call nf_add_attribute(ncid,vidTXadv_mld(itrc),
     &                       indxTXadv_mld+itrc-1,5, NF_FOUT, ierr)
!
! indxTYadv on MLD
!
           lvar=lenstr(vname(1,indxTYadv_mld+itrc-1))
           ierr=nf_def_var (ncid, vname(1,indxTYadv_mld+itrc-1)(1:lvar),
     &                           NF_FOUT, 3, r2dgrd, vidTYadv_mld(itrc))
# ifndef AVRH
           lvar=lenstr(vname(2,indxTYadv_mld+itrc-1))
           ierr=nf_put_att_text (ncid, vidTYadv_mld(itrc), 'long_name',
     &                     lvar, vname(2,indxTYadv_mld+itrc-1)(1:lvar))
# else
           text='averaged '/ /vname(2,indxTYadv_mld+itrc-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidTYadv_mld(itrc), 'long_name', 
     &       lvar, text(1:lvar))
# endif
           lvar=lenstr(vname(3,indxTYadv_mld+itrc-1))
           ierr=nf_put_att_text (ncid,vidTYadv_mld(itrc), 'units', lvar,
     &                            vname(3,indxTYadv_mld+itrc-1)(1:lvar))
           lvar=lenstr(vname(4,indxTYadv_mld+itrc-1))
           ierr=nf_put_att_text (ncid,vidTYadv_mld(itrc), 'field',
     &                      lvar, vname(4,indxTYadv_mld+itrc-1)(1:lvar))

       call nf_add_attribute(ncid,vidTYadv_mld(itrc),
     &                       indxTYadv_mld+itrc-1,5, NF_FOUT, ierr)
!
! indxTVadv on MLD
!
           lvar=lenstr(vname(1,indxTVadv_mld+itrc-1))
           ierr=nf_def_var (ncid, vname(1,indxTVadv_mld+itrc-1)(1:lvar),
     &                           NF_FOUT, 3, r2dgrd, vidTVadv_mld(itrc))
# ifndef AVRH
           lvar=lenstr(vname(2,indxTVadv_mld+itrc-1))
           ierr=nf_put_att_text (ncid, vidTVadv_mld(itrc), 'long_name',
     &                     lvar, vname(2,indxTVadv_mld+itrc-1)(1:lvar))
# else
           text='averaged '/ /vname(2,indxTVadv_mld+itrc-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidTVadv_mld(itrc), 'long_name', 
     &       lvar, text(1:lvar))
# endif
           lvar=lenstr(vname(3,indxTVadv_mld+itrc-1))
           ierr=nf_put_att_text (ncid,vidTVadv_mld(itrc), 'units', lvar,
     &                            vname(3,indxTVadv_mld+itrc-1)(1:lvar))
           lvar=lenstr(vname(4,indxTVadv_mld+itrc-1))
           ierr=nf_put_att_text (ncid,vidTVadv_mld(itrc), 'field',
     &                      lvar, vname(4,indxTVadv_mld+itrc-1)(1:lvar))

       call nf_add_attribute(ncid,vidTVadv_mld(itrc),
     &                       indxTVadv_mld+itrc-1,5, NF_FOUT, ierr)
!
! indxTHmix on MLD
!
           lvar=lenstr(vname(1,indxTHmix_mld+itrc-1))
           ierr=nf_def_var (ncid, vname(1,indxTHmix_mld+itrc-1)(1:lvar),
     &                           NF_FOUT, 3, r2dgrd, vidTHmix_mld(itrc))
# ifndef AVRH
           lvar=lenstr(vname(2,indxTHmix_mld+itrc-1))
           ierr=nf_put_att_text (ncid, vidTHmix_mld(itrc), 'long_name',
     &                      lvar, vname(2,indxTHmix_mld+itrc-1)(1:lvar))
# else
           text='averaged '/ /vname(2,indxTHmix_mld+itrc-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidTHmix_mld(itrc), 'long_name',
     &                                            lvar, text(1:lvar))
# endif
           lvar=lenstr(vname(3,indxTHmix_mld+itrc-1))
           ierr=nf_put_att_text (ncid,diaTHmix_mld(itrc), 'units', lvar,
     &                            vname(3,indxTHmix_mld+itrc-1)(1:lvar))
           lvar=lenstr(vname(4,indxTHmix_mld+itrc-1))
           ierr=nf_put_att_text (ncid,vidTHmix_mld(itrc), 'field',
     &                      lvar, vname(4,indxTHmix_mld+itrc-1)(1:lvar))

       call nf_add_attribute(ncid,vidTHmix_mld(itrc),
     &                       indxTHmix_mld+itrc-1,5, NF_FOUT, ierr)
!
!indxTVmix on MLD
!
           lvar=lenstr(vname(1,indxTVmix_mld+itrc-1))
           ierr=nf_def_var (ncid, vname(1,indxTVmix_mld+itrc-1)(1:lvar),
     &                           NF_FOUT, 3, r2dgrd, vidTVmix_mld(itrc))
# ifndef AVRH
           lvar=lenstr(vname(2,indxTVmix_mld+itrc-1))
           ierr=nf_put_att_text (ncid, vidTVmix_mld(itrc), 'long_name',
     &                     lvar, vname(2,indxTVmix_mld+itrc-1)(1:lvar))
# else
           text='averaged '/ /vname(2,indxTVmix_mld+itrc-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidTVmix_mld(itrc), 'long_name', 
     &       lvar, text(1:lvar))
# endif
           lvar=lenstr(vname(3,indxTVmix_mld+itrc-1))
           ierr=nf_put_att_text (ncid,diaTVmix_mld(itrc), 'units', lvar,
     &                            vname(3,indxTVmix_mld+itrc-1)(1:lvar))
           lvar=lenstr(vname(4,indxTVmix_mld+itrc-1))
           ierr=nf_put_att_text (ncid,vidTVmix_mld(itrc), 'field',
     &                      lvar, vname(4,indxTVmix_mld+itrc-1)(1:lvar))

       call nf_add_attribute(ncid,vidTVmix_mld(itrc),
     &                       indxTVmix_mld+itrc-1,5, NF_FOUT, ierr)
!
! indxTForc - Solar radiation and nudging body forces - on MLD
!
           lvar=lenstr(vname(1,indxTForc_mld+itrc-1))
           ierr=nf_def_var (ncid,vname(1,indxTForc_mld+itrc-1)(1:lvar),
     &                        NF_FOUT, 3, r2dgrd, vidTForc_mld(itrc))
# ifndef AVRH
           lvar=lenstr(vname(2,indxTForc_mld+itrc-1))
           ierr=nf_put_att_text (ncid, vidTForc_mld(itrc), 'long_name', 
     &                      lvar, vname(2,indxTForc_mld+itrc-1)(1:lvar))
# else
           text='averaged '/ /vname(2,indxTForc_mld+itrc-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidTForc_mld(itrc), 'long_name', 
     &          lvar, text(1:lvar))
# endif
           lvar=lenstr(vname(3,indxTForc_mld+itrc-1))
           ierr=nf_put_att_text (ncid, vidTForc_mld(itrc),'units',     
     &                     lvar, vname(3,indxTForc_mld+itrc-1)(1:lvar))
           lvar=lenstr(vname(4,indxTForc_mld+itrc-1))
           ierr=nf_put_att_text (ncid, vidTForc_mld(itrc),'field',     
     &                     lvar, vname(4,indxTForc_mld+itrc-1)(1:lvar))

       call nf_add_attribute(ncid,vidTForc_mld(itrc),
     &                       indxTForc_mld+itrc-1,5, NF_FOUT, ierr)
!
! Trate indxTrate on MLD
!
           lvar=lenstr(vname(1,indxTrate_mld+itrc-1))
           ierr=nf_def_var (ncid, vname(1,indxTrate_mld+itrc-1)(1:lvar),
     &                           NF_FOUT, 3, r2dgrd, vidTrate_mld(itrc))
# ifndef AVRH
           lvar=lenstr(vname(2,indxTrate_mld+itrc-1))
           ierr=nf_put_att_text (ncid, vidTrate_mld(itrc), 'long_name', 
     &                     lvar, vname(2,indxTrate_mld+itrc-1)(1:lvar))
# else
           text='averaged '/ /vname(2,indxTrate_mld+itrc-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidTrate_mld(itrc), 'long_name', 
     &                                          lvar, text(1:lvar))
# endif
           lvar=lenstr(vname(3,indxTrate_mld+itrc-1))
           ierr=nf_put_att_text (ncid, vidTrate_mld(itrc),'units',
     &                     lvar, vname(3,indxTrate_mld+itrc-1)(1:lvar))
           lvar=lenstr(vname(4,indxTrate_mld+itrc-1))
           ierr=nf_put_att_text (ncid, vidTrate_mld(itrc),'field',
     &                     lvar, vname(4,indxTrate_mld+itrc-1)(1:lvar))

       call nf_add_attribute(ncid,vidTrate_mld(itrc),
     &                       indxTrate_mld+itrc-1,5, NF_FOUT, ierr)
!
! Tentr indxTentr on MLD
!
           lvar=lenstr(vname(1,indxTentr_mld+itrc-1))
           ierr=nf_def_var (ncid, vname(1,indxTentr_mld+itrc-1)(1:lvar),
     &                           NF_FOUT, 3, r2dgrd, vidTentr_mld(itrc))
# ifndef AVRH
           lvar=lenstr(vname(2,indxTentr_mld+itrc-1))
           ierr=nf_put_att_text (ncid, vidTentr_mld(itrc), 'long_name',
     &                     lvar, vname(2,indxTentr_mld+itrc-1)(1:lvar))
# else
           text='averaged '/ /vname(2,indxTentr_mld+itrc-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidTentr_mld(itrc), 'long_name',
     &                                          lvar, text(1:lvar))
# endif
           lvar=lenstr(vname(3,indxTentr_mld+itrc-1))
           ierr=nf_put_att_text (ncid, vidTentr_mld(itrc),'units',
     &                     lvar, vname(3,indxTentr_mld+itrc-1)(1:lvar))
           lvar=lenstr(vname(4,indxTentr_mld+itrc-1))
           ierr=nf_put_att_text (ncid, vidTentr_mld(itrc),'field',
     &                     lvar, vname(4,indxTentr_mld+itrc-1)(1:lvar))

       call nf_add_attribute(ncid,vidTentr_mld(itrc),
     &                       indxTentr_mld+itrc-1,5, NF_FOUT, ierr)
         endif
!
#endif /* DIAGNOSTICS_TS_MLD */
       enddo
!
!
! Leave definition mode.
! ----- ---------- -----
!
        ierr=nf_enddef(ncid)
        write(stdout,'(6x,4A,1x,A,i4)') 
#ifdef AVRH
     &        'DEF_DIAG_AVG - Created ',
#else      
     &        'DEF_DIAG - Created ',
#endif /*AVRH*/
     &                'new netCDF file ''', 
     &                 ncname(1:lstr), '''.'
     &                 MYID
!
! Open an existing file and prepare for appending data.
! ==== == ======== ==== === ======= === ========= =====
! Inquire about the dimensions and variables. Check for
! consistency with model dimensions. In the case when file 
! is rejected (whether it cannot be opened, or something
! is wrong with its dimensions) create a new file.2
!
! After that verify that all necessary variables are already
! defined there and find their netCDF IDs.
!
      elseif (ncid.eq.-1) then
        ierr=nf_open (ncname(1:lstr), nf_write, ncid)
        if (ierr. ne. nf_noerr) then
          ierr=checkdims (ncid, ncname, lstr, rec) 
          if (ierr .eq. nf_noerr) then
            if (rec_per_file.eq.0) then
              ierr=rec+1 - total_rec
            else
              ierr=rec+1 - (1+mod(total_rec-1, rec_per_file))
            endif
            if (ierr.gt.0) then
              MPI_master_only write( stdout,
     &                 '(/1x,A,I5,1x,A/8x,3A,I5,/8x,A,I5,1x,A/)'
     &     ) 'DEF_DIAGS/DIAGS_AVG WARNING: Actual number of records',
     &               rec,  'in netCDF file',  '''',  ncname(1:lstr),
     &             ''' exceeds the record number from restart data',
     &             rec+1-ierr,'/', total_rec,', restart is assumed.'
              rec=rec-ierr
            elseif (rec_per_file.eq.0) then
              total_rec=rec+1           ! <-- set to the next record
#if defined MPI & !defined PARALLEL_FILES
              if (mynode.gt.0) total_rec=total_rec-1
#endif
            endif
            ierr=nf_noerr
          endif
        endif
!
        if (ierr. ne. nf_noerr) then
#if defined MPI & !defined PARALLEL_FILES
          if (mynode.eq.0) then
            create_new_file=.true.
            goto 10
          else
      MPI_master_only  write(stdout,'(/1x,4A,2x,A,I4/)') 
     &         'DEF_DIAGS_HIS/AVG ERROR: ',
     &         'Cannot open file ''', ncname(1:lstr), '''.'
     &                   MYID
            goto 99                                       !--> ERROR
          endif
#else
          create_new_file=.true.
          goto 10
#endif
        endif
!
! Find netCDF IDs of evolving model variables:
! ---- ------ --- -- -------- ----- ----------
!
! Time step indices:
!         
        ierr=nf_inq_varid (ncid, 'time_step', vidTstep)
        if (ierr .ne. nf_noerr) then
          write(stdout,1) 'time_step', ncname(1:lstr)
          goto 99                                         !--> ERROR
        endif
!
! Time.
!
        lvar=lenstr(vname(1,indxTime))
        ierr=nf_inq_varid (ncid,vname(1,indxTime)(1:lvar),vidTime)
        if (ierr .ne. nf_noerr) then
          write(stdout,1) vname(1,indxTime)(1:lvar), ncname(1:lstr)
          goto 99                                         !--> ERROR 
        endif
!
! Time2.
!
        lvar=lenstr(vname(1,indxTime2))
        ierr=nf_inq_varid (ncid,vname(1,indxTime2)(1:lvar),vidTime2)
        if (ierr .ne. nf_noerr) then
          write(stdout,1) vname(1,indxTime2)(1:lvar), ncname(1:lstr)
          goto 99                                         !--> ERROR 
        endif
!
! Tracer flux diagnostics variables :
! ------ ---- ----------- --------- 
!
!
        do itrc=1,NT
          if (wrt3D(itrc)) then
!
! indxTXadv
!
          lvar=lenstr(vname(1,indxTXadv+itrc-1))
          ierr=nf_inq_varid (ncid, vname(1,indxTXadv+itrc-1)(1:lvar),
     &                                               vidTXadv(itrc))
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxTXadv+itrc-1)(1:lvar),
     &                                         ncname(1:lstr) 
            goto 99                                       !--> ERROR
          endif
!
! indxTYadv
!
          lvar=lenstr(vname(1,indxTYadv+itrc-1))
          ierr=nf_inq_varid (ncid, vname(1,indxTYadv+itrc-1)(1:lvar),
     &                                               vidTYadv(itrc))
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxTYadv+itrc-1)(1:lvar),
     &                                         ncname(1:lstr) 
            goto 99                                       !--> ERROR
          endif
!
! indxTVadv
!
          lvar=lenstr(vname(1,indxTVadv+itrc-1))
          ierr=nf_inq_varid (ncid, vname(1,indxTVadv+itrc-1)(1:lvar),
     &                                               vidTVadv(itrc))
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxTVadv+itrc-1)(1:lvar),
     &                                         ncname(1:lstr) 
            goto 99                                       !--> ERROR
          endif
!
! indxTHmix
!
          lvar=lenstr(vname(1,indxTHmix+itrc-1))
          ierr=nf_inq_varid (ncid, vname(1,indxTHmix+itrc-1)(1:lvar),
     &                                               vidTHmix(itrc))
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxTHmix+itrc-1)(1:lvar),
     &                                         ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
!
! indxTVmix
!
          lvar=lenstr(vname(1,indxTVmix+itrc-1))
          ierr=nf_inq_varid (ncid, vname(1,indxTVmix+itrc-1)(1:lvar),
     &                                               vidTVmix(itrc))
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxTVmix+itrc-1)(1:lvar),
     &                                         ncname(1:lstr) 
            goto 99                                       !--> ERROR
          endif
!
! indxTForc
!
          lvar=lenstr(vname(1,indxTForc+itrc-1))
          ierr=nf_inq_varid (ncid, vname(1,indxTForc+itrc-1)(1:lvar),
     &                                               vidTForc(itrc))
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxTForc+itrc-1)(1:lvar),
     &                                         ncname(1:lstr) 
            goto 99                                       !--> ERROR
          endif
!
! indxTrate
!
          lvar=lenstr(vname(1,indxTrate+itrc-1))
          ierr=nf_inq_varid (ncid, vname(1,indxTrate+itrc-1)(1:lvar),
     &                                               vidTrate(itrc))
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxTrate+itrc-1)(1:lvar),
     &                                         ncname(1:lstr) 
            goto 99                                       !--> ERROR
          endif
!
        endif
!
#ifdef DIAGNOSTICS_TS_MLD

        if (wrt2D(itrc)) then
!
! indxTXadv on MLD
!
        lvar=lenstr(vname(1,indxTXadv_mld+itrc-1))
        ierr=nf_inq_varid (ncid,vname(1,indxTXadv_mld+itrc-1)(1:lvar),
     &                                           vidTXadv_mld(itrc))
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxTXadv_mld+itrc-1)(1:lvar),
     &                                         ncname(1:lstr) 
            goto 99                                       !--> ERROR
          endif
!
! indxTYadv on MLD
!
        lvar=lenstr(vname(1,indxTYadv_mld+itrc-1))
        ierr=nf_inq_varid (ncid,vname(1,indxTYadv_mld+itrc-1)(1:lvar),
     &                                           vidTYadv_mld(itrc))
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxTYadv_mld+itrc-1)(1:lvar),
     &                                         ncname(1:lstr) 
            goto 99                                       !--> ERROR
          endif
!
! indxTVadv on MLD
!
        lvar=lenstr(vname(1,indxTVadv_mld+itrc-1))
        ierr=nf_inq_varid (ncid,vname(1,indxTVadv_mld+itrc-1)(1:lvar),
     &                                           vidTVadv_mld(itrc))
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxTVadv_mld+itrc-1)(1:lvar),
     &                                         ncname(1:lstr) 
            goto 99                                       !--> ERROR
          endif
!
! indxTHmix on MLD
!
        lvar=lenstr(vname(1,indxTHmix_mld+itrc-1))
        ierr=nf_inq_varid (ncid,vname(1,indxTHmix_mld+itrc-1)(1:lvar),
     &                                          vidTHmix_mld(itrc))
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxTHmix_mld+itrc-1)(1:lvar),
     &                                         ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
!
! indxTVmix on MLD
!
        lvar=lenstr(vname(1,indxTVmix_mld+itrc-1))
        ierr=nf_inq_varid (ncid,vname(1,indxTVmix_mld+itrc-1)(1:lvar),
     &                                          vidTVmix_mld(itrc))
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxTVmix_mld+itrc-1)(1:lvar),
     &                                         ncname(1:lstr) 
            goto 99                                       !--> ERROR
          endif
!
! indxTForc on MLD
!
        lvar=lenstr(vname(1,indxTForc_mld+itrc-1))
        ierr=nf_inq_varid (ncid,vname(1,indxTForc_mld+itrc-1)(1:lvar),
     &                                           vidTForc_mld(itrc))
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxTForc_mld+itrc-1)(1:lvar),
     &                                         ncname(1:lstr) 
            goto 99                                       !--> ERROR
          endif
!
! indxTrate on MLD
!
        lvar=lenstr(vname(1,indxTrate_mld+itrc-1))
        ierr=nf_inq_varid (ncid,vname(1,indxTrate_mld+itrc-1)(1:lvar),
     &                                      vidTrate_mld(itrc))
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxTrate_mld+itrc-1)(1:lvar),
     &                                         ncname(1:lstr) 
            goto 99                                       !--> ERROR
          endif
!
! indxTentr on MLD
!
        lvar=lenstr(vname(1,indxTentr_mld+itrc-1))
        ierr=nf_inq_varid (ncid,vname(1,indxTentr_mld+itrc-1)(1:lvar),
     &                                        vidTentr_mld(itrc))
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxTentr_mld+itrc-1)(1:lvar),
     &                                         ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
!
        endif
!
#endif /*DIAGNOSTICS_TS_MLD*/
!
      enddo
!
        MPI_master_only write(*,'(6x,2A,i4,1x,A,i4)') 
     &                     'DEF_DIAG -- Opened ',
     &                     'existing file  from record =', rec
     &                      MYID
!
!
!
        MPI_master_only write(*,'(6x,2A,i4,1x,A,i4)') 
     &                     'DEF_DIAG -- Opened ',
     &                     'existing file  from record =', rec
     &                      MYID
!
#if defined MPI & !defined PARALLEL_FILES
      else
        ierr=nf_open (ncname(1:lstr), nf_write, ncid)
        if (ierr .ne. nf_noerr) then
        MPI_master_only  write(stdout,'(/1x,4A,2x,A,I4/)') 
     &                'DEF_DIAG ERROR: ',
     &                'Cannot open file ''', ncname(1:lstr), '''.'
     &                 MYID
          goto 99                                         !--> ERROR
        endif
#endif
!
      endif             !<--  create_new_file  
!
      ierr=nf_set_fill (ncid, nf_nofill, lvar)
      if (ierr .ne. nf_noerr) then
       write(*,'(6x,2A,i4,1x,A,i4)') 'DEF_DIAG ERROR: Cannot ', 
     &    'switch to ''nf_nofill'' more; netCDF error code =', ierr 
      endif
   1  format(/1x,'DEF_DIAG ERROR: Cannot find variable ''',
     &                   A, ''' in netCDF file ''', A, '''.'/)
!
#if (defined PUT_GRID_INTO_HISTORY && !defined AVRH)\
 || (defined PUT_GRID_INTO_AVERAGES && defined AVRH)
!
! Write grid variables.
! ----- ---- ----------
!
      if (total_rec.le.1) call wrt_grid (ncid, ncname, lstr)
#endif
#if defined AVRH
!
! Set initial average timedia.
! --- ------- ------- --------
!
      if (ntsdia_avg.eq.1) then
        timedia_avg=time-0.5*float(nwrtdia_avg)*dt
      else
        timedia_avg=time-0.5*float(nwrtdia_avg)*dt+float(ntsdia_avg)*dt
      endif
#endif
!
  11  format(/' DEF_DIAGS - unable to create diag file: ',a)
  20  format(/' DEF_DIAGS - error while writing variable: ',a,
     &        /,15x,'into diag  file: ',a)
!
  99  return
      end
!
#undef ncname
#undef rec_per_file
#undef wrt3D
#undef vidTime
#undef vidTime2
#undef vidTstep
!
#undef vidTXadv
#undef vidTYadv
#undef vidTVadv
#undef vidTHmix
#undef vidTVmix
#undef vidTForc
#undef vidTrate
!
#ifdef DIAGNOSTICS_TS_MLD
#undef wrt2D
#undef vidTXadv_mld
#undef vidTYadv_mld
#undef vidTVadv_mld
#undef vidTHmix_mld
#undef vidTVmix_mld
#undef vidTForc_mld
#undef vidTrate_mld
#undef vidTentr_mld
#endif /*DIAGNOSTICS_TS_MLD*/
!
# ifdef AVERAGES
#  ifndef AVRH
#   define AVRH
#   include "def_diags.F"
#   endif
#  endif /* AVERAGES */
!
#else /* DIAGNOSTICS_TS*/
      subroutine def_diags_empty()
      return
      end
#endif /* DIAGNOSTICS_TS*/

