! $Id: def_diagsM.F 1468 2014-02-10 12:58:35Z rblod $
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
#if defined DIAGNOSTICS_UV
# ifndef AVRH                  
      subroutine def_diagsM(ncid,total_rec,ierr) 
# else
      subroutine def_diagsM_avg(ncid,total_rec,ierr)
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
     &      , r3dgrd(4),u3dgrd(4),v3dgrd(4),w3dgrd(4),itrc
# endif
!
# ifndef AVRH
!
#  define ncname dianameM
#  define rec_per_file nrpfdiaM
#  define wrt wrtdiaM
#  define vidTime diaTimeM
#  define vidTime2 diaTime2M
#  define vidTstep diaTstepM
!
#  define vidMXadv diaMXadv
#  define vidMYadv diaMYadv
#  define vidMVadv diaMVadv
#  define vidMCor diaMCor
#  define vidMPrsgrd diaMPrsgrd
#  define vidMHmix diaMHmix
#  define vidMVmix diaMVmix
#  define vidMrate diaMrate
#  if defined MRL_WCI
#   define vidMvf diaMvf
#   define vidMbrk diaMbrk
#   define vidMStCo diaMStCo
#   define vidMVvf diaMVvf
#   define vidMPrscrt diaMPrscrt
#   define vidMsbk diaMsbk
#   define vidMbwf diaMbwf
#   define vidMfrc diaMfrc
#  endif 
!
# else /* AVRH */
!
#  define ncname dianameM_avg
#  define rec_per_file nrpfdiaM_avg
#  define wrt wrtdiaM_avg
#  define vidTime diaTimeM_avg
#  define vidTime2 diaTime2M_avg
#  define vidTstep diaTstepM_avg
!
#  define vidMXadv diaMXadv_avg
#  define vidMYadv diaMYadv_avg
#  define vidMVadv diaMVadv_avg
#  define vidMCor diaMCor_avg
#  define vidMPrsgrd diaMPrsgrd_avg
#  define vidMHmix diaMHmix_avg
#  define vidMVmix diaMVmix_avg
#  define vidMrate diaMrate_avg
#  if defined MRL_WCI
#   define vidMvf diaMvf_avg
#   define vidMbrk diaMbrk_avg
#   define vidMStCo diaMStCo_avg
#   define vidMVvf diaMVvf_avg
#   define vidMPrscrt diaMPrscrt_avg
#   define vidMsbk diaMsbk_avg
#   define vidMbwf diaMbwf_avg
#   define vidMfrc diaMfrc_avg
#  endif 
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
      create_new_file=ldefdiaM
#else
      create_new_file=ldefdiaM_avg
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
!     Define grid variables :
! ------ ---- ---------
!
      if (total_rec.le.1) then
# ifdef SOLVE3D
         call def_grid_3d(ncid, r2dgrd, u2dgrd, v2dgrd 
     &                   ,r3dgrd, w3dgrd)
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
        ierr=nf_put_att_text (ncid, vidTstep, 'long_name', 48,
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
     &       lvar, vname(2,indxTime)(1:lvar))
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

! Momentum diagnostics variables.
! -------- ----------- --------

!
        do itrc=1,2  ! use itrc indice = 1 for u_momentum
!                    !                 = 2 for v_momentum 
          if (wrt(itrc)) then
!
! indxMXadv
!
          if (itrc.eq.1) then
            r3dgrd=u3dgrd
          else
            r3dgrd=v3dgrd
          endif
!
          lvar=lenstr(vname(1,indxMXadv+itrc-1))
          ierr=nf_def_var (ncid, vname(1,indxMXadv+itrc-1)(1:lvar),
     &                     NF_FOUT, 4, r3dgrd, vidMXadv(itrc))
# ifndef AVRH
          lvar=lenstr(vname(2,indxMXadv+itrc-1))
          ierr=nf_put_att_text (ncid, vidMXadv(itrc), 'long_name',
     &                      lvar, vname(2,indxMXadv+itrc-1)(1:lvar))
# else
          text='averaged '/ /vname(2,indxMXadv+itrc-1)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidMXadv(itrc), 'long_name', 
     &                          lvar, text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxMXadv+itrc-1))
          ierr=nf_put_att_text (ncid, vidMXadv(itrc), 'units', lvar,
     &                          vname(3,indxMXadv+itrc-1)(1:lvar))
          lvar=lenstr(vname(4,indxMXadv+itrc-1))
          ierr=nf_put_att_text (ncid,vidMXadv(itrc), 'field',
     &                      lvar, vname(4,indxMXadv+itrc-1)(1:lvar))

        call nf_add_attribute(ncid, vidMXadv(itrc), indxMXadv+itrc-1, 5, 
     &                        NF_FOUT, ierr)
!
! indxMYadv
!
          lvar=lenstr(vname(1,indxMYadv+itrc-1))
          ierr=nf_def_var (ncid, vname(1,indxMYadv+itrc-1)(1:lvar),
     &                     NF_FOUT, 4, r3dgrd, vidMYadv(itrc))
# ifndef AVRH
          lvar=lenstr(vname(2,indxMYadv+itrc-1))
          ierr=nf_put_att_text (ncid, vidMYadv(itrc), 'long_name',
     &                     lvar, vname(2,indxMYadv+itrc-1)(1:lvar))
# else
          text='averaged '/ /vname(2,indxMYadv+itrc-1)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidMYadv(itrc), 'long_name', 
     &                          lvar, text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxMYadv+itrc-1))
          ierr=nf_put_att_text (ncid, vidMYadv(itrc), 'units', lvar,
     &                          vname(3,indxMYadv+itrc-1)(1:lvar))
          lvar=lenstr(vname(4,indxMYadv+itrc-1))
          ierr=nf_put_att_text (ncid,vidMYadv(itrc), 'field',
     &                      lvar, vname(4,indxMYadv+itrc-1)(1:lvar))

        call nf_add_attribute(ncid, vidMYadv(itrc), indxMYadv+itrc-1, 5, 
     &                        NF_FOUT, ierr)
!
! indxMVadv
!
          lvar=lenstr(vname(1,indxMVadv+itrc-1))
          ierr=nf_def_var (ncid, vname(1,indxMVadv+itrc-1)(1:lvar),
     &                     NF_FOUT, 4, r3dgrd, vidMVadv(itrc))
# ifndef AVRH
          lvar=lenstr(vname(2,indxMVadv+itrc-1))
          ierr=nf_put_att_text (ncid, vidMVadv(itrc), 'long_name',
     &                    lvar, vname(2,indxMVadv+itrc-1)(1:lvar))
# else
          text='averaged '/ /vname(2,indxMVadv+itrc-1)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidMVadv(itrc), 'long_name', 
     &                          lvar, text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxMVadv+itrc-1))
          ierr=nf_put_att_text (ncid, vidMVadv(itrc), 'units', lvar,
     &                          vname(3,indxMVadv+itrc-1)(1:lvar))
          lvar=lenstr(vname(4,indxMVadv+itrc-1))
          ierr=nf_put_att_text (ncid,vidMVadv(itrc), 'field',
     &                      lvar, vname(4,indxMVadv+itrc-1)(1:lvar))

        call nf_add_attribute(ncid, vidMVadv(itrc), indxMVadv+itrc-1, 5, 
     &                        NF_FOUT, ierr)
!
! indxMCor
!
          lvar=lenstr(vname(1,indxMCor+itrc-1))
          ierr=nf_def_var (ncid, vname(1,indxMCor+itrc-1)(1:lvar),
     &                     NF_FOUT, 4, r3dgrd, vidMCor(itrc))
# ifndef AVRH
          lvar=lenstr(vname(2,indxMCor+itrc-1))
          ierr=nf_put_att_text (ncid, vidMCor(itrc), 'long_name',
     &                      lvar, vname(2,indxMCor+itrc-1)(1:lvar))
# else
          text='averaged '/ /vname(2,indxMCor+itrc-1)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidMCor(itrc), 'long_name',
     &                          lvar, text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxMCor+itrc-1))
          ierr=nf_put_att_text (ncid, diaMCor(itrc), 'units', lvar,
     &                          vname(3,indxMCor+itrc-1)(1:lvar))
          lvar=lenstr(vname(4,indxMCor+itrc-1))
          ierr=nf_put_att_text (ncid,vidMCor(itrc), 'field',
     &                      lvar, vname(4,indxMCor+itrc-1)(1:lvar))

        call nf_add_attribute(ncid, vidMCor(itrc), indxMCor+itrc-1, 5, 
     &                        NF_FOUT, ierr)
!
! indxMPrsgrd
!
          lvar=lenstr(vname(1,indxMPrsgrd+itrc-1))
          ierr=nf_def_var (ncid, vname(1,indxMPrsgrd+itrc-1)(1:lvar),
     &                     NF_FOUT, 4, r3dgrd, vidMPrsgrd(itrc))
# ifndef AVRH
          lvar=lenstr(vname(2,indxMPrsgrd+itrc-1))
          ierr=nf_put_att_text (ncid, vidMPrsgrd(itrc), 'long_name',
     &                      lvar, vname(2,indxMPrsgrd+itrc-1)(1:lvar))
# else
          text='averaged '/ /vname(2,indxMPrsgrd+itrc-1)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidMPrsgrd(itrc), 'long_name',
     &                          lvar, text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxMPrsgrd+itrc-1))
          ierr=nf_put_att_text (ncid, diaMPrsgrd(itrc), 'units', lvar,
     &                          vname(3,indxMPrsgrd+itrc-1)(1:lvar))
          lvar=lenstr(vname(4,indxMPrsgrd+itrc-1))
          ierr=nf_put_att_text (ncid,vidMPrsgrd(itrc), 'field',
     &                      lvar, vname(4,indxMPrsgrd+itrc-1)(1:lvar))

        call nf_add_attribute(ncid, vidMPrsgrd(itrc),indxMPrsgrd+itrc-1, 
     &                        5, NF_FOUT, ierr)
!
! indxMHmix
!
          lvar=lenstr(vname(1,indxMHmix+itrc-1))
          ierr=nf_def_var (ncid, vname(1,indxMHmix+itrc-1)(1:lvar),
     &                     NF_FOUT, 4, r3dgrd, vidMHmix(itrc))
# ifndef AVRH
          lvar=lenstr(vname(2,indxMHmix+itrc-1))
          ierr=nf_put_att_text (ncid, vidMHmix(itrc), 'long_name',
     &                      lvar, vname(2,indxMHmix+itrc-1)(1:lvar))
# else
          text='averaged '/ /vname(2,indxMHmix+itrc-1)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidMHmix(itrc), 'long_name',
     &                          lvar, text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxMHmix+itrc-1))
          ierr=nf_put_att_text (ncid, diaMHmix(itrc), 'units', lvar,
     &                          vname(3,indxMHmix+itrc-1)(1:lvar))
          lvar=lenstr(vname(4,indxMHmix+itrc-1))
          ierr=nf_put_att_text (ncid,vidMHmix(itrc), 'field',
     &                      lvar, vname(4,indxMHmix+itrc-1)(1:lvar))

        call nf_add_attribute(ncid, vidMHmix(itrc), indxMHmix+itrc-1, 5, 
     &                        NF_FOUT, ierr)
!
!indxMVmix
!
          lvar=lenstr(vname(1,indxMVmix+itrc-1))
          ierr=nf_def_var (ncid, vname(1,indxMVmix+itrc-1)(1:lvar),
     &                     NF_FOUT, 4, r3dgrd, vidMVmix(itrc))
# ifndef AVRH
          lvar=lenstr(vname(2,indxMVmix+itrc-1))
          ierr=nf_put_att_text (ncid, vidMVmix(itrc), 'long_name',
     &                    lvar, vname(2,indxMVmix+itrc-1)(1:lvar))
# else
          text='averaged '/ /vname(2,indxMVmix+itrc-1)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidMVmix(itrc), 'long_name', 
     &                          lvar, text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxMVmix+itrc-1))
          ierr=nf_put_att_text (ncid, diaMVmix(itrc), 'units', lvar,
     &                          vname(3,indxMVmix+itrc-1)(1:lvar))
          lvar=lenstr(vname(4,indxMVmix+itrc-1))
          ierr=nf_put_att_text (ncid,vidMVmix(itrc), 'field',
     &                      lvar, vname(4,indxMVmix+itrc-1)(1:lvar))

        call nf_add_attribute(ncid, vidMVmix(itrc), indxMVmix+itrc-1, 5, 
     &                        NF_FOUT, ierr)
#ifdef MRL_WCI
!
! indxMvf
!
          lvar=lenstr(vname(1,indxMvf+itrc-1))
          ierr=nf_def_var (ncid, vname(1,indxMvf+itrc-1)(1:lvar),
     &                     NF_FOUT, 4, r3dgrd, vidMvf(itrc))
# ifndef AVRH
          lvar=lenstr(vname(2,indxMvf+itrc-1))
          ierr=nf_put_att_text (ncid, vidMvf(itrc), 'long_name',
     &                    lvar, vname(2,indxMvf+itrc-1)(1:lvar))
# else
          text='averaged '/ /vname(2,indxMvf+itrc-1)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidMvf(itrc), 'long_name', 
     &                          lvar, text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxMvf+itrc-1))
          ierr=nf_put_att_text (ncid, diaMvf(itrc), 'units', lvar,
     &                          vname(3,indxMvf+itrc-1)(1:lvar))
          lvar=lenstr(vname(4,indxMvf+itrc-1))
          ierr=nf_put_att_text (ncid,vidMvf(itrc), 'field',
     &                      lvar, vname(4,indxMvf+itrc-1)(1:lvar))

        call nf_add_attribute(ncid, vidMvf(itrc), indxMvf+itrc-1, 5, 
     &                        NF_FOUT, ierr)
!
! indxMbrk
!
          lvar=lenstr(vname(1,indxMbrk+itrc-1))
          ierr=nf_def_var (ncid, vname(1,indxMbrk+itrc-1)(1:lvar),
     &                     NF_FOUT, 4, r3dgrd, vidMbrk(itrc))
# ifndef AVRH
          lvar=lenstr(vname(2,indxMbrk+itrc-1))
          ierr=nf_put_att_text (ncid, vidMbrk(itrc), 'long_name',
     &                    lvar, vname(2,indxMbrk+itrc-1)(1:lvar))
# else
          text='averaged '/ /vname(2,indxMbrk+itrc-1)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidMbrk(itrc), 'long_name', 
     &                          lvar, text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxMbrk+itrc-1))
          ierr=nf_put_att_text (ncid, diaMbrk(itrc), 'units', lvar,
     &                          vname(3,indxMbrk+itrc-1)(1:lvar))
          lvar=lenstr(vname(4,indxMbrk+itrc-1))
          ierr=nf_put_att_text (ncid,vidMbrk(itrc), 'field',
     &                      lvar, vname(4,indxMbrk+itrc-1)(1:lvar))

        call nf_add_attribute(ncid, vidMbrk(itrc), indxMbrk+itrc-1, 5, 
     &                        NF_FOUT, ierr)
!
! indxMStCo
!
          lvar=lenstr(vname(1,indxMStCo+itrc-1))
          ierr=nf_def_var (ncid, vname(1,indxMStCo+itrc-1)(1:lvar),
     &                     NF_FOUT, 4, r3dgrd, vidMStCo(itrc))
# ifndef AVRH
          lvar=lenstr(vname(2,indxMStCo+itrc-1))
          ierr=nf_put_att_text (ncid, vidMStCo(itrc), 'long_name',
     &                    lvar, vname(2,indxMStCo+itrc-1)(1:lvar))
# else
          text='averaged '/ /vname(2,indxMStCo+itrc-1)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidMStCo(itrc), 'long_name', 
     &                          lvar, text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxMStCo+itrc-1))
          ierr=nf_put_att_text (ncid, diaMStCo(itrc), 'units', lvar,
     &                          vname(3,indxMStCo+itrc-1)(1:lvar))
          lvar=lenstr(vname(4,indxMStCo+itrc-1))
          ierr=nf_put_att_text (ncid,vidMStCo(itrc), 'field',
     &                      lvar, vname(4,indxMStCo+itrc-1)(1:lvar))

        call nf_add_attribute(ncid, vidMStCo(itrc), indxMStCo+itrc-1, 5, 
     &                        NF_FOUT, ierr)
!
! indxMVvf
!
          lvar=lenstr(vname(1,indxMVvf+itrc-1))
          ierr=nf_def_var (ncid, vname(1,indxMVvf+itrc-1)(1:lvar),
     &                     NF_FOUT, 4, r3dgrd, vidMVvf(itrc))
# ifndef AVRH
          lvar=lenstr(vname(2,indxMVvf+itrc-1))
          ierr=nf_put_att_text (ncid, vidMVvf(itrc), 'long_name',
     &                    lvar, vname(2,indxMVvf+itrc-1)(1:lvar))
# else
          text='averaged '/ /vname(2,indxMVvf+itrc-1)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidMVvf(itrc), 'long_name', 
     &                          lvar, text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxMVvf+itrc-1))
          ierr=nf_put_att_text (ncid, diaMVvf(itrc), 'units', lvar,
     &                          vname(3,indxMVvf+itrc-1)(1:lvar))
          lvar=lenstr(vname(4,indxMVvf+itrc-1))
          ierr=nf_put_att_text (ncid,vidMVvf(itrc), 'field',
     &                      lvar, vname(4,indxMVvf+itrc-1)(1:lvar))

        call nf_add_attribute(ncid, vidMVvf(itrc), indxMVvf+itrc-1, 5, 
     &                        NF_FOUT, ierr)
!
! indxMPrscrt
!
          lvar=lenstr(vname(1,indxMPrscrt+itrc-1))
          ierr=nf_def_var (ncid, vname(1,indxMPrscrt+itrc-1)(1:lvar),
     &                     NF_FOUT, 4, r3dgrd, vidMPrscrt(itrc))
# ifndef AVRH
          lvar=lenstr(vname(2,indxMPrscrt+itrc-1))
          ierr=nf_put_att_text (ncid, vidMPrscrt(itrc), 'long_name',
     &                    lvar, vname(2,indxMPrscrt+itrc-1)(1:lvar))
# else
          text='averaged '/ /vname(2,indxMPrscrt+itrc-1)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidMPrscrt(itrc), 'long_name', 
     &                          lvar, text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxMPrscrt+itrc-1))
          ierr=nf_put_att_text (ncid, diaMPrscrt(itrc), 'units', lvar,
     &                          vname(3,indxMPrscrt+itrc-1)(1:lvar))
          lvar=lenstr(vname(4,indxMPrscrt+itrc-1))
          ierr=nf_put_att_text (ncid,vidMPrscrt(itrc), 'field',
     &                      lvar, vname(4,indxMPrscrt+itrc-1)(1:lvar))

        call nf_add_attribute(ncid, vidMPrscrt(itrc), indxMPrscrt+itrc-1, 5, 
     &                        NF_FOUT, ierr)
!
! indxMsbk
!
          lvar=lenstr(vname(1,indxMsbk+itrc-1))
          ierr=nf_def_var (ncid, vname(1,indxMsbk+itrc-1)(1:lvar),
     &                     NF_FOUT, 4, r3dgrd, vidMsbk(itrc))
# ifndef AVRH
          lvar=lenstr(vname(2,indxMsbk+itrc-1))
          ierr=nf_put_att_text (ncid, vidMsbk(itrc), 'long_name',
     &                    lvar, vname(2,indxMsbk+itrc-1)(1:lvar))
# else
          text='averaged '/ /vname(2,indxMsbk+itrc-1)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidMsbk(itrc), 'long_name', 
     &                          lvar, text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxMsbk+itrc-1))
          ierr=nf_put_att_text (ncid, diaMsbk(itrc), 'units', lvar,
     &                          vname(3,indxMsbk+itrc-1)(1:lvar))
          lvar=lenstr(vname(4,indxMsbk+itrc-1))
          ierr=nf_put_att_text (ncid,vidMsbk(itrc), 'field',
     &                      lvar, vname(4,indxMsbk+itrc-1)(1:lvar))

        call nf_add_attribute(ncid, vidMsbk(itrc), indxMsbk+itrc-1, 5, 
     &                        NF_FOUT, ierr)
!
! indxMbwf
!
          lvar=lenstr(vname(1,indxMbwf+itrc-1))
          ierr=nf_def_var (ncid, vname(1,indxMbwf+itrc-1)(1:lvar),
     &                     NF_FOUT, 4, r3dgrd, vidMbwf(itrc))
# ifndef AVRH
          lvar=lenstr(vname(2,indxMbwf+itrc-1))
          ierr=nf_put_att_text (ncid, vidMbwf(itrc), 'long_name',
     &                    lvar, vname(2,indxMbwf+itrc-1)(1:lvar))
# else
          text='averaged '/ /vname(2,indxMbwf+itrc-1)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidMbwf(itrc), 'long_name', 
     &                          lvar, text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxMbwf+itrc-1))
          ierr=nf_put_att_text (ncid, diaMbwf(itrc), 'units', lvar,
     &                          vname(3,indxMbwf+itrc-1)(1:lvar))
          lvar=lenstr(vname(4,indxMbwf+itrc-1))
          ierr=nf_put_att_text (ncid,vidMbwf(itrc), 'field',
     &                      lvar, vname(4,indxMbwf+itrc-1)(1:lvar))

        call nf_add_attribute(ncid, vidMbwf(itrc), indxMbwf+itrc-1, 5, 
     &                        NF_FOUT, ierr)
!
! indxMfrc
!
          lvar=lenstr(vname(1,indxMfrc+itrc-1))
          ierr=nf_def_var (ncid, vname(1,indxMfrc+itrc-1)(1:lvar),
     &                     NF_FOUT, 4, r3dgrd, vidMfrc(itrc))
# ifndef AVRH
          lvar=lenstr(vname(2,indxMfrc+itrc-1))
          ierr=nf_put_att_text (ncid, vidMfrc(itrc), 'long_name',
     &                    lvar, vname(2,indxMfrc+itrc-1)(1:lvar))
# else
          text='averaged '/ /vname(2,indxMfrc+itrc-1)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidMfrc(itrc), 'long_name', 
     &                          lvar, text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxMfrc+itrc-1))
          ierr=nf_put_att_text (ncid, diaMfrc(itrc), 'units', lvar,
     &                          vname(3,indxMfrc+itrc-1)(1:lvar))
          lvar=lenstr(vname(4,indxMfrc+itrc-1))
          ierr=nf_put_att_text (ncid,vidMfrc(itrc), 'field',
     &                      lvar, vname(4,indxMfrc+itrc-1)(1:lvar))

        call nf_add_attribute(ncid, vidMfrc(itrc), indxMfrc+itrc-1, 5, 
     &                        NF_FOUT, ierr)
#endif
!
! Mrate indxMrate
!
          lvar=lenstr(vname(1,indxMrate+itrc-1))
          ierr=nf_def_var (ncid, vname(1,indxMrate+itrc-1)(1:lvar),
     &                     NF_FOUT, 4, r3dgrd, vidMrate(itrc))
# ifndef AVRH
          lvar=lenstr(vname(2,indxMrate+itrc-1))
          ierr=nf_put_att_text (ncid, vidMrate(itrc), 'long_name', 
     &                    lvar, vname(2,indxMrate+itrc-1)(1:lvar))
# else
          text='averaged '/ /vname(2,indxMrate+itrc-1)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidMrate(itrc), 'long_name', 
     &                          lvar, text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxMrate+itrc-1))
          ierr=nf_put_att_text (ncid, vidMrate(itrc),'units',
     &                    lvar, vname(3,indxMrate+itrc-1)(1:lvar))
          lvar=lenstr(vname(4,indxMrate+itrc-1))
          ierr=nf_put_att_text (ncid, vidMrate(itrc),'field',
     &                    lvar, vname(4,indxMrate+itrc-1)(1:lvar))

        call nf_add_attribute(ncid, vidMrate(itrc), indxMrate+itrc-1, 5, 
     &                        NF_FOUT, ierr)
!
        endif
      enddo
!
! Leave definition mode.
! ----- ---------- -----
!
        ierr=nf_enddef(ncid)
        write(stdout,'(6x,4A,1x,A,i4)') 
#ifdef AVRH
     &        'DEF_DIAGM_AVG - Created ',
#else      
     &        'DEF_DIAGM - Created ',
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
     &     ) 'DEF_DIAGSM/DIAGSM_AVG WARNING: Actual number of records',
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
        write(stdout,'(/1x,4A,2x,A,I4/)') 'DEF_DIAGSM_HIS/AVG ERROR: ',
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

! momentum flux diagnostics variables :
! -------- ---- ----------- ----------
!
!
        do itrc=1,2  ! use itrc indice = 1 for u_momentum
!                    !                 = 2 for v_momentum 
          if (wrt(itrc)) then
! indxMXadv
!
         lvar=lenstr(vname(1,indxMXadv+itrc-1))
         ierr=nf_inq_varid (ncid, vname(1,indxMXadv+itrc-1)(1:lvar),
     &                      vidMXadv(itrc))
         if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxMXadv+itrc-1)(1:lvar),
     &                     ncname(1:lstr) 
           goto 99                                       !--> ERROR
         endif
!
! indxMYadv
!
         lvar=lenstr(vname(1,indxMYadv+itrc-1))
         ierr=nf_inq_varid (ncid, vname(1,indxMYadv+itrc-1)(1:lvar),
     &                      vidMYadv(itrc))
         if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxMYadv+itrc-1)(1:lvar),
     &                     ncname(1:lstr) 
           goto 99                                       !--> ERROR
         endif                               
!
! indxMVadv
!
         lvar=lenstr(vname(1,indxMVadv+itrc-1))
         ierr=nf_inq_varid (ncid, vname(1,indxMVadv+itrc-1)(1:lvar),
     &                      vidMVadv(itrc))
         if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxMVadv+itrc-1)(1:lvar),
     &                     ncname(1:lstr) 
           goto 99                                       !--> ERROR
         endif
!
! indxMCor
!
         lvar=lenstr(vname(1,indxMCor+itrc-1))
         ierr=nf_inq_varid (ncid, vname(1,indxMCor+itrc-1)(1:lvar),
     &                      vidMCor(itrc))
         if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxMCor+itrc-1)(1:lvar),
     &                     ncname(1:lstr)
           goto 99                                       !--> ERROR
         endif
!
! indxMPrsgrd
!
         lvar=lenstr(vname(1,indxMPrsgrd+itrc-1))
         ierr=nf_inq_varid (ncid, vname(1,indxMPrsgrd+itrc-1)(1:lvar),
     &                      vidMPrsgrd(itrc))
         if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxMPrsgrd+itrc-1)(1:lvar),
     &                     ncname(1:lstr)
           goto 99                                       !--> ERROR
         endif
!
! indxMHmix
!
         lvar=lenstr(vname(1,indxMHmix+itrc-1))
         ierr=nf_inq_varid (ncid, vname(1,indxMHmix+itrc-1)(1:lvar),
     &                      vidMHmix(itrc))
         if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxMHmix+itrc-1)(1:lvar),
     &                     ncname(1:lstr)
           goto 99                                       !--> ERROR
         endif
!
! indxMVmix
!
         lvar=lenstr(vname(1,indxMVmix+itrc-1))
         ierr=nf_inq_varid (ncid, vname(1,indxMVmix+itrc-1)(1:lvar),
     &                      vidMVmix(itrc))
         if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxMVmix+itrc-1)(1:lvar),
     &                     ncname(1:lstr) 
           goto 99                                       !--> ERROR
         endif
#if defined MRL_WCI
!
! indxMvf
!
         lvar=lenstr(vname(1,indxMvf+itrc-1))
         ierr=nf_inq_varid (ncid, vname(1,indxMvf+itrc-1)(1:lvar),
     &                      vidMvf(itrc))
         if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxMvf+itrc-1)(1:lvar),
     &                     ncname(1:lstr) 
           goto 99                                       !--> ERROR
         endif
!
! indxMbrk
!
         lvar=lenstr(vname(1,indxMbrk+itrc-1))
         ierr=nf_inq_varid (ncid, vname(1,indxMbrk+itrc-1)(1:lvar),
     &                      vidMbrk(itrc))
         if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxMbrk+itrc-1)(1:lvar),
     &                     ncname(1:lstr) 
           goto 99                                       !--> ERROR
         endif
!
! indxMStCo
!
         lvar=lenstr(vname(1,indxMStCo+itrc-1))
         ierr=nf_inq_varid (ncid, vname(1,indxMStCo+itrc-1)(1:lvar),
     &                      vidMStCo(itrc))
         if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxMStCo+itrc-1)(1:lvar),
     &                     ncname(1:lstr) 
           goto 99                                       !--> ERROR
         endif
!
! indxMVvf
!
         lvar=lenstr(vname(1,indxMVvf+itrc-1))
         ierr=nf_inq_varid (ncid, vname(1,indxMVvf+itrc-1)(1:lvar),
     &                      vidMVvf(itrc))
         if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxMVvf+itrc-1)(1:lvar),
     &                     ncname(1:lstr) 
           goto 99                                       !--> ERROR
         endif
!
! indxMPrscrt
!
         lvar=lenstr(vname(1,indxMPrscrt+itrc-1))
         ierr=nf_inq_varid (ncid, vname(1,indxMPrscrt+itrc-1)(1:lvar),
     &                      vidMPrscrt(itrc))
         if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxMPrscrt+itrc-1)(1:lvar),
     &                     ncname(1:lstr) 
           goto 99                                       !--> ERROR
         endif
!
! indxMsbk
!
         lvar=lenstr(vname(1,indxMsbk+itrc-1))
         ierr=nf_inq_varid (ncid, vname(1,indxMsbk+itrc-1)(1:lvar),
     &                      vidMsbk(itrc))
         if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxMsbk+itrc-1)(1:lvar),
     &                     ncname(1:lstr) 
           goto 99                                       !--> ERROR
         endif
!
! indxMbwf
!
         lvar=lenstr(vname(1,indxMbwf+itrc-1))
         ierr=nf_inq_varid (ncid, vname(1,indxMbwf+itrc-1)(1:lvar),
     &                      vidMbwf(itrc))
         if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxMbwf+itrc-1)(1:lvar),
     &                     ncname(1:lstr) 
           goto 99                                       !--> ERROR
         endif
!
! indxMfrc
!
         lvar=lenstr(vname(1,indxMfrc+itrc-1))
         ierr=nf_inq_varid (ncid, vname(1,indxMfrc+itrc-1)(1:lvar),
     &                      vidMfrc(itrc))
         if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxMfrc+itrc-1)(1:lvar),
     &                     ncname(1:lstr) 
           goto 99                                       !--> ERROR
         endif
#endif
!
! indxMrate
!
         lvar=lenstr(vname(1,indxMrate+itrc-1))
         ierr=nf_inq_varid (ncid, vname(1,indxMrate+itrc-1)(1:lvar),
     &                      vidMrate(itrc))
         if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxMrate+itrc-1)(1:lvar),
     &                     ncname(1:lstr) 
           goto 99                                       !--> ERROR
         endif
!
       endif
      enddo
!
        MPI_master_only write(*,'(6x,2A,i4,1x,A,i4)') 
     &                     'DEF_DIAGM -- Opened ',
     &                     'existing file  from record =', rec
     &                      MYID
!
        MPI_master_only write(*,'(6x,2A,i4,1x,A,i4)') 
     &                     'DEF_DIAGM -- Opened ',
     &                     'existing file  from record =', rec
     &                      MYID
!
#if defined MPI & !defined PARALLEL_FILES
      else
        ierr=nf_open (ncname(1:lstr), nf_write, ncid)
        if (ierr .ne. nf_noerr) then
          write(stdout,'(/1x,4A,2x,A,I4/)') 'DEF_DIAGM ERROR: ',
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
        write(*,'(6x,2A,i4,1x,A,i4)') 'DEF_DIAGM ERROR: Cannot ', 
     &    'switch to ''nf_nofill'' more; netCDF error code =', ierr 
      endif
   1  format(/1x,'DEF_DIAGM ERROR: Cannot find variable ''',
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
! Set initial average timediaM.
! --- ------- ------- --------
!
      if (ntsdiaM_avg.eq.1) then
        timediaM_avg=time-0.5*float(nwrtdiaM_avg)*dt
      else
        timediaM_avg=time-0.5*float(nwrtdiaM_avg)
     &                       *dt+float(ntsdiaM_avg)*dt
      endif
#endif
!
  11  format(/' DEF_DIAGSM - unable to create diag file: ',a)
  20  format(/' DEF_DIAGSM - error while writing variable: ',a,
     &        /,15x,'into diag  file: ',a)
!
  99  return
      end
!
#undef ncname
#undef rec_per_file
#undef wrt
#undef vidTime
#undef vidTime2
#undef vidTstep
!
#undef vidMXadv
#undef vidMYadv
#undef vidMVadv
#undef vidMCor
#undef vidMPrsgrd
#undef vidMHmix
#undef vidMVmix
#undef vidMrate
!
# ifdef AVERAGES
#  ifndef AVRH
#   define AVRH
#   include "def_diagsM.F"
#   endif
#  endif /* AVERAGES */
!
#else /*DIAGNOSTICS_UV */
      subroutine def_diagsM_empty()
      return
      end
#endif /* DIAGNOSTICS_UV */

