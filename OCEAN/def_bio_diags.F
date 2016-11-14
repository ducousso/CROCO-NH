! $Id: def_bio_diags.F 1571 2014-07-01 12:38:05Z gcambon $
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
#ifdef DIAGNOSTICS_BIO
#ifndef AVRH                  
      subroutine def_bio_diags(ncid,total_rec,ierr) 
#else
      subroutine def_bio_diags_avg(ncid,total_rec,ierr)

#endif
                                 ! Create diag data NetCDF file:
                                 ! Define its variables, dimensions,
                                 ! and attributes      
      implicit none
# include "param.h"
# include "mixing.h"
# include "ncscrum.h"
# include "scalars.h"
# include "strings.h"
# include "netcdf.inc"
!
      logical create_new_file
      integer ncid, total_rec, ierr, rec, lstr,lvar,lenstr, timedim
     &      , r2dgrd(3), u2dgrd(3), v2dgrd(3),auxil(2),checkdims 
#ifdef SOLVE3D
     &      , r3dgrd(4), u3dgrd(4), v3dgrd(4), w3dgrd(4), iflux
#endif
#ifndef AVRH
# define ncname dianamebio
# define rec_per_file nrpfdiabio
# define wrtFlux wrtdiabioFlux
# define wrtGasExc wrtdiabioGasExc
# define wrtVSink wrtdiabioVSink
# define vidTime diaTimebio
# define vidTime2 diaTime2bio
# define vidTstep diaTstepbio
# define vidFlux diabioFlux
# define vidGasExc diabioGasExc
# define vidVSink diabioVSink
# define vidSed hisSed
#else /* AVRH */
# define ncname dianamebio_avg
# define rec_per_file nrpfdiabio_avg
# define wrtFlux wrtdiabioFlux_avg
# define wrtGasExc wrtdiabioGasExc_avg
# define wrtVSink wrtdiabioVSink_avg
# define vidTime diaTimebio_avg
# define vidTime2 diaTime2bio_avg
# define vidTstep diaTstepbio_avg
# define vidFlux diabioFlux_avg
# define vidGasExc diabioGasExc_avg
# define vidVSink diabioVSink_avg
# define vidSed avgSed
        character*60 text
#endif /* AVRH */

      if (may_day_flag.ne.0) return      !-->  EXIT

      ierr=0
      lstr=lenstr(ncname)
      if (rec_per_file.gt.0) then
        lvar=total_rec-(1+mod(total_rec-1, rec_per_file))
        call insert_time_index (ncname, lstr, lvar, ierr)
        if (ierr .ne. 0) goto 99
      endif

!  Create a new diagnostics data file.
!-------------------------------------
!
#ifndef AVRH
      create_new_file = ldefdiabio
#else
      create_new_file = ldefdiabio_avg
#endif
      if (ncid.ne.-1) create_new_file=.false.
#if defined MPI & !defined PARALLEL_FILES
      if (mynode.gt.0) create_new_file=.false.
#endif

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


        auxil(2)=timedim
        r2dgrd(3)=timedim           ! Free surface

        u2dgrd(2)=r2dgrd(2)         ! 2D UBAR-type
        u2dgrd(3)=timedim

        v2dgrd(1)=r2dgrd(1)         ! 2D VBAR-type
        v2dgrd(3)=timedim
        
!        b3dgrd(1)=r2dgrd(1)         !
!        b3dgrd(2)=r2dgrd(2)         ! 3D BED-type
!        b3dgrd(4)=timedim           !

#ifdef SOLVE3D
        r3dgrd(1)=r2dgrd(1)         !
        r3dgrd(2)=r2dgrd(2)         ! 3D RHO-type
        r3dgrd(4)=timedim           !

        u3dgrd(1)=u2dgrd(1)         !
        u3dgrd(2)=r2dgrd(2)         ! 3D U-type
        u3dgrd(3)=r3dgrd(3)         !
        u3dgrd(4)=timedim

        v3dgrd(1)=r2dgrd(1)         !
        v3dgrd(2)=v2dgrd(2)         ! 3D V-type
        v3dgrd(3)=r3dgrd(3)         !
        v3dgrd(4)=timedim

        w3dgrd(1)=r2dgrd(1)         !
        w3dgrd(2)=r2dgrd(2)         ! 3D W-type
        w3dgrd(4)=timedim           !


#endif

#if (defined PUT_GRID_INTO_HISTORY && !defined AVRH)\
 || (defined PUT_GRID_INTO_AVERAGES && defined AVRH)
!
! Define grid variables.
! ------ ---- ----------
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
!  Define running parameters.
!----------------------------
!
!  Define variables and their attributes.
!-----------------------------------------------------------
!
!  Define time step and model time.
!
! Time step number and time record indices:
!
        ierr=nf_def_var (ncid, 'time_step', nf_int, 2, auxil,
     &                                                 vidTstep)
        ierr=nf_put_att_text (ncid, diaTstepbio, 'long_name', 48,
     &       'time step and record numbers from initialization')

!
! Time.
!
        lvar=lenstr(vname(1,indxTime))
        ierr=nf_def_var (ncid, vname(1,indxTime)(1:lvar),
     &                            NF_FOUT, 1, timedim, vidTime)
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
        lvar=lenstr(vname(2,indxTime))
        ierr=nf_put_att_text (ncid, vidTime, 'long_name', lvar,
     &                                vname(2,indxTime)(1:lvar))

        lvar=lenstr(vname(3,indxTime))
        ierr=nf_put_att_text (ncid, vidTime, 'units',  lvar,
     &                                vname(3,indxTime)(1:lvar))
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
        lvar=lenstr(vname(2,indxTime2))
        ierr=nf_put_att_text (ncid, vidTime2, 'long_name', lvar,
     &                                vname(2,indxTime2)(1:lvar))

        lvar=lenstr(vname(3,indxTime2))
        ierr=nf_put_att_text (ncid, vidTime2, 'units',  lvar,
     &                                vname(3,indxTime2)(1:lvar))
        lvar=lenstr(vname(4,indxTime2))
        ierr=nf_put_att_text (ncid, vidTime2, 'field',  lvar,
     &                                vname(4,indxTime2)(1:lvar))

        call nf_add_attribute(ncid, vidTime2, indxTime2, 5, 
     &       NF_FOUT, ierr)
!
! Tracer diagnostics variables.
!
!
! biogeochemical fluxes
        do iflux = 1, NumFluxTerms
          if (wrtFlux(iflux)) then
            lvar=lenstr(vname(1,indxbioFlux+iflux-1))
            ierr=nf_def_var (ncid, vname(1,indxbioFlux+iflux-1)(1:lvar),
     &           NF_FOUT, 4, r3dgrd, vidFlux(iflux))
#  ifndef AVRH
            lvar=lenstr(vname(2,indxbioFlux+iflux-1))
            ierr=nf_put_att_text (ncid, vidFlux(iflux), 'long_name',
     &           lvar, vname(2,indxbioFlux+iflux-1)(1:lvar))
#  else
            text='averaged '/ /vname(2,indxbioFlux+iflux-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidFlux(iflux), 'long_name',
     &          lvar, text(1:lvar))
#  endif
           lvar=lenstr(vname(3,indxbioFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidFlux(iflux), 'units',
     &          lvar, vname(3,indxbioFlux+iflux-1)(1:lvar))
           lvar=lenstr(vname(4,indxbioFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidFlux(iflux), 'field',
     &          lvar, vname(4,indxbioFlux+iflux-1)(1:lvar))
           
           call nf_add_attribute(ncid, vidFlux(iflux), 
     &          indxbioFlux+iflux-1, 5, NF_FOUT, ierr)
         endif
       end do
! vertical sinking fluxes
        do iflux = 1, NumVSinkTerms
          if (wrtVSink(iflux)) then
            lvar=lenstr(vname(1,indxbioVSink+iflux-1))
#ifdef PISCES
# ifdef key_trc_diaadd
            ierr=nf_def_var (ncid,
     &           vname(1,indxbioVSink+iflux-1)(1:lvar),
     &          NF_FOUT, 3, r2dgrd, vidVSink(iflux))
# endif
#else
            ierr=nf_def_var (ncid,
     &           vname(1,indxbioVSink+iflux-1)(1:lvar),
     &           NF_FOUT, 4, w3dgrd, vidVSink(iflux))
#endif
#  ifndef AVRH
            lvar=lenstr(vname(2,indxbioVSink+iflux-1))
            ierr=nf_put_att_text (ncid, vidVSink(iflux),
     &           'long_name',
     &           lvar, vname(2,indxbioVSink+iflux-1)(1:lvar))
#  else
            text='averaged '/ /vname(2,indxbioVSink+iflux-1)
            lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidVSink(iflux),
     &           'long_name',
     &           lvar, text(1:lvar))
#  endif
           lvar=lenstr(vname(3,indxbioVSink+iflux-1))
           ierr=nf_put_att_text (ncid, vidVSink(iflux), 'units',
     &          lvar, vname(3,indxbioVSink+iflux-1)(1:lvar))
           lvar=lenstr(vname(4,indxbioVSink+iflux-1))
           ierr=nf_put_att_text (ncid, vidVSink(iflux), 'field',
     &          lvar, vname(4,indxbioVSink+iflux-1)(1:lvar))
           
          call nf_add_attribute(ncid, vidVSink(iflux), 
     &          indxbioVSink+iflux-1, 5, NF_FOUT, ierr)
        endif
      end do
!-------------------------------------------------------------------------------- 
#  if (defined BIO_NChlPZD && defined OXYGEN) || defined BIO_BioEBUS
! gas exchange fluxes
        do iflux = 1, NumGasExcTerms
          if (wrtGasExc(iflux)) then
            lvar=lenstr(vname(1,indxGasExcFlux+iflux-1))
            ierr=nf_def_var (ncid,
     &           vname(1,indxGasExcFlux+iflux-1)(1:lvar),
     &           NF_FOUT, 3, r2dgrd, vidGasExc(iflux))
#  ifndef AVRH
            lvar=lenstr(vname(2,indxGasExcFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidGasExc(iflux),
     &           'long_name',
     &           lvar, vname(2,indxGasExcFlux+iflux-1)(1:lvar))
#  else
           text='averaged '/ /vname(2,indxGasExcFlux+iflux-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidGasExc(iflux),
     &          'long_name',
     &          lvar, text(1:lvar))
#  endif
           lvar=lenstr(vname(3,indxGasExcFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidGasExc(iflux), 'units',
     &          lvar, vname(3,indxGasExcFlux+iflux-1)(1:lvar))
           lvar=lenstr(vname(4,indxGasExcFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidGasExc(iflux), 'field',
     &          lvar, vname(4,indxGasExcFlux+iflux-1)(1:lvar))
           
           call nf_add_attribute(ncid, vidGasExc(iflux), 
     &          indxGasExcFlux+iflux-1, 5, NF_FOUT, ierr)
         endif
       end do
#  endif
!--------------------------------------------------------------------------------
         
!
! Leave definition mode.
! ----- ---------- -----
!
        ierr=nf_enddef(ncid)
        write(stdout,'(6x,4A,1x,A,i4)') 
#ifdef AVRH
     &        'DEF_BIO_DIAG_AVG - Created ',
#else      
     &        'DEF_BIO_DIAG - Created ',
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
     &     ) 'DEF_BIO_DIAGS/DIAGS_BIO_AVG WARNING:Actual nb of records',
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

        if (ierr. ne. nf_noerr) then
#if defined MPI & !defined PARALLEL_FILES
          if (mynode.eq.0) then
            create_new_file=.true.
            goto 10
          else
          MPI_master_only  write(stdout,'(/1x,4A,2x,A,I4/)') 
     &       'DEF_BIO_DIAGS_HIS/AVG ERROR :',
     &       'Cannot open file ''', ncname(1:lstr), '''.'
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
! Tracer flux diagnostics variables.
!
!
        do iflux = 1, NumFluxTerms
          if (wrtFlux(iflux)) then
            lvar=lenstr(vname(1,indxbioFlux+iflux-1))
            ierr=nf_inq_varid (ncid, vname(1,indxbioFlux+iflux-1)(1:lvar),
     &           vidFlux(iflux))
            if (ierr .ne. nf_noerr) then
              write(stdout,1) vname(1,indxbioFlux+iflux-1)(1:lvar),
     &             ncname(1:lstr)
              goto 99           !--> ERROR
            endif
          endif
        enddo
!     
!     Vertical sinking flux
!     
        do iflux = 1, NumVSinkTerms
          if (wrtVSink(iflux)) then
            lvar=lenstr(vname(1,indxbioVSink+iflux-1))
            ierr=nf_inq_varid(ncid,vname(1,indxbioVSink+iflux-1)(1:lvar),
     &           vidVSink(iflux))
            if (ierr .ne. nf_noerr) then
              write(stdout,1) vname(1,indxbioVSink+iflux-1)(1:lvar),
     &             ncname(1:lstr) 
              goto 99           !--> ERROR
            endif
          endif                                 
        enddo
!--------------------------------------------------------------------------------
#  if (defined BIO_NChlPZD && defined OXYGEN) || defined BIO_BioEBUS
!  Gas exchange fluxes
!
        do iflux = 1, NumGasExcTerms
          if (wrtGasExc(iflux)) then
            lvar=lenstr(vname(1,indxGasExcFlux+iflux-1))
            ierr=nf_inq_varid (ncid, 
     &           vname(1,indxGasExcFlux+iflux-1)(1:lvar),
     &           vidGasExc(iflux))
            if (ierr .ne. nf_noerr) then
              write(stdout,1) vname(1,indxGasExcFlux+iflux-1)(1:lvar),
     &             ncname(1:lstr)
              goto 99           !--> ERROR
            endif
          endif
        enddo
#endif  
!--------------------------------------------------------------------------------
        
!     write(*,'(6x,2A,i4,1x,A,i4)') 'DEF_BIO_DIAG -- Opened ',
!     &                     'existing file  from record =', rec
!     &                      MYID

#if defined MPI & !defined PARALLEL_FILES
      else
        ierr=nf_open (ncname(1:lstr), nf_write, ncid)
        if (ierr .ne. nf_noerr) then
         MPI_master_only write(stdout,'(/1x,4A,2x,A,I4/)') 
     &                'DEF_BIO_DIAG ERROR: ',
     &                'Cannot open file ''', ncname(1:lstr), '''.'
     &                 MYID

          goto 99                                         !--> ERROR
        endif
#endif

      endif             !<--  create_new_file  

      ierr=nf_set_fill (ncid, nf_nofill, lvar)
      if (ierr .ne. nf_noerr) then
        write(*,'(6x,2A,i4,1x,A,i4)') 'DEF_BIO_DIAG ERROR: Cannot ', 
     &    'switch to ''nf_nofill'' more; netCDF error code =', ierr 
      endif
   1  format(/1x,'DEF_DIAG ERROR: Cannot find variable ''',
     &                   A, ''' in netCDF file ''', A, '''.'/)
#if (defined PUT_GRID_INTO_HISTORY && !defined AVRH)\
 || (defined PUT_GRID_INTO_AVERAGES && defined AVRH)
!
! Write grid variables.
! ----- ---- ----------
!
      if (total_rec.le.1) call wrt_grid (ncid, ncname, lstr)
#endif

  11  format(/' DEF_BIO_DIAGS - unable to create diag file: ',a)
  20  format(/' DEF_BIO_DIAGS - error while writing variable: ',a,
     &        /,15x,'into diag  file: ',a)

  99  return
      end

#undef ncname
#undef rec_per_file
#undef wrtFlux
#undef wrtVSink
#undef wrtGasExc
#undef vidTime
#undef vidTime2
#undef vidTstep
#undef vidFlux
#undef vidGasExc
#undef vidVSink
#undef vidSed

# ifdef AVERAGES
#  ifndef AVRH
#   define AVRH
#   include "def_bio_diags.F"
#  endif
# endif /* AVERAGES */

#else /* DIAGNOSTICS_BIO */
      subroutine def_bio_diags_empty()
      return
      end
#endif /* DIAGNOSTICS_BIO */

