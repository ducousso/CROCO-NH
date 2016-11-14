! $Id: put_global_atts.F 1458 2014-02-03 15:01:25Z gcambon $
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

      subroutine put_global_atts (ncid, ierr)
      implicit none
      integer ncid, ierr, nf_ftype, lvar,lenstr
#include "param.h"
#include "scalars.h"
#include "ncscrum.h"
#include "strings.h"

#ifdef SEDIMENT
# include "sediment.h"
      character(32) namatt
      integer i,j
#endif /* FLOATS */
#ifdef FLOATS
# include "init_floats.h"
# include "ncscrum_floats.h"
      real tempreal(i_floats)
      integer tempint(i_floats),i
#endif /* FLOATS */
#ifdef STATIONS
# include "sta.h"
# include "nc_sta.h"
#endif
#include "netcdf.inc"
#if defined MPI && defined PARALLEL_FILES
      integer*4 ibuff(4)
!
! Put global attribute 'partition' which identifies subdomain
! within the processor grid individually for each file.
!
      ibuff(1)=ii
      ibuff(2)=jj
      ibuff(3)=NP_XI
      ibuff(4)=NP_ETA
      ierr=nf_put_att_int (ncid, nf_global, 'partition',
     &                                  nf_int, 4, ibuff)
#endif

!
! Decide about output file type and precision for floating point
! variables (restart file always has the same precision as in the
! code, typically double precision, while all others can be made
! single precision.
!
      if (ncid.eq.ncidrst) then
        nf_ftype=NF_FTYPE
      else
        nf_ftype=NF_FOUT
      endif

      if (ncid.eq.ncidrst) then
        ierr=nf_put_att_text (ncid, nf_global, 'type',  17,
     &                                 'ROMS restart file')
      elseif (ncid.eq.ncidhis) then
        ierr=nf_put_att_text (ncid, nf_global, 'type',  17,
     &                                 'ROMS history file')
#ifdef AVERAGES
      elseif (ncid.eq.ncidavg) then
        ierr=nf_put_att_text (ncid, nf_global, 'type',  18,
     &                                'ROMS averages file')
#endif
#ifdef STATIONS
      elseif (ncid.eq.ncidsta) then
        ierr=nf_put_att_text(ncid,  nf_global, 'type', 18,
     &                                'ROMS stations file')
#endif
#ifdef FLOATS
      elseif (ncid.eq.ncidflt) then
        ierr=nf_put_att_text(ncid,  nf_global, 'type', 24,
     &                          'ROMS floats history file')
#endif
#if (defined DIAGNOSTICS_TS && defined DIAGNOSTICS_TS_ADV) 
      elseif (ncid.eq.nciddia) then
        ierr=nf_put_att_text(ncid,  nf_global, 'type', 51,
     &     'ROMS DIAGTS history file - flux form for adv. terms')
#ifdef AVERAGES
      elseif (ncid.eq.nciddia_avg) then
        ierr=nf_put_att_text(ncid,  nf_global, 'type', 51,
     &     'ROMS DIAGTS average file - flux form for adv. terms')
#endif
#endif
#if (defined DIAGNOSTICS_TS && !defined DIAGNOSTICS_TS_ADV) 
      elseif (ncid.eq.nciddia) then
        ierr=nf_put_att_text(ncid,  nf_global, 'type', 57,
     &     'ROMS DIAGTS history file - divergence form for adv. terms')
#ifdef AVERAGES
      elseif (ncid.eq.nciddia_avg) then
        ierr=nf_put_att_text(ncid,  nf_global, 'type', 57,
     &     'ROMS DIAGTS average file - divergence form for adv. terms')
#endif
#endif


      endif


      lvar=lenstr(title)
      ierr=nf_put_att_text(ncid, nf_global, 'title',   lvar,
     &                                         title(1:lvar))
#ifdef START_DATE
      lvar=lenstr(start_date)
      ierr=nf_put_att_text(ncid, nf_global, 'start_date',lvar,
     &                                     start_date(1:lvar))
#endif
      lvar=lenstr(date_str)
      ierr=nf_put_att_text(ncid, nf_global, 'date',    lvar,
     &                                      date_str(1:lvar))
      lvar=lenstr(rstname)
      ierr=nf_put_att_text(ncid, nf_global, 'rst_file',lvar,
     &                                       rstname(1:lvar))
      lvar=lenstr(hisname)
      ierr=nf_put_att_text(ncid, nf_global, 'his_file',lvar,
     &                                       hisname(1:lvar))
#ifdef AVERAGES
      lvar=lenstr(avgname)
      ierr=nf_put_att_text(ncid, nf_global, 'avg_file',lvar,
     &                                       avgname(1:lvar))
#endif
#ifdef STATIONS
      lvar=lenstr(staname)
      ierr=nf_put_att_text(ncid, nf_global, 'sta_file',lvar,
     &                                       staname(1:lvar))
      lvar=lenstr(staposname)
      ierr=nf_put_att_text(ncid, nf_global, 'spos_file',lvar,
     &                                    staposname(1:lvar))
#endif

#ifdef FLOATS
      lvar=lenstr(fltname)
      ierr=nf_put_att_text(ncid, nf_global, 'flt_file',lvar,
     &                                       fltname(1:lvar))
      lvar=lenstr(fposnam)
      ierr=nf_put_att_text(ncid, nf_global, 'fpos_file',lvar,
     &                                       fposnam(1:lvar))

      if (ncid.eq.ncidflt) then
# ifdef FLOATS_GLOBAL_ATTRIBUTES
      do i=1,i_floats
      tempreal(i)=Ft0(i)
      enddo
      ierr=nf_put_att_FTYPE(ncid, nf_global, 'Ft0',nf_ftype,
     &                         i_floats, tempreal)
      do i=1,i_floats
      tempreal(i)=Fx0(i)
      enddo
      ierr=nf_put_att_FTYPE(ncid, nf_global, 'Fx0',nf_ftype,
     &                         i_floats, tempreal)
      do i=1,i_floats
      tempreal(i)=Fy0(i)
      enddo
      ierr=nf_put_att_FTYPE(ncid, nf_global, 'Fy0',nf_ftype,
     &                         i_floats, tempreal)
      do i=1,i_floats
      tempreal(i)=Fz0(i)
      enddo
      ierr=nf_put_att_FTYPE(ncid, nf_global, 'Fz0',nf_ftype,
     &                         i_floats, tempreal)
      do i=1,i_floats
      tempint(i)=Fgrd(i)
      enddo
      ierr=nf_put_att_int(ncid, nf_global, 'Fgrd',nf_int,
     &                         i_floats, tempint)
      do i=1,i_floats
      tempint(i)=Fcoor(i)
      enddo
      ierr=nf_put_att_int(ncid, nf_global, 'Fcoor',nf_int,
     &                         i_floats, tempint)
      do i=1,i_floats
      tempint(i)=Ftype(i)
      enddo
      ierr=nf_put_att_int(ncid, nf_global, 'Ftype',nf_int,
     &                         i_floats, tempint)
      do i=1,i_floats
      tempint(i)=Fcount(i)
      enddo
      ierr=nf_put_att_int(ncid, nf_global, 'Fcount',nf_int,
     &                         i_floats, tempint)
      do i=1,i_floats
      tempreal(i)=Fdt(i)
      enddo
      ierr=nf_put_att_FTYPE(ncid, nf_global, 'Fdt',nf_ftype,
     &                         i_floats, tempreal)
      do i=1,i_floats
      tempreal(i)=Fdx(i)
      enddo
      ierr=nf_put_att_FTYPE(ncid, nf_global, 'Fdx',nf_ftype,
     &                         i_floats, tempreal)
      do i=1,i_floats
      tempreal(i)=Fdy(i)
      enddo
      ierr=nf_put_att_FTYPE(ncid, nf_global, 'Fdy',nf_ftype,
     &                         i_floats, tempreal)
      do i=1,i_floats
      tempreal(i)=Fdz(i)
      enddo
      ierr=nf_put_att_FTYPE(ncid, nf_global, 'Fdz',nf_ftype,
     &                         i_floats, tempreal)
# endif
      endif
#endif /* FLOATS */

#ifndef ANA_GRID
      lvar=lenstr(grdname)
      ierr=nf_put_att_text(ncid, nf_global, 'grd_file',lvar,
     &                                       grdname(1:lvar))
#endif
#ifndef ANA_INITIAL
      lvar=lenstr(ininame)
      ierr=nf_put_att_text(ncid, nf_global, 'ini_file',lvar,
     &                                       ininame(1:lvar))
#endif
#if !defined ANA_SMFLUX || !defined ANA_STFLUX \
 || !defined ANA_BTFLUX \
 || (defined BBL && !defined ANA_BSEDIM)  \
 || (defined BBL && !defined ANA_WWAVE)   \
 || (defined SALINITY && !defined ANA_SSFLUX)  \
 || ((defined LMD_SKPP || defined LMD_BKPP)  && !defined ANA_SRFLUX)
      lvar=lenstr(frcname)
      ierr=nf_put_att_text(ncid, nf_global,'frc_file', lvar,
     &                                       frcname(1:lvar))
#endif
#ifdef PSOURCE_NCFILE
      lvar=lenstr(qbarname)
      ierr=nf_put_att_text(ncid, nf_global, 'qbar_file',lvar,
     &                                       qbarname(1:lvar))
#endif
#ifdef ASSIMILATION
      lvar=lenstr(assname)
      ierr=nf_put_att_text(ncid, nf_global,'ass_file', lvar,
     &                                       assname(1:lvar))
      lvar=lenstr(aparnam)
      ierr=nf_put_att_text(ncid, nf_global,'apar_file',lvar,
     &                                       aparnam(1:lvar))
#endif
#ifdef SOLVE3D
!
! S-coordinate control parameters "theta_s", "theta_b", "Tcline"
! and "hc" (written as as global attributes).
!
# ifdef  NEW_S_COORD
      ierr=nf_put_att_text (ncid, nf_global, 'VertCoordType',3,'NEW')
# endif
# ifdef  LMD_SKPP2005
      ierr=nf_put_att_text (ncid, nf_global, 'skpp',4,'2005')
# endif
      ierr=nf_put_att_FTYPE(ncid, nf_global,'theta_s',nf_ftype,
     &                                            1,  theta_s)
      ierr=nf_put_att_text (ncid, nf_global,'theta_s_expl',38,
     &                   'S-coordinate surface control parameter')

      ierr=nf_put_att_FTYPE(ncid,nf_global,'theta_b',nf_ftype, 1,
     &                                                       theta_b)
      ierr=nf_put_att_text (ncid,nf_global,'theta_b_expl',37,
     &                       'S-coordinate bottom control parameter')

      ierr=nf_put_att_FTYPE(ncid,nf_global,'Tcline', nf_ftype, 1,
     &                                                        Tcline)
      ierr=nf_put_att_text (ncid,nf_global,'Tcline_expl',39,
     &                     'S-coordinate surface/bottom layer width')
      ierr=nf_put_att_text (ncid, nf_global,'Tcline_units',5,'meter')

      ierr=nf_put_att_FTYPE(ncid, nf_global, 'hc',nf_ftype, 1, hc)
      ierr=nf_put_att_text (ncid, nf_global, 'hc_expl',38,
     &                      'S-coordinate parameter, critical depth')
      ierr=nf_put_att_text (ncid, nf_global, 'hc_units', 5, 'meter')
!
! S-coordinate independent variables "sc_w", "sc_r" and stretching
! curves "Cs_w", "Cs_r" at W- and RHO-points.
!
      ierr=nf_put_att_FTYPE(ncid, nf_global,'sc_w',nf_ftype, N+1,
     &                                                          sc_w)
      ierr=nf_put_att_text (ncid, nf_global,'sc_w_expl', 24,
     &                                    'S-coordinate at W-points')
      ierr=nf_put_att_FTYPE(ncid, nf_global,'Cs_w',nf_ftype, N+1,
     &                                                          Cs_w)
      ierr=nf_put_att_text (ncid, nf_global,'Cs_w_expl',42,
     &                  'S-coordinate stretching curves at W-points')
      ierr=nf_put_att_FTYPE(ncid,nf_global,'sc_r',nf_ftype,N,sc_r)
      ierr=nf_put_att_text (ncid, nf_global,'sc_r_expl', 24,
     &                                    'S-coordinate at W-points')
      ierr=nf_put_att_FTYPE(ncid,nf_global,'Cs_r',nf_ftype,N,Cs_r)
      ierr=nf_put_att_text (ncid, nf_global,'Cs_r_expl',44,
     &                'S-coordinate stretching curves at RHO-points')
#endif
!
! Time stepping parameters.
!
      ierr=nf_put_att_int(ncid,nf_global,'ntimes',  nf_int,1,ntimes)
      ierr=nf_put_att_int(ncid,nf_global,'ndtfast', nf_int,1,ndtfast)
      ierr=nf_put_att_FTYPE(ncid,nf_global,'dt',    nf_ftype, 1,  dt)
      ierr=nf_put_att_FTYPE(ncid,nf_global,'dtfast',nf_ftype, 1,
     &                                                        dtfast)
      ierr=nf_put_att_int  (ncid,nf_global,'nwrt',  nf_int,  1, nwrt)
#ifdef AVERAGES
      ierr=nf_put_att_int  (ncid,nf_global,'ntsavg',nf_int, 1,ntsavg)
      ierr=nf_put_att_text (ncid,nf_global,'ntsavg_expl',59,
     & 'starting time-step for accumulation of time-averaged fields')

      ierr=nf_put_att_int  (ncid,nf_global,'navg',  nf_int, 1,  navg)
      ierr=nf_put_att_text (ncid,nf_global,'navg_expl',50,
     &          'number of time-steps between time-averaged records')
#endif
#ifdef STATIONS
      ierr=nf_put_att_int  (ncid,nf_global,'nsta',  nf_int, 1,  nsta)
      ierr=nf_put_att_text (ncid,nf_global,'nsta_expl', 45,
     &               'number of time-steps between stations records')
#endif
!
! Horizontal viscosity and mixing coefficients.
!
#ifdef UV_VIS2
      ierr=nf_put_att_FTYPE(ncid,nf_global,'visc2',nf_ftype,1,visc2)
      ierr=nf_put_att_text (ncid,nf_global,'visc2_expl',41,
     &                   'Laplacian mixing coefficient for momentum')
      ierr=nf_put_att_text (ncid,nf_global,'visc2_units',15,
     &                                             'meter2 second-1')
#endif
#ifdef UV_VIS4
      ierr=nf_put_att_FTYPE(ncid,nf_global,'visc4',nf_ftype, 1,visc4)
      ierr=nf_put_att_text (ncid,nf_global,'visc4_expl', 42,
     &                  'biharmonic mixing coefficient for momentum')
      ierr=nf_put_att_text (ncid,nf_global,'visc4_units', 15,
     &                                             'meter4 second-1')
#endif
#ifdef SOLVE3D
# ifdef TS_DIF2
      ierr=nf_put_att_FTYPE(ncid,nf_global,'tnu2',nf_ftype, 1,tnu2)
      ierr=nf_put_att_text (ncid,nf_global,'tnu2_expl',40,
     &                    'Laplacian mixing coefficient for tracers')
      ierr=nf_put_att_text (ncid,nf_global,'tnu2_units',15,
     &                                             'meter2 second-1')
# endif
# ifdef TS_DIF4
      ierr=nf_put_att_FTYPE(ncid,nf_global,'tnu4', nf_ftype, 1,tnu4)
      ierr=nf_put_att_text (ncid,nf_global,'tnu4_expl',41,
     &                   'biharmonic mixing coefficient for tracers')
      ierr=nf_put_att_text (ncid,nf_global,'units',15,
     &                                             'meter4 second-1')
# endif
# if defined GLS_MIXING && defined GLS_DIF2
      ierr=nf_put_att_FTYPE(ncid,nf_global,'gls_diff2',nf_ftype, 1,
     &                                                     gls_diff2)
      ierr=nf_put_att_text (ncid,nf_global,'gls_diff2_expl',49,
     &           'Laplacian mixing coefficient for turbulent energy')
      ierr=nf_put_att_text (ncid,nf_global,'gls_diff2_units',15,
     &                                             'meter2 second-1')
# endif
# if !defined LMD_MIXING && !defined BVF_MIXING
!
! Background vertical viscosity and tracer mixing coefficients.
!
      ierr=nf_put_att_FTYPE(ncid,nf_global,'Akv_bak',nf_ftype, 1,
     &                                                      Akv_bak)
      ierr=nf_put_att_text (ncid,nf_global,'Akv_bak_expl',51,
     &         'background vertical mixing coefficient for momentum')
      ierr=nf_put_att_text (ncid,nf_global,'Akv_bak_units',15,
     &                                             'meter2 second-1')

      ierr=nf_put_att_FTYPE(ncid,nf_global,'Akt_bak',nf_ftype, NT,
     &                                                       Akt_bak)
      ierr=nf_put_att_text (ncid,nf_global,'Akt_bak_expl', 50,
     &          'background vertical mixing coefficient for tracers')
      ierr=nf_put_att_text (ncid,nf_global,'Akt_bak_units', 15,
     &                                             'meter2 second-1')
# endif
# ifdef GLS_MIXING
!
!  Background eddy viscosity for TKE and GLS
!
      ierr=nf_put_att_FTYPE(ncid,nf_global,'Akk_bak',nf_ftype,
     &                                                     1,Akk_bak)
      ierr=nf_put_att_text (ncid,nf_global,'Akk_bak_expl',59,
     & 'background vertical mixing coefficient for turbulent energy')
      ierr=nf_put_att_text (ncid,nf_global,'Akk_bak_units',15,
     &                                             'meter2 second-1')
!
      ierr=nf_put_att_FTYPE(ncid,nf_global,'Akp_bak',nf_ftype,
     &                                                     1,Akp_bak)
      ierr=nf_put_att_text (ncid,nf_global,'Akp_bak_expl',55,
     &     'background vertical mixing coefficient for length scale')
      ierr=nf_put_att_text (ncid,nf_global,'Akp_bak_units',15,
     &                                             'meter2 second-1')
# endif
#endif
!
! Bottom drag coefficients.
!
      if (Zob.ne.0.) then
        ierr=nf_put_att_FTYPE(ncid,nf_global,'Zob',nf_ftype,1,Zob)
        ierr=nf_put_att_text (ncid,nf_global,'Zob_expl',46,
     &               'VonKarman/Prandtl log layer : roughness scale')
        ierr=nf_put_att_text (ncid,nf_global,'Zob_units',5,
     &                                                       'meter')
        ierr=nf_put_att_FTYPE(ncid,nf_global,'Cdb_max',nf_ftype,1,
     &                                                       Cdb_max)
        ierr=nf_put_att_FTYPE(ncid,nf_global,'Cdb_min',nf_ftype,1,
     &                                                       Cdb_min)
        ierr=nf_put_att_text (ncid,nf_global,'Cdb_expl',37,
     &                         'Range of quadratic drag coefficient')
      elseif (rdrg2.gt.0.) then
        ierr=nf_put_att_FTYPE(ncid,nf_global,'rdrg2',nf_ftype,1,rdrg2)
        ierr=nf_put_att_text (ncid,nf_global,'rdrg2_expl',26,
     &                                  'quadratic drag coefficient')
        ierr=nf_put_att_text (ncid,nf_global,'rdrg2_units',14,
     &                                              'nondimensional')
      elseif (rdrg.ne.0) then
        ierr=nf_put_att_FTYPE(ncid,nf_global,'rdrg',nf_ftype,1,rdrg)
        ierr=nf_put_att_text (ncid,nf_global,'rdrg_expl',23,
     &                                     'linear drag coefficient')
        ierr=nf_put_att_text (ncid,nf_global,'rdrg_units',14,
     &                                              'meter second-1')
      endif
!
#ifdef SOLVE3D
!
! Equation of State parameters.
!
      ierr=nf_put_att_FTYPE(ncid,nf_global,'rho0',nf_ftype, 1,rho0)
      ierr=nf_put_att_text (ncid,nf_global,'rho0_expl', 45,
     &               'Mean density used in Boussinesq approximation')
      ierr=nf_put_att_text (ncid,nf_global,'rho0_units', 16,
     &                                            'kilogram meter-3')
!
# ifndef NONLIN_EOS
      ierr=nf_put_att_FTYPE(ncid,nf_global,'R0',  nf_ftype, 1, R0)
      ierr=nf_put_att_text (ncid,nf_global,'R0_expl', 51,
     &         'Background density used in linear equation of state')
      ierr=nf_put_att_text (ncid,nf_global,'R0_units', 16,
     &                                            'kilogram meter-3')
!
      ierr=nf_put_att_FTYPE(ncid,nf_global,'Tcoef',nf_ftype, 1,Tcoef)
      ierr=nf_put_att_text (ncid,nf_global,'Tcoef_expl',29,
     &                               'thermal expansion coefficient')
      ierr=nf_put_att_text (ncid,nf_global,'Tcoef_units',10,
     &                                                 'kg.m-3.C-1')
!
      ierr=nf_put_att_FTYPE(ncid,nf_global,'Scoef',nf_ftype, 1,Scoef)
      ierr=nf_put_att_text (ncid,nf_global,'Scoef_expl', 30,
     &                              'Saline contraction coefficient')
      ierr=nf_put_att_text (ncid,nf_global,'Scoef_units', 12, 
     &                                                'kg.m-3.psu-1')
# endif
!
! Various parameters.
!
# ifdef BODYFORCE
      ierr=nf_put_att_int (ncid,nf_global,'levsfrc',nf_int,1,levsfrc)
      ierr=nf_put_att_text(ncid,nf_global, 'levsfrc_expl', 38,
     &                      'Shallowest level for body-force stress')
      ierr=nf_put_att_int (ncid,nf_global,'levbfrc',nf_int,1,levbfrc)
      ierr=nf_put_att_text(ncid,nf_global,'levbfrc_expl', 35,
     &                         'Deepest level for body-force stress')
# endif
#endif /* SOLVE3D */
!
! Slipperiness parameters.
!
      ierr=nf_put_att_FTYPE(ncid,nf_global,'gamma2',nf_ftype, 1,
     &                                                       gamma2)
      ierr=nf_put_att_text (ncid,nf_global,'gamma2_expl', 22,
     &                                    'Slipperiness parameter')
!
! Sponge parameters
!
# ifdef SPONGE
      ierr=nf_put_att_FTYPE(ncid,nf_global,'x_sponge',nf_ftype, 1,
     &                                                     x_sponge)
      ierr=nf_put_att_FTYPE(ncid,nf_global,'v_sponge',nf_ftype, 1,
     &                                                     v_sponge)
      ierr=nf_put_att_text (ncid,nf_global,'sponge_expl', 51,
     &        'Sponge parameters : extent (m) & viscosity (m2.s-1)')
# endif
!
! Sediment parameters
!
# ifdef SEDIMENT
      do i=1, NST
        write(namatt,'(A,I1)') ' sand_',i
        ierr=nf_put_att_FTYPE(ncid,nf_global,'Sd'//TRIM(namatt),
     &       nf_ftype, 1, Sd(i))
      enddo
      ierr=nf_put_att_text(ncid,nf_global,'Sd_expl', 32,
     &                         'Diameter of grain per size class')       

      do i=1, NST
        write(namatt,'(A,I1)') ' sand_',i
        ierr=nf_put_att_FTYPE(ncid,nf_global,'Csed'//TRIM(namatt),
     &       nf_ftype, 1, Csed(i))
      enddo
      ierr=nf_put_att_text(ncid,nf_global,'Csed_expl', 21,
     &                         'Initial concentration')       

      do i=1, NST
        write(namatt,'(A,I1)') ' sand_',i
        ierr=nf_put_att_FTYPE(ncid,nf_global,'Srho'//TRIM(namatt),
     &       nf_ftype, 1,Srho(i))
      enddo
      ierr=nf_put_att_text(ncid,nf_global,'Srho', 43,
     &                  'Density of sediment material per size class')       

      do i=1, NST
        write(namatt,'(A,I1)') ' sand_',i
        ierr=nf_put_att_FTYPE(ncid,nf_global,'Wsed '//TRIM(namatt),
     &       nf_ftype, 1,Wsed(i))
      enddo
      ierr=nf_put_att_text(ncid,nf_global,'Wsed_expl', 32,
     &                         'Settling velocity per size class')       

      do i=1, NST
        write(namatt,'(A,I1)') ' sand_',i
        ierr=nf_put_att_FTYPE(ncid,nf_global,'Erate'//TRIM(namatt),
     &       nf_ftype, 1,Erate(i))
       enddo
      ierr=nf_put_att_text(ncid,nf_global,'Erate_expl', 27,
     &                         'Erosion rate per size class')       

      do i=1, NST
        write(namatt,'(A,I1)') ' sand_',i
        ierr=nf_put_att_FTYPE(ncid,nf_global,'tau_ce'//TRIM(namatt),
     &       nf_ftype, 1,tau_ce(i))
       enddo
      ierr=nf_put_att_text(ncid,nf_global,'tau_ce_expl', 41,
     &                   'Critical shear stress for sediment motion')       

      do i=1, NST
        do j=1, NLAY
          write(namatt,'(A,I1,A,I1)') ' sand_',i, ' layer_',j
          ierr=nf_put_att_FTYPE(ncid,nf_global,'bfr'//TRIM(namatt),
     &         nf_ftype, 1,bfr(j,i))
        enddo
      enddo
      ierr=nf_put_att_text(ncid,nf_global,'bfr_expl', 52,
     &         'Volume fraction of each size class in each bed layer')       
  
      do i=1, NLAY
         write(namatt,'(A,I1,A,I1)') ' layer_',i
         ierr=nf_put_att_FTYPE(ncid,nf_global,'Bthk'//TRIM(namatt),
     &        nf_ftype, 1, Bthk(i))
      enddo
      ierr=nf_put_att_text(ncid,nf_global,'Bthk_expl', 33,
     &                         'Initial thicknesses of bed layers')       
      do i=1, NLAY
         ierr=nf_put_att_FTYPE(ncid,nf_global,'Bpor'//TRIM(namatt),
     &        nf_ftype, 1, Bpor(i))
      enddo
      ierr=nf_put_att_text(ncid,nf_global,'Bpor_expl', 30,
     &                         'Initial porosity of bed layers')       

      ierr=nf_put_att_FTYPE(ncid,nf_global,'Hrip',nf_ftype, 1,
     &                                            Hrip)
      ierr=nf_put_att_text(ncid,nf_global,'Hrip_expl', 21,
     &                         'Initial ripple height')       
      ierr=nf_put_att_FTYPE(ncid,nf_global,'Lrip',nf_ftype, 1,
     &                                            Lrip)
      ierr=nf_put_att_text(ncid,nf_global,'Lrip_expl', 21,
     &                         'Initial ripple length')       
      ierr=nf_put_att_FTYPE(ncid,nf_global,'bedload_coeff',nf_ftype, 1,
     &                                            bedload_coeff)
      ierr=nf_put_att_text(ncid,nf_global,'bedload_coeff_expl', 19,
     &                         'Bedload coefficient')       
       ierr=nf_put_att_FTYPE(ncid,nf_global,'morph_fac',nf_ftype, 1,
     &                                            morph_fac)
      ierr=nf_put_att_text(ncid,nf_global,'morph_fac_expl', 26,
     &                         'Morphological scale factor')
      
      ierr=nf_put_att_text (ncid,nf_global,'sponge_expl', 51,
     &        'Sponge parameters : extent (m) & viscosity (m2.s-1)')
# endif
!
! List of Source Codes and Activated CPP-switches
!
      lvar=lenstr(srcs)
      ierr=nf_put_att_text (ncid,nf_global, 'SRCS', lvar,
     &                                        srcs(1:lvar))
!
      lvar=lenstr(Coptions)
      ierr=nf_put_att_text(ncid,nf_global, 'CPP-options',
     &                              lvar, Coptions(1:lvar))
      return
      end
