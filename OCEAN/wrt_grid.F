! $Id: wrt_grid.F 1458 2014-02-03 15:01:25Z gcambon $
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

      subroutine wrt_grid (ncid, ncname, lstr)
!
! Write grid variables in output NetCDF file, which may be restart,
! history, averages, etc. All variables are assumed to be previously
! defined by def_grid.
!
! Arguments: ncid    netCDF unit-ID of NetCDF file, which must be
!                            already opened and in definition mode;
!            ncname  netCDF file name (used only in error messages)
!            lstr    length of ncname
!
      implicit none
      character*(*) ncname
      integer i, j, k, ncid, lstr, varid, ierr, nf_fwrite,  
     &        nf_fwrite_x, nf_fwrite_y
#include "param.h"
#include "scalars.h"
#include "ncscrum.h"
#include "grid.h"
#include "netcdf.inc"
#ifdef SOLVE3D
#include "ocean3d.h"
#endif
!
!# include "compute_auxiliary_bounds.h"
!
#if defined MPI & !defined PARALLEL_FILES
      if (mynode.gt.0) goto 1 
#endif
!
! Grid type switch: Spherical or Cartesian.
!
      ierr=nf_inq_varid (ncid, 'spherical', varid)
      ierr=nf_put_var1_text (ncid, varid, 1,
#ifdef SPHERICAL
     &                                         'T')
#else
     &                                         'F')
#endif
      if (ierr.ne.nf_noerr) then
        write(stdout,2) 'spherical', ncname(1:lstr)
        goto 99                                           !--> ERROR
      endif
!
! Physical Dimensions of Model Domain, xl,el.
!
      ierr=nf_inq_varid (ncid, 'xl', varid)
      ierr=nf_put_var1_FTYPE (ncid, varid, 1, xl)
      if (ierr.ne.nf_noerr) then
        write(stdout,2) 'xl', ncname(1:lstr)
        goto 99                                           !--> ERROR
      endif
      ierr=nf_inq_varid (ncid, 'el', varid)
      ierr=nf_put_var1_FTYPE (ncid, varid, 1, el)
      if (ierr.ne.nf_noerr) then
        write(stdout,2) 'el', ncname(1:lstr)
        goto 99                                           !--> ERROR
      endif

#ifdef WET_DRY
!
! Critical Depth for Drying cells
!
      ierr=nf_inq_varid (ncid, 'Dcrit', varid)
      ierr=nf_put_var1_FTYPE (ncid, varid, 1, D_wetdry)
      if (ierr.ne.nf_noerr) then
        write(stdout,2) 'xl', ncname(1:lstr)
        goto 99                                           !--> ERROR
      endif
#endif

#ifdef SOLVE3D
!
! Variable dimension on vertical : s_rho and s_w
!
      ierr=nf_inq_varid (ncid, 's_rho', varid)
      ierr=nf_put_var_FTYPE(ncid, varid, sc_r)
      if (ierr.ne.nf_noerr) then
         write(stdout,2) 's_rho', ncname(1:lstr)
         goto 99                !--> ERROR
      endif 

      ierr=nf_inq_varid (ncid, 's_w', varid)
      ierr=nf_put_var_FTYPE(ncid, varid, sc_w)
      if (ierr.ne.nf_noerr) then
         write(stdout,2) 's_w', ncname(1:lstr)
         goto 99                !--> ERROR
      endif 

!
! S-coordinate independent variables "sc_w", "sc_r" and stretching
! curves "Cs_w", "Cs_r" at W- and RHO-points.
!
      ierr=nf_inq_varid (ncid, 'Cs_r', varid)
      ierr=nf_put_var_FTYPE(ncid,varid ,Cs_r)
      if (ierr.ne.nf_noerr) then
         write(stdout,2) 'Cs_r', ncname(1:lstr)
         goto 99                !--> ERROR
      endif 
!
      ierr=nf_inq_varid (ncid, 'Cs_w', varid)
      ierr=nf_put_var_FTYPE(ncid,varid ,Cs_w)
      if (ierr.ne.nf_noerr) then
         write(stdout,2) 'Cs_w', ncname(1:lstr)
         goto 99                !--> ERROR
      endif 
!
      ierr=nf_inq_varid (ncid, 'sc_r', varid)
      ierr=nf_put_var_FTYPE(ncid,varid ,sc_r)
      if (ierr.ne.nf_noerr) then
         write(stdout,2) 's_r', ncname(1:lstr)
         goto 99                !--> ERROR
      endif 
!
      ierr=nf_inq_varid (ncid, 'sc_w', varid)
      ierr=nf_put_var_FTYPE(ncid, varid,sc_w)
      if (ierr.ne.nf_noerr) then
         write(stdout,2) 's_w', ncname(1:lstr)
         goto 99                !--> ERROR
      endif 
!
      ierr=nf_inq_varid (ncid, 'hc', varid)
      ierr=nf_put_var1_FTYPE (ncid, varid, 1, hc)
      if (ierr.ne.nf_noerr) then
        write(stdout,2) 'hc', ncname(1:lstr)
        goto 99                 !--> ERROR
      endif
!
      ierr=nf_inq_varid (ncid, 'Vtransform', varid)
      ierr=nf_put_var1_FTYPE (ncid, varid, 1, Vtransform)
      if (ierr.ne.nf_noerr) then
        write(stdout,2) 'Vtransform', ncname(1:lstr)
        goto 99                 !--> ERROR
      endif
!
#endif
!
! Bathymetry.
!
  1   ierr=nf_inq_varid(ncid,'h',varid)
      ierr=nf_fwrite (h(START_2D_ARRAY), ncid, varid, 0, r2dvar)
      if (ierr.ne.nf_noerr) then
        write(stdout,2) 'h', ncname(1:lstr)
        goto 99                                           !--> ERROR
      endif
!
! Coriolis parameter.
!
      ierr=nf_inq_varid(ncid,'f',varid)
      ierr=nf_fwrite (f(START_2D_ARRAY), ncid, varid, 0, r2dvar)
      if (ierr.ne.nf_noerr) then
        write(stdout,2) 'f', ncname(1:lstr)
        goto 99                                           !--> ERROR
      endif
!
! Curvilinear transformation metrics.
!
      ierr=nf_inq_varid(ncid,'pm',varid)
      ierr=nf_fwrite (pm(START_2D_ARRAY), ncid, varid, 0, r2dvar)
      if (ierr.ne.nf_noerr) then
        write(stdout,2) 'pm', ncname(1:lstr)
        goto 99                                           !--> ERROR
      endif
      ierr=nf_inq_varid(ncid,'pn',varid)
      ierr=nf_fwrite (pn(START_2D_ARRAY), ncid, varid, 0, r2dvar)
      if (ierr.ne.nf_noerr) then
        write(stdout,2) 'pn', ncname(1:lstr)
        goto 99                                           !--> ERROR
      endif
!
! Longitude/latitude or cartezian coordinates of RHO-points
!
#ifdef SPHERICAL
      ierr=nf_inq_varid(ncid,'lon_rho',varid)
      ierr=nf_fwrite (lonr(START_2D_ARRAY), ncid, varid, 0, r2dvar)
      if (ierr.ne.nf_noerr) then
        write(stdout,2) 'lon_rho', ncname(1:lstr)
        goto 99                                           !--> ERROR
      endif
      ierr=nf_inq_varid(ncid,'lat_rho',varid)
      ierr=nf_fwrite (latr(START_2D_ARRAY), ncid, varid, 0, r2dvar)
      if (ierr.ne.nf_noerr) then
        write(stdout,2) 'lat_rho', ncname(1:lstr)
        goto 99                                           !--> ERROR
      endif
      ierr=nf_inq_varid(ncid,'lon_u',varid)
      ierr=nf_fwrite (lonu(START_2D_ARRAY), ncid, varid, 0, u2dvar)
      if (ierr.ne.nf_noerr) then
        write(stdout,2) 'lon_u', ncname(1:lstr)
        goto 99                                           !--> ERROR
      endif
      ierr=nf_inq_varid(ncid,'lat_u',varid)
      ierr=nf_fwrite (latu(START_2D_ARRAY), ncid, varid, 0, u2dvar)
      if (ierr.ne.nf_noerr) then
        write(stdout,2) 'lat_u', ncname(1:lstr)
        goto 99                                           !--> ERROR
      endif
      ierr=nf_inq_varid(ncid,'lon_v',varid)
      ierr=nf_fwrite (lonv(START_2D_ARRAY), ncid, varid, 0, v2dvar)
      if (ierr.ne.nf_noerr) then
        write(stdout,2) 'lon_v', ncname(1:lstr)
        goto 99                                           !--> ERROR
      endif
      ierr=nf_inq_varid(ncid,'lat_v',varid)
      ierr=nf_fwrite (latv(START_2D_ARRAY), ncid, varid, 0, v2dvar)
      if (ierr.ne.nf_noerr) then
        write(stdout,2) 'lat_v', ncname(1:lstr)
        goto 99                                           !--> ERROR
      endif
#else
      ierr=nf_inq_varid(ncid,'x_rho',varid)
      ierr=nf_fwrite (xr(START_2D_ARRAY), ncid, varid, 0, r2dvar)
      if (ierr.ne.nf_noerr) then
        write(stdout,2) 'x_rho', ncname(1:lstr)
        goto 99                                           !--> ERROR
      endif
      ierr=nf_inq_varid(ncid,'y_rho',varid)
      ierr=nf_fwrite (yr(START_2D_ARRAY), ncid, varid, 0, r2dvar)
      if (ierr.ne.nf_noerr) then
        write(stdout,2) 'y_rho', ncname(1:lstr)
        goto 99                                           !--> ERROR
      endif
#endif
#ifdef CURVGRID
!
! Angle between XI-axis and EAST at RHO-points
!
      ierr=nf_inq_varid(ncid,'angle',varid)
      ierr=nf_fwrite (angler(START_2D_ARRAY), ncid, varid, 0, r2dvar)
      if (ierr.ne.nf_noerr) then
        write(stdout,2) 'angle', ncname(1:lstr)
        goto 99                                           !--> ERROR
      endif
#endif
#ifdef MASKING
!
! Masking fields at RHO-points.
!
      ierr=nf_inq_varid(ncid,'mask_rho',varid)
      ierr=nf_fwrite (rmask(START_2D_ARRAY), ncid, varid, 0, r2dvar)
      if (ierr.ne.nf_noerr) then
        write(stdout,2) 'mask_rho', ncname(1:lstr)
        goto 99                                           !--> ERROR
      endif
#endif

!
! Variable horizontal grid indices
! 
      ierr=nf_inq_varid (ncid, 'xi_rho', varid)
      ierr=nf_fwrite_x (ncid, varid, r2dvar)
      if (ierr.ne.nf_noerr) then
         write(stdout,2) 'xi_rho', ncname(1:lstr)
         goto 99                !--> ERROR
      endif 
      ierr=nf_inq_varid (ncid, 'eta_rho', varid)
      ierr=nf_fwrite_y (ncid, varid, r2dvar)
      if (ierr.ne.nf_noerr) then
         write(stdout,2) 'eta_rho', ncname(1:lstr)
         goto 99                !--> ERROR
      endif
      ierr=nf_inq_varid (ncid, 'xi_u', varid)
      ierr=nf_fwrite_x (ncid, varid, u2dvar)
      if (ierr.ne.nf_noerr) then
         write(stdout,2) 'xi_u', ncname(1:lstr)
         goto 99                !--> ERROR
      endif 
      ierr=nf_inq_varid (ncid, 'eta_v', varid)
      ierr=nf_fwrite_y (ncid, varid, v2dvar)
      if (ierr.ne.nf_noerr) then
         write(stdout,2) 'eta_v', ncname(1:lstr)
         goto 99                !--> ERROR
      endif   

      MPI_master_only write(stdout,'(6x,4A,2x,A,I4)') 
     &                 'WRT_GRID -- wrote grid ',
     &                 'data into file ''', ncname(1:lstr), '''.'
     &                  MYID
      return
  2   format(/1x,'WRT_GRID - error while writing variable ''', A,
     &              ''' into', /11x, 'netCDF file ''', A, '''.'/)
  99  return
      end
