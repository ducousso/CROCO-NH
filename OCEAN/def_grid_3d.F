! $Id: def_grid_3d.F 1458 2014-02-03 15:01:25Z gcambon $
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
#if defined SOLVE3D 
      subroutine def_grid_3d (ncid, r2dgrd, u2dgrd, v2dgrd, 
     &                        r3dgrd, w3dgrd)
!
! Define grid variables in output NetCDF file for 3D configuration, 
! which may be restart, history, averages, etc..
!
! Arguments: ncid    NetCDF unit-ID of NetCDF file, which must be
!                            already opened and in definition mode; 
!            r2dgrd  integer array of size 2, which contains NetCDF
!                        IDs for dimensions 'xi_rho' and 'eta_rho'.
!            v2dgrd  integer array of size 2, which contains NetCDF
!                        IDs for dimensions 'eta_v' and 'eta_rho'.
!            u2dgrd  integer array of size 2, which contains NetCDF
!                        IDs for dimensions 'xi_rho' and 'eta_u'.
!            r3dgrd  integer array of size 2, which contains NetCDF
!                        IDs for dimensions 'N' .
!            w3dgrd  integer array of size 2, which contains NetCDF
!                        IDs for dimensions 'N+1'.
      implicit none
      integer ncid, r2dgrd(2), u2dgrd(2), v2dgrd(2), r3dgrd(3), 
     &        w3dgrd(3), nf_ftype, varid, ierr
      character(len=30) strtmp
#include "param.h"
#include "ncscrum.h"
#include "netcdf.inc"
!
! Decide about precision:
!
      if (ncid.eq.ncidrst) then
        nf_ftype=NF_FTYPE
      else
        nf_ftype=NF_FOUT
      endif
!   
!
! Grid type switch: Spherical or Cartesian.
!
      ierr=nf_def_var (ncid, 'spherical', nf_char, 0, 0, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',24,
     &                                   'grid type logical switch')
      ierr=nf_put_att_text (ncid, varid, 'option_T', 9, 'spherical')
      ierr=nf_put_att_text (ncid, varid, 'option_F', 9, 'cartesian')
!
! Physical Dimensions of Model Domain, xl,el.
!
      ierr=nf_def_var (ncid, 'xl', nf_ftype, 0, 0, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',   33,
     &                   'domain length in the XI-direction')
      ierr=nf_put_att_text (ncid, varid, 'units', 5, 'meter')

      ierr=nf_def_var (ncid, 'el', nf_ftype, 0, 0, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',   34,
     &                  'domain length in the ETA-direction')
      ierr=nf_put_att_text (ncid, varid, 'units', 5, 'meter')

!
! Variable-dimensions : xi_rho, eta_rho, eta_v, xi_u
!
      ierr=nf_def_var (ncid, 'xi_rho', nf_ftype, 1, r2dgrd(1), varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',   23,
     &     'x-dimension of the grid')
      ierr=nf_put_att_text (ncid, varid, 'standard_name',12,
     &     'x_grid_index')
      ierr=nf_put_att_text (ncid, varid, 'axis', 1, 'X')
      write(strtmp,'(i0":"i0" ")') 2, xi_rho-1
      ierr=nf_put_att_text (ncid, varid, 'c_grid_dynamic_range',
     &  len_trim(strtmp), trim(strtmp))
!
      ierr=nf_def_var (ncid, 'xi_u', nf_ftype, 1, u2dgrd(1), varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',   37,
     &     'x-dimension of the grid at u location')
      ierr=nf_put_att_text (ncid, varid, 'standard_name', 26, 
     &     'x_grid_index_at_u_location')
      ierr=nf_put_att_text (ncid, varid, 'axis', 1, 'X')
      ierr=nf_put_att_FTYPE(ncid, varid, 'c_grid_axis_shift',
     &     nf_ftype, 1, 0.5)
#if defined EW_PERIODIC
      write(strtmp,'(i0":"i0" ")') 1, xi_u-1
#else
      write(strtmp,'(i0":"i0" ")') 2, xi_u-1
#endif
      ierr=nf_put_att_text (ncid, varid, 'c_grid_dynamic_range',
     &  len_trim(strtmp), trim(strtmp))
!
      ierr=nf_def_var (ncid, 'eta_rho', nf_ftype, 1, r2dgrd(2), varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',   23,
     &     'y-dimension of the grid')
      ierr=nf_put_att_text (ncid, varid, 'standard_name', 12, 
     &     'y_grid_index')
      ierr=nf_put_att_text (ncid, varid, 'axis', 1, 'Y')
      write(strtmp,'(i0":"i0" ")') 2, eta_rho-1
      ierr=nf_put_att_text (ncid, varid, 'c_grid_dynamic_range',
     &  len_trim(strtmp), trim(strtmp))
!
      ierr=nf_def_var (ncid, 'eta_v', nf_ftype, 1, v2dgrd(2), varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',    37,
     &     'y-dimension of the grid at v location')
      ierr=nf_put_att_text (ncid, varid, 'standard_name', 26, 
     &     'x_grid_index_at_v_location')
      ierr=nf_put_att_text (ncid, varid, 'axis', 1, 'Y')
      ierr=nf_put_att_FTYPE(ncid, varid, 'c_grid_axis_shift',
     &     nf_ftype, 1, 0.5)
#if defined NS_PERIODIC
      write(strtmp,'(i0":"i0" ")') 1, eta_v-1
#else
      write(strtmp,'(i0":"i0" ")') 2, eta_v-1
#endif
      ierr=nf_put_att_text (ncid, varid, 'c_grid_dynamic_range',
     &  len_trim(strtmp), trim(strtmp))

#ifdef WET_DRY
!
! Critical Depth for Drying cells
!
      ierr=nf_def_var (ncid, 'Dcrit', nf_ftype, 0, 0, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',   31,
     &                   'Critical Depth for Drying cells')
      ierr=nf_put_att_text (ncid, varid, 'units', 5, 'meter')
#endif

#ifdef SOLVE3D
!
! Vtransform 
!
      ierr=nf_def_var (ncid, 'Vtransform', nf_ftype, 0, 0, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',   49,
     &    'vertical terrain-following transformation equation')    
!
! Variable-dimensions vertical : s_rho and s_w 
!
      ierr=nf_def_var (ncid, 's_rho', nf_ftype, 1, r3dgrd(3), varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',   26,
     &     'S-coordinate at RHO-points')
      ierr=nf_put_att_text (ncid, varid, 'axis', 1, 'Z')
      ierr=nf_put_att_text (ncid, varid, 'positive', 2, 'up')
#if defined NEW_S_COORD
      ierr=nf_put_att_text (ncid, varid, 'standard_name',21,
     &     'ocean_s_coordinate_g2')
#else
      ierr=nf_put_att_text (ncid, varid, 'standard_name',21,
     &     'ocean_s_coordinate_g1')
#endif
      ierr=nf_put_att_text (ncid, varid, 'formula_terms',46,
     &     's: sc_r C: Cs_r eta: zeta depth: h depth_c: hc')
!-
      ierr=nf_def_var (ncid, 's_w', nf_ftype, 1, w3dgrd(3), varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',   24,
     &     'S-coordinate at W-points')
      ierr=nf_put_att_text (ncid, varid, 'axis', 1, 'Z')
      ierr=nf_put_att_text (ncid, varid, 'positive', 2, 'up')
      ierr=nf_put_att_FTYPE(ncid, varid, 'c_grid_axis_shift',
     &     nf_ftype, 1, -0.5)
#if defined NEW_S_COORD
      ierr=nf_put_att_text (ncid, varid, 'standard_name',35,
     &     'ocean_s_coordinate_g2_at_w_location')
#else
      ierr=nf_put_att_text (ncid, varid, 'standard_name',35,
     &     'ocean_s_coordinate_g1_at_w_location')
#endif
      ierr=nf_put_att_text (ncid, varid, 'formula_terms',46,
     &     's: sc_w C: Cs_w eta: zeta depth: h depth_c: hc')
!----
! S-coordinate
      ierr=nf_def_var (ncid, 'sc_r', nf_ftype,  1, r3dgrd(3), varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',   36,
     &     'ocean s roms coordinate at rho point')
#if defined NEW_S_COORD
      ierr=nf_put_att_text (ncid, varid, 'Vtransform',1, '2')
#else
      ierr=nf_put_att_text (ncid, varid, 'Vtransform',1, '1')
#endif /* NEW_S_COORD */    
!
      ierr=nf_def_var (ncid, 'sc_w', nf_ftype, 1, w3dgrd(3), varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',   34,
     &     'ocean s roms coordinate at w point')
#if defined NEW_S_COORD
      ierr=nf_put_att_text (ncid, varid, 'Vtransform',1, '2')
#else
      ierr=nf_put_att_text (ncid, varid, 'Vtransform',1, '1')
#endif /* NEW_S_COORD */ 

!----
! Streching curves Cs_r, Cs_w
      ierr=nf_def_var (ncid, 'Cs_r', nf_ftype,  1, r3dgrd(3), varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',   44,
     &     'S-coordinate stretching curves at RHO-points')
!      ierr=nf_put_att_text (ncid, varid, 'standard_name',40,
!     &     '')
!
      ierr=nf_def_var (ncid, 'Cs_w', nf_ftype, 1, w3dgrd(3), varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',   42,
     &     'S-coordinate stretching curves at W-points')
!      ierr=nf_put_att_text (ncid, varid, 'standard_name',38,
!     &     '')
!----
! hc
      ierr=nf_def_var (ncid, 'hc', nf_ftype, 0, 0, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',   38,
     &                   'S-coordinate parameter, critical depth')
      ierr=nf_put_att_text (ncid, varid, 'units', 5, 'meter')
#endif
!
! Bathymetry.
!
      ierr=nf_def_var (ncid, 'h', nf_ftype, 2, r2dgrd, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',  24,
     &     'bathymetry at RHO-points')
      ierr=nf_put_att_text (ncid, varid, 'units', 5, 'meter')
      ierr=nf_put_att_text (ncid, varid, 'field', 12,
     &     'bath, scalar')
      ierr=nf_put_att_text (ncid, varid, 'standard_name', 33,
     &     'model_sea_floor_depth_below_geoid')
#ifdef SPHERICAL
      ierr=nf_put_att_text (ncid, varid, 'coordinates', 15,
     &                      'lat_rho lon_rho')
#else
      ierr=nf_put_att_text (ncid, varid, 'coordinates', 11,
     &                      'x_rho y_rho')
#endif
!
! Coriolis Parameter.
!
      ierr=nf_def_var (ncid,'f', nf_ftype, 2, r2dgrd, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',   32,
     &                        'Coriolis parameter at RHO-points')
      ierr=nf_put_att_text (ncid, varid, 'units',  8, 'second-1')
      ierr=nf_put_att_text (ncid, varid, 'field', 16,
     &                                        'coriolis, scalar')
      ierr=nf_put_att_text (ncid, varid, 'standard_name', 18,
     &     'coriolis_parameter')
#ifdef SPHERICAL
      ierr=nf_put_att_text (ncid, varid, 'coordinates', 15,
     &                      'lat_rho lon_rho')
#else
      ierr=nf_put_att_text (ncid, varid, 'coordinates', 11,
     &                      'x_rho y_rho')
#endif
!
! Curvilinear coordinates metric coefficients pm,pn.
!
      ierr=nf_def_var (ncid, 'pm', nf_ftype, 2, r2dgrd, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name', 35,
     &                      'curvilinear coordinates metric in XI')
      ierr=nf_put_att_text (ncid, varid, 'units',  7,    'meter-1')
      ierr=nf_put_att_text (ncid, varid, 'field', 10, 'pm, scalar')
#ifdef SPHERICAL
      ierr=nf_put_att_text (ncid, varid, 'coordinates', 15,
     &                      'lat_rho lon_rho')
#else
      ierr=nf_put_att_text (ncid, varid, 'coordinates', 11,
     &                      'x_rho y_rho')
#endif
      ierr=nf_put_att_text (ncid, varid, 'standard_name', 22,
     &                      'inverse_of_cell_x_size')
!-
      ierr=nf_def_var (ncid, 'pn', nf_ftype, 2, r2dgrd, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name', 36,
     &     'curvilinear coordinates metric in ETA')
      ierr=nf_put_att_text (ncid, varid, 'units',  7,    'meter-1')
      ierr=nf_put_att_text (ncid, varid, 'field', 10, 'pn, scalar')
#ifdef SPHERICAL
      ierr=nf_put_att_text (ncid, varid, 'coordinates', 15,
     &                      'lat_rho lon_rho')
#else
      ierr=nf_put_att_text (ncid, varid, 'coordinates', 11,
     &                      'x_rho y_rho')
#endif
      ierr=nf_put_att_text (ncid, varid, 'standard_name', 22,
     &                      'inverse_of_cell_y_size')
!
! Longitude/latitude or cartezian coordinatess of RHO-points.
!
#ifdef SPHERICAL
      ierr=nf_def_var (ncid, 'lon_rho', nf_ftype, 2, r2dgrd, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name', 23,
     &                                   'longitude of RHO-points')
      ierr=nf_put_att_text (ncid, varid, 'units', 11,  'degree_east')
      ierr=nf_put_att_text (ncid, varid, 'field', 15,
     &                                   'lon_rho, scalar')
      ierr=nf_put_att_text (ncid, varid, 'standard_name', 9,'longitude')
      ierr=nf_def_var (ncid, 'lat_rho', nf_ftype, 2, r2dgrd, varid)
      ierr=nf_put_att_text (ncid,varid,'long_name',22,
     &                                 'latitude of RHO-points')
      ierr=nf_put_att_text (ncid, varid, 'units', 12,
     &                                   'degree_north')
      ierr=nf_put_att_text (ncid, varid, 'field', 15,
     &                                   'lat_rho, scalar')
      ierr=nf_put_att_text (ncid, varid, 'standard_name', 8,'latitude')
!=======================================================================
      ierr=nf_def_var (ncid, 'lon_u', nf_ftype, 2, u2dgrd, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name', 21,
     &                                   'longitude of U-points')
      ierr=nf_put_att_text (ncid, varid, 'units', 11,  'degree_east')
      ierr=nf_put_att_text (ncid, varid, 'field', 13,
     &                                   'lon_u, scalar')
      ierr=nf_put_att_text (ncid, varid, 'standard_name', 23,
     &                                   'longitude_at_u_location')
!--
      ierr=nf_def_var (ncid, 'lat_u', nf_ftype, 2, u2dgrd, varid)
      ierr=nf_put_att_text (ncid,varid,'long_name',20,
     &                                 'latitude of U-points')
      ierr=nf_put_att_text (ncid, varid, 'units', 12,
     &                                   'degree_north')
      ierr=nf_put_att_text (ncid, varid, 'field', 13,
     &                                   'lat_u, scalar')
      ierr=nf_put_att_text (ncid, varid, 'standard_name', 22,
     &                                   'latitude_at_u_location')
!=======================================================================
      ierr=nf_def_var (ncid, 'lon_v', nf_ftype, 2, v2dgrd, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name', 21,
     &                                   'longitude of V-points')
      ierr=nf_put_att_text (ncid, varid, 'units', 11,  'degree_east')
      ierr=nf_put_att_text (ncid, varid, 'field', 13,
     &                                   'lon_v, scalar')
      ierr=nf_put_att_text (ncid, varid, 'standard_name', 23,
     &                                   'longitude_at_v_location')
!--
      ierr=nf_def_var (ncid, 'lat_v', nf_ftype, 2, v2dgrd, varid)
      ierr=nf_put_att_text (ncid,varid,'long_name',20,
     &                                 'latitude of V-points')
      ierr=nf_put_att_text (ncid, varid, 'units', 12,
     &                                   'degree_north')
      ierr=nf_put_att_text (ncid, varid, 'field', 13,
     &                                   'lat_v, scalar')
      ierr=nf_put_att_text (ncid, varid, 'standard_name', 22,
     &                                   'latitude_at_v_location')
!=======================================================================
#else
      ierr=nf_def_var (ncid, 'x_rho', nf_ftype, 2, r2dgrd, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name', 25,
     &                                   'x-locations of RHO-points')
      ierr=nf_put_att_text (ncid, varid, 'units', 5, 'meter')
      ierr=nf_put_att_text (ncid, varid, 'field',13, 'x_rho, scalar')
      ierr=nf_put_att_text (ncid, varid, 'standard_name', 18,
     &                                   'plane_x_coordinate')

      ierr=nf_def_var (ncid, 'y_rho', nf_ftype, 2, r2dgrd, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',25,
     &                                   'y-locations of RHO-points')
      ierr=nf_put_att_text (ncid, varid, 'units', 5, 'meter')
      ierr=nf_put_att_text (ncid, varid, 'field',11, 'y_rho, scalar')
      ierr=nf_put_att_text (ncid, varid, 'standard_name', 18,
     &                                   'plane_y_coordinate')
#endif
#ifdef CURVGRID
!
! Angle between XI-axis and EAST at RHO-points
!
      ierr=nf_def_var (ncid, 'angle', nf_ftype, 2, r2dgrd,varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',30,
     &                                 'angle between XI-axis and EAST')
      ierr=nf_put_att_text (ncid, varid, 'units', 7, 'radians')
      ierr=nf_put_att_text (ncid, varid, 'field',13, 'angle, scalar')
!      ierr=nf_put_att_text (ncid, varid, 'standard_name', 1,' ')
      ierr=nf_put_att_text (ncid, varid, 'coordinates', 15,
     &                                   'lat_rho lon_rho')
#endif
#ifdef MASKING
!
! Land-Sea mask at RHO-points.
!
      ierr=nf_def_var (ncid, 'mask_rho', nf_ftype, 2, r2dgrd, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',18,
     &                                   'mask on RHO-points')
      ierr=nf_put_att_text (ncid, varid, 'option_0',    4,   'land' )
      ierr=nf_put_att_text (ncid, varid, 'option_1',    5,   'water')
      ierr=nf_put_att_text (ncid, varid, 'standard_name', 16,
     &                                   'land_binary_mask')
# ifdef SPHERICAL
      ierr=nf_put_att_text (ncid, varid, 'coordinates', 15,
     &                      'lat_rho lon_rho')
# else
      ierr=nf_put_att_text (ncid, varid, 'coordinates', 11,
     &                      'x_rho y_rho')
# endif
#endif
!
#else /* SOLVE3D */
      subroutine def_grid_3d_empty
#endif  /* SOLVE3D */ 
      return
      end



