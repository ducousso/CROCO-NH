! $Id: online_bulk_var.F 1458 2014-02-03 15:01:25Z gcambon $
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
!
!  This is the "online_bulk_var.F" script
!------------------------------------------------------------------------------
!  This file contains the subfunctions enabling the specificatin of the files 
!  names and variables names for the online NCEP/CFSR tool:
!------------------------------------------------------------------------------
#include "cppdefs.h"
#if defined BULK_FLUX
#if defined ONLINE /* JG ONLINE */

!*******************************************************************************
      subroutine get_bulknetcdf_names(bvar)
!------------------------------------------------------------------------------
!  This subfunction specify the path and netcdf file name of the forcing field
!  used to ensure the online bulk forcing in the online NCEP/CFSR tool. This 
!  name depends on the considered variable as well as the year (Y) and the month 
!  (M) of the ongoing computation
!------------------------------------------------------------------------------
!  The names are adapted using:
!  - the path name: pathbulk
!  - the variable name: blk_vname(1,bvar)
!  - the year Y: bulkyear
!  - the month M: bulkmonth1/2
!
!  INPUT
!  bvar (index of the considered variable)
!  OUTPUT
!  bulkfilename(bvar) (path and file name of the considered variable)
      implicit none
# include "param.h"
# include "online.h"
      integer, intent(in) :: bvar
      character bulkyear*4, bulkmonth1*1, bulkmonth2*2
      integer lenstr
      ! If first iteration, Y and M from roms.in file
      if(itbulkO(bvar).eq.0) then
        bulkyearnum(bvar)=yearnum
        bulkmonthnum(bvar)=monthnum
      endif

      ! Variable to add the year in the name
      write(unit=bulkyear,fmt='(I4)') bulkyearnum(bvar) 

      if(bulkmonthnum(bvar).lt.10) then
        ! Variable to add the month in the name (month<10)
        write(unit=bulkmonth1,fmt='(I1)') bulkmonthnum(bvar)   
        ! Filename
        bulkfilename(bvar)=pathbulk(1:lenstr(pathbulk))//
     &  blk_vname(1,bvar)(1:lenstr(blk_vname(1,bvar)))//
     &  '_Y'//bulkyear//'M'//bulkmonth1//'.nc'
      else
        ! Variable to add the mont in the name (month>10)
        write(unit=bulkmonth2,fmt='(I2)') bulkmonthnum(bvar) 
        ! Filename  
        bulkfilename(bvar)=pathbulk(1:lenstr(pathbulk))//
     &  blk_vname(1,bvar)(1:lenstr(blk_vname(1,bvar)))//
     &  '_Y'//bulkyear//'M'//bulkmonth2//'.nc'
      endif

      return
      end


!*******************************************************************************
      subroutine get_bulkvar_names
!------------------------------------------------------------------------------
!  This subfunction specify the variables names and caracteristics associated 
!  with the various forcing fields used to generate online bulk forcing in the 
!  online NCEP/CFSR tool:
!------------------------------------------------------------------------------
!  For each variable:
!  - blk_vname(1,X)=netcdf variable name
!  - blk_vname(2,X)=details regarding the variable
!  - blk_vname(3,X)=units of the variable
!  - blk_vname(4,X)=?????
!
! OUTPUT
!  Variables names and characteristics in "blk_vname"
      implicit none
# include "param.h"
# include "online.h"

#  ifdef ERA_ECMWF

      ! Temperature_height_above_ground
      blk_vname(1,1)='T2M                                      '
      blk_vname(2,1)='Temperature                              '
      blk_vname(3,1)='Kelvin                                   '
      blk_vname(4,1)='??????                                   '

      ! Specific_humidity 
      blk_vname(1,2)='Q                                        '
      blk_vname(2,2)='Specific Humidity at surface             '
      blk_vname(3,2)='Kg/Kg                                    '
      blk_vname(4,2)='??????                                   '

      ! Precipitation_rate
      blk_vname(1,3)='TP                                       '
      blk_vname(2,3)='Precipitation_rate                       '
      blk_vname(3,3)='kg/m^2/s                                 '
      blk_vname(4,3)='??????                                   '

      ! Downward_Short-Wave_Rad_Flux_surface
      blk_vname(1,4)='xxx                                      '
      blk_vname(2,4)='xxx                                      '
      blk_vname(3,4)='W/m^2                                    '
      blk_vname(4,4)='??????                                   '

      ! Upward_Short-Wave_Rad_Flux_surface 
      blk_vname(1,5)='SSR                                      '
      blk_vname(2,5)='NET SHORT WAVE!!!                        '
      blk_vname(3,5)='W/m^2                                    '
      blk_vname(4,5)='??????                                   '

      ! Downward_Long-Wave_Rad_Flux
      blk_vname(1,6)='STRD                                     '
      blk_vname(2,6)='Downward_Long-Wave_Rad_Flux              '
      blk_vname(3,6)='W/m^2                                    '
      blk_vname(4,6)='??????                                   '

      ! Upward_Long-Wave_Rad_Flux_surface
      blk_vname(1,7)='NOPE                                     '
      blk_vname(2,7)='??????                                   '
      blk_vname(3,7)='W/m^2                                    '
      blk_vname(4,7)='??????                                   '

      ! U-component_of_wind 
      blk_vname(1,8)='U10M                                     '
      blk_vname(2,8)='U10M                                     '
      blk_vname(3,8)='m/s                                      '
      blk_vname(4,8)='??????                                   '

      ! V-component_of_wind
      blk_vname(1,9)='V10M                                     '
      blk_vname(2,9)='V10M                                     '
      blk_vname(3,9)='m/s                                      '
      blk_vname(4,9)='??????                                   '

#  else

      ! Temperature_height_above_ground
      blk_vname(1,1)='Temperature_height_above_ground          '
      blk_vname(2,1)='??????                                   '
      blk_vname(3,1)='??????                                   '
      blk_vname(4,1)='??????                                   '

      ! Specific_humidity 
      blk_vname(1,2)='Specific_humidity                        '
      blk_vname(2,2)='Specific Humidity at surface             '
      blk_vname(3,2)='Kg/Kg                                    '
      blk_vname(4,2)='??????                                   '

      ! Precipitation_rate
      blk_vname(1,3)='Precipitation_rate                       '
      blk_vname(2,3)='??????                                   '
      blk_vname(3,3)='kg/m^2/s                                 '
      blk_vname(4,3)='??????                                   '

      ! Downward_Short-Wave_Rad_Flux_surface
      blk_vname(1,4)='Downward_Short-Wave_Rad_Flux_surface     '
      blk_vname(2,4)='??????                                   '
      blk_vname(3,4)='W/m^2                                    '
      blk_vname(4,4)='??????                                   '

      ! Upward_Short-Wave_Rad_Flux_surface 
      blk_vname(1,5)='Upward_Short-Wave_Rad_Flux_surface       '
      blk_vname(2,5)='??????                                   '
      blk_vname(3,5)='W/m^2                                    '
      blk_vname(4,5)='??????                                   '

      ! Downward_Long-Wave_Rad_Flux
      blk_vname(1,6)='Downward_Long-Wave_Rad_Flux              '
      blk_vname(2,6)='??????                                   '
      blk_vname(3,6)='W/m^2                                    '
      blk_vname(4,6)='??????                                   '

      ! Upward_Long-Wave_Rad_Flux_surface
      blk_vname(1,7)='Upward_Long-Wave_Rad_Flux_surface        '
      blk_vname(2,7)='??????                                   '
      blk_vname(3,7)='W/m^2                                    '
      blk_vname(4,7)='??????                                   '

      ! U-component_of_wind 
      blk_vname(1,8)='U-component_of_wind                      '
      blk_vname(2,8)='??????                                   '
      blk_vname(3,8)='m/s                                      '
      blk_vname(4,8)='??????                                   '

      ! V-component_of_wind
      blk_vname(1,9)='V-component_of_wind                      '
      blk_vname(2,9)='??????                                   '
      blk_vname(3,9)='m/s                                      '
      blk_vname(4,9)='??????                                   '

#  endif

      return
      end

#endif /* JG ONLINE */
#endif /* BULK_FLUX */

