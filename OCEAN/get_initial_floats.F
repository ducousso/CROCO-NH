! $Id: get_initial_floats.F 1458 2014-02-03 15:01:25Z gcambon $
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
#ifdef FLOATS
                                   ! Read initial conditions for 
      subroutine get_initial_floats! the floats variables from 
                                   ! NetCDF initialization file.
      implicit none
#include "param.h"
#include "scalars.h"
#include "ncscrum.h"
#include "netcdf.inc"
#include "floats.h"
      real bufftrack(1:6,NFT+1,Mfloats) 
      integer i, j, ncid, indx, varid,  ierr, lstr, lvar, lenstr
     &    , start(2), count(2), ibuff(Mfloats), checkdims, temp(4)
     &    , startTinfo(3), countTinfo(3)
     &    , starttrack(4), counttrack(4)

!
! Open initial conditions netCDF file for floats reading. 
! set record from which the data will be read.
!
! The record is set as follows: (1) if only one time record is
! available in the file, then that record is used REGARDLESS of
! value of nrrec supplied from the parameter file; (2) if the
! file has multiple records and nrrec is positive, then nrrec is
! used, provided that nrrec is within the available records; 
! (3) if the file has multiple records and nrrec<0, then THE LAST 
! available record is used.
!
      if (may_day_flag.ne.0) return      !-->  EXIT
      lstr=lenstr(ininame)
      ierr=nf_open(ininame(1:lstr), nf_nowrite, ncid)
      if (ierr .ne. nf_noerr) write (stdout,*) 'DAUBE'
      ierr=checkdims (ncid, ininame, lstr, indx)
      if (ierr .ne. nf_noerr) write (stdout,*) 'DAUBE2'
      if (indx.eq.0) then
          indx=1
      elseif (indx.gt.0 .and. nrrec.gt.0 .and. nrrec.le.indx) then
          indx=nrrec
      endif

c     if (ierr .ne. nf_noerr) then
c       write(stdout,'(/1x,2A/15x,3A)') 'GET_INITIAL_FLOATS ERROR: ',
c        ' Cannot open netCDF file', '''', ininame(1:lstr) ,'''.'
c       goto 99                                           !--> ERROR
c     endif
!
! ---- -- -------- ----- ----------
! nfloats: find netCDF id, read value
!
      ierr=nf_inq_varid (ncid, 'nfloats', varid)
      ierr=nf_get_var1_int (ncid, varid, indx, nfloats)
      if (ierr .ne. nf_noerr) then
          write (stdout,'(/1x,2A/8x,2A/)') 'GET_INITIAL_FLOATS ',
     &            'ERROR: Cannot read nfloats in netCDF file ', 
     &             ininame(1:lstr),'.' 
              goto 99                                    !--> ERROR
      endif

!------------------------------------
! Tinfo: find netCDF id and read values

      ierr=nf_inq_varid (ncid, 'Tinfo', varid)
      if (ierr .eq. nf_noerr) then
          startTinfo(1)=1
          startTinfo(2)=1
          startTinfo(3)=indx
          countTinfo(1)=5
          countTinfo(2)=nfloats
          countTinfo(3)=1
          ierr=nf_get_vara_FTYPE (ncid, varid, startTinfo, 
     &                          countTinfo, Tinfo)
          if (ierr .ne. nf_noerr) then
              write (stdout,'(/1x,2A/8x,2A/)') 'GET_INITIAL_FLOATS ',
     &            'ERROR: Cannot read Tinfo in netCDF file ',
     &             ininame(1:lstr),'.'
              goto 99                                    !--> ERROR
          endif
      endif

!------------------------------------
! grid level :  find netCDF id and read values

      ierr=nf_inq_varid (ncid, 'grid_level', varid)
      if (ierr .eq. nf_noerr) then
          start(1)=1
          start(2)=indx
          count(1)=nfloats
          count(2)=1
          ierr=nf_get_vara_int (ncid, varid, start, count,fltgrd)
          if (ierr .ne. nf_noerr) then
              write (stdout,'(/1x,2A/8x,2A/)') 'GET_INITIAL_FLOATS ',
     &            'ERROR: Cannot read grid_level in netCDF file ',
     &             ininame(1:lstr),'.'
              goto 99                                    !--> ERROR
          endif
      endif

!------------------------------------
! track: find netCDF id and read values

      ierr=nf_inq_varid (ncid, 'track', varid)
      if (ierr .eq. nf_noerr) then
          starttrack(1)=1
          starttrack(2)=1
          starttrack(3)=1
          starttrack(4)=indx
          counttrack(1)=6
          counttrack(2)=NFT+1
          counttrack(3)=nfloats
          counttrack(4)=1

          ierr=nf_get_vara_FTYPE (ncid, varid, starttrack, 
     &                          counttrack, bufftrack)

          if (ierr .eq. nf_noerr) then

              temp(1)=nfp1  ! IMPORTANT: takes into account 
              temp(2)=nf    ! that these time step indices 
              temp(3)=nfm1  ! will be advanced once before 
              temp(4)=nfm2  ! 1rst computation

              do 10 j=1,nfloats
              do 10 i=1,NFT+1
                  track(ixgrd,temp(i),j)=bufftrack(1,i,j)
                  track(iygrd,temp(i),j)=bufftrack(2,i,j)
                  track(izgrd,temp(i),j)=bufftrack(3,i,j)
                  track(ixrhs,temp(i),j)=bufftrack(4,i,j)
                  track(iyrhs,temp(i),j)=bufftrack(5,i,j)
                  track(izrhs,temp(i),j)=bufftrack(6,i,j)
 10           continue

          else
              write (stdout,'(/1x,2A/8x,2A/)') 'GET_INITIAL_FLOATS ',
     &         'ERROR: Cannot read track in netCDF file ',
     &             ininame(1:lstr),'.'
              goto 99                                    !--> ERROR
          endif
      endif
!
!  Close input NetCDF file.
!
      ierr=nf_close(ncid)
      return
  99  may_day_flag=2
      return
      end

#else 

      subroutine get_initial_floats_empty
      return
      end
#endif  /* FLOATS */
