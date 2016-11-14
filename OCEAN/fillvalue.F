! $Id: fillvalue.F 1458 2014-02-03 15:01:25Z gcambon $
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
      subroutine fillvalue2d(work2d_tmp,ncid,vid,indx,
     &                       record,typevar2d,typefile)
      implicit none
      
#include "param.h"
!     Declaration
      real work2d_tmp(GLOBAL_2D_ARRAY)
      logical mask(GLOBAL_2D_ARRAY) 
      real mask2(GLOBAL_2D_ARRAY)
      integer ierr, record, lstr, lvar, lenstr,
     &     nf_fwrite, typevar2d, vid, ncid, indx, typefile
      character*20 text
      
!     Include the common      
#include "scalars.h"
#include "ncscrum.h"
#include "grid.h"
#include "netcdf.inc"

#ifdef MASKING
      if (typevar2d.eq.r2dvar) then
         mask2=rmask
      elseif (typevar2d.eq.u2dvar) then
         mask2=umask
      elseif (typevar2d.eq.v2dvar)then
         mask2=vmask
      endif    
!
      if (typefile ==filetype_his) then
         text='WRT_HIS'
      elseif (typefile ==filetype_avg) then
         text='WRT_AVG'
      elseif (typefile ==filetype_dia) then
         text='WRT_DIA'
      elseif (typefile ==filetype_dia_avg) then
         text='WRT_DIA_AVG'
      elseif (typefile ==filetype_diaM) then
         text='WRT_DIAM'
      elseif (typefile ==filetype_diaM_avg) then
         text='WRT_DIAM_AVG'
      endif
 !
# ifdef FILLVAL
! Fill with spval where rmask==0 .
      mask = (work2d_tmp.eq.mask2)
      WHERE (mask.eqv.mask_val)
          work2d_tmp(:,:) = spval
      END WHERE
# endif
#endif
      ierr=nf_fwrite(work2d_tmp, ncid, vid, record, typevar2d)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indx))
         write(stdout,1) TRIM(text), vname(1,indx)(1:lvar), record, ierr
     &        MYID
         goto 99                !--> ERROR
      endif

 1    format(/1x, A,' ERROR while writing variable ''', A,
     &     ''' into history file.', /11x, 'Time record:',
     &     I6,3x,'netCDF error code',i4,3x,a,i4)   

      goto 100 
  99  may_day_flag=3
 100  continue      
      
      return
      end

!====================================================================
      
      subroutine fillvalue3d(work3d_tmp,ncid,vid,indx,
     &                       record,typevar3d,typefile)
      implicit none
      
#include "param.h"
!     Declaration
      real work3d_tmp(GLOBAL_2D_ARRAY,N)
      real work2d_tmp(GLOBAL_2D_ARRAY)
      integer ierr, record, lstr, lvar, lenstr,
     &     nf_fwrite, typevar3d, vid, ncid, indx, k, typefile
      logical mask(GLOBAL_2D_ARRAY)
      real mask2(GLOBAL_2D_ARRAY)
      character*20 text
!     Include the common      
#include "scalars.h"
#include "ncscrum.h"
#include "grid.h"
#include "netcdf.inc"
!   
#ifdef MASKING
      if (typevar3d.eq.r3dvar) then
         mask2=rmask
      elseif (typevar3d.eq.w3dvar) then
         mask2=rmask
      elseif (typevar3d.eq.u3dvar) then
         mask2=umask
      elseif (typevar3d.eq.v3dvar) then
         mask2=vmask
      endif   
!
      if (typefile ==filetype_his) then
         text='WRT_HIS'
      elseif (typefile ==filetype_avg) then
         text='WRT_AVG'
      elseif (typefile ==filetype_dia) then
         text='WRT_DIA'
      elseif (typefile ==filetype_dia_avg) then
         text='WRT_DIA_AVG'
      elseif (typefile ==filetype_diaM) then
         text='WRT_DIAM'
      elseif (typefile ==filetype_diaM_avg) then
         text='WRT_DIAM_AVG'
      endif   
!     
# ifdef FILLVAL
      work2d_tmp = work3d_tmp(:,:,1)
      mask = (work2d_tmp.eq.mask2)
        do k=1,N
          WHERE (mask.eqv.mask_val)
            work3d_tmp(:,:,k) = spval
          END WHERE
      enddo
# endif
#endif
      ierr=nf_fwrite(work3d_tmp, ncid, vid, record,typevar3d)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indx))
         write(stdout,1) TRIM(text), vname(1,indx)(1:lvar), record, ierr
     &        MYID
         goto 99                !--> ERROR
      endif

  1    format(/1x, A,' ERROR while writing variable ''', A,
     &     ''' into history file.', /11x, 'Time record:',
     &     I6,3x,'netCDF error code',i4,3x,a,i4)   

      goto 100 
 99   may_day_flag=3
 100  continue      
      
      return
      end

!====================================================================

      subroutine fillvalue3d_w(work3d_tmp,ncid,vid,indx,
     &                       record,typevar3d,typefile)
      implicit none
      
#include "param.h"
!     Declaration
      real work3d_tmp(GLOBAL_2D_ARRAY,0:N)
      real work2d_tmp(GLOBAL_2D_ARRAY)
      integer ierr, record, lstr, lvar, lenstr,
     &     nf_fwrite, typevar3d, vid, ncid, indx, k, nn, typefile
      logical mask(GLOBAL_2D_ARRAY)
      real mask2(GLOBAL_2D_ARRAY)
      character*20 text
!     Include the common      
#include "scalars.h"
#include "ncscrum.h"
#include "grid.h"
#include "netcdf.inc"
!
#ifdef MASKING     
      if (typevar3d.eq.r3dvar) then
         mask2=rmask
      elseif (typevar3d.eq.w3dvar) then
         mask2=rmask
      elseif (typevar3d.eq.u3dvar) then
         mask2=umask
      elseif (typevar3d.eq.v3dvar) then
         mask2=vmask
      endif  
!
      if (typefile ==filetype_his) then
         text='WRT_HIS'
      elseif (typefile ==filetype_avg) then
         text='WRT_AVG'
      elseif (typefile ==filetype_dia) then
         text='WRT_DIA'
      elseif (typefile ==filetype_dia_avg) then
         text='WRT_DIA_AVG'
      elseif (typefile ==filetype_diaM) then 
         text='WRT_DIAM'
      elseif (typefile ==filetype_diaM_avg) then
         text='WRT_DIAM_AVG'
      endif   
!     
# ifdef FILLVAL
      work2d_tmp = work3d_tmp(:,:,1)
      mask = (work2d_tmp.eq.mask2)
      do k=0,N
         WHERE (mask.eqv.mask_val)
            work3d_tmp(:,:,k) = spval
        END WHERE
      enddo
# endif
#endif      
      ierr=nf_fwrite(work3d_tmp, ncid, vid, record,typevar3d)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indx))
         write(stdout,1) TRIM(text), vname(1,indx)(1:lvar), record, ierr
     &        MYID
         goto 99                !--> ERROR
      endif
      
 1    format(/1x, A,' ERROR while writing variable ''', A,
     &     ''' into history file.', /11x, 'Time record:',
     &     I6,3x,'netCDF error code',i4,3x,a,i4)
      
      goto 100 
 99   may_day_flag=3
 100  continue      
      
      return
      end
      
