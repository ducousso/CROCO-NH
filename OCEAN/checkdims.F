! $Id: checkdims.F 1458 2014-02-03 15:01:25Z gcambon $
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

      integer function checkdims (ncid, ncname, lstr, recsize)
!
! Verify that all spatial dimensions in file with netCDF ID=ncid and
! name=ncname are consistent with the corresponding model dimensions
! and find the current size of unlimited dimension. 
!
! input:  ncid       netCDF ID, name and length of name of
!         ncname     input netCDF file (all three arguments
!         lstr       arguments are assumed defined).
!
! output: checkdims  (returned value) error status [=nf_noerr=0,
!                                          if no error occurs.]
!         recsize    current size of unlimited dimension
!                    [=0, if there is no unlimited dimension].
!
      implicit none
      character     ncname*80
      character     dimname*16
      integer ncid,    lstr,     ndims,  nvars, ngatts, recdim, 
     &        recsize, ierr,     dimsize,    i,   ldim,  lenstr 
#include "param.h"
#include "ncscrum.h"
#include "netcdf.inc"

      recsize=0 !--> will remain 0, if no unlimited dimension exists.

      ierr=nf_inq (ncid, ndims, nvars, ngatts, recdim)
      if (ierr .ne. nf_noerr) then
        write(stdout,'(/1x,4A/)') 'CHECKDIMS ERROR while inquiring ',
     &                  'about netCDF file ''', ncname(1:lstr), '''.'
      else 
        do i=1,ndims
          ierr=nf_inq_dim (ncid, i, dimname, dimsize)
          if (ierr .ne. nf_noerr) then
            write(stdout,'(/1x,2A,I3/8x,3A/)') 'CHECKDIMS ERROR ',
     &                  'while inquiring about dimension ID =', i,
     &                  'in netCDF file ''', ncname(1:lstr), '''.' 
            goto 99                                       !--> ERROR
          endif
          ldim=lenstr(dimname)
          if ((ldim.eq.6 .and. dimname(1:ldim).eq.'xi_rho') .or. 
     &        (ldim.eq.4 .and. dimname(1:ldim).eq.'xi_v'  )) then
            if (dimsize.ne.xi_rho) then
              write(stdout,1) dimname(1:ldim), dimsize, xi_rho
              goto 99                                     !--> ERROR
            endif
          elseif ((ldim.eq.4 .and. dimname(1:ldim).eq.'xi_u'  ) .or.
     &            (ldim.eq.6 .and. dimname(1:ldim).eq.'xi_psi')) then
            if (dimsize.ne.xi_u) then
              write(stdout,1) dimname(1:ldim), dimsize, xi_u
              goto 99                                     !--> ERROR
            endif
          elseif ((ldim.eq.7 .and. dimname(1:ldim).eq.'eta_rho') .or. 
     &            (ldim.eq.5 .and. dimname(1:ldim).eq.'eta_u' )) then
            if (dimsize.ne.eta_rho) then
              write(stdout,1) dimname(1:ldim), dimsize, eta_rho
              goto 99                                     !--> ERROR
            endif
          elseif ((ldim.eq.7 .and. dimname(1:ldim).eq.'eta_v'  ) .or.
     &            (ldim.eq.5 .and. dimname(1:ldim).eq.'eta_psi'))then
            if (dimsize.ne.eta_v) then
              write(stdout,1) dimname(1:ldim), dimsize, eta_v
              goto 99                                     !--> ERROR
            endif
#ifdef SOLVE3D
          elseif (ldim.eq.5 .and. dimname(1:ldim).eq.'s_rho') then
            if (dimsize.ne.N) then
              write(stdout,1) dimname(1:ldim), dimsize, N
              goto 99                                     !--> ERROR
            endif
          elseif (ldim.eq.3 .and. dimname(1:ldim).eq.'s_w') then
            if (dimsize.ne.N+1) then
              write(stdout,1) dimname(1:ldim), dimsize, N+1
              goto 99                                     !--> ERROR
            endif
#endif
          elseif (i.eq.recdim) then
            recsize=dimsize
          endif
        enddo
        checkdims=nf_noerr 
        return
      endif
  1   format(/' CHECKDIMS ERROR: inconsistent size of dimension ''',
     &                      A, ''':', i5, 1x, '(must be', i5, ').'/)
  99  checkdims=nf_noerr+1 
      return
      end
