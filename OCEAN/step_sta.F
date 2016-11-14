! $Id: step_sta.F 1458 2014-02-03 15:01:25Z gcambon $
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

#ifdef STATIONS
      subroutine step_sta (Lstr,Lend)
!
!  step station variables
!=====================================================================
!
# ifdef AGRIF
      use Agrif_Util
# endif
      implicit none
#include "param.h"
#include "sta.h"
#include "grid.h"
#include "nc_sta.h"
#include "ocean2d.h"
#include "ocean3d.h"
#include "scalars.h"
#include "coupling.h"
      integer Lstr, Lend, mon_thread, tmpnfp1, tmpnf, 
     &        tmpnfm1, tmpnfm2, entier
      integer i, itrc, iflt, level, rcoeft,rcoefx,rcoefy, 
     &        k, xfloat, yfloat, index1, index2, i1, j1
      real    cff1, cff2, cff3, cff4, xrhs, yrhs, zrhs,
     &        invrcoeft, tmptrack , zfloat, d1, d2, temp, summ,
     &        temp2,tmp,tmp2
      integer nfltmax, indx(Lend-Lstr+1), nfltmax_bak,id
# ifdef AGRIF
      logical test
# endif      
      itrc=0
      zrhs=0.0

# ifdef AGRIF
      level=Agrif_Fixed()
      rcoeft=int(Agrif_rhot())
      rcoefx=Agrif_irhox()
      rcoefy=Agrif_irhoy()
      invrcoeft=1./float(rcoeft)
# else
      level=0
      rcoeft=1
      rcoefx=1
      rcoefy=1
      invrcoeft=1.
# endif
!
! check if the station can be transfered to a finer
! grid
!
# ifdef AGRIF 
      if (Agrif_nb_step() .eq. 0) then 
         do iflt=Lstr,Lend
            if (NINT(stainfo(istagrd,iflt)) .eq. level) then 
              entier=nint(stainfo(istagrd,iflt))
              call Agrif_transfer_floatsp2c(entier,
     &                              stainfo(istaxgrd,iflt),
     &                              stainfo(istaygrd,iflt),stadeltap2c)
              stainfo(istagrd,iflt)=float(entier)
            endif
         enddo
      endif  ! first time step
# endif /* AGRIF */
!
! Save indices of stations to be processed into a special array.
!
      nfltmax=0
      do iflt=Lstr,Lend
# ifdef AGRIF 
        if (stainfo(istagrd,iflt).eq.level) then
# endif /* AGRIF */
          nfltmax=nfltmax+1
          indx(nfltmax)=iflt
# ifdef AGRIF 
        endif
# endif /* AGRIF */
!
! Determine of  sigma level depth 
! using the child grid bathymetry and the four surrounding points 
!
# ifndef ALL_SIGMA
#  ifdef SOLVE3D
        zfloat=stadata(istazgrd,iflt)
        if (zfloat.lt.0.0) then
          xfloat=INT(stainfo(istaxgrd,iflt))
          d1=stainfo(istaxgrd,iflt)-xfloat
          yfloat=INT(stainfo(istaygrd,iflt))
          d2=stainfo(istaygrd,iflt)-yfloat
          stainfo(istazgrd,iflt)=0. ! default bottom value
          summ=0.
          do index1=0,1
            xfloat=xfloat+index1
            do index2=0,1
              yfloat=yfloat+index2
              temp=((1-index1)*(1-d1)+index1*d1)*
     &       ((1-index2)*(1-d2)+index2*d2)
              summ=summ+temp
              do k=N,1,-1
                if ((z_w(xfloat,yfloat,k)-zfloat)*
     &             (zfloat-z_w(xfloat,yfloat,k-1)).ge.0.0) then
                  temp2=(FLOAT(k-1)+
     &            (zfloat-z_w(xfloat,yfloat,k-1))/Hz(xfloat,yfloat,k))
                  stainfo(istazgrd,iflt)=stainfo(istazgrd,iflt)+temp2*
     &           temp
                endif
              enddo
            enddo
          enddo
          if (summ .ne. 0) then
            stainfo(istazgrd,iflt)=stainfo(istazgrd,iflt)/summ
          endif
        endif
c       write(*,*) 'FLT# ',iflt, 'stazgrd= ', stainfo(istazgrd,iflt)
#  else
        stainfo(istazgrd,iflt)=0.0
#  endif
# endif /* ALL_SIGMA */
      enddo
 
!---------------------------------------------------------------------
!  Interpolate various output variables at the corrected locations,
!  if writing occurs at next time step. Not optimal yet since 
!  diags routines are called three times for level 3 (instead of one). 
!  Build an AMR function to optimize this. 
!---------------------------------------------------------------------
       
      if (diagsta) then

# ifdef AGRIF
        call Agrif_laststep(test)
        if (test) then
# endif

# ifdef SPHERICAL
          call interp_r2d_sta (lonr(START_2D_ARRAY), istalon,
     &                                      nfltmax, indx)
          call interp_r2d_sta (latr(START_2D_ARRAY), istalat,
     &                                      nfltmax, indx)
# else
          call interp_r2d_sta (  xr(START_2D_ARRAY), istalon,
     &                                      nfltmax, indx)
          call interp_r2d_sta (  yr(START_2D_ARRAY), istalat,
     &                                      nfltmax, indx)
# endif

# ifdef SOLVE3D
          call interp_r2d_sta (Zt_avg1(START_2D_ARRAY), istaz,
     &                                      nfltmax, indx)
# else
          call interp_r2d_sta (zeta(START_2D_ARRAY,knew), istaz,
     &                                      nfltmax, indx)
          if (wrtsta(indxstaVel)) then 
            call interp_r2d_sta (ubar(START_2D_ARRAY,knew), istau,
     &                                       nfltmax, indx)
            call interp_r2d_sta (vbar(START_2D_ARRAY,knew), istav,
     &                                      nfltmax, indx)
          endif

# endif

# ifdef SOLVE3D
#  ifdef ALL_SIGMA
          do k=1,N
           do iflt=Lstr,Lend
            stainfo(istazgrd,iflt)=float(k)
           enddo
#  endif
           call interp_w3d_sta (z_w(START_2D_ARRAY,0), istadpt,
     &                                       nfltmax, indx)
 
           if (wrtsta(indxstaRho)) then 
             call interp_r3d_sta (rho(START_2D_ARRAY,1), istaden,
     &                                      nfltmax, indx)
           endif

           if (wrtsta(indxstaVel)) then 
             call interp_r3d_sta (u(START_2D_ARRAY,1,nnew), istau,
     &                                       nfltmax, indx)
             call interp_r3d_sta (v(START_2D_ARRAY,1,nnew), istav,
     &                                      nfltmax, indx)
           endif
 
           if (wrtsta(indxstaTemp)) then
             itrc=1
             call interp_r3d_sta (t(START_2D_ARRAY,1,nnew,itrc),
     &                            istatem, nfltmax, indx)
           endif

#  ifdef SALINITY
           if (wrtsta(indxstaSalt)) then
             itrc=2
             call interp_r3d_sta (t(START_2D_ARRAY,1,nnew,itrc),
     &                             istasal, nfltmax, indx)
           endif
#  endif
#  ifdef ALL_SIGMA
           do iflt=Lstr,Lend
            staSigm(istadpt,iflt,k)=stadata(istadpt,iflt)
            if (wrtsta(indxstaRho)) then 
              staSigm(istaden,iflt,k)=stadata(istaden,iflt)
            endif
            if (wrtsta(indxstaVel)) then 
              staSigm(istau,iflt,k)=stadata(istau,iflt)
              staSigm(istav,iflt,k)=stadata(istav,iflt)
            endif
            if (wrtsta(indxstaTemp)) then
              staSigm(istatem,iflt,k)=stadata(istatem,iflt)
            endif
#   ifdef SALINITY
            if (wrtsta(indxstaSalt)) then
              staSigm(istasal,iflt,k)=stadata(istasal,iflt)
            endif
#   endif
           enddo
c           write(*,*), 'sigma ', stainfo(istazgrd,1), 
c     &      'depth', staSigm(istadpt,1,k), ' temp ',
c     &       staSigm(istatem,1,k)
          enddo ! all_sigma k cycle
#  endif  /* ALL_SIGMA */
# endif /* SOLVE3D */

          if (wrtsta(indxstaGrd)) then
            do id=1,nfltmax
              iflt=indx(id)
              stadata(istaxgrd,iflt)=stainfo(istaxgrd,iflt)
              stadata(istaygrd,iflt)=stainfo(istaygrd,iflt)
            enddo
          endif

# ifdef AGRIF
        endif  !Agrif_laststep
# endif 
      endif  !diagsta
      return
      end

#else 

      subroutine step_sta_empty 
      return
      end

#endif /* STATIONS */

