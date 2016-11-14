! $Id: interp_sta.F 1458 2014-02-03 15:01:25Z gcambon $
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
      subroutine interp_r3d_sta (A, ifld, nfltmax, indx)
      implicit none
# include "param.h"
# include "grid.h"
# include "sta.h"
# include "scalars.h"
      integer ifld, nfltmax, indx(nfltmax)
      real A(GLOBAL_2D_ARRAY,N)
      integer id,iflt, i1,i2, j1,j2, k1,k2
      real p1,p2, q1,q2, r1,r2, cff1, cff2

      do id=1,nfltmax
        iflt=indx(id)

        k1=int(stainfo(istazgrd,iflt)+0.5)
        r2=stainfo(istazgrd,iflt)+0.5 - float(k1)
        r1=1.-r2
        k1=max(k1,   1)
        k2=min(k1+1, N)

        i1=int(stainfo(istaxgrd,iflt))
        i2=i1+1
        p2=stainfo(istaxgrd,iflt)-float(i1)
        p1=1.-p2

        j1=int(stainfo(istaygrd,iflt))
        j2=j1+1
        q2=stainfo(istaygrd,iflt)-float(j1)
        q1=1.0-q2

#ifdef MASKING
        cff1=p1*q1*rmask(i1,j1)*(r1*A(i1,j1,k1)+r2*A(i1,j1,k2))
     &      +p2*q1*rmask(i2,j1)*(r1*A(i2,j1,k1)+r2*A(i2,j1,k2))
     &      +p1*q2*rmask(i1,j2)*(r1*A(i1,j2,k1)+r2*A(i1,j2,k2))
     &      +p2*q2*rmask(i2,j2)*(r1*A(i2,j2,k1)+r2*A(i2,j2,k2))

        cff2=q1*(p1*rmask(i1,j1) + p2*rmask(i2,j1))
     &      +q2*(p1*rmask(i1,j2) + p2*rmask(i2,j2))

        if (cff2.gt.0.) then
          stadata(ifld,iflt)=cff1/cff2
        else
          stadata(ifld,iflt)=0.0
        endif
#else
        stadata(ifld,iflt)=p1*q1*(r1*A(i1,j1,k1)+r2*A(i1,j1,k2))
     &                    +p2*q1*(r1*A(i2,j1,k1)+r2*A(i2,j1,k2))
     &                    +p1*q2*(r1*A(i1,j2,k1)+r2*A(i1,j2,k2))
     &                    +p2*q2*(r1*A(i2,j2,k1)+r2*A(i2,j2,k2))
#endif
      enddo
      return
      end
!
!======================================================================
!
      subroutine interp_w3d_sta (A, ifld, nfltmax,indx)
      implicit none
# include "param.h"
# include "sta.h"
# include "scalars.h"
      integer ifld, nfltmax, indx(nfltmax)
      real A(GLOBAL_2D_ARRAY,0:N)
      integer id,iflt, i1,i2, j1,j2, k1,k2
      real p1,p2, q1,q2, r1,r2

      do id=1,nfltmax
        iflt=indx(id)

        k1=int(stainfo(istazgrd,iflt))
        r2=stainfo(istazgrd,iflt) - float(k1)
        r1=1.-r2
        k1=max(k1,   0)
        k2=min(k1+1, N)

        i1=int(stainfo(istaxgrd,iflt))
        i2=i1+1
        p2=stainfo(istaxgrd,iflt)-float(i1)
        p1=1.-p2

        j1=int(stainfo(istaygrd,iflt))
        j2=j1+1
        q2=stainfo(istaygrd,iflt)-float(j1)
        q1=1.0-q2

        stadata(ifld,iflt)=p1*q1*(r1*A(i1,j1,k1)+r2*A(i1,j1,k2))
     &                    +p2*q1*(r1*A(i2,j1,k1)+r2*A(i2,j1,k2))
     &                    +p1*q2*(r1*A(i1,j2,k1)+r2*A(i1,j2,k2))
     &                    +p2*q2*(r1*A(i2,j2,k1)+r2*A(i2,j2,k2))
      enddo
      return
      end
!
!======================================================================
!
      subroutine interp_r2d_sta (A, ifld, nfltmax, indx)
      implicit none
# include "param.h"
# include "sta.h"
# include "scalars.h"
      integer ifld, nfltmax, indx(nfltmax)
      real A(GLOBAL_2D_ARRAY)
      integer id,iflt, i1,i2, j1,j2
      real p1,p2, q1,q2

      do id=1,nfltmax
        iflt=indx(id)

        i1=int(stainfo(istaxgrd,iflt))
        i2=i1+1
        p2=stainfo(istaxgrd,iflt)-float(i1)
        p1=1.-p2

        j1=int(stainfo(istaygrd,iflt))
        j2=j1+1
        q2=stainfo(istaygrd,iflt)-float(j1)
        q1=1.0-q2

        stadata(ifld,iflt)=q1*(p1*A(i1,j1) + p2*A(i2,j1))
     &                    +q2*(p1*A(i1,j2) + p2*A(i2,j2))
      enddo
      return
      end
!
!======================================================================
!
      subroutine fill_sta_ini
      implicit none
# include "param.h"
# include "sta.h"
# include "scalars.h"
# include "nc_sta.h"
      integer iflt
      do iflt=1,nstas
         stadata(istadpt,iflt)=stainfo(istazgrd,iflt)    ! Either depth or
         if (wrtsta(indxstaGrd)) then
           stadata(istazgrd,iflt)=stainfo(istazgrd,iflt) ! sigma level position
                                                         ! is unavailaible
           stadata(istaxgrd,iflt)=stainfo(istaxgrd,iflt) ! for initial storage 
           stadata(istaygrd,iflt)=stainfo(istaygrd,iflt)
         endif
      enddo
      return
      end
!
!======================================================================
!
      subroutine interp_r2d_sta_ini (A, ifld)
      implicit none
# include "param.h"
# include "sta.h"
# include "scalars.h"
      integer ifld
      real A(GLOBAL_2D_ARRAY)
      integer iflt, i1,i2, j1,j2
      real p1,p2, q1,q2

      do iflt=1,nstas

        i1=int(stainfo(istaxgrd,iflt))
        i2=i1+1
        p2=stainfo(istaxgrd,iflt)-float(i1)
        p1=1.-p2

        j1=int(stainfo(istaygrd,iflt))
        j2=j1+1
        q2=stainfo(istaygrd,iflt)-float(j1)
        q1=1.0-q2

        stadata(ifld,iflt)=q1*(p1*A(i1,j1) + p2*A(i2,j1))
     &                    +q2*(p1*A(i1,j2) + p2*A(i2,j2))
      enddo
      return
      end

#else
      subroutine interp_sta_empty
      end
#endif
