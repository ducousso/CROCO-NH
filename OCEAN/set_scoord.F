! $Id: set_scoord.F 1339 2013-10-01 15:02:13Z gcambon $
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
#ifdef SOLVE3D

      subroutine set_scoord     ! Input:   hmin,   Tcline,
!                               !          theta_s, theta_b 
! Define S-coordinate system.   !
!                               ! Output:  hc, sc_w(0:N), Cs_w(0:N)
      implicit none             !              sc_r(1:N), Cs_r(1:N)
# include "param.h"
# include "scalars.h"
      integer k
      real cff,cff1,cff2,cff3, ds, sc, csf
!
! Set S-Curves in domain [-1 < sc < 0] at vertical W- and RHO-points.
!
# ifndef NEW_S_COORD
      hc=min(hmin,Tcline)
      cff1=1./sinh(theta_s)
      cff2=0.5/tanh(0.5*theta_s)

      sc_w(0)=-1.0
      Cs_w(0)=-1.0

      cff=1./float(N)
      do k=1,N,+1
        sc_w(k)=cff*float(k-N)
        Cs_w(k)=(1.-theta_b)*cff1*sinh(theta_s*sc_w(k))
     &             +theta_b*(cff2*tanh(theta_s*(sc_w(k)+0.5))-0.5)

        sc_r(k)=cff*(float(k-N)-0.5)
        Cs_r(k)=(1.-theta_b)*cff1*sinh(theta_s*sc_r(k))
     &             +theta_b*(cff2*tanh(theta_s*(sc_r(k)+0.5))-0.5)
      enddo
# else
      hc= Tcline
      ds=1./float(N)      
      do k=1,N,+1
        sc     = ds*(float(k-N)-0.5)
        Cs_r(k)=CSF(sc, theta_s,theta_b)
        sc_r(k)=sc      
      enddo
      
      sc_w(0) = -1.0
      sc_w(N) =  0.        
      Cs_w(0) = -1.0
      Cs_w(N) =  0. 
      
      do k=1,N-1,+1
        sc     = ds*float(k-N)
        Cs_w(k)=CSF(sc, theta_s,theta_b)
        sc_w(k)=sc      
      enddo            
# endif
!
! Report information about vertical S-levels.
!
# ifdef WET_DRY
        if (hmin.lt.0.) hmin=D_wetdry
# endif
      MPI_master_only write(stdout,'(/1x,A/,/1x,A,10x,A/)')
     &                       'Vertical S-coordinate System:',
     &                       'level   S-coord     Cs-curve',
     &                       'at_hmin  over_slope     at_hmax' 
      do k=N,0,-1
# ifdef NEW_S_COORD
        sc=ds*(k-N)
        cff1 = hmin*(hc*sc + hmin*Cs_w(k))/(hc+hmin)
        cff2 = 0.5*hmax*(hc*sc + 0.5*hmax*Cs_w(k))/(hc+0.5*hmax)
        cff3 = hmax*(hc*sc + hmax*Cs_w(k))/(hc+hmax)
# else
        cff1=sc_w(k)*hc+(hmin-hc)*Cs_w(k)
        cff2=sc_w(k)*hc+(0.5*(hmin+hmax)-hc)*Cs_w(k)
        cff3=sc_w(k)*hc+(hmax-hc)*Cs_w(k)
# endif
        MPI_master_only write(stdout,'(I6,2F12.7,4x,3F12.3)')
     &                     k, sc_w(k),Cs_w(k), cff1,cff2,cff3
      enddo
      return
      end
!======================================================================
#ifdef NEW_S_COORD
                                            ! NOTE: Mathematical 
      function CSF (sc, theta_s,theta_b)    ! limits of CSF,csrf for
      implicit none                         ! theta_s, theta_b --> 0
      real CSF, sc, theta_s,theta_b,csrf    ! match that under "else"
                                            ! logical branches.
      if (theta_s.gt.0.D0) then
        csrf=(1.D0-cosh(theta_s*sc))/(cosh(theta_s)-1.D0)
      else
        csrf=-sc**2
      endif
      if (theta_b.gt.0.D0) then
        CSF=(exp(theta_b*csrf)-1.D0)/(1.D0-exp(-theta_b))  
      else
        CSF=csrf
      endif
      return
      end      
# endif /* NEW_S_COORD */    
!======================================================================          
#else
      subroutine set_scoord_empty
      return
      end
#endif /* SOLVE3D */
