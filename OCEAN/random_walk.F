! $Id: random_walk.F 1458 2014-02-03 15:01:25Z gcambon $
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
#ifdef RANDOM_WALK

      subroutine random_walk (indx,nfltmax)
      implicit none
#  include "param.h"
#  include "grid.h"
#  include "floats.h"
#  include "scalars.h"
#  include "ocean3d.h"
# ifdef RANDOM_VERTICAL
#  include "mixing.h"
# endif

      integer i,k,ierr,iseed,ifix,jfix,zfix
      integer nfltmax,indx(nfltmax), iflt
      real randz(nfltmax+1), deltaw
# ifdef DIEL_MIGRATION
      real tofday, wflag, wmig
# endif
# ifdef RANDOM_HORIZONTAL
      real kh, epsl, lxy, deltau
# endif
      integer i1,i2, j1,j2, k1,k2
      real p1,p2, q1,q2, r1,r2,znow
     & ,d1, d2, temp, sum,temp2,maxdepth

!  in the case of some larvae (e.g. crab) the 
!  vertical migration is limited by some maximum
!  in depth

      maxdepth=-60.

# ifdef IBM
      if(ibmdata(ibmzoe,iflt).eq.1.) then
        maxdepth =-18.
      elseif(ibmdata(ibmzoe,iflt).eq.2.) then
        maxdepth =-18.
      elseif(ibmdata(ibmzoe,iflt).eq.3.) then
        maxdepth =-21.
      elseif(ibmdata(ibmzoe,iflt).eq.4.) then
        maxdepth =-27.
      elseif(ibmdata(ibmzoe,iflt).eq.5.) then
        maxdepth =-30.
      endif
# endif

!
!  We calculate a velocity fluctuation based on a 
!  a random number (uniformly distribute between  [0 1])
!  scaled by the vertical tracer mixing Ak and its vertical gradient
!
!  deltaw=sqrt(2*Akt/dt)*random_n+(dAkt/dz)
!

!
!  A similar estimate is done for the horiz. diffusion
!  using Kh = epsl(1/3) L(4/3) we take epsl = 10-7
!  L will be the larges unresolved scale. Here
!  we use (grid cell)/2.0
!
# ifdef RANDOM_HORIZONTAL
      epsl=.0000001
# endif

# ifdef DIEL_MIGRATION

!  A wmig is added in case of forced (time dependent)
!  vertical migration. Particles sink from 10 - 12 pm. 
!  and rise from 4 -6 am. The rising process is not so
!  dramatic as the the sinking one. Some of the particles
!  do not surface completely. To simulate that in the surfacing 
!  phase we add a random fluctuation.

      tofday=tdays-int(tdays) 
      if(tofday.gt.0.9167 .and. tofday.lt.1) then
        wflag =  1.            
      elseif(tofday.gt.0.1667 .and. tofday.lt.0.25) then
        wflag =  -1.
      else
        wflag = 0.
      endif
# endif

# if defined RANDOM_VERTICAL || defined RANDOM_HORIZONTAL
! calculate random number varing a "seed" with tdays
! wich will be stored in vector of with the same size
! of the number of floats. NOTE: the firts element of the vector
! is fixed and is not used 

      do i=1,nfltmax+1
       randz(i)=0.
      enddo

      iseed = int(tdays*10+
     &  track(ixgrd,nfp1,1)*10+
     &  track(iygrd,nfp1,1)*10+
     &  track(izgrd,nfp1,1)*10)
      call urng (iseed, randz, nfltmax+1, ierr)
# endif

      do i=1,nfltmax
        iflt=indx(i)
        ifix=int(track(ixgrd,nfp1,iflt))
        d1=track(ixgrd,nfp1,iflt)-ifix
        jfix=int(track(iygrd,nfp1,iflt))
        d2=track(iygrd,nfp1,iflt)-jfix
        zfix=int(track(izgrd,nfp1,iflt))

        deltaw=0.
# ifdef RANDOM_VERTICAL
        deltaw=sqrt(2.*max(Akt(ifix,jfix,zfix,1),0.)/dt)*
     &         (randz(i+1)*2.-1.) +dAktdz(ifix,jfix,zfix)

! In future interpolate Akt dAktdz to float position.
c       write(*,*)iflt,randz(i+1),dAktdz(ifix,jfix,zfix),
c    &          Akt(ifix,jfix,zfix,1),deltaw

# endif

# ifdef DIEL_MIGRATION
        if(wflag.ne.0.) then
! scale vertical velocity by depth
          wmig = wflag*30./(60.*60.)    ! 30 m./ hour
          if(wflag.lt.0.) then
            deltaw=wmig
          else
!           deltaw=wmig
            deltaw=deltaw+0.7*wmig+0.3*wmig*(randz(i+1)*2.-1.)
!           deltaw=deltaw+wmig+0.5*wmig*(randz(i+1)*2.-1.)
          endif
        endif
# endif

!
! Calculate new vertical float position. First gets former
! position and adds the random displacement
!
! get actual depth.

        znow=0.

c       write(*,*) ' RANDOM WALK '
c       write(*,*) 'track(izgrd)', track(izgrd,nfp1,iflt)
c       write(*,*) 'Random displacement ', deltaw*dt

        k1=int(track(izgrd,nfp1,iflt))
        r2=track(izgrd,nfp1,iflt) - float(k1)
        r1=1.-r2
        k1=max(k1,   0)
        k2=min(k1+1, N)
  
        i1=int(track(ixgrd,nfp1,iflt))
        i2=i1+1
        p2=track(ixgrd,nfp1,iflt)-float(i1)
        p1=1.-p2
  
        j1=int(track(iygrd,nfp1,iflt))
        j2=j1+1
        q2=track(iygrd,nfp1,iflt)-float(j1)
        q1=1.0-q2
  
        znow=p1*q1*(r1*z_w(i1,j1,k1)+r2*z_w(i1,j1,k2))
     &              +p2*q1*(r1*z_w(i2,j1,k1)+r2*z_w(i2,j1,k2))
     &              +p1*q2*(r1*z_w(i1,j2,k1)+r2*z_w(i1,j2,k2))
     &              +p2*q2*(r1*z_w(i2,j2,k1)+r2*z_w(i2,j2,k2))

c       write(*,*) 'ZNOW ', znow
        znow=znow+deltaw*dt
c       write(*,*) 'ZNOW upgraded', znow

! put some limits to znow to prevent having values above sea level
! and under bot.

        znow=max(z_w(ifix,jfix,1),znow)
!       znow=min(z_w(ifix,jfix,N)-z_w(ifix,jfix,N)*0.02,znow)
        znow=min(z_w(ifix,jfix,N-1),znow)
# ifdef DIEL_MIGRATION
        znow=max(znow,maxdepth)
# endif
  
! use Capet code to get new sigma level for the float

        track(izgrd,nfp1,iflt)=0.
        sum=0.
        do k1=0,1
          ifix=ifix+k1
          do k2=0,1
            jfix=jfix+k2
            temp=((1-k1)*(1-d1)+k1*d1)*
     &           ((1-k2)*(1-d2)+k2*d2)
            sum=sum+temp
            do k=N,1,-1
             if ((z_w(ifix,jfix,k)-znow)*
     &              (znow-z_w(ifix,jfix,k-1)).ge.0.0) then
                temp2=(float(k-1)+
     &              (znow-z_w(ifix,jfix,k-1))/Hz(ifix,jfix,k))
                track(izgrd,nfp1,iflt)=track(izgrd,nfp1,iflt)+temp2*
     &                            temp
  
             endif
            enddo !  -> k
          enddo !  -> k2
        enddo !  -> k1
        if (sum .ne. 0) then
          track(izgrd,nfp1,iflt)=track(izgrd,nfp1,iflt)/sum
        endif
        track(izgrd,nfp1,iflt)=max(0.,min(float(N),
     &                     track(izgrd,nfp1,iflt)))
c       write(*,*) 'new track ', track(izgrd,nfp1,iflt)

# ifdef RANDOM_HORIZONTAL
! calculate the horizontal random walk and scale by grid cell size

        lxy =((1./pn(ifix,jfix))+(1./pm(ifix,jfix)))/2.
        kh  = epsl**(1./3.)*lxy**(4./3.)

        deltau=sqrt(2.*kh/dt)*(randz(i+1)*2.-1.) 
     &        *tanh(dt*sec2day*float(iic-ntstart))
        track(ixgrd,nfp1,iflt)=track(ixgrd,nfp1,iflt)+
     &       deltau*umask(ifix,jfix)*dt/lxy

! to have different rand numb. in the directions 
! we use the inverse of the random number vector in 
! y direction

        deltau=sqrt(2.*kh/dt)*(randz(nfltmax+1-i+1)*2.-1.) 
     &        *tanh(dt*sec2day*float(iic-ntstart))
        track(iygrd,nfp1,iflt)=track(iygrd,nfp1,iflt)+ 
     &       deltau*vmask(ifix,jfix)*dt/lxy
!
! These random horizontal excursions are not isopycnal
! In a way they also generate some vertical random variability
!
# endif

      enddo !  -> nfltmax

      end !  -> random_walk

      subroutine urng (ix, x, n, ierr)
!
!=======================================================================
!                                                                      !
!  Uniform random-number generator from the NSWC Library               !
!                                                                      !
!  Uses the recursion ix = ix*a mod p, where 0 < ix < p                !
!                                                                      !
!  Written by Linus Schrage, University of Chicago. Adapted for NSWC   !
!  Library by A. H. Morris. Modernised & included in ROMS by Mark      !
!  Hadfield, NIWA.                                                     !
!                                                                      !
!=======================================================================
!
!  Imported variable declarations.
!
      integer, intent(in) :: n

      integer, intent(inout) :: ix

      integer, intent(out) :: ierr

!#ifdef ASSUMED_SHAPE
!      real(8), intent(out) :: x(:)
!#else
      real(8), intent(out) :: x(n)
!#endif
!
!  Local variable declarations.
!    
      integer, parameter :: a = 16807          ! 7^5
      integer, parameter :: b15 = 32768        ! 2^15
      integer, parameter :: b16 = 65536        ! 2^16
      integer, parameter :: p = 2147483647     ! 2^31-1

      integer :: fhi, k, l, leftlo, xalo, xhi

      real(8), parameter :: s = 0.465661E-09
!
!-----------------------------------------------------------------------
!  Generate random numbers.
!-----------------------------------------------------------------------
!

      if (n.le.0) then
        ierr=1
        return
      endif
      if ((ix.le.0).or.(ix.ge.p)) then
         ierr=2
         return
      endif
!
      ierr=0
!
      do l=1,n
!
! Get 15 high order bits of "ix".
!
        xhi=ix/b16
!
! Get 16 lower bits of ix and multiply with "a".
!
        xalo=(ix-xhi*b16)*a
!
! Get 15 high order bits of the product.
!
        leftlo=xalo/b16
!
! Form the 31 highest bits of "a*ix".
!
        fhi=xhi*a+leftlo
!
! Obtain the overflow past the 31st bit of "a*ix".
!
        k=fhi/b15
!
! Assemble all the parts and presubtract "p". The parentheses are
! essential.
!
        ix=(((xalo-leftlo*b16)-p)+(fhi-k*b15)*b16)+k
!
! Add "p" if necessary.
!
        if (ix.lt.0) ix=ix+p
!
! Rescale "ix", to interpret it as a value between 0 and 1.
! the scale factor "s" is selected to be as near "1/p" as is
! appropriate in order that the floating value for "ix = 1",
! namely "s", be roughly the same distance from 0 as "(p-1)*s"
! is from 1. The current value for "s" assures us that "x(l)"
! is less than 1 for any floating point arithmetic of 6
! or more digits.
!
         x(l)=real(ix,8)*s
      enddo
      return
      end 

#else
      subroutine random_walk_empty
      end
#endif    /* RANDOM_WALK */
