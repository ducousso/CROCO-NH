! $Id: init_sta.F 1586 2014-07-30 14:57:11Z marchesiello $
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
       subroutine init_sta
#ifdef STATIONS
!
!====================================================================
!                                                                    !
!  This routine reads in and process station locations from          !
!  input station script.                                             !
!                                                                    !
!=====================================================================
!
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "nc_sta.h"
# include "sta.h"
# include "scalars.h"
# include "init_sta.h"
!
      integer Ncount, i, icard, iunit, j, k, lstr, mc, nc
      integer index(Msta)
      integer lenstr
      real xsta, ysta, zsta
      real Ip0(Msta), Jp0(Msta), lat(Msta), lon(Msta)
      character*35 frmt
      parameter (iunit=50)
!
# ifdef MPI
#  define LOCALLM Lmmpi
#  define LOCALMM Mmmpi
# else
#  define LOCALLM Lm
#  define LOCALMM Mm
# endif
!
      k=0
!
!---------------------------------------------------------------------
!  Read in stations locations.
!---------------------------------------------------------------------
!
      lstr=lenstr(staposname)
      open(iunit,file=staposname(1:lstr),form='formatted',
     &  status='old', err=195)
!!     write(stdout,10)
!! 10  format(/,' STATION processing parameters:',/)
!
!  Read input parameters according to their input card number.
!
      icard=0

      do while (icard.lt.99)
!
!  Read in stations identification title.
!
        if (icard.eq.1) then
          read(iunit,'(a)',err=70) statitle
          lstr=lenstr(statitle)
!!          MPI_master_only write(stdout,20) statitle(1:lstr)
!!  20      format(/,2x,a)
!
!  Read in station location.
!
        elseif (icard.eq.2) then
          i=0
          nstas=0
          do while (.true.)
            i=i+1
            read(iunit,*,err=40) stat0(i), stax0(i), stay0(i), staz0(i),
     &                           STgrd(i),stacoor(i)
            nstas=i
          enddo
  40      if (stat0(i).ne.99.0) then
            write(stdout,50) icard, i, staposname
  50        format(/,
     &            ' INIT_STATIONS - error while reading input card: ',
     &            i2, ', STATIONS location entry: ',i3,/,15x,
     &            'in input script: ',a)
            stop
          else
            Ncount=i-1
            goto 90
          endif
          if (Msta.lt.nstas) then
            write(stdout,60) Msta, nstas
  60        format(/,' INIT_STATIONS - too small dimension parameter,',
     &             ' Msta',2i6,/,15x,'change file  param.h  and',
     &             ' recompile.')
            stop
          endif
        endif
!
!  Read last input card ID.
!
        read(iunit,*,err=70) icard
      enddo
      goto 90
!
!  Error while reading input parameters.
!
  70  write(stdout,80) icard, staposname
  80  format(/,' INIT_STATIONS - error while reading input card: ',
     &       i2,15x,'in input script: ',a)

  90  close(iunit)

      MPI_master_only write(stdout,100) nstas
 100  format(2x,' INIT_STATIONS -- Number of stations (nstas) : ',i6,/)
!
!---------------------------------------------------------------------
!  Set station location.
!---------------------------------------------------------------------
!
!  Set time of station recording (seconds after model initialization) and
!  horizontal positions (grid units). Fill the initial vertical level or 
!  depth position.
!
      mc=0
      nc=0
      do i=1,Ncount
          nc=nc+1
          stainfo(istatstr,nc)=(tdays+stat0(i))*day2sec
          stainfo(istazgrd,nc)=staz0(i)
          stainfo(istagrd,i)=FLOAT(STgrd(i)) ! each station belongs 
          if (stacoor(i).eq.0) then          ! to this grid initially
# ifdef MPI
            stax0(i)=stax0(i)-ii*Lm
            stay0(i)=stay0(i)-jj*Mm
# endif
            if (stax0(i).ge.0 .and. stax0(i).lt.LOCALLM+1 .and.
     &          stay0(i).ge.0 .and. stay0(i).lt.LOCALMM+1) then
              stainfo(istaxgrd,nc)=MIN(MAX(0.5,stax0(i)),
     &                                     FLOAT(LOCALLM)+0.5)
              stainfo(istaygrd,nc)=MIN(MAX(0.5,stay0(i)),
     &                                     FLOAT(LOCALMM)+0.5)
            else
              nc=nc-1
            endif
          else
            mc=mc+1
            if (stax0(i).ge.lonu(1,1) .and. 
     &          stax0(i).lt.lonu(LOCALLM+1,1) .and.
     &          stay0(i).ge.latv(1,1) .and. 
     &          stay0(i).lt.latv(1,LOCALMM+1)) then
              lon(mc)=stax0(i)
              lat(mc)=stay0(i)
              index(mc)=nc
            else
              mc=mc-1
              nc=nc-1
            endif
          endif
      enddo
!
!  Set number of stations to compute.
!
      nstas0=nstas
      nstas=nc
!
!  If applicable, convert station positions (lon,lat) to grid
!  units.
!
      if (mc.gt.0) then
# ifdef SPHERICAL
          call hindices (Ip0,Jp0,lon,lat,mc,lonr,latr)
# else
          call hindices (Ip0,Jp0,lon,lat,mc,xr,yr)
# endif
        do i=1,mc
          nc=index(i)
          stainfo(istaxgrd,nc)=MIN(MAX(0.5,Ip0(i)),FLOAT(LOCALLM)+0.5)
          stainfo(istaygrd,nc)=MIN(MAX(0.5,Jp0(i)),FLOAT(LOCALMM)+0.5)
        enddo
      endif
!
      return

 195  write(stdout,205) staposname
 205  format(/,'station file ',A,/,
     &   ' not found => station initialization using restart file')

!         call get_initial_sta
!         ldefsta= .false.

! Test initialization
!      do nc=1,nfloats
!      write(stdout,53) nc, Tinfo(ixgrd,nc), Tinfo(iygrd,nc),
!     &    Tinfo(izgrd,nc), NINT(Tinfo(igrd,nc)), Tinfo(itstr,nc)
!      enddo
! Fin test

#endif /* STATIONS */
      return
      end

#if defined STATIONS && !defined FLOATS
      subroutine hindices (Ipos,Jpos,Xpos,Ypos,Npos,Xgrd,Ygrd)
!
!================================================ Hernan G. Arango ===
!  Copyright (c) 2000 Rutgers/UCLA                                   !
!======================================== Alexander F. Shchepetkin ===
!                                                                    !
!  Given position vectors Xpos and Ypos of size Npos,  this routine  !
!  finds the corresponding indices Ipos and Jpos of the  model grid  !
!  (Xgrd,Ygrd) cell containing each requested position.              !
!                                                                    !
!  Calls:    Try_Range                                               !
!                                                                    !
!=====================================================================
!
# ifdef AGRIF
        USE Agrif_Util
# endif
      implicit none
# include "param.h"
# include "scalars.h"
!
      logical found
      logical Try_Range
      integer Imax, Imin, Jmax, Jmin, Npos, i0, j0, k
      real Ipos(Npos), Jpos(Npos), Xpos(Npos), Ypos(Npos), 
     &  deltax, deltay, dx1, dy1, dx2, dy2, c1, c2
      real Xgrd(GLOBAL_2D_ARRAY),
     &     Ygrd(GLOBAL_2D_ARRAY)
!
!---------------------------------------------------------------------
!  Determine grid cell indices containing requested position points.
!  Then, interpolate to fractional cell position.
!---------------------------------------------------------------------
!
!  Initialize all indices.
!
      do k=1,Npos
        Ipos(k)=0.0
        Jpos(k)=0.0
      enddo
!
!  Check each position to find if it falls inside the whole domain.
!  Once it is stablished that it inside, find the exact cell to which
!  it belongs by successively dividing the domain by a half (binary
!  search).
!
      do k=1,Npos

        Imin=0
        Imax=LOCALLM+1
        Jmin=0
        Jmax=LOCALMM+1

        found=Try_Range(Imin,Imax,Jmin,Jmax,Xpos(k),Ypos(k),Xgrd,Ygrd)

        if (found) then
          do while (((Imax-Imin).gt.1).or.((Jmax-Jmin).gt.1))
            if ((Imax-Imin).gt.1) then
              i0=(Imin+Imax)/2
              found=Try_Range(Imin,i0,Jmin,Jmax,Xpos(k),Ypos(k),
     &                        Xgrd,Ygrd)
              if (found) then
                Imax=i0
              else
                Imin=i0
              endif
            endif
            if ((Jmax-Jmin).gt.1) then
              j0=(Jmin+Jmax)/2
              found=Try_Range(Imin,Imax,Jmin,j0,Xpos(k),Ypos(k),
     &                        Xgrd,Ygrd)
              if (found) then
                Jmax=j0
              else
                Jmin=j0
              endif
            endif
          enddo

! Improved interpolation block (02/06/03). Not totally 
! accurate in case where the grid is strongly deformed. 
! The nonalignment of xsi (eta) axis with longitude 
! (latitude) direction is taken into account. 

          dy1=Ygrd(Imin,Jmin+1)-Ygrd(Imin,Jmin)
          dx1=Xgrd(Imin,Jmin+1)-Xgrd(Imin,Jmin)
          dy2=Ygrd(Imin+1,Jmin)-Ygrd(Imin,Jmin)
          dx2=Xgrd(Imin+1,Jmin)-Xgrd(Imin,Jmin)

          c1=Xpos(k)        *dy1-Ypos(k)        *dx1
          c2=Xgrd(Imin,Jmin)*dy2-Ygrd(Imin,Jmin)*dx2
          deltax=(c1*dx2-c2*dx1)/(dx2*dy1-dy2*dx1)
          deltax=(deltax-Xgrd(Imin,Jmin))/dx2
          Ipos(k)=FLOAT(Imin)+MIN(MAX(0.0,deltax),1.0)

          c1=Xgrd(Imin,Jmin)*dy1-Ygrd(Imin,Jmin)*dx1
          c2=Xpos(k)        *dy2-Ypos(k)        *dx2
          deltay=(c1*dy2-c2*dy1)/(dx2*dy1-dy2*dx1) 
          deltay=(deltay-Ygrd(Imin,Jmin))/dy1
          Jpos(k)=FLOAT(Jmin)+MIN(MAX(0.0,deltay),1.0)

        endif
      enddo
      return
      end

      function Try_Range (Imin,Imax,Jmin,Jmax,Xo,Yo,Xgrd,Ygrd)
!
!================================================ Hernan G. Arango ===
!  Copyright (c) 2000 Rutgers/UCLA                                   !
!======================================== Alexander F. Shchepetkin ===
!                                                                    !
!  Given a grided domain with matrix coordinates Xgrd and Ygrd, this !
!  function finds if the point (Xo,Yo)  is inside the box defined by !
!  the requested corners (Imin,Jmin) and (Imax,Jmax). It will return !
!  logical switch  Try_Range=.true.  if (Xo,Yo) is inside, otherwise !
!  it will return false.                                             !
!                                                                    !
!  Calls:   inside                                                   !
!                                                                    !
!=====================================================================
!
      implicit none
#include "param.h"
!
      logical Try_Range, inside
      integer Imax, Imin, Jmax, Jmin, Nb, NX, i, j, shft
      parameter(NX=2*(LLm0+2)+2*(MMm0+2)+1)
      real Xb(NX), Yb(NX), Xo, Yo
      real Xgrd(GLOBAL_2D_ARRAY),
     &     Ygrd(GLOBAL_2D_ARRAY)
!
!---------------------------------------------------------------------
!  Define closed polygon.
!---------------------------------------------------------------------
!
!  Note that the last point (Xb(Nb),Yb(Nb)) does not repeat first
!  point (Xb(1),Yb(1)).  Instead, in function inside, it is implied
!  that the closing segment is (Xb(Nb),Yb(Nb))-->(Xb(1),Yb(1)). In
!  fact, function inside sets Xb(Nb+1)=Xb(1) and Yb(Nb+1)=Yb(1).
!
      Nb=2*(Jmax-Jmin+Imax-Imin)
      shft=1-Imin
      do i=Imin,Imax-1
        Xb(i+shft)=Xgrd(i,Jmin)
        Yb(i+shft)=Ygrd(i,Jmin)
      enddo
      shft=1-Jmin+Imax-Imin
      do j=Jmin,Jmax-1
        Xb(j+shft)=Xgrd(Imax,j)
        Yb(j+shft)=Ygrd(Imax,j)
      enddo
      shft=1+Jmax-Jmin+2*Imax-Imin
      do i=Imax,Imin+1,-1
        Xb(shft-i)=Xgrd(i,Jmax)
        Yb(shft-i)=Ygrd(i,Jmax)
      enddo
      shft=1+2*Jmax-Jmin+2*(Imax-Imin)
      do j=Jmax,Jmin+1,-1
        Xb(shft-j)=Xgrd(Imin,j)
        Yb(shft-j)=Ygrd(Imin,j)
      enddo
!
!---------------------------------------------------------------------
!  Check if point (Xo,Yo) is inside of the defined polygon.
!---------------------------------------------------------------------
!
      Try_Range=inside(Xo,Yo,Xb,Yb,Nb)
      return
      end
      function inside (Xo,Yo,Xb,Yb,Nb)
!
!================================================ Hernan G. Arango ===
!  Copyright (c) 2000 Rutgers/UCLA                                   !
!======================================== Alexander F. Shchepetkin ===
!                                                                    !
!  Given the vectors Xb and Yb of size Nb, defining the coordinates  !
!  of a closed polygon,  this function find if the point (Xo,Yo) is  !
!  inside the polygon.  If the point  (Xo,Yo)  falls exactly on the  !
!  boundary of the polygon, it still considered inside.              !
!                                                                    !
!  This algorithm does not rely on the setting of  Xb(Nb)=Xb(1) and  !
!  Yb(Nb)=Yb(1).  Instead, it assumes that the last closing segment  !
!  is (Xb(Nb),Yb(Nb)) --> (Xb(1),Yb(1)).                             !
!                                                                    !
!  Reference:                                                        !
!                                                                    !
!    Reid, C., 1969: A long way from Euclid. Oceanography EMR,       !
!      page 174.                                                     !
!                                                                    !
!  Algorithm:                                                        !
!                                                                    !
!  The decision whether the point is  inside or outside the polygon  !
!  is done by counting the number of crossings from the ray (Xo,Yo)  !
!  to (Xo,-infinity), hereafter called meridian, by the boundary of  !
!  the polygon.  In this counting procedure,  a crossing is counted  !
!  as +2 if the crossing happens from "left to right" or -2 if from  !
!  "right to left". If the counting adds up to zero, then the point  !
!  is outside.  Otherwise,  it is either inside or on the boundary.  !
!                                                                    !
!  This routine is a modified version of the Reid (1969) algorithm,  !
!  where all crossings were counted as positive and the decision is  !
!  made  based on  whether the  number of crossings is even or odd.  !
!  This new algorithm may produce different results  in cases where  !
!  Xo accidentally coinsides with one of the (Xb(k),k=1:Nb) points.  !
!  In this case, the crossing is counted here as +1 or -1 depending  !
!  of the sign of (Xb(k+1)-Xb(k)).  Crossings  are  not  counted if  !
!  Xo=Xb(k)=Xb(k+1).  Therefore, if Xo=Xb(k0) and Yo>Yb(k0), and if  !
!  Xb(k0-1) < Xb(k0) < Xb(k0+1),  the crossing is counted twice but  !
!  with weight +1 (for segments with k=k0-1 and k=k0). Similarly if  !
!  Xb(k0-1) > Xb(k0) > Xb(k0+1), the crossing is counted twice with  !
!  weight -1 each time.  If,  on the other hand,  the meridian only  !
!  touches the boundary, that is, for example, Xb(k0-1) < Xb(k0)=Xo  !
!  and Xb(k0+1) < Xb(k0)=Xo, then the crossing is counted as +1 for  !
!  segment k=k0-1 and -1 for segment k=k0, resulting in no crossing. !
!                                                                    !
!  Note 1: (Explanation of the logical condition)                    !
!                                                                    !
!  Suppose  that there exist two points  (x1,y1)=(Xb(k),Yb(k))  and  !
!  (x2,y2)=(Xb(k+1),Yb(k+1)),  such that,  either (x1 < Xo < x2) or  !
!  (x1 > Xo > x2).  Therefore, meridian x=Xo intersects the segment  !
!  (x1,y1) -> (x2,x2) and the ordinate of the point of intersection  !
!  is:                                                               !
!                                                                    !
!                 y1*(x2-Xo) + y2*(Xo-x1)                            !
!             y = -----------------------                            !
!                          x2-x1                                     !
!                                                                    !
!  The mathematical statement that point  (Xo,Yo)  either coinsides  !
!  with the point of intersection or lies to the north (Yo>=y) from  !
!  it is, therefore, equivalent to the statement:                    !
!                                                                    !
!         Yo*(x2-x1) >= y1*(x2-Xo) + y2*(Xo-x1),   if   x2-x1 > 0    !
!  or                                                                !
!         Yo*(x2-x1) <= y1*(x2-Xo) + y2*(Xo-x1),   if   x2-x1 < 0    !
!                                                                    !
!  which, after noting that  Yo*(x2-x1) = Yo*(x2-Xo + Xo-x1) may be  !
!  rewritten as:                                                     !
!                                                                    !
!        (Yo-y1)*(x2-Xo) + (Yo-y2)*(Xo-x1) >= 0,   if   x2-x1 > 0    !
!  or                                                                !
!        (Yo-y1)*(x2-Xo) + (Yo-y2)*(Xo-x1) <= 0,   if   x2-x1 < 0    !
!                                                                    !
!  and both versions can be merged into  essentially  the condition  !
!  that (Yo-y1)*(x2-Xo)+(Yo-y2)*(Xo-x1) has the same sign as x2-x1.  !
!  That is, the product of these two must be positive or zero.       !
!                                                                    !
!=====================================================================
!
      implicit none
!
      logical inside
      integer Nb, Nstep, crossings, i, inc, k, kk, nc
      parameter (Nstep=128)
      integer index(Nstep)
      real Xb(Nb+1), Yb(Nb+1), Xo, Yo, dx1, dx2, dxy
!
!---------------------------------------------------------------------
!  Find intersections.
!---------------------------------------------------------------------
!
!  Set crossings counter and close the contour of the polygon.
!
      crossings=0
      Xb(Nb+1)=Xb(1)
      Yb(Nb+1)=Yb(1)
!
!  The search is optimized.  First select the indices of segments
!  where Xb(k) is different from Xb(k+1) and Xo falls between them.
!  Then, further investigate these segments in a separate loop.
!  Doing it in two stages takes less time because the first loop is
!  pipelined.
!
      do kk=0,Nb-1,Nstep
        nc=0
        do k=kk+1,MIN(kk+Nstep,Nb)
          if (((Xb(k+1)-Xo)*(Xo-Xb(k)).ge.0.0).and.
     &        (Xb(k).ne.Xb(k+1))) then
            nc=nc+1
            index(nc)=k
          endif
        enddo
        do i=1,nc
          k=index(i)
          if (Xb(k).ne.Xb(k+1)) then
            dx1=Xo-Xb(k)
            dx2=Xb(k+1)-Xo
            dxy=dx2*(Yo-Yb(k))-dx1*(Yb(k+1)-Yo)
            inc=0
            if ((Xb(k).eq.Xo).and.(Yb(k).eq.Yo)) then
              crossings=1
              goto 10
            elseif (((dx1.eq.0.0).and.(Yo.ge.Yb(k  ))).or.
     &              ((dx2.eq.0.0).and.(Yo.ge.Yb(k+1)))) then
              inc=1
            elseif ((dx1*dx2.gt.0.0).and.             ! See Note 1
     &              ((Xb(k+1)-Xb(k))*dxy.ge.0.0)) then
              inc=2
            endif
            if (Xb(k+1).gt.Xb(k)) then
              crossings=crossings+inc
            else
              crossings=crossings-inc
            endif
          endif
        enddo
      enddo
!
!  Determine if point (Xo,Yo) is inside of closed polygon.
!
  10  if (crossings.eq.0) then
        inside=.false.
      else
        inside=.true.
      endif
      return
      end
#endif /* STATIONS */
