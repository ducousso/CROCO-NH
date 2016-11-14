! $Id: step_floats.F 1458 2014-02-03 15:01:25Z gcambon $
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
      subroutine step_floats (Lstr,Lend)
!
!================================================== John M. Klinck ===
!  Copyright (c) 2001 Rutgers/UCLA                                   !
!================================================ Hernan G. Arango ===
!                                                                    !
!  This routine time-steps  simulated  floats  trajectories using a  !
!  fifth-order scheme based on Adams-Bashforth (4) predictor         !
!  Adams-Moulton (4) corrector.                                      !
!                                                                    !
!=====================================================================
!
# ifdef AGRIF
      USE Agrif_Util
# endif
      implicit none
#include "param.h"
#include "floats.h"
#include "grid.h"
#include "ncscrum_floats.h"
#include "ocean2d.h"
#include "ocean3d.h"
#include "scalars.h"
c--#define CDEBUG
      integer Lstr, Lend, mon_thread, tmpnfp1, tmpnf, 
     & tmpnfm1, tmpnfm2, entier
      integer i, itrc, iflt, level, rcoeft,rcoefx,rcoefy, 
     & k, xfloat, yfloat, xfloat0, yfloat0, index1, index2, i1, j1
      real cff1, cff2, cff3, cff4, xrhs, yrhs, zrhs
     & ,invrcoeft, tmptrack , zfloat, d1, d2, temp, summ
     & ,temp2,tmp,tmp2
      integer nfltmax, indx(Lend-Lstr+1), nfltmax_bak,id
# ifdef AGRIF
      logical test
# endif      
# ifdef RANDOM_WALK
c      real tmp(Lend-Lstr+1)
# endif
      itrc=0
      zrhs=0.0

# ifdef AGRIF
      level=Agrif_Fixed()
      floattindex(level)=nfp1 
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


!  CHECK ALL FLOATS BELONGING TO THIS GRID AND TRY
!  TO PUT THEM (BEFORE RELEASE -  Condition over Tinfo) 
!  ON THE FINER GRID IF IT EXISTS 

# ifdef AGRIF 
c      write(*,*) 'Agrif_nb_step = ',Agrif_nb_step()
c      write(*,*) 'level = ', Agrif_Fixed()
c      write(*,*) 'Agrif_Nb_Fine_Grids ',Agrif_Nb_Fine_Grids
      if (Agrif_nb_step() .eq. 0) then 

c        if (.not. Agrif_Root()) then  !initialization of rhot2root
c           rhot(level,2)=rhot2root(Agrif_Parent_Fixed(),2)*rcoeft
c           rhot(level,1)=rcoeft
c        endif

         do iflt=Lstr,Lend

            if (NINT(Tinfo(igrd,iflt)) .eq. level) then 

c          write(*,*)'OLDTINFO',iflt,'  ',Tinfo(igrd,iflt),
c    &     Tinfo(ixgrd,iflt), Tinfo(iygrd,iflt)

              entier=nint(Tinfo(igrd,iflt))
              call Agrif_transfer_floatsp2c (entier,Tinfo(ixgrd,iflt),
     &                         Tinfo(iygrd,iflt),deltap2c)
              Tinfo(igrd,iflt)=float(entier)
            endif

c          write(*,*)'NEWTINFO',iflt,'  ',Tinfo(igrd,iflt),
c    &     Tinfo(ixgrd,iflt), Tinfo(iygrd,iflt)

         enddo
      endif  ! first time step
# endif /* AGRIF */


!  CHECK ALL FLOATS BELONGING TO THIS GRID AND TRY
!  TO PUT THEM (AFTER RELEASE - Condition over fltgrd 
!  and track) ON THE FINER GRID IF IT EXISTS.

# ifdef AGRIF      

          do iflt=Lstr,Lend
            if (fltgrd(iflt).eq.level) then 

            call Agrif_transfer_floatsp2c(fltgrd(iflt),
     &  track(ixgrd,nf,iflt),track(iygrd,nf,iflt),deltap2c)

!        if float transferred -> adapt velocities and grid value. 
!        Take care AMR_coeffreft >= 3 is assumed here
!        for temporal interpolation
     
            if (fltgrd(iflt) .ne. level) then 

! track values are made consistent with the time indexing in 
! the new grid (for one position index and all the velocity index).
! This takes into account the fact that child indexes will be advanced 
! in time in the new grid 
              tmpnfp1=floattindex(fltgrd(iflt))
              tmpnf=mod(tmpnfp1+3,NFT+1)
              tmpnfm1=mod(tmpnfp1+2,NFT+1)
              tmpnfm2=mod(tmpnfp1+1,NFT+1)

              track(ixgrd,tmpnfp1,iflt)=track(ixgrd,nf,iflt)
              track(iygrd,tmpnfp1,iflt)=track(iygrd,nf,iflt)
              track(izgrd,tmpnfp1,iflt)=track(izgrd,nf,iflt)

              tmptrack=track(ixrhs,nfm1,iflt)
              track(ixrhs,tmpnfp1,iflt)=track(ixrhs,nf,iflt)
              track(ixrhs,tmpnfm2,iflt)=(3*tmptrack+(rcoeft-3)*
     &              track(ixrhs,tmpnfp1,iflt))*invrcoeft*rcoefx
              track(ixrhs,tmpnfm1,iflt)=(2*tmptrack+(rcoeft-2)*
     &              track(ixrhs,tmpnfp1,iflt))*invrcoeft*rcoefx
              track(ixrhs,tmpnf,iflt)=(tmptrack+(rcoeft-1)*
     &                               track(ixrhs,tmpnfp1,iflt))

              tmptrack=track(iyrhs,nfm1,iflt)
              track(iyrhs,tmpnfp1,iflt)=track(iyrhs,nf,iflt)
              track(iyrhs,tmpnfm2,iflt)=(3*tmptrack+(rcoeft-3)*
     &  track(iyrhs,tmpnfp1,iflt))*invrcoeft*rcoefy
              track(iyrhs,tmpnfm1,iflt)=(2*tmptrack+(rcoeft-2)*
     &  track(iyrhs,tmpnfp1,iflt))*invrcoeft*rcoefy
              track(iyrhs,tmpnf,iflt)=(tmptrack+(rcoeft-1)*
     &  track(iyrhs,tmpnfp1,iflt))

              tmptrack=track(izrhs,nfm1,iflt)
              track(izrhs,tmpnfp1,iflt)=track(izrhs,nf,iflt)
              track(izrhs,tmpnfm2,iflt)=(3*tmptrack+(rcoeft-3)*
     &  track(izrhs,tmpnfp1,iflt))*invrcoeft
              track(izrhs,tmpnfm1,iflt)=(2*tmptrack+(rcoeft-2)*
     &  track(izrhs,tmpnfp1,iflt))*invrcoeft
              track(izrhs,tmpnf,iflt)=(tmptrack+(rcoeft-1)*
     &  track(izrhs,tmpnfp1,iflt))

            endif
          endif 
        enddo /* iflt*/
# endif /* AGRIF */


!
! Save indices of floats to be processes into a special array.
!
      nfltmax=0
      do iflt=Lstr,Lend
        if (fltgrd(iflt).eq.level) then
          nfltmax=nfltmax+1
          indx(nfltmax)=iflt
        endif
      enddo
      
!
!---------------------------------------------------------------------
!  Predictor step: compute first guess floats locations using a
!                4th-order Adams-Bashforth time-stepping scheme.
!---------------------------------------------------------------------
!
      do id=1,nfltmax
!        write(*,*) 'pos (XA) ', track(ixgrd,nf,iflt), '   ',
!     &     track(iygrd,nf,iflt), '  ', track(izgrd,nf,iflt)
        iflt=indx(id)
        track(ixgrd,nfp1,iflt)=track(ixgrd,nf,iflt)+
     &                       dt/24.*(55.*track(ixrhs,nf  ,iflt)-
     &                               59.*track(ixrhs,nfm1,iflt)+
     &                               37.*track(ixrhs,nfm2,iflt)-
     &                                9.*track(ixrhs,nfm3,iflt))
        track(iygrd,nfp1,iflt)=track(iygrd,nf,iflt)+
     &                        dt/24.*(55.*track(iyrhs,nf  ,iflt)-
     &                                59.*track(iyrhs,nfm1,iflt)+
     &                                37.*track(iyrhs,nfm2,iflt)-
     &                                 9.*track(iyrhs,nfm3,iflt))
# ifdef SOLVE3D
        track(izgrd,nfp1,iflt)=track(izgrd,nf,iflt)+
     &                        dt/24.*(55.*track(izrhs,nf  ,iflt)-
     &                                59.*track(izrhs,nfm1,iflt)+
     &                                37.*track(izrhs,nfm2,iflt)-
     &                                 9.*track(izrhs,nfm3,iflt))
        track(izgrd,nfp1,iflt)=max(0.,min(float(N),
     &                     track(izgrd,nfp1,iflt)))
# endif
      enddo

!
!---------------------------------------------------------------------
!  Calculate slopes at new time-step.
!---------------------------------------------------------------------
!
# ifdef SOLVE3D
      call rhs_floats (u(START_2D_ARRAY,1,nnew), 
     &                 v(START_2D_ARRAY,1,nnew),
     &                We(START_2D_ARRAY,0),
     &                           nfltmax, indx)

# else
      call rhs_floats (ubar(START_2D_ARRAY,knew),
     &                 vbar(START_2D_ARRAY,knew),
     &                            nfltmax, indx)
# endif 

!
!---------------------------------------------------------------------
!  Corrector step: correct floats locations using a 5th order
!     blended Adams-Bashforth/Adams-Moulton time-stepping scheme.
!---------------------------------------------------------------------
!
      do id=1,nfltmax
        iflt=indx(id)
        track(ixgrd,nfp1,iflt)=1./270*(
     &          19.* track(ixgrd,nfp1,iflt)+
     &         251.*(track(ixgrd,nf,iflt)+ dt/24.*( 
     &                  9.*track(ixrhs,nfp1,iflt)+
     &                 19.*track(ixrhs,nf  ,iflt)-
     &                  5.*track(ixrhs,nfm1,iflt)+
     &                     track(ixrhs,nfm2,iflt) )))

        track(iygrd,nfp1,iflt)=1./270*(
     &          19.* track(iygrd,nfp1,iflt)+
     &         251.*(track(iygrd,nf,iflt)+ dt/24.*(
     &                  9.*track(iyrhs,nfp1,iflt)+
     &                 19.*track(iyrhs,nf  ,iflt)-
     &                  5.*track(iyrhs,nfm1,iflt)+
     &                     track(iyrhs,nfm2,iflt) )))
# ifdef SOLVE3D
        track(izgrd,nfp1,iflt)=1./270*(
     &          19.* track(izgrd,nfp1,iflt)+
     &         251.*(track(izgrd,nf,iflt)+ dt/24.*(
     &                  9.*track(izrhs,nfp1,iflt)+
     &                 19.*track(izrhs,nf  ,iflt)-
     &                  5.*track(izrhs,nfm1,iflt)+
     &                     track(izrhs,nfm2,iflt) )))

        track(izgrd,nfp1,iflt)=max(0., min(float(N),
     &                      track(izgrd,nfp1,iflt)))

# endif
      enddo

# ifdef RANDOM_WALK
      call random_walk(indx,nfltmax)
c        do id=1,nfltmax
c           iflt=indx(id)
c           tmp(id)=track(izgrd,nfp1,iflt)
c        enddo
cC compute dAkv/dz at tmp (linear interpolation) and adds 
cC the right random walk components to the vertical position
c        call random_walk(tmp,id)
c        do id=1,nfltmax
c           iflt=indx(id)
c           track(izgrd,nfp1,iflt)=tmp(id)
c        enddo
# endif

!
!---------------------------------------------------------------------
!  If appropriate, activate the release of new floats and set initial
!  positions for all time levels.
!---------------------------------------------------------------------
!
      nfltmax_bak=nfltmax
      cff1=time-0.5*dt
      cff2=time+0.5*dt
      do iflt=Lstr,Lend

CDEBUG      write(*,*)'Tinfo(itstr)',Tinfo(itstr,iflt)
CDEBUG      write(*,*)'Tinfo(igrd)',nint(Tinfo(igrd,iflt))
CDEBUG      write(*,*)'level',level

        if (nint(Tinfo(igrd,iflt)).eq.level .and.
     &            Tinfo(itstr,iflt).gt.cff1 .and.
     &            Tinfo(itstr,iflt).lt.cff2) then
          nfltmax=nfltmax+1   ! Add newly released floats
          indx(nfltmax)=iflt  ! to the list
        endif
      enddo

      do id=nfltmax_bak+1,nfltmax
        iflt=indx(id)
        fltgrd(iflt)=nint(Tinfo(igrd,iflt))
        track(ixgrd,0,iflt)=Tinfo(ixgrd,iflt)
        track(iygrd,0,iflt)=Tinfo(iygrd,iflt)
        track(ixgrd,1,iflt)=Tinfo(ixgrd,iflt)
        track(iygrd,1,iflt)=Tinfo(iygrd,iflt)
        track(ixgrd,2,iflt)=Tinfo(ixgrd,iflt)
        track(iygrd,2,iflt)=Tinfo(iygrd,iflt)
        track(ixgrd,3,iflt)=Tinfo(ixgrd,iflt)
        track(iygrd,3,iflt)=Tinfo(iygrd,iflt)
!       write(*,*) 'release of float ', iflt 
!       write(*,*) 'position is (XA) ', track(ixgrd,i,iflt),
!     & '    ', track(iygrd,i,iflt)
       i=0

!     DETERMINATION OF FLOAT SIGMA LEVEL (WITH WEIGHTS) 
!     DONE HERE AND NO LONGER IN init_floats.F to 
!     achieve a better accuracy (using the child grid 
!     bathymetry and the four surrounding points). 

# ifdef SOLVE3D
        zfloat=Tinfo(izgrd,iflt)

        if (zfloat.lt.0.0) then
          xfloat0=INT(Tinfo(ixgrd,iflt))
          d1=Tinfo(ixgrd,iflt)-xfloat0
          yfloat0=INT(Tinfo(iygrd,iflt))
          d2=Tinfo(iygrd,iflt)-yfloat0
          track(izgrd,i,iflt)=0. ! default bottom value
          summ=0.
          do index1=0,1
          xfloat=xfloat0+index1
          do index2=0,1
          yfloat=yfloat0+index2

          temp=((1-index1)*(1-d1)+index1*d1)*
     &   ((1-index2)*(1-d2)+index2*d2)
          summ=summ+temp
          
          do k=N,1,-1

            if ((z_w(xfloat,yfloat,k)-zfloat)*
     &          (zfloat-z_w(xfloat,yfloat,k-1)).ge.0.0) then
            temp2=(FLOAT(k-1)+
     &     (zfloat-z_w(xfloat,yfloat,k-1))/Hz(xfloat,yfloat,k))
            track(izgrd,i,iflt)=track(izgrd,i,iflt)+temp2*
     &     temp
           
           endif
          enddo

          enddo
          enddo

          if (summ .ne. 0) then 
          track(izgrd,i,iflt)=track(izgrd,i,iflt)/summ
          endif
        endif
# else
        track(izgrd,i,iflt)=0.0
# endif

        do i=0,NFT
            track(izgrd,i,iflt)=track(izgrd,0,iflt)
        enddo
      enddo

!
!---------------------------------------------------------------------
!  Calculate slopes with corrected locations.
!---------------------------------------------------------------------
!
# ifdef SOLVE3D
      call rhs_floats (u(START_2D_ARRAY,1,nnew),
     &                 v(START_2D_ARRAY,1,nnew),
     &                We(START_2D_ARRAY,0),
     &                           nfltmax, indx)

# else
      call rhs_floats (ubar(START_2D_ARRAY,knew),
     &                 vbar(START_2D_ARRAY,knew),
     &                            nfltmax, indx)
# endif
!
!  If newly released floats, initialize slopes at all time levels.
!
      do id=nfltmax_bak+1,nfltmax
        iflt=indx(id)
        xrhs=track(ixrhs,nfp1,iflt)
        yrhs=track(iyrhs,nfp1,iflt)
# ifdef SOLVE3D
        zrhs=track(izrhs,nfp1,iflt)
# endif
        do i=0,NFT
          track(ixrhs,i,iflt)=xrhs
          track(iyrhs,i,iflt)=yrhs
# ifdef SOLVE3D
          track(izrhs,i,iflt)=zrhs
# endif
        enddo
      enddo
 
!---------------------------------------------------------------------
!  Interpolate various output variables at the corrected locations,
!  if writing occurs at next time step. Not optimal yet since 
!  diags routines are called three times for level 3 (instead of one). 
!  Build an AMR function to optimize this. 
!---------------------------------------------------------------------

      if (wrtflt(indxfltVel)) then
         do id=1,nfltmax
           iflt=indx(id)
           nrecvel(iflt)=nrecvel(iflt)+1
           i1=int(track(ixgrd,nfp1,iflt))
           j1=int(track(iygrd,nfp1,iflt))
           trackaux(ifvel,iflt)=trackaux(ifvel,iflt)+
     &   sqrt(
     &        (track(ixrhs,nfp1,iflt)/pm(i1,j1))**2+
     &        (track(iyrhs,nfp1,iflt)/pn(i1,j1))**2  )
         enddo
      endif
       
      if (diagfloats) then

# ifdef AGRIF
        call Agrif_laststep(test)
        if (test) then
# endif

# ifdef SPHERICAL
          call interp_r2d_type (lonr(START_2D_ARRAY), iflon,
     &                                      nfltmax, indx)
          call interp_r2d_type (latr(START_2D_ARRAY), iflat,
     &                                      nfltmax, indx)
# else
          call interp_r2d_type (  xr(START_2D_ARRAY), iflon,
     &                                      nfltmax, indx)
          call interp_r2d_type (  yr(START_2D_ARRAY), iflat,
     &                                      nfltmax, indx)
# endif

# ifdef SOLVE3D
          call interp_w3d_type (z_w(START_2D_ARRAY,0), ifdpt,
     &                                      nfltmax, indx)

          if (wrtflt(indxfltRho)) then 
            call interp_r3d_type (rho(START_2D_ARRAY,1), ifden,
     &                                      nfltmax, indx)
          endif

          if (wrtflt(indxfltTemp)) then
            itrc=1
            call interp_r3d_type (t(START_2D_ARRAY,1,nnew,itrc),
     &                            iftem, nfltmax, indx)
          endif

#  ifdef SALINITY
          if (wrtflt(indxfltSalt)) then
            itrc=2
            call interp_r3d_type (t(START_2D_ARRAY,1,nnew,itrc),
     &                            ifsal, nfltmax, indx)
          endif
#  endif
# endif /* SOLVE3D */

          if (wrtflt(indxfltGrd)) then
            do id=1,nfltmax
              iflt=indx(id)
              trackaux(ixgrd,iflt)=track(ixgrd,nfp1,iflt)
              trackaux(iygrd,iflt)=track(iygrd,nfp1,iflt)
# ifdef SOLVE3D
              trackaux(izgrd,iflt)=track(izgrd,nfp1,iflt)
# endif
            enddo
          endif

          if (wrtflt(indxfltVel)) then !average for Vel instead of sum
            do id=1,nfltmax
              iflt=indx(id)
              trackaux(ifvel,iflt)=trackaux(ifvel,iflt)/
     &                             nrecvel(iflt)
              nrecvel(iflt)=0
            enddo
          endif

# ifdef AGRIF
        endif  !Agrif_laststep
# endif AGRIF

      endif  !diagfloats


!---------------------------------------------------------------------
!  Determine floats status (dead or alive).
!  Tranfers a float from a child to its parent if necessary
!  and if the time step allows this trasnfer
!  The possibility to have periodic boundary conditions
!  has been removed to simplify the conditional structure.
!---------------------------------------------------------------------
!
# ifdef AGRIF
      if (.not. Agrif_Root()) then
        if (Agrif_nbstepint() .eq. int(Agrif_rhot())-1) then

          do id=1,nfltmax
            iflt=indx(id)
            if (   track(ixgrd,nfp1,iflt) .gt. float(Lm)+0.5-deltac2p
     &        .or. track(ixgrd,nfp1,iflt) .lt. 0.5+deltac2p
     &        .or. track(iygrd,nfp1,iflt) .gt. float(Mm)+0.5-deltac2p
     &        .or. track(iygrd,nfp1,iflt) .lt. 0.5+deltac2p   ) then

! Modify velocities and position for new grid level. Not 
! perfect since the velocity is going to be exact in the 
! new grid for time nf only (and nfm1 if refinement coef is 3)
! Note : it is ok to just copy the value for z since grid 
! topographies match near the boundaries. 
              fltgrd(iflt)=Agrif_Parent_Fixed()
              tmpnfp1=floattindex(fltgrd(iflt))
              tmpnf=mod(tmpnfp1+3,NFT+1)
              tmpnfm1=mod(tmpnfp1+2,NFT+1)
              tmpnfm2=mod(tmpnfp1+1,NFT+1)

              tmptrack=track(ixrhs,nfm2,iflt)/rcoefx
              track(ixrhs,tmpnfp1,iflt)=track(ixrhs,nfp1,iflt)/rcoefx
              track(ixrhs,tmpnf,iflt)=tmptrack
              track(ixrhs,tmpnfm1,iflt)=tmptrack
              track(ixrhs,tmpnfm2,iflt)=tmptrack

              tmptrack=track(iyrhs,nfm2,iflt)/rcoefy
              track(iyrhs,tmpnfp1,iflt)=track(iyrhs,nf,iflt)/rcoefy
              track(iyrhs,tmpnf,iflt)=tmptrack
              track(iyrhs,tmpnfm1,iflt)=tmptrack
              track(iyrhs,tmpnfm2,iflt)=tmptrack

              tmptrack=track(izrhs,nfm2,iflt)
              track(izrhs,tmpnfp1,iflt)=track(izrhs,nf,iflt)
              track(izrhs,tmpnf,iflt)=tmptrack
              track(izrhs,tmpnfm1,iflt)=tmptrack
              track(izrhs,tmpnfm2,iflt)=tmptrack

              track(ixgrd,tmpnfp1,iflt)=(track(ixgrd,nfp1,iflt)-0.5)/
     &                                        rcoefx+AGRIF_Ix()-0.5
              track(iygrd,tmpnfp1,iflt)=(track(iygrd,nfp1,iflt)-0.5)/
     &                                        rcoefy+AGRIF_Iy()-0.5
              track(izgrd,tmpnfp1,iflt)=track(izgrd,nfp1,iflt)
            endif  !CLOSE TO ANY BOUNDARY
          enddo
        endif
      else  ! AGRIF_Root()
        do id=1,nfltmax
          iflt=indx(id)
          if  (track(ixgrd,nfp1,iflt) .gt. float(Lm)+0.5
     &      .or.    track(ixgrd,nfp1,iflt) .lt. 0.5
     &      .or.    track(iygrd,nfp1,iflt) .gt. float(Mm)+0.5
     &      .or.    track(iygrd,nfp1,iflt) .lt. 0.5   ) then
 
              fltgrd(iflt)=-1 ! float dead
          endif
        enddo
      endif
# else 
      do id=1,nfltmax
        iflt=indx(id)
        if (track(ixgrd,nfp1,iflt) .gt. float(Lm)+0.5
     &    .or.    track(ixgrd,nfp1,iflt) .lt. 0.5
     &    .or.    track(iygrd,nfp1,iflt) .gt. float(Mm)+0.5
     &    .or.    track(iygrd,nfp1,iflt) .lt. 0.5   ) then

          fltgrd(iflt)=-1 ! float dead
        endif
      enddo
# endif /* AGRIF */
# ifdef IBM
      do id=1,nfltmax
        iflt=indx(id)
        if(fltgrd(iflt).ne. -1) then
           ibmdata(ibmage,iflt)=ibmdata(ibmage,iflt)+dt*sec2day
c          write(*,*), 'FLT = ', iflt, ' IBM AGE = ', ibmdata(ibmage,iflt)
! determination of zoel stage
           if(ibmdata(ibmage,iflt).lt.9.5) then
              ibmdata(ibmzoe,iflt) = 1.
           elseif(ibmdata(ibmage,iflt).lt.17.5) then
              ibmdata(ibmzoe,iflt) = 2.
           elseif(ibmdata(ibmage,iflt).lt.26.1) then
              ibmdata(ibmzoe,iflt) = 3.
           elseif(ibmdata(ibmage,iflt).lt.37.4) then
              ibmdata(ibmzoe,iflt) = 4.
           else
              ibmdata(ibmzoe,iflt) = 5.
           endif
        endif
c          write(*,*), 'FLT = ', iflt, ' IBM AGE = ', ibmdata(ibmage,iflt)
c    &       ,' IBM ZOE = ', ibmdata(ibmzoe,iflt)
      enddo
# endif
      return
      end

#else 

      subroutine step_floats_empty 
      return
      end

#endif /* FLOATS */

