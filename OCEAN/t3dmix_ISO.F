! $Id: t3dmix_ISO.F 1466 2014-02-06 17:37:07Z marchesiello $
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
#define SWTRIADS  /* spatial scheme TRIADS or SWTRIADS */
!
#ifndef CHILD_SPG
      subroutine t3dmix (tile)
      implicit none
      integer tile, itrc, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      do itrc=1,NTRA_T3DMIX

# ifdef AGRIF
        if (AGRIF_Root()) then
          call t3dmix_tile (istr,iend,jstr,jend, itrc, A3d(1,1,trd),
#  ifdef TS_MIX_IMP
     &                                                 A3d(1,2,trd),
#  endif
     &                                    A3d(1,3,trd),A3d(1,4,trd),
     &                    A2d(1, 1,trd), A2d(1, 2,trd),A2d(1,3,trd), 
     &                    A2d(1, 5,trd), A2d(1, 7,trd),A2d(1,9,trd)
#  ifdef TS_MIX_IMP
     &                   ,A2d(1,10,trd),A2d(1,11,trd),A2d(1,12,trd),
     &                                  A2d(1,13,trd),A2d(1,14,trd)
#  endif
     &                    )
        else
          call t3dmix_child_tile (istr,iend,jstr,jend, itrc, 
     &                                                 A3d(1,1,trd),
#  ifdef TS_MIX_IMP
     &                                                 A3d(1,2,trd),
#  endif
     &                                    A3d(1,3,trd),A3d(1,4,trd),
     &                       A2d(1,1,trd),A2d(1,2,trd),A2d(1,3,trd),
     &                       A2d(1,5,trd),A2d(1,7,trd),A2d(1,9,trd)
#  ifdef TS_MIX_IMP
     &                   ,A2d(1,10,trd),A2d(1,11,trd),A2d(1,12,trd),
     &                                  A2d(1,13,trd),A2d(1,14,trd)
#  endif
     &                    )
        endif
# else 
        call t3dmix_tile (istr,iend,jstr,jend, itrc,   A3d(1,1,trd),
#  ifdef TS_MIX_IMP
     &                                                 A3d(1,2,trd),
#  endif
     &                                    A3d(1,3,trd),A3d(1,4,trd),
     &                      A2d(1,1,trd), A2d(1,2,trd),A2d(1,3,trd),
     &                      A2d(1,5,trd), A2d(1,7,trd),A2d(1,9,trd)
#  ifdef TS_MIX_IMP
     &                   ,A2d(1,10,trd),A2d(1,11,trd), A2d(1,12,trd),
     &                                   A2d(1,13,trd),A2d(1,14,trd)
#  endif
     &                    )
# endif   /* AGRIF */

      enddo
      return
      end
!
!---------------------------------------------------------------------
!
!PARENT
! 
      subroutine t3dmix_tile (istr,iend,jstr,jend, itrc, LapT,
# ifdef TS_MIX_IMP
     &                                                          Akz,
# endif
     &                                                diff3u,diff3v,
     &                                      FX,FE,FC,dTdr, dTdx,dTde
# ifdef TS_MIX_IMP
     &                                              ,FFC,CF,BC,CD,DC
# endif
     &                        )
!
#else /* CHILD_SPG */
!
! CHILD
!
      subroutine t3dmix_child_tile (istr,iend,jstr,jend, itrc, LapT,
# ifdef TS_MIX_IMP
     &                                                          Akz,
# endif
     &                                                diff3u,diff3v,
     &                                      FX,FE,FC,dTdr, dTdx,dTde
# ifdef TS_MIX_IMP
     &                                              ,FFC,CF,BC,CD,DC
# endif
     &                        )
!
#endif /* CHILD_SPG */
!
!---------------------------------------------------------------------
! ******************************Common Code***************************
!---------------------------------------------------------------------
!
!
!========================================================================
!
!  This subroutine computes rotated isopycnic or geopotential horizontal 
!  mixing terms for tracer equations. The Method of Stabilizing Correction 
!  is applied to prevent a time-step constraint associated with the 
!  vertical fluxes of the rotated diffusion.
!
!  References:
!  
!  Lemarie F. et al., 2012: On the Stability and accuracy of the hamonic
!  and biharmonic isoneutral mixing operators in ocean models. Ocean
!  Modelling, in press.
!
!  Marchesiello P. L. Debreu and X. Couvelard, 2009:  Spurious
!  diapycnal mixing in terrain-following coordinate models: the problem
!  and a solution. Ocean Modelling, 26, 156-169.
!
!  * 1998,     A. Shchepetkin: first implementation of explicit operators
!  * Mar 2012, F. Lemarié: refines TRIADS/SWTRIADS operators and implements 
!              MSC techniques in CROCO
!  * Mai 2012, P. Marchesiello: corrections and adjustment to CROCO 
!              CPP choices 
!
!========================================================================
!
      implicit none
#include "param.h"
      integer istr,iend,jstr,jend, itrc, i,j,k,k1,k2, kmld,
     &        imin,imax,jmin,jmax, indx, idx,ide
      real LapT (PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &   diff3u (PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &   diff3v (PRIVATE_2D_SCRATCH_ARRAY,0:N),
#ifdef TS_MIX_IMP
     &      Akz (PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &       CF (PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &       DC (PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &       CD (PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &       BC (PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &       FFC(PRIVATE_1D_SCRATCH_ARRAY,0:N),
#endif
     &       FX (PRIVATE_2D_SCRATCH_ARRAY),
     &       FE (PRIVATE_2D_SCRATCH_ARRAY),
     &       FC (PRIVATE_2D_SCRATCH_ARRAY,2),
     &     dTdr (PRIVATE_2D_SCRATCH_ARRAY,2),
     &     dTdx (PRIVATE_2D_SCRATCH_ARRAY,2),
     &     dTde (PRIVATE_2D_SCRATCH_ARRAY,2)
       real cff,cff1,
     &      TRIADS1,TRIADS2,TRIADS3,TRIADS4,sumX,sumE,sig,
     &      SLOPEXQ1,SLOPEXQ2,SLOPEXQ3,SLOPEXQ4,
     &      SLOPEYQ1,SLOPEYQ2,SLOPEYQ3,SLOPEYQ4
       real wgt(0:4)
#include "grid.h"
#include "ocean3d.h"
#include "mixing.h"
#ifdef CLIMAT_TS_MIXH
# include "climat.h"
#endif
#include "scalars.h"
#ifdef DIAGNOSTICS_TS
# include "diagnostics.h"
#endif
#ifdef AGRIF
# include "zoom.h"
#endif
!
#include "compute_auxiliary_bounds.h"
!
#ifdef CHILD_SPG
# define TCLM tsponge
#else
# define TCLM tclm
#endif
!
#ifdef MASKING
# define SWITCH *
#else
# define SWITCH !
#endif
!
#ifdef TS_MIX_GEO
# define MAX min
# define MIN max
# define LT gt
# define GT lt
#endif
!
      wgt=(/0.,1.,0.5,0.33333333333,0.25/)

#ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        imin=istr
      else
        imin=istr-1
      endif
      if (EASTERN_EDGE) then
        imax=iend
      else
        imax=iend+1 
      endif
#else
      imin=istr-1
      imax=iend+1
#endif
#ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        jmin=jstr
      else
        jmin=jstr-1
      endif
      if (NORTHERN_EDGE) then
        jmax=jend
      else
        jmax=jend+1
      endif
#else
      jmin=jstr-1
      jmax=jend+1
#endif

!=============================================================
! First compute diffusivity coefficient according to model
! configuration
!=============================================================
      do k=1,N
        do j=jmin,jmax
          do i=imin,imax+1
            diff3u(i,j,k)= 
#ifdef TS_DIF2
     &                      0.5*(diff2(i,j,itrc)+diff2(i-1,j,itrc))
# if defined DIF_COEF_3D && defined TS_DIF_SMAGO
     &                     +0.5*(diff3d_r(i,j,k)+diff3d_r(i-1,j,k))
# endif
#elif defined TS_DIF4
     &                     +sqrt(
     &                      0.5*(diff4(i,j,itrc)+diff4(i-1,j,itrc))
# ifdef DIF_COEF_3D
#  ifdef TS_DIF_SMAGO
     &                     +0.5*(diff3d_r(i,j,k)+diff3d_r(i-1,j,k))
     &                                         *om_u(i,j)*on_u(i,j)
#  elif defined TS_HADV_RSUP3 || defined TS_HADV_RSUP5
     &                     +diff3d_u(i,j,k)
#  endif
# endif
     &                           )
#endif
          enddo
        enddo
        do j=jmin,jmax+1
          do i=imin,imax
            diff3v(i,j,k)= 
#ifdef TS_DIF2
     &                      0.5*(diff2(i,j,itrc)+diff2(i,j-1,itrc))
# if defined DIF_COEF_3D && defined TS_DIF_SMAGO
     &                     +0.5*(diff3d_r(i,j,k)+diff3d_r(i,j-1,k))
# endif
#elif defined TS_DIF4
     &                     +sqrt(
     &                      0.5*(diff4(i,j,itrc)+diff4(i,j-1,itrc))
# ifdef DIF_COEF_3D
#  ifdef TS_DIF_SMAGO
     &                     +0.5*(diff3d_r(i,j,k)+diff3d_r(i,j-1,k))
     &                                         *om_v(i,j)*on_v(i,j)
#  elif defined TS_HADV_RSUP3 || defined TS_HADV_RSUP5
     &                     +diff3d_v(i,j,k)
#  endif
# endif
     &                           )
#endif
          enddo
        enddo
      enddo

!==================================================================
!                ---------------------
!               /                    /
!              /                    /
!        dTdx(i,j,k2)         dTdx(i+1,j,k2)        <---- k + 1
!            /                    /
!           /                    / 
!          ---- dTdr(i,j,k2) ----                   <---- k + 1/2
!         /                    /
!        /                    /
!  dTdx(i,j,k1)        dTdx(i+1,j,k1)               <---- k 
!      /                    /
!     /                    /
!     ---- dTdr(i,j,k1) ---                        <---- k - 1/2
!
!==================================================================
!
!  Add in horizontal harmonic or biharmonic diffusion along 
!  rotated surfaces.
!
!  The biharmonic operator is computed by applying the rotated
!  Laplacian operator twice.
!
!                                            !!! WARNING: RECURSIVE
!    FIRST rotated Laplacian operator:           BLOCKING SEQUENCE
!
!==================================================================  

!==========================================================
! Start computation of fluxes with lateral tracer gradients
!==========================================================
      k2=1
      do k=0,N,+1
       k1=k2
       k2=3-k1
        if (k.lt.N) then
          do j=jmin,jmax
            do i=imin,imax+1
              cff=0.5*(pm(i,j)+pm(i-1,j)) SWITCH umask(i,j)
              dTdx(i,j,k2)=cff*( t(i  ,j,k+1,nstp,itrc)
     &                          -t(i-1,j,k+1,nstp,itrc)
#if defined CLIMAT_TS_MIXH || defined CLIMAT_TS_MIXH_FINE
     &                       -TCLM(i  ,j,k+1,itrc)
     &                       +TCLM(i-1,j,k+1,itrc)
#endif
     &                         )
            enddo
          enddo
          do j=jmin,jmax+1
            do i=imin,imax
              cff=0.5*(pn(i,j)+pn(i,j-1)) SWITCH vmask(i,j)
              dTde(i,j,k2)=cff*( t(i,j  ,k+1,nstp,itrc)
     &                          -t(i,j-1,k+1,nstp,itrc)
#if defined CLIMAT_TS_MIXH || defined CLIMAT_TS_MIXH_FINE
     &                       -TCLM(i,j  ,k+1,itrc)
     &                       +TCLM(i,j-1,k+1,itrc)
#endif
     &                         )
            enddo
          enddo
        endif
 
        if (k.eq.0 .or. k.eq.N) then
!========================================================
! Set bottom and top boundary condition  (FC = 0).
!========================================================
          do j=jmin-1,jmax+1
            do i=imin-1,imax+1
               FC  (i,j,k2) = 0.0
#ifdef TS_MIX_IMP
               Akz (i,j,k )= 0.0   
#endif
             enddo
          enddo
          if (k.eq.0) then
            do j=jmin-1,jmax+1
              do i=imin-1,imax+1
                dTdr(i,j,k2)= idRz(i,j,1)*( t(i,j,2,nstp,itrc)
     &                                    - t(i,j,1,nstp,itrc)
# if defined CLIMAT_TS_MIXH || defined CLIMAT_TS_MIXH_FINE
     &                                 - TCLM(i,j,2,itrc)
     &                                 + TCLM(i,j,1,itrc)
# endif
     &                                     )
              enddo
            enddo
          endif ! <-- k.eq.0
        else
!========================================================
! Compute ( delta rho )^-1
!========================================================
          do j=jmin-1,jmax+1
            do i=imin-1,imax+1
              FC(i,j,k2)  = idRz(i,j,k)*( z_r (i,j,k+1)-z_r (i,j,k) )
              dTdr(i,j,k2)= idRz(i,j,k)*( t(i,j,k+1,nstp,itrc)  
     &                                  - t(i,j,k  ,nstp,itrc) 
#if defined CLIMAT_TS_MIXH || defined CLIMAT_TS_MIXH_FINE
     &                                -TCLM(i,j,k+1,itrc)
     &                                +TCLM(i,j,k  ,itrc)
#endif
     &                                  )
            enddo
          enddo
        endif   ! --> k.eq.0 .or. k.eq.N

        if (k.gt.0) then
!========================================================
! Compute the horizontal components of the tensor
!========================================================
          cff=0.5 
          do j=jmin,jmax
            do i=imin,imax+1
              FX(i,j)=cff*diff3u(i,j,k)*(Hz(i,j,k)+Hz(i-1,j,k))
     &               *on_u(i,j)*(   dTdx(i,j,k1) -
#ifdef SWTRIADS
     &       0.5*( MAX(dRdx(i,j,k),0.)*(dTdr(i-1,j,k1)+dTdr(i,j,k2))
     &            +MIN(dRdx(i,j,k),0.)*(dTdr(i-1,j,k2)+dTdr(i,j,k1)))
#else
     &                0.25*dRdx(i,j,k)*( dTdr(i-1,j,k1)+dTdr(i,j,k2)
     &                                 + dTdr(i-1,j,k2)+dTdr(i,j,k1))
#endif
     &                                                              )
            enddo
          enddo
 
          do j=jmin,jmax+1
            do i=imin,imax
              FE(i,j)=cff*diff3v(i,j,k)*(Hz(i,j,k)+Hz(i,j-1,k))
     &               *om_v(i,j)*(   dTde(i,j,k1) - 
#ifdef SWTRIADS
     &       0.5*( MAX(dRde(i,j,k),0.)*(dTdr(i,j-1,k1)+dTdr(i,j,k2))
     &            +MIN(dRde(i,j,k),0.)*(dTdr(i,j-1,k2)+dTdr(i,j,k1)))
#else
     &              0.25*dRde(i,j,k)*(   dTdr(i,j-1,k1)+dTdr(i,j,k2)
     &                              +    dTdr(i,j-1,k2)+dTdr(i,j,k1))
#endif
     &                                                              )
            enddo
          enddo

# if defined AGRIF && defined AGRIF_CONSERV_TRA
          MYFX(IstrR:IendR,JstrR:JendR,k,itrc)=
     &          MYFX(IstrR:IendR,JstrR:JendR,k,itrc)+
     &          dt*FX(IstrR:IendR,JstrR:JendR)
          MYFY(IstrR:IendR,JstrR:JendR,k,itrc)=
     &          MYFY(IstrR:IendR,JstrR:JendR,k,itrc)+
     &          dt*FE(IstrR:IendR,JstrR:JendR)
# endif


          if (k.lt.N) then
!========================================================
! Compute the vertical component
!========================================================
            do j=jmin,jmax
              do i=imin,imax
                TRIADS1=dRdx(i  ,j,k  )*dTdr(i,j,k2)-dTdx(i  ,j,k1)
                TRIADS2=dRdx(i  ,j,k+1)*dTdr(i,j,k2)-dTdx(i  ,j,k2)
                TRIADS3=dRdx(i+1,j,k+1)*dTdr(i,j,k2)-dTdx(i+1,j,k2)
                TRIADS4=dRdx(i+1,j,k  )*dTdr(i,j,k2)-dTdx(i+1,j,k1)
#ifdef SWTRIADS
                sumX=0.
                idx=0

                if (dRdx(i  ,j,k  ) .GT. 0.) then
                 sumX=     diff3u(i  ,j,k  )*dRdx(i  ,j,k  )*TRIADS1
                 idx=idx+1
                endif

                if (dRdx(i  ,j,k+1) .LT. 0.) then
                 sumX=sumX+diff3u(i  ,j,k+1)*dRdx(i  ,j,k+1)*TRIADS2
                 idx=idx+1
                endif

                if (dRdx(i+1,j,k+1) .GT. 0.) then
                 sumX=sumX+diff3u(i+1,j,k+1)*dRdx(i+1,j,k+1)*TRIADS3
                 idx=idx+1
                endif

                if (dRdx(i+1,j,k  ) .LT. 0.) then
                 sumX=sumX+diff3u(i+1,j,k  )*dRdx(i+1,j,k  )*TRIADS4
                 idx=idx+1
                endif
#else
                sumX = diff3u(i  ,j,k  )*dRdx(i  ,j,k  )*TRIADS1
     &               + diff3u(i  ,j,k+1)*dRdx(i  ,j,k+1)*TRIADS2
     &               + diff3u(i+1,j,k+1)*dRdx(i+1,j,k+1)*TRIADS3
     &               + diff3u(i+1,j,k  )*dRdx(i+1,j,k  )*TRIADS4  
                idx = 4
#endif

                TRIADS1=dRde(i,j  ,k  )*dTdr(i,j,k2)-dTde(i,j  ,k1)
                TRIADS2=dRde(i,j  ,k+1)*dTdr(i,j,k2)-dTde(i,j  ,k2)
                TRIADS3=dRde(i,j+1,k+1)*dTdr(i,j,k2)-dTde(i,j+1,k2)
                TRIADS4=dRde(i,j+1,k  )*dTdr(i,j,k2)-dTde(i,j+1,k1)
#ifdef SWTRIADS
                sumE=0.
                ide=0

                if (dRde(i,j  ,k  ) .GT. 0.) then
                 sumE=     diff3v(i,j  ,k  )*dRde(i,j  ,k  )*TRIADS1
                 ide=ide+1
                endif

                if (dRde(i,j  ,k+1) .LT. 0.) then
                 sumE=sumE+diff3v(i,j  ,k+1)*dRde(i,j  ,k+1)*TRIADS2
                 ide=ide+1
                endif

                if (dRde(i,j+1,k+1) .GT. 0.) then
                 sumE=sumE+diff3v(i,j+1,k+1)*dRde(i,j+1,k+1)*TRIADS3
                 ide=ide+1
                endif

                if (dRde(i,j+1,k  ) .LT. 0.) then
                 sumE=sumE+diff3v(i,j+1,k  )*dRde(i,j+1,k  )*TRIADS4
                 ide=ide+1
                endif
#else
                sumE = diff3v(i,j  ,k  )*dRde(i,j  ,k  )*TRIADS1
     &               + diff3v(i,j  ,k+1)*dRde(i,j  ,k+1)*TRIADS2
     &               + diff3v(i,j+1,k+1)*dRde(i,j+1,k+1)*TRIADS3
     &               + diff3v(i,j+1,k  )*dRde(i,j+1,k  )*TRIADS4   
                ide = 4
#endif

#if defined TS_DIF2 && defined TS_MIX_IMP
!========================================================
! Compute stabilizing vertical diffusivity Akz
!========================================================

                SLOPEXQ1=(FC(i,j,k2)*dRdx(i  ,j,k  ))**2
                SLOPEXQ2=(FC(i,j,k2)*dRdx(i  ,j,k+1))**2
                SLOPEXQ3=(FC(i,j,k2)*dRdx(i+1,j,k+1))**2
                SLOPEXQ4=(FC(i,j,k2)*dRdx(i+1,j,k  ))**2

                SLOPEYQ1=(FC(i,j,k2)*dRde(i,j  ,k  ))**2
                SLOPEYQ2=(FC(i,j,k2)*dRde(i,j  ,k+1))**2
                SLOPEYQ3=(FC(i,j,k2)*dRde(i,j+1,k+1))**2
                SLOPEYQ4=(FC(i,j,k2)*dRde(i,j+1,k  ))**2

                Akz(i,j,k) = max(
     &                       diff3u(i  ,j,k  )*SLOPEXQ1,
     &                       diff3u(i  ,j,k+1)*SLOPEXQ2,
     &                       diff3u(i+1,j,k+1)*SLOPEXQ3,
     &                       diff3u(i+1,j,k  )*SLOPEXQ4)
     &                      +max(
     &                       diff3v(i,j  ,k  )*SLOPEYQ1,
     &                       diff3v(i,j  ,k+1)*SLOPEYQ2,
     &                       diff3v(i,j+1,k+1)*SLOPEYQ3,
     &                       diff3v(i,j+1,k  )*SLOPEYQ4)

!       if (i.eq.15 .and. j.eq.15 .and. itrc.eq.1) then
!        print *,'k Akz ',k,Akz(i,j,k)
!       endif
#endif /* TS_DIF2 && TS_MIX_IMP */

!== at this point FC(i,j,k2)=(drho/dz)^(-1)
                FC(i,j,k2)=(sumX*wgt(idx)+sumE*wgt(ide))*FC(i,j,k2)

              enddo
            enddo
          endif  ! <-- k.lt.N

#ifdef TS_DIF4
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! BIHARMONIC PIECE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
! Compute first laplacian
!
          do j=jmin,jmax
            do i=imin,imax
              LapT(i,j,k)=( pm(i,j)*pn(i,j)*( FX(i+1,j)-FX(i,j)
     &                                       +FE(i,j+1)-FE(i,j))
     &                     +FC(i,j,k2)-FC(i,j,k1)    )/Hz(i,j,k)
            enddo
          enddo
        endif   ! <-- k.gt.0
      enddo   ! --> k

!========================================================
!  Apply boundary conditions to the Laplacian
!  (for the cases other than periodic: closed or gradient)
!========================================================
# ifndef EW_PERIODIC
        if (WESTERN_EDGE) then
          do k=1,N
            do j=jmin,jmax
#  ifndef OBC_WEST 
              LapT(istr-1,j,k)=0.
#  else
              LapT(istr-1,j,k)=LapT(istr,j,k)
#  endif
            enddo
          enddo
        endif
        if (EASTERN_EDGE) then
          do k=1,N
            do j=jmin,jmax
#  ifndef OBC_EAST 
              LapT(iend+1,j,k)=0.
#  else
              LapT(iend+1,j,k)=LapT(iend,j,k)
#  endif
            enddo
          enddo
        endif
# endif /* !EW_PERIODIC */
# ifndef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do k=1,N
            do i=imin,imax
#  ifndef OBC_SOUTH
              LapT(i,jstr-1,k)=0.
#  else
              LapT(i,jstr-1,k)=LapT(i,jstr,k)
#  endif
            enddo
          enddo
        endif
        if (NORTHERN_EDGE) then
          do k=1,N
            do i=imin,imax
#  ifndef OBC_NORTH
              LapT(i,jend+1,k)=0.
#  else
              LapT(i,jend+1,k)=LapT(i,jend,k)
#  endif
            enddo
          enddo
        endif
# endif /* !NS_PERIODIC */

!========================================================
!  The SECOND rotated Laplacian operator:
!========================================================

      k2=1
      do k=0,N,+1
       k1=k2
       k2=3-k1
 
        if (k.lt.N) then
          do j=jstr,jend
            do i=istr,iend+1
              cff=0.5*(pm(i,j)+pm(i-1,j)) SWITCH umask(i,j)
              dTdx(i,j,k2)=cff*(LapT(i,j,k+1)-LapT(i-1,j,k+1))
            enddo
          enddo
          do j=jstr,jend+1
            do i=istr,iend
              cff=0.5*(pn(i,j)+pn(i,j-1)) SWITCH vmask(i,j)
              dTde(i,j,k2)=cff*(LapT(i,j,k+1)-LapT(i,j-1,k+1))
            enddo
          enddo
        endif
 
        if (k.eq.0 .or. k.eq.N) then
          do j=jstr-1,jend+1
            do i=istr-1,iend+1
              FC(i,j,k2)=0.0
              dTdr(i,j,k2)=0.0
# ifdef TS_MIX_IMP
              Akz (i,j,k )= 0.0   
# endif
            enddo
          enddo
          if (k.eq.0) then
            do j=jstr-1,jend+1
              do i=istr-1,iend+1
                dTdr(i,j,k2)= idRz(i,j,1)
     &                    *( LapT(i,j,2)-LapT(i,j,1) )
              enddo
            enddo
          endif

        else
!========================================================
! Compute ( delta rho )^-1
!========================================================
          do j=jstr-1,jend+1
            do i=istr-1,iend+1
              FC(i,j,k2)  = idRz(i,j,k)*( z_r (i,j,k+1)-z_r (i,j,k) )
              dTdr(i,j,k2)= idRz(i,j,k)*( LapT(i,j,k+1)-LapT(i,j,k) )
            enddo
          enddo
        endif
 
        if (k.gt.0) then
!========================================================
! Compute the horizontal components of the tensor
!========================================================            
          cff=0.5
          do j=jstr,jend
            do i=istr,iend+1
              FX(i,j)=-cff*diff3u(i,j,k)*(Hz(i,j,k)+Hz(i-1,j,k))
     &         *on_u(i,j)*(   dTdx(i  ,j,k1) -
# ifdef SWTRIADS
     &       0.5*( MAX(dRdx(i,j,k),0.)*(dTdr(i-1,j,k1)+dTdr(i,j,k2))
     &            +MIN(dRdx(i,j,k),0.)*(dTdr(i-1,j,k2)+dTdr(i,j,k1)))
# else
     &                0.25*dRdx(i,j,k)*( dTdr(i-1,j,k1)+dTdr(i,j,k2)
     &                                +  dTdr(i-1,j,k2)+dTdr(i,j,k1))
# endif
     &                                                              )
            enddo
          enddo

          do j=jstr,jend+1
            do i=istr,iend
              FE(i,j)=-cff*diff3v(i,j,k)*(Hz(i,j,k)+Hz(i,j-1,k))
     &        *om_v(i,j)*(  dTde(i,j  ,k1) - 
# ifdef SWTRIADS
     &       0.5*( MAX(dRde(i,j,k),0.)*(dTdr(i,j-1,k1)+dTdr(i,j,k2))
     &            +MIN(dRde(i,j,k),0.)*(dTdr(i,j-1,k2)+dTdr(i,j,k1)))
# else
     &              0.25*dRde(i,j,k)*(   dTdr(i,j-1,k1)+dTdr(i,j,k2)
     &                              +    dTdr(i,j-1,k2)+dTdr(i,j,k1))
# endif
     &                                                              ) 
            enddo
          enddo

          if (k.lt.N) then
!========================================================
! Compute the vertical component
!========================================================
            do j=jstr,jend
              do i=istr,iend

                TRIADS1=dRdx(i  ,j,k  )*dTdr(i,j,k2)-dTdx(i  ,j,k1)
                TRIADS2=dRdx(i  ,j,k+1)*dTdr(i,j,k2)-dTdx(i  ,j,k2)
                TRIADS3=dRdx(i+1,j,k+1)*dTdr(i,j,k2)-dTdx(i+1,j,k2)
                TRIADS4=dRdx(i+1,j,k  )*dTdr(i,j,k2)-dTdx(i+1,j,k1)
# ifdef SWTRIADS
                sumX=0.
                idx=0

                if (dRdx(i  ,j,k  ) .GT. 0.) then
                 sumX=     diff3u(i  ,j,k  )*dRdx(i  ,j,k  )*TRIADS1
                 idx=idx+1
                endif

                if (dRdx(i  ,j,k+1) .LT. 0.) then
                 sumX=sumX+diff3u(i  ,j,k+1)*dRdx(i  ,j,k+1)*TRIADS2
                 idx=idx+1
                endif

                if (dRdx(i+1,j,k+1) .GT. 0.) then
                 sumX=sumX+diff3u(i+1,j,k+1)*dRdx(i+1,j,k+1)*TRIADS3
                 idx=idx+1
                endif

                if (dRdx(i+1,j,k  ) .LT. 0.) then
                 sumX=sumX+diff3u(i+1,j,k  )*dRdx(i+1,j,k  )*TRIADS4
                 idx=idx+1
                endif
# else
                sumX = diff3u(i  ,j,k  )*dRdx(i  ,j,k  )*TRIADS1
     &               + diff3u(i  ,j,k+1)*dRdx(i  ,j,k+1)*TRIADS2
     &               + diff3u(i+1,j,k+1)*dRdx(i+1,j,k+1)*TRIADS3
     &               + diff3u(i+1,j,k  )*dRdx(i+1,j,k  )*TRIADS4
                idx = 4
# endif

                TRIADS1=dRde(i,j  ,k  )*dTdr(i,j,k2)-dTde(i,j  ,k1)
                TRIADS2=dRde(i,j  ,k+1)*dTdr(i,j,k2)-dTde(i,j  ,k2)
                TRIADS3=dRde(i,j+1,k+1)*dTdr(i,j,k2)-dTde(i,j+1,k2)
                TRIADS4=dRde(i,j+1,k  )*dTdr(i,j,k2)-dTde(i,j+1,k1)
# ifdef SWTRIADS
                sumE=0.
                ide=0

                if (dRde(i,j  ,k  ) .GT. 0.) then
                 sumE=     diff3v(i,j  ,k  )*dRde(i,j  ,k  )*TRIADS1
                 ide=ide+1
                endif

                if (dRde(i,j  ,k+1) .LT. 0.) then
                 sumE=sumE+diff3v(i,j  ,k+1)*dRde(i,j  ,k+1)*TRIADS2
                 ide=ide+1
                endif

                if (dRde(i,j+1,k+1) .GT. 0.) then
                 sumE=sumE+diff3v(i,j+1,k+1)*dRde(i,j+1,k+1)*TRIADS3
                 ide=ide+1
                endif

                if (dRde(i,j+1,k  ) .LT. 0.) then
                 sumE=sumE+diff3v(i,j+1,k  )*dRde(i,j+1,k  )*TRIADS4
                 ide=ide+1
                endif
# else
                sumE = diff3v(i,j  ,k  )*dRde(i,j  ,k  )*TRIADS1
     &               + diff3v(i,j  ,k+1)*dRde(i,j  ,k+1)*TRIADS2
     &               + diff3v(i,j+1,k+1)*dRde(i,j+1,k+1)*TRIADS3
     &               + diff3v(i,j+1,k  )*dRde(i,j+1,k  )*TRIADS4
                ide = 4
# endif

# ifdef TS_MIX_IMP
!========================================================
! Compute stabilizing vertical diffusivity Akz
!========================================================
                SLOPEXQ1=(FC(i,j,k2)*dRdx(i  ,j,k  ))**2
                SLOPEXQ2=(FC(i,j,k2)*dRdx(i  ,j,k+1))**2
                SLOPEXQ3=(FC(i,j,k2)*dRdx(i+1,j,k+1))**2
                SLOPEXQ4=(FC(i,j,k2)*dRdx(i+1,j,k  ))**2

                SLOPEYQ1=(FC(i,j,k2)*dRde(i,j  ,k  ))**2
                SLOPEYQ2=(FC(i,j,k2)*dRde(i,j  ,k+1))**2
                SLOPEYQ3=(FC(i,j,k2)*dRde(i,j+1,k+1))**2
                SLOPEYQ4=(FC(i,j,k2)*dRde(i,j+1,k  ))**2

                cff = 1./(z_r(i,j,k+1)-z_r(i,j,k))

                Akz(i,j,k) = 8.*( max(
     &                       diff3u(i  ,j,k  )*SLOPEXQ1,
     &                       diff3u(i  ,j,k+1)*SLOPEXQ2,
     &                       diff3u(i+1,j,k+1)*SLOPEXQ3,
     &                       diff3u(i+1,j,k  )*SLOPEXQ4)
     &                           +max(
     &                       diff3v(i,j  ,k  )*SLOPEYQ1,
     &                       diff3v(i,j  ,k+1)*SLOPEYQ2,
     &                       diff3v(i,j+1,k+1)*SLOPEYQ3,
     &                       diff3v(i,j+1,k  )*SLOPEYQ4)
     &                          )*( max(
     &                diff3u(i  ,j,k  )*(pm(i  ,j)**2+SLOPEXQ1*cff**2),
     &                diff3u(i  ,j,k+1)*(pm(i  ,j)**2+SLOPEXQ2*cff**2),
     &                diff3u(i+1,j,k+1)*(pm(i+1,j)**2+SLOPEXQ3*cff**2),
     &                diff3u(i+1,j,k  )*(pm(i+1,j)**2+SLOPEXQ4*cff**2))
     &                             +max(
     &                diff3v(i,j  ,k  )*(pn(i,j  )**2+SLOPEYQ1*cff**2),
     &                diff3v(i,j  ,k+1)*(pn(i,j  )**2+SLOPEYQ2*cff**2),
     &                diff3v(i,j+1,k+1)*(pn(i,j+1)**2+SLOPEYQ3*cff**2),
     &                diff3v(i,j+1,k  )*(pn(i,j+1)**2+SLOPEYQ4*cff**2))
     &                            )

!       if (i.eq.15 .and. j.eq.15 .and. itrc.eq.1) then
!        print *,'k Akz ',k,Akz(i,j,k)
!       endif

# endif /* TS_MIX_IMP */

!
! Finalize vertical flux computation
!   at this point FC(i,j,k2)=(drho/dz)^(-1)
!   then FC(i,j,k2) will contain the vertical fluxes
!
                FC(i,j,k2)=-(sumX*wgt(idx)+sumE*wgt(ide))*FC(i,j,k2)

             enddo
            enddo

          endif ! --> k.lt.N
#endif /* TS_DIF4 */

!++========================================================
!++ Perform time stepping
!++========================================================

          do j=jstr,jend
            do i=istr,iend
#ifdef TS_MIX_IMP
              t(i,j,k,nnew,itrc)=Hz(i,j,k)*t(i,j,k,nnew,itrc) 
     &         + dt*(
     &                   pm(i,j)*pn(i,j)*( FX(i+1,j)-FX(i,j)
     &                                    +FE(i,j+1)-FE(i,j))
     &                  +FC(i,j,k2)-FC(i,j,k1)    )
#else
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc) + dt*(
     &                   pm(i,j)*pn(i,j)*( FX(i+1,j)-FX(i,j)
     &                                    +FE(i,j+1)-FE(i,j))
     &                  +FC(i,j,k2)-FC(i,j,k1)    )/Hz(i,j,k)
#endif
            enddo
          enddo

#ifdef DIAGNOSTICS_TS
!
! Tracer diagnostics
!
          do j=jstr,jend
            do i=istr,iend
              cff1=pm(i,j)*pn(i,j)
              THmix(i,j,k,itrc)=FX(i+1,j)-FX(i,j)
     &                         +FE(i,j+1)-FE(i,j)
     &                         +(FC(i,j,k2)-FC(i,j,k1))/cff1
     &                                   SWITCH rmask(i,j)
            enddo
          enddo
#endif

        endif  ! <-- k.gt.0
      enddo   ! --> k

# ifdef TS_MIX_IMP
#  ifdef VADV_ADAPT_IMP
       do i=Istr,Iend
            DC(i,0)=dt*pn(i,j)*pm(i,j)
       enddo
#  endif
      do j=jstr,jend 
# define FC FFC
# include "t3dmix_tridiagonal.h"
# undef FC   
      enddo
# endif

# undef MIN
# undef MAX
# undef LT
# undef GT
# undef diff3u 
# undef diff3v 

#ifdef DIAGNOSTICS_TS
!
! Tracer diagnostics
!
        do k=1,N
          do j=jstr,jend
            do i=istr,iend
              cff1=pm(i,j)*pn(i,j)
              Trate(i,j,k,itrc)=(Hz(i,j,k)*t(i,j,k,nnew,itrc)
     &                      -Hz_bak(i,j,k)*t(i,j,k,nstp,itrc))
     &                                              /(dt*cff1)
     &                                       SWITCH rmask(i,j)
!
! Divide all diagnostic terms by the cell volume
! (Hz(i,j,k,itrc)/(pm(i,j).*pn(i,j)). There after the unit
! of diag terms are: (unit of tracers)* s-1.
!
              THmix(i,j,k,itrc)=THmix(i,j,k,itrc)*cff1/Hz(i,j,k)
              Trate(i,j,k,itrc)=Trate(i,j,k,itrc)*cff1/Hz(i,j,k)
# ifdef TS_MIX_IMP
              TVmix(i,j,k,itrc)=TVmix(i,j,k,itrc)*cff1/Hz(i,j,k)
# endif
            enddo
          enddo
        enddo
!
!==========================================================
! Tracer diagnostics averaged over the MLD
!==========================================================
!
# if defined DIAGNOSTICS_TS_MLD && !defined SPONGE_DIF2
#  define T_mld_nnew FX
#  define T_mld_nstp FE
       do j=Jstr,Jend
         do i=Istr,Iend
            THmix_mld(i,j,itrc)=0.
#  ifdef TS_MIX_IMP
            TVmix_mld(i,j,itrc)=0.
#  endif
            T_mld_nnew(i,j)=0.
            T_mld_nstp(i,j)=0.
         enddo
       enddo
       do j=Jstr,Jend
         do i=Istr,Iend
#  ifdef LMD_SKPP
           kmld=kbl(i,j)
#  else
           kmld=N-5
#  endif
           do k=N,kmld,-1
             cff=Hz(i,j,k)/(z_w(i,j,N)-z_w(i,j,kmld-1))
             THmix_mld(i,j,itrc)=THmix_mld(i,j,itrc)+
     &                           THmix(i,j,k,itrc)*cff
#  ifdef TS_MIX_IMP
             TVmix_mld(i,j,itrc)=TVmix_mld(i,j,itrc)+
     &                           TVmix(i,j,k,itrc)*cff
#  endif
             T_mld_nnew(i,j)=T_mld_nnew(i,j)+
     &                       t(i,j,k,nnew,itrc)*cff
           enddo
         enddo
       enddo

       do j=Jstr,Jend
         do i=Istr,Iend
#  ifdef LMD_SKPP
           if (kbl_nstp(i,j).eq.0) kbl_nstp(i,j)=kbl(i,j)
           kmld=kbl_nstp(i,j)
#  else
           kmld=N-5
#  endif
           do k=N,kmld,-1
             cff=Hz_bak(i,j,k)/(z_w(i,j,N)-z_w(i,j,kmld-1))
             T_mld_nstp(i,j)=T_mld_nstp(i,j)+
     &                       t(i,j,k,nstp,itrc)*cff
           enddo
           if (itrc .eq. NT) kbl_nstp(i,j)=kbl(i,j)
         enddo
       enddo
       do j=Jstr,Jend
         do i=Istr,Iend
            Trate_mld(i,j,itrc)=(T_mld_nnew(i,j)-T_mld_nstp(i,j))/dt
            Tentr_mld(i,j,itrc)=Trate_mld(i,j,itrc)-
     &                          TXadv_mld(i,j,itrc)-
     &                          TYadv_mld(i,j,itrc)-
     &                          TVadv_mld(i,j,itrc)-
     &                          TVmix_mld(i,j,itrc)-
     &                          THmix_mld(i,j,itrc)-
     &                          TForc_mld(i,j,itrc)
         enddo
       enddo
#  undef T_mld_nnew 
#  undef T_mld_nstp
# endif /* DIAGNOSTICS_TS_MLD && !SPONGE_DIF2 */

# ifdef DIAGNOSTICS_DEBUG
       if (istr.eq.1 .and. jstr.eq.1 .and. itrc.eq.itemp) then
         i=5
         j=5
#   if defined DIAGNOSTICS_TS_MLD
        cff=Trate_mld(i,j,itrc)-
     &      TXadv_mld(i,j,itrc)-
     &      TYadv_mld(i,j,itrc)-
     &      TVadv_mld(i,j,itrc)-
     &      TVmix_mld(i,j,itrc)-
     &      THmix_mld(i,j,itrc)-
     &      Tentr_mld(i,j,itrc)-
     &      TForc_mld(i,j,itrc)
           MPI_master_only write(stdout,'(A,2x,1pe10.3)')
     &     ' T3DMIX: T budget closure MLD     :', cff

#   endif /* DIAGNOSTICS_TS_MLD */
        cff=Trate(i,j,N-5,itrc)-
     &       TXadv(i,j,N-5,itrc)-
     &       TYadv(i,j,N-5,itrc)-
     &       TVadv(i,j,N-5,itrc)-
     &       TVmix(i,j,N-5,itrc)-
     &       THmix(i,j,N-5,itrc)-
     &       TForc(i,j,N-5,itrc)
           MPI_master_only write(stdout,'(A,2x,1pe10.3)')
     &     ' T3DMIX: T budget closure at k=N-5:', cff
      endif
# endif /* DIAGNOSTICS_DEBUG */
#endif /* DIAGNOSTICS_TS */
!
!
!==========================================================
! Data exchange at the boundaries or interfaces
!==========================================================
!
#if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
# ifdef THREE_GHOST_POINTS_TS
        call exchange_r3d_3pts_tile (Istr,Iend,Jstr,Jend,
     &                               t(START_2D_ARRAY,1,nnew,itrc))
# else
        call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                          t(START_2D_ARRAY,1,nnew,itrc))
# endif
#endif
      return
      end
 
#ifndef CHILD_SPG
# undef TCLM
# define CHILD_SPG
# ifdef AGRIF
#  include "t3dmix_ISO.F"
# endif
# undef CHILD_SPG
#endif  /* !CHILD_SPG */

