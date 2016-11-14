! $Id: t3dmix_spg.F 1466 2014-02-06 17:37:07Z marchesiello $
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
#if defined SOLVE3D && defined SPONGE_DIF2
!
# ifdef TCLIMATOLOGY
#  define CLIMAT_TS_SPONGE
# endif
!
# ifndef CHILD_SPG
      subroutine t3dmix_spg (tile)
      implicit none
      integer tile, itrc, trd, omp_get_thread_num
#  include "param.h"
#  include "private_scratch.h"
#  include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      do itrc=1,NT

#  ifdef AGRIF
        if (AGRIF_Root()) then
          call t3dmix_spg_tile (istr,iend,jstr,jend, itrc, 
     &                                   A2d(1, 1,trd), A2d(1, 2,trd)
#   ifndef TS_MIX_S
     &                   ,A2d(1, 3,trd), A2d(1, 5,trd), A2d(1, 7,trd), 
     &                    A2d(1, 9,trd), A2d(1,10,trd), A2d(1,11,trd),
     &                    A2d(1,12,trd), A2d(1,13,trd), A2d(1,14,trd),
     &                                   A2d(1,15,trd), A2d(1,16,trd),
     &                                                  A3d(1, 1,trd)
#   endif
     &                                                              )
        else
          call t3dmix_spg_child_tile (istr,iend,jstr,jend, itrc, 
     &                                   A2d(1, 1,trd), A2d(1, 2,trd)
#   ifndef TS_MIX_S
     &                   ,A2d(1, 3,trd), A2d(1, 5,trd), A2d(1, 7,trd), 
     &                    A2d(1, 9,trd), A2d(1,10,trd), A2d(1,11,trd),
     &                    A2d(1,12,trd), A2d(1,13,trd), A2d(1,14,trd),
     &                                   A2d(1,15,trd), A2d(1,16,trd),
     &                                                  A3d(1, 1,trd)
#   endif
     &                                                              )
        endif
#  else 
          call t3dmix_spg_tile (istr,iend,jstr,jend, itrc,   
     &                                   A2d(1, 1,trd), A2d(1, 2,trd)
#   ifndef TS_MIX_S
     &                   ,A2d(1, 3,trd), A2d(1, 5,trd), A2d(1, 7,trd), 
     &                    A2d(1, 9,trd), A2d(1,10,trd), A2d(1,11,trd),
     &                    A2d(1,12,trd), A2d(1,13,trd), A2d(1,14,trd),
     &                                   A2d(1,15,trd), A2d(1,16,trd),
     &                                                  A3d(1, 1,trd)
#   endif
     &                                                              )
#  endif   /* AGRIF */

      enddo
      return
      end
!
!---------------------------------------------------------------------
!
! PARENT
!
! Compute laplacien diffusion in the parent grid sponge
! Diffusion applied on T-TCLM
!
      subroutine t3dmix_spg_tile (istr,iend,jstr,jend, itrc, FX,FE
#  ifndef TS_MIX_S
     &                                           ,FC,dTdr,dTdx,dTde,
     &                                              FFC,CF,BC,CD,DC,
     &                                                diff3u,diff3v,
     &                                                          Akz
#  endif
     &                                                            )
!
#  undef CLIMAT_TS_SPONGE_FINE

# else /* CHILD_SPG */
!
! CHILD
!
! Compute laplacien diffusion in the child sponge using 
! t3dmix_fine.F. Diffusion always applied applied on T-TCLM in fine grids
! (cpp keys :CLIMAT_TS_SPONGE_FINE)
!
      subroutine t3dmix_spg_child_tile (istr,iend,jstr,jend, itrc, FX,FE
#  ifndef TS_MIX_S
     &                                          ,FC,dTdr, dTdx,dTde,
     &                                              FFC,CF,BC,CD,DC,
     &                                                diff3u,diff3v,
     &                                                          Akz
#  endif
     &                                                             )
!
#  define CLIMAT_TS_SPONGE_FINE
!
# endif /* CHILD_SPG */
!
!---------------------------------------------------------------------
! ****************************** Common Code *************************
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
!  * Jan 2014, P. Marchesiello: adapted from t3dmix_ISO
!
!========================================================================
!
      implicit none
# include "param.h"
      integer istr,iend,jstr,jend, itrc, i,j,k,k1,k2, kmld,
     &        imin,imax,jmin,jmax, indx, idx,ide
      real   FX (PRIVATE_2D_SCRATCH_ARRAY),
     &       FE (PRIVATE_2D_SCRATCH_ARRAY)
      real   cff,cff1
# ifndef TS_MIX_S
      real  Akz (PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &       CF (PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &       DC (PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &       CD (PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &       BC (PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &       FFC(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &       FC (PRIVATE_2D_SCRATCH_ARRAY,2),
     &     dTdr (PRIVATE_2D_SCRATCH_ARRAY,2),
     &     dTdx (PRIVATE_2D_SCRATCH_ARRAY,2),
     &     dTde (PRIVATE_2D_SCRATCH_ARRAY,2),
     &   diff3u (PRIVATE_2D_SCRATCH_ARRAY),
     &   diff3v (PRIVATE_2D_SCRATCH_ARRAY)
      real  TRIADS1,TRIADS2,TRIADS3,TRIADS4,sumX,sumE,sig,
     &      SLOPEXQ1,SLOPEXQ2,SLOPEXQ3,SLOPEXQ4,
     &      SLOPEYQ1,SLOPEYQ2,SLOPEYQ3,SLOPEYQ4
      real  wgt(0:4)
# endif
# include "grid.h"
# include "ocean3d.h"
# include "mixing.h"
# ifdef CLIMAT_TS_SPONGE
#  include "climat.h"
# endif
# include "scalars.h"
# ifdef DIAGNOSTICS_TS
#  include "diagnostics.h"
# endif
# ifdef AGRIF
#  include "zoom.h"
# endif
!
# include "compute_auxiliary_bounds.h"
!
# ifdef CHILD_SPG
#  define TCLM tsponge
# else
#  define TCLM tclm
# endif
!
# ifdef MASKING
#  define SWITCH *
# else
#  define SWITCH !
# endif
!
# ifdef TS_MIX_GEO
#  define MAX min
#  define MIN max
#  define LT gt
#  define GT lt
# endif
!
# ifndef TS_MIX_S
      wgt=(/0.,1.,0.5,0.33333333333,0.25/)
# endif

# ifndef EW_PERIODIC
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
# else
      imin=istr-1
      imax=iend+1
# endif
# ifndef NS_PERIODIC
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
# else
      jmin=jstr-1
      jmax=jend+1
# endif

# ifdef TS_MIX_S
!==================================================================
!
! Add in horizontal Laplacian diffusion along constant S-surfaces.
!
!==================================================================
      do k=1,N
!
!  Compute XI- and ETA-components of diffusive tracer flux.
!
        do j=Jstr,Jend
          do i=Istr,Iend+1
          FX(i,j)=0.25*(diff2_sponge(i,j)+diff2_sponge(i-1,j))
     &                   *pmon_u(i,j)*(Hz(i,j,k)+Hz(i-1,j,k))*(
     &                t(i,j,k,nrhs,itrc)-t(i-1,j,k,nrhs,itrc)
#  if defined CLIMAT_TS_SPONGE || defined CLIMAT_TS_SPONGE_FINE 
     &                   -TCLM(i,j,k,itrc)+TCLM(i-1,j,k,itrc)
#  endif
     &                                      ) SWITCH umask(i,j)
          enddo
        enddo
        do j=Jstr,Jend+1
          do i=Istr,Iend
          FE(i,j)=0.25*(diff2_sponge(i,j)+diff2_sponge(i,j-1))
     &                     *pnom_v(i,j)*(Hz(i,j,k)+Hz(i,j-1,k))*(
     &                 t(i,j,k,nrhs,itrc)-t(i,j-1,k,nrhs,itrc)
#  if defined CLIMAT_TS_SPONGE || defined CLIMAT_TS_SPONGE_FINE 
     &                    -TCLM(i,j,k,itrc)+TCLM(i,j-1,k,itrc)
#  endif
     &                                      ) SWITCH vmask(i,j)
          enddo
        enddo
!
!  Add in horizontal diffusion of tracer [T m^3/s].
!
        do j=Jstr,Jend
          do i=Istr,Iend
            cff1=pm(i,j)*pn(i,j)
            t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)+dt*cff1
     &                 *(FX(i+1,j)-FX(i,j)+FE(i,j+1)-FE(i,j))
     &                                             /Hz(i,j,k)
          enddo
        enddo
      enddo

#  ifdef DIAGNOSTICS_TS
!
! Tracer diagnostics
!
          do j=jstr,jend
            do i=istr,iend
              cff1=pm(i,j)*pn(i,j)
              THmix(i,j,k,itrc)=FX(i+1,j)-FX(i,j)
     &                         +FE(i,j+1)-FE(i,j)
     &                                SWITCH rmask(i,j)
            enddo
          enddo
#  endif



# else /* TS_MIX_S */
!==================================================================
!
!  Add in horizontal harmonic diffusion along rotated surfaces.
!
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
#  define TS_SPONGE_IMP
!
!=============================================================
! First compute diffusivity coefficient at u and v points 
!=============================================================
      do j=jmin,jmax
        do i=imin,imax+1
          diff3u(i,j)=0.5*(diff2_sponge(i,j)+diff2_sponge(i-1,j))
        enddo
      enddo
      do j=jmin,jmax+1
        do i=imin,imax
          diff3v(i,j)=0.5*(diff2_sponge(i,j)+diff2_sponge(i,j-1))
        enddo
      enddo
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
#  if defined CLIMAT_TS_SPONGE || defined CLIMAT_TS_SPONGE_FINE
     &                       -TCLM(i  ,j,k+1,itrc)
     &                       +TCLM(i-1,j,k+1,itrc)
#  endif
     &                         )
            enddo
          enddo
          do j=jmin,jmax+1
            do i=imin,imax
              cff=0.5*(pn(i,j)+pn(i,j-1)) SWITCH vmask(i,j)
              dTde(i,j,k2)=cff*( t(i,j  ,k+1,nstp,itrc)
     &                          -t(i,j-1,k+1,nstp,itrc)
#  if defined CLIMAT_TS_SPONGE || defined CLIMAT_TS_SPONGE_FINE
     &                       -TCLM(i,j  ,k+1,itrc)
     &                       +TCLM(i,j-1,k+1,itrc)
#  endif
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
#  ifdef TS_SPONGE_IMP
               Akz (i,j,k )= 0.0   
#  endif
             enddo
          enddo
          if (k.eq.0) then
            do j=jmin-1,jmax+1
              do i=imin-1,imax+1
                dTdr(i,j,k2)= idRz(i,j,1)*( t(i,j,2,nstp,itrc)
     &                                    - t(i,j,1,nstp,itrc)
#  if defined CLIMAT_TS_SPONGE || defined CLIMAT_TS_SPONGE_FINE
     &                                 - TCLM(i,j,2,itrc)
     &                                 + TCLM(i,j,1,itrc)
#  endif
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
#  if defined CLIMAT_TS_SPONGE || defined CLIMAT_TS_SPONGE_FINE
     &                                -TCLM(i,j,k+1,itrc)
     &                                +TCLM(i,j,k  ,itrc)
#  endif
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
              FX(i,j)=cff*diff3u(i,j)*(Hz(i,j,k)+Hz(i-1,j,k))
     &               *on_u(i,j)*(   dTdx(i,j,k1) -
     &                0.25*dRdx(i,j,k)*( dTdr(i-1,j,k1)+dTdr(i,j,k2)
     &                                 + dTdr(i-1,j,k2)+dTdr(i,j,k1))
     &                                                              )
            enddo
          enddo
 
          do j=jmin,jmax+1
            do i=imin,imax
              FE(i,j)=cff*diff3v(i,j)*(Hz(i,j,k)+Hz(i,j-1,k))
     &               *om_v(i,j)*(   dTde(i,j,k1) - 
     &              0.25*dRde(i,j,k)*(   dTdr(i,j-1,k1)+dTdr(i,j,k2)
     &                              +    dTdr(i,j-1,k2)+dTdr(i,j,k1))
     &                                                              )
            enddo
          enddo


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
                sumX = diff3u(i  ,j)*dRdx(i  ,j,k  )*TRIADS1
     &               + diff3u(i  ,j)*dRdx(i  ,j,k+1)*TRIADS2
     &               + diff3u(i+1,j)*dRdx(i+1,j,k+1)*TRIADS3
     &               + diff3u(i+1,j)*dRdx(i+1,j,k  )*TRIADS4  
                idx = 4

                TRIADS1=dRde(i,j  ,k  )*dTdr(i,j,k2)-dTde(i,j  ,k1)
                TRIADS2=dRde(i,j  ,k+1)*dTdr(i,j,k2)-dTde(i,j  ,k2)
                TRIADS3=dRde(i,j+1,k+1)*dTdr(i,j,k2)-dTde(i,j+1,k2)
                TRIADS4=dRde(i,j+1,k  )*dTdr(i,j,k2)-dTde(i,j+1,k1)
                sumE = diff3v(i,j  )*dRde(i,j  ,k  )*TRIADS1
     &               + diff3v(i,j  )*dRde(i,j  ,k+1)*TRIADS2
     &               + diff3v(i,j+1)*dRde(i,j+1,k+1)*TRIADS3
     &               + diff3v(i,j+1)*dRde(i,j+1,k  )*TRIADS4   
                ide = 4

#  ifdef TS_SPONGE_IMP
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
     &                       diff3u(i  ,j)*SLOPEXQ1,
     &                       diff3u(i  ,j)*SLOPEXQ2,
     &                       diff3u(i+1,j)*SLOPEXQ3,
     &                       diff3u(i+1,j)*SLOPEXQ4)
     &                      +max(
     &                       diff3v(i,j  )*SLOPEYQ1,
     &                       diff3v(i,j  )*SLOPEYQ2,
     &                       diff3v(i,j+1)*SLOPEYQ3,
     &                       diff3v(i,j+1)*SLOPEYQ4)
#  endif /* TS_SPONGE_IMP */

!== at this point FC(i,j,k2)=(drho/dz)^(-1)
                FC(i,j,k2)=(sumX*wgt(idx)+sumE*wgt(ide))*FC(i,j,k2)

              enddo
            enddo
          endif  ! <-- k.lt.N

!++========================================================
!++ Perform time stepping
!++========================================================

          do j=jstr,jend
            do i=istr,iend
#  ifdef TS_SPONGE_IMP
              t(i,j,k,nnew,itrc)=Hz(i,j,k)*t(i,j,k,nnew,itrc) 
     &         + dt*(
     &                   pm(i,j)*pn(i,j)*( FX(i+1,j)-FX(i,j)
     &                                    +FE(i,j+1)-FE(i,j))
     &                  +FC(i,j,k2)-FC(i,j,k1)    )
#  else
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc) + dt*(
     &                   pm(i,j)*pn(i,j)*( FX(i+1,j)-FX(i,j)
     &                                    +FE(i,j+1)-FE(i,j))
     &                  +FC(i,j,k2)-FC(i,j,k1)    )/Hz(i,j,k)
#  endif
            enddo
          enddo

#  ifdef DIAGNOSTICS_TS
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
#  endif

        endif  ! <-- k.gt.0
      enddo   ! --> k

#  ifdef TS_SPONGE_IMP
!========================================================
! Compute stabilizing correction
!========================================================
!
#   define FC FFC
      do j=jstr,jend 
#    ifdef SALINITY
        indx=min(itrc,isalt)
#    else
        indx=min(itrc,itemp)
#    endif
!++
!++ Explicit vertical Laplacian
!++
        do i=istr,iend
          do k=1,N-1
            CD(i,k) = Akz(i,j,k)*
     &         (t(i,j,k+1,nstp,itrc)-t(i,j,k,nstp,itrc))
     &                     / ( z_r(i,j,k+1)-z_r(i,j,k) )
          enddo 
          CD(i,0) = 0.
          CD(i,N) = 0.
        enddo 
!++
!++ Implicit Part
!++
!
! First pass: 
! Compute the modified tridiagonal matrix coefficients for 
! the implicit vertical diffusion terms at future time step, 
! located at horizontal RHO-points and vertical W-points.
!
        do i=istr,iend
          FC(i,1)=dt*(Akz(i,j,1))/( z_r(i,j,2)-z_r(i,j,1) )
          cff=1./(Hz(i,j,1)+FC(i,1))
          CF(i,1)= cff*FC(i,1)
          DC(i,1)= cff*(t(i,j,1,nnew,itrc)-dt*(CD(i,1)-CD(i,0)))
        enddo
        do k=2,N-1,+1
          do i=istr,iend
            FC(i,k)=dt*(Akz(i,j,k))/( z_r(i,j,k+1)-z_r(i,j,k) ) 
            cff=1./(Hz(i,j,k)+FC(i,k)+FC(i,k-1)*(1.-CF(i,k-1)))
            CF(i,k)=cff*FC(i,k)
            DC(i,k)=cff*(t(i,j,k,nnew,itrc)+FC(i,k-1)*DC(i,k-1)
     &                                 -dt*(CD(i,k)-CD(i,k-1)))
          enddo
        enddo
!
! Second pass: back-substitution
!
        do i=istr,iend
           t(i,j,N,nnew,itrc)=( t(i,j,N,nnew,itrc)
     &                         -dt*(CD(i,N)-CD(i,N-1))   
     &                         +FC(i,N-1)*DC(i,N-1) )
     &                      /(Hz(i,j,N)+FC(i,N-1)*(1.-CF(i,N-1)))
        enddo
        do k=N-1,1,-1
          do i=istr,iend
            t(i,j,k,nnew,itrc)=DC(i,k)+CF(i,k)*t(i,j,k+1,nnew,itrc)
          enddo
        enddo           !--> discard FC,CF,DC
      enddo
#   undef FC 
#  endif /* TS_SPONGE_IMP */

#  undef MIN
#  undef MAX
#  undef LT
#  undef GT
# endif /* TS_MIX_S */
!
!=================================================================
!
! Finalize tracer diagnostics
!
!=================================================================
!
# if defined AGRIF && defined AGRIF_CONSERV_TRA
        do k=1,N
          MYFX(IstrR:IendR,JstrR:JendR,k,itrc)=
     &                 MYFX(IstrR:IendR,JstrR:JendR,k,itrc)+
     &                        dt*FX(IstrR:IendR,JstrR:JendR)
          MYFY(IstrR:IendR,JstrR:JendR,k,itrc)=
     &                 MYFY(IstrR:IendR,JstrR:JendR,k,itrc)+
     &                        dt*FE(IstrR:IendR,JstrR:JendR)
        enddo
# endif

# ifdef DIAGNOSTICS_TS
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
            enddo
          enddo
        enddo
!
!==========================================================
! Tracer diagnostics averaged over the MLD
!==========================================================
!
#  if defined DIAGNOSTICS_TS_MLD
#   define T_mld_nnew FX
#   define T_mld_nstp FE
       do j=Jstr,Jend
         do i=Istr,Iend
            THmix_mld(i,j,itrc)=0.
            T_mld_nnew(i,j)=0.
            T_mld_nstp(i,j)=0.
         enddo
       enddo
       do j=Jstr,Jend
         do i=Istr,Iend
#   ifdef LMD_SKPP
           kmld=kbl(i,j)
#   else
           kmld=N-5
#   endif
           do k=N,kmld,-1
             cff=Hz(i,j,k)/(z_w(i,j,N)-z_w(i,j,kmld-1))
             THmix_mld(i,j,itrc)=THmix_mld(i,j,itrc)+
     &                           THmix(i,j,k,itrc)*cff
             T_mld_nnew(i,j)=T_mld_nnew(i,j)+
     &                       t(i,j,k,nnew,itrc)*cff
           enddo
         enddo
       enddo

       do j=Jstr,Jend
         do i=Istr,Iend
#   ifdef LMD_SKPP
           if (kbl_nstp(i,j).eq.0) kbl_nstp(i,j)=kbl(i,j)
           kmld=kbl_nstp(i,j)
#   else
           kmld=N-5
#   endif
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
#   undef T_mld_nnew 
#   undef T_mld_nstp
#  endif /* DIAGNOSTICS_TS_MLD */

#  ifdef DIAGNOSTICS_DEBUG
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
#  endif /* DIAGNOSTICS_DEBUG */
# endif /* DIAGNOSTICS_TS */
!
!
!==========================================================
! Data exchange at the boundaries or interfaces
!==========================================================
!
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
#  ifdef THREE_GHOST_POINTS_TS
        call exchange_r3d_3pts_tile (Istr,Iend,Jstr,Jend,
     &                               t(START_2D_ARRAY,1,nnew,itrc))
#  else
        call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                          t(START_2D_ARRAY,1,nnew,itrc))
#  endif
# endif
      return
      end
 
# ifndef CHILD_SPG
#  undef TCLM
#  define CHILD_SPG
#  ifdef AGRIF
#   include "t3dmix_spg.F"
#  endif
#  undef CHILD_SPG
# endif  /* !CHILD_SPG */

#else
      subroutine t3dmix_spg_empty
      end
#endif /* SOLVE3D && SPONGE_DIF2 */


