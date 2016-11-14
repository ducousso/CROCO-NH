! $Id: uv3dmix4_GP.F 1458 2014-02-03 15:01:25Z gcambon $
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
#ifndef CHILD_SPG
      subroutine uv3dmix (tile)
!====================================================================!
!                                                                    !
!  This subroutine computes  biharmonic mixing of momentum,  rotated !
!  along geopotentials, from the horizontal divergence of the stress !
!  tensor.  A transverse isotropy is assumed so the stress tensor is !
!  splitted into vertical and horizontal subtensors.                 !
!  Components of the stress tensor are:                              !
!                                                                    !
!                         du     dv                                  !
!         s_xx = -s_yy = ---- - -----                                !
!                         dx     dy                                  !
!                                                                    !
!                         dv     du                                  !
!         s_xy =  s_yx = ---- + ----                                 !
!                         dx     dy                                  !
!                                                                    !
!  Reference:                                                        !
!                                                                    !
!      Wajsowicz, R.C, 1993: A consistent formulation of the         !
!         anisotropic stress tensor for use in models of the         !
!         large-scale ocean circulation, JCP, 105, 333-338.          !
!                                                                    !
!      Sadourny, R. and K. Maynard, 1997: Formulations of            !
!         lateral diffusion in geophysical fluid dynamics            !
!         models, In "Numerical Methods of Atmospheric and           !
!         Oceanic Modelling". Lin, Laprise, and Ritchie,             !
!         Eds., NRC Research Press, 547-556.                         !
!                                                                    !
!      Griffies, S.M. and R.W. Hallberg, 2000: Biharmonic            !
!         friction with a Smagorinsky-like viscosity for             !
!         use in large-scale eddy-permitting ocean models,           !
!         Monthly Weather Rev., 128, 8, 2935-2946.                   !
!                                                                    !
!=====================================================================
!
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
!
#ifdef MASKING
# define SWITCH *
#else
# define SWITCH !
#endif
! 
# ifdef AGRIF
      if (AGRIF_Root()) then
       call uv3dmix_tile (Istr,Iend,Jstr,Jend,
     &                      A3d(1, 1,trd),A3d(1, 2,trd),A2d(1, 1,trd),
     &                      A2d(1, 2,trd),A2d(1, 3,trd),A2d(1, 4,trd),
     &                      A2d(1, 5,trd),A2d(1, 7,trd),A2d(1, 9,trd),
     &                      A2d(1,11,trd),A2d(1,13,trd),A2d(1,15,trd),
     &                      A2d(1,17,trd),A2d(1,19,trd),A2d(1,21,trd),
     &                      A2d(1,23,trd),A2d(1,25,trd),A2d(1,27,trd))
      else
       call uv3dmix_child_tile (Istr,Iend,Jstr,Jend,
     &                      A3d(1, 1,trd),A3d(1, 2,trd),A2d(1, 1,trd),
     &                      A2d(1, 2,trd),A2d(1, 3,trd),A2d(1, 4,trd),
     &                      A2d(1, 5,trd),A2d(1, 7,trd),A2d(1, 9,trd),
     &                      A2d(1,11,trd),A2d(1,13,trd),A2d(1,15,trd),
     &                      A2d(1,17,trd),A2d(1,19,trd),A2d(1,21,trd),
     &                      A2d(1,23,trd),A2d(1,25,trd),A2d(1,27,trd))
       endif
      return
      end
# else 
      call uv3dmix_tile (Istr,Iend,Jstr,Jend,
     &                      A3d(1, 1,trd),A3d(1, 2,trd),A2d(1, 1,trd),
     &                      A2d(1, 2,trd),A2d(1, 3,trd),A2d(1, 4,trd),
     &                      A2d(1, 5,trd),A2d(1, 7,trd),A2d(1, 9,trd),
     &                      A2d(1,11,trd),A2d(1,13,trd),A2d(1,15,trd),
     &                      A2d(1,17,trd),A2d(1,19,trd),A2d(1,21,trd),
     &                      A2d(1,23,trd),A2d(1,25,trd),A2d(1,27,trd))
      return
      end
# endif   /* AGRIF */ 
!
!---------------------------------------------------------------------
!*********************************************************************
!---------------------------------------------------------------------
!
!PARENT
!
      subroutine uv3dmix_tile (Istr,Iend,Jstr,Jend,LapU,LapV,UFx,
     &                              UFe,VFx,VFe,UFs,VFs,dnUdx,dmUde,
     &                              dUdz,dnVdx,dmVde,dVdz,dZdx_r,
     &                              dZdx_p,dZde_r,dZde_p)

# undef CLIMAT_UV_MIXH_FINE
!
#else
!
! CHILD
!
      subroutine uv3dmix_child_tile (Istr,Iend,Jstr,Jend,LapU,LapV,UFx,
     &                              UFe,VFx,VFe,UFs,VFs,dnUdx,dmUde,
     &                              dUdz,dnVdx,dmVde,dVdz,dZdx_r,
     &                              dZdx_p,dZde_r,dZde_p)
!
! Diffusion always applied on U-UCLM in fine grid
# if !defined UV_HADV_RSUP3
#  define CLIMAT_UV_MIXH_FINE
# endif
!
#endif /* CHILD_SPG */
!
!---------------------------------------------------------------------
! ******************************Common Code***************************
!---------------------------------------------------------------------
!
      implicit none
#include "param.h"
#include "grid.h"
#include "mixing.h"
#include "coupling.h"
#include "ocean3d.h"
#include "scalars.h"
#ifdef CLIMAT_UV_MIXH
# include "climat.h"
#endif
#ifdef DIAGNOSTICS_UV
# include "diagnostics.h"
#endif
!
      integer   Iend, Istr, Jend, Jstr, i, j, k, k1, k2, 
     &          imin,imax,jmin,jmax,iminU,JminV, indx
      real      cff, cff1, cff2, cff3, cff4, cff5, cff6, cff7, cff8,
     &          dmUdz, dnUdz, dmVdz, dnVdz, viscoef
      real      LapU(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &          LapV(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &           UFe(PRIVATE_2D_SCRATCH_ARRAY),
     &           VFe(PRIVATE_2D_SCRATCH_ARRAY),
     &           UFx(PRIVATE_2D_SCRATCH_ARRAY),
     &           VFx(PRIVATE_2D_SCRATCH_ARRAY),
     &           UFs(PRIVATE_2D_SCRATCH_ARRAY,2),
     &           VFs(PRIVATE_2D_SCRATCH_ARRAY,2),
     &         dmUde(PRIVATE_2D_SCRATCH_ARRAY,2),
     &         dmVde(PRIVATE_2D_SCRATCH_ARRAY,2),
     &         dnUdx(PRIVATE_2D_SCRATCH_ARRAY,2),
     &         dnVdx(PRIVATE_2D_SCRATCH_ARRAY,2),
     &          dUdz(PRIVATE_2D_SCRATCH_ARRAY,2),
     &          dVdz(PRIVATE_2D_SCRATCH_ARRAY,2),
     &        dZde_p(PRIVATE_2D_SCRATCH_ARRAY,2),
     &        dZde_r(PRIVATE_2D_SCRATCH_ARRAY,2),
     &        dZdx_p(PRIVATE_2D_SCRATCH_ARRAY,2),
     &        dZdx_r(PRIVATE_2D_SCRATCH_ARRAY,2)
!
#ifdef AGRIF
#include "zoom.h"
#endif
!
#include "compute_auxiliary_bounds.h"
!
#ifdef CHILD_SPG
#define UCLM usponge
#define VCLM vsponge
#else
#define UCLM uclm
#define VCLM vclm
#endif
!
#ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        imin=istr
        iminU=istr+1
      else
        imin=istr-1
        iminU=imin
      endif
      if (EASTERN_EDGE) then
        imax=iend
      else
        imax=iend+1
      endif
#else
      imin=istr-1
      imax=iend+1
      iminU=imin
#endif
#ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        jmin=jstr
        jminV=jstr+1
      else
        jmin=jstr-1
        jminV=jmin
      endif
      if (NORTHERN_EDGE) then
        jmax=jend
      else
        jmax=jend+1
      endif
#else
      jmin=jstr-1
      jmax=jend+1
      jminV=jmin
#endif

      indx=3-nstp    !--> time index for target arrays;
!
!--------------------------------------------------------------------
!  Compute horizontal biharmonic viscosity along geopotential
!  surfaces.  The biharmonic operator is computed by applying
!  the harmonic operator twice.
!--------------------------------------------------------------------
!
!  Compute horizontal and vertical gradients.  Notice the recursive
!  blocking sequence. For momentum balance purposes, Hz appears only
!  when computing the second harmonic operator.
!  The vertical placement of the gradients is:
!
!    dZdx_r, dZde_r, dnUdx, dmVde(:,:,k1) k      rho-points
!    dZdx_r, dZde_r, dnUdx, dmVde(:,:,k2) k+1    rho-points
!    dZdx_p, dZde_p, dnVdx, dmUde(:,:,k1) k      psi-points
!    dZdx_p, dZde_p, dnVdx, dmUde(:,:,k2) k+1    psi-points
!                       UFs, dUdz(:,:,k1) k-1/2  WU-points
!                       UFs, dUdz(:,:,k2) k+1/2  WU-points
!                       VFs, dVdz(:,:,k1) k-1/2  WV-points
!                       VFs, dVdz(:,:,k2) k+1/2  WV-points
!
      k2=1
      do k=0,N
        k1=k2
        k2=3-k1
        if (k.lt.N) then
!
!  Compute slopes (nondimensional) at RHO- and PSI-points.
!
          do j=jmin-1,jmax+1
            do i=iminU-1,imax+1
              UFx(i,j)=pm_u(i,j)*dz_u(i,j,k+1)
#ifdef MASKING
     &                                     * umask(i,j)
#endif
            enddo
          enddo
          do j=jminV-1,jmax+1
            do i=imin-1,imax+1
              VFe(i,j)=pn_v(i,j)*dz_v(i,j,k+1)
#ifdef MASKING
     &                                     * vmask(i,j)
#endif
            enddo
          enddo
!
!  Compute momentum horizontal (1/m/s) and vertical (1/s) gradients.
!
          do j=jminV-1,jmax
            do i=iminU-1,imax
              dnUdx(i,j,k2)=pm(i,j)*
#if defined CLIMAT_UV_MIXH || defined CLIMAT_UV_MIXH_FINE
     &         (pn_u(i+1,j)*(u(i+1,j,k+1,nstp)- UCLM(i+1,j,k+1)) -
     &          pn_u(i  ,j)*(u(i  ,j,k+1,nstp)- UCLM(i  ,j,k+1)) )
#else
     &                      (pn_u(i+1,j)*u(i+1,j,k+1,nstp)-
     &                       pn_u(i  ,j)*u(i  ,j,k+1,nstp))
#endif
#ifdef MASKING
     &                                     * rmask(i,j)
#endif

              dmVde(i,j,k2)=pn(i,j)*
#if defined CLIMAT_UV_MIXH || defined CLIMAT_UV_MIXH_FINE
     &         (pm_v(i,j+1)*( v(i,j+1,k+1,nstp)- VCLM(i,j+1,k+1)) -
     &          pm_v(i,j  )*( v(i,j  ,k+1,nstp)- VCLM(i,j  ,k+1)) )
#else
     &                      (pm_v(i,j+1)*v(i,j+1,k+1,nstp)-
     &                       pm_v(i,j  )*v(i,j  ,k+1,nstp))
#endif
#ifdef MASKING
     &                                     * rmask(i,j)
#endif
              dZdx_r(i,j,k2)=0.5*(UFx(i,j)+UFx(i+1,j))
              dZde_r(i,j,k2)=0.5*(VFe(i,j)+VFe(i,j+1))
            enddo
          enddo
          do j=jmin,jmax+1
            do i=imin,imax+1
              dmUde(i,j,k2)=0.25*(pn(i-1,j  )+pn(i,j  )+
     &                            pn(i-1,j-1)+pn(i,j-1))*
#if defined CLIMAT_UV_MIXH || defined CLIMAT_UV_MIXH_FINE
     &         (pm_u(i,j  )*(u(i,j  ,k+1,nstp)-UCLM(i,j  ,k+1)) -
     &          pm_u(i,j-1)*(u(i,j-1,k+1,nstp)-UCLM(i,j-1,k+1)) )
#else
     &                      (pm_u(i,j  )*u(i,j  ,k+1,nstp)-
     &                       pm_u(i,j-1)*u(i,j-1,k+1,nstp))
#endif
#ifdef MASKING
     &                                     * pmask(i,j)
#endif

              dnVdx(i,j,k2)=0.25*(pm(i-1,j  )+pm(i,j  )+
     &                            pm(i-1,j-1)+pm(i,j-1))*
#if defined CLIMAT_UV_MIXH || defined CLIMAT_UV_MIXH_FINE
     &         (pn_v(i  ,j)*(v(i  ,j,k+1,nstp)-VCLM(i  ,j,k+1)) -
     &          pn_v(i-1,j)*(v(i-1,j,k+1,nstp)-VCLM(i-1,j,k+1)) )
#else
     &                      (pn_v(i  ,j)*v(i  ,j,k+1,nstp)-
     &                       pn_v(i-1,j)*v(i-1,j,k+1,nstp))
#endif
#ifdef MASKING
     &                                     * pmask(i,j)
#endif
              dZde_p(i,j,k2)=0.5*(VFe(i-1,j)+VFe(i,j))
              dZdx_p(i,j,k2)=0.5*(UFx(i,j-1)+UFx(i,j))
            enddo
          enddo
        endif
        if ((k.eq.0).or.(k.eq.N)) then
          do j=jmin-1,jmax+1
            do i=iminU-1,imax+1
              dUdz(i,j,k2)=0.0
              UFs(i,j,k2)=0.0
            enddo
          enddo
          do j=jminV-1,jmax+1
            do i=imin-1,imax+1
              dVdz(i,j,k2)=0.0
              VFs(i,j,k2)=0.0
            enddo
          enddo
        else
          do j=jmin-1,jmax+1
            do i=iminU-1,imax+1
              dUdz(i,j,k2)=(u(i,j,k+1,nstp)-u(i,j,k,nstp))/
     &                           (z_u(i,j,k+1)-z_u(i,j,k))
            enddo
          enddo
          do j=jminV-1,jmax+1
            do i=imin-1,imax+1
              dVdz(i,j,k2)=(v(i,j,k+1,nstp)-v(i,j,k,nstp))/
     &                           (z_v(i,j,k+1)-z_v(i,j,k))
            enddo
          enddo
        endif
!
!  Compute components of the rotated viscous flux (m^4 s-^3/2) along
!  geopotential surfaces in the XI- and ETA-directions.
!
        if (k.gt.0) then
          do j=jminV-1,jmax
            do i=iminU-1,imax
              cff=(on_r(i,j)*(dnUdx(i,j,k1)-0.5*pn(i,j)*
     &                        (MIN(dZdx_r(i,j,k1),0.0)*
     &                             (dUdz(i,j,k1)+dUdz(i+1,j,k2))+
     &                         MAX(dZdx_r(i,j,k1),0.0)*
     &                             (dUdz(i,j,k2)+dUdz(i+1,j,k1))))-
     &             om_r(i,j)*(dmVde(i,j,k1)-0.5*pm(i,j)*
     &                        (MIN(dZde_r(i,j,k1),0.0)*
     &                             (dVdz(i,j,k1)+dVdz(i,j+1,k2))+
     &                         MAX(dZde_r(i,j,k1),0.0)*
     &                             (dVdz(i,j,k2)+dVdz(i,j+1,k1)))))
#ifdef MASKING
     &                                               * rmask(i,j)
#endif
              UFx(i,j)=on_r(i,j)*on_r(i,j)*cff
              VFe(i,j)=om_r(i,j)*om_r(i,j)*cff
            enddo
          enddo
          do j=jmin,jmax+1
            do i=imin,imax+1
              cff=(on_p(i,j)*(dnVdx(i,j,k1)-
     &                        0.125*(pn(i-1,j-1)+pn(i-1,j)+
     &                               pn(i  ,j-1)+pn(i  ,j))*
     &                        (MIN(dZdx_p(i,j,k1),0.0)*
     &                             (dVdz(i-1,j,k1)+dVdz(i,j,k2))+
     &                         MAX(dZdx_p(i,j,k1),0.0)*
     &                             (dVdz(i-1,j,k2)+dVdz(i,j,k1))))+
     &             om_p(i,j)*(dmUde(i,j,k1)-
     &                        0.125*(pm(i-1,j-1)+pm(i-1,j)+
     &                               pm(i  ,j-1)+pm(i  ,j))*
     &                        (MIN(dZde_p(i,j,k1),0.0)*
     &                             (dUdz(i,j-1,k1)+dUdz(i,j,k2))+
     &                         MAX(dZde_p(i,j,k1),0.0)*
     &                             (dUdz(i,j-1,k2)+dUdz(i,j,k1)))))
#ifdef MASKING
     &                                              * pmask(i,j)
#endif
              UFe(i,j)=om_p(i,j)*om_p(i,j)*cff
              VFx(i,j)=on_p(i,j)*on_p(i,j)*cff
            enddo
          enddo
!
!  Compute vertical flux (m^2 s^-3/2) due to sloping terrain-following
!  surfaces.
!
          if (k.lt.N) then
            do j=jmin,jmax
              do i=iminU,imax
                cff=pn_u(i,j)
                dnUdz=cff*dUdz(i,j,k2)
                dnVdz=cff*0.25*(dVdz(i-1,j+1,k2)+dVdz(i,j+1,k2)+
     &                          dVdz(i-1,j  ,k2)+dVdz(i,j  ,k2))
                cff=pm_u(i,j)
                dmUdz=cff*dUdz(i,j,k2)
                dmVdz=cff*0.25*(dVdz(i-1,j+1,k2)+dVdz(i,j+1,k2)+
     &                          dVdz(i-1,j  ,k2)+dVdz(i,j  ,k2))
                cff1=MIN(dZdx_r(i-1,j,k1),0.0)
                cff2=MIN(dZdx_r(i  ,j,k2),0.0)
                cff3=MAX(dZdx_r(i-1,j,k2),0.0)
                cff4=MAX(dZdx_r(i  ,j,k1),0.0)
                UFs(i,j,k2)=on_u(i,j)*0.5*
     &                      (cff1*(cff1*dnUdz-dnUdx(i-1,j,k1))+
     &                       cff2*(cff2*dnUdz-dnUdx(i  ,j,k2))+
     &                       cff3*(cff3*dnUdz-dnUdx(i-1,j,k2))+
     &                       cff4*(cff4*dnUdz-dnUdx(i  ,j,k1)))
                cff1=MIN(dZde_p(i,j  ,k1),0.0)
                cff2=MIN(dZde_p(i,j+1,k2),0.0)
                cff3=MAX(dZde_p(i,j  ,k2),0.0)
                cff4=MAX(dZde_p(i,j+1,k1),0.0)
                UFs(i,j,k2)=UFs(i,j,k2)+om_u(i,j)*0.5*
     &                      (cff1*(cff1*dmUdz-dmUde(i,j  ,k1))+
     &                       cff2*(cff2*dmUdz-dmUde(i,j+1,k2))+
     &                       cff3*(cff3*dmUdz-dmUde(i,j  ,k2))+
     &                       cff4*(cff4*dmUdz-dmUde(i,j+1,k1)))
                cff1=MIN(dZde_p(i,j  ,k1),0.0)
                cff2=MIN(dZde_p(i,j+1,k2),0.0)
                cff3=MAX(dZde_p(i,j  ,k2),0.0)
                cff4=MAX(dZde_p(i,j+1,k1),0.0)
                cff5=MIN(dZdx_p(i,j  ,k1),0.0)
                cff6=MIN(dZdx_p(i,j+1,k2),0.0)
                cff7=MAX(dZdx_p(i,j  ,k2),0.0)
                cff8=MAX(dZdx_p(i,j+1,k1),0.0)
                UFs(i,j,k2)=UFs(i,j,k2)+on_u(i,j)*0.5*
     &                      (cff1*(cff5*dnVdz-dnVdx(i,j  ,k1))+
     &                       cff2*(cff6*dnVdz-dnVdx(i,j+1,k2))+
     &                       cff3*(cff7*dnVdz-dnVdx(i,j  ,k2))+
     &                       cff4*(cff8*dnVdz-dnVdx(i,j+1,k1)))
                cff1=MIN(dZdx_r(i-1,j,k1),0.0)
                cff2=MIN(dZdx_r(i  ,j,k2),0.0)
                cff3=MAX(dZdx_r(i-1,j,k2),0.0)
                cff4=MAX(dZdx_r(i  ,j,k1),0.0)
                cff5=MIN(dZde_r(i-1,j,k1),0.0)
                cff6=MIN(dZde_r(i  ,j,k2),0.0)
                cff7=MAX(dZde_r(i-1,j,k2),0.0)
                cff8=MAX(dZde_r(i  ,j,k1),0.0)
                UFs(i,j,k2)=UFs(i,j,k2)-om_u(i,j)*0.5*
     &                      (cff1*(cff5*dmVdz-dmVde(i-1,j,k1))+
     &                       cff2*(cff6*dmVdz-dmVde(i  ,j,k2))+
     &                       cff3*(cff7*dmVdz-dmVde(i-1,j,k2))+
     &                       cff4*(cff8*dmVdz-dmVde(i  ,j,k1)))
              enddo
            enddo
!
            do j=jminV,jmax
              do i=imin,imax
                cff=pn_v(i,j)
                dnUdz=cff*0.25*(dUdz(i,j  ,k2)+dUdz(i+1,j  ,k2)+
     &                          dUdz(i,j-1,k2)+dUdz(i+1,j-1,k2))
                dnVdz=cff*dUdz(i,j,k2)
                cff=pm_v(i,j)
                dmUdz=cff*0.25*(dUdz(i,j  ,k2)+dUdz(i+1,j  ,k2)+
     &                          dUdz(i,j-1,k2)+dUdz(i+1,j-1,k2))
                dmVdz=cff*dUdz(i,j,k2)
                cff1=MIN(dZdx_p(i  ,j,k1),0.0)
                cff2=MIN(dZdx_p(i+1,j,k2),0.0)
                cff3=MAX(dZdx_p(i  ,j,k2),0.0)
                cff4=MAX(dZdx_p(i+1,j,k1),0.0)
                VFs(i,j,k2)=on_v(i,j)*
     &                      (cff1*(cff1*dnVdz-dnVdx(i  ,j,k1))+
     &                       cff2*(cff2*dnVdz-dnVdx(i+1,j,k2))+
     &                       cff3*(cff3*dnVdz-dnVdx(i  ,j,k2))+
     &                       cff4*(cff4*dnVdz-dnVdx(i+1,j,k1)))
                cff1=MIN(dZde_r(i,j-1,k1),0.0)
                cff2=MIN(dZde_r(i,j  ,k2),0.0)
                cff3=MAX(dZde_r(i,j-1,k2),0.0)
                cff4=MAX(dZde_r(i,j  ,k1),0.0)
                VFs(i,j,k2)=VFs(i,j,k2)+om_v(i,j)*
     &                      (cff1*(cff1*dmVdz-dmVde(i,j-1,k1))+
     &                       cff2*(cff2*dmVdz-dmVde(i,j  ,k2))+
     &                       cff3*(cff3*dmVdz-dmVde(i,j-1,k2))+
     &                       cff4*(cff4*dmVdz-dmVde(i,j  ,k1)))
                cff1=MIN(dZde_r(i,j-1,k1),0.0)
                cff2=MIN(dZde_r(i,j  ,k2),0.0)
                cff3=MAX(dZde_r(i,j-1,k2),0.0)
                cff4=MAX(dZde_r(i,j  ,k1),0.0)
                cff5=MIN(dZdx_r(i,j-1,k1),0.0)
                cff6=MIN(dZdx_r(i,j  ,k2),0.0)
                cff7=MAX(dZdx_r(i,j-1,k2),0.0)
                cff8=MAX(dZdx_r(i,j  ,k1),0.0)
                VFs(i,j,k2)=VFs(i,j,k2)-on_v(i,j)*
     &                      (cff1*(cff5*dnUdz-dnUdx(i,j-1,k1))+
     &                       cff2*(cff6*dnUdz-dnUdx(i,j  ,k2))+
     &                       cff3*(cff7*dnUdz-dnUdx(i,j-1,k2))+
     &                       cff4*(cff8*dnUdz-dnUdx(i,j  ,k1)))
                cff1=MIN(dZdx_p(i  ,j,k1),0.0)
                cff2=MIN(dZdx_p(i+1,j,k2),0.0)
                cff3=MAX(dZdx_p(i  ,j,k2),0.0)
                cff4=MAX(dZdx_p(i+1,j,k1),0.0)
                cff5=MIN(dZde_p(i  ,j,k1),0.0)
                cff6=MIN(dZde_p(i+1,j,k2),0.0)
                cff7=MAX(dZde_p(i  ,j,k2),0.0)
                cff8=MAX(dZde_p(i+1,j,k1),0.0)
                VFs(i,j,k2)=VFs(i,j,k2)+om_v(i,j)*
     &                      (cff1*(cff5*dmUdz-dmUde(i  ,j,k1))+
     &                       cff2*(cff6*dmUdz-dmUde(i+1,j,k2))+
     &                       cff3*(cff7*dmUdz-dmUde(i  ,j,k2))+
     &                       cff4*(cff8*dmUdz-dmUde(i+1,j,k1)))
              enddo
            enddo
          endif
!
! Compute first harmonic operator (m s^-3/2).
!
          do j=jmin,jmax
            do i=iminU,imax
              LapU(i,j,k)=pm_u(i,j)*pn_u(i,j)*
     &                     (pn_u(i,j)*(UFx(i  ,j)-UFx(i-1,j))+
     &                      pm_u(i,j)*(UFe(i,j+1)-UFe(i,j  )))+
     &                   (UFs(i,j,k2)-UFs(i,j,k1))/
     &                    (0.5*(Hz(i-1,j,k)+Hz(i,j,k)))
#ifdef MASKING
     &                   * umask(i,j)
#endif
            enddo
          enddo
          do j=jminV,jmax
            do i=imin,imax
              LapV(i,j,k)=pm_v(i,j)*pn_v(i,j)*
     &                     (pn_v(i,j)*(VFx(i+1,j)-VFx(i  ,j))-
     &                      pm_v(i,j)*(VFe(i,j  )-VFe(i,j-1)))+
     &                   (VFs(i,j,k2)-VFs(i,j,k1))/
     &                     (0.5*(Hz(i,j-1,k)+Hz(i,j,k)))
#ifdef MASKING
     &                   * vmask(i,j)
#endif
            enddo
          enddo
!
!  Apply boundary conditions (other than periodic) to the first
!  harmonic operator. These are gradient or closed (free slip or
!  no slip) boundary conditions.
!
#ifndef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=jmin,jmax
# ifndef OBC_WEST
            LapU(Istr,j,k)=0.0
# else
            LapU(Istr,j,k)=LapU(Istr+1,j,k)
# endif
          enddo
          do j=jminV,jmax
# ifndef OBC_WEST
            LapV(Istr-1,j,k)=gamma2*LapV(Istr,j,k)
# else
            LapV(Istr-1,j,k)=0.0
# endif
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=jmin,jmax
# ifndef OBC_EAST
            LapU(Iend+1,j,k)=0.0
# else
            LapU(Iend+1,j,k)=LapU(Iend,j,k)
# endif
          enddo
          do j=jminV,jmax
# ifndef OBC_EAST
            LapV(Iend+1,j,k)=gamma2*LapV(Iend,j,k)
# else
            LapV(Iend+1,j,k)=0.0
# endif
          enddo
        endif
#endif /* !EW_PERIODIC */
#ifndef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=iminU,imax
# ifndef OBC_SOUTH
            LapU(i,Jstr-1,k)=gamma2*LapU(i,Jstr,k)
# else
            LapU(i,Jstr-1,k)=0.0
# endif
          enddo
          do i=imin,imax
# ifndef OBC_SOUTH
            LapV(i,Jstr,k)=0.0
# else
            LapV(i,Jstr,k)=LapV(i,Jstr+1,k)
# endif
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=iminU,imax
# ifndef OBC_NORTH
            LapU(i,Jend+1,k)=gamma2*LapU(i,Jend,k)
# else
            LapU(i,Jend+1,k)=0.0
# endif
          enddo
          do i=imin,imax
# ifndef OBC_NORTH
            LapV(i,Jend+1,k)=0.0
# else
            LapV(i,Jend+1,k)=LapV(i,Jend,k)
# endif
          enddo
        endif
#endif /* !NS_PERIODIC */
!
! Corner points
!
#if !defined EW_PERIODIC && !defined NS_PERIODIC
        IF ((SOUTHERN_EDGE).and.(WESTERN_EDGE)) THEN
          LapU(Istr  ,Jstr-1,k)=0.5*(LapU(Istr+1,Jstr-1,k)+
     &                               LapU(Istr  ,Jstr  ,k))
          LapV(Istr-1,Jstr  ,k)=0.5*(LapV(Istr-1,Jstr+1,k)+
     &                               LapV(Istr  ,Jstr  ,k))
        END IF
        IF ((SOUTHERN_EDGE).and.(EASTERN_EDGE)) THEN
          LapU(Iend+1,Jstr-1,k)=0.5*(LapU(Iend  ,Jstr-1,k)+
     &                               LapU(Iend+1,Jstr  ,k))
          LapV(Iend+1,Jstr  ,k)=0.5*(LapV(Iend  ,Jstr  ,k)+
     &                               LapV(Iend+1,Jstr+1,k))
        END IF
        IF ((NORTHERN_EDGE).and.(WESTERN_EDGE)) THEN
          LapU(Istr  ,Jend+1,k)=0.5*(LapU(Istr+1,Jend+1,k)+
     &                               LapU(Istr  ,Jend  ,k))
          LapV(Istr-1,Jend+1,k)=0.5*(LapV(Istr  ,Jend+1,k)+
     &                               LapV(Istr-1,Jend  ,k))
        END IF
        IF ((NORTHERN_EDGE).and.(EASTERN_EDGE)) THEN
          LapU(Iend+1,Jend+1,k)=0.5*(LapU(Iend  ,Jend+1,k)+
     &                               LapU(Iend+1,Jend  ,k))
          LapV(Iend+1,Jend+1,k)=0.5*(LapV(Iend  ,Jend+1,k)+
     &                               LapV(Iend+1,Jend  ,k))
        END IF
#endif
        endif
      enddo
!
!---------------------------------------------------------------------
!  Compute horizontal and vertical gradients associated with the
!  second rotated harmonic operator.
!---------------------------------------------------------------------
!
      k2=1
      do k=0,N
        k1=k2
        k2=3-k1
        if (k.lt.N) then
!
!  Compute slopes (nondimensional) at RHO- and PSI-points.
!
          do j=Jstr-1,Jend+1
            do i=IstrU-1,Iend+1
              UFx(i,j)=pm_u(i,j)*dz_u(i,j,k+1)
#ifdef MASKING
     &                * umask(i,j)
#endif
            enddo
          enddo
          do j=JstrV-1,Jend+1
            do i=Istr-1,Iend+1
              VFe(i,j)=pn_v(i,j)*dz_v(i,j,k+1)
#ifdef MASKING
     &                * vmask(i,j)
#endif
            enddo
          enddo
!
!  Compute momentum horizontal (m^-1 s^-3/2) and vertical (s^-3/2)
!  gradients.
!
          do j=JstrV-1,Jend
            do i=IstrU-1,Iend
              dnUdx(i,j,k2)=pm(i,j)*
     &                      (pn_u(i+1,j)*LapU(i+1,j,k+1)-
     &                       pn_u(i  ,j)*LapU(i  ,j,k+1)) 
#ifdef MASKING
     &                     * rmask(i,j)
#endif
              dmVde(i,j,k2)=pn(i,j)*
     &                      (pm_v(i,j+1)*LapV(i,j+1,k+1)-
     &                       pm_v(i,j  )*LapV(i,j  ,k+1)) 
#ifdef MASKING
     &                     * rmask(i,j)
#endif
              dZdx_r(i,j,k2)=0.5*(UFx(i,j)+UFx(i+1,j))
              dZde_r(i,j,k2)=0.5*(VFe(i,j)+VFe(i,j+1))

            enddo
          enddo
          do j=Jstr,Jend+1
            do i=Istr,Iend+1
              dmUde(i,j,k2)=0.25*(pn(i-1,j  )+pn(i,j  )+
     &                            pn(i-1,j-1)+pn(i,j-1))*
     &                      (pm_u(i,j  )*LapU(i,j  ,k+1)-
     &                       pm_u(i,j-1)*LapU(i,j-1,k+1))
#ifdef MASKING
     &                     * pmask(i,j)
#endif
              dnVdx(i,j,k2)=0.125*(pm(i-1,j  )+pm(i,j  )+
     &                             pm(i-1,j-1)+pm(i,j-1))*
     &                      (pn_v(i  ,j)*LapV(i  ,j,k+1)-
     &                       pn_v(i-1,j)*LapV(i-1,j,k+1))
#ifdef MASKING
     &                     * pmask(i,j)
#endif
              dZde_p(i,j,k2)=0.5*(VFe(i-1,j)+VFe(i,j))
              dZdx_p(i,j,k2)=0.5*(UFx(i,j-1)+UFx(i,j))
            enddo
          enddo
        endif
        if ((k.eq.0).or.(k.eq.N)) then
          do j=Jstr-1,Jend+1
            do i=IstrU-1,Iend+1
              dUdz(i,j,k2)=0.0
              UFs(i,j,k2)=0.0
            enddo
          enddo
          do j=JstrV-1,Jend+1
            do i=Istr-1,Iend+1
              dVdz(i,j,k2)=0.0
              VFs(i,j,k2)=0.0
            enddo
          enddo
        else
          do j=Jstr-1,Jend+1
            do i=IstrU-1,Iend+1
              dUdz(i,j,k2)=(LapU(i,j,k+1)-LapU(i,j,k))/
     &                     (0.5*(z_r(i-1,j,k+1)-z_r(i-1,j,k)+
     &                           z_r(i  ,j,k+1)-z_r(i  ,j,k)))
            enddo
          enddo
          do j=JstrV-1,Jend+1
            do i=Istr-1,Iend+1
              dVdz(i,j,k2)=(LapV(i,j,k+1)-LapV(i,j,k))/
     &                     (0.5*(z_r(i,j-1,k+1)-z_r(i,j-1,k)+
     &                           z_r(i,j  ,k+1)-z_r(i,j  ,k)))
            enddo
          enddo
        endif
!
!  Compute components of the rotated viscous flux (m5/s2) along
!  geopotential surfaces in the XI- and ETA-directions.
!
        if (k.gt.0) then
          do j=JstrV-1,Jend
            do i=IstrU-1,Iend
              cff=Hz(i,j,k)*
     &            (on_r(i,j)*(dnUdx(i,j,k1)-0.5*pn(i,j)*
     &                        (MIN(dZdx_r(i,j,k1),0.0)*
     &                             (dUdz(i,j,k1)+dUdz(i+1,j,k2))+
     &                         MAX(dZdx_r(i,j,k1),0.0)*
     &                             (dUdz(i,j,k2)+dUdz(i+1,j,k1))))-
     &             om_r(i,j)*(dmVde(i,j,k1)-0.5*pm(i,j)*
     &                        (MIN(dZde_r(i,j,k1),0.0)*
     &                             (dVdz(i,j,k1)+dVdz(i,j+1,k2))+
     &                         MAX(dZde_r(i,j,k1),0.0)*
     &                             (dVdz(i,j,k2)+dVdz(i,j+1,k1)))))
#ifdef MASKING
     &                   * rmask(i,j)
#endif

              UFx(i,j)=on_r(i,j)*on_r(i,j)*cff
              VFe(i,j)=om_r(i,j)*om_r(i,j)*cff
#ifdef VIS_COEF_3D
# ifdef UV_HADV_RSUP3
              UFx(i,j)=viscU_r(i,j,k)*UFx(i,j)
              VFe(i,j)=viscV_r(i,j,k)*VFe(i,j)
# else
              UFx(i,j)=visc3d_r(i,j,k)*UFx(i,j)
              VFe(i,j)=visc3d_r(i,j,k)*VFe(i,j)
# endif
#else
              UFx(i,j)=visc4_r(i,j)*UFx(i,j)
              VFe(i,j)=visc4_r(i,j)*VFe(i,j)
#endif
            enddo
          enddo
          do j=Jstr,Jend+1
            do i=Istr,Iend+1
              cff=0.25*(Hz(i-1,j  ,k)+Hz(i,j  ,k)+
     &                  Hz(i-1,j-1,k)+Hz(i,j-1,k))*
     &            (on_p(i,j)*(dnVdx(i,j,k1)-
     &                        0.125*(pn(i-1,j-1)+pn(i-1,j)+
     &                               pn(i  ,j-1)+pn(i  ,j))*
     &                        (MIN(dZdx_p(i,j,k1),0.0)*
     &                             (dVdz(i-1,j,k1)+dVdz(i,j,k2))+
     &                         MAX(dZdx_p(i,j,k1),0.0)*
     &                             (dVdz(i-1,j,k2)+dVdz(i,j,k1))))+
     &             om_p(i,j)*(dmUde(i,j,k1)-
     &                        0.125*(pm(i-1,j-1)+pm(i-1,j)+
     &                               pm(i  ,j-1)+pm(i  ,j))*
     &                        (MIN(dZde_p(i,j,k1),0.0)*
     &                             (dUdz(i,j-1,k1)+dUdz(i,j,k2))+
     &                         MAX(dZde_p(i,j,k1),0.0)*
     &                             (dUdz(i,j-1,k2)+dUdz(i,j,k1)))))
#ifdef MASKING
     &                  * pmask(i,j)
#endif
            UFe(i,j)=om_p(i,j)*om_p(i,j)*cff
            VFx(i,j)=on_p(i,j)*on_p(i,j)*cff
#ifdef VIS_COEF_3D
# ifdef UV_HADV_RSUP3
            UFe(i,j)=viscU_p(i,j,k)*UFe(i,j)
            VFx(i,j)=viscV_p(i,j,k)*VFx(i,j)
# else
            UFe(i,j)=visc3d_p(i,j,k)*UFe(i,j)
            VFx(i,j)=visc3d_p(i,j,k)*VFx(i,j)
# endif
#else
            UFe(i,j)=visc4_p(i,j)*UFe(i,j)
            VFx(i,j)=visc4_p(i,j)*VFx(i,j)
#endif
            enddo
          enddo
!
!  Compute vertical flux (m2/s2) due to sloping terrain-following
!  surfaces.
!
          if (k.lt.N) then
            do j=Jstr,Jend
              do i=IstrU,Iend
#ifdef VIS_COEF_3D
# ifdef UV_HADV_RSUP3
                viscoef=0.5*(viscU_r(i-1,j,k)+viscU_r(i,j,k))
# else
                viscoef=0.5*(visc3d_r(i-1,j,k)+visc3d_r(i,j,k))
# endif
#else
                viscoef=0.5*(visc4_r(i-1,j)+visc4_r(i,j))
#endif
                cff=0.5*(pn(i-1,j)+pn(i,j))
                dnUdz=cff*dUdz(i,j,k2)
                dnVdz=cff*0.25*(dVdz(i-1,j+1,k2)+dVdz(i,j+1,k2)+
     &                          dVdz(i-1,j  ,k2)+dVdz(i,j  ,k2))
                cff=0.5*(pm(i-1,j)+pm(i,j))
                dmUdz=cff*dUdz(i,j,k2)
                dmVdz=cff*0.25*(dVdz(i-1,j+1,k2)+dVdz(i,j+1,k2)+
     &                          dVdz(i-1,j  ,k2)+dVdz(i,j  ,k2))
                cff1=MIN(dZdx_r(i-1,j,k1),0.0)
                cff2=MIN(dZdx_r(i  ,j,k2),0.0)
                cff3=MAX(dZdx_r(i-1,j,k2),0.0)
                cff4=MAX(dZdx_r(i  ,j,k1),0.0)
                UFs(i,j,k2)=on_u(i,j)*0.5*viscoef*
     &                      (cff1*(cff1*dnUdz-dnUdx(i-1,j,k1))+
     &                       cff2*(cff2*dnUdz-dnUdx(i  ,j,k2))+
     &                       cff3*(cff3*dnUdz-dnUdx(i-1,j,k2))+
     &                       cff4*(cff4*dnUdz-dnUdx(i  ,j,k1)))
                cff1=MIN(dZde_p(i,j  ,k1),0.0)
                cff2=MIN(dZde_p(i,j+1,k2),0.0)
                cff3=MAX(dZde_p(i,j  ,k2),0.0)
                cff4=MAX(dZde_p(i,j+1,k1),0.0)
                UFs(i,j,k2)=UFs(i,j,k2)+om_u(i,j)*0.5*viscoef*
     &                      (cff1*(cff1*dmUdz-dmUde(i,j  ,k1))+
     &                       cff2*(cff2*dmUdz-dmUde(i,j+1,k2))+
     &                       cff3*(cff3*dmUdz-dmUde(i,j  ,k2))+
     &                       cff4*(cff4*dmUdz-dmUde(i,j+1,k1)))
                cff1=MIN(dZde_p(i,j  ,k1),0.0)
                cff2=MIN(dZde_p(i,j+1,k2),0.0)
                cff3=MAX(dZde_p(i,j  ,k2),0.0)
                cff4=MAX(dZde_p(i,j+1,k1),0.0)
                cff5=MIN(dZdx_p(i,j  ,k1),0.0)
                cff6=MIN(dZdx_p(i,j+1,k2),0.0)
                cff7=MAX(dZdx_p(i,j  ,k2),0.0)
                cff8=MAX(dZdx_p(i,j+1,k1),0.0)
                UFs(i,j,k2)=UFs(i,j,k2)+on_u(i,j)*0.5*viscoef*
     &                      (cff1*(cff5*dnVdz-dnVdx(i,j  ,k1))+
     &                       cff2*(cff6*dnVdz-dnVdx(i,j+1,k2))+
     &                       cff3*(cff7*dnVdz-dnVdx(i,j  ,k2))+
     &                       cff4*(cff8*dnVdz-dnVdx(i,j+1,k1)))
                cff1=MIN(dZdx_r(i-1,j,k1),0.0)
                cff2=MIN(dZdx_r(i  ,j,k2),0.0)
                cff3=MAX(dZdx_r(i-1,j,k2),0.0)
                cff4=MAX(dZdx_r(i  ,j,k1),0.0)
                cff5=MIN(dZde_r(i-1,j,k1),0.0)
                cff6=MIN(dZde_r(i  ,j,k2),0.0)
                cff7=MAX(dZde_r(i-1,j,k2),0.0)
                cff8=MAX(dZde_r(i  ,j,k1),0.0)
                UFs(i,j,k2)=UFs(i,j,k2)-om_u(i,j)*0.5*viscoef*
     &                      (cff1*(cff5*dmVdz-dmVde(i-1,j,k1))+
     &                       cff2*(cff6*dmVdz-dmVde(i  ,j,k2))+
     &                       cff3*(cff7*dmVdz-dmVde(i-1,j,k2))+
     &                       cff4*(cff8*dmVdz-dmVde(i  ,j,k1)))
              enddo
            enddo
!
            do j=JstrV,Jend
              do i=Istr,Iend
#ifdef VIS_COEF_3D
# ifdef UV_HADV_RSUP3
                viscoef=0.5*(viscV_r(i,j-1,k)+viscV_r(i,j,k))
# else
                viscoef=0.5*(visc3d_r(i,j-1,k)+visc3d_r(i,j,k))
# endif
#else
                viscoef=0.5*(visc4_r(i,j-1)+visc4_r(i,j))
#endif
                cff=0.5*(pn(i,j-1)+pn(i,j))
                dnUdz=cff*0.25*(dUdz(i,j  ,k2)+dUdz(i+1,j  ,k2)+
     &                          dUdz(i,j-1,k2)+dUdz(i+1,j-1,k2))
                dnVdz=cff*dUdz(i,j,k2)
                cff=0.5*(pm(i,j-1)+pm(i,j))
                dmUdz=cff*0.25*(dUdz(i,j  ,k2)+dUdz(i+1,j  ,k2)+
     &                          dUdz(i,j-1,k2)+dUdz(i+1,j-1,k2))
                dmVdz=cff*dUdz(i,j,k2)
                cff1=MIN(dZdx_p(i  ,j,k1),0.0)
                cff2=MIN(dZdx_p(i+1,j,k2),0.0)
                cff3=MAX(dZdx_p(i  ,j,k2),0.0)
                cff4=MAX(dZdx_p(i+1,j,k1),0.0)
                VFs(i,j,k2)=on_v(i,j)*0.5*viscoef*
     &                      (cff1*(cff1*dnVdz-dnVdx(i  ,j,k1))+
     &                       cff2*(cff2*dnVdz-dnVdx(i+1,j,k2))+
     &                       cff3*(cff3*dnVdz-dnVdx(i  ,j,k2))+
     &                       cff4*(cff4*dnVdz-dnVdx(i+1,j,k1)))
                cff1=MIN(dZde_r(i,j-1,k1),0.0)
                cff2=MIN(dZde_r(i,j  ,k2),0.0)
                cff3=MAX(dZde_r(i,j-1,k2),0.0)
                cff4=MAX(dZde_r(i,j  ,k1),0.0)
                VFs(i,j,k2)=VFs(i,j,k2)+om_v(i,j)*0.5*viscoef*
     &                      (cff1*(cff1*dmVdz-dmVde(i,j-1,k1))+
     &                       cff2*(cff2*dmVdz-dmVde(i,j  ,k2))+
     &                       cff3*(cff3*dmVdz-dmVde(i,j-1,k2))+
     &                       cff4*(cff4*dmVdz-dmVde(i,j  ,k1)))
                cff1=MIN(dZde_r(i,j-1,k1),0.0)
                cff2=MIN(dZde_r(i,j  ,k2),0.0)
                cff3=MAX(dZde_r(i,j-1,k2),0.0)
                cff4=MAX(dZde_r(i,j  ,k1),0.0)
                cff5=MIN(dZdx_r(i,j-1,k1),0.0)
                cff6=MIN(dZdx_r(i,j  ,k2),0.0)
                cff7=MAX(dZdx_r(i,j-1,k2),0.0)
                cff8=MAX(dZdx_r(i,j  ,k1),0.0)
                VFs(i,j,k2)=VFs(i,j,k2)-on_v(i,j)*0.5*viscoef*
     &                      (cff1*(cff5*dnUdz-dnUdx(i,j-1,k1))+
     &                       cff2*(cff6*dnUdz-dnUdx(i,j  ,k2))+
     &                       cff3*(cff7*dnUdz-dnUdx(i,j-1,k2))+
     &                       cff4*(cff8*dnUdz-dnUdx(i,j  ,k1)))
                cff1=MIN(dZdx_p(i  ,j,k1),0.0)
                cff2=MIN(dZdx_p(i+1,j,k2),0.0)
                cff3=MAX(dZdx_p(i  ,j,k2),0.0)
                cff4=MAX(dZdx_p(i+1,j,k1),0.0)
                cff5=MIN(dZde_p(i  ,j,k1),0.0)
                cff6=MIN(dZde_p(i+1,j,k2),0.0)
                cff7=MAX(dZde_p(i  ,j,k2),0.0)
                cff8=MAX(dZde_p(i+1,j,k1),0.0)
                VFs(i,j,k2)=VFs(i,j,k2)+om_v(i,j)*0.5*viscoef*
     &                      (cff1*(cff5*dmUdz-dmUde(i  ,j,k1))+
     &                       cff2*(cff6*dmUdz-dmUde(i+1,j,k2))+
     &                       cff3*(cff7*dmUdz-dmUde(i  ,j,k2))+
     &                       cff4*(cff8*dmUdz-dmUde(i+1,j,k1)))
              enddo
            enddo
          endif
!
! Apply viscous terms. Note that at this stage arrays u,v(...,3-nstp)
! contain Hz*U and Hz*V with units of [m2/s].   Also compute vertical
! integral of viscous terms and add it into coupling terms for the
! barotropic mode
!
          do j=Jstr,Jend
            do i=IstrU,Iend
              cff=pn_u(i,j)*(UFx(i,j)-UFx(i-1,j))
     &           +pm_u(i,j)*(UFe(i,j+1)-UFe(i,j))
              cff1=pm_u(i,j)*pn_u(i,j)*cff
              rufrc(i,j)=rufrc(i,j) - cff
              u(i,j,k,indx)=u(i,j,k,indx) - dt*(cff1+UFs(i,j,k2)
     &                                              -UFs(i,j,k1))
#ifdef DIAGNOSTICS_UV
              MHmix(i,j,k,1) = -2*(cff1+UFs(i,j,k2)-UFs(i,j,k1))
     &                                   /(Hz(i-1,j,k)+Hz(i,j,k))
     &                                    SWITCH umask(i,j)
#endif
            enddo
          enddo
          do j=JstrV,Jend
            do i=Istr,Iend
              cff=pn_v(i,j)*(VFx(i+1,j)-VFx(i,j))
     &           -pm_v(i,j)*(VFe(i,j)-VFe(i,j-1))
              cff1=pm_v(i,j)*pn_v(i,j)*cff 
              rvfrc(i,j)=rvfrc(i,j) - cff
              v(i,j,k,indx)=v(i,j,k,indx) - dt*(cff1+VFs(i,j,k2)
     &                                              -VFs(i,j,k1))
#ifdef DIAGNOSTICS_UV
              MHmix(i,j,k,2) = -2*(cff1+VFs(i,j,k2)-VFs(i,j,k1))
     &                                   /(Hz(i,j,k)+Hz(i,j-1,k))
     &                                    SWITCH vmask(i,j)
#endif
            enddo
          enddo
!
        endif
      enddo
!
      return
      end
!
!
#ifndef CHILD_SPG
# undef UCLM VCLM
# define CHILD_SPG
# ifdef AGRIF
#  include "uv3dmix4_GP.F"
# endif
# undef CHILD_SPG
#endif  /* !CHILD_SPG */ 

