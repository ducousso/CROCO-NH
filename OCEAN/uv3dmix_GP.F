! $Id: uv3dmix_GP.F 1547 2014-06-13 09:31:52Z penven $
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
     &    A2d(1, 1,trd), A2d(1, 2,trd), A2d(1, 3,trd), A2d(1,4,trd),
     &    A2d(1, 5,trd), A2d(1, 7,trd), A2d(1, 9,trd), A2d(1,11,trd),
     &    A2d(1,13,trd), A2d(1,15,trd), A2d(1,17,trd), A2d(1,19,trd),
     &    A2d(1,21,trd), A2d(1,23,trd), A2d(1,25,trd), A2d(1,27,trd))
      else
       call uv3dmix_child_tile (Istr,Iend,Jstr,Jend,
     &    A2d(1, 1,trd), A2d(1, 2,trd), A2d(1, 3,trd), A2d(1,4,trd),
     &    A2d(1, 5,trd), A2d(1, 7,trd), A2d(1, 9,trd), A2d(1,11,trd),
     &    A2d(1,13,trd), A2d(1,15,trd), A2d(1,17,trd), A2d(1,19,trd),
     &    A2d(1,21,trd), A2d(1,23,trd), A2d(1,25,trd), A2d(1,27,trd))
      endif
      return
      end
# else 
      call uv3dmix_tile (Istr,Iend,Jstr,Jend,
     &    A2d(1, 1,trd), A2d(1, 2,trd), A2d(1, 3,trd), A2d(1,4,trd),
     &    A2d(1, 5,trd), A2d(1, 7,trd), A2d(1, 9,trd), A2d(1,11,trd),
     &    A2d(1,13,trd), A2d(1,15,trd), A2d(1,17,trd), A2d(1,19,trd),
     &    A2d(1,21,trd), A2d(1,23,trd), A2d(1,25,trd), A2d(1,27,trd))
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
      subroutine uv3dmix_tile (Istr,Iend,Jstr,Jend, UFx, UFe,
     &                             VFx,VFe, UFs,VFs, dnUdx,  dmUde,
     &                             dUdz,    dnVdx,   dmVde,  dVdz,
     &                             dZdx_r,  dZdx_p,  dZde_r, dZde_p)
# undef CLIMAT_UV_MIXH_FINE
!
#else
!
! CHILD
!
      subroutine uv3dmix_child_tile (Istr,Iend,Jstr,Jend, UFx, UFe,
     &                             VFx,VFe, UFs,VFs, dnUdx,  dmUde,
     &                             dUdz,    dnVdx,   dmVde,  dVdz,
     &                             dZdx_r,  dZdx_p,  dZde_r, dZde_p)
! Diffusion always applied on U-UCLM in fine grid
# if !defined UV_HADV_RSUP3
#  undef CLIMAT_UV_MIXH_FINE
# endif
!
#endif /* CHILD_SPG */
!
!--------------------------------------------------------------------
! Compute harmonic mixing of momentum, rotated along geopotentials,
! from the horizontal divergence of the  stress  tensor. Components
! of stress tensor are:
!                         du     dv
!         s_xx = -s_yy = ---- - -----
!                         dx     dy
!
!                         du     dv
!         s_xy =  s_yx = ---- + ----
!                         dy     dx
!
! A transverse  isotropy is assumed so the stress tensor is splitted
! into vertical and horizontal subtensors.
!
!  Reference:
!
! [1] Stelling, G. S., and J. A. Th. M. van Kester, 1994: On the
!       approximation of horizontal gradients in sigma-coordinates
!       for bathymetry with steep bottom slopes. Int. J. Num. Meth.
!       in Fluids, v. 18, pp. 915-935.
!
! [2] Wajsowicz, R.C, 1993: A consistent formulation of the
!       anisotropic stress tensor for use in models of the
!       large-scale ocean circulation, JCP, 105, 333-338.
!
! [3] Sadourny, R. and K. Maynard, 1997: Formulations of lateral
!       diffusion in geophysical fluid dynamics models, In
!       "Numerical Methods of Atmospheric and Oceanic Modelling".
!       Lin, Laprise, and Ritchie, Eds., NRC Research Press,
!       547-556.
!
! [4] Griffies, S.M. and R.W. Hallberg, 2000: Biharmonic friction
!       with a Smagorinsky-like viscosity for use in large-scale
!       eddy-permitting ocean models, Monthly Weather Rev.,v. 128,
!       No. 8, pp. 2935-2946.
!---------------------------------------------------------------------
!---------------------------------------------------------------------
! ******************************Common Code***************************
!---------------------------------------------------------------------
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k, k1,k2, indx
      real UFe(PRIVATE_2D_SCRATCH_ARRAY),
     &     VFe(PRIVATE_2D_SCRATCH_ARRAY),     cff,
     &     UFx(PRIVATE_2D_SCRATCH_ARRAY),     cff1,
     &     VFx(PRIVATE_2D_SCRATCH_ARRAY),     cff2,
     &     UFs(PRIVATE_2D_SCRATCH_ARRAY,2),   cff3,
     &     VFs(PRIVATE_2D_SCRATCH_ARRAY,2),   cff4,
     &   dmUde(PRIVATE_2D_SCRATCH_ARRAY,2),   cff5,
     &   dmVde(PRIVATE_2D_SCRATCH_ARRAY,2),   cff6,
     &   dnUdx(PRIVATE_2D_SCRATCH_ARRAY,2),   cff7,
     &   dnVdx(PRIVATE_2D_SCRATCH_ARRAY,2),   cff8,
     &    dUdz(PRIVATE_2D_SCRATCH_ARRAY,2),
     &    dVdz(PRIVATE_2D_SCRATCH_ARRAY,2),   dmUdz,
     &  dZde_p(PRIVATE_2D_SCRATCH_ARRAY,2),   dnUdz,
     &  dZde_r(PRIVATE_2D_SCRATCH_ARRAY,2),   dmVdz,
     &  dZdx_p(PRIVATE_2D_SCRATCH_ARRAY,2),   dnVdz,
     &  dZdx_r(PRIVATE_2D_SCRATCH_ARRAY,2)
 
#include "param.h"
#include "scalars.h"
#include "grid.h"
#include "ocean3d.h"
#include "coupling.h"
#include "mixing.h"
#ifdef CLIMAT_UV_MIXH
# include "climat.h"
#endif
#ifdef DIAGNOSTICS_UV
# include "diagnostics.h"
#endif
#ifdef AGRIF
#include "zoom.h"
#endif
!
# include "compute_auxiliary_bounds.h"
!
#ifdef CHILD_SPG
#define UCLM usponge
#define VCLM vsponge
#else
#define UCLM uclm
#define VCLM vclm
#endif
!
      indx=3-nstp    !--> time index for target arrays;
!
! Compute horizontal harmonic viscosity along geopotential surfaces.
!--------------------------------------------------------------------
!
! Compute horizontal and vertical gradients.  Notice the recursive
! blocking sequence.  The vertical placement of the gradients is:
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
! Compute sigma-slopes (nondimensional) at RHO- and PSI-points.
! Compute momentum horizontal (1/m/s) and vertical (1/s) gradients.
!
      k2=1
      do k=0,N,+1  !--> irreversible
        k1=k2
        k2=3-k1
        if (k.lt.N) then
          do j=Jstr-1,Jend+1
            do i=IstrU-1,Iend+1
              UFx(i,j)=(z_r(i,j,k+1)-z_r(i-1,j,k+1))*pm_u(i,j)
#ifdef MASKING
     &                                       * umask(i,j)
#endif
            enddo
          enddo
          do j=JstrV-1,Jend+1
            do i=Istr-1,Iend+1
              VFe(i,j)=(z_r(i,j,k+1)-z_r(i,j-1,k+1))*pn_v(i,j)
#ifdef MASKING
     &                                       * vmask(i,j)
#endif
            enddo
          enddo
          do j=JstrV-1,Jend
            do i=IstrU-1,Iend
              dnUdx(i,j,k2)=pm(i,j)*
#if defined CLIMAT_UV_MIXH || defined CLIMAT_UV_MIXH_FINE
     &        ( pn_u(i+1,j)*(u(i+1,j,k+1,nstp)-UCLM(i+1,j,k+1))
     &         -pn_u(i  ,j)*(u(i  ,j,k+1,nstp)-UCLM(i  ,j,k+1)) )
#else
     &              (pn_u(i+1,j)*u(i+1,j,k+1,nstp)
     &              -pn_u(i  ,j)*u(i  ,j,k+1,nstp))
#endif
#ifdef MASKING
     &                                       * rmask(i,j)
#endif
              dmVde(i,j,k2)=pn(i,j)*
#if defined CLIMAT_UV_MIXH || defined CLIMAT_UV_MIXH_FINE
     &        ( pm_v(i,j+1)*(v(i,j+1,k+1,nstp)-VCLM(i,j+1,k+1))
     &         -pm_v(i,j  )*(v(i,j  ,k+1,nstp)-VCLM(i,j  ,k+1)) )
#else
     &              (pm_v(i,j+1)*v(i,j+1,k+1,nstp)
     &              -pm_v(i,j  )*v(i,j  ,k+1,nstp))
#endif
#ifdef MASKING
     &                                       * rmask(i,j)
#endif
              dZdx_r(i,j,k2)=0.5*(UFx(i,j)+UFx(i+1,j))
              dZde_r(i,j,k2)=0.5*(VFe(i,j)+VFe(i,j+1))
            enddo
          enddo
          do j=Jstr,Jend+1
            do i=Istr,Iend+1
              dmUde(i,j,k2)=0.25*(pn(i,j)+pn(i-1,j)+pn(i,j-1)
     &                                            +pn(i-1,j-1))
#if defined CLIMAT_UV_MIXH || defined CLIMAT_UV_MIXH_FINE
     &       *( pm_u(i,j  )*(u(i,j  ,k+1,nstp)-UCLM(i,j,k+1))
     &         -pm_u(i,j-1)*(u(i,j-1,k+1,nstp)-UCLM(i,j-1,k+1)) )
#else
     &             *(pm_u(i,j  )*u(i,j  ,k+1,nstp)
     &              -pm_u(i,j-1)*u(i,j-1,k+1,nstp))
#endif
#ifdef MASKING
     &                                          * pmask(i,j)
#endif
              dnVdx(i,j,k2)=0.25*(pm(i,j)+pm(i-1,j)+pm(i,j-1)
     &                                            +pm(i-1,j-1))
#if defined CLIMAT_UV_MIXH || defined CLIMAT_UV_MIXH_FINE
     &       *( pn_v(i  ,j)*(v(i  ,j,k+1,nstp)-VCLM(i  ,j,k+1))
     &         -pn_v(i-1,j)*(v(i-1,j,k+1,nstp)-VCLM(i-1,j,k+1)) )
#else
     &             *(pn_v(i  ,j)*v(i  ,j,k+1,nstp)
     &              -pn_v(i-1,j)*v(i-1,j,k+1,nstp))
#endif
#ifdef MASKING
     &                                       * pmask(i,j)
#endif
              dZde_p(i,j,k2)=0.5*(VFe(i-1,j)+VFe(i,j))
              dZdx_p(i,j,k2)=0.5*(UFx(i,j-1)+UFx(i,j))
            enddo
          enddo          !--> discard UFx,VFe, keep all others
        endif
 
        if (k.eq.0 .or. k.eq.N) then
          do j=Jstr-1,Jend+1
            do i=IstrU-1,Iend+1
              dUdz(i,j,k2)=0.
              UFs(i,j,k2)=0.
            enddo
          enddo
          do j=JstrV-1,Jend+1
            do i=Istr-1,Iend+1
              dVdz(i,j,k2)=0.
              VFs(i,j,k2)=0.
            enddo
          enddo
        else
          do j=Jstr-1,Jend+1
            do i=IstrU-1,Iend+1
#if defined CLIMAT_UV_MIXH || defined CLIMAT_UV_MIXH_FINE
              dUdz(i,j,k2)=2.*(u(i,j,k+1,nstp)-u(i,j,k,nstp)
     &                            -UCLM(i,j,k+1)+UCLM(i,j,k))
#else
              dUdz(i,j,k2)=2.*(u(i,j,k+1,nstp)-u(i,j,k,nstp))
#endif
     &                          /( z_r(i-1,j,k+1)-z_r(i-1,j,k)
     &                            +z_r(i  ,j,k+1)-z_r(i  ,j,k))
            enddo
          enddo
          do j=JstrV-1,Jend+1
            do i=Istr-1,Iend+1
#if defined CLIMAT_UV_MIXH || defined CLIMAT_UV_MIXH_FINE
              dVdz(i,j,k2)=2.*(v(i,j,k+1,nstp)-v(i,j,k,nstp)
     &                            -VCLM(i,j,k+1)+VCLM(i,j,k))
#else
              dVdz(i,j,k2)=2.*(v(i,j,k+1,nstp)-v(i,j,k,nstp))
#endif
     &                          /( z_r(i,j-1,k+1)-z_r(i,j-1,k)
     &                            +z_r(i,j  ,k+1)-z_r(i,j  ,k))
            enddo
          enddo
        endif
!
! Compute components of the rotated viscous flux [m5/s2] along
! geopotential surfaces in the XI- and ETA-directions.
!
        if (k.gt.0) then
          do j=JstrV-1,Jend
            do i=IstrU-1,Iend
              cff=Hz(i,j,k)*max(0.,visc2_r(i,j)
#ifdef SPONGE_VIS2
     &                            ,visc2_sponge_r(i,j)
#endif
#ifdef UV_VIS_SMAGO
     &                            ,visc3d_r(i,j,k)
#endif
     &                         )
     &      *( om_r(i,j)*( dnUdx(i,j,k1) - 0.5*pn(i,j)*(
     &           min(dZdx_r(i,j,k1),0.)*(dUdz(i,j,k1)+dUdz(i+1,j,k2))
     &          +max(dZdx_r(i,j,k1),0.)*(dUdz(i,j,k2)+dUdz(i+1,j,k1))
     &                                                             ))
     &        -om_r(i,j)*( dmVde(i,j,k1) - 0.5*pm(i,j)*(
     &           min(dZde_r(i,j,k1),0.)*(dVdz(i,j,k1)+dVdz(i,j+1,k2))
     &          +max(dZde_r(i,j,k1),0.)*(dVdz(i,j,k2)+dVdz(i,j+1,k1))
     &                                                            )))
#ifdef MASKING
     &                                              * rmask(i,j)
#endif
              UFx(i,j)=om_r(i,j)*om_r(i,j)*cff
              VFe(i,j)=om_r(i,j)*om_r(i,j)*cff
            enddo
          enddo
          do j=Jstr,Jend+1
            do i=Istr,Iend+1
            cff=0.25*max(0.,visc2_p(i,j)
#ifdef SPONGE_VIS2
     &                     ,visc2_sponge_p(i,j)
#endif
#ifdef UV_VIS_SMAGO
     &                     ,visc3d_p(i,j,k)
#endif
     &                  )
     &             *(Hz(i,j,k)+Hz(i-1,j,k)+Hz(i,j-1,k)+Hz(i-1,j-1,k))
     &       *( om_p(i,j)*( dnVdx(i,j,k1)-0.125*( pn(i,j)+pn(i-1,j)
     &                                        +pn(i,j-1)+pn(i-1,j-1))
     &        *( min(dZdx_p(i,j,k1),0.)*(dVdz(i-1,j,k1)+dVdz(i,j,k2))
     &          +max(dZdx_p(i,j,k1),0.)*(dVdz(i-1,j,k2)+dVdz(i,j,k1))
     &                                                             )) 
     &        + om_p(i,j)*( dmUde(i,j,k1)-0.125*( pm(i,j)+pm(i-1,j)
     &                                        +pm(i,j-1)+pm(i-1,j-1))
     &        *( min(dZde_p(i,j,k1),0.)*(dUdz(i,j-1,k1)+dUdz(i,j,k2))
     &          +max(dZde_p(i,j,k1),0.)*(dUdz(i,j-1,k2)+dUdz(i,j,k1))
     &                                                            )))
#ifdef MASKING
     &                                              * pmask(i,j)
#endif
              UFe(i,j)=om_p(i,j)*om_p(i,j)*cff
              VFx(i,j)=om_p(i,j)*om_p(i,j)*cff
            enddo
          enddo
!
! Compute vertical flux [m^2/s^2] due to sloping terrain-following
! surfaces.
!
          if (k.lt.N) then
            do j=Jstr,Jend
              do i=IstrU,Iend
                cff1=pn_u(i,j)
                cff2=pm_u(i,j)
                cff=0.25*( dVdz(i,j  ,k2)+dVdz(i-1,j  ,k2)
     &                    +dVdz(i,j+1,k2)+dVdz(i-1,j+1,k2))
                dnUdz=cff1*dUdz(i,j,k2)
                dmUdz=cff2*dUdz(i,j,k2)
                dnVdz=cff1*cff
                dmVdz=cff2*cff
 
                cff1=min(dZdx_r(i-1,j,k1),0.)
                cff2=min(dZdx_r(i  ,j,k2),0.)
                cff3=max(dZdx_r(i-1,j,k2),0.)
                cff4=max(dZdx_r(i  ,j,k1),0.)
                cff5=min(dZde_r(i-1,j,k1),0.)
                cff6=min(dZde_r(i  ,j,k2),0.)
                cff7=max(dZde_r(i-1,j,k2),0.)
                cff8=max(dZde_r(i  ,j,k1),0.)
 
                cff=om_u(i,j)*( cff1*(cff1*dnUdz-dnUdx(i-1,j,k1))
     &                         +cff2*(cff2*dnUdz-dnUdx(i  ,j,k2))
     &                         +cff3*(cff3*dnUdz-dnUdx(i-1,j,k2))
     &                         +cff4*(cff4*dnUdz-dnUdx(i  ,j,k1))
     &                                                          )
     &             -om_u(i,j)*( cff1*(cff5*dmVdz-dmVde(i-1,j,k1))
     &                         +cff2*(cff6*dmVdz-dmVde(i  ,j,k2))
     &                         +cff3*(cff7*dmVdz-dmVde(i-1,j,k2))
     &                         +cff4*(cff8*dmVdz-dmVde(i  ,j,k1))
     &                                                          )
                cff1=min(dZde_p(i,j  ,k1),0.)
                cff2=min(dZde_p(i,j+1,k2),0.)
                cff3=max(dZde_p(i,j  ,k2),0.)
                cff4=max(dZde_p(i,j+1,k1),0.)
                cff5=min(dZdx_p(i,j  ,k1),0.)
                cff6=min(dZdx_p(i,j+1,k2),0.)
                cff7=max(dZdx_p(i,j  ,k2),0.)
                cff8=max(dZdx_p(i,j+1,k1),0.)
 
                cff=cff + om_u(i,j)*(
     &                          cff1*(cff1*dmUdz-dmUde(i,j  ,k1))
     &                         +cff2*(cff2*dmUdz-dmUde(i,j+1,k2))
     &                         +cff3*(cff3*dmUdz-dmUde(i,j  ,k2))
     &                         +cff4*(cff4*dmUdz-dmUde(i,j+1,k1))
     &                                                          )
     &             +om_u(i,j)*( cff1*(cff5*dnVdz-dnVdx(i,j  ,k1))
     &                         +cff2*(cff6*dnVdz-dnVdx(i,j+1,k2))
     &                         +cff3*(cff7*dnVdz-dnVdx(i,j  ,k2))
     &                         +cff4*(cff8*dnVdz-dnVdx(i,j+1,k1))
     &                                                          )

                UFs(i,j,k2)=0.25*cff*max(0.,visc2_r(i,j)+visc2_r(i-1,j)
#ifdef SPONGE_VIS2
     &                       ,visc2_sponge_r(i,j)+visc2_sponge_r(i-1,j)
#endif
#ifdef UV_VIS_SMAGO
     &                               ,visc3d_r(i,j,k)+visc3d_r(i-1,j,k)
#endif
     &                                  )
              enddo
            enddo
 
            do j=JstrV,Jend
              do i=Istr,Iend
                cff1=pn_v(i,j)
                cff2=pm_v(i,j)
                cff=0.25*( dUdz(i,j,k2)+dUdz(i+1,j,k2)
     &                +dUdz(i,j-1,k2)+dUdz(i+1,j-1,k2))
                dnUdz=cff1*cff
                dmUdz=cff2*cff
                dnVdz=cff1*dVdz(i,j,k2)
                dmVdz=cff2*dVdz(i,j,k2)
 
                cff1=min(dZdx_p(i  ,j,k1),0.)
                cff2=min(dZdx_p(i+1,j,k2),0.)
                cff3=max(dZdx_p(i  ,j,k2),0.)
                cff4=max(dZdx_p(i+1,j,k1),0.)
                cff5=min(dZde_p(i  ,j,k1),0.)
                cff6=min(dZde_p(i+1,j,k2),0.)
                cff7=max(dZde_p(i  ,j,k2),0.)
                cff8=max(dZde_p(i+1,j,k1),0.)
 
                cff=om_v(i,j)*( cff1*(cff1*dnVdz-dnVdx(i  ,j,k1))
     &                         +cff2*(cff2*dnVdz-dnVdx(i+1,j,k2))
     &                         +cff3*(cff3*dnVdz-dnVdx(i  ,j,k2))
     &                         +cff4*(cff4*dnVdz-dnVdx(i+1,j,k1))
     &                                                          )
     &             +om_v(i,j)*( cff1*(cff5*dmUdz-dmUde(i  ,j,k1))
     &                         +cff2*(cff6*dmUdz-dmUde(i+1,j,k2))
     &                         +cff3*(cff7*dmUdz-dmUde(i  ,j,k2))
     &                         +cff4*(cff8*dmUdz-dmUde(i+1,j,k1))
     &                                                          )
                cff1=min(dZde_r(i,j-1,k1),0.)
                cff2=min(dZde_r(i,j  ,k2),0.)
                cff3=max(dZde_r(i,j-1,k2),0.)
                cff4=max(dZde_r(i,j  ,k1),0.)
                cff5=min(dZdx_r(i,j-1,k1),0.)
                cff6=min(dZdx_r(i,j  ,k2),0.)
                cff7=max(dZdx_r(i,j-1,k2),0.)
                cff8=max(dZdx_r(i,j  ,k1),0.)
 
                cff=cff+om_v(i,j)*(
     &                          cff1*(cff1*dmVdz-dmVde(i,j-1,k1))
     &                         +cff2*(cff2*dmVdz-dmVde(i,j  ,k2))
     &                         +cff3*(cff3*dmVdz-dmVde(i,j-1,k2))
     &                         +cff4*(cff4*dmVdz-dmVde(i,j  ,k1))
     &                                                          )
     &             -om_v(i,j)*( cff1*(cff5*dnUdz-dnUdx(i,j-1,k1))
     &                         +cff2*(cff6*dnUdz-dnUdx(i,j  ,k2))
     &                         +cff3*(cff7*dnUdz-dnUdx(i,j-1,k2))
     &                         +cff4*(cff8*dnUdz-dnUdx(i,j  ,k1))
     &                                                          )
 
                VFs(i,j,k2)=0.25*cff*max(0.,visc2_r(i,j)+visc2_r(i,j-1)
#ifdef SPONGE_VIS2
     &                       ,visc2_sponge_r(i,j)+visc2_sponge_r(i,j-1)
#endif
#ifdef UV_VIS_SMAGO
     &                               ,visc3d_r(i,j,k)+visc3d_r(i,j-1,k)
#endif
     &                                  )
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
              rufrc(i,j)=rufrc(i,j) + cff
              u(i,j,k,indx)=u(i,j,k,indx) + dt*(cff1+UFs(i,j,k2)
     &                                              -UFs(i,j,k1))
#ifdef DIAGNOSTICS_UV
              MHmix(i,j,k,1) = 2*(cff1+UFs(i,j,k2)-UFs(i,j,k1))
     &                                   /(Hz(i-1,j,k)+Hz(i,j,k))
     &                                   SWITCH umask(i,j)
#endif
            enddo
          enddo
          do j=JstrV,Jend
            do i=Istr,Iend
              cff=pn_v(i,j)*(VFx(i+1,j)-VFx(i,j))
     &           -pm_v(i,j)*(VFe(i,j)-VFe(i,j-1))
              cff1=pm_v(i,j)*pn_v(i,j)*cff 
              rvfrc(i,j)=rvfrc(i,j) + cff
              v(i,j,k,indx)=v(i,j,k,indx) + dt*(cff1+VFs(i,j,k2)
     &                                              -VFs(i,j,k1))
#ifdef DIAGNOSTICS_UV
              MHmix(i,j,k,2) = 2*(cff1+VFs(i,j,k2)-VFs(i,j,k1))
     &                                   /(Hz(i,j,k)+Hz(i,j-1,k))
     &                                   SWITCH vmask(i,j)
#endif
            enddo
          enddo
        endif
      enddo
!
      return
      end
!
#ifndef CHILD_SPG
# undef UCLM VCLM
# define CHILD_SPG
# ifdef AGRIF
#  include "uv3dmix_GP.F"
# endif
# undef CHILD_SPG
#endif  /* !CHILD_SPG */ 

