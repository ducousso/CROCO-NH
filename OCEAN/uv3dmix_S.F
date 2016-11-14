! $Id: uv3dmix_S.F 1547 2014-06-13 09:31:52Z penven $
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
     &                        A2d(1,1,trd), A2d(1,2,trd),
     &                        A2d(1,3,trd), A2d(1,4,trd))
       else
        call uv3dmix_child_tile (Istr,Iend,Jstr,Jend, 
     &                        A2d(1,1,trd), A2d(1,2,trd),
     &                        A2d(1,3,trd), A2d(1,4,trd))
       endif 
      return
      end
# else 
      call uv3dmix_tile (Istr,Iend,Jstr,Jend, 
     &                        A2d(1,1,trd), A2d(1,2,trd),
     &                        A2d(1,3,trd), A2d(1,4,trd))
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
      subroutine uv3dmix_tile (Istr,Iend,Jstr,Jend, 
     &                                       UFx,UFe,VFx,VFe)
# undef CLIMAT_UV_MIXH_FINE
!
#else
!
! CHILD
!
      subroutine uv3dmix_child_tile (Istr,Iend,Jstr,Jend, 
     &                                         UFx,UFe,VFx,VFe)
!
! Diffusion always applied on U-UCLM in fine grid
# if !defined UV_HADV_RSUP3
#  undef CLIMAT_UV_MIXH_FINE
# endif
!
#endif /* CHILD_SPG */
!
!--------------------------------------------------------------------
! Computes harmonic mixing of momentum, along constant S-surfaces 
! as horizontal divergence of the stress tensor. Components of the
! stress tensor are:
!                         du     dv
!         s_xx = -s_yy = ---- - -----
!                         dx     dy
!
!                         du     dv
!         s_xy =  s_yx = ---- + ----
!                         dy     dx
!
! Reference:
!
! Wajsowicz, R.C, 1993: A consistent formulation of the anisotropic
!     stress tensor for use in models of the large-scale ocean
!     circulation, JCP, 105, 333-338.
!
! Sadourny, R. and K. Maynard, 1997: Formulations of lateral
!     diffusion in geophysical fluid dynamics models, In "Numerical
!     Methods of Atmospheric and Oceanic Modelling". Lin, Laprise,
!     and Ritchie, Eds., NRC Research Press, 547-556.
!
! Griffies, S.M. and R.W. Hallberg, 2000: Biharmonic friction with
!     a Smagorinsky-like viscosity for use in large-scale eddy-
!     permitting ocean models, Mon. Wea. Rev., 128, 8, 2935-2946.
!---------------------------------------------------------------------
!---------------------------------------------------------------------
! ******************************Common Code***************************
!---------------------------------------------------------------------
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k, indx
      real UFe(PRIVATE_2D_SCRATCH_ARRAY),
     &     UFx(PRIVATE_2D_SCRATCH_ARRAY), cff,
     &     VFe(PRIVATE_2D_SCRATCH_ARRAY), cff1,
     &     VFx(PRIVATE_2D_SCRATCH_ARRAY)
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
#ifdef NBQ
      integer kp
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
!  Compute flux-components of the horizontal divergence of the stress
!  tensor (m5/s2) in XI- and ETA-directions.
!
      do k=1,N
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            cff=Hz(i,j,k)*max(0.,visc2_r(i,j)
#ifdef SPONGE_VIS2
     &                          ,visc2_sponge_r(i,j)
#endif
#ifdef UV_VIS_SMAGO
     &                          ,visc3d_r(i,j,k)
#endif
     &                       )*
#if defined CLIMAT_UV_MIXH || defined CLIMAT_UV_MIXH_FINE
     &    ( pmon_r(i,j)*
     &        ( pn_u(i+1,j)*(u(i+1,j,k,nstp)-UCLM(i+1,j,k))
     &         -pn_u(i  ,j)*(u(i  ,j,k,nstp)-UCLM(i  ,j,k)) )
     &     -pnom_r(i,j)*
     &        ( pm_v(i,j+1)*(v(i,j+1,k,nstp)-VCLM(i,j+1,k))
     &         -pm_v(i,j  )*(v(i,j  ,k,nstp)-VCLM(i,j  ,k)) ) )
#else
     &    ( pmon_r(i,j)*( pn_u(i+1,j)*u(i+1,j,k,nstp)
     &                   -pn_u(i  ,j)*u(i  ,j,k,nstp) )
     &     -pnom_r(i,j)*( pm_v(i,j+1)*v(i,j+1,k,nstp)
     &                   -pm_v(i,j  )*v(i,j  ,k,nstp) ) )
#endif
            UFx(i,j)=on_r(i,j)*on_r(i,j)*cff
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
     &                    )*
     &             (Hz(i-1,j,k)+Hz(i,j,k)+Hz(i-1,j-1,k)+Hz(i,j-1,k))*
#if defined CLIMAT_UV_MIXH || defined CLIMAT_UV_MIXH_FINE
     &      (  pmon_p(i,j)
     &              *( pn_v(i  ,j)*(v(i  ,j,k,nstp)-VCLM(i  ,j,k))
     &                -pn_v(i-1,j)*(v(i-1,j,k,nstp)-VCLM(i-1,j,k)) )
     &        +pnom_p(i,j)
     &              *( pm_u(i,j  )*(u(i,j  ,k,nstp)-UCLM(i  ,j,k))
     &                -pm_u(i,j-1)*(u(i,j-1,k,nstp)-UCLM(i,j-1,k)) ))
#else
     &      (  pmon_p(i,j)
     &                     *( pn_v(i  ,j)*v(i  ,j,k,nstp)
     &                       -pn_v(i-1,j)*v(i-1,j,k,nstp) )
     &        +pnom_p(i,j)
     &                     *( pm_u(i,j  )*u(i,j  ,k,nstp)
     &                       -pm_u(i,j-1)*u(i,j-1,k,nstp) ))
#endif
#ifdef MASKING
     &                                            * pmask(i,j)
#endif
!#ifdef WET_DRY
!     &                                            * pmask_wet(i,j)
!#endif
            UFe(i,j)=om_p(i,j)*om_p(i,j)*cff
            VFx(i,j)=on_p(i,j)*on_p(i,j)*cff
          enddo
        enddo
!
! Apply viscous terms. Note that at this stage arrays u,v(...,3-nstp)
! contain Hz*U and Hz*V with units of [m2/s].   Also compute vertical
! integral of viscous terms and add it into coupling terms for the
! barotropic mode
!
        do j=Jstr,Jend
          do i=IstrU,Iend
            cff=pn_u(i,j)*(UFx(i,j)-UFx(i-1,j))
     &         +pm_u(i,j)*(UFe(i,j+1)-UFe(i,j))
            cff1=pm_u(i,j)*pn_u(i,j)*cff
            rufrc(i,j)=rufrc(i,j) + cff
            u(i,j,k,indx)=u(i,j,k,indx) + dt*cff1
#ifdef DIAGNOSTICS_UV
            MHmix(i,j,k,1) = 2*cff1/(Hz(i-1,j,k)+Hz(i,j,k)) 
     &                       SWITCH umask(i,j)
#endif
          enddo
        enddo
        do j=JstrV,Jend
          do i=Istr,Iend
            cff=pn_v(i,j)*(VFx(i+1,j)-VFx(i,j))
     &         -pm_v(i,j)*(VFe(i,j)-VFe(i,j-1))
            cff1=pm_v(i,j)*pn_v(i,j)*cff 
            rvfrc(i,j)=rvfrc(i,j) + cff
            v(i,j,k,indx)=v(i,j,k,indx) + dt*cff1
#ifdef DIAGNOSTICS_UV
            MHmix(i,j,k,2) = 2*cff1/(Hz(i,j-1,k)+Hz(i,j,k))
     &                       SWITCH vmask(i,j)
#endif
          enddo
        enddo
      enddo

#ifdef NBQ
!
!  Compute XI- and ETA-components of diffusive wz flux.
!
# define FX UFx
# define FE UFe
      do k=1,N
        kp=min(k+1,N)
        do j=jstr,jend
          do i=istr,iend+1
              FX(i,j)=0.25*
     &                max(0.,visc2_r(i,j)+visc2_r(i-1,j)
#ifdef SPONGE_VIS2
     &                      ,visc2_sponge_r(i,j)+visc2_sponge_r(i-1,j)
#endif
#ifdef UV_VIS_SMAGO
     &                      ,0.5*(visc3d_r(i,j,k )+visc3d_r(i-1,j,k )
     &                           +visc3d_r(i,j,kp)+visc3d_r(i-1,j,kp))
#endif
     &                       )
     &                   *pmon_u(i,j)*(Hz(i,j,k )+Hz(i-1,j,k )+
     &                                 Hz(i,j,kp)+Hz(i-1,j,kp))
     &                       *(wz(i,j,k,nstp)-wz(i-1,j,k,nstp))
     &                                        SWITCH umask(i,j)
          enddo
        enddo
        do j=jstr,jend+1
          do i=istr,iend
              FE(i,j)=0.25*
     &                max(0.,visc2_r(i,j)+visc2_r(i,j-1)
#ifdef SPONGE_VIS2
     &                      ,visc2_sponge_r(i,j)+visc2_sponge_r(i,j-1)
#endif
#ifdef UV_VIS_SMAGO
     &                      ,0.5*(visc3d_r(i,j,k )+visc3d_r(i,j-1,k )
     &                           +visc3d_r(i,j,kp)+visc3d_r(i,j-1,kp))
#endif
     &                       )
     &                   *pnom_v(i,j)*(Hz(i,j,k )+Hz(i,j-1,k )+
     &                                 Hz(i,j,kp)+Hz(i,j-1,kp))
     &                       *(wz(i,j,k,nstp)-wz(i,j-1,k,nstp))
     &                                        SWITCH vmask(i,j)
          enddo
        enddo
!
!  Add in horizontal diffusion of wz
        do j=jstr,jend
          do i=istr,iend
            cff1=pm(i,j)*pn(i,j)
            wz(i,j,k,indx)=wz(i,j,k,indx)+dt*cff1
     &                     *(FX(i+1,j)-FX(i,j)+FE(i,j+1)-FE(i,j))
     &                                    /(Hz(i,j,k)+Hz(i,j,kp))
          enddo
        enddo
      enddo ! <-- k
# undef FX
# undef FE
#endif
!
      return
      end
!
#ifndef CHILD_SPG
# undef UCLM 
# undef VCLM
# define CHILD_SPG
# ifdef AGRIF
#  include "uv3dmix_S.F"
# endif
# undef CHILD_SPG
#endif  /* !CHILD_SPG */
!
