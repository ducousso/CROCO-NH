! $Id: uv3dmix_spg.F 1458 2014-02-03 15:01:25Z gcambon $
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
#if defined SOLVE3D && defined SPONGE_VIS2
!
# define UV_MIX_S_SPONGE
!
# if defined M3CLIMATOLOGY
#  define CLIMAT_UV_SPONGE
# endif
!
!
#ifdef MASKING
# define SWITCH *
#else
# define SWITCH !
#endif
!
# ifndef CHILD_SPG
      subroutine uv3dmix_spg (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
#  include "param.h"
#  include "private_scratch.h"
#  include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
#  ifdef AGRIF 
      if (AGRIF_Root()) then
      call uv3dmix_spg_tile (Istr,Iend,Jstr,Jend, 
     &                        A2d(1,1,trd), A2d(1,2,trd),
     &                        A2d(1,3,trd), A2d(1,4,trd),
     &                        A2d(1, 5,trd), A2d(1, 7,trd), 
     &                        A2d(1, 9,trd), A2d(1,11,trd),
     &                        A2d(1,13,trd), A2d(1,15,trd), 
     &                        A2d(1,17,trd), A2d(1,19,trd),
     &                        A2d(1,21,trd), A2d(1,23,trd), 
     &                        A2d(1,25,trd), A2d(1,27,trd))
      else
      call uv3dmix_spg_child_tile (Istr,Iend,Jstr,Jend, 
     &                        A2d(1,1,trd), A2d(1,2,trd),
     &                        A2d(1,3,trd), A2d(1,4,trd),
     &                        A2d(1, 5,trd), A2d(1, 7,trd), 
     &                        A2d(1, 9,trd), A2d(1,11,trd),
     &                        A2d(1,13,trd), A2d(1,15,trd), 
     &                        A2d(1,17,trd), A2d(1,19,trd),
     &                        A2d(1,21,trd), A2d(1,23,trd), 
     &                        A2d(1,25,trd), A2d(1,27,trd))
       endif
      return
      end
#  else
      call uv3dmix_spg_tile (Istr,Iend,Jstr,Jend, 
     &                        A2d(1,1,trd), A2d(1,2,trd),
     &                        A2d(1,3,trd), A2d(1,4,trd),   
     &                        A2d(1, 5,trd), A2d(1, 7,trd), 
     &                        A2d(1, 9,trd), A2d(1,11,trd),
     &                        A2d(1,13,trd), A2d(1,15,trd), 
     &                        A2d(1,17,trd), A2d(1,19,trd),
     &                        A2d(1,21,trd), A2d(1,23,trd), 
     &                        A2d(1,25,trd), A2d(1,27,trd))
       return
       end
#  endif   /* AGRIF */

!
!---------------------------------------------------------------------
!*********************************************************************
!---------------------------------------------------------------------
!
!PARENT
!
      subroutine uv3dmix_spg_tile (Istr,Iend,Jstr,Jend, 
     &                             UFx,UFe,VFx,VFe,
     &                             UFs,VFs, dnUdx,  dmUde,
     &                             dUdz,    dnVdx,   dmVde,  dVdz,
     &                             dZdx_r,  dZdx_p,  dZde_r, dZde_p)

#  undef CLIMAT_UV_SPONGE_FINE
!
! Compute laplacien diffusion in the parent grid sponge. 
# else
!
! CHILD
!
      subroutine uv3dmix_spg_child_tile(Istr,Iend,Jstr,Jend,
     &                             UFx,UFe,VFx,VFe,
     &                             UFs,VFs, dnUdx,  dmUde,
     &                             dUdz,    dnVdx,   dmVde,  dVdz,
     &                             dZdx_r,  dZdx_p,  dZde_r, dZde_p)

! Compute laplacien diffusion in the child sponge using 
! u3dmix_fine.F. Diffusion always applied on U-UCLM in fine grids 
! (cpp keys :CLIMAT_UV_SPONGE_FINE)
#  define CLIMAT_UV_SPONGE_FINE
        
# endif /* CHILD_SPG */
!
!---------------------------------------------------------------------
! ******************************Common Code***************************
!---------------------------------------------------------------------
!
!
!--------------------------------------------------------------------
!
! Computes harmonic mixing of momentum as horizontal divergence of the 
! stress tensor. Components of the stress tensor are:
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
!--------------------------------------------------------------------
!

      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k, k1, k2,indx
      real UFe(PRIVATE_2D_SCRATCH_ARRAY),
     &     UFx(PRIVATE_2D_SCRATCH_ARRAY),     cff,
     &     VFe(PRIVATE_2D_SCRATCH_ARRAY),     cff1,
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


# include "param.h"
# include "scalars.h"
# include "grid.h"
# include "ocean3d.h"
# include "coupling.h"
# include "mixing.h"
# ifdef  CLIMAT_UV_SPONGE
#  include "climat.h"
# endif
# ifdef DIAGNOSTICS_UV
#  include "diagnostics.h"
# endif
# ifdef AGRIF
#  include "zoom.h"
# endif
!
# include "compute_auxiliary_bounds.h"
!
# ifdef CHILD_SPG
#  define UCLM usponge
#  define VCLM vsponge
# else
#  define UCLM uclm
#  define VCLM vclm
# endif

      indx=3-nstp    !--> time index for target arrays;
!
# ifdef UV_MIX_S_SPONGE
!
!====================================================================
!
! Compute horizontal harmonic viscosity along S-surfaces
! in SPONGE layers
!
!====================================================================
!
!
!  Compute flux-components of the horizontal divergence of the stress
!  tensor (m5/s2) in XI- and ETA-directions.
!
      do k=1,N
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            cff=Hz(i,j,k)*visc2_sponge_r(i,j)*
#  if defined CLIMAT_UV_SPONGE || defined CLIMAT_UV_SPONGE_FINE
     &    ( pmon_r(i,j)*
     &        ( pn_u(i+1,j)*(u(i+1,j,k,nstp)-UCLM(i+1,j,k))
     &         -pn_u(i  ,j)*(u(i  ,j,k,nstp)-UCLM(i  ,j,k)) )
     &     -pnom_r(i,j)*
     &        ( pm_v(i,j+1)*(v(i,j+1,k,nstp)-VCLM(i,j+1,k))
     &         -pm_v(i,j  )*(v(i,j  ,k,nstp)-VCLM(i,j  ,k)) ) )
#  else
     &    ( pmon_r(i,j)*( pn_u(i+1,j)*u(i+1,j,k,nstp)
     &                   -pn_u(i  ,j)*u(i  ,j,k,nstp) )
     &     -pnom_r(i,j)*( pm_v(i,j+1)*v(i,j+1,k,nstp)
     &                   -pm_v(i,j  )*v(i,j  ,k,nstp) ) )
#  endif
            UFx(i,j)=on_r(i,j)*on_r(i,j)*cff
            VFe(i,j)=om_r(i,j)*om_r(i,j)*cff
          enddo
        enddo
        do j=Jstr,Jend+1
          do i=Istr,Iend+1
            cff=0.25*visc2_sponge_p(i,j)*
     &              (Hz(i-1,j,k)+Hz(i,j,k)+Hz(i-1,j-1,k)+Hz(i,j-1,k))*
#  if defined CLIMAT_UV_SPONGE || defined CLIMAT_UV_SPONGE_FINE
     &      (  pmon_p(i,j)
     &              *( pn_v(i  ,j)*(v(i  ,j,k,nstp)-VCLM(i  ,j,k))
     &                -pn_v(i-1,j)*(v(i-1,j,k,nstp)-VCLM(i-1,j,k)) )
     &        +pnom_p(i,j)
     &              *( pm_u(i,j  )*(u(i,j  ,k,nstp)-UCLM(i,j  ,k))
     &                -pm_u(i,j-1)*(u(i,j-1,k,nstp)-UCLM(i,j-1,k)) ) )
#  else
     &      (  pmon_p(i,j)
     &                     *( pn_v(i  ,j)*v(i  ,j,k,nstp)
     &                       -pn_v(i-1,j)*v(i-1,j,k,nstp) )
     &        +pnom_p(i,j)
     &                     *( pm_u(i,j  )*u(i,j  ,k,nstp)
     &                       -pm_u(i,j-1)*u(i,j-1,k,nstp) ) ) 
#  endif
#  ifdef MASKING
     &                                               * pmask(i,j)
#  endif
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
#  ifdef DIAGNOSTICS_UV
            MHmix(i,j,k,1) = 2*cff1/(Hz(i-1,j,k)+Hz(i,j,k))
     &                       SWITCH umask(i,j)
#   ifdef UV_VIS4
     &                     + MHmix(i,j,k,1)
#   endif
#  endif
          enddo
        enddo
        do j=JstrV,Jend
          do i=Istr,Iend
            cff=pn_v(i,j)*(VFx(i+1,j)-VFx(i,j))
     &         -pm_v(i,j)*(VFe(i,j)-VFe(i,j-1))
            cff1=pm_v(i,j)*pn_v(i,j)*cff 
            rvfrc(i,j)=rvfrc(i,j) + cff
            v(i,j,k,indx)=v(i,j,k,indx) + dt*cff1
#  ifdef DIAGNOSTICS_UV
            MHmix(i,j,k,2) = 2*cff1/(Hz(i,j,k)+Hz(i,j-1,k))
     &                       SWITCH vmask(i,j)
#   ifdef UV_VIS4
     &                     + MHmix(i,j,k,2)
#   endif
#  endif
          enddo
        enddo
      enddo
 
# else  /* UV_MIX_S_SPONGE */
!
!====================================================================
!
! Compute horizontal harmonic viscosity along geopotential surfaces
! in SPONGE layers
!
!====================================================================
!
      k2=1
      do k=0,N,+1  !--> irreversible
        k1=k2
        k2=3-k1
        if (k.lt.N) then
          do j=Jstr-1,Jend+1
            do i=IstrU-1,Iend+1
              UFx(i,j)=(z_r(i,j,k+1)-z_r(i-1,j,k+1))*pm_u(i,j)
#  ifdef MASKING
     &                                              * umask(i,j)
#  endif
            enddo
          enddo
          do j=JstrV-1,Jend+1
            do i=Istr-1,Iend+1
              VFe(i,j)=(z_r(i,j,k+1)-z_r(i,j-1,k+1))*pn_v(i,j)
#  ifdef MASKING
     &                                             * vmask(i,j)
#  endif
            enddo
          enddo
          do j=JstrV-1,Jend
            do i=IstrU-1,Iend
              dnUdx(i,j,k2)=pm(i,j)*
#  if defined CLIMAT_UV_SPONGE || defined CLIMAT_UV_SPONGE_FINE
     &        ( pn_u(i+1,j)*(u(i+1,j,k+1,nstp)-UCLM(i+1,j,k+1))
     &         -pn_u(i  ,j)*(u(i  ,j,k+1,nstp)-UCLM(i  ,j,k+1)) )
#   ifdef MASKING
     &                                             * rmask(i,j)
#   endif
#  else
     &              (pn_u(i+1,j)*u(i+1,j,k+1,nstp)
     &              -pn_u(i  ,j)*u(i  ,j,k+1,nstp))
#  ifdef MASKING
     &                                             * rmask(i,j)
#  endif
#  endif
              dmVde(i,j,k2)=pn(i,j)*
#  if defined CLIMAT_UV_SPONGE || defined CLIMAT_UV_SPONGE_FINE
     &        ( pm_v(i,j+1)*(v(i,j+1,k+1,nstp)-VCLM(i,j+1,k+1))
     &         -pm_v(i,j  )*(v(i,j  ,k+1,nstp)-VCLM(i,j  ,k+1)) )
#   ifdef MASKING
     &                                              * rmask(i,j)
#   endif
#  else
     &              (pm_v(i,j+1)*v(i,j+1,k+1,nstp)
     &              -pm_v(i,j  )*v(i,j  ,k+1,nstp))
#   ifdef MASKING
     &                                              * rmask(i,j)
#   endif
#  endif
              dZdx_r(i,j,k2)=0.5*(UFx(i,j)+UFx(i+1,j))
              dZde_r(i,j,k2)=0.5*(VFe(i,j)+VFe(i,j+1))
            enddo
          enddo
          do j=Jstr,Jend+1
            do i=Istr,Iend+1
              dmUde(i,j,k2)=0.25*(pn(i,j)+pn(i-1,j)+pn(i,j-1)
     &                                            +pn(i-1,j-1))
#  if defined CLIMAT_UV_SPONGE || defined CLIMAT_UV_SPONGE_FINE
     &       *( pm_u(i,j  )*(u(i,j  ,k+1,nstp)-UCLM(i,j,k+1))
     &         -pm_u(i,j-1)*(u(i,j-1,k+1,nstp)-UCLM(i,j-1,k+1)) )
#   ifdef MASKING
     &                                              * pmask(i,j)
#   endif
#  else
     &             *(pm_u(i,j  )*u(i,j  ,k+1,nstp)
     &              -pm_u(i,j-1)*u(i,j-1,k+1,nstp))
#   ifdef MASKING
     &                                              * pmask(i,j)
#   endif
#  endif
              dnVdx(i,j,k2)=0.25*(pm(i,j)+pm(i-1,j)+pm(i,j-1)
     &                                            +pm(i-1,j-1))
#  if defined CLIMAT_UV_SPONGE || defined CLIMAT_UV_SPONGE_FINE
     &       *( pn_v(i  ,j)*(v(i  ,j,k+1,nstp)-VCLM(i  ,j,k+1))
     &         -pn_v(i-1,j)*(v(i-1,j,k+1,nstp)-VCLM(i-1,j,k+1)) )
#   ifdef MASKING
     &                                              * pmask(i,j)
#   endif
#  else
     &             *(pn_v(i  ,j)*v(i  ,j,k+1,nstp)
     &              -pn_v(i-1,j)*v(i-1,j,k+1,nstp))
#   ifdef MASKING
     &                                              * pmask(i,j)
#   endif
#  endif
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
#  if defined CLIMAT_UV_SPONGE || defined CLIMAT_UV_SPONGE_FINE
              dUdz(i,j,k2)=2.*(u(i,j,k+1,nstp)-u(i,j,k,nstp)
     &                            -UCLM(i,j,k+1)+UCLM(i,j,k))
#  else
              dUdz(i,j,k2)=2.*(u(i,j,k+1,nstp)-u(i,j,k,nstp))
#  endif
     &                          /( z_r(i-1,j,k+1)-z_r(i-1,j,k)
     &                            +z_r(i  ,j,k+1)-z_r(i  ,j,k))
            enddo
          enddo
          do j=JstrV-1,Jend+1
            do i=Istr-1,Iend+1
#  if defined CLIMAT_UV_SPONGE || defined CLIMAT_UV_SPONGE_FINE
              dVdz(i,j,k2)=2.*(v(i,j,k+1,nstp)-v(i,j,k,nstp)
     &                            -VCLM(i,j,k+1)+VCLM(i,j,k))
#  else
              dVdz(i,j,k2)=2.*(v(i,j,k+1,nstp)-v(i,j,k,nstp))
#  endif
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
              cff=visc2_sponge_r(i,j)*Hz(i,j,k)*(
     &         om_r(i,j)*( dnUdx(i,j,k1) - 0.5*pn(i,j)*(
     &           min(dZdx_r(i,j,k1),0.)*(dUdz(i,j,k1)+dUdz(i+1,j,k2))
     &          +max(dZdx_r(i,j,k1),0.)*(dUdz(i,j,k2)+dUdz(i+1,j,k1))
     &                                                             ))
     &        -om_r(i,j)*( dmVde(i,j,k1) - 0.5*pm(i,j)*(
     &           min(dZde_r(i,j,k1),0.)*(dVdz(i,j,k1)+dVdz(i,j+1,k2))
     &          +max(dZde_r(i,j,k1),0.)*(dVdz(i,j,k2)+dVdz(i,j+1,k1))
     &                                                            )))
#  ifdef MASKING
     &                                          * rmask(i,j)
#  endif

              UFx(i,j)=om_r(i,j)*om_r(i,j)*cff
              VFe(i,j)=om_r(i,j)*om_r(i,j)*cff
            enddo
          enddo
          do j=Jstr,Jend+1
            do i=Istr,Iend+1
              cff=0.25*visc2_sponge_p(i,j)
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
#  ifdef MASKING
     &                                        * pmask(i,j)
#  endif
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
                cff=0.25*( dVdz(i,j,k2)+dVdz(i-1,j,k2)
     &                +dVdz(i,j+1,k2)+dVdz(i-1,j+1,k2))
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
                UFs(i,j,k2)=0.25*( visc2_sponge_r(i-1,j)
     &                            +visc2_sponge_r(i,j)  )*cff
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
                VFs(i,j,k2)=0.25*( visc2_sponge_r(i,j-1)
     &                            +visc2_sponge_r(i,j)  )*cff
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
#  ifdef DIAGNOSTICS_UV
              MHmix(i,j,k,1) = 2*(cff1+UFs(i,j,k2)-UFs(i,j,k1))
     &                         /(Hz(i-1,j,k)+Hz(i,j,k))
     &                          SWITCH umask(i,j)
#   ifdef UV_VIS4
     &                        + MHmix(i,j,k,1)
#   endif
#  endif
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
#  ifdef DIAGNOSTICS_UV
              MHmix(i,j,k,2) = 2*(cff1+VFs(i,j,k2)-VFs(i,j,k1))
     &                         /(Hz(i,j,k)+Hz(i,j-1,k))
     &                          SWITCH vmask(i,j)
#   ifdef UV_VIS4
     &                        + MHmix(i,j,k,2)
#   endif
#  endif
            enddo
          enddo
        endif
      enddo
# endif /* UV_MIX_S_SPONGE */

# ifdef NBQ
!==================================================================
!
! WZ horizontal Laplacian diffusion along constant S-surfaces.
!
!==================================================================
#  define FX UFe
#  define FE VFe

      do k=1,N-1
!
!  Compute XI- and ETA-components of diffusive wz flux.
!
        do j=Jstr,Jend
          do i=Istr,Iend+1
          FX(i,j)=0.125*(visc2_sponge_r(i,j)+visc2_sponge_r(i-1,j))
     &                    *pmon_u(i,j)*(Hz(i,j,k  )+Hz(i-1,j,k  )+
     &                                  Hz(i,j,k+1)+Hz(i-1,j,k+1))
     &                          *(wz(i,j,k,nstp)-wz(i-1,j,k,nstp))
     &                                           SWITCH umask(i,j)
          enddo
        enddo
        do j=Jstr,Jend+1
          do i=Istr,Iend
          FE(i,j)=0.125*(visc2_sponge_r(i,j)+visc2_sponge_r(i,j-1))
     &                    *pnom_v(i,j)*(Hz(i,j,k  )+Hz(i,j-1,k  )+
     &                                  Hz(i,j,k+1)+Hz(i,j-1,k+1))
     &                          *(wz(i,j,k,nstp)-wz(i,j-1,k,nstp))
     &                                           SWITCH vmask(i,j)
          enddo
        enddo
!
!  Apply viscous term. Note that at this stage arrays wz(...,3-nstp)
!  contains Hz*W with units of [m2/s].
!
        do j=Jstr,Jend
          do i=Istr,Iend
            cff1=pm(i,j)*pn(i,j)
            wz(i,j,k,indx)=wz(i,j,k,indx)+dt*cff1*(FX(i+1,j)-FX(i,j)
     &                                           + FE(i,j+1)-FE(i,j))
          enddo
        enddo
      enddo ! 1,N-1
!
!  Repeat computation for top layer N
!
      do j=Jstr,Jend
        do i=Istr,Iend+1
        FX(i,j)=0.25*(visc2_sponge_r(i,j)+visc2_sponge_r(i-1,j))
     &                  *pmon_u(i,j)*(Hz(i,j,N  )+Hz(i-1,j,N  ))
     &                        *(wz(i,j,N,nrhs)-wz(i-1,j,N,nrhs))
     &                                         SWITCH umask(i,j)
        enddo
      enddo
      do j=Jstr,Jend+1
        do i=Istr,Iend
        FE(i,j)=0.25*(visc2_sponge_r(i,j)+visc2_sponge_r(i,j-1))
     &                  *pnom_v(i,j)*(Hz(i,j,N  )+Hz(i,j-1,N  ))
     &                        *(wz(i,j,k,nstp)-wz(i,j-1,k,nstp))
     &                                         SWITCH vmask(i,j)
        enddo
      enddo
      do j=Jstr,Jend
        do i=Istr,Iend
          cff1=pm(i,j)*pn(i,j)
          wz(i,j,N,indx)=wz(i,j,N,indx)+dt*cff1*(FX(i+1,j)-FX(i,j)
     &                                         + FE(i,j+1)-FE(i,j))
        enddo
      enddo
#  undef FX 
#  undef FE
# endif /* NBQ */


      return
      end

# ifndef CHILD_SPG
#  undef UCLM 
#  undef VCLM
#  define CHILD_SPG
#  ifdef AGRIF
#   include "uv3dmix_spg.F"
#  endif
#  undef CHILD_SPG
# endif  /* !CHILD_SPG */

#else
      subroutine uv3dmix_spg_empty
      end
#endif  /* SOLVE3D && SPONGE_VIS2 */
