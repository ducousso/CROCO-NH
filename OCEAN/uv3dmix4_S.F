! $Id: uv3dmix4_S.F 1458 2014-02-03 15:01:25Z gcambon $
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
!  This subroutine computes biharmonic mixing of momentum, along     !
!  constant  S-surfaces, from the  horizontal divergence  of the     !
!  stress tensor. A transverse isotropy is assumed so the stress     !
!  tensor is splitted into vertical and horizontal subtensors.       !
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
!  References:                                                       !
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
     &                      A2d(1, 1,trd),A2d(1, 3,trd),A2d(1, 5,trd),
     &                      A2d(1, 7,trd),A2d(1, 9,trd),A2d(1,11,trd))
      else
      call uv3dmix_child_tile (Istr,Iend,Jstr,Jend,
     &                      A2d(1, 1,trd),A2d(1, 3,trd),A2d(1, 5,trd),
     &                      A2d(1, 7,trd),A2d(1, 9,trd),A2d(1,11,trd))


       endif
      return
      end
# else 
      call uv3dmix_tile (Istr,Iend,Jstr,Jend,
     &                      A2d(1, 1,trd),A2d(1, 3,trd),A2d(1, 5,trd),
     &                      A2d(1, 7,trd),A2d(1, 9,trd),A2d(1,11,trd))
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
      subroutine uv3dmix_tile (Istr,Iend,Jstr,Jend,UFx,UFe,LapU,
     &                            VFx,VFe,LapV)
# undef CLIMAT_UV_MIXH_FINE
!
#else
!
! CHILD
!
      subroutine uv3dmix_child_tile (Istr,Iend,Jstr,Jend,UFx,UFe,LapU,
     &                            VFx,VFe,LapV)
!
!Diffusion always applied on U-UCLM in fine grid
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
      integer  Iend, Istr, Jend, Jstr, i, j, k, indx,
     &         imin,imax,jmin,jmax, IminU,JminV
      real     cff,cff1,
     &         LapU(PRIVATE_2D_SCRATCH_ARRAY),
     &         LapV(PRIVATE_2D_SCRATCH_ARRAY),
     &          UFe(PRIVATE_2D_SCRATCH_ARRAY),
     &          UFx(PRIVATE_2D_SCRATCH_ARRAY),
     &          VFe(PRIVATE_2D_SCRATCH_ARRAY),
     &          VFx(PRIVATE_2D_SCRATCH_ARRAY)
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

      do k=1,N
!
!---------------------------------------------------------------------
!  Compute horizontal biharmonic viscosity along constant S-surfaces.
!  The biharmonic operator is computed by applying the harmonic
!  operator twice.
!---------------------------------------------------------------------
!
!  Compute flux-components of the horizontal divergence of the stress
!  tensor (m4 s^-3/2) in XI- and ETA-directions. For momentum balance
!  purposes, the thickness "Hz" appears only when computing the second 
!  harmonic operator.
!                         du     dv
!         s_xx = -s_yy = ---- - -----
!                         dx     dy
!
        do j=jminV-1,jmax
          do i=iminU-1,imax
            cff=pmon_r(i,j)*
#if defined CLIMAT_UV_MIXH || defined CLIMAT_UV_MIXH_FINE
     &           (pn_u(i+1,j)*(u(i+1,j,k,nstp)-UCLM(i+1,j,k))-
     &            pn_u(i  ,j)*(u(i  ,j,k,nstp)-UCLM(i  ,j,k)) ) - 
#else
     &           (pn_u(i+1,j)*u(i+1,j,k,nstp)-
     &            pn_u(i  ,j)*u(i  ,j,k,nstp) ) -
#endif
     &          pnom_r(i,j)*
#if defined CLIMAT_UV_MIXH || defined CLIMAT_UV_MIXH_FINE
     &           (pm_v(i,j+1)*(v(i,j+1,k,nstp)-VCLM(i,j+1,k))-
     &            pm_v(i,j  )*(v(i,j  ,k,nstp)-VCLM(i,j  ,k)) )
#else
     &           (pm_v(i,j+1)*v(i,j+1,k,nstp)-
     &            pm_v(i,j  )*v(i,j  ,k,nstp))
#endif

            UFx(i,j)=on_r(i,j)*on_r(i,j)*cff
            VFe(i,j)=om_r(i,j)*om_r(i,j)*cff
          enddo
        enddo
!                         dv     du
!         s_xy =  s_yx = ---- + ----
!                         dx     dy
        do j=jmin,jmax+1
          do i=imin,imax+1
            cff=pmon_p(i,j)*
#if defined CLIMAT_UV_MIXH || defined CLIMAT_UV_MIXH_FINE
     &           (pn_v(i  ,j)*(v(i  ,j,k,nstp)-VCLM(i  ,j,k)) -
     &            pn_v(i-1,j)*(v(i-1,j,k,nstp)-VCLM(i-1,j,k)) )+
#else
     &           (pn_v(i  ,j)*v(i  ,j,k,nstp)-
     &            pn_v(i-1,j)*v(i-1,j,k,nstp))+
#endif
     &          pnom_p(i,j)*
#if defined CLIMAT_UV_MIXH || defined CLIMAT_UV_MIXH_FINE
     &           (pm_u(i,j  )*( u(i,j  ,k,nstp)-UCLM(i,j  ,k))-
     &            pm_u(i,j-1)*( u(i,j-1,k,nstp)-UCLM(i,j-1,k)) )
#else
     &           (pm_u(i,j  )*u(i,j  ,k,nstp)-
     &            pm_u(i,j-1)*u(i,j-1,k,nstp))
#endif
#ifdef MASKING
     &                               * pmask(i,j)
#endif

            UFe(i,j)=om_p(i,j)*om_p(i,j)*cff
            VFx(i,j)=on_p(i,j)*on_p(i,j)*cff
          enddo
        enddo
!
!  Compute first harmonic operator (m s^-3/2).
!
        do j=jmin,jmax
          do i=iminU,imax
            LapU(i,j)=0.125*
     &                (pm(i-1,j)+pm(i,j))*(pn(i-1,j)+pn(i,j))*
     &                ((pn(i-1,j)+pn(i,j))*(UFx(i,j  )-UFx(i-1,j))+
     &                 (pm(i-1,j)+pm(i,j))*(UFe(i,j+1)-UFe(i  ,j)))
          enddo
        enddo
        do j=jminV,jmax
          do i=imin,imax
            LapV(i,j)=0.125*
     &                (pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))*
     &                ((pn(i,j-1)+pn(i,j))*(VFx(i+1,j)-VFx(i,j  ))-
     &                 (pm(i,j-1)+pm(i,j))*(VFe(i  ,j)-VFe(i,j-1)))
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
            LapU(Istr,j)=0.0
# else
            LapU(Istr,j)=LapU(Istr+1,j)
# endif
          enddo
          do j=jminV,jmax
# ifndef OBC_WEST
            LapV(Istr-1,j)=gamma2*LapV(Istr,j)
# else
            LapV(Istr-1,j)=0.0
# endif
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=jmin,jmax
# ifndef OBC_EAST
            LapU(Iend+1,j)=0.0
# else
            LapU(Iend+1,j)=LapU(Iend,j)
# endif
          enddo
          do j=jminV,jmax
# ifndef OBC_EAST
            LapV(Iend+1,j)=gamma2*LapV(Iend,j)
# else
            LapV(Iend+1,j)=0.0
# endif
          enddo
        endif
#endif /* !EW_PERIODIC */
#ifndef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=iminU,imax
# ifndef OBC_SOUTH
            LapU(i,Jstr-1)=gamma2*LapU(i,Jstr)
# else
            LapU(i,Jstr-1)=0.0
# endif
          enddo
          do i=imin,imax
# ifndef OBC_SOUTH
            LapV(i,Jstr)=0.0
# else
            LapV(i,Jstr)=LapV(i,Jstr+1)
# endif
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=iminU,imax
# ifndef OBC_NORTH
            LapU(i,Jend+1)=gamma2*LapU(i,Jend)
# else
            LapU(i,Jend+1)=0.0
# endif
          enddo
          do i=imin,imax
# ifndef OBC_NORTH
            LapV(i,Jend+1)=0.0
# else
            LapV(i,Jend+1)=LapV(i,Jend)
# endif
          enddo
        endif
#endif /* !NS_PERIODIC */
!
! Corner points
!
#if !defined EW_PERIODIC && !defined NS_PERIODIC
        IF ((SOUTHERN_EDGE).and.(WESTERN_EDGE)) THEN
          LapU(Istr  ,Jstr-1)=0.5*(LapU(Istr+1,Jstr-1)+
     &                             LapU(Istr  ,Jstr  ))
          LapV(Istr-1,Jstr  )=0.5*(LapV(Istr-1,Jstr+1)+
     &                             LapV(Istr  ,Jstr  ))
        END IF
        IF ((SOUTHERN_EDGE).and.(EASTERN_EDGE)) THEN
          LapU(Iend+1,Jstr-1)=0.5*(LapU(Iend  ,Jstr-1)+
     &                             LapU(Iend+1,Jstr  ))
          LapV(Iend+1,Jstr  )=0.5*(LapV(Iend  ,Jstr  )+
     &                             LapV(Iend+1,Jstr+1))
        END IF
        IF ((NORTHERN_EDGE).and.(WESTERN_EDGE)) THEN
          LapU(Istr  ,Jend+1)=0.5*(LapU(Istr+1,Jend+1)+
     &                             LapU(Istr  ,Jend  ))
          LapV(Istr-1,Jend+1)=0.5*(LapV(Istr  ,Jend+1)+
     &                             LapV(Istr-1,Jend  ))
        END IF
        IF ((NORTHERN_EDGE).and.(EASTERN_EDGE)) THEN
          LapU(Iend+1,Jend+1)=0.5*(LapU(Iend  ,Jend+1)+
     &                             LapU(Iend+1,Jend  ))
          LapV(Iend+1,Jend+1)=0.5*(LapV(Iend  ,Jend+1)+
     &                             LapV(Iend+1,Jend  ))
        END IF
#endif
!
!---------------------------------------------------------------------
!  Compute horizontal gradients associated with the
!  second harmonic operator.
!---------------------------------------------------------------------
!
!  Compute flux-components of the horizontal divergence of the
!  harmonic stress tensor (m4/s2) in XI- and ETA-directions.
!
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            cff=Hz(i,j,k)*
     &          (pmon_r(i,j)*
     &           (pn_u(i+1,j)*LapU(i+1,j)-
     &            pn_u(i  ,j)*LapU(i  ,j))-
     &           pnom_r(i,j)*
     &           (pm_v(i,j+1)*LapV(i,j+1)-
     &            pm_v(i,j  )*LapV(i,j  )))
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
     &                Hz(i-1,j-1,k)+Hz(i,j-1,k))*
     &          (pmon_p(i,j)*
     &           (pn_v(i  ,j)*LapV(i  ,j)-
     &            pn_v(i-1,j)*LapV(i-1,j))+
     &           pnom_p(i,j)*
     &           (pm_u(i,j  )*LapU(i,j  )-
     &            pm_u(i,j-1)*LapU(i,j-1)))
#ifdef MASKING
     &                             * pmask(i,j)
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
! Time-step biharmonic, S-surfaces viscosity term.  Notice that
! momentum at this stage is HzU and HzV and has units m2/s.
! Also compute vertical integral of viscous terms and add it 
! into coupling terms for the barotropic mode
!
        do j=Jstr,Jend
          do i=IstrU,Iend
            cff=pn_u(i,j)*(UFx(i,j)-UFx(i-1,j))
     &         +pm_u(i,j)*(UFe(i,j+1)-UFe(i,j))
            cff1=pm_u(i,j)*pn_u(i,j)*cff
            rufrc(i,j)=rufrc(i,j) - cff
            u(i,j,k,indx)=u(i,j,k,indx) - dt*cff1
#ifdef DIAGNOSTICS_UV
            MHmix(i,j,k,1) = - 2*cff1/(Hz(i-1,j,k)+Hz(i,j,k))
     &                       SWITCH umask(i,j)
#endif
          enddo
        enddo
        do j=JstrV,Jend
          do i=Istr,Iend
            cff=pn_v(i,j)*(VFx(i+1,j)-VFx(i,j))
     &         -pm_v(i,j)*(VFe(i,j)-VFe(i,j-1))
            cff1=pm_v(i,j)*pn_v(i,j)*cff 
            rvfrc(i,j)=rvfrc(i,j) - cff
            v(i,j,k,indx)=v(i,j,k,indx) - dt*cff1
#ifdef DIAGNOSTICS_UV
            MHmix(i,j,k,2) = - 2*cff1/(Hz(i,j-1,k)+Hz(i,j,k))
     &                       SWITCH vmask(i,j)
#endif
          enddo
        enddo
      enddo
!
      return
      end
!
#ifndef CHILD_SPG
# undef UCLM VCLM
# define CHILD_SPG
# ifdef AGRIF
#  include "uv3dmix4_S.F"
# endif
# undef CHILD_SPG
#endif  /* !CHILD_SPG */ 



