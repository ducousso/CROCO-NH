! $Id: t3dmix_S.F 1466 2014-02-06 17:37:07Z marchesiello $
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
            call t3dmix_tile (istr,iend,jstr,jend, itrc, 
     &               A2d(1,1,trd),A2d(1,2,trd), A2d(1,3,trd),
     &                            A2d(1,4,trd), A2d(1,5,trd))

        else
            call t3dmix_child_tile (istr,iend,jstr,jend, itrc,
     &               A2d(1,1,trd), A2d(1,2,trd), A2d(1,3,trd),
     &                             A2d(1,4,trd), A2d(1,5,trd))

        endif
# else 
       call t3dmix_tile(istr,iend,jstr,jend, itrc, A2d(1,1,trd),
     &                               A2d(1,2,trd), A2d(1,3,trd),
     &                               A2d(1,4,trd), A2d(1,5,trd))
# endif   /* AGRIF */

       enddo
       return
       end
!
!---------------------------------------------------------------------
!*********************************************************************
!---------------------------------------------------------------------
!
!PARENT
! 
      subroutine t3dmix_tile (istr,iend,jstr,jend, itrc,
     &                    FX,FE, LapT, diff3u,diff3v)
!
#else
!
! CHILD
!
      subroutine t3dmix_child_tile(istr,iend,jstr,jend, itrc,
     &                     FX,FE, LapT, diff3u,diff3v)
!
#endif /* CHILD_SPG */
!
!---------------------------------------------------------------------
! ******************************Common Code***************************
!---------------------------------------------------------------------
!!   
      implicit none
#include "param.h"
      integer itrc, istr,iend,jstr,jend, i,j,k, kmld,
     &        imin,imax,jmin,jmax
      real    FX(PRIVATE_2D_SCRATCH_ARRAY),     cff,
     &        FE(PRIVATE_2D_SCRATCH_ARRAY),     cff1,
     &        LapT(PRIVATE_2D_SCRATCH_ARRAY),   cff2,
     &        diff3u(PRIVATE_2D_SCRATCH_ARRAY),
     &        diff3v(PRIVATE_2D_SCRATCH_ARRAY)

#include "grid.h"
#include "ocean3d.h"
#include "mixing.h"
# ifdef CLIMAT_TS_MIXH
#include "climat.h"
#endif
#include "scalars.h"
#ifdef DIAGNOSTICS_TS
# include "diagnostics.h"
#endif
!
#ifdef AGRIF
#include "zoom.h"
#endif
!
#include "compute_auxiliary_bounds.h"
!
#ifdef CHILD_SPG
#define TCLM tsponge
#else
#define TCLM tclm
#endif
!
#ifdef MASKING
# define SWITCH *
#else
# define SWITCH !
#endif
!
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
      do k=1,N

!++=============================================================
!++ Compute total diffusivity according to model configuration
!++=============================================================
        do j=jmin,jmax
          do i=imin,imax+1
            diff3u(i,j)= 
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
#  endif
#  ifdef TS_HADV_RSUP3
     &                     +diff3d_u(i,j,k)
#  endif
     &                           )
# endif
#endif
          enddo
        enddo
        do j=jmin,jmax+1
          do i=imin,imax
            diff3v(i,j)= 
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
#  endif
#  ifdef TS_HADV_RSUP3
     &                     +diff3d_v(i,j,k)
#  endif
     &                           )
# endif
#endif
          enddo
        enddo
 
#ifdef TS_DIF2
!
!--------------------------------------------------------------------
!  Add in horizontal Laplacian diffusion along constant S-surfaces.
!--------------------------------------------------------------------
!
!  Compute XI- and ETA-components of diffusive tracer flux.
!
        do j=jstr,jend
          do i=istr,iend+1
              FX(i,j)=0.5*diff3u(i,j)
     &                   *pmon_u(i,j)*(Hz(i,j,k)+Hz(i-1,j,k))*(
     &                     t(i,j,k,nrhs,itrc)-t(i-1,j,k,nrhs,itrc)
# if defined CLIMAT_TS_MIXH || defined CLIMAT_TS_MIXH_FINE
     &                        -TCLM(i,j,k,itrc)+TCLM(i-1,j,k,itrc)
# endif
     &                                       ) SWITCH umask(i,j)
          enddo
        enddo
        do j=jstr,jend+1
          do i=istr,iend
              FE(i,j)=0.5*diff3v(i,j)
     &                   *pnom_v(i,j)*(Hz(i,j,k)+Hz(i,j-1,k))*(
     &                    t(i,j,k,nrhs,itrc)-t(i,j-1,k,nrhs,itrc)
# if defined CLIMAT_TS_MIXH || defined CLIMAT_TS_MIXH_FINE
     &                       -TCLM(i,j,k,itrc)+TCLM(i,j-1,k,itrc)
# endif
     &                                        ) SWITCH vmask(i,j)
          enddo
        enddo
!
!  Add in horizontal diffusion of tracer [T m^3/s].
!
        do j=jstr,jend
          do i=istr,iend
            cff1=pm(i,j)*pn(i,j)
            t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)+dt*cff1
     &                 *(FX(i+1,j)-FX(i,j)+FE(i,j+1)-FE(i,j))
     &                                             /Hz(i,j,k)
          enddo
        enddo

# ifdef DIAGNOSTICS_TS
!
! Tracer diagnostics
!
        do j=jstr,jend
          do i=istr,iend
            cff1=pm(i,j)*pn(i,j)
            THmix(i,j,k,itrc)=FX(i+1,j)-FX(i,j)
     &                       +FE(i,j+1)-FE(i,j)
#  ifdef MASKING
     &                                        * rmask(i,j)
#  endif

            Trate(i,j,k,itrc)=(Hz(i,j,k)*t(i,j,k,nnew,itrc)
     &                    -Hz_bak(i,j,k)*t(i,j,k,nstp,itrc))
     &                                            /(dt*cff1)
#   ifdef MASKING
     &                                        * rmask(i,j)
#   endif
!
! Divide all diagnostic terms by the cell volume
! (Hz(i,j,k,itrc)/(pm(i,j).*pn(i,j)). There after the unit
! of diag terms are: (unit of tracers)* s-1.
!
            THmix(i,j,k,itrc)=THmix(i,j,k,itrc)*cff1/Hz(i,j,k)
            Trate(i,j,k,itrc)=Trate(i,j,k,itrc)*cff1/Hz(i,j,k)
          enddo
        enddo
# endif /* DIAGNOSTICS */

#endif /* TS_DIF2 */
!
!
#ifdef TS_DIF4
!
!--------------------------------------------------------------------
!  Compute/Add in horizontal biharmonic diffusion along constant
!  S-surfaces.
!--------------------------------------------------------------------
!  The biharmonic operator is computed by applying the Laplacian
!  operator twice.
!
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

!
!  Compute horizontal tracer flux in the XI-direction at U-points.
!
          do j=jmin,jmax
            do i=imin,imax+1
            FX(i,j)=0.5*diff3u(i,j)
     &                 *pmon_u(i,j)*(Hz(i,j,k)+Hz(i-1,j,k))*(
     &                   t(i,j,k,nrhs,itrc)-t(i-1,j,k,nrhs,itrc)
# if defined CLIMAT_TS_MIXH || defined CLIMAT_TS_MIXH_FINE
     &                      -TCLM(i,j,k,itrc)+TCLM(i-1,j,k,itrc)
# endif
     &                                        ) SWITCH umask(i,j)
          enddo
        enddo
!
!  Compute horizontal tracer flux in the ETA-direction at V-points.
!
        do j=jmin,jmax+1
          do i=imin,imax
            FE(i,j)=0.5*diff3v(i,j)
     &                 *pnom_v(i,j)*(Hz(i,j,k)+Hz(i,j-1,k))*(
     &                     t(i,j,k,nrhs,itrc)-t(i,j-1,k,nrhs,itrc)
# if defined CLIMAT_TS_MIXH || defined CLIMAT_TS_MIXH_FINE
     &                        -TCLM(i,j,k,itrc)+TCLM(i,j-1,k,itrc)
# endif
     &                                        ) SWITCH vmask(i,j)
          enddo
        enddo
!
!  Compute first Laplacian, without mixing coefficient.
!  Multiply by the metrics of the second Laplacian.
!  Save into work array "LapT".
!
         do j=jmin,jmax
            do i=imin,imax
            LapT(i,j)=(FX(i+1,j)-FX(i,j)+FE(i,j+1)-FE(i,j))
     &                          *pm(i,j)*pn(i,j)/Hz(i,j,k)
          enddo
        enddo
!
!  Apply boundary conditions (except periodic; closed or gradient)
!  to the first Laplacian.
!
# ifndef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=jmin,jmax
#  ifndef OBC_WEST
            LapT(istr-1,j)=0.
#  else
            LapT(istr-1,j)=LapT(istr,j)
#  endif
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=jmin,jmax
#  ifndef OBC_EAST
            LapT(iend+1,j)=0.
#  else
            LapT(iend+1,j)=LapT(iend,j)
#  endif
          enddo
        endif
# endif /* !EW_PERIODIC */
# ifndef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=imin,imax
#  ifndef OBC_SOUTH
            LapT(i,jstr-1)=0.
#  else
            LapT(i,jstr-1)=LapT(i,jstr)
#  endif
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=imin,imax
#  ifndef OBC_NORTH
            LapT(i,jend+1)=0.
#  else
            LapT(i,jend+1)=LapT(i,jend)
#  endif
          enddo
        endif
# endif /* !NS_PERIODIC */
!
!  Compute FX=d(LapT)/d(xi) and FE=d(LapT)/d(eta) terms
!  After that cmpute and add in biharmonic mixing [T m^3/s].
!  Multiply by mixing coefficient.
!
        do j=jstr,jend
          do i=istr,iend+1
              FX(i,j)=-0.5*diff3u(i,j)
     &                    *pmon_u(i,j)*(Hz(i,j,k)+Hz(i-1,j,k))
     &                                *(LapT(i,j)-LapT(i-1,j)) 
#    ifdef MASKING
     &                                        * umask(i,j)
#    endif
          enddo
        enddo
        do j=jstr,jend+1
          do i=istr,iend
              FE(i,j)=-0.5*diff3v(i,j)
     &                    *pnom_v(i,j)*(Hz(i,j,k)+Hz(i,j-1,k))
     &                                *(LapT(i,j)-LapT(i,j-1))
#    ifdef MASKING
     &                                         * vmask(i,j)
#    endif
          enddo
        enddo
!
!  Add in horizontal diffusion of tracer [T m^3/s].
!
        do j=jstr,jend
          do i=istr,iend
            cff1=pm(i,j)*pn(i,j)
            t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)+dt*cff1
     &                 *(FX(i+1,j)-FX(i,j)+FE(i,j+1)-FE(i,j))
     &                                             /Hz(i,j,k)
          enddo
        enddo

# ifdef DIAGNOSTICS_TS
!
! Tracer diagnostics
!
        do j=jstr,jend
          do i=istr,iend
            cff1=pm(i,j)*pn(i,j)
            THmix(i,j,k,itrc)=FX(i+1,j)-FX(i,j)
     &                       +FE(i,j+1)-FE(i,j)
#  ifdef MASKING
     &                                        * rmask(i,j)
#  endif
            Trate(i,j,k,itrc)=(Hz(i,j,k)*t(i,j,k,nnew,itrc)
     &                    -Hz_bak(i,j,k)*t(i,j,k,nstp,itrc))
     &                                            /(dt*cff1)
#   ifdef MASKING
     &                                        * rmask(i,j)
#   endif
!
! Divide all diagnostic terms by the cell volume
! (Hz(i,j,k,itrc)/(pm(i,j).*pn(i,j)). There after the unit
! of diag terms are: (unit of tracers)* s-1.
!
            THmix(i,j,k,itrc)=THmix(i,j,k,itrc)*cff1/Hz(i,j,k)
            Trate(i,j,k,itrc)=Trate(i,j,k,itrc)*cff1/Hz(i,j,k)
          enddo
        enddo
# endif /* DIAGNOSTICS_TS */

#endif /* TS_DIF4 */
      enddo   ! --> k
!---------------------------------------------------------------------
!
# ifdef DIAGNOSTICS_TS
#  if defined DIAGNOSTICS_TS_MLD && !defined SPONGE_DIF2
!==========================================================
! Tracer diagnostics averaged over the MLD
!==========================================================
!
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
#  endif /* DIAGNOSTICS_TS_MLD && !SPONGE_DIF2 */

#  ifdef DIAGNOSTICS_DEBUG
       if (istr.eq.1 .and. jstr.eq.1 .and. itrc.eq.itemp) then
         i=5
         j=5
#   if defined DIAGNOSTICS_TS_MLD && !defined SPONGE_DIF2
         cff=Trate_mld(i,j,itrc)-
     &       TXadv_mld(i,j,itrc)-
     &       TYadv_mld(i,j,itrc)-
     &       TVadv_mld(i,j,itrc)-
     &       TVmix_mld(i,j,itrc)-
     &       THmix_mld(i,j,itrc)-
     &       Tentr_mld(i,j,itrc)-
     &       TForc_mld(i,j,itrc)
       print *,'T3DMIX_S  : T budget closure MLD   : ',cff
#   endif /* DIAGNOSTICS_TS_MLD && !SPONGE_DIF2 */
          cff=Trate(i,j,N-5,itrc)-
     &        TXadv(i,j,N-5,itrc)-
     &        TYadv(i,j,N-5,itrc)-
     &        TVadv(i,j,N-5,itrc)-
     &        TVmix(i,j,N-5,itrc)-
     &        THmix(i,j,N-5,itrc)-
     &        TForc(i,j,N-5,itrc)
       print *,'T3DMIX_S  : T budget closure k=N-5 : ',cff
      endif
#  endif /* DIAGNOSTICS_DEBUG */
# endif /* DIAGNOSTICS_TS */
!------------------------------------------------------------------
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
#  ifdef THREE_GHOST_POINTS_TS
        call exchange_r3d_3pts_tile (Istr,Iend,Jstr,Jend,
     &                               t(START_2D_ARRAY,1,nnew,itrc))
#  else
        call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                          t(START_2D_ARRAY,1,nnew,itrc))
#  endif
# endif
!
      return
      end

#ifndef CHILD_SPG
# define CHILD_SPG
# ifdef AGRIF
#  include "t3dmix_S.F"
# endif
# undef CHILD_SPG
#endif  /* !CHILD_SPG */
