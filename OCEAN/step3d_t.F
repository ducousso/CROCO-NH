! $Id: step3d_t.F 1576 2014-07-03 16:15:16Z gcambon $
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
#ifdef SOLVE3D

      subroutine step3d_t (tile)
!
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "ocean3d.h"
# include "compute_tile_bounds.h"

      trd=omp_get_thread_num()
      call step3d_t_tile (Istr,Iend,Jstr,Jend,
     &                    A2d(1,1,trd), A2d(1,2,trd), A2d(1,3,trd),
     &                    A2d(1,4,trd), A2d(1,5,trd), A2d(1,6,trd),
     &                    A2d(1,7,trd), A2d(1,8,trd), A2d(1,9,trd),
     &                                                A3d(1,1,trd))
      return
      end

      subroutine step3d_t_tile (Istr,Iend,Jstr,Jend,
     &                          FX,FE, WORK, FC,CF,BC,DC,EC,GC, swdk)

      implicit none
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "mixing.h"
# include "climat.h"
# include "scalars.h"
# include "sources.h"
# include "forces.h"
# ifdef DIAGNOSTICS_TS
# include "diagnostics.h"
# endif
      integer Istr,Iend,Jstr,Jend, itrc, i,j,k, indx, kmld
     &       ,imin,imax,jmin,jmax,iAkt,nadv
# ifdef PSOURCE
     &       ,is
# endif
# ifdef AGRIF
# include "zoom.h"
# endif
      real FX(PRIVATE_2D_SCRATCH_ARRAY),
     &     FE(PRIVATE_2D_SCRATCH_ARRAY),   cff,
     &     WORK(PRIVATE_2D_SCRATCH_ARRAY), epsil,
     &     FC(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     CF(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     BC(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     DC(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     EC(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     GC(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     swdk(PRIVATE_2D_SCRATCH_ARRAY,0:N)
      real cff1,cff2,gama,dRz,hbltmp,sig,dXmax,dEmax,
     &     dpth,smax,amax,amaxx
# ifdef TS_VADV_FCT
      real tmax,tmin,Ppos,Qpos,Pneg,Qneg,lmdmax
# endif
      parameter (epsil=1.E-16)
!
#ifdef MASKING
# define SWITCH *
#else
# define SWITCH !
#endif
!
# if defined TS_HADV_UP5 || defined TS_HADV_C6 \
      || defined TS_HADV_WENO5 || defined BIO_HADV_WENO5
!
!--------------------------------------------------------------------
! Definition of flux operators: 1st, 2nd, 3rd, 4th, 5th or 6th order,
! used in UP5 and C6 advection schemes (and order degradation near 
! land masks). cdiff is part of laplacian diffusion in flux1 (used
! near mask): 
!    0 --> flux1=flux2 (second order C2 advection scheme)
!    1 --> flux1 gives 1st order monotonic UP1 advection scheme
!--------------------------------------------------------------------
!
      REAL    :: q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2
      REAL    :: ua, vel, cdiff, cdif
      REAL    :: flux1, flux2, flux3, flux4, flux5, flux6
      REAL    :: flx2, flx3, flx4, flx5
      REAL    :: mask0, mask1, mask2, mask3

      flux2(q_im1, q_i, ua, cdiff) = 0.5*( q_i + q_im1 )

      flux1(q_im1, q_i, ua, cdiff) = flux2(q_im1, q_i, ua, cdiff) -
     &      0.5*cdiff*sign(1.,ua)*(q_i-q_im1)

      flux4(q_im2, q_im1, q_i, q_ip1, ua) = 
     &      ( 7.*(q_i + q_im1) - (q_ip1 + q_im2) )/12.0

      flux3(q_im2, q_im1, q_i, q_ip1, ua) =
     &      flux4(q_im2, q_im1, q_i, q_ip1, ua) +
     &      sign(1.,ua)*((q_ip1 -
     &      q_im2)-3.*(q_i-q_im1))/12.0

      flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =
     &      ( 37.*(q_i+q_im1) - 8.*(q_ip1+q_im2)
     &      +(q_ip2+q_im3) )/60.0

      flux5(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =
     &      flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua)
     &      -sign(1.,ua)*( 
     &      (q_ip2-q_im3)-5.*(q_ip1-q_im2)+10.*(q_i-q_im1) )/60.0
# endif
# if  defined TS_HADV_WENO5 || defined BIO_HADV_WENO5 || defined TS_VADV_WENO5
      REAL    :: flux3_weno, flux5_weno
# endif
!
# include "compute_auxiliary_bounds.h"

      nadv = 3   !<-- do not remove: used in compute_tracer_fluxes.h
!
!======================================================================
!
!  Compute horizontal advection
!
!======================================================================
!
      do itrc=1,NT
        do k=1,N

# ifdef BIO_HADV_WENO5
          if (itrc .le. 2) then
# endif
!
!----------------------------------------------------------
! Compute fluxes FX and FE
!----------------------------------------------------------
!
#  undef PREDICTOR
#  include "compute_horiz_tracer_fluxes.h"

!
!----------------------------------------------------------
! WENO5 advection for passive tracers (biology ...)
!----------------------------------------------------------
!
# ifdef BIO_HADV_WENO5
          else  !<-- if (itrc .gt. 2)
#  define FLUX5 flux5_weno
#  define FLUX3 flux3_weno
#  define FLUX2 flux1
#  define UP5_MASKING
!
            cdif=1.
#  include "t3dadv_order5.h"
!
#  undef FLUX5
#  undef FLUX3
#  undef FLUX2
#  undef UP5_MASKING
          endif
# endif
!
!----------------------------------------------------------
! Apply point sources for river runoff simulations
!----------------------------------------------------------
!
# ifdef PSOURCE
          do is=1,Nsrc
#  ifdef MPI
           i=Isrc_mpi(is,mynode)
           j=Jsrc_mpi(is,mynode)
#  else
            i=Isrc(is)
            j=Jsrc(is)
#  endif
            if (Istr.le.i .and. i.le.Iend+1
     &                   .and. Jstr.le.j .and. j.le.Jend+1) then
              if (Dsrc(is).eq.0) then
                if (Lsrc(is,itrc)) then
                  FX(i,j)=Huon(i,j,k)*Tsrc(is,k,itrc)
#  ifdef MASKING
                else
                  if (rmask(i,j).eq.0 .and. rmask(i-1,j).eq.1) then
                    FX(i,j)=Huon(i,j,k)*t(i-1,j,k,3,itrc)
                  elseif (rmask(i,j).eq.1. .and. rmask(i-1,j).eq.0) then
                    FX(i,j)=Huon(i,j,k)*t(i  ,j,k,3,itrc)
                  endif
#  endif
                endif
              else
                if (Lsrc(is,itrc)) then
                  FE(i,j)=Hvom(i,j,k)*Tsrc(is,k,itrc)
#  ifdef MASKING
                else
                  if (rmask(i,j).eq.0 .and. rmask(i,j-1).eq.1) then
                    FE(i,j)=Hvom(i,j,k)*t(i,j-1,k,3,itrc)
                  elseif (rmask(i,j).eq.1 .and. rmask(i,j-1).eq.0) then
                    FE(i,j)=Hvom(i,j,k)*t(i,j  ,k,3,itrc)
                  endif
#  endif
                endif
              endif
            endif
          enddo
# endif /* PSOURCE */
!
!----------------------------------------------------------
! Finalize horizontal advection: compute flux divergences
!----------------------------------------------------------
!
          do j=Jstr,Jend
            do i=Istr,Iend
              t(i,j,k,nnew,itrc)=Hz_bak(i,j,k)*t(i,j,k,nstp,itrc)
     &                     -dt*pm(i,j)*pn(i,j)*( FX(i+1,j)-FX(i,j)
     &                                          +FE(i,j+1)-FE(i,j)
     &                                                           )
            enddo
          enddo                 !-->  discard FX,FE

# if defined AGRIF && defined AGRIF_CONSERV_TRA
          IF (Agrif_Root()) Then
          MYFX(IstrR:IendR,JstrR:JendR,k,itrc)=
     &        dt*FX(IstrR:IendR,JstrR:JendR)
          MYFY(IstrR:IendR,JstrR:JendR,k,itrc)=
     &        dt*FE(IstrR:IendR,JstrR:JendR)
          Else
          MYFX(IstrR:IendR,JstrR:JendR,k,itrc)=
     &       MYFX(IstrR:IendR,JstrR:JendR,k,itrc)
     &       +dt*FX(IstrR:IendR,JstrR:JendR)
          MYFY(IstrR:IendR,JstrR:JendR,k,itrc)=
     &       MYFY(IstrR:IendR,JstrR:JendR,k,itrc)
     &      +dt*FE(IstrR:IendR,JstrR:JendR)
          EndIF
# endif
!
!----------------------------------------------------------
! Store diagnostic transport term
!----------------------------------------------------------
!
# ifdef DIAGNOSTICS_TS
          do j=Jstr,Jend
            do i=Istr,Iend 
              TXadv(i,j,k,itrc)=-(FX(i+1,j)-FX(i,j)) SWITCH rmask(i,j)
              TYadv(i,j,k,itrc)=-(FE(i,j+1)-FE(i,j)) SWITCH rmask(i,j)
          enddo
        enddo
# endif /* DIAGNOSTICS_TS */

        enddo    !<-- k
      enddo    !<-- itrc

!
!----------------------------------------------------------
! Compute fraction of the solar shortwave flux "swdk"
! penetrating to grid level depth (at vertical w-points).
! (swdk is used later in this routine)
!----------------------------------------------------------
!
# if defined LMD_SKPP || defined LMD_BKPP || defined GLS_MIXING
#  define wrk1 FX
#  define wrk2 FE
      do k=1,N-1
        do j=Jstr,Jend
          do i=Istr,Iend
            wrk1(i,j)=z_w(i,j,k)-z_w(i,j,N)
          enddo
        enddo
        call lmd_swfrac_tile (Istr,Iend,Jstr,Jend,1.,wrk1,wrk2)
        do j=Jstr,Jend
          do i=Istr,Iend
            swdk(i,j,k)=wrk2(i,j)
          enddo
        enddo
      enddo
#  undef wrk1
#  undef wrk2
# endif /* LMD_SKPP || LMD_BKPP */
!
!
!======================================================================
!
!  Compute vertical advection
!
!======================================================================
!
      do j=Jstr,Jend
        do itrc=1,NT
!
!----------------------------------------------------------
! Compute vertical fluxes FC
!----------------------------------------------------------
!
# undef PREDICTOR
# include "compute_vert_tracer_fluxes.h"

!----------------------------------------------------------
! Apply flux correction for monotonicity
!----------------------------------------------------------
!
#  ifdef TS_VADV_FCT
#   include "FCT.h"          
#  endif
!
!----------------------------------------------------------
! Apply vertical advective flux divergence
!----------------------------------------------------------
!
          do k=1,N
            do i=Istr,Iend
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)-CF(i,0)*(FC(i,k)
     &                                                      -FC(i,k-1))
            enddo
          enddo               !--> discard FC

# ifdef DIAGNOSTICS_TS
       do k=1,N 
         do i=Istr,Iend
           TVadv(i,j,k,itrc) = -(FC(i,k)-FC(i,k-1))
#  ifdef MASKING
     &         * rmask(i,j)
#  endif
         enddo
       enddo
# endif /* DIAGNOSTICS_TS */  
!
!
!======================================================================
!
!  Compute surface and Bottom forcing
!
!======================================================================
!
!----------------------------------------------------------
! Add top and bottom fluxes
!----------------------------------------------------------
!
          do i=Istr,Iend
            FC(i,N)=dt*stflx(i,j,itrc)
            FC(i,0)=-dt*btflx(i,j,itrc)
          enddo
!
!----------------------------------------------------------
! Add solar radiation flux in temperature equation
! Also compute the nonlocal transport flux for unstable
! (convective) forcing conditions into matrix DC when using
! the Large et al. 1994 KPP scheme.
!----------------------------------------------------------
!
          if (itrc.eq.itemp) then
            do k=1,N-1
              do i=Istr,Iend
                FC(i,k)=0.
# if defined LMD_SKPP || defined LMD_BKPP || defined GLS_MIXING
     &    +dt*srflx(i,j)*swdk(i,j,k)
#  ifdef LMD_NONLOCAL
     &    -dt*ghats(i,j,k)*(stflx(i,j,itemp)-srflx(i,j))
#  endif
# endif
              enddo
            enddo
# ifdef SALINITY 
          elseif (itrc.eq.isalt) then
            do k=1,N-1
              do i=Istr,Iend
                FC(i,k)=0.
# if defined LMD_SKPP || defined LMD_BKPP
#  ifdef LMD_NONLOCAL
     &    -dt*ghats(i,j,k)*stflx(i,j,isalt)
#  endif
# endif
              enddo
            enddo
# endif
          endif
# ifdef SALINITY 
          if (itrc.eq.itemp .or. itrc.eq.isalt) then
# else
          if (itrc.eq.itemp) then
# endif
            do k=1,N
              do i=Istr,Iend
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)+FC(i,k )
     &                                               -FC(i,k-1)
# ifdef DIAGNOSTICS_TS
                TForc(i,j,k,itrc)=(FC(i,k)-FC(i,k-1))
     &                                    /(dt*pm(i,j)*pn(i,j))
#  ifdef MASKING
     &                                             * rmask(i,j)
#  endif
# endif  /* DIAGNOSTICS_TS */
              enddo
            enddo
          endif
!
!
!======================================================================
!
! Compute vertical mixing (implicit step)
!
! if lateral rotated diffusion is chosen, this step is performed
! after lateral diffusion to include implicit correction to lateral
! diffusion
!
!======================================================================
!
#  ifdef VADV_ADAPT_IMP
       do i=Istr,Iend
            DC(i,0)=dt*pn(i,j)*pm(i,j)
       enddo
#  endif

# ifdef TS_MIX_IMP
#  ifdef BIO_HADV_WENO5
#   undef TS_MIX_IMP 
         if (itrc .gt. 2) then   ! for passive tracers only
#   include "t3dmix_tridiagonal.h"
#   define TS_MIX_IMP
         else
#  endif
         do k=1,N
           do i=istr,iend
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc) / Hz(i,j,k)
           enddo
         enddo
#  ifdef BIO_HADV_WENO5
         endif
#  endif
# else
#  include "t3dmix_tridiagonal.h"
# endif

# ifdef CONST_TRACERS
          do k=1,N
            do i=Istr,Iend
              t(i,j,k,nnew,itrc)=t(i,j,k,nstp,itrc)
            enddo
          enddo
#  ifdef DIAGNOSTICS_TS
          do k=1,N
            do i=Istr,Iend
               TVmix(i,j,k,itrc)=0.0
#   ifdef MASKING
     &              * rmask(i,j)
#   endif
            enddo
          enddo
#  endif /* DIAGNOSTICS_TS */
# endif /* CONST_TRACERS */
        enddo      ! <-- itrc

!
!----------------------------------------------------------
! Compute vertical gradient in vertical T-diffusion 
! coefficient for floats random walk.
!----------------------------------------------------------
!
# if defined FLOATS && defined  RANDOM_WALK
          do k=1,N
            do i=Istr,Iend
              dAktdz(i,j,k)=(Akt(i,j,k,1)-Akt(i,j,k-1,1))/Hz(i,j,k)
            enddo
          enddo
# endif
      enddo      ! <-- j
!
!======================================================================
!  Set lateral boundary conditions; nudge toward tracer climatology;
!  apply land-sea mask and exchange periodic boundary conditions.
!======================================================================
!
      do itrc=1,NT
        call t3dbc_tile (Istr,Iend,Jstr,Jend, nnew,itrc, WORK)
      enddo
!
!======================================================================
!  Compute biological fluxes
!======================================================================
!
# ifdef BIOLOGY
      call biology_tile (Istr,Iend,Jstr,Jend)
# endif
!
!======================================================================
!  Compute inverse vertical Rho gradients for use in t3dmix
!======================================================================
!
# if !defined TS_MIX_S && \
      (defined TS_DIF4 || defined TS_DIF2 || defined SPONGE_DIF2)

#  ifdef TS_MIX_GEO
!
! --- Geopotential diffusion: idRz contains idZ values ---
!
      do k=1,N-1
        do j=jstr,jend
          do i=istr,iend
            idRz(i,j,k) = 1./(z_r(i,j,k+1)-z_r(i,j,k))
          enddo
        enddo
      enddo
      do j=jstr,jend
        do i=istr,iend
          idRz(i,j,N) = 0.
          idRz(i,j,0) = 0.
        enddo
      enddo

#  elif defined TS_MIX_ISO
!
!  --- Isopycnal diffusion: idRz is inverse vertical rho gradient ---
!
      do k=1,N-1
        do j=jstr,jend
          do i=istr,iend
#   ifdef SPLIT_EOS
            dpth=z_w(i,j,N)-0.5*(z_r(i,j,k+1)+z_r(i,j,k))
            dRz =rho1(i,j,k+1)-rho1(i,j,k)
     &          +(qp1(i,j,k+1)- qp1(i,j,k))
     &               *dpth*(1.-2.*qp2*dpth)
#   else
            dRz = rho1(i,j,k+1)-rho1(i,j,k)
#   endif
            cff  = min( 1./dRz,-1.E-14 )   !  minimum stratification imposed
            cff1 = 1./(z_r(i,j,k+1)-z_r(i,j,k))
#   ifdef LMD_SKPP2005
            hbltmp=hbls(i,j,3-nstp)
#   elif defined LMD_SKPP1994
            hbltmp=hbl(i,j)
#   else
            hbltmp=10.
#   endif
            gama = 1.                                      ! mixing becomes
            sig = (z_w(i,j,N)-z_w(i,j,k))/max(hbltmp,10.)  ! isosigma within  
            if (sig .lt. 1.) gama = sig*sig*(3.-2.*sig)    ! PBL depth (>10m)
            dXmax = max(abs(dRdx(i,j,k  )),abs(dRdx(i+1,j,k  )),
     &                  abs(dRdx(i,j,k+1)),abs(dRdx(i+1,j,k+1)),1E-14)
            dEmax = max(abs(dRde(i,j,k  )),abs(dRde(i,j+1,k  )),
     &                  abs(dRde(i,j,k+1)),abs(dRde(i,j+1,k+1)),1E-14)
            smax=min(Rslope_max,Gslope_max*min(pm(i,j),pn(i,j))/cff1)
!
            idRz(i,j,k) = max(cff,   -smax*gama*cff1 / dXmax,
     &                               -smax*gama*cff1 / dEmax )
          enddo
        enddo
      enddo
      do j=jstr,jend
        do i=istr,iend
          idRz(i,j,N) = 0.
          idRz(i,j,0) = 0.
        enddo
      enddo
#  endif /* TS_MIX_ISO */
!
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_w3d_tile (  istr,iend,jstr,jend,
     &                          idRz(START_2D_ARRAY,0)  )
#  endif
!
# endif /* TS_DIF2 || TS_DIF4 || SPONGE_DIF2 */
!
!
      do itrc=1,NT   !<-- itrc
!
!======================================================================
!  Compute Nudging terms
!======================================================================
!
# if defined TNUDGING && defined TCLIMATOLOGY
#  ifdef ZONAL_NUDGING
        if (iic.eq.ntstart .or. mod(iic,10).eq.0) then
          call zonavg_3d(istr,iend,jstr,jend,
     &                  t(START_2D_ARRAY,1,nnew,itrc),tzon(:,:,itrc))
        endif
        if (iic.eq.ntstart) then
          call zonavg_3d(istr,iend,jstr,jend,
     &                  tclm(START_2D_ARRAY,1,itrc),tclmzon(:,:,itrc))
        endif
#  endif /* ZONAL_NUDGING */

        do k=1,N
          do j=Jstr,Jend
            do i=Istr,Iend
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)
     &                    +dt*Tnudgcof(i,j,k,itrc)*
#  ifdef ZONAL_NUDGING
     &                         (tclmzon(j,k,itrc)-tzon(j,k,itrc))
#  else
     &                         (tclm(i,j,k,itrc)-t(i,j,k,nnew,itrc))
#  endif /* ZONAL_NUDGING */
#  ifdef DIAGNOSTICS_TS
              TForc(i,j,k,itrc)=(TForc(i,j,k,itrc)
     &                    +Tnudgcof(i,j,k,itrc)
     &                    *(tclm(i,j,k,itrc)
#   ifdef ZONAL_NUDGING
     &                                           -tzon(j,k,itrc))
#   else
     &                                           -t(i,j,k,nnew,itrc))
#   endif /* ZONAL_NUDGING */
     &                    *(Hz(i,j,k)/(pm(i,j)*pn(i,j))))
#   ifdef MASKING
     &                    *rmask(i,j)
#   endif
#  endif  /* DIAGNOSTICS_TS */
            enddo
          enddo
        enddo
# endif /* TNUDGING && TCLIMATOLOGY */
!
!======================================================================
! Compute the tendency term of tracer diagnostics
! Divide all diagnostic terms by the cell volume
! (Hz(i,j,k,itrc)/(pm(i,j).*pn(i,j)). There after the unit
! of diagnostic terms will be: (unit of tracers)* s-1.
!
! Note: the Horizontal mixing term is computed in t3dmix
! where Trate is updated accordingly
!======================================================================
!
# ifdef DIAGNOSTICS_TS
        do k=1,N
          do j=Jstr,Jend
            do i=Istr,Iend
              Trate(i,j,k,itrc)=(Hz(i,j,k)*t(i,j,k,nnew,itrc)
     &                          -Hz_bak(i,j,k)*t(i,j,k,nstp,itrc))
     &                                       /(dt*pm(i,j)*pn(i,j))
#  ifdef MASKING
     &                   *rmask(i,j)
#  endif
!
#  ifdef DIAGNOSTICS_TS_ADV
              TXadv(i,j,k,itrc)=TXadv(i,j,k,itrc)+
     &           t(i,j,k,3,itrc)*(Huon(i+1,j,k)-Huon(i,j,k))
              TYadv(i,j,k,itrc)=TYadv(i,j,k,itrc)+
     &           t(i,j,k,3,itrc)*(Hvom(i,j+1,k)-Hvom(i,j,k))
              TVadv(i,j,k,itrc)=TVadv(i,j,k,itrc)-
     &           t(i,j,k,3,itrc)*(Huon(i+1,j,k)-Huon(i,j,k)+
     &                            Hvom(i,j+1,k)-Hvom(i,j,k))
#  endif /* DIAGNOSTICS_TS_ADV */
!
              cff=pm(i,j)*pn(i,j)/Hz(i,j,k)
              TXadv(i,j,k,itrc)=TXadv(i,j,k,itrc)*cff
              TYadv(i,j,k,itrc)=TYadv(i,j,k,itrc)*cff
              TVadv(i,j,k,itrc)=TVadv(i,j,k,itrc)*cff
              TVmix(i,j,k,itrc)=TVmix(i,j,k,itrc)*cff
              TForc(i,j,k,itrc)=TForc(i,j,k,itrc)*cff
              Trate(i,j,k,itrc)=Trate(i,j,k,itrc)*cff
              THmix(i,j,k,itrc)=0.
            enddo
          enddo
        enddo
!
!----------------------------------------------------------
! Compute tracer diagnostics averaged over the MLD
!----------------------------------------------------------
!
#  ifdef DIAGNOSTICS_TS_MLD
#   define T_mld_nnew FX
#   define T_mld_nstp FE
        do j=Jstr,Jend
          do i=Istr,Iend
            TXadv_mld(i,j,itrc)=0.
            TYadv_mld(i,j,itrc)=0.
            TVadv_mld(i,j,itrc)=0.
            TVmix_mld(i,j,itrc)=0.
            THmix_mld(i,j,itrc)=0.
            TForc_mld(i,j,itrc)=0.
            Trate_mld(i,j,itrc)=0.
            Tentr_mld(i,j,itrc)=0.
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
              TXadv_mld(i,j,itrc)=TXadv_mld(i,j,itrc)+
     &                            TXadv(i,j,k,itrc)*cff
              TYadv_mld(i,j,itrc)=TYadv_mld(i,j,itrc)+
     &                            TYadv(i,j,k,itrc)*cff
              TVadv_mld(i,j,itrc)=TVadv_mld(i,j,itrc)+
     &                            TVadv(i,j,k,itrc)*cff
              TVmix_mld(i,j,itrc)=TVmix_mld(i,j,itrc)+
     &                            TVmix(i,j,k,itrc)*cff
              TForc_mld(i,j,itrc)=TForc_mld(i,j,itrc)+
     &                            TForc(i,j,k,itrc)*cff
              T_mld_nnew(i,j)     =T_mld_nnew(i,j)+
     &                            t(i,j,k,nnew,itrc)*cff
            enddo
          enddo
        enddo

#   if (!defined TS_DIF2 && !defined TS_DIF4 && !defined SPONGE_DIF2) \
       || defined DIAGNOSTICS_DEBUG
!
! Compute entrainement/detrainement term.
! If diffusion terms are computed in t3dmix routines, entrainement 
! terms are also computed in those routines
!
        do j=Jstr,Jend
          do i=Istr,Iend
#    ifdef LMD_SKPP
            if (kbl_nstp(i,j).eq.0) kbl_nstp(i,j)=kbl(i,j)
            kmld=kbl_nstp(i,j)
#    else
            kmld=N-5
#    endif
            do k=N,kmld,-1
              cff=Hz_bak(i,j,k)/(z_w(i,j,N)-z_w(i,j,kmld-1))
              T_mld_nstp(i,j)=T_mld_nstp(i,j)+
     &                        t(i,j,k,nstp,itrc)*cff
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
     &                          TForc_mld(i,j,itrc)
          enddo
        enddo
#   endif /* TS_DIF2 && TS_DIF4 && SPONGE_DIF2 */
#   undef T_mld_nnew 
#   undef T_mld_nstp
#  endif /* DIAGNOSTICS_TS_MLD */

#  ifdef DIAGNOSTICS_DEBUG
        if (istr.eq.1 .and. jstr.eq.1 .and. itrc.eq.itemp) then
          i=5
          j=5
#   if defined DIAGNOSTICS_TS_MLD
          cff=Trate_mld(i,j,itrc)-
     &        TXadv_mld(i,j,itrc)-
     &        TYadv_mld(i,j,itrc)-
     &        TVadv_mld(i,j,itrc)-
     &        TVmix_mld(i,j,itrc)-
     &        THmix_mld(i,j,itrc)-
     &        Tentr_mld(i,j,itrc)-
     &        TForc_mld(i,j,itrc)
             MPI_master_only write(stdout,'(A,2x,1pe10.3)')
     &       ' STEP3D_T: T budget closure MLD     :', cff
!
#   endif /* DIAGNOSTICS_TS_MLD */
          cff=Trate(i,j,N-5,itrc)-
     &        TXadv(i,j,N-5,itrc)-
     &        TYadv(i,j,N-5,itrc)-
     &        TVadv(i,j,N-5,itrc)-
     &        TVmix(i,j,N-5,itrc)-
     &        THmix(i,j,N-5,itrc)-
     &        TForc(i,j,N-5,itrc)
            MPI_master_only write(stdout,'(A,2x,1pe10.3)')
     &      ' STEP3D_T: T budget closure at k=N-5:', cff
        endif
#  endif /* DIAGNOSTICS_DEBUG */
# endif /* DIAGNOSTICS_TS */
!
!
!======================================================================
!  Set PHYSICAL lateral boundary conditions.
!======================================================================
!
!----------------------------------------------------------
! Apply land mask
!----------------------------------------------------------
!
# ifdef MASKING
#  ifdef EW_PERIODIC
#   define I_RANGE Istr,Iend
#  else
#   define I_RANGE IstrR,IendR
#  endif
#  ifdef NS_PERIODIC
#   define J_RANGE Jstr,Jend
#  else
#   define J_RANGE JstrR,JendR
#  endif
        do k=1,N
          do j=J_RANGE
            do i=I_RANGE
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)*rmask(i,j)
            enddo
          enddo
        enddo
#  undef I_RANGE
#  undef J_RANGE
# endif /* MASKING */
!
!----------------------------------------------------------
! Exchange periodic boundaries and computational margins.
!----------------------------------------------------------
!
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
#  ifdef THREE_GHOST_POINTS_TS
        call exchange_r3d_3pts_tile (Istr,Iend,Jstr,Jend,
     &                          t(START_2D_ARRAY,1,nnew,itrc))
#  else
        call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                          t(START_2D_ARRAY,1,nnew,itrc))
#  endif
# endif

      enddo   ! <-- itrc

      return
      end
!
!======================================================================
!
# if defined TS_HADV_WENO5 || defined BIO_HADV_WENO5 || defined TS_VADV_WENO5
!    
      function flux5_weno(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua)
!
!     WENO5: Weighted Essentially Non-Oscillatory scheme 
!            with 5th-order accuracy
!
! This function computes tracer reconstruction at the grid cell's left 
! edge (u-point i-1/2 or v-point j-1/2). WENO5 uses a convex combination 
! of the polynomials reconstructed on the three ENO3 stencils in order
! to achieve higher accuracy on smooth profiles. Both left and right 
! combinations of mirror symetric stencils around i-1/2 (j-1/2) points 
! are computed. For stability, the upstream stencil combination, 
! identified by the sign of ua (u or v), is selected. 
!
! The scheme includes improvements from Borges et al., 2008 (WENO_Z), i.e.,
! smoothness indicators of higher order (with new non-oscillatory weights) 
! that provide a scheme with less dissipation, higher resolution and better 
! monotonicity preservation than the classical WENO5. In the meantime, WENO_Z 
! removes the need to tune parameter Eps for added dissipation (needed in the 
! original scheme due to suboptimal performance on critical points). 
! 
! References:
!  Guang-Shan JIANG and Chi-Wang SHU, 1996: Efficient Implementation of 
!  Weighted ENO Schemes. JOURNAL OF COMPUTATIONAL PHYSICS 126, 202–228.
!
!  Rong WANG and Raymond J. SPITERI, 2007: Linear instability of the 
!  fifth-order WENO method, SIAM J. Numer. Anal., 45, 1871-1901.
!
!  Borges R., M. Carmona, B. Costa, W.S. Don, 2008: An improved weighted 
!  essentially non-oscillatory scheme for hyperbolic conservation laws.
!  Journal of Computational Physics 227 (2008) 3191–3211
!
!  Implementation: P. Marchesiello and J. Demange 2013
!
      implicit none
      REAL    :: flux5_weno
      REAL    :: q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua
      REAL    :: IS0, IS1, IS2
      REAl    :: d0, d1, d2
      REAl    :: a0, a1, a2
      REAL    :: w0, w1, w2
      REAL    :: p0, p1, p2
      REAL    :: Eps, cff1, cff2, T5

#  define WENO_Z

! Epsilon value for computing weigths 
#  ifdef WENO_Z
      Eps = 1.e-40
#  else
!  --> from 1.e-7 (smoother) to 1.e-5 (sharper)
      Eps = 1.e-6
#  endif
! Weigths coefficients
      d0=1./10.
      d1=6./10.
      d2=3./10.
! other coefficients
      cff1=13./12.
      cff2=1./6.

!
      if (ua .ge. 0.) then        ! Take upstream stencils for stability
!
! === Reconstruction at u_i-1/2 using LEFT stencils ===
!
! Nonlinear Smoothness Indicators
        IS0 = cff1*(q_im3 - 2.*q_im2 + q_im1)**2
     &                        + 0.25*(q_im3 - 4.*q_im2 + 3*q_im1)**2
        IS1 = cff1*(q_im2 - 2.*q_im1 + q_i)**2
     &                        + 0.25*(q_im2 - q_i)**2
        IS2 = cff1*(q_im1 - 2.*q_i + q_ip1)**2
     &                        + 0.25*(3.*q_im1 - 4.*q_i + q_ip1)**2
#  ifdef WENO_Z
! Non-normalized stencil weights
! with parameter T5 of new smoothness indicator
        T5 = abs(IS2-IS0)
        a0  = d0*(1+T5/(Eps+IS0))
        a1  = d1*(1+T5/(Eps+IS1))
        a2  = d2*(1+T5/(Eps+IS2))
#  else
        a0  = d0/(Eps+IS0)**2
        a1  = d1/(Eps+IS1)**2
        a2  = d2/(Eps+IS2)**2
#  endif
! Normalized Weigths
        w0  = a0/(a0+a1+a2)
        w1  = a1/(a0+a1+a2)
        w2  = a2/(a0+a1+a2)
! Polynomials
        p0  = cff2*(2.*q_im3 - 7.*q_im2 + 11.*q_im1)
        p1  = cff2*(-q_im2   + 5.*q_im1 + 2.*q_i)
        p2  = cff2*(2.*q_im1 + 5.*q_i   - q_ip1)
! Combination
        flux5_weno = w0*p0 + w1*p1 +w2*p2

      else
!
! === Reconstruction at u_i-1/2 using RIGHT stencils ===
!
! Nonlinear Smoothness Indicators
        IS0 = cff1*(q_ip2 - 2.*q_ip1 + q_i)**2
     &                         + 0.25*(q_ip2 -4.*q_ip1 + 3*q_i)**2
        IS1 = cff1*(q_ip1 - 2.*q_i + q_im1)**2
     &                         + 0.25*(q_ip1-q_im1)**2
        IS2 = cff1*(q_i   - 2.*q_im1 + q_im2)**2
     &                         + 0.25*(3.*q_i -4.*q_im1 + q_im2)**2
#  ifdef WENO_Z
! Non-normalized stencil weights
! with parameter T5 of new smoothness indicator
        T5 = abs(IS2-IS0)
        a0  = d0*(1+T5/(Eps+IS0))
        a1  = d1*(1+T5/(Eps+IS1))
        a2  = d2*(1+T5/(Eps+IS2))
#  else
        a0  = d0/(Eps+IS0)**2
        a1  = d1/(Eps+IS1)**2
        a2  = d2/(Eps+IS2)**2
#  endif
! Normalized Weigths
        w0  = a0/(a0+a1+a2)
        w1  = a1/(a0+a1+a2)
        w2  = a2/(a0+a1+a2)
! Polynomials
        p0  = cff2*(2.*q_ip2 - 7.*q_ip1 + 11.*q_i)
        p1  = cff2*(-q_ip1   + 5.*q_i   + 2.*q_im1)
        p2  = cff2*(2.*q_i   + 5.*q_im1 - q_im2)
! Combination
        flux5_weno = w0*p0 + w1*p1 +w2*p2

      endif

      return
      end
!
!======================================================================
!
      function flux3_weno( q_im2, q_im1, q_i, q_ip1, ua)
!
!     WENO3: Weighted Essentially Non-Oscillatory of 3rd-order Accuracy
!
! This function computes tracer reconstruction at the grid cell's left 
! edge (u-point i-1/2 or v-point j-1/2).
!
! Implementation: P. Marchesiello and J. Demange 2013
!
      implicit none
      REAL    :: flux3_weno
      REAL    :: q_im2, q_im1, q_i, q_ip1, ua
      REAL    :: IS0, IS1
      REAl    :: a0, a1
      REAL    :: w0, w1
      REAL    :: p0, p1 
      REAL    :: Eps, d0,d1, T3
!
! Epsilon value for computing weigths 
#  ifdef WENO_Z
      Eps = 1.e-40
#  else
!  --> from 1.e-7 (smoother) to 1.e-5 (sharper)
      Eps = 1.e-6
#  endif
      d0=1./3.
      d1=2./3.
!
      if (ua .ge. 0.) then        ! Take upstream stencils for stability

        IS0 = (q_im1-q_im2)**2
        IS1 = (q_im1-q_i)**2
#  ifdef WENO_Z
        T3 = abs(IS1-IS0)
        a0  = d0*(1+T3/(Eps+IS0))
        a1  = d1*(1+T3/(Eps+IS1))
#  else
        a0  = 1./(3.*(Eps+IS0)**2)
        a1  = 2./(3.*(Eps+IS1)**2)
#  endif
        w0  = a0/(a0+a1)
        w1  = a1/(a0+a1)
        p0  = 1./2.*(3.*q_im1-q_im2)
        p1  = 1./2.*(q_im1+q_i)

        flux3_weno = w0*p0 + w1*p1

      else

        IS0 = (q_i-q_ip1)**2
        IS1 = (q_im1-q_i)**2
#  ifdef WENO_Z
        T3 = abs(IS1-IS0)
        a0  = d0*(1+T3/(Eps+IS0))
        a1  = d1*(1+T3/(Eps+IS1))
#  else
        a0  = 1./(3.*(Eps+IS0)**2)
        a1  = 2./(3.*(Eps+IS1)**2)
#  endif
        w0  = a0/(a0+a1)
        w1  = a1/(a0+a1)
        p0  = 1./2.*(3.*q_i-q_ip1)
        p1  = 1./2.*(q_im1+q_i)

        flux3_weno = w0*p0 + w1*p1

      endif

      return
      end
# endif /* WENO */
!
!======================================================================
!
#else
      subroutine step3d_t_empty
      return
      end
#endif  /* SOLVE3D */

