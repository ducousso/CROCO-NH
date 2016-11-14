! $Id: step2d.F 1615 2014-12-17 13:27:07Z rblod $
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
#define PRED_COUPLED_MODE

      subroutine step2d (tile)
      implicit none
      integer tile, trd
#include "param.h"
#include "private_scratch.h"
C$    integer omp_get_thread_num
#include "compute_tile_bounds.h"
      trd=0
C$    trd=omp_get_thread_num()
      
      call step2D_FB_tile ( Istr,Iend,Jstr,Jend, A2d(1,1,trd)
     &                    , A2d(1, 2,trd), A2d(1, 3,trd), A2d(1, 4,trd)
     &                    , A2d(1, 5,trd), A2d(1, 6,trd), A2d(1, 7,trd)
     &                    , A2d(1, 8,trd), A2d(1, 9,trd) 
#ifdef M2_HADV_UP3
     &                    , A2d(1,10,trd), A2d(1,11,trd) 
#else
     &                    , A2d(1,10,trd), A2d(1,11,trd) 
     &                    , A2d(1,12,trd), A2d(1,13,trd)     
#endif
     &                    )

      return
      end
 
      subroutine step2D_FB_tile (Istr,Iend,Jstr,Jend, zeta_new,
     &                           Dnew,rubar,rvbar, 
     &                           Drhs, UFx,UFe,
     &                           VFx,VFe
#ifdef M2_HADV_UP3
     &                          ,wrk1,wrk2
#else                           
     &                          ,urhs,vrhs 
     &                          ,DUon,DVom
#endif
     &                          )

!
!======================================================================
! Perform one time step for barotropic mode (free-surface and baro-
! tropic 2D momentum equations) using Generalized Forward-Backward
! AB3-AM4 algorithm. Also calculate fast-time averages to interact
! with baroclinic mode.
!
! Forward-Backward schemes for coupled equations (here free-surface zeta
! and 2D momentum ubar and vbar) are stable for longer time-steps 
! (alpha_max>1) than synchronous schemes while only needing evaluation 
! of each r.h.s. term ones. Rather than the original Euler FB schemes, 
! a generalized FB scheme with an AB3-AM4 step is used here for its added 
! robustness (for Coriolis and advection terms). It is a combination of 
! AB3-like step for zeta and AM4-like step for ubar and vbar but with 
! coefficients chosen for maximum stability (e.g. beta=5/12 in original 
! AB3 is replaced by 0.281105):
!
! AB3 Forward step:
! ----------------
! ubarrhs = (3/2+beta)*ubar(n) - (1/2+2*beta)*ubar(n-1) + beta*ubar(n-2)
! vbarrhs = (3/2+beta)*vbar(n) - (1/2+2*beta)*vbar(n-1) + beta*vbar(n-2)
! zeta(n+1) = zeta(n) + RZETA(ubarhs,vbarrhs)
!
! AM4 Backward step:
! -----------------
! zetarhs = (1/2+gamma+2*epsilon) * zeta(n+1) + 
!           (1/2-2*gamma-3*epsilon) * zeta(n) +
!           gamma * zeta(n-1) + epsilon * zeta(n-2)
! ubar(n+1) = ubar(n) + RUBAR(zetarhs,ubarrhs,vbarrhs)
! vbar(n+1) = vbar(n) + RVBAR(zetarhs,ubarrhs,vbarrhs)
!
! beta=0.281105   gamma=0.0880    epsilon=0.013 
!
! Note that the first 2D step is performed with the original Euler FB 
! scheme and the second 2D step with a AB2-AM3 FB scheme with coefficients 
! again chosen for maximum stability.
!
! Reference:
! ---------
! Shchepetkin, A.F., and J.C. McWilliams, 2009: Computational kernel 
! algorithms for fine-scale, multiprocess, longtime oceanic simulations. 
! Pp. 119–182 in Handbook of Numerical Analysis: Computational Methods 
! for the Atmosphere and Oceans. R.M. Teman and J.J. Tribbia, eds, 
! Elsevier Science.
!
!======================================================================
!
#ifdef NBQ
      use module_nh
      use module_nbq
#endif

      implicit none
#include "param.h"
      integer Istr,Iend,Jstr,Jend, i,j, kbak, kold,
#ifdef MPI
     &         err,
#endif
#ifdef PSOURCE
     &        is,
#endif
     &        imin,imax,jmin,jmax
      real sum_c
      real    VMAX,VMAXL
      real zeta_new(PRIVATE_2D_SCRATCH_ARRAY),  cff,
     &         Dnew(PRIVATE_2D_SCRATCH_ARRAY),  cff0,
     &        rubar(PRIVATE_2D_SCRATCH_ARRAY),  cff1,
     &        rvbar(PRIVATE_2D_SCRATCH_ARRAY),  cff2,
     &         Drhs(PRIVATE_2D_SCRATCH_ARRAY),  cff3,
     &          UFx(PRIVATE_2D_SCRATCH_ARRAY),
     &          UFe(PRIVATE_2D_SCRATCH_ARRAY),  DUnew,
     &          VFx(PRIVATE_2D_SCRATCH_ARRAY),  DVnew,
     &          VFe(PRIVATE_2D_SCRATCH_ARRAY)
#ifdef M2_HADV_UP3
      real     wrk1(PRIVATE_2D_SCRATCH_ARRAY),
     &         wrk2(PRIVATE_2D_SCRATCH_ARRAY)
      real     curvX,curvE,cffE,cffX, gamma
      parameter (gamma = -0.25)
#else
      real     urhs(PRIVATE_2D_SCRATCH_ARRAY),  
     &         vrhs(PRIVATE_2D_SCRATCH_ARRAY),
     &         DUon(PRIVATE_2D_SCRATCH_ARRAY),
     &         DVom(PRIVATE_2D_SCRATCH_ARRAY)
#endif
#ifdef M2FILTER_NONE
      real :: myepsilon,mybeta,myalpha,mygamma
#endif
#include "grid.h"
#include "ocean2d.h"
#include "ocean3d.h"
#include "coupling.h"
#include "forces.h"
#ifdef MRL_WCI
      real vstu,ustv,dudx,dvdx,dude,dvde
#endif
#include "mixing.h"
#include "climat.h"
#include "scalars.h"
#include "sources.h"
#ifdef POT_TIDES
# include "tides.h"
#endif
#ifdef AGRIF
# include "zoom.h"
        integer irhox, irhoy, irhot
#endif
#if defined INTERNAL || defined BODYTIDE
      real U0, omega
#endif
#ifdef WET_DRY
      real cff1_WD,cff2_WD
#endif
#ifdef NBQ
# include "nbq.h"
      integer k
#endif
!
#ifdef MASKING
# define SWITCH *
#else
# define SWITCH !
#endif
!
#ifdef MPI
#include "mpi_roms.h"
      include 'mpif.h'
# define LOCALLM Lmmpi
# define LOCALMM Mmmpi
#else
# define LOCALLM Lm
# define LOCALMM Mm
#endif   
#include "compute_auxiliary_bounds.h"
!
!======================================================================
! AB3 Forward Step: Computes zeta(n+1) and zetarhs
!======================================================================
!
! Preliminary step: compute total depth (meters) of the water
! ----------------- column and vertically integrated mass fluxes
! which are needed to compute divergence in rhs_zeta and as input
! data to compute nonlinear advection terms for the barotropic
! momentum equations.
!
#ifdef M2FILTER_NONE
# ifdef TANK
         myalpha   = 0.01
# else
         myalpha   = 0.5
# endif
         mybeta    = 0.281105
         myepsilon = 0.00976186 - 0.13451357*myalpha
         mygamma   = 0.08344500 - 0.51358400*myalpha
#endif
#ifdef AGRIF
        irhox = Agrif_Irhox()
        irhoy = Agrif_Irhoy()
        irhot = Agrif_Irhot()
#endif
      if (FIRST_2D_STEP) then         ! Meaning of temporal indices
        kbak=kstp                     ! ------- -- -------- -------
        kold=kstp                     ! m-2     m-1      m      m+1
        cff1=1.                       ! kold    kbak    kstp    knew
        cff2=0.
        cff3=0.
      elseif (FIRST_2D_STEP+1) then
        kbak=kstp-1
        if (kbak.lt.1) kbak=4
        kold=kbak
        cff1=1.                  ! Logically AB2-AM3 forward-backward 
        cff2=0.                  ! scheme with coefficients chosen for
        cff3=0.                  ! maximum stability ... (see below)
      else
        kbak=kstp-1
        if (kbak.lt.1) kbak=4
        kold=kbak-1
        if (kold.lt.1) kold=4
#ifdef M2FILTER_NONE
        cff1= 1.5+mybeta
        cff2=-2.0*mybeta-0.5
        cff3= mybeta
#else
        cff1= 1.781105
        cff2=-1.06221
        cff3= 0.281105
#endif
      endif
!
#if defined EW_PERIODIC || defined MPI || !defined M2_HADV_UP3
      imin=IstrU-2
      imax=Iend+1
# define IV_EXT_RANGE Istr-1,Iend+1
#else
      imin=max(IstrU-3,0)
      imax=min(Iend+2,Lm+1)
# define IV_EXT_RANGE max(Istr-2,0),min(Iend+2,Lm+1)
#endif
#if defined NS_PERIODIC || defined MPI || !defined M2_HADV_UP3
      jmin=JstrV-2
      jmax=Jend+1
# define JU_EXT_RANGE Jstr-1,Jend+1
#else
      jmin=max(JstrV-3,0)
      jmax=min(Jend+2,Mm+1)
# define JU_EXT_RANGE max(Jstr-2,0),min(Jend+2,Mm+1)
#endif
!
      do j=jmin,jmax
        do i=imin,imax
          Drhs(i,j)=cff1*zeta(i,j,kstp)+cff2*zeta(i,j,kbak)
     &                                 +cff3*zeta(i,j,kold)
#ifndef NBQ
     &                                             + h(i,j)
#endif
        enddo
      enddo

      do j=JU_EXT_RANGE
        do i=imin+1,imax
          urhs(i,j)=cff1*ubar(i,j,kstp) +cff2*ubar(i,j,kbak)
     &                                  +cff3*ubar(i,j,kold)
#if defined MRL_WCI && defined MASKING
          urhs(i,j)=urhs(i,j)*umask(i,j)+ust2d(i,j)*(umask(i,j)-1.0)
#endif
          DUon(i,j)=0.5*(Drhs(i,j)+Drhs(i-1,j))*on_u(i,j)*( urhs(i,j)
#ifdef MRL_WCI
     &                                                   + ust2d(i,j)
#endif
     &                                                              )
        enddo
      enddo
#undef JU_EXT_RANGE

      do j=jmin+1,jmax
        do i=IV_EXT_RANGE
          vrhs(i,j)=cff1*vbar(i,j,kstp) +cff2*vbar(i,j,kbak)
     &                                  +cff3*vbar(i,j,kold)
#if defined MRL_WCI && defined MASKING
          vrhs(i,j)=vrhs(i,j)*vmask(i,j)+vst2d(i,j)*(vmask(i,j)-1.0)
#endif
          DVom(i,j)=0.5*(Drhs(i,j)+Drhs(i,j-1))*om_v(i,j)*( vrhs(i,j)
#ifdef MRL_WCI
     &                                                   + vst2d(i,j)
#endif
     &                                                              )
        enddo
      enddo
#undef IV_EXT_RANGE

#ifdef M2_HADV_UP3
# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,
     &                   urhs(START_2D_ARRAY))
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,
     &                   vrhs(START_2D_ARRAY))
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,
     &                   Duon (START_2D_ARRAY))
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,
     &                   Dvom(START_2D_ARRAY))
# endif
#endif

#ifdef OBC_VOLCONS
      call set_DUV_bc_tile (Istr,Iend,Jstr,Jend, Drhs, DUon,DVom)
#endif

#ifdef RVTK_DEBUG_ADVANCED
C$OMP BARRIER
C$OMP MASTER
       call check_tab2d(zeta(:,:,kstp),'zeta step2d #1','r')
       call check_tab2d(ubar(:,:,kstp),'ubar step2d #1','u')
       call check_tab2d(vbar(:,:,kstp),'vbar step2d #1','v')
C$OMP END MASTER       
#endif    
!
!-----------------------------------------------------------------------
! Advance free-surface:   Compute zeta_new, which is at new time
!-------- ---- --------   step, and interpolate half-step backward
! for the subsequent computation of barotropic pressure-gradient
! terms. It should be noted that because Forward Euler step is used 
! to update zeta during the first barotropic step, the pressure term  
! must be computed via Backward step to keep it numerically stable.
! However, this would interfere with the computation of forcing terms
! "rufrc,rvfrc" because computation of pressure gradient in 3D mode
! uses exactly the initial value of "zeta", rather than value changed
! by one barotropic time step.  To resolve this conflict, the
! pressure gradient term computation during the first barotropic
! step is computed in two stages: first use just zeta(:,:,kstp) to
! insure exact consistency with 3D mode; then, after "rufrc,rvfrc"
! are finalized, add a correction term based on the difference
! zeta_new(:,:)-zeta(:,:,kstp) to "rubar,rvbar" to make them
! consistent with Backward step for pressure gradient terms.
!-----------------------------------------------------------------------
!
      if (FIRST_2D_STEP) then
#ifdef SOLVE3D
        cff0=0.                !---> Compute pressure-gradient
        cff1=1.                !  terms using just zeta(:,:,kstp)
#else
        cff0=1.
        cff1=0.
#endif
        cff2=0.
        cff3=0.
      elseif (FIRST_2D_STEP+1) then
        cff0= 1.0833333333333    ! Logically AB2-AM3 forward-backward
        cff1=-0.1666666666666    ! scheme with coefficients chosen for
        cff2= 0.0833333333333    ! maximum stability, while maintaining
        cff3= 0.                 ! third-accuracy; alpha_max=1.73
      else
#ifdef M2FILTER_NONE
        cff0=0.5+2.*myepsilon+mygamma+2.*myalpha
        cff1=1.-cff0-mygamma-myepsilon
        cff2=mygamma
        cff3=myepsilon
#else
        cff0=0.614
        cff1=0.285
        cff2=0.088
        cff3=0.013
#endif
      endif
 
#define zwrk UFx
#define rzeta  UFe
#define rzeta2  VFe
#define rzetaSA VFx
!
!-----------------------------------------------------------------------
! Computes zeta(n+1)
!-----------------------------------------------------------------------
!
      do j=JstrV-1,Jend
        do i=IstrU-1,Iend
#ifdef NBQ
          l_nh=ijk2lq_nh(i,j,N)
#endif
          zeta_new(i,j)=zeta(i,j,kstp) + dtfast*pm(i,j)*pn(i,j)
     &                                   *(DUon(i,j)-DUon(i+1,j  )
     &                                    +DVom(i,j)-DVom(i  ,j+1))
#ifdef NBQ_CONS4
     &        - rhscine_nh(l_nh,2)*dtfast/rho0
#endif
#ifdef ZONAL_NUDGING
          zeta(i,j,knew)=zeta_new(i,j)
#endif
        enddo
      enddo
!
!-----------------------------------------------------------------------
! Add nudging terms
!-----------------------------------------------------------------------
!
#ifdef ZNUDGING
# ifdef ZONAL_NUDGING
      if (iic.eq.ntstart .or. mod(iic,10).eq.0) then
        if (FIRST_2D_STEP) then
          call zonavg_2d(Istr,Iend,Jstr,Jend,
     &                   zeta(START_2D_ARRAY,knew),zetazon)
        endif
      endif
      if (iic.eq.ntstart) then
        if (FIRST_2D_STEP) then
          call zonavg_2d(Istr,Iend,Jstr,Jend,
     &                   ssh(START_2D_ARRAY),sshzon)
        endif
      endif
# endif  /* ZONAL_NUDGING */
      do j=JstrV-1,Jend
        do i=IstrU-1,Iend
          zeta_new(i,j)=zeta_new(i,j) + dtfast*Znudgcof(i,j)
# ifdef ZONAL_NUDGING
     &                                 *(sshzon(j)-zetazon(j))
# else
     &                                 *(ssh(i,j)-zeta_new(i,j))
# endif /* ZONAL_NUDGING */
        enddo
      enddo
#endif /* ZNUDGING */
!
!-----------------------------------------------------------------------
! Computes zetarhs to use in mometum equations
!-----------------------------------------------------------------------
!
#ifdef NBQ
      do j=JstrV-1,Jend
        do i=IstrU-1,Iend
          zeta_new(i,j)=zeta_new(i,j) SWITCH rmask(i,j)
          Dnew(i,j)=zeta_new(i,j)
          zwrk(i,j)=cff0*zeta_new(i,j) /rhobar_nbq(i,j,knew)
     &             +cff1*zeta(i,j,kstp)/rhobar_nbq(i,j,kstp)
     &             +cff2*zeta(i,j,kbak)/rhobar_nbq(i,j,kbak)
     &             +cff3*zeta(i,j,kold)/rhobar_nbq(i,j,kold)
     &             -h(i,j)
# if defined VAR_RHO_2D && defined SOLVE3D
          rzeta(i,j)=(1.+rhoS(i,j))*zwrk(i,j)
          rzeta2(i,j)=rzeta(i,j)*zwrk(i,j)
          rzetaSA(i,j)=zwrk(i,j)*(rhoS(i,j)-rhoA(i,j))
# else
          rzeta(i,j)=zwrk(i,j)
          rzeta2(i,j)=zwrk(i,j)*zwrk(i,j)
# endif
        enddo
      enddo
#else
      do j=JstrV-1,Jend
        do i=IstrU-1,Iend
          zeta_new(i,j)=zeta_new(i,j) SWITCH rmask(i,j)
          Dnew(i,j)=zeta_new(i,j)+h(i,j)
          zwrk(i,j)=cff0*zeta_new(i,j) +cff1*zeta(i,j,kstp)
     &             +cff2*zeta(i,j,kbak)+cff3*zeta(i,j,kold)
# if defined VAR_RHO_2D && defined SOLVE3D
          rzeta(i,j)=(1.+rhoS(i,j))*zwrk(i,j)
          rzeta2(i,j)=rzeta(i,j)*zwrk(i,j)
          rzetaSA(i,j)=zwrk(i,j)*(rhoS(i,j)-rhoA(i,j))
# else
          rzeta(i,j)=zwrk(i,j)
          rzeta2(i,j)=zwrk(i,j)*zwrk(i,j)
# endif
        enddo
      enddo
#endif /* NBQ */
!
!-----------------------------------------------------------------------
! Load new free-surface values into shared array
! Modify new free-surface to ensure that depth is > Dcrit for masked
! cells.
!-----------------------------------------------------------------------
!
      do j=JstrV-1,Jend
        do i=IstrU-1,Iend
          zeta(i,j,knew)=zeta_new(i,j) 
#if defined WET_DRY && defined MASKING
# ifdef NBQ
          zeta(i,j,knew)=zeta(i,j,knew)+ 
     &               rhobar_nbq(i,j,knew)*Dcrit(i,j)*(1.-rmask(i,j))
# else
          zeta(i,j,knew)=zeta(i,j,knew)+ 
     &                   (Dcrit(i,j)-h(i,j))*(1.-rmask(i,j))
# endif
#endif
        enddo
      enddo 
!
!-----------------------------------------------------------------------
! Load rhs values into additional AGRIF shared array for nesting
!-----------------------------------------------------------------------
! 
#ifdef AGRIF
      if (FIRST_2D_STEP) then
        do j=Jstr-1,Jend+1
          do i=Istr-1,Iend+1
            Zt_avg3(i,j,0)=zeta(i,j,kstp)       
          enddo
        enddo 
        do j=JstrR,JendR
          do i=Istr,IendR
          du_avg3(i,j,0)  = DUon(i,j)
          enddo
        enddo 
        do j=Jstr,JendR
          do i=IstrR,IendR
          dv_avg3(i,j,0)  = DVom(i,j)
          enddo
        enddo 
      endif

# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                   Zt_avg3(START_2D_ARRAY,0))
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,
     &                   du_avg3(START_2D_ARRAY,0))
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,
     &                   dv_avg3(START_2D_ARRAY,0))
# endif

# ifdef RVTK_DEBUG_ADVANCED
       if (.not.agrif_Root()) then
C$OMP BARRIER
C$OMP MASTER
       call check_tab2d(Zt_avg3(:,:,0),'Zt_avg3 (index 0) step2d','r')
       call check_tab2d(DU_avg3(:,:,0),'DU_avg3 (index 0) step2d','u')
       call check_tab2d(DV_avg3(:,:,0),'DV_avg3 (index 0) step2d','v')
C$OMP END MASTER  
       endif
# endif  
#endif /* AGRIF */         
!
!-----------------------------------------------------------------------
! Debug zeta
!-----------------------------------------------------------------------
!
#ifdef RVTK_DEBUG_ADVANCED
C$OMP BARRIER
C$OMP MASTER
       call check_tab2d(zeta(:,:,knew),'zeta step2d #2','rint')
C$OMP END MASTER       
#endif       
!
!-----------------------------------------------------------------------
! Compute wet/dry masks
!-----------------------------------------------------------------------
!
#ifdef WET_DRY
      call wetdry_tile (Istr,Iend,Jstr,Jend)
#endif
!
!-----------------------------------------------------------------------
! Set boundary conditions for the free-surface
!-----------------------------------------------------------------------
!
      call zetabc_tile (Istr,Iend,Jstr,Jend)
!
!----------------------------------------------------------------------
! Compute time averaged fields over all short timesteps.
!
! Reset/initialise arrays for averaged fields during the first
! barotropic time step; Accumulate averages after that. Include
! physical boundary points, but not periodic ghost points or
! computation  MPI computational margins.
!----------------------------------------------------------------------
!
#ifdef SOLVE3D
      cff1=weight(1,iif)
      cff2=weight(2,iif)
      if (FIRST_2D_STEP) then
        do j=JstrR,JendR
          do i=IstrR,IendR
            Zt_avg1(i,j)=cff1*zeta(i,j,knew)
            DU_avg1(i,j,nnew)=0.
            DV_avg1(i,j,nnew)=0.
            DU_avg2(i,j)=cff2*DUon(i,j)
            DV_avg2(i,j)=cff2*DVom(i,j)
          enddo
        enddo 
      else
        do j=JstrR,JendR
          do i=IstrR,IendR
            Zt_avg1(i,j)=Zt_avg1(i,j)+cff1*zeta(i,j,knew)
            DU_avg2(i,j)=DU_avg2(i,j)+cff2*DUon(i,j)
            DV_avg2(i,j)=DV_avg2(i,j)+cff2*DVom(i,j)
          enddo
        enddo
      endif
#endif

#ifdef NBQ_CONSOUT
      if (cff1.ne.0) call consout_nbq(20)
#endif
!
!
!======================================================================
! AM4 Backward Step: Computes ubar,vbar at time n+1
!                    first computes rubar,rvbar
!======================================================================
!
!
! Compute pressure-gradient terms  NOTE that "rubar" and "rvbar"
!-------- -------- -------- -----  are computed within the same
! fused loop despite the fact that their normal index ranges are
! different. Fusing loops causes redundant computation of one
! column of "rubar" on the western physical boundary and one row
! of "rvbar" on the southern, but, at the same time it allows to
! share references to array elements (i,j) which results in an
! increase of computational density by almost a factor of 1.5
! resulting in overall more efficient code pipelined in 26 cycles
! (61% of peak speed) on R10000 vs. 16+16 cycles of separate loop
! version for the case when both CPP switches below are defined.
!
      cff=0.5*g
      do j=Jstr,Jend
        do i=Istr,Iend
          rubar(i,j)=cff*on_u(i,j)*( (h(i-1,j)+h(i,j))*(rzeta(i-1,j)
     &                        -rzeta(i,j)) +rzeta2(i-1,j)-rzeta2(i,j)
#if defined VAR_RHO_2D && defined SOLVE3D
     &              +(h(i-1,j)-h(i,j))*( rzetaSA(i-1,j)+rzetaSA(i,j)
     &                        +0.333333333333*(rhoA(i-1,j)-rhoA(i,j))
     &                                      *(zwrk(i-1,j)-zwrk(i,j)))
#endif
#ifdef MRL_WCI
     &                  + ( h(i-1,j)+h(i,j)+rzeta(i-1,j)+rzeta(i,j) )
     &                                       *( sup(i,j)-sup(i-1,j) )
#endif
#if defined POT_TIDES && !defined SOLVE3D
     &                  + ( h(i-1,j)+h(i,j)+rzeta(i-1,j)+rzeta(i,j) )
     &                                   *( Ptide(i,j)-Ptide(i-1,j) )
#endif
     &                                                              )
! 
          rvbar(i,j)=cff*om_v(i,j)*( (h(i,j-1)+h(i,j))*(rzeta(i,j-1)
     &                        -rzeta(i,j)) +rzeta2(i,j-1)-rzeta2(i,j)
#if defined VAR_RHO_2D && defined SOLVE3D
     &              +(h(i,j-1)-h(i,j))*( rzetaSA(i,j-1)+rzetaSA(i,j)
     &                        +0.333333333333*(rhoA(i,j-1)-rhoA(i,j))
     &                                      *(zwrk(i,j-1)-zwrk(i,j)))
#endif
#ifdef MRL_WCI
     &                  + ( h(i,j-1)+h(i,j)+rzeta(i,j-1)+rzeta(i,j) )
     &                                       *( sup(i,j)-sup(i,j-1) )
#endif
#if defined POT_TIDES && !defined SOLVE3D
     &                  + ( h(i,j-1)+h(i,j)+rzeta(i,j-1)+rzeta(i,j) )
     &                                   *( Ptide(i,j)-Ptide(i,j-1) )
#endif
     &                                                              )
        enddo
      enddo            !--> discard  zwrk, rzeta, rzeta2, rzetaSA

#undef rzetaSA
#undef rzeta2
#undef rzeta
#undef zwrk
!
!========================================================================
! Compute horizontal advection terms for momentum equations (2D only)
!-------- ---------- --------- ----- --- -------- --------- --- -----
! NOTE: mathematically necessary (minimal) index ranges for momentum-
! flux components are 
!
!      UFx(IstrU-1:Iend,Jstr:Jend)   VFx(Istr:Iend+1,JstrV:Jend)
!      UFe(IstrU:Iend,Jstr:Jend+1)   VFe(Istr,Iend,JstrV-1,Jend)
!
! however, for the purpose computational efficiency, these ranges are
! unified by suppressing U,V-suffices in order to allow fusion of the
! consecutive loops. This leads to slight increase of the redundant
! computations near western and southern boundaries in non-periodic
! directions. 
!========================================================================
!
#ifdef UV_ADV
!
# if defined SOLVE3D || !defined M2_HADV_UP3
!-----------------------------------------------------------------------
! Centered Second order advection scheme
!
! Numerical diffusion of momentum is implicitely added through 3D
! forcing of advection in rufrc and rvfrc (i.e., diffusion is
! at slow time scale)
!-----------------------------------------------------------------------
      do j=Jstr,Jend
        do i=Istr-1,Iend
          UFx(i,j)=0.25*(DUon(i,j)+DUon(i+1,j))
     &                 *(urhs(i,j)+urhs(i+1,j))

          VFx(i+1,j)=0.25*(DUon(i+1,j)+DUon(i+1,j-1))
     &                   *(vrhs(i+1,j)+vrhs(i,j))
#  ifdef MASKING
     &                                 *pmask(i+1,j)
#  endif
#  ifdef WET_DRY0
     &                                 *pmask_wet(i+1,j)
#  endif
        enddo
      enddo
      do j=Jstr-1,Jend
        do i=Istr,Iend
          VFe(i,j)=0.25*(DVom(i,j)+DVom(i,j+1))
     &                 *(vrhs(i,j)+vrhs(i,j+1))

          UFe(i,j+1)=0.25*(DVom(i,j+1)+DVom(i-1,j+1))
     &                   *(urhs(i,j+1)+urhs(i,j))
#  ifdef MASKING
     &                                 *pmask(i,j+1)
#  endif
#  ifdef WET_DRY0
     &                                 *pmask_wet(i,j+1)
#  endif
        enddo
      enddo
      do j=Jstr,Jend
        do i=Istr,Iend
          rubar(i,j)=rubar(i,j)-UFx(i,j)+UFx(i-1,j)
     &                         -UFe(i,j+1)+UFe(i,j)

          rvbar(i,j)=rvbar(i,j)-VFx(i+1,j)+VFx(i,j)
     &                         -VFe(i,j)+VFe(i,j-1)
        enddo
      enddo !--> discard UFx,VFe,UFe,VFx, DUon,DVom

# else   
!-----------------------------------------------------------------------
!  UP3 (QUICK) spatial advection scheme as in 3D part
!
!  in this case, there is no numerical requirement for explicit viscosity
!-----------------------------------------------------------------------
#  define uxx wrk1
#  define Huxx wrk2
!
#  ifdef EW_PERIODIC
#   define IU_EXT_RANGE IstrU-1,Iend+1
#  else
#   ifdef MPI
        if (WEST_INTER) then
          imin=IstrU-1
        else
          imin=max(IstrU-1,2)
        endif
        if (EAST_INTER) then
          imax=Iend+1
        else
          imax=min(Iend+1,Lmmpi)
        endif
#    define IU_EXT_RANGE imin,imax
#   else
#    define IU_EXT_RANGE max(IstrU-1,2),min(Iend+1,Lm)
#   endif
#  endif

        do j=Jstr,Jend
          do i=IU_EXT_RANGE
            uxx(i,j)=urhs(i-1,j)-2.*urhs(i,j)+urhs(i+1,j)
            Huxx(i,j)=Duon(i-1,j)-2.*Duon(i,j)+Duon(i+1,j)
          enddo
        enddo
#   undef IU_EXT_RANGE
#  ifndef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=Jstr,Jend
            uxx(1,j)=uxx(2,j)
            Huxx(1,j)=Huxx(2,j)
          enddo
        endif
        if (EASTERN_EDGE) then
#   ifdef MPI        
          do j=Jstr,Jend
            uxx(Lmmpi+1,j)=uxx(Lmmpi,j)
            Huxx(Lmmpi+1,j)=Huxx(Lmmpi,j)
          enddo
#   else
          do j=Jstr,Jend
            uxx(Lm+1,j)=uxx(Lm,j)
            Huxx(Lm+1,j)=Huxx(Lm,j)
          enddo
#   endif          
        endif
#  endif

        do j=Jstr,Jend
          do i=IstrU-1,Iend
            cffX=urhs(i,j)+urhs(i+1,j)
            if (cffX.gt.0.) then
              curvX=uxx(i,j)
            else
              curvX=uxx(i+1,j)
            endif
            UFx(i,j)=0.25*(cffX+gamma*curvX)*( Duon(i,j)+
     &               Duon(i+1,j)-0.125*(Huxx(i,j)+Huxx(i+1,j)))
          enddo
        enddo
#  undef Huxx
#  undef uxx
#  define vee wrk1
#  define Hvee wrk2
      
#  ifdef NS_PERIODIC
#   define JV_EXT_RANGE JstrV-1,Jend+1
#  else
#   ifdef MPI
        if (SOUTH_INTER) then
          jmin=JstrV-1
        else
          jmin=max(JstrV-1,2)
        endif
        if (NORTH_INTER) then
          jmax=Jend+1
        else
          jmax=min(Jend+1,Mmmpi)
        endif
#    define JV_EXT_RANGE jmin,jmax
#   else
#    define JV_EXT_RANGE max(JstrV-1,2),min(Jend+1,Mm)
#   endif
#  endif

        do j=JV_EXT_RANGE
          do i=Istr,Iend
            vee(i,j) =vrhs(i,j-1)-2.*vrhs(i,j)+vrhs(i,j+1)
            Hvee(i,j)=Dvom(i,j-1)-2.*Dvom(i,j)+Dvom(i,j+1)
          enddo
        enddo
#   undef JV_EXT_RANGE
#  ifndef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=Istr,Iend
            vee(i,1)=vee(i,2)
            Hvee(i,1)=Hvee(i,2)
          enddo
        endif
        if (NORTHERN_EDGE) then
#   ifdef MPI
          do i=Istr,Iend
            vee(i,Mmmpi+1)=vee(i,Mmmpi)
            Hvee(i,Mmmpi+1)=Hvee(i,Mmmpi)
          enddo
#   else        
          do i=Istr,Iend
            vee(i,Mm+1)=vee(i,Mm)
            Hvee(i,Mm+1)=Hvee(i,Mm)
          enddo
#   endif          
        endif
#  endif
        do j=JstrV-1,Jend
          do i=Istr,Iend
            cffE=vrhs(i,j)+vrhs(i,j+1)
            if (cffE.gt.0.) then
              curvE=vee(i,j)
            else
              curvE=vee(i,j+1)
            endif
            VFe(i,j)=0.25*(cffE+gamma*curvE)*( Dvom(i,j)+
     &               Dvom(i,j+1)-0.125*(Hvee(i,j)+Hvee(i,j+1)))
          enddo
        enddo
#  undef Hvee
#  undef vee
#  define uee wrk1
#  define Hvxx wrk2

#  ifdef NS_PERIODIC
#   define JU_EXT_RANGE Jstr-1,Jend+1
#  else
#   ifdef MPI
        if (SOUTH_INTER) then
          jmin=Jstr-1
        else
          jmin=max(Jstr-1,1)
        endif
        if (NORTH_INTER) then
          jmax=Jend+1
        else
          jmax=min(Jend+1,Mmmpi)
        endif
#    define JU_EXT_RANGE jmin,jmax
#   else
#    define JU_EXT_RANGE max(Jstr-1,1),min(Jend+1,Mm)
#   endif
#  endif

        do j=JU_EXT_RANGE
          do i=IstrU,Iend
            uee(i,j)=urhs(i,j-1)-2.*urhs(i,j)+urhs(i,j+1)
          enddo
        enddo
#   undef JU_EXT_RANGE
#  ifndef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=IstrU,Iend
            uee(i,0)=uee(i,1)
          enddo
        endif
        if (NORTHERN_EDGE) then
#   ifdef MPI
          do i=IstrU,Iend
            uee(i,Mmmpi+1)=uee(i,Mmmpi)
          enddo
#   else        
          do i=IstrU,Iend
            uee(i,Mm+1)=uee(i,Mm)
          enddo
#   endif          
        endif
#  endif
        do j=Jstr,Jend+1
          do i=IstrU-1,Iend
           Hvxx(i,j)=Dvom(i-1,j)-2.*Dvom(i,j)+Dvom(i+1,j)
          enddo
        enddo
        do j=Jstr,Jend+1
          do i=IstrU,Iend
            cffX=urhs(i,j)+urhs(i,j-1)
            cffE=Dvom(i,j)+Dvom(i-1,j)
            if (cffE.gt.0.) then
              curvX=uee(i,j-1)
            else
              curvX=uee(i,j)
            endif
            UFe(i,j)=0.25*(cffX+gamma*curvX)*(cffE-0.125*(
     &                             Hvxx(i,j)+Hvxx(i-1,j) ))
          enddo
        enddo
#  undef Hvxx
#  undef uee
#  define vxx wrk1
#  define Huee wrk2

#  ifdef EW_PERIODIC 
#   define IV_EXT_RANGE Istr-1,Iend+1
#  else
#   ifdef MPI
        if (WEST_INTER) then
          imin=Istr-1
        else
          imin=max(Istr-1,1)
        endif
        if (EAST_INTER) then
          imax=Iend+1
        else
          imax=min(Iend+1,Lmmpi)
        endif
#    define IV_EXT_RANGE imin,imax
#   else
#    define IV_EXT_RANGE max(Istr-1,1),min(Iend+1,Lm)
#   endif
#  endif

        do j=JstrV,Jend
          do i=IV_EXT_RANGE
            vxx(i,j)=vrhs(i-1,j)-2.*vrhs(i,j)+vrhs(i+1,j)
          enddo
        enddo
#   undef IV_EXT_RANGE
#  ifndef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=JstrV,Jend
            vxx(0,j)=vxx(1,j)
          enddo
        endif
        if (EASTERN_EDGE) then
#   ifdef MPI
          do j=JstrV,Jend
            vxx(Lmmpi+1,j)=vxx(Lmmpi,j)
          enddo
#   else        
          do j=JstrV,Jend
            vxx(Lm+1,j)=vxx(Lm,j)
          enddo
#   endif          
        endif
        
#  endif
        do j=JstrV-1,Jend
          do i=Istr,Iend+1
           Huee(i,j)=Duon(i,j-1)-2.*Duon(i,j)+Duon(i,j+1)
          enddo
        enddo
        do j=JstrV,Jend
          do i=Istr,Iend+1
            cffE=vrhs(i,j)+vrhs(i-1,j)
            cffX=Duon(i,j)+Duon(i,j-1)
            if (cffX.gt.0.) then
              curvE=vxx(i-1,j)
            else
              curvE=vxx(i,j)
            endif
            VFx(i,j)=0.25*(cffE+gamma*curvE)*(cffX-0.125*(
     &                             Huee(i,j)+Huee(i,j-1) ))
          enddo
        enddo
#undef Huee
#undef vxx
        do j=Jstr,Jend
          do i=IstrU,Iend
            rubar(i,j)=rubar(i,j)-UFx(i,j  )+UFx(i-1,j)
     &                           -UFe(i,j+1)+UFe(i  ,j)
          enddo
        enddo
        do j=JstrV,Jend
          do i=Istr,Iend
            rvbar(i,j)=rvbar(i,j)-VFx(i+1,j)+VFx(i,j  )
     &                           -VFe(i  ,j)+VFe(i,j-1)
          enddo
        enddo
# endif /* !SOLVE3D && M2_HADV_UP3 */
#endif /* UV_ADV */
!
!-----------------------------------------------------------------------
! Compute Coriolis (2D and 3D) term and advective curvilinear metric
! terms (2D only).
!-----------------------------------------------------------------------
!
#if defined UV_COR || (defined CURVGRID && defined UV_ADV)
      do j=JstrV-1,Jend
        do i=IstrU-1,Iend
          cff=Drhs(i,j)*(
# ifdef UV_COR
     &                   fomn(i,j)
# endif
# if (defined CURVGRID && defined UV_ADV)
     &          +0.5*( dndx(i,j)*(vrhs(i,j)+vrhs(i,j+1))
     &                -dmde(i,j)*(urhs(i,j)+urhs(i+1,j)))
# endif
     &                   )
# ifdef MRL_WCI
#  if defined CURVGRID && defined UV_ADV
          cff1 = Drhs(i,j)*(
     &    0.5*( dndx(i,j)*(vst2d(i,j)+vst2d(i,j+1))
     &          -dmde(i,j)*(ust2d(i,j)+ust2d(i+1,j)) ))
#  else
          cff1 = 0.0
#  endif
          UFx(i,j)=(cff+cff1)*(vrhs(i,j)+vrhs(i,j+1))
     &                 +cff*(vst2d(i,j)+vst2d(i,j+1))
          VFe(i,j)=(cff+cff1)*(urhs(i,j)+urhs(i+1,j))
     &                 +cff*(ust2d(i,j)+ust2d(i+1,j))
# else
          UFx(i,j)=cff*(vrhs(i,j)+vrhs(i,j+1))
          VFe(i,j)=cff*(urhs(i,j)+urhs(i+1,j))
# endif
        enddo
      enddo
      do j=Jstr,Jend
        do i=IstrU,Iend
          rubar(i,j)=rubar(i,j)+0.25*(UFx(i,j)+UFx(i-1,j))
        enddo
      enddo
      do j=JstrV,Jend
        do i=Istr,Iend
          rvbar(i,j)=rvbar(i,j)-0.25*(VFe(i,j)+VFe(i,j-1))
        enddo
      enddo
#endif
!
!-----------------------------------------------------------------------
! Compute horizontal viscous stress terms (2D only).
!-----------------------------------------------------------------------
!
#ifdef SOLVE3D
# undef UV_VIS2
#endif
#ifdef UV_VIS2
      do j=Jstr-1,Jend
        do i=Istr-1,Iend
          cff=2.*Drhs(i,j)*visc2_r(i,j)
          UFx(i,j)=cff*(ubar(i+1,j,kstp)-ubar(i,j,kstp))
     &                                 *pm(i,j)*on_r(i,j)
          VFe(i,j)=cff*(vbar(i,j+1,kstp)-vbar(i,j,kstp))
     &                                 *pn(i,j)*om_r(i,j)

          cff1=0.0625*visc2_p(i+1,j+1)*( Drhs(i,j)
     &       +Drhs(i+1,j)+Drhs(i,j+1)+Drhs(i+1,j+1) )*(
     &          (pn(i+1,j+1)+pn(i,j+1)+pn(i+1,j)+pn(i,j))
     &             *(ubar(i+1,j+1,kstp)-ubar(i+1,j,kstp))
     &         +(pm(i+1,j+1)+pm(i,j+1)+pm(i+1,j)+pm(i,j))
     &             *(vbar(i+1,j+1,kstp)-vbar(i,j+1,kstp))
     &                                                  )
# ifdef MASKING
     &                     *pmask(i+1,j+1)
# endif
          UFe(i+1,j+1)=cff1*om_p(i+1,j+1)
          VFx(i+1,j+1)=cff1*on_p(i+1,j+1)
        enddo
      enddo
      do j=Jstr,Jend
        do i=Istr,Iend
          rubar(i,j)=rubar(i,j)+UFx(i,j)-UFx(i-1,j)
     &                         +UFe(i,j+1)-UFe(i,j)

          rvbar(i,j)=rvbar(i,j)+VFx(i+1,j)-VFx(i,j)
     &                         +VFe(i,j)-VFe(i,j-1)
        enddo
      enddo
#endif /* UV_VIS2 */
!
!-----------------------------------------------------------------------
! Linear and/or quadratic bottom stress.
!-----------------------------------------------------------------------
!
# ifdef BBL
        do j=Jstr,Jend
          do i=IstrU,Iend
            rubar(i,j)=rubar(i,j) - bustr(i,j)
     &                             *om_u(i,j)*on_u(i,j)
          enddo
        enddo
        do j=JstrV,Jend
          do i=Istr,Iend
            rvbar(i,j)=rvbar(i,j) - bvstr(i,j)
     &                             *om_v(i,j)*on_v(i,j)
          enddo
        enddo
# else
      if (rdrg2.gt.0.) then
        do j=Jstr,Jend
          do i=IstrU,Iend
            cff=0.25*( vbar(i  ,j,kstp)+vbar(i  ,j+1,kstp)
     &                +vbar(i-1,j,kstp)+vbar(i-1,j+1,kstp))
 
            rubar(i,j)=rubar(i,j) - ubar(i,j,kstp)*( rdrg+rdrg2
     &              *sqrt(ubar(i,j,kstp)*ubar(i,j,kstp)+cff*cff)
     &                               )*om_u(i,j)*on_u(i,j)
          enddo
        enddo
        do j=JstrV,Jend
          do i=Istr,Iend
            cff=0.25*( ubar(i,j  ,kstp)+ubar(i+1,j  ,kstp)
     &                +ubar(i,j-1,kstp)+ubar(i+1,j-1,kstp))
 
            rvbar(i,j)=rvbar(i,j) - vbar(i,j,kstp)*( rdrg+rdrg2
     &              *sqrt(cff*cff+vbar(i,j,kstp)*vbar(i,j,kstp))
     &                               )*om_v(i,j)*on_v(i,j)
          enddo
        enddo
      else if (rdrg.gt.0.0) then
        do j=Jstr,Jend
          do i=IstrU,Iend
            rubar(i,j)=rubar(i,j) - rdrg*ubar(i,j,kstp)
     &                             *om_u(i,j)*on_u(i,j)
          enddo
        enddo
        do j=JstrV,Jend
          do i=Istr,Iend
            rvbar(i,j)=rvbar(i,j) - rdrg*vbar(i,j,kstp)
     &                             *om_v(i,j)*on_v(i,j)
          enddo
        enddo
      endif
# endif
!
!-----------------------------------------------------------------------
! Add 2D vortex-force terms combined with advection terms
!-----------------------------------------------------------------------
!
#ifdef MRL_WCI
      do j=Jstr,Jend
        do i=IstrU,Iend
          vstu = 0.25*( vst2d(i,j) + vst2d(i,j+1)
     &                 +vst2d(i-1,j)+vst2d(i-1,j+1) )
          dudx = 0.5*( urhs(i+1,j)-urhs(i-1,j) )
          dvdx = 0.5*( vrhs(i,j) - vrhs(i-1,j)
     &                 +vrhs(i,j+1) - vrhs(i-1,j+1) )
          rubar(i,j) = rubar(i,j) + 0.5*om_u(i,j)*
     &                      ( Drhs(i-1,j)+Drhs(i,j) )
     &               *( ust2d(i,j)*dudx + vstu*dvdx )
        enddo
      enddo

      do j=JstrV,Jend
        do i=Istr,Iend
          ustv = 0.25*( ust2d(i,j) + ust2d(i+1,j)
     &                 +ust2d(i,j-1)+ust2d(i+1,j-1) )
          dude = 0.5*( urhs(i,j) - urhs(i,j-1)
     &                 +urhs(i+1,j) - urhs(i+1,j-1) )
ccc          dvde = 0.5*( vrhs(i,j-1)-vrhs(i,j+1) )
          dvde = 0.5*( vrhs(i,j+1)-vrhs(i,j-1) )
          rvbar(i,j) = rvbar(i,j) + 0.5*on_v(i,j)*
     &                      ( Drhs(i,j-1)+Drhs(i,j) )
     &               *( ustv*dude + vst2d(i,j)*dvde )
        enddo
      enddo
#endif
!
!-----------------------------------------------------------------------
! Coupling between 2D and 3D parts.
!--------- ------- -- --- -- ------
! Before the predictor step of the first barotropic time step
! arrays "rufrc" and "rvfrc" contain vertically integrals of the
! 3D right-hand-side terms for the momentum equations (including
! surface and bottom stresses, if so prescribed).
!
! During the first barotropic time step connvert them into forcing
! terms by subtracting the fast-time "rubar" and "rvbar" from them;
! These forcing terms are then extrapolated forward in time using
! optimized Adams-Bashforth weights, so that the resultant rufrc
! and rvfrc are centered effectively at time n+1/2. From now on,
! these newly computed forcing terms will remain constant during
! the fast time stepping and will added to "rubar" and "rvbar"
! during all subsequent barotropic time steps.
!-----------------------------------------------------------------------
!
#ifdef SOLVE3D
      if (FIRST_2D_STEP) then

# ifdef PRED_COUPLED_MODE
        if (FIRST_TIME_STEP) then
          cff3=0.                        ! This version is designed
          cff2=0.                        ! for coupling during 3D
          cff1=1.                        ! predictor sub-step: here
        elseif (FIRST_TIME_STEP+1) then  ! forcing term "rufrc" is
          cff3=0.                        ! computed as instantaneous
          cff2=-0.5                      ! value at 3D time step
          cff1=1.5                       ! "nstp" first, and then
        else                             ! extrapolated half-step
          cff3=0.281105                  ! forward using  AM3-like
          cff2=-0.5-2.*cff3              ! weights optimized for
          cff1=1.5+cff3                  ! maximum stability (with
        endif                            ! special care for startup)
        do j=Jstr,Jend
          do i=IstrU,Iend
            cff=rufrc(i,j)-rubar(i,j)
            rufrc(i,j)=cff1*cff + cff2*rufrc_bak(i,j,3-nstp)
     &                          + cff3*rufrc_bak(i,j,nstp)
            rufrc_bak(i,j,nstp)=cff
          enddo
        enddo
        do j=JstrV,Jend
          do i=Istr,Iend
            cff=rvfrc(i,j)-rvbar(i,j)
            rvfrc(i,j)=cff1*cff + cff2*rvfrc_bak(i,j,3-nstp)
     &                          + cff3*rvfrc_bak(i,j,nstp)
            rvfrc_bak(i,j,nstp)=cff
          enddo
        enddo
#  ifdef NBQ
!
! Extrapolate at n+1/2 internal forcing term for NBQ equations
! (units of rho*Hz*dudt)
!
        do k=1,N
          do j=Jstr,Jend
            do i=IstrU,Iend
              cff=ruint_nbq(i,j,k)
              ruint_nbq(i,j,k)=cff1*cff 
     &                       + cff2*ruint_bak_nbq(i,j,k,3-nstp)
     &                       + cff3*ruint_bak_nbq(i,j,k,nstp)
              ruint_bak_nbq(i,j,k,nstp)=cff
            enddo
          enddo
          do j=JstrV,Jend
            do i=Istr,Iend
              cff=rvint_nbq(i,j,k)
              rvint_nbq(i,j,k)=cff1*cff
     &                       + cff2*rvint_bak_nbq(i,j,k,3-nstp)
     &                       + cff3*rvint_bak_nbq(i,j,k,nstp)
              rvint_bak_nbq(i,j,k,nstp)=cff
            enddo
          enddo
        enddo
        do k=0,N
          do j=Jstr,Jend
            do i=Istr,Iend
              cff=rwint_nbq(i,j,k)
              rwint_nbq(i,j,k)=cff1*cff
     &                       + cff2*rwint_bak_nbq(i,j,k,3-nstp)
     &                       + cff3*rwint_bak_nbq(i,j,k,nstp)
              rwint_bak_nbq(i,j,k,nstp)=cff
            enddo
          enddo
        enddo
#  endif /* NBQ */
# else
        do j=Jstr,Jend                       ! This version is
          do i=Istr,Iend                     ! designed for coupling
            rufrc(i,j)=rufrc(i,j)-rubar(i,j) ! during 3D corrector
            rvfrc(i,j)=rvfrc(i,j)-rvbar(i,j) ! sub-step: no forward
          enddo                              ! extrapolation needs
        enddo                                ! to be performed.
# endif
!
!-----------------------------------------------------------------------
! Since coupling requires that pressure gradient term is computed
! using zeta(:,:,kstp) instead of zeta_new(:,:) needed to achieve
! numerical stability, apply compensation to shift pressure gradient
! terms from "kstp" to "knew": in essense, convert the fist 2D step
! from Forward Euler to Forward-Backward].
!-----------------------------------------------------------------------
!
# define zwrk UFx
# define rzeta  UFe
# define rzeta2  VFe
# define rzetaSA VFx

# ifdef NBQ
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            zwrk(i,j)=zeta_new(i,j) /rhobar_nbq(i,j,knew)
     &               -zeta(i,j,kstp)/rhobar_nbq(i,j,kstp)
#  if defined VAR_RHO_2D && defined SOLVE3D
            rzeta(i,j)=(1.+rhoS(i,j))*zwrk(i,j)
            rzeta2(i,j)=rzeta(i,j)*
     &          (( zeta_new(i,j) /rhobar_nbq(i,j,knew)
     &            +zeta(i,j,kstp)/rhobar_nbq(i,j,kstp) )
     &            -2.*h(i,j))
            rzetaSA(i,j)=zwrk(i,j)*(rhoS(i,j)-rhoA(i,j))
#  else
            rzeta(i,j)=zwrk(i,j)
            rzeta2(i,j)=zwrk(i,j)*
     &           ((zeta_new(i,j) /rhobar_nbq(i,j,knew)
     &            +zeta(i,j,kstp)/rhobar_nbq(i,j,kstp))
     &            -2.*h(i,j))
#  endif
          enddo
        enddo
# else
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            zwrk(i,j)=zeta_new(i,j)-zeta(i,j,kstp)
#  if defined VAR_RHO_2D && defined SOLVE3D
            rzeta(i,j)=(1.+rhoS(i,j))*zwrk(i,j)
            rzeta2(i,j)=rzeta(i,j)*(zeta_new(i,j)+zeta(i,j,kstp))
            rzetaSA(i,j)=zwrk(i,j)*(rhoS(i,j)-rhoA(i,j))
#  else
            rzeta(i,j)=zwrk(i,j)
            rzeta2(i,j)=zwrk(i,j)*(zeta_new(i,j)+zeta(i,j,kstp))
#  endif
          enddo
        enddo
# endif /* NBQ */

        cff=0.5*g
        do j=Jstr,Jend
          do i=Istr,Iend
            rubar(i,j)=rubar(i,j) +cff*on_u(i,j)*( (h(i-1,j)+h(i,j))
     &          *(rzeta(i-1,j)-rzeta(i,j)) +rzeta2(i-1,j)-rzeta2(i,j)
# if defined VAR_RHO_2D && defined SOLVE3D
     &              +(h(i-1,j)-h(i,j))*( rzetaSA(i-1,j)+rzetaSA(i,j)
     &                        +0.333333333333*(rhoA(i-1,j)-rhoA(i,j))
     &                                     *(zwrk(i-1,j)-zwrk(i,j)) )
# endif
     &                                                              )
!
            rvbar(i,j)=rvbar(i,j) +cff*om_v(i,j)*( (h(i,j-1)+h(i,j))
     &          *(rzeta(i,j-1)-rzeta(i,j)) +rzeta2(i,j-1)-rzeta2(i,j)

#  if defined VAR_RHO_2D && defined SOLVE3D
     &              +(h(i,j-1)-h(i,j))*( rzetaSA(i,j-1)+rzetaSA(i,j)
     &                        +0.333333333333*(rhoA(i,j-1)-rhoA(i,j))
     &                                     *(zwrk(i,j-1)-zwrk(i,j)) )
#  endif
     &                                                              )
          enddo
        enddo            !--> discard  zwrk, rzeta, rzeta2, rzetaSA

# undef rzetaSA
# undef rzeta2
# undef rzeta
# undef zwrk

      endif   !<-- FIRST_2D_STEP

#ifdef NBQ
      do k=1,N
        do j=Jstr,Jend
          do i=IstrU,Iend
            ruext_nbq(i,j,k)=(rufrc(i,j)+rubar(i,j))*pm_u(i,j)*pn_u(i,j)
     &                                *(Hz_half(i,j,k)+Hz_half(i-1,j,k))
     &                                          /(Drhs(i,j)+Drhs(i-1,j))
          enddo
        enddo
        do j=JstrV,Jend
          do i=Istr,Iend
            rvext_nbq(i,j,k)=(rvfrc(i,j)+rvbar(i,j))*pm_v(i,j)*pn_v(i,j)
     &                                *(Hz_half(i,j,k)+Hz_half(i,j-1,k))
     &                                          /(Drhs(i,j)+Drhs(i,j-1))
          enddo
        enddo
      enddo
#endif

#endif /* SOLVE3D */

!
!-----------------------------------------------------------------------
! Resolve 3D NBQ equation for ndtnbq time steps
! This provides NBQ-filtered rhs terms for the barotropic equation.
! Then perform 2D filter of these terms to get forcing terms for 
! the total equation.
!-----------------------------------------------------------------------
!
#ifdef NBQ
!
! Resolve 3D NBQ equation for ndtnbq time steps
!
      if (FIRST_2D_STEP) then
        call grid_coef_nh
        call mat_mom_nh
        call mat_cont_nh
      endif

# define WORK UFx
      call step3d_nbq (Istr,Iend,Jstr,Jend, WORK)
# undef WORK

!
! Get filtered rhs terms
! and multiply by dx*dy to get units of rho*Hz*dx*dy*ru
!
        cff1=weight(1,iif)
        cff2=weight(2,iif)
        if (FIRST_2D_STEP) then
          do j=Jstr,Jend
            do i=Istr,Iend
              rhobar_nbq_avg1(i,j)=cff1*rhobar_nbq(i,j,knew)
            enddo
          enddo 
          do k=1,N
            do j=Jstr,Jend
              do i=Istr,Iend
                rho_nbq_avg1(i,j,k)=cff1*rho_nbq_ext(i,j,k)
                rho_nbq_avg2(i,j,k)=cff2*rho_nbq_ext(i,j,k)
              enddo
            enddo 
          enddo
          do k=1,N
            do j=Jstr,Jend
              do i=IstrU,Iend
                ru_nbq_avg1(i,j,k)=cff1*ru_nbq_ext(i,j,k)
                ru_nbq_avg2(i,j,k)=cff2*ru_nbq_ext(i,j,k)
              enddo
            enddo 
          enddo
          do k=1,N
            do j=JstrV,Jend
              do i=Istr,Iend
                rv_nbq_avg1(i,j,k)=cff1*rv_nbq_ext(i,j,k)
                rv_nbq_avg2(i,j,k)=cff2*rv_nbq_ext(i,j,k)
              enddo
            enddo 
          enddo
          do k=0,N
            do j=Jstr,Jend
              do i=Istr,Iend
                rw_nbq_avg1(i,j,k)=cff1*rw_nbq_ext(i,j,k)
                rw_nbq_avg2(i,j,k)=cff2*rw_nbq_ext(i,j,k)
              enddo
            enddo 
          enddo
        else ! not. FIRST_2D_STEP
          do j=Jstr,Jend
            do i=Istr,Iend
              rhobar_nbq_avg1(i,j)=rhobar_nbq_avg1(i,j)
     &                              +cff1*rhobar_nbq(i,j,knew)
            enddo
          enddo 
          do k=1,N
            do j=Jstr,Jend
              do i=Istr,Iend
                rho_nbq_avg1(i,j,k)=rho_nbq_avg1(i,j,k)
     &                              +cff1*rho_nbq_ext(i,j,k)
                rho_nbq_avg2(i,j,k)=rho_nbq_avg2(i,j,k)
     &                              +cff2*rho_nbq_ext(i,j,k)
              enddo
            enddo 
          enddo
          do k=1,N
            do j=Jstr,Jend
              do i=IstrU,Iend
                 ru_nbq_avg1(i,j,k)=ru_nbq_avg1(i,j,k)
     &                              +cff1*ru_nbq_ext(i,j,k)
                 ru_nbq_avg2(i,j,k)=ru_nbq_avg2(i,j,k)
     &                              +cff2*ru_nbq_ext(i,j,k)
              enddo
            enddo 
          enddo
          do k=1,N
            do j=JstrV,Jend
              do i=Istr,Iend
                 rv_nbq_avg1(i,j,k)=rv_nbq_avg1(i,j,k)
     &                              +cff1*rv_nbq_ext(i,j,k)
                 rv_nbq_avg2(i,j,k)=rv_nbq_avg2(i,j,k)
     &                              +cff2*rv_nbq_ext(i,j,k)
              enddo
            enddo 
          enddo
          do k=0,N
            do j=Jstr,Jend
              do i=Istr,Iend
                 rw_nbq_avg1(i,j,k)=rw_nbq_avg1(i,j,k)
     &                              +cff1*rw_nbq_ext(i,j,k)
                 rw_nbq_avg2(i,j,k)=rw_nbq_avg2(i,j,k)
     &                              +cff2*rw_nbq_ext(i,j,k)
              enddo
            enddo 
          enddo
        endif ! FIRST_2D_STEP
#endif

!
!-----------------------------------------------------------------------
! Perform time step for the 2D momentum equations. 

! Also compute fast-time averaged barotropic mass fluxes. 
! Doing so on the fly yields a more computationally dense code and 
! eliminates repeated multiplication by Dnew (since mass fluxes are 
! actually available as volatile variables DUnew,DVnew at this moment. 
! However, as a result of this arrangement, a special code is needed 
! to compute fast-time averages along the physical boundaries, which is 
! done below.
!-----------------------------------------------------------------------
!

#define Dstp DUon
 
      do j=JstrV-1,Jend
        do i=IstrU-1,Iend
#ifdef NBQ
          Dstp(i,j)=zeta(i,j,kstp)
#else
          Dstp(i,j)=zeta(i,j,kstp)+h(i,j)
#endif
        enddo
      enddo

      cff=0.5*dtfast
#ifdef SOLVE3D
      cff1=0.5*weight(1,iif)
#else
      cff2=2.*dtfast
#endif
      do j=Jstr,Jend
        do i=IstrU,Iend
          DUnew=( (Dstp(i,j)+Dstp(i-1,j))*ubar(i,j,kstp)
     &        +cff*(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
#ifdef SOLVE3D
# ifdef NBQ
     &              *(rubar(i,j)+rufrc(i,j)+rubar_nbq(i,j))
# else
     &                             *(rubar(i,j)+rufrc(i,j))
# endif
#else
     &                          *rubar(i,j)+cff2*sustr(i,j)
# ifdef MRL_WCI
     &                                    +cff2*brk2dx(i,j)
#  ifdef WAVE_STREAMING
     &                                    +cff2*frc2dx(i,j)
#  endif
# endif
#endif
     &                                                    )
#ifdef MASKING
     &                                         *umask(i,j)
#endif
#ifdef WET_DRY
          cff1_WD=ABS(ABS(umask_wet(i,j))-1.)
          cff2_WD=0.5+SIGN(0.5,DUnew)*umask_wet(i,j)
          umask_wet(i,j)=0.5*umask_wet(i,j)*cff1_WD
     &                         +cff2_WD*(1.-cff1_WD)
          DUnew=DUnew*umask_wet(i,j)
# ifdef MRL_WCI
          ust2d(i,j)=ust2d(i,j)*umask_wet(i,j)
# endif
#endif
          ubar(i,j,knew)=DUnew/(Dnew(i,j)+Dnew(i-1,j))
#ifdef SOLVE3D
          DU_avg1(i,j,nnew)=DU_avg1(i,j,nnew) +cff1*on_u(i,j)*( DUnew
# ifdef MRL_WCI
     &                 +(Dnew(i,j)+Dnew(i-1,j))*ust2d(i,j)
# endif
     &                                                   )
#endif
        enddo
      enddo
         
      do j=JstrV,Jend
        do i=Istr,Iend
          DVnew=( (Dstp(i,j)+Dstp(i,j-1))*vbar(i,j,kstp)
     &        +cff*(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
#ifdef SOLVE3D
# ifdef NBQ
     &              *(rvbar(i,j)+rvfrc(i,j)+rvbar_nbq(i,j))
# else
     &                             *(rvbar(i,j)+rvfrc(i,j))
# endif
#else
     &                          *rvbar(i,j)+cff2*svstr(i,j)
# ifdef MRL_WCI
     &                                    +cff2*brk2de(i,j)
#  ifdef WAVE_STREAMING
     &                                    +cff2*frc2de(i,j)
#  endif
# endif
#endif
     &                                                    )
#ifdef MASKING
     &                                         *vmask(i,j)
#endif
#ifdef WET_DRY
          cff1_WD=ABS(ABS(vmask_wet(i,j))-1.)
          cff2_WD=0.5+SIGN(0.5,DVnew)*vmask_wet(i,j)
          vmask_wet(i,j)=0.5*vmask_wet(i,j)*cff1_WD
     &                        +cff2_WD*(1.-cff1_WD)
          DVnew=DVnew*vmask_wet(i,j)
# ifdef MRL_WCI
          vst2d(i,j)=vst2d(i,j)*vmask_wet(i,j)
# endif
#endif
          vbar(i,j,knew)=DVnew/(Dnew(i,j)+Dnew(i,j-1))
#ifdef SOLVE3D
          DV_avg1(i,j,nnew)=DV_avg1(i,j,nnew) +cff1*om_v(i,j)*(DVnew
# ifdef MRL_WCI
     &                 +(Dnew(i,j)+Dnew(i,j-1))*vst2d(i,j)
# endif
     &                                                   )
#endif
        enddo
      enddo
!
!-----------------------------------------------------------------------
!  Set 2D Momemtum nudging
!-----------------------------------------------------------------------
!
#if defined M2NUDGING && defined M2CLIMATOLOGY

# ifdef ZONAL_NUDGING
      if (iic.eq.ntstart .or. mod(iic,10).eq.0) then
        if (FIRST_2D_STEP) then
          call zonavg_2d(Istr,Iend,Jstr,Jend,
     &                   ubar(START_2D_ARRAY,knew),ubzon)
          call zonavg_2d(Istr,Iend,Jstr,Jend,
     &                   vbar(START_2D_ARRAY,knew),vbzon)
        endif
      endif
      if (iic.eq.ntstart) then
        if (FIRST_2D_STEP) then
          call zonavg_2d(Istr,Iend,Jstr,Jend,
     &                   ubclm(START_2D_ARRAY),ubclmzon)
          call zonavg_2d(Istr,Iend,Jstr,Jend,
     &                   vbclm(START_2D_ARRAY),vbclmzon)
        endif
      endif
# endif /* ZONAL_NUDGING */

      do j=Jstr,Jend
        do i=IstrU,Iend
# ifdef ZONAL_NUDGING        
          DUnew = dtfast*M2nudgcof(i,j)*(ubclmzon(j)-ubzon(j))
# else          
          DUnew = dtfast*M2nudgcof(i,j)*(ubclm(i,j)-ubar(i,j,knew))
# endif
# ifdef MASKING
     &                 * umask(i,j)
# endif 
# ifdef WET_DRY
     &                 * umask_wet(i,j)
# endif
          ubar(i,j,knew)=ubar(i,j,knew) + DUnew
# ifdef SOLVE3D
          DU_avg1(i,j,nnew)=DU_avg1(i,j,nnew) +cff1*DUnew*
     &                         (Dnew(i,j)+Dnew(i-1,j))*on_u(i,j)
# endif
        enddo
      enddo
      do j=JstrV,Jend
        do i=Istr,Iend
# if defined ZONAL_NUDGING  
          DVnew = dtfast*M2nudgcof(i,j)*(vbclmzon(j)-vbzon(j)) 
# else
          DVnew = dtfast*M2nudgcof(i,j)*(vbclm(i,j)-vbar(i,j,knew))    
# endif      
# ifdef MASKING
     &                 * vmask(i,j)
# endif 
# ifdef WET_DRY
     &                 * vmask_wet(i,j)
# endif
          vbar(i,j,knew)=vbar(i,j,knew) + DVnew
# ifdef SOLVE3D
          DV_avg1(i,j,nnew)=DV_avg1(i,j,nnew) +cff1*DVnew*
     &                         (Dnew(i,j)+Dnew(i,j-1))*om_v(i,j)
# endif

        enddo
      enddo
#endif
!
!-----------------------------------------------------------------------
!  Body force for the Internal Tide test case
!-----------------------------------------------------------------------
!
#if defined INTERNAL || defined BODYTIDE
      omega=2.*pi/(12.4*3600.)
      U0=0.02
      do j=Jstr,Jend
        do i=IstrU,Iend
          DUnew = dtfast*omega*U0*cos(omega*time)
# ifdef MASKING
     &                 * umask(i,j)
# endif
# ifdef WET_DRY
     &                 * umask_wet(i,j)
# endif
          ubar(i,j,knew)=ubar(i,j,knew) + DUnew
#ifdef SOLVE3D
          DU_avg1(i,j,nnew)=DU_avg1(i,j,nnew) +cff1*DUnew*
     &                         (Dnew(i,j)+Dnew(i-1,j))*on_u(i,j)
#endif
        enddo
      enddo
      do j=JstrV,Jend
        do i=Istr,Iend
          DVnew = dtfast*0.5*(f(i,j)+f(i,j-1))*
     &                   U0*sin(omega*time)
# ifdef MASKING
     &                 * vmask(i,j)
# endif
# ifdef WET_DRY
     &                 * vmask_wet(i,j)
# endif
          vbar(i,j,knew)=vbar(i,j,knew) + DVnew
#ifdef SOLVE3D
          DV_avg1(i,j,nnew)=DV_avg1(i,j,nnew) +cff1*DVnew*
     &                         (Dnew(i,j)+Dnew(i,j-1))*om_v(i,j)
#endif

        enddo
      enddo
#endif     
!
!-----------------------------------------------------------------------
! Set boundary conditions and compute integral mass flux accross
! all open boundaries, if any.
!-----------------------------------------------------------------------
!
      call u2dbc_tile (Istr,Iend,Jstr,Jend, UFx) 
      call v2dbc_tile (Istr,Iend,Jstr,Jend, UFx)

#ifdef OBC_VOLCONS
      call obc_flux_tile (Istr,Iend,Jstr,Jend)
#endif
!
!-----------------------------------------------------------------------
! Compute fast-time averaged barotropic mass fluxes along physical
! boundaries.
!-----------------------------------------------------------------------
!
#ifdef SOLVE3D
# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        do j=Jstr-1,JendR
          Dnew(Istr-1,j)=h(Istr-1,j)+zeta(Istr-1,j,knew)
        enddo
      endif
      if (EASTERN_EDGE) then
        do j=Jstr-1,JendR
          Dnew(Iend+1,j)=h(Iend+1,j)+zeta(Iend+1,j,knew)
        enddo
      endif
# endif
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do i=Istr-1,IendR
          Dnew(i,Jstr-1)=h(i,Jstr-1)+zeta(i,Jstr-1,knew)
        enddo
      endif
      if (NORTHERN_EDGE) then
        do i=Istr-1,IendR
          Dnew(i,Jend+1)=h(i,Jend+1)+zeta(i,Jend+1,knew)
        enddo
      endif
# endif
      cff1=0.5*weight(1,iif)
# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        do j=JstrR,JendR
          DU_avg1(IstrU-1,j,nnew)=DU_avg1(IstrU-1,j,nnew)
     &         +cff1*(Dnew(IstrU-1,j)
     &         +Dnew(IstrU-2,j))*(ubar(IstrU-1,j,knew)
# ifdef MRL_WCI
     &                                             +ust2d(IstrU-1,j)
# endif
     &                                             )*on_u(IstrU-1,j)
        enddo
        do j=JstrV,Jend
          DV_avg1(Istr-1,j,nnew)=DV_avg1(Istr-1,j,nnew)
     &       +cff1*(Dnew(Istr-1,j)
     &       +Dnew(Istr-1,j-1) )*(vbar(Istr-1,j,knew)
# ifdef MRL_WCI
     &                                              +vst2d(Istr-1,j)
# endif
     &                                              )*om_v(Istr-1,j)
        enddo
      endif
        
      if (EASTERN_EDGE) then
        do j=JstrR,JendR
          DU_avg1(Iend+1,j,nnew)=DU_avg1(Iend+1,j,nnew)
     &            +cff1*( Dnew(Iend+1,j)
     &            +Dnew(Iend,j) )*(ubar(Iend+1,j,knew)
# ifdef MRL_WCI
     &                                              +ust2d(Iend+1,j)
# endif
     &                                              )*on_u(Iend+1,j)
        enddo
        do j=JstrV,Jend
          DV_avg1(Iend+1,j,nnew)=DV_avg1(Iend+1,j,nnew)
     &        +cff1*( Dnew(Iend+1,j)
     &        +Dnew(Iend+1,j-1) )*(vbar(Iend+1,j,knew)
# ifdef MRL_WCI
     &                                              +vst2d(Iend+1,j)
# endif
     &                                              )*om_v(Iend+1,j)
        enddo
      endif
# endif
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do i=IstrU,Iend
          DU_avg1(i,Jstr-1,nnew)=DU_avg1(i,Jstr-1,nnew)
     &        +cff1*( Dnew(i,Jstr-1)
     &        +Dnew(i-1,Jstr-1) )*(ubar(i,Jstr-1,knew)
# ifdef MRL_WCI
     &                                              +ust2d(i,Jstr-1)
# endif
     &                                              )*on_u(i,Jstr-1)
        enddo
        do i=IstrR,IendR
          DV_avg1(i,JstrV-1,nnew)=DV_avg1(i,JstrV-1,nnew)
     &         +cff1*(Dnew(i,JstrV-1)
     &         +Dnew(i,JstrV-2))*(vbar(i,JstrV-1,knew)
# ifdef MRL_WCI
     &                                              +vst2d(i,JstrV-1)
# endif
     &                                              )*om_v(i,JstrV-1)
        enddo
      endif
      if (NORTHERN_EDGE) then
        do i=IstrU,Iend
          DU_avg1(i,Jend+1,nnew)=DU_avg1(i,Jend+1,nnew)
     &        +cff1*( Dnew(i,Jend+1)
     &        +Dnew(i-1,Jend+1) )*(ubar(i,Jend+1,knew)
# ifdef MRL_WCI
     &                                               +ust2d(i,Jend+1)
# endif
     &                                               )*on_u(i,Jend+1)
        enddo
        do i=IstrR,IendR
          DV_avg1(i,Jend+1,nnew)=DV_avg1(i,Jend+1,nnew)
     &            +cff1*( Dnew(i,Jend+1)
     &            +Dnew(i,Jend) )*(vbar(i,Jend+1,knew)
# ifdef MRL_WCI
     &                                               +vst2d(i,Jend+1)
# endif
     &                                               )*om_v(i,Jend+1)
        enddo
      endif
# endif 
#endif
!
!-----------------------------------------------------------------------
! Apply point sources for river runoff simulations
!-----------------------------------------------------------------------
!
#ifdef PSOURCE
      do is=1,Nsrc 
# ifdef MPI
        i=Isrc_mpi(is,mynode)
        j=Jsrc_mpi(is,mynode)
# else
        i=Isrc(is)
        j=Jsrc(is)
# endif
        if (IstrR.le.i .and. i.le.IendR .and.
     &      JstrR.le.j .and. j.le.JendR) then
          if (Dsrc(is).eq.0) then
            ubar(i,j,knew)=2.*Qbar(is)/( on_u(i,j)
     &                   *(Dnew(i-1,j)+Dnew(i,j)) )
# ifdef SOLVE3D
            DU_avg1(i,j,nnew)=Qbar(is)
# endif
          else
            vbar(i,j,knew)=2.*Qbar(is)/( om_v(i,j)
     &                   *(Dnew(i,j-1)+Dnew(i,j)) )
# ifdef SOLVE3D
            DV_avg1(i,j,nnew)=Qbar(is)
# endif
          endif
        endif
      enddo
#endif
!
!-----------------------------------------------------------------------
!  Diagnostics
!-----------------------------------------------------------------------
!
#ifndef SOLVE3D
      call diag_tile (Istr,Iend,Jstr,Jend, UFx,UFe)
#endif
!
!-----------------------------------------------------------------------
!  Exchange boundary information.
!-----------------------------------------------------------------------
!
#if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                   zeta(START_2D_ARRAY,knew))
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,
     &                   ubar(START_2D_ARRAY,knew))
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,
     &                   vbar(START_2D_ARRAY,knew))
# if defined MRL_WCI && defined WET_DRY
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,
     &                   ust2d(START_2D_ARRAY))
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,
     &                   vst2d(START_2D_ARRAY))
# endif
#endif
!
!-----------------------------------------------------------------------
!  Debugging ubar,vbar
!-----------------------------------------------------------------------
!
#ifdef RVTK_DEBUG_ADVANCED
C$OMP BARRIER
C$OMP MASTER
       call check_tab2d(ubar(:,:,knew),'ubar step2d #2','u')
       call check_tab2d(vbar(:,:,knew),'vbar step2d #2','v')
C$OMP END MASTER       
#endif   
!
!-----------------------------------------------------------------------
!  Apply conservation requirements for nesting
!-----------------------------------------------------------------------
!
#ifdef AGRIF
      if (.NOT.Agrif_Root()) THEN
        do j=Jstr-1,Jend+1
          do i=Istr-1,Iend+1
            Zt_avg3(i,j,iif)=zeta(i,j,knew)
          enddo
        enddo 
        do j=JstrR,JendR
          do i=Istr,IendR
            DU_avg3(i,j,iif) = 0.5*(h(i,j)+zeta(i,j,knew)+
     &          h(i-1,j)+zeta(i-1,j,knew)) *on_u(i,j)*ubar(i,j,knew)
          enddo
        enddo 
        do j=Jstr,JendR
          do i=IstrR,IendR
            DV_avg3(i,j,iif) = 0.5*(h(i,j)+zeta(i,j,knew)+
     &          h(i,j-1)+zeta(i,j-1,knew)) *om_v(i,j)*vbar(i,j,knew)
          enddo
        enddo
 
# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                        Zt_avg3(START_2D_ARRAY,iif))
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,
     &                        DU_avg3(START_2D_ARRAY,iif))
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,
     &                        DV_avg3(START_2D_ARRAY,iif))
# endif

# ifdef RVTK_DEBUG_ADVANCED
C$OMP BARRIER
C$OMP MASTER
       call check_tab2d(Zt_avg3(:,:,iif),'Zt_avg3 step2d','r')
       call check_tab2d(DU_avg3(:,:,iif),'DU_avg3 step2d','u')
       call check_tab2d(DV_avg3(:,:,iif),'DV_avg3 step2d','v')
C$OMP END MASTER       
# endif   
      endif
# ifdef AGRIF_CONSERV_VOL
       if (iif == nfast) then
       if (agrif_root()) then
         do j=JstrR,JendR
           do i=IstrR,IendR
             DU_avg1(i,j,5) = dt * DU_avg2(i,j)
             DV_avg1(i,j,5) = dt * DV_avg2(i,j)
           enddo
         enddo
       else
         do j=JstrR,JendR
           do i=IstrR,IendR
             DU_avg1(i,j,5) = dt * DU_avg2(i,j)
             DV_avg1(i,j,5) = dt * DV_avg2(i,j)
             DU_avg1(i,j,4) = DU_avg1(i,j,4) + DU_avg1(i,j,5)
             DV_avg1(i,j,4) = DV_avg1(i,j,4) + DV_avg1(i,j,5)
           enddo
         enddo       
       endif
       endif
# endif
#endif /* AGRIF */
!
!-----------------------------------------------------------------------
!  TEST FOR CFL VIOLATION. IF SO, PRINT AND STOP
!-----------------------------------------------------------------------
!
      VMAXL=100.
      VMAX=0.
      do j=Jstr,Jend
        do i=Istr,Iend
          cff1=ubar(i,j,knew)
          cff2=vbar(i,j,knew)
          cff=max(abs(cff1),abs(cff2))
          IF (cff.GE.VMAX .or. cff1.ne.cff1 .or. cff2.ne.cff2) THEN
            IF (cff.GE.VMAX .and. cff1.eq.cff1 .and. cff2.eq.cff2) THEN 
              VMAX=cff
            ELSE
              VMAX=666.
            ENDIF
#ifdef MPI
            imax=i+iminmpi-1
            jmax=j+jminmpi-1
#else
            imax=i
            jmax=j
#endif
          ENDIF
        enddo
      enddo
      IF (VMAX.GT.VMAXL) THEN
        write(stdout,'(9(A/))')
     &     '                                         ',
     &     '                                         ',
     &     ' ======================================= ',
     &     ' =                                     = ',
     &     ' =   STEP2D:   ABNORMAL JOB END        = ',
     &     ' =                 BLOW UP             = ',
     &     ' =                                     = ',
     &     ' ======================================= ',
     &     '                                         '
        if (VMAX.eq.666.) then
          write(stdout,'(A,F10.2)')
     &                                            '  VMAX (M/S) =   NaN'
        else
          write(stdout,'(A,F10.2)') 
     &                                            '  VMAX (M/S) =',VMAX
        endif
        write(stdout,'(A,2I6)')  
     &                                       '  IMAX JMAX  =',imax,jmax
#ifdef SOLVE3D
        write(stdout,'(A,2I6/)')
     &                                         '  IINT IEXT  =',iic,iif
#else
        write(stdout,'(A,I6/)')    '  IIC        =',iic
#endif
        may_day_flag=1
#ifdef MPI
        call mpi_abort (MPI_COMM_WORLD, err)
#else
        stop                            !-->  EXIT
#endif
      ENDIF

!       ! Fin CN
!       if (iic.eq.10) then
!#ifdef MPI       
!       call MPI_Finalize (i)
!       if (mynode.eq.0) write(6,*) 'fin step2d'
!#else
!       write(6,*) 'fin step2d'
!#endif       
!       stop 
!       endif

#if defined DEBUG_NBQ && defined MPI
!        if (iic.eq.10) then
!         call MPI_Finalize (i)
!         if (mynode.eq.0) write(6,*) 'Stop in step2d: debug NBQ'
!         stop
!        endif
#endif

  
      return
      end
