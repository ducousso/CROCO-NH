! $Id: omega.F 1458 2014-02-03 15:01:25Z gcambon $
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
!======================================================================
!
      subroutine omega (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call omega_tile (Istr,Iend,Jstr,Jend, A2d(1,1,trd)
# ifdef VADV_ADAPT_IMP
     &                                     ,A2d(1,2,trd)
# endif
     &                                                  )
      return
      end
!======================================================================
!
      subroutine omega_tile (Istr,Iend,Jstr,Jend, wrk
# ifdef VADV_ADAPT_IMP
     &      ,Cu_adv
# endif
     &                      )      
!======================================================================
!
! Compute S-coordinate vertical velocity, w=[Hz/(m*n)]*omega [m^3/s],
! which has the meaning of FINITE_VOLUME WATER FLUX through MOVING
! grid-box interfaces of RHO-boxes. 
! 
! To do so, integrate divergence of horizontal mass fluxes from
! bottom up, starting with the no-normal flow boundary condition at 
! the bottom (k=0); Once this operation is done, W(:,:,N) contains
! the vertical velocity at the free surface, which is, in its turn,
! the temporal tendency of the free surface, d_zeta/d_t;
! In order to compute the S-coordinate vertical velocity, one
! needs to subtract the vertical velocities of moving S-coordinate
! isosurfaces, which are proportional the product of d_zeta/d_t
! and the fraction of the distance from the point to the bottom 
! divided by the total depth of water column, i.e. the whole
! S-coordinate system is "brethes" by linear in Z-space expansion
! and contraction set by free-surface variation.
!
! VADV_ADAPT_IMP: Adaptive implicit vertical advection:
! ----------------------------------------------------
! Shchepetkin A.F., 2015: An adaptive, Courant-number-dependent
! implicit scheme for vertical advection in oceanic modeling,
! Ocean Modelling, 91, 38-69.
!
! c_min: Courant number threshold value for fully explicit advection
! c_max: Courant number threshold value for fully implicit advection
! between c_min and c_max: mixed advection
! if the scheme is only applied at corrector step, c_max takes on
! a slightly different value for stability reasons
!
!======================================================================
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
# include "param.h"

# include "nbq.h" 
      real wrk   (PRIVATE_1D_SCRATCH_ARRAY    )
      real wrk2  (PRIVATE_1D_SCRATCH_ARRAY    )
# ifdef VADV_ADAPT_IMP      
      real cu_adv(PRIVATE_1D_SCRATCH_ARRAY,0:N)
      real gamma,Cdt,cff,my_cu_adv,c_min,c_max,cu_cut,fcu
#  ifdef VADV_ADAPT_PRED
      parameter(c_min=0.6,c_max=1.0,cu_cut=2.*c_max-c_min,
     &          fcu=4.*c_max*(c_max-c_min))
#  else
      parameter(c_min=0.6,c_max=0.95,cu_cut=2.*c_max-c_min,
     &          fcu=4.*c_max*(c_max-c_min))
#  endif
# endif
# ifdef MOVING_BATHY
      real cmorph
# endif

# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
!
# ifdef MPI
#  define LOCALLM Lmmpi
#  define LOCALMM Mmmpi
# else
#  define LOCALLM Lm
#  define LOCALMM Mm
# endif

# include "compute_auxiliary_bounds.h"
!
!--------------------------------------------------------
!
# ifdef VADV_ADAPT_IMP
      gamma = 0.08333333333333
      if(nrhs.eq.3) then
        cdt =     dt                 !<-- Corrector
      elseif(FIRST_TIME_STEP) then
        cdt = 0.5*dt
      else
        cdt = (1.-2*gamma)*dt        !<-- Predictor
      endif
# endif
# ifdef MOVING_BATHY
      if(nrhs.eq.3) then
        cmorph=1.                    !<-- Corrector
      else
        cmorph=0.                    !<-- Predictor
      endif
# endif
!
!---------------------------------------------------------
! 
      do j=Jstr,Jend
!
!---------------------------------------------------------
!  Integrate divergence of horizontal mass fluxes
!---------------------------------------------------------
!
        do i=Istr,Iend 
# ifdef VADV_ADAPT_IMP
          Wi(i,j,0)=0.
# endif
# ifdef MOVING_BATHY
          We(i,j,0)=cmorph*om_r(i,j)*on_r(i,j)*dh(i,j)/dt
# else
          We(i,j,0)=0.
#  ifdef NBQ_CONS2
          We2(i,j,0)=0.
#  endif
# endif
        enddo
        do k=1,N,+1
          do i=Istr,Iend
            We(i,j,k)=We(i,j,k-1) -Huon(i+1,j,k)+Huon(i,j,k)
     &                            -Hvom(i,j+1,k)+Hvom(i,j,k)
# ifdef NBQ_CONS1
     &                     -dHzdt(i,j,k)*om_r(i,j)*on_r(i,j)
# endif
# ifdef NBQ_CONS2
            We2(i,j,k)=We2(i,j,k-1) -Huon(i+1,j,k)+Huon(i,j,k)
     &                              -Hvom(i,j+1,k)+Hvom(i,j,k)
# endif
!
!---------------------------------------------------------
!  Compute the horizontal Courant number
!---------------------------------------------------------
!
# ifdef VADV_ADAPT_IMP
            Cu_adv(i,k)=max(HUon(i+1,j,k),0.)-min(Huon(i,j,k),0.) 
     &                 +max(Hvom(i,j+1,k),0.)-min(Hvom(i,j,k),0.)
# endif
          enddo
        enddo
!
!---------------------------------------------------------
!  Subtract vertical velocities of moving S-coordinate 
!  isosurfaces
!---------------------------------------------------------
!
        do i=Istr,Iend
          wrk(i)=We(i,j,N)/(z_w(i,j,N)-z_w(i,j,0))
        enddo
        do k=N-1,1,-1
          do i=Istr,Iend
            We(i,j,k)=We(i,j,k)-wrk(i)*(z_w(i,j,k)-z_w(i,j,0))
          enddo
        enddo
        do i=Istr,Iend
          We(i,j,N)=0.
# ifdef VADV_ADAPT_IMP
          Wi(i,j,N)=0.
# endif
        enddo
               
# ifdef NBQ_CONS2
        do i=Istr,Iend
          dzdt_nbq(i,j,N) = (We2(i,j,N)*pm(i,j)*pn(i,j)
     &                        - (z_w(i,j,N)-z_w(i,j,0))*drhobardt(i,j))
     &                        / rhobar_nbq_int(i,j)
        enddo
        do k=N-1,1,-1 
          do i=Istr,Iend
            dzdt_nbq(i,j,k) =  dzdt_nbq(i,j,N)*(z_w(i,j,k)-z_w(i,j,0))
     &                                        /(z_w(i,j,N)-z_w(i,j,0))  
          enddo
        enddo
# endif /* NBQ_CONS2 */
!
#ifdef VADV_ADAPT_IMP
!---------------------------------------------------------
! Compute the explicit and implicit components of Omega
! based on local Courant number Cu_adv
!---------------------------------------------------------
!
        do k=1,N
          do i=Istr,Iend
            cff = cdt*pm(i,j)*pn(i,j)/Hz(i,j,k)
            Cu_adv(i,k)=cff*(
     &                   Cu_adv(i,k)
     &                 + max(We(i,j,k),0.)-min(We(i,j,k-1),0.)
     &                       )        !<-- 3D Advective Courant number
          enddo
        enddo

        do k=1,N-1
          do i=istr,iend
             my_cu_adv = max(Cu_adv(i,k+1),Cu_adv(i,k))
             if(my_cu_adv.lt.c_min) then           !<-- Fully explicit
                 cff = 0.
             elseif(my_cu_adv.lt.cu_cut) then      !<-- Mixed explicit
                 cff = (my_cu_adv-c_min)**2
                 cff = cff / (fcu+cff)
             else                                  !<-- Fully implicit
                 cff = (my_cu_adv-c_max)/my_cu_adv
             endif
             Wi(i,j,k) =     cff *We(i,j,k)
             We(i,j,k) = (1.-cff)*We(i,j,k)
          enddo
        enddo
#endif

      enddo  ! j loop
!
!---------------------------------------------------------      
!  Set lateral boundary conditions.
!---------------------------------------------------------
!
# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        do k=0,N
          do j=Jstr,Jend
            We(0,j,k)=We(1,j,k)
#ifdef VADV_ADAPT_IMP
            Wi(0,j,k)=Wi(1,j,k)
#endif
          enddo
        enddo
      endif
      if (EASTERN_EDGE) then
        do k=0,N
          do j=Jstr,Jend
            We(LOCALLM+1,j,k)=We(LOCALLM,j,k)
#ifdef VADV_ADAPT_IMP            
            Wi(LOCALLM+1,j,k)=Wi(LOCALLM,j,k)            
#endif            
          enddo
        enddo
      endif
# endif
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do k=0,N
          do i=Istr,Iend
            We(i,0,k)=We(i,1,k)
#ifdef VADV_ADAPT_IMP            
            Wi(i,0,k)=Wi(i,1,k)            
#endif             
          enddo
        enddo
      endif
      if (NORTHERN_EDGE) then
        do k=0,N
          do i=Istr,Iend
            We(i,LOCALMM+1,k)=We(i,LOCALMM,k)
#ifdef VADV_ADAPT_IMP 
            Wi(i,LOCALMM+1,k)=Wi(i,LOCALMM,k)
#endif 
          enddo
        enddo
      endif
#  ifndef EW_PERIODIC
      if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=0,N
          We(0,0,k)=We(1,1,k)
#ifdef VADV_ADAPT_IMP           
          Wi(0,0,k)=Wi(1,1,k)
#endif           
        enddo
      endif
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        do k=0,N
          We(0,LOCALMM+1,k)=We(1,LOCALMM,k)
#ifdef VADV_ADAPT_IMP   
          Wi(0,LOCALMM+1,k)=Wi(1,LOCALMM,k)        
#endif           
        enddo
      endif
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=0,N
          We(LOCALLM+1,0,k)=We(LOCALLM,1,k)
#ifdef VADV_ADAPT_IMP 
          Wi(LOCALLM+1,0,k)=Wi(LOCALLM,1,k)
#endif 
        enddo
      endif
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        do k=0,N
          We(LOCALLM+1,LOCALMM+1,k)=We(LOCALLM,LOCALMM,k)
#ifdef VADV_ADAPT_IMP           
          Wi(LOCALLM+1,LOCALMM+1,k)=Wi(LOCALLM,LOCALMM,k)
#endif          
        enddo
      endif
#  endif
# endif

# if defined EW_PERIODIC || defined NS_PERIODIC  || defined MPI
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend, We(START_2D_ARRAY,0))
#ifdef VADV_ADAPT_IMP       
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend, Wi(START_2D_ARRAY,0))
#endif      
# endif
      return
      end
#else
      subroutine omega_empty
      return
      end
#endif /* SOLVE3D */
