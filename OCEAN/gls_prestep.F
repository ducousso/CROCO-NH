! $Id: gls_prestep.F 1511 2014-04-02 16:31:48Z gcambon $
!
!======================================================================
! CROCO is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al)
! and Rutgers University (Arango et al) are under MIT/X style license.
! CROCO specific routines (nesting) are under CeCILL-C license.
!
! CROCO website : http://www.croco-ocean.org/
!======================================================================
!
! Umlauf and Burchard (2003) have come up with a generic two-equation
! turbulence closure scheme which can be tuned to behave like several
! of the traditional schemes, including that of Mellor and Yamada 2.5.
! This is known as the Generic Length Scale, or GLS vertical mixing
! scheme and was introduced to ROMS by Warner et al. (2005).
! Warner's implementation contains 4 schemes: 
!           k-kl (~ MY2.5), k-epsilon, k-omega, k-gen
!
! References: 
! 
! Umlauf, L. and H. Burchard, 2001:  A generic length-scale 
!   Equation for geophysical turbulence models. 
!
! Warner, J. C., C. R. Sherwood, H. G. Arango, and R. P. Signell,
!   2005: Performance of four turbulence closure methods implemented
!   using a generic length scale method, Ocean Modelling, 8, 81-113
!
! This file contains the computations of GLS turbulent closure
! at predictor step. It was adapted from the RUTGERS code 
! by S. Herbette and P. Marchesiello, 2013
!
!======================================================================
!
#include "cppdefs.h"
#if defined SOLVE3D && defined GLS_MIXING
# define GLS_VADV_AKIMA

!**********************************************************************
      subroutine gls_prestep (tile)
!
      implicit none
      integer tile,  trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "ocean3d.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call gls_prestep_tile (Istr, Iend, Jstr, Jend,
     &                                                   A3d(1, 3,trd),
     &        A2d(1,1,trd), A2d(1,2,trd), A2d(1, 3,trd), A2d(1, 4,trd),
     &        A2d(1,5,trd), A2d(1,6,trd), A2d(1, 7,trd), A2d(1, 8,trd), 
     &                      A2d(1,9,trd), A2d(1,10,trd), A2d(1,11,trd))
      return
      end
!
!**********************************************************************
      subroutine gls_prestep_tile (Istr, Iend, Jstr, Jend, 
     &                             Hz_half,
     &                             FC, CF, FCL, CFL,
     &                             FX, FE, FXL, FEL,
     &                             DC, WORK, WORKL) 
!
!--------------------------------------------------------------------
! Preliminary step: initialize computations of the new time step
! 3D primitive variables.
!
! The actual time step will be completed in step3d_gls, after the
! time step [n] r.h.s. terms and new-time step Hz will be available
! after completing rhs3d computations and 2D (barotropic
! mode) computations.
!--------------------------------------------------------------------
!
      implicit none
# include "param.h"
      integer Istr,Iend,Jstr,Jend, i, j,k, indx
     &       ,imin,imax,jmin,jmax
      real cff, cffl, cff1, cff2, gamma, epsil
      real FC(PRIVATE_1D_SCRATCH_ARRAY,0:N),  
     &     FCL(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     CF(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     CFL(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     DC(PRIVATE_1D_SCRATCH_ARRAY,0:N),  
     &     FX(PRIVATE_2D_SCRATCH_ARRAY),  
     &     FE(PRIVATE_2D_SCRATCH_ARRAY),
     &     FXL(PRIVATE_2D_SCRATCH_ARRAY),
     &     FEL(PRIVATE_2D_SCRATCH_ARRAY),
     &     WORK(PRIVATE_2D_SCRATCH_ARRAY),
     &     WORKL(PRIVATE_2D_SCRATCH_ARRAY),
     &     Hz_half(PRIVATE_2D_SCRATCH_ARRAY,N)
      parameter (gamma=1./6., epsil=1.E-16)
      real cx, cy, dft, dfx, dfy, tau, tau_in, tau_out
# include "grid.h"
# include "ocean3d.h"
# include "coupling.h"
# include "ocean2d.h"
# include "forces.h"
# include "mixing.h"
# include "scalars.h"
# ifdef MASKING
#  define SWITCH *
# else
#  define SWITCH !
# endif
!
!
# include "compute_auxiliary_bounds.h"
!
      indx=3-nstp
      
! ======= =======
! Compute Hz_half
! ======= =======

      if (FIRST_TIME_STEP) then
        cff=0.5*dt
        cff1=1.
        cff2=0.
      else
        cff=(1.-gamma)*dt
        cff1=0.5+gamma
        cff2=0.5-gamma
      endif
      do k=1,N
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            Hz_half(i,j,k)=cff1*Hz(i,j,k)+cff2*Hz_bak(i,j,k)
     &        -cff*pm(i,j)*pn(i,j)*( Huon(i+1,j,k)-Huon(i,j,k)
     &                              +Hvom(i,j+1,k)-Hvom(i,j,k)
     &                                  +We(i,j,k)-We(i,j,k-1)
     &                                                       )
          enddo
        enddo
      enddo
!
! Start computation of the auxiliary tke and gls fields.
!--------------------------------------------------------
!  Once it will be completed, tke(:,:,???,:) and gls(:,:,???,:)
!  are effectively halfway between time steps n and n+1. A high spatial 
!  order, centered, non-conservative [but constancy preserving!] scheme 
!  is used for this auxiliary step. This is done by introducing an 
!  artificial continuity equation [Easter, 1993].
!
!  Since this field will be used exclussively to compute the high-
!  order fluxes during subsequent step3d_t operation, the final
!  values of tke/gls(i,j,k,??) alfer step3d_gls will be computed in
!  a flux-conservative manner. The overall time step will be both 
!  conservative and constancy preserving.
!
!  This preliminary step shall be done before field tke/gls(:,:,:,???,:)
!  loses its meaningful values during the pre-step operation.
!
!===============================================================
!
! Compute horizontal advection (4th order centered)
!
!===============================================================
!
      do k=1,N
!
#  define grad WORK
#  define gradL WORKL
#  ifdef EW_PERIODIC
#   define I_EXT_RANGE Istr-1,Iend+2
#  else
#   ifdef MPI
          if (WEST_INTER) then
            imin=Istr-1
          else
            imin=max(Istr-1,1)
          endif
          if (EAST_INTER) then
            imax=Iend+2
          else
            imax=min(Iend+2,Lmmpi+1)
          endif
#    define I_EXT_RANGE imin,imax
#   else
#    define I_EXT_RANGE max(Istr-1,1),min(Iend+2,Lm+1)
#   endif
#  endif
# ifdef NS_PERIODIC
#  define J_EXT_RANGE Jstr-1,Jend+2
# else
#  ifdef MPI
          if (SOUTH_INTER) then
            jmin=Jstr-1
          else
            jmin=max(Jstr-1,1)
          endif
          if (NORTH_INTER) then
            jmax=Jend+2
          else
            jmax=min(Jend+2,Mmmpi+1)
          endif
#   define J_EXT_RANGE jmin,jmax
#  else
#   define J_EXT_RANGE max(Jstr-1,1),min(Jend+2,Mm+1)
#  endif
# endif

!
! FX/FXL (tke/gls)
!----------------------------------------------------
! Within the domain
        do j=Jstr,Jend
          do i=I_EXT_RANGE
            FX(i,j)=(tke(i,j,k,nstp)-tke(i-1,j,k,nstp))
#  ifdef MASKING
     &                                               *umask(i,j)
#  endif
            FXL(i,j)=(gls(i,j,k,nstp)-gls(i-1,j,k,nstp))
# ifdef MASKING
     &                                               *umask(i,j)
# endif
            enddo
          enddo
#  undef I_EXT_RANGE
! On the edges
#  ifndef EW_PERIODIC 
          if (WESTERN_EDGE) then
            do j=Jstr,Jend
              FX(0,j)=FX(1,j)
              FXL(0,j)=FXL(1,j)
            enddo
          endif
          if (EASTERN_EDGE) then
#   ifdef MPI
            do j=Jstr,Jend
              FX(Lmmpi+2,j)=FX(Lmmpi+1,j)
              FXL(Lmmpi+2,j)=FXL(Lmmpi+1,j)
            enddo
#   else
             do j=Jstr,Jend
              FX(Lm+2,j)=FX(Lm+1,j)
              FXL(Lm+2,j)=FXL(Lm+1,j)
            enddo
#   endif
          endif
#  endif
!
! and grad/gradL(FX/FXL)
!----------------------------------------------------
! Within the domain
          do j=Jstr,Jend
            do i=Istr-1,Iend+1
              grad(i,j)=0.5*(FX(i+1,j)+FX(i,j))
              gradL(i,j)=0.5*(FXL(i+1,j)+FXL(i,j))
            enddo
          enddo             !--> discard FX
!
! Compute FX/FXL (tke/gls)
!----------------------------------------------------
          do j=Jstr,Jend
            do i=Istr,Iend+1
              FX(i,j)=0.5*( tke(i,j,k,nstp)+tke(i-1,j,k,nstp)
     &                     -0.333333333333*(grad(i,j)-grad(i-1,j))
     &                                               )*Huon(i,j,k)

              FXL(i,j)=0.5*( gls(i,j,k,nstp)+gls(i-1,j,k,nstp)
     &                     -0.333333333333*(gradL(i,j)-gradL(i-1,j))
     &                                               )*Huon(i,j,k)
            enddo
          enddo            !--> discard grad
!
! FE/FEL (tke/gls)
!----------------------------------------------------
! Within the domain
          do j=J_EXT_RANGE
            do i=Istr,Iend
              FE(i,j)=(tke(i,j,k,nstp)-tke(i,j-1,k,nstp))
#  ifdef MASKING
     &                                               *vmask(i,j)
#  endif
              FEL(i,j)=(gls(i,j,k,nstp)-gls(i,j-1,k,nstp))
# ifdef MASKING
     &                                               *vmask(i,j)
# endif
            enddo
          enddo
#  undef J_EXT_RANGE
! On the edges
#  ifndef NS_PERIODIC
          if (SOUTHERN_EDGE) then
            do i=Istr,Iend
              FE(i,0)=FE(i,1)
              FEL(i,0)=FEL(i,1)
            enddo
          endif
          if (NORTHERN_EDGE) then
#   ifdef MPI
            do i=Istr,Iend
              FE(i,Mmmpi+2)=FE(i,Mmmpi+1)
              FEL(i,Mmmpi+2)=FEL(i,Mmmpi+1)
            enddo
#   else
            do i=Istr,Iend
              FE(i,Mm+2)=FE(i,Mm+1)
              FEL(i,Mm+2)=FEL(i,Mm+1)
            enddo
#   endif
          endif
#  endif
!
! grad/gradL(FE/FEL)
!----------------------------------------------------
! Within the domain
          do j=Jstr-1,Jend+1
            do i=Istr,Iend
              grad(i,j)=0.5*(FE(i,j+1)+FE(i,j))
              gradL(i,j)=0.5*(FEL(i,j+1)+FEL(i,j))
            enddo
          enddo            !--> discard FE
!
! Compute FE/FEL (tke/gls)
!----------------------------------------------------
          do j=Jstr,Jend+1
            do i=Istr,Iend
            FE(i,j)=0.5*( tke(i,j,k,nstp)+tke(i,j-1,k,nstp)
     &                     -0.333333333333*(grad(i,j)-grad(i,j-1))
     &                                               )*Hvom(i,j,k)
            FEL(i,j)=0.5*( gls(i,j,k,nstp)+gls(i,j-1,k,nstp)
     &                     -0.333333333333*(gradL(i,j)-gradL(i,j-1))
     &                                               )*Hvom(i,j,k)
#  undef grad
#  undef gradL
            enddo
          enddo            !--> discard grad
!
!--------------------------------------------------------
! Finalize horizontal advection: compute flux divergences
!--------------------------------------------------------
! ======= === ==== =====
! tke/gls new time step:
! ======= === ==== =====
!
          if (FIRST_TIME_STEP) then
            cff=0.5*dt
            do j=Jstr,Jend
              do i=Istr,Iend
                tke(i,j,k,nnew)=Hz(i,j,k)*tke(i,j,k,nstp)
     &                      -cff*pm(i,j)*pn(i,j)*( FX(i+1,j)-FX(i,j)
     &                                            +FE(i,j+1)-FE(i,j))
                gls(i,j,k,nnew)=Hz(i,j,k)*gls(i,j,k,nstp)
     &                      -cff*pm(i,j)*pn(i,j)*( FXL(i+1,j)-FXL(i,j)
     &                                            +FEL(i,j+1)-FEL(i,j))
              enddo
            enddo
          else
            cff=(1.-gamma)*dt
            cff1=0.5+gamma
            cff2=0.5-gamma
            do j=Jstr,Jend
              do i=Istr,Iend
                tke(i,j,k,nnew)=cff1*Hz(i,j,k)*tke(i,j,k,nstp)
     &                         +cff2*Hz_bak(i,j,k)*tke(i,j,k,indx)
     &                      -cff*pm(i,j)*pn(i,j)*( FX(i+1,j)-FX(i,j)
     &                                            +FE(i,j+1)-FE(i,j))
                gls(i,j,k,nnew)=cff1*Hz(i,j,k)*gls(i,j,k,nstp)
     &                         +cff2*Hz_bak(i,j,k)*gls(i,j,k,indx)
     &                      -cff*pm(i,j)*pn(i,j)*( FXL(i+1,j)-FXL(i,j)
     &                                            +FEL(i,j+1)-FEL(i,j))
              enddo
            enddo
          endif

        enddo   ! <-- k

!
! Continue computation of the auxiliary tracer field: auxiliary
! continuity equation (the same for all tracers): DC=1/Hz_half_new,
! where Hz_half_new is Hz at time step n+1/2 as it would be computed
! from three-dimensional divergence of volume fluxes Huon,Hvom and W.
!
!===============================================================
!
! Compute vertical advection
!
!===============================================================
!
! Finalize computation of the auxiliary tracer field: compute its
! change due to vertical advection. Computation of vertical advective
! fluxes requires interpolation of tracer values to the verical grid-
! box interfaces (W-points). This can be done by either using
! parabolic spline interpolation or, more simple local cubic
! polynomial [linear interpolation is considered obsolete].
!
! ======== =====
! Vertical Flux:
! ======== =====
!
! DC (Hz_half)
!----------------------------------------------------
      do j=Jstr,Jend
        do k=1,N
          do i=Istr,Iend
            DC(i,k)=1./Hz_half(i,j,k)
          enddo
        enddo
!
! Vertical flux using parabolic splines
!----------------------------------------------------
! Construc parabolic splines
# ifdef GLS_VADV_SPLINES
                              ! Construct parabolic splines: here
          do i=Istr,Iend      ! CF is the set of vertical derivatives
            FC(i,0)=0.        ! of the tke/gls field t(:,:,:,nstp,:),
            CF(i,0)=0.        ! FC is an auxiliary scratch variable. 
          enddo
          do k=1,N-1,+1
            do i=Istr,Iend
              cff=1./(2.*Hz(i,j,k+1)+Hz(i,j,k)*(2.-FC(i,k-1)))
              FC(i,k)=cff*Hz(i,j,k+1)        ! FCL ????
              CF(i,k)=cff*( 6.*( tke(i,j,k+1,nstp)
     &               -tke(i,j,k,nstp))-Hz(i,j,k)*CF(i,k-1))
              CFL(i,k)=cff*( 6.*( gls(i,j,k+1,nstp)
     &               -gls(i,j,k,nstp))-Hz(i,j,k)*CFL(i,k-1))
          enddo
          enddo
          do i=Istr,Iend
            CF(i,N)=0.
            CFL(i,N)=0.
          enddo
          do k=N-1,1,-1       !<-- irreversible
! brutal part JC
            do i=Istr,Iend
            CF(i,k)=CF(i,k)-FC(i,k)*CF(i,k+1)
            CFL(i,k)=CFL(i,k)-FC(i,k)*CFL(i,k+1)    ! FCL ????
          enddo
        enddo               !--> discard FC, keep CF
! Compute vertical advective fluxes 
        cff=1./3.           ! Compute vertical advective fluxes 
        do k=1,N-1          ! FC=W*[spline-interpolated tracer]
          do i=Istr,Iend
            FC(i,k)=We(i,j,k)*( tke(i,j,k,nstp)+cff*Hz(i,j,k)
     &                                   *(CF(i,k)+0.5*CF(i,k-1)) )
            FCL(i,k)=We(i,j,k)*( gls(i,j,k,nstp)+cff*Hz(i,j,k)
     &                                   *(CFL(i,k)+0.5*CFL(i,k-1)) )
          enddo
        enddo               !--> discard CF
        do i=Istr,Iend
          FC(i,N)=0.
          FCL(i,N)=0.
          FC(i,0)=0.
          FCL(i,0)=0.
        enddo

!
! Compute vertical advective fluxes using 4th-order Akima scheme
!---------------------------------------------------------------
# elif defined GLS_VADV_AKIMA
        do k=1,N-1
          do i=istr,iend
            FC(i,k)=tke(i,j,k+1,nstp)-tke(i,j,k,nstp)
            FCL(i,k)=gls(i,j,k+1,nstp)-gls(i,j,k,nstp)
          enddo
        enddo
        do i=istr,iend
          FC(i,0)=FC(i,1)
          FC(i,N)=FC(i,N-1)
          FCL(i,0)=FCL(i,1)
          FCL(i,N)=FCL(i,N-1)
        enddo
        do k=1,N
          do i=istr,iend
            cff=2.*FC(i,k)*FC(i,k-1)
            cffL=2.*FCL(i,k)*FCL(i,k-1)
            if (cff.gt.epsil) then
              CF(i,k)=cff/(FC(i,k)+FC(i,k-1))
            else
              CF(i,k)=0.
            endif
            if (cffL.gt.epsil) then
              CFL(i,k)=cffL/(FCL(i,k)+FCL(i,k-1))
            else
              CFL(i,k)=0.
            endif
          enddo
        enddo            !--> discard FC
        do k=1,N-1
          do i=istr,iend
            FC(i,k)=0.5*( tke(i,j,k,nstp)+tke(i,j,k+1,nstp)
     &                   -0.333333333333*(CF(i,k+1)-CF(i,k)) )*We(i,j,k)
            FCL(i,k)=0.5*( gls(i,j,k,nstp)+gls(i,j,k+1,nstp)
     &                 -0.333333333333*(CFL(i,k+1)-CFL(i,k)) )*We(i,j,k)
          enddo
        enddo            !--> discard CF
        do i=istr,iend
          FC(i,0)=0.
          FC(i,N)=0.
          FCL(i,0)=0.
          FCL(i,N)=0.
        enddo

!
! Compute vertical advective fluxes using 2th-order centered scheme
!------------------------------------------------------------------
# elif GLS_VADV_C2
        do k=1,N-1
          do i=Istr,Iend
            FC(i,k)=0.5*We(i,j,k)*(tke(i,j,k  ,nstp)
     &                            +tke(i,j,k+1,nstp))
            FCL(i,k)=0.5*We(i,j,k)*(gls(i,j,k  ,nstp)
     &                             +gls(i,j,k+1,nstp))
          enddo
        enddo
        do i=Istr,Iend
          FC(i, 0)=0.
          FC(i,N )=0.
          FCL(i, 0)=0.
          FCL(i,N )=0.
       enddo

!
! Compute vertical advective fluxes using 4th-order centered scheme
!------------------------------------------------------------------
# else
        do k=2,N-2
          do i=Istr,Iend
            FC(i,k)=We(i,j,k)*(
     &                     0.58333333333333*( tke(i,j,k  ,nstp)
     &                                       +tke(i,j,k+1,nstp))
     &                    -0.08333333333333*( tke(i,j,k-1,nstp)
     &                                       +tke(i,j,k+2,nstp))
     &                                                            )
            FCL(i,k)=We(i,j,k)*(
     &                     0.58333333333333*( gls(i,j,k  ,nstp)
     &                                       +gls(i,j,k+1,nstp))
     &                    -0.08333333333333*( gls(i,j,k-1,nstp)
     &                                       +gls(i,j,k+2,nstp))
     &                                                            )
          enddo
        enddo
        do i=Istr,Iend
          FC(i, 0)=0.0
          FC(i,  1)=We(i,j,  1)*(     0.5*tke(i,j,  1,nstp)
     &                       +0.58333333333333*tke(i,j,  2,nstp)
     &                       -0.08333333333333*tke(i,j,  3,nstp)
     &                                                            )
          FC(i,N-1)=We(i,j,N-1)*(     0.5*tke(i,j,N  ,nstp)
     &                       +0.58333333333333*tke(i,j,N-1,nstp)
     &                       -0.08333333333333*tke(i,j,N-2,nstp)
     &                                                            )
          FC(i,N )=0.0
          FCL(i, 0)=0.0
          FCL(i,  1)=We(i,j,  1)*(     0.5*gls(i,j,  1,nstp)
     &                       +0.58333333333333*gls(i,j,  2,nstp)
     &                       -0.08333333333333*gls(i,j,  3,nstp)
     &                                                            )
          FCL(i,N-1)=We(i,j,N-1)*(     0.5*gls(i,j,N  ,nstp)
     &                       +0.58333333333333*gls(i,j,N-1,nstp)
     &                       -0.08333333333333*gls(i,j,N-2,nstp)
     &                                                            )
          FCL(i,N )=0.0
        enddo
# endif

! ======= === ==== =====
! tke/gls new time step:
! ======= === ==== =====
        if (FIRST_TIME_STEP) then
          cff=0.5*dt
        else
          cff=(1.-gamma)*dt
        endif
        do k=1,N
          do i=Istr,Iend
            tke(i,j,k,nnew)=DC(i,k)*( tke(i,j,k,nnew)
     &               -cff*pm(i,j)*pn(i,j)*(FC(i,k)-FC(i,k-1)))
            gls(i,j,k,nnew)=DC(i,k)*( gls(i,j,k,nnew)
     &               -cff*pm(i,j)*pn(i,j)*(FCL(i,k)-FCL(i,k-1)))
          enddo
        enddo            !--> discard FC
      enddo

!---------------------------------------------------------------
! Apply lateral boundary conditions.
!---------------------------------------------------------------
!
! End of the subfunction with the update of tke and gls distributions
! along the edges via extrapolation in "gls_bc.h"
!
# define grad WORK
# define tmp tke
# undef  KV_UPDATE
# include "gls_bc.h"
# undef grad
# undef tmp
# define grad WORKL
# define tmp gls
# undef KV_UPDATE
# include "gls_bc.h"
# undef grad
# undef tmp

!---------------------------------------------------------------
! Exchange computational margines and/or periodic boundaries:
!---------------------------------------------------------------
! Exchange of data for a parallel simulatoion
!
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend,
     &                        tke(START_2D_ARRAY,0,nnew))
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend,
     &                        gls(START_2D_ARRAY,0,nnew))
# endif


#else
      subroutine gls_prestep_empty
#endif
      return
      end
