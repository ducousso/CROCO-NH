! $Id: pre_step3d.F 1568 2014-06-30 15:57:49Z gcambon $
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
# ifndef VADV_ADAPT_PRED
#  undef VADV_ADAPT_IMP
# endif

      subroutine pre_step3d (tile)
!
      implicit none
      integer tile,  trd,omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "ocean3d.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call pre_step3d_tile (Istr,Iend,Jstr,Jend,
     &                A3d(1,1,trd), A3d(1,2,trd), A3d(1,5,trd),
     &                A2d(1,1,trd), A2d(1,2,trd), A2d(1,3,trd),
# ifdef VADV_ADAPT_IMP     
     &                                            A2d(1,4,trd), 
# endif           
     &                A2d(1,1,trd), A2d(1,2,trd), A2d(1,3,trd)
# ifndef NBQ
     &                                          , A3d(1,3,trd)
# endif
     &                                                        )
      return
      end

      subroutine pre_step3d_tile (Istr,Iend,Jstr,Jend, ru,rv,rw,
     &                                                 FC,CF,DC, 
# ifdef VADV_ADAPT_IMP     
     &                                                       WC, 
# endif     
     &                                               FX,FE,WORK
# ifndef NBQ
     &                                                 ,Hz_half
# endif
     &                                                         )  
!
!--------------------------------------------------------------------
! Preliminary step: initialize computations of the new time step
! 3D primitive variables.
!
! Since r.h.s. arrays ru,rv,rt(:,:,???[,:]), which at this moment
! contain r.h.s at time step n-2 will be overwritten by the
! subsequent routines within rhs3d driver, both [n-1 and n-2] old-
! time-step r.h.s. term in Adams-Bashforth stepping scheme are
! added at this time to the time step [n] fields and the result is
! stored as u,v,t(:,:,???[,:]).
!
! The actual time step will be completed in step3d, after the
! time step [n] r.h.s. terms and new-time step Hz will be available
! after the completion rhs3d computations and the 2D (barotropic
! mode) computations.
!--------------------------------------------------------------------
!
#ifdef NBQ
      use module_nh
      use module_nbq
#endif
#ifdef NHMG
      USE nhmg, only : nhmg_solve
      USE mg_grids
      use mg_tictoc
# ifdef NHMG_DEBUG
      USE nhmg_debug, only : nhmg_write_pred_in
      USE nhmg_debug, only : nhmg_write_pred_inter1
      USE nhmg_debug, only : nhmg_write_pred_inter2
# endif
#endif
      implicit none
# include "param.h"
      integer Istr,Iend,Jstr,Jend, itrc, i,j,k, indx
     &       ,imin,imax,jmin,jmax,nadv,iAkt
# ifdef PSOURCE
     &       ,is
# endif
      real   ru(PRIVATE_2D_SCRATCH_ARRAY,N),    cff,
     &       rv(PRIVATE_2D_SCRATCH_ARRAY,N),    cff1,
     &       rw(PRIVATE_2D_SCRATCH_ARRAY,0:N),     
     &       FC(PRIVATE_1D_SCRATCH_ARRAY,0:N),  cff2,
     &       CF(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &       DC(PRIVATE_1D_SCRATCH_ARRAY,0:N),  gamma, 
     &       FX(PRIVATE_2D_SCRATCH_ARRAY),      epsil,
     &       FE(PRIVATE_2D_SCRATCH_ARRAY),        cdt,
     &     WORK(PRIVATE_2D_SCRATCH_ARRAY)
# ifdef VADV_ADAPT_IMP      
      real   WC(PRIVATE_1D_SCRATCH_ARRAY,0:N) 
# endif 
# ifndef NBQ
      real Hz_half(PRIVATE_2D_SCRATCH_ARRAY,N)
# endif
# ifdef WET_DRY
      real cff3,cff4,cff5
#  endif
      parameter (gamma=1./6., epsil=1.E-16)
# include "grid.h"
# include "ocean3d.h"
# include "coupling.h"
# include "ocean2d.h"
# include "forces.h"
# include "mixing.h"
# include "scalars.h"
# ifdef PSOURCE
#  include "sources.h"
# endif
# ifdef NBQ
#  include "nbq.h"
# endif
# ifdef MASKING
#  define SWITCH *
# else
#  define SWITCH !
# endif
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
# if defined TS_HADV_UP5 ||  defined TS_HADV_C6 \
      || defined TS_HADV_WENO5 ||  defined BIO_HADV_WENO5 \
      || defined TS_VADV_WENO5

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
!
!--------------------------------------------------------------------
!
# include "compute_auxiliary_bounds.h"
!
      indx=3-nstp
      nadv=  nstp   !<-- do not remove: used in compute_tracer_fluxes.h

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
# ifdef VADV_ADAPT_IMP     
     &                                  +Wi(i,j,k)-Wi(i,j,k-1)     
# endif     
     &                                                       )
          enddo
        enddo
      enddo
!
!--------------------------------------------------------------------
!
# ifdef NBQ
      do k=1,N
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            Hzr_half_nbq(i,j,k)=Hz_half(i,j,k)/
     &                          (1.5*rho_nbq_avg1(i,j,k)
     &                          -0.5*rho_nbq_avg2(i,j,k))
          enddo
        enddo
      enddo
!
!  Set lateral boundary conditions for Hz_half
!
#  ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        do k=0,N
          do j=Jstr,Jend
            Hzr_half_nbq(0,j,k)=Hzr_half_nbq(1,j,k)
          enddo
        enddo
      endif
      if (EASTERN_EDGE) then
        do k=0,N
          do j=Jstr,Jend
            Hzr_half_nbq(LOCALLM+1,j,k)=Hzr_half_nbq(LOCALLM,j,k)
          enddo
        enddo
      endif
#  endif
#  ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do k=0,N
          do i=Istr,Iend
            Hzr_half_nbq(i,0,k)=Hzr_half_nbq(i,1,k)
          enddo
        enddo
      endif
      if (NORTHERN_EDGE) then
        do k=0,N
          do i=Istr,Iend
            Hzr_half_nbq(i,LOCALMM+1,k)=Hzr_half_nbq(i,LOCALMM,k)
          enddo
        enddo
      endif
#  endif
#  ifndef EW_PERIODIC
      if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=0,N
          Hzr_half_nbq(0,0,k)=Hzr_half_nbq(1,1,k)
        enddo
      endif
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        do k=0,N
          Hzr_half_nbq(0,LOCALMM+1,k)=Hzr_half_nbq(1,LOCALMM,k)
        enddo
      endif
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=0,N
          Hzr_half_nbq(LOCALLM+1,0,k)=Hzr_half_nbq(LOCALLM,1,k)
        enddo
      endif
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        do k=0,N
          Hzr_half_nbq(LOCALLM+1,LOCALMM+1,k)=
     &                     Hzr_half_nbq(LOCALLM,LOCALMM,k)
        enddo
      endif
#  endif
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                        Hzr_half_nbq(START_2D_ARRAY,1))
#  endif
!
!  Set extended range
!
#  ifdef EW_PERIODIC
      imin=Istr-2
      imax=Iend+2
#  else
      if (WESTERN_EDGE) then
        imin=Istr-1
      else
        imin=Istr-2
      endif
      if (EASTERN_EDGE) then
        imax=Iend+1
      else
        imax=Iend+2
      endif
#  endif
#  ifdef NS_PERIODIC
      jmin=Jstr-2
      jmax=Jend+2
#  else
      if (SOUTHERN_EDGE) then
        jmin=Jstr-1
      else
        jmin=Jstr-2
      endif
      if (NORTHERN_EDGE) then
        jmax=Jend+1
      else
        jmax=Jend+2
      endif
#  endif
!
!  Compute other vertical grid variables at n+1/2
!
      do j=jmin,jmax
        do i=imin,imax
           zw_half_nbq(i,j,0)=-h(i,j)
           zr_half_nbq(i,j,1)=zw_half_nbq(i,j,0)
     &                        +0.5*Hzr_half_nbq(i,j,1)
        enddo
      enddo
      do k=1,N
        do j=jmin,jmax
          do i=imin,imax
            zw_half_nbq(i,j,k)=zw_half_nbq(i,j,k-1)+Hzr_half_nbq(i,j,k)
          enddo
        enddo
      enddo
      do k=2,N
        do j=jmin,jmax
          do i=imin,imax
            zr_half_nbq(i,j,k)=zr_half_nbq(i,j,k-1)
     &                  +0.5*(Hzr_half_nbq(i,j,k)+Hzr_half_nbq(i,j,k-1))
          enddo
        enddo
      enddo
      do k=1,N-1
        do j=jmin,jmax
          do i=imin,imax
            Hzw_half_nbq(i,j,k)=z_r(i,j,k+1)-z_r(i,j,k)
        enddo
      enddo
      enddo
      do j=jmin,jmax
        do i=imin,imax
          Hzr_half_nbq(i,j,0  )=0.
          Hzr_half_nbq(i,j,N+1)=0.
          Hzw_half_nbq(i,j,0  )=z_r(i,j,1)-z_w(i,j,0)
          Hzw_half_nbq(i,j,N  )=z_w(i,j,N)-z_r(i,j,N)
        enddo
      enddo
!
!  Initialize NBQ rhs mometum terms
!
      qdm_u_ext(:,:,0) = 0.

# endif /* NBQ */
!
!--------------------------------------------------------------------
!  Start computation of the auxiliary tracer field
!  -----------------------------------------------
!  Once it will be completed, t(:,:,???,:) is effectively halfway
!  between time steps n and n+1. A high spatial order, centered,
!  non-conservative [but constancy preserving!] scheme is used for
!  this auxiliary step. This is done by introducing an artificial
!  continuity equation [Easter, 1993].
!
!  Since this field will be used exclussively to compute the high-
!  order fluxes during subsequent step3d_t operation, the final
!  values of t(i,j,k,??,itrc) alfer step3d_t will be computed in
!  a flux-conservative manner. The overall time step will be both 
!  conservative and constancy preserving.
!
!  This preliminary step shall be done before field t(:,:,:,???,:)
!  loses its meaningful values during the pre-step operation. 
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
! Advection for active tracers: Compute fluxes FX and FE
!----------------------------------------------------------
!
#  define PREDICTOR
#  include "compute_horiz_tracer_fluxes.h"
#  undef PREDICTOR

!
!----------------------------------------------------------
! C6 advection for passive tracers (biology ...)
!----------------------------------------------------------
!
# ifdef BIO_HADV_WENO5
          else  ! if (itrc .gt. 2)
#  define FLUX5 flux6
#  define FLUX3 flux4
#  define FLUX2 flux2
#  undef  UP5_MASKING
!
            cdif=0.
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
                    FX(i,j)=Huon(i,j,k)*t(i-1,j,k,nstp,itrc)
                  elseif (rmask(i,j).eq.1. .and. rmask(i-1,j).eq.0) then
                    FX(i,j)=Huon(i,j,k)*t(i  ,j,k,nstp,itrc)
                  endif
#  endif
                endif
              else
                if (Lsrc(is,itrc)) then
                  FE(i,j)=Hvom(i,j,k)*Tsrc(is,k,itrc)
#  ifdef MASKING
                else
                  if (rmask(i,j).eq.0 .and. rmask(i,j-1).eq.1) then
                    FE(i,j)=Hvom(i,j,k)*t(i,j-1,k,nstp,itrc)
                  elseif (rmask(i,j).eq.1 .and. rmask(i,j-1).eq.0) then
                    FE(i,j)=Hvom(i,j,k)*t(i,j  ,k,nstp,itrc)
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
          if (FIRST_TIME_STEP) then
            cff=0.5*dt
            do j=Jstr,Jend
              do i=Istr,Iend
                t(i,j,k,nnew,itrc)=Hz(i,j,k)*t(i,j,k,nstp,itrc)
     &                      -cff*pm(i,j)*pn(i,j)*( FX(i+1,j)-FX(i,j)
     &                                            +FE(i,j+1)-FE(i,j))
              enddo
            enddo
          else
            cff=(1.-gamma)*dt
            cff1=0.5+gamma
            cff2=0.5-gamma
            do j=Jstr,Jend
              do i=Istr,Iend
                t(i,j,k,nnew,itrc)=cff1*Hz(i,j,k)*t(i,j,k,nstp,itrc)
     &                            +cff2*Hz_bak(i,j,k)*t(i,j,k,indx,itrc)
     &                         -cff*pm(i,j)*pn(i,j)*( FX(i+1,j)-FX(i,j)
     &                                               +FE(i,j+1)-FE(i,j))
              enddo
            enddo
          endif

        enddo   ! <-- k
      enddo       ! <-- itrc

!
! Continue computation of the auxiliary tracer field: auxiliary
! continuity equation (the same for all tracers): DC=1/Hz_half_new,
! where Hz_half_new is Hz at time step n+1/2 as it would be computed
! from three-dimensional divergence of volume fluxes Huon,Hvom and W.
!
!======================================================================
!
! Compute vertical advection
!
!======================================================================
!
! Finalize computation of the auxiliary tracer field: compute its
! change due to vertical advection. Computation of vertical advective
! fluxes requires interpolation of tracer values to the verical grid-
! box interfaces (W-points). This can be done by either using
! parabolic spline interpolation or, more simple local cubic
! polynomial [linear interpolation is considered obsolete].
!      
      if (FIRST_TIME_STEP) then
            cdt=0.5*dt
      else
            cdt=(1.-gamma)*dt
      endif 

      do j=Jstr,Jend

        do i=Istr,Iend
# ifndef VADV_ADAPT_IMP 
           do k=1,N
             DC(i,k)=1./Hz_half(i,j,k)
           enddo   
# endif
             DC(i,0)=cdt*pn(i,j)*pm(i,j)
        enddo
        
        do itrc=1,NT
!
!----------------------------------------------------------------------
! Compute vertical fluxes FC
!----------------------------------------------------------------------
!
# define PREDICTOR
# include "compute_vert_tracer_fluxes.h"
# undef PREDICTOR
!
!----------------------------------------------------------------------    
!++ Apply vertical advection for tracers (DC corresponds to 1/Hz_half)
!----------------------------------------------------------------------
!
          do k=1,N
            do i=Istr,Iend
# ifdef VADV_ADAPT_IMP
              t(i,j,k,nnew,itrc)= t(i,j,k,nnew,itrc)  !<-- in this case division by Hz
     &               -DC(i,0)*(FC(i,k)-FC(i,k-1))     !<-- in the tridiagonal matrix problem
# else
              t(i,j,k,nnew,itrc)=DC(i,k)*( t(i,j,k,nnew,itrc)
     &               -DC(i,0)*(FC(i,k)-FC(i,k-1)))
#  ifdef CONST_TRACERS 
              t(i,j,k,nnew,itrc)=t(i,j,k,nstp,itrc)  
#  endif
# endif
            enddo
          enddo            !--> discard FC  
!
!----------------------------------------------------------------------
!  Compute implicit vertical advection/diffusion
!----------------------------------------------------------------------
!
# ifdef VADV_ADAPT_IMP
#  ifdef SALINITY
        iAKt=min(itrc,isalt)
#  else
        iAKt=min(itrc,itemp)
#  endif  
#  define TRIDIAG_TRA
#  undef  TRIDIAG_U
#  undef  TRIDIAG_V 
#  undef  TRIDIAG_W 
#   include "tridiag_pred.h"
#  undef TRIDIAG_TRA
# endif          
!
        enddo   !<-- itrc  !--> discard DC
      enddo     !<-- j

!
!======================================================================
!

# ifdef NHMG_DEBUG

      write(*,*) 'pre_step3d : u,v,wz before applying rhs. [u]'
      write(*,*) maxval(u(1:Lm+1,1:Mm,:,nstp))
      write(*,*) maxval(v(1:Lm,1:Mm+1,:,nstp))
      write(*,*) maxval(wz(1:Lm,1:Mm,:,nstp))
      write(*,*) 'pred_step3d : ru,rv,rw. [Vol*u/dt]'
      write(*,*) maxval(ru(1:Lm+1,1:Mm,:))
      write(*,*) maxval(rv(1:Lm,1:Mm+1,:))
      write(*,*) maxval(rw(1:Lm,1:Mm,:))

      call nhmg_write_pred_in(
     &     Lm, Mm, N,
     &     Hz_bak(-1:Lm+2,-1:Mm+2,1:N),
     &     Hz(-1:Lm+2,-1:Mm+2,1:N),
     &     Hz_half(-1:Lm+2,-1:Mm+2,1:N),
     &     u(-1:Lm+2,-1:Mm+2,1:N,indx),
     &     v(-1:Lm+2,-1:Mm+2,1:N,indx),
     &     wz(-1:Lm+2,-1:Mm+2,0:N,indx),
     &     u(-1:Lm+2,-1:Mm+2,1:N,nstp),
     &     v(-1:Lm+2,-1:Mm+2,1:N,nstp),
     &     wz(-1:Lm+2,-1:Mm+2,0:N,nstp),
     &     ru(-1:Lm+2,-1:Mm+2,1:N),
     &     rv(-1:Lm+2,-1:Mm+2,1:N),
     &     rw(-1:Lm+2,-1:Mm+2,0:N) )

# endif

!
!======================================================================
!
! Momentum equations: time stepping to time n+1/2
!
!======================================================================
!
      do j=Jstr,Jend
        do i=IstrU,Iend
          DC(i,0)=pm_u(i,j)*pn_u(i,j)
        enddo
        
        if (FIRST_TIME_STEP) then
          do k=1,N
            do i=IstrU,Iend
# ifdef VADV_ADAPT_IMP              
              cff = 1.
# else
              cff = 2./(Hz_half(i,j,k)+Hz_half(i-1,j,k))
# endif           
              u(i,j,k,nnew)=( u(i,j,k,nstp)*0.5*(Hz(i,j,k)+Hz(i-1,j,k))
     &                                           +cdt*DC(i,0)*ru(i,j,k)
     &                      )*cff
              u(i,j,k,indx)=u(i,j,k,nstp)*0.5*(Hz(i,j,k)+
     &                                         Hz(i-1,j,k))
# ifdef NBQ
              qdm_u(i,j,k,1) = 0.
              qdm_u(i,j,k,2) = u(i,j,k,nstp)*0.5*(Hz(i,j,k)+Hz(i-1,j,k))
              ruint_nbq(i,j,k) = qdm_u(i,j,k,2)/dt
              qdm_u_ext(i,j,0) = qdm_u_ext(i,j,0) + ruint_nbq(i,j,k)
# endif
            enddo
          enddo

# ifdef NBQ
          do k=1,N
            do i=IstrU,Iend
               ruint_nbq(i,j,k)=ruint_nbq(i,j,k)
     &            - qdm_u_ext(i,j,0) *(Hz(i,j,k)+Hz(i-1,j,k))
     &                               /(Zt_avg1(i,j)+Zt_avg1(i-1,j))
            enddo
          enddo
# endif

        else  ! NOT FIRST_TIME_STEP

          cff1=0.5+gamma
          cff2=0.5-gamma
          do k=1,N
            do i=IstrU,Iend
# ifdef VADV_ADAPT_IMP              
              cff = 1.
# else
              cff = 2./(Hz_half(i,j,k)+Hz_half(i-1,j,k))
# endif 

              u(i,j,k,nnew)=( cff1*u(i,j,k,nstp)*0.5*(Hz(i,j,k)+
     &                                                Hz(i-1,j,k))
     &                       +cff2*u(i,j,k,indx)*0.5*(Hz_bak(i,j,k)+
     &                                                Hz_bak(i-1,j,k))
     &                       +cdt*DC(i,0)*(ru(i,j,k)
# ifdef NBQ
     &                                    +ru_nbq_avg1(i,j,k)
# endif
     &                                    ) )*cff
              u(i,j,k,indx)=u(i,j,k,nstp)*0.5*(Hz(i,j,k)+
     &                                         Hz(i-1,j,k))

# ifdef NBQ
              qdm_u(i,j,k,1) = qdm_u(i,j,k,2)
              qdm_u(i,j,k,2) = u(i,j,k,nstp)*0.5*(Hz(i,j,k)+
     &                                            Hz(i-1,j,k))
              ruint_nbq(i,j,k) = (qdm_u(i,j,k,2)-qdm_u(i,j,k,1))/dt
     &                                  -DC(i,0)*ru_nbq_avg2(i,j,k)
              qdm_u_ext(i,j,0) = qdm_u_ext(i,j,0) + ruint_nbq(i,j,k)
# endif
            enddo
          enddo  

# ifdef NBQ
          do k=1,N
            do i=IstrU,Iend
               ruint_nbq(i,j,k)=ruint_nbq(i,j,k)
     &            - qdm_u_ext(i,j,0) *(Hz(i,j,k)+Hz(i-1,j,k))
     &                               /(Zt_avg1(i,j)+Zt_avg1(i-1,j))
            enddo
          enddo
# endif
     
        endif
!
!======================================================================
!
!  Compute implicit vertical advection/viscosity
!
# ifdef VADV_ADAPT_IMP
#  undef  TRIDIAG_TRA
#  define TRIDIAG_U
#  undef  TRIDIAG_V
#  undef  TRIDIAG_W 
#   include "tridiag_pred.h"
# endif
!
!======================================================================
!
        if (j.ge.JstrV) then
          do i=Istr,Iend
            DC(i,0)=pm_v(i,j)*pn_v(i,j)
          enddo
          if (FIRST_TIME_STEP) then
            do k=1,N
              do i=Istr,Iend
# ifdef VADV_ADAPT_IMP              
                cff = 1.
# else
                cff = 2./(Hz_half(i,j,k)+Hz_half(i,j-1,k))
# endif 
                v(i,j,k,nnew)=(v(i,j,k,nstp)*0.5*(Hz(i,j,k)+Hz(i,j-1,k))
     &                                           +cdt*DC(i,0)*rv(i,j,k)
     &                           )*cff
                v(i,j,k,indx)=v(i,j,k,nstp)*0.5*(Hz(i,j  ,k)+
     &                                           Hz(i,j-1,k))
# ifdef NBQ
                qdm_v(i,j,k,1) = 0.
                qdm_v(i,j,k,2) = v(i,j,k,nstp)*0.5*(Hz(i,j,k)
     &                                             +Hz(i,j-1,k))
                rvint_nbq(i,j,k) = qdm_v(i,j,k,2)/dt
                qdm_v_ext(i,j,0) = qdm_u_ext(i,j,0) + rvint_nbq(i,j,k)
# endif
              enddo
            enddo

# ifdef NBQ
            do k=1,N
              do i=Istr,Iend
                rvint_nbq(i,j,k)=rvint_nbq(i,j,k)
     &                  - qdm_v_ext(i,j,0) *(Hz(i,j,k)+Hz(i,j-1,k))
     &                               /(Zt_avg1(i,j)+Zt_avg1(i,j-1))
              enddo
            enddo
# endif

          else  ! NOT FIRST_TIME_STEP

            cff1=0.5+gamma
            cff2=0.5-gamma
            do k=1,N
              do i=Istr,Iend
              
# ifdef VADV_ADAPT_IMP              
                cff = 1.
# else
                cff = 2./(Hz_half(i,j,k)+Hz_half(i,j-1,k))
# endif              

                v(i,j,k,nnew)=( cff1*v(i,j,k,nstp)*0.5*(Hz(i,j,k)+
     &                                                  Hz(i,j-1,k))
     &                         +cff2*v(i,j,k,indx)*0.5*(Hz_bak(i,j,k)+
     &                                                  Hz_bak(i,j-1,k))
     &                     +cdt*DC(i,0)*(rv(i,j,k)
# ifdef NBQ
     &                                  +rv_nbq_avg1(i,j,k)
# endif
     &                                  )  )*cff
                v(i,j,k,indx)=v(i,j,k,nstp)*0.5*(Hz(i,j,k)+
     &                                           Hz(i,j-1,k))

# ifdef NBQ
                qdm_v(i,j,k,1) = qdm_v(i,j,k,2)
                qdm_v(i,j,k,2) = v(i,j,k,nstp)*0.5*(Hz(i,j,k)+
     &                                              Hz(i,j-1,k))
                rvint_nbq(i,j,k) = (qdm_v(i,j,k,2)-qdm_v(i,j,k,1))/dt
     &                                    -DC(i,0)*rv_nbq_avg2(i,j,k)
                qdm_v_ext(i,j,0) = qdm_u_ext(i,j,0) + rvint_nbq(i,j,k)
# endif
              enddo
            enddo               !--> discard DC(:,0)

# ifdef NBQ
            do k=1,N
              do i=Istr,Iend
                rvint_nbq(i,j,k)=rvint_nbq(i,j,k)
     &                  - qdm_v_ext(i,j,0) *(Hz(i,j,k)+Hz(i,j-1,k))
     &                               /(Zt_avg1(i,j)+Zt_avg1(i,j-1))
              enddo
            enddo
# endif

          endif
        endif
!
!======================================================================
!
!  Compute implicit vertical advection/viscosity
!
# ifdef VADV_ADAPT_IMP
# undef  TRIDIAG_TRA
# undef  TRIDIAG_U
# define TRIDIAG_V
# undef  TRIDIAG_W 
#  include "tridiag_pred.h"
# endif
!
!======================================================================
!
      enddo     !<-- j
      
# ifdef NBQ
!----------------------------------------------------------
!<-- at this point 
!<-- rw            contains internal 3D advection + Coriolis
!<-- rw_nbq_avg1   contains NBQ pressure gradient + gravity 
!                                        + second viscosity 
!----------------------------------------------------------
      do j=Jstr,Jend
!==
      if (FIRST_TIME_STEP) then
!==        
        do k=1,N-1
          do i=Istr,Iend
            wz(i,j,k,nnew)=( wz(i,j,k,nstp)*(Hz(i,j,k)+Hz(i,j,k+1))
     &                                          +dt*pn(i,j)*pm(i,j)
     &                              *(rw(i,j,k)+rw_nbq_avg1(i,j,k))
     &                     )   /  (Hz_half(i,j,k)+Hz_half(i,j,k+1)) !<-- wz has units m.s-1
            wz(i,j,k,indx)=wz(i,j,k,nstp)*0.5*(Hz(i,j,k  )+
     &                                         Hz(i,j,k+1)) !<-- wz(indx) has units kg.m-1.s-1
#  ifdef NBQ
            qdm_w(i,j,k,1)=0.
            qdm_w(i,j,k,2)=wz(i,j,k,nstp)*(Hz(i,j,k)+Hz(i,j,k+1))/2.
            rwint_nbq(i,j,k)= qdm_w(i,j,k,2)/dt
     &                      - pn(i,j)*pm(i,j)*rw_nbq_avg2(i,j,k)
#  endif
          enddo
        enddo
!== Special treatment for k=N because the control volume at the top is Hz/2       
        do i=Istr,Iend
            wz(i,j,N,nnew)=( wz(i,j,N,nstp)*Hz(i,j,N)
     &                            +dt*pn(i,j)*pm(i,j)
     &                *(rw(i,j,N)+rw_nbq_avg1(i,j,N))
     &                     )        /  Hz_half(i,j,N) !<-- wz has units m.s-1
            wz(i,j,N,indx)=wz(i,j,N,nstp)*0.5*Hz(i,j,N) !<-- wz(indx) has units kg.m-1.s-1
#  ifdef NBQ
            qdm_w(i,j,N,1)=0.
            qdm_w(i,j,N,2)=wz(i,j,N,nstp)*Hz(i,j,N)/2.
            rwint_nbq(i,j,N)=(qdm_w(i,j,N,2)-qdm_w(i,j,N,1))/dt
     &                      - pn(i,j)*pm(i,j)*rw_nbq_avg2(i,j,N)
#  endif
        enddo        
!==        
      else  ! NOT FIRST_TIME_STEP
!==      
        cff1=0.5+gamma
        cff2=0.5-gamma
!==        
        do k=1,N-1
          do i=Istr,Iend
             wz(i,j,k,nnew)=( cff1*wz(i,j,k,nstp)*(Hz(i,j,k  )+         ! n      
     &                                             Hz(i,j,k+1))         !
     &                       +cff2*wz(i,j,k,indx)*(Hz_bak(i,j,k  )+     ! n - 1
     &                                             Hz_bak(i,j,k+1))     !
     &                       +2.*cdt*pn(i,j)*pm(i,j)                    ! n - 1/2
     &                              *(rw(i,j,k)+rw_nbq_avg1(i,j,k))     !
     &                      ) / (Hz_half(i,j,k)+Hz_half(i,j,k+1))  !<-- wz has units m.s-1
             wz(i,j,k,indx)=wz(i,j,k,nstp)*0.5*(Hz(i,j,k  )+
     &                                          Hz(i,j,k+1)) !<-- wz(indx) has units kg.m-1.s-1
#  ifdef NBQ
             qdm_w(i,j,k,1)=qdm_w(i,j,k,2)
             qdm_w(i,j,k,2)=wz(i,j,k,nstp)*(Hz(i,j,k  )+
     &                                      Hz(i,j,k+1))/2.
             rwint_nbq(i,j,k)=(qdm_w(i,j,k,2)-qdm_w(i,j,k,1))/dt
     &                      - pn(i,j)*pm(i,j)*rw_nbq_avg2(i,j,k)
#  endif
          enddo
       enddo              

!== Special treatment for k=N because the control volume at the top is Hz/2             
       do i=Istr,Iend
             wz(i,j,N,nnew)=( cff1*wz(i,j,N,nstp)*Hz    (i,j,N)
     &                       +cff2*wz(i,j,N,indx)*Hz_bak(i,j,N)
     &                       +2.*cdt*pn(i,j)*pm(i,j)
     &                          *(rw(i,j,N)+rw_nbq_avg1(i,j,N))
     &                      )/ Hz_half(i,j,N)                !<-- wz has units m.s-1
             wz(i,j,N,indx)=wz(i,j,N,nstp)*0.5*Hz(i,j,N)   !<-- wz(indx) has units kg.m-1.s-1
#  ifdef NBQ
             qdm_w(i,j,N,1)=qdm_w(i,j,N,2)
             qdm_w(i,j,N,2)=wz(i,j,N,nstp)*Hz(i,j,N)/2.
             rwint_nbq(i,j,N)=(qdm_w(i,j,N,2)-qdm_w(i,j,N,1))/dt
     &                      -  pn(i,j)*pm(i,j)*rw_nbq_avg2(i,j,N)
#  endif
       enddo            
!==                  
      endif

      enddo
!==
# endif /* NBQ */
     
# if defined NHMG
!----------------------------------------------------------
!<-- at this point 
!<-- rw            contains internal 3D advection + Coriolis
!----------------------------------------------------------
      do j=Jstr,Jend
!==
      if (FIRST_TIME_STEP) then
!==        
        do k=1,N-1
          do i=Istr,Iend
             ! rw is a tendency*horizontal area
             ! wz(:,:,:,nstp) and wz(:,:,:,indx) are velocity
             ! wz(:,:,:,nnew) is velocity 
             cff = (pn(i,j)*pm(i,j)) /(0.5*(Hz(i,j,k)+Hz(i,j,k+1)))
             wz(i,j,k,nnew)=wz(i,j,k,nstp) + cdt * rw(i,j,k)*cff
             wz(i,j,k,indx)=wz(i,j,k,nstp)
          enddo
        enddo
        do i=Istr,Iend
           cff = (pn(i,j)*pm(i,j)) /(0.5*(Hz(i,j,N)))
           wz(i,j,N,nnew)=wz(i,j,N,nstp) + cdt * rw(i,j,N)*cff
           wz(i,j,N,indx)=wz(i,j,N,nstp)
        enddo
!==        
      else  ! NOT FIRST_TIME_STEP
!==      
        cff1=0.5+gamma
        cff2=0.5-gamma
!==        
        do k=1,N-1
           do i=Istr,Iend
              cff = (pn(i,j)*pm(i,j)) /(0.5*(Hz(i,j,k)+Hz(i,j,k+1)))
              wz(i,j,k,nnew)= 
     &             (cff1*wz(i,j,k,nstp) + cff2*wz(i,j,k,indx)) 
     &             +cdt*rw(i,j,k)*cff
              wz(i,j,k,indx)=wz(i,j,k,nstp)
           enddo
        enddo     
        do i=Istr,Iend
           cff = (pn(i,j)*pm(i,j)) /(0.5*(Hz(i,j,N)))
           wz(i,j,N,nnew)= 
     &          (cff1*wz(i,j,N,nstp) + cff2*wz(i,j,N,indx)) 
     &          +cdt*rw(i,N,k)*cff
           wz(i,j,N,indx)=wz(i,j,N,nstp)
        enddo
!==   
      endif
      do i=Istr,Iend
         DC(i,0)=pm(i,j)*pn(i,j)
      enddo

!
!======================================================================
!
!  Compute implicit vertical advection/viscosity
!
# ifdef VADV_ADAPT_IMP
#  undef  TRIDIAG_TRA
#  undef  TRIDIAG_U
#  undef  TRIDIAG_V
#  define TRIDIAG_W
#   include "tridiag_pred.h"
# endif
!
!======================================================================
!
      enddo
!==

# endif /* NHMG */

# ifdef NHMG_DEBUG
      write(*,*) 'pre_step3d : u,v,wz after applying rhs. [u]'
      write(*,*) maxval(u(1:Lm+1,1:Mm,:,nnew))
      write(*,*) maxval(v(1:Lm,1:Mm+1,:,nnew))
      write(*,*) maxval(wz(1:Lm,1:Mm,:,nnew))
# endif

!
!======================================================================
! Set PHYSICAL lateral boundary conditions for tracers.
! and exchange periodic boundaries and computational margins.
!======================================================================
!
      do itrc=1,NT
        call t3dbc_tile (Istr,Iend,Jstr,Jend, nnew,itrc, WORK)
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
#  ifdef THREE_GHOST_POINTS_TS
        call exchange_r3d_3pts_tile (Istr,Iend,Jstr,Jend,
     &                               t(START_2D_ARRAY,1,nnew,itrc))
#  else
        call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                          t(START_2D_ARRAY,1,nnew,itrc))
#  endif
# endif
      enddo
!
!======================================================================
! Set PHYSICAL lateral boundary conditions for momentum.
!======================================================================
!
      call u3dbc_tile (Istr,Iend,Jstr,Jend, WORK)
      call v3dbc_tile (Istr,Iend,Jstr,Jend, WORK)
# if defined NBQ || defined NHMG
      call w3dbc_tile (Istr,Iend,Jstr,Jend, WORK)      
# endif
!
!======================================================================
! Non-hydro projection of momentum
!======================================================================
!

# ifdef NHMG_DEBUG

      write(*,*) 'pre_step3d : u,v,wz before nh projection. [u]'
      write(*,*) maxval(u(1:Lm+1,1:Mm,:,nnew))
      write(*,*) maxval(v(1:Lm,1:Mm+1,:,nnew))
      write(*,*) maxval(wz(1:Lm,1:Mm,:,nnew))
      write(*,*) 'pre_step3d : rufrc,rvfrc before nh projection.'
      write(*,*) maxval(rufrc(1:Lm+1,1:Mm))
      write(*,*) maxval(rvfrc(1:Lm,1:Mm+1))

      call nhmg_write_pred_inter1(
     &     Lm, Mm, N,
     &     u(-1:Lm+2,-1:Mm+2,1:N,nnew),
     &     v(-1:Lm+2,-1:Mm+2,1:N,nnew),
     &     wz(-1:Lm+2,-1:Mm+2,0:N,nnew),
     &     rufrc(-1:Lm+2,-1:Mm+2),
     &     rvfrc(-1:Lm+2,-1:Mm+2) )

# endif

# ifdef NHMG

      cff = 1./cdt

      if (iic.ge.(ntstart+2)) then
!      if (iic.lt.0) then

         call tic(1,'AB2 del q')

         do k=1,N
            do j=Jstr,Jend
               do i=IstrU,Iend
                  u(i,j,k,nnew) = u(i,j,k,nnew)
     &                 + (1.5*nhdu(i,j,k,iprec2)-0.5*nhdu(i,j,k,iprec1))*cdt
     &                 /(0.5*(Hz_half(i,j,k) + Hz_half(i-1,j,k))) * pn_u(i,j)
!                  rufrc(i,j) = rufrc(i,j) 
!     &                 + (1.5*nhdu(i,j,k,iprec2)-0.5*nhdu(i,j,k,iprec1)) 
!     &                 * om_u(i,j)
                  rufrc(i,j) = rufrc(i,j) 
     &                 + (nhdu(i,j,k,iprec2)) 
     &                 * om_u(i,j)
               enddo
            enddo
            
            do j=JstrV,Jend
               do i=Istr,Iend
                  v(i,j,k,nnew) = v(i,j,k,nnew)
     &                 + (1.5*nhdv(i,j,k,iprec2)-0.5*nhdv(i,j,k,iprec1))*cdt
     &                 /(0.5*(Hz_half(i,j,k) + Hz_half(i,j-1,k))) * pm_v(i,j)
!                  rvfrc(i,j) = rvfrc(i,j)
!     &                 + (1.5*nhdv(i,j,k,iprec2)-0.5*nhdv(i,j,k,iprec1)) 
!     &                 * on_v(i,j)
                  rvfrc(i,j) = rvfrc(i,j)
     &                 + (nhdv(i,j,k,iprec2)) 
     &                 * on_v(i,j)
               enddo
            enddo
            
            do j=Jstr,Jend
               do i=Istr,Iend
                  wz(i,j,k,nnew) = wz(i,j,k,nnew) 
     &                 + (1.5*nhdw(i,j,k,iprec2)-0.5*nhdw(i,j,k,iprec1))*cdt
     &                 * (pm(i,j)*pn(i,j))
               enddo
            enddo
         enddo
         
         call toc(1,'AB2 del q')
         
      else

         call tic(1,'swap predictor')
         
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
         call exchange_u3d_tile (Istr,Iend,Jstr,Jend,
     &        u(START_2D_ARRAY,1,nnew))
         call exchange_v3d_tile (Istr,Iend,Jstr,Jend,
     &        v(START_2D_ARRAY,1,nnew))
# endif
       
# if defined NHMG_MASKING
         do k=1,N    
            do j=Jstr,Jend
               do i=IstrU,Iend
                  u(i,j,k,nnew) = u(i,j,k,nnew)*umask(i,j)
               enddo
            enddo
         enddo
        do k=1,N    
            do j=JstrV,Jend
               do i=Istr,Iend
                  v(i,j,k,nnew) = v(i,j,k,nnew)*vmask(i,j)
               enddo
            enddo
         enddo
         do k=1,N    
            do j=Jstr,Jend
               do i=Istr,Iend
                  wz(i,j,k,nnew) = wz(i,j,k,nnew)*rmask(i,j)
               enddo
            enddo
         enddo
# endif

         call nhmg_solve(u(:,:,:,nnew),
     &                   v(:,:,:,nnew),
     &                   wz(:,:,:,nnew),
     &                   Hz_half,.true.)
         
         ! add the non-hydro correction (du, dv, dw are in flux form)
         ! add the non-hydrostatic pressure gradient to rufrc, rvfrc
         do k=1,N    
            do j=Jstr,Jend
               do i=IstrU,Iend
                  u(i,j,k,nnew) = u(i,j,k,nnew) + grid(1)%du(k,j,i)
     &                 /(0.5*(Hz_half(i,j,k) + Hz_half(i-1,j,k))) 
     &                 * pn_u(i,j)
!instab comes from rufrc
                  rufrc(i,j) = rufrc(i,j) + om_u(i,j)*grid(1)%du(k,j,i)*cff 
               enddo
            enddo
            
            do j=JstrV,Jend
               do i=Istr,Iend
                  v(i,j,k,nnew) = v(i,j,k,nnew) + grid(1)%dv(k,j,i)
     &                 /(0.5*(Hz_half(i,j,k) + Hz_half(i,j-1,k))) 
     &                 * pm_v(i,j)
!instab comes from rvfrc
                  rvfrc(i,j) = rvfrc(i,j) + on_v(i,j)*grid(1)%dv(k,j,i)*cff
               enddo
            enddo
            
            do j=Jstr,Jend
               do i=Istr,Iend
                  wz(i,j,k,nnew) = wz(i,j,k,nnew) + grid(1)%dw(k+1,j,i)
     &                 * (pm(i,j)*pn(i,j))
               enddo
            enddo
         enddo
        
         call toc(1,'swap predictor')

      endif      

# endif

# ifdef NHMG_DEBUG
      write(*,*) 'pre_step3d : u,v,wz after nh projection. [u]'
      write(*,*) maxval(u(1:Lm+1,1:Mm,:,nnew))
      write(*,*) maxval(v(1:Lm,1:Mm+1,:,nnew))
      write(*,*) maxval(wz(1:Lm,1:Mm,:,nnew))
      write(*,*) 'pre_step3d : rufrc,rvfrc after nh projection.'
      write(*,*) maxval(rufrc(1:Lm+1,1:Mm))
      write(*,*) maxval(rvfrc(1:Lm,1:Mm+1))
# endif

!
!======================================================================
! Coupling, include ghost points associated with PHYSICAL
! boundaries ONLY. Do not touch periodic ghost points or
! internal computational margins (MPI code). 
!======================================================================
!
# ifdef EW_PERIODIC
#  define IU_RANGE Istr,Iend
#  define IV_RANGE Istr,Iend
# else
#  define IU_RANGE Istr,IendR
#  define IV_RANGE IstrR,IendR
# endif

# ifdef NS_PERIODIC
#  define JU_RANGE Jstr,Jend
#  define JV_RANGE Jstr,Jend
# else
#  define JU_RANGE JstrR,JendR
#  define JV_RANGE Jstr,JendR
# endif
!
c$$$      do j=JU_RANGE                      ! XI-component
c$$$        do i=IU_RANGE
c$$$          DC(i,0)=0.
c$$$          CF(i,0)=0.
c$$$        enddo
c$$$        do k=1,N,+1
c$$$          do i=IU_RANGE
c$$$            DC(i,k)=0.5*(Hz(i,j,k)+Hz(i-1,j,k))*on_u(i,j)
c$$$            DC(i,0)=DC(i,0)+DC(i,k)
c$$$            CF(i,0)=CF(i,0)+DC(i,k)*u(i,j,k,nnew)
c$$$          enddo
c$$$        enddo
c$$$        if (FIRST_TIME_STEP) then
c$$$          cff1=1.
c$$$          cff2=0.
c$$$        else
c$$$          cff1=1.5
c$$$          cff2=0.5
c$$$        endif
c$$$        do i=IU_RANGE
c$$$          CF(i,0)=( CF(i,0)-cff1*DU_avg1(i,j,nstp)
c$$$     &                     +cff2*DU_avg1(i,j,indx) )/DC(i,0)
c$$$        enddo
c$$$        do k=N,1,-1
c$$$          do i=IU_RANGE
c$$$            u(i,j,k,nnew)=(u(i,j,k,nnew)-CF(i,0))
c$$$# ifdef MASKING
c$$$     &                                 *umask(i,j)
c$$$# endif
c$$$# ifdef WET_DRY
c$$$            cff3=wetdry(i-1,j)+wetdry(i,j)
c$$$            IF (cff3.eq.1.) THEN
c$$$              cff3=wetdry(i-1,j)-wetdry(i,j)
c$$$            END IF
c$$$            cff4=ABS(ABS(cff3)-1.)
c$$$            cff5=0.5+SIGN(0.5,(cff1*DU_avg1(i,j,nstp)
c$$$     &                        -cff2*DU_avg1(i,j,indx)))*cff3
c$$$            umask_wet(i,j)=0.5*cff3*cff4+cff5*(1.-cff4)
c$$$!            IF (cff1*DU_avg1(i,j,nstp)
c$$$!     &         -cff2*DU_avg1(i,j,indx).eq.0. .and.
c$$$!     &          wetdry(i-1,j)+wetdry(i,j).le.1.) THEN
c$$$!              umask_wet(i,j)=0.
c$$$!            END IF
c$$$            u(i,j,k,nnew)=u(i,j,k,nnew)*umask_wet(i,j)
c$$$# endif
c$$$          enddo
c$$$        enddo
c$$$      enddo
c$$$
c$$$      do j=JV_RANGE
c$$$        do i=IV_RANGE                   ! ETA-component
c$$$          DC(i,0)=0.
c$$$          CF(i,0)=0.
c$$$        enddo
c$$$        do k=1,N,+1
c$$$          do i=IV_RANGE
c$$$            DC(i,k)=0.5*(Hz(i,j,k)+Hz(i,j-1,k))*om_v(i,j)
c$$$            DC(i,0)=DC(i,0)+DC(i,k)
c$$$            CF(i,0)=CF(i,0)+DC(i,k)*v(i,j,k,nnew)
c$$$          enddo
c$$$        enddo
c$$$        if (FIRST_TIME_STEP) then
c$$$          cff1=1.
c$$$          cff2=0.
c$$$        else
c$$$          cff1=1.5
c$$$          cff2=0.5
c$$$        endif
c$$$        do i=IV_RANGE
c$$$          CF(i,0)=( CF(i,0)-cff1*DV_avg1(i,j,nstp)
c$$$     &                     +cff2*DV_avg1(i,j,indx) )/DC(i,0)
c$$$        enddo
c$$$        do k=N,1,-1
c$$$          do i=IV_RANGE
c$$$            v(i,j,k,nnew)=(v(i,j,k,nnew)-CF(i,0))
c$$$# ifdef MASKING
c$$$     &                                  *vmask(i,j)
c$$$# endif
c$$$# ifdef WET_DRY
c$$$            cff3=wetdry(i,j-1)+wetdry(i,j)
c$$$            IF (cff3.eq.1.) THEN
c$$$              cff3=wetdry(i,j-1)-wetdry(i,j)
c$$$            END IF
c$$$            cff4=ABS(ABS(cff3)-1.)
c$$$            cff5=0.5+SIGN(0.5,(cff1*DV_avg1(i,j,nstp)
c$$$     &                        -cff2*DV_avg1(i,j,indx)))*cff3
c$$$            vmask_wet(i,j)=0.5*cff3*cff4+cff5*(1.-cff4)
c$$$!            IF (cff1*DV_avg1(i,j,nstp)
c$$$!     &         -cff2*DV_avg1(i,j,indx).eq.0. .and.
c$$$!     &          wetdry(i,j-1)+wetdry(i,j).le.1.) THEN
c$$$!              vmask_wet(i,j)=0.
c$$$!            END IF
c$$$            v(i,j,k,nnew)=v(i,j,k,nnew)*vmask_wet(i,j)
c$$$# endif
c$$$          enddo
c$$$        enddo
c$$$      enddo
!
!======================================================================
! Exchange periodic boundaries and computational margins.
!======================================================================
!
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,
     &                        u(START_2D_ARRAY,1,nnew))
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend,
     &                        v(START_2D_ARRAY,1,nnew))
#  if defined NBQ || defined NHMG
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend,
     &                        wz(START_2D_ARRAY,0,nnew))     
#  endif
# endif

!
!======================================================================
!

# ifdef NHMG_DEBUG

      call nhmg_write_pred_inter2(
     &     Lm, Mm, N,
     &     u(-1:Lm+2,-1:Mm+2,1:N,nnew),
     &     v(-1:Lm+2,-1:Mm+2,1:N,nnew),
     &     wz(-1:Lm+2,-1:Mm+2,0:N,nnew),
     &     rufrc(-1:Lm+2,-1:Mm+2),
     &     rvfrc(-1:Lm+2,-1:Mm+2) )

# endif

!
!======================================================================
! Prepare to start two-dimensional time stepping:
! set the initial values of the fast-time-step free-surface
! field to its fast-time-averaged values corresponding
! to the time step n (nstp).
!======================================================================
!
# ifdef MPI
      do j=JU_RANGE
        do i=IV_RANGE
          zeta(i,j,knew)=Zt_avg1(i,j)
        enddo
      enddo
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                          zeta(START_2D_ARRAY,knew))
# else
      do j=JstrR,JendR
        do i=IstrR,IendR
          zeta(i,j,knew)=Zt_avg1(i,j)
        enddo
      enddo
# endif
!
# undef IU_RANGE
# undef JU_RANGE
# undef IV_RANGE
# undef JV_RANGE
#else
      subroutine pre_step3d_empty
#endif
      return
      end
