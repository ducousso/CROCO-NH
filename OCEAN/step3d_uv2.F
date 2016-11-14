! $Id: step3d_uv2.F 1480 2014-02-18 14:56:20Z rblod $
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

      subroutine step3d_uv2 (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()

      call step3d_uv2_tile (Istr,Iend,Jstr,Jend, A2d(1,1,trd),
     &                                  A2d(1,2,trd), A2d(1,3,trd),
     &                                                A2d(1,4,trd)
#ifdef VADV_ADAPT_IMP
     &                                               ,A2d(1,5,trd)
#endif
     &                                                            )
      return
      end

      subroutine step3d_uv2_tile (Istr,Iend,Jstr,Jend, BC,CF,FC,DC
#ifdef VADV_ADAPT_IMP
     &                                                         ,WC
#endif
     &                                                            )
!
!--------------------------------------------------------------------
!  This routine  performs the timestep for the horizontal momentum
! equations. Due to the implicit treatment of vertical viscosity, it
! solves the following tri-diagonal problem:
!
!          FC(k-1) * F(k-1) + BC(k) * F(k) + FC(k) * F(k+1) = DC(k)
!
! where
!          BC(k) = Hz(k) - FC(k) - FC(k-1)
! and
!          FC(k) = - dt * Ak(k) / Hz(k)
! and
!          F(k) is the unknown field: u,v at time step nnew.
!
!  As long as all diffusivity/viscosity coefficients Ak(k) are
! nonnegative, the tri-diagonal matrics is diagonally dominant,
! so that a simple Gaussian elimination algorithm is stable,
! (e.g., Richtmeyer annd  Morton,  1967).
! The implicit vertical viscosity/diffusion terms are discretized
! using implicit backward time step.
!-------------------------------------------------------------------- 
!
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "coupling.h"
# include "forces.h"
# include "mixing.h"
# include "scalars.h"
# include "sources.h"
# if defined M3NUDGING && defined M3CLIMATOLOGY
#  include "climat.h"
# endif
# ifdef DIAGNOSTICS_UV
#  include "diagnostics.h"
# endif
      integer Istr,Iend,Jstr,Jend, i,j,k
# ifdef PSOURCE
     &       ,is
# endif
      real BC(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     CF(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     FC(PRIVATE_1D_SCRATCH_ARRAY,0:N), cff,
     &     DC(PRIVATE_1D_SCRATCH_ARRAY,0:N)
# ifdef VADV_ADAPT_IMP
      real WC(PRIVATE_1D_SCRATCH_ARRAY,0:N)
# endif
      real grad(PRIVATE_2D_SCRATCH_ARRAY)
      real dpth,aa,cc
# ifdef TS_MIX_ISO
      real dRx,dRe, cfilt
# endif
!
# include "compute_auxiliary_bounds.h"
!
# ifdef MASKING
#  define SWITCH *
# else
#  define SWITCH !
# endif
# ifdef TS_MIX_ISO_FILT
      if (iic.eq.ntstart) then
       cfilt=1.                 ! exponential
      else                      ! time smoothing
       cfilt=csmooth            ! coefficient
      endif
# endif
!
!--------------------------------------------------------------------
! Time step momentum equation in the XI-direction.
!
! Compute coefficients FC [dt*Akv/Hz] for the implicit
! vertical viscosity term at future time level at horizontal
! U-points and vertical W-points.
! Load the right-hand-side terms into matrix DC;
! Also compute the main diagonal matrix coeddicient BC.
!--------------------------------------------------------------------
!
      do j=Jstr,Jend
!
! The vertical viscosity term is not added as an explicit divergence term
! like the other terms due to the semi-implicit backward Euler scheme. 
! We store first u(i,j,k,nnew) ...
!
# ifdef DIAGNOSTICS_UV
        do k=1,N
          do i=IstrU,Iend
             MVmix(i,j,k,1)=u(i,j,k,nnew)
          enddo
        enddo
# endif
!--------------------------------------------------------------------
! Resolve the tri-diagonal system. Also perform coupling between the
! barotropic and baroclinic modes. For the purpose of the processor
! efficiency these two operations are partially overlapped. So that
! in the third and fourth loop below operations above the separator
! (!-) belong to the tri-diagonal solver, while operations below it
! belong to the coupling.
!
! The coupling procedure implies vertical integration of the newly
! computed XI-component velocity [which ends up being stored in the
! scratch array DC(:,1:N) after the tri-diagonal problem is resolved,
! the fourth loop below] in order to obtain its vertical mean [stored
! as DC(:,0)], and replace it with the more accurate mean obtained
! from the 2D barotropic sub-model ubar=DU_avg1/(D*on_u), where
! D=CF(:,0) is the total depth of water column.
!
!--------------------------------------------------------------------
        do i=IstrU,Iend
          FC(i,0)=0.
          FC(i,N)=0.
# ifdef VADV_ADAPT_IMP
          DC(i,0)=0.25*dt*(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
          WC(i,0)=0.
          WC(i,N)=0.
# endif
        enddo

        do k=1,N-1
          do i=IstrU,Iend
            FC(i,k)=-dt*(Akv(i,j,k)+Akv(i-1,j,k))
     &                 /( z_r(i,j,k+1)+z_r(i-1,j,k+1)
     &                   -z_r(i,j,k  )-z_r(i-1,j,k  ))
# ifdef VADV_ADAPT_IMP
            WC(i,k)=DC(i,0)*0.5*(Wi(i,j,k)+Wi(i-1,j,k))
# endif
          enddo
        enddo

        do k=1,N
          do i=IstrU,Iend
            DC(i,k)=u(i,j,k,nnew)
            BC(i,k)=0.5*(Hz(i,j,k)+Hz(i-1,j,k))-FC(i,k)-FC(i,k-1)
# ifdef VADV_ADAPT_IMP
     &              +max(WC(i,k  ),0.)-min(WC(i,k-1),0.)
# endif
          enddo
        enddo      !--> discard DC(:,0); keep FC
!
! Apply wind forcing.
!
# ifndef BODYFORCE
        do i=IstrU,Iend
          DC(i,N)=DC(i,N)+dt*sustr(i,j)
          DC(i,1)=DC(i,1)-dt*bustr(i,j)
        enddo
# endif
        do i=IstrU,Iend
          cff=1./BC(i,1)
          CF(i,1)=cff*( FC(i,1)                  !<-- q(1) = c(1)/b(1)
# ifdef VADV_ADAPT_IMP
     &                +min( WC(i,1),0.)
# endif
     &                                 )
          DC(i,1)=cff*DC(i,1)
        enddo
        do k=2,N-1
          do i=IstrU,Iend
# ifdef VADV_ADAPT_IMP
              aa = FC(i,k-1)-max(WC(i,k-1),0.)   !<-- a = sub   (lower) diagonal
              cc = FC(i,k  )+min(WC(i,k  ),0.)   !<-- c = super (upper) diagonal
# else
              aa = FC(i,k-1)
              cc = FC(i,k  )
# endif
            cff=1./(BC(i,k)-aa*CF(i,k-1))        !<-- p    = 1/(b(k)-a(k)*q(k))
            CF(i,k)=cff*cc
            DC(i,k)=cff*(DC(i,k)-aa*DC(i,k-1))          
          enddo
        enddo
        do i=IstrU,Iend
# ifdef VADV_ADAPT_IMP
              aa = FC(i,N-1)-max(WC(i,N-1),0.)   !<-- a = sub   (lower) diagonal
# else
              aa = FC(i,N-1)
# endif
          DC(i,N)=(DC(i,N)-aa*DC(i,N-1))
     &           /(BC(i,N)-aa*CF(i,N-1))

          CF(i,0)=0.5*(Hz(i,j,N)+Hz(i-1,j,N))
          DC(i,0)=CF(i,0)*DC(i,N)
        enddo
        do k=N-1,1,-1
          do i=IstrU,Iend
              DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)

            cff=0.5*(Hz(i,j,k)+Hz(i-1,j,k))
            CF(i,0)=CF(i,0)+cff
            DC(i,0)=DC(i,0)+cff*DC(i,k)
          enddo
        enddo      !--> discard FC,BC; keep DC,CF(:,0)

        do i=IstrU,Iend
          DC(i,0)=(DC(i,0)*on_u(i,j)-DU_avg1(i,j,nnew))
     &                             /(CF(i,0)*on_u(i,j))
        enddo
        do k=1,N
          do i=IstrU,Iend
            u(i,j,k,nnew)=(DC(i,k)-DC(i,0)) SWITCH umask(i,j)
# ifdef DIAGNOSTICS_UV
            MVmix(i,j,k,1)=-( MVmix(i,j,k,1) - u(i,j,k,nnew)*
     &                           0.5*(Hz(i,j,k)+Hz(i-1,j,k)) )
     &                    *om_u(i,j)*on_u(i,j)/dt SWITCH umask(i,j)
# endif 
!
          enddo
        enddo        !--> discard DC,CF(:,0)
!
!--------------------------------------------------------------------
! Time step momentum equation in the ETA-direction.

! Compute off-diagonal matrix coefficients FC [dt*Akv/Hz]
! for the implicit vertical viscosity term at the new time step,
! at horizontal V-points and vertical W-points.
! Set vertical boundary conditions.
! After that load the right-hand-side terms into matrix DC;
! Also compute the main diagonal matrix coefficient BC.
!--------------------------------------------------------------------
!
        if (j.ge.JstrV) then
!
! The vertical viscosity term is not added as an explicit divergence term
! like the other terms due to the semi-implicit backward Euler scheme. 
! We store first v(i,j,k,nnew) ...
!
# ifdef DIAGNOSTICS_UV
          do k=1,N
            do i=Istr,Iend
               MVmix(i,j,k,2)=v(i,j,k,nnew)
            enddo
         enddo
# endif /* DIAGNOSTICS_UV */
!
          do i=Istr,Iend
            FC(i,0)=0.
            FC(i,N)=0.
# ifdef VADV_ADAPT_IMP
            DC(i,0)=0.25*dt*(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
            WC(i,0)=0.
            WC(i,N)=0.
# endif
          enddo

          do k=1,N-1
            do i=Istr,Iend
              FC(i,k)=-dt*(Akv(i,j,k)+Akv(i,j-1,k))
     &                   /( z_r(i,j,k+1)+z_r(i,j-1,k+1)
     &                     -z_r(i,j,k  )-z_r(i,j-1,k  ))
# ifdef VADV_ADAPT_IMP
              WC(i,k)=DC(i,0)*0.5*(Wi(i,j,k)+Wi(i,j-1,k))
# endif
            enddo
          enddo

          do k=1,N
            do i=Istr,Iend
              DC(i,k)=v(i,j,k,nnew)
              BC(i,k)=0.5*(Hz(i,j,k)+Hz(i,j-1,k))-FC(i,k)-FC(i,k-1)
# ifdef VADV_ADAPT_IMP
     &              +max(WC(i,k  ),0.)-min(WC(i,k-1),0.)
# endif
            enddo
          enddo
!
! Apply wind forcing
!
# ifndef BODYFORCE
          do i=Istr,Iend
            DC(i,N)=DC(i,N)+dt*svstr(i,j)
            DC(i,1)=DC(i,1)-dt*bvstr(i,j)
          enddo
# endif          
!
! Resolve the tri-diagonal system. Also perform coupling between the
! barotropic and baroclinic modes (similarly to the XI-component, see
! comment above).
!
          do i=Istr,Iend
            cff=1./BC(i,1)
            CF(i,1)=cff*( FC(i,1)                  !<-- q(1) = c(1)/b(1)
# ifdef VADV_ADAPT_IMP
     &                +min( WC(i,1),0.)
# endif
     &                                 )
            DC(i,1)=cff*DC(i,1)
          enddo
          do k=2,N-1
            do i=Istr,Iend
# ifdef VADV_ADAPT_IMP
              aa = FC(i,k-1)-max(WC(i,k-1),0.)     !<-- a = sub   (lower) diagonal
              cc = FC(i,k  )+min(WC(i,k  ),0.)     !<-- c = super (upper) diagonal
# else
              aa = FC(i,k-1)
              cc = FC(i,k  )
# endif
              cff=1./(BC(i,k)-aa*CF(i,k-1))
              CF(i,k)=cff*cc
              DC(i,k)=cff*(DC(i,k)-aa*DC(i,k-1))
            enddo
          enddo
          do i=Istr,Iend
# ifdef VADV_ADAPT_IMP
              aa = FC(i,N-1)-max(WC(i,N-1),0.)     !<-- a = sub   (lower) diagonal
# else
              aa = FC(i,N-1)
# endif
            DC(i,N)=(DC(i,N)-aa*DC(i,N-1))
     &             /(BC(i,N)-aa*CF(i,N-1))

            CF(i,0)=0.5*(Hz(i,j,N)+Hz(i,j-1,N))
            DC(i,0)=CF(i,0)*DC(i,N)
          enddo
          do k=N-1,1,-1
            do i=Istr,Iend
              DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)

              cff=0.5*(Hz(i,j,k)+Hz(i,j-1,k))
              CF(i,0)=CF(i,0)+cff
              DC(i,0)=DC(i,0)+cff*DC(i,k)
            enddo
          enddo      !--> discard FC,BC; keep DC,CF(:,0)

          do i=Istr,Iend
            DC(i,0)=(DC(i,0)*om_v(i,j)-DV_avg1(i,j,nnew))
     &                               /(CF(i,0)*om_v(i,j))
          enddo
          do k=1,N
            do i=Istr,Iend
              v(i,j,k,nnew)=(DC(i,k)-DC(i,0)) SWITCH vmask(i,j)
!
# ifdef DIAGNOSTICS_UV
              MVmix(i,j,k,2)=-( MVmix(i,j,k,2) - v(i,j,k,nnew)*
     &                             0.5*(Hz(i,j,k)+Hz(i,j-1,k)) )
     &                        *om_v(i,j)*on_v(i,j)/dt SWITCH vmask(i,j)
# endif /* DIAGNOSTICS_UV */
!
            enddo
          enddo       !--> discard DC,CF(:,0)
        endif
      enddo      ! <-- j

!
# ifdef DIAGNOSTICS_UV
      do k=1,N
        do j=Jstr,Jend
          do i=Istr,Iend
            Mrate(i,j,k,1)=( 0.5*(Hz(i,j,k)+Hz(i-1,j,k))*u(i,j,k,nnew)
     &              -0.5*(Hz_bak(i,j,k)+Hz_bak(i-1,j,k))*u(i,j,k,nstp) )
     &                                           *om_u(i,j)*on_u(i,j)/dt
     &                                           SWITCH umask(i,j)
!
            Mrate(i,j,k,2)=( 0.5*(Hz(i,j,k)+Hz(i,j-1,k))*v(i,j,k,nnew)
     &              -0.5*(Hz_bak(i,j,k)+Hz_bak(i,j-1,k))*v(i,j,k,nstp) )
     &                                           *om_v(i,j)*on_v(i,j)/dt
     &                                           SWITCH vmask(i,j)
!
! Divide all diagnostic terms by the cell volume Hz/(pm*pn).
! There after the unit of diag terms are :
! (unit of velocity) * s-1  =  m * s-2
!
            cff=2.*pm_u(i,j)*pn_u(i,j)/(Hz(i,j,k)+Hz(i-1,j,k))
            MXadv(i,j,k,1)   = MXadv(i,j,k,1)*cff
            MYadv(i,j,k,1)   = MYadv(i,j,k,1)*cff
            MVadv(i,j,k,1)   = MVadv(i,j,k,1)*cff
            MCor(i,j,k,1)    = MCor(i,j,k,1)*cff
            MPrsgrd(i,j,k,1) = MPrsgrd(i,j,k,1)*cff
            MHmix(i,j,k,1)   = MHmix(i,j,k,1)*cff
            MVmix(i,j,k,1)   = MVmix(i,j,k,1)*cff
            Mrate(i,j,k,1)   = Mrate(i,j,k,1)*cff
#  ifdef MRL_WCI
            Mvf(i,j,k,1)     = Mvf(i,j,k,1)*cff
            Mbrk(i,j,k,1)    = Mbrk(i,j,k,1)*cff
            MStCo(i,j,k,1)   = MStCo(i,j,k,1)*cff
            MVvf(i,j,k,1)    = MVvf(i,j,k,1)*cff
            MPrscrt(i,j,k,1) = MPrscrt(i,j,k,1)*cff
            Msbk(i,j,k,1)    = Msbk(i,j,k,1)*cff
            Mbwf(i,j,k,1)    = Mbwf(i,j,k,1)*cff
            Mfrc(i,j,k,1)    = Mfrc(i,j,k,1)*cff
#  endif
!
            cff=2.*pm_v(i,j)*pn_v(i,j)/(Hz(i,j,k)+Hz(i,j-1,k))
            MXadv(i,j,k,2)   = MXadv(i,j,k,2)*cff
            MYadv(i,j,k,2)   = MYadv(i,j,k,2)*cff
            MVadv(i,j,k,2)   = MVadv(i,j,k,2)*cff
            MCor(i,j,k,2)    = MCor(i,j,k,2)*cff
            MPrsgrd(i,j,k,2) = MPrsgrd(i,j,k,2)*cff
            MHmix(i,j,k,2)   = MHmix(i,j,k,2)*cff
            MVmix(i,j,k,2)   = MVmix(i,j,k,2)*cff
            Mrate(i,j,k,2)   = Mrate(i,j,k,2)*cff
#  ifdef MRL_WCI
            Mvf(i,j,k,2)     = Mvf(i,j,k,2)*cff
            Mbrk(i,j,k,2)    = Mbrk(i,j,k,2)*cff
            MStCo(i,j,k,2)   = MStCo(i,j,k,2)*cff
            MVvf(i,j,k,2)    = MVvf(i,j,k,2)*cff
            MPrscrt(i,j,k,2) = MPrscrt(i,j,k,2)*cff
            Msbk(i,j,k,2)    = Msbk(i,j,k,2)*cff
            Mbwf(i,j,k,2)    = Mbwf(i,j,k,2)*cff
            Mfrc(i,j,k,2)    = Mfrc(i,j,k,2)*cff
#  endif
          enddo
        enddo
      enddo
# endif /* DIAGNOSTICS_UV */
!
!--------------------------------------------------------------------
!  Compute wet/dry masks for total velocity
!--------------------------------------------------------------------
!
# ifdef WET_DRY
      CALL wetdry_tile (Istr,Iend,Jstr,Jend)
# endif
!
!--------------------------------------------------------------------
! Set PHYSICAL lateral boundary conditions.
!--------------------------------------------------------------------
!
      call v3dbc_tile (Istr,Iend,Jstr,Jend, grad)   
      call u3dbc_tile (Istr,Iend,Jstr,Jend, grad)
!
!
!--------------------------------------------------------------------
! Coupling 2D and 3D momentum equations:
!
! Compute inaccurate vertical mean of the three-dimensional
! velocity field, then subtract it and replace it with the vertically
! integrated (barotropic) velocity field computed from the two-
! dimensional submodel. After that compute mass fluxes through grid
! box faces.
!
! Meaning of scratch variables below: 
!
! DC(i,k) [where k=1,N]  height of grid box for U- or V-cell. 
! DC(i,0) total depth of water column at horizontal U- or V-points.
! CF(i,0) vertically integrated mass flux/error/correction for
!                       the new time step velocity u,v(:,:,:,nnew)
! FC(i,0) vertically integrated mass flux/error/correction for the
!             intermediate time step [n+1/2] mass fluxes Huon,Hvom.
!
! This procedure also replaces instantaneous (with respect to the
! fast time step) vertically integrated (barotropic) velocities with
! their values based on fast-time-averaged mass fluxes. These are to
! be used as initial conditions for the barotropic mode at the new
! time step.
!
! Explanation of horizontal loop indices: in the case of periodic
! boundaries (in either direction) the coupling and computation of 
! mass fluxes Huon,Hvom is performed within the internal range of
! indices (excluding ghost zones), after that the ghost points for
! the newly computed arrays are exchanged; in the case of nonperiodic
! boundaries the coupling is done over the extendeed range of indices
! (that is including boundary points).
!--------------------------------------------------------------------
!
# ifdef EW_PERIODIC
#  define IU_RANGE Istr,Iend
#  define IV_RANGE Istr,Iend
# else
#  define IU_RANGE Istr,IendR
#  define IV_RANGE IstrR,IendR
# endif
!
# ifdef NS_PERIODIC
#  define JU_RANGE Jstr,Jend
#  define JV_RANGE Jstr,Jend
# else
#  define JU_RANGE JstrR,JendR
#  define JV_RANGE Jstr,JendR
# endif
!
!--------------------------------------------------
!  Couple XI-component of velocity
!--------------------------------------------------
!
      do j=JU_RANGE 
        do i=IU_RANGE
          DC(i,0)=0.
          CF(i,0)=0.
          FC(i,0)=0.
        enddo
        do k=1,N,+1
          do i=IU_RANGE
            DC(i,k)=0.5*(Hz(i,j,k)+Hz(i-1,j,k))*on_u(i,j)
            DC(i,0)=DC(i,0)+DC(i,k)
            CF(i,0)=CF(i,0)+DC(i,k)*u(i,j,k,nnew)
          enddo
        enddo
        do i=IU_RANGE
          DC(i,0)=1./DC(i,0)
          CF(i,0)=DC(i,0)*(CF(i,0)-DU_avg1(i,j,nnew))
# ifdef MRL_WCI
     &                               +ust2d(i,j)
# endif
          ubar(i,j,knew)=DC(i,0)*DU_avg1(i,j,nnew)
# ifdef MRL_WCI
     &                               -ust2d(i,j)
#  ifdef MASKING
          ubar(i,j,knew)=ubar(i,j,knew)*umask(i,j)
     &                +ust2d(i,j)*(umask(i,j)-1.0)
#  endif
# endif
# ifdef WET_DRY
          ubar(i,j,knew)=ubar(i,j,knew)*umask_wet(i,j)
# endif
        enddo
        do k=N,1,-1
          do i=IU_RANGE
            u(i,j,k,nnew)=(u(i,j,k,nnew)-CF(i,0)) 
# ifdef MASKING
     &                                *umask(i,j)
#  ifdef MRL_WCI
     &               +ust(i,j,k)*(umask(i,j)-1.0)
#  endif
# endif
# ifdef WET_DRY
            u(i,j,k,nnew)=u(i,j,k,nnew)*umask_wet(i,j)
#  ifdef MRL_WCI
            ust(i,j,k)=ust(i,j,k)*umask_wet(i,j)
#  endif
# endif

!--# define TR
# ifdef TR
#  ifdef MRL_WCI
            FC(i,k)=DC(i,k)*(0.5*(u(i,j,k,nstp)+u(i,j,k,nnew)) 
     &                                            +ust(i,j,k))
#  else
            FC(i,k)=0.5*(u(i,j,k,nstp)+u(i,j,k,nnew))*DC(i,k)
#  endif
# else
#  define EPSIL 0.125
#  define DELTA 0.75
#  ifdef MRL_WCI
            FC(i,k)=DELTA*Huon(i,j,k) +
     &              EPSIL*DC(i,k)*(u(i,j,k,nstp)+u(i,j,k,nnew) 
     &                                        +2.0*ust(i,j,k))
#  else
            FC(i,k)=DELTA*Huon(i,j,k) +
     &              EPSIL*DC(i,k)*(u(i,j,k,nstp)+u(i,j,k,nnew))
#  endif
# endif /* TR */

            FC(i,0)=FC(i,0)+FC(i,k)
          enddo
        enddo
        do i=IU_RANGE
          FC(i,0)=DC(i,0)*(FC(i,0)-DU_avg2(i,j))
        enddo
        do k=1,N,+1
          do i=IU_RANGE
            Huon(i,j,k)=FC(i,k)-DC(i,k)*FC(i,0)
          enddo
        enddo
!
!-------------------------------------------------
! Compute density XI gradients for use in t3dmix
!-------------------------------------------------
!
# if !defined TS_MIX_S && \
      (defined TS_DIF4 || defined TS_DIF2 || defined SPONGE_DIF2)
!
        do k=1,N-1,+1
          do i=IU_RANGE
            cff =0.5*(pm(i,j)+pm(i-1,j)) SWITCH umask(i,j)
#  ifdef TS_MIX_ISO
#   ifdef SPLIT_EOS
            dpth=0.5*( z_w(i,j,N)+z_w(i-1,j,N)
     &                -z_r(i,j,k)-z_r(i-1,j,k))
            dRx=cff*( rho1(i,j,k)-rho1(i-1,j,k)
     &             +(  qp1(i,j,k)- qp1(i-1,j,k))
     &                     *dpth*(1.-qp2*dpth) )
#   else
            dRx=cff*(rho1(i,j,k)-rho1(i-1,j,k))
#   endif
#   ifdef TS_MIX_ISO_FILT
            dRx  = cfilt*dRx + (1.-cfilt)*dRdx(i,j,k)  ! exp. time smoothing
#   endif
            dRdx(i,j,k)=dRx
#  elif defined TS_MIX_GEO
            dRdx(i,j,k)=cff*(z_r(i,j,k)-z_r(i-1,j,k))  ! dRdx get dZdx values
#  else
            dRdx(i,j,k)=0.
#  endif
          enddo
        enddo
        do i=IU_RANGE
          dRdx(i,j,N)=0.  ! no rotation at surface level
        enddo
!
!  Smooth vertically at interior points.
!
#  ifdef TS_MIX_ISO_FILT
      if (mod(iic-ntstart,ismooth).eq.0) then
        do k=N-1,2,-1
          do i=IU_RANGE
            dRdx(i,j,k)=0.05*dRdx(i,j,k-1)+
     &                  0.90*dRdx(i,j,k  )+
     &                  0.05*dRdx(i,j,k+1)
          enddo
        enddo
      endif
#  endif /* TS_MIX_ISO_FILT */
!
# endif /* TS_DIF2 || TS_DIF4 || SPONGE_DIF2 */
!
!--------------------------------------------------
!  Couple ETA-component of velocity
!--------------------------------------------------
!
        if (j.ge.Jstr) then
          do i=IV_RANGE
            DC(i,0)=0.
            CF(i,0)=0.
            FC(i,0)=0.
          enddo
          do k=1,N,+1
            do i=IV_RANGE
              DC(i,k)=0.5*(Hz(i,j,k)+Hz(i,j-1,k))*om_v(i,j)
              DC(i,0)=DC(i,0)+DC(i,k)
              CF(i,0)=CF(i,0)+DC(i,k)*v(i,j,k,nnew)
            enddo
          enddo
          do i=IV_RANGE
            DC(i,0)=1./DC(i,0)
            CF(i,0)=DC(i,0)*(CF(i,0)-DV_avg1(i,j,nnew))
# ifdef MRL_WCI
     &                                 +vst2d(i,j)
# endif
            vbar(i,j,knew)=DC(i,0)*DV_avg1(i,j,nnew)
# ifdef MRL_WCI
     &                                 -vst2d(i,j)
#  ifdef MASKING
            vbar(i,j,knew)=vbar(i,j,knew)*vmask(i,j)
     &                  +vst2d(i,j)*(vmask(i,j)-1.0)
#  endif
# endif
# ifdef WET_DRY
          vbar(i,j,knew)=vbar(i,j,knew)*vmask_wet(i,j)
# endif
          enddo
          do k=N,1,-1
            do i=IV_RANGE
              v(i,j,k,nnew)=(v(i,j,k,nnew)-CF(i,0)) 
# ifdef MASKING
     &                                  *vmask(i,j)
#  ifdef MRL_WCI
     &                 +vst(i,j,k)*(vmask(i,j)-1.0)
#  endif
# endif
# ifdef WET_DRY
              v(i,j,k,nnew)=v(i,j,k,nnew)*vmask_wet(i,j)
#  ifdef MRL_WCI
              vst(i,j,k)=vst(i,j,k)*vmask_wet(i,j)
#  endif
# endif

# ifdef TR
#  ifdef MRL_WCI
              FC(i,k)=DC(i,k)*(0.5*(v(i,j,k,nstp)+v(i,j,k,nnew))
     &                                              +vst(i,j,k))
#  else
              FC(i,k)=0.5*(v(i,j,k,nstp)+v(i,j,k,nnew))*DC(i,k) 
#  endif
# else
#  ifdef MRL_WCI
              FC(i,k)=DELTA*Hvom(i,j,k)+
     &                EPSIL*DC(i,k)*(v(i,j,k,nstp)+v(i,j,k,nnew)
     &                                          +2.0*vst(i,j,k))
#  else
              FC(i,k)=DELTA*Hvom(i,j,k)+
     &                EPSIL*DC(i,k)*(v(i,j,k,nstp)+v(i,j,k,nnew))
#  endif
#  undef DELTA
#  undef EPSIL
# endif /* TR */

              FC(i,0)=FC(i,0)+FC(i,k)
            enddo
          enddo
          do i=IV_RANGE
            FC(i,0)=DC(i,0)*(FC(i,0)-DV_avg2(i,j))
          enddo
          do k=1,N,+1
            do i=IV_RANGE
              Hvom(i,j,k)=FC(i,k)-DC(i,k)*FC(i,0)
            enddo
          enddo
!
!-------------------------------------------------
! Compute density ETA gradients for use in t3dmix
!-------------------------------------------------
!
# if !defined TS_MIX_S && (defined TS_DIF4 || defined TS_DIF2)
          do k=1,N-1,+1
            do i=IV_RANGE
              cff=0.5*(pn(i,j)+pn(i,j-1)) SWITCH vmask(i,j)
#  ifdef  TS_MIX_ISO
#   ifdef SPLIT_EOS
              dpth=0.5*( z_w(i,j,N)+z_w(i,j-1,N)
     &                -  z_r(i,j,k)-z_r(i,j-1,k))
              dRe=cff*( rho1(i,j,k)-rho1(i,j-1,k)
     &               +(  qp1(i,j,k)- qp1(i,j-1,k))
     &                       *dpth*(1.-qp2*dpth) )
#   else
              dRe=cff*(rho1(i,j,k)-rho1(i,j-1,k))
#   endif
#   ifdef TS_MIX_ISO_FILT
              dRe  = cfilt*dRe + (1.-cfilt)*dRde(i,j,k)  ! exp. time smoothing
#   endif
              dRde(i,j,k)=dRe
#  elif defined TS_MIX_GEO
              dRde(i,j,k)=cff*(z_r(i,j,k)-z_r(i,j-1,k))  ! dRde gets dZde values
#  else
              dRde(i,j,k)=0.
#  endif
            enddo
          enddo
          do i=IV_RANGE
            dRde(i,j,N)=0.    ! no rotation at surface level
          enddo
!
!  Smooth vertically at interior points.
!
#  ifdef TS_MIX_ISO_FILT
      if (mod(iic-ntstart,ismooth).eq.0) then
        do k=N-1,2,-1
          do i=IV_RANGE
            dRde(i,j,k)=0.05*dRde(i,j,k-1)+
     &                  0.90*dRde(i,j,k  )+
     &                  0.05*dRde(i,j,k+1)
          enddo
        enddo
      endif
#  endif /* TS_MIX_ISO_FILT */
!
# endif /* TS_DIF2 || TS_DIF4 */
!
!
        endif  !<-- j.ge.Jstr
      enddo    !<-- j loop
!
!
!--------------------------------------------------------------------
! Apply mass point sources
!--------------------------------------------------------------------
! 
! This is done after the coupling is performed (for now) to avoid
! having (u,v) and (Huon,Hvom) affected by masked values or
! variables not containing mass point sources info during the 
! coupling process (P. Marchesiello, 17 Jun 2004).
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
        if (IstrR.le.i .and. i.le.IendR .and.
     &      JstrR.le.j .and. j.le.JendR) then
          if (Dsrc(is).eq.0) then
            do k=1,N
              u(i,j,k,nnew)=2.*Qsrc(is,k)/( on_u(i,j)*(
     &                       z_w(i-1,j,k)-z_w(i-1,j,k-1)
     &                      +z_w(i  ,j,k)-z_w(i  ,j,k-1)
     &                                                ))
              Huon(i,j,k)=0.5*(Hz(i,j,k)+Hz(i-1,j,k))*on_u(i,j)*
     &                                            u(i,j,k,nnew)
            enddo
          else
            do k=1,N
              v(i,j,k,nnew)=2.*Qsrc(is,k)/( om_v(i,j)*(
     &                       z_w(i,j-1,k)-z_w(i,j-1,k-1)
     &                      +z_w(i,j  ,k)-z_w(i,j  ,k-1)
     &                                                ))
              Hvom(i,j,k)=0.5*(Hz(i,j,k)+Hz(i,j-1,k))*om_v(i,j)*
     &                                            v(i,j,k,nnew)
            enddo
          endif
        endif
      enddo
# endif
!
!--------------------------------------------------------------------
!  Set 3D Momemtum nudging
!--------------------------------------------------------------------
!
# if defined M3NUDGING && defined M3CLIMATOLOGY

#  ifdef ZONAL_NUDGING
      if (iic.eq.ntstart .or. mod(iic,10).eq.0) then
        call zonavg_3d(istr,iend,jstr,jend,
     &                        u(START_2D_ARRAY,1,nnew),uzon)
        call zonavg_3d(istr,iend,jstr,jend,
     &                        v(START_2D_ARRAY,1,nnew),vzon)
      endif
      if (iic.eq.ntstart) then
        call zonavg_3d(istr,iend,jstr,jend,
     &                        uclm(START_2D_ARRAY,1),uclmzon)
        call zonavg_3d(istr,iend,jstr,jend,
     &                        vclm(START_2D_ARRAY,1),vclmzon)
      endif
#  endif /* ZONAL_NUDGING */

      do k=1,N
        do j=JU_RANGE
          do i=IU_RANGE
            u(i,j,k,nnew)=u(i,j,k,nnew)+dt*M3nudgcof(i,j)*
#  ifdef ZONAL_NUDGING
     &                           (uclmzon(j,k)-uzon(j,k))     
#  else
     &                           (uclm(i,j,k)-u(i,j,k,nnew))
#  endif /* ZONAL_NUDGING */
#  ifdef MASKING
            u(i,j,k,nnew)=u(i,j,k,nnew)*umask(i,j)
#  endif
# ifdef WET_DRY
            u(i,j,k,nnew)=u(i,j,k,nnew)*umask_wet(i,j)
# endif
          enddo
        enddo
      enddo
      do k=1,N
        do j=JV_RANGE
          do i=IV_RANGE
            v(i,j,k,nnew)=v(i,j,k,nnew)+dt*M3nudgcof(i,j)*
#  ifdef ZONAL_NUDGING
     &                            (vclmzon(j,k)-vzon(j,k))     
#  else
     &                            (vclm(i,j,k)-v(i,j,k,nnew))
#  endif
#  ifdef MASKING
            v(i,j,k,nnew)=v(i,j,k,nnew)*vmask(i,j)
#  endif
# ifdef WET_DRY
            v(i,j,k,nnew)=v(i,j,k,nnew)*vmask_wet(i,j)
# endif
          enddo
        enddo
      enddo
# endif

# undef IU_RANGE
# undef JU_RANGE
# undef IV_RANGE
# undef JV_RANGE
!
!--------------------------------------------------------------------
! Exchange periodic boundaries and computational margins.
!--------------------------------------------------------------------
!
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,
     &                                 u(START_2D_ARRAY,1,nnew))
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend,
     &                                 v(START_2D_ARRAY,1,nnew))

      call exchange_u3d_tile (Istr,Iend,Jstr,Jend, 
     &                                   Huon(START_2D_ARRAY,1))
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend, 
     &                                   Hvom(START_2D_ARRAY,1))

      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,
     &                                   ubar(START_2D_ARRAY,knew))

      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,
     &                                   vbar(START_2D_ARRAY,knew))
#   if defined TS_MIX_ISO || defined TS_MIX_GEO
      call exchange_u3d_tile (istr,iend,jstr,jend, dRdx  )
      call exchange_v3d_tile (istr,iend,jstr,jend, dRde  )
#  endif
# endif
!
      return
      end
#else
      subroutine step3d_uv_empty 
      return
      end
#endif /* SOLVE3D */
