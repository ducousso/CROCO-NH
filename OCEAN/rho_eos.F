! $Id: rho_eos.F 1458 2014-02-03 15:01:25Z gcambon $
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
# ifdef NONLIN_EOS
# define DUKO_2001
!
      subroutine rho_eos (tile)
!
      implicit none
      integer tile, trd, omp_get_thread_num
#  include "param.h"
#  include "private_scratch.h"
#  include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call rho_eos_tile (Istr,Iend,Jstr,Jend, A2d(1,1,trd), 
     &                                        A2d(1,2,trd))
      return
      end

      subroutine rho_eos_tile (Istr,Iend,Jstr,Jend, K_up,K_dw)
!
!======================================================================
! Compute density anomaly via Equation Of State (EOS) for seawater.
! Following Jackett and McDougall, 1995, physical EOS is assumed to
! have form 
!
!                           rho0 + rho1(T,S)
!           rho(T,S,z) = ------------------------                 (1)
!                         1 - 0.1*|z|/K(T,S,|z|)
!
! where rho1(T,S) is sea-water density perturbation [kg/m^3] at
! standard pressure of 1 Atm (sea surface); |z| is absolute depth,
! i.e. distance from free-surface to the point at which density is
! computed, and
!
!     K(T,S,|z|) = K00 + K01(T,S) + K1(T,S)*|z| + K2(T,S)*|z|^2.  (2)
!
! To reduce errors of pressure-gradient scheme associated with
! nonlinearity of compressibility effects, as well as to reduce
! roundoff errors, the dominant part of density profile,
!
!                           rho0
!                     ----------------                            (3)
!                      1 - 0.1|z|/K00
!
! is removed from from (1). [Since (3) is purely a function of z,
! it does not contribute to pressure gradient.]  This results in
!
!                   rho1 - rho0*[K01+K1*|z|+K2*|z|^2]/[K00-0.1|z|] 
!    rho1 + 0.1|z| -----------------------------------------------
!                        K00 + K01 + (K1-0.1)*|z| + K2*|z|^2
!                                                                 (4)
! which is suitable for pressure-gradient calculation. 
!
! Optionally, if CPP-switch SPLIT_EOS is defined, term proportional 
! to |z| is linearized using smallness 0.1|z|/[K00 + K01] << 1 and
! the resultant EOS has form
! 
!              rho(T,S.z) = rho1(T,S) + qp1(T,S)*|z|               (5)
!
! where 
!                            rho1 - rho0*K01(T,S)/K00
!              qp1(T,S)= 0.1 --------------------------            (6)
!                                K00 + K01(T,S)
!
! is stored in a special array.
!
! This splitting allows representation of spatial derivatives (and
! also differences) of density as sum of adiabatic derivatives and
! compressible part according to 
!
!         d rho     d rho1           d qp1             d |z|
!        ------- = -------- + |z| * -------  +  qp1 * -------      (7)
!         d x,s      d x,s           d x,s            d x,s
!
!                  |<----- adiabatic ----->|   |<- compress ->|
!
! so that constraining of adiabatic derivative for monotonicity is
! equivalent to enforcement of physically stable stratification.
! [This separation and constraining algorithm is subsequently used
! in computation of pressure gradient within prsgrd32ACx-family
! schemes.] 
!
! If so prescribed compute the Brunt-Väisäla frequency [1/s^2] at
! horizontal RHO-points and vertical W-points,
!
!                          g    d rho  |
!             bvf^2 = - ------ ------- |                          (8)
!                        rho0    d z   | adiabatic
!
! where density anomaly difference is computed by adiabatically
! rising/lowering the water parcel from RHO point above/below to
! the W-point depth at "z_w".
!
! References:
! ----------
!  Shchepetkin, A.F., McWilliams, J.C., 2003: A method for computing 
!  horizontal pressure-gradient force in an oceanic model with a 
!  non-aligned vertical coordinate. J. Geophys. Res. 108 (C3), 3090.
!
!====================================================================== 
!
      implicit none
#  include "param.h"
      integer Istr,Iend,Jstr,Jend, i,j,k 
      real K_up(PRIVATE_1D_SCRATCH_ARRAY,0:N), K0,
     &     K_dw(PRIVATE_1D_SCRATCH_ARRAY,0:N), K1,K2,

     &     r00,r01,r02,r03,r04,r05,    K00,K01,K02,K03,K04,  dr00, 
     &     r10,r11,r12,r13,r14,        K10,K11,K12,K13,      Ts, Tt,
     &     rS0,rS1,rS2,                KS0,KS1,KS2,          sqrtTs,
     &     r20,                                              dpth,
     &             B00,B01,B02,B03,    E00,E01,E02,           
     &             B10,B11,B12,        E10,E11,E12,          cff,
     &                 BS1,                            cff1,cff2

      parameter(r00=999.842594,   r01=6.793952E-2,  r02=-9.095290E-3,
     &                            r03=1.001685E-4,  r04=-1.120083E-6,
     &                                              r05=6.536332E-9,
     &          r10=0.824493,     r11=-4.08990E-3,  r12=7.64380E-5,
     &                            r13=-8.24670E-7,  r14=5.38750E-9,
     &          rS0=-5.72466E-3,  rS1=1.02270E-4,   rS2=-1.65460E-6,
     &          r20=4.8314E-4,

     &          K00=19092.56,     K01=209.8925,     K02=-3.041638,
     &                            K03=-1.852732e-3, K04=-1.361629e-5,
     &          K10=104.4077,     K11=-6.500517,    K12=0.1553190,
     &                                              K13=2.326469e-4,
     &          KS0=-5.587545,    KS1=+0.7390729,   KS2=-1.909078e-2,


     &          B00=0.4721788,    B01=0.01028859,   B02=-2.512549e-4,
     &                                              B03=-5.939910e-7,
     &          B10=-0.01571896,  B11=-2.598241e-4, B12=7.267926e-6,
     &                            BS1=2.042967e-3,

     &          E00=+1.045941e-5, E01=-5.782165e-10,E02=+1.296821e-7,
     &          E10=-2.595994e-7, E11=-1.248266e-9, E12=-3.508914e-9)
#  ifdef DUKO_2001
      real K0_Duk
#  endif
#  include "grid.h"
#  include "ocean3d.h"
#  include "coupling.h"
#  include "scalars.h"
#  if defined ANA_VMIX || defined BVF_MIXING || defined GLS_MIXING \
      || defined LMD_MIXING || defined LMD_SKPP || defined LMD_BKPP
#   include "mixing.h"
#  endif
!
#  include "compute_extended_bounds.h"
!
#  ifdef DUKO_2001
      Tt=3.8D0
      Ts=34.5D0
      sqrtTs=sqrt(Ts)
      K0_Duk= Tt*( K01+Tt*( K02+Tt*( K03+Tt*K04 )))
     &       +Ts*( K10+Tt*( K11+Tt*( K12+Tt*K13 ))
     &            +sqrtTs*( KS0+Tt*( KS1+Tt*KS2 )))
#  endif

!
!  compute rho as a perturbation to rho0 (at the surface)
!
      dr00=r00-rho0
!
      do j=JstrR,JendR

        do k=1,N                                       ! NONLINEAR
          do i=IstrR,IendR                             !  EQUATION
            Tt=t(i,j,k,nrhs,itemp)                     !  OF STATE
#  ifdef SALINITY
            Ts=max(t(i,j,k,nrhs,isalt), 0.)
#  else
            Ts=34.5
#  endif
            sqrtTs=sqrt(Ts)
            rho1(i,j,k)=( dr00 +Tt*( r01+Tt*( r02+Tt*( r03+Tt*(
     &                                            r04+Tt*r05 ))))
     &                         +Ts*( r10+Tt*( r11+Tt*( r12+Tt*(
     &                                            r13+Tt*r14 )))
     &                              +sqrtTs*(rS0+Tt*(
     &                                    rS1+Tt*rS2 ))+Ts*r20 ))
#  ifdef MASKING
     &                                                *rmask(i,j)
#  endif     
            K0= Tt*( K01+Tt*( K02+Tt*( K03+Tt*K04 )))
     &         +Ts*( K10+Tt*( K11+Tt*( K12+Tt*K13 ))
     &              +sqrtTs*( KS0+Tt*( KS1+Tt*KS2 )))

#  ifdef SPLIT_EOS
#   ifdef DUKO_2001
            qp1(i,j,k)= 0.1D0*(rho0+rho1(i,j,k))*(K0_Duk-K0)
     &                               /((K00+K0)*(K00+K0_Duk))
#   else
            qp1(i,j,k)=0.1*(K00*rho1(i,j,k)-rho0*K0)/(K00*(K00+K0))
#   endif
#   ifdef MASKING 
     &                                                  *rmask(i,j)
#   endif
            dpth=z_w(i,j,N)-z_r(i,j,k)
            rho(i,j,k)=rho1(i,j,k) +qp1(i,j,k)*dpth*(1.-qp2*dpth)

#  else /* ! SPLIT_EOS */

            K1=B00+Tt*(B01+Tt*(B02+Tt*B03)) +Ts*( B10+Tt*( B11
     &                                    +Tt*B12 )+sqrtTs*BS1 )

            K2=E00+Tt*(E01+Tt*E02) +Ts*(E10+Tt*(E11+Tt*E12))

            dpth=z_w(i,j,N)-z_r(i,j,k)
            cff=K00-0.1*dpth
            cff1=K0+dpth*(K1+K2*dpth)
            rho(i,j,k)=( rho1(i,j,k)*cff*(K00+cff1)
     &                           -0.1*dpth*rho0*cff1
     &                            )/(cff*(cff+cff1))
#  endif /* SPLIT_EOS */
#  ifdef MASKING
            rho(i,j,k)=rho(i,j,k)*rmask(i,j)
#  endif
!
!-----------------------------------------------------------------------
!  Compute Brunt-Vaisala frequency (1/s2) at horizontal RHO-points
!  and vertical W-points!                  
!-----------------------------------------------------------------------
!
#  if defined ANA_VMIX || defined BVF_MIXING || defined GLS_MIXING \
      || defined LMD_MIXING || defined LMD_SKPP || defined LMD_BKPP
#   ifndef SPLIT_EOS
            dpth=z_w(i,j,N)-z_w(i,j,k)
            K_up(i,k)=K0+dpth*(K1+K2*dpth)
            dpth=z_w(i,j,N)-z_w(i,j,k-1)
            K_dw(i,k)=K0+dpth*(K1+K2*dpth)
#   endif
          enddo
        enddo

        cff=g/rho0
        do k=1,N-1
          do i=IstrR,IendR
#   ifdef SPLIT_EOS
            dpth=z_w(i,j,N)-0.5*(z_r(i,j,k+1)+z_r(i,j,k))
            cff2=( rho1(i,j,k+1)-rho1(i,j,k)             ! Elementary
     &                        +(qp1(i,j,k+1)-qp1(i,j,k)) ! adiabatic 
     &                            *dpth*(1.-2.*qp2*dpth) ! difference
     &                       )

            bvf(i,j,k)=-cff*cff2 / (z_r(i,j,k+1)-z_r(i,j,k))
#   else
           cff1=0.1*(z_w(i,j,N)-z_w(i,j,k))

            bvf(i,j,k)=-cff*(   (rho1(i,j,k+1)-rho1(i,j,k))
     &                        *(K00+K_dw(i,k+1))*(K00+K_up(i,k))

     &                     -cff1*( rho0*(K_dw(i,k+1)-K_up(i,k))
     &                            +K00*(rho1(i,j,k+1)-rho1(i,j,k))
     &                                  +rho1(i,j,k+1)*K_dw(i,k+1)
     &                                      -rho1(i,j,k)*K_up(i,k)

     &          ) )/(  (K00+K_dw(i,k+1)-cff1)*(K00+K_up(i,k)-cff1)
     &                                  *(z_r(i,j,k+1)-z_r(i,j,k))
     &                                                          )
#   endif
#   ifdef MASKING
            bvf (i,j,k)= bvf (i,j,k)*rmask(i,j)
#   endif
#  endif /* ANA_VMIX ... */
          enddo
        enddo
#  if defined ANA_VMIX || defined BVF_MIXING || defined GLS_MIXING \
      || defined LMD_MIXING || defined LMD_SKPP || defined LMD_BKPP
        do i=istrR,iendR
          bvf(i,j,N)=bvf(i,j,N-1)
          bvf(i,j,0)=bvf(i,j,  1)
        enddo
#  endif

# else /* ! NONLIN_EOS */
!
!======================================================================
!               LINEAR EQUATION OF STATE
!======================================================================
!
      subroutine rho_eos (tile)

      implicit none 
      integer tile
#  include "param.h"
#  include "compute_tile_bounds.h"
      call rho_eos_tile (Istr,Iend,Jstr,Jend)
      return
      end

      subroutine rho_eos_tile (Istr,Iend,Jstr,Jend)
      implicit none
#  include "param.h"
      integer Istr,Iend,Jstr,Jend, i,j,k
      real    cff,cff1
#  include "grid.h"
#  include "ocean3d.h"
#  include "coupling.h"
#  include "scalars.h"
#  include "mixing.h"
!
#  include "compute_extended_bounds.h"
!
!-----------------------------------------------------------------------
!  Compute density anomaly sigma-t (kg/m3 - 1000) using the linear
!  equation of state. Then Convert sigma-t into density anomaly 
!  refered to rho0
!-----------------------------------------------------------------------
!
      do j=JstrR,JendR
        do k=1,N
          do i=IstrR,IendR
            rho1(i,j,k)=R0 -Tcoef*(t(i,j,k,nrhs,itemp)-T0)
#  ifdef SALINITY
     &                     +Scoef*(t(i,j,k,nrhs,isalt)-S0)
#  endif
            rho(i,j,k)=rho1(i,j,k)+1000-rho0
#  ifdef MASKING
            rho1(i,j,k)=rho1(i,j,k)*rmask(i,j)
            rho(i,j,k)=rho(i,j,k)*rmask(i,j)
#  endif
          enddo
        enddo
!
!-----------------------------------------------------------------------
!  Compute Brunt-Vaisala frequency (1/s2) at horizontal RHO-points
!  and vertical W-points.
!-----------------------------------------------------------------------
!
#  if defined ANA_VMIX || defined BVF_MIXING || defined GLS_MIXING \
      || defined LMD_MIXING || defined LMD_SKPP || defined LMD_BKPP
        cff=g/rho0
        do k=1,N-1
          do i=IstrR,IendR
            bvf(i,j,k)=cff*(rho(i,j,k)-rho(i,j,k+1))
     &                    /(z_r(i,j,k+1)-z_r(i,j,k))
#   ifdef MASKING
     &                                   *rmask(i,j)
#   endif
          enddo
        enddo
#  endif 
# endif /* NONLIN_EOS */

# ifdef VAR_RHO_2D
!
!======================================================================
!          COMPUTE DEPTH-AVERAGED DENSITY FOR MODE COUPLING
!                 OF BAROTROPIC PRESSURE GRADIENT
!======================================================================
! In the code segment below "rhoA" is vertically averaged density
! perturbation normalized by rho0, while "rhoS" is vertically
! integrated pressure normalized by (1/2)*total_depth^2/rho0, i.e.,
! by vertically integrated pressure generated by water column with
! uniform density rho0. Hence both "rhoA" and "rhoS" are
! nondimensional quantities of comparable value.
!----------------------------------------------------------------------
! 
        do i=IstrR,IendR 
#  ifdef SPLIT_EOS
          dpth=z_w(i,j,N)-z_r(i,j,N)
          cff=HZR(i,j,N)*(rho1(i,j,N)+qp1(i,j,N)*dpth*(1.-qp2*dpth))
#  else
          cff=HZR(i,j,N)*rho(i,j,N)
#  endif
          rhoS(i,j)=0.5*cff*HZR(i,j,N)
          rhoA(i,j)=cff
        enddo
        do k=N-1,1,-1
          do i=IstrR,IendR
#  ifdef SPLIT_EOS
            dpth=z_w(i,j,N)-z_r(i,j,k)
            cff=HZR(i,j,k)*(rho1(i,j,k)+qp1(i,j,k)*dpth*(1.-qp2*dpth))
#  else
            cff=HZR(i,j,k)*rho(i,j,k)
#  endif
            rhoS(i,j)=rhoS(i,j)+HZR(i,j,k)*(rhoA(i,j)+0.5*cff)
            rhoA(i,j)=rhoA(i,j)+cff
          enddo
        enddo
        cff1=1./rho0
        do i=IstrR,IendR
          cff=1./(z_w(i,j,N)-z_w(i,j,0))
          rhoA(i,j)=cff*cff1*rhoA(i,j)
          rhoS(i,j)=2.*cff*cff*cff1*rhoS(i,j)
        enddo
# endif /* VAR_RHO_2D */

      enddo    ! <-- j
      return
      end

# ifdef RESET_RHO0
!
!======================================================================
!
!               Reset background density rho0 (kg/m3)
!             as initial volume averaged sigma-t + 1000
!
! This is done to minimize Boussinesq errors. The alternative is to
! let model users choose rho0 appropriately in roms.in, but users
! generally miss it.
!
! Reference:
! ---------
! Shchepetkin A.F., J.C. McWilliams, 2011: Accurate Boussinesq oceanic 
! modeling with a practical, Stiffened Equation of State. 
! Ocean Modelling, 38, 41-70.
!
! Patrick Marchesiello Sep. 2015
!======================================================================
!
      subroutine reset_rho0 (tile)
!
      implicit none
      integer tile, trd, omp_get_thread_num
#  include "param.h"
#  include "private_scratch.h"
#  include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call reset_rho0_tile (Istr,Iend,Jstr,Jend)
      return
      end

      subroutine reset_rho0_tile (Istr,Iend,Jstr,Jend)
      implicit none
#  include "param.h"
      integer Istr,Iend,Jstr,Jend, i,j,k, NSUB
      real    cff,cff1
#  ifdef NONLIN_EOS
      real dr00, Ts,Tt,sqrtTs,
     &     r00,r01,r02,r03,r04,r05,
     &     r10,r11,r12,r13,r14,
     &     rS0,rS1,rS2,
     &     r20
      parameter(r00=999.842594,   r01=6.793952E-2,  r02=-9.095290E-3,
     &                            r03=1.001685E-4,  r04=-1.120083E-6,
     &                                              r05=6.536332E-9,
     &          r10=0.824493,     r11=-4.08990E-3,  r12=7.64380E-5,
     &                            r13=-8.24670E-7,  r14=5.38750E-9,
     &          rS0=-5.72466E-3,  rS1=1.02270E-4,   rS2=-1.65460E-6,
     &          r20=4.8314E-4)
#  endif
      real*QUAD mrho,vol
#  ifdef MPI
      include 'mpif.h'
#  include "mpi_roms.h"
      integer ierr
      real*QUAD allrho(1,NNODES),allvol(1,NNODES)
#  endif
#  include "grid.h"
#  include "ocean3d.h"
#  include "scalars.h"
!
#  include "compute_extended_bounds.h"
!
      mrho=QuadZero
      vol=QuadZero
#  ifdef NONLIN_EOS
      dr00=r00-1000.
#  endif
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
#  ifdef NONLIN_EOS
!
!  Compute density anomaly sigma-t(T,S) from nonlinear EOS
!
            Tt=t(i,j,k,1,itemp)
#   ifdef SALINITY
            Ts=max(t(i,j,k,1,isalt), 0.)
#   else
            Ts=34.5
#   endif
            sqrtTs=sqrt(Ts)
            rho1(i,j,k)=( dr00 +Tt*( r01+Tt*( r02+Tt*( r03+Tt*(
     &                                            r04+Tt*r05 ))))
     &                         +Ts*( r10+Tt*( r11+Tt*( r12+Tt*(
     &                                            r13+Tt*r14 )))
     &                              +sqrtTs*(rS0+Tt*(
     &                                    rS1+Tt*rS2 ))+Ts*r20 ))
#  else
!
!  Compute density anomaly sigma-t from linear EOS
!
            rho1(i,j,k)=R0 -Tcoef*(t(i,j,k,nrhs,itemp)-T0)
#   ifdef SALINITY
     &                     +Scoef*(t(i,j,k,nrhs,isalt)-S0)
#   endif
#  endif /* NONLIN_EOS */
!
!  Make local volume average of sigma-t(T,S)
!
            cff1=HZR(i,j,k)*om_r(i,j)*on_r(i,j)
            mrho=mrho+cff1*rho1(i,j,k)
            vol=vol+cff1
          enddo
        enddo
      enddo
      mrho=mrho/vol
!
!  Make global volume average of sigma-t(T,S) ~ sigma-t(T0,S0)
!
      if (SINGLE_TILE_MODE) then
        NSUB=1
       else
        NSUB=NSUB_X*NSUB_E
      endif
C$OMP CRITICAL (rho_cr_rgn)
      if (tile_count.eq.0) then
        avg_rho=QuadZero               ! Reset global summations
        avg_vol=QuadZero
      endif 
      avg_rho=avg_rho+mrho*vol         ! Make global summations
      avg_vol=avg_vol+vol              ! among threads
      tile_count=tile_count+1          ! This counter identifies
      if (tile_count.eq.NSUB) then     ! the last thread, whoever
        tile_count=0                   ! it is, not always master.
        mrho=avg_rho/avg_vol
#  ifdef MPI
        call MPI_ALLGATHER(mrho,1,MPI_DOUBLE_PRECISION,
     &                   allrho,1,MPI_DOUBLE_PRECISION,
     &                            MPI_COMM_WORLD,ierr)
        call MPI_ALLGATHER(vol,1,MPI_DOUBLE_PRECISION,
     &                  allvol,1,MPI_DOUBLE_PRECISION,
     &                           MPI_COMM_WORLD,ierr)
        mrho=QuadZero
        vol=QuadZero
        do i=1,NNODES
          mrho=mrho+allrho(1,i)*allvol(1,i)
          vol=vol+allvol(1,i)
        enddo
        rho0=1000. + mrho/vol   !  add 1000. to get rho0 from sigma-t(T0,S0)
#  else
        rho0=1000. + mrho
#  endif

        MPI_master_only write(stdout,'(F10.4,2x,A,1x,A/)')
     &        rho0, 'rho0     Reset Boussinesq approximation',
     &                                 'mean density, kg/m3.'

      endif  ! <-- tile_count.eq.nsubs
!
C$OMP END CRITICAL (rho_cr_rgn)
      return
      end
# endif /* RESET_RHO0 */

#else
      subroutine rho_eos_empty
      end
#endif /* SOLVE3D */

