! $Id: set_depth.F 1484 2014-03-17 14:01:55Z rblod $
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
#if defined SOLVE3D || defined WET_DRY

      subroutine set_depth (tile)
      implicit none
# include "param.h"
      integer tile, trd
C$    integer omp_get_thread_num
# include "compute_tile_bounds.h"
      call set_depth_tile (Istr,Iend,Jstr,Jend)
      return
      end

      subroutine set_depth_tile (Istr,Iend,Jstr,Jend)
!
!----------------------------------------------------------------------
! Create S-coordinate system: based on model topography h(i,j),
! fast-time-averaged free-surface field and vertical coordinate
! transformation metrics compute evolving depths of of the three-
! dimensional model grid (z_r,z_w) and vertical heights of model
! grid boxes.
!----------------------------------------------------------------------
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
      real cff_r,cff1_r,cff_w,cff1_w, z_r0,z_w0, zetatmp
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "coupling.h"
# include "scalars.h"
# ifdef MRL_WCI
#   include "forces.h"
# endif
# ifdef NBQ
#  include "nbq.h"
# endif
      real eps
      parameter (eps=1.e-14)
!
# include "compute_auxiliary_bounds.h"
!
# ifdef EW_PERIODIC
#  define I_RANGE Istr,Iend
# else
#  define I_RANGE IstrR,IendR
# endif
# ifdef NS_PERIODIC
#  define J_RANGE Jstr,Jend
# else
#  define J_RANGE JstrR,JendR
# endif
!
      if (iic.eq.0) then  ! During initialization and /or restart: copy initial
        do j=J_RANGE      ! free surface field into array for holding fast-time
          do i=I_RANGE    ! averaged free surface.
# ifdef WET_DRY
            wetdry(i,j)=1.
            IF (h(i,j).eq.0.) THEN
              h(i,j)=eps
            END IF
#  ifdef NBQ
            IF (zeta(i,j,1) .lt. Dcrit(i,j)) THEN
              zeta(i,j,1)=Dcrit(i,j)
#  else
            IF (zeta(i,j,1) .lt. Dcrit(i,j)-h(i,j)) THEN
              zeta(i,j,1)=Dcrit(i,j)-h(i,j)
#  endif
              zeta(i,j,2)=zeta(i,j,1)
              zeta(i,j,3)=zeta(i,j,1)
              zeta(i,j,4)=zeta(i,j,1)
              wetdry(i,j)=0.
            ENDIF
# endif
# ifdef SOLVE3D
#  ifdef NEW_S_COORD 
            hinv(i,j)=1./(h(i,j)+hc)      
#  else
            hinv(i,j)=1./h(i,j) 
#  endif
            Zt_avg1(i,j)=zeta(i,j,1)    
#  ifdef NBQ
            if (zeta(i,j,1).eq.0.) then     ! Set Zt_avg1 and rhobar_nbq_avg1
              Zt_avg1(i,j) = h(i,j)         ! before call to ana_initial (zeta=0)
              zeta(i,j,1)  = Zt_avg1(i,j)
              zeta(i,j,2)  = zeta(i,j,1)
              zeta(i,j,3)  = zeta(i,j,1)
              zeta(i,j,4)  = zeta(i,j,1)
            else
             if (zeta(i,j,1).eq.h(i,j).and.rhobar_nbq(i,j,1).ne.1.) then
              Zt_avg1(i,j) = rhobar_nbq(i,j,1)*h(i,j)
              zeta(i,j,1)  = Zt_avg1(i,j)
              zeta(i,j,2)  = zeta(i,j,1)
              zeta(i,j,3)  = zeta(i,j,1)
              zeta(i,j,4)  = zeta(i,j,1)
             endif
            endif
#  endif
# endif /* SOLVE3D */
          enddo
        enddo

# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend, hinv)
#  ifdef WET_DRY
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend, h)
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend, 
     &                        zeta(START_2D_ARRAY,1))
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend, 
     &                        zeta(START_2D_ARRAY,2))
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend, 
     &                        zeta(START_2D_ARRAY,3))
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend, 
     &                        zeta(START_2D_ARRAY,4))
#  endif
# endif
      endif   !  iic==0

# ifdef NBQ_CONSOUT
      call consout_nbq(10)
# endif

# ifdef SOLVE3D
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend, Zt_avg1)
#  endif

      do j=J_RANGE              !!! WARNING: Setting must be consistent 
        do i=I_RANGE            !!!          with omega.F
#  ifdef MOVING_BATHY
          h(i,j)=h(i,j)+dh(i,j)
#  endif
          z_w(i,j,0)=-h(i,j)
        enddo

        do k=1,N,+1
#  ifdef NEW_S_COORD
          cff_w =hc*sc_w(k)
          cff_r =hc*sc_r(k)
          cff1_w=Cs_w(k)
          cff1_r=Cs_r(k)
#  else
          cff_w =hc*(sc_w(k)-Cs_w(k))
          cff_r =hc*(sc_r(k)-Cs_r(k))
          cff1_w=Cs_w(k)
          cff1_r=Cs_r(k)
#  endif
          do i=I_RANGE
            zetatmp=Zt_avg1(i,j)
#  ifdef NBQ
     &              /rhobar_nbq_avg1(i,j)-h(i,j)
            if (rhobar_nbq_avg1(i,j).eq.0) stop 'Error set_depth (1)'
            if (rhobar_nbq(i,j,1).eq.0)    stop 'Error set_depth (2)'
#   if defined MASKING && !defined WET_DRY
            zetatmp=zetatmp*rmask(i,j)
#   endif
#  endif
            z_w0=cff_w+cff1_w*h(i,j) 
            z_r0=cff_r+cff1_r*h(i,j)            
#  ifdef NEW_S_COORD
#   ifdef MOVING_BATHY
            hinv(i,j)=1./(h(i,j)+hc)
#   endif
            z_w(i,j,k)=z_w0*h(i,j)*hinv(i,j)+zetatmp
     &                                       *(1.+z_w0*hinv(i,j)) 
            z_r(i,j,k)=z_r0*h(i,j)*hinv(i,j)+zetatmp
     &                                       *(1.+z_r0*hinv(i,j))
#  else
#   ifdef MOVING_BATHY
            hinv(i,j)=1./h(i,j)
#   endif
            z_w(i,j,k)=z_w0+zetatmp*(1.+z_w0*hinv(i,j))      
            z_r(i,j,k)=z_r0+zetatmp*(1.+z_r0*hinv(i,j))
#  endif
            Hz_bak(i,j,k)=Hz(i,j,k)
            Hz(i,j,k)=z_w(i,j,k)-z_w(i,j,k-1)
#  ifdef NBQ
            Hzr(i,j,k)=Hz(i,j,k)
            Hz(i,j,k) =Hz(i,j,k)*rho_nbq_avg1(i,j,k)
            if (rho_nbq_ext(i,j,k).eq.0) stop 'Error set_depth (3)'
#  endif
          enddo
        enddo
      enddo

#  ifdef NBQ_CONS5
      call cons_nbq(30)
#  endif
#  ifdef NBQ_CONSOUT
      call consout_nbq(11)
#  endif

#  undef I_RANGE
#  undef J_RANGE

#  if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend, 
     &                        z_w(START_2D_ARRAY,0))
      call exchange_r3d_tile (Istr,Iend,Jstr,Jend, 
     &                        z_r(START_2D_ARRAY,1))
      call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                        Hz(START_2D_ARRAY,1))
      call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                        Hz_bak(START_2D_ARRAY,1))
#   ifdef MOVING_BATHY
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend, h)
#   endif
#   ifdef NBQ
      call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                        Hzr(START_2D_ARRAY,1))

#   endif
#  endif 

#ifdef NHMG
!     Compute slopes
      do k = 1, N
         do j = 0,Mm+1
            do i = 0,Lm+1
               dzdxi (i,j,k)=0.5*(z_r(i+1,j,k)-z_r(i-1,j,k))*pn(i,j)  
               dzdeta(i,j,k)=0.5*(z_r(i,j+1,k)-z_r(i,j-1,k))*pm(i,j) 
            enddo
         enddo
      enddo
      ! z_r is buffer filled in the interior halos but not in the outer halos
      if (WESTERN_EDGE) then
         do k = 1, N
            do j = 0,Mm+1
               dzdxi (0,j,k) = 0.
               dzdxi (1,j,k) = 0.
               dzdeta(0,j,k) = 0.
               dzdeta(1,j,k) = 0.
            enddo
         enddo
       endif
      if (EASTERN_EDGE) then
         do k = 1, N
            do j = 0,Mm+1
               dzdxi (Lm  ,j,k) = 0.
               dzdxi (Lm+1,j,k) = 0.
               dzdeta(Lm  ,j,k) = 0.
               dzdeta(Lm+1,j,k) = 0.
            enddo
         enddo
       endif
      if (SOUTHERN_EDGE) then
         do k = 1, N
            do i = 0,Lm+1
               dzdxi (i,0,k) = 0.
               dzdxi (i,1,k) = 0.
               dzdeta(i,0,k) = 0.
               dzdeta(i,1,k) = 0.
            enddo
         enddo
       endif
      if (NORTHERN_EDGE) then
         do k = 1, N
            do i = 0,Lm+1
               dzdxi (i,Mm  ,k) = 0.
               dzdxi (i,Mm+1,k) = 0.
               dzdeta(i,Mm  ,k) = 0.
               dzdeta(i,Mm+1,k) = 0.
            enddo
         enddo
       endif

#endif /* NHMG */
      return
      end
!
!======================================================================
!
!                       subroutine set_HUV
!
!======================================================================
!
      subroutine set_HUV (tile)
      implicit none
#  include "param.h"
      integer tile, trd
C$    integer omp_get_thread_num
#  include "compute_tile_bounds.h"
      call set_HUV_tile (Istr,Iend,Jstr,Jend)
      return
      end

      subroutine set_HUV_tile (Istr,Iend,Jstr,Jend)

      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k

      real grad(PRIVATE_2D_SCRATCH_ARRAY)
#  include "param.h"
#  include "grid.h"
#  include "ocean3d.h"
#  include "scalars.h"
!
#  ifdef MRL_WCI
#    include "forces.h"
#  endif
#  include "compute_auxiliary_bounds.h"
!
#  ifdef EW_PERIODIC 
#   define IU_RANGE Istr,Iend
#   define IV_RANGE Istr,Iend
#  else
#   define IU_RANGE Istr,IendR
#   define IV_RANGE IstrR,IendR
#  endif

#  ifdef NS_PERIODIC 
#   define JU_RANGE Jstr,Jend
#   define JV_RANGE Jstr,Jend
#  else
#   define JU_RANGE JstrR,JendR
#   define JV_RANGE Jstr,JendR
#  endif

      do k=1,N
        do j=JU_RANGE
          do i=IU_RANGE
            Huon(i,j,k)=0.5*(Hz(i,j,k)+Hz(i-1,j,k))*on_u(i,j)
     &                                       *( u(i,j,k,nrhs)
#  ifdef MRL_WCI
     &                                           + ust(i,j,k)
#  endif
     &                                                      )
#  if defined MRL_WCI && defined MASKING
            Huon(i,j,k)=Huon(i,j,k)*umask(i,j)
#  endif

#  if defined UV_VIS4 && defined UV_MIX_GEO
            z_u(i,j,k)=0.5*(z_r(i,j,k)+z_r(i-1,j,k))
            dz_u(i,j,k)=z_r(i,j,k)-z_r(i-1,j,k)
#  endif
          enddo
        enddo
        do j=JV_RANGE
          do i=IV_RANGE
            Hvom(i,j,k)=0.5*(Hz(i,j,k)+Hz(i,j-1,k))*om_v(i,j)
     &                                       *( v(i,j,k,nrhs)
#  ifdef MRL_WCI
     &                                           + vst(i,j,k)
#  endif
     &                                                      )
#  if defined MRL_WCI && defined MASKING
            Hvom(i,j,k)=Hvom(i,j,k)*vmask(i,j)
#  endif

#  if defined UV_VIS4 && defined UV_MIX_GEO
            z_v(i,j,k)=0.5*(z_r(i,j,k)+z_r(i,j-1,k))
            dz_v(i,j,k)=z_r(i,j,k)-z_r(i,j-1,k)
#  endif
          enddo
        enddo
      enddo

#  undef IU_RANGE
#  undef JU_RANGE
#  undef IV_RANGE
#  undef JV_RANGE
!
! Exchange periodic boundaries, if so prescribed.
!
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend, 
     &                                         Huon(START_2D_ARRAY,1))
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend, 
     &                                         Hvom(START_2D_ARRAY,1))
#   if defined UV_VIS4 && defined UV_MIX_GEO
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,
     &                                         z_u(START_2D_ARRAY,1))
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend,
     &                                         z_v(START_2D_ARRAY,1))
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,
     &                                         dz_u(START_2D_ARRAY,1))
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend,
     &                                         dz_v(START_2D_ARRAY,1))
#   endif
#  endif

# ifdef NHMG
!
      if (iic.eq.0) then !! Only during initialization
!     TODO: If you had better information, this will overwrite it
!     TODO: We should be able to specify if we can do better in either ana_initial of reading netcdf files


        ! We initialize using continuity in the interior
        do j=Jstr,Jend
          do k=1,N
            do i=Istr,Iend
               wz(i,j,k,1)=wz(i,j,k-1,1)
     &        - pm(i,j)*pn(i,j)*( Huon(i+1,j,k)-Huon(i,j,k)
     &                           +Hvom(i,j+1,k)-Hvom(i,j,k) )
            enddo
          enddo
        enddo

        call w3dbc_tile (Istr,Iend,Jstr,Jend, grad)

#   if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
        call exchange_w3d_tile (Istr,Iend,Jstr,Jend,
     &                                 wz(START_2D_ARRAY,0,1))
#   endif
      endif
# endif /* NHMG */
      return
      end
!
!======================================================================
!
!                       subroutine set_HUV1
!
!======================================================================
!
      subroutine set_HUV1 (tile)
      implicit none
#  include "param.h"
      integer tile, trd
C$    integer omp_get_thread_num
#  include "compute_tile_bounds.h"
      call set_HUV1_tile (Istr,Iend,Jstr,Jend)
      return
      end

      subroutine set_HUV1_tile (Istr,Iend,Jstr,Jend)
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
#  include "param.h"
#  include "grid.h"
#  include "ocean3d.h"
#  include "scalars.h"
#  ifdef MRL_WCI
#    include "forces.h"
#  endif
!
#  include "compute_auxiliary_bounds.h"
!
#  ifdef EW_PERIODIC
#   define IU_RANGE Istr,Iend
#   define IV_RANGE Istr,Iend
#  else
#   define IU_RANGE Istr,IendR
#   define IV_RANGE IstrR,IendR
#  endif

#  ifdef NS_PERIODIC
#   define JU_RANGE Jstr,Jend
#   define JV_RANGE Jstr,Jend
#  else
#   define JU_RANGE JstrR,JendR
#   define JV_RANGE Jstr,JendR
#  endif

      do k=1,N
        do j=JU_RANGE
          do i=IU_RANGE
            Huon(i,j,k)=0.25*(3.*(Hz(i,j,k)+Hz(i-1,j,k))
     &                   -Hz_bak(i,j,k)-Hz_bak(i-1,j,k))
     &                        *on_u(i,j)*( u(i,j,k,nrhs)
#  ifdef MRL_WCI
     &                                      + ust(i,j,k) 
#  endif
     &                                                 )
#  if defined MRL_WCI && defined MASKING
            Huon(i,j,k)=Huon(i,j,k)*umask(i,j)
#  endif

          enddo
        enddo
        do j=JV_RANGE
          do i=IV_RANGE
            Hvom(i,j,k)=0.25*( 3.*(Hz(i,j,k)+Hz(i,j-1,k))
     &                    -Hz_bak(i,j,k)-Hz_bak(i,j-1,k))
     &                         *om_v(i,j)*( v(i,j,k,nrhs)
#  ifdef MRL_WCI
     &                                       + vst(i,j,k)
#  endif
     &                                                  )
#  if defined MRL_WCI && defined MASKING
            Hvom(i,j,k)=Hvom(i,j,k)*vmask(i,j)
#  endif
          enddo
        enddo
      enddo
#  undef IU_RANGE
#  undef JU_RANGE
#  undef IV_RANGE
#  undef JV_RANGE
!
! Exchange periodic boundaries, if so prescribed.
!
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend, 
     &                                     Huon(START_2D_ARRAY,1))
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend,
     &                                     Hvom(START_2D_ARRAY,1))
#  endif
      return
      end
!
!======================================================================
!
!                       subroutine set_HUV2
!
!======================================================================
!
      subroutine set_HUV2 (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
#  include "param.h"
#  include "private_scratch.h"
#  include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call set_HUV2_tile (Istr,Iend,Jstr,Jend, A2d(1,1,trd),
     &                                         A2d(1,2,trd))
      return
      end

      subroutine set_HUV2_tile (Istr,Iend,Jstr,Jend,DC,FC)
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
#  include "param.h"

      real DC(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     FC(PRIVATE_1D_SCRATCH_ARRAY,0:N)

      real grad(PRIVATE_2D_SCRATCH_ARRAY)
#  include "grid.h"
#  include "ocean3d.h"
#  include "scalars.h"
#  ifdef MRL_WCI
#    include "forces.h"
#  endif
#  include "coupling.h"
!
#  include "compute_auxiliary_bounds.h"
!
#  ifdef EW_PERIODIC
#   define IU_RANGE Istr,Iend
#   define IV_RANGE Istr,Iend
#  else
#   define IU_RANGE Istr,IendR
#   define IV_RANGE IstrR,IendR
#  endif

#  ifdef NS_PERIODIC
#   define JU_RANGE Jstr,Jend
#   define JV_RANGE Jstr,Jend
#  else
#   define JU_RANGE JstrR,JendR
#   define JV_RANGE Jstr,JendR
#  endif

      do j=JU_RANGE
        do i=IU_RANGE
          DC(i,0)=0.
          FC(i,0)=0.
        enddo
        do k=1,N
          do i=IU_RANGE
            DC(i,k)=0.5*(Hz(i,j,k)+Hz(i-1,j,k))*on_u(i,j)
            DC(i,0)=DC(i,0)+DC(i,k)
            FC(i,0)=FC(i,0)+DC(i,k)*u(i,j,k,nrhs)
          enddo
        enddo


        do i=IU_RANGE
          FC(i,0)=(FC(i,0)-DU_avg2(i,j))/DC(i,0)
#  ifdef MRL_WCI
     &                               +ust2d(i,j)
#  endif
        enddo
        do k=1,N
          do i=IU_RANGE
#  ifndef NO_SPLIT
            u(i,j,k,nrhs)=(u(i,j,k,nrhs)-FC(i,0))
#  else
            u(i,j,k,nrhs)=u(i,j,k,nrhs)
#  endif
#  ifdef MASKING
     &                                *umask(i,j)
#    ifdef MRL_WCI
     &               +ust(i,j,k)*(umask(i,j)-1.0)
#    endif
#  endif
            Huon(i,j,k)=DC(i,k)*( u(i,j,k,nrhs)
#  ifdef MRL_WCI
     &                             + ust(i,j,k)
#  endif
     &                                        )
          enddo
        enddo

      enddo

      do j=JV_RANGE
        do i=IV_RANGE
          DC(i,0)=0.
          FC(i,0)=0.
        enddo
        do k=1,N
          do i=IV_RANGE
            DC(i,k)=0.5*(Hz(i,j,k)+Hz(i,j-1,k))*om_v(i,j)
            DC(i,0)=DC(i,0)+DC(i,k)
            FC(i,0)=FC(i,0)+DC(i,k)*v(i,j,k,nrhs)
          enddo
        enddo

        do i=IV_RANGE
          FC(i,0)=(FC(i,0)-DV_avg2(i,j))/DC(i,0)
#  ifdef MRL_WCI
     &                               +vst2d(i,j)
#  endif
        enddo
        do k=1,N
          do i=IV_RANGE
#  ifndef NO_SPLIT
            v(i,j,k,nrhs)=(v(i,j,k,nrhs)-FC(i,0))
#  else
            v(i,j,k,nrhs)=v(i,j,k,nrhs)
#  endif
#  ifdef MASKING
     &                                *vmask(i,j)
#    ifdef MRL_WCI
     &               +vst(i,j,k)*(vmask(i,j)-1.0)
#    endif
#  endif
            Hvom(i,j,k)=DC(i,k)*( v(i,j,k,nrhs)
#  ifdef MRL_WCI
     &                             + vst(i,j,k)
#  endif
     &                                        )
          enddo
        enddo
      enddo

#  undef IU_RANGE
#  undef JU_RANGE
#  undef IV_RANGE
#  undef JV_RANGE
!
! Exchange periodic boundaries, if so prescribed.
!
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,
     &                                    Huon(START_2D_ARRAY,1))
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,
     &                                  u(START_2D_ARRAY,1,nrhs))
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend,
     &                                    Hvom(START_2D_ARRAY,1))
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend,
     &                                  v(START_2D_ARRAY,1,nrhs))
#  endif

# if defined NHMG && !defined NO_SPLIT

!     Since we have corrected u and v in the 2D to 3D coupling, we now need to make wz consistent again
      do j=Jstr,Jend
        do k=1,N          
          do i=Istr,Iend
            DC(i,0)=pm(i,j)*pn(i,j)
            ! at this time, Huon and Hvom can used : 
            ! they are defined at n+1/2
            wz(i,j,k,nrhs)=wz(i,j,k-1,nrhs) 
     &      - DC(i,0)*( Huon(i+1,j,k)-Huon(i,j,k)
     &                 +Hvom(i,j+1,k)-Hvom(i,j,k) )
          enddo
        enddo
      enddo

      call w3dbc_tile (Istr,Iend,Jstr,Jend, grad)


! exchange
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend,
     &                                 wz(START_2D_ARRAY,0,nrhs)) 
# endif

# endif /* NHMG */

# endif /* SOLVE3D */
      return
      end
#else
      subroutine set_depth_empty
      return
      end
#endif /* SOLVE3D || WET_DRY */

