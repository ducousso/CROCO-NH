! $Id: bvf_mix.F 1458 2014-02-03 15:01:25Z gcambon $
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
#ifdef BVF_MIXING

      subroutine bvf_mix (tile)
      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call bvf_mix_tile (Istr,Iend,Jstr,Jend)
      return
      end

      subroutine bvf_mix_tile (Istr,Iend,Jstr,Jend)
      implicit none
# include "param.h"
# include "mixing.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j,k
      real bvf_numin, bvf_numax, bvf_nu0c, bvf_nu0, cff
      parameter (
     &    bvf_numin=3.e-5,  ! Upper and lower bounds for 
     &    bvf_numax=4.e-4,  !   vertical diffusion, [m2/s];
     &    bvf_nu0c=1.,      ! Proportionality constant, [m2/s2];
     &    bvf_nu0=1.0e-7)   ! Convective diffusion [m2/s] in 
                            !                unstable regime.
!
!  Compute diffusivities using Brunt-Vaisala frequency based vertical
!  mixing scheme.  Set viscosity to its background value. If static
!  unstable regime, set diffusivities to "bvf_nu0c".
!
      do k=1,N-1
        do j=Jstr,Jend
          do i=Istr,Iend
            Akv(i,j,k)=Akv_bak
            if (bvf(i,j,k).lt.0.) then
              Akv(i,j,k)=bvf_nu0c
              Akt(i,j,k,itemp)=bvf_nu0c
# ifdef SALINITY
              Akt(i,j,k,isalt)=bvf_nu0c
# endif
            elseif (bvf(i,j,k).eq.0.) then
              Akv(i,j,k)=Akv_bak
              Akt(i,j,k,itemp)=Akt_bak(itemp)
# ifdef SALINITY
              Akt(i,j,k,isalt)=Akt_bak(isalt)
# endif
            else
              cff=bvf_nu0/sqrt(bvf(i,j,k))
              Akt(i,j,k,itemp)=min(bvf_numax,max(bvf_numin,cff))
              Akv(i,j,k)=Akt(i,j,k,itemp)
# ifdef SALINITY
              Akt(i,j,k,isalt)=Akt(i,j,k,itemp)
# endif
            endif
          enddo
        enddo
      enddo

# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend, Akv)
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend,
     &                     Akt(START_2D_ARRAY,0,itemp))
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend,
     &                     Akt(START_2D_ARRAY,0,isalt))
# endif
#else
      subroutine bvf_mix_empty
#endif
      return
      end
