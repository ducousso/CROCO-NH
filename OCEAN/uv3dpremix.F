! $Id: uv3dpremix.F 1458 2014-02-03 15:01:25Z gcambon $

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
# if defined SOLVE3D && defined AGRIF
 
      subroutine uv3dpremix (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()

      call uv3dpremix_tile (Istr,Iend,Jstr,Jend,
     &        A3d(1,1,trd),A3d(1,2,trd))
      return
      end
 
      subroutine uv3dpremix_tile (Istr,Iend,Jstr,Jend,CF,DC)
!
! Computes harmonic mixing of momentum, along constant S-surfaces
! as horizontal divergence of the stress tensor. Components of the
! stress tensor are:
!                         du     dv
!         s_xx = -s_yy = ---- - -----
!                         dx     dy
!
!                         du     dv
!         s_xy =  s_yx = ---- + ----
!                         dy     dx
!
! Reference:
!
! Wajsowicz, R.C, 1993: A consistent formulation of the anisotropic
!     stress tensor for use in models of the large-scale ocean
!     circulation, JCP, 105, 333-338.
!
! Sadourny, R. and K. Maynard, 1997: Formulations of lateral
!     diffusion in geophysical fluid dynamics models, In "Numerical
!     Methods of Atmospheric and Oceanic Modelling". Lin, Laprise,
!     and Ritchie, Eds., NRC Research Press, 547-556.
!
! Griffies, S.M. and R.W. Hallberg, 2000: Biharmonic friction with
!     a Smagorinsky-like viscosity for use in large-scale eddy-
!     permitting ocean models, Mon. Wea. Rev., 128, 8, 2935-2946.
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k, indx
      real CF(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &     DC(PRIVATE_2D_SCRATCH_ARRAY,0:N)
     
# include "param.h"
# include "scalars.h"
# include "grid.h"
# include "ocean3d.h"
# include "coupling.h"
# include "mixing.h"
# include "zoom.h"
# ifdef DIAGNOSTICS_UV
#  include "diagnostics.h"
# endif
!
# ifdef MPI
#  define LOCALLM Lmmpi
#  define LOCALMM Mmmpi
# else
#  define LOCALLM Lm
#  define LOCALMM Mm
# endif
       integer decal
       real tinterp,onemtinterp, rrhot
       integer :: nold
       integer :: irhot
       external interpspongeu, interpspongev
       integer :: parentnbstep
       real DC(Istr-1:Iend+1,Jstr-1:Jend+1,0:N)
       real CF(Istr-1:Iend+1,Jstr-1:Jend+1,0:N)
!
# include "compute_auxiliary_bounds.h"

      irhot = Agrif_Irhot()
      rrhot = real(irhot)
      decal = 2*max(Agrif_Irhox(),Agrif_Irhoy())

      parentnbstep=Agrif_Parent_Nb_Step()
                       
C$OMP BARRIER
C$OMP MASTER

      If ((nbcoarse == 1).AND.(UVspongeTimeindex .NE. parentnbstep)) THEN

      Call Agrif_Set_bc(uspongeid,(/-decal,0/),
     &         InterpolationShouldbemade=.TRUE.)
      Call Agrif_Set_bc(vspongeid,(/-decal,0/),
     &         InterpolationShouldbemade=.TRUE.)

       tinterp = 1.

      Call Agrif_Bc_Variable(uspongeid,calledweight=tinterp,
     &                         procname=interpspongeu)
      Call Agrif_Bc_Variable(vspongeid,calledweight=tinterp,
     &                         procname=interpspongev)

      UVTimesponge = 3 - UVTimesponge
        UVspongeTimeindex = parentnbstep
        UVspongeTimeindex2 = agrif_nb_step()

      ENDIF

C$OMP END MASTER
C$OMP BARRIER

       if (agrif_nb_step() .EQ. UVspongeTimeindex2) then

       if (SOUTHERN_EDGE) then  
         do k=1,N

         do j=JstrR,JstrR+decal
         do i=Istr,IendR
          U_sponge_south(i,j,k,UVTimesponge)=
     &              usponge(i,j,k)       
#ifdef MASKING
     &    *umask(i,j)
#endif
         enddo
         enddo

         do j=Jstr,Jstr+decal-1
         do i=IstrR,IendR
          V_sponge_south(i,j,k,UVTimesponge)=
     &              vsponge(i,j,k)         
#ifdef MASKING
     &    *vmask(i,j)
#endif
         enddo
         enddo

         enddo
       endif
                     
      if (NORTHERN_EDGE) then
         do k=1,N
         do j=JendR-decal,JendR
         do i=Istr,IendR
          U_sponge_north(i,j,k,UVTimesponge)=
     &              usponge(i,j,k)          
#ifdef MASKING
     &    *vmask(i,j)
#endif
         enddo
         enddo

         do j=JendR-decal+1,JendR
         do i=IstrR,IendR
          V_sponge_north(i,j,k,UVTimesponge)=
     &              vsponge(i,j,k)     
#ifdef MASKING
     &    *vmask(i,j)
#endif
         enddo
         enddo

         enddo
       endif               


      if (WESTERN_EDGE) then
         do k=1,N
         do j=JstrR,JendR
         do i=Istr,Istr+decal-1 
          U_sponge_west(i,j,k,UVTimesponge)=
     &              usponge(i,j,k)       
         enddo
         enddo

         do j=Jstr,JendR
         do i=IstrR,IstrR+decal      
          V_sponge_west(i,j,k,UVTimesponge)=
     &              vsponge(i,j,k)        
         enddo
         enddo

         enddo


       endif
                    
      if (EASTERN_EDGE) then  
         do k=1,N
         do j=JstrR,JendR
         do i=IendR-decal+1,IendR
          U_sponge_east(i,j,k,UVTimesponge)=
     &              usponge(i,j,k)    
         enddo
         enddo

         do j=Jstr,JendR
         do i=IendR-decal,IendR
          V_sponge_east(i,j,k,UVTimesponge)=
     &              vsponge(i,j,k)             
         enddo
         enddo

         enddo  
       endif
      
      ENDIF    


       tinterp = real(nbcoarse-1)/rrhot
       IF (nbstep3d .LT. irhot) tinterp = 0.
       onemtinterp = -tinterp
       tinterp = 1.+tinterp
       
       nold = 3 - UVTimesponge

       if (SOUTHERN_EDGE) then
       do k=1,N
       do j=JstrR,JstrR+decal
       do i=Istr,IendR
          usponge(i,j,k) =
     &          onemtinterp*U_sponge_south(i,j,k,nold)
     &       +tinterp*U_sponge_south(i,j,k,UVTimesponge)
       enddo
       enddo

       do j=Jstr,Jstr+decal-1
       do i=IstrR,IendR
          vsponge(i,j,k) =
     &   onemtinterp*V_sponge_south(i,j,k,nold)
     &       +tinterp*V_sponge_south(i,j,k,UVTimesponge)  
       enddo
       enddo

       enddo
       endif
       
       if (NORTHERN_EDGE) then
       do k=1,N
       do j=JendR-decal,JendR
       do i=Istr,IendR
          usponge(i,j,k) =
     &       onemtinterp*U_sponge_north(i,j,k,nold)
     &       +tinterp*U_sponge_north(i,j,k,UVTimesponge)
       enddo
       enddo

       do j=JendR-decal+1,JendR
       do i=IstrR,IendR
          vsponge(i,j,k) =
     &       onemtinterp*V_sponge_north(i,j,k,nold)
     &       +tinterp*V_sponge_north(i,j,k,UVTimesponge)       
       enddo
       enddo

       enddo           
       endif
       
      if (WESTERN_EDGE) then
         do k=1,N
         do j=JstrR,JendR
         do i=Istr,Istr+decal-1
          usponge(i,j,k) =
     &       onemtinterp*U_sponge_west(i,j,k,nold)
     &       +tinterp*U_sponge_west(i,j,k,UVTimesponge)


         enddo
         enddo

         do j=Jstr,JendR
         do i=IstrR,IstrR+decal
          vsponge(i,j,k) =
     &       onemtinterp*V_sponge_west(i,j,k,nold)
     &       +tinterp*V_sponge_west(i,j,k,UVTimesponge)
         enddo
         enddo

         enddo

       endif      
       
      if (EASTERN_EDGE) then    
         do k=1,N
         do j=JstrR,JendR
         do i=IendR-decal+1,IendR
          usponge(i,j,k) =
     &       onemtinterp*U_sponge_east(i,j,k,nold)
     &       +tinterp*U_sponge_east(i,j,k,UVTimesponge)
         enddo
         enddo

         do j=Jstr,JendR
         do i=IendR-decal,IendR
          vsponge(i,j,k) =
     &       onemtinterp*V_sponge_east(i,j,k,nold)
     &       +tinterp*V_sponge_east(i,j,k,UVTimesponge)       
         enddo
         enddo

         enddo 
       endif

         do j=JstrR,JendR
         do i=IstrR,IendR
          DC(i,j,0) = 0.
          CF(i,j,0) = 0.
         enddo
         enddo
                          
         do k=1,N
         do j=JstrR,JendR
         do i=Istr,IendR
          DC(i,j,k) = 0.5*(Hz(i,j,k)+Hz(i-1,j,k))*on_u(i,j)
          DC(i,j,0) = DC(i,j,0) + DC(i,j,k)
          CF(i,j,0) = CF(i,j,0) + DC(i,j,k)*usponge(i,j,k)
         enddo
         enddo
         enddo

         do j=JstrR,JendR
         do i=Istr,IendR
         DC(i,j,0)=1./DC(i,j,0)
         CF(i,j,0)=DC(i,j,0)
     &                   *(CF(i,j,0)-DU_avg1(i,j,nstp))
         enddo
         enddo

         do k = N,1,-1
         do j=JstrR,JendR
         do i=Istr,IendR
         usponge(i,j,k) = (usponge(i,j,k)-CF(i,j,0))
#ifdef MASKING
     &             * umask(i,j)
#endif
         enddo
         enddo
         enddo

         do j=JstrR,JendR
         do i=IstrR,IendR
          DC(i,j,0) = 0.
          CF(i,j,0) = 0.
         enddo
         enddo

         do k=1,N
         do j=Jstr,JendR
         do i=IstrR,IendR
          DC(i,j,k) = 0.5*(Hz(i,j,k)+Hz(i,j-1,k))*om_v(i,j)
          DC(i,j,0) = DC(i,j,0) + DC(i,j,k)
          CF(i,j,0) = CF(i,j,0) + DC(i,j,k)*vsponge(i,j,k)
         enddo
         enddo
         enddo

         do j=Jstr,JendR
         do i=IstrR,IendR
         DC(i,j,0)=1./DC(i,j,0)
         CF(i,j,0)=DC(i,j,0)
     &                   *(CF(i,j,0)-DV_avg1(i,j,nstp))
         enddo
         enddo

         do k = N,1,-1
         do j=Jstr,JendR
         do i=IstrR,IendR
         vsponge(i,j,k) = (vsponge(i,j,k)-CF(i,j,0))
#ifdef MASKING
     &       * vmask(i,j)
#endif
         enddo
         enddo
         enddo

# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,
     &                                 usponge(START_2D_ARRAY,1))
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend,
     &                                 vsponge(START_2D_ARRAY,1))
# endif

      return
      end

      subroutine interpspongeu(tabres,i1,i2,j1,j2,k1,k2,before)
      implicit none
# include "param.h"
# include "ocean3d.h"
# include "scalars.h"
# include "zoom.h"

       integer i1,i2,j1,j2,k1,k2
       logical before
       real tabres(i1:i2,j1:j2,k1:k2)
       if (before) then
           tabres(i1:i2,j1:j2,k1:k2) = u(i1:i2,j1:j2,k1:k2,nstp)
       else
           usponge(i1:i2,j1:j2,k1:k2) = tabres(i1:i2,j1:j2,k1:k2)
       endif

      return
      end

      subroutine interpspongev(tabres,i1,i2,j1,j2,k1,k2,before)
      implicit none
# include "param.h"
# include "ocean3d.h"
# include "scalars.h"
# include "zoom.h"

       integer i1,i2,j1,j2,k1,k2
       logical before
       real tabres(i1:i2,j1:j2,k1:k2)
       if (before) then
           tabres(i1:i2,j1:j2,k1:k2) = v(i1:i2,j1:j2,k1:k2,nstp)
       else
           vsponge(i1:i2,j1:j2,k1:k2) = tabres(i1:i2,j1:j2,k1:k2)
       endif
      return
      end    
            
#else
      subroutine uv3dpremix_empty
      end
#endif
 
