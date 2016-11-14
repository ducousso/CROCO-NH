! $Id: t3dpremix.F 1466 2014-02-06 17:37:07Z marchesiello $
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
#if defined SOLVE3D && defined AGRIF
      subroutine t3dpremix (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      
        call t3dpremix_tile (Istr,Iend,Jstr,Jend)
      
      return
      end

      subroutine t3dpremix_tile (Istr,Iend,Jstr,Jend)

      implicit none
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "mixing.h"
# include "climat.h"
# include "scalars.h"
# include "zoom.h"
      integer itrc, Istr,Iend,Jstr,Jend, i,j,k
!
# ifdef MPI
#  define LOCALLM Lmmpi
#  define LOCALMM Mmmpi
# else
#  define LOCALLM Lm
#  define LOCALMM Mm
# endif
!
       integer decal
       real maxdiff
       real tinterp,onemtinterp, rrhot
       integer :: nold
       integer :: irhot
       external interpsponget
       integer :: parentnbstep
!
# include "compute_auxiliary_bounds.h"
!

      irhot = Agrif_Irhot()
      rrhot = real(irhot)
      decal = 2*max(Agrif_Irhox(),Agrif_Irhoy())

      parentnbstep=Agrif_Parent_Nb_Step()

C$OMP BARRIER
C$OMP MASTER

      If ((nbcoarse == 1).AND.(TspongeTimeindex .NE. parentnbstep)) THEN

!       tsponge = 0.

        Call Agrif_Set_bc(tspongeid,(/-decal,0/),
     &     InterpolationShouldbemade=.TRUE.)
# ifdef MASKING
        Agrif_UseSpecialvalue=.true.
# endif
        Agrif_Specialvalue=0.

        tinterp = 1.
        Call Agrif_Bc_Variable(tspongeid,calledweight=tinterp,
     &                              procname=interpsponget)
      
        Agrif_UseSpecialvalue=.false.

        TTimesponge = 3 - TTimesponge
       
        TspongeTimeindex = parentnbstep
        TspongeTimeindex2 = agrif_nb_step()
      ENDIF

C$OMP END MASTER
C$OMP BARRIER
       
      if (agrif_nb_step() .EQ. TspongeTimeindex2) then

        if (SOUTHERN_EDGE) then  
          do itrc=1,NT
            do k=1,N
              do j=JstrR,JstrR+decal
                do i=IstrR,IendR
                  T_sponge_south(i,j,k,TTimesponge,itrc)=
     &                                tsponge(i,j,k,itrc)          
                enddo
              enddo
            enddo
          enddo
        endif
                     
        if (NORTHERN_EDGE) then
          do itrc=1,NT
            do k=1,N
              do j=JendR-decal,JendR
                do i=IstrR,IendR
                  T_sponge_north(i,j,k,TTimesponge,itrc)=
     &                                tsponge(i,j,k,itrc)          
                enddo
              enddo
            enddo
          enddo
        endif               


        if (WESTERN_EDGE) then
          do itrc=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IstrR+decal      
                  T_sponge_west(i,j,k,TTimesponge,itrc)=
     &                               tsponge(i,j,k,itrc)              
                enddo
              enddo
            enddo
          enddo
        endif
                    
        if (EASTERN_EDGE) then      
          do itrc=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IendR-decal,IendR
                  T_sponge_east(i,j,k,TTimesponge,itrc)=
     &                               tsponge(i,j,k,itrc)          
                enddo
              enddo
            enddo
          enddo  
        endif
    
      ENDIF
      
      
      tinterp = 0.5+(real(nbcoarse)-0.5)/rrhot

      IF (nbstep3d .LT. irhot) tinterp = 1.

      onemtinterp = 1.-tinterp
       
      nold = 3 - TTimesponge

      if (SOUTHERN_EDGE) then
        do itrc=1,NT
          do k=1,N
            do j=JstrR,JstrR+decal
              do i=IstrR,IendR
                tsponge(i,j,k,itrc) =
     &                   onemtinterp*T_sponge_south(i,j,k,nold,itrc)
     &               +tinterp*T_sponge_south(i,j,k,TTimesponge,itrc)
              enddo
            enddo
          enddo
        enddo
      endif
       
      if (NORTHERN_EDGE) then
        do itrc=1,NT
          do k=1,N
            do j=JendR-decal,JendR
              do i=IstrR,IendR
                tsponge(i,j,k,itrc) =
     &                  onemtinterp*T_sponge_north(i,j,k,nold,itrc)
     &              +tinterp*T_sponge_north(i,j,k,TTimesponge,itrc)
              enddo
            enddo
          enddo
        enddo             
      endif
       
      if (WESTERN_EDGE) then
        do itrc=1,NT
          do k=1,N
            do j=JstrR,JendR
              do i=IstrR,IstrR+decal
                tsponge(i,j,k,itrc) =
     &                    onemtinterp*T_sponge_west(i,j,k,nold,itrc)
     &               + tinterp*T_sponge_west(i,j,k,TTimesponge,itrc)
              enddo
            enddo
          enddo
        enddo
      endif      
       
      if (EASTERN_EDGE) then      
        do itrc=1,NT
          do k=1,N
            do j=JstrR,JendR
              do i=IendR-decal,IendR
                tsponge(i,j,k,itrc)=
     &                    onemtinterp*T_sponge_east(i,j,k,nold,itrc)
     &               + tinterp*T_sponge_east(i,j,k,TTimesponge,itrc)  
              enddo
            enddo
          enddo
        enddo  
      endif

      do itrc=1,NT
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
#  ifdef THREE_GHOST_POINTS_TS
        call exchange_r3d_3pts_tile (Istr,Iend,Jstr,Jend,
     &                               tsponge(START_2D_ARRAY,1,itrc))
#  else
        call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                          tsponge(START_2D_ARRAY,1,itrc))
#  endif
# endif
      enddo
     
      return
      end

      subroutine interpsponget(tabres,i1,i2,j1,j2,k1,k2,m1,m2,before)
      implicit none
# include "param.h"
# include "ocean3d.h"
# include "scalars.h"
# include "zoom.h"

      integer i1,i2,j1,j2,k1,k2,m1,m2
      logical :: before
      real tabres(i1:i2,j1:j2,k1:k2,m1:m2)

      if (before) then
          tabres(i1:i2,j1:j2,k1:k2,m1:m2) =
     &               t(i1:i2,j1:j2,k1:k2,nrhs,m1:m2)
      else
          tsponge(i1:i2,j1:j2,k1:k2,m1:m2) = tabres(i1:i2,j1:j2,k1:k2,m1:m2)
      endif
      return
      end  
#else
      subroutine t3dpremix_empty
      end
# endif /* SOLVE3D && AGRIF */
