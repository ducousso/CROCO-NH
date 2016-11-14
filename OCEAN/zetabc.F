! $Id: zetabc.F 1409 2014-01-06 16:34:49Z marchesiello $
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
#ifndef CHILD
!
# include "cppdefs.h"
      subroutine zetabc_tile(Istr,Iend,Jstr,Jend)
# ifdef AGRIF      
      use AGRIF_Util
      integer Istr,Iend,Jstr,Jend
      if (AGRIF_Root()) then
        call zetabc_parent_tile(Istr,Iend,Jstr,Jend)
      else
        call zetabc_child_tile(Istr,Iend,Jstr,Jend)
c        call zetabc_interp_tile(Istr,Iend,Jstr,Jend)
      endif
      return
      end
!
! PARENT
!
      subroutine zetabc_parent_tile(Istr,Iend,Jstr,Jend)
# endif
!
! Set boundary conditions for free surface zeta(:,:,knew)
! for the parent grid.
!
#else
!
! CHILD
!
      subroutine zetabc_child_tile(Istr,Iend,Jstr,Jend)
!
! Set boundary conditions for free surface zeta(:,:,knew)
! for the child grid.
!
#endif /* CHILD */
!
! Common Code
!
# include "set_obc_definitions.h"
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j
      real cff,cx,dft,dfx, eps
      parameter (eps=1.D-20)
#include "param.h"
#include "boundary.h"
#include "climat.h"
#include "grid.h"
#include "ocean2d.h"
#include "scalars.h"
#ifdef AGRIF
#include "private_scratch.h"
#include "zoom.h"
      integer trd
C$    integer omp_get_thread_num
#endif
!
#include "compute_auxiliary_bounds.h"
!
!  If child grid, interpolate zeta from parent
!
#ifdef CHILD
      trd=0
C$    trd=omp_get_thread_num()
      call zetabc_interp_tile(Istr,Iend,Jstr,Jend
     &                    ,A1dETA(1,9+5*NWEIGHT)
     &                    ,A1dETA(1,11+7*NWEIGHT)
     &                    ,A1dETA(1,13+9*NWEIGHT)
     &                    ,A1dETA(1,15+9*NWEIGHT)     
     &                    ,A1dXI(1,9+5*NWEIGHT)
     &                    ,A1dXI(1,11+7*NWEIGHT)
     &                    ,A1dXI(1,13+9*NWEIGHT)
     &                    ,A1dXI(1,15+9*NWEIGHT))
#endif
! 
#ifndef EW_COM_PERIODIC
!
!====================================================================
!                            WESTERN BC
!====================================================================
      if (WESTERN_EDGE) then
# if defined OBC_COM_WEST && defined OBC_COM_ZORLANSKI
!                                           Eastern edge Radiation BC
!                                           ======= ==== ========= ==
        do j=JstrV-1,Jend
          dft=zeta(1,j,kstp)-zeta(1,j,knew)
          dfx=zeta(1,j,knew)-zeta(2,j,knew)

          if (dfx*dft .lt. 0.) dft=0.     ! <--  SUPPRESS INFLOW

          cx=dft*dfx
          cff=max(dfx*dfx, eps)

          zeta(0,j,knew)=(cff*zeta(0,j,kstp)+cx*zeta(1,j,knew))
     &                                                /(cff+cx)
#  ifdef MASKING
     &                      *rmask(0,j)
#  endif
        enddo
# elif defined OBC_COM_WEST && defined OBC_COM_ZCHAPMAN
!                                           Western edge Chapman BC
!                                           ======= ==== ======= ==
        do j=JstrV-1,Jend
#  ifdef NBQ
          cx=dtfast*pm(1,j)*sqrt(g*zeta(1,j,kstp))
#  else
          cx=dtfast*pm(1,j)*sqrt(g*(h(1,j)+zeta(1,j,kstp)))
#  endif
          zeta(0,j,knew)=(zeta(0,j,kstp)+cx*zeta(1,j,knew))/(1.+cx)
#  ifdef MASKING
     &                      *rmask(0,j)
#  endif
        enddo
# elif defined OBC_COM_WEST && defined OBC_COM_ZSPECIFIED
!                                           Western edge Specified BC
!                                           ======= ==== ========= ==
        do j=JstrV-1,Jend
#   ifdef Z_FRC_BRY
          zeta(0,j,knew)=zetabry_west(j)
#   else
          zeta(0,j,knew)=SSH(0,j)
#   endif
#  ifdef MASKING
     &                      *rmask(0,j)
#  endif
        enddo
# else
        do j=JstrV-1,Jend                     ! Western edge gradient BC
          zeta(0,j,knew)=zeta(1,j,knew)    ! ======= ==== ======== ==
#  ifdef MASKING
     &                      *rmask(0,j)
#  endif
        enddo
# endif
      endif
!
!====================================================================
!                            EASTERN BC
!====================================================================
      if (EASTERN_EDGE) then
#  if defined OBC_COM_EAST && defined OBC_COM_ZORLANSKI
!                                           Eastern edge Radiation BC
!                                           ======= ==== ========= ==
        do j=JstrV-1,Jend
          dft=zeta(Iend,j,kstp)-zeta(Iend,j,knew)
          dfx=zeta(Iend,j,knew)-zeta(Iend-1,j,knew)

          if (dfx*dft .lt. 0.) dft=0.     ! <--  SUPPRESS INFLOW

          cx=dft*dfx
          cff=max(dfx*dfx, eps)

          zeta(Iend+1,j,knew)=(cff*zeta(Iend+1,j,kstp)+
     &                              cx*zeta(Iend,j,knew))/(cff+cx)
#  ifdef MASKING
     &                                            *rmask(Iend+1,j)
#  endif
        enddo
# elif defined OBC_COM_EAST && defined OBC_COM_ZCHAPMAN
!                                           Eastern edge Chapman BC
!                                           ======= ==== ======= ==
        do j=JstrV-1,Jend
#  ifdef NBQ
          cx=dtfast*pm(Iend,j)*sqrt(g*zeta(Iend,j,kstp))
#  else
          cx=dtfast*pm(Iend,j)*sqrt(g*(h(Iend,j)+zeta(Iend,j,kstp)))
#  endif
          zeta(Iend+1,j,knew)=(zeta(Iend+1,j,kstp)
     &                              +cx*zeta(Iend,j,knew))/(1.+cx)
#  ifdef MASKING
     &                      *rmask(Iend+1,j)
#  endif
        enddo
# elif defined OBC_COM_EAST && defined OBC_COM_ZSPECIFIED
!                                           Eastern edge Specified BC
!                                           ======= ==== ========= ==
        do j=JstrV-1,Jend
#   ifdef Z_FRC_BRY
          zeta(Iend+1,j,knew)=zetabry_east(j) 
#   else
          zeta(Iend+1,j,knew)=SSH(Iend+1,j)
#   endif
#  ifdef MASKING
     &                      *rmask(Iend+1,j)
#  endif
        enddo
#  else
        do j=JstrV-1,Jend                         ! Eastern edge gradient BC
          zeta(Iend+1,j,knew)=zeta(Iend,j,knew)! ======= ==== ======== ==
#  ifdef MASKING
     &                        *rmask(Iend+1,j)
#  endif
        enddo
# endif
      endif
#endif        /* !EW_COM_PERIODIC */

#ifndef NS_COM_PERIODIC
!
!====================================================================
!                            SOUTHERN BC
!====================================================================
      if (SOUTHERN_EDGE) then
# if defined OBC_COM_SOUTH && defined OBC_COM_ZORLANSKI
!                                           Southern edge Radiation BC
!                                           ======= ==== ========= ==
        do i=IstrU-1,Iend
          dft=zeta(i,1,kstp)-zeta(i,1,knew)
          dfx=zeta(i,1,knew)-zeta(i,2,knew)

          if (dfx*dft .lt. 0.) dft=0.     ! <--  SUPPRESS INFLOW

          cx=dft*dfx
          cff=max(dfx*dfx, eps)

          zeta(i,0,knew)=(cff*zeta(i,0,kstp)+cx*zeta(i,1,knew))
     &                                                /(cff+cx)
#  ifdef MASKING
     &                      *rmask(i,0)
#  endif
        enddo
# elif defined OBC_COM_SOUTH && defined OBC_COM_ZCHAPMAN
!                                           Southern edge Chapman BC
!                                           ======== ==== ======= ==
        do i=IstrU-1,Iend
#  ifdef NBQ
          cx=dtfast*pn(i,1)*sqrt(g*zeta(i,1,kstp))
#  else
          cx=dtfast*pn(i,1)*sqrt(g*(h(i,1)+zeta(i,1,kstp)))
#  endif
          zeta(i,0,knew)=(zeta(i,0,kstp)
     &                              +cx*zeta(i,1,knew))/(1.+cx)
#  ifdef MASKING
     &                      *rmask(i,0)
#  endif
        enddo         
# elif defined OBC_COM_SOUTH && defined OBC_COM_ZSPECIFIED
!                                           Southern edge Specified BC
!                                           ======== ==== ========= ==
        do i=IstrU-1,Iend
#   ifdef Z_FRC_BRY
          zeta(i,0,knew)=zetabry_south(i)
#   else
          zeta(i,0,knew)=SSH(i,0)
#   endif
#  ifdef MASKING
     &                      *rmask(i,0)
#  endif
        enddo         
#  else
        do i=IstrU-1,Iend                    ! Southern edge gradient BC
          zeta(i,0,knew)=zeta(i,1,knew)   ! ======== ==== ======== ==
#  ifdef MASKING
     &                      *rmask(i,0)
#  endif
        enddo
# endif  /* OBC_COM_SOUTH */
      endif
!
!====================================================================
!                            NORTHERN BC
!====================================================================
      if (NORTHERN_EDGE) then
#  if defined OBC_COM_NORTH && defined OBC_COM_ZORLANSKI
!                                           Northern edge Radiation BC
!                                           ======= ==== ========= ==
        do i=IstrU-1,Iend
          dft=zeta(i,Jend,kstp)-zeta(i,Jend,knew)
          dfx=zeta(i,Jend,knew)-zeta(i,Jend-1,knew)

          if (dfx*dft .lt. 0.) dft=0.     ! <--  SUPPRESS INFLOW

          cx=dft*dfx
          cff=max(dfx*dfx, eps)

          zeta(i,Jend+1,knew)=(cff*zeta(i,Jend+1,kstp)+
     &                              cx*zeta(i,Jend,knew))/(cff+cx)
#  ifdef MASKING
     &                                            *rmask(i,Jend+1)
#  endif
        enddo
# elif defined OBC_COM_NORTH && defined OBC_COM_ZCHAPMAN
!                                           Northern edge Chapman BC
!                                           ======== ==== ======= ==
        do i=IstrU-1,Iend
#  ifdef NBQ
          cx=dtfast*pn(i,Jend)*sqrt(g*zeta(i,Jend,kstp))
#  else
          cx=dtfast*pn(i,Jend)*sqrt(g*(h(i,Jend)+zeta(i,Jend,kstp)))
#  endif
          zeta(i,Jend+1,knew)=(zeta(i,Jend+1,kstp)
     &                              +cx*zeta(i,Jend,knew))/(1.+cx)
#  ifdef MASKING
     &                      *rmask(i,Jend+1)
#  endif
        enddo
# elif defined OBC_COM_NORTH && defined OBC_COM_ZSPECIFIED
!                                           Northern edge Specified BC
!                                           ======== ==== ========= ==
        do i=IstrU-1,Iend
#   ifdef Z_FRC_BRY
          zeta(i,Jend+1,knew)=zetabry_north(i)
#   else
          zeta(i,Jend+1,knew)=SSH(i,Jend+1)
#   endif
#  ifdef MASKING
     &                      *rmask(i,Jend+1)
#  endif
        enddo
# else 
!                                            Northern edge gradient BC
!                                            ======== ==== ======== ==
        do i=IstrU-1,Iend
          zeta(i,Jend+1,knew)=zeta(i,Jend,knew)
#  ifdef MASKING
     &                       *rmask(i,Jend+1)
#  endif
        enddo
# endif   /* OBC_COM_NORTH */
      endif
#endif       /* !NS_COM_PERIODIC */
!
!====================================================================
!                   CORNERS (between open boundaries)
!====================================================================
#if defined OBC_COM_SOUTH && defined OBC_COM_WEST
      if (SOUTHERN_EDGE .and. WESTERN_EDGE) then
        zeta(0,0,knew)=0.5*(zeta(1,0 ,knew)+zeta(0 ,1,knew))
#  ifdef MASKING
     &                       *rmask(0,0)
#  endif        
      endif
#endif
#if defined OBC_COM_SOUTH && defined OBC_COM_EAST
      if (SOUTHERN_EDGE .and. EASTERN_EDGE) then
        zeta(Iend+1,0,knew)=0.5*(zeta(Iend+1,1 ,knew)+zeta(Iend,0,knew))
#  ifdef MASKING
     &                       *rmask(Iend+1,0)
#  endif                
      endif
#endif
#if defined OBC_COM_NORTH && defined OBC_COM_WEST
      if (NORTHERN_EDGE .and. WESTERN_EDGE) then
        zeta(0,Jend+1,knew)=0.5*(zeta(0,Jend,knew)+zeta(1 ,Jend+1,knew))
#  ifdef MASKING
     &                       *rmask(0,Jend+1)
#  endif                
      endif
#endif
#if defined OBC_COM_NORTH && defined OBC_COM_EAST
      if (NORTHERN_EDGE .and. EASTERN_EDGE) then
        zeta(Iend+1,Jend+1,knew)=0.5*(zeta(Iend+1,Jend,knew)+
     &                            zeta(Iend,Jend+1,knew))
#  ifdef MASKING
     &                       *rmask(Iend+1,Jend+1)
#  endif             
      endif
#endif
!
#if defined WET_DRY
!
!====================================================================
! Ensure that water level on boundary cells is above bed elevation.
!====================================================================
!
# ifndef NBQ
#  ifndef EW_COM_PERIODIC
      if (WESTERN_EDGE) then
        DO j=JstrV-1,Jend
          IF (zeta(Istr-1,j,knew).le.
     &        (Dcrit(Istr-1,j)-h(Istr-1,j))) THEN
            zeta(Istr-1,j,knew)=Dcrit(Istr-1,j)-h(Istr-1,j)-eps
          END IF
        END DO
      END IF
      if (EASTERN_EDGE) then
        DO j=JstrV-1,Jend
          IF (zeta(Iend+1,j,knew).le.
     &        (Dcrit(Iend+1,j)-h(Iend+1,j))) THEN
            zeta(Iend+1,j,knew)=Dcrit(Iend+1,j)-h(Iend+1,j)-eps
          END IF
        END DO
      END IF
#  endif
#  ifndef NS_COM_PERIODIC
      if (SOUTHERN_EDGE) then
        DO i=IstrU-1,Iend
          IF (zeta(i,Jstr-1,knew).le.
     &       (Dcrit(i,Jstr-1)-h(i,Jstr-1))) THEN
               zeta(i,Jstr-1,knew)=Dcrit(i,Jstr-1)-h(i,Jstr-1)-eps
          END IF
        END DO
      END IF
      if (NORTHERN_EDGE) then
        DO i=IstrU-1,Iend
          IF (zeta(i,Jend+1,knew).le.
     &       (Dcrit(i,Jend+1)-h(i,Jend+1))) THEN
               zeta(i,Jend+1,knew)=Dcrit(i,Jend+1)-h(i,Jend+1)-eps
          END IF
        END DO
      END IF
#  endif
#  if !defined EW_COM_PERIODIC && !defined NS_COM_PERIODIC
      if (SOUTHERN_EDGE .and. WESTERN_EDGE) then
        IF (zeta(Istr-1,Jstr-1,knew).le.
     &      (Dcrit(Istr-1,Jstr-1)-h(Istr-1,Jstr-1))) THEN
            zeta(Istr-1,Jstr-1,knew)=
     &            Dcrit(Istr-1,Jstr-1)-h(Istr-1,Jstr-1)-eps
        END IF
      END IF
      if (SOUTHERN_EDGE .and. EASTERN_EDGE) then
        IF (zeta(Iend+1,Jstr-1,knew).le.
     &      (Dcrit(Iend+1,Jstr-1)-h(Iend+1,Jstr-1))) THEN
            zeta(Iend+1,Jstr-1,knew)=
     &            Dcrit(Iend+1,Jstr-1)-h(Iend+1,Jstr-1)-eps
        END IF
      END IF
      if (NORTHERN_EDGE .and. WESTERN_EDGE) then
        IF (zeta(Istr-1,Jend+1,knew).le.
     &      (Dcrit(Istr-1,Jend+1)-h(Istr-1,Jend+1))) THEN
            zeta(Istr-1,Jend+1,knew)=
     &            Dcrit(Istr-1,Jend+1)-h(Istr-1,Jend+1)-eps
        END IF
      END IF
      if (NORTHERN_EDGE .and. EASTERN_EDGE) then
        IF (zeta(Iend+1,Jend+1,knew).le.
     &      (Dcrit(Iend+1,Jend+1)-h(Iend+1,Jend+1))) THEN
            zeta(Iend+1,Jend+1,knew)=
     &            Dcrit(Iend+1,Jend+1)-h(Iend+1,Jend+1)-eps
        END IF
      END IF
#  endif

# else /* NBQ */

#  ifndef EW_COM_PERIODIC
      if (WESTERN_EDGE) then
        DO j=JstrV-1,Jend
          IF (zeta(Istr-1,j,knew).le.Dcrit(Istr-1,j)) THEN
            zeta(Istr-1,j,knew)=Dcrit(Istr-1,j)-eps
          END IF
        END DO
      END IF
      if (EASTERN_EDGE) then
        DO j=JstrV-1,Jend
          IF (zeta(Iend+1,j,knew).le.Dcrit(Iend+1,j)) THEN
            zeta(Iend+1,j,knew)=Dcrit(Iend+1,j)-eps
          END IF
        END DO
      END IF
#  endif
#  ifndef NS_COM_PERIODIC
      if (SOUTHERN_EDGE) then
        DO i=IstrU-1,Iend
          IF (zeta(i,Jstr-1,knew).le.Dcrit(i,Jstr-1)) THEN
               zeta(i,Jstr-1,knew)=Dcrit(i,Jstr-1)-eps
          END IF
        END DO
      END IF
      if (NORTHERN_EDGE) then
        DO i=IstrU-1,Iend
          IF (zeta(i,Jend+1,knew).le.Dcrit(i,Jend+1)) THEN
               zeta(i,Jend+1,knew)=Dcrit(i,Jend+1)-eps
          END IF
        END DO
      END IF
#  endif
#  if !defined EW_COM_PERIODIC && !defined NS_COM_PERIODIC
      if (SOUTHERN_EDGE .and. WESTERN_EDGE) then
        IF (zeta(Istr-1,Jstr-1,knew).le.Dcrit(Istr-1,Jstr-1)) THEN
            zeta(Istr-1,Jstr-1,knew)=Dcrit(Istr-1,Jstr-1)-eps
        END IF
      END IF
      if (SOUTHERN_EDGE .and. EASTERN_EDGE) then
        IF (zeta(Iend+1,Jstr-1,knew).le.Dcrit(Iend+1,Jstr-1)) THEN
            zeta(Iend+1,Jstr-1,knew)=Dcrit(Iend+1,Jstr-1)-eps
        END IF
      END IF
      if (NORTHERN_EDGE .and. WESTERN_EDGE) then
        IF (zeta(Istr-1,Jend+1,knew).le.Dcrit(Istr-1,Jend+1)) THEN
            zeta(Istr-1,Jend+1,knew)=Dcrit(Istr-1,Jend+1)-eps
        END IF
      END IF
      if (NORTHERN_EDGE .and. EASTERN_EDGE) then
        IF (zeta(Iend+1,Jend+1,knew).le.Dcrit(Iend+1,Jend+1)) THEN
            zeta(Iend+1,Jend+1,knew)=Dcrit(Iend+1,Jend+1)-eps
        END IF
      END IF
#  endif
# endif /* NBQ */
#endif /* WET_DRY */


      return
      end
#ifndef CHILD
# define CHILD
# ifdef AGRIF
#  include "zetabc.F"
# endif
# undef CHILD
#endif  /* !CHILD */
 

