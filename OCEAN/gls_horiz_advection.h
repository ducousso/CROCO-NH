! $Id: gls_horiz_advection.h 1160 2013-06-11 09:48:32Z gcambon $
!
!======================================================================
! CROCO is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! CROCO specific routines (nesting) are under CeCILL-C license.
! 
! CROCO website : http://www.croco-ocean.org/
!
!======================================================================
!
!# include "compute_auxiliary_bounds.h"
!
!===============================================================
!
! Compute horizontal advection for GLS turbulent closure
!
!===============================================================
!
!-------------------------------------------------
! Fourth or Third order advection scheme (default)
!-------------------------------------------------
!
#  if !defined GLS_HADV_C4
#   define GLS_HADV_UP3
#  endif
!
#  ifdef GLS_HADV_UP3
#   define curv WORK
#  else
#   define grad WORK
#  endif
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
#  ifdef NS_PERIODIC
#   define J_EXT_RANGE Jstr-1,Jend+2
#  else
#   ifdef MPI
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
#    define J_EXT_RANGE jmin,jmax
#   else
#    define J_EXT_RANGE max(Jstr-1,1),min(Jend+2,Mm+1)
#   endif
#  endif
          do j=Jstr,Jend
            do i=I_EXT_RANGE
              FX(i,j)=(tmp(i,j,k,3)-tmp(i-1,j,k,3))
# ifdef MASKING
     &                                        *umask(i,j)
# endif
            enddo
          enddo
# undef I_EXT_RANGE
# ifndef EW_PERIODIC
          if (WESTERN_EDGE) then
            do j=Jstr,Jend
              FX(0,j)=FX(1,j)
            enddo
          endif
          if (EASTERN_EDGE) then
#  ifdef MPI
            do j=Jstr,Jend
              FX(Lmmpi+2,j)=FX(Lmmpi+1,j)
            enddo
#  else
            do j=Jstr,Jend
              FX(Lm+2,j)=FX(Lm+1,j)
            enddo
#  endif
          endif
# endif

          do j=Jstr,Jend
            do i=Istr-1,Iend+1
# ifdef GLS_HADV_UP3
              curv(i,j)=FX(i+1,j)-FX(i,j)
# else
              grad(i,j)=0.5*(FX(i+1,j)+FX(i,j))
# endif
            enddo
          enddo             !--> discard FX


          do j=Jstr,Jend
            do i=Istr,Iend+1
# ifdef GLS_HADV_UP3
              if (Huon(i,j,k) .gt. 0.) then
                cff=curv(i-1,j)
              else
                cff=curv(i,j)
              endif
              FX(i,j)=0.5*( tmp(i,j,k,3)+tmp(i-1,j,k,3)
     &                           -0.333333333333*cff )*Huon(i,j,k)
# else
              FX(i,j)=0.5*( tmp(i,j,k,3)+tmp(i-1,j,k,3)
     &                     -0.333333333333*(grad(i,j)-grad(i-1,j))
     &                                               )*Huon(i,j,k)
# endif
            enddo          !--> discard grad
          enddo

          do j=J_EXT_RANGE
            do i=Istr,Iend
              FE(i,j)=(tmp(i,j,k,3)-tmp(i,j-1,k,3))
# ifdef MASKING
     &                                         *vmask(i,j)
# endif
            enddo
          enddo
# undef J_EXT_RANGE
# ifndef NS_PERIODIC
          if (SOUTHERN_EDGE) then
            do i=Istr,Iend
              FE(i,0)=FE(i,1)
            enddo
          endif
          if (NORTHERN_EDGE) then
#  ifdef MPI
            do i=Istr,Iend
              FE(i,Mmmpi+2)=FE(i,Mmmpi+1)
            enddo
#  else
            do i=Istr,Iend
              FE(i,Mm+2)=FE(i,Mm+1)
            enddo
#  endif
          endif
# endif

          do j=Jstr-1,Jend+1
            do i=Istr,Iend
# ifdef GLS_HADV_UP3
              curv(i,j)=FE(i,j+1)-FE(i,j)
# else
              grad(i,j)=0.5*(FE(i,j+1)+FE(i,j))
# endif
            enddo
          enddo            !--> discard FE


          do j=Jstr,Jend+1
            do i=Istr,Iend
# ifdef GLS_HADV_UP3
              if (Hvom(i,j,k) .gt. 0.) then
                cff=curv(i,j-1)
              else
                cff=curv(i,j)
              endif
              FE(i,j)=0.5*( tmp(i,j,k,3)+tmp(i,j-1,k,3)
     &                          -0.333333333333*cff )*Hvom(i,j,k)
#  undef curv
# else
              FE(i,j)=0.5*( tmp(i,j,k,3)+tmp(i,j-1,k,3)
     &                     -0.333333333333*(grad(i,j)-grad(i,j-1))
     &                                               )*Hvom(i,j,k)
#  undef grad
# endif
            enddo
          enddo            !--> discard grad

