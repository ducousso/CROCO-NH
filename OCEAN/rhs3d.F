! $Id: rhs3d.F 1458 2014-02-03 15:01:25Z gcambon $
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

      subroutine rhs3d (tile)
!
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call rhs3d_tile (Istr,Iend,Jstr,Jend,
     &                                    A3d(1,1,trd), A3d(1,2,trd),
     &                      A2d(1,1,trd), A2d(1,2,trd), A2d(1,3,trd),
     &                      A2d(1,1,trd), A2d(1,2,trd), A2d(1,3,trd),
     &                      A2d(1,4,trd), A2d(1,5,trd), A2d(1,6,trd))
      return
      end

      subroutine rhs3d_tile (Istr,Iend,Jstr,Jend, ru,rv, CF,FC,DC,
     &                                 wrk1,wrk2, UFx,UFe, VFx,VFe)
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
# ifdef MPI
     &                   ,imin,imax,jmin,jmax
# endif
# include "param.h"
# ifdef DIAGNOSTICS_UV
#  include "diagnostics.h"
# endif
      real ru(PRIVATE_2D_SCRATCH_ARRAY,N),
     &     rv(PRIVATE_2D_SCRATCH_ARRAY,N),
     &     CF(PRIVATE_1D_SCRATCH_ARRAY,0:N),  cff,cff1,cff2,
     &     FC(PRIVATE_1D_SCRATCH_ARRAY,0:N),  cffX,
     &     DC(PRIVATE_1D_SCRATCH_ARRAY,0:N),  cffE,      
     &     wrk1(PRIVATE_2D_SCRATCH_ARRAY),    curvX,
     &     wrk2(PRIVATE_2D_SCRATCH_ARRAY),    curvE,
     &      UFx(PRIVATE_2D_SCRATCH_ARRAY),
     &      UFe(PRIVATE_2D_SCRATCH_ARRAY),
     &      VFx(PRIVATE_2D_SCRATCH_ARRAY), 
     &      VFe(PRIVATE_2D_SCRATCH_ARRAY),    gamma 
      parameter (gamma = -0.25)
# include "grid.h"
# include "ocean3d.h"
# include "coupling.h"
# include "forces.h"
# include "scalars.h"
# ifdef NBQ
#  include "nbq.h"
# endif
# ifdef MRL_WCI
      real vstu,dudx,dvdx,ustv,dude,dvde
# endif
!
# include "compute_auxiliary_bounds.h"
!
#ifdef MASKING
# define SWITCH *
#else
# define SWITCH !
#endif
!
# ifdef BODYFORCE
#  define Uwrk UFx
#  define Vwrk VFe
#  define wrk UFe
!
!====================================================================
!
!  Apply surface and bottom stresses as body forces if selected
!
!====================================================================
!
!  Apply surface stress as a bodyforce: determine the thickness [m]
!  of the surface layer; then add in surface stress as a bodyfoce.
!
      do j=JstrV-1,Jend
        do i=IstrU-1,Iend
          wrk(i,j)=0.
        enddo
      enddo
      do k=N,levsfrc,-1
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            wrk(i,j)=wrk(i,j)+Hz(i,j,k)
          enddo
        enddo
      enddo
      do j=Jstr,Jend
        do i=IstrU,Iend
          Uwrk(i,j)=sustr(i,j)*4./((wrk(i-1,j)+wrk(i,j))
     &                             *(pm (i-1,j)+pm (i,j))
     &                             *(pn (i-1,j)+pn (i,j)))
        enddo
      enddo
      do j=JstrV,Jend
        do i=Istr,Iend
          Vwrk(i,j)=svstr(i,j)*4./((wrk(i,j-1)+wrk(i,j))
     &                             *(pm (i,j-1)+pm (i,j))
     &                             *(pn (i,j-1)+pn (i,j)))
        enddo
      enddo             !--> discard wrk
      do k=levsfrc,N
        do j=Jstr,Jend
          do i=IstrU,Iend
            ru(i,j,k)=ru(i,j,k)+Uwrk(i,j)*(Hz(i,j,k)+
     &                                     Hz(i-1,j,k))
          enddo
        enddo
        do j=JstrV,Jend
          do i=Istr,Iend
            rv(i,j,k)=rv(i,j,k)+Vwrk(i,j)*(Hz(i,j,k)+
     &                                     Hz(i,j-1,k))
          enddo
        enddo
      enddo          !--> discard Uwrk,Vwrk
!
!  Apply bottom stress as a bodyforce: determine the thickness [m]
!  of the bottom layer; then add in bottom stress as a bodyfoce.
!
      do j=JstrV-1,Jend
        do i=IstrU-1,Iend
          wrk(i,j)=0.
        enddo
      enddo
      do k=1,levbfrc
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            wrk(i,j)=wrk(i,j)+Hz(i,j,k)
          enddo
        enddo
      enddo
      do j=Jstr,Jend
        do i=IstrU,Iend
          Uwrk(i,j)=bustr(i,j)*4./((wrk(i-1,j)+wrk(i,j))
     &                             *(pm (i-1,j)+pm (i,j))
     &                             *(pn (i-1,j)+pn (i,j)))
        enddo
      enddo
      do j=JstrV,Jend
        do i=Istr,Iend
          Vwrk(i,j)=bvstr(i,j)*4./((wrk(i,j-1)+wrk(i,j))
     &                             *(pm (i,j-1)+pm (i,j))
     &                             *(pn (i,j-1)+pn (i,j)))
        enddo
      enddo             !--> discard wrk
      do k=1,levbfrc
        do j=Jstr,Jend
          do i=IstrU,Iend
            ru(i,j,k)=ru(i,j,k)-Uwrk(i,j)*(Hz(i,j,k)+
     &                                               Hz(i-1,j,k))
          enddo
        enddo
        do j=JstrV,Jend
          do i=Istr,Iend
            rv(i,j,k)=rv(i,j,k)-Vwrk(i,j)*(Hz(i,j,k)+
     &                                               Hz(i,j-1,k))
          enddo
        enddo
      enddo         !--> discard Uwrk,Vwrk
#  undef wrk
#  undef Vwrk
#  undef Uwrk
# endif /* BODYFORCE */

      do k=1,N
!
# if defined UV_COR || (defined CURVGRID && defined UV_ADV)
!
!====================================================================
!
!  Add in Coriolis, Stokes-Coriolis and curvilinear transformation 
!  terms, if any.
!
!====================================================================
!
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            cff=0.5*Hz(i,j,k)*(
#  ifdef UV_COR
     &              fomn(i,j)
#  endif
#  if (defined CURVGRID && defined UV_ADV)
     &             +0.5*( (v(i,j,k,nrhs)+v(i,j+1,k,nrhs))*dndx(i,j)
     &                   -(u(i,j,k,nrhs)+u(i+1,j,k,nrhs))*dmde(i,j))
#  endif 
     &                                                             )
#  ifdef MRL_WCI
#    if defined CURVGRID && defined UV_ADV
             cff1=0.5*Hz(i,j,k)*(
     &           0.5*( dndx(i,j)*(vst(i,j,k)+vst(i,j+1,k))
     &                -dmde(i,j)*(ust(i,j,k)+ust(i+1,j,k)) ))
#    else
          cff1 = 0.0
#    endif
            UFx(i,j)=(cff+cff1)*(v(i,j,k,nrhs)+v(i,j+1,k,nrhs))  
            UFe(i,j)=cff*(vst(i,j,k)+vst(i,j+1,k))
            VFe(i,j)=(cff+cff1)*(u(i,j,k,nrhs)+u(i+1,j,k,nrhs))
            VFx(i,j)=cff*(ust(i,j,k)+ust(i+1,j,k))
# else
            UFx(i,j)=cff*(v(i,j,k,nrhs)+v(i,j+1,k,nrhs))
            VFe(i,j)=cff*(u(i,j,k,nrhs)+u(i+1,j,k,nrhs))
# endif
# ifdef NBQ
            UFx(i,j)=UFx(i,j)
     &      +0.5*Hz(i,j,k)*eomn(i,j)*cosa(i,j)
     &                            *(wz(i,j,k-1,nrhs)+wz(i,j,k,nrhs)) 
            VFe(i,j)=VFe(i,j)   
     &      -0.5*Hz(i,j,k)*eomn(i,j)*sina(i,j)
     &                            *(wz(i,j,k-1,nrhs)+wz(i,j,k,nrhs))
# endif
          enddo
        enddo

        do j=Jstr,Jend
          do i=IstrU,Iend
            ru(i,j,k)=ru(i,j,k)+0.5*(UFx(i,j)+UFx(i-1,j))
#  ifdef MRL_WCI
     &                         +0.5*(UFe(i,j)+UFe(i-1,j))
#  endif

#  ifdef DIAGNOSTICS_UV
            if (nnew.ne.3) then
              MCor(i,j,k,1) = 0.5*(UFx(i,j)+UFx(i-1,j)) 
     &                        SWITCH umask(i,j)
#   ifdef MRL_WCI
              MStCo(i,j,k,1) = 0.5*(UFe(i,j)+UFe(i-1,j))
     &                        SWITCH umask(i,j)
#   endif
            endif
#  endif
          enddo
        enddo

        do j=JstrV,Jend
          do i=Istr,Iend
            rv(i,j,k)=rv(i,j,k)-0.5*(VFe(i,j)+VFe(i,j-1))
#  ifdef MRL_WCI
     &                         -0.5*(VFx(i,j)+VFx(i,j-1))
#  endif

#  ifdef DIAGNOSTICS_UV
            if (nnew.ne.3) then
              MCor(i,j,k,2) = -0.5*(VFe(i,j)+VFe(i,j-1))
     &                        SWITCH vmask(i,j)
#   ifdef MRL_WCI
              MStCo(i,j,k,2) = -0.5*(VFx(i,j)+VFx(i,j-1))
     &                        SWITCH vmask(i,j)
#   endif
            endif
#  endif
          enddo
        enddo
# endif /* UV_COR || (CURVGRID && UV_ADV) */


# ifdef UV_ADV
!
!======================================================================
!
!  Add in horizontal advection of momentum.
!  Compute diagonal [UFx,VFe] and off-diagonal [UFe,VFx] components
!  of tensor of momentum flux due to horizontal advection; after that
!  add in horizontal advection terms.
!
!======================================================================
!
#  if !(defined UV_HADV_UP3 || defined UV_HADV_C4 || defined UV_HADV_C2)
#   define UV_HADV_UP3
#  endif

#  if defined UV_HADV_UP3 || defined UV_HADV_C4
!
!----------------------------------------------------------
! Fourth or Third order advection scheme (default)
!----------------------------------------------------------
!
#   define uxx wrk1
#   define Huxx wrk2
!
#   ifdef EW_PERIODIC
#    define IU_EXT_RANGE IstrU-1,Iend+1
#   else
#    ifdef MPI
        if (WEST_INTER) then
          imin=IstrU-1
        else
          imin=max(IstrU-1,2)
        endif
        if (EAST_INTER) then
          imax=Iend+1
        else
          imax=min(Iend+1,Lmmpi)
        endif
#     define IU_EXT_RANGE imin,imax
#    else
#     define IU_EXT_RANGE max(IstrU-1,2),min(Iend+1,Lm)
#    endif
#   endif

        do j=Jstr,Jend
          do i=IU_EXT_RANGE
            uxx(i,j)=(u(i-1,j,k,nrhs)-2.*u(i,j,k,nrhs)
     &               +u(i+1,j,k,nrhs))  SWITCH umask(i,j)
            Huxx(i,j)=(Huon(i-1,j,k)-2.*Huon(i,j,k)
     &                +Huon(i+1,j,k))   SWITCH umask(i,j)
          enddo
        enddo
#   undef IU_EXT_RANGE
#   ifndef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=Jstr,Jend
            uxx(1,j)=uxx(2,j)
            Huxx(1,j)=Huxx(2,j)
          enddo
        endif
        if (EASTERN_EDGE) then
#    ifdef MPI        
          do j=Jstr,Jend
            uxx(Lmmpi+1,j)=uxx(Lmmpi,j)
            Huxx(Lmmpi+1,j)=Huxx(Lmmpi,j)
          enddo
#    else
          do j=Jstr,Jend
            uxx(Lm+1,j)=uxx(Lm,j)
            Huxx(Lm+1,j)=Huxx(Lm,j)
          enddo
#    endif          
        endif
#   endif

        do j=Jstr,Jend
          do i=IstrU-1,Iend
#   ifdef UV_HADV_C4
            UFx(i,j)=0.25*( u(i,j,k,nrhs)+u(i+1,j,k,nrhs)
     &                         -0.125*(uxx(i,j)+uxx(i+1,j))
     &                  )*( Huon(i,j,k)+Huon(i+1,j,k)
     &                      -0.125*(Huxx(i,j)+Huxx(i+1,j)))
#   else
            cffX=u(i,j,k,nrhs)+u(i+1,j,k,nrhs)
            if (cffX.gt.0.) then
              curvX=uxx(i,j)
            else
              curvX=uxx(i+1,j)
            endif
            UFx(i,j)=0.25*(cffX+gamma*curvX)*( Huon(i,j,k)+
     &            Huon(i+1,j,k)-0.125*(Huxx(i,j)+Huxx(i+1,j)))
#   endif
          enddo
        enddo
#   undef Huxx
#   undef uxx
#   define vee wrk1
#   define Hvee wrk2
      
#   ifdef NS_PERIODIC
#    define JV_EXT_RANGE JstrV-1,Jend+1
#   else
#    ifdef MPI
        if (SOUTH_INTER) then
          jmin=JstrV-1
        else
          jmin=max(JstrV-1,2)
        endif
        if (NORTH_INTER) then
          jmax=Jend+1
        else
          jmax=min(Jend+1,Mmmpi)
        endif
#     define JV_EXT_RANGE jmin,jmax
#    else
#     define JV_EXT_RANGE max(JstrV-1,2),min(Jend+1,Mm)
#    endif
#   endif

        do j=JV_EXT_RANGE
          do i=Istr,Iend
            vee(i,j)=(v(i,j-1,k,nrhs)-2.*v(i,j,k,nrhs)
     &               +v(i,j+1,k,nrhs))  SWITCH  vmask(i,j)
            Hvee(i,j)=(Hvom(i,j-1,k)-2.*Hvom(i,j,k)
     &                +Hvom(i,j+1,k))   SWITCH  vmask(i,j)
          enddo
        enddo
#   undef JV_EXT_RANGE
#   ifndef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=Istr,Iend
            vee(i,1)=vee(i,2)
            Hvee(i,1)=Hvee(i,2)
          enddo
        endif
        if (NORTHERN_EDGE) then
#    ifdef MPI
          do i=Istr,Iend
            vee(i,Mmmpi+1)=vee(i,Mmmpi)
            Hvee(i,Mmmpi+1)=Hvee(i,Mmmpi)
          enddo
#    else        
          do i=Istr,Iend
            vee(i,Mm+1)=vee(i,Mm)
            Hvee(i,Mm+1)=Hvee(i,Mm)
          enddo
#    endif          
        endif
#   endif
        do j=JstrV-1,Jend
          do i=Istr,Iend
#   ifdef UV_HADV_C4
            VFe(i,j)=0.25*( v(i,j,k,nrhs)+v(i,j+1,k,nrhs)
     &                        -0.125*(vee(i,j)+vee(i,j+1))
     &                  )*( Hvom(i,j,k)+Hvom(i,j+1,k)
     &                      -0.125*(Hvee(i,j)+Hvee(i,j+1)))
#   else
            cffE=v(i,j,k,nrhs)+v(i,j+1,k,nrhs)
            if (cffE.gt.0.) then
              curvE=vee(i,j)
            else
              curvE=vee(i,j+1)
            endif
            VFe(i,j)=0.25*(cffE+gamma*curvE)*( Hvom(i,j,k)+
     &             Hvom(i,j+1,k)-0.125*(Hvee(i,j)+Hvee(i,j+1)))
#   endif
          enddo
        enddo
#   undef Hvee
#   undef vee
#   define uee wrk1
#   define Hvxx wrk2

#   ifdef NS_PERIODIC
#    define JU_EXT_RANGE Jstr-1,Jend+1
#   else
#    ifdef MPI
        if (SOUTH_INTER) then
          jmin=Jstr-1
        else
          jmin=max(Jstr-1,1)
        endif
        if (NORTH_INTER) then
          jmax=Jend+1
        else
          jmax=min(Jend+1,Mmmpi)
        endif
#     define JU_EXT_RANGE jmin,jmax
#    else
#     define JU_EXT_RANGE max(Jstr-1,1),min(Jend+1,Mm)
#    endif
#   endif

        do j=JU_EXT_RANGE
          do i=IstrU,Iend
            uee(i,j)=(u(i,j-1,k,nrhs)-u(i,j,k,nrhs)) SWITCH pmask(i,j  )
     &              +(u(i,j+1,k,nrhs)-u(i,j,k,nrhs)) SWITCH pmask(i,j+1)
          enddo
        enddo
#   undef JU_EXT_RANGE
#   ifndef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=IstrU,Iend
            uee(i,0)=uee(i,1)
          enddo
        endif
        if (NORTHERN_EDGE) then
#    ifdef MPI
          do i=IstrU,Iend
            uee(i,Mmmpi+1)=uee(i,Mmmpi)
          enddo
#    else        
          do i=IstrU,Iend
            uee(i,Mm+1)=uee(i,Mm)
          enddo
#    endif          
        endif
#   endif
        do j=Jstr,Jend+1
          do i=IstrU-1,Iend
           Hvxx(i,j)=Hvom(i-1,j,k)-2.*Hvom(i,j,k)+Hvom(i+1,j,k)
          enddo
        enddo
        do j=Jstr,Jend+1
          do i=IstrU,Iend
#   ifdef UV_HADV_C4
            UFe(i,j)=0.25*( u(i,j,k,nrhs)+u(i,j-1,k,nrhs)
     &                        -0.125*(uee(i,j)+uee(i,j-1))
     &                  )*( Hvom(i,j,k)+Hvom(i-1,j,k)
     &                     -0.125*(Hvxx(i,j)+Hvxx(i-1,j)))
#   else
            cffX=u(i,j,k,nrhs)+u(i,j-1,k,nrhs)
            cffE=Hvom(i,j,k)+Hvom(i-1,j,k)
            if (cffE.gt.0.) then
              curvX=uee(i,j-1)
            else
              curvX=uee(i,j)
            endif
            UFe(i,j)=0.25*(cffX+gamma*curvX)*(cffE-0.125*(
     &                             Hvxx(i,j)+Hvxx(i-1,j) ))
#   endif
          enddo
        enddo
#   undef Hvxx
#   undef uee
#   define vxx wrk1
#   define Huee wrk2

#   ifdef EW_PERIODIC 
#    define IV_EXT_RANGE Istr-1,Iend+1
#   else
#    ifdef MPI
        if (WEST_INTER) then
          imin=Istr-1
        else
          imin=max(Istr-1,1)
        endif
        if (EAST_INTER) then
          imax=Iend+1
        else
          imax=min(Iend+1,Lmmpi)
        endif
#     define IV_EXT_RANGE imin,imax
#    else
#     define IV_EXT_RANGE max(Istr-1,1),min(Iend+1,Lm)
#    endif
#   endif

        do j=JstrV,Jend
          do i=IV_EXT_RANGE
            vxx(i,j)=(v(i-1,j,k,nrhs)-v(i,j,k,nrhs)) SWITCH pmask(i  ,j)
     &              +(v(i+1,j,k,nrhs)-v(i,j,k,nrhs)) SWITCH pmask(i+1,j)
          enddo
        enddo
#   undef IV_EXT_RANGE
#   ifndef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=JstrV,Jend
            vxx(0,j)=vxx(1,j)
          enddo
        endif
        if (EASTERN_EDGE) then
#    ifdef MPI
          do j=JstrV,Jend
            vxx(Lmmpi+1,j)=vxx(Lmmpi,j)
          enddo
#    else        
          do j=JstrV,Jend
            vxx(Lm+1,j)=vxx(Lm,j)
          enddo
#    endif          
        endif
        
#   endif
        do j=JstrV-1,Jend
          do i=Istr,Iend+1
           Huee(i,j)=Huon(i,j-1,k)-2.*Huon(i,j,k)+Huon(i,j+1,k)
          enddo
        enddo
        do j=JstrV,Jend
          do i=Istr,Iend+1
#   ifdef UV_HADV_C4
            VFx(i,j)=0.25*( v(i,j,k,nrhs)+v(i-1,j,k,nrhs)
     &                        -0.125*(vxx(i,j)+vxx(i-1,j))
     &                  )*( Huon(i,j,k)+Huon(i,j-1,k)
     &                     -0.125*(Huee(i,j)+Huee(i,j-1)))
#   else
            cffE=v(i,j,k,nrhs)+v(i-1,j,k,nrhs)
            cffX=Huon(i,j,k)+Huon(i,j-1,k)
            if (cffX.gt.0.) then
              curvE=vxx(i-1,j)
            else
              curvE=vxx(i,j)
            endif
            VFx(i,j)=0.25*(cffE+gamma*curvE)*(cffX-0.125*(
     &                             Huee(i,j)+Huee(i,j-1) ))
#   endif
          enddo
        enddo
#   undef Huee
#   undef vxx

#  else /* --> UV_HADV_C2 */
!
!----------------------------------------------------------
! Second order advection scheme (default)
!----------------------------------------------------------
!
        do j=Jstr,Jend
          do i=IstrU-1,Iend
            UFx(i,j)=0.25*(u(i,j,k,nrhs)+u(i+1,j,k,nrhs))
     &                      *(Huon(i,j,k)+Huon(i+1,j,k))
          enddo
        enddo
        do j=JstrV-1,Jend
          do i=Istr,Iend
            VFe(i,j)=0.25*(v(i,j,k,nrhs)+v(i,j+1,k,nrhs))
     &                       *(Hvom(i,j,k)+Hvom(i,j+1,k))
          enddo
        enddo
        do j=Jstr,Jend+1
          do i=IstrU,Iend
            UFe(i,j)=0.25*(u(i,j,k,nrhs)+u(i,j-1,k,nrhs))
     &                      *( Hvom(i,j,k)+Hvom(i-1,j,k))
          enddo
        enddo
        do j=JstrV,Jend
          do i=Istr,Iend+1
            VFx(i,j)=0.25*(v(i,j,k,nrhs)+v(i-1,j,k,nrhs))
     &                       *(Huon(i,j,k)+Huon(i,j-1,k))
          enddo
        enddo
#  endif /* UV_HADV_UP3 */
!
!----------------------------------------------------------
! Finalize horizontal advection: compute flux divergences
!----------------------------------------------------------
!
        do j=Jstr,Jend
          do i=IstrU,Iend
            ru(i,j,k)=ru(i,j,k)-UFx(i,j  )+UFx(i-1,j)
     &                         -UFe(i,j+1)+UFe(i  ,j)
#  ifdef DIAGNOSTICS_UV
            if (nnew.ne.3) then
              MXadv(i,j,k,1) = -UFx(i,j)+UFx(i-1,j) SWITCH umask(i,j)
              MYadv(i,j,k,1) = -UFe(i,j+1)+UFe(i,j) SWITCH umask(i,j)
            endif
#  endif
          enddo
        enddo
        do j=JstrV,Jend
          do i=Istr,Iend
            rv(i,j,k)=rv(i,j,k)-VFx(i+1,j)+VFx(i,j  )
     &                         -VFe(i  ,j)+VFe(i,j-1)
#  ifdef DIAGNOSTICS_UV
            if (nnew.ne.3) then
              MXadv(i,j,k,2) = -VFx(i+1,j)+VFx(i,j) SWITCH vmask(i,j)
              MYadv(i,j,k,2) = -VFe(i,j)+VFe(i,j-1) SWITCH vmask(i,j)
            endif
#  endif
          enddo
        enddo
# endif /* UV_ADV */
      enddo    ! <-- k

      do j=Jstr,Jend
# ifdef UV_ADV
!
!======================================================================
!
!  Compute and add in vertical advection terms
!
!======================================================================
!
#  ifdef UV_VADV_SPLINES
!
!----------------------------------------------------------
!  U VERTICAL ADVECTION:
!
!  Construct conservative parabolic splines for the 
!  vertical derivatives "CF" of u-momentum.
!----------------------------------------------------------
!
        do k=1,N
          do i=IstrU,Iend
            DC(i,k)=0.5625*(HZR(i  ,j,k)+HZR(i-1,j,k))
     &             -0.0625*(HZR(i+1,j,k)+HZR(i-2,j,k))
          enddo
        enddo
        do i=IstrU,Iend     ! Construct parabolic splines: here
          FC(i,0)=0.        ! CF is the set off vertical derivatives
          CF(i,0)=0.        ! of the velocity field u(:,:,:,nrhs),
        enddo               ! FC is an auxiliary scratch variable.
        do k=1,N-1,+1
          do i=IstrU,Iend
            cff=1./(2.*DC(i,k+1)+DC(i,k)*(2.-FC(i,k-1)))
            FC(i,k)=cff*DC(i,k+1)
            CF(i,k)=cff*(6.*(u(i,j,k+1,nrhs)-u(i,j,k,nrhs))
     &                                  -DC(i,k)*CF(i,k-1))
          enddo
        enddo
        do i=IstrU,Iend
          CF(i,N)=0.
        enddo
        do k=N-1,1,-1
          do i=IstrU,Iend
            CF(i,k)=CF(i,k)-FC(i,k)*CF(i,k+1)
          enddo
        enddo               !--> discard FC
!
! Compute vertical advective fluxes 
! FC=W*[spline-interpolated velocity]
!
        do k=1,N-1
          do i=IstrU,Iend
            FC(i,k)=( 0.5625*(We(i  ,j,k)+We(i-1,j,k))-0.0625*(
     &                                 We(i+1,j,k)+We(i-2,j,k) ))
     &             *( u(i,j,k,nrhs)+DC(i,k)*(
     &                              0.33333333333333*CF(i,k  )
     &                             +0.16666666666667*CF(i,k-1)
     &                                                       ))
          enddo
        enddo               !--> discard CF,DC
        do i=IstrU,Iend
          FC(i,N)=0.
#   ifdef MOVING_BATHY
          FC(i,0)=0.5*u(i,j,1,nrhs)*
     &            (We(i  ,j,0)+
     &             We(i-1,j,0))
#   else
          FC(i,0)=0.
#   endif
        enddo
#  elif defined UV_VADV_C2
!
!----------------------------------------------------------
!  Compute vertical advective fluxes 
!  using Second order advection scheme
!----------------------------------------------------------
!
        do k=1,N-1
          do i=IstrU,Iend
            FC(i,k)=0.25*(u(i,j,k,nrhs)+u(i,j,k+1,nrhs))
     &                            *(We(i,j,k)+We(i-1,j,k))
          enddo
        enddo
        do i=IstrU,Iend
#   ifdef MOVING_BATHY
          FC(i,0)=0.5*u(i,j,1,nrhs)*
     &            (We(i  ,j,0)+
     &             We(i-1,j,0))
#   else
          FC(i,0)=0.
#   endif
          FC(i,N)=0.
        enddo
#  else
!
!----------------------------------------------------------
!  Compute vertical advective fluxes using
!  Fourth order advection scheme
!----------------------------------------------------------
!
        do k=2,N-2
          do i=IstrU,Iend
            FC(i,k)=( 0.5625*(u(i,j,k  ,nrhs)+u(i,j,k+1,nrhs))
     &                 -0.0625*(u(i,j,k-1,nrhs)+u(i,j,k+2,nrhs)))
     &                         *( 0.5625*(We(i  ,j,k)+We(i-1,j,k))
     &                           -0.0625*(We(i+1,j,k)+We(i-2,j,k)))
          enddo
        enddo
        do i=IstrU,Iend
          FC(i,N)=0.
          FC(i,N-1)=( 0.5625*(u(i,j,N-1,nrhs)+u(i,j,N,nrhs))
     &                   -0.0625*(u(i,j,N-2,nrhs)+u(i,j,N,nrhs)))
     &                     *( 0.5625*(We(i  ,j,N-1)+We(i-1,j,N-1))
     &                       -0.0625*(We(i+1,j,N-1)+We(i-2,j,N-1)))

          FC(i,  1)=( 0.5625*(u(i,j,  1,nrhs)+u(i,j,2,nrhs))
     &                   -0.0625*(u(i,j,  1,nrhs)+u(i,j,3,nrhs)))
     &                         *( 0.5625*(We(i  ,j,1)+We(i-1,j,1))
     &                           -0.0625*(We(i+1,j,1)+We(i-2,j,1)))
#   ifdef MOVING_BATHY
          FC(i,0)=2.*
     &     (0.5625* u(i,j,1,nrhs)-
     &     -0.0625* u(i,j,2,nrhs)
     &     )*
     &     (0.5625*(We(i  ,j,0)+
     &              We(i-1,j,0)
     &             )
     &      -0.0625*(We(i+1,j,0)+
     &                   We(i-2,j,0)
     &              )
     &      )
#   else
          FC(i,0)=0.
#   endif
        enddo
#  endif
!
!----------------------------------------------------------
! Finalize U vertical advection: compute flux divergences
!----------------------------------------------------------
!
        do k=1,N
          do i=IstrU,Iend
            ru(i,j,k)=ru(i,j,k)-FC(i,k)+FC(i,k-1)
#  ifdef DIAGNOSTICS_UV
            if (nnew.ne.3) then
              MVadv(i,j,k,1) = -FC(i,k)+FC(i,k-1) SWITCH umask(i,j)
            endif
#  endif
          enddo
        enddo               !--> discard FC

        if (j.ge.JstrV) then
#  ifdef UV_VADV_SPLINES
!
!----------------------------------------------------------
!  V VERTICAL ADVECTION:
!
!  Construct conservative parabolic splines for the 
!  vertical derivatives "CF" of u-momentum.
!----------------------------------------------------------
!
          do k=1,N
            do i=Istr,Iend
              DC(i,k)=0.5625*(HZR(i  ,j,k)+HZR(i,j-1,k))
     &               -0.0625*(HZR(i,j+1,k)+HZR(i,j-2,k))
            enddo             
          enddo             
          do i=Istr,Iend    ! Construct parabolic splines: here
            FC(i,0)=0.      ! CF is the set off vertical derivatives
            CF(i,0)=0.      ! of the velocity field v(:,:,:,nrhs),
          enddo             ! FC is an auxiliary scratch variable.
          do k=1,N-1,+1
            do i=Istr,Iend
              cff=1./(2.*DC(i,k+1)+DC(i,k)*(2.-FC(i,k-1)))
              FC(i,k)=cff*DC(i,k+1)
              CF(i,k)=cff*(6.*(v(i,j,k+1,nrhs)-v(i,j,k,nrhs))
     &                                    -DC(i,k)*CF(i,k-1))
            enddo
          enddo
          do i=Istr,Iend
            CF(i,N)=0.
          enddo
          do k=N-1,1,-1
            do i=Istr,Iend
              CF(i,k)=CF(i,k)-FC(i,k)*CF(i,k+1)
            enddo
          enddo             !--> discard FC
!
! Compute vertical advective fluxes
! FC=W*[spline-interpolated velocity]
!
          do k=1,N-1
            do i=Istr,Iend
              FC(i,k)=( 0.5625*(We(i,j  ,k)+We(i,j-1,k))-0.0625*(
     &                                   We(i,j+1,k)+We(i,j-2,k) ))
     &               *( v(i,j,k,nrhs)+DC(i,k)*(
     &                                0.33333333333333*CF(i,k  )
     &                               +0.16666666666667*CF(i,k-1)
     &                                                         ))
            enddo
          enddo             !--> discard CF,DC
          do i=Istr,Iend
            FC(i,N)=0.
#   ifdef MOVING_BATHY
            FC(i,0)=0.5*v(i,j,1,nrhs)*
     &              (We(i,j  ,0)+
     &               We(i,j-1,0))
#   else
            FC(i,0)=0.
#   endif
          enddo
#  elif defined UV_VADV_C2
!
!----------------------------------------------------------
!  Second order advection scheme
!----------------------------------------------------------
!
          do k=1,N-1
            do i=Istr,Iend
              FC(i,k)=0.25*(v(i,j,k,nrhs)+v(i,j,k+1,nrhs))
     &                              *(We(i,j,k)+We(i,j-1,k))
            enddo
          enddo
          do i=Istr,Iend
#   ifdef MOVING_BATHY
            FC(i,0)=0.5*v(i,j,1,nrhs)*
     &              (We(i,j  ,0)+
     &               We(i,j-1,0))
#   else
            FC(i,0)=0.
#   endif
            FC(i,N)=0.
          enddo
#  else
!
!----------------------------------------------------------
!  Fourth order advection scheme
!----------------------------------------------------------
!
          do k=2,N-2
            do i=Istr,Iend
              FC(i,k)=( 0.5625*(v(i,j,k,nrhs)+v(i,j,k+1,nrhs))
     &                   -0.0625*(v(i,j,k-1,nrhs)+v(i,j,k+2,nrhs)))
     &                           *( 0.5625*(We(i,j  ,k)+We(i,j-1,k))
     &                             -0.0625*(We(i,j+1,k)+We(i,j-2,k)))
            enddo
          enddo
          do i=Istr,Iend
            FC(i,N)=0.
            FC(i,N-1)=( 0.5625*(v(i,j,N-1,nrhs)+v(i,j,N,nrhs))
     &                   -0.0625*(v(i,j,N-2,nrhs)+v(i,j,N,nrhs)))
     &                     *( 0.5625*(We(i,j  ,N-1)+We(i,j-1,N-1))
     &                       -0.0625*(We(i,j+1,N-1)+We(i,j-2,N-1)))

            FC(i,  1)=(+0.5625*(v(i,j,  1,nrhs)+v(i,j,2,nrhs))
     &                   -0.0625*(v(i,j,  1,nrhs)+v(i,j,3,nrhs)))
     &                         *( 0.5625*(We(i,j  ,1)+We(i,j-1,1))
     &                           -0.0625*(We(i,j+1,1)+We(i,j-2,1)))
#   ifdef MOVING_BATHY
          FC(i,0)=2.*
     &     (0.5625* v(i,j,1,nrhs)-
     &     -0.0625* v(i,j,2,nrhs)
     &     )*
     &     (0.5625*(We(i,j  ,0)+
     &              We(i,j-1,0)
     &             )
     &      -0.0625*(We(i,j+1,0)+
     &               We(i,j-2,0)
     &              )
     &      )
#   else
          FC(i,0)=0.
#   endif
           enddo
#  endif
!
!----------------------------------------------------------
! Finalize V vertical advection: compute flux divergences
!----------------------------------------------------------
!
          do k=1,N
            do i=Istr,Iend
              rv(i,j,k)=rv(i,j,k)-FC(i,k)+FC(i,k-1)
#  ifdef DIAGNOSTICS_UV
            if (nnew.ne.3) then
              MVadv(i,j,k,2) = -FC(i,k)+FC(i,k-1) SWITCH vmask(i,j)
            endif
#  endif
            enddo
          enddo
        endif
# endif /* UV_ADV */

!
!======================================================================
!
! Add combined vortex-force and advection terms & breaking terms
!
!======================================================================
!
# ifdef MRL_WCI
        do k=1,N
          do i=IstrU,iend
            cff  =0.5*on_u(i,j)*(Hz(i-1,j,k)+Hz(i,j,k))
            vstu =0.25*( vst(i  ,j,k) +vst(i  ,j+1,k)
     &                  +vst(i-1,j,k) +vst(i-1,j+1,k) )
            dudx =0.5*( u(i+1,j,k,nrhs) -u(i-1,j,k,nrhs) )
            dvdx =0.5*( v(i  ,j,k,nrhs) -v(i-1,j,k,nrhs)
     &                 +v(i,j+1,k,nrhs) -v(i-1,j+1,k,nrhs) )
            ru(i,j,k) =ru(i,j,k) + cff
     &                           *(ust(i,j,k)*dudx+vstu*dvdx)
#  ifndef WAVE_SFC_BREAK
     &                           +cff*om_u(i,j)*brk3dx(i,j,k)
#  endif
#  if defined WAVE_STREAMING && defined WAVE_BODY_STREAMING
     &                           +cff*om_u(i,j)*frc3dx(i,j,k)
#  endif
#  ifdef DIAGNOSTICS_UV
            if (nnew.ne.3) then
              Mvf(i,j,k,1)  = cff*(ust(i,j,k)*dudx +vstu*dvdx)
#   ifndef WAVE_SFC_BREAK
              Mbrk(i,j,k,1) = cff*om_u(i,j)*brk3dx(i,j,k)
#   endif
#   if defined WAVE_STREAMING && defined WAVE_BODY_STREAMING
              Mfrc(i,j,k,1) = cff*om_u(i,j)*frc3dx(i,j,k)
#   endif
            endif
#  endif
          enddo


          do i=istr,iend
            cff  =0.5*om_v(i,j)*(Hz(i,j-1,k)+Hz(i,j,k))
            ustv =0.25*( ust(i,j  ,k) +ust(i+1,j  ,k)
     &                  +ust(i,j-1,k) +ust(i+1,j-1,k) )
            dude =0.5*( u(i  ,j,k,nrhs) -u(i  ,j-1,k,nrhs)
     &                 +u(i+1,j,k,nrhs) -u(i+1,j-1,k,nrhs) )
ccc            dvde =0.5*( v(i,j-1,k,nrhs) -v(i,j+1,k,nrhs) )
            dvde =0.5*( v(i,j+1,k,nrhs) -v(i,j-1,k,nrhs) )
            rv(i,j,k) =rv(i,j,k) + cff
     &                            *(ustv*dude+vst(i,j,k)*dvde)
#  ifndef WAVE_SFC_BREAK
     &                            +cff*on_v(i,j)*brk3de(i,j,k)
#  endif
#  if defined WAVE_STREAMING && defined WAVE_BODY_STREAMING
     &                            +cff*on_v(i,j)*frc3de(i,j,k)
#  endif
#  ifdef DIAGNOSTICS_UV
            if (nnew.ne.3) then
              Mvf(i,j,k,2)  = cff*(ustv*dude + vst(i,j,k)*dvde)
#   ifndef WAVE_SFC_BREAK
              Mbrk(i,j,k,2) = cff*on_v(i,j)*brk3de(i,j,k)
#   endif
#   if defined WAVE_STREAMING && defined WAVE_BODY_STREAMING
              Mfrc(i,j,k,2) = cff*on_v(i,j)*frc3de(i,j,k)
#   endif
            endif
#  endif
         enddo
      enddo

#  if defined WAVE_STREAMING && !defined WAVE_BODY_STREAMING
        do i=IstrU,iend
          ru(i,j,1) =ru(i,j,1) + om_u(i,j)*on_u(i,j)*frc2dx(i,j)
        enddo
        do i=istr,iend
          rv(i,j,1) =rv(i,j,1) + om_v(i,j)*on_v(i,j)*frc2de(i,j)
        enddo
#  endif

# endif   /* MRL_WCI */

!
!======================================================================
!
!  Start computation of the forcing terms for the 2D (barotropic
!  mode) momentum equations: vertically integrate the just computed
!  r.h.s "ru" and "rv". Also, if so prescribed, add in the
!  difference between surface (wind) and bottom (drag) stresses.
!
!  The computation of the 2D forcing terms will be finalized in
!  "rhs2d" during the first barotropic time step, when the
!  barotropically computed r.h.ss "rubar", "rvbar" will be subtracted
!  from the vertically integrated (here) "rufrc", "rvfrc".
!
!======================================================================
!
        do i=IstrU,Iend
          rufrc(i,j)=ru(i,j,1)
# ifndef BODYFORCE
     &        +(sustr(i,j)-bustr(i,j))*om_u(i,j)*on_u(i,j)
# endif
#  if defined MRL_WCI && defined WAVE_SFC_BREAK
     &                    +brk2dx(i,j)*om_u(i,j)*on_u(i,j)
#  endif
        enddo
        do k=2,N
          do i=IstrU,Iend
            rufrc(i,j)=rufrc(i,j)+ru(i,j,k)
          enddo
        enddo
        if (j.ge.JstrV) then
          do i=Istr,Iend
            rvfrc(i,j)=rv(i,j,1)
# ifndef BODYFORCE
     &       +(svstr(i,j)-bvstr(i,j))*om_v(i,j)*on_v(i,j)
# endif
# if defined MRL_WCI && defined WAVE_SFC_BREAK
     &                   +brk2de(i,j)*om_v(i,j)*on_v(i,j)
# endif
          enddo
          do k=2,N
            do i=Istr,Iend
              rvfrc(i,j)=rvfrc(i,j)+rv(i,j,k)
            enddo
          enddo
        endif
      enddo      !<-- j

      return
      end
#else
      subroutine rhs3d_empty
      end
#endif /* SOLVE3D */

