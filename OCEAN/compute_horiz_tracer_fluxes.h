# if defined TS_HADV_UP5 || defined TS_HADV_C6 || defined TS_HADV_WENO5
!----------------------------------------------------------
#  ifdef PREDICTOR
!
!----------------------------------------------------------
! Sixth order advection scheme [PREDICTOR]
!----------------------------------------------------------
!
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
#  else
!----------------------------------------------------------
! 5th or 6th order or WENO5 advection schemes [CORRECTOR]
!----------------------------------------------------------
!
#  ifdef TS_HADV_C6
#   define FLUX5 flux6
#   define FLUX3 flux3
#   define FLUX2 flux2
#   undef  UP5_MASKING
#  elif defined TS_HADV_WENO5
#   define FLUX5 flux5_weno
#   define FLUX3 flux3
#   define FLUX2 flux1
#   define UP5_MASKING
#  else
#   define FLUX5 flux5
#   define FLUX3 flux3
#   define FLUX2 flux1
#   define UP5_MASKING
#  endif
!
            cdif=0.2 
#  include "t3dadv_order5.h"
!
#  undef FLUX5
#  undef FLUX3
#  undef FLUX2
#  undef UP5_MASKING
#  endif  
!----------------------------------------------------------
# elif defined TS_HADV_C2
!
!----------------------------------------------------------
! Second order advection scheme
!----------------------------------------------------------
!
          do j=Jstr,Jend
            do i=Istr,Iend+1
              FX(i,j)=0.5*(t(i,j,k,nadv,itrc)+t(i-1,j,k,nadv,itrc))
     &                                                 *Huon(i,j,k)
            enddo
          enddo
          do j=Jstr,Jend+1
            do i=Istr,Iend
              FE(i,j)=0.5*(t(i,j,k,nadv,itrc)+t(i,j-1,k,nadv,itrc))
     &                                                 *Hvom(i,j,k)
            enddo
          enddo  

# else   /*  --> UP3 (default) or C4 */
!
!----------------------------------------------------------
! Fourth or Third order advection scheme
!----------------------------------------------------------


!----------------------------------------------------------
#  ifdef PREDICTOR
!----------------------------------------------------------
! PREDICTOR [Fourth order advection scheme]
!
#    define grad WORK
!----------------------------------------------------------
#  else
!----------------------------------------------------------
!  CORRECTOR
!
#  if !(defined TS_HADV_C4 || defined TS_HADV_AKIMA)
#   define TS_HADV_UP3
#  endif
!
#  ifdef TS_HADV_UP3
#   define curv WORK
#  else
#   define grad WORK
#  endif
!------------------------------------------------------------
#  endif
!------------------------------------------------------------


!------------------------------------------------------------
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
!-------------------------------------------------------------------
          do j=Jstr,Jend
            do i=I_EXT_RANGE
              FX(i,j)=(t(i,j,k,nadv,itrc)-t(i-1,j,k,nadv,itrc))
#  ifdef MASKING
     &                                               *umask(i,j)
#  endif
            enddo
          enddo

#  undef I_EXT_RANGE
#  ifndef EW_PERIODIC 
          if (WESTERN_EDGE) then
            do j=Jstr,Jend
              FX(0,j)=FX(1,j)
            enddo
          endif
          if (EASTERN_EDGE) then
#   ifdef MPI
            do j=Jstr,Jend
              FX(Lmmpi+2,j)=FX(Lmmpi+1,j)
            enddo
#   else
             do j=Jstr,Jend
              FX(Lm+2,j)=FX(Lm+1,j)
            enddo
#   endif
          endif
#  endif
!---------------------------------------------------------------------
          do j=Jstr,Jend 
            do i=Istr-1,Iend+1
#  if (defined TS_HADV_C4 || defined PREDICTOR)
              grad(i,j)=0.5*(FX(i+1,j)+FX(i,j))
#  elif defined TS_HADV_AKIMA
              cff=2.*FX(i+1,j)*FX(i,j)
              if (cff.gt.epsil) then
                grad(i,j)=cff/(FX(i+1,j)+FX(i,j))
              else
                grad(i,j)=0.
              endif
#  elif defined TS_HADV_UP3
              curv(i,j)=FX(i+1,j)-FX(i,j)
#  endif
            enddo
          enddo             !--> discard FX
          do j=Jstr,Jend
            do i=Istr,Iend+1
#  if (defined TS_HADV_UP3 && !defined PREDICTOR)
              if (Huon(i,j,k) .gt. 0.) then
                cff=curv(i-1,j)
              else
                cff=curv(i,j)
              endif
              FX(i,j)=0.5*( t(i,j,k,nadv,itrc)+t(i-1,j,k,nadv,itrc)
     &                           -0.333333333333*cff )*Huon(i,j,k)
#   else
              FX(i,j)=0.5*( t(i,j,k,nadv,itrc)+t(i-1,j,k,nadv,itrc)
     &                     -0.333333333333*(grad(i,j)-grad(i-1,j))
     &                                                )*Huon(i,j,k)
#   endif
            enddo
          enddo            !--> discard grad
!---------------------------------------------------------------------
          do j=J_EXT_RANGE
            do i=Istr,Iend
              FE(i,j)=(t(i,j,k,nadv,itrc)-t(i,j-1,k,nadv,itrc))
#  ifdef MASKING
     &                                               *vmask(i,j)
#  endif
            enddo
          enddo
#  undef J_EXT_RANGE
#  ifndef NS_PERIODIC
          if (SOUTHERN_EDGE) then
            do i=Istr,Iend
              FE(i,0)=FE(i,1)
            enddo
          endif
          if (NORTHERN_EDGE) then
#   ifdef MPI
            do i=Istr,Iend
              FE(i,Mmmpi+2)=FE(i,Mmmpi+1)
            enddo
#   else
            do i=Istr,Iend
              FE(i,Mm+2)=FE(i,Mm+1)
            enddo
#   endif
          endif
#  endif
!---------------------------------------------------------------------
          do j=Jstr-1,Jend+1           !<-- C4 [only for pred]
            do i=Istr,Iend
#  if (defined TS_HADV_C4 || defined PREDICTOR)
              grad(i,j)=0.5*(FE(i,j+1)+FE(i,j))
#  elif defined TS_HADV_AKIMA
              cff=2.*FE(i,j+1)*FE(i,j)
              if (cff.gt.epsil) then
                grad(i,j)=cff/(FE(i,j+1)+FE(i,j))
              else
                grad(i,j)=0.
              endif
#  elif defined TS_HADV_UP3
              curv(i,j)=FE(i,j+1)-FE(i,j)
#  endif
            enddo
          enddo            !--> discard FE
          do j=Jstr,Jend+1
            do i=Istr,Iend
#  if (defined TS_HADV_UP3 && !defined PREDICTOR)
              if (Hvom(i,j,k) .gt. 0.) then
                cff=curv(i,j-1)
              else
                cff=curv(i,j)
              endif
              FE(i,j)=0.5*( t(i,j,k,nadv,itrc)+t(i,j-1,k,nadv,itrc)
     &                          -0.333333333333*cff )*Hvom(i,j,k)
#   undef curv
#  else
              FE(i,j)=0.5*( t(i,j,k,nadv,itrc)+t(i,j-1,k,nadv,itrc)
     &                     -0.333333333333*(grad(i,j)-grad(i,j-1))
     &                                               )*Hvom(i,j,k)
#   undef grad
#  endif
            enddo
          enddo            !--> discard grad
!---------------------------------------------------------------------
# endif /* TS_HADV_UP5 */


