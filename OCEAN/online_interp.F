! $Id: online_interp.F 1458 2014-02-03 15:01:25Z gcambon $
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
!
!  This is the "interp.F" script (based on the interpolation implemented in the 
!  Roms Rutgers version) 
!------------------------------------------------------------------------------
!  This file contains the subfunctions enabling the online interpolation of 
!  forcing datasets on the simulation domain using linear or cubic approach. 
!  These functions applied for all discretisations of the domain, MPI or OPENMP.  
!------------------------------------------------------------------------------
#include "cppdefs.h"

      SUBROUTINE linterp2d (LBx, UBx, LBy, UBy,
     &                      Xinp, Yinp, Finp,
     &                      Istr, Iend, Jstr, Jend, 
     &                      Xout, Yout, Fout)
!
!=======================================================================
!                                                                      !
!  Given any gridded 2D field, Finp, this routine linearly interpolate !
!  to locations (Xout,Yout).  To facilitate the  interpolation  within !
!  any irregularly gridded 2D field,  the fractional grid cell indices !
!  (Iout,Jout) with respect Finp are needed at input.  Notice that the !
!  routine "hindices" can be used to compute these indices.            !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     LBx        I-dimension lower bound of gridded field, Finp.       !
!     UBx        I-dimension upper bound of gridded field, Finp.       !
!     LBy        J-dimension lower bound of gridded field, Finp.       !
!     UBy        J-dimension upper bound of gridded field, Finp.       !
!     Xinp       X-locations of gridded field, Finp.                   !
!     Yinp       Y-locations of gridded field, Finp.                   !
!     Finp       2D field to interpolate from.                         !
!     Istr       Starting data I-index to interpolate, Fout.           !
!     Iend       Ending   data I-index to interpolate, Fout.           !
!     Jstr       Starting data J-index to interpolate, Fout.           !
!     Jend       Ending   data J-index to interpolate, Fout.           !
!     Xout       X-locations to interpolate, Fout.                     !
!     Yout       Y-locations to interpolate, Fout.                     !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     Fout       Interpolated 2D field.                                !
!                                                                      !
!=======================================================================
!
!
      implicit none
# include "param.h"
!
!  Imported variable declarations.
!
      integer, intent(in) :: LBx, UBx, LBy, UBy
      integer, intent(in) :: Istr, Iend, Jstr, Jend
!
      real(kind=8), intent(in) :: Xinp(LBx:UBx)
      real(kind=8), intent(in) :: Yinp(LBy:UBy)
      real(kind=8), intent(in) :: Finp(LBx:UBx,LBy:UBy)
!
      real(kind=8), intent(in) :: Xout(GLOBAL_2D_ARRAY)
      real(kind=8), intent(in) :: Yout(GLOBAL_2D_ARRAY)
!
      real(kind=8), intent(out) :: Fout(GLOBAL_2D_ARRAY)
!
!  Local variable declarations.
!
      integer i, i1, i2, j, j1, j2, ii, jj
      real(kind=8) cff, x, x1, x2, y, y1, y2
!
!-----------------------------------------------------------------------
!  Linearly interpolate requested field
!-----------------------------------------------------------------------
!
      DO j=Jstr,Jend
        DO i=Istr,Iend
!          i1=INT(Iout(i,j))
!          i2=i1+1
!          j1=INT(Jout(i,j))
!          j2=j1+1
           DO ii=LBx,(UBx-1)
             if ((Xinp(ii).le.Xout(i,j)).and.
     &           (Xinp(ii+1).gt.Xout(i,j))) then
               i1=ii
               i2=ii+1      
               goto 10 
             endif    
           enddo 
           print*, 'Did not find i1 and i2'
           goto 100 
 10        continue 
           DO jj=LBy,(UBy-1)
             if ((Yinp(jj).le.Yout(i,j)).and.
     &           (Yinp(jj+1).gt.Yout(i,j))) then
               j1=jj
               j2=jj+1     
               goto 20 
             endif
           enddo 
           print*, 'Did not find j1 and j2'
           goto 100 
 20        continue 

          IF (((LBx.le.i1).and.(i1.le.UBx)).and. 
     &        ((LBy.le.j1).and.(j1.le.UBy))) THEN
            x1=Xinp(i1)
            x2=Xinp(i2)
            y1=Yinp(j1)
            y2=Yinp(j2)
            x=Xout(i,j)
            y=Yout(i,j)
           
            cff= Finp(i1,j1)*(x2-x )*(y2-y ) 
     &          +Finp(i2,j1)*(x -x1)*(y2-y ) 
     &          +Finp(i1,j2)*(x2-x )*(y -y1) 
     &          +Finp(i2,j2)*(x -x1)*(y -y1) 

            Fout(i,j)=cff/((x2-x1)*(y2-y1))
          END IF
        END DO
      END DO
      RETURN
 100  continue
      print*, 'error in linterp2d'
      END SUBROUTINE linterp2d

! ******************************************************************************
      SUBROUTINE cinterp2d (LBx, UBx, LBy, UBy,
     &                      Xinp, Yinp, Finp,
     &                      Istr, Iend, Jstr, Jend,
     &                      Xout, Yout, Fout)
!
!=======================================================================
!                                                                      !
!  Given any gridded 2D field,  Finp, at locations (Xinp,Yinp) this    !
!  routine performs bicubic interpolation at locations (Xout,Yout).    !
!  To facilitate the interpolation within any  irregularly  gridded    !
!  field, the fractional grid cell indices (Iout,Jout) with respect    !
!  Finp are needed at input. Notice that the routine "hindices" can    !
!  be used to compute these indices.                                   !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     LBx        I-dimension lower bound of gridded field, Finp.       !
!     UBx        I-dimension upper bound of gridded field, Finp.       !
!     LBy        J-dimension lower bound of gridded field, Finp.       !
!     UBy        J-dimension upper bound of gridded field, Finp.       !
!     Xinp       X-locations of gridded field, Finp.                   !
!     Yinp       Y-locations of gridded field, Finp.                   !
!     Finp       2D field to interpolate from.                         !
!     Istr       Starting data I-index to interpolate, Fout.           !
!     Iend       Ending   data I-index to interpolate, Fout.           !
!     Jstr       Starting data J-index to interpolate, Fout.           !
!     Jend       Ending   data J-index to interpolate, Fout.           !
!     Xout       X-locations to interpolate, Fout.                     !
!     Yout       Y-locations to interpolate, Fout.                     !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     Fout       Interpolated 2D field.                                !
!                                                                      !
!=======================================================================
!
      implicit none
# include "param.h"
# include "grid.h"
!
!  Imported variable declarations.
!
      integer, intent(in) :: LBx, UBx, LBy, UBy
      integer, intent(in) :: Istr, Iend, Jstr, Jend
!
      real(kind=8), intent(in) :: Xinp(LBx:UBx)
      real(kind=8), intent(in) :: Yinp(LBy:UBy)
      real(kind=8), intent(in) :: Finp(LBx:UBx,LBy:UBy)
!
      real(kind=8), intent(in) :: Xout(GLOBAL_2D_ARRAY)
      real(kind=8), intent(in) :: Yout(GLOBAL_2D_ARRAY)
!
      real(kind=8), intent(out) :: Fout(GLOBAL_2D_ARRAY)
!
!  Local variable declarations.
!
      integer i, ic, iter, i1, i2, j, jc, j1, j2, ii, jj

      real(kind=8) :: a11, a12, a21, a22
      real(kind=8) :: e11, e12, e21, e22
      real(kind=8) :: cff, d1, d2, dfc, dx, dy, eta, xi, xy, yx
      real(kind=8) :: f0, fx, fxx, fxxx, fxxy, fxy, fxyy, fy, fyy, fyyy

      real(kind=8), parameter :: C01 = 1.0/48.0
      real(kind=8), parameter :: C02 = 1.0/32.0
      real(kind=8), parameter :: C03 = 0.0625                  ! 1/16
      real(kind=8), parameter :: C04 = 1.0/6.0
      real(kind=8), parameter :: C05 = 0.25
      real(kind=8), parameter :: C06 = 0.5
      real(kind=8), parameter :: C07 = 0.3125                  ! 5/16
      real(kind=8), parameter :: C08 = 0.625                   ! 5/8
      real(kind=8), parameter :: C09 = 1.5
      real(kind=8), parameter :: C10 = 13.0/24.0

      real(kind=8), parameter :: LIMTR = 3.0
      real(kind=8), parameter :: spv = 0.0            ! HGA need work

      real(kind=8), dimension(-1:2,-1:2) :: dfx, dfy, ff
!
!-----------------------------------------------------------------------
!  Interpolates requested field locations (Xout,Yout).
!-----------------------------------------------------------------------
!
      DO j=Jstr,Jend
        DO i=Istr,Iend
!          i1=INT(Iout(i,j))
!          i2=i1+1
!          j1=INT(Jout(i,j))
!          j2=j1+1
           DO ii=LBx,(UBx-1)
             if ((Xinp(ii).le.Xout(i,j)).and.
     &           (Xinp(ii+1).gt.Xout(i,j))) then
               i1=ii
               i2=ii+1       
               goto 10 
             endif  
           enddo 
           print*, 'Did not find i1 and i2',
     &           Istr,Iend,Jstr,Jend,i,j,Xout(i,j),Xout(i-1,j)
           goto 100 
 10        continue 
           DO jj=LBy,UBy-1
             if ((Yinp(jj).le.Yout(i,j)).and.
     &           (Yinp(jj+1).gt.Yout(i,j))) then
               j1=jj
               j2=jj+1      
               goto 20 
             endif  
           enddo 
           print*, 'Did not find j1 and j2'
           goto 100 
 20        continue 

          IF (((LBx.le.i1).and.(i1.le.UBx)).and. 
     &        ((LBy.le.j1).and.(j1.le.UBy))) THEN
!
!  Determine local fractional coordinates (xi,eta) corresponding to
!  the target point (Xout,Yout) on the grid (Xinp,Yinp). Here, "xi"
!  and "eta" are defined, in such a way, that xi=eta=0 corresponds
!  to the middle of the cell (i1:i1+1,j1:j1+1), while xi=+/-1/2 and
!  eta=+/-1/2 (any combination +/- signs) corresponds to the four
!  corner points of the cell. Inside the cell it is assumed that
!  (Xout,Yout) are expressed via bi-linear functions of (xi,eta),
!  where term proportional to xi*eta does not vanish because
!  coordinate transformation may be at least weakly non-orthogonal
!  due to discretization errors. The associated non-linear system
!  is solved by iterative method of Newton.
!
            xy=Xinp(i2)-Xinp(i1)-Xinp(i2)+Xinp(i1)
            yx=Yinp(j2)-Yinp(j2)-Yinp(j1)+Yinp(j1)
            dx=Xout(i,j)-0.25*(Xinp(i2)+Xinp(i1)+ 
     &                         Xinp(i2)+Xinp(i1))
            dy=Yout(i,j)-0.25*(Yinp(j2)+Yinp(j2)+ 
     &                         Yinp(j1)+Yinp(j1))
!
!  The coordinate transformation matrix:
!
!           e11 e12
!           e21 e22
!
!  contains derivatives of (Xinp,Yinp) with respect to (xi,eta). Because
!  the coordinates may be non-orthogonal (at least due to discretization
!  errors), the nonlinear system
!
!           e11*xi+e12*eta+xy*xi*eta=dx
!           e21*xi+e22*eta+yx*xi*eta=dy
!
!  needs to be solved in order to retain symmetry.
!
            e11=0.5*(Xinp(i2)-Xinp(i1)+Xinp(i2)-Xinp(i1))
            e12=0.5*(Xinp(i2)+Xinp(i1)-Xinp(i2)-Xinp(i1))
            e21=0.5*(Yinp(j2)-Yinp(j2)+Yinp(j1)-Yinp(j1))
            e22=0.5*(Yinp(j2)+Yinp(j2)-Yinp(j1)-Yinp(j1))
!
            cff=1.0/(e11*e22-e12*e21)
            xi=cff*(e22*dx-e12*dy)
            eta=cff*(e11*dy-e21*dx)
!
            DO iter=1,4
              d1=dx-e11*xi-e12*eta-xy*xi*eta
              d2=dy-e21*xi-e22*eta-yx*xi*eta
              a11=e11+xy*eta
              a12=e12+xy*xi
              a21=e21+yx*eta
              a22=e22+yx*xi
              cff=1.0/(a11*a22-a12*a21)
              xi =xi +cff*(a22*d1-a12*d2)
              eta=eta+cff*(a11*d2-a21*d1)
            END DO

#ifndef CUBIC_MASKED
!
!  Genuinely two-dimensional, isotropic cubic interpolation scheme
!  using 12-point stencil.  In the code below the interpolated field,
!  Fout, is expanded into two-dimensional Taylor series of local
!  fractional coordinates "xi" and "eta", retaining all terms of
!  combined power up to third order (that is, xi, eta, xi^2, eta^2,
!  xi*eta, xi^3, eta^3, xi^2*eta, and xi*eta^2), with all
!  coefficients (i.e, derivatives) computed via           x  x
!  two-dimensional finite difference expressions          |  |
!  of "natural" order of accuracy: 4th-order for       x--x--x--x
!  the field itself and its first derivatives in          |  |
!  both directions; and 2nd-order for all higher-      x--x--x--x
!  order derivatives. The permissible range of            |  |
!  of coordinates is -1/2 < xi,eta < +1/2, which          x--x
!  covers the central cell on the stencil, while
!  xi=eta=0 corresponds to its center. This interpolation scheme has
!  the property that if xi,eta=+/-1/2 (any combination of +/- signs)
!  it reproduces exactly value of the function at the corresponding
!  corner of the central "working" cell. However, it does not pass
!  exactly through the  extreme points of the stencil, where either
!  xi=+/-3/2 or eta+/-3/2. And, unlike a split-directional scheme,
!  when interpolating along the line eta=+/-1/2 (similarly xi=+/-1/2),
!  it has non-zero contribution from points on the side from the line,
!  except if xi=-1/2; 0; +1/2 (similarly eta=-1/2; 0; +1/2).
!
            DO jc=-1,2
              DO ic=-1,2
                ff(ic,jc)=Finp(MAX(1,MIN(UBx,i1+ic)),
     &                         MAX(1,MIN(UBy,j1+jc)))
              END DO
            END DO

            f0=C07*(ff(1,1)+ff(1,0)+ff(0,1)+ff(0,0))- 
     &         C02*(ff(2,0)+ff(2,1)+ff(1,2)+ff(0,2)+ 
     &              ff(-1,1)+ff(-1,0)+ff(0,-1)+ff(1,-1))

            fx=C08*(ff(1,1)+ff(1,0)-ff(0,1)-ff(0,0))-
     &         C01*(ff(2,1)+ff(2,0)-ff(-1,1)-ff(-1,0))- 
     &         C03*(ff(1,2)-ff(0,2)+ff(1,-1)-ff(0,-1))

            fy=C08*(ff(1,1)-ff(1,0)+ff(0,1)-ff(0,0))- 
     &         C01*(ff(1,2)+ff(0,2)-ff(1,-1)-ff(0,-1))-
     &         C03*(ff(2,1)-ff(2,0)+ff(-1,1)-ff(-1,0))

            fxy=ff(1,1)-ff(1,0)-ff(0,1)+ff(0,0)

            fxx=C05*(ff(2,1)-ff(1,1)-ff(0,1)+ff(-1,1)+ 
     &               ff(2,0)-ff(1,0)-ff(0,0)+ff(-1,0))

            fyy=C05*(ff(1,2)-ff(1,1)-ff(1,0)+ff(1,-1)+  
     &               ff(0,2)-ff(0,1)-ff(0,0)+ff(0,-1))

            fxxx=C06*(ff(2,1)+ff(2,0)-ff(-1,1)-ff(-1,0))-
     &           C09*(ff(1,1)+ff(1,0)-ff(0,1)-ff(0,0))

            fyyy=C06*(ff(1,2)+ff(0,2)-ff(1,-1)-ff(0,-1))-
     &           C09*(ff(1,1)-ff(1,0)+ff(0,1)-ff(0,0))

            fxxy=C06*(ff(2,1)-ff(1,1)-ff(0,1)+ff(-1,1)- 
     &                ff(2,0)+ff(1,0)+ff(0,0)-ff(-1,0))

            fxyy=C06*(ff(1,2)-ff(1,1)-ff(1,0)+ff(1,-1)- 
     &                ff(0,2)+ff(0,1)+ff(0,0)-ff(0,-1))
#else
!
!  Algorithm below is equivalent to the one above, except that special
!  care is taken to avoid interpolation accross land. This is achieved
!  by shortening the stencil and reducing order of polynomial, if
!  extreme points of the stencil touch land. This is achieved by
!  expressing all f0,fx,fy,...,fxyy in terms of values of interpolated
!  field at the four corners of central cell (which already checked to
!  stay away from land), and eight one-sided differences dfx,dfy (see
!  below) in such a way that field values at the extreme points of the
!  12-point stencil do not participate directly into f0,fx,...,fxyy.
!  Should an extreme point of the stencil touch land, thus making it
!  impossible to compute the corresponding one-sided difference, this
!  difference is retracted toward the center of the stencil.
!
!  Optionally, a slope-limiting algorithm may be employed to prevent
!  spurious oscillations of the interpolant. This is a valuable property,
!  if dealing with rough data, however, as a side effect, it turns off
!  high-order interpolation in the vicinity of extrema.
!
!  The slope-limiting algorithm employed here checks that two consecutive
!  elementary differences, "dfx" and "dfc" have the same sign and differ
!  in magnitude by no more than factor of 3.
!
            ff(0,0)=Finp(i1,j1)
            ff(1,0)=Finp(i2,j1)
            ff(0,1)=Finp(i1,j2)
            ff(1,1)=Finp(i2,j2)
!
            dfc=ff(1,1)-ff(0,1)
            IF (i1+2.gt.UBx) THEN
              dfx(1,1)=dfc
            ELSE IF (Finp(i1+2,j2).eq.spv) THEN
              dfx(1,1)=dfc
            ELSE
              dfx(1,1)=Finp(i1+2,j2)-ff(1,1)
# ifdef LIMTR
              IF ((dfx(1,1)*dfc).lt.0.0) THEN
                dfx(1,1)=0.0
              ELSE IF (ABS(dfx(1,1)).gt.(LIMTR*ABS(dfc))) THEN
                dfx(1,1)=LIMTR*dfc
              END IF
# endif
            END IF
!
            dfc=ff(1,0)-ff(0,0)
            IF ((i1+2).gt.UBx) THEN
              dfx(1,0)=dfc
            ELSE IF (Finp(i1+2,j1).eq.spv) THEN
              dfx(1,0)=dfc
            ELSE
              dfx(1,0)=Finp(i1+2,j1)-ff(1,0)
# ifdef LIMTR
              IF ((dfx(1,0)*dfc).lt.0.0) THEN
                dfx(1,0)=0.0
              ELSE IF (ABS(dfx(1,0)).gt.(LIMTR*ABS(dfc))) THEN
                dfx(1,0)=LIMTR*dfc
              END IF
# endif
            END IF
!
            dfc=ff(1,1)-ff(0,1)
            IF (i1-1.lt.1) THEN
              dfx(0,1)=dfc
            ELSE IF (Finp(i1-1,j2).eq.spv) THEN
              dfx(0,1)=dfc
            ELSE
              dfx(0,1)=ff(0,1)-Finp(i1-1,j2)
# ifdef LIMTR
              IF ((dfx(0,1)*dfc).lt.0.0) THEN
                dfx(0,1)=0.0
              ELSE IF (ABS(dfx(0,1)).gt.(LIMTR*ABS(dfc))) THEN
                dfx(0,1)=LIMTR*dfc
              END IF
# endif
            END IF
!
            dfc=ff(1,0)-ff(0,0)
            IF (i1-1.lt.1) THEN
              dfx(0,0)=dfc
            ELSE IF (Finp(i1-1,j1).eq.spv) THEN
              dfx(0,0)=dfc
            ELSE
              dfx(0,0)=ff(0,0)-Finp(i1-1,j1)
# ifdef LIMTR
              IF ((dfx(0,0)*dfc).lt.0.0) THEN
                dfx(0,0)=0.0
              ELSE IF (ABS(dfx(0,0)).gt.(LIMTR*ABS(dfc))) THEN
                dfx(0,0)=LIMTR*dfc
              END IF
# endif
            END IF
!
            dfc=ff(1,1)-ff(1,0)
            IF (j1+2.gt.UBy) THEN
              dfy(1,1)=dfc
            ELSE IF (Finp(i2,j1+2).eq.spv) THEN
              dfy(1,1)=dfc
            ELSE
              dfy(1,1)=Finp(i2,j1+2)-ff(1,1)
# ifdef LIMTR
              IF ((dfy(1,1)*dfc).lt.0.0) THEN
                dfy(1,1)=0.0
              ELSEIF (ABS(dfy(1,1)).gt.(LIMTR*ABS(dfc))) THEN
                dfy(1,1)=LIMTR*dfc
              END IF
# endif
            END IF
!
            dfc=ff(0,1)-ff(0,0)
            IF (j1+2.gt.UBy) THEN
              dfy(0,1)=dfc
            ELSE IF (Finp(i1,j1+2).eq.spv) THEN
              dfy(0,1)=dfc
            ELSE
              dfy(0,1)=Finp(i1,j1+2)-ff(0,1)
# ifdef LIMTR
              IF ((dfy(0,1)*dfc).lt.0.0) THEN
                dfy(0,1)=0.0
              ELSE IF (ABS(dfy(0,1)).gt.(LIMTR*ABS(dfc))) THEN
                dfy(0,1)=LIMTR*dfc
              END IF
# endif
            END IF
!
            dfc=ff(1,1)-ff(1,0)
            IF (j1-1.lt.1) THEN
              dfy(1,0)=dfc
            ELSE IF (Finp(i2,j1-1).eq.spv) THEN
              dfy(1,0)=dfc
            ELSE
              dfy(1,0)=ff(1,0)-Finp(i2,j1-1)
# ifdef LIMTR
              IF ((dfy(1,0)*dfc).lt.0.0) THEN
                dfy(1,0)=0.0
              ELSE IF (ABS(dfy(1,0)).gt.(LIMTR*ABS(dfc))) THEN
                dfy(1,0)=LIMTR*dfc
              END IF
# endif
            END IF
!
            dfc=ff(0,1)-ff(0,0)
            IF (j1-1.lt.1) THEN
              dfy(0,0)=dfc
            ELSE IF (Finp(i1,j1-1).eq.spv) THEN
              dfy(0,0)=dfc
            ELSE
              dfy(0,0)=f(0,0)-Finp(i1,j1-1)
# ifdef LIMTR
              IF ((dfy(0,0)*dfc).lt.0.0) THEN
                dfy(0,0)=0.0
              ELSEIF (ABS(dfy(0,0)).gt.(LIMTR*ABS(dfc))) THEN
                dfy(0,0)=LIMTR*dfc
              END IF
# endif
            END IF
!
            f0=C05*(ff(1,1)+ff(1,0)+ff(0,1)+ff(0,0))-
     &         C02*(dfx(1,1)+dfx(1,0)-dfx(0,1)-dfx(0,0)+
     &              dfy(1,1)-dfy(1,0)+dfy(0,1)-dfy(0,0))

            fx=C10*(ff(1,1)-ff(0,1)+ff(1,0)-ff(0,0))-
     &         C01*(dfx(1,1)+dfx(1,0)+dfx(0,1)+dfx(0,0))-
     &         C03*(dfy(1,1)-dfy(0,1)-dfy(1,0)+dfy(0,0))

            fy=C10*(ff(1,1)-ff(1,0)+ff(0,1)-ff(0,0))-
     &         C01*(dfy(1,1)+dfy(0,1)+dfy(1,0)+dfy(0,0))-
     &         C03*(dfx(1,1)-dfx(1,0)-dfx(0,1)+dfx(0,0))

            fxy=ff(1,1)-ff(1,0)-ff(0,1)+ff(0,0)

            fxx=C05*(dfx(1,1)-dfx(0,1)+dfx(1,0)-dfx(0,0))

            fyy=C05*(dfy(1,1)-dfy(1,0)+dfy(0,1)-dfy(0,0))

            fxxx=C06*(dfx(1,1)+dfx(1,0)+dfx(0,1)+dfx(0,0))-
     &           ff(1,1)+ff(0,1)-ff(1,0)+ff(0,0)

            fyyy=C06*(dfy(1,1)+dfy(0,1)+dfy(1,0)+dfy(0,0))-
     &           ff(1,1)+ff(1,0)-ff(0,1)+ff(0,0)

            fxxy=C06*(dfx(1,1)-dfx(0,1)-dfx(1,0)+dfx(0,0))

            fxyy=C06*(dfy(1,1)-dfy(1,0)-dfy(0,1)+dfy(0,0))
#endif
            Fout(i,j)=f0+
     &                fx*xi+ 
     &                fy*eta+
     &                C06*fxx*xi*xi+ 
     &                fxy*xi*eta+  
     &                C06*fyy*eta*eta+
     &                C04*fxxx*xi*xi*xi+
     &                C06*fxxy*xi*xi*eta+
     &                C04*fyyy*eta*eta*eta+
     &                C06*fxyy*xi*eta*eta
          END IF
        END DO
      END DO

      RETURN

 100  continue
      print*, 'error in cinterp2d'
      END SUBROUTINE cinterp2d
