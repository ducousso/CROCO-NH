!
! $Id: modinterpbasic.F 779 2007-12-22 17:04:17Z rblod $
!
!     AGRIF (Adaptive Grid Refinement In Fortran)
!
!     Copyright (C) 2003 Laurent Debreu (Laurent.Debreu@imag.fr)
!                        Christophe Vouland (Christophe.Vouland@imag.fr)
!
!     This program is free software; you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published by
!     the Free Software Foundation; either version 2 of the License, or
!     (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program; if not, write to the Free Software
!     Foundation, Inc., 59 Temple Place- Suite 330, Boston, MA 02111-1307, USA.
!
!
!> Module Agrif_InterpBasic
!>
!> Contains different procedures of interpolation (linear,lagrange, spline,...) used in
!! the Agrif_Interpolation module.
!
module Agrif_InterpBasic
!
    use Agrif_Types
!
    implicit none
!
    real, dimension(5,Agrif_MaxRaff,3)      :: tabppm
    real, dimension(Agrif_MaxRaff)          :: tabdiff2, tabdiff3
    real, dimension(:),   allocatable       :: tabtest4
    real, dimension(:,:), allocatable       :: coeffparent
    integer, dimension(:,:), allocatable    :: indparent
    integer, dimension(:,:), allocatable    :: indparentppm, indchildppm
    integer, dimension(:), allocatable      :: indparentppm_1d, indchildppm_1d
!
    private :: Agrif_limiter_vanleer
!
contains
!
!===================================================================================================
!  subroutine Agrif_basicinterp_linear1D
!
!> Linear 1D interpolation on a child grid (vector y) from its parent grid (vector x).
!---------------------------------------------------------------------------------------------------
subroutine Agrif_basicinterp_linear1D ( x, y, np, nc, s_parent, s_child, ds_parent, ds_child )
!---------------------------------------------------------------------------------------------------
    real, dimension(np), intent(in)     :: x            !< Coarse input data from parent
    real, dimension(nc), intent(out)    :: y            !< Fine output data to child
    integer,             intent(in)     :: np           !< Length of input array
    integer,             intent(in)     :: nc           !< Length of output array
    real,                intent(in)     :: s_parent     !< Parent grid position (s_root = 0)
    real,                intent(in)     :: s_child      !< Child  grid position (s_root = 0)
    real,                intent(in)     :: ds_parent    !< Parent grid dx (ds_root = 1)
    real,                intent(in)     :: ds_child     !< Child  grid dx (ds_root = 1)
!
    integer :: i, coeffraf, locind_parent_left
    real    :: globind_parent_left, globind_parent_right
    real    :: invds, invds2, ypos, ypos2, diff
!
    coeffraf = nint(ds_parent/ds_child)
!
    if ( coeffraf == 1 ) then
        locind_parent_left = 1 + nint((s_child - s_parent)/ds_parent)
        y(1:nc) = x(locind_parent_left:locind_parent_left+nc-1)
        return
    endif
!
    ypos = s_child
    locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent)
    globind_parent_left  = s_parent + (locind_parent_left - 1)*ds_parent
    globind_parent_right = globind_parent_left + ds_parent
!
    invds  = 1./ds_parent
    invds2 = ds_child/ds_parent
    ypos2 = ypos*invds
    globind_parent_right = globind_parent_right*invds
!
    do i = 1,nc-1
!
        if (ypos2 > globind_parent_right) then
            locind_parent_left = locind_parent_left + 1
            globind_parent_right = globind_parent_right + 1.
            ypos2 = ypos*invds+(i-1)*invds2
        endif
!
        diff = globind_parent_right - ypos2
        y(i) = (diff*x(locind_parent_left) + (1.-diff)*x(locind_parent_left+1))
        ypos2 = ypos2 + invds2
!
    enddo
!
    ypos = s_child + (nc-1)*ds_child
    locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent)
!
    if (locind_parent_left == np) then
        y(nc) = x(np)
    else
        globind_parent_left = s_parent + (locind_parent_left - 1)*ds_parent
        y(nc) = ((globind_parent_left + ds_parent - ypos)*x(locind_parent_left)  &
                           + (ypos - globind_parent_left)*x(locind_parent_left+1))*invds
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_basicinterp_linear1D
!===================================================================================================
!
!===================================================================================================
!  subroutine Linear1dPrecompute2d
!
!> Computes 2D coefficients and index for a linear 1D interpolation on a child grid (vector y)
!! from its parent grid (vector x).
!---------------------------------------------------------------------------------------------------
subroutine Linear1dPrecompute2d ( np2, np, nc, s_parent, s_child, ds_parent, ds_child, dir )
!---------------------------------------------------------------------------------------------------
    integer, intent(in) :: np,nc,np2
    real,    intent(in) :: s_parent, s_child
    real,    intent(in) :: ds_parent, ds_child
    integer, intent(in) :: dir
!
    integer :: i,coeffraf,locind_parent_left,inc,inc1,inc2
    integer, dimension(:,:), allocatable :: indparent_tmp
    real, dimension(:,:), allocatable :: coeffparent_tmp
    real    :: ypos,globind_parent_left,globind_parent_right
    real    :: invds, invds2, invds3
    real :: ypos2,diff
!
    coeffraf = nint(ds_parent/ds_child)
!
    ypos = s_child
    locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent)
    globind_parent_left = s_parent + (locind_parent_left - 1)*ds_parent
    globind_parent_right = globind_parent_left + ds_parent
!
    invds = 1./ds_parent
    invds2 = ds_child/ds_parent
    invds3 = 0.5/real(coeffraf)
    ypos2 = ypos*invds
    globind_parent_right=globind_parent_right*invds
!
    if (.not.allocated(indparent)) then
        allocate(indparent(nc*np2,3),coeffparent(nc*np2,3))
    else
        if ( size(indparent,1) < nc*np2 ) then
            allocate(coeffparent_tmp(size(indparent,1),size(indparent,2)))
            allocate(  indparent_tmp(size(indparent,1),size(indparent,2)))
            coeffparent_tmp = coeffparent
            indparent_tmp   = indparent
            deallocate(indparent,coeffparent)
            allocate(indparent(nc*np2,3),coeffparent(nc*np2,3))
            coeffparent(1:size(coeffparent_tmp,1),1:size(coeffparent_tmp,2)) = coeffparent_tmp
            indparent(  1:size(indparent_tmp,  1),1:size(indparent_tmp,  2)) = indparent_tmp
            deallocate(indparent_tmp,coeffparent_tmp)
        endif
    endif
!
    do i = 1,nc-1
!
        if (ypos2 > globind_parent_right) then
            locind_parent_left = locind_parent_left + 1
            globind_parent_right = globind_parent_right + 1.
            ypos2 = ypos*invds+(i-1)*invds2
        endif
!
        diff = globind_parent_right - ypos2
        diff = invds3*nint(2*coeffraf*diff)
        indparent(i,dir) = locind_parent_left
        coeffparent(i,dir) = diff
        ypos2 = ypos2 + invds2
!
    enddo
!
    ypos = s_child + (nc-1)*ds_child
    locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent)

    if (locind_parent_left == np) then
        indparent(nc,dir) = locind_parent_left-1
        coeffparent(nc,dir) = 0.
    else
        globind_parent_left = s_parent + (locind_parent_left - 1)*ds_parent
        indparent(nc,dir) = locind_parent_left
        diff = (globind_parent_left + ds_parent - ypos) * invds
        diff = invds3*nint(2*coeffraf*diff)
        coeffparent(nc,dir) = diff
    endif

    do i=2, np2
        inc  =  i*nc
        inc1 = (i-1)*nc
        inc2 = (i-2)*nc
!CDIR ALTCODE
        indparent(1+inc1:inc,dir) = indparent(1+inc2:inc1,dir)+np
!CDIR ALTCODE
        coeffparent(1+inc1:inc,dir) =coeffparent(1:nc,dir)
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Linear1dPrecompute2d
!===================================================================================================
!
!===================================================================================================
!  subroutine Linear1dAfterCompute
!
!> Carries out a linear 1D interpolation on a child grid (vector y) from its parent grid (vector x)
!! using precomputed coefficient and index.
!---------------------------------------------------------------------------------------------------
subroutine Linear1dAfterCompute ( x, y, np, nc, dir )
!---------------------------------------------------------------------------------------------------
    integer,             intent(in)     :: np, nc
    real, dimension(np), intent(in)     :: x
    real, dimension(nc), intent(out)    :: y
    integer,             intent(in)     :: dir
!
    integer :: i
!
!CDIR ALTCODE
!CDIR NODEP
    do i = 1,nc
        y(i) = coeffparent(i,dir)  * x(MAX(indparent(i,dir),1)) + &
           (1.-coeffparent(i,dir)) * x(indparent(i,dir)+1)
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Linear1dAfterCompute
!===================================================================================================
!
!===================================================================================================
!  subroutine Lagrange1d
!
!> Carries out a lagrange 1D interpolation on a child grid (vector y) from its parent grid
!! (vector x).
!---------------------------------------------------------------------------------------------------
subroutine Lagrange1d ( x, y, np, nc, s_parent, s_child, ds_parent, ds_child )
!---------------------------------------------------------------------------------------------------
    integer,             intent(in)     :: np, nc
    real, dimension(np), intent(in)     :: x
    real, dimension(nc), intent(out)    :: y
    real,                intent(in)     :: s_parent, s_child
    real,                intent(in)     :: ds_parent, ds_child
!
    integer :: i, coeffraf, locind_parent_left
    real    :: ypos,globind_parent_left
    real    :: deltax, invdsparent
    real    :: t2,t3,t4,t5,t6,t7,t8
!
    if (np <= 2) then
        call Agrif_basicinterp_linear1D(x,y,np,nc,s_parent,s_child,ds_parent,ds_child)
        return
    endif
!
    coeffraf = nint(ds_parent/ds_child)
!
    if (coeffraf == 1) then
        locind_parent_left = 1 + nint((s_child - s_parent)/ds_parent)
        y(1:nc) = x(locind_parent_left:locind_parent_left+nc-1)
        return
    endif
!
    invdsparent = 1./ds_parent
    ypos = s_child
!
    do i = 1,nc
!
        locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent)
        globind_parent_left = s_parent + (locind_parent_left - 1)*ds_parent

        deltax = invdsparent*(ypos-globind_parent_left)
        deltax = nint(coeffraf*deltax)/real(coeffraf)

        ypos = ypos + ds_child
        if (abs(deltax) <= 0.0001) then
            y(i)=x(locind_parent_left)
            cycle
        endif
!
        t2 = deltax - 2.
        t3 = deltax - 1.
        t4 = deltax + 1.

        t5 = -(1./6.)*deltax*t2*t3
        t6 = 0.5*t2*t3*t4
        t7 = -0.5*deltax*t2*t4
        t8 = (1./6.)*deltax*t3*t4

        y(i) = t5*x(locind_parent_left-1) + t6*x(locind_parent_left)    &
              +t7*x(locind_parent_left+1) + t8*x(locind_parent_left+2)
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Lagrange1d
!===================================================================================================
!
!===================================================================================================
!  subroutine Constant1d
!
!> Carries out a constant 1D interpolation on a child grid (vector y) from its parent grid (vector x).
!---------------------------------------------------------------------------------------------------
subroutine Constant1d ( x, y, np, nc, s_parent, s_child, ds_parent, ds_child )
!---------------------------------------------------------------------------------------------------
    integer,             intent(in)     :: np, nc
    real, dimension(np), intent(in)     :: x
    real, dimension(nc), intent(out)    :: y
    real,                intent(in)     :: s_parent, s_child
    real,                intent(in)     :: ds_parent, ds_child
!
    integer :: i, coeffraf, locind_parent
    real    :: ypos
!
    coeffraf = nint(ds_parent/ds_child)
!
    if (coeffraf == 1) then
        locind_parent = 1 + nint((s_child - s_parent)/ds_parent)
        y(1:nc) = x(locind_parent:locind_parent+nc-1)
        return
    endif
!
    ypos = s_child
!
    do i = 1,nc
!
        locind_parent = 1 + nint((ypos - s_parent)/ds_parent)
        y(i) = x(locind_parent)
        ypos = ypos + ds_child
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Constant1d
!===================================================================================================
!
!===================================================================================================
!  subroutine Linear1dConserv
!
!> Carries out a conservative linear 1D interpolation on a child grid (vector y) from its parent 
!! grid (vector x).
!---------------------------------------------------------------------------------------------------
subroutine Linear1dConserv ( x, y, np, nc, s_parent, s_child, ds_parent, ds_child )
!---------------------------------------------------------------------------------------------------
    integer,             intent(in)     :: np, nc
    real, dimension(np), intent(in)     :: x
    real, dimension(nc), intent(out)    :: y
    real,                intent(in)     :: s_parent, s_child
    real,                intent(in)     :: ds_parent, ds_child
!
    real, dimension(:), allocatable :: ytemp
    integer :: i,coeffraf,locind_parent_left,locind_parent_last
    real    :: ypos,xdiffmod,xpmin,xpmax,slope
    integer :: i1,i2,ii
    integer :: diffmod
!
    coeffraf = nint(ds_parent/ds_child)
!
    if (coeffraf == 1) then
        locind_parent_left = 1 + nint((s_child - s_parent)/ds_parent)
        y(1:nc) = x(locind_parent_left:locind_parent_left+nc-1)
        return
    endif
!
    diffmod = 0
    if (mod(coeffraf,2) == 0) diffmod = 1

    xdiffmod = real(diffmod)/2.

    allocate(ytemp(-2*coeffraf:nc+2*coeffraf))
!
    ypos = s_child
!
    locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent)
    locind_parent_last = 1 + agrif_ceiling((ypos +(nc - 1) *ds_child - s_parent)/ds_parent)

    xpmin = s_parent + (locind_parent_left-1)*ds_parent
    xpmax = s_parent + (locind_parent_last-1)*ds_parent

    i1 = 1+agrif_int((xpmin-s_child)/ds_child)
    i2 = 1+agrif_int((xpmax-s_child)/ds_child)

    i = i1

    if (locind_parent_left == 1) then
        slope = (x(locind_parent_left+1)-x(locind_parent_left))/(coeffraf)
    else
        slope = (x(locind_parent_left+1)-x(locind_parent_left-1))/(2.*coeffraf)
    endif

    do ii = i-coeffraf/2+diffmod,i+coeffraf/2
        ytemp(ii) = x(locind_parent_left)+(ii-i-xdiffmod/2.)*slope
    enddo

    locind_parent_left = locind_parent_left + 1

    do i = i1+coeffraf, i2-coeffraf,coeffraf
        slope = (x(locind_parent_left+1)-x(locind_parent_left-1))/(2.*coeffraf)
        do ii = i-coeffraf/2+diffmod,i+coeffraf/2
            ytemp(ii) = x(locind_parent_left)+(ii-i-xdiffmod/2.)*slope
        enddo
        locind_parent_left = locind_parent_left + 1
    enddo

    i = i2

    if (locind_parent_left == np) then
        slope = (x(locind_parent_left)-x(locind_parent_left-1))/(coeffraf)
    else
        slope = (x(locind_parent_left+1)-x(locind_parent_left-1))/(2.*coeffraf)
    endif

    do ii = i-coeffraf/2+diffmod,nc
        ytemp(ii) = x(locind_parent_left)+(ii-i-xdiffmod/2.)*slope
    enddo
!
    y(1:nc)=ytemp(1:nc)
!
    deallocate(ytemp)
!---------------------------------------------------------------------------------------------------
end subroutine Linear1dConserv
!===================================================================================================
!
!===================================================================================================
!  subroutine Linear1dConservLim
!
!> Carries out a limited conservative linear 1D interpolation on a child grid (vector y) from
!! its parent grid (vector x).
!---------------------------------------------------------------------------------------------------
subroutine Linear1dConservLim ( x, y, np, nc, s_parent, s_child, ds_parent, ds_child )
!---------------------------------------------------------------------------------------------------
    integer,             intent(in)     :: np, nc
    real, dimension(np), intent(in)     :: x
    real, dimension(nc), intent(out)    :: y
    real,                intent(in)     :: s_parent, s_child
    real,                intent(in)     :: ds_parent, ds_child
!
    real, dimension(:), allocatable :: ytemp
    integer :: i,coeffraf,locind_parent_left,locind_parent_last
    real    :: ypos,xdiffmod,xpmin,xpmax,slope
    integer :: i1,i2,ii
    integer :: diffmod
!
    coeffraf = nint(ds_parent/ds_child)
!
    if (coeffraf == 1) then
        locind_parent_left = 1 + nint((s_child - s_parent)/ds_parent)
        y(1:nc) = x(locind_parent_left:locind_parent_left+nc-1)
        return
    endif
!
    if (coeffraf /= 3) then
        print *,'Linear1dConservLim not ready for refinement ratio = ', coeffraf
        stop
    endif
!
    diffmod = 0
    if (mod(coeffraf,2) == 0) diffmod = 1

    xdiffmod = real(diffmod)/2.

    allocate(ytemp(-2*coeffraf:nc+2*coeffraf))
!
    ypos = s_child
!
    locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent)
    locind_parent_last = 1 + agrif_ceiling((ypos +(nc - 1) *ds_child - s_parent)/ds_parent)

    xpmin = s_parent + (locind_parent_left-1)*ds_parent
    xpmax = s_parent + (locind_parent_last-1)*ds_parent

    i1 = 1+agrif_int((xpmin-s_child)/ds_child)
    i2 = 1+agrif_int((xpmax-s_child)/ds_child)

    i = i1

    if (locind_parent_left == 1) then
        slope=0.
    else
        slope = Agrif_limiter_vanleer(x(locind_parent_left-1:locind_parent_left+1))
        slope = slope / coeffraf
    endif

    do ii = i-coeffraf/2+diffmod,i+coeffraf/2
        ytemp(ii) = x(locind_parent_left)+(ii-i-xdiffmod/2.)*slope
    enddo

    locind_parent_left = locind_parent_left + 1

    do i = i1+coeffraf, i2-coeffraf,coeffraf
        slope = Agrif_limiter_vanleer(x(locind_parent_left-1:locind_parent_left+1))
        slope = slope / coeffraf
        do ii=i-coeffraf/2+diffmod,i+coeffraf/2
            ytemp(ii) = x(locind_parent_left)+(ii-i-xdiffmod/2.)*slope
        enddo
        locind_parent_left = locind_parent_left + 1
    enddo

    i = i2

    if (locind_parent_left == np) then
        slope=0.
    else
        slope = Agrif_limiter_vanleer(x(locind_parent_left-1:locind_parent_left+1))
        slope = slope / coeffraf
    endif

    do ii=i-coeffraf/2+diffmod,nc
        ytemp(ii) = x(locind_parent_left)+(ii-i-xdiffmod/2.)*slope
    enddo
!
    y(1:nc) = ytemp(1:nc)
!
    deallocate(ytemp)
!---------------------------------------------------------------------------------------------------
end subroutine Linear1dConservLim
!===================================================================================================
!
!===================================================================================================
!  subroutine PPM1d
!
!> Carries out a 1D interpolation and apply monotonicity constraints using piecewise parabolic
!! method (PPM) on a child grid (vector y) from its parent grid (vector x).
!---------------------------------------------------------------------------------------------------
subroutine PPM1d ( x, y, np, nc, s_parent, s_child, ds_parent, ds_child )
!---------------------------------------------------------------------------------------------------
    integer,             intent(in)     :: np, nc
    real, dimension(np), intent(in)     :: x
    real, dimension(nc), intent(out)    :: y
    real,                intent(in)     :: s_parent, s_child
    real,                intent(in)     :: ds_parent, ds_child
!
    integer :: i,coeffraf,locind_parent_left,locind_parent_last
    integer :: iparent,ipos,pos,nmin,nmax
    real    :: ypos
    integer :: i1,jj
    real :: xpmin,a
!
    real, dimension(np) :: xl,delta,a6,slope
    integer :: diffmod
    real :: invcoeffraf
!
    coeffraf = nint(ds_parent/ds_child)
!
    if (coeffraf == 1) then
        locind_parent_left = 1 + nint((s_child - s_parent)/ds_parent)
!CDIR ALTCODE
!CDIR SHORTLOOP
        y(1:nc) = x(locind_parent_left:locind_parent_left+nc-1)
        return
    endif
!
    invcoeffraf = ds_child/ds_parent
!
    if( .not. allocated(tabtest4) ) then
        allocate(tabtest4(-2*coeffraf:nc+2*coeffraf))
    else
        if (size(tabtest4) < nc+4*coeffraf+1) then
            deallocate( tabtest4 )
            allocate(tabtest4(-2*coeffraf:nc+2*coeffraf))
        endif
    endif
!
    ypos = s_child
!
    locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent)
    locind_parent_last = 1 + agrif_ceiling((ypos +(nc - 1)*ds_child - s_parent)/ds_parent)
!
    xpmin = s_parent + (locind_parent_left-1)*ds_parent
    i1 = 1+agrif_int((xpmin-s_child)/ds_child)
!
!CDIR NOVECTOR
    do i=1,coeffraf
        tabdiff2(i) = (real(i)-0.5)*invcoeffraf
    enddo
!
    a = invcoeffraf**2
    tabdiff3(1) = (1./3.)*a
    a = 2.*a
!CDIR NOVECTOR
    do i=2,coeffraf
        tabdiff3(i) = tabdiff3(i-1)+(real(i)-1)*a
    enddo
!
    if      ( locind_parent_last+2 <= np ) then
        nmax = locind_parent_last+2
    else if ( locind_parent_last+1 <= np ) then
        nmax = locind_parent_last+1
    else
        nmax = locind_parent_last
    endif
!
    if (locind_parent_left-1 >= 1) then
        nmin = locind_parent_left-1
    else
        nmin = locind_parent_left
    endif
!
!CDIR ALTCODE
!CDIR SHORTLOOP
    do i = nmin,nmax
        slope(i) = x(i) - x(i-1)
    enddo
!
!CDIR ALTCODE
!CDIR SHORTLOOP
    do i = nmin+1,nmax-1
        xl(i)= 0.5*(x(i-1)+x(i))-0.08333333333333*(slope(i+1)-slope(i-1))
    enddo
!
! apply parabolic monotonicity
!CDIR ALTCODE
!CDIR SHORTLOOP
    do i = locind_parent_left,locind_parent_last
        delta(i) = xl(i+1) - xl(i)
        a6(i) = 6.*x(i)-3.*(xl(i) +xl(i+1))
    enddo
!
    diffmod = 0
    if (mod(coeffraf,2) == 0) diffmod = 1
!
    ipos = i1
!
    do iparent = locind_parent_left,locind_parent_last
        pos=1
!CDIR ALTCODE
!CDIR SHORTLOOP
        do jj = ipos-coeffraf/2+diffmod,ipos+coeffraf/2
!
            tabtest4(jj) = xl(iparent) + tabdiff2(pos) *  (delta(iparent)+a6(iparent)) &
                                       - tabdiff3(pos) *  a6(iparent)
            pos = pos+1
        enddo
        ipos = ipos + coeffraf
    enddo
!
!CDIR ALTCODE
!CDIR SHORTLOOP
    y(1:nc) = tabtest4(1:nc)
!---------------------------------------------------------------------------------------------------
end subroutine PPM1d
!===================================================================================================
!
!===================================================================================================
!  subroutine PPM1dPrecompute2d
!
!> Computes 2D coefficients and index for a 1D interpolation using piecewise parabolic method
!---------------------------------------------------------------------------------------------------
subroutine PPM1dPrecompute2d ( np2, np, nc, s_parent, s_child, ds_parent, ds_child, dir )
!---------------------------------------------------------------------------------------------------
    integer,             intent(in)     :: np2, np, nc
    real,                intent(in)     :: s_parent, s_child
    real,                intent(in)     :: ds_parent, ds_child
    integer,             intent(in)     :: dir
!
    integer, dimension(:,:), allocatable :: indparent_tmp
    integer, dimension(:,:), allocatable :: indchild_tmp
    integer :: i,coeffraf,locind_parent_left,locind_parent_last
    integer :: iparent,ipos,pos
    real    :: ypos
    integer :: i1,jj
    real :: xpmin,a
!
    integer :: diffmod
    real :: invcoeffraf
!
    coeffraf = nint(ds_parent/ds_child)
!
    invcoeffraf = ds_child/ds_parent
!
    if (.not.allocated(indparentppm)) then
        allocate(indparentppm(np2*nc,3),indchildppm(np2*nc,3))
    else
        if (size(indparentppm,1) < np2*nc) then
            allocate(   &
                indparent_tmp(size(indparentppm,1),size(indparentppm,2)), &
                indchild_tmp( size(indparentppm,1),size(indparentppm,2)))
            indparent_tmp = indparentppm
            indchild_tmp  = indchildppm
            deallocate(indparentppm,indchildppm)
            allocate(indparentppm(np2*nc,3),indchildppm(np2*nc,3))
            indparentppm(1:size(indparent_tmp,1),1:size(indparent_tmp,2)) = indparent_tmp
            indchildppm( 1:size(indparent_tmp,1),1:size(indparent_tmp,2)) = indchild_tmp
            deallocate(indparent_tmp,indchild_tmp)
        endif
    endif

    if (.not.allocated(indparentppm_1d)) then
        allocate(indparentppm_1d(-2*coeffraf:nc+2*coeffraf),    &
                 indchildppm_1d( -2*coeffraf:nc+2*coeffraf))
    else
        if (size(indparentppm_1d) < nc+4*coeffraf+1) then
            deallocate(indparentppm_1d,indchildppm_1d)
            allocate(indparentppm_1d(-2*coeffraf:nc+2*coeffraf),&
                     indchildppm_1d( -2*coeffraf:nc+2*coeffraf))
        endif
    endif
!
    ypos = s_child
!
    locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent)
    locind_parent_last = 1 + agrif_ceiling((ypos +(nc - 1)*ds_child - s_parent)/ds_parent)
!
    xpmin = s_parent + (locind_parent_left-1)*ds_parent
    i1 = 1+agrif_int((xpmin-s_child)/ds_child)
!
    do i = 1,coeffraf
        tabdiff2(i)=(real(i)-0.5)*invcoeffraf
    enddo
!
    a = invcoeffraf**2
    tabdiff3(1) = (1./3.)*a
    a = 2.*a
!CDIR ALTCODE
    do i = 2,coeffraf
        tabdiff3(i) = tabdiff3(i-1)+(real(i)-1)*a
    enddo

!CDIR ALTCODE
    do i = 1,coeffraf
        tabppm(1,i,dir) = 0.08333333333333*(-1.+4*tabdiff2(i)-3*tabdiff3(i))
        tabppm(2,i,dir) = 0.08333333333333*(7.-26.*tabdiff2(i)+18.*tabdiff3(i))
        tabppm(3,i,dir) = 0.08333333333333*(7.+30*tabdiff2(i)-30*tabdiff3(i))
        tabppm(4,i,dir) = 0.08333333333333*(-1.-10.*tabdiff2(i)+18.*tabdiff3(i))
        tabppm(5,i,dir) = 0.08333333333333*(2*tabdiff2(i)-3*tabdiff3(i))
    enddo
!
    diffmod = 0
    if (mod(coeffraf,2) == 0) diffmod = 1
!
    ipos = i1
!
    do iparent = locind_parent_left,locind_parent_last
        pos=1
!CDIR ALTCODE
        do jj = ipos - coeffraf/2+diffmod,ipos + coeffraf/2
            indparentppm_1d(jj) = iparent-2
            indchildppm_1d(jj)  = pos
            pos = pos+1
        enddo
        ipos = ipos + coeffraf
    enddo
!
    do i = 1,np2
        indparentppm(1+(i-1)*nc:i*nc,dir) = indparentppm_1d(1:nc) + (i-1)*np
        indchildppm (1+(i-1)*nc:i*nc,dir) = indchildppm_1d (1:nc)
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine PPM1dPrecompute2d
!===================================================================================================
!
!===================================================================================================
!subroutine PPM1dPrecompute(np,nc,&
!                    s_parent,s_child,ds_parent,ds_child)
!!
!!CC   Description:
!!CC   subroutine to compute coefficient and index for  a 1D interpolation
!!CC   using piecewise parabolic method
!!C    Method:
!!
!!     Declarations:
!!
!      Implicit none
!!
!!     Arguments
!      Integer             :: np,nc
!!      Real, Dimension(:),Allocatable :: ytemp
!      Real                :: s_parent,s_child,ds_parent,ds_child
!!
!!     Local scalars
!      Integer :: i,coeffraf,locind_parent_left,locind_parent_last
!      Integer :: iparent,ipos,pos,nmin,nmax
!      Real    :: ypos
!      integer :: i1,jj
!      Real :: xpmin,a
!!
!      Real :: xrmin,xrmax,am3,s2,s1
!      Real, Dimension(np) :: xl,delta,a6,slope
!!      Real, Dimension(:),Allocatable  :: diff,diff2,diff3
!      INTEGER :: diffmod
!      REAL :: invcoeffraf
!!
!      coeffraf = nint(ds_parent/ds_child)
!!
!      If (coeffraf == 1) Then
!          return
!      End If
!      invcoeffraf = ds_child/ds_parent
!!
!
!      if (.not.allocated(indparentppm)) then
!      allocate(indparentppm(-2*coeffraf:nc+2*coeffraf,1),&
!         indchildppm(-2*coeffraf:nc+2*coeffraf,1))
!      else
!      if (size(indparentppm,1)<nc+4*coeffraf+1) then
!      deallocate(indparentppm,indchildppm)
!      allocate(indparentppm(-2*coeffraf:nc+2*coeffraf,1),&
!         indchildppm(-2*coeffraf:nc+2*coeffraf,1))
!      endif
!      endif
!
!      ypos = s_child
!!
!      locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent)
!      locind_parent_last = 1 +&
!      agrif_ceiling((ypos +(nc - 1)&
!      *ds_child - s_parent)/ds_parent)
!!
!      xpmin = s_parent + (locind_parent_left-1)*ds_parent
!      i1 = 1+agrif_int((xpmin-s_child)/ds_child)
!!
!!
!
!      Do i=1,coeffraf
!        tabdiff2(i)=(real(i)-0.5)*invcoeffraf
!      EndDo
!
!      a = invcoeffraf**2
!      tabdiff3(1) = (1./3.)*a
!      a=2.*a
!!CDIR ALTCODE
!!!!CDIR SHORTLOOP
!      Do i=2,coeffraf
!        tabdiff3(i) = tabdiff3(i-1)+(real(i)-1)*a
!      EndDo
!
!!CDIR ALTCODE
!!!!CDIR SHORTLOOP
!      Do i=1,coeffraf
!      tabppm(1,i,1) = 0.08333333333333*(-1.+4*tabdiff2(i)-3*tabdiff3(i))
!      tabppm(2,i,1) = 0.08333333333333*&
!              (7.-26.*tabdiff2(i)+18.*tabdiff3(i))
!      tabppm(3,i,1) =0.08333333333333*(7.+30*tabdiff2(i)-30*tabdiff3(i))
!      tabppm(4,i,1) = 0.08333333333333*&
!              (-1.-10.*tabdiff2(i)+18.*tabdiff3(i))
!      tabppm(5,i,1) = 0.08333333333333*(2*tabdiff2(i)-3*tabdiff3(i))
!      End Do
!!
!!
!        diffmod = 0
!       IF (mod(coeffraf,2) == 0) diffmod = 1
!!
!        ipos = i1
!!
!        Do iparent = locind_parent_left,locind_parent_last
!             pos=1
!!CDIR ALTCODE
!!CDIR SHORTLOOP
!             Do jj = ipos - coeffraf/2+diffmod,ipos + coeffraf/2
!         indparentppm(jj,1) = iparent-2
!         indchildppm(jj,1) = pos
!               pos = pos+1
!             End do
!             ipos = ipos + coeffraf
!!
!        End do
!
!      Return
!      End subroutine ppm1dprecompute
!===================================================================================================
!
!===================================================================================================
!  subroutine PPM1dAfterCompute
!
! Carries out a 1D interpolation and apply monotonicity constraints using piecewise parabolic
! method (PPM) on a child grid (vector y) from its parent grid (vector x).
! Use precomputed coefficient and index.
!---------------------------------------------------------------------------------------------------
subroutine PPM1dAfterCompute ( x, y, np, nc, dir )
!---------------------------------------------------------------------------------------------------
    real, dimension(np), intent(in)     :: x
    real, dimension(nc), intent(out)    :: y
    integer,             intent(in)     :: np, nc
    integer,             intent(in)     :: dir
!
    integer :: i
!
    do i = 1,nc
        y(i) = tabppm(1,indchildppm(i,dir),dir) * x(indparentppm(i,dir)  ) + &
               tabppm(2,indchildppm(i,dir),dir) * x(indparentppm(i,dir)+1) + &
               tabppm(3,indchildppm(i,dir),dir) * x(indparentppm(i,dir)+2) + &
               tabppm(4,indchildppm(i,dir),dir) * x(indparentppm(i,dir)+3) + &
               tabppm(5,indchildppm(i,dir),dir) * x(indparentppm(i,dir)+4)
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine PPM1dAfterCompute
!===================================================================================================
!
!===================================================================================================
!  subroutine weno1d
!
! Carries out a 1D interpolation and apply monotonicity constraints
! using WENO method on a child grid (vector y) from its parent grid (vector x).
!---------------------------------------------------------------------------------------------------
!subroutine weno1dnew ( x, y, np, nc, s_parent, s_child, ds_parent, ds_child )
!---------------------------------------------------------------------------------------------------
!
!CC   Description:
!CC   subroutine to do a 1D interpolation and apply monotonicity constraints
!CC   using piecewise parabolic method
!CC   on a child grid (vector y) from its parent grid (vector x).
!C    Method:
!
!     Declarations:
!
!      Implicit none
!
!     Arguments
!      Integer             :: np,nc
!      Real, Dimension(np) :: x
!      Real, Dimension(nc) :: y
!      Real, Dimension(:),Allocatable :: ytemp
!      Real                :: s_parent,s_child,ds_parent,ds_child
!
!     Local scalars
!      Integer :: i,coeffraf,locind_parent_left,locind_parent_last
!      Integer :: iparent,ipos,pos,nmin,nmax
!      Real    :: ypos
!      integer :: i1,jj
!      Real :: xpmin,cavg,a,b
!
!      Real :: xrmin,xrmax,am3,s2,s1
!      Real, Dimension(np) :: xr,xl,delta,a6,slope,slope2,smooth
!      Real, Dimension(:),Allocatable  :: diff,diff2,diff3
!      INTEGER :: diffmod
!      REAL :: invcoeffraf
!      integer :: s,l,k
!      integer :: etan, etap
!      real :: delta0, delta1, delta2
!      real :: epsilon
!      parameter (epsilon = 1.D-8)
!      real, dimension(:,:), allocatable :: ak, ck
!
!      coeffraf = nint(ds_parent/ds_child)
!
!      If (coeffraf == 1) Then
!          locind_parent_left = 1 + nint((s_child - s_parent)/ds_parent)
!          y(1:nc) = x(locind_parent_left:locind_parent_left+nc-1)
!          return
!      End If
!      invcoeffraf = ds_child/ds_parent
!      Allocate(ak(0:1,coeffraf))
!      Allocate(ck(0:1,coeffraf))
!
!
!      Allocate(ytemp(-2*coeffraf:nc+2*coeffraf))
!      ypos = s_child
!
!      locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent)
!      locind_parent_last = 1 +&
!      agrif_ceiling((ypos +(nc - 1)&
!      *ds_child - s_parent)/ds_parent)
!
!      xpmin = s_parent + (locind_parent_left-1)*ds_parent
!      i1 = 1+agrif_int((xpmin-s_child)/ds_child)
!
!      Allocate( diff(coeffraf),diff2(coeffraf),diff3(coeffraf) )
!
!      diff(1)=0.5*invcoeffraf
!      do i=2,coeffraf
!         diff(i) = diff(i-1)+invcoeffraf
!      enddo
!
!      ak = 0.
!      ck = 0.
!
!      do i=1,coeffraf
!         do k=0,1
!         do s=0,2
!         do l=0,2
!           if (l /= s) then
!            ak(k,i) = ak(k,i)+(diff(i)-(k-l+1.))
!           endif
!         enddo
!         enddo
!         enddo
!
!         etap = 0
!         etan = 0
!         do k=0,1
!          if (ak(k,i) > 0) then
!            etap = etap+1
!          else if (ak(k,i) < 0) then
!            etan = etan + 1
!          endif
!         enddo
!
!         do k=0,1
!           if (ak(k,i) == 0) then
!            Ck(k,i) = 1.
!           else if (ak(k,i) > 0) then
!            Ck(k,i) = 1./(etap * ak(k,i))
!           else
!            Ck(k,i) = -1./(etan * ak(k,i))
!           endif
!         enddo
!      enddo
!
!
!      a = 0.
!      b = invcoeffraf
!      Do i=1,coeffraf
!         diff2(i) = 0.5*(b*b - a*a)
!         diff3(i) = (1./3.)*(b*b*b - a*a*a)
!         a = a + invcoeffraf
!         b = b + invcoeffraf
!      End do
!
!      if( locind_parent_last+2 <= np ) then
!           nmax = locind_parent_last+2
!      elseif( locind_parent_last+1 <= np ) then
!           nmax = locind_parent_last+1
!      else
!           nmax = locind_parent_last
!      endif
!
!      if(locind_parent_left-2 >= 1) then
!          nmin = locind_parent_left-2
!      elseif(locind_parent_left-1 >= 1) then
!          nmin = locind_parent_left-1
!      else
!          nmin = locind_parent_left
!      endif
!
!      Do i = nmin+1,nmax
!         slope(i) = (x(i) - x(i-1))
!      Enddo
!      DO i=nmin+2,nmax
!        smooth(i) = 0.5*(slope(i)**2+slope(i-1)**2)&
!       +(slope(i)-slope(i-1))**2
!      enddo
!
!        diffmod = 0
!        IF (mod(coeffraf,2) == 0) diffmod = 1
!
!        ipos = i1
!
!        Do iparent = locind_parent_left,locind_parent_last
!             pos=1
!
!            delta0=1./(epsilon+smooth(iparent  ))**3
!            delta1=1./(epsilon+smooth(iparent+1))**3
!            delta2=1./(epsilon+smooth(iparent+2))**3
!
!             Do jj = ipos - coeffraf/2+diffmod,ipos + coeffraf/2
!
!               pos = pos+1
!             End do
!             ipos = ipos + coeffraf
!
!        End do
!
!
!        y(1:nc)=ytemp(1:nc)
!        deallocate(ytemp)
!        deallocate(diff, diff2, diff3)
!
!        deallocate(ak,ck)
!
!      Return
!      End subroutine weno1dnew
!===================================================================================================
!
!===================================================================================================
!  subroutine WENO1d
!
!> Carries out a a 1D interpolation using WENO method on a child grid (vector y) from its parent 
!! grid (vector x).
!---------------------------------------------------------------------------------------------------
subroutine WENO1d ( x, y, np, nc, s_parent, s_child, ds_parent, ds_child )
!---------------------------------------------------------------------------------------------------
    integer,             intent(in)     :: np, nc
    real, dimension(np), intent(in)     :: x
    real, dimension(nc), intent(out)    :: y
    real,                intent(in)     :: s_parent, s_child
    real,                intent(in)     :: ds_parent, ds_child
!
    real, dimension(:), allocatable :: ytemp
    integer :: i,coeffraf,locind_parent_left,locind_parent_last
    integer :: iparent,ipos,pos,nmin,nmax
    real    :: ypos
    integer :: i1,jj
    real :: xpmin
!
    real, dimension(np) :: slope
    real, dimension(:), allocatable  :: diff
    integer :: diffmod
    real :: invcoeffraf
    real :: delta0, delta1, sumdelta
    real, parameter :: epsilon = 1.d-8
!
    coeffraf = nint(ds_parent/ds_child)
!
    if (coeffraf == 1) then
        locind_parent_left = 1 + nint((s_child - s_parent)/ds_parent)
        y(1:nc) = x(locind_parent_left:locind_parent_left+nc-1)
        return
    endif
!
    invcoeffraf = ds_child/ds_parent
!
    allocate(ytemp(-2*coeffraf:nc+2*coeffraf))
    ypos = s_child
!
    locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent)
    locind_parent_last = 1 + agrif_ceiling((ypos +(nc - 1) *ds_child - s_parent)/ds_parent)
!
    xpmin = s_parent + (locind_parent_left-1)*ds_parent
    i1 = 1+agrif_int((xpmin-s_child)/ds_child)
!
    allocate(diff(coeffraf))
    diff(1) = 0.5*invcoeffraf
    do i = 2,coeffraf
        diff(i) = diff(i-1)+invcoeffraf
    enddo
!
    if ( locind_parent_last+2 <= np ) then
        nmax = locind_parent_last+2
    else if ( locind_parent_last+1 <= np ) then
        nmax = locind_parent_last+1
      else
        nmax = locind_parent_last
    endif
!
    if(locind_parent_left-1 >= 1) then
        nmin = locind_parent_left-1
    else
        nmin = locind_parent_left
    endif
!
    do i = nmin+1,nmax
        slope(i) = x(i) - x(i-1)
    enddo
!
    diffmod = 0
    if (mod(coeffraf,2) == 0) diffmod = 1
!
    ipos = i1
!
    do iparent = locind_parent_left,locind_parent_last
        pos=1
        delta0 = 1./(epsilon+slope(iparent  )**2)**2
        delta1 = 1./(epsilon+slope(iparent+1)**2)**2
        sumdelta = 1./(delta0+delta1)
        do jj = ipos - coeffraf/2+diffmod,ipos + coeffraf/2
            ytemp(jj) = x(iparent)+(diff(pos)-0.5)*( delta0*slope(iparent) + &
                                                     delta1*slope(iparent+1))*sumdelta
            pos = pos+1
        enddo
        ipos = ipos + coeffraf
    enddo
!
    y(1:nc) = ytemp(1:nc)
    deallocate(ytemp)
    deallocate(diff)
!---------------------------------------------------------------------------------------------------
end subroutine WENO1d
!===================================================================================================
!
!===================================================================================================
!  subroutine ENO1d
!
!> Carries out a 1D interpolation using piecewise polynomial ENO reconstruction technique
!! on a child grid (vector y) from its parent grid (vector x).
!! \see ---- p 163-164 Computational gasdynamics ----
!---------------------------------------------------------------------------------------------------
subroutine ENO1d ( x, y, np, nc, s_parent, s_child, ds_parent, ds_child )
!---------------------------------------------------------------------------------------------------
    integer,             intent(in)     :: np, nc
    real, dimension(np), intent(in)     :: x
    real, dimension(nc), intent(out)    :: y
    real,                intent(in)     :: s_parent, s_child
    real,                intent(in)     :: ds_parent, ds_child
!
    integer :: i,coeffraf,locind_parent_left,locind_parent_last
    integer :: ipos, pos
    real    :: ypos,xi
    integer :: i1,jj
    real :: xpmin
!
    real, dimension(:),   allocatable  :: ytemp
    real, dimension(:,:), allocatable  :: xbar
    real, dimension(1:np+1)            :: xhalf
    real, dimension(3,np)              :: dd, c
    integer :: diffmod, left
!
    coeffraf = nint(ds_parent/ds_child)
!
    if (coeffraf == 1) then
        locind_parent_left = 1 + nint((s_child - s_parent)/ds_parent)
        y(1:nc) = x(locind_parent_left:locind_parent_left+nc-1)
        return
    end if

    diffmod = 0
    if (mod(coeffraf,2) == 0) diffmod = 1
!
    allocate(ytemp(-2*coeffraf:nc+2*coeffraf))
    ypos = s_child
!
    locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent)
    locind_parent_last = 1 + agrif_ceiling((ypos +(nc - 1) *ds_child - s_parent)/ds_parent)
!
    xpmin = s_parent + (locind_parent_left-1)*ds_parent
    i1 = 1+agrif_int((xpmin-s_child)/ds_child)
!
    do i = 1,np+1
        xhalf(i) = i - 0.5
    enddo
!
! Compute divided differences
!
    dd(1,1:np)   = x(1:np)
    dd(2,1:np-1) = 0.5*( dd(1,2:np) - dd(1,1:np-1) )
    dd(3,1:np-2) = (1./3.)*( dd(2,2:np-1) - dd(2,1:np-2) )
!
    allocate( xbar( coeffraf,2 ) )
    xi = 0.5
    do i = 1,coeffraf
        xbar(i,1) = (i-1)*ds_child/ds_parent - xi
        xbar(i,2) =  i   *ds_child/ds_parent - xi
    enddo
!
    ipos = i1
!
    do i = locind_parent_left,locind_parent_last
        left = i
        do jj = 2,3
            if(abs(dd(jj,left)) > abs(dd(jj,left-1))) left = left-1
        enddo
!
!       convert to Taylor series form
        call taylor(i,xhalf(left:left+2),dd(1:3,left),c(1:3,i))
    enddo
!
! Evaluate the reconstruction on each cell
!
    do i = locind_parent_left,locind_parent_last
!
        pos = 1
!
        do jj = ipos - coeffraf/2+diffmod,ipos + coeffraf/2
            ytemp(jj) = ( c(1,i)*(xbar(pos,2)-xbar(pos,1))              &
                        + c(2,i)*(xbar(pos,2)*xbar(pos,2) -             &
                                  xbar(pos,1)*xbar(pos,1))              &
                        + c(3,i)*(xbar(pos,2)*xbar(pos,2)*xbar(pos,2) - &
                                  xbar(pos,1)*xbar(pos,1)*xbar(pos,1))  &
                        ) * coeffraf
            pos = pos+1
        enddo
        ipos = ipos + coeffraf
!
    enddo
!
    y(1:nc) = ytemp(1:nc)
    deallocate(ytemp,xbar)
!---------------------------------------------------------------------------------------------------
end subroutine ENO1d
!===================================================================================================
!
!     **************************************************************************  
!CC   Subroutine ppm1d_lim
!     ************************************************************************** 
! 
      Subroutine ppm1d_lim(x,y,np,nc,s_parent,s_child,ds_parent,ds_child) 
!
!CC   Description:
!CC   Subroutine to do a 1D interpolation and apply monotonicity constraints
!CC   using piecewise parabolic method  
!CC   on a child grid (vector y) from its parent grid (vector x).
!C    Method:
!
!     Declarations:
!
      Implicit none
!        
!     Arguments
      Integer             :: np,nc      
      Real, Dimension(np) :: x      
      Real, Dimension(nc) :: y
      Real, Dimension(:),Allocatable :: ytemp
      Real                :: s_parent,s_child,ds_parent,ds_child
!
!     Local scalars
      Integer :: i,coeffraf,locind_parent_left,locind_parent_last
      Integer :: iparent,ipos,pos,nmin,nmax
      Real    :: ypos
      integer :: i1,jj
      Real :: xpmin,cavg,a,b
!      
      Real :: xrmin,xrmax,am3,s2,s1  
      Real, Dimension(np) :: dela,xr,xl,delta,a6,slope,slope2
      Real, Dimension(:),Allocatable  :: diff,diff2,diff3    
      INTEGER :: diffmod
!      
      coeffraf = nint(ds_parent/ds_child)
!
      If (coeffraf == 1) Then
          locind_parent_left = 1 + nint((s_child - s_parent)/ds_parent)
          y(1:nc) = x(locind_parent_left:locind_parent_left+nc-1)
          return
      End If
!      
      Allocate(ytemp(-2*coeffraf:nc+2*coeffraf))
      ypos = s_child  
!
      locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent) 
      locind_parent_last = 1 + &
           agrif_ceiling((ypos +(nc - 1)  &
           *ds_child - s_parent)/ds_parent)  
!
      xpmin = s_parent + (locind_parent_left-1)*ds_parent       
      i1 = 1+agrif_int((xpmin-s_child)/ds_child)        
!     
      Allocate( diff(coeffraf),diff2(coeffraf),diff3(coeffraf) )
!      
         diff(:) = ds_child/ds_parent
!      
      Do i=1,coeffraf
         a = real(i-1)*ds_child/ds_parent
         b = real(i)*ds_child/ds_parent
         diff2(i) = 0.5*(b*b - a*a)  
         diff3(i) = (1./3.)*(b*b*b - a*a*a)
      End do
!
      if( locind_parent_last+2 <= np ) then
           nmax = locind_parent_last+2    
      else if( locind_parent_last+1 <= np ) then
           nmax = locind_parent_last+1
      else
           nmax = locind_parent_last 
      endif     
!      
      if(locind_parent_left-1 >= 1) then
          nmin = locind_parent_left-1
      else 
          nmin = locind_parent_left
      endif    
! 
      Do i = nmin,nmax
         slope(i) = x(i) - x(i-1)
         slope2(i) = 2.*abs(slope(i))
      Enddo
!
      Do i = nmin,nmax-1
         dela(i) = 0.5 * ( slope(i) + slope(i+1) )
! Van Leer slope limiter
         dela(i) = min( abs(dela(i)),slope2(i), &
                       slope2(i+1) )*sign(1.,dela(i))
         IF( slope(i)*slope(i+1) <= 0. ) dela(i) = 0.
      Enddo
!
      Do i = nmin,nmax-2
         xr(i) = x(i) + (1./2.)*slope(i+1) + (-1./6.)*dela(i+1) &
                                          + ( 1./6. )*dela(i)
      Enddo
!
      Do i = nmin,nmax-2
         xrmin = min(x(i),x(i+1))
         xrmax = max(x(i),x(i+1))
         xr(i) = min(xr(i),xrmax)
         xr(i) = max(xr(i),xrmin)
         xl(i+1) = xr(i)         
      Enddo
! apply parabolic monotonicity
       Do i = locind_parent_left,locind_parent_last
          If( ( (xr(i)-x(i))* (x(i)-xl(i)) ) .le. 0. ) then
             xl(i) = x(i) 
             xr(i) = x(i)
          Endif          
          delta(i) = xr(i) - xl(i)
          am3 = 3. * x(i)
          s1  = am3 - 2. * xr(i)
          s2  = am3 - 2. * xl(i)
          IF( delta(i) * (xl(i) - s1) .le. 0. ) xl(i) = s1
          IF( delta(i) * (s2 - xr(i)) .le. 0. ) xr(i) = s2
          delta(i) = xr(i) - xl(i)
          a6(i) = 6.*x(i)-3.*(xl(i) +xr(i))
!
       End do   
!
        diffmod = 0
       IF (mod(coeffraf,2) == 0) diffmod = 1           
!
        ipos = i1
!               
        Do iparent = locind_parent_left,locind_parent_last       
             pos=1
             cavg = 0.
             Do jj = ipos - coeffraf/2+diffmod,ipos + coeffraf/2
!
               ytemp(jj) = (diff(pos)*xl(iparent)   &
                  + diff2(pos) &
                  *  (delta(iparent)+a6(iparent)) &
                  - diff3(pos)*a6(iparent))*coeffraf
                              
               cavg = cavg + ytemp(jj)
               pos = pos+1 
             End do 
             ipos = ipos + coeffraf
!
        End do     
!
!
        y(1:nc)=ytemp(1:nc)                                 
        deallocate(ytemp)                
        deallocate(diff, diff2, diff3)
      Return
      End Subroutine ppm1d_lim
!===================================================================================================
!  subroutine taylor
!---------------------------------------------------------------------------------------------------
subroutine taylor ( ind, xhalf, dd, c )
!---------------------------------------------------------------------------------------------------
    integer,            intent(in)  :: ind
    real, dimension(3), intent(in)  :: xhalf
    real, dimension(3), intent(in)  :: dd
    real, dimension(3), intent(out) :: c
!
    real, dimension(0:3,0:3) :: d
    integer                  :: i, j
!
    d(0,0:3) = 1.0
    do i = 1,3
        d(i,0) = (ind-xhalf(i))*d(i-1,0)
    enddo
!
    do i = 1,3
        do j = 1,3-i
            d(i,j) = d(i,j-1) + (ind-xhalf(i+j))*d(i-1,j)
        enddo
    enddo
!
    do j = 1,3
        c(j) = 0.
        do i=0,3-j
            c(j) = c(j) + d(i,j)*dd(i+j)
        enddo
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine taylor
!===================================================================================================
!
!===================================================================================================
!  function Agrif_limiter_vanleer
!---------------------------------------------------------------------------------------------------
real function Agrif_limiter_vanleer ( tab ) result(res)
!---------------------------------------------------------------------------------------------------
    real, dimension(3), intent(in) :: tab
!
    real :: p1, p2, p3

    p1 =    (tab(3)-tab(1))/2.
    p2 = 2.*(tab(2)-tab(1))
    p3 = 2.*(tab(3)-tab(2))

    if ((p1>0.).AND.(p2>0.).AND.(p3>0)) then
        res = minval((/p1,p2,p3/))
    elseif ((p1<0.).AND.(p2<0.).AND.(p3<0)) then
        res = maxval((/p1,p2,p3/))
    else
        res=0.
    endif
!---------------------------------------------------------------------------------------------------
end function Agrif_limiter_vanleer
!===================================================================================================
!
end module Agrif_InterpBasic
