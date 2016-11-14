!
! $Id: modupdatebasic.F 779 2007-12-22 17:04:17Z rblod $
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
!     Foundation, Inc., 59 Temple Place -  Suite 330, Boston, MA 02111-1307, USA.
!
!
!
!> Module containing different procedures of update (copy, average, full_weighting)
!! used in the #Agrif_Update module.
!===================================================================================================
!
module Agrif_UpdateBasic
!
    use Agrif_Types

    implicit none

    integer, dimension(:,:), allocatable :: indchildcopy
    integer, dimension(:,:), allocatable :: indchildaverage
!
contains
!
!===================================================================================================
!  subroutine Agrif_basicupdate_copy1d
!
!> Carries out a copy on a parent grid (vector x) from its child grid (vector y).
!---------------------------------------------------------------------------------------------------
subroutine Agrif_basicupdate_copy1d ( x, y, np, nc, s_parent, s_child, ds_parent, ds_child )
!---------------------------------------------------------------------------------------------------
    real, dimension(np), intent(out)    :: x            !< Coarse output data to parent
    real, dimension(nc), intent(in)     :: y            !< Fine input data from child
    integer,             intent(in)     :: np           !< Length of parent array
    integer,             intent(in)     :: nc           !< Length of child  array
    real,                intent(in)     :: s_parent     !< Parent grid position (s_root = 0)
    real,                intent(in)     :: s_child      !< Child  grid position (s_root = 0)
    real,                intent(in)     :: ds_parent    !< Parent grid dx (ds_root = 1)
    real,                intent(in)     :: ds_child     !< Child  grid dx (ds_root = 1)
!---------------------------------------------------------------------------------------------------
    integer :: i, locind_child_left, coeffraf
!
    coeffraf = nint(ds_parent/ds_child)
    locind_child_left = 1 + nint((s_parent - s_child)/ds_child)
!
    if ( coeffraf == 1 ) then
!CDIR ALTCODE
        x(1:np) = y(locind_child_left:locind_child_left+np-1)
        return
    endif
!
!CDIR ALTCODE
    do i = 1,np
        x(i) = y(locind_child_left)
        locind_child_left = locind_child_left + coeffraf
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_basicupdate_copy1d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_basicupdate_copy1d_before
!
!> Precomputes index for a copy on a parent grid (vector x) from its child grid (vector y).
!---------------------------------------------------------------------------------------------------
subroutine Agrif_basicupdate_copy1d_before ( nc2, np, nc, s_parent, s_child, ds_parent, ds_child, dir )
!---------------------------------------------------------------------------------------------------
    integer,             intent(in)     :: nc2          !< Length of parent array
    integer,             intent(in)     :: np           !< Length of parent array
    integer,             intent(in)     :: nc           !< Length of child  array
    real,                intent(in)     :: s_parent     !< Parent grid position (s_root = 0)
    real,                intent(in)     :: s_child      !< Child  grid position (s_root = 0)
    real,                intent(in)     :: ds_parent    !< Parent grid dx (ds_root = 1)
    real,                intent(in)     :: ds_child     !< Child  grid dx (ds_root = 1)
    integer,             intent(in)     :: dir          !< Direction
!---------------------------------------------------------------------------------------------------
    integer, dimension(:,:), allocatable    :: indchildcopy_tmp
    integer :: i, n_old, locind_child_left, coeffraf
!
    coeffraf = nint(ds_parent/ds_child)
!
    locind_child_left = 1 + nint((s_parent - s_child)/ds_child)

    if ( .not.allocated(indchildcopy) ) then
        allocate(indchildcopy(np*nc2, 3))
    else
        n_old = size(indchildcopy,1)
        if ( n_old < np*nc2 ) then
            allocate( indchildcopy_tmp(1:n_old, 3))
            indchildcopy_tmp = indchildcopy
            deallocate(indchildcopy)
            allocate(indchildcopy(np*nc2, 3))
            indchildcopy(1:n_old,:) = indchildcopy_tmp
            deallocate(indchildcopy_tmp)
        endif
    endif
!
    do i = 1,np
        indchildcopy(i,dir) = locind_child_left
        locind_child_left = locind_child_left + coeffraf
    enddo
!
    do i = 2,nc2
        indchildcopy(1+(i-1)*np:i*np,dir) = indchildcopy(1+(i-2)*np:(i-1)*np,dir) + nc
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_basicupdate_copy1d_before
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_basicupdate_copy1d_after
!
!> Carries out a copy on a parent grid (vector x) from its child grid (vector y)
!! using precomputed index.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_basicupdate_copy1d_after ( x, y, np, nc, dir )
!---------------------------------------------------------------------------------------------------
    real, dimension(np), intent(out)    :: x            !< Coarse output data to parent
    real, dimension(nc), intent(in)     :: y            !< Fine input data from child
    integer,             intent(in)     :: np           !< Length of parent array
    integer,             intent(in)     :: nc           !< Length of child  array
    integer,             intent(in)     :: dir          !< Direction
!---------------------------------------------------------------------------------------------------
    integer :: i
!
!CDIR ALTCODE
    do i = 1,np
        x(i) = y(indchildcopy(i,dir))
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_basicupdate_copy1d_after
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_basicupdate_average1d
!
!> Carries out an update by average on a parent grid (vector x)from its child grid (vector y).
!---------------------------------------------------------------------------------------------------
subroutine Agrif_basicupdate_average1d ( x, y, np, nc, s_parent, s_child, ds_parent, ds_child )
!---------------------------------------------------------------------------------------------------
    REAL, DIMENSION(np), intent(out)    :: x
    REAL, DIMENSION(nc), intent(in)     :: y
    INTEGER,             intent(in)     :: np,nc
    REAL,                intent(in)     :: s_parent,  s_child
    REAL,                intent(in)     :: ds_parent, ds_child
!
    INTEGER :: i, ii, locind_child_left, coeffraf
    REAL    :: xpos, invcoeffraf
    INTEGER :: nbnonnuls
    INTEGER :: diffmod
!
    coeffraf = nint(ds_parent/ds_child)
    invcoeffraf = 1./coeffraf
!
    if (coeffraf == 1) then
        locind_child_left = 1 + nint((s_parent - s_child)/ds_child)
        x(1:np) = y(locind_child_left:locind_child_left+np-1)
        return
    endif
!
    xpos = s_parent
    x = 0.
!
    diffmod = 0
!
    IF ( mod(coeffraf,2) == 0 ) diffmod = 1
!
    locind_child_left = 1 + agrif_int((xpos - s_child)/ds_child)
!
    IF (Agrif_UseSpecialValueInUpdate) THEN
        do i = 1,np
            nbnonnuls = 0
!CDIR NOVECTOR
            do ii = -coeffraf/2+locind_child_left+diffmod, &
                     coeffraf/2+locind_child_left
                IF (y(ii) /= Agrif_SpecialValueFineGrid) THEN
!                    nbnonnuls = nbnonnuls + 1
                    x(i) = x(i) + y(ii)
                ENDIF
            enddo
!            IF (nbnonnuls /= 0) THEN
!                x(i) = x(i)/nbnonnuls
!            ELSE
!                x(i) = Agrif_SpecialValueFineGrid
!            ENDIF
            locind_child_left = locind_child_left + coeffraf
        enddo
    ELSE
!
!CDIR ALTCODE
        do i = 1,np
!CDIR NOVECTOR
            do ii = -coeffraf/2+locind_child_left+diffmod, &
                     coeffraf/2+locind_child_left
                x(i) = x(i) + y(ii)
            enddo
!            x(i) = x(i)*invcoeffraf
            locind_child_left = locind_child_left + coeffraf
        enddo
        IF (.not.Agrif_Update_Weights) THEN
           x = x * invcoeffraf
        ENDIF
    ENDIF
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_basicupdate_average1d
!===================================================================================================
!
!===================================================================================================
!  subroutine Average1dPrecompute
!
!> Carries out an update by average on a parent grid (vector x)from its child grid (vector y).
!---------------------------------------------------------------------------------------------------
subroutine Average1dPrecompute ( nc2, np, nc, s_parent, s_child, ds_parent, ds_child, dir )
!---------------------------------------------------------------------------------------------------
    INTEGER, intent(in) :: nc2, np, nc
    REAL,    intent(in) :: s_parent,  s_child
    REAL,    intent(in) :: ds_parent, ds_child
    INTEGER, intent(in) :: dir
!
    INTEGER, DIMENSION(:,:), ALLOCATABLE :: indchildaverage_tmp
    INTEGER :: i, locind_child_left, coeffraf
    REAL    :: xpos
    INTEGER :: diffmod
!
    coeffraf = nint(ds_parent/ds_child)
    xpos = s_parent
    diffmod = 0
!
    IF ( mod(coeffraf,2) == 0 ) diffmod = 1
!
    locind_child_left = 1 + agrif_int((xpos - s_child)/ds_child)
!
    if (.not.allocated(indchildaverage)) then
        allocate(indchildaverage(np*nc2,3))
    else
        if (size(indchildaverage,1)<np*nc2) then
            allocate( indchildaverage_tmp(size(indchildaverage,1),size(indchildaverage,2)))
            indchildaverage_tmp = indchildaverage
            deallocate(indchildaverage)
            allocate(indchildaverage(np*nc2,3))
            indchildaverage(1:size(indchildaverage_tmp,1),1:size(indchildaverage_tmp,2)) = indchildaverage_tmp
            deallocate(indchildaverage_tmp)
        endif
    endif
!
    do i = 1,np
        indchildaverage(i,dir)= -coeffraf/2+locind_child_left+diffmod
        locind_child_left = locind_child_left + coeffraf
    enddo
!
    do i = 2,nc2
        indchildaverage(1+(i-1)*np:i*np,dir) = indchildaverage(1+(i-2)*np:(i-1)*np,dir) + nc
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Average1dPrecompute
!===================================================================================================
!
!===================================================================================================
!  subroutine Average1dAfterCompute
!
!> Carries out an update by average on a parent grid (vector x) from its child grid (vector y).
!---------------------------------------------------------------------------------------------------
subroutine Average1dAfterCompute ( x, y, np, nc, s_parent, s_child, ds_parent, ds_child, dir )
!---------------------------------------------------------------------------------------------------
    REAL, DIMENSION(np), intent(inout)  :: x
    REAL, DIMENSION(nc), intent(in)     :: y
    INTEGER,             intent(in)     :: np, nc
    REAL,                intent(in)     :: s_parent,  s_child
    REAL,                intent(in)     :: ds_parent, ds_child
    INTEGER,             intent(in)     :: dir
!
    REAL    :: invcoeffraf
    INTEGER :: i, j, coeffraf
    INTEGER, DIMENSION(np) :: nbnonnuls
    REAL, DIMENSION(0:7), parameter :: invcoeff = (/1.,1.,0.5,1./3.,0.25,0.2,1./6.,1./7./)
!
    coeffraf = nint(ds_parent/ds_child)
    invcoeffraf = 1./coeffraf
!
    IF (Agrif_UseSpecialValueInUpdate) THEN
!
!        nbnonnuls = 0
        do  j = 1,coeffraf
            do i = 1,np
                IF (y(indchildaverage(i,dir) + j -1) /= Agrif_SpecialValueFineGrid) THEN
!                    nbnonnuls(i) = nbnonnuls(i) + 1
                    x(i) = x(i) +  y(indchildaverage(i,dir) + j-1 )
                ENDIF
            enddo
        enddo
        do i=1,np
  !          x(i) = x(i)*invcoeff(nbnonnuls(i))
  !          if (nbnonnuls(i) == 0) x(i) = Agrif_SpecialValueFineGrid
        enddo
!
    ELSE
!
!CDIR NOLOOPCHG
        do  j = 1,coeffraf
!CDIR VECTOR
            do i= 1,np
                x(i) = x(i) + y(indchildaverage(i,dir) + j-1 )
            enddo
        enddo
        IF (.not.Agrif_Update_Weights) THEN
           x = x * invcoeffraf
        ENDIF
!
    ENDIF

!---------------------------------------------------------------------------------------------------
end subroutine Average1dAfterCompute
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_basicupdate_full_weighting1D
!
!> Carries out an update by full_weighting on a parent grid (vector x) from its child grid (vector y).
!---------------------------------------------------------------------------------------------------
subroutine Agrif_basicupdate_full_weighting1D ( x, y, np, nc, s_parent, s_child, ds_parent, ds_child )
!---------------------------------------------------------------------------------------------------
    real, dimension(np), intent(out)    :: x
    real, dimension(nc), intent(in)     :: y
    integer,             intent(in)     :: np, nc
    real,                intent(in)     :: s_parent,  s_child
    real,                intent(in)     :: ds_parent, ds_child
!---------------------------------------------------------------------------------------------------
    REAL    :: xpos, xposfin
    INTEGER :: i, ii, diffmod
    INTEGER :: it1, it2
    INTEGER :: i1,  i2
    INTEGER :: coeffraf
    INTEGER :: locind_child_left
    REAL    :: sumweight, invsumweight
    REAL    :: weights(-Agrif_MaxRaff:Agrif_MaxRaff)

    coeffraf = nint(ds_parent/ds_child)
    locind_child_left = 1 + agrif_int((s_parent-s_child)/ds_child)
!
    if (coeffraf == 1) then
        x(1:np) = y(locind_child_left:locind_child_left+np-1)
        return
    endif
!
    xpos = s_parent
    x = 0.
!
    xposfin = s_child + ds_child * (locind_child_left - 1)
    IF (abs(xposfin - xpos) < 0.001) THEN
        diffmod = 0
    ELSE
        diffmod = 1
    ENDIF
!
    if (diffmod == 1) THEN
        invsumweight=1./(2.*coeffraf**2)
        do i = -coeffraf,-1
            weights(i) = invsumweight*(2*(coeffraf+i)+1)
        enddo
        do i = 0,coeffraf-1
            weights(i) = weights(-(i+1))
        enddo
        it1 = -coeffraf
        i1 = -(coeffraf-1)+locind_child_left
        i2 = 2*coeffraf - 1
        
    else
        invsumweight=1./coeffraf**2
        do i = -(coeffraf-1),0
            weights(i) = invsumweight*(coeffraf + i)
        enddo
        do i=1,coeffraf-1
            weights(i) = invsumweight*(coeffraf - i)
        enddo
        it1 = -(coeffraf-1)
        i1 = -(coeffraf-1)+locind_child_left
        i2 = 2*coeffraf - 2

    endif
!
    sumweight = 0.
    MYLOOP : do i = 1,np
!
        it2 = it1

!    sumweight = 0.
    
        do ii = i1,i1+i2
!
            IF (Agrif_UseSpecialValueInUpdate) THEN
                IF (y(ii) /= Agrif_SpecialValueFineGrid) THEN
                    x(i) = x(i) + weights(it2)*y(ii)
!                    sumweight = sumweight+weights(it2)
                ELSE
                    x(i) = Agrif_SpecialValueFineGrid
                    i1=i1+coeffraf
                    CYCLE MYLOOP
                ENDIF
            ELSE
                x(i) = x(i) + weights(it2)*y(ii)
            ENDIF

            it2 = it2+1
!
        enddo
!
        i1 = i1 + coeffraf
!
        enddo MYLOOP
        
        IF (Agrif_UseSpecialValueInUpdate) THEN
          x = x * coeffraf ! x will be divided by coeffraf later in modupdate.F90
        ENDIF
        
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_basicupdate_full_weighting1D
!===================================================================================================
!
end module Agrif_UpdateBasic
