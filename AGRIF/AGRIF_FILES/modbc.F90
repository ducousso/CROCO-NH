!
! $Id: modbc.F 779 2007-12-22 17:04:17Z rblod $
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
!> Module Agrif_Boundary.
!>
!> Contains subroutines to calculate the boundary conditions on the child grids from their
!> parent grids.
!
module Agrif_Boundary
!
    use Agrif_Interpolation
!
    implicit none
!
contains
!
!===================================================================================================
!  subroutine Agrif_CorrectVariable
!
!> subroutine to calculate the boundary conditions on a fine grid
!---------------------------------------------------------------------------------------------------
subroutine Agrif_CorrectVariable ( parent, child, pweight, weight, procname )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variable), pointer       :: parent       !< Variable on the parent grid
    type(Agrif_Variable), pointer       :: child        !< Variable on the child grid
    logical                             :: pweight      !< Indicates if weight is used for the time interpolation
    real                                :: weight       !< Coefficient for the time interpolation
    procedure()                         :: procname     !< Data recovery procedure
!
    type(Agrif_Grid)    , pointer :: Agrif_Child_Gr, Agrif_Parent_Gr
    type(Agrif_Variable), pointer :: root_var   ! Variable on the root grid
    integer                :: nbdim  ! Number of dimensions of the grid variable
    integer                :: n
    integer, dimension(6)  :: lb_child       ! Index of the first point inside the domain for
                                                !    the child grid variable
    integer, dimension(6)  :: lb_parent      ! Index of the first point inside the domain for
                                                !    the parent grid variable
    integer, dimension(6)  :: ub_child     !  Upper bound on the child grid
    integer, dimension(6)  :: nb_child       ! Number of cells for child
    integer, dimension(6)  :: posvartab_child   ! Position of the variable on the cell
    integer, dimension(6)  :: loctab_child      ! Indicates if the child grid has a common border
                                                !    with the root grid
    real, dimension(6)     :: s_child, s_parent   ! Positions of the parent and child grids
    real, dimension(6)     :: ds_child, ds_parent ! Space steps of the parent and child grids
!
    call PreProcessToInterpOrUpdate( parent,   child,       &
                                     nb_child, ub_child,    &
                                     lb_child, lb_parent,   &
                                      s_child,  s_parent,   &
                                     ds_child, ds_parent, nbdim, interp=.true.)
    root_var => child % root_var
    Agrif_Child_Gr => Agrif_Curgrid
    Agrif_Parent_Gr => Agrif_Curgrid % parent
!
    loctab_child(:) = 0
    posvartab_child(1:nbdim) = root_var % posvar(1:nbdim)
!
    do n = 1,nbdim
!
        select case(root_var % interptab(n))
!
        case('x') ! x DIMENSION
!
            if (Agrif_Curgrid % NearRootBorder(1))      loctab_child(n) = -1
            if (Agrif_Curgrid % DistantRootBorder(1))   loctab_child(n) = -2
            if ((Agrif_Curgrid % NearRootBorder(1)) .AND. &
                (Agrif_Curgrid % DistantRootBorder(1))) loctab_child(n) = -3
!
        case('y') ! y DIMENSION
!
            if (Agrif_Curgrid % NearRootBorder(2))      loctab_child(n) = -1
            if (Agrif_Curgrid % DistantRootBorder(2))   loctab_child(n) = -2
            if ((Agrif_Curgrid % NearRootBorder(2)) .AND. &
                (Agrif_Curgrid % DistantRootBorder(2))) loctab_child(n) = -3
!
        case('z') ! z DIMENSION
!
            if (Agrif_Curgrid % NearRootBorder(3))      loctab_child(n) = -1
            if (Agrif_Curgrid % DistantRootBorder(3))   loctab_child(n) = -2
            if ((Agrif_Curgrid % NearRootBorder(3)) .AND. &
                (Agrif_Curgrid % DistantRootBorder(3))) loctab_child(n) = -3
!
        case('N') ! No space DIMENSION
!
            posvartab_child(n) = 1
            loctab_child(n) = -3
!
        end select
!
    enddo
!
    call Agrif_Correctnd(parent, child, pweight, weight,                    &
                         lb_child(1:nbdim), lb_parent(1:nbdim),             &
                         nb_child(1:nbdim), posvartab_child(1:nbdim),       &
                         loctab_child(1:nbdim),                             &
                         s_child(1:nbdim), s_parent(1:nbdim),               &
                         ds_child(1:nbdim),ds_parent(1:nbdim), nbdim, procname )
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_CorrectVariable
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Correctnd
!
!> calculates the boundary conditions for a nD grid variable on a fine grid by using
!> a space and time interpolations; it is called by the #Agrif_CorrectVariable procedure
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Correctnd ( parent, child, pweight, weight,                        &
                             pttab_child, pttab_Parent,                             &
                             nbtab_Child, posvartab_Child, loctab_Child,            &
                             s_Child, s_Parent, ds_Child, ds_Parent,                &
                             nbdim, procname )
!---------------------------------------------------------------------------------------------------
#if defined AGRIF_MPI
    include 'mpif.h'
#endif
!
    TYPE(Agrif_Variable), pointer       :: parent       !< Variable on the parent grid
    TYPE(Agrif_Variable), pointer       :: child        !< Variable on the child grid
    LOGICAL                             :: pweight      !< Indicates if weight is used for the temporal interpolation
    REAL                                :: weight       !< Coefficient for the temporal interpolation
    INTEGER, DIMENSION(nbdim)   :: pttab_child          !< Index of the first point inside the domain for the parent grid variable
    INTEGER, DIMENSION(nbdim)   :: pttab_Parent         !< Index of the first point inside the domain for the child  grid variable
    INTEGER, DIMENSION(nbdim)   :: nbtab_Child          !< Number of cells of the child grid
    INTEGER, DIMENSION(nbdim)   :: posvartab_Child      !< Position of the grid variable (1 or 2)
    INTEGER, DIMENSION(nbdim)   :: loctab_Child         !< Indicates if the child grid has a common border with the root grid
    REAL   , DIMENSION(nbdim)   :: s_Child,  s_Parent   !< Positions of the parent and child grids
    REAL   , DIMENSION(nbdim)   :: ds_Child, ds_Parent  !< Space steps of the parent and child grids
    INTEGER                             :: nbdim        !< Number of dimensions of the grid variable
    procedure()                         :: procname     !< Data recovery procedure
!
    INTEGER,DIMENSION(6)                :: type_interp     ! Type of interpolation (linear, spline,...)
    INTEGER,DIMENSION(6,6)              :: type_interp_bc  ! Type of interpolation (linear, spline,...)
    INTEGER,DIMENSION(nbdim,2,2)        :: childarray
    INTEGER,DIMENSION(nbdim,2)          :: lubglob
    INTEGER                             :: kindex       ! Index used for safeguard and time interpolation
    INTEGER,DIMENSION(nbdim,2,2)        :: indtab       ! Arrays indicating the limits of the child
    INTEGER,DIMENSION(nbdim,2,2)        :: indtruetab   ! grid variable where boundary conditions are
    INTEGER,DIMENSION(nbdim,2,2,nbdim)  :: ptres,ptres2 ! calculated
    INTEGER,DIMENSION(nbdim)            :: coords
    INTEGER                             :: i, nb, ndir
    INTEGER                             :: n, sizetab
    INTEGER                             :: ibeg, iend
    INTEGER                             :: i1,i2,j1,j2,k1,k2,l1,l2,m1,m2,n1,n2
    REAL                                :: c1t,c2t      ! Coefficients for the time interpolation (c2t=1-c1t)
#if defined AGRIF_MPI
!
    INTEGER, DIMENSION(nbdim)   :: lower, upper
    INTEGER, DIMENSION(nbdim)   :: ltab, utab
!
#endif
!
    type_interp_bc = child % root_var % type_interp_bc
    coords         = child % root_var % coords
!
    ibeg = child % bcinf
    iend = child % bcsup
!
    indtab(1:nbdim,2,1) = pttab_child(1:nbdim) + nbtab_child(1:nbdim) + ibeg
    indtab(1:nbdim,2,2) = indtab(1:nbdim,2,1) + ( iend - ibeg )

    indtab(1:nbdim,1,1) = pttab_child(1:nbdim) - iend
    indtab(1:nbdim,1,2) = pttab_child(1:nbdim) - ibeg

    WHERE (posvartab_child(1:nbdim) == 2)
        indtab(1:nbdim,1,1) = indtab(1:nbdim,1,1) - 1
        indtab(1:nbdim,1,2) = indtab(1:nbdim,1,2) - 1
    END WHERE
!
    call Agrif_get_var_global_bounds(child,lubglob,nbdim)
!
    indtruetab(1:nbdim,1,1) = max(indtab(1:nbdim,1,1), lubglob(1:nbdim,1))
    indtruetab(1:nbdim,1,2) = max(indtab(1:nbdim,1,2), lubglob(1:nbdim,1))
    indtruetab(1:nbdim,2,1) = min(indtab(1:nbdim,2,1), lubglob(1:nbdim,2))
    indtruetab(1:nbdim,2,2) = min(indtab(1:nbdim,2,2), lubglob(1:nbdim,2))
!
    do nb = 1,nbdim
        do ndir = 1,2
!
            if (loctab_child(nb) /= (-ndir) .AND. loctab_child(nb) /= -3) then
!
                do n = 1,2
                    ptres(nb,n,ndir,nb) = indtruetab(nb,ndir,n)
                enddo
!
                do i = 1,nbdim
!
                    if (i /= nb) then
!
                        if (loctab_child(i) == -1 .OR. loctab_child(i) == -3) then
                            ptres(i,1,ndir,nb) = pttab_child(i)
                        else
                            ptres(i,1,ndir,nb) = indtruetab(i,1,1)
                        endif
                        if (loctab_child(i) == -2 .OR. loctab_child(i) == -3) then
                            if (posvartab_child(i) == 1) then
                                ptres(i,2,ndir,nb) = pttab_child(i) + nbtab_child(i)
                            else
                                ptres(i,2,ndir,nb) = pttab_child(i) + nbtab_child(i) - 1
                            endif
                        else
                            ptres(i,2,ndir,nb) = indtruetab(i,2,2)
                        endif
!
                    endif
!
                enddo

!
#if defined AGRIF_MPI
                call Agrif_get_var_bounds_array(child,lower,upper,nbdim)

                do i = 1,nbdim
!
                    Call Agrif_GetLocalBoundaries(ptres(i,1,ndir,nb), ptres(i,2,ndir,nb),  &
                                                  coords(i), lower(i), upper(i), ltab(i), utab(i) )
                    ptres2(i,1,ndir,nb) = max(ltab(i),lower(i))
                    ptres2(i,2,ndir,nb) = min(utab(i),upper(i))
                    if ((i == nb) .AND. (ndir == 1)) then
                        ptres2(i,2,ndir,nb) = max(utab(i),lower(i))
                    elseif ((i == nb) .AND. (ndir == 2)) then
                        ptres2(i,1,ndir,nb) = min(ltab(i),upper(i))
                    endif
!
                enddo
#else
                ptres2(:,:,ndir,nb) = ptres(:,:,ndir,nb)
#endif
            endif
!
        enddo   ! ndir = 1,2
    enddo       ! nb = 1,nbdim
!
    if ( child % interpIndex /= Agrif_Curgrid % parent % ngridstep .OR. &
         child % Interpolationshouldbemade ) then
!
!     Space interpolation
!
        kindex = 1
!
        do nb = 1,nbdim

            type_interp = type_interp_bc(nb,:)

            do ndir = 1,2
!
                if (loctab_child(nb) /= (-ndir) .AND. loctab_child(nb) /= -3) then
!
                    call Agrif_InterpnD(type_interp, parent, child,             &
                                        ptres(1:nbdim,1,ndir,nb),               &
                                        ptres(1:nbdim,2,ndir,nb),               &
                                        pttab_child(1:nbdim),                   &
                                        pttab_Parent(1:nbdim),                  &
                                        s_Child(1:nbdim), s_Parent(1:nbdim),    &
                                        ds_Child(1:nbdim),ds_Parent(1:nbdim),   &
                                        NULL(), .FALSE., nbdim,                 &
                                        childarray,                             &
                                        child%memberin(nb,ndir), .TRUE., procname, coords(nb), ndir)

                    child % childarray(1:nbdim,:,:,nb,ndir) = childarray

                    if (.not. child%interpolationshouldbemade) then
!
!                       Safeguard of the values of the grid variable (at times n and n+1 on the parent grid)
!
                        sizetab = 1
                        do i = 1,nbdim
                            sizetab = sizetab * (ptres2(i,2,ndir,nb)-ptres2(i,1,ndir,nb)+1)
                        enddo

                        call saveAfterInterp(child,ptres2(:,:,ndir,nb),kindex,sizetab,nbdim)
!
                    endif
!
                endif
!
            enddo   ! ndir = 1,2
        enddo       ! nb = 1,nbdim
!
        child % interpIndex = Agrif_Curgrid % parent % ngridstep
!
    endif
!
    if (.not. child%interpolationshouldbemade) then
!
!       Calculation of the coefficients c1t and c2t for the temporary interpolation
!
        if (pweight) then
            c1t = weight
        else
            c1t = (REAL(Agrif_Nbstepint()) + 1.) / Agrif_Rhot()
        endif
        c2t = 1. - c1t
!
!       Time interpolation
!
        kindex = 1
!
        do nb = 1,nbdim
            do ndir = 1,2
                if (loctab_child(nb) /= (-ndir) .AND. loctab_child(nb) /= -3) then
                    Call timeInterpolation(child,ptres2(:,:,ndir,nb),kindex,c1t,c2t,nbdim)
                endif
            enddo
        enddo
!
    endif
!
    do nb = 1,nbdim
    do ndir = 1,2
        if ( (loctab_child(nb) /= (-ndir)) .AND. (loctab_child(nb) /= -3) .AND. child%memberin(nb,ndir) ) then
            select case(nbdim)
            case(1)
                i1 = child % childarray(1,1,2,nb,ndir)
                i2 = child % childarray(1,2,2,nb,ndir)

                call procname(parray1(i1:i2),                               &
                              i1,i2, .FALSE.,coords(nb),ndir)
            case(2)
                i1 = child % childarray(1,1,2,nb,ndir)
                i2 = child % childarray(1,2,2,nb,ndir)
                j1 = child % childarray(2,1,2,nb,ndir)
                j2 = child % childarray(2,2,2,nb,ndir)

                call procname(parray2(i1:i2,j1:j2),                         &
                              i1,i2,j1,j2, .FALSE.,coords(nb),ndir)
            case(3)
                i1 = child % childarray(1,1,2,nb,ndir)
                i2 = child % childarray(1,2,2,nb,ndir)
                j1 = child % childarray(2,1,2,nb,ndir)
                j2 = child % childarray(2,2,2,nb,ndir)
                k1 = child % childarray(3,1,2,nb,ndir)
                k2 = child % childarray(3,2,2,nb,ndir)

                call procname(parray3(i1:i2,j1:j2,k1:k2),                   &
                              i1,i2,j1,j2,k1,k2, .FALSE.,coords(nb),ndir)
            case(4)
                i1 = child % childarray(1,1,2,nb,ndir)
                i2 = child % childarray(1,2,2,nb,ndir)
                j1 = child % childarray(2,1,2,nb,ndir)
                j2 = child % childarray(2,2,2,nb,ndir)
                k1 = child % childarray(3,1,2,nb,ndir)
                k2 = child % childarray(3,2,2,nb,ndir)
                l1 = child % childarray(4,1,2,nb,ndir)
                l2 = child % childarray(4,2,2,nb,ndir)

                call procname(parray4(i1:i2,j1:j2,k1:k2,l1:l2),             &
                              i1,i2,j1,j2,k1,k2,l1,l2, .FALSE.,coords(nb),ndir)
            case(5)
                i1 = child % childarray(1,1,2,nb,ndir)
                i2 = child % childarray(1,2,2,nb,ndir)
                j1 = child % childarray(2,1,2,nb,ndir)
                j2 = child % childarray(2,2,2,nb,ndir)
                k1 = child % childarray(3,1,2,nb,ndir)
                k2 = child % childarray(3,2,2,nb,ndir)
                l1 = child % childarray(4,1,2,nb,ndir)
                l2 = child % childarray(4,2,2,nb,ndir)
                m1 = child % childarray(5,1,2,nb,ndir)
                m2 = child % childarray(5,2,2,nb,ndir)

                call procname(parray5(i1:i2,j1:j2,k1:k2,l1:l2,m1:m2),       &
                              i1,i2,j1,j2,k1,k2,l1,l2,m1,m2, .FALSE.,coords(nb),ndir)
            case(6)
                i1 = child % childarray(1,1,2,nb,ndir)
                i2 = child % childarray(1,2,2,nb,ndir)
                j1 = child % childarray(2,1,2,nb,ndir)
                j2 = child % childarray(2,2,2,nb,ndir)
                k1 = child % childarray(3,1,2,nb,ndir)
                k2 = child % childarray(3,2,2,nb,ndir)
                l1 = child % childarray(4,1,2,nb,ndir)
                l2 = child % childarray(4,2,2,nb,ndir)
                m1 = child % childarray(5,1,2,nb,ndir)
                m2 = child % childarray(5,2,2,nb,ndir)
                n1 = child % childarray(6,1,2,nb,ndir)
                n2 = child % childarray(6,2,2,nb,ndir)

                call procname(parray6(i1:i2,j1:j2,k1:k2,l1:l2,m1:m2,n1:n2), &
                              i1,i2,j1,j2,k1,k2,l1,l2,m1,m2,n1,n2, .FALSE.,coords(nb),ndir)
            end select
        endif
    enddo
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Correctnd
!===================================================================================================
!
!===================================================================================================
!  subroutine saveAfterInterp
!
!> saves the values of the grid variable on the fine grid after the space interpolation
!---------------------------------------------------------------------------------------------------
subroutine saveAfterInterp ( child_var, bounds, kindex, newsize, nbdim )
!---------------------------------------------------------------------------------------------------
    TYPE (Agrif_Variable),       INTENT(inout)  :: child_var !< The fine grid variable
    INTEGER, DIMENSION(nbdim,2), INTENT(in)     :: bounds
    INTEGER,                     INTENT(inout)  :: kindex    !< Index indicating where this safeguard
                                                             !<     is done on the fine grid
    INTEGER,                     INTENT(in)     :: newsize
    INTEGER,                     INTENT(in)     :: nbdim
!
    INTEGER     :: ir,jr,kr,lr,mr,nr
!
!    Allocation of the array oldvalues2d
!
    if (newsize .LE. 0) return
!
    Call Agrif_Checksize(child_var,kindex+newsize)

    if (child_var % interpIndex /= Agrif_Curgrid % parent % ngridstep ) then
        child_var % oldvalues2d(1,kindex:kindex+newsize-1) = &
        child_var % oldvalues2d(2,kindex:kindex+newsize-1)
    endif

    SELECT CASE (nbdim)
    CASE (1)
!CDIR ALTCODE
        do ir = bounds(1,1), bounds(1,2)
            child_var % oldvalues2d(2,kindex) = parray1(ir)
            kindex = kindex + 1
        enddo
!
    CASE (2)
        do jr = bounds(2,1),bounds(2,2)
!CDIR ALTCODE
        do ir = bounds(1,1),bounds(1,2)
            child_var % oldvalues2d(2,kindex) = parray2(ir,jr)
            kindex = kindex + 1
        enddo
        enddo
!
    CASE (3)
        do kr = bounds(3,1),bounds(3,2)
        do jr = bounds(2,1),bounds(2,2)
!CDIR ALTCODE
        do ir = bounds(1,1),bounds(1,2)
            child_var % oldvalues2d(2,kindex) = parray3(ir,jr,kr)
            kindex = kindex + 1
        enddo
        enddo
        enddo
!
      CASE (4)
        do lr = bounds(4,1),bounds(4,2)
        do kr = bounds(3,1),bounds(3,2)
        do jr = bounds(2,1),bounds(2,2)
!CDIR ALTCODE
        do ir = bounds(1,1),bounds(1,2)
            child_var % oldvalues2d(2,kindex) = parray4(ir,jr,kr,lr)
            kindex = kindex + 1
        enddo
        enddo
        enddo
        enddo
!
    CASE (5)
        do mr = bounds(5,1),bounds(5,2)
        do lr = bounds(4,1),bounds(4,2)
        do kr = bounds(3,1),bounds(3,2)
        do jr = bounds(2,1),bounds(2,2)
!CDIR ALTCODE
        do ir = bounds(1,1),bounds(1,2)
            child_var % oldvalues2d(2,kindex) = parray5(ir,jr,kr,lr,mr)
            kindex = kindex + 1
        enddo
        enddo
        enddo
        enddo
        enddo
!
    CASE (6)
        do nr = bounds(6,1),bounds(6,2)
        do mr = bounds(5,1),bounds(5,2)
        do lr = bounds(4,1),bounds(4,2)
        do kr = bounds(3,1),bounds(3,2)
        do jr = bounds(2,1),bounds(2,2)
!CDIR ALTCODE
        do ir = bounds(1,1),bounds(1,2)
            child_var % oldvalues2d(2,kindex) = parray6(ir,jr,kr,lr,mr,nr)
            kindex = kindex + 1
        enddo
        enddo
        enddo
        enddo
        enddo
        enddo
    END SELECT
!---------------------------------------------------------------------------------------------------
end subroutine saveAfterInterp
!===================================================================================================
!
!===================================================================================================
!  subroutine timeInterpolation
!
!> subroutine for a linear time interpolation on the child grid
!---------------------------------------------------------------------------------------------------
subroutine timeInterpolation ( child_var, bounds, kindex, c1t, c2t, nbdim )
!---------------------------------------------------------------------------------------------------
    TYPE (Agrif_Variable)       :: child_var !< The fine grid variable
    INTEGER, DIMENSION(nbdim,2) :: bounds
    INTEGER                     :: kindex    !< Index indicating the values of the fine grid got
                                             !<    before and after the space interpolation and
                                             !<    used for the time interpolation
    REAL                        :: c1t, c2t  !< Coefficients for the time interpolation (c2t=1-c1t)
    INTEGER                     :: nbdim
!
    INTEGER :: ir,jr,kr,lr,mr,nr
!
    SELECT CASE (nbdim)
    CASE (1)
!CDIR ALTCODE
        do ir = bounds(1,1),bounds(1,2)
            parray1(ir) = c2t*child_var % oldvalues2d(1,kindex) + &
                                      c1t*child_var % oldvalues2d(2,kindex)
            kindex = kindex + 1
        enddo
!
    CASE (2)
        do jr = bounds(2,1),bounds(2,2)
!CDIR ALTCODE
        do ir = bounds(1,1),bounds(1,2)
            parray2(ir,jr) = c2t*child_var % oldvalues2d(1,kindex) + &
                                         c1t*child_var % oldvalues2d(2,kindex)
            kindex = kindex + 1
        enddo
        enddo
!
    CASE (3)
        do kr = bounds(3,1),bounds(3,2)
        do jr = bounds(2,1),bounds(2,2)
!CDIR ALTCODE
        do ir = bounds(1,1),bounds(1,2)
            parray3(ir,jr,kr) = c2t*child_var % oldvalues2d(1,kindex) + &
                                            c1t*child_var % oldvalues2d(2,kindex)
            kindex = kindex + 1
        enddo
        enddo
        enddo
!
    CASE (4)
        do lr = bounds(4,1),bounds(4,2)
        do kr = bounds(3,1),bounds(3,2)
        do jr = bounds(2,1),bounds(2,2)
!CDIR ALTCODE
        do ir = bounds(1,1),bounds(1,2)
            parray4(ir,jr,kr,lr) = c2t*child_var % oldvalues2d(1,kindex) + &
                                               c1t*child_var % oldvalues2d(2,kindex)
            kindex = kindex + 1
        enddo
        enddo
        enddo
        enddo
!
    CASE (5)
        do mr=bounds(5,1),bounds(5,2)
        do lr=bounds(4,1),bounds(4,2)
        do kr=bounds(3,1),bounds(3,2)
        do jr=bounds(2,1),bounds(2,2)
!CDIR ALTCODE
        do ir=bounds(1,1),bounds(1,2)
            parray5(ir,jr,kr,lr,mr) = c2t*child_var % oldvalues2d(1,kindex) + &
                                                  c1t*child_var % oldvalues2d(2,kindex)
            kindex = kindex + 1
        enddo
        enddo
        enddo
        enddo
        enddo
!
    CASE (6)
        do nr=bounds(6,1),bounds(6,2)
        do mr=bounds(5,1),bounds(5,2)
        do lr=bounds(4,1),bounds(4,2)
        do kr=bounds(3,1),bounds(3,2)
        do jr=bounds(2,1),bounds(2,2)
!CDIR ALTCODE
        do ir=bounds(1,1),bounds(1,2)
            parray6(ir,jr,kr,lr,mr,nr) = c2t*child_var % oldvalues2d(1,kindex) + &
                                                     c1t*child_var % oldvalues2d(2,kindex)
            kindex = kindex + 1
        enddo
        enddo
        enddo
        enddo
        enddo
        enddo
    END SELECT
!---------------------------------------------------------------------------------------------------
end subroutine timeInterpolation
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Checksize
!
!> subroutine used in the saveAfterInterp procedure to allocate the oldvalues2d array
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Checksize ( child_var, newsize )
!---------------------------------------------------------------------------------------------------
    TYPE (Agrif_Variable), INTENT(inout)  :: child_var !< The fine grid variable
    INTEGER              , INTENT(in)     :: newsize   !< Size of the domains where the boundary
                                                       !<    conditions are calculated
!
    REAL, DIMENSION(:,:), Allocatable :: tempoldvalues ! Temporary array
!
    if (.NOT. associated(child_var % oldvalues2d)) then
!
        allocate(child_var % oldvalues2d(2,newsize))
        child_var % oldvalues2d = 0.
!
    else
!
        if (SIZE(child_var % oldvalues2d,2) < newsize) then
!
            allocate(tempoldvalues(2,SIZE(child_var % oldvalues2d,2)))
            tempoldvalues = child_var % oldvalues2d
            deallocate(child_var % oldvalues2d)
            allocate(  child_var % oldvalues2d(2,newsize))
            child_var % oldvalues2d = 0.
            child_var % oldvalues2d(:,1:SIZE(tempoldvalues,2)) = tempoldvalues(:,:)
            deallocate(tempoldvalues)
!
        endif
!
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Checksize
!===================================================================================================
!
end module Agrif_Boundary

