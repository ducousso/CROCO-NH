!
! $Id: modmask.F 779 2007-12-22 17:04:17Z rblod $
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
!> Module Agrif_Mask.
!>
!> Module for masks.
!
module Agrif_Mask
!
    use Agrif_Types
!
    implicit none
!
contains
!
!===================================================================================================
!  subroutine Agrif_CheckMasknD
!
!> Called in the procedure #Agrif_InterpnD to recalculate the value of the parent grid variable
!! when this one is equal to Agrif_SpecialValue.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_CheckMasknD ( tempP, parent, pbtab, petab, ppbtab, ppetab, noraftab, nbdim )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variable), pointer   :: tempP  !< Part of the parent grid used for the interpolation of the child grid
    type(Agrif_Variable), pointer   :: parent !< The parent grid
    integer, dimension(nbdim)       :: pbtab  !< limits of the parent grid used
    integer, dimension(nbdim)       :: petab  !< interpolation of the child grid
    integer, dimension(nbdim)       :: ppbtab, ppetab
    logical, dimension(nbdim)       :: noraftab
    integer                         :: nbdim
!
    integer :: i0,j0,k0,l0,m0,n0
!
    select case (nbdim)
    case (1)
        do i0 = pbtab(1),petab(1)
            if (tempP%array1(i0) == Agrif_SpecialValue) then
                call CalculNewValTempP((/i0/),tempP,parent,ppbtab,ppetab,noraftab,nbdim)
            endif
        enddo
    case (2)
        do j0 = pbtab(2),petab(2)
        do i0 = pbtab(1),petab(1)
            if (tempP%array2(i0,j0) == Agrif_SpecialValue) then
                call CalculNewValTempP((/i0,j0/),tempP,parent,ppbtab,ppetab, noraftab,nbdim)
            endif
        enddo
        enddo
    case (3)
        do k0 = pbtab(3),petab(3)
        do j0 = pbtab(2),petab(2)
        do i0 = pbtab(1),petab(1)
            if (tempP%array3(i0,j0,k0) == Agrif_SpecialValue) then
!------CDIR NEXPAND
                call CalculNewValTempP3D((/i0,j0,k0/), &
                    tempP%array3(ppbtab(1),ppbtab(2),ppbtab(3)),  &
                    parent%array3(ppbtab(1),ppbtab(2),ppbtab(3)), &
                    ppbtab,ppetab,noraftab,MaxSearch,Agrif_SpecialValue)

!            Call CalculNewValTempP((/i0,j0,k0/),
!     &                             tempP,parent,
!     &                             ppbtab,ppetab,
!     &                             noraftab,nbdim)

            endif
        enddo
        enddo
        enddo
    case (4)
        do l0 = pbtab(4),petab(4)
        do k0 = pbtab(3),petab(3)
        do j0 = pbtab(2),petab(2)
        do i0 = pbtab(1),petab(1)
            if (tempP%array4(i0,j0,k0,l0) == Agrif_SpecialValue) then
                call CalculNewValTempP4D((/i0,j0,k0,l0/), &
                    tempP%array4(ppbtab(1),ppbtab(2),ppbtab(3),ppbtab(4)),  &
                    parent%array4(ppbtab(1),ppbtab(2),ppbtab(3),ppbtab(4)), &
                    ppbtab,ppetab,noraftab,MaxSearch,Agrif_SpecialValue)
            endif
        enddo
        enddo
        enddo
        enddo
    case (5)
        do m0 = pbtab(5),petab(5)
        do l0 = pbtab(4),petab(4)
        do k0 = pbtab(3),petab(3)
        do j0 = pbtab(2),petab(2)
        do i0 = pbtab(1),petab(1)
            if (tempP%array5(i0,j0,k0,l0,m0) == Agrif_SpecialValue) then
                call CalculNewValTempP((/i0,j0,k0,l0,m0/), &
                    tempP,parent,ppbtab,ppetab,noraftab,nbdim)
            endif
        enddo
        enddo
        enddo
        enddo
        enddo
    case (6)
        do n0 = pbtab(6),petab(6)
        do m0 = pbtab(5),petab(5)
        do l0 = pbtab(4),petab(4)
        do k0 = pbtab(3),petab(3)
        do j0 = pbtab(2),petab(2)
        do i0 = pbtab(1),petab(1)
            if (tempP%array6(i0,j0,k0,l0,m0,n0) == Agrif_SpecialValue) then
                call CalculNewValTempP((/i0,j0,k0,l0,m0,n0/), &
                    tempP,parent,ppbtab,ppetab,noraftab,nbdim)
            endif
        enddo
        enddo
        enddo
        enddo
        enddo
        enddo
    end select
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_CheckMasknD
!===================================================================================================
!
!===================================================================================================
!  subroutine CalculNewValTempP
!
!> Called in the procedure #Agrif_InterpnD to recalculate the value of the parent grid variable
!! when this one is equal to Agrif_SpecialValue.
!---------------------------------------------------------------------------------------------------
subroutine CalculNewValTempP ( indic, tempP, parent, ppbtab, ppetab, noraftab, nbdim )
!---------------------------------------------------------------------------------------------------
    integer, dimension(nbdim)       :: indic
    type(Agrif_Variable), pointer   :: tempP  !< Part of the parent grid used for the interpolation of the child grid
    type(Agrif_Variable), pointer   :: parent !< The parent grid
    integer, dimension(nbdim)       :: ppbtab, ppetab
    logical, dimension(nbdim)       :: noraftab
    integer                         :: nbdim
!
    integer                     :: i,ii,iii,jj,kk,ll,mm,nn
    integer, dimension(nbdim)   :: imin,imax,idecal
    integer                     :: nbvals
    real                        :: res
    real                        :: valparent
    integer                     :: ValMax
    logical                     :: firsttest
!
    ValMax = 1
    do iii = 1,nbdim
        if (.NOT.noraftab(iii)) then
            ValMax = max(ValMax,ppetab(iii)-indic(iii))
            ValMax = max(ValMax,indic(iii)-ppbtab(iii))
        endif
    enddo
!
    Valmax = min(Valmax,MaxSearch)
!
!CDIR NOVECTOR
    imin = indic
!CDIR NOVECTOR
    imax = indic
!
    i = 1
    firsttest = .TRUE.
!
    do while (i <= ValMax)
!
        if ( (i == 1).AND.(firsttest) ) i = Valmax
!
        do iii = 1,nbdim
            if (.NOT.noraftab(iii)) then
                imin(iii) = max(indic(iii) - i,ppbtab(iii))
                imax(iii) = min(indic(iii) + i,ppetab(iii))
                if (firsttest) then
                    if (indic(iii) > ppbtab(iii)) then
!CDIR NOVECTOR
                        idecal = indic
                        idecal(iii) = idecal(iii)-1
                        SELECT CASE(nbdim)
                        CASE (1)
                            if (tempP%array1(idecal(1) &
                                    ) == Agrif_SpecialValue) imin(iii) = imax(iii)
                        CASE (2)
                            if (tempP%array2(idecal(1), idecal(2) &
                                    ) == Agrif_SpecialValue) imin(iii) = imax(iii)
                        CASE (3)
                            if (tempP%array3(idecal(1), &
                                             idecal(2), idecal(3) &
                                    ) == Agrif_SpecialValue) imin(iii) = imax(iii)
                        CASE (4)
                            if (tempP%array4(idecal(1), idecal(2), &
                                             idecal(3), idecal(4)  &
                                    ) == Agrif_SpecialValue) imin(iii) = imax(iii)
                        CASE (5)
                            if (tempP%array5(idecal(1), idecal(2), &
                                             idecal(3), idecal(4), &
                                             idecal(5)             &
                                    ) == Agrif_SpecialValue) imin(iii) = imax(iii)
                        CASE (6)
                            if (tempP%array6(idecal(1), idecal(2), &
                                             idecal(3), idecal(4), &
                                             idecal(5), idecal(6)  &
                                    ) == Agrif_SpecialValue) imin(iii) = imax(iii)
                        END SELECT
                    endif
                endif
            endif
        enddo
!
        Res = 0.
        Nbvals = 0
!
        SELECT CASE(nbdim)
        CASE (1)
!CDIR ALTCODE
!CDIR SHORTLOOP
            do ii = imin(1),imax(1)
                ValParent = parent%array1(ii)
                if ( ValParent /= Agrif_SpecialValue) then
                    Res = Res + ValParent
                    Nbvals = Nbvals + 1
                endif
            enddo
!
        CASE (2)
            do jj = imin(2),imax(2)
!CDIR ALTCODE
!CDIR SHORTLOOP
            do ii = imin(1),imax(1)
                ValParent = parent%array2(ii,jj)
                if ( ValParent /= Agrif_SpecialValue) then
                    Res = Res + ValParent
                    Nbvals = Nbvals + 1
                endif
            enddo
            enddo

        CASE (3)
            do kk = imin(3),imax(3)
            do jj = imin(2),imax(2)
!CDIR ALTCODE
!CDIR SHORTLOOP
            do ii = imin(1),imax(1)
                ValParent = parent%array3(ii,jj,kk)
                if ( ValParent /= Agrif_SpecialValue) then
                    Res = Res + ValParent
                    Nbvals = Nbvals + 1
                endif
            enddo
            enddo
            enddo

        CASE (4)
            do ll = imin(4),imax(4)
            do kk = imin(3),imax(3)
            do jj = imin(2),imax(2)
!CDIR ALTCODE
!CDIR SHORTLOOP
            do ii = imin(1),imax(1)
                ValParent = parent%array4(ii,jj,kk,ll)
                if ( ValParent /= Agrif_SpecialValue) then
                    Res = Res + ValParent
                    Nbvals = Nbvals + 1
                endif
            enddo
            enddo
            enddo
            enddo

        CASE (5)
            do mm = imin(5),imax(5)
            do ll = imin(4),imax(4)
            do kk = imin(3),imax(3)
            do jj = imin(2),imax(2)
!CDIR ALTCODE
!CDIR SHORTLOOP
            do ii = imin(1),imax(1)
                ValParent = parent%array5(ii,jj,kk,ll,mm)
                if ( ValParent /= Agrif_SpecialValue) then
                    Res = Res + ValParent
                    Nbvals = Nbvals + 1
                endif
            enddo
            enddo
            enddo
            enddo
            enddo

        CASE (6)
            do nn = imin(6),imax(6)
            do mm = imin(5),imax(5)
            do ll = imin(4),imax(4)
            do kk = imin(3),imax(3)
            do jj = imin(2),imax(2)
!CDIR ALTCODE
!CDIR SHORTLOOP
            do ii = imin(1),imax(1)
                ValParent = parent%array6(ii,jj,kk,ll,mm,nn)
                if ( ValParent /= Agrif_SpecialValue) then
                    Res = Res + ValParent
                    Nbvals = Nbvals + 1
                endif
            enddo
            enddo
            enddo
            enddo
            enddo
            enddo

        END SELECT
!
        if (Nbvals > 0) then
            if (firsttest) then
                firsttest = .FALSE.
                i=1
                cycle
            endif
            SELECT CASE(nbdim)
            CASE (1)
                tempP%array1(indic(1))           = Res/Nbvals
            CASE (2)
                tempP%array2(indic(1), indic(2)) = Res/Nbvals
            CASE (3)
                tempP%array3(indic(1), indic(2), &
                             indic(3))           = Res/Nbvals
            CASE (4)
                tempP%array4(indic(1), indic(2), &
                             indic(3), indic(4)) = Res/Nbvals
            CASE (5)
                tempP%array5(indic(1), indic(2), &
                             indic(3), indic(4), &
                             indic(5))           = Res/Nbvals
            CASE (6)
                tempP%array6(indic(1), indic(2), &
                             indic(3), indic(4), &
                             indic(5), indic(6)) = Res/Nbvals
            END SELECT
            exit
        else
            if (firsttest) exit
            i = i + 1
        endif
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine CalculNewValTempP
!===================================================================================================
!
!===================================================================================================
!  subroutine CalculNewValTempP3D
!
!> Called in the procedure #Agrif_InterpnD to recalculate the value of the parent grid variable
!! when this one is equal to Agrif_SpecialValue.
!---------------------------------------------------------------------------------------------------
subroutine CalculNewValTempP3D ( indic, tempP, parent, ppbtab, ppetab, noraftab, &
                                 MaxSearch, Agrif_SpecialValue )
!---------------------------------------------------------------------------------------------------
    integer, parameter          :: nbdim = 3
    integer, dimension(nbdim)   :: indic
    integer, dimension(nbdim)   :: ppbtab, ppetab
    logical, dimension(nbdim)   :: noraftab
    integer                     :: MaxSearch
    real                        :: Agrif_SpecialValue
    real, dimension(ppbtab(1):ppetab(1), &
                    ppbtab(2):ppetab(2), &
                    ppbtab(3):ppetab(3)) &
                                :: tempP, parent  !< Part of the parent grid used for
                                                  !< the interpolation of the child grid
!
    integer                     :: i,ii,iii,jj,kk
    integer, dimension(nbdim)   :: imin,imax,idecal
    integer                     :: Nbvals
    real                        :: Res
    real                        :: ValParent
    integer                     :: ValMax
    logical                     :: Existunmasked
!
    ValMax = 1
!CDIR NOVECTOR
    do iii = 1,nbdim
        if (.NOT.noraftab(iii)) then
            ValMax = max(ValMax,ppetab(iii)-indic(iii))
            ValMax = max(ValMax,indic(iii)-ppbtab(iii))
        endif
    enddo
!
    Valmax = min(Valmax,MaxSearch)
!
!CDIR NOVECTOR
    imin = indic
!CDIR NOVECTOR
    imax = indic

!CDIR NOVECTOR
    idecal = indic
    i = Valmax
!
    do iii = 1,nbdim
        if (.NOT.noraftab(iii)) then
            imin(iii) = max(indic(iii) - i,ppbtab(iii))
            imax(iii) = min(indic(iii) + i,ppetab(iii))

            if (indic(iii) > ppbtab(iii)) then
                idecal(iii) = idecal(iii)-1
                if (tempP(idecal(1),idecal(2),idecal(3)) == Agrif_SpecialValue) then
                    imin(iii) = imax(iii)
                endif
                idecal(iii) = idecal(iii)+1
            endif
        endif
    enddo
!
    Existunmasked = .FALSE.
!
    do kk = imin(3),imax(3)
    do jj = imin(2),imax(2)
!CDIR NOVECTOR
    do ii = imin(1),imax(1)
        if ( parent(ii,jj,kk) /= Agrif_SpecialValue) then
            Existunmasked = .TRUE.
            exit
        endif
    enddo
    enddo
    enddo
!
    if (.Not.Existunmasked) return
!
    i = 1
!
    do while(i <= ValMax)
!
        do iii = 1 , nbdim
            if (.NOT.noraftab(iii)) then
                imin(iii) = max(indic(iii) - i,ppbtab(iii))
                imax(iii) = min(indic(iii) + i,ppetab(iii))
            endif
        enddo
!
        Res = 0.
        Nbvals = 0
!
        do kk = imin(3),imax(3)
        do jj = imin(2),imax(2)
!CDIR NOVECTOR
        do ii = imin(1),imax(1)
            ValParent = parent(ii,jj,kk)
            if ( ValParent /= Agrif_SpecialValue) then
                Res = Res + ValParent
                Nbvals = Nbvals + 1
            endif
        enddo
        enddo
        enddo
!
        if (Nbvals > 0) then
            tempP(indic(1),indic(2),indic(3)) = Res/Nbvals
            exit
        else
            i = i + 1
        endif
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine CalculNewValTempP3D
!===================================================================================================
!
!===================================================================================================
!  subroutine CalculNewValTempP4D
!
!> Called in the procedure #Agrif_InterpnD to recalculate the value of the parent grid variable
!! when this one is equal to Agrif_SpecialValue.
!---------------------------------------------------------------------------------------------------
subroutine CalculNewValTempP4D ( indic, tempP, parent, ppbtab, ppetab, noraftab, &
                                 MaxSearch, Agrif_SpecialValue )
!---------------------------------------------------------------------------------------------------
    integer, parameter          :: nbdim = 4
    integer, dimension(nbdim)   :: indic
    integer, dimension(nbdim)   :: ppbtab, ppetab
    logical, dimension(nbdim)   :: noraftab
    integer                     :: MaxSearch
    real                        :: Agrif_SpecialValue
    real, dimension(ppbtab(1):ppetab(1), &
                    ppbtab(2):ppetab(2), &
                    ppbtab(3):ppetab(3), &
                    ppbtab(4):ppetab(4)) &
                                :: tempP, parent  !< Part of the parent grid used for
                                                  !< the interpolation of the child grid
!
    integer                     :: i,ii,iii,jj,kk,ll
    integer, dimension(nbdim)   :: imin,imax,idecal
    integer                     :: Nbvals
    real                        :: Res
    real                        :: ValParent
    integer                     :: ValMax
!
    logical                     :: firsttest
!
    ValMax = 1
    do iii = 1,nbdim
        if (.NOT.noraftab(iii)) then
            ValMax = max(ValMax,ppetab(iii)-indic(iii))
            ValMax = max(ValMax,indic(iii)-ppbtab(iii))
        endif
    enddo
!
    Valmax = min(Valmax,MaxSearch)
!
    imin = indic
    imax = indic
!
    i = 1
    firsttest = .TRUE.
    idecal = indic
!
    do while (i <= ValMax)
!
        if ((i == 1).AND.(firsttest)) i = Valmax

        do iii = 1,nbdim
            if (.NOT.noraftab(iii)) then
                imin(iii) = max(indic(iii) - i,ppbtab(iii))
                imax(iii) = min(indic(iii) + i,ppetab(iii))
                if (firsttest) then
                    if (indic(iii) > ppbtab(iii)) then
                        idecal(iii) = idecal(iii)-1
                        if (tempP(idecal(1),idecal(2),idecal(3),idecal(4)) == Agrif_SpecialValue) then
                            imin(iii) = imax(iii)
                        endif
                        idecal(iii) = idecal(iii)+1
                    endif
                endif
            endif
        enddo
!
        Res = 0.
        Nbvals = 0
!
        do ll = imin(4),imax(4)
        do kk = imin(3),imax(3)
        do jj = imin(2),imax(2)
        do ii = imin(1),imax(1)
            ValParent = parent(ii,jj,kk,ll)
            if ( ValParent /= Agrif_SpecialValue) then
                Res = Res + ValParent
                Nbvals = Nbvals + 1
            endif
        enddo
        enddo
        enddo
        enddo
!
        if (Nbvals > 0) then
            if (firsttest) then
                firsttest = .FALSE.
                i=1
                cycle
            endif

            tempP(indic(1),indic(2),indic(3),indic(4)) = Res/Nbvals
            exit
        else
            if (firsttest) exit
            i = i + 1
        endif
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine CalculNewValTempP4D
!===================================================================================================
!
end module Agrif_Mask
