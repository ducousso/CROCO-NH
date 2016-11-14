!
! $Id: modcluster.F 662 2007-05-25 15:58:52Z opalod $
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
!     Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
!
!
!> Module Agrif_Clustering
!>
!> Contains subroutines to create and initialize the grid hierarchy from the
!> AGRIF_FixedGrids.in file.
!
module Agrif_Clustering
!
    use Agrif_CurgridFunctions
    use Agrif_Init_Vars
    use Agrif_Save
!
    implicit none
!
    abstract interface
        subroutine init_proc()
        end subroutine init_proc
    end interface
!
contains
!
!===================================================================================================
!  subroutine Agrif_Cluster_All
!
!> Subroutine for the clustering. A temporary grid hierarchy, pointed by parent_rect, is created.
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Cluster_All ( g, parent_rect )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Grid)     , pointer   :: g        !< Pointer on the current grid
    TYPE(Agrif_Rectangle), pointer   :: parent_rect
!
    TYPE(Agrif_LRectangle), pointer  :: parcours
    TYPE(Agrif_Grid)      , pointer  :: newgrid
    REAL                             :: g_eps
    INTEGER                          :: i
!
    g_eps = huge(1.)
    do i = 1,Agrif_Probdim
        g_eps = min(g_eps, g % Agrif_dx(i))
    enddo
!
    g_eps = g_eps / 100.
!
!   Necessary condition for clustering
    do i = 1,Agrif_Probdim
        if ( g%Agrif_dx(i)/Agrif_coeffref(i) < (Agrif_mind(i)-g_eps)) return
    enddo
!
    nullify(parent_rect%childgrids)
!
    call Agrif_ClusterGridnD(g,parent_rect)
!
    parcours => parent_rect % childgrids
!
    do while ( associated(parcours) )
!
!       Newgrid is created. It is a copy of a fine grid created previously by clustering.
        allocate(newgrid)
!
        do i = 1,Agrif_Probdim
            newgrid % nb(i) = (parcours % r % imax(i) - parcours % r % imin(i)) * Agrif_Coeffref(i)
            newgrid % Agrif_x(i)  = g % Agrif_x(i)  + (parcours % r % imin(i) -1) * g%Agrif_dx(i)
            newgrid % Agrif_dx(i) = g % Agrif_dx(i) / Agrif_Coeffref(i)
        enddo
!
        if ( Agrif_Probdim == 1 ) then
            allocate(newgrid%tabpoint1D(newgrid%nb(1)+1))
            newgrid%tabpoint1D = 0
        endif
!
        if ( Agrif_Probdim == 2 ) then
            allocate(newgrid%tabpoint2D(newgrid%nb(1)+1, newgrid%nb(2)+1))
            newgrid%tabpoint2D = 0
        endif
!
        if ( Agrif_Probdim == 3 ) then
            allocate(newgrid%tabpoint3D(newgrid%nb(1)+1, newgrid%nb(2)+1, newgrid%nb(3)+1))
            newgrid%tabpoint3D = 0
        endif
!
!       Points detection on newgrid
        call Agrif_TabpointsnD(Agrif_mygrid,newgrid)
!
!       recursive call to Agrif_Cluster_All
        call Agrif_Cluster_All(newgrid, parcours % r)
!
        parcours => parcours % next
!
        if ( Agrif_Probdim == 1 ) deallocate(newgrid%tabpoint1D)
        if ( Agrif_Probdim == 2 ) deallocate(newgrid%tabpoint2D)
        if ( Agrif_Probdim == 3 ) deallocate(newgrid%tabpoint3D)
!
        deallocate(newgrid)
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Cluster_All
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_TabpointsnD
!
!> Copy on newgrid of points detected on the grid hierarchy pointed by g.
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_TabpointsnD ( g, newgrid )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Grid), pointer   :: g, newgrid
!
    TYPE(Agrif_PGrid), pointer  :: parcours
!
    REAL                  :: g_eps, newgrid_eps, eps
    REAL   , DIMENSION(3) :: newmin, newmax
    REAL   , DIMENSION(3) :: gmin, gmax
    REAL   , DIMENSION(3) :: xmin
    INTEGER, DIMENSION(3) :: igmin, inewmin
    INTEGER, DIMENSION(3) :: inewmax
    INTEGER               :: i,  j,  k
    INTEGER               :: i0, j0, k0
!
    parcours => g % child_list % first
!
    do while( associated(parcours) )
        call Agrif_TabpointsnD(parcours%gr,newgrid)
        parcours => parcours % next
    enddo
!
    g_eps = 10.
    newgrid_eps = 10.
!
    do i = 1,Agrif_probdim
        g_eps = min( g_eps , g % Agrif_dx(i) )
        newgrid_eps = min(newgrid_eps,newgrid%Agrif_dx(i))
    enddo
!
    eps = min(g_eps,newgrid_eps)/100.
!
    do i = 1,Agrif_probdim
!
         if ( newgrid%Agrif_dx(i) < (g%Agrif_dx(i)-eps) ) return
!
         gmin(i) = g%Agrif_x(i)
         gmax(i) = g%Agrif_x(i) + g%nb(i) * g%Agrif_dx(i)
!
         newmin(i) = newgrid%Agrif_x(i)
         newmax(i) = newgrid%Agrif_x(i) + newgrid%nb(i) * newgrid%Agrif_dx(i)
!
         if (gmax(i) < newmin(i)) return
         if (gmin(i) > newmax(i)) return
!
         inewmin(i) = 1 - floor(-(max(gmin(i),newmin(i))-newmin(i)) / newgrid%Agrif_dx(i))
!
         xmin(i) = newgrid%Agrif_x(i) + (inewmin(i)-1)*newgrid%Agrif_dx(i)
!
         igmin(i) = 1 + nint((xmin(i)-gmin(i))/g%Agrif_dx(i))
!
         inewmax(i) = 1 + int(   (min(gmax(i),newmax(i))-newmin(i)) / newgrid%Agrif_dx(i))
!
    enddo
!
    if ( Agrif_probdim == 1 ) then
        i0 = igmin(1)
        do i = inewmin(1),inewmax(1)
            newgrid%tabpoint1D(i) = max( newgrid%tabpoint1D(i), g%tabpoint1D(i0) )
        enddo
        i0 = i0 + int(newgrid%Agrif_dx(1)/g%Agrif_dx(1))
    endif
!
    if ( Agrif_probdim == 2 ) then
        i0 = igmin(1)
        do i = inewmin(1),inewmax(1)
            j0 = igmin(2)
            do j = inewmin(2),inewmax(2)
                newgrid%tabpoint2D(i,j) = max( newgrid%tabpoint2D(i,j), g%tabpoint2D(i0,j0) )
                j0 = j0 + int(newgrid%Agrif_dx(2)/g%Agrif_dx(2))
            enddo
            i0 = i0 + int(newgrid%Agrif_dx(1)/g%Agrif_dx(1))
        enddo
    endif
!
    if ( Agrif_probdim == 3 ) then
        i0 = igmin(1)
        do i = inewmin(1),inewmax(1)
            j0 = igmin(2)
            do j = inewmin(2),inewmax(2)
                k0 = igmin(3)
                do k = inewmin(3),inewmax(3)
                    newgrid%tabpoint3D(i,j,k) = max( newgrid%tabpoint3D(i,j,k), g%tabpoint3D(i0,j0,k0))
                    k0 = k0 + int(newgrid%Agrif_dx(3)/g%Agrif_dx(3))
                enddo
                j0 = j0 + int(newgrid%Agrif_dx(2)/g%Agrif_dx(2))
            enddo
            i0 = i0 + int(newgrid%Agrif_dx(1)/g%Agrif_dx(1))
        enddo
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_TabpointsnD
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_ClusterGridnD
!
!> Clustering on the grid pointed by g.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_ClusterGridnD ( g, parent_rect )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Grid)     , pointer  :: g          !< Pointer on the current grid
    TYPE(Agrif_Rectangle), pointer  :: parent_rect
!
    TYPE(Agrif_Rectangle) :: newrect
    TYPE(Agrif_Variable_i)  :: newflag
!
    INTEGER               :: i,j,k
    INTEGER, DIMENSION(3) :: sx
    INTEGER               :: bufferwidth,flagpoints
    INTEGER               :: n1,n2,m1,m2,o1,o2
!
    bufferwidth = int(Agrif_Minwidth/5.)
!
    do i = 1,Agrif_probdim
        sx(i) = g % nb(i) + 1
    enddo
!
    if ( Agrif_probdim == 1 ) then
        allocate(newflag%iarray1(sx(1)))
        newflag%iarray1 = 0
    endif
    if ( Agrif_probdim == 2 ) then
        allocate(newflag%iarray2(sx(1),sx(2)))
        newflag%iarray2 = 0
    endif
    if ( Agrif_probdim == 3 ) then
        allocate(newflag%iarray3(sx(1),sx(2),sx(3)))
        newflag%iarray3 = 0
    endif
!
    flagpoints = 0
!
    if ( bufferwidth>0 ) then
!
        if ( Agrif_probdim == 1 ) then
            do i = bufferwidth,sx(1)-bufferwidth+1
                if (g % tabpoint1D(i) == 1) then
                    m1 = i - bufferwidth + 1
                    m2 = i + bufferwidth - 1
                    flagpoints = flagpoints + 1
                    newflag%iarray1(m1:m2) = 1
                endif
            enddo
        endif
!
        if ( Agrif_probdim == 2 ) then
            do i = bufferwidth,sx(1)-bufferwidth+1
            do j = bufferwidth,sx(2)-bufferwidth+1
                if (g % tabpoint2D(i,j) == 1) then
                    n1 = j - bufferwidth + 1
                    n2 = j + bufferwidth - 1
                    m1 = i - bufferwidth + 1
                    m2 = i + bufferwidth - 1
                    flagpoints = flagpoints + 1
                    newflag%iarray2(m1:m2,n1:n2) = 1
                endif
            enddo
            enddo
        endif
!
        if ( Agrif_probdim == 3 ) then
            do i = bufferwidth,sx(1)-bufferwidth+1
            do j = bufferwidth,sx(2)-bufferwidth+1
            do k = bufferwidth,sx(3)-bufferwidth+1
                if (g % tabpoint3D(i,j,k) == 1) then
                    o1 = k - bufferwidth + 1
                    o2 = k + bufferwidth - 1
                    n1 = j - bufferwidth + 1
                    n2 = j + bufferwidth - 1
                    m1 = i - bufferwidth + 1
                    m2 = i + bufferwidth - 1
                    flagpoints = flagpoints + 1
                    newflag%iarray3(m1:m2,n1:n2,o1:o2) = 1
                endif
            enddo
            enddo
            enddo
        endif
!
    else
!
        flagpoints = 1
        if ( Agrif_probdim == 1 ) newflag%iarray1 = g % tabpoint1D
        if ( Agrif_probdim == 2 ) newflag%iarray2 = g % tabpoint2D
        if ( Agrif_probdim == 3 ) newflag%iarray3 = g % tabpoint3D
!
    endif
!
    if (flagpoints == 0) then
        if ( Agrif_probdim == 1 ) deallocate(newflag%iarray1)
        if ( Agrif_probdim == 2 ) deallocate(newflag%iarray2)
        if ( Agrif_probdim == 3 ) deallocate(newflag%iarray3)
        return
    endif
!
    do i = 1 , Agrif_probdim
        newrect % imin(i) = 1
        newrect % imax(i) = sx(i)
    enddo
!
    call Agrif_Clusternd(newflag,parent_rect%childgrids,newrect)
!
    if ( Agrif_probdim == 1 ) deallocate(newflag%iarray1)
    if ( Agrif_probdim == 2 ) deallocate(newflag%iarray2)
    if ( Agrif_probdim == 3 ) deallocate(newflag%iarray3)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_ClusterGridnD
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_ClusternD
!
!> Clustering on the grid pointed by oldB.
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Clusternd ( flag, boxlib, oldB )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Variable_i)            :: flag
    TYPE(Agrif_LRectangle), pointer :: boxlib
    TYPE(Agrif_Rectangle)           :: oldB
!
    TYPE(Agrif_LRectangle),pointer :: tempbox,parcbox,parcbox2
    TYPE(Agrif_Rectangle) :: newB,newB2
    INTEGER :: i,j,k
    INTEGER, DIMENSION(:), allocatable :: i_sig, j_sig, k_sig
    INTEGER, DIMENSION(3) :: ipu,ipl
    INTEGER, DIMENSION(3) :: istr,islice
    REAL :: cureff, neweff
    INTEGER :: ValMax,ValSum,TailleTab
    INTEGER :: nbpoints,nbpointsflag
    LOGICAL :: test
!
                              allocate( i_sig(oldB%imin(1):oldB%imax(1)) )
    if ( Agrif_probdim >= 2 ) allocate( j_sig(oldB%imin(2):oldB%imax(2)) )
    if ( Agrif_probdim == 3 ) allocate( k_sig(oldB%imin(3):oldB%imax(3)) )
!
    test = .FALSE.
    do i = 1,Agrif_probdim
        test = test .OR. ( (oldB%imax(i)-oldB%imin(i)+1) <  Agrif_Minwidth)
    enddo
    if ( test ) return
!
    if ( Agrif_probdim == 1 ) i_sig = flag%iarray1
    if ( Agrif_probdim == 2 ) then
        do i = oldB%imin(1),oldB%imax(1)
            i_sig(i) = SUM(flag%iarray2(i, oldB%imin(2):oldB%imax(2)))
        enddo
        do j = oldB%imin(2),oldB%imax(2)
            j_sig(j) = SUM(flag%iarray2(oldB%imin(1):oldB%imax(1),j))
        enddo
    endif
    if ( Agrif_probdim == 3 ) then
        do i = oldB%imin(1),oldB%imax(1)
            i_sig(i) = SUM(flag%iarray3(i,oldB%imin(2):oldB%imax(2),    &
                                          oldB%imin(3):oldB%imax(3)))
        enddo
        do j = oldB%imin(2),oldB%imax(2)
            j_sig(j) = SUM(flag%iarray3(  oldB%imin(1):oldB%imax(1), j, &
                                          oldB%imin(3):oldB%imax(3)))
        enddo
        do k = oldB%imin(3),oldB%imax(3)
            k_sig(k) = SUM(flag%iarray3(  oldB%imin(1):oldB%imax(1),    &
                                          oldB%imin(2):oldB%imax(2), k) )
        enddo
    endif
!
    do i = 1,Agrif_probdim
        ipl(i) = oldB%imin(i)
        ipu(i) = oldB%imax(i)
    enddo
!
                              call Agrif_Clusterprune(i_sig,ipl(1),ipu(1))
    if ( Agrif_probdim >= 2 ) call Agrif_Clusterprune(j_sig,ipl(2),ipu(2))
    if ( Agrif_probdim == 3 ) call Agrif_Clusterprune(k_sig,ipl(3),ipu(3))
!
    test = .TRUE.
    do i = 1,Agrif_probdim
        test = test .AND. (ipl(i) == oldB%imin(i))
        test = test .AND. (ipu(i) == oldB%imax(i))
    enddo

    if (.NOT. test) then
        do i = 1 , Agrif_probdim
            newB%imin(i) = ipl(i)
            newB%imax(i) = ipu(i)
        enddo
!
        if ( Agrif_probdim == 1 ) nbpoints = SUM(flag%iarray1(newB%imin(1):newB%imax(1)))
        if ( Agrif_probdim == 2 ) nbpoints = SUM(flag%iarray2(newB%imin(1):newB%imax(1),    &
                                                              newB%imin(2):newB%imax(2)))
        if ( Agrif_probdim == 3 ) nbpoints = SUM(flag%iarray3(newB%imin(1):newB%imax(1),    &
                                                              newB%imin(2):newB%imax(2),    &
                                                              newB%imin(3):newB%imax(3)))
!
        if ( Agrif_probdim == 1 ) TailleTab = (newB%imax(1)-newB%imin(1)+1)
        if ( Agrif_probdim == 2 ) TailleTab = (newB%imax(1)-newB%imin(1)+1) * &
                                              (newB%imax(2)-newB%imin(2)+1)
        if ( Agrif_probdim == 3 ) TailleTab = (newB%imax(1)-newB%imin(1)+1) * &
                                              (newB%imax(2)-newB%imin(2)+1) * &
                                              (newB%imax(3)-newB%imin(3)+1)
        neweff = REAL(nbpoints) / TailleTab
!
        if (nbpoints > 0) then
!
            if ((neweff > Agrif_efficiency)) then
                call Agrif_Add_Rectangle(newB,boxlib)
                return
            else
!
                tempbox => boxlib
                newB2 = newB
                call Agrif_Clusternd(flag,boxlib,newB)
!
!               Compute new efficiency
                cureff = neweff
                parcbox2 => boxlib
                nbpoints = 0
                nbpointsflag = 0
!
                do while (associated(parcbox2))
                    if (associated(parcbox2,tempbox)) exit
                    newB = parcbox2%r
!
                    if ( Agrif_probdim == 1 ) Valsum = SUM(flag%iarray1(newB%imin(1):newB%imax(1)))
                    if ( Agrif_probdim == 2 ) Valsum = SUM(flag%iarray2(newB%imin(1):newB%imax(1), &
                                                                        newB%imin(2):newB%imax(2)))
                    if ( Agrif_probdim == 3 ) Valsum = SUM(flag%iarray3(newB%imin(1):newB%imax(1), &
                                                                        newB%imin(2):newB%imax(2), &
                                                                        newB%imin(3):newB%imax(3)))
                    nbpointsflag = nbpointsflag + ValSum
!
                    if ( Agrif_probdim == 1 ) TailleTab = (newB%imax(1)-newB%imin(1)+1)
                    if ( Agrif_probdim == 2 ) TailleTab = (newB%imax(1)-newB%imin(1)+1) * &
                                                          (newB%imax(2)-newB%imin(2)+1)
                    if ( Agrif_probdim == 3 ) TailleTab = (newB%imax(1)-newB%imin(1)+1) * &
                                                          (newB%imax(2)-newB%imin(2)+1) * &
                                                          (newB%imax(3)-newB%imin(3)+1)
                    nbpoints = nbpoints + TailleTab
                    parcbox2 => parcbox2%next
                enddo
!
! coefficient 1.05 avant 1.15 possibilite de laisser choix a l utilisateur
                if ( REAL(nbpointsflag)/REAL(nbpoints) < (1.0001*cureff)) then
                    parcbox2 => boxlib
                    do while (associated(parcbox2))
                        if (associated(parcbox2,tempbox)) exit
                        deallocate(parcbox2%r)
                        parcbox2 => parcbox2%next
                    enddo
                    boxlib => tempbox
                    call Agrif_Add_Rectangle(newB2,boxlib)
                    return
                endif
            endif
        endif
        return
    endif
!
    do i = 1,Agrif_Probdim
        istr(i) = oldB%imax(i)
        islice(i) = oldB%imin(i)
    enddo
!
                              call Agrif_Clusterslice(i_sig,islice(1),istr(1))
    if ( Agrif_probdim >= 2 ) call Agrif_Clusterslice(j_sig,islice(2),istr(2))
    if ( Agrif_probdim == 3 ) call Agrif_Clusterslice(k_sig,islice(3),istr(3))
!
    ValSum = 0
    do i = 1,Agrif_Probdim
        Valsum = valSum + islice(i)
    enddo
!
    if ( Valsum == -Agrif_Probdim ) then
        call Agrif_Add_Rectangle(oldB,boxlib)
        return
    endif
!
    nullify(tempbox)
    tempbox => boxlib
    if ( Agrif_probdim == 1 ) cureff = (oldB%imax(1)-oldB%imin(1)+1)
    if ( Agrif_probdim == 2 ) cureff = (oldB%imax(1)-oldB%imin(1)+1) * &
                                       (oldB%imax(2)-oldB%imin(2)+1)
    if ( Agrif_probdim == 3 ) cureff = (oldB%imax(1)-oldB%imin(1)+1) * &
                                       (oldB%imax(2)-oldB%imin(2)+1) * &
                                       (oldB%imax(3)-oldB%imin(3)+1)
    nullify(parcbox)
!
    do i = 1,Agrif_Probdim
        newB%imax(i) = oldB%imax(i)
        newB%imin(i) = oldB%imin(i)
    enddo
!
    ValMax = 0
    do i = 1 , Agrif_Probdim
        ValMax = Max(ValMax,istr(i))
    enddo
!
    if (istr(1) == ValMax ) then
        newB%imax(1) = islice(1)
        call Agrif_Add_Rectangle(newB,parcbox)
        newB%imin(1) = islice(1)+1
        newB%imax(1) = oldB%imax(1)
        call Agrif_Add_Rectangle(newB,parcbox)
    else if ( Agrif_probdim >= 2 ) then
        if (istr(2) == ValMax ) then
            newB%imax(2) = islice(2)
            call Agrif_Add_Rectangle(newB,parcbox)
            newB%imin(2) = islice(2)+1
            newB%imax(2) = oldB%imax(2)
            call Agrif_Add_Rectangle(newB,parcbox)
        else if ( Agrif_probdim == 3 ) then
            newB%imax(3) = islice(3)
            call Agrif_Add_Rectangle(newB,parcbox)
            newB%imin(3) = islice(3)+1
            newB%imax(3) = oldB%imax(3)
            call Agrif_Add_Rectangle(newB,parcbox)
        endif
    endif
!
    do while ( associated(parcbox) )
        newB = parcbox%r
!
        if ( Agrif_probdim == 1 ) nbpoints = SUM(flag%iarray1(newB%imin(1):newB%imax(1)))
        if ( Agrif_probdim == 2 ) nbpoints = SUM(flag%iarray2(newB%imin(1):newB%imax(1),    &
                                                              newB%imin(2):newB%imax(2)))
        if ( Agrif_probdim == 3 ) nbpoints = SUM(flag%iarray3(newB%imin(1):newB%imax(1),    &
                                                              newB%imin(2):newB%imax(2),    &
                                                              newB%imin(3):newB%imax(3)))
!
        if ( Agrif_probdim == 1 ) TailleTab = (newB%imax(1)-newB%imin(1)+1)
        if ( Agrif_probdim == 2 ) TailleTab = (newB%imax(1)-newB%imin(1)+1) * &
                                              (newB%imax(2)-newB%imin(2)+1)
        if ( Agrif_probdim == 3 ) TailleTab = (newB%imax(1)-newB%imin(1)+1) * &
                                              (newB%imax(2)-newB%imin(2)+1) * &
                                              (newB%imax(3)-newB%imin(3)+1)
        neweff = REAL(nbpoints) / TailleTab
!
        if ( nbpoints > 0 ) then
!
            if ( (neweff > Agrif_efficiency)) then
                call Agrif_Add_Rectangle(newB,boxlib)
            else
                tempbox => boxlib
                newB2 = newB
                call Agrif_Clusternd(flag,boxlib,newB)
!
!               Compute new efficiency
                cureff = neweff
                parcbox2 => boxlib
                nbpoints = 0
                nbpointsflag = 0
!
                do while (associated(parcbox2))
                    if (associated(parcbox2,tempbox)) exit
                    newB = parcbox2%r
!
                    if ( Agrif_probdim == 1 ) ValSum = SUM(flag%iarray1(newB%imin(1):newB%imax(1)))
                    if ( Agrif_probdim == 2 ) ValSum = SUM(flag%iarray2(newB%imin(1):newB%imax(1),  &
                                                                        newB%imin(2):newB%imax(2)))
                    if ( Agrif_probdim == 3 ) ValSum = SUM(flag%iarray3(newB%imin(1):newB%imax(1),  &
                                                                        newB%imin(2):newB%imax(2),  &
                                                                        newB%imin(3):newB%imax(3)))
                    nbpointsflag = nbpointsflag + ValSum
!
                    if ( Agrif_probdim == 1 ) TailleTab = (newB%imax(1)-newB%imin(1)+1)
                    if ( Agrif_probdim == 2 ) TailleTab = (newB%imax(1)-newB%imin(1)+1) * &
                                                          (newB%imax(2)-newB%imin(2)+1)
                    if ( Agrif_probdim == 3 ) TailleTab = (newB%imax(1)-newB%imin(1)+1) * &
                                                          (newB%imax(2)-newB%imin(2)+1) * &
                                                          (newB%imax(3)-newB%imin(3)+1)
                    nbpoints = nbpoints + TailleTab
                    parcbox2 => parcbox2%next
                enddo
!
                if  ( REAL(nbpointsflag)/REAL(nbpoints) < (1.15*cureff)) then
                    parcbox2 => boxlib
                    do while (associated(parcbox2))
                        if (associated(parcbox2,tempbox)) exit
                        deallocate(parcbox2%r)
                        parcbox2 => parcbox2%next
                    enddo
                    boxlib => tempbox
                    call Agrif_Add_Rectangle(newB2,boxlib)
                endif
            endif
        endif
        parcbox => parcbox%next
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Clusternd
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Clusterslice
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Clusterslice ( sig, slice, str )
!---------------------------------------------------------------------------------------------------
    INTEGER,                       intent(inout) :: slice
    INTEGER,                       intent(inout) :: str
    INTEGER, DIMENSION(slice:str), intent(in)    :: sig
!
    INTEGER                         :: ideb, ifin
    INTEGER                         :: i, t, a, di, ts
    INTEGER, DIMENSION(slice:str)   :: lap
!
    ideb = slice
    ifin = str
!
    if (SIZE(sig) <= 2*Agrif_Minwidth) then
        str = -1
        slice = -1
        return
    endif
!
    t = -1
    a = -1
!
    do i = ideb+Agrif_Minwidth-1,ifin-Agrif_Minwidth
        if (sig(i) == 0) then
            if ((i-ideb) < (ifin-i)) then
                di = i - ideb
            else
                di = ifin - i
            endif
!
            if (di > t) then
                a = i
                t = di
            endif
        endif
    enddo
!
    if (a /= -1) then
        slice = a
        str = t
        return
    endif
!
    t = -1
    a = -1
!
    do i = ideb+1,ifin-1
        lap(i) = sig(i+1) + sig(i-1) - 2*sig(i)
    enddo
!
    do i = ideb + Agrif_Minwidth-1,ifin-Agrif_Minwidth
        if ((lap(i+1)*lap(i)) <= 0) then
            ts = ABS(lap(i+1) - lap(i))
            if (ts > t) then
                t = ts
                a = i
            endif
       endif
    enddo
!
    if (a == (ideb + Agrif_Minwidth - 1)) then
        a = -1
        t = -1
    endif
!
    slice = a
    str = t
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Clusterslice
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Clusterprune
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Clusterprune ( sig, pl, pu )
!---------------------------------------------------------------------------------------------------
    INTEGER,                   intent(inout)    :: pl, pu
    INTEGER, DIMENSION(pl:pu), intent(in)       :: sig
!
    INTEGER :: ideb, ifin
    INTEGER :: diff, addl, addu, udist, ldist
!
    ideb = pl
    ifin = pu
!
    if (SIZE(sig) <= Agrif_Minwidth) return
!
    do while ((sig(pl) == 0) .AND. (pl < ifin))
        pl = pl + 1
    enddo
!
    do while ((sig(pu) == 0) .AND. (pu > ideb))
        pu = pu - 1
    enddo
!
    if ( (pu-pl) < Agrif_Minwidth ) then
        diff = Agrif_Minwidth - (pu - pl + 1)
        udist = ifin - pu
        ldist = pl - ideb
        addl = diff / 2
        addu = diff - addl
        if (addu > udist) then
            addu = udist
            addl = diff - addu
        endif
!
        if (addl > ldist) then
            addl = ldist
            addu = diff - addl
        endif
!
        pu = pu + addu
        pl = pl - addl
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Clusterprune
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Add_Rectangle
!
!> Adds the Agrif_Rectangle R in a list managed by LR.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Add_Rectangle ( R, LR )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Rectangle)           :: R
    TYPE(Agrif_LRectangle), pointer :: LR
!
    TYPE(Agrif_LRectangle), pointer :: newrect
!
    integer                         :: i
!
    allocate(newrect)
    allocate(newrect % r)
!
    newrect % r = R
!
    do i = 1,Agrif_Probdim
        newrect % r % spaceref(i) = Agrif_Coeffref(i)
        newrect % r % timeref(i)  = Agrif_Coeffreft(i)
    enddo
!
    newrect % r % number = -1
    newrect % next => LR
    LR => newrect
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Add_Rectangle
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Copy_Rectangle
!
!> Creates and returns a copy of Agrif_Rectangle R.
!---------------------------------------------------------------------------------------------------
function Agrif_Copy_Rectangle ( R, expand ) result( copy )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Rectangle), pointer, intent(in) :: R
    integer, optional,              intent(in) :: expand
!
    TYPE(Agrif_Rectangle), pointer :: copy
!
    allocate(copy)
!
    copy = R
    if ( present(expand) ) then
        copy % imin = copy % imin - expand
        copy % imax = copy % imax + expand
    endif
    copy % childgrids => R % childgrids
!---------------------------------------------------------------------------------------------------
end function Agrif_Copy_Rectangle
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Read_Fix_Grd
!
!> Creates the grid hierarchy from the reading of the AGRIF_FixedGrids.in file.
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Read_Fix_Grd ( parent_rect, j, nunit )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Rectangle), pointer   :: parent_rect !< Pointer on the first grid of the grid hierarchy
    INTEGER                          :: j          !< Number of the new grid
    INTEGER                          :: nunit      !< unit associated with file
!
    TYPE(Agrif_Rectangle)            :: newrect    ! Pointer on a new grid
    TYPE(Agrif_LRectangle), pointer  :: parcours   ! Pointer for the recursive procedure
    TYPE(Agrif_LRectangle), pointer  :: newlrect
    TYPE(Agrif_LRectangle), pointer  :: end_list
    INTEGER                          :: i,n        ! for each child grid
    INTEGER                          :: nb_grids   ! Number of child grids
!
!   Reading of the number of child grids
    read(nunit,*,end=99) nb_grids
!
    allocate(end_list)
!
    parent_rect % childgrids => end_list
!
!   Reading of imin(1),imax(1),imin(2),imax(2),imin(3),imax(3), and space and
!        time refinement factors for each child grid.
!   Creation and addition of the new grid to the grid hierarchy.
!
    do n = 1,nb_grids
!
        allocate(newlrect)
        newrect % number = j   ! Number of the grid
!
        if ( Agrif_USE_ONLY_FIXED_GRIDS == 0 ) then
            if (Agrif_Probdim == 3) then
                read(nunit,*) newrect % imin(1), newrect % imax(1), &
                              newrect % imin(2), newrect % imax(2), &
                              newrect % imin(3), newrect % imax(3), &
                              newrect % spaceref(1), newrect % spaceref(2), newrect % spaceref(3), &
                              newrect % timeref(1), newrect % timeref(2), newrect % timeref(3)
            elseif (Agrif_Probdim == 2) then
                read(nunit,*) newrect % imin(1), newrect % imax(1), &
                              newrect % imin(2), newrect % imax(2), &
                              newrect % spaceref(1), newrect % spaceref(2), &
                              newrect % timeref(1),  newrect % timeref(2)
            elseif (Agrif_Probdim == 1) then
                read(nunit,*) newrect % imin(1), newrect % imax(1), &
                              newrect % spaceref(1), &
                              newrect % timeref(1)
            endif
        else
            if (Agrif_Probdim == 3) then
                read(nunit,*) newrect % imin(1), newrect % imax(1), &
                              newrect % imin(2), newrect % imax(2), &
                              newrect % imin(3), newrect % imax(3), &
                              newrect % spaceref(1), newrect % spaceref(2), newrect % spaceref(3), &
                              newrect % timeref(1)
            elseif (Agrif_Probdim == 2) then
                read(nunit,*) newrect % imin(1), newrect % imax(1), &
                              newrect % imin(2), newrect % imax(2), &
                              newrect % spaceref(1), newrect % spaceref(2), &
                              newrect % timeref(1)
            elseif (Agrif_Probdim == 1) then
                read(nunit,*) newrect % imin(1), newrect % imax(1), &
                              newrect % spaceref(1), &
                              newrect % timeref(1)
            endif
!
            if ( Agrif_probdim >= 2 ) then
                do i = 2,Agrif_probdim
                    newrect % timeref(i) = newrect % timeref(1)
                enddo
            endif
!
        endif
!
!       Addition to the grid hierarchy
!
        j = j + 1
        allocate(newlrect%r)
        newlrect % r = newrect
        end_list % next => newlrect
        end_list => end_list % next
!
    enddo
!
    parent_rect % childgrids => parent_rect % childgrids % next
    parcours => parent_rect % childgrids
!
!   recursive operation to create the grid hierarchy branch by branch
!
    do while ( associated(parcours) )
        call Agrif_Read_Fix_Grd(parcours % r, j, nunit)
        parcours => parcours % next
    enddo
99  continue
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Read_Fix_Grd
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Create_Grids
!
!> Creates the grid hierarchy (g) from the one created with the #Agrif_Read_Fix_Grd or
!! #Agrif_Cluster_All procedures (parent_rect).
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Create_Grids ( parent_grid, parent_rect )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Grid)     , pointer  :: parent_grid  !< Pointer on the root coarse grid
    TYPE(Agrif_Rectangle), pointer  :: parent_rect  !< Pointer on the root coarse grid of the grid hierarchy
                                                    !!   created with the #Agrif_Read_Fix_Grd subroutine
!
    TYPE(Agrif_Grid)      , pointer :: child_grid   ! Newly created child grid
    TYPE(Agrif_PGrid)     , pointer :: child_grid_p
    TYPE(Agrif_LRectangle), pointer :: child_rect_p
    type(Agrif_Rectangle),  pointer :: child_rect
!
    INTEGER                         :: i
    INTEGER, save                   :: moving_grid_id = 0
!
    child_rect_p => parent_rect % childgrids
!
!   Creation of the grid hierarchy from the one created by using the Agrif_Read_Fix_Grd subroutine
!
    do while ( associated(child_rect_p) )
!
        child_rect => child_rect_p % r
!
        allocate(child_grid)
!
!       Pointer on the parent grid
        child_grid % parent => parent_grid
        child_grid % rect_in_parent => Agrif_Copy_Rectangle(child_rect_p % r, expand=Agrif_Extra_Boundary_Cells)
!
        moving_grid_id = moving_grid_id+1
        child_grid % grid_id = moving_grid_id
!
        do i = 1,Agrif_Probdim
            child_grid % spaceref(i) = child_rect % spaceref(i)
            child_grid % timeref(i)  = child_rect % timeref(i)
            child_grid % nb(i) = (child_rect % imax(i) - child_rect % imin(i)) * child_rect % spaceref(i)
            child_grid % ix(i) =  child_rect % imin(i)
            child_grid % Agrif_dt(i) = parent_grid % Agrif_dt(i) / REAL(child_grid % timeref(i))
            child_grid % Agrif_dx(i) = parent_grid % Agrif_dx(i) / REAL(child_grid % spaceref(i))
            child_grid % Agrif_x(i)  = parent_grid % Agrif_x(i) + &
                                            (child_rect % imin(i) - 1) * parent_grid % Agrif_dx(i)
        enddo
!
!       Size of the grid in terms of cpu cost (nx*ny*timeref)
        child_grid % size = product( child_grid % nb(1:Agrif_Probdim) ) * child_grid % timeref(1)
!
!       Level of the current grid
        child_grid % level = child_grid % parent % level + 1
        if (child_grid % level > Agrif_MaxLevelLoc) then
          Agrif_MaxLevelLoc = child_grid%level
        endif
!
!       Number of the grid pointed by child_grid
        child_grid % fixedrank = child_rect % number
!
!       Grid pointed by child_grid is a fixed grid
        child_grid % fixed = ( child_grid % fixedrank > 0 )
!
!       Update the total number of fixed grids
        if (child_grid % fixed) then
            Agrif_nbfixedgrids = Agrif_nbfixedgrids + 1
        endif
!
!       Initialize integration counter
        child_grid % ngridstep = 0
!
!       Test indicating if the current grid has a common border with the root
!       coarse grid in the x direction
        do i = 1 , Agrif_Probdim
!
            child_grid % NearRootBorder(i) = (                  &
                (child_grid % parent % NearRootBorder(i)) .AND. &
                (child_grid % ix(i) == 1) )
!
            child_grid % DistantRootBorder(i) = (                  &
                (child_grid % parent % DistantRootBorder(i)) .AND. &
                (child_grid % ix(i) + (child_grid%nb(i)/child_grid%spaceref(i))-1  == child_grid % parent % nb(i)) )
        enddo
!
!       Writing in output files
        child_grid % oldgrid = .FALSE.
!
#if defined AGRIF_MPI
        child_grid % communicator = parent_grid % communicator
#endif
!
!       Definition of the characteristics of the variable of the grid pointed by child_grid
        call Agrif_Create_Var(child_grid)
!
!       Addition of this grid to the grid hierarchy
        call Agrif_gl_append( parent_grid % child_list, child_grid )
!
        child_rect_p => child_rect_p % next
!
    enddo
!
!   Recursive call to the subroutine Agrif_Create_Fixed_Grids to create the grid hierarchy
!
    child_grid_p => parent_grid % child_list % first
    child_rect_p => parent_rect % childgrids
!
    do while ( associated(child_rect_p) )
        call Agrif_Create_Grids( child_grid_p % gr, child_rect_p % r )
        child_grid_p => child_grid_p % next
        child_rect_p => child_rect_p % next
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Create_Grids
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Init_Hierarchy
!
!> Initializes all the grids except the root coarse grid (this one, pointed by Agrif_Types::Agrif_Mygrid, is
!! initialized by the subroutine Agrif_Util#Agrif_Init_Grids defined in the module Agrif_Util and
!! called in the main program).
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Init_Hierarchy ( g, procname )
!---------------------------------------------------------------------------------------------------
    use Agrif_seq
!
    type(Agrif_Grid), pointer       :: g            !< Pointer on the current grid
    procedure(init_proc), optional  :: procname     !< Initialisation subroutine (Default: Agrif_InitValues)
!
    TYPE(Agrif_PGrid), pointer :: parcours  ! Pointer for the recursive call
    LOGICAL                    :: Init_Hierarchy
!
! Initialise the grand mother grid (if any)
!
    if ( associated(g, Agrif_Mygrid) .and. agrif_coarse ) then
        call Agrif_Instance(Agrif_Coarsegrid)
        call Agrif_Allocation(Agrif_Coarsegrid)
        call Agrif_initialisations(Agrif_Coarsegrid)
        call Agrif_InitWorkspace()
!
!       Initialization by interpolation (this routine is written by the user)
        if (present(procname)) Then
            call procname()
        else
            call Agrif_InitValues()
        endif
        call Agrif_Instance(Agrif_Mygrid)
    endif

    parcours => g % child_list % first
!
    do while ( associated(parcours) )
!
        Init_Hierarchy = .false.
        if ( Agrif_USE_FIXED_GRIDS == 1 .OR. Agrif_USE_ONLY_FIXED_GRIDS == 1 ) then
            if ( (parcours%gr%fixed) .AND. (Agrif_Mygrid%ngridstep == 0) ) then
                Init_Hierarchy = .true.
            endif
        endif
!
        if (.NOT. parcours % gr % fixed) Init_Hierarchy = .true.
        if (parcours % gr % oldgrid)     Init_Hierarchy = .false.
!
        if (Init_Hierarchy) then
!
!           Instanciation of the grid pointed by parcours%gr and its variables
            call Agrif_Instance(parcours % gr)
!
!           Allocation of the arrays containing values of the variables of the
!           grid pointed by parcours%gr
!
            call Agrif_Allocation(parcours % gr)
            call Agrif_initialisations(parcours % gr)
!
            if ( Agrif_USE_ONLY_FIXED_GRIDS == 0 ) then
!               Initialization by copy of the grids created by clustering
                call Agrif_Allocate_Restore(parcours % gr)
                call Agrif_CopyFromOld_All(parcours % gr, Agrif_oldmygrid)
            endif
!
!           Initialization by interpolation (this routine is written by the user)
            call Agrif_InitWorkSpace()
            if (present(procname)) Then
                call procname()
            else
                call Agrif_InitValues()
            endif
!
            if ( Agrif_USE_ONLY_FIXED_GRIDS == 0 ) then
                call Agrif_Free_Restore(parcours % gr)
            endif
!
        endif
!
        parcours => parcours % next
!
    enddo
!
    parcours => g % child_list % first
!
!   recursive operation to initialize all the grids
    do while ( associated(parcours) )
        call Agrif_Init_Hierarchy(parcours%gr,procname)
        parcours => parcours%next
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Init_Hierarchy
!===================================================================================================
!
#if defined AGRIF_MPI
!===================================================================================================
!  subroutine Agrif_Init_Hierarchy_Parallel_1
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Init_Hierarchy_Parallel_1 ( g )
!---------------------------------------------------------------------------------------------------
    use Agrif_seq
!
    type(Agrif_Grid), pointer       :: g            !< Pointer on the current grid
!
    TYPE(Agrif_PGrid), pointer :: parcours  ! Pointer for the recursive call
    LOGICAL                    :: Init_Hierarchy
!
    parcours => g % child_list % first
!
    do while ( associated(parcours) )
!
        Init_Hierarchy = .false.
        if ( Agrif_USE_FIXED_GRIDS == 1 .OR. Agrif_USE_ONLY_FIXED_GRIDS == 1 ) then
            if ( (parcours%gr%fixed) .AND. (Agrif_Mygrid%ngridstep == 0) ) then
                Init_Hierarchy = .true.
            endif
        endif
!
        if (.NOT. parcours % gr % fixed) Init_Hierarchy = .true.
        if (parcours % gr % oldgrid)     Init_Hierarchy = .false.
!
        if (Init_Hierarchy) then
!
!           Instanciation of the grid pointed by parcours%gr and its variables
            call Agrif_Instance(parcours % gr)
!
!           Allocation of the arrays containing values of the variables of the
!           grid pointed by parcours%gr
!
            call Agrif_Allocation(parcours % gr)
            call Agrif_initialisations(parcours % gr)
!
        endif
!
        parcours => parcours % next
!
    enddo
!
    parcours => g % child_list % first
!
!   recursive operation to initialize all the grids
    do while ( associated(parcours) )
        call Agrif_Init_Hierarchy_Parallel_1(parcours%gr)
        parcours => parcours%next
    enddo
!
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Init_Hierarchy_Parallel_1
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Init_Hierarchy_Parallel_2
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Init_Hierarchy_Parallel_2 ( g, procname )
!---------------------------------------------------------------------------------------------------
    use Agrif_seq
!
    type(Agrif_Grid),     pointer   :: g            !< Pointer on the current grid
    procedure(init_proc), optional  :: procname     !< Initialisation subroutine (Default: Agrif_InitValues)
!
    type(Agrif_PGrid), pointer :: parcours  ! Pointer for the recursive call
    integer :: is
!
    call Agrif_Instance(g)
    call Agrif_seq_init_sequences( g )
!
    if ( .not. associated(g % child_seq) ) return
!
    do is = 1, g % child_seq % nb_seqs
!
        parcours => Agrif_seq_select_child(g,is)
!
!     Instanciation of the variables of the current grid
        call Agrif_Instance(parcours % gr)
!
!     Initialization by interpolation (this routine is written by the user)
        if (present(procname)) Then
            call procname()
        else
            call Agrif_InitValues()
        endif
!
        call Agrif_Init_ProcList(parcours % gr % proc_def_list, &
                                 parcours % gr % proc_def_in_parent_list % nitems)
!
        call Agrif_Init_Hierarchy_Parallel_2(parcours%gr,procname)
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Init_Hierarchy_Parallel_2
!===================================================================================================
#endif
!
!===================================================================================================
!  subroutine Agrif_Allocate_Restore
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Allocate_Restore ( Agrif_Gr )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Grid), pointer  :: Agrif_Gr   !< Pointer on the root coarse grid
!
    integer :: i
!
    do i = 1,Agrif_NbVariables(0)
!
        if ( Agrif_Mygrid%tabvars(i) % restore ) then
            if ( Agrif_Gr%tabvars(i) % nbdim == 1 ) then
                allocate( Agrif_Gr%tabvars(i)%Restore1D( &
                    lbound(Agrif_Gr%tabvars(i)%array1,1):&
                    ubound(Agrif_Gr%tabvars(i)%array1,1)))
                Agrif_Gr%tabvars(i)%Restore1D = 0
            endif
            if ( Agrif_Gr%tabvars(i) % nbdim == 2 ) then
                allocate( Agrif_Gr%tabvars(i)%Restore2D( &
                    lbound(Agrif_Gr%tabvars(i)%array2,1):&
                    ubound(Agrif_Gr%tabvars(i)%array2,1),&
                    lbound(Agrif_Gr%tabvars(i)%array2,2):&
                    ubound(Agrif_Gr%tabvars(i)%array2,2)))
                Agrif_Gr%tabvars(i)%Restore2D = 0
            endif
            if ( Agrif_Mygrid%tabvars(i) % nbdim == 3 ) then
                allocate( Agrif_Gr%tabvars(i)%Restore3D( &
                    lbound(Agrif_Gr%tabvars(i)%array3,1):&
                    ubound(Agrif_Gr%tabvars(i)%array3,1),&
                    lbound(Agrif_Gr%tabvars(i)%array3,2):&
                    ubound(Agrif_Gr%tabvars(i)%array3,2),&
                    lbound(Agrif_Gr%tabvars(i)%array3,3):&
                    ubound(Agrif_Gr%tabvars(i)%array3,3)))
                Agrif_Gr%tabvars(i)%Restore3D = 0
            endif
        endif
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Allocate_Restore
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Free_Restore
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Free_Restore ( Agrif_Gr )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Grid), pointer  :: Agrif_Gr   !< Pointer on the root coarse grid
!
    TYPE(Agrif_Variable), pointer :: var
    integer                       :: i
!
    do i = 1,Agrif_NbVariables(0)
!
        if ( Agrif_Mygrid % tabvars(i) % restore ) then
!
            var = Agrif_Gr % tabvars(i)
!
            if (associated(var%Restore1D)) deallocate(var%Restore1D)
            if (associated(var%Restore2D)) deallocate(var%Restore2D)
            if (associated(var%Restore3D)) deallocate(var%Restore3D)
            if (associated(var%Restore4D)) deallocate(var%Restore4D)
            if (associated(var%Restore5D)) deallocate(var%Restore5D)
            if (associated(var%Restore6D)) deallocate(var%Restore6D)
!
        endif
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Free_Restore
!===================================================================================================
!
end module Agrif_Clustering
