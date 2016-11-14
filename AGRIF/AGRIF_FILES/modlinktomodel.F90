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
!> Module Agrif_Link
!>
!> This module is used to link AGRIF files to the model.
!
module Agrif_Link
!
    interface
!
        subroutine Agrif_clustering_def ( )
!
        end subroutine Agrif_clustering_def
!
        subroutine Agrif_Set_numberofcells ( Agrif_Gr )
            use Agrif_Grids, only : Agrif_Grid
            type(Agrif_Grid), pointer :: Agrif_Gr   !< Pointer on the current grid
        end subroutine Agrif_Set_numberofcells
!
        subroutine Agrif_Get_numberofcells ( Agrif_Gr )
            use Agrif_Grids, only : Agrif_Grid
            type(Agrif_Grid), pointer :: Agrif_Gr   !< Pointer on the current grid
        end subroutine Agrif_Get_numberofcells
!
    end interface
!
    abstract interface
!
        subroutine alloc_proc ( Agrif_Gr )
            use Agrif_Grids, only : Agrif_Grid
            type(Agrif_Grid), pointer :: Agrif_Gr   !< Pointer on the current grid
        end subroutine alloc_proc
!
        subroutine typedef_proc ( )
            implicit none
        end subroutine typedef_proc
!
    end interface
    
    procedure(alloc_proc)   :: Agrif_Allocationcalls
    procedure(typedef_proc) :: Agrif_probdim_modtype_def
!
end module Agrif_Link
!
!===================================================================================================
!  function Agrif_parent
!        modify by conv. To use : un_parent = Agrif_Parent(un)
!===================================================================================================
!  function Agrif_Get_Coarse_Grid
!        modify by conv. To use : un_Mygrid = Agrif_Get_Coarse_grid(un)
!===================================================================================================
!  function Agrif_Rhox
!        modify by conv. To use : var = Agrif_Rhox()
!                    REAL(Agrif_Curgrid % spaceref(1))
!===================================================================================================
!  function Agrif_Parent_Rhox
!        modify by conv. To use : var = Agrif_Parent_Rhox()
!                    REAL(Agrif_Curgrid % parent % spaceref(1))
!===================================================================================================
!  function Agrif_Irhox
!        modify by conv. To use : var = Agrif_Parent_IRhox()
!                    Agrif_Curgrid % spaceref(1)
!===================================================================================================
!  function Agrif_Rhoy
!        modify by conv. To use : var = Agrif_Rhoy()
!                    REAL(Agrif_Curgrid % spaceref(2))
!===================================================================================================
!  function Agrif_Parent_Rhoy
!        modify by conv. To use : var = Agrif_Parent_Rhoy()
!                    REAL(Agrif_Curgrid % parent % spaceref(2))
!===================================================================================================
!  function Agrif_Irhoy
!        modify by conv. To use : var = Agrif_Parent_IRhoy()
!                    Agrif_Curgrid % spaceref(2)
!===================================================================================================


!===================================================================================================
!  function Agrif_Rhoz
!        modify by conv. To use : var = Agrif_Rhoz()
!                    REAL(Agrif_Curgrid % spaceref(3))
!===================================================================================================
!  function Agrif_Parent_Rhoz
!        modify by conv. To use : var = Agrif_Parent_Rhoz()
!                    REAL(Agrif_Curgrid % parent % spaceref(3))
!===================================================================================================
!  function Agrif_Irhoz
!        modify by conv. To use : var = Agrif_Parent_IRhoz()
!                    Agrif_Curgrid % spaceref(3)

!===================================================================================================
!  function Agrif_NearCommonBorderX
!        modify by conv. To use : var = Agrif_NearCommonBorderX()
!                       Agrif_Curgrid % NearRootBorder(1)
!===================================================================================================
!  function Agrif_NearCommonBorderY
!        modify by conv. To use : var = Agrif_NearCommonBorderY()
!                       Agrif_Curgrid % NearRootBorder(2)
!===================================================================================================
!  function Agrif_NearCommonBorderZ
!        modify by conv. To use : var = Agrif_NearCommonBorderZ()
!                       Agrif_Curgrid % NearRootBorder(3)
!===================================================================================================
!  function Agrif_DistantCommonBorderX
!        modify by conv. To use : var = Agrif_DistantCommonBorderX()
!                       Agrif_Curgrid % DistantRootBorder(1)
!===================================================================================================
!  function Agrif_DistantCommonBorderY
!        modify by conv. To use : var = Agrif_DistantCommonBorderY()
!                       Agrif_Curgrid % DistantRootBorder(2)
!===================================================================================================
!  function Agrif_DistantCommonBorderZ
!        modify by conv. To use : var = Agrif_DistantCommonBorderZ()
!                       Agrif_Curgrid % DistantRootBorder(3)
!===================================================================================================
!  function Agrif_Nb_Step
!        modify by conv. To use : var = Agrif_Nb_Step()
!                          Agrif_Curgrid % ngridstep
!===================================================================================================
!  function Agrif_Nb_Fine_Grids
!        modify by conv. To use : var = Agrif_Nb_Fine_Grids()
!                         Agrif_nbfixedgrids
!===================================================================================================
!  function Agrif_Ix
!        modify by conv. To use : var = Agrif_Ix()
!                         Agrif_Curgrid % ix(1)
!===================================================================================================
!  function Agrif_Parent_Ix
!        modify by conv. To use : var = Agrif_Parent_Ix()
!                        Agrif_Curgrid % parent % ix(1)
!===================================================================================================
!  function Agrif_Iy
!        modify by conv. To use : var = Agrif_Iy()
!                        Agrif_Curgrid % ix(2)
!===================================================================================================
!  function Agrif_Parent_Iy
!        modify by conv. To use : var = Agrif_Parent_Iy()
!                       Agrif_Curgrid % parent % ix(2)
!===================================================================================================
!  function Agrif_Iz
!        modify by conv. To use : var = Agrif_Iz()
!                      Agrif_Curgrid % ix(3)
!===================================================================================================
!  function Agrif_Parent_Iz
!        modify by conv. To use : var = Agrif_Parent_Iz()
!                     Agrif_Curgrid % parent % ix(3)
!===================================================================================================
!  function Agrif_Get_grid_id
!        modify by conv. To use : var = Agrif_Get_grid_id()
!                    Agrif_Curgrid % grid_id
!===================================================================================================
!  function Agrif_Get_parent_id
!        modify by conv. To use : var = Agrif_Get_parent_id()
!                    Agrif_Curgrid % parent % grid_id
!===================================================================================================
