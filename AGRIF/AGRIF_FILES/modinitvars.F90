!
! $Id: modinitvars.F 662 2007-05-25 15:58:52Z opalod $
!
!     Agrif (Adaptive Grid Refinement In Fortran)
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
!> Module Agrif_Init_Vars
!>
!> Initialization of the variables of the current grid.
!
module Agrif_Init_Vars
!
    use Agrif_Types
    use Agrif_Grids
    use Agrif_Link
!
    implicit none
!
contains
!
!===================================================================================================
!  subroutine Agrif_Create_Var
!
!> Allocation of the list of grid variables for grid Agrif_Gr.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Create_Var ( Agrif_Gr )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), pointer   :: Agrif_Gr  !< Pointer on the current grid
!
    integer :: nb
!
    if (Agrif_NbVariables(0) > 0) allocate(Agrif_Gr % tabvars  (Agrif_NbVariables(0)))
    if (Agrif_NbVariables(1) > 0) allocate(Agrif_Gr % tabvars_c(Agrif_NbVariables(1)))
    if (Agrif_NbVariables(2) > 0) allocate(Agrif_Gr % tabvars_r(Agrif_NbVariables(2)))
    if (Agrif_NbVariables(3) > 0) allocate(Agrif_Gr % tabvars_l(Agrif_NbVariables(3)))
    if (Agrif_NbVariables(4) > 0) allocate(Agrif_Gr % tabvars_i(Agrif_NbVariables(4)))
!
    if ( Agrif_Gr % fixedrank /= 0 ) then
        do nb = 1, Agrif_NbVariables(0)
            Agrif_Gr % tabvars(nb) % parent_var => Agrif_Gr % parent % tabvars(nb)
            Agrif_Gr % tabvars(nb) % nbdim      =  Agrif_Mygrid % tabvars(nb) % nbdim
            Agrif_Gr % tabvars(nb) % root_var   => Agrif_Mygrid % tabvars(nb)
        enddo
        do nb = 1, Agrif_NbVariables(1)
            Agrif_Gr % tabvars_c(nb) % parent_var => Agrif_Gr % parent % tabvars_c(nb)
            Agrif_Gr % tabvars_c(nb) % nbdim      =  Agrif_Mygrid % tabvars_c(nb) % nbdim
            Agrif_Gr % tabvars_c(nb) % root_var   => Agrif_Mygrid % tabvars_c(nb)
        enddo
        do nb = 1, Agrif_NbVariables(2)
            Agrif_Gr % tabvars_r(nb) % parent_var => Agrif_Gr % parent % tabvars_r(nb)
            Agrif_Gr % tabvars_r(nb) % nbdim      =  Agrif_Mygrid % tabvars_r(nb) % nbdim
            Agrif_Gr % tabvars_r(nb) % root_var   => Agrif_Mygrid % tabvars_r(nb)
        enddo
        do nb = 1, Agrif_NbVariables(3)
            Agrif_Gr % tabvars_l(nb) % parent_var => Agrif_Gr % parent % tabvars_l(nb)
            Agrif_Gr % tabvars_l(nb) % nbdim      =  Agrif_Mygrid % tabvars_l(nb) % nbdim
            Agrif_Gr % tabvars_l(nb) % root_var   => Agrif_Mygrid % tabvars_l(nb)
        enddo
        do nb = 1, Agrif_NbVariables(4)
            Agrif_Gr % tabvars_i(nb) % parent_var => Agrif_Gr % parent % tabvars_i(nb)
            Agrif_Gr % tabvars_i(nb) % nbdim      =  Agrif_Mygrid % tabvars_i(nb) % nbdim
            Agrif_Gr % tabvars_i(nb) % root_var   => Agrif_Mygrid % tabvars_i(nb)
        enddo
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Create_Var
!===================================================================================================
!
end module Agrif_Init_Vars
