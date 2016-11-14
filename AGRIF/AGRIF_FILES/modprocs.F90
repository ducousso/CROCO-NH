module Agrif_Procs
!
    implicit none
!
    type Agrif_Proc
        integer               :: pn       !< Proc index in coarse grid
        integer               :: pi       !< Proc index in x-direction (informative only, could be removed)
        integer               :: pj       !< Proc index in y-direction (informative only, could be removed)
        integer, dimension(3) :: imin
        integer, dimension(3) :: imax
        integer               :: nb_seqs = 0  !< Number of integration sequences the proc is attached to.
        integer               :: grid_id = 0  !< Grid id the proc is attached to.
    end type Agrif_Proc
!
    type Agrif_Proc_p
        type(Agrif_Proc),   pointer   :: proc => NULL()  !< Pointer to the actual proc structure
        type(Agrif_Proc_p), pointer   :: next => NULL()  !< Next proc in the list
    end type Agrif_Proc_p
!
    type Agrif_Proc_List
        integer                       :: nitems = 0      !< Number of elements in the list
        type(Agrif_Proc_p), pointer   :: first => NULL() !< First proc in the list
        type(Agrif_Proc_p), pointer   :: last  => NULL() !< Last proc inserted in the list
    end type Agrif_Proc_List
!
contains
!
!===================================================================================================
subroutine Agrif_pl_append ( proclist, proc )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Proc_List),     intent(inout)  :: proclist
    type(Agrif_Proc), pointer, intent(in)     :: proc
!
    type(Agrif_Proc_p), pointer   :: new_pp
!
    allocate( new_pp )
!
    new_pp % proc => proc
    new_pp % next => NULL()
!
    if ( associated(proclist % last) ) then
        ! the list is not empty, let 'proc' be the next after the last (ie. the last one).
        proclist % last % next => new_pp
    else
        ! the list has just been initialized. Let 'proc' be the first one.
        proclist % first => new_pp
    endif
    ! anyway, for next time 'proc' will be the last one. 
    proclist % last => new_pp
    proclist % nitems = proclist % nitems + 1
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_pl_append
!===================================================================================================
!
!===================================================================================================
subroutine Agrif_pl_print_array ( proclist )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Proc_List), intent(in) :: proclist
!
    type(Agrif_Proc_p), pointer :: pp
    type(Agrif_Proc),   pointer :: proc
!
    pp => proclist % first
!
    write(*,'("/-------+-----+-----+------+------+------+------+------\")')
    write(*,'("| iproc | ipx | ipy | imin | imax | jmin | jmax | grid |")')
    write(*,'("|-------+-----+-----+------+------+------+------+------|")')
    do while ( associated(pp) )
        proc => pp % proc
        write(*,'("|",i6," |",i4," |",i4," |",i5," :",i5," |",i5," :",i5," | ",i4," |")') &
            proc % pn, proc % pi, proc % pj, proc % imin(1), proc % imax(1), proc % imin(2), proc % imax(2), &
            proc % grid_id
        pp => pp % next
    enddo
    write(*,'("\-------+-----+-----+------+------+------+------+------/")')
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_pl_print_array
!===================================================================================================
!
!===================================================================================================
subroutine Agrif_pl_print ( proclist )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Proc_List), intent(in) :: proclist
!
    type(Agrif_Proc_p), pointer :: pp
!
    pp => proclist % first
    do while ( associated(pp) )
        write(*,'(i0,",")',advance='no') pp % proc % pn
        pp => pp % next
    enddo
    write(*,*)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_pl_print
!===================================================================================================
!
!===================================================================================================
subroutine Agrif_pl_copy ( proclist, copy )
!
!< Carries out a copy of 'proclist' into 'copy'
!---------------------------------------------------------------------------------------------------
    type(Agrif_Proc_List), intent(in)    :: proclist
    type(Agrif_Proc_List), intent(out)   :: copy
!
    type(Agrif_Proc_p),    pointer    :: pp
!
    call Agrif_pl_clear(copy)
!
    pp => proclist % first
    do while ( associated(pp) )
        call Agrif_pl_append( copy, pp % proc )
        pp => pp % next
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_pl_copy
!===================================================================================================
!
!===================================================================================================
subroutine Agrif_pl_deep_copy ( proclist, copy )
!
!< Carries out a deep copy of 'proclist' into 'copy'
!---------------------------------------------------------------------------------------------------
    type(Agrif_Proc_List), intent(in)    :: proclist
    type(Agrif_Proc_List), intent(out)   :: copy
!
    type(Agrif_Proc_p), pointer :: pp
    type(Agrif_Proc),   pointer :: new_proc
!
    call Agrif_pl_clear(copy)
!
    pp => proclist % first
    do while ( associated(pp) )
        allocate( new_proc )
        new_proc = pp % proc
        call Agrif_pl_append( copy, new_proc )
        pp => pp % next
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_pl_deep_copy
!===================================================================================================
!
!===================================================================================================
subroutine Agrif_pl_clear ( proclist )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Proc_List), intent(inout)  :: proclist
!
    type(Agrif_Proc_p), pointer    :: pp, ppd
!
    pp => proclist % first
!    
    do while( associated(pp) )
        ppd => pp
        pp  => pp % next
        deallocate(ppd)
    enddo
    
    proclist % first => NULL()
    proclist % last  => NULL()
    proclist % nitems = 0
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_pl_clear
!===================================================================================================
!
!===================================================================================================
subroutine Agrif_pl_to_array ( proclist, procarray )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Proc_List),                       intent(in)   :: proclist
    type(Agrif_Proc), dimension(:), allocatable, intent(out)  :: procarray
!
    type(Agrif_Proc_p), pointer   :: pp
!
    allocate( procarray(1:proclist % nitems) )
!
    pp => proclist % first
    do while ( associated(pp) )
        procarray(pp%proc%pn+1) = pp % proc
        pp => pp % next
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_pl_to_array
!===================================================================================================
!
!===================================================================================================
function Agrif_pl_search_proc ( proclist, rank ) result ( proc )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Proc_List), intent(in)   :: proclist
    integer,               intent(in)   :: rank
!
    type(Agrif_Proc_p), pointer :: pp
    type(Agrif_Proc),   pointer :: proc
    logical :: found
!
    found = .false.
    proc => NULL()
    pp => proclist % first
    do while ( .not.found .and. associated(pp) )
        if ( pp % proc % pn == rank ) then
            proc => pp % proc
            return
        else
            pp => pp % next
        endif
    enddo
!---------------------------------------------------------------------------------------------------
end function Agrif_pl_search_proc
!===================================================================================================
!
end module Agrif_Procs
