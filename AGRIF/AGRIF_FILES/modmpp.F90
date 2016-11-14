!
! $Id: modmpp.F 779 2007-12-22 17:04:17Z rblod $
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
module Agrif_Mpp
!
    use Agrif_Arrays
    use Agrif_Grids
!
    implicit none
!
    interface
        subroutine Agrif_get_proc_info ( imin, imax, jmin, jmax )
            integer, intent(out) :: imin, imax
            integer, intent(out) :: jmin, jmax
        end subroutine Agrif_get_proc_info
    end interface
!
    integer, private :: Agrif_MPI_prec
!
    private :: Agrif_get_proc_info
!
contains
!
#if defined AGRIF_MPI
!===================================================================================================
!  subroutine Agrif_MPI_Init
!---------------------------------------------------------------------------------------------------
subroutine Agrif_MPI_Init ( comm )
!---------------------------------------------------------------------------------------------------
    integer, optional, intent(in) :: comm    !< MPI communicator to be attached to the root grid.
!
    include 'mpif.h'
    integer :: code, ierr
    logical :: mpi_was_called
    integer :: current_mpi_prec
!
    call MPI_INITIALIZED( mpi_was_called, code )
    if( code /= MPI_SUCCESS ) then
        write(*,*) ': Error in routine mpi_initialized'
        call MPI_ABORT( MPI_COMM_WORLD, code, ierr )
    endif
    if( .not. mpi_was_called ) then
        write(*,*) '### AGRIF Error : you should call Agrif_MPI_Init *after* MPI_Init.'
        stop
    endif

    current_mpi_prec = KIND(1.0)
    if (current_mpi_prec == 4) then
      Agrif_MPI_prec = MPI_REAL4
    else
      Agrif_MPI_prec = MPI_REAL8
    endif
!
    if ( present(comm) ) then
        call Agrif_MPI_switch_comm(comm)
    else
        call Agrif_MPI_switch_comm(MPI_COMM_WORLD)
    endif
!
    Agrif_Mygrid % communicator = Agrif_mpi_comm
!
    if ( Agrif_Parallel_sisters ) then
        call Agrif_Init_ProcList( Agrif_Mygrid % proc_def_list, Agrif_Nbprocs )
        call Agrif_pl_copy( Agrif_Mygrid % proc_def_list, Agrif_Mygrid % required_proc_list )
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_MPI_Init
!===================================================================================================
!
!===================================================================================================
subroutine Agrif_MPI_switch_comm ( comm )
!---------------------------------------------------------------------------------------------------
    integer, intent(in) :: comm    !< MPI communicator you want to switch to.
!
    include 'mpif.h'
    integer :: code
    logical :: mpi_was_called
!
    call MPI_INITIALIZED( mpi_was_called, code )
    if ( .not. mpi_was_called ) return
!
    call MPI_COMM_SIZE(comm, Agrif_Nbprocs, code)
    call MPI_COMM_RANK(comm, Agrif_ProcRank, code)
    Agrif_mpi_comm = comm
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_MPI_switch_comm
!===================================================================================================
!
!===================================================================================================
function Agrif_MPI_get_grid_comm ( ) result ( comm )
!---------------------------------------------------------------------------------------------------
    integer :: comm
    comm = Agrif_Curgrid % communicator
!---------------------------------------------------------------------------------------------------
end function Agrif_MPI_get_grid_comm
!===================================================================================================
!
!===================================================================================================
subroutine Agrif_MPI_set_grid_comm ( comm )
!---------------------------------------------------------------------------------------------------
    integer, intent(in) :: comm
    Agrif_Curgrid % communicator = comm
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_MPI_set_grid_comm
!===================================================================================================
!
!===================================================================================================
subroutine Agrif_Init_ProcList ( proclist, nbprocs )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Proc_List), intent(inout) :: proclist
    integer,               intent(in)    :: nbprocs
!
    include 'mpif.h'
    type(Agrif_Proc), pointer     :: new_proc
    integer                       :: p, ierr
    integer                       :: imin, imax, jmin, jmax
    integer, dimension(5)         :: local_proc_grid_info
    integer, dimension(5,nbprocs) :: all_procs_grid_info
!
    call Agrif_get_proc_info(imin, imax, jmin, jmax)
!
    local_proc_grid_info(:) = (/Agrif_Procrank, imin, jmin, imax, jmax/)
!
    call MPI_ALLGATHER(local_proc_grid_info, 5, MPI_INTEGER, &
                       all_procs_grid_info,  5, MPI_INTEGER, Agrif_mpi_comm, ierr)
!
    do p = 1,nbprocs
!
        allocate(new_proc)
        new_proc % pn = all_procs_grid_info(1,p)
        new_proc % imin(1) = all_procs_grid_info(2,p)
        new_proc % imin(2) = all_procs_grid_info(3,p)
        new_proc % imax(1) = all_procs_grid_info(4,p)
        new_proc % imax(2) = all_procs_grid_info(5,p)
        call Agrif_pl_append( proclist, new_proc )
!
    enddo
!
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Init_ProcList
!===================================================================================================
!
!===================================================================================================
!  subroutine Get_External_Data_first
!---------------------------------------------------------------------------------------------------
subroutine Get_External_Data_first ( pttruetab, cetruetab, pttruetabwhole, cetruetabwhole,  &
                                     nbdim, memberoutall, coords, sendtoproc, recvfromproc, &
                                     imin, imax, imin_recv, imax_recv, bornesmin, bornesmax )
!---------------------------------------------------------------------------------------------------
    include 'mpif.h'
!
    integer,                                     intent(in)  :: nbdim
    integer, dimension(nbdim,0:Agrif_NbProcs-1), intent(in)  :: pttruetab,     cetruetab
    integer, dimension(nbdim,0:Agrif_NbProcs-1), intent(in)  :: pttruetabwhole,cetruetabwhole
    logical, dimension(0:Agrif_Nbprocs-1),       intent(in)  :: memberoutall
    integer, dimension(nbdim),                   intent(in)  :: coords
    logical, dimension(0:Agrif_Nbprocs-1),       intent(out) :: sendtoproc
    logical, dimension(0:Agrif_Nbprocs-1),       intent(out) :: recvfromproc
    integer, dimension(nbdim,0:Agrif_NbProcs-1), intent(out) :: imin,imax
    integer, dimension(nbdim,0:Agrif_NbProcs-1), intent(out) :: imin_recv,imax_recv
    integer, dimension(nbdim,0:Agrif_NbProcs-1), intent(in) :: bornesmin, bornesmax
!
    integer :: imintmp, imaxtmp, i, j, k, i1
    integer :: imin1,imax1
    logical :: tochange,tochangebis
    integer, dimension(nbdim,0:Agrif_NbProcs-1)    :: pttruetab2,cetruetab2
!
! pttruetab2 and cetruetab2 are modified arrays in order to always
! send the most inner points
!
    pttruetab2(:,Agrif_Procrank) = pttruetab(:,Agrif_Procrank)
    cetruetab2(:,Agrif_Procrank) = cetruetab(:,Agrif_Procrank)
!
    do k = 0,Agrif_Nbprocs-1
    do i = 1,nbdim
        tochangebis = .TRUE.
        DO i1 = 1,nbdim
            IF (i /= i1) THEN
                IF ( (pttruetab(i1,Agrif_Procrank) /= pttruetab(i1,k))  .OR. &
                     (cetruetab(i1,Agrif_Procrank) /= cetruetab(i1,k))) THEN
                    tochangebis = .FALSE.
                    EXIT
                ENDIF
            ENDIF
        ENDDO
        IF (tochangebis) THEN
            imin1 = max(pttruetab(i,Agrif_Procrank), pttruetab(i,k))
            imax1 = min(cetruetab(i,Agrif_Procrank), cetruetab(i,k))
! Always send the most interior points

            tochange = .false.
            IF (cetruetab(i,Agrif_Procrank) > cetruetab(i,k)) THEN
                DO j=imin1,imax1
                    IF ((bornesmax(i,k)-j) > (j-bornesmin(i,Agrif_Procrank))) THEN
                        imintmp = j+1
                        tochange = .TRUE.
                    ELSE
                        EXIT
                    ENDIF
                ENDDO
            ENDIF

            if (tochange) then
                pttruetab2(i,Agrif_Procrank) = imintmp
            endif

            tochange = .FALSE.
            imaxtmp=0
            IF (pttruetab(i,Agrif_Procrank) < pttruetab(i,k)) THEN
                DO j=imax1,imin1,-1
                    IF ((j-bornesmin(i,k)) > (bornesmax(i,Agrif_Procrank)-j)) THEN
                        imaxtmp = j-1
                        tochange = .TRUE.
                    ELSE
                        EXIT
                    ENDIF
                ENDDO
            ENDIF

            if (tochange) then
                cetruetab2(i,Agrif_Procrank) = imaxtmp
            endif
        ENDIF
    enddo
    enddo

    do k = 0,Agrif_NbProcs-1
!
        sendtoproc(k) = .true.
!
        IF ( .not. memberoutall(k) ) THEN
            sendtoproc(k) = .false.
        ELSE
!CDIR SHORTLOOP
        do i = 1,nbdim
            imin(i,k) = max(pttruetab2(i,Agrif_Procrank), pttruetabwhole(i,k))
            imax(i,k) = min(cetruetab2(i,Agrif_Procrank), cetruetabwhole(i,k))
!
            if ( (imin(i,k) > imax(i,k)) .and. (coords(i) /= 0) ) then
                sendtoproc(k) = .false.
            endif
        enddo
        ENDIF
    enddo
!
    call Exchangesamelevel_first(sendtoproc,nbdim,imin,imax,recvfromproc,imin_recv,imax_recv)
!---------------------------------------------------------------------------------------------------
end subroutine Get_External_Data_first
!===================================================================================================
!
!===================================================================================================
!  subroutine ExchangeSameLevel_first
!---------------------------------------------------------------------------------------------------
subroutine ExchangeSameLevel_first ( sendtoproc, nbdim, imin, imax, recvfromproc, &
                                     imin_recv, imax_recv )
!---------------------------------------------------------------------------------------------------
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1),       intent(in)  :: sendtoproc
    INTEGER,                                     intent(in)  :: nbdim
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1), intent(in)  :: imin
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1), intent(in)  :: imax
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1),       intent(out) :: recvfromproc
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1), intent(out) :: imin_recv
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1), intent(out) :: imax_recv
!
    include 'mpif.h'
    INTEGER :: k
    INTEGER :: etiquette = 100
    INTEGER :: code
    LOGICAL :: res
    INTEGER, DIMENSION(MPI_STATUS_SIZE)   :: statut
    INTEGER, DIMENSION(nbdim,2,0:Agrif_Nbprocs-1)    :: iminmax_temp

    do k = 0,Agrif_ProcRank-1
!
        call MPI_SEND(sendtoproc(k),1,MPI_LOGICAL,k,etiquette,Agrif_mpi_comm,code)
!
        if (sendtoproc(k)) then
            iminmax_temp(:,1,k) = imin(:,k)
            iminmax_temp(:,2,k) = imax(:,k)
            call MPI_SEND(iminmax_temp(:,:,k),2*nbdim,MPI_INTEGER,k,etiquette,Agrif_mpi_comm,code)
        endif
!
    enddo
!
!   Reception from others processors of the necessary part of the parent grid
    do k = Agrif_ProcRank+1,Agrif_Nbprocs-1
!
        call MPI_RECV(res,1,MPI_LOGICAL,k,etiquette,Agrif_mpi_comm,statut,code)
        recvfromproc(k) = res
!
        if (recvfromproc(k)) then
            call MPI_RECV(iminmax_temp(:,:,k),2*nbdim,MPI_INTEGER,k,etiquette, &
                    Agrif_mpi_comm,statut,code)
            imin_recv(:,k) = iminmax_temp(:,1,k)
            imax_recv(:,k) = iminmax_temp(:,2,k)
        endif
!
    enddo

!   Reception from others processors of the necessary part of the parent grid
    do k = Agrif_ProcRank+1,Agrif_Nbprocs-1
!
        call MPI_SEND(sendtoproc(k),1,MPI_LOGICAL,k,etiquette,Agrif_mpi_comm,code)
!
        if (sendtoproc(k)) then
!
            iminmax_temp(:,1,k) = imin(:,k)
            iminmax_temp(:,2,k) = imax(:,k)

            call MPI_SEND(iminmax_temp(:,:,k),2*nbdim,MPI_INTEGER,k,etiquette, &
                    Agrif_mpi_comm,code)
        endif
!
    enddo
!
!
!   Reception from others processors of the necessary part of the parent grid
    do k = Agrif_ProcRank-1,0,-1
!
        call MPI_RECV(res,1,MPI_LOGICAL,k,etiquette,Agrif_mpi_comm,statut,code)
        recvfromproc(k) = res
!
        if (recvfromproc(k)) then
!
            call MPI_RECV(iminmax_temp(:,:,k),2*nbdim,MPI_INTEGER,k,etiquette, &
                    Agrif_mpi_comm,statut,code)

            imin_recv(:,k) = iminmax_temp(:,1,k)
            imax_recv(:,k) = iminmax_temp(:,2,k)
        endif
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine ExchangeSamelevel_first
!===================================================================================================
!
!===================================================================================================
!  subroutine ExchangeSameLevel
!---------------------------------------------------------------------------------------------------
subroutine ExchangeSameLevel ( sendtoproc, recvfromproc, nbdim,    &
                               pttruetabwhole, cetruetabwhole,     &
                               imin, imax, imin_recv, imax_recv,   &
                               memberout, tempC, tempCextend )
!---------------------------------------------------------------------------------------------------
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1),       intent(in)    :: sendtoproc
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1),       intent(in)    :: recvfromproc
    INTEGER,                                     intent(in)    :: nbdim
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1), intent(in)    :: pttruetabwhole
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1), intent(in)    :: cetruetabwhole
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1), intent(in)    :: imin,      imax
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1), intent(in)    :: imin_recv, imax_recv
    LOGICAL,                                     intent(in)    :: memberout
    TYPE(Agrif_Variable), pointer,               intent(inout) :: tempC, tempCextend
!
    include 'mpif.h'
    INTEGER :: i,k
    INTEGER :: etiquette = 100
    INTEGER :: code, datasize
    INTEGER, DIMENSION(MPI_STATUS_SIZE)   :: statut
    TYPE(Agrif_Variable), pointer, SAVE   :: temprecv
!
    IF (memberout) THEN
        call Agrif_array_allocate(tempCextend, pttruetabwhole(:,Agrif_ProcRank),  &
                                               cetruetabwhole(:,Agrif_ProcRank),nbdim)
        call Agrif_var_set_array_tozero(tempCextend,nbdim)
    ENDIF
!
    IF (sendtoproc(Agrif_ProcRank)) THEN
        call Agrif_var_copy_array(tempCextend,imin(:,Agrif_Procrank),imax(:,Agrif_Procrank), &
                                  tempC,      imin(:,Agrif_Procrank),imax(:,Agrif_Procrank), &
                                  nbdim)
    ENDIF
!
    do k = 0,Agrif_ProcRank-1
!
        if (sendtoproc(k)) then
!
            datasize = 1
!
!CDIR SHORTLOOP
            do i = 1,nbdim
                datasize = datasize * (imax(i,k)-imin(i,k)+1)
            enddo
!
            SELECT CASE(nbdim)
            CASE(1)
                call MPI_SEND(tempC%array1(imin(1,k):imax(1,k)),    &
                        datasize,Agrif_MPI_prec,k,etiquette,      &
                        Agrif_mpi_comm,code)
            CASE(2)
                call MPI_SEND(tempC%array2(imin(1,k):imax(1,k),     &
                                           imin(2,k):imax(2,k)),    &
                        datasize,Agrif_MPI_prec,k,etiquette,      &
                        Agrif_mpi_comm,code)
            CASE(3)
                call Agrif_Send_3Darray(tempC%array3,lbound(tempC%array3),imin(:,k),imax(:,k),k)
            CASE(4)
                call MPI_SEND(tempC%array4(imin(1,k):imax(1,k),     &
                                           imin(2,k):imax(2,k),     &
                                           imin(3,k):imax(3,k),     &
                                           imin(4,k):imax(4,k)),    &
                        datasize,Agrif_MPI_prec,k,etiquette,      &
                        Agrif_mpi_comm,code)
            CASE(5)
                call MPI_SEND(tempC%array5(imin(1,k):imax(1,k),     &
                                           imin(2,k):imax(2,k),     &
                                           imin(3,k):imax(3,k),     &
                                           imin(4,k):imax(4,k),     &
                                           imin(5,k):imax(5,k)),    &
                        datasize,Agrif_MPI_prec,k,etiquette,      &
                        Agrif_mpi_comm,code)
            CASE(6)
                call MPI_SEND(tempC%array6(imin(1,k):imax(1,k),     &
                                           imin(2,k):imax(2,k),     &
                                           imin(3,k):imax(3,k),     &
                                           imin(4,k):imax(4,k),     &
                                           imin(5,k):imax(5,k),     &
                                           imin(6,k):imax(6,k)),    &
                        datasize,Agrif_MPI_prec,k,etiquette,      &
                        Agrif_mpi_comm,code)
            END SELECT
!
        endif
    enddo
!
!   Reception from others processors of the necessary part of the parent grid
    do k = Agrif_ProcRank+1,Agrif_Nbprocs-1
!
        if (recvfromproc(k)) then
!
            datasize = 1
!
!CDIR SHORTLOOP
            do i = 1,nbdim
                datasize = datasize * (imax_recv(i,k)-imin_recv(i,k)+1)
            enddo

            if (.not.associated(temprecv)) allocate(temprecv)
            call Agrif_array_allocate(temprecv,imin_recv(:,k),imax_recv(:,k),nbdim)

            SELECT CASE(nbdim)
            CASE(1)
                call MPI_RECV(temprecv%array1,datasize,Agrif_MPI_prec,k,etiquette, &
                        Agrif_mpi_comm,statut,code)
            CASE(2)
                call MPI_RECV(temprecv%array2,datasize,Agrif_MPI_prec,k,etiquette, &
                        Agrif_mpi_comm,statut,code)
            CASE(3)
                call MPI_RECV(temprecv%array3,datasize,Agrif_MPI_prec,k,etiquette, &
                        Agrif_mpi_comm,statut,code)
            CASE(4)
                call MPI_RECV(temprecv%array4,datasize,Agrif_MPI_prec,k,etiquette, &
                        Agrif_mpi_comm,statut,code)
            CASE(5)
                call MPI_RECV(temprecv%array5,datasize,Agrif_MPI_prec,k,etiquette, &
                        Agrif_mpi_comm,statut,code)
            CASE(6)
                call MPI_RECV(temprecv%array6,datasize,Agrif_MPI_prec,k,etiquette, &
                        Agrif_mpi_comm,statut,code)
            END SELECT

            call Agrif_var_replace_value(tempCextend,temprecv,imin_recv(:,k),imax_recv(:,k),0.,nbdim)
            call Agrif_array_deallocate(temprecv,nbdim)
!
        endif
    enddo

!   Reception from others processors of the necessary part of the parent grid
    do k = Agrif_ProcRank+1,Agrif_Nbprocs-1
!
        if (sendtoproc(k)) then
!
            SELECT CASE(nbdim)
            CASE(1)
                datasize=SIZE(tempC%array1(imin(1,k):imax(1,k)))
                call MPI_SEND(tempC%array1(imin(1,k):imax(1,k)),    &
                        datasize,Agrif_MPI_prec,k,etiquette,      &
                        Agrif_mpi_comm,code)
            CASE(2)
                datasize=SIZE(tempC%array2(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k)))
                call MPI_SEND(tempC%array2(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k)),    &
                        datasize,Agrif_MPI_prec,k,etiquette,      &
                        Agrif_mpi_comm,code)
            CASE(3)
                datasize=SIZE(tempC%array3(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k)))
                call MPI_SEND(tempC%array3(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k)),    &
                        datasize,Agrif_MPI_prec,k,etiquette,      &
                        Agrif_mpi_comm,code)
            CASE(4)
                datasize=SIZE(tempC%array4(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k)))
                call MPI_SEND(tempC%array4(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k)),    &
                        datasize,Agrif_MPI_prec,k,etiquette,      &
                        Agrif_mpi_comm,code)
            CASE(5)
                datasize=SIZE(tempC%array5(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k),     &
                                               imin(5,k):imax(5,k)))
                call MPI_SEND(tempC%array5(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k),     &
                                               imin(5,k):imax(5,k)),    &
                        datasize,Agrif_MPI_prec,k,etiquette,      &
                        Agrif_mpi_comm,code)
            CASE(6)
                datasize=SIZE(tempC%array6(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k),     &
                                               imin(5,k):imax(5,k),     &
                                               imin(6,k):imax(6,k)))
                call MPI_SEND(tempC%array6(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k),     &
                                               imin(5,k):imax(5,k),     &
                                               imin(6,k):imax(6,k)),    &
                        datasize,Agrif_MPI_prec,k,etiquette,      &
                        Agrif_mpi_comm,code)
            END SELECT
!
        endif
!
    enddo
!
!   Reception from others processors of the necessary part of the parent grid
    do k = Agrif_ProcRank-1,0,-1
!
        if (recvfromproc(k)) then
!
            if (.not.associated(temprecv)) allocate(temprecv)
            call Agrif_array_allocate(temprecv,imin_recv(:,k),imax_recv(:,k),nbdim)

            SELECT CASE(nbdim)
            CASE(1)
                datasize=SIZE(temprecv%array1)
                call MPI_RECV(temprecv%array1,datasize,Agrif_MPI_prec,k,etiquette,&
                        Agrif_mpi_comm,statut,code)
            CASE(2)
                datasize=SIZE(temprecv%array2)
                call MPI_RECV(temprecv%array2,datasize,Agrif_MPI_prec,k,etiquette,&
                        Agrif_mpi_comm,statut,code)
            CASE(3)
                datasize=SIZE(temprecv%array3)
                call MPI_RECV(temprecv%array3,datasize,Agrif_MPI_prec,k,etiquette,&
                        Agrif_mpi_comm,statut,code)
            CASE(4)
                datasize=SIZE(temprecv%array4)
                call MPI_RECV(temprecv%array4,datasize,Agrif_MPI_prec,k,etiquette,&
                          Agrif_mpi_comm,statut,code)
            CASE(5)
                datasize=SIZE(temprecv%array5)
                call MPI_RECV(temprecv%array5,datasize,Agrif_MPI_prec,k,etiquette,&
                         Agrif_mpi_comm,statut,code)
            CASE(6)
                datasize=SIZE(temprecv%array6)
                call MPI_RECV(temprecv%array6,datasize,Agrif_MPI_prec,k,etiquette,&
                        Agrif_mpi_comm,statut,code)
            END SELECT

            call Agrif_var_replace_value(tempCextend,temprecv,imin_recv(:,k),imax_recv(:,k),0.,nbdim)
            call Agrif_array_deallocate(temprecv,nbdim)
!
        endif
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine ExchangeSamelevel
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Send_3Darray
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Send_3Darray ( tab3D, bounds, imin, imax, k )
!---------------------------------------------------------------------------------------------------
    integer, dimension(3),                                     intent(in) :: bounds
    real, dimension(bounds(1):,bounds(2):,bounds(3):), target, intent(in) :: tab3D
    integer, dimension(3),                                     intent(in) :: imin, imax
    integer,                                                   intent(in) :: k
!
    integer :: etiquette = 100
    integer :: datasize, code
    include 'mpif.h'

    datasize = SIZE(tab3D(imin(1):imax(1),  &
                          imin(2):imax(2),  &
                          imin(3):imax(3)))

    call MPI_SEND( tab3D( imin(1):imax(1),  &
                          imin(2):imax(2),  &
                          imin(3):imax(3)), &
                          datasize,Agrif_MPI_prec,k,etiquette,Agrif_mpi_comm,code)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Send_3Darray
!===================================================================================================
!
#else
    subroutine dummy_Agrif_Mpp ()
    end subroutine dummy_Agrif_Mpp
#endif
!
end Module Agrif_Mpp
