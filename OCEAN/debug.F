! $Id: debug.F 1458 2014-02-03 15:01:25Z gcambon $
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
#include "cppdefs.h"
#if defined RVTK_DEBUG || defined RVTK_DEBUG_ADVANCED
          subroutine debug_ini()
          
          open(107,file="check_file",form='unformatted')
          return
          end subroutine debug_ini
          
          subroutine check_tab2d(tab,comment,typevar)
      implicit none
#  include "param.h"
#  include "scalars.h"  
#  include "ocean2d.h" 
#  include "mpi_roms.h"
#if defined MPI
          include 'mpif.h'
#endif   
          real,dimension(GLOBAL_2D_ARRAY) :: tab
          integer Lmseq,Mmseq
          integer lb(2),ub(2)
          real,dimension(:,:),allocatable :: tabread
          character*(*) :: comment,typevar
          integer i0,j0,i,j,i1,j1,i2,j2
          integer iseq1, iseq2, jseq1, jseq2
          logical mystop

#if defined MPI
          logical globstop
          integer ierr
#endif
          
#if !defined MPI && !defined OPENMP
          write(107)Lm,Mm
          write(107)lbound(tab)
          write(107)ubound(tab)
          write(107)tab
#else
          read(107)Lmseq,Mmseq
          read(107)lb
          read(107)ub
          allocate(tabread(lb(1):ub(1),lb(2):ub(2)))
          read(107)tabread

      if (typevar == 'uint') then
      i1=2
      i2=Lmmpi
      j1=1
      j2=Mmmpi
      elseif (typevar == 'u') then
      i1=1
      i2=Lmmpi+1
      j1=0
      j2=Mmmpi+1
      endif
      if (typevar == 'vint') then
      i1=1
      i2=Lmmpi
      j1=2
      j2=Mmmpi
      elseif (typevar == 'v') then
      i1=0
      i2=Lmmpi+1      
      j1=1
      j2=Mmmpi+1
      endif    
      if (typevar == 'rint') then
      i1=1
      i2=Lmmpi
      j1=1
      j2=Mmmpi
      elseif (typevar == 'r') then
      i1=0
      i2=Lmmpi+1
      j1=0
      j2=Mmmpi+1
      endif          
      
      iseq1 = 0
      iseq2 = Lmseq+1
      jseq1 = 0
      jseq2 = Mmseq+1


      mystop =.false.
      DebugExit : do j=max(j1,jseq1-jminmpi+1),min(j2,jseq2-jminmpi+1)
                  j0 = j+jminmpi-1
                  do i=max(i1,iseq1-iminmpi+1),min(i2,iseq2-iminmpi+1)     
                    i0 = i+iminmpi-1
                    if (tabread(i0,j0)/=tab(i,j)) then
                       write(*,'(A,A,2x,4i4,3e20.12)')'BUGBIN = ',
     &                 comment,i0,j0,i,j,
     &                 tabread(i0,j0),tab(i,j),
     &                 abs(tabread(i0,j0)-tab(i,j))
!                       print *,'iif = ',iif,iic
#ifdef AGRIF
                       print *,'GRID# ',Agrif_CFixed()
#endif                    
                       mystop=.true.
                       exit DebugExit
                     endif
                   enddo
                 enddo DebugExit
#ifdef MPI         
                       call MPI_allreduce(mystop,globstop,1,
     &                      MPI_LOGICAL,MPI_LOR,MPI_COMM_WORLD,ierr)
                        mystop=globstop
#endif
                       if (mystop) then
                         stop
                       endif
!      print *,'DEBUGBIN DE ',comment,' OK'
            
            deallocate(tabread)
#endif         

#ifdef MPI
            call MPI_Barrier(MPI_COMM_WORLD,ierr)
#endif
#if defined MPI || defined OPENMP
          MPI_master_only print *,'CHECK ',comment,' PASSED'
#ifdef AGRIF
     &     ,' ON GRID ',Agrif_CFixed()
#endif    
#endif
          return
          end subroutine check_tab2d
          
          subroutine check_tab3d(tab,comment,typevar)
      implicit none
#  include "param.h"
#  include "scalars.h"  
#  include "ocean2d.h" 
#if defined MPI
          include 'mpif.h'
#endif   
#if defined MPI
          logical globstop
          integer ierr
#endif 
          real,dimension(GLOBAL_2D_ARRAY,N) :: tab
          integer Lmseq,Mmseq
          integer lb(3),ub(3)
          real,dimension(:,:,:),allocatable :: tabread
          character*(*) :: comment,typevar
          integer i0,j0,k,i,j,i1,i2,j1,j2
          integer iseq1, iseq2, jseq1, jseq2 
          logical mystop         
          

#if !defined MPI && !defined OPENMP
          write(107)Lm,Mm
          write(107)lbound(tab)
          write(107)ubound(tab)
          write(107)tab
#else
          read(107)Lmseq,Mmseq
          read(107)lb
          read(107)ub
          allocate(tabread(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)))
          read(107)tabread

      if (typevar == 'uint') then
      i1=2
      i2=Lmmpi
      j1=1
      j2=Mmmpi
      elseif (typevar == 'u') then
      i1=1
      i2=Lmmpi+1
      j1=0
      j2=Mmmpi+1
      endif
      if (typevar == 'vint') then
      i1=1
      i2=Lmmpi
      j1=2
      j2=Mmmpi
      elseif (typevar == 'v') then
      i1=0
      i2=Lmmpi+1      
      j1=1
      j2=Mmmpi+1
      endif    
      if (typevar == 'rint') then
      i1=1
      i2=Lmmpi
      j1=1
      j2=Mmmpi
      elseif (typevar == 'r') then
      i1=0
      i2=Lmmpi+1
      j1=0
      j2=Mmmpi+1
      endif   

      iseq1 = 0
      iseq2 = Lmseq+1
      jseq1 = 0
      jseq2 = Mmseq+1
      
      mystop =.false.
      DebugExit : do k=1,N
                do j=max(j1,jseq1-jminmpi+1),min(j2,jseq2-jminmpi+1)
                 j0 = j+jminmpi-1
                do i=max(i1,iseq1-iminmpi+1),min(i2,iseq2-iminmpi+1)     
                    i0 = i+iminmpi-1
                     if (tabread(i0,j0,k)/=tab(i,j,k)) then
                       write(*,'(A,A,2x,5i4,3e20.12)')'BUGBIN = ',
     &                 comment,
     &                 i0,j0,i,j,k,
     &                 tabread(i0,j0,k),tab(i,j,k),
     &                 abs(tabread(i0,j0,k)-tab(i,j,k))
!                      print *,'iif = ',iif,iic
#ifdef AGRIF
                       print *,'GRID# ',Agrif_CFixed()
#endif    
                        mystop=.true.
                       exit DebugExit
                      endif
                     enddo
                    enddo
                   enddo DebugExit
#ifdef MPI   
                       call MPI_allreduce(mystop,globstop,1,
     &                 MPI_LOGICAL,MPI_LOR,MPI_COMM_WORLD,ierr)
                       mystop=globstop
#endif
                       if (mystop) then
                       stop
                       endif
!print *,'DEBUG DE ',comment,' OK'
            
            deallocate(tabread)
#endif          
#ifdef MPI
            call MPI_Barrier(MPI_COMM_WORLD,ierr)
#endif
#if defined MPI || defined OPENMP
          MPI_master_only print *,'CHECK ',comment,' PASSED'
#ifdef AGRIF
     &     ,' ON GRID ',Agrif_CFixed()
#endif    
#endif
          return
          end subroutine check_tab3d    
          
#else   
! de if RVTK_DEBUG !
        subroutine debug_ini()
        return
        end subroutine debug_ini
#endif                
