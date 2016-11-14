#include "cppdefs.h"
#if defined NBQ && defined TRACETXT
! Module pour ecritre les trace d exectuion du programme 
! au format ASCII
module module_tracetxt_out
    integer,parameter :: nblicomm=100
    character(len=50)                :: filecur,filetrace,dircur='',repabase='trace'
    character(len=50),dimension(nblicomm) :: comment=''
    integer :: lunittrace=222
    character(len=6) :: int2str
    
    character(len=82),dimension(5),parameter :: infobase = (/ &
'%MatrixMarket matrix coordinate double general                                 ', & 
'%==============================================================================', &
'%                                                                              ', &
'% This ASCII file represents a sparse MxN matrix with L                        ', &
!'% nonzeros in the following Matrix Market format: 				', &
!'%				   						', &
!'% +----------------------------------------------+				', &
!'% |%%MatrixMarket matrix coordinate dble general| <--- header line		', &
!'% |%                                             | <--+			', &
!'% |% comments                                    |    |-0 or more comment line', &
!'% |%                                             | <--+			', &          
!'% |    M  N  L                                   | <--- rows, columns, entries', &
!'% |    I1  J1  A(I1, J1)                         | <--+			', &
!'% |    I2  J2  A(I2, J2)                         |    |			', &
!'% |    I3  J3  A(I3, J3)                         |    |-- L lines		', &
!'% |        . . .                                 |    |			', &
!'% |    IL JL  A(IL, JL)                          | <--+			', &
!'% +----------------------------------------------+				', &   
!'%										', &
!'% Indices are 1-based, i.e. A(1,1) is the first element.			', &
!'%										', &
'%=============================================================================='  &
     /)

    contains

    !--------------------------------------------------------------------
    subroutine print_comment
#ifdef TRACETXT
     integer :: bcl
     do bcl=1,size(infobase)
         write(lunittrace,*) trim(infobase(bcl))
     enddo
     do bcl=1,100
        if (comment(bcl) /= '')  write(lunittrace,*) comment(bcl)
     enddo
     call free_comment
#endif    
    end subroutine print_comment 
    !--------------------------------------------------------------------
    
    !--------------------------------------------------------------------
    subroutine free_comment
#ifdef TRACETXT
      comment = ''
#endif
      end subroutine free_comment
    !--------------------------------------------------------------------
    
    !--------------------------------------------------------------------
    subroutine printmat_mm(filename,n,m,nnz,ai,aj,ax)     
      character(len=*),intent(in) :: filename
      integer,intent(in) :: n,m,nnz
      integer,dimension(:),intent(in) :: ai,aj
      double precision,dimension(:),intent(in) :: ax
      integer :: bcl1,bcl2
#ifdef TRACETXT 
      open(lunittrace,file=trim(dircur)//filename)
      call print_comment
      !print *,"n=",n,nnz
      write(lunittrace,*) n,m,nnz
      do bcl1=1,n
         do bcl2=ai(bcl1),ai(bcl1+1)-1
	    write(lunittrace,*) bcl1,aj(bcl2),ax(bcl2)
	 enddo
      enddo
      close(lunittrace)
#endif    
    end subroutine printmat_mm
    !--------------------------------------------------------------------
   
    !--------------------------------------------------------------------
    subroutine print_mat2ijk(filename,n,m,nnz,ai,aj,l2i,l2j,l2k)
      character(len=*),intent(in) :: filename
      integer,intent(in) :: n,m,nnz
      integer,dimension(:),intent(in) :: ai,aj,l2i,l2j,l2k
      integer :: bcl1,bcl2
#ifdef TRACETXT 
      open(lunittrace,file=trim(dircur)//filename)
      call print_comment
      !print *,"n=",n,nnz
      do bcl1=1,n
         do bcl2=ai(bcl1),ai(bcl1+1)-1
	    write(lunittrace,*) bcl1,aj(bcl2)," :: ",l2i(bcl1),l2j(bcl1),l2k(bcl1)
	 enddo
      enddo
      close(lunittrace)
#endif
    end subroutine print_mat2ijk
    !--------------------------------------------------------------------

    !--------------------------------------------------------------------
     subroutine print_ijk2mat(filename,n,m,nnz,ai,aj,l2i,l2j,l2k)
      character(len=*),intent(in) :: filename
      integer,intent(in) :: n,m,nnz
      integer,dimension(:),intent(in) :: ai,aj,l2i,l2j,l2k
      integer :: bcl1,bcl2
#ifdef TRACETXT 
      open(lunittrace,file=trim(dircur)//filename)
      call print_comment
      do bcl1=1,n
         do bcl2=ai(bcl1),ai(bcl1+1)-1
	    write(lunittrace,*) bcl1,aj(bcl2)," :: ",l2i,l2j(bcl2),l2k(bcl2)
	 enddo
      enddo
      close(lunittrace)
#endif
    end subroutine print_ijk2mat
    !--------------------------------------------------------------------
 
   
    !--------------------------------------------------------------------
    function int2string(n)
     integer,intent(in) :: n
     character(len=6) :: int2string
#ifdef TRACETXT 
     int2string='000000'
     if ( n < 10) 			write(int2string(6:6), '(i1)') n
     if (( n > 9).and.(n<100)) 		write(int2string(5:6), '(i2)') n
     if (( n > 99).and.(n<1000))        write(int2string(4:6), '(i3)') n
     if (( n > 999).and.(n<10000)) 	write(int2string(3:6), '(i4)') n
     if (( n > 9999).and.(n<100000)) 	write(int2string(2:6), '(i5)') n
     if (( n > 99999).and.(n<1000000)) 	write(int2string(1:6), '(i6)') n
     if (n > 999999) 			int2string='xxxxxx'
#endif    
     return
     end function int2string
     !--------------------------------------------------------------------
    
    !--------------------------------------------------------------------
    subroutine set_tracedirectory(iter)
	integer,intent(in) :: iter
	character(len=50) :: string
#ifdef TRACETXT 
	dircur = trim(repabase) // '/' // int2string(iter) // '/'
	print *,dircur
	string = 'mkdir -p ' // trim(dircur)
	call system(string)
#endif    
    end subroutine set_tracedirectory
    !--------------------------------------------------------------------
    
    
end module module_tracetxt_out
#else
module module_tracetxt_out_empty
end module module_tracetxt_out_empty
#endif
