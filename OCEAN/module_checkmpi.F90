
module module_checkmpi
!! Write in file fontiere ifr, jr of a array in a domain MPI
    integer,parameter :: lunitcheck = 223
  
  contains
! For 3D tarray
subroutine checkmpiT3D_file(file,tab,lb,ub,istr,iend,jstr,jend,rank)
implicit none
  character(len=*),intent(in) 			:: file
  integer,dimension(:),intent(in) 		:: lb,ub
  double precision,dimension(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)),intent(in)	&
						:: tab
  integer,intent(in) 				:: istr,iend,jstr,jend,rank
  integer,save 					:: nbpass=0
  character(len=256) 				:: fileout
  character(len=3) 				:: rankstr
  integer 					:: i,j,k
  logical :: dir_e
  
!   if (rank < 10) write(rankstr(3:3),'(i1)') rank
!   if ((rank > 9).and.(rank<100)) write(rankstr(2:3),'(i2)') rank
!   if ((rank > 99).and.(rank<1000)) write(rankstr(1:3),'(i3)') rank
  write(fileout,'(a,i0,a,i0)')trim(file)//'_',rank,'_',nbpass
!   fileout=trim(file)//'_'//rankstr
  print *,rank,"file=",fileout
  
  inquire(file=fileout, exist=dir_e)
  if (dir_e) then
    open(unit=lunitcheck,file=fileout,access='sequential',form='formatted',position='append')
  else
    open(unit=lunitcheck,file=fileout,access='sequential',form='formatted')
  endif  

  write(lunitcheck,*) "--------------",nbpass," ----------------"
  write(lunitcheck,*) 'Array dim= (',lb(1),':',ub(1),',',lb(2),':',ub(2),',', &
				     lb(3),':',ub(3),')'
  write(lunitcheck,*) 'i=>',istr,iend,' --- j=>',jstr,jend
  do i=istr,iend
  do j=jstr,jend
   write(lunitcheck,*) 'i=>',i,'j=>',j,'k=>',lb(3),'->',ub(3)
  do k=lb(3),ub(3)
     write(lunitcheck,*) tab(i,j,k)
  enddo
  enddo
  enddo
  close(lunitcheck)
  
end subroutine checkmpiT3D_file

subroutine checkmpiT4D_file(file,tab,lb,ub,istr,iend,jstr,jend,rank)
implicit none
  character(len=*),intent(in) 			:: file
  integer,dimension(:),intent(in) 		:: lb,ub
  double precision,dimension(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)),intent(in)	&
						:: tab
  integer,intent(in) 				:: istr,iend,jstr,jend,rank
  integer,save 					:: nbpass=0
  character(len=256) 				:: fileout
  character(len=3) 				:: rankstr
  integer 					:: i,j,k,t
  
!   if (rank < 10) write(rankstr(3:3),'(i1)') rank
!   if ((rank > 9).and.(rank<100)) write(rankstr(2:3),'(i2)') rank
!   if ((rank > 99).and.(rank<1000)) write(rankstr(1:3),'(i3)') rank
  write(fileout,'(a,i0,a,i0)')trim(file)//'_',rank,'_',nbpass
!   fileout=trim(file)//'_'//rankstr 
  print *,rank,"file=",fileout
  
  open(unit=lunitcheck,file=fileout,access='sequential',form='formatted')

  write(lunitcheck,*) "--------------",nbpass," ----------------"
  write(lunitcheck,*) 'Array dim= (',lb(1),':',ub(1),',',lb(2),':',ub(2),',', &
				     lb(3),':',ub(3),',',lb(4),':',ub(4),')'
  write(lunitcheck,*) 'i=>',istr,iend,' --- j=>',jstr,jend
  do i=istr,iend
  do j=jstr,jend
   write(lunitcheck,*) 'i=>',i,'j=>',j,'k=>',lb(3),'->',ub(3)
  do t=lb(4),ub(4)
  do k=lb(3),ub(3)
     write(lunitcheck,*) tab(i,j,k,t)
  enddo
  enddo
  enddo
  enddo
  close(lunitcheck)
  
end subroutine checkmpiT4D_file


end module module_checkmpi