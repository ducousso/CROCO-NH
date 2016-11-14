#include "cppdefs.h"
#ifdef NBQ

      subroutine output_nbq(ichoix)
!-------------------------------------------------------------------
!
!                   Several Outputs for MPI testings
!
!-------------------------------------------------------------------
      use module_nh 
      use module_nbq

      implicit none

# include "param_F90.h"
# include "scalars_F90.h"
# include "ocean3d.h"
# include "ocean2d.h"
# include "grid.h"
# include "nbq.h"

      integer :: i,j,k,ichoix,j_o
      integer::p1_nbq,p2_nbq,p3_nbq,p4_nbq,p5_nbq
      integer::ierrmpi_o
      character*16 name_o


       if (ichoix.eq.0) then
!
!-------------------------------------------------------------------
! Time steps & grid size
!-------------------------------------------------------------------
!
        MPI_master_only write(stdout,'(A/,5(E12.4,2x,A/),A/)')   &
          ' ======================= NBQ ==================== ',  &
         dt,        '  dt (int)  Internal timestep [s]',         &
         dtfast,    '  dt (ext)  External timestep [s]',         &
         dtnbq,     '  dt (nbq)  Acoustic timestep [s]',         &
         om_u(2,2), '  dx        XI  grid step     [m]',         &
         on_u(2,2), '  dy        ETA grid step     [m]',         &
          ' ======================= NBQ ==================== '

       endif

       if (ichoix.eq.1) then

#ifdef MPI
      if (mynode.lt.10) then
         write (name_o,'(a,i1,a)') 'grid_nbq_',mynode,'.dat'
      elseif (mynode.lt.100) then
         write (name_o,'(a,i2,a)') 'grid_nbq_',mynode,'.dat'
      elseif (mynode.lt.1000) then
         write (name_o,'(a,i3,a)') 'grid_nbq_',mynode,'.dat'
      endif
#else
      name_o = 'grid_nbq_s.dat'
#endif

       open(unit=10,file=name_o)

#ifdef MPI
       if (WEST_INTER) then
        write (10,*) 'WEST_INTER is true'
       else
        write (10,*) 'WEST_INTER is false'
       endif
       if (EAST_INTER) then
        write (10,*) 'EAST_INTER is true'
       else
        write (10,*) 'EAST_INTER is false'
       endif
       if (SOUTH_INTER) then
        write (10,*) 'SOUTH_INTER is true'
       else
        write (10,*) 'SOUTH_INTER is false'
       endif
       if (NORTH_INTER) then
        write (10,*) 'NORTH_INTER is true'
       else
        write (10,*) 'NORTH_INTER is false'
       endif

       write (10,*) 

# ifdef EW_PERIODIC
       write (10,*) 'EW_PERIODIC defined'
       write (10,*) 
# endif

# ifdef NS_PERIODIC
       write (10,*) 'NS_PERIODIC defined'
       write (10,*) 
# endif
       
       if (WEST_INTER_NBQ) then
        write (10,*) 'WEST_INTER_NBQ is true'
       else
        write (10,*) 'WEST_INTER_NBQ is false'
       endif
       if (EAST_INTER_NBQ) then
        write (10,*) 'EAST_INTER_NBQ is true'
       else
        write (10,*) 'EAST_INTER_NBQ is false'
       endif
       if (SOUTH_INTER_NBQ) then
        write (10,*) 'SOUTH_INTER_NBQ is true'
       else
        write (10,*) 'SOUTH_INTER_NBQ is false'
       endif
       if (NORTH_INTER_NBQ) then
        write (10,*) 'NORTH_INTER_NBQ is true'
       else
        write (10,*) 'NORTH_INTER_NBQa is false'
       endif
       write (10,*) 
#endif

!......Grid characteristics:
       write(10,*) 'Grid(masse) :'
       write(10,*) istr_nh,iend_nh,jstr_nh,jend_nh
       write(10,*) 'Grid(q) :'
       write(10,*) istrq_nh,iendq_nh,jstrq_nh,jendq_nh
       write(10,*) 'Grid(u) :'
       write(10,*) istru_nh,iendu_nh,jstru_nh,jendu_nh
       write(10,*) 'Grid(v) :'
       write(10,*) istrv_nh,iendv_nh,jstrv_nh,jendv_nh
       write(10,*)
       write(10,*) 'Num(q) :'
       write(10,*) neqq_nh(1:7)
       write(10,*) 'Num(u) :'
       write(10,*) nequ_nh(1:7)
       write(10,*) 'Num(v) :'
       write(10,*) neqv_nh(1:7)
       write(10,*) 'Num(w) :'
       write(10,*) neqw_nh(1:7)
       write(10,*)
       write(10,*) 'DNum(q) :'
       write(10,*) neqq_nh(1),(neqq_nh(k+1)-neqq_nh(k),k=1,6)
       write(10,*) 'DNum(u) :'
       write(10,*) nequ_nh(1),(nequ_nh(k+1)-nequ_nh(k),k=1,6)
       write(10,*) 'DNum(v) :'
       write(10,*) neqv_nh(1)-nequ_nh(7),(neqv_nh(k+1)-neqv_nh(k),k=1,6)
       write(10,*) 'DNum(w) :'
       write(10,*) neqw_nh(1)-neqv_nh(7),(neqw_nh(k+1)-neqw_nh(k),k=1,6)
       write(10,*)
       write(10,*) 'Mat Mom  :'
       write(10,*) (momi_nh(nequ_nh(k)+1),k=1,7)
       write(10,*) (momi_nh(neqv_nh(k)+1),k=1,7)
       write(10,*) (momi_nh(neqw_nh(k)+1),k=1,5)
       write(10,*) 'Mat Cont :'
       write(10,*) (conti_nh(neqq_nh(k)+1),k=1,7)

!......Q-Points:
       write(10,*)   
       write(10,*) 'Q-points: mijk2lq_nh(istr_nh-1:iend_nh+1,jstr_nh-1:jend_nh+1,N)'

       if (jend_nh-jstr_nh.ge.18) then
           j_o = jstr_nh+8
       else
           j_o = jend_nh+1
       endif

       if (iend_nh-istr_nh.ge.18) then
          do j=jstr_nh-1,j_o
             write(10,'(10I1,A3,10I1)') (mijk2lq_nh(i,j,N),i=istr_nh-1,istr_nh+8) &
                                       ,'...' &
                                       ,(mijk2lq_nh(i,j,N),i=iend_nh-8,iend_nh+1)  
          enddo
       else
          do j=jstr_nh-1,j_o
             write(10,'(20I1)') (mijk2lq_nh(i,j,N),i=istr_nh-1,iend_nh+1) 
          enddo
       endif

       if (jend_nh-jstr_nh.ge.18) then
          write (10,*) '         ...'

          if (iend_nh-istr_nh.ge.18) then
             do j=max(jend_nh-8,jstr_nh),jend_nh+1
                write(10,'(10I1,A3,10I1)') (mijk2lq_nh(i,j,N),i=istr_nh-1,istr_nh+8) &
                                       ,'...' &
                                       ,(mijk2lq_nh(i,j,N),i=iend_nh-8,iend_nh+1)  
             enddo
          else
             do j=max(jend_nh-8,jstr_nh),jend_nh+1
                write(10,'(20I1)') (mijk2lq_nh(i,j,N),i=istr_nh-1,iend_nh+1) 
             enddo
          endif
      endif

!.....U-Points:
       write(10,*)
       write(10,*) 'U-points: mijk2lmom_nh(istru_nh-1:iendu_nh+1,jstr_nh-1:jend_nh+1,N,1)'

       if (iendu_nh-istru_nh.ge.18) then
          do j=jstr_nh-1,j_o
             write(10,'(10I1,A3,10I1)') (mijk2lmom_nh(i,j,N,1),i=istru_nh-1,istru_nh+8) &
                                       ,'...' &
                                       ,(mijk2lmom_nh(i,j,N,1),i=iendu_nh-8,iendu_nh+1)  
          enddo
       else
          do j=jstr_nh-1,j_o
             write(10,'(20I1)') (mijk2lmom_nh(i,j,N,1),i=istru_nh-1,iendu_nh+1) 
          enddo
       endif

       if (jend_nh-jstr_nh.ge.18) then
          write (10,*) '         ...'
          if (iendu_nh-istru_nh.ge.18) then
             do j=max(jend_nh-8,jstr_nh),jend_nh+1
                write(10,'(10I1,A3,10I1)') (mijk2lmom_nh(i,j,N,1),i=istru_nh-1,istru_nh+8) &
                                       ,'...' &
                                       ,(mijk2lmom_nh(i,j,N,1),i=iendu_nh-8,iendu_nh+1)  
             enddo
          else
             do j=max(jend_nh-8,jstr_nh),jend_nh+1
                write(10,'(20I1)') (mijk2lmom_nh(i,j,N,1),i=istru_nh-1,iendu_nh+1) 
             enddo
          endif
       endif

!.....V-Points:
       write(10,*)    
       write(10,*) 'V-points: mijk2lmom_nh(istr_nh-1:iend_nh+1,jstrv_nh-1:jendv_nh+1,N,2)'

       if (jendv_nh-jstrv_nh.ge.18) then
           j_o = jstrv_nh+8
       else
           j_o = jendv_nh+1
       endif

       if (iend_nh-istr_nh.ge.18) then
          do j=jstrv_nh-1,j_o
             write(10,'(10I1,A3,10I1)') (mijk2lmom_nh(i,j,N,2),i=istr_nh-1,istr_nh+8) &
                                       ,'...' &
                                       ,(mijk2lmom_nh(i,j,N,2),i=iend_nh-8,iend_nh+1)  
          enddo
       else
          do j=jstrv_nh-1,j_o
             write(10,'(20I1)') (mijk2lmom_nh(i,j,N,2),i=istr_nh-1,iend_nh+1) 
          enddo
       endif

       if (jendv_nh-jstrv_nh.ge.18) then
          write (10,*) '         ...'

          if (iend_nh-istr_nh.ge.18) then
             do j=max(jendv_nh-8,jstrv_nh),jendv_nh+1
                write(10,'(10I1,A3,10I1)') (mijk2lmom_nh(i,j,N,2),i=istr_nh-1,istr_nh+8) &
                                       ,'...' &
                                       ,(mijk2lmom_nh(i,j,N,2),i=iend_nh-8,iend_nh+1)  
             enddo
          else
             do j=max(jendv_nh-8,jstrv_nh),jendv_nh+1
                write(10,'(20I1)') (mijk2lmom_nh(i,j,N,2),i=istr_nh-1,iend_nh+1) 
             enddo
          endif
       endif

!......W-Points:
       write(10,*)  
       write(10,*) 'W-points: mijk2lmom_nh(istr_nh-1:iend_nh+1,jstr_nh-1:jend_nh+1,N,3)'

       if (jend_nh-jstr_nh.ge.18) then
           j_o = jstr_nh+8
       else
           j_o = jend_nh+1
       endif

       if (iend_nh-istr_nh.ge.18) then
          do j=jstr_nh-1,j_o
             write(10,'(10I1,A3,10I1)') (mijk2lmom_nh(i,j,N,3),i=istr_nh-1,istr_nh+8) &
                                       ,'...' &
                                       ,(mijk2lmom_nh(i,j,N,3),i=iend_nh-8,iend_nh+1)  
          enddo
       else
          do j=jstr_nh-1,j_o
             write(10,'(20I1)') (mijk2lmom_nh(i,j,N,3),i=istr_nh-1,iend_nh+1) 
          enddo
       endif

       if (jend_nh-jstr_nh.ge.18) then
          write (10,*) '         ...'

          if (iend_nh-istr_nh.ge.18) then
             do j=max(jend_nh-8,jstr_nh),jend_nh+1
                write(10,'(10I1,A3,10I1)') (mijk2lmom_nh(i,j,N,3),i=istr_nh-1,istr_nh+8) &
                                       ,'...' &
                                       ,(mijk2lmom_nh(i,j,N,3),i=iend_nh-8,iend_nh+1)  
             enddo
          else
             do j=max(jend_nh-8,jstr_nh),jend_nh+1
                write(10,'(20I1)') (mijk2lmom_nh(i,j,N,3),i=istr_nh-1,iend_nh+1) 
             enddo
          endif
      endif

      close(10)
      
!      call mpi_finalize(ierrmpi_o)
!      stop 'coucou0'

       endif


      end subroutine output_nbq

#else
      subroutine output_nbq_empty
      end subroutine output_nbq_empty
#endif

