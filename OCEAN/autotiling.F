! $Id: autotiling.F 1458 2014-02-03 15:01:25Z gcambon $
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
#ifdef AUTOTILING
        subroutine init_auto_tiling
#ifdef AGRIF
        Use Agrif_Util
#endif
        implicit none
#include "param.h"
#include "autotiling.h"
#include "private_scratch.h"
#ifdef PISCES
# include "parameter.h"
#endif
        nbsampling = 1
        lastiic = 0
        nullify(A2d)
        nullify(A3d)

        cpu_domain(0) = 0.
        cpu_domain(1) = 0.

        call allocate_private_scratch

        return
        end

      subroutine allocate_private_scratch
      implicit none
#include "param.h"
#include "private_scratch.h"
#include "ncscrum.h"
#include "autotiling.h"

      integer size_XI,size_ETA,se,sse, sz,ssz

# include "dynderivparam.h"
#ifdef PISCES
# include "parameter.h"
#endif
      if (associated(A2d)) then
      deallocate(A2d,A3d)
      endif
      allocate(A2d(N2d,NSA,0:NPP-1), A3d(N3d,4,0:NPP-1))
      return
      end
      
      subroutine deallocate_private_scratch
      implicit none
#include "param.h"
#include "private_scratch.h"
#ifdef PISCES
# include "parameter.h"
#endif
      integer size_XI,size_ETA,se,sse, sz,ssz

      deallocate(A2d, A3d)
      return
      end      

      real function curtime()
        call cpu_time(curtime)
      end

      subroutine begin_timing
      implicit none
#include "param.h"
#include "private_scratch.h"
#include "scalars.h"
#include "ncscrum.h"
#include "autotiling.h"
#ifdef PISCES
# include "parameter.h"
#endif
      real curtime

      cpu_domain(0) = curtime()
 
      return
      end

      subroutine end_timing
      implicit none
#include "param.h"
#include "private_scratch.h"
#include "scalars.h"
#include "ncscrum.h"
#include "autotiling.h"
#ifdef PISCES
# include "parameter.h"
#endif
      real curtime

      cpu_domain(1) = cpu_domain(1)+curtime()-cpu_domain(0)
 
      return
      end

      subroutine auto_tiling
      implicit none
#include "param.h"
#include "mpi_roms.h"
#include "private_scratch.h"
#include "scalars.h"
#include "ncscrum.h"
#include "autotiling.h"
#ifdef PISCES
# include "parameter.h"
#endif
      integer size_XI,size_ETA,se,sse, sz,ssz
      real timemean(MAX_NSUB_X,MAX_NSUB_E)      

      real curtime
      real cfft
      integer i,j,nbval
      integer reducecluster
      integer min_X, min_E
      real valmin
      logical :: conttri
      real tmpval
      integer iflag, nmid, nmidp1
      integer ierr
#ifdef MPI
      include 'mpif.h'
      real cputot
#endif       

!       return
# include "dynderivparam.h"
    
      if (iic <= nbsampling+1) then
 
      cpu_domain(1) = 0.

      nbinst = 0
      lastiic = iic
      
      nsub_xmin = 1
      nsub_xmax = MAX_NSUB_X
      nsub_emin = NPP
      nsub_emax = MAX_NSUB_E

      nsub_decalx = max((nsub_xmax-nsub_xmin)/nbx,1)
      nsub_decale = max((nsub_emax-nsub_emin)/nbe,1)
      
      curdistrib = 1
      
      do j=1,MAX_NSUB_E
      do i=1,MAX_NSUB_X
        if (mod(i*j,NPP) /= 0) then
          nbinst(i,j) = nbdistrib * nbvalid
          times(i,j,:) = Huge(1.)
        endif
      enddo
      enddo


      else
      
      lastiic = iic
      cfft = cpu_domain(1)
      cpu_domain(1) = 0.

#ifdef MPI
      call MPI_allreduce(cfft,cputot,1,
     &    MPI_DOUBLE_PRECISION, MPI_MAX,MPI_COMM_WORLD,ierr)
       cfft = cputot
#endif

      if (cfft < 0.1) then
         nbsampling = nbsampling + 1
         return
      endif
      
      nbinst(NSUB_X,NSUB_E)=nbinst(NSUB_X,NSUB_E)+1  
      times(NSUB_X,NSUB_E,nbinst(NSUB_X,NSUB_E)) = cfft/nbsampling
            
      reducecluster = 1
      do j=nsub_emin,nsub_emax,nsub_decale
      do i=nsub_xmin,nsub_xmax,nsub_decalx      
      if (nbinst(i,j) < curdistrib*nbvalid) then
      NSUB_X=i
      NSUB_E=j
      reducecluster = 0
      goto 10
      endif
      enddo
      enddo
 10   continue      
 
       print *,'nsub = ',NSUB_X,NSUB_E

!      print *,'sur la grille = ',Agrif_Fixed()
!      print *,'reducecluster = ',reducecluster,nbinst(nsub_xmin,nsub_emin)
!      print *,'nsub = ',nsub_xmin,nsub_emin


      
       if (reducecluster == 1) then

         do j=nsub_emin,nsub_emax,nsub_decale
         do i=nsub_xmin,nsub_xmax,nsub_decalx
         conttri = .TRUE.
         do while (conttri)
           conttri=.FALSE.
           do nbval=1,nbinst(i,j)-1
             if (times(i,j,nbval) > times(i,j,nbval+1)) then
               tmpval = times(i,j,nbval)
               times(i,j,nbval) = times(i,j,nbval+1)
               times(i,j,nbval+1)=tmpval
               conttri=.TRUE.
             endif
           enddo
         enddo
         
         nmid = nbinst(i,j)/2
         nmidp1=nmid+1
         iflag = nbinst(i,j)-2*nmid
         if (iflag == 0) then
           timemean(i,j)=0.5*(times(i,j,nmid)+times(i,j,nmidp1))
         else  
           timemean(i,j)=times(i,j,nmidp1)    
         endif
         enddo
         enddo
         
         valmin= Huge(1.)/2.

         min_X = NSUB_X
         min_E = NSUB_E

         do j=nsub_emin,nsub_emax,nsub_decale
         do i=nsub_xmin,nsub_xmax,nsub_decalx
           if (timemean(i,j) < valmin) then
             valmin = timemean(i,j)
             min_X = i
             min_E = j
           endif
         enddo
         enddo         
                  

         
         i=1
         j=1
         timemean(i,j)=sum(times(i,j,1:nbinst(i,j)))/nbinst(i,j)

!         print *,'Gain = ',timemean(1,1)/timemean(min_X,min_E)
!         print *,'temps diff = ',timemean(1,1),timemean(min_X,min_E)
#ifdef AGRIF
         print *,'Current Grid = ',Agrif_Fixed()
#endif
         print *,'Current optimal distribution = ',min_X,min_E
         nsub_xmin = max(min_X-nsub_decalx/2,1)
         nsub_xmax = min(min_X+nsub_decalx/2,MAX_NSUB_X)
         nsub_emin = max(min_E-nsub_decale/2,1)
         nsub_emax = min(min_E+nsub_decale/2,MAX_NSUB_E)
          print *,'New distribution = ',nsub_xmin,nsub_xmax,nsub_emin,nsub_emax
          print *,'min_X = ',min_X,min_E
         nsub_decalx = max((nsub_xmax-nsub_xmin)/nbx,1)
         nsub_decale = max((nsub_emax-nsub_emin)/nbe,1)
           NSUB_X=min_X
           NSUB_E=min_E
         print *,'NEWNSUB = ',NSUB_X,NSUB_E
         if ((nsub_xmax == nsub_xmin) .AND. (nsub_emin == nsub_emax)) then
!           nbsampling = -1 ! end of optimization
           if (curdistrib < nbdistrib) then
           nbsampling = int(nbsampling * 1.51)
           nsub_xmin = max(nsub_xmin-1,1)
           nsub_emin = max(nsub_emin-1,1)
           nsub_xmax = min(nsub_xmax+1,MAX_NSUB_X)
           nsub_emax = min(nsub_emax+1,MAX_NSUB_E)
           nsub_decalx = 1
           nsub_decale = 1
           curdistrib = curdistrib + 1
           else
           print *,'This is the final choice'
!           pause
           nbsampling = -1
           endif
         endif
!         nbinst(nsub_xmin:nsub_xmax,nsub_emin:nsub_emax) = 0         
       endif ! <- reducecluster == 1
       
       call allocate_private_scratch

      endif ! <- iic == nbsampling
      
      return
      end  


#else
        subroutine auto_tiling_empty
        return
        end
#endif
