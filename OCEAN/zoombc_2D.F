! $Id: zoombc_2D.F 1615 2014-12-17 13:27:07Z rblod $
!
!======================================================================
! CROCO is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! CROCO specific routines (nesting) are under CeCILL-C license.
!
! This routine belongs to the specific CROCO package.
! 
! CROCO website : http://www.croco-ocean.org
!======================================================================
!
!====================================================================
!                   subroutine Agrif_u2dbc_interp_tile
!====================================================================
!   
#include "cppdefs.h"     
#ifdef AGRIF     
      subroutine u2dbc_interp_tile(Istr,Iend,Jstr,Jend
     &        ,DU_west4,DU_east4,DU_west6,DU_east6
     &        ,DU_south4,DU_north4,DU_south6,DU_north6)
      use AGRIF_Util
!     implicit none
# include "param.h"
# include "grid.h"
# include "ocean2d.h"   
# include "scalars.h"
# include "climat.h"
# include "boundary.h"
# include "zoom.h"
# include "coupling.h"
      integer Istr,Iend,Jstr,Jend, i,j
      real t1,t2,t3,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19
      logical ptinterp
      real t4,t5,t6,tfin,c1,c2,c3,cff1,cff2,cff3
      external :: u2dinterp
      integer :: irhox, irhoy, irhot
      real :: rrhox, rrhoy, rrhot
      real :: tinterp, onemtinterp
      integer :: iter
      integer :: ipu, jpu, parentnnew
      integer :: parentnbstep
      real :: cffx, cffy
      real,dimension(PRIVATE_1DETA_SCRATCH_ARRAY) :: DU_west, DU_east
      real,dimension(GLOBAL_1D_ARRAYETA) :: DU_west6,DU_east6     
      real,dimension(GLOBAL_1D_ARRAYETA,0:NWEIGHT) :: DU_west4, DU_east4
      real,dimension(PRIVATE_1DXI_SCRATCH_ARRAY) :: DU_south,DU_north
      real,dimension(GLOBAL_1D_ARRAYXI) :: DU_south6,DU_north6     
      real,dimension(GLOBAL_1D_ARRAYXI,0:NWEIGHT) :: DU_south4,DU_north4
      real :: lastcff
C      real,dimension(:,:),allocatable :: tabtemp2d
!$AGRIF_DO_NOT_TREAT
!      real,dimension(:,:,:),pointer :: coarsevalues
!      integer :: nbgrid, indinterp
!      common/interp2d/coarsevalues, nbgrid, indinterp
      integer :: nbgrid,indinterp
      common/interp2d/indinterp,nbgrid
!$AGRIF_END_DO_NOT_TREAT     
# ifdef MPI
       include 'mpif.h'
# endif
  
# ifdef MPI
#  define LOCALLM Lmmpi
#  define LOCALMM Mmmpi
# else
#  define LOCALLM Lm
#  define LOCALMM Mm
# endif   
!          
# include "compute_auxiliary_bounds.h"
!     return
      irhox=Agrif_Irhox()
      irhoy=Agrif_Irhoy()
      irhot=Agrif_Irhot()

      rrhox = real(irhox)
      rrhoy = real(irhoy)
      rrhot = real(irhot)


      parentnbstep=
     & Agrif_Parent(iif)
     
      if (U2DTimeindex .NE. parentnbstep) then

C$OMP BARRIER
C$OMP MASTER      

        dUinterp = 0.

        tinterp=1.
# ifdef MASKING
        Agrif_UseSpecialValue = .true.
# endif
        Agrif_SpecialValue = 0.

        nbgrid = Agrif_Fixed()
C        indinterp = 0 indinterp is set to zero in zetabc

        Call Agrif_Bc_variable(ubarid,
     &    calledweight=tinterp,procname = u2dinterp)
C        dUinterp(0:i2u,j1t:j2t)=tabtemp2d
C        deallocate(tabtemp2d)


        Agrif_UseSpecialvalue=.false.
C$OMP END MASTER
C$OMP BARRIER
        U2DTimeindex = parentnbstep
        U2DTimeindex2 = agrif_nb_step()
      endif

       if (agrif_nb_step() .EQ. U2DTimeindex2) then

          lastcff=0.
          do iter=iif,iif+irhot-1
          lastcff=lastcff+((iter+1-iif)/rrhot)*
     &        weight2(iif+irhot-1,iter)
          enddo
          
          lastcff=1./lastcff
               
# ifdef AGRIF_OBC_WEST
      if (WESTERN_EDGE) then
          i = Istr

          do j=Jstr,Jend
          DU_west4(j,iif-1) = DU_avg3(i,j,iif-1)
          enddo
          
          if (iif+irhot-1<=nfast) then
           DU_west6(Jstr:Jend)=0.
          do iter=0,iif-1
          cff1=weight2(iif+irhot-1,iter)
            do j=Jstr,Jend
             DU_west6(j) = DU_west6(j) +cff1*DU_west4(j,iter)
            enddo
          enddo
          
          do iter=iif,iif+irhot-2
          do j=Jstr,Jend
          DU_west6(j)=DU_west6(j)+((iif+irhot-1-iter)/rrhot)*
     &        weight2(iif+irhot-1,iter)*DU_west4(j,iif-1)
          enddo
          enddo
          

          
          do j=Jstr,Jend
          DU_west6(j)=lastcff*((dUinterp(i,j)/rrhoy)-DU_west6(j))
#  ifdef MASKING
     &                               * umask(i,j)
#  endif                     
          enddo      
          
          do j=Jstr,Jend
          DU_west6(j)=(DU_west6(j)-DU_west4(j,iif-1))/rrhot
          enddo 
          endif
                                       
      endif
# endif 
# ifdef AGRIF_OBC_EAST
      if (EASTERN_EDGE) then
          i=Iend+1         
          
          do j=Jstr,Jend
          DU_east4(j,iif-1) = DU_avg3(i,j,iif-1)
          enddo
          
          if (iif+irhot-1<=nfast) then
           DU_east6(Jstr:Jend)=0.
          do iter=0,iif-1
          cff1=weight2(iif+irhot-1,iter)
            do j=Jstr,Jend
             DU_east6(j) = DU_east6(j) +cff1*DU_east4(j,iter)
            enddo
          enddo
          
          do iter=iif,iif+irhot-2
          do j=Jstr,Jend
          DU_east6(j)=DU_east6(j)+((iif+irhot-1-iter)/rrhot)*
     &        weight2(iif+irhot-1,iter)*DU_east4(j,iif-1)
          enddo
          enddo

          do j=Jstr,Jend
          DU_east6(j)=lastcff*((dUinterp(i,j)/rrhoy)-DU_east6(j))
#  ifdef MASKING
     &                               * umask(i,j)
#  endif          
          enddo      
          
          do j=Jstr,Jend
          DU_east6(j)=(DU_east6(j)-DU_east4(j,iif-1))/rrhot
          enddo 
          endif          
                                 
      endif
# endif 

# ifdef AGRIF_OBC_SOUTH

      if (SOUTHERN_EDGE) then
          j=Jstr-1
          
          do i=Istr,Iend
          DU_south4(i,iif-1) = DU_avg3(i,j,iif-1)
          enddo
          
          if (iif+irhot-1<=nfast) then
           DU_south6(Istr:Iend)=0.
          do iter=0,iif-1
          cff1=weight2(iif+irhot-1,iter)
            do i=Istr,Iend
             DU_south6(i) = DU_south6(i) +cff1*DU_south4(i,iter)
            enddo
          enddo
          
          do iter=iif,iif+irhot-2
          do i=Istr,Iend
          DU_south6(i)=DU_south6(i)+((iif+irhot-1-iter)/rrhot)*
     &        weight2(iif+irhot-1,iter)*DU_south4(i,iif-1)
          enddo
          enddo
          
          do i=Istr,Iend
          DU_south6(i)=lastcff*((dUinterp(i,j)/rrhox)-DU_south6(i))
#  ifdef MASKING
     &                               * umask(i,j)
#  endif          
          enddo      
          
          do i=Istr,Iend
          DU_south6(i)=(DU_south6(i)-DU_south4(i,iif-1))/rrhot
          enddo 
          endif           
                                       
      endif
      
# endif 
# ifdef AGRIF_OBC_NORTH
      if (NORTHERN_EDGE) then  
          j=Jend+1
          
          do i=Istr,Iend
          DU_north4(i,iif-1) = DU_avg3(i,j,iif-1)
          enddo
          
          if (iif+irhot-1<=nfast) then
           DU_north6(Istr:Iend)=0.
          do iter=0,iif-1
          cff1=weight2(iif+irhot-1,iter)
            do i=Istr,Iend
             DU_north6(i) = DU_north6(i) +cff1*DU_north4(i,iter)
            enddo
          enddo
          
          do iter=iif,iif+irhot-2
          do i=Istr,Iend
          DU_north6(i)=DU_north6(i)+((iif+irhot-1-iter)/rrhot)*
     &        weight2(iif+irhot-1,iter)*DU_north4(i,iif-1)
          enddo
          enddo
          

          
          do i=Istr,Iend
          DU_north6(i)=lastcff*((dUinterp(i,j)/rrhox)-DU_north6(i))
#  ifdef MASKING
     &                               * umask(i,j)
#  endif             
          enddo      
          
          do i=Istr,Iend
          DU_north6(i)=(DU_north6(i)-DU_north4(i,iif-1))/rrhot
          enddo 
          endif

      endif
# endif
      endif

# ifdef AGRIF_OBC_WEST
      if (WESTERN_EDGE) then
          i = Istr        

          do j=Jstr,Jend
           DU_west4(j,iif-1) = DU_avg3(i,j,iif-1)
          enddo
           
          do j=Jstr,Jend
            DU_west(j)=DU_west4(j,iif-1)+DU_west6(j)
          enddo
       endif
# endif 

# ifdef AGRIF_OBC_EAST
      if (EASTERN_EDGE) then
          i=Iend+1
          
          do j=Jstr,Jend
           DU_east4(j,iif-1) = DU_avg3(i,j,iif-1)
          enddo
           
          do j=Jstr,Jend
            DU_east(j)=DU_east4(j,iif-1)+DU_east6(j)
          enddo 
        endif
#  endif

# ifdef AGRIF_OBC_SOUTH
      if (SOUTHERN_EDGE) then
          j=Jstr-1
          
          do i=Istr,Iend
           DU_south4(i,iif-1) = DU_avg3(i,j,iif-1)
          enddo
           
          do i=Istr,Iend
            DU_south(i)=DU_south4(i,iif-1)+DU_south6(i)
          enddo
       endif
# endif

# ifdef AGRIF_OBC_NORTH
      if (NORTHERN_EDGE) then
          j=Jend+1

          do i=Istr,Iend
           DU_north4(i,iif-1) = DU_avg3(i,j,iif-1)
          enddo
           
          do i=Istr,Iend
            DU_north(i)=DU_north4(i,iif-1)+DU_north6(i)
          enddo         
       endif
# endif  
          

!# if defined M2_FRC_BRY || defined M2NUDGING
!
! Apply the value to ubclm or ubarbry
!
      cffx = g*dtfast*2./(1.+rrhox)

#  ifdef AGRIF_2WAY
!     cffx = 0.
#  endif

#  ifdef AGRIF_OBC_WEST
      if (WESTERN_EDGE) then
        do j=Jstr,Jend
#   ifdef M2_FRC_BRY
          ubarbry_west(j)=
#   else
          ubclm(Istr,j)= 
#   endif
     &                   (cffx/om_u(Istr,j))*
     &                   (SSH(Istr,j)-zeta(Istr,j,knew))  +

#   ifdef AGRIF_FLUX_BC
     &              (2.*DU_west(j)/((h(Istr-1,j)+zeta(Istr-1,j,knew)
     &                                 +h(Istr,j)+zeta(Istr,j,knew))
     &                                                *on_u(Istr,j)))
#   else
     &                  DU_west(j)
#   endif
#   ifdef MASKING
     &                  *umask(Istr,j)
#   endif
        enddo   

      endif
#  endif

#  ifdef AGRIF_OBC_EAST
      if (EASTERN_EDGE) then
        do j=Jstr,Jend
#   ifdef M2_FRC_BRY
          ubarbry_east(j)=
#   else
          ubclm(Iend+1,j)=
#   endif
     &                    -(cffx/om_u(Iend+1,j))*
     &                     (SSH(Iend,j)-zeta(Iend,j,knew)) +

#   ifdef AGRIF_FLUX_BC
     &                  (2.*DU_east(j)/(( h(Iend,j)+zeta(Iend,j,knew)
     &                              +h(Iend+1,j)+zeta(Iend+1,j,knew))
     &                                              *on_u(Iend+1,j)))
#   else
     &                      DU_east(j)
#   endif
#   ifdef MASKING
     &                      *umask(Iend+1,j)
#   endif
        enddo       
      endif
#  endif

#  ifdef AGRIF_OBC_SOUTH
      if (SOUTHERN_EDGE) then       
        do i=Istr,Iend
#   ifdef M2_FRC_BRY
          ubarbry_south(i)=
#   else
          ubclm(i,Jstr-1)=
#   endif
#   ifdef AGRIF_FLUX_BC
     &              (2.*DU_south(i)/(( h(i,Jstr-1)+zeta(i,Jstr-1,knew)
     &                           +h(i-1,Jstr-1)+zeta(i-1,Jstr-1,knew))
     &                                                *on_u(i,Jstr-1)))
#   else
     &                  DU_south(i)
#   endif
#   ifdef MASKING
     &                  *umask(i,Jstr-1)
#   endif
      enddo
      endif
#  endif

#  ifdef AGRIF_OBC_NORTH
      if (NORTHERN_EDGE) then
        do i=Istr,Iend
#   ifdef M2_FRC_BRY
          ubarbry_north(i)=
#   else
          ubclm(i,Jend+1)=
#   endif
#   ifdef AGRIF_FLUX_BC
     &            (2.*DU_north(i)/(( h(i,Jend+1)+zeta(i,Jend+1,knew)
     &                         +h(i-1,Jend+1)+zeta(i-1,Jend+1,knew))
     &                                             *on_u(i,Jend+1)))
#   else
     &                DU_north(i)
#   endif
#   ifdef MASKING
     &                *umask(i,Jend+1)
#   endif
        enddo

      endif
#  endif
!# endif * M2_FRC_BRY || M2NUDGING */

      return
      end

      subroutine u2Dinterp(tabres,i1,i2,j1,j2,before)
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "scalars.h"
# include "zoom.h"

       integer i1,i2,j1,j2
       real tabres(i1:i2,j1:j2)
       logical before
       integer i,j,iter,isize
       real :: cff1
!$AGRIF_DO_NOT_TREAT
      integer :: indinterp,nbgrid
      common/interp2d/indinterp,nbgrid
!$AGRIF_END_DO_NOT_TREAT  

       integer :: oldindinterp

       if (before) then
       isize = (j2-j1+1)*(i2-i1+1)
       
       IF (iif == 1) THEN
         IF (.NOT.allocated(coarsevaluesinterp)) THEN
         Allocate(coarsevaluesinterp(isize,0:nfast))
         ELSE
         CALL checksizeinterp(indinterp+isize)
         ENDIF

         oldindinterp = indinterp

         do j=j1,j2
         do i=i1,i2
         oldindinterp=oldindinterp+1
         coarsevaluesinterp(oldindinterp,0) =
     &     0.5*(h(i-1,j)+zeta(i-1,j,kstp)+h(i,j)+
     &    zeta(i,j,kstp))*on_u(i,j)*ubar(i,j,kstp)
         enddo
         enddo

       ENDIF

       oldindinterp = indinterp

       do j=j1,j2
       do i=i1,i2
       oldindinterp=oldindinterp+1
       coarsevaluesinterp(oldindinterp,iif) =
     &   0.5*(h(i-1,j)+zeta(i-1,j,knew)+h(i,j)+
     &  zeta(i,j,knew))*on_u(i,j)*ubar(i,j,knew)
       enddo
       enddo

       do iter=0,iif
          cff1 = weight2(iif,iter)
          call copy1d(tabres,coarsevaluesinterp(indinterp+1,iter),
     &                cff1,isize)
       enddo

       indinterp = oldindinterp

       else
!         tabtmp = tabres
         dUinterp(i1:i2,j1:j2) = tabres
       endif
      return
      end
      subroutine u2Dinterp_old(tabres,i1,i2,j1,j2)
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "scalars.h"
# include "zoom.h"

       integer i1,i2,j1,j2
       real tabres(i1:i2,j1:j2)
       integer i,j,iter,isize
       real :: cff1
!$AGRIF_DO_NOT_TREAT
      integer :: indinterp,nbgrid
      common/interp2d/indinterp,nbgrid
!$AGRIF_END_DO_NOT_TREAT

       integer :: oldindinterp

       isize = (j2-j1+1)*(i2-i1+1)

       IF (iif == 1) THEN
       IF (.NOT.allocated(coarsevaluesinterp)) THEN
       Allocate(coarsevaluesinterp(isize,0:nfast))
       ELSE       
       CALL checksizeinterp(indinterp+isize)
       ENDIF
       
       oldindinterp = indinterp

       do j=j1,j2
       do i=i1,i2
       oldindinterp=oldindinterp+1
       coarsevaluesinterp(oldindinterp,0) = 
     &  0.5*(h(i-1,j)+zeta(i-1,j,kstp)+h(i,j)+
     &  zeta(i,j,kstp))*on_u(i,j)*ubar(i,j,kstp)
       enddo
       enddo
              
       ENDIF
       
       oldindinterp = indinterp
       
       do j=j1,j2
       do i=i1,i2
       oldindinterp=oldindinterp+1
       coarsevaluesinterp(oldindinterp,iif) =
     &   0.5*(h(i-1,j)+zeta(i-1,j,knew)+h(i,j)+
     &  zeta(i,j,knew))*on_u(i,j)*ubar(i,j,knew)
       enddo
       enddo
          
       do iter=0,iif
          cff1 = weight2(iif,iter)
          call copy1d(tabres,coarsevaluesinterp(indinterp+1,iter),
     &                cff1,isize)
       enddo
       
       indinterp = oldindinterp     
       
      return
      end      
!
!====================================================================
!                   subroutine Agrif_v2dbc_interp_tile
!====================================================================
!
      subroutine v2dbc_interp_tile(Istr,Iend,Jstr,Jend
     &        ,DV_west4,DV_east4,DV_west6,DV_east6
     &        ,DV_south4,DV_north4,DV_south6,DV_north6)
      use AGRIF_Util
!     implicit none
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "scalars.h"
# include "climat.h"
# include "boundary.h"
# include "zoom.h"
# include "coupling.h"
      integer Istr,Iend,Jstr,Jend, i,j
      real t1,t2,t3,t4,t5,t6,dv1,dv1np1,dv2,tfin,c1,c2,c3,cff1
      real t7,t8,t9,t10,t11
      external :: v2dinterp
      integer :: irhox, irhoy, irhot
      real :: rrhox, rrhoy, rrhot
      real :: tinterp, onemtinterp
      integer :: iter  
      integer :: parentnbstep    
      real :: cffy
      real,dimension(PRIVATE_1DETA_SCRATCH_ARRAY) :: DV_west, DV_east
      real,dimension(GLOBAL_1D_ARRAYETA) :: DV_west6,DV_east6     
      real,dimension(GLOBAL_1D_ARRAYETA,0:NWEIGHT) :: DV_west4, DV_east4
      real,dimension(PRIVATE_1DXI_SCRATCH_ARRAY) :: DV_south,DV_north
      real,dimension(GLOBAL_1D_ARRAYXI) :: DV_south6,DV_north6     
      real,dimension(GLOBAL_1D_ARRAYXI,0:NWEIGHT) :: DV_south4,DV_north4

      real :: lastcff
      real,dimension(:,:),allocatable :: tabtemp2d
!$AGRIF_DO_NOT_TREAT
      integer :: indinterp,nbgrid
      common/interp2d/indinterp,nbgrid
!$AGRIF_END_DO_NOT_TREAT      
#ifdef MPI
       include 'mpif.h'
#endif
  
# ifdef MPI
#  define LOCALLM Lmmpi
#  define LOCALMM Mmmpi
# else
#  define LOCALLM Lm
#  define LOCALMM Mm
# endif   
!          
# include "compute_auxiliary_bounds.h"
!
!      return
      irhox=Agrif_Irhox()
      irhoy=Agrif_Irhoy()
      irhot=Agrif_Irhot()
      
      rrhox = real(irhox)
      rrhoy = real(irhoy)
      rrhot = real(irhot)


      parentnbstep=
     &  Agrif_Parent(iif)
      
      if (V2DTimeindex .NE. parentnbstep) then

        
C$OMP BARRIER
C$OMP MASTER

        dVinterp = 0.

        tinterp=1.
#ifdef MASKING        
        Agrif_UseSpecialValue = .true.
#endif

        Agrif_SpecialValue = 0.
        
        nbgrid = Agrif_Fixed()
C        indinterp = 0 indinterp is set to zero in zetabc
C        allocate(tabtemp2d(i1t:i2t,0:j2v))
C        Call Agrif_Bc_variable(tabtemp2d,vbarid,calledweight=tinterp,
C     &   procname = v2dinterp)
        Call Agrif_Bc_variable(vbarid,calledweight=tinterp,
     &   procname = v2dinterp)
C        dVinterp(i1t:i2t,0:j2v)=tabtemp2d
C        deallocate(tabtemp2d)

        Agrif_UseSpecialvalue=.false.
C$OMP END MASTER
C$OMP BARRIER
        V2DTimeindex = parentnbstep
        V2DTimeindex2 = agrif_nb_step()
      endif
      
      if (agrif_nb_step() .EQ. V2DTimeindex2) then

          lastcff=0.
          do iter=iif,iif+irhot-1
          lastcff=lastcff+((iter+1-iif)/rrhot)*
     &        weight2(iif+irhot-1,iter)
          enddo
          
          lastcff=1./lastcff
                                
# ifdef AGRIF_OBC_SOUTH
      if (SOUTHERN_EDGE) then
          j=Jstr
          
          do i=Istr,Iend
          DV_south4(i,iif-1) = DV_avg3(i,j,iif-1)
          enddo
          
          if (iif+irhot-1<=nfast) then
           DV_south6(Istr:Iend)=0.
          do iter=0,iif-1
          cff1=weight2(iif+irhot-1,iter)
            do i=Istr,Iend
             DV_south6(i) = DV_south6(i) +cff1*DV_south4(i,iter)
            enddo
          enddo
          
          do iter=iif,iif+irhot-2
          do i=Istr,Iend
          DV_south6(i)=DV_south6(i)+((iif+irhot-1-iter)/rrhot)*
     &        weight2(iif+irhot-1,iter)*DV_south4(i,iif-1)
          enddo
          enddo
          
          do i=Istr,Iend
          DV_south6(i)=lastcff*((dVinterp(i,j)/rrhox)-DV_south6(i))
#  ifdef MASKING
     &                               * vmask(i,j)
#   endif          
          enddo      
          
          do i=Istr,Iend
          DV_south6(i)=(DV_south6(i)-DV_south4(i,iif-1))/rrhot
          enddo 
          endif           
                                       
      endif
# endif 

# ifdef AGRIF_OBC_NORTH
      if (NORTHERN_EDGE) then  
          j=Jend+1
          
          do i=Istr,Iend
          DV_north4(i,iif-1) = DV_avg3(i,j,iif-1)
          enddo
          
          if (iif+irhot-1<=nfast) then
           DV_north6(Istr:Iend)=0.
          do iter=0,iif-1
          cff1=weight2(iif+irhot-1,iter)
            do i=Istr,Iend
             DV_north6(i) = DV_north6(i) +cff1*DV_north4(i,iter)
            enddo
          enddo
          
          do iter=iif,iif+irhot-2
          do i=Istr,Iend
          DV_north6(i)=DV_north6(i)+((iif+irhot-1-iter)/rrhot)*
     &        weight2(iif+irhot-1,iter)*DV_north4(i,iif-1)
          enddo
          enddo
          

          
          do i=Istr,Iend
          DV_north6(i)=lastcff*((dVinterp(i,j)/rrhox)-DV_north6(i))
#  ifdef MASKING
     &                               * vmask(i,j)
#   endif             
          enddo      
          
          do i=Istr,Iend
          DV_north6(i)=(DV_north6(i)-DV_north4(i,iif-1))/rrhot
          enddo 
          endif                 
                                     
      endif
# endif

# ifdef AGRIF_OBC_WEST             
      if (WESTERN_EDGE) then
          i = Istr-1

          do j=Jstr,Jend
          DV_west4(j,iif-1) = DV_avg3(i,j,iif-1)
          enddo
          
          if (iif+irhot-1<=nfast) then
           DV_west6(Jstr:Jend)=0.
          do iter=0,iif-1
          cff1=weight2(iif+irhot-1,iter)
            do j=Jstr,Jend
             DV_west6(j) = DV_west6(j) +cff1*DV_west4(j,iter)
            enddo
          enddo
          
          do iter=iif,iif+irhot-2
          do j=Jstr,Jend
          DV_west6(j)=DV_west6(j)+((iif+irhot-1-iter)/rrhot)*
     &        weight2(iif+irhot-1,iter)*DV_west4(j,iif-1)
          enddo
          enddo
          

          
          do j=Jstr,Jend
          DV_west6(j)=lastcff*((dVinterp(i,j)/rrhoy)-DV_west6(j))
#  ifdef MASKING
     &                               * vmask(i,j)
#   endif                     
          enddo      
          
          do j=Jstr,Jend
          DV_west6(j)=(DV_west6(j)-DV_west4(j,iif-1))/rrhot
          enddo 
          endif
                                       
      endif
# endif

# ifdef AGRIF_OBC_EAST       
      if (EASTERN_EDGE) then
          i=Iend+1         
          
          do j=Jstr,Jend
          DV_east4(j,iif-1) = DV_avg3(i,j,iif-1)
          enddo
          
          if (iif+irhot-1<=nfast) then
           DV_east6(Jstr:Jend)=0.
          do iter=0,iif-1
          cff1=weight2(iif+irhot-1,iter)
            do j=Jstr,Jend
             DV_east6(j) = DV_east6(j) +cff1*DV_east4(j,iter)
            enddo
          enddo
          
          do iter=iif,iif+irhot-2
          do j=Jstr,Jend
          DV_east6(j)=DV_east6(j)+((iif+irhot-1-iter)/rrhot)*
     &        weight2(iif+irhot-1,iter)*DV_east4(j,iif-1)
          enddo
          enddo
          
          do j=Jstr,Jend
          DV_east6(j)=lastcff*((dVinterp(i,j)/rrhoy)-DV_east6(j))
#  ifdef MASKING
     &                               * vmask(i,j)
#   endif          
          enddo      
          
          do j=Jstr,Jend
          DV_east6(j)=(DV_east6(j)-DV_east4(j,iif-1))/rrhot
          enddo 
          endif          
                                 
      endif
# endif
      endif
          

                      
# ifdef AGRIF_OBC_SOUTH
      if (SOUTHERN_EDGE) then
          j=Jstr
          
          do i=Istr,Iend
           DV_south4(i,iif-1) = DV_avg3(i,j,iif-1)
          enddo
           
          do i=Istr,Iend
            DV_south(i)=DV_south4(i,iif-1)+DV_south6(i)
          enddo
                                       
      endif
# endif 

# ifdef AGRIF_OBC_NORTH
      if (NORTHERN_EDGE) then  
          j=Jend+1

          do i=Istr,Iend
           DV_north4(i,iif-1) = DV_avg3(i,j,iif-1)
          enddo
           
          do i=Istr,Iend
            DV_north(i)=DV_north4(i,iif-1)+DV_north6(i)
          enddo
                                               
      endif
# endif

# ifdef AGRIF_OBC_WEST             
      if (WESTERN_EDGE) then
          i = Istr-1         

          do j=Jstr,Jend
           DV_west4(j,iif-1) = DV_avg3(i,j,iif-1)
          enddo
           
          do j=Jstr,Jend
            DV_west(j)=DV_west4(j,iif-1)+DV_west6(j)
          enddo
                                       
      endif
# endif

# ifdef AGRIF_OBC_EAST       
      if (EASTERN_EDGE) then
          i=Iend+1
          
          do j=Jstr,Jend
           DV_east4(j,iif-1) = DV_avg3(i,j,iif-1)
          enddo
           
          do j=Jstr,Jend
            DV_east(j)=DV_east4(j,iif-1)+DV_east6(j)
          enddo          
                                 
      endif
# endif      
          
!# if defined M2_FRC_BRY || defined M2NUDGING
!
! Apply the value to vbclm or vbarbry
!
      cffy = g*dtfast*2./(1.+rrhoy)

#  ifdef AGRIF_2WAY
!     cffy = 0.
#  endif

#  ifdef AGRIF_OBC_SOUTH
      if (SOUTHERN_EDGE) then
         do i=Istr,Iend
#   ifdef M2_FRC_BRY
           vbarbry_south(i)=
#   else
           vbclm(i,Jstr)=
#   endif
     &                   (cffy/on_v(i,Jstr))*
     &                   (SSH(i,Jstr)-zeta(i,Jstr,knew))   +

#   ifdef AGRIF_FLUX_BC
     &              (2.*DV_south(i)/((h(i,Jstr-1)+zeta(i,Jstr-1,knew)
     &                                  +h(i,Jstr)+zeta(i,Jstr,knew))
     &                                                 *om_v(i,Jstr)))
#   else
     &                  DV_south(i)
#   endif
#   ifdef MASKING
     &                  *vmask(i,Jstr)
#   endif 
       enddo
      endif
#  endif 

#  ifdef AGRIF_OBC_NORTH
      if (NORTHERN_EDGE) then
        do i=Istr,Iend
#   ifdef M2_FRC_BRY
          vbarbry_north(i)=
#   else
          vbclm(i,Jend+1)=
#   endif
     &                    -(cffy/on_v(i,Jend+1))*
     &                     (SSH(i,Jend)-zeta(i,Jend,knew))  +

#   ifdef AGRIF_FLUX_BC
     &                  (2.*DV_north(i)/(( h(i,Jend)+zeta(i,Jend,knew)
     &                               +h(i,Jend+1)+zeta(i,Jend+1,knew))
     &                                               *om_v(i,Jend+1)))
#   else
     &                  DV_north(i)
#   endif
#   ifdef MASKING
     &                  *vmask(i,Jend+1)
#   endif 
        enddo
      endif
#  endif

#  ifdef AGRIF_OBC_EAST       
      if (EASTERN_EDGE) then
        do j=Jstr,Jend
#   ifdef M2_FRC_BRY
          vbarbry_east(j)=
#   else
          vbclm(Iend+1,j)=
#   endif
#   ifdef AGRIF_FLUX_BC
     &             (2.*DV_east(j)/(( h(Iend+1,j)+zeta(Iend+1,j,knew)
     &                           +h(Iend+1,j-1)+zeta(Iend+1,j-1,knew))
     &                                             *om_v(Iend+1,j)))
#   else
     &                 DV_east(j)
#   endif
#   ifdef MASKING
     &                 *vmask(Iend+1,j)
#   endif
        enddo
      endif       
#  endif 

#  ifdef AGRIF_OBC_WEST             
      if (WESTERN_EDGE) then
        do j=Jstr,Jend
#   ifdef M2_FRC_BRY
          vbarbry_west(j)=
#   else
          vbclm(Istr-1,j)=
#   endif
#   ifdef AGRIF_FLUX_BC
     &                (2.*DV_west(j)/((h(Istr-1,j)+zeta(Istr-1,j,knew)
     &                           +h(Istr-1,j-1)+zeta(Istr-1,j-1,knew))
     &                                                *om_v(Istr-1,j)))
#   else
     &                  DV_west(j)
#   endif
#   ifdef MASKING
     &                  *vmask(Istr-1,j)
#   endif
        enddo
      endif
#  endif
!# endif /* M2_FRC_BRY || M2NUDGING */
      return
      end

      subroutine v2Dinterp(tabres,i1,i2,j1,j2,before)
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "scalars.h"
# include "zoom.h"

       integer i1,i2,j1,j2
       real tabres(i1:i2,j1:j2)
       logical before
       integer i,j,iter,isize
       real :: cff1
!$AGRIF_DO_NOT_TREAT
      integer :: indinterp,nbgrid
      common/interp2d/indinterp,nbgrid
!$AGRIF_END_DO_NOT_TREAT  

       integer :: oldindinterp

       if (before) then
       isize = (j2-j1+1)*(i2-i1+1)
       
       IF (iif == 1) THEN
       IF (.NOT.allocated(coarsevaluesinterp)) THEN
       Allocate(coarsevaluesinterp(isize,0:nfast))
       ELSE       
       CALL checksizeinterp(indinterp+isize)
       ENDIF
       
       oldindinterp = indinterp
       
       do j=j1,j2
       do i=i1,i2
       oldindinterp=oldindinterp+1
       coarsevaluesinterp(oldindinterp,0) = 
     &  0.5*(h(i,j-1)+zeta(i,j-1,kstp)+h(i,j)+
     &  zeta(i,j,kstp))*om_v(i,j)*vbar(i,j,kstp)
       enddo
       enddo
              
       ENDIF
       
       oldindinterp = indinterp
       
       do j=j1,j2
       do i=i1,i2
       oldindinterp=oldindinterp+1
       coarsevaluesinterp(oldindinterp,iif) =
     &   0.5*(h(i,j-1)+zeta(i,j-1,knew)+h(i,j)+
     &  zeta(i,j,knew))*om_v(i,j)*vbar(i,j,knew)
       enddo
       enddo
          
       do iter=0,iif
          cff1 = weight2(iif,iter)
          call copy1d(tabres,coarsevaluesinterp(indinterp+1,iter),
     &                cff1,isize)
       enddo
       
       indinterp = oldindinterp

       else
          dVinterp(i1:i2,j1:j2) = tabres
       endif
      return
      end   
      
      subroutine copy1d(tab1,tab2,cff,isize)
      integer :: i,isize
      real :: cff
      real,dimension(isize) :: tab1,tab2
      
      do i=1,isize
      tab1(i)=tab1(i)+cff*tab2(i)
      enddo
      return
      end subroutine copy1d

!
      subroutine checksizeinterp(isize)
      real,dimension(:,:),allocatable :: tempvalues
      integer :: n1,isize
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "scalars.h"
# include "zoom.h"      
!$AGRIF_DO_NOT_TREAT
      integer :: indinterp,nbgrid
      common/interp2d/indinterp,nbgrid
!$AGRIF_END_DO_NOT_TREAT 

       IF (size(coarsevaluesinterp,1).LT.(isize)) THEN
       n1 = size(coarsevaluesinterp,1)
       allocate(tempvalues(n1,0:nfast))
       tempvalues=coarsevaluesinterp(1:n1,0:nfast)
       deallocate(coarsevaluesinterp)
       allocate(coarsevaluesinterp(isize,0:nfast))
       coarsevaluesinterp(1:n1,0:nfast) = tempvalues
              
       deallocate(tempvalues)
       
       ENDIF      
      return
      end                 
!
!====================================================================
!                   subroutine Agrif_zetabc_interp_tile
!====================================================================
!
      subroutine zetabc_interp_tile(Istr,Iend,Jstr,Jend
     &        ,Zeta_west4,Zeta_east4,Zeta_west6,Zeta_east6
     &        ,Zeta_south4,Zeta_north4,Zeta_south6,Zeta_north6)
      use AGRIF_Util
!     implicit none
# include "param.h"
# include "boundary.h"
# include "climat.h"
# include "grid.h"
# include "ocean2d.h"
# include "scalars.h"
# include "zoom.h"
      integer Istr,Iend,Jstr,Jend, i,j, i1, j1
      real tinterp
      Integer itrcind
      INTEGER :: parentknew, parentkstp,nbstep3dparent
      real t1,t2,t3,t4,t5,t6,t9,t10,t11,t7
      real tin(2), tout(2)
      real cff1
      real zeta1,zeta2
      external zetainterp
      integer :: irhot, irhox, irhoy
      real :: rrhot
      integer :: iter
      real :: onemtinterp
      integer :: parentnbstep
      integer :: pngrid

      real,dimension(Istr-1:Istr,GLOBAL_1D_ARRAYETA) :: Zeta_west6
      real,dimension(Iend:Iend+1,GLOBAL_1D_ARRAYETA) :: Zeta_east6     
      real,dimension(GLOBAL_1D_ARRAYXI,Jstr-1:Jstr) :: Zeta_south6
      real,dimension(GLOBAL_1D_ARRAYXI,Jend:Jend+1) :: Zeta_north6 
     
      real,dimension(Istr-1:Istr,Jstr-1:Jend+1) :: Zeta_west7
      real,dimension(Iend:Iend+1,Jstr-1:Jend+1) :: Zeta_east7     
      real,dimension(Istr-1:Iend+1,Jstr-1:Jstr) :: Zeta_south7
      real,dimension(Istr-1:Iend+1,Jend:Jend+1) :: Zeta_north7 

      real,dimension(Istr-1:Istr,GLOBAL_1D_ARRAYETA,0:NWEIGHT) :: Zeta_west4
      real,dimension(Iend:Iend+1,GLOBAL_1D_ARRAYETA,0:NWEIGHT) :: Zeta_east4
      real,dimension(GLOBAL_1D_ARRAYXI,Jstr-1:Jstr,0:NWEIGHT) :: Zeta_south4
      real,dimension(GLOBAL_1D_ARRAYXI,Jend:Jend+1,0:NWEIGHT) :: Zeta_north4
!      real,dimension(:,:),allocatable :: tabtemp2d
      real lastcff
!$AGRIF_DO_NOT_TREAT
      integer :: nbgrid, indinterp
      common/interp2d/indinterp,nbgrid
!$AGRIF_END_DO_NOT_TREAT 
                 
#  ifdef MPI
#   define LOCALLM Lmmpi
#   define LOCALMM Mmmpi
#  else
#   define LOCALLM Lm
#   define LOCALMM Mm
#  endif
!
# include "compute_auxiliary_bounds.h"

!
!       return      
       irhot = Agrif_Irhot()
       irhox = Agrif_Irhox()
       irhoy = Agrif_Irhoy()
       rrhot = real(irhot)



      parentnbstep=
     &  Agrif_Parent(iif)

      if (ZetaTimeindex .NE. parentnbstep) then
      
C$OMP BARRIER
C$OMP MASTER

        dZtinterp = 0.

            tinterp=1.
                          
#ifdef MASKING            
            Agrif_UseSpecialvalue=.true.
#endif            
            Agrif_Specialvalue=0.

        nbgrid = Agrif_Fixed()
        pngrid = Agrif_Parent_Fixed()
        if (nbgrid == grids_at_level(pngrid,0)) indinterp = 0

C        allocate(tabtemp2d(i1t:i2t,j1t:j2t))
            Call Agrif_Bc_variable(zetaid,
     &                                        calledweight=tinterp,
     &                                        procname=zetainterp)
C       dZtinterp(i1t:i2t,j1t:j2t)=tabtemp2d
C       deallocate(tabtemp2d)
            Agrif_UseSpecialvalue=.false.
C$OMP END MASTER
C$OMP BARRIER
            ZetaTimeindex = parentnbstep
            ZetaTimeindex2 = agrif_nb_step()
          endif       
          
          if (agrif_nb_step() .EQ. ZetaTimeindex2) then

          lastcff=0.
          do iter=iif,iif+irhot-1
          lastcff=lastcff+((iter+1-iif)/rrhot)*
     &        weight2(iif+irhot-1,iter)
          enddo
          
          lastcff=1./lastcff
              
# ifdef AGRIF_OBC_SOUTH
      if (SOUTHERN_EDGE) then

          do j=Jstr-1,Jstr
          do i=IstrU-1,Iend
          Zeta_south4(i,j,iif-1) = Zt_avg3(i,j,iif-1)
          enddo
          enddo
          
          if (iif+irhot-1<=nfast) then
           Zeta_south7(IstrU-1:Iend,Jstr-1:Jstr)=0.
          do iter=0,iif-1
          cff1=weight2(iif+irhot-1,iter)
            do j=Jstr-1,Jstr
            do i=IstrU-1,Iend
             Zeta_south7(i,j) = Zeta_south7(i,j)
     &                         +cff1*Zeta_south4(i,j,iter)
            enddo
            enddo
          enddo
          
          do iter=iif,iif+irhot-2
          do j=Jstr-1,Jstr          
          do i=IstrU-1,Iend
          Zeta_south7(i,j)=Zeta_south7(i,j)+((iif+irhot-1-iter)/rrhot)*
     &        weight2(iif+irhot-1,iter)*Zeta_south4(i,j,iif-1)
          enddo
          enddo
          enddo          
          

          do j=Jstr-1,Jstr         
          do i=IstrU-1,Iend
          Zeta_south7(i,j)=lastcff*(dZtinterp(i,j)-Zeta_south7(i,j))
          enddo
          enddo                
          
          do j=Jstr-1,Jstr
          do i=IstrU-1,Iend
          Zeta_south6(i,j)=(Zeta_south7(i,j)-Zeta_south4(i,j,iif-1))
     &                           /rrhot
          enddo 
          enddo          
          endif 
                                          
      endif
# endif 

# ifdef AGRIF_OBC_NORTH
      if (NORTHERN_EDGE) then
             
          do j=Jend,Jend+1
          do i=IstrU-1,Iend
          Zeta_north4(i,j,iif-1) = Zt_avg3(i,j,iif-1)
          enddo
          enddo
          
          if (iif+irhot-1<=nfast) then
           Zeta_north7(IstrU-1:Iend,Jend:Jend+1)=0.
          do iter=0,iif-1
          cff1=weight2(iif+irhot-1,iter)
            do j=Jend,Jend+1
            do i=IstrU-1,Iend
             Zeta_north7(i,j) = Zeta_north7(i,j)
     &                         +cff1*Zeta_north4(i,j,iter)
            enddo
            enddo
          enddo
          
          do iter=iif,iif+irhot-2
          do j=Jend,Jend+1          
          do i=IstrU-1,Iend
          Zeta_north7(i,j)=Zeta_north7(i,j)+((iif+irhot-1-iter)/rrhot)*
     &        weight2(iif+irhot-1,iter)*Zeta_north4(i,j,iif-1)
          enddo
          enddo
          enddo          
          
          
          do j=Jend,Jend+1         
          do i=IstrU-1,Iend
          Zeta_north7(i,j)=lastcff*(dZtinterp(i,j)-Zeta_north7(i,j))
          enddo
          enddo
                   
          do j=Jend,Jend+1
          do i=IstrU-1,Iend
          Zeta_north6(i,j)=(Zeta_north7(i,j)-Zeta_north4(i,j,iif-1))
     &                           /rrhot
          enddo 
          enddo
                             
          endif                              
      endif
# endif

# ifdef AGRIF_OBC_WEST             
      if (WESTERN_EDGE) then           

          do j=JstrV-1,Jend
          do i=Istr-1,Istr
          Zeta_west4(i,j,iif-1) = Zt_avg3(i,j,iif-1)
          enddo
          enddo
          
          if (iif+irhot-1<=nfast) then
           Zeta_west7(Istr-1:Istr,JstrV-1:Jend)=0.
          do iter=0,iif-1
          cff1=weight2(iif+irhot-1,iter)
            do j=JstrV-1,Jend
            do i=Istr-1,Istr
             Zeta_west7(i,j) = Zeta_west7(i,j)
     &                         +cff1*Zeta_west4(i,j,iter)
            enddo
            enddo
          enddo
          
          do iter=iif,iif+irhot-2
          do j=JstrV-1,Jend
          do i=Istr-1,Istr
          Zeta_west7(i,j)=Zeta_west7(i,j)+((iif+irhot-1-iter)/rrhot)*
     &        weight2(iif+irhot-1,iter)*Zeta_west4(i,j,iif-1)
          enddo
          enddo
          enddo          
          

          do j=JstrV-1,Jend
          do i=Istr-1,Istr
          Zeta_west7(i,j)=lastcff*(dZtinterp(i,j)-Zeta_west7(i,j))
          enddo
          enddo                
          
          do j=JstrV-1,Jend
          do i=Istr-1,Istr
          Zeta_west6(i,j)=(Zeta_west7(i,j)-Zeta_west4(i,j,iif-1))
     &                           /rrhot
          enddo 
          enddo          
          endif 
                                          
      endif
# endif

# ifdef AGRIF_OBC_EAST       
      if (EASTERN_EDGE) then

          do j=JstrV-1,Jend
          do i=Iend,Iend+1
          Zeta_east4(i,j,iif-1) = Zt_avg3(i,j,iif-1)
          enddo
          enddo
          
          if (iif+irhot-1<=nfast) then
           Zeta_east7(Iend:Iend+1,JstrV-1:Jend)=0.
          do iter=0,iif-1
          cff1=weight2(iif+irhot-1,iter)
            do j=JstrV-1,Jend
            do i=Iend,Iend+1
             Zeta_east7(i,j) = Zeta_east7(i,j)
     &                         +cff1*Zeta_east4(i,j,iter)
            enddo
            enddo
          enddo
          
          do iter=iif,iif+irhot-2
          do j=JstrV-1,Jend
          do i=Iend,Iend+1
          Zeta_east7(i,j)=Zeta_east7(i,j)+((iif+irhot-1-iter)/rrhot)*
     &        weight2(iif+irhot-1,iter)*Zeta_east4(i,j,iif-1)
          enddo
          enddo
          enddo
          

          do j=JstrV-1,Jend         
          do i=Iend,Iend+1
          Zeta_east7(i,j)=lastcff*(dZtinterp(i,j)-Zeta_east7(i,j))
          enddo
          enddo                
          
          do j=JstrV-1,Jend
          do i=Iend,Iend+1
          Zeta_east6(i,j)=(Zeta_east7(i,j)-Zeta_east4(i,j,iif-1))
     &                           /rrhot
          enddo 
          enddo          
          endif 
                                         
      endif
# endif
          endif

#  ifdef AGRIF_OBC_SOUTH             
          if (SOUTHERN_EDGE) then
          
          do j=Jstr-1,Jstr
          do i=IstrU-1,Iend
          Zeta_south4(i,j,iif-1) = Zt_avg3(i,j,iif-1)
          enddo
          enddo
          do j=Jstr-1,Jstr
          do i=IstrU-1,Iend
          SSH(i,j)=Zeta_south4(i,j,iif-1)+Zeta_south6(i,j)
          enddo
          enddo
#   if defined Z_FRC_BRY
            do i=IstrU-1,Iend
              zetabry_south(i)=Zeta_south4(i,Jstr-1,iif-1)+
     &                         Zeta_south6(i,Jstr-1)
            enddo
#   endif
          
          endif
#  endif
#  ifdef AGRIF_OBC_NORTH 
          if (NORTHERN_EDGE) then
          
          do j=Jend,Jend+1
          do i=IstrU-1,Iend
          Zeta_north4(i,j,iif-1) = Zt_avg3(i,j,iif-1)
          enddo
          enddo
          do j=Jend,Jend+1
          do i=IstrU-1,Iend
          SSH(i,j)=Zeta_north4(i,j,iif-1)+Zeta_north6(i,j)
          enddo
          enddo
#   if defined Z_FRC_BRY
            do i=IstrU-1,Iend
              zetabry_north(i)=Zeta_north4(i,Jend+1,iif-1)+
     &                         Zeta_north6(i,Jend+1)
            enddo
#   endif
          endif
#  endif
#  ifdef AGRIF_OBC_WEST 
          if (WESTERN_EDGE) then
          
          do j=JstrV-1,Jend
          do i=Istr-1,Istr
          Zeta_west4(i,j,iif-1) = Zt_avg3(i,j,iif-1)
          enddo
          enddo
          do j=JstrV-1,Jend
          do i=Istr-1,Istr
          SSH(i,j)=Zeta_west4(i,j,iif-1)+Zeta_west6(i,j)
          enddo
          enddo
#   if defined Z_FRC_BRY
            do j=JstrV-1,Jend
              zetabry_west(j)=Zeta_west4(Istr-1,j,iif-1)+
     &                        Zeta_west6(Istr-1,j)
            enddo
#   endif          
          endif
#  endif
#  ifdef AGRIF_OBC_EAST
          if (EASTERN_EDGE) then
          
          do j=JstrV-1,Jend
          do i=Iend,Iend+1
          Zeta_east4(i,j,iif-1) = Zt_avg3(i,j,iif-1)
          enddo
          enddo
          do j=JstrV-1,Jend
          do i=Iend,Iend+1
          SSH(i,j)=Zeta_east4(i,j,iif-1)+Zeta_east6(i,j)
          enddo
          enddo
#   if defined Z_FRC_BRY
            do j=JstrV-1,Jend
              zetabry_east(j)=Zeta_east4(Iend+1,j,iif-1)+
     &                        Zeta_east6(Iend+1,j)
            enddo
#   endif
          endif
#  endif
     
      return
      end

      subroutine zetainterp(tabres,i1,i2,j1,j2,before)
      implicit none
# include "param.h"
# include "ocean2d.h"
# include "scalars.h"
# include "zoom.h"

       integer i1,i2,j1,j2
       real tabres(i1:i2,j1:j2)
       logical before
       integer i,j,iter,isize
       real :: cff1
!$AGRIF_DO_NOT_TREAT
      integer :: indinterp,nbgrid
      common/interp2d/indinterp,nbgrid
!$AGRIF_END_DO_NOT_TREAT  

       integer :: oldindinterp

       if (before) then
       isize = (j2-j1+1)*(i2-i1+1)
       
       IF (iif == 1) THEN
       IF (.NOT.allocated(coarsevaluesinterp)) THEN
       Allocate(coarsevaluesinterp(isize,0:nfast))
       ELSE       
       CALL checksizeinterp(indinterp+isize)
       ENDIF
       
       oldindinterp = indinterp
       
       do j=j1,j2
       do i=i1,i2
       oldindinterp=oldindinterp+1
       coarsevaluesinterp(oldindinterp,0) = 
     &  zeta(i,j,kstp)
       enddo
       enddo
              
       ENDIF
       
       oldindinterp = indinterp
       
       do j=j1,j2
       do i=i1,i2
       oldindinterp=oldindinterp+1
       coarsevaluesinterp(oldindinterp,iif) =
     &   zeta(i,j,knew)
       enddo
       enddo
          
       do iter=0,iif
          cff1 = weight2(iif,iter)
          call copy1d(tabres,coarsevaluesinterp(indinterp+1,iter),
     &                cff1,isize)
       enddo
       
       indinterp = oldindinterp
        else
         dZtinterp(i1:i2,j1:j2) = tabres
       endif     
      return
      end
      
# ifdef WKB_WWAVE
      subroutine wkbbc_interp_tile(Istr,Iend,Jstr,Jend)
      use AGRIF_Util
# include "param.h"
# include "scalars.h"
# include "boundary.h"
# include "zoom.h"
# include "wkb_wwave.h"
      external :: wacinterp, wkxinterp,wkeinterp
#ifdef WAVE_ROLLER
     &,warinterp
#endif
!          
# include "compute_auxiliary_bounds.h"

      dWactinterp = 0.
      dWarinterp  = 0.
      dWkxtinterp = 0.
      dWketinterp = 0.
      Agrif_UseSpecialValue = .TRUE.
      Agrif_SpecialValue = 0.
   
      Call Agrif_Bc_variable(wacid,
     &   procname=wacinterp)
#ifdef WAVE_ROLLER
      Call Agrif_Bc_variable(warid,
     &   procname=warinterp)
#endif
      Call Agrif_Bc_variable(wkxid,
     &   procname=wkxinterp)
      Call Agrif_Bc_variable(wkeid,
     &   procname=wkeinterp)
       Agrif_UseSpecialValue = .false.

#  ifdef AGRIF_OBC_SOUTH
       if (SOUTHERN_EDGE) then
         do i=IstrR,IendR
           wacbry_south(i)=dWactinterp(i,Jstr-1)
#ifdef WAVE_ROLLER
           warbry_south(i)=dWartinterp(i,Jstr-1)
#endif
           wkxbry_south(i)=dWkxtinterp(i,Jstr-1)
           wkebry_south(i)=dWketinterp(i,Jstr-1)
         enddo
      endif
#  endif
#  ifdef AGRIF_OBC_NORTH
      if (NORTHERN_EDGE) then      
        do i=IstrR,IendR
          wacbry_north(i)=dWactinterp(i,Jend+1)
#ifdef WAVE_ROLLER
          warbry_north(i)=dWartinterp(i,Jend+1)
#endif
          wkxbry_north(i)=dWkxtinterp(i,Jend+1)
          wkebry_north(i)=dWketinterp(i,Jend+1)
        enddo
      endif      
#  endif
#  ifdef AGRIF_OBC_WEST
      if (WESTERN_EDGE) then      
        do j=JstrR,JendR
          wacbry_west(j)=dWactinterp(Istr-1,j)
#ifdef WAVE_ROLLER
          warbry_west(j)=dWartinterp(Istr-1,j)
#endif
          wkxbry_west(j)=dWkxtinterp(Istr-1,j)
          wkebry_west(j)=dWketinterp(Istr-1,j)
        enddo
      endif      
#  endif
#  ifdef AGRIF_OBC_EAST
      if (EASTERN_EDGE) then      
        do j=JstrR,JendR
          wacbry_east(j)=dWactinterp(Iend+1,j)
#ifdef WAVE_ROLLER
          warbry_east(j)=dWartinterp(Iend+1,j)
#endif
          wkxbry_east(j)=dWkxtinterp(Iend+1,j)
          wkebry_east(j)=dWketinterp(Iend+1,j)
        enddo
      endif
#  endif     
      return
      end
      
      subroutine wacinterp(tabres,i1,i2,j1,j2,before)
      implicit none
# include "param.h"
# include "wkb_wwave.h"
# include "scalars.h"
# include "zoom.h"

       integer i1,i2,j1,j2
       logical before
       real tabres(i1:i2,j1:j2)

       if (before) then
       tabres(i1:i2,j1:j2) = 
     &               wac(i1:i2,j1:j2,2)
       else
       dWactinterp(i1:i2,j1:j2)=tabres(i1:i2,j1:j2)
       endif
      return
      end       
#ifdef WAVE_ROLLER
      subroutine warinterp(tabres,i1,i2,j1,j2,before)
      implicit none
# include "param.h"
# include "wkb_wwave.h"
# include "scalars.h"
# include "zoom.h"

       integer i1,i2,j1,j2
       logical before
       real tabres(i1:i2,j1:j2)

       if (before) then
       tabres(i1:i2,j1:j2) = 
     &               war(i1:i2,j1:j2,2)
       else
         dWartinterp(i1:i2,j1:j2)=tabres(i1:i2,j1:j2)
       endif
      return
      end       
#endif
      subroutine wkxinterp(tabres,i1,i2,j1,j2,before)
      implicit none
# include "param.h"
# include "wkb_wwave.h"
# include "scalars.h"
# include "zoom.h"

       integer i1,i2,j1,j2
       logical before
       real tabres(i1:i2,j1:j2)

       if (before) then
       tabres(i1:i2,j1:j2) = 
     &               wkx(i1:i2,j1:j2,2)
       else
         dWkxtinterp(i1:i2,j1:j2)=tabres(i1:i2,j1:j2)
       endif
      return
      end       
      subroutine wkeinterp(tabres,i1,i2,j1,j2,before)
      implicit none
# include "param.h"
# include "wkb_wwave.h"
# include "scalars.h"
# include "zoom.h"

       integer i1,i2,j1,j2
       logical before
       real tabres(i1:i2,j1:j2)

       if (before) then
       tabres(i1:i2,j1:j2) = 
     &               wke(i1:i2,j1:j2,2)
       else
       dWketinterp(i1:i2,j1:j2)=tabres(i1:i2,j1:j2)
       endif
      return
      end       

# endif /* WKB_WWAVE */           
#else
      subroutine zommbc_2D_empty()
      return
      end
#endif
