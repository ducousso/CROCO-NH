! $Id: update2D.F 1458 2014-02-03 15:01:25Z gcambon $
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
#include "cppdefs.h"
!====================================================================
!                   subroutine Agrif_update
!====================================================================
!
# if defined AGRIF && defined SOLVE3D && defined AGRIF_2WAY

      subroutine ExchangeParentValues()
#  include "param.h"
#  include "coupling.h"

#if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r2d_tile (1,Lm,1,Mm,
     &                   Zt_avg1(START_2D_ARRAY))
#endif

      end subroutine ExchangeParentValues


      subroutine update2d
!
! Update in case of 2-way nesting
!
      use Agrif_Util
      implicit none
#  include "param.h"
#  include "grid.h"
#  include "ocean3d.h"
#  include "ocean2d.h"
#  include "scalars.h"
#  include "zoom.h"	
#  include "coupling.h"
!      real tabtemp(GLOBAL_2D_ARRAY,4)
      external updateubar,updatevbar,updatezeta
      external updateduavg2, updatedvavg2
      real,dimension(:,:),pointer :: ztavg1_parent,duavg2parent
      real,dimension(:,:),pointer :: parenth,parenton_u,parentom_v
      real,dimension(:,:),pointer ::dvavg2parent
      integer i,j,ipu,jpu,ipv,jpv,ipr,jpr
      real :: dtparent
      real,dimension(:,:),pointer :: pmparent,pnparent,rmaskparent
      real,dimension(:,:,:),pointer :: zetaparent,parentzeta
      real,dimension(:,:,:),pointer :: parentubar,parentdu_avg1
      real,dimension(:,:,:),pointer :: parentdv_avg1,parentvbar
      real :: t1,t2,t3,t4,t5,t6,tind(5)
      integer j1
      integer :: parentknew,parentiif,parentnnew,parentkstp
      integer :: iter, irhox, irhoy, irhot
      real :: cff
      real :: invrrhot
      integer :: pp
      integer :: idecal
      real,dimension(:,:), allocatable :: duavg2parentold
      real,dimension(:,:), allocatable :: dvavg2parentold 
      integer :: pngrid

      
!$AGRIF_DO_NOT_TREAT
      logical global_update_2d
      common/globalupdate/global_update_2d
      integer :: iiffine, irhotfine, nbgrid
      common/updateubar2val/iiffine,irhotfine,
     &   nbgrid      
!$AGRIF_END_DO_NOT_TREAT      
# ifdef MPI
#  define LOCALLM Lmmpi
#  define LOCALMM Mmmpi
# else
#  define LOCALLM Lm
#  define LOCALMM Mm
# endif 
      
!      return

      irhot = Agrif_Irhot()

      
      IF ((mod(iif+(nbcoarse-1)*mod(nfast,irhot),irhot).NE.0)) RETURN

         iiffine = iif
         irhotfine = Agrif_Irhot()
         nbgrid = Agrif_Fixed()
         
C$OMP BARRIER
C$OMP MASTER
        irhox = Agrif_Irhox()
        irhoy = Agrif_Irhoy()
     
        pngrid = Agrif_Parent_Fixed()
        if (nbgrid == grids_at_level(pngrid,0)) then
           Call Agrif_Set_Parent(indupdate,0) 
        endif
        
#ifdef MASKING
      Agrif_UseSpecialValueInUpdate = .TRUE.
#endif
      Agrif_SpecialValueFineGrid = 0.
      
      indupdate = 0
      
      global_update_2d = .FALSE.

#if defined AGRIF_UPDATE_DECAL
       Call Agrif_Update_Variable(updatezetaid,
     &         locupdate=(/1,4/),procname = updatezeta)

      Agrif_UseSpecialValueInUpdate = .FALSE.
            
      Call Agrif_Update_Variable(updateubarid,
     &     locupdate1=(/0,3/),locupdate2=(/1,4/),
     &     procname = updateubar)
     
      Call Agrif_Update_Variable(updatevbarid,
     &     locupdate1=(/1,4/),locupdate2=(/0,3/),
     &     procname = updatevbar) 
     
#else
       Call Agrif_Update_Variable(updatezetaid,
     &         locupdate=(/0,1/),procname = updatezeta)

      Agrif_UseSpecialValueInUpdate = .FALSE.
            
      Call Agrif_Update_Variable(updateubarid,
     &     locupdate1=(/0,1/),locupdate2=(/0,1/),
     &     procname = updateubar)
     
      Call Agrif_Update_Variable(updatevbarid,
     &     locupdate1=(/0,1/),locupdate2=(/0,1/),
     &     procname = updatevbar)          
#endif

      IF (iif == nfast) THEN
        
      IF (nbcoarse == irhot) THEN

      global_update_2d = .TRUE.

#ifdef MASKING
      Agrif_UseSpecialValueInUpdate = .TRUE.
#endif
      Agrif_SpecialValueFineGrid = 0.

#if defined AGRIF_UPDATE_DECAL
      Call Agrif_Update_Variable(updatezetaid,
     &         locupdate=(/1,0/),procname = updatezeta)

      Agrif_UseSpecialValueInUpdate = .FALSE.

      Call Agrif_Update_Variable(updateubarid,
     &     locupdate1=(/0,-1/),locupdate2=(/1,-2/),
     &     procname = updateubar)
     
      Call Agrif_Update_Variable(updatevbarid,
     &     locupdate1=(/1,-2/),locupdate2=(/0,-1/),
     &     procname = updatevbar)
     
#else
      Call Agrif_Update_Variable(updatezetaid,
     &         procname = updatezeta)

      Agrif_UseSpecialValueInUpdate = .FALSE.

      Call Agrif_Update_Variable(updateubarid,
     &     procname = updateubar)
     
      Call Agrif_Update_Variable(updatevbarid,
     &     procname = updatevbar)
#endif

      Agrif_UseSpecialValueInUpdate = .FALSE.

#ifdef AGRIF_CONSERV_VOL

#if defined AGRIF_UPDATE_DECAL && !defined AGRIF_OLD_CONSERV

      Call Agrif_Update_Variable(updateduavg2id,
     &     locupdate1=(/0,0/),locupdate2=(/1,1/),        
     &         procname = updateduavg2)
     
      Call Agrif_Update_Variable(updatedvavg2id,
     &     locupdate1=(/1,1/),locupdate2=(/0,0/),         
     &         procname = updatedvavg2)

#if defined MPI
      Call Agrif_ChildGrid_To_ParentGrid()
      Call ExchangeParentValues()
      Call Agrif_ParentGrid_To_ChildGrid()
#endif
     
#endif

#endif
      DU_avg1(:,:,4) = 0.
      DV_avg1(:,:,4) = 0.
      ENDIF
      
      ENDIF
C$OMP END MASTER

      return
      end
      
      Subroutine Updateubar(tabres,i1,i2,j1,j2,k1,k2,before,nb,ndir)
      implicit none
#  include "param.h"
#  include "grid.h"
#  include "ocean2d.h"
#  include "coupling.h"
#  include "scalars.h"
#  include "zoom.h"
      integer i1,i2,j1,j2,k1,k2
      real tabres(i1:i2,j1:j2,k1:k2)
      logical before
      integer nb, ndir
      real hzu
      real t1,t2,t3
      
      integer i,j
       real t4,t8
      integer :: iter, iifparent
      integer :: oldindupdate
      real :: rrhoy
      real :: cff, cff1, cff2
      logical :: western_side, eastern_side,northern_side,southern_side
!$AGRIF_DO_NOT_TREAT
      logical global_update_2d
      common/globalupdate/global_update_2d
!      real,dimension(:,:,:),pointer :: finevalues
!      real,dimension(:,:,:),pointer :: coarsevalues
      integer :: iiffine, irhotfine, nbgrid
      common/updateubar2val/iiffine,irhotfine,
     &   nbgrid      
!      common/updateubar2val/finevalues,coarsevalues, iiffine,irhotfine,
!     &   nbgrid
!$AGRIF_END_DO_NOT_TREAT  
       integer irhot, ibegin,isize  
       real tabtemp(i1:i2,j1:j2) 
       integer :: n1,n2,n3 
              
              
      IF (before) THEN
         rrhoy = real(Agrif_Irhoy())
 
         IF (global_update_2d) THEN
           tabres(i1:i2,j1:j2,1) = rrhoy * DU_avg1(i1:i2,j1:j2,nnew)
           RETURN
         ENDIF

         irhot=Agrif_Irhot()
         
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)

         ibegin = min(irhot,iif+1)
         
          IF (iif .LE. irhot) THEN
          ibegin = iif + 1
          ENDIF

          do iter=1,ibegin
          do j=j1,j2  
          do i=i1,i2                 
          tabres(i,j,iter) = du_avg3(i,j,iif-iter+1)
          enddo
          enddo          
          enddo
         
         tabres = rrhoy * tabres
         
       ELSE

       IF (global_update_2d) THEN
         DU_avg1(i1:i2,j1:j2,nnew) = tabres(i1:i2,j1:j2,1)
#ifdef MASKING
     &             *umask(i1:i2,j1:j2)
#endif         
         RETURN
       ENDIF
              
       ibegin = min(irhotfine,iiffine+1)

        IF (iiffine .LE. irhotfine) THEN
         ibegin = iiffine + 1
        ENDIF

       isize = (j2-j1+1)*(i2-i1+1)
       
       IF ((nbstep3d == 0).AND.(iif == 1)) THEN
       IF (.NOT.allocated(finevalues)) THEN
       Allocate(finevalues(isize,0:nfast))
       Allocate(coarsevalues(isize,0:nfast))
       ELSE       
       CALL checksize(indupdate+isize)
       ENDIF
       ENDIF
       
          do iter=1,ibegin
          oldindupdate = indupdate
          do j=j1,j2  
          do i=i1,i2
          oldindupdate = oldindupdate + 1
          finevalues(oldindupdate,iiffine-iter+1)
     &         = tabres(i,j,iter)
          enddo
          enddo          
          enddo
          
          IF (iif == 1) THEN
          oldindupdate = indupdate
          do j=j1,j2
          do i=i1,i2
          oldindupdate = oldindupdate + 1
          coarsevalues(oldindupdate,0) = DU_avg1(i,j,nstp)
          enddo
          enddo        
          ENDIF   
          
          
         tabtemp = 0.
         do iter=0,iif-1
         cff = -weight2(iif,iter)
         call copy1d(tabtemp,coarsevalues(indupdate+1,iter),
     &               cff,isize)
         enddo    
         
         do iter=0,iiffine
         cff=weight2(iiffine,iter)
         call copy1d(tabtemp,finevalues(indupdate+1,iter),
     &               cff,isize)
         enddo
         
         tabtemp = tabtemp/weight2(iif,iif)
                               
         DO j=j1,j2
           DO i=i1,i2
!           if (i>lbound(h,1)) then
        t1 = 0.5*(zeta(i,j,knew)+h(i,j)+
     &  zeta(i-1,j,knew)+h(i-1,j))*on_u(i,j)
!            else
!            t1 = 1.
!            endif
           t2 = tabtemp(i,j)
#ifdef MASKING
     &           * umask(i,j)
#endif
          ubar(i,j,knew) = t2/t1
         indupdate = indupdate + 1
          coarsevalues(indupdate,iif) = tabtemp(i,j)
              
          ENDDO
         ENDDO 
       ENDIF
      
      return
      end 
      
      Subroutine Updatezeta(tabres,i1,i2,j1,j2,k1,k2,before,nb,ndir)
      implicit none
#  include "param.h"
#  include "grid.h"
#  include "ocean2d.h"
#  include "coupling.h"
#  include "scalars.h"
#  include "zoom.h"
      integer i1,i2,j1,j2,k1,k2
      real tabres(i1:i2,j1:j2,k1:k2)
      logical before
      integer nb, ndir
      real hzu
      real t1,t2,t3
      
      integer i,j
       real t4,t8
      integer :: iter, iifparent
      integer :: oldindupdate
      real :: cff, cff1, cff2
      logical :: western_side, eastern_side,northern_side,southern_side
!$AGRIF_DO_NOT_TREAT
      logical global_update_2d
      common/globalupdate/global_update_2d
      integer :: iiffine, irhotfine, nbgrid
      common/updateubar2val/iiffine,irhotfine,
     &   nbgrid
!$AGRIF_END_DO_NOT_TREAT  
       integer irhot, ibegin,isize  
       real tabtemp(i1:i2,j1:j2) 
       integer :: n1,n2,n3 

      IF (before) THEN
 
         IF (global_update_2d) THEN
           tabres(i1:i2,j1:j2,1) = Zt_avg1(i1:i2,j1:j2)
           RETURN
         ENDIF
                 
         irhot=Agrif_Irhot()
         
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1) 
         northern_side = (nb == 2).AND.(ndir == 2)

         ibegin = min(irhot,iif+1)

         IF (iif .LE. irhot) THEN
            ibegin = iif + 1
         ENDIF  

          do iter=1,ibegin
          do j=j1,j2  
          do i=i1,i2    
          tabres(i,j,iter) = Zt_avg3(i,j,iif-iter+1)
          enddo
          enddo          
          enddo

       ELSE

       IF (global_update_2d) THEN
         Zt_avg1(i1:i2,j1:j2) = tabres(i1:i2,j1:j2,1)
#ifdef MASKING
     &             *rmask(i1:i2,j1:j2)
#endif
         RETURN
       ENDIF
              
       ibegin = min(irhotfine,iiffine+1)
       
       IF (iiffine .LE. irhotfine) THEN
        ibegin = iiffine + 1
       ENDIF
       
       isize = (j2-j1+1)*(i2-i1+1)
       
       IF ((nbstep3d == 0).AND.(iif == 1)) THEN
       IF (.NOT.allocated(finevalues)) THEN
       Allocate(finevalues(isize,0:nfast))
       Allocate(coarsevalues(isize,0:nfast))
       ELSE       
       CALL checksize(indupdate+isize)
       ENDIF
       ENDIF
       
          do iter=1,ibegin
          oldindupdate = indupdate
          do j=j1,j2  
          do i=i1,i2
          oldindupdate = oldindupdate + 1
          finevalues(oldindupdate,iiffine-iter+1)
     &         = tabres(i,j,iter)
          enddo
          enddo          
          enddo
          
          IF (iif == 1) THEN
          oldindupdate = indupdate
          do j=j1,j2
          do i=i1,i2
          oldindupdate = oldindupdate + 1
          coarsevalues(oldindupdate,0) = zeta(i,j,kstp)
          enddo
          enddo        
          ENDIF   
          
          
         tabtemp = 0.
         do iter=0,iif-1
         cff = -weight2(iif,iter)
         call copy1d(tabtemp,coarsevalues(indupdate+1,iter),
     &               cff,isize)
         enddo    
         
         do iter=0,iiffine
         cff=weight2(iiffine,iter)
         call copy1d(tabtemp,finevalues(indupdate+1,iter),
     &               cff,isize)        
         enddo
         
         tabtemp = tabtemp/weight2(iif,iif)
                               
         DO j=j1,j2
           DO i=i1,i2
            
           t2 = tabtemp(i,j)
#ifdef MASKING
     &           * rmask(i,j)
#endif
          zeta(i,j,knew) = t2
         indupdate = indupdate + 1
          coarsevalues(indupdate,iif) = tabtemp(i,j)
              
          ENDDO
         ENDDO 
       ENDIF
      
      return
      end           
  
      Subroutine Updatevbar(tabres,i1,i2,j1,j2,k1,k2,before,nb,ndir)
      implicit none
#  include "param.h"
#  include "grid.h"
#  include "ocean2d.h"
#  include "coupling.h"
#  include "scalars.h"
#  include "zoom.h"
      integer i1,i2,j1,j2,k1,k2
      real tabres(i1:i2,j1:j2,k1:k2)
      logical before
      integer nb, ndir
      real hzu
      real t1,t2,t3
      
      integer i,j
       real t4,t8
      integer :: iter, iifparent
      integer :: oldindupdate
      real :: rrhox
      real :: cff, cff1, cff2
      logical :: western_side, eastern_side,northern_side,southern_side
!$AGRIF_DO_NOT_TREAT
      logical global_update_2d
      common/globalupdate/global_update_2d
      integer :: iiffine, irhotfine, nbgrid
      common/updateubar2val/iiffine,irhotfine,
     &   nbgrid
!$AGRIF_END_DO_NOT_TREAT  
       integer irhot, ibegin,isize  
       real tabtemp(i1:i2,j1:j2) 
       integer :: n1,n2,n3 
              
              
      IF (before) THEN
         rrhox = real(Agrif_Irhox())

         IF (global_update_2d) THEN
           tabres(i1:i2,j1:j2,1) = rrhox * DV_avg1(i1:i2,j1:j2,nnew)
           RETURN
         ENDIF
                  
         irhot=Agrif_Irhot()
         
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1) 
         northern_side = (nb == 2).AND.(ndir == 2)

         ibegin = min(irhot,iif+1)
         IF (iif .LE. irhot) THEN
           ibegin = iif + 1
         ENDIF
     
          do iter=1,ibegin
          do j=j1,j2  
          do i=i1,i2                 
          tabres(i,j,iter) = dv_avg3(i,j,iif-iter+1)
          enddo
          enddo          
          enddo

         tabres = rrhox * tabres

       ELSE

       IF (global_update_2d) THEN
         DV_avg1(i1:i2,j1:j2,nnew) = tabres(i1:i2,j1:j2,1)
#ifdef MASKING
     &             *vmask(i1:i2,j1:j2)
#endif
         RETURN
       ENDIF

       ibegin = min(irhotfine,iiffine+1)

       IF (iiffine .LE. irhotfine) THEN
         ibegin = iiffine + 1
       ENDIF

       isize = (j2-j1+1)*(i2-i1+1)
       
       IF ((nbstep3d == 0).AND.(iif == 1)) THEN
       IF (.NOT.allocated(finevalues)) THEN
       Allocate(finevalues(isize,0:nfast))
       Allocate(coarsevalues(isize,0:nfast))
       ELSE       
       CALL checksize(indupdate+isize)
       ENDIF
       ENDIF
       
          do iter=1,ibegin
          oldindupdate = indupdate
          do j=j1,j2  
          do i=i1,i2
          oldindupdate = oldindupdate + 1
          finevalues(oldindupdate,iiffine-iter+1)
     &         = tabres(i,j,iter)
          enddo
          enddo          
          enddo
          
          IF (iif == 1) THEN
          oldindupdate = indupdate
          do j=j1,j2
          do i=i1,i2
          oldindupdate = oldindupdate + 1
          coarsevalues(oldindupdate,0) = DV_avg1(i,j,nstp)
          enddo
          enddo        
          ENDIF   
          
          
         tabtemp = 0.
         do iter=0,iif-1
         cff = -weight2(iif,iter)
         call copy1d(tabtemp,coarsevalues(indupdate+1,iter),
     &               cff,isize)
         enddo    
         
         do iter=0,iiffine
         cff=weight2(iiffine,iter)
         call copy1d(tabtemp,finevalues(indupdate+1,iter),
     &               cff,isize)
         enddo
         
         tabtemp = tabtemp/weight2(iif,iif)
                               
         DO j=j1,j2
           DO i=i1,i2
         t1 = 0.5*(zeta(i,j,knew)+h(i,j)+
     &  zeta(i,j-1,knew)+h(i,j-1))*om_v(i,j)
           t2 = tabtemp(i,j)
#ifdef MASKING
     &           * vmask(i,j)
#endif
          vbar(i,j,knew) = t2/t1
         indupdate = indupdate + 1
          coarsevalues(indupdate,iif) = tabtemp(i,j)
              
          ENDDO
         ENDDO 
       ENDIF
      
      return
      end 
            
      Subroutine Updateduavg2(tabres,i1,i2,j1,j2,before,nb,ndir)
      implicit none
#  include "param.h"
#  include "grid.h"
#  include "ocean2d.h"
#  include "coupling.h"
#  include "scalars.h"
      integer i1,i2,j1,j2
      real tabres(i1:i2,j1:j2)
      logical before
      real :: rrhoy
      integer :: i,j
      integer :: nb,ndir
      real :: t1
      logical :: western_side, eastern_side

      IF (before) THEN
      
         rrhoy = real(Agrif_Irhoy())

         tabres(i1:i2,j1:j2) = rrhoy * DU_avg1(i1:i2,j1:j2,4)
         
       ELSE
       
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)

         if (western_side) then
         i = i1
         do j=j1,j2
         Zt_avg1(i-1,j) = Zt_avg1(i-1,j)-pm(i-1,j)*pn(i-1,j)*
     &  (tabres(i,j) - DU_avg1(i,j,5))
#ifdef MASKING
     &                * umask(i,j)
#endif  
         enddo
         endif
         
         if (eastern_side) then
         i = i2
         do j=j1,j2  
         Zt_avg1(i,j) = Zt_avg1(i,j)+pm(i,j)*pn(i,j)*
     &  (tabres(i,j) - DU_avg1(i,j,5))
#ifdef MASKING
     &                * umask(i,j)
#endif  
         enddo
         endif
      
          DU_avg2(i1:i2,j1:j2) = (tabres(i1:i2,j1:j2)/dt)
#ifdef MASKING
     &                * umask(i1:i2,j1:j2)
#endif

       ENDIF
      
      return
      end      

      Subroutine Updatedvavg2(tabres,i1,i2,j1,j2,before,nb,ndir)
      implicit none
#  include "param.h"
#  include "grid.h"
#  include "ocean2d.h"
#  include "coupling.h"
#  include "scalars.h"
#  include "coupling.h"
      integer i1,i2,j1,j2
      real tabres(i1:i2,j1:j2)
      logical before
      real :: rrhox
      integer :: i,j
      integer :: nb,ndir
      real :: t1
      logical :: northern_side,southern_side
                    
      IF (before) THEN
      
         rrhox = real(Agrif_Irhox())

         tabres(i1:i2,j1:j2) = rrhox * DV_avg1(i1:i2,j1:j2,4)
         
       ELSE
       
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)
         if (southern_side) then
         j = j1
         do i=i1,i2
         Zt_avg1(i,j-1) = Zt_avg1(i,j-1)-pm(i,j-1)*pn(i,j-1)*
     &  (tabres(i,j) - DV_avg1(i,j,5))
#ifdef MASKING
     &                * vmask(i,j)
#endif  
         enddo
         endif
         
         if (northern_side) then
         j = j1
         do i=i1,i2  
         Zt_avg1(i,j) = Zt_avg1(i,j)+pm(i,j)*pn(i,j)*
     &  (tabres(i,j) - DV_avg1(i,j,5))
#ifdef MASKING
     &                * vmask(i,j)
#endif  
         enddo
         endif
         
          DV_avg2(i1:i2,j1:j2) = (tabres(i1:i2,j1:j2)/dt)
#ifdef MASKING
     &                * vmask(i1:i2,j1:j2)
#endif

       ENDIF
             
      return
      end
            

      subroutine checksize(isize)
      integer :: isize
      real,dimension(:,:),allocatable :: tempvalues
      integer :: n1,n3
#  include "param.h"
#  include "grid.h"
#  include "ocean2d.h"
#  include "coupling.h"
#  include "scalars.h"
#  include "zoom.h"     
!$AGRIF_DO_NOT_TREAT
      logical global_update_2d
      common/globalupdate/global_update_2d
      integer :: iiffine, irhotfine, nbgrid
      common/updateubar2val/iiffine,irhotfine,
     &   nbgrid
!$AGRIF_END_DO_NOT_TREAT

       IF (size(finevalues,1).LT.(isize)) THEN
       n1 = size(finevalues,1)
       allocate(tempvalues(n1,0:nfast))
       tempvalues=finevalues(1:n1,0:nfast)
       deallocate(finevalues)
       allocate(finevalues(isize,0:nfast))
       finevalues(1:n1,0:nfast) = tempvalues
       
       tempvalues=coarsevalues(1:n1,0:nfast)
       deallocate(coarsevalues)
       allocate(coarsevalues(isize,0:nfast))
       coarsevalues(1:n1,0:nfast) = tempvalues
              
       deallocate(tempvalues)      
       ENDIF      
      return
      end
#else
        subroutine update2D_empty
        return
        end
#endif
