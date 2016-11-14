! $Id: update3D.F 1458 2014-02-03 15:01:25Z gcambon $
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
#if defined AGRIF && defined SOLVE3D && defined AGRIF_2WAY
      
      subroutine ResetAlready()
#include "zoom.h"
      Alreadyupdated = .FALSE.
      end subroutine ResetAlready    

# define UPDATE_UV
      subroutine Agrif_update_np1
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
#  include "coupling.h"
#  include "zoom.h"
      integer NNO,NNOX,NNOY,ich,jch,k,itrc,ipr,jpr,nnewpr
      integer irmin,irmax,jrmin,jrmax,iumin,iumax,jvmin,jvmax
      integer ii,jj,iint,jint
      real eps,cff,fxc,surfc 
      parameter (eps=1.E-20)
      real tabtemp(GLOBAL_2D_ARRAY,N,NT)
      External UpdateTranp1, Updateunp1, Updatevnp1
      real,dimension(:,:,:,:), pointer :: myfxparent,myfyparent,uparent
      real,dimension(:,:,:,:), allocatable :: myfxoldparent,myfyoldparent
      real,dimension(:,:,:,:), pointer :: vparent
      real,dimension(:,:),pointer :: pmparent,pnparent,rmaskparent
      real,dimension(:,:,:),pointer :: Hzparent
      real,dimension(:,:,:,:,:),pointer ::tparent
      integer ipu,jpu,ipv,jpv
      real HTEMP(GLOBAL_2D_ARRAY,N)
      integer j,i
      real t1,t2,t3,t4,t5, t6, t7
      real dtparent
      integer nnewparent,parentnnew
      real tind(5)
      integer j1
      integer irhot
      integer idecal
      external :: updatemyfx, updatemyfy
!

!      return

#ifdef MASKING
      Agrif_UseSpecialValueInUpdate = .TRUE.
#endif
      Agrif_SpecialValueFineGrid = 0.
      
      irhot=Agrif_Irhot()

      !IF (mod(nbstep3d,5) == (irhot-1)) THEN
      IF (.TRUE.) THEN

#ifdef AGRIF_UPDATE_DECAL
      Call Agrif_Update_Variable(updatetid,
     &         procname = updateTranp1,locupdate=(/1,0/))
      Agrif_UseSpecialValueInUpdate = .FALSE.
#else
      Call Agrif_Update_Variable(updatetid,
     &         procname = updateTranp1)
      Agrif_UseSpecialValueInUpdate = .FALSE.
#endif
      ELSE

#ifdef AGRIF_UPDATE_DECAL
      Call Agrif_Update_Variable(updatetid,
     &         locupdate=(/1,3/),procname = updateTranp1)
     
      Agrif_UseSpecialValueInUpdate = .FALSE.
           
!      Call Agrif_Update_Variable(tabtemp(:,:,:,1),updateuid,
!     &     locupdate1=(/0,2/),locupdate2=(/1,3/),
!     &     procname = updateunp1)
!      Call Agrif_Update_Variable(tabtemp(:,:,:,1),updatevid,
!     &     locupdate1=(/1,3/),locupdate2=(/0,2/),
!     &     procname = updatevnp1) 
#else
      Call Agrif_Update_Variable(updatetid,
     &         locupdate=(/0,1/),procname = updateTranp1)
     
      Agrif_UseSpecialValueInUpdate = .FALSE.
           
!      Call Agrif_Update_Variable(tabtemp(:,:,:,1),updateuid,
!     &     locupdate1=(/0,2/),locupdate2=(/0,2/),
!     &     procname = updateunp1)
!      Call Agrif_Update_Variable(tabtemp(:,:,:,1),updatevid,
!     &     locupdate1=(/0,2/),locupdate2=(/0,2/),
!     &     procname = updatevnp1) 
#endif    
      ENDIF

#if !defined AGRIF_CONSERV_TRA
       RETURN
#else

#if defined AGRIF_UPDATE_DECAL && !defined AGRIF_OLD_CONSERV

      Agrif_UseSpecialValueInUpdate = .FALSE.

      
      Call Agrif_Update_Variable(updatemyfxid,
     &     locupdate1=(/0,0/),locupdate2=(/1,1/),
     &     procname = updatemyfx)
    
      Call Agrif_Update_Variable(updatemyfyid,
     &     locupdate1=(/1,1/),locupdate2=(/0,0/),
     &     procname = updatemyfy) 
#endif
               
#if defined AGRIF_UPDATE_DECAL && !defined AGRIF_OLD_DECAL
      idecal = Agrif_irhox()
#else
      idecal = 0
#endif

#ifdef MPI
       if (.not.WEST_INTER) then
#endif
       myfx(1+idecal,0:Mmmpi+1,:,:) = 0.
#ifdef MPI
       endif
#endif

#ifdef MPI
       if (.not.EAST_INTER) then
#endif
       myfx(Lmmpi+1-idecal,0:Mmmpi+1,:,:) = 0.
#ifdef MPI
       endif
#endif

#ifdef MPI
       if (.not.SOUTH_INTER) then
#endif
       myfy(0:Lmmpi+1,1+idecal,:,:) = 0.
#ifdef MPI
       endif
#endif

#ifdef MPI
       if (.not.NORTH_INTER) then
#endif
       myfy(0:Lmmpi+1,Mmmpi+1-idecal,:,:) = 0.
#ifdef MPI
       endif
#endif

#if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      Call Agrif_ChildGrid_To_ParentGrid()
      Call ExchangeParentValuesTracers()
      Call Agrif_ParentGrid_To_ChildGrid()
#endif
      
#endif

      return
      end
      
      subroutine ExchangeParentValuesTracers()
#  include "param.h"
#  include "scalars.h"
#  include "ocean3d.h"
      integer itrc      
#if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      do itrc = 1,NT
        call exchange_r3d_tile (1,Lm,1,Mm,
     &                          t(START_2D_ARRAY,1,nnew,itrc))
      enddo
#endif

      end subroutine ExchangeParentValuesTracers      
      
      
      subroutine Agrif_update_uv_np1
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
#  include "coupling.h"
#  include "zoom.h"
      integer NNO,NNOX,NNOY,ich,jch,k,itrc,ipr,jpr,nnewpr
      integer irmin,irmax,jrmin,jrmax,iumin,iumax,jvmin,jvmax
      integer ii,jj,iint,jint
      real eps,cff,fxc,surfc 
      parameter (eps=1.E-20)
      real tabtemp(GLOBAL_2D_ARRAY,N,NT)
      External UpdateTranp1, Updateunp1, Updatevnp1
      real,dimension(:,:,:,:), pointer :: myfxparent,myfyparent,uparent
      real,dimension(:,:,:,:), allocatable :: myfxoldparent,myfyoldparent
      real,dimension(:,:,:,:), pointer :: vparent
      real,dimension(:,:),pointer :: pmparent,pnparent,rmaskparent
      real,dimension(:,:,:),pointer :: Hzparent
      real,dimension(:,:,:,:,:),pointer ::tparent
      integer ipu,jpu,ipv,jpv
      real HTEMP(GLOBAL_2D_ARRAY,N)
      integer j,i
      real t1,t2,t3,t4,t5, t6, t7
      real dtparent
      integer nnewparent,parentnnew
      real tind(5)
      integer j1
      integer irhot
      integer idecal
      external :: updatemyfx, updatemyfy
      external :: updateunp1pre
      external :: updatevnp1pre
!$AGRIF_DO_NOT_TREAT
      integer :: nbcoarsechild
      common/updateprestepint/nbcoarsechild      
!$AGRIF_END_DO_NOT_TREAT
!

      nbcoarsechild = nbcoarse

      Call Agrif_ChildGrid_To_ParentGrid()
      Call ResetAlready()
      Call Agrif_ParentGrid_To_ChildGrid()
     
#ifdef MASKING
      Agrif_UseSpecialValueInUpdate = .FALSE.
#endif
      Agrif_SpecialValueFineGrid = 0.
     
      Agrif_UseSpecialValueInUpdate = .FALSE.

#if defined AGRIF_CONSERV_TRA
#if defined AGRIF_UPDATE_DECAL && !defined AGRIF_OLD_CONSERV
!      Call Agrif_Update_Variable(tabtemp(:,:,:,1),updatehuonid,
!     &     locupdate1=(/0,4/),locupdate2=(/1,5/),
!     &     procname = updateunp1pre)
!      Call Agrif_Update_Variable(tabtemp(:,:,:,1),updatehvomid,
!     &     locupdate1=(/1,5/),locupdate2=(/0,5/),
!     &     procname = updatevnp1pre)
     
      Call Agrif_Update_Variable(updatehuonid,
     &     locupdate1=(/0,0/),locupdate2=(/1,1/),
     &     procname = updateunp1pre)
      Call Agrif_Update_Variable(updatehvomid,
     &     locupdate1=(/1,1/),locupdate2=(/0,0/),
     &     procname = updatevnp1pre)
     
#endif
#endif
     
      IF (nbcoarse .NE. Agrif_Irhot()) return

#ifdef MASKING
      Agrif_UseSpecialValueInUpdate = .TRUE.
#endif
      Agrif_SpecialValueFineGrid = 0.
      
      irhot=Agrif_Irhot()
      
      !IF (mod(nbstep3d,1) == (irhot-1)) THEN
      IF (.TRUE.) THEN

#ifdef AGRIF_UPDATE_DECAL
      Agrif_UseSpecialValueInUpdate = .FALSE.
           
      Call Agrif_Update_Variable(updateuid,
     &     locupdate1=(/0,-1/),locupdate2=(/1,-2/),
     &     procname = updateunp1)
     
      Call Agrif_Update_Variable(updatevid,
     &     locupdate1=(/1,-2/),locupdate2=(/0,-1/),
     &     procname = updatevnp1)
     
#else
      Agrif_UseSpecialValueInUpdate = .FALSE.
           
      Call Agrif_Update_Variable(updateuid,
     &     procname = updateunp1)
      Call Agrif_Update_Variable(updatevid,
     &     procname = updatevnp1)
#endif
      ELSE

#ifdef AGRIF_UPDATE_DECAL
     
      Agrif_UseSpecialValueInUpdate = .FALSE.
           
      Call Agrif_Update_Variable(updateuid,
     &     locupdate1=(/0,2/),locupdate2=(/1,3/),
     &     procname = updateunp1)
      Call Agrif_Update_Variable(updatevid,
     &     locupdate1=(/1,3/),locupdate2=(/0,2/),
     &     procname = updatevnp1) 
#else
     
      Agrif_UseSpecialValueInUpdate = .FALSE.
           
      Call Agrif_Update_Variable(updateuid,
     &     locupdate1=(/0,2/),locupdate2=(/0,2/),
     &     procname = updateunp1)
      Call Agrif_Update_Variable(updatevid,
     &     locupdate1=(/0,2/),locupdate2=(/0,2/),
     &     procname = updatevnp1) 
#endif    
      ENDIF

      return
      end
      
      
      Subroutine UpdateTranp1pre(tabres,i1,i2,j1,j2,k1,k2,l1,l2,before)
      implicit none
#  include "param.h"
#  include "ocean3d.h"
#  include "scalars.h"
#  include "zoom.h"
#  include "grid.h"
!$AGRIF_DO_NOT_TREAT
      integer :: nbcoarsechild
      common/updateprestepint/nbcoarsechild
!$AGRIF_END_DO_NOT_TREAT
      integer i1,i2,j1,j2,k1,k2,l1,l2
      real tabres(i1:i2,j1:j2,k1:k2,l1:l2)
      logical before
      
      integer i,j,k,l
      real :: invrrhot
      
      IF (before) THEN
      
        invrrhot = 1./real(Agrif_Irhot())
        DO l=l1,l2
        DO k=k1,k2
        DO j=j1,j2
        DO i=i1,i2
          tabres(i,j,k,l) = invrrhot * t(i,j,k,nrhs,l)
#ifdef MASKING
     &                       *rmask(i,j)
#endif          
        ENDDO
        ENDDO
        ENDDO
        ENDDO
       
       ELSE
       
         IF (nbcoarsechild == 1) THEN
           DO l=l1,l2
           DO k=k1,k2
           DO j=j1,j2
           DO i=i1,i2
             t(i,j,k,nrhs,l) = tabres(i,j,k,l)
#ifdef MASKING             
     &                         * rmask(i,j)
#endif
           ENDDO
           ENDDO
           ENDDO 
           ENDDO
        
         ELSE
       

           DO j=j1,j2
           DO i=i1,i2
             IF (.Not.Alreadyupdated(i,j,1)) THEN
               DO l=l1,l2
               DO k=k1,k2             
                 t(i,j,k,nrhs,l) = (t(i,j,k,nrhs,l)+tabres(i,j,k,l))
#ifdef MASKING             
     &                              * rmask(i,j)
#endif    
               ENDDO
               ENDDO 
               Alreadyupdated(i,j,1) = .TRUE.
             ENDIF
           ENDDO 
           ENDDO 
        
          ENDIF        
        ENDIF
	
      return
      end 
    
      
      Subroutine Updateunp1pre(tabres,i1,i2,j1,j2,k1,k2,before,nb,ndir)
      implicit none
#  include "param.h"
#  include "ocean3d.h"
#  include "grid.h"
#  include "scalars.h"
#  include "zoom.h"
#  include "coupling.h"
      integer i1,i2,j1,j2,k1,k2,nb,ndir
      real tabres(i1:i2,j1:j2,k1:k2)
      logical before

!$AGRIF_DO_NOT_TREAT
      integer :: nbcoarsechild
      common/updateprestepint/nbcoarsechild
!$AGRIF_END_DO_NOT_TREAT      
      
      integer i,j,k
      real :: invrrhot
      real t1,t2,t3

      IF (before) THEN
      
        invrrhot = 1./real(Agrif_Irhot())
        
        DO k=k1,k2
        DO j=j1,j2
        DO i=i1,i2
          tabres(i,j,k) = invrrhot * Huon(i,j,k)
#ifdef MASKING
     &                       *umask(i,j)
#endif          
        ENDDO
        ENDDO
        ENDDO
       
       ELSE

         IF (nbcoarsechild == 1) THEN
           DO k=k1,k2
           DO j=j1,j2
           DO i=i1,i2
             Huonagrif(i,j,k) = 3.*tabres(i,j,k)
#ifdef MASKING             
     &                         * umask(i,j)
#endif
           ENDDO
           ENDDO
           ENDDO 
        
         ELSE
         
           DO j=j1,j2
           DO i=i1,i2
             IF (.Not.Alreadyupdated(i,j,2)) THEN
               DO k=k1,k2             
         Huonagrif(i,j,k) = (Huonagrif(i,j,k)+3.*tabres(i,j,k))
#ifdef MASKING             
     &                              * umask(i,j)
#endif
          Huon(i,j,k) = Huonagrif(i,j,k)
               ENDDO
               Alreadyupdated(i,j,2) = .TRUE.
             ENDIF
           ENDDO 
           ENDDO

          ENDIF        
        ENDIF
              
      return
      end

      
      
      Subroutine Updatevnp1pre(tabres,i1,i2,j1,j2,k1,k2,before,nb,ndir)
      implicit none
#  include "param.h"
#  include "ocean3d.h"
#  include "grid.h"
#  include "scalars.h"
#  include "zoom.h"
      integer i1,i2,j1,j2,k1,k2,nb,ndir
      real tabres(i1:i2,j1:j2,k1:k2)
      logical before

!$AGRIF_DO_NOT_TREAT
      integer :: nbcoarsechild
      common/updateprestepint/nbcoarsechild
!$AGRIF_END_DO_NOT_TREAT      
      
      integer i,j,k
      
      real :: invrrhot

      IF (before) THEN
      
        invrrhot = 1./real(Agrif_Irhot())
        DO k=k1,k2
        DO j=j1,j2
        DO i=i1,i2
          tabres(i,j,k) = invrrhot * Hvom(i,j,k)
#ifdef MASKING
     &                       *vmask(i,j)
#endif          
        ENDDO
        ENDDO
        ENDDO
       
       ELSE
         IF (nbcoarsechild == 1) THEN
           DO k=k1,k2
           DO j=j1,j2
           DO i=i1,i2
             Hvomagrif(i,j,k) = 3.*tabres(i,j,k)
#ifdef MASKING             
     &                         * vmask(i,j)
#endif
           ENDDO
           ENDDO
           ENDDO 
        
         ELSE
       

           DO j=j1,j2
           DO i=i1,i2
             IF (.Not.Alreadyupdated(i,j,3)) THEN
               DO k=k1,k2             
           Hvomagrif(i,j,k)= (Hvomagrif(i,j,k)+3.*tabres(i,j,k))
#ifdef MASKING
     &                              * vmask(i,j)
#endif    
              Hvom(i,j,k) = Hvomagrif(i,j,k)
               ENDDO
               Alreadyupdated(i,j,3) = .TRUE.
             ENDIF
           ENDDO 
           ENDDO
           
          ENDIF        
        ENDIF
      
      return
      end      

                  
      Subroutine UpdateTranp1(tabres,i1,i2,j1,j2,k1,k2,l1,l2,before)
      implicit none
#  include "param.h"
#  include "ocean3d.h"
#  include "scalars.h"
#  include "grid.h"
      integer i1,i2,j1,j2,k1,k2,l1,l2
      real tabres(i1:i2,j1:j2,k1:k2,l1:l2)
      logical before
      
      integer i,j,k,l

!      print *,'je rentre ',before
            
      IF (before) THEN
      DO l=l1,l2
        DO k=k1,k2
         DO j=j1,j2
           DO i=i1,i2
          !  tabres(i,j,k,l) = Hz(i,j,k)*t(i,j,k,nnew,l)
	    tabres(i,j,k,l) = t(i,j,k,nnew,l)
            ENDDO
         ENDDO
        ENDDO
       ENDDO
       ELSE
      DO l=l1,l2
        DO k=k1,k2
         DO j=j1,j2
          DO i=i1,i2
!           IF (Hz(i,j,k).NE.0.) THEN
!              t(i,j,k,nnew,l) = tabres(i,j,k,l)/Hz(i,j,k)
!           ENDIF
	      t(i,j,k,nnew,l) = tabres(i,j,k,l)
#ifdef MASKING
     &                         *rmask(i,j)
#endif          
          ENDDO
         ENDDO
       ENDDO 
        ENDDO

       ENDIF
      return
      end
 
      Subroutine Updatemyfx(tabres,i1,i2,j1,j2,k1,k2,l1,l2,before,
     &   nb,ndir)
      implicit none
#  include "param.h"
#  include "ocean3d.h"
#  include "scalars.h"
#  include "grid.h"
#  include "zoom.h"
      integer i1,i2,j1,j2,k1,k2,l1,l2
      real tabres(i1:i2,j1:j2,k1:k2,l1:l2)
      logical before
      
      integer i,j,k,l
      integer nb,ndir
      real :: t1, rrhoy
      logical :: western_side, eastern_side
            
      IF (before) THEN
      
         rrhoy = real(Agrif_Irhoy())
         
      DO l=l1,l2
        DO k=k1,k2
         DO j=j1,j2
           DO i=i1,i2
	    tabres(i,j,k,l) = rrhoy*myfx(i,j,k,l)
            ENDDO
         ENDDO
        ENDDO
       ENDDO
       
       ELSE
       
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
      
      if (western_side) then
      i=i1
      DO l=l1,l2
        DO k=k1,k2
         DO j=j1,j2
         t(i-1,j,k,nnew,l)=t(i-1,j,k,nnew,l)-
     &  pm(i-1,j)*pn(i-1,j)*(tabres(i,j,k,l)-myfx(i,j,k,l))
     &  /Hz(i-1,j,k)
         ENDDO
       ENDDO 
       ENDDO
       endif
       
      if (eastern_side) then
      i=i2
      DO l=l1,l2
        DO k=k1,k2
         DO j=j1,j2
         t(i,j,k,nnew,l)=t(i,j,k,nnew,l)+
     &  pm(i,j)*pn(i,j)*(tabres(i,j,k,l)-myfx(i,j,k,l))
     &  /Hz(i,j,k)
         ENDDO
       ENDDO 
       ENDDO
       endif

       ENDIF
      return
      end
      
      
      Subroutine Updatemyfy(tabres,i1,i2,j1,j2,k1,k2,l1,l2,before,
     &   nb,ndir)
      implicit none
#  include "param.h"
#  include "ocean3d.h"
#  include "scalars.h"
#  include "grid.h"
#  include "zoom.h"
      integer i1,i2,j1,j2,k1,k2,l1,l2
      real tabres(i1:i2,j1:j2,k1:k2,l1:l2)
      logical before
      
      integer i,j,k,l
      integer nb,ndir
      real :: t1, rrhox
      logical :: northern_side,southern_side
            
      IF (before) THEN
      
         rrhox = real(Agrif_Irhox())
         
      DO l=l1,l2
        DO k=k1,k2
         DO j=j1,j2
           DO i=i1,i2
	    tabres(i,j,k,l) = rrhox * myfy(i,j,k,l)
            ENDDO
         ENDDO
        ENDDO
       ENDDO
       
       ELSE
       
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)

      if (southern_side) then
      
      j=j1
      DO l=l1,l2
        DO k=k1,k2
         DO i=i1,i2
         t(i,j-1,k,nnew,l)=t(i,j-1,k,nnew,l)-
     &  pm(i,j-1)*pn(i,j-1)*(tabres(i,j,k,l)-myfy(i,j,k,l))
     &  /Hz(i,j-1,k)
         ENDDO
       ENDDO 
       ENDDO
       

       endif
       
      if (northern_side) then
      j=j2
      DO l=l1,l2
        DO k=k1,k2
         DO i=i1,i2
         t(i,j,k,nnew,l)=t(i,j,k,nnew,l)+
     &  pm(i,j)*pn(i,j)*(tabres(i,j,k,l)-myfy(i,j,k,l))
     &  /Hz(i,j,k)
         ENDDO
       ENDDO 
       ENDDO
       endif

       ENDIF
      return
      end      
      
      Subroutine Updateunp1(tabres,i1,i2,j1,j2,k1,k2,before)
      implicit none
#  include "param.h"
#  include "ocean3d.h"
#  include "grid.h"
#  include "scalars.h"
      integer i1,i2,j1,j2,k1,k2
      real tabres(i1:i2,j1:j2,k1:k2)
      logical before
      real :: hzu
      
      integer i,j,k
      
      IF (before) THEN
        DO k=k1,k2
         DO j=j1,j2
           DO i=i1,i2
            tabres(i,j,k) = u(i,j,k,nnew)
            ENDDO
         ENDDO
        ENDDO
       ELSE
        DO k=k1,k2
         DO j=j1,j2
           DO i=i1,i2
          u(i,j,k,nnew) = tabres(i,j,k)
#ifdef MASKING
     &           * umask(i,j)
#endif     
          ENDDO
         ENDDO
       ENDDO        
       ENDIF
      
      return
      end
               
      
      Subroutine Updatevnp1(tabres,i1,i2,j1,j2,k1,k2,before)
      implicit none
#  include "param.h"
#  include "ocean3d.h"
#  include "grid.h"
#  include "scalars.h"
      integer i1,i2,j1,j2,k1,k2
      real tabres(i1:i2,j1:j2,k1:k2)
      logical before
      real hzv
      
      integer i,j,k
      
      IF (before) THEN
        DO k=k1,k2
         DO j=j1,j2
           DO i=i1,i2
             tabres(i,j,k) = v(i,j,k,nnew)
            ENDDO
         ENDDO
        ENDDO
       ELSE
        DO k=k1,k2
         DO j=j1,j2
          DO i=i1,i2
          v(i,j,k,nnew) = tabres(i,j,k)
#ifdef MASKING
     &              * vmask(i,j)
#endif     
          ENDDO
         ENDDO
       ENDDO        
       ENDIF
      
      return
      end
#else
        subroutine update3D_empty
        return
        end
#endif
