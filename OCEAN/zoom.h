! $Id: zoom.h 1615 2014-12-17 13:27:07Z rblod $
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
#  ifdef MPI
#   define LOCALLM Lmmpi
#   define LOCALMM Mmmpi
#  else
#   define LOCALLM Lm
#   define LOCALMM Mm
#  endif      

#ifdef AGRIF
# ifdef AGRIF_OBC_WEST
#  ifdef SOLVE3D
      real T_west(0:1,-1:Mm+2+padd_E,N,4,NT)
      common/zoom3D_TW/T_west
      real U_west(1:1,-1:Mm+2+padd_E,N,4)
      common/zoom3D_UW/U_west 
      real V_west(0:0,-1:Mm+2+padd_E,N,4)
      common/zoom3D_VW/V_west
#  endif
# endif
# ifdef AGRIF_OBC_EAST
#  ifdef SOLVE3D
      real T_east(LOCALLM:LOCALLM+1,-1:Mm+2+padd_E,N,4,NT)
      common/zoom3D_TE/T_east
      real U_east(LOCALLM+1:LOCALLM+1,-1:Mm+2+padd_E,N,4)
      common/zoom3D_UE/U_east 
      real V_east(LOCALLM+1:LOCALLM+1,-1:Mm+2+padd_E,N,4)
      common/zoom3D_VE/V_east
#  endif
# endif
# ifdef AGRIF_OBC_SOUTH   
#  ifdef SOLVE3D
      real T_south(-1:Lm+2+padd_X,0:1,N,4,NT)
      common/zoom3D_TS/T_south
      real U_south(-1:Lm+2+padd_X,0:0,N,4)
      common/zoom3D_US/U_south 
      real V_south(-1:Lm+2+padd_X,1:1,N,4)
      common/zoom3D_VS/V_south
#  endif
# endif
# ifdef AGRIF_OBC_NORTH  
#  ifdef SOLVE3D
      real T_north(-1:Lm+2+padd_X,LOCALMM:LOCALMM+1,N,4,NT)     
      common/zoom3D_TN/T_north
      real U_north(-1:Lm+2+padd_X,LOCALMM+1:LOCALMM+1,N,4)
      common/zoom3D_UN/U_north 
      real V_north(-1:Lm+2+padd_X,LOCALMM+1:LOCALMM+1,N,4)
      common/zoom3D_VN/V_north
#  endif
# endif
      integer Zetatimeindex, Zetatimeindex2
      common/zoom2D_ZetaT/Zetatimeindex, Zetatimeindex2
      integer U2DTimeindex, U2DTimeindex2
      common/zoom2D_UT/U2DTimeindex, U2DTimeindex2
      integer V2DTimeindex, V2DTimeindex2
      common/zoom2D_VT/V2DTimeindex, V2DTimeindex2
# ifdef SOLVE3D
      integer Ttimeindex
      common/zoom3D_TT/Ttimeindex
      integer Utimeindex
      common/zoom3D_UT/Utimeindex
      integer Vtimeindex
      common/zoom3D_VT/Vtimeindex
# endif

      real weight2(0:NWEIGHT,0:NWEIGHT)
      common/weighting/weight2

      real updateTprof(GLOBAL_2D_ARRAY,N,NT)
      common/updateTprofile/updateTprof
      integer indupdate
      real myvalues(3*7*(2*(Lm+3)+2*(Mm+3)),0:NWEIGHT)
      common/updatevalues/myvalues,indupdate

      integer nbcoarse
      common/nestingmanag/nbcoarse
      
      real myfx(GLOBAL_2D_ARRAY,N,NT)
      real myfy(GLOBAL_2D_ARRAY,N,NT)
      common/myfluxes/myfx,myfy
       
      real Zt_avg2(GLOBAL_2D_ARRAY,4)
      common/averagebaro/Zt_avg2
      
      real dZtinterp(GLOBAL_2D_ARRAY)
      real dUinterp(GLOBAL_2D_ARRAY)
      real dVinterp(GLOBAL_2D_ARRAY)            
# ifdef WKB_WWAVE
      real dWactinterp(GLOBAL_2D_ARRAY)
      real dWartinterp(GLOBAL_2D_ARRAY)
      real dWkxtinterp(GLOBAL_2D_ARRAY)
      real dWketinterp(GLOBAL_2D_ARRAY)            
# endif
      common/zoombc2D/dZtinterp,dUinterp,dVinterp
# ifdef WKB_WWAVE
     &  ,dWactinterp,dWkxtinterp,dWketinterp,dWacinterp
# endif
       logical Alreadyupdated(GLOBAL_2D_ARRAY,3)
       common/updateprestep/Alreadyupdated

       real usponge(GLOBAL_2D_ARRAY,N)
       real vsponge(GLOBAL_2D_ARRAY,N)
       real tsponge(GLOBAL_2D_ARRAY,N,NT)
       common/sponge_com/usponge, vsponge, tsponge

       real Huonagrif(GLOBAL_2D_ARRAY,N)
       real Hvomagrif(GLOBAL_2D_ARRAY,N)
       common/huvagrif/Huonagrif,Hvomagrif

      real Zt_avg3(GLOBAL_2D_ARRAY,0:NWEIGHT)
      common/zoom2D_Zeta2/Zt_avg3
      real DU_avg3(GLOBAL_2D_ARRAY,0:NWEIGHT)
      common/zoom2D_U2/DU_avg3
      real DV_avg3(GLOBAL_2D_ARRAY,0:NWEIGHT)    
      common/zoom2D_V2/DV_avg3 
       
#  ifdef SOLVE3D
      real T_sponge_west(0:10,-1:Mm+2+padd_E,N,2,NT)
      common/zoom3D_sponge_TW/T_sponge_west
      real U_sponge_west(1:11,-1:Mm+2+padd_E,N,2)
      common/zoom3D_sponge_UW/U_sponge_west 
      real V_sponge_west(0:10,-1:Mm+2+padd_E,N,2)
      common/zoom3D_sponge_VW/V_sponge_west
      
      real T_sponge_east(LOCALLM-9:LOCALLM+1,-1:Mm+2+padd_E,N,2,NT)
      common/zoom3D_sponge_TE/T_sponge_east
      real U_sponge_east(LOCALLM-9:LOCALLM+1,-1:Mm+2+padd_E,N,2)
      common/zoom3D_sponge_UE/U_sponge_east 
      real V_sponge_east(LOCALLM-9:LOCALLM+1,-1:Mm+2+padd_E,N,2)
      common/zoom3D_sponge_VE/V_sponge_east
      
      real T_sponge_south(-1:Lm+2+padd_X,0:10,N,2,NT)
      common/zoom3D_sponge_TS/T_sponge_south
      real U_sponge_south(-1:Lm+2+padd_X,0:10,N,2)
      common/zoom3D_sponge_US/U_sponge_south 
      real V_sponge_south(-1:Lm+2+padd_X,1:11,N,2)
      common/zoom3D_sponge_VS/V_sponge_south
            
      real T_sponge_north(-1:Lm+2+padd_X,LOCALMM-9:LOCALMM+1,N,2,NT)     
      common/zoom3D_sponge_TN/T_sponge_north
      real U_sponge_north(-1:Lm+2+padd_X,LOCALMM-9:LOCALMM+1,N,2)
      common/zoom3D_sponge_UN/U_sponge_north 
      real V_sponge_north(-1:Lm+2+padd_X,LOCALMM-9:LOCALMM+1,N,2)
      common/zoom3D_sponge_VN/V_sponge_north
      
      integer TTimesponge, UVTimesponge
      common/zoom3D_sponge_times/TTimesponge, UVTimesponge            
#  endif
            
      real A1dXI(GLOBAL_1D_ARRAYXI,10*NWEIGHT)
      real A1dETA(GLOBAL_1D_ARRAYETA,10*NWEIGHT)
      common/work_agrif/A1dXI,A1dETA 

      integer TspongeTimeindex, TspongeTimeindex2
      integer UVspongeTimeindex, UVspongeTimeindex2
      common/zoom3D_sponge/TspongeTimeindex, UVspongeTimeindex,
     &      TspongeTimeindex2, UVspongeTimeindex2 

      real,dimension(:,:),allocatable :: finevalues
      real,dimension(:,:),allocatable :: coarsevalues
      common/gridinter/finevalues,coarsevalues
      
      real,dimension(:,:),allocatable :: coarsevaluesinterp
      common/gridinter2/coarsevaluesinterp

      integer j1t,i1t,i2t,j2t
      integer i1u,i2u,j1v,j2v
      common/arraysindices/i1t,j1t,i2t,j2t,
     &     i1u,i2u,j1v,j2v
            
      integer hid, zetaid,ubarid,vbarid,uid,vid,tid
      integer rmaskid
      integer tspongeid, uspongeid, vspongeid
# ifdef WKB_WWAVE
      integer wacid,warid,wkxid,wkeid
      integer hrmid,frqid,wsbid,wvnid,wcgid
# endif
      common/varids/hid,zetaid,ubarid,vbarid,uid,vid,tid,
     &  tspongeid, uspongeid, vspongeid, rmaskid
# ifdef WKB_WWAVE
     &  ,wacid,warid,wkxid,wkeid
     &  ,hrmid,frqid,wsbid,wvnid,wcgid
     
# endif
      integer updatezetaid, updateubarid, updatevbarid
      integer updateduavg2id, updatedvavg2id
      integer updatetid, updateuid, updatevid
      integer updatemyfxid, updatemyfyid
      integer updatehuonid, updatehvomid
      common/varidsupdate/updatezetaid, updateubarid, updatevbarid,
     &       updateduavg2id, updatedvavg2id,
     &       updatetid, updateuid, updatevid, updatemyfxid,
     &       updatemyfyid,updatehuonid, updatehvomid

!$AGRIF_DO_NOT_TREAT
      integer :: iind
      integer :: sortedint(0:10000)
      integer :: whichstep(0:10000)
      integer :: grids_at_level(0:20,0:100)
      integer :: parent_grid(0:20)
      integer :: coeff_ref_time(0:20)
      integer :: nbtimes, nbmaxtimes
      common/rootintegrate/nbtimes, nbmaxtimes,
     &    iind,sortedint,whichstep,
     &    grids_at_level,parent_grid,coeff_ref_time
!$AGRIF_END_DO_NOT_TREAT

#endif
