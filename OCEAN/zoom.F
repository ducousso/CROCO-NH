! $Id: zoom.F 1615 2014-12-17 13:27:07Z rblod $
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
#ifdef AGRIF
!
! AGRIF related routines:
!
! Agrif_initworkspace      : initialize array sizes.
! Agrif_initvalues         : perform ROMS initial procedures 
!                            (grid, initial conditions, etc...)
! declare_zoom_variables   :
! ResetMask                :
! Updateh                  :
! UpdateGridhis            :
! Agrif_transfer_floatsp2c : checks if a float must be transferred 
!                            from parent to child grid
! Agrif_laststep           : determines whether this time step is 
!                            the last one before a ROOT time step.
! Agrif_Invloc             : MPI related 
! Agrif_detect             :
! Agrif_Before_Regridding  :
! initzeta                 :
! initubar                 :
! initvbar                 :
! initu                    :
! initv                    :
! initt                    :
! computenbmaxtimes        :
! write_number             :
! write_prestep3d          :
! write_step2d             :
! write_step3d1            :
! write_step3d2            :
! agrif_read_fixed         :
!
!====================================================================
!                   subroutine Agrif_initworkspace
!====================================================================
!
      subroutine Agrif_initworkspace()
# ifdef XIOS
      USE xios          ! XIOS module
# endif
      implicit none
# ifdef MPI
      integer ierr  
# endif          
# include "param.h"
# include "private_scratch.h"
# include "ncscrum.h"
# include "scalars.h"
# ifdef AUTOTILING
#  include "autotiling.h"
# endif
# include "mpi_roms.h"

      integer size_XI,size_ETA,se,sse, sz,ssz
!$AGRIF_DO_NOT_TREAT
      logical, SAVE :: MPIisinitialize = .FALSE.
!$AGRIF_END_DO_NOT_TREAT

      IF (Agrif_Root()) THEN
# include "dynparam.h"
# ifdef MPI
        IF (.Not.MPIisinitialize) THEN
#  if defined XIOS
           Call xios_initialize( "roms",return_comm=MPI_COMM_WORLD)
#  elif defined OA_COUPLING || defined OW_COUPLING
           Call cpl_prism_init
#  else
           Call MPI_Init(ierr)
#  endif
           MPIisinitialize = .TRUE.
        ENDIF
# endif
# ifdef AGRIF_ADAPTIVE
      If (Agrif_Nb_step() == 0) Then
        Call Agrif_Set_coeffref_x(3)
        Call Agrif_Set_coeffref_y(3)
        Call Agrif_Set_coeffreft_x(3)
        Call Agrif_Set_coeffreft_y(3) 
        Call Agrif_Set_Minwidth(18)
        Call Agrif_Set_Regridding(300*(nfast+2))
      EndIf
# endif

      ENDIF
# ifdef AUTOTILING
      If (Agrif_Nb_Step() == 0) Then
        NSUB_X = 1
        NSUB_E = NPP
      EndIf
# endif
# include "dynderivparam.h"
# ifdef MPI
      Call MPI_Setup(ierr)
# endif     

      return
      end
!
!====================================================================
!                   subroutine Agrif_initvalues
!====================================================================
!
      subroutine Agrif_initvalues()
      use AGRIF_UTIL
#ifdef PISCES
      USE pisces_ini     ! PISCES modules
      USE trcini_pisces
#endif
      implicit none
      integer tile, ierr, trd, subs
# ifdef WKB_WWAVE
      integer winterp 
# endif
# include "param.h"
# include "private_scratch.h"
# include "ncscrum.h"
# include "scalars.h"
# include "grid.h"
# include "ocean2d.h"
# include "zoom.h"
# ifdef SOLVE3D
#  include "coupling.h"
#  include "ocean3d.h"
# endif
# ifdef WKB_WWAVE
#  include "wkb_wwave.h"
# endif
# ifdef AUTOTILING
#  include "autotiling.h"
# endif

      integer size_XI,size_ETA,se,sse, sz,ssz
      real res1,res2,res3,res4,res5,res6,cff1,cff2
      integer i,j
      integer ipr,jpr,itrc
      real,dimension(:,:),pointer :: hparent,umaskparent,rmaskparent
      real htest(GLOBAL_2D_ARRAY)
      real tind(5)
      integer ipu,jpu,ipv,jpv,k
      real gradh(GLOBAL_2D_ARRAY)
      real hbis(GLOBAL_2D_ARRAY)
      real hcur(GLOBAL_2D_ARRAY)
      real hmean(GLOBAL_2D_ARRAY)
      real hbis2(GLOBAL_2D_ARRAY)
      real hbis3(GLOBAL_2D_ARRAY)      
      real CF(GLOBAL_2D_ARRAY,0:N)
      real DC(GLOBAL_2D_ARRAY,0:N)
      integer nbloop
      integer nbconstraint
      integer n1,n2
      real valconstraints(100000,2)
      integer iprmin,iprmax,jprmin,jprmax
      real,dimension(:,:),allocatable :: hconstraint,constraints,
     &gradconstraints
      integer :: irhot   
      external hinterp, updateh, updatermask
      external initzeta,initubar,initvbar,inith
      external initu,initv,initt
# ifdef WKB_WWAVE
      external initwac,initwkx,initwke,initwar
      external inithrm,initfrq,initwcg,initwsb,initwvn
# endif
!$AGRIF_DO_NOT_TREAT
       integer isens
       common/interph/isens
!$AGRIF_END_DO_NOT_TREAT     

# ifdef MPI
#  define LOCALLM Lmmpi
#  define LOCALMM Mmmpi
# else
#  define LOCALLM Lm
#  define LOCALMM Mm
# endif 
      
# include "dynderivparam.h"

# ifdef AUTOTILING
      call init_auto_tiling
# endif
       
!      Call Agrif_Set_type(zeta,(/2,2,0/),(/1,1,0/))
!      Call Agrif_Set_raf(zeta,(/'x','y','N'/))
# ifdef SOLVE3D
!      Call Agrif_Set_type(DU_avg1,(/1,2,0/),(/1,1,0/))
!      Call Agrif_Set_type(DV_avg1,(/2,1,0/),(/1,1,0/))
!      Call Agrif_Set_raf(DU_avg1,(/'x','y','N'/))
!      Call Agrif_Set_raf(DV_avg1,(/'x','y','N'/))
!      
!      Call Agrif_Set_type(DU_avg2,(/1,2/),(/1,1/))
!      Call Agrif_Set_type(DV_avg2,(/2,1/),(/1,1/))
!      Call Agrif_Set_raf(DU_avg2,(/'x','y'/))
!      Call Agrif_Set_raf(DV_avg2,(/'x','y'/))
!
!      Call Agrif_Set_type(u,(/1,2,0,0/),(/1,1,0,0/))
!      Call Agrif_Set_type(v,(/2,1,0,0/),(/1,1,0,0/))
!      Call Agrif_Set_raf(u,(/'x','y','N','N'/))
!      Call Agrif_Set_raf(v,(/'x','y','N','N'/))
!
!
!      Call Agrif_Set_type(Hz,(/1,2,0,0/),(/1,1,0,0/))
!      Call Agrif_Set_type(Hz_bak,(/2,1,0,0/),(/1,1,0,0/))
!      Call Agrif_Set_raf(Hz,(/'x','y','N','N'/))
!      Call Agrif_Set_raf(Hz_bak,(/'x','y','N','N'/))
!      
!      Call Agrif_Set_type(t,(/2,2,0,0,0/),(/1,1,0,0,0/))
!      Call Agrif_Set_raf(t,(/'x','y','N','N','N'/))
!      
!      Call Agrif_Set_type(Zt_avg1,(/2,2/),(/1,1/))
!      Call Agrif_Set_raf(Zt_avg1,(/'x','y'/))
!
!      Call Agrif_Set_type(Zt_avg2,(/2,2,0/),(/1,1,0/))
!      Call Agrif_Set_raf(Zt_avg2,(/'x','y','N'/))
!            
!      Call Agrif_Set_type(h,(/2,2/),(/1,1/))
!      Call Agrif_Set_raf(h,(/'x','y'/))  

#  ifdef MASKING      
!      Call Agrif_Set_type(rmask,(/2,2/),(/1,1/))
!      Call Agrif_Set_raf(rmask,(/'x','y'/))          
#  endif            

# else
!      Call Agrif_Set_type(ubar,(/1,2,0/),(/1,1,0/))
!      Call Agrif_Set_type(vbar,(/2,1,0/),(/1,1,0/))
!      Call Agrif_Set_raf(ubar,(/'x','y','N'/))
!      Call Agrif_Set_raf(vbar,(/'x','y','N'/))
# endif
!
!      Call Agrif_Set_bcinterp(zeta,AGRIF_linear)
!      Call Agrif_Set_bc(zeta,(/-1,0/))
!
!      Call Agrif_Set_bcinterp(h,AGRIF_linear)
!      Call Agrif_Set_interp(h,AGRIF_linear)
!      Call Agrif_Set_bc(h,(/0,0/))
     
# if defined AGRIF_UPDATE_FULLWEIGHTING || defined AGRIF_UPDATE_MIX        
!      Call Agrif_Set_UpdateType(h,update=
!     &   Agrif_Update_Full_Weighting)
# endif

# if defined AGRIF_UPDATE_MIX_HIGH
!      Call Agrif_Set_UpdateType(h,update=
!     &   Agrif_Update_4rdorder)
# endif

# ifdef MASKING      
!      Call Agrif_Set_UpdateType(rmask,update=
!     &   Agrif_Update_Average)
# endif

      call declare_zoom_variables()

# ifdef MPI
      call MPI_Setup (ierr)
      if (ierr.ne.0) goto 100         !--> ERROR
# endif
!
!----------------------------------------------------------------------
!  Read in tunable model parameters in roms.in file
!----------------------------------------------------------------------
!
      call read_inp (ierr)
      if (ierr.ne.0) goto 100
!
!----------------------------------------------------------------------
!  Initialize global model parameters
!----------------------------------------------------------------------
!
!  Gobal scalar variables
!
      call init_scalars (ierr)
      if (ierr.ne.0) goto 100
!
!  Generic Length Scale (GLS) model parameters
!
# ifdef SOLVE3D
#  ifdef GLS_MIXING
      call init_gls (ierr)
#  endif
!
!  PISCES biogeochemeical model parameters
!
#  ifdef PISCES
!      call get_dust
#  endif
!
!  Read sediment initial values and parameters from sediment.in file
!
#  ifdef SEDIMENT
      if (Agrif_Fixed().GE.Agrif_lev_sedim) then
        call init_sediment
      endif
#  endif
# endif
!
!----------------------------------------------------------------------
!  Create parallel threads 
!  initialize (FIRST-TOUCH) model global arrays (most of them
!  are just set to to zero).
!----------------------------------------------------------------------
!
C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1
!        call start_timers()
        call init_arrays (tile) 
      enddo 
CR      write(*,*) '-11' MYID
!
!----------------------------------------------------------------------
!  Set horizontal grid, model bathymetry and Land/Sea mask
!----------------------------------------------------------------------
!
# ifdef ANA_GRID
!
!  Set grid analytically
!
C$OMP PARALLEL DO PRIVATE(tile) 
      do tile=0,NSUB_X*NSUB_E-1 
        call ana_grid (tile) 
      enddo
        call Agrif_Init_Variable(hid,procname=inith)
# else
!
!  Read grid from GRID NetCDF file 
!
      call get_grid 
      if (may_day_flag.ne.0) goto 99  !-->  EXIT
# endif
!
!  Compute various metric term combinations.
!
C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1
        call setup_grid1 (tile)
      enddo
CR      write(*,*) '-10' MYID
!
C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1
        call setup_grid2 (tile)
      enddo
CR      write(*,*) ' -9' MYID
!
!----------------------------------------------------------------------
!  Setup vertical grid variables setup vertical S-coordinates 
!  and fast-time averaging for coupling of 
!  split-explicit baroropic mode.
!----------------------------------------------------------------------
!
# ifdef SOLVE3D
!
!  Set vertical S-coordinate functions
!
      call set_scoord 
!
!  Set fast-time averaging for coupling of split-explicit baroropic mode.
!
      call set_weights
# endif
CR      write(*,*) ' -8' MYID 
!
!  Create three-dimensional S-coordinate system,
!  which may be needed by ana_initial 
!  (here it is assumed that free surface zeta=0).
!
# ifdef SOLVE3D
C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1
        call set_depth (tile)
      enddo
CR      write(*,*) ' -7' MYID
!
!  Make grid diagnostics
!
C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1
        call grid_stiffness (tile) 
      enddo
# endif
!
!----------------------------------------------------------------------
!  Initialize XIOS I/O server
!----------------------------------------------------------------------
!
# ifdef XIOS
      do tile=0,NSUB_X*NSUB_E-1
        call  init_xios(tile)
      enddo
# endif
!
!----------------------------------------------------------------------
!  Set initial conditions for momentum and tracer variables
!----------------------------------------------------------------------
!
# ifdef ANA_INITIAL
      if (nrrec.ne.0) then
# endif
# ifndef AGRIF_ADAPTIVE
!
!  Read from NetCDF file
!
        call get_initial
# else
        call Agrif_Declare_Variable((/2,2/),(/1,1/),(/'x','y'/)
     &   ,lbound(Zt_avg1),ubound(Zt_avg1),zetaid,torestore=.true.)
        call Agrif_Declare_Variable((/1,2/),(/1,1/),(/'x','y'/)
     &   ,lbound(DU_avg2),ubound(DU_avg2),ubarid,torestore=.true.)
        call Agrif_Declare_Variable((/2,1/),(/1,1/),(/'x','y'/)
     &   ,lbound(DV_avg2),ubound(DV_avg2),vbarid,torestore=.true.)  
     
        allocate(text(GLOBAL_2D_ARRAY,N,NT))  
             
        call Agrif_Declare_Variable((/2,2,0,0/),(/1,1,0,0/),
     &    (/'x','y','N','N'/),lbound(text),ubound(text),tid,
     &    torestore=.true.)
        call Agrif_Declare_Variable((/1,2,0/),(/1,1,0/),
     &   (/'x','y','N'/),lbound(Huon),ubound(Huon),uid,
     &   torestore=.true.)
        call Agrif_Declare_Variable((/2,1,0/),(/1,1,0/),
     &   (/'x','y','N'/),lbound(Hvom),ubound(Hvom),vid,
     &   torestore=.true.) 
             
        call Agrif_Set_bc(zetaid,(/-1,0/))     
        call Agrif_Set_bc(ubarid,(/0,0/))
        call Agrif_Set_bc(vbarid,(/0,0/))
        call Agrif_Set_bc(tid,(/-1,0/))     
        call Agrif_Set_bc(uid,(/0,0/))
        call Agrif_Set_bc(vid,(/0,0/))
        
        call Agrif_Set_bcinterp(zetaid,interp=Agrif_ppm)        
        call Agrif_Set_bcinterp(ubarid,interp1=Agrif_linear,
     &                                 interp2=Agrif_ppm)
        call Agrif_Set_bcinterp(vbarid,interp1=Agrif_ppm,
     &                                 interp2=Agrif_linear)
        call Agrif_Set_bcinterp(tid,interp=Agrif_ppm)
        call Agrif_Set_bcinterp(uid,interp1=Agrif_linear,
     &                                 interp2=Agrif_ppm)
        call Agrif_Set_bcinterp(vid,interp1=Agrif_ppm,
     &                                 interp2=Agrif_linear)
        
        call Agrif_Set_interp(zetaid,interp=Agrif_ppm)
        call Agrif_Set_interp(ubarid,interp1=Agrif_linear,
     &                                 interp2=Agrif_ppm)
        call Agrif_Set_interp(vbarid,interp1=Agrif_ppm,
     &                                 interp2=Agrif_linear)
        call Agrif_Set_interp(tid,interp=Agrif_ppm)
        call Agrif_Set_interp(uid,interp1=Agrif_linear,
     &                                 interp2=Agrif_ppm)
        call Agrif_Set_interp(vid,interp1=Agrif_ppm,
     &                                 interp2=Agrif_linear)
     
        call Agrif_Init_Variable(zetaid,procname=initzeta)

        call Agrif_Init_Variable(ubarid,procname=initubar)
        call Agrif_Init_Variable(vbarid,procname=initvbar)

        call Agrif_Init_Variable(tid,procname=initt)

        do itrc=1,NT
          got_tini(itrc) = .true.
        enddo

        call Agrif_Init_Variable(uid,procname=initu)
        call Agrif_Init_Variable(vid,procname=initv)

        if (Agrif_Parent_Nb_Step() == 0) then
          call get_initial
        endif
# endif /* AGRIF_ADAPTIVE */
# ifdef ANA_INITIAL
      endif  ! (nrrec.ne.0)
# endif
!
!  Set initial conditions analytically for ideal cases
!  or for tracer variables not present in NetCDF file
!
C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1
        call ana_initial (tile)
      enddo
!
!----------------------------------------------------------------------
!  Initialize specific PISCES variables
!----------------------------------------------------------------------
!
# if defined BIOLOGY && defined PISCES
C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1
        call  pisces_ini_tile (tile)
      enddo
      CALL trc_nam_pisces
# endif
CR      write(*,*) ' -6' MYID 
      if (may_day_flag.ne.0) goto 99 !-->  EXIT
!
!----------------------------------------------------------------------
!  Finalize grid setup
!----------------------------------------------------------------------
!
!  Finalize vertical grid now that zeta is knowned
!  zeta is also corrected here fr Wetting/Drying
!  in both 2D and 3D cases
!
# if defined  SOLVE3D || defined WET_DRY
C$OMP PARALLEL DO PRIVATE(tile)      ! Create three-dimensional
      do tile=0,NSUB_X*NSUB_E-1      ! S-coordinate system: at this
        call set_depth (tile)        ! time free surface is set to
      enddo                          ! a non-zero field, either 
CR      write(*,*)  ' -5' MYID       ! analytically or from restart.
# endif

# ifdef SOLVE3D
#  ifdef AGRIF_ADAPTIVE
      if (Agrif_Parent_Nb_Step() /= 0) then
      do j=0,Mm+1
      do i=1,Lm+1
        DC(i,j,0)=0.
        CF(i,j,0)=0.
      enddo
      enddo
      
      do k=1,N
      do j=0,Mm+1
      do i=1,Lm+1
        DC(i,j,k) = 0.5*(Hz(i,j,k)+Hz(i-1,j,k))*on_u(i,j)
        DC(i,j,0)=DC(i,j,0)+DC(i,j,k)
        CF(i,j,0)=CF(i,j,0)+DC(i,j,k)*u(i,j,k,1)
      enddo
      enddo
      enddo
      
      do j=0,Mm+1
      do i=1,Lm+1
        DC(i,j,0)=1./DC(i,j,0)
        CF(i,j,0)=DC(i,j,0)*(CF(i,j,0)-DU_avg1(i,j,1))
        ubar(i,j,1) = DC(i,j,0)*DU_avg1(i,j,1)
      enddo
      enddo
      
      do k=N,1,-1
      do j=0,Mm+1
      do i=0,Lm+1
        u(i,j,k,1) = (u(i,j,k,1)-CF(i,j,0))
      enddo
      enddo
      enddo
      
      do j=1,Mm+1
      do i=0,Lm+1
        DC(i,j,0)=0.
        CF(i,j,0)=0.
      enddo
      enddo
      
      do k=1,N
      do j=1,Mm+1
      do i=0,Lm+1
        DC(i,j,k) = 0.5*(Hz(i,j,k)+Hz(i,j-1,k))*om_v(i,j)
        DC(i,j,0)=DC(i,j,0)+DC(i,j,k)
        CF(i,j,0)=CF(i,j,0)+DC(i,j,k)*v(i,j,k,1)
      enddo
      enddo
      enddo
      
      do j=1,Mm+1
      do i=0,Lm+1
        DC(i,j,0)=1./DC(i,j,0)
        CF(i,j,0)=DC(i,j,0)*(CF(i,j,0)-DV_avg1(i,j,1))
        vbar(i,j,1) = DC(i,j,0)*DV_avg1(i,j,1)
      enddo
      enddo
      
      do k=N,1,-1
      do j=1,Mm+1
      do i=0,Lm+1
        v(i,j,k,1) = (v(i,j,k,1)-CF(i,j,0))
      enddo
      enddo
      enddo     
          
      endif 
#  endif /* AGRIF_ADAPTIVE */
!
!----------------------------------------------------------------------
!  Initialize diagnostic fields: mass flux, rho, omega
!----------------------------------------------------------------------
!
C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1
        call set_HUV (tile)
      enddo
CR      write(*,*)  ' -4' MYID
C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1
        call omega (tile)
        call rho_eos (tile)
      enddo
CR      write(*,*)  ' -3' MYID
# endif /* SOLVE3D */
!
!----------------------------------------------------------------------
! Create environment for lateral forcing:
!----------------------------------------------------------------------
! Set nudging coefficient for sea surface hight and tracer climatology;
! Set ANALYTICAL:
!   bottom sediment grain size [m] and density [kg/m^3]
!     used in bottom boundary layer formulation;
!   surface forcing from NetCDF file
!----------------------------------------------------------------------
!
# if defined TNUDGING || defined ZNUDGING || defined SPONGE \
                      || (defined BBL && defined ANA_BSEDIM)
!
C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1
#  if defined TNUDGING || defined ZNUDGING || defined SPONGE
        call set_nudgcof_fine (tile)
#  endif
#  if defined BBL && defined ANA_BSEDIM
        if (Agrif_Fixed().GE.Agrif_lev_sedim) then
          call ana_bsedim (tile)
        endif
#  endif
#  if defined SEDIMENT && defined ANA_SEDIMENT
        if (Agrif_Fixed().GE.Agrif_lev_sedim) then
          call ana_sediment (tile)
        endif
#  endif
      enddo !<-- tile
# endif
CR      write(*,*) ' -2' MYID 
!
!  Read bottom sediment grain size [m] and density [kg/m^3] 
!  from input NetCDF file;
!
# if defined BBL && !defined ANA_BSEDIM && !defined SEDIMENT
      if (Agrif_Fixed().GE.Agrif_lev_sedim) then
          call get_bsedim
      endif
# endif
# if defined SEDIMENT && !defined ANA_SEDIMENT
      if (Agrif_Fixed().GE.Agrif_lev_sedim) then
          call get_sediment
      endif
# endif
!
!  Read surface forcing from NetCDF file
!
      call get_vbc
!
!----------------------------------------------------------------------
!  WKB surface wave model:
!
!  initialization and initial run to equilibrium 
!----------------------------------------------------------------------
!
# ifdef WKB_WWAVE
      call Agrif_Set_interp(wacid,interp=Agrif_ppm)
      call Agrif_Init_Variable(wacid,procname=initwac)
      call Agrif_Set_interp(wkxid,interp=Agrif_ppm)
      call Agrif_Init_Variable(wkxid,procname=initwkx)
      call Agrif_Set_interp(wkeid,interp=Agrif_ppm)
      call Agrif_Init_Variable(wkeid,procname=initwke)
        
      call Agrif_Set_interp(hrmid,interp=Agrif_ppm)
      call Agrif_Init_Variable(hrmid,procname=inithrm)
      call Agrif_Set_interp(frqid,interp=Agrif_ppm)
      call Agrif_Init_Variable(frqid,procname=initfrq)
      call Agrif_Set_interp(wsbid,interp=Agrif_ppm)
      call Agrif_Init_Variable(wsbid,procname=initwsb)
      call Agrif_Set_interp(wvnid,interp=Agrif_ppm)
      call Agrif_Init_Variable(wvnid,procname=initwvn)
      call Agrif_Set_interp(wcgid,interp=Agrif_ppm)
      call Agrif_Init_Variable(wcgid,procname=initwcg)
#  ifdef WAVE_ROLLER
      call Agrif_Set_interp(warid,interp=Agrif_ppm)
      call Agrif_Init_Variable(warid,procname=initwar)
#  endif
C$OMP BARRIER
C$OMP MASTER
      MPI_master_only write(stdout,'(/1x,A/)')
     &     'WKB: started child wave computation.'
C$OMP END MASTER
      wkb_agrif_done=.FALSE.
      iic=0
      winfo=1
      iwave=1
      thwave=1.D+10
#  ifndef ANA_BRY_WKB
C$OMP PARALLEL DO PRIVATE(tile)
!      do tile=0,NSUB_X*NSUB_E-1
!        call set_bry_wkb (tile)
!      enddo
#  endif
#  ifdef MRL_CEW
C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1
        call wkb_cew_prep (tile)
      enddo
C$OMP BARRIER
      wint=0
      do winterp=1,interp_max
        wint=wint+1
        if (wint.gt.2) wint=1
C$OMP PARALLEL DO PRIVATE(tile)
        do tile=0,NSUB_X*NSUB_E-1
          call wkb_uvfield (tile, winterp)
        enddo
      enddo
C$OMP BARRIER
C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1
        call wkb_cew_finalize (tile)
      enddo
C$OMP BARRIER
#  endif
      do while (iwave.le.1000.and.thwave.ge.1.D-10)
        wstp=wnew
        wnew=wstp+1
        if (wnew.ge.3) wnew=1
C$OMP PARALLEL DO PRIVATE(tile)
        do tile=0,NSUB_X*NSUB_E-1
          call wkb_wwave (tile)
        enddo
        call wkb_diag (0)
        iwave=iwave+1
        thwave=max(av_wac,av_wkn)
      enddo

#  if defined RVTK_DEBUG || defined RVTK_DEBUG_ADVANCED
C$OMP BARRIER
C$OMP MASTER
      call check_tab2d(wac(:,:,wnew),'wac initialisation #1','r')
C$OMP END MASTER
#  endif

      first_time=0
C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1
        call mrl_wci (tile)
      enddo

C$OMP BARRIER
C$OMP MASTER
        MPI_master_only write(stdout,'(/1x,A/)')
     &     'WKB: completed child wave computation.'
C$OMP END MASTER
# endif /* WKB_WWAVE */
!
CR      write(*,*) ' -1' MYID 
      if (may_day_flag.ne.0) goto 99     !-->  EXIT
!
!----------------------------------------------------------------------
!  Write initial fields into history NetCDF files
!----------------------------------------------------------------------
!
# ifdef XIOS
!RB to be checked
!      if (nrrec.eq.0) then
!        do tile=0,NSUB_X*NSUB_E-1
!          call send_xios_diags(tile)
!        enddo
!      endif
# else
      if (ldefhis .and. wrthis(indxTime)) call wrt_his
# endif

CR      write(*,*) '  0' MYID
      if (may_day_flag.ne.0) goto 99     !-->  EXIT

 99   continue
 100  continue
 
      next_kstp=kstp
      time_start=time

!
!----------------------------------------------------------------------
!   AGRIF initialization
!----------------------------------------------------------------------
!
      U2DTimeindex = -1
      V2DTimeindex = -1
      ZetaTimeindex = -1
# ifdef SOLVE3D
      Ttimeindex = -1
      Utimeindex = -1
      Vtimeindex = -1
#  ifdef AGRIF_OBC_SOUTH
#   ifdef MPI        
            if (.not.SOUTH_INTER) then
#   endif
                U_south(0:LOCALLM+1,0:0,1:N,4)=
     &          u(0:LOCALLM+1,0:0,1:N,1)
     
                V_south(0:LOCALLM+1,1:1,1:N,4)=
     &          v(0:LOCALLM+1,1:1,1:N,1)
     
                T_south(0:LOCALLM+1,0:1,1:N,4,1:NT)=
     &          t(0:LOCALLM+1,0:1,1:N,1,1:NT)     
          
#   ifdef MPI           
            endif          
#   endif            
#  endif            
#  ifdef AGRIF_OBC_NORTH
#   ifdef MPI        
            if (.not.NORTH_INTER) then  
#   endif
                U_north(0:LOCALLM+1,LOCALMM+1:LOCALMM+1,1:N,4)=
     &          u(0:LOCALLM+1,LOCALMM+1:LOCALMM+1,1:N,1)
     
                V_north(0:LOCALLM+1,LOCALMM+1:LOCALMM+1,1:N,4)=
     &          v(0:LOCALLM+1,LOCALMM+1:LOCALMM+1,1:N,1) 
     
                T_north(0:LOCALLM+1,LOCALMM:LOCALMM+1,1:N,4,1:NT)=
     &          t(0:LOCALLM+1,LOCALMM:LOCALMM+1,1:N,1,1:NT)         
          
#   ifdef MPI                    
            endif  
#   endif            
#  endif            
#  ifdef AGRIF_OBC_WEST
#   ifdef MPI        
            if (.not.WEST_INTER) then
#   endif
                U_west(1:1,0:LOCALMM+1,1:N,4)=
     &          u(1:1,0:LOCALMM+1,1:N,1)
     
                V_west(0:0,0:LOCALMM+1,1:N,4)=
     &            v(0:0,0:LOCALMM+1,1:N,1)
     
                T_west(0:1,0:LOCALMM+1,1:N,4,1:NT)=
     &          t(0:1,0:LOCALMM+1,1:N,1,1:NT)     
                    
#   ifdef MPI            
            endif
#   endif            
#  endif            
#  ifdef AGRIF_OBC_EAST
#   ifdef MPI        
            if (.not.EAST_INTER) then
#   endif
                U_east(LOCALLM+1:LOCALLM+1,0:LOCALMM+1,1:N,4)=
     &          u(LOCALLM+1:LOCALLM+1,0:LOCALMM+1,1:N,1)
     
                V_east(LOCALLM+1:LOCALLM+1,0:LOCALMM+1,1:N,4)=
     &          v(LOCALLM+1:LOCALLM+1,0:LOCALMM+1,1:N,1)
     
                T_east(LOCALLM:LOCALLM+1,0:LOCALMM+1,1:N,4,1:NT)=
     &          t(LOCALLM:LOCALLM+1,0:LOCALMM+1,1:N,1,1:NT)     
#   ifdef MPI               
            endif  
#   endif                            
#  endif      
# endif /* SOLVE3D */

      TspongeTimeindex = -1
      UVspongeTimeindex = -1

      iif = -1
      
# ifdef AGRIF_ADAPTIVE
      ntstart = 1 + Agrif_irhot()*
     &   (Agrif_Parent(iic)-1)   
      time_start=Agrif_Parent(time_start)
     &+dt*Agrif_irhot()
     & *float(Agrif_Parent(iic)
     &-Agrif_Parent(ntstart))
# endif      
      iic = ntstart

      nbcoarse = 1
      nbstep3d = 0
      
      TTimesponge = 1
      UVTimesponge = 1

      usponge = 0.
      vsponge = 0.
      
      myfx = 0.
      myfy = 0.
      DU_avg1(:,:,4:5) = 0.
      DV_avg1(:,:,4:5) = 0.
      
# ifdef AGRIF_2WAY   
#  ifdef MASKING
      Agrif_UseSpecialValueInUpdate = .TRUE.
      Agrif_SpecialValueFineGrid = 0.
      
      Call Agrif_Update_Variable(rmaskid,procname=updatermask)
      
      Agrif_UseSpecialValueInUpdate = .FALSE.   

      Call Agrif_ChildGrid_To_ParentGrid()
      Call ResetMask()
      Call Agrif_ParentGrid_To_ChildGrid()
#  endif 
   
      Agrif_UseSpecialValueInUpdate = .TRUE.
      Agrif_SpecialValueFineGrid = 0.   
      Call Agrif_Update_Variable(hid,locupdate=(/1,0/),
     &     procname=updateh)
      Agrif_UseSpecialValueInUpdate = .FALSE.

      isens = 1 
      Call Agrif_Bc_Variable(ubarid,calledweight=1.,procname=hinterp)
          
      isens = 2
      Call Agrif_Bc_Variable(vbarid,calledweight=1.,procname=hinterp)

      Agrif_UseSpecialValueInUpdate = .TRUE.
      Agrif_SpecialValueFineGrid = 0.   
      Call Agrif_Update_Variable(hid,locupdate=(/1,0/),
     &     procname=updateh)
      Agrif_UseSpecialValueInUpdate = .FALSE.

      Call Agrif_ChildGrid_To_ParentGrid()
      Call UpdateGridhis()
      Call Agrif_ParentGrid_To_ChildGrid()
# endif /* AGRIF_2WAY */
    
# ifdef AGRIF_ADAPTIVE
      call Agrif_Set_Regridding(300*(nfast+2))
      call Agrif_Set_Minwidth(18)
      call Agrif_Set_Rafmax(2)
# endif
     
      return
      end
!
!====================================================================
!                   subroutine declare_zoom_variables
!====================================================================
!
      subroutine declare_zoom_variables()
      use AGRIF_UTIL
      implicit none
# include "param.h"
# include "ncscrum.h"
# include "scalars.h"
# include "zoom.h"
      integer :: irhot
      
      i1t = 0
      i2t = Lm+1
      j1t = 0
      j2t = Mm+1

# ifdef MPI
      if (WEST_INTER) then
        i1t = -1
      endif
      if (EAST_INTER) then
        i2t = Lmmpi+2
      else
        i2t = Lmmpi+1
      endif 
      if (SOUTH_INTER) then
        j1t = -1
      endif
      if (NORTH_INTER) then
        j2t = Mmmpi+2
      else
        j2t = Mmmpi+1
      endif           
# endif    

      i1u = i1t
      i2u = i2t
      j1v = j1t
      j2v = j2t
      
      irhot = Agrif_Irhot()
      
      Call Agrif_Declare_Variable((/2,2,0/),(/1,1,0/),
     &    (/'x','y','N'/),(/i1t,j1t,1/),
     &    (/i2t,j2t,irhot+1/),updatezetaid)

# if defined AGRIF_UPDATE_FULLWEIGHTING || defined AGRIF_UPDATE_MIX        
      Call Agrif_Set_UpdateType(updatezetaid,update=
     &Agrif_Update_full_weighting)
# endif

# if defined AGRIF_UPDATE_MIX_HIGH
      Call Agrif_Set_UpdateType(updatezetaid,update=
     &Agrif_Update_4rdorder)
# endif

# if defined AGRIF_UPDATE_AVERAGE || defined AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(updatezetaid,update=
     &Agrif_Update_Average)
# endif
     
      Call Agrif_Declare_Variable((/1,2,0/),(/1,1,0/),
     &    (/'x','y','N'/),(/0,j1t,1/),
     &    (/i2u,j2t,irhot+1/),updateubarid)

      Call Agrif_Declare_Variable((/1,2/),(/1,1/),
     &    (/'x','y'/),(/0,j1t/),
     &    (/i2u,j2t/),updateduavg2id)
     
# ifdef AGRIF_UPDATE_FULLWEIGHTING         
      Call Agrif_Set_UpdateType(updateubarid,update=
     &Agrif_Update_full_weighting)
      Call Agrif_Set_UpdateType(updateduavg2id,update=
     &Agrif_Update_full_weighting)
# endif

# ifdef AGRIF_UPDATE_MIX
      Call Agrif_Set_UpdateType(updateubarid,
     &update1=Agrif_Update_Average,
     &update2=Agrif_Update_Full_Weighting)
      Call Agrif_Set_UpdateType(updateduavg2id,
     &update1=Agrif_Update_Average,
     &update2=Agrif_Update_Full_Weighting)
# endif

# ifdef AGRIF_UPDATE_MIX_HIGH
      Call Agrif_Set_UpdateType(updateubarid,
     &update1=Agrif_Update_Full_Weighting,
     &update2=Agrif_Update_4rdorder)
      Call Agrif_Set_UpdateType(updateduavg2id,
     &update1=Agrif_Update_Full_Weighting,
     &update2=Agrif_Update_4rdorder)
     
# endif

# ifdef AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(updateubarid,
     &update1=Agrif_Update_Copy,
     &update2=Agrif_Update_Average)
      Call Agrif_Set_UpdateType(updateduavg2id,
     &update1=Agrif_Update_Copy,
     &update2=Agrif_Update_Average)
#endif

#ifdef AGRIF_UPDATE_AVERAGE
      Call Agrif_Set_UpdateType(updateubarid,
     &update=Agrif_Update_Average)   
      Call Agrif_Set_UpdateType(updateduavg2id,
     &update=Agrif_Update_Average) 
# endif

      Call Agrif_Declare_Variable((/2,1,0/),(/1,1,0/),
     &    (/'x','y','N'/),(/i1t,0,1/),
     &    (/i2t,j2v,irhot+1/),updatevbarid)
     
      Call Agrif_Declare_Variable((/2,1/),(/1,1/),
     &    (/'x','y'/),(/i1t,0/),
     &    (/i2t,j2v/),updatedvavg2id)

# ifdef AGRIF_UPDATE_FULLWEIGHTING          
      Call Agrif_Set_UpdateType(updatevbarid,update=
     &Agrif_Update_full_weighting)  
      Call Agrif_Set_UpdateType(updatedvavg2id,update=
     &Agrif_Update_full_weighting)
# endif

# ifdef AGRIF_UPDATE_MIX
      Call Agrif_Set_UpdateType(updatevbarid,
     &update1=Agrif_Update_Full_Weighting,
     &update2=Agrif_Update_Average)
      Call Agrif_Set_UpdateType(updatedvavg2id,
     &update1=Agrif_Update_Full_Weighting,
     &update2=Agrif_Update_Average)
# endif

# ifdef AGRIF_UPDATE_MIX_HIGH
      Call Agrif_Set_UpdateType(updatevbarid,
     &update1=Agrif_Update_4rdorder,
     &update2=Agrif_Update_Full_Weighting)
      Call Agrif_Set_UpdateType(updatedvavg2id,
     &update1=Agrif_Update_4rdorder,
     &update2=Agrif_Update_Full_Weighting)
# endif

# ifdef AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(updatevbarid,
     &update1=Agrif_Update_Average,
     &update2=Agrif_Update_Copy)
      Call Agrif_Set_UpdateType(updatedvavg2id,
     &update1=Agrif_Update_Average,
     &update2=Agrif_Update_Copy)
# endif

# ifdef AGRIF_UPDATE_AVERAGE
      Call Agrif_Set_UpdateType(updatevbarid,
     &update=Agrif_Update_Average)   
      Call Agrif_Set_UpdateType(updatedvavg2id,
     &update=Agrif_Update_Average) 
# endif

     
      Call Agrif_Declare_Variable((/2,2,0,0/),(/1,1,0,0/),
     &    (/'x','y','N','N'/),(/i1t,j1t,1,1/),
     &    (/i2t,j2t,N,NT/),updatetid)

# if defined AGRIF_UPDATE_FULLWEIGHTING || defined AGRIF_UPDATE_MIX
      Call Agrif_Set_UpdateType(updatetid,update=
     &Agrif_Update_full_weighting)
# endif

# if defined AGRIF_UPDATE_MIX_HIGH
      Call Agrif_Set_UpdateType(updatetid,update=
     &Agrif_Update_4rdorder)
# endif

# if defined AGRIF_UPDATE_AVERAGE || defined AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(updatetid,update=
     &Agrif_Update_Average)
# endif

      Call Agrif_Declare_Variable((/1,2,0/),(/1,1,0/),
     &    (/'x','y','N'/),(/i1u,j1t,1/),
     &    (/i2u,j2t,N/),updateuid)

# ifdef AGRIF_UPDATE_FULLWEIGHTING           
      Call Agrif_Set_UpdateType(updateuid,update=
     &Agrif_Update_full_weighting)
# endif

# ifdef AGRIF_UPDATE_MIX
      Call Agrif_Set_UpdateType(updateuid,
     &update1=Agrif_Update_Average,
     &update2=Agrif_Update_Full_Weighting)
# endif

# ifdef AGRIF_UPDATE_MIX_HIGH
      Call Agrif_Set_UpdateType(updateuid,
     &update1=Agrif_Update_Full_Weighting,
     &update2=Agrif_Update_4rdorder)
# endif

# ifdef AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(updateuid,
     &update1=Agrif_Update_Copy,
     &update2=Agrif_Update_Average)
# endif

# ifdef AGRIF_UPDATE_AVERAGE
      Call Agrif_Set_UpdateType(updateuid,
     &update=Agrif_Update_Average)    
# endif

      Call Agrif_Declare_Variable((/1,2,0/),(/1,1,0/),
     &    (/'x','y','N'/),(/i1u,j1t,1/),
     &    (/i2u,j2t,N/),updatehuonid)

# ifdef AGRIF_UPDATE_FULLWEIGHTING           
      Call Agrif_Set_UpdateType(updatehuonid,update=
     &Agrif_Update_full_weighting)
# endif

# ifdef AGRIF_UPDATE_MIX
      Call Agrif_Set_UpdateType(updatehuonid,
     &update1=Agrif_Update_Average,
     &update2=Agrif_Update_Full_Weighting)
# endif

# ifdef AGRIF_UPDATE_MIX_HIGH
      Call Agrif_Set_UpdateType(updatehuonid,
     &update1=Agrif_Update_Full_Weighting,
     &update2=Agrif_Update_4rdorder)
# endif

# ifdef AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(updatehuonid,
     &update1=Agrif_Update_Copy,
     &update2=Agrif_Update_Average)
# endif

# ifdef AGRIF_UPDATE_AVERAGE
      Call Agrif_Set_UpdateType(updatehuonid,
     &update=Agrif_Update_Average)    
# endif

      Call Agrif_Declare_Variable((/1,2,0,0/),(/1,1,0,0/),
     &    (/'x','y','N','N'/),(/i1u,j1t,1,1/),
     &    (/i2u,j2t,N,NT/),updatemyfxid)

# ifdef AGRIF_UPDATE_FULLWEIGHTING           
      Call Agrif_Set_UpdateType(updatemyfxid,update=
     &Agrif_Update_full_weighting)
# endif

# ifdef AGRIF_UPDATE_MIX
      Call Agrif_Set_UpdateType(updatemyfxid,
     &update1=Agrif_Update_Average,
     &update2=Agrif_Update_Full_Weighting)
# endif

# ifdef AGRIF_UPDATE_MIX_HIGH
      Call Agrif_Set_UpdateType(updatemyfxid,
     &update1=Agrif_Update_Full_Weighting,
     &update2=Agrif_Update_Average)
# endif

# ifdef AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(updatemyfxid,
     &update1=Agrif_Update_Copy,
     &update2=Agrif_Update_Average)
# endif

# ifdef AGRIF_UPDATE_AVERAGE
      Call Agrif_Set_UpdateType(updatemyfxid,
     &update=Agrif_Update_Average)    
# endif

      Call Agrif_Declare_Variable((/2,1,0/),(/1,1,0/),
     &    (/'x','y','N'/),(/i1t,j1v,1/),
     &    (/i2t,j2v,N/),updatevid)

# ifdef AGRIF_UPDATE_FULLWEIGHTING           
      Call Agrif_Set_UpdateType(updatevid,update=
     &Agrif_Update_full_weighting)
# endif

# ifdef AGRIF_UPDATE_MIX     
      Call Agrif_Set_UpdateType(updatevid,
     &update1=Agrif_Update_Full_Weighting,
     &update2=Agrif_Update_Average)
# endif

# ifdef AGRIF_UPDATE_MIX_HIGH  
      Call Agrif_Set_UpdateType(updatevid,
     &update1=Agrif_Update_4rdorder,
     &update2=Agrif_Update_Full_Weighting)
# endif

# ifdef AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(updatevid,
     &update1=Agrif_Update_Average,
     &update2=Agrif_Update_Copy)
# endif

# ifdef AGRIF_UPDATE_AVERAGE     
      Call Agrif_Set_UpdateType(updatevid,
     &update=Agrif_Update_Average)     
# endif

      Call Agrif_Declare_Variable((/2,1,0/),(/1,1,0/),
     &    (/'x','y','N'/),(/i1t,j1v,1/),
     &    (/i2t,j2v,N/),updatehvomid)

# ifdef AGRIF_UPDATE_FULLWEIGHTING           
      Call Agrif_Set_UpdateType(updatehvomid,update=
     &Agrif_Update_full_weighting)
# endif

# ifdef AGRIF_UPDATE_MIX     
      Call Agrif_Set_UpdateType(updatehvomid,
     &update1=Agrif_Update_Full_Weighting,
     &update2=Agrif_Update_Average)
# endif

# ifdef AGRIF_UPDATE_MIX_HIGH 
      Call Agrif_Set_UpdateType(updatehvomid,
     &update1=Agrif_Update_4rdorder,
     &update2=Agrif_Update_Full_Weighting)
# endif

# ifdef AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(updatehvomid,
     &update1=Agrif_Update_Average,
     &update2=Agrif_Update_Copy)
# endif

# ifdef AGRIF_UPDATE_AVERAGE     
      Call Agrif_Set_UpdateType(updatehvomid,
     &update=Agrif_Update_Average)     
# endif

      Call Agrif_Declare_Variable((/2,1,0,0/),(/1,1,0,0/),
     &    (/'x','y','N','N'/),(/i1t,j1v,1,1/),
     &    (/i2t,j2v,N,NT/),updatemyfyid)

# ifdef AGRIF_UPDATE_FULLWEIGHTING           
      Call Agrif_Set_UpdateType(updatemyfyid,update=
     &Agrif_Update_full_weighting)
# endif

# ifdef AGRIF_UPDATE_MIX     
      Call Agrif_Set_UpdateType(updatemyfyid,
     &update1=Agrif_Update_Full_Weighting,
     &update2=Agrif_Update_Average)
# endif

# ifdef AGRIF_UPDATE_MIX_HIGH    
      Call Agrif_Set_UpdateType(updatemyfyid,
     &update1=Agrif_Update_4rdorder,
     &update2=Agrif_Update_Full_Weighting)
# endif

# ifdef AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(updatemyfyid,
     &update1=Agrif_Update_Average,
     &update2=Agrif_Update_Copy)
# endif

# ifdef AGRIF_UPDATE_AVERAGE     
      Call Agrif_Set_UpdateType(updatemyfyid,
     &update=Agrif_Update_Average)     
# endif

#ifdef WKB_WWAVE
         call Agrif_Declare_Variable((/2,2/),(/1,1/),(/'x','y'/),
     &   (/i1t,j1t/),(/i2t,j2t/),wacid)
         call Agrif_Declare_Variable((/2,2/),(/1,1/),(/'x','y'/),
     &   (/i1t,j1t/),(/i2t,j2t/),wkxid)
         call Agrif_Declare_Variable((/2,2/),(/1,1/),(/'x','y'/),
     &   (/i1t,j1t/),(/i2t,j2t/),wkeid)

         call Agrif_Declare_Variable((/2,2/),(/1,1/),(/'x','y'/),
     &   (/i1t,j1t/),(/i2t,j2t/),hrmid)
         call Agrif_Declare_Variable((/2,2/),(/1,1/),(/'x','y'/),
     &   (/i1t,j1t/),(/i2t,j2t/),frqid)
         call Agrif_Declare_Variable((/2,2/),(/1,1/),(/'x','y'/),
     &   (/i1t,j1t/),(/i2t,j2t/),wsbid)
         call Agrif_Declare_Variable((/2,2/),(/1,1/),(/'x','y'/),
     &   (/i1t,j1t/),(/i2t,j2t/),wvnid)
         call Agrif_Declare_Variable((/2,2/),(/1,1/),(/'x','y'/),
     &   (/i1t,j1t/),(/i2t,j2t/),wcgid)

# ifdef WAVE_ROLLER
         call Agrif_Declare_Variable((/2,2/),(/1,1/),(/'x','y'/),
     &   (/i1t,j1t/),(/i2t,j2t/),warid)
# endif
#endif

     
        call Agrif_Declare_Variable((/2,2/),(/1,1/),(/'x','y'/),
     &   (/i1t,j1t/),(/i2t,j2t/),hid)
        call Agrif_Declare_Variable((/2,2/),(/1,1/),(/'x','y'/),
     &   (/i1t,j1t/),(/i2t,j2t/),rmaskid)
        call Agrif_Declare_Variable((/2,2/),(/1,1/),(/'x','y'/),
     &   (/i1t,j1t/),(/i2t,j2t/),zetaid)
        call Agrif_Declare_Variable((/1,2/),(/1,1/),(/'x','y'/),
     &   (/0,j1t/),(/i2u,j2t/),ubarid)
        call Agrif_Declare_Variable((/2,1/),(/1,1/),(/'x','y'/),
     &   (/i1t,0/),(/i2t,j2v/),vbarid)    
             
        call Agrif_Declare_Variable((/2,2,0,0/),(/1,1,0,0/),
     &    (/'x','y','N','N'/),(/i1t,j1t,1,1/),
     &    (/i2t,j2t,N,NT/),tid)
     
        call Agrif_Declare_Variable((/2,2,0,0/),(/1,1,0,0/),
     &    (/'x','y','N','N'/),(/i1t,j1t,1,1/),
     &    (/i2t,j2t,N,NT/),tspongeid)
          
        call Agrif_Declare_Variable((/1,2,0/),(/1,1,0/),
     &   (/'x','y','N'/),(/i1u,j1t,1/),
     &   (/i2u,j2t,N/),uid)
     
        call Agrif_Declare_Variable((/1,2,0/),(/1,1,0/),
     &   (/'x','y','N'/),(/i1u,j1t,1/),
     &   (/i2u,j2t,N/),uspongeid)
          
        call Agrif_Declare_Variable((/2,1,0/),(/1,1,0/),
     &   (/'x','y','N'/),(/i1t,j1v,1/),
     &   (/i2t,j2v,N/),vid)
     
        call Agrif_Declare_Variable((/2,1,0/),(/1,1,0/),
     &   (/'x','y','N'/),(/i1t,j1v,1/),
     &   (/i2t,j2v,N/),vspongeid)     
             

#ifdef WKB_WWAVE
         call Agrif_Set_bc(wacid,(/0,0/))
         call Agrif_Set_interp(wacid,interp=Agrif_ppm)
         call Agrif_Set_bcinterp(wacid,interp=Agrif_ppm)
         call Agrif_Set_bc(wkxid,(/0,0/))
          call Agrif_Set_interp(wkxid,interp=Agrif_ppm)
          call Agrif_Set_bcinterp(wkxid,interp=Agrif_ppm)
         call Agrif_Set_bc(wkeid,(/0,0/))
         call Agrif_Set_bcinterp(wkeid,interp=Agrif_ppm)
         call Agrif_Set_interp(wkeid,interp=Agrif_ppm)

         call Agrif_Set_bc(hrmid,(/0,0/))
         call Agrif_Set_bcinterp(hrmid,interp=Agrif_ppm)
         call Agrif_Set_interp(hrmid,interp=Agrif_ppm)
         
         call Agrif_Set_bc(frqid,(/0,0/))
         call Agrif_Set_bcinterp(frqid,interp=Agrif_ppm)
         call Agrif_Set_interp(frqid,interp=Agrif_ppm)

         call Agrif_Set_bc(wsbid,(/0,0/))
         call Agrif_Set_bcinterp(wsbid,interp=Agrif_ppm)
         call Agrif_Set_interp(wsbid,interp=Agrif_ppm)

         call Agrif_Set_bc(wvnid,(/0,0/))
         call Agrif_Set_bcinterp(wvnid,interp=Agrif_ppm)
          call Agrif_Set_interp(wvnid,interp=Agrif_ppm)
        
         call Agrif_Set_bc(wcgid,(/0,0/))
         call Agrif_Set_bcinterp(wcgid,interp=Agrif_ppm)
         call Agrif_Set_interp(wcgid,interp=Agrif_ppm)

# ifdef WAVE_ROLLER
         call Agrif_Set_bc(warid,(/0,0/))
         call Agrif_Set_bcinterp(warid,interp=Agrif_ppm)
# endif
#endif 

        call Agrif_Set_bc(hid,(/0,0/)) 
        call Agrif_Set_bc(zetaid,(/-1,0/))     
        call Agrif_Set_bc(ubarid,(/0,0/))
        call Agrif_Set_bc(vbarid,(/0,0/))
        call Agrif_Set_bc(tid,(/-1,0/))     
        call Agrif_Set_bc(uid,(/0,0/))
        call Agrif_Set_bc(vid,(/0,0/))
        
        call Agrif_Set_bcinterp(hid,   interp=AGRIF_linear)        
        call Agrif_Set_interp(hid,   interp=AGRIF_linear)        
        call Agrif_Set_bcinterp(zetaid,interp=Agrif_lagrange)        
        call Agrif_Set_bcinterp(ubarid,interp1=Agrif_lagrange,
     &                                 interp2=Agrif_ppm)
        call Agrif_Set_bcinterp(vbarid,interp1=Agrif_ppm,
     &                                 interp2=Agrif_lagrange)
        call Agrif_Set_bcinterp(tid,interp=Agrif_lagrange)
        call Agrif_Set_bcinterp(tspongeid,interp=Agrif_lagrange)
        
        call Agrif_Set_bcinterp(uid,interp1=Agrif_lagrange,
     &                                 interp2=Agrif_ppm)
     
        call Agrif_Set_bcinterp(uspongeid,interp1=Agrif_lagrange,
     &                                 interp2=Agrif_ppm)
          
        call Agrif_Set_bcinterp(vid,interp1=Agrif_ppm,
     &                                 interp2=Agrif_lagrange)
     
        call Agrif_Set_bcinterp(vspongeid,interp1=Agrif_ppm,
     &                                 interp2=Agrif_lagrange) 

      Call Agrif_Set_UpdateType(rmaskid,update=
     &Agrif_Update_Average)
     
#if defined AGRIF_UPDATE_FULLWEIGHTING || defined AGRIF_UPDATE_MIX        
      Call Agrif_Set_UpdateType(hid,update=
     &Agrif_Update_full_weighting)
#endif

#if defined AGRIF_UPDATE_MIX_HIGH
      Call Agrif_Set_UpdateType(hid,update=
     &Agrif_Update_4rdorder)
#endif

#if defined AGRIF_UPDATE_AVERAGE || defined AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(hid,update=
     &Agrif_Update_Average)
#endif

      end subroutine declare_zoom_variables
!
!====================================================================
!                   subroutine ResetMask
!====================================================================
!
# ifdef MASKING
       recursive subroutine ResetMask()
       Use Agrif_Util
#  include "param.h"
#  include "grid.h"
#  include "scalars.h"
#  include "zoom.h"
       integer i,j
       external updatermask
       
#  ifdef MPI
      call exchange_r2d_tile (1,Lm,1,Mm,  rmask)
#  endif

      do j=0,Mm+1
      do i=1,Lm+1
        umask(i,j) = rmask(i,j)*rmask(i-1,j)
       enddo
       enddo

       do j=1,Mm+1
       do i=0,Lm+1
        vmask(i,j) = rmask(i,j)*rmask(i,j-1)
       enddo
       enddo
       
       do j=1,Mm+1
       do i=1,Lm+1
          pmask(i,j)=rmask(i,j)*rmask(i-1,j)*rmask(i,j-1)
     &                                      *rmask(i-1,j-1)
          pmask2(i,j)=pmask(i,j)
          if (gamma2.lt.0.) pmask(i,j)=2.-pmask(i,j)
      enddo
      enddo

#  ifdef MPI
      call exchange_u2d_tile (1,Lm,1,Mm,  umask)
      call exchange_v2d_tile (1,Lm,1,Mm,  vmask)
      call exchange_p2d_tile (1,Lm,1,Mm,  pmask)
      call exchange_p2d_tile (1,Lm,1,Mm,  pmask2)
#  endif

      if (.Not.Agrif_Root()) Then
        Agrif_UseSpecialValueInUpdate = .TRUE.
        Agrif_SpecialValueFineGrid = 0.
      
        Call Agrif_Update_Variable(rmaskid,procname=updatermask)
      
        Agrif_UseSpecialValueInUpdate = .FALSE.   

        Call Agrif_ChildGrid_To_ParentGrid()
        Call ResetMask()
        Call Agrif_ParentGrid_To_ChildGrid()
      endif

       end subroutine ResetMask
# endif /* MASKING */      
!
!====================================================================
!                   subroutine Updateh
!====================================================================
!
      Subroutine Updateh(tabres,i1,i2,j1,j2,before)
      implicit none
#  include "param.h"
#  include "grid.h"
#  include "ocean2d.h"
#  include "ocean3d.h"
#  include "coupling.h"
#  include "scalars.h"
#  include "zoom.h"
      integer i1,i2,j1,j2
      real tabres(i1:i2,j1:j2)
      logical before
      real cff_r,cff1_r,cff_w,cff1_w, z_r0,z_w0
      
      integer i,j,k  
       real tabtemp(i1:i2,j1:j2) 
              
      IF (before) THEN
 
        tabres(i1:i2,j1:j2) = h(i1:i2,j1:j2)
# ifdef MASKING
     &                        *rmask(i1:i2,j1:j2)
# endif
         
       ELSE

         do j=j1,j2
           do i=i1,i2
# ifndef WET_DRY
#  ifdef NEW_S_COORD
             h(i,j)=max(tabres(i,j),hmin)
#  else
             h(i,j)=max(tabres(i,j),hc)
#  endif
# endif
# ifdef NEW_S_COORD 
             hinv(i,j)=1./(h(i,j)+hc)      
# else
             hinv(i,j)=1./h(i,j) 
# endif
           enddo
         enddo
       
         do j=j1,j2
           do i=i1,i2
             z_w(i,j,0) = -h(i,j)
           enddo
           do k=1,N,+1
# ifdef NEW_S_COORD
             cff_w =hc*sc_w(k)
             cff_r =hc*sc_r(k)
             cff1_w=Cs_w(k)
             cff1_r=Cs_r(k)
# else
             cff_w =hc*(sc_w(k)-Cs_w(k))
             cff_r =hc*(sc_r(k)-Cs_r(k))
             cff1_w=Cs_w(k)
             cff1_r=Cs_r(k)
# endif
             do i=i1,i2
               z_w0=cff_w+cff1_w*h(i,j) 
               z_r0=cff_r+cff1_r*h(i,j)
# ifdef NEW_S_COORD                              
               z_w(i,j,k)=z_w0*h(i,j)*hinv(i,j)+Zt_avg1(i,j)
     &                                          *(1.+z_w0*hinv(i,j)) 
               z_r(i,j,k)=z_r0*h(i,j)*hinv(i,j)+Zt_avg1(i,j)
     &                                          *(1.+z_r0*hinv(i,j))    
# else
               z_w(i,j,k)=z_w0+Zt_avg1(i,j)*(1.+z_w0*hinv(i,j))      
               z_r(i,j,k)=z_r0+Zt_avg1(i,j)*(1.+z_r0*hinv(i,j)) 
# endif
               Hz_bak(i,j,k)=Hz(i,j,k)
               Hz(i,j,k)=z_w(i,j,k)-z_w(i,j,k-1)
             enddo
           enddo
         enddo

       ENDIF
      
      return
      end
!====================================================================
!                   subroutine Updatermask
!====================================================================
!
#ifdef MASKING
      Subroutine Updatermask(tabres,i1,i2,j1,j2,before)
      implicit none
#  include "param.h"
#  include "grid.h"
#  include "ocean2d.h"
#  include "ocean3d.h"
#  include "coupling.h"
#  include "scalars.h"
#  include "zoom.h"
      integer i1,i2,j1,j2
      real tabres(i1:i2,j1:j2)
      logical before
      
      integer i,j,k
              
      IF (before) THEN
        tabres(i1:i2,j1:j2) = rmask(i1:i2,j1:j2)
       ELSE
        rmask(i1:i2,j1:j2)=tabres(i1:i2,j1:j2)
       ENDIF
      
      return
      end
#endif /*  MASKING */
!
!====================================================================
!                   subroutine UpdateGridhis
!====================================================================
!
      recursive Subroutine UpdateGridhis()
      implicit none
# include "param.h"
# include "grid.h"
# include "scalars.h"
# include "ncscrum.h"

# ifdef XIOS
!RB to be checked
!      call iom_swap( "roms" )
!      call send_xios_diags(0)
# else
      call wrt_his
# endif
      if (.Not.Agrif_Root()) Then
        Call Agrif_ChildGrid_To_ParentGrid()
        Call UpdateGridhis()
        Call Agrif_ParentGrid_To_ChildGrid()
      endif
      
      end subroutine UpdateGridhis


      subroutine hinterp(tabres,i1,i2,j1,j2,before,nb,ndir)
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "scalars.h"
# ifdef MPI
#  define LOCALLM Lmmpi
#  define LOCALMM Mmmpi
# else
#  define LOCALLM Lm
#  define LOCALMM Mm
# endif 
!$AGRIF_DO_NOT_TREAT
       integer isens
       common/interph/isens
!$AGRIF_END_DO_NOT_TREAT

       integer i1,i2,j1,j2
       logical before
       real tabres(i1:i2,j1:j2)
       integer i,j
       integer nb,ndir
       logical :: western_side, eastern_side
       logical :: northern_side,southern_side
       
       if (before) then
           IF (isens == 1) THEN
           do j=j1,j2
           do i=max(i1,lbound(h,1)+1),i2
             tabres(i,j) = 0.5*(h(i-1,j)+h(i,j))
           enddo
           enddo       
           ELSE
           do j=max(j1,lbound(h,2)+1),j2
           do i=i1,i2
             tabres(i,j) = 0.5*(h(i,j-1)+h(i,j))
           enddo
           enddo       
           ENDIF  
       else
       
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1)
         northern_side = (nb == 2).AND.(ndir == 2)

         if (isens == 1) then
           if ( western_side ) then    
              Do j=j1,j2
                 h(0,j) = 2.*tabres(1,j)-h(1,j)
# ifdef NEW_S_COORD
                 hinv(0,j) = 1./(h(0,j)+hc)
# else
                 hinv(0,j) = 1./h(0,j)
# endif
              enddo
           endif
           
           if ( eastern_side ) then 
              Do j=j1,j2
                 h(LOCALLM+1,j) = 2.*tabres(LOCALLM+1,j)-h(LOCALLM,j)
# ifdef NEW_S_COORD
                 hinv(LOCALLM+1,j) = 1./(h(LOCALLM+1,j)+hc)
# else
                 hinv(LOCALLM+1,j) = 1./h(LOCALLM+1,j)
# endif
              enddo
           endif
         elseif (isens == 2) then
           if ( southern_side ) then      
             Do i=i1,i2
               h(i,0) = 2.*tabres(i,1)-h(i,1)
# ifdef NEW_S_COORD
               hinv(i,0) = 1./(h(i,0)+hc)
# else
               hinv(i,0) = 1./h(i,0)
# endif
             enddo
           endif
           
           if ( northern_side ) then
             Do i=i1,i2
               h(i,LOCALMM+1) = 2.*tabres(i,LOCALMM+1)-h(i,LOCALMM)
# ifdef NEW_S_COORD
               hinv(i,LOCALMM+1) = 1./(h(i,LOCALMM+1)+hc)
# else
               hinv(i,LOCALMM+1) = 1./h(i,LOCALMM+1)
# endif
             enddo
           endif
         endif ! <- endif isens

        endif  ! <- endif before
      return
      end      
      subroutine hinterp_old(tabres,i1,i2,j1,j2)
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "scalars.h"
!$AGRIF_DO_NOT_TREAT
       integer isens
       common/interph/isens
!$AGRIF_END_DO_NOT_TREAT

       integer i1,i2,j1,j2
       real tabres(i1:i2,j1:j2)
       integer i,j
       
       IF (isens == 1) THEN
       do j=j1,j2
       do i=max(i1,lbound(h,1)+1),i2
         tabres(i,j) = 0.5*(h(i-1,j)+h(i,j))
       enddo
       enddo       
       ELSE
       do j=max(j1,lbound(h,2)+1),j2
       do i=i1,i2
         tabres(i,j) = 0.5*(h(i,j-1)+h(i,j))
       enddo
       enddo       
       ENDIF  
       
      return
      end      

# ifdef WKB_WWAVE
      subroutine initwac(tabres,i1,i2,j1,j2,before)
      implicit none
# include "wkb_wwave.h"
       integer i1,i2,j1,j2
       logical before
       real tabres(i1:i2,j1:j2)
       integer i,j

       if (before) then
       do j=j1,j2
       do i=i1,i2
         tabres(i,j) = wac(i,j,2)
       enddo
       enddo
       else
       wac(i1:i2,j1:j2,1)=tabres
       wac(i1:i2,j1:j2,2)=tabres
       endif
      return
      end


      subroutine inithrm(tabres,i1,i2,j1,j2,before)
      implicit none
# include "wkb_wwave.h"
       integer i1,i2,j1,j2
       logical before
       real tabres(i1:i2,j1:j2)
       integer i,j

       if (before) then
       do j=j1,j2
       do i=i1,i2
         tabres(i,j) = hrm(i,j,2)
       enddo
       enddo
       else
       hrm(i1:i2,j1:j2,1)=tabres
       hrm(i1:i2,j1:j2,2)=tabres
       endif
      return
      end
      
      subroutine initfrq(tabres,i1,i2,j1,j2,before)
      implicit none
# include "wkb_wwave.h"
       integer i1,i2,j1,j2
       logical before
       real tabres(i1:i2,j1:j2)
       integer i,j

       if (before) then
       do j=j1,j2
       do i=i1,i2
         tabres(i,j) = frq(i,j,2)
       enddo
       enddo
       else
       frq(i1:i2,j1:j2,1)=tabres
       frq(i1:i2,j1:j2,2)=tabres
       endif
      return
      end
      subroutine initwsb(tabres,i1,i2,j1,j2,before)
      implicit none
# include "wkb_wwave.h"
       integer i1,i2,j1,j2
       logical before
       real tabres(i1:i2,j1:j2)
       integer i,j

       if (before) then
       do j=j1,j2
       do i=i1,i2
         tabres(i,j) = wsb(i,j,2)
       enddo
       enddo
       else
       wsb(i1:i2,j1:j2,1)=tabres
       wsb(i1:i2,j1:j2,2)=tabres
       endif
      return
      end
      
       subroutine initwvn(tabres,i1,i2,j1,j2,before)
      implicit none
# include "wkb_wwave.h"
       integer i1,i2,j1,j2
       logical before
       real tabres(i1:i2,j1:j2)
       integer i,j

       if (before) then
       do j=j1,j2
       do i=i1,i2
         tabres(i,j) = wvn(i,j,2)
       enddo
       enddo
       else
       wvn(i1:i2,j1:j2,1)=tabres
       wvn(i1:i2,j1:j2,2)=tabres
       endif
      return
      end
     
      subroutine initwcg(tabres,i1,i2,j1,j2,before)
      implicit none
# include "wkb_wwave.h"
       integer i1,i2,j1,j2
       logical before
       real tabres(i1:i2,j1:j2)
       integer i,j

       if (before) then
       do j=j1,j2
       do i=i1,i2
         tabres(i,j) = wcg(i,j,2)
       enddo
       enddo
       else
       wcg(i1:i2,j1:j2,1)=tabres
       wcg(i1:i2,j1:j2,2)=tabres
       endif
      return
      end

      
#  ifdef WAVE_ROLLER
      subroutine initwar(tabres,i1,i2,j1,j2,before)
      implicit none
# include "wkb_wwave.h"
       integer i1,i2,j1,j2
       logical before
       real tabres(i1:i2,j1:j2)
       integer i,j

       if (before) then
       do j=j1,j2
       do i=i1,i2
         tabres(i,j) = war(i,j,2)
       enddo
       enddo
       else
       war(i1:i2,j1:j2,1)=tabres
       war(i1:i2,j1:j2,2)=tabres
       endif
      return
      end
#  endif
      subroutine initwkx(tabres,i1,i2,j1,j2,before)
      implicit none
# include "wkb_wwave.h"
       integer i1,i2,j1,j2
       logical before
       real tabres(i1:i2,j1:j2)
       integer i,j

       if (before) then
       do j=j1,j2
       do i=i1,i2
         tabres(i,j) = wkx(i,j,2)
       enddo
       enddo
       else
       wkx(i1:i2,j1:j2,1)=tabres
       wkx(i1:i2,j1:j2,2)=tabres
       endif
      return
      end
      subroutine initwke(tabres,i1,i2,j1,j2,before)
      implicit none
# include "wkb_wwave.h"
       integer i1,i2,j1,j2
       logical before
       real tabres(i1:i2,j1:j2)
       integer i,j

       if (before) then
       do j=j1,j2
       do i=i1,i2
         tabres(i,j) = wke(i,j,2)
       enddo
       enddo
       else
       wke(i1:i2,j1:j2,1)=tabres
       wke(i1:i2,j1:j2,2)=tabres       
       endif
      return
      end
# endif
!
!====================================================================
!                   subroutine Agrif_transfer_floatsp2c
!====================================================================
!
# if defined FLOATS || defined STATIONS
        subroutine Agrif_transfer_floatsp2c(rank,xfloat,yfloat,delta)
        use Agrif_Util
!----------------------------------------------------------------------
! checks if a float must be transferred from one grid to the child grid
! margin is the sum of the distance between rho and velocity points (0.5)
! plus a safe value to ensure that the float can stay inside the child
! at least coeffreft time steps.
!----------------------------------------------------------------------
        type(Agrif_grid), Pointer :: childgrid
        type(Agrif_pgrid), Pointer :: parcours

        real xfloat,yfloat,margin,delta,tmp1,tmp2
        integer rank,timeindex

        parcours=>Agrif_Curgrid%child_list%first
        do while (associated(parcours))
          childgrid=>parcours%gr
          margin=0.5+delta
          tmp1=childgrid%spaceref(1)
     &         *(xfloat+0.5-childgrid%ix(1))+0.5
          if ((tmp1 .ge. margin).and.
     &        (tmp1 .le. (childgrid%nb(1)+1-margin))) then
            tmp2=childgrid%spaceref(2)
     &           *(yfloat+0.5-childgrid%ix(2))+0.5
            if ((tmp2 .ge. margin).and.
     &          (tmp2 .le. (childgrid%nb(2)+1-margin))) then
              rank=childgrid%fixedrank
              xfloat=tmp1
              yfloat=tmp2
              exit
            endif
          endif
          parcours=>parcours%next
        enddo
        end subroutine Agrif_transfer_floatsp2c
!
!====================================================================
!                   subroutine Agrif_laststep
!====================================================================
!
        subroutine Agrif_laststep(logic)
        use Agrif_Util

!---------------------------------------------------------
! determines whether this time step is the last one before
! a ROOT time step.
!---------------------------------------------------------
        Implicit none
        logical logic
        type(Agrif_grid), Pointer :: parcours
        integer tmp, tmp2,rhot,stepmod

        logic=.false.
        tmp=0
        tmp2=1
        parcours=>Agrif_Curgrid

        Do While (associated(parcours%parent))
          rhot=1
          rhot=max(rhot,parcours%timeref(1),parcours%timeref(2))
          stepmod=mod(parcours%ngridstep,int(rhot))
          tmp2=tmp2*rhot
          tmp=stepmod+rhot*tmp
          parcours=>parcours%parent
        End Do
        tmp2=tmp2-1
        if (tmp2 .eq. tmp) logic=.true.
        return
        end subroutine

# endif /* FLOATS || STATIONS */
!
!====================================================================
!                   subroutine Agrif_Invloc
!====================================================================
!
# if defined MPI
        Subroutine Agrif_Invloc(indloc,proc,dir,indglob)
        implicit none
        integer indloc, proc, dir, indglob
# include "param.h"        
        If (dir == 1) Then
           indglob = indloc + iminmpi-1          
        Else If (dir == 2) Then
           indglob = indloc + jminmpi-1
        Else 
           indglob = indloc
        End If
        Return
        End

        subroutine Agrif_get_proc_info ( oimin, oimax, ojmin, ojmax )
        implicit none
        integer, intent(out) :: oimin, oimax
        integer, intent(out) :: ojmin, ojmax
# include "param.h"        
        oimin = iminmpi
        oimax = imaxmpi
        ojmin = jminmpi
        ojmax = jmaxmpi
        end subroutine Agrif_get_proc_info
# endif
!
        subroutine Agrif_estimate_parallel_cost(i1,i2,j1,j2,nbprocs,grid_cost)
        implicit none
        integer, intent(in)  :: i1, i2, j1, j2, nbprocs
        real,    intent(out) :: grid_cost

!        grid_cost = SIZE((/i1:i2, j1:j2/)) / real(nbprocs)     ! FIXME: Does not work with conv
        grid_cost = (i2-i1+1)*(j2-j1+1) / real(nbprocs)

        end subroutine agrif_estimate_parallel_cost

      SUBROUTINE Agrif_detect(taberr,sizexy)
!
!     Modules used:
!
      use AGRIF_types
      implicit none
      integer tile, ierr, trd, subs
# include "param.h"
# include "private_scratch.h"
# include "ncscrum.h"
# include "scalars.h"
# include "grid.h"
# include "ocean2d.h"
# include "zoom.h"
#ifdef SOLVE3D
# include "coupling.h"
# include "ocean3d.h"
#endif
!
!     Declarations:
!
!     Variables
!
      Integer, Dimension(2) :: sizexy
      Integer,Dimension(sizexy(1),sizexy(2))   :: taberr  ! Pointer on the current grid
!
!     Begin
!
       real vort(GLOBAL_2D_ARRAY)

      integer i,j
      real crit

      vort = 0.
      do j=1,Mm+1
      do i=1,Lm+1
        vort(i,j) = (v(i,j,N,nnew)-v(i-1,j,N,nnew))
     &     -(u(i,j,N,nnew)-u(i,j-1,N,nnew))
      enddo
      enddo
!      
      crit = maxval(abs(vort))
      taberr=0.
      do j=1,Mm+1
      do i=1,Lm+1
          if (abs(vort(i,j)) > 0.8*crit) then
            taberr(i,j) = 1
          endif
      enddo
      enddo
      
      Return
      End Subroutine Agrif_detect
!
!====================================================================
!                   subroutine Agrif_Before_Regridding
!====================================================================
!
      Subroutine Agrif_Before_Regridding()
# include "param.h"
# include "private_scratch.h"
# include "ncscrum.h"
# include "scalars.h"
# include "grid.h"
# include "ocean2d.h"
# include "zoom.h"   
# ifdef SOLVE3D
#  include "coupling.h"
#  include "ocean3d.h"
# endif
   
      real tabtemp2d(GLOBAL_2D_ARRAY)
      real tabtemp3d(GLOBAL_2D_ARRAY,N)
      real text(GLOBAL_2D_ARRAY,N,NT)
      integer itrc
  
      tabtemp2d = Zt_avg1
      
      Call Agrif_Save_ForRestore(tabtemp2d,zetaid)
      tabtemp2d = Agrif_irhoy()*DU_avg1(:,:,nnew)
      Call Agrif_Save_ForRestore(tabtemp2d,ubarid)
      tabtemp2d = Agrif_irhox()*DV_avg1(:,:,nnew)      
      Call Agrif_Save_ForRestore(tabtemp2d,vbarid)
      
      tabtemp3d = u(:,:,:,nnew)
      Call Agrif_Save_ForRestore(tabtemp3d,uid)
      tabtemp3d = v(:,:,:,nnew)
      Call Agrif_Save_ForRestore(tabtemp3d,vid)

      do itrc=1,NT
            text(:,:,:,itrc) = t(:,:,:,nnew,itrc)
      enddo
      Call Agrif_Save_ForRestore(text,tid)        
                    
      Return
      End Subroutine Agrif_Before_Regridding
      subroutine inith(tabres,i1,i2,j1,j2,before)
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "scalars.h"
# include "zoom.h"
# include "ocean3d.h"
# include "coupling.h"

       integer i1,i2,j1,j2
       real tabres(i1:i2,j1:j2)
       logical before
        write(*,*) i1,i2
       if (before) then
         tabres = h(i1:i2,j1:j2)
       else
         h(i1:i2,j1:j2) = tabres
       endif

      return
      end


      subroutine initzeta(tabres,i1,i2,j1,j2,before)
      implicit none
# include "param.h"
# include "ocean2d.h"
# include "scalars.h"
# include "zoom.h"
# include "ocean3d.h"
# include "coupling.h"

       integer i1,i2,j1,j2
       real tabres(i1:i2,j1:j2)
       logical before

       if (before) then
         tabres = Zt_avg1(i1:i2,j1:j2)
       else
         zeta(i1:i2,j1:j2,1) = tabres
       endif
             
      return
      end
      subroutine initzeta_old(tabres,i1,i2,j1,j2)
      implicit none
# include "param.h"
# include "ocean2d.h"
# include "scalars.h"
# include "zoom.h"
# include "ocean3d.h"
# include "coupling.h"

       integer i1,i2,j1,j2
       real tabres(i1:i2,j1:j2)

       tabres = Zt_avg1(i1:i2,j1:j2)
             
      return
      end
      
      subroutine initubar(tabres,i1,i2,j1,j2,before)
      implicit none
# include "param.h"
# include "ocean2d.h"
# include "scalars.h"
# include "zoom.h"
# include "ocean3d.h"
# include "coupling.h"

       integer i1,i2,j1,j2
       real tabres(i1:i2,j1:j2)
       logical before

       if (before) then
         tabres = DU_avg1(i1:i2,j1:j2,nnew)
       else
         DU_avg1(i1:i2,j1:j2,1) = tabres / Agrif_irhoy()
       endif
             
      return
      end 
      subroutine initubar_old(tabres,i1,i2,j1,j2)
      implicit none
# include "param.h"
# include "ocean2d.h"
# include "scalars.h"
# include "zoom.h"
# include "ocean3d.h"
# include "coupling.h"

       integer i1,i2,j1,j2
       real tabres(i1:i2,j1:j2)
       
       tabres = DU_avg1(i1:i2,j1:j2,nnew)
             
      return
      end 
      
      subroutine initvbar(tabres,i1,i2,j1,j2,before)
      implicit none
# include "param.h"
# include "ocean2d.h"
# include "scalars.h"
# include "zoom.h"
# include "ocean3d.h"
# include "coupling.h"

       integer i1,i2,j1,j2
       real tabres(i1:i2,j1:j2)
       logical before
       
       if (before) then
         tabres = DV_avg1(i1:i2,j1:j2,nnew)
       else
         DV_avg1(i1:i2,j1:j2,1) = tabres / Agrif_irhox()
       endif
             
      return
      end            
      subroutine initvbar_old(tabres,i1,i2,j1,j2)
      implicit none
# include "param.h"
# include "ocean2d.h"
# include "scalars.h"
# include "zoom.h"
# include "ocean3d.h"
# include "coupling.h"

       integer i1,i2,j1,j2
       real tabres(i1:i2,j1:j2)
       
       tabres = DV_avg1(i1:i2,j1:j2,nnew)
             
      return
      end            
      
      subroutine initu(tabres,i1,i2,j1,j2,k1,k2,before)
      implicit none
# include "param.h"
# include "ocean3d.h"
# include "scalars.h"
# include "zoom.h"

       integer i1,i2,j1,j2,k1,k2
       real tabres(i1:i2,j1:j2,k1:k2)
       logical before
       
       if (before) then
         tabres = u(i1:i2,j1:j2,k1:k2,nnew)
       else
         u(i1:i2,j1:j2,k1:k2,1) = tabres
       endif      
      return
      end 
      subroutine initu_old(tabres,i1,i2,j1,j2,k1,k2)
      implicit none
# include "param.h"
# include "ocean3d.h"
# include "scalars.h"
# include "zoom.h"

       integer i1,i2,j1,j2,k1,k2
       real tabres(i1:i2,j1:j2,k1:k2)
       
       tabres = u(i1:i2,j1:j2,k1:k2,nnew)
             
      return
      end 
      
      subroutine initv(tabres,i1,i2,j1,j2,k1,k2,before)
      implicit none
# include "param.h"
# include "ocean3d.h"
# include "scalars.h"
# include "zoom.h"

       integer i1,i2,j1,j2,k1,k2
       real tabres(i1:i2,j1:j2,k1:k2)
       logical before
       
       if (before) then
         tabres = v(i1:i2,j1:j2,k1:k2,nnew)
       else
         v(i1:i2,j1:j2,k1:k2,1) = tabres
       endif      
      return
      end
      subroutine initv_old(tabres,i1,i2,j1,j2,k1,k2)
      implicit none
# include "param.h"
# include "ocean3d.h"
# include "scalars.h"
# include "zoom.h"

       integer i1,i2,j1,j2,k1,k2
       real tabres(i1:i2,j1:j2,k1:k2)
       
       tabres = v(i1:i2,j1:j2,k1:k2,nnew)
             
      return
      end
      
      subroutine initt(tabres,i1,i2,j1,j2,k1,k2,m1,m2,before)
      implicit none
# include "param.h"
# include "ocean3d.h"
# include "scalars.h"
# include "zoom.h"

       integer i1,i2,j1,j2,k1,k2,m1,m2
       real tabres(i1:i2,j1:j2,k1:k2,m1:m2)
       logical before
       integer itrc
       
       if (before) then
         tabres = t(i1:i2,j1:j2,k1:k2,nnew,m1:m2)
       else 
         do itrc=m1,m2
             got_tini(itrc) = .true.
             t(i1:i2,j1:j2,k1:k2,1,itrc)=tabres(i1:i2,j1:j2,k1:k2,itrc)
         enddo        
       endif   
      return
      end                   
      subroutine initt_old(tabres,i1,i2,j1,j2,k1,k2,m1,m2)
      implicit none
# include "param.h"
# include "ocean3d.h"
# include "scalars.h"
# include "zoom.h"

       integer i1,i2,j1,j2,k1,k2,m1,m2
       real tabres(i1:i2,j1:j2,k1:k2,m1:m2)
       
       tabres = t(i1:i2,j1:j2,k1:k2,nnew,m1:m2)
             
      return
      end                   
!
!====================================================================
!                   subroutine computenbmaxtimes
!====================================================================
!
      subroutine computenbmaxtimes()
      implicit none
#include "param.h"
#include "scalars.h"
#include "zoom.h"
      integer nunit
      integer j, jp
      integer :: nb
      
      integer :: nbstep2d(0:20)
      
      nunit = Agrif_Get_Unit()
    
      open(nunit,file='AGRIF_FixedGrids.in',form='formatted')
      j=1
      jp = 0
      call agrif_read_fixed(jp,j,nunit)
      close(nunit)
      
      nbstep2d = 0
      
      call write_number(0,nbstep2d)
      
      nb = 0
      do while (sortedint(nb) /= -1)
        nbmaxtimes = nb
        nb = nb + 1
      enddo
      
      return
      end
!
!====================================================================
!                   subroutine write_number
!====================================================================
!
      recursive subroutine write_number(j,nbstep2d)
      implicit none
#include "param.h"
#include "scalars.h"
#include "zoom.h"
      integer :: j
      integer nb
      integer ngrid
      integer :: nbstep2d(0:20)
      
      call write_prestep3d(j)
      
      do nb=1,nfast
       call write_step2d(j,nbstep2d)
      enddo
      
      end subroutine write_number
!
!====================================================================
!                   subroutine write_prestep3d
!====================================================================
!
      recursive subroutine write_prestep3d(j)
      implicit none
#include "param.h"
#include "scalars.h"
#include "zoom.h"
      integer :: j
      integer nb
      integer ngrid
      integer :: nbstep2d(0:20)
      
      iind = iind + 1
      sortedint(iind) = j
      whichstep(iind) = 0
      
      ngrid = 0
      do while (grids_at_level(j,ngrid)/=-1)
      call write_prestep3d(grids_at_level(j,ngrid))
      ngrid = ngrid + 1
      enddo
      
      end subroutine write_prestep3d
!
!====================================================================
!                   subroutine write_step2d
!====================================================================
!
      recursive subroutine write_step2d(j,nbstep2d)
      implicit none
#include "param.h"
#include "scalars.h"
#include "zoom.h"
      integer :: j
      integer nb
      integer ngrid
      integer :: nbstep2d(0:20)

      iind = iind + 1
      sortedint(iind) = j
      whichstep(iind) = 1
      nbstep2d(j) = nbstep2d(j)+1
      
      ngrid = 0
      do while (grids_at_level(j,ngrid)/=-1)
      do nb=1,coeff_ref_time(grids_at_level(j,ngrid))
      call write_step2d(grids_at_level(j,ngrid),nbstep2d)
      enddo
      ngrid = ngrid + 1
      enddo
      
      if (nbstep2d(j) == nfast) then
        if (j /= 0) then
          if (nbstep2d(parent_grid(j)) /= nfast) then
            call write_step3d1(j)
            call write_step3d2(j)
            call write_prestep3d(j)
          endif
        endif
        if (j == 0) then
          call write_step3d1(j)
          call write_step3d2(j)
        endif
        nbstep2d(j) = 0
      endif
      
      end subroutine write_step2d
!
!====================================================================
!                   subroutine write_step3d1
!====================================================================
!
      recursive subroutine write_step3d1(j)
      implicit none
#include "param.h"
#include "scalars.h"
#include "zoom.h"
      integer :: j
      integer nb
      integer ngrid
      integer :: nbstep2d(0:20)
      
      iind = iind + 1
      sortedint(iind) = j
      whichstep(iind) = 2
      
      ngrid = 0
      do while (grids_at_level(j,ngrid)/=-1)
      call write_step3d1(grids_at_level(j,ngrid))
      ngrid = ngrid + 1
      enddo
      
      end subroutine write_step3d1
!
!====================================================================
!                   subroutine write_step3d2
!====================================================================
!
      recursive subroutine write_step3d2(j)
      implicit none
#include "param.h"
#include "scalars.h"
#include "zoom.h"
      integer :: j
      integer nb
      integer ngrid
      integer :: nbstep2d(0:20)
      
      iind = iind + 1
      sortedint(iind) = j
      whichstep(iind) = 3
      
      ngrid = 0
      do while (grids_at_level(j,ngrid)/=-1)
      call write_step3d2(grids_at_level(j,ngrid))
      ngrid = ngrid + 1
      enddo
      
      end subroutine write_step3d2
!
!====================================================================
!                   subroutine agrif_read_fixed
!====================================================================
!
      recursive subroutine agrif_read_fixed(jp,j,nunit)
      implicit none
#include "param.h"
#include "scalars.h"
#include "zoom.h"
      integer :: j,nunit,jp
      integer :: i
      integer :: nb_grids
      integer :: grid_number
      integer :: r_imin,r_imax,r_jmin,r_jmax,r_rhox,r_rhoy,r_time
      integer :: gridnum(1000)
      
      read(nunit,*)nb_grids
      
      do i=1,nb_grids
        grid_number = j
        grids_at_level(jp,i-1)=j
        gridnum(i)=j
        parent_grid(j) = jp
        read(nunit,*)r_imin,r_imax,r_jmin,r_jmax,r_rhox,r_rhoy,r_time
        coeff_ref_time(j)=r_time
        j=j+1
      enddo
      
      do i=1,nb_grids
       call agrif_read_fixed(gridnum(i),j,nunit)
      enddo
      
      return
      end

#else  /* AGRIF */
!
!====================================================================
!                   nothing...
!====================================================================
!
      subroutine zoom_empty
      return
      end
#endif  /* AGRIF */
