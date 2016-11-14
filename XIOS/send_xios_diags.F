! $Id: init_sta.F 697 2011-04-11 12:35:17Z gcambon $
!
!======================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
! 
! ROMS_AGRIF website : http://roms.mpl.ird.fr
!======================================================================
!


#include "cppdefs.h"

#ifdef XIOS

      subroutine send_xios_diags (tile)
      implicit none
     
# include "param.h"
# ifdef SOLVE3D
#  include "work.h"
#  include "ncscrum.h"
# endif
      integer tile
# include "compute_tile_bounds.h"

      call send_xios_diags_tile (tile,Istr,Iend,Jstr,Jend)

      return
      end


      subroutine send_xios_diags_tile (tile,Istr,Iend,Jstr,Jend)

!
!================================================== John M. Klinck ===
!  Copyright (c) 2000 Rutgers/UCLA                                   !
!================================================ Hernan G. Arango ===
!                                                                    !
!  This routine sends every diagnostics fields to Xios server                                              !
!                                                                    !
!=====================================================================
!

          USE xios     ! XIOS io
#if defined BIOLOGY && defined PISCES
          USE trcwri_pisces
#endif

      implicit none
# include "param.h"
# include "ncscrum.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "forces.h"
# include "mixing.h"
# ifdef SEDIMENT
#  include "sediment.h"
# endif
# ifdef BBL
#  include "bbl.h"
# endif
# ifdef WKB_WWAVE
#  include "wkb_wwave.h"
# endif
# ifdef MASKING
#  define SWITCH *
# else
#  define SWITCH !
# endif
# ifdef NBQ
# include "nbq.h"
# endif
# include "scalars.h"
# include "work.h"
# include "private_scratch.h"
!
      integer tile
      integer Istr,Iend,Jstr,Jend
      integer i,j,k
      integer ilc
      character*20 nametrc
      real eps
      parameter (eps=1.D-20)
      real stf_cff
      real zz(GLOBAL_2D_ARRAY,0:N)
      parameter(stf_cff=86400/0.01)
!
# include "compute_auxiliary_bounds.h"
!
#  define I_EXT_RANGE Istr-1,Iend+1
#  define J_EXT_RANGE Jstr-1,Jend+1


!       ilc=iic   ! number of time step since start
       ilc=1+iic-ntstart   ! number of time step since restart
       CALL xios_update_calendar(ilc)

!      call xios_send_field("scrum_time",time)
!      call xios_send_field("time",time)

! static attributes
# ifdef SOLVE3D
!
! Variable dimension on vertical : s_rho and s_w
!
! done automatically
!
! S-coordinate independent variables "sc_w", "sc_r" and stretching
! curves "Cs_w", "Cs_r" at W- and RHO-points.
!
!
! Ugly fix to write scalars until XIOS 2.0
!
      do k=1,N
      zz(:,:,k)=Cs_r(k)
      end do
      call xios_send_field("Cs_r",zz(:,:,1:N) )
      do k=0,N
      zz(:,:,k)=Cs_w(k)
      end do
      call xios_send_field("Cs_w",zz)
      do k=1,N
      zz(:,:,k)=sc_r(k)
      end do
      call xios_send_field("sc_r",zz(:,:,1:N) )
      do k=0,N
      zz(:,:,k)=sc_w(k)
      end do
      call xios_send_field("sc_w",zz)
      zz(:,:,1)=hc
      call xios_send_field("hc",zz(:,:,1))
      zz(:,:,1)=theta_s
      call xios_send_field("theta_s",zz(:,:,1))
      zz(:,:,1)=theta_b
      call xios_send_field("theta_b",zz(:,:,1))
      zz(:,:,1)=Tcline
      call xios_send_field("Tcline",zz(:,:,1))
      zz(:,:,1)=REAL(Vtransform)
      call xios_send_field("Vtransform",zz(:,:,1))
# endif
!
! Bathymetry.
!
      call xios_send_field("h",h)
!
! Coriolis parameter.
!
      call xios_send_field("f",f)
!
! Curvilinear transformation metrics.
!
      call xios_send_field("pm",pm)
      call xios_send_field("pn",pn)
!
! Longitude/latitude or cartezian coordinates of RHO-points
!
# ifdef SPHERICAL
      call xios_send_field("lon_rho",lonr)
      call xios_send_field("lat_rho",latr)
      call xios_send_field("lon_u",lonu)
      call xios_send_field("lat_u",latu)
      call xios_send_field("lon_v",lonv)
      call xios_send_field("lat_v",latv)
# else  
      call xios_send_field("x_rho",xr)
      call xios_send_field("y_rho",yr)
# endif
# ifdef CURVGRID
      call xios_send_field("angle",angler)
# endif
# ifdef MASKING
      call xios_send_field("mask_rho",rmask)
# endif

!
! Critical Depth for Drying cells
!
# ifdef WET_DRY
      call xios_send_field("Dcrit",Dcrit)
# endif

# ifdef NBQ     
      work2d=zeta(:,:,fast_indx_out)/rhobar_nbq(:,:) - h(:,:)
# else
      work2d=zeta(:,:,fast_indx_out)
# endif
      call xios_send_field("zeta",work2d)
      call xios_send_field("ubar",ubar(:,:,fast_indx_out))
      call xios_send_field("vbar",vbar(:,:,fast_indx_out))

# ifdef MOVING_BATHY
      call xios_send_field("hmorph",h)
# endif
# ifdef WET_DRY
      call xios_send_field("rmask_wet",rmask_wet)
      call xios_send_field("umask_wet",umask_wet)
      call xios_send_field("vmask_wet",vmask_wet)
#endif

      do j=0,Mm
         do i=0,Lm
            work2d(i,j)=0.5*sqrt((bustr(i,j)+bustr(i+1,j))**2
     &                           +(bvstr(i,j)+bvstr(i,j+1))**2)
     &                                                    *rho0
         enddo
      enddo
      call xios_send_field("bustr",bustr*rho0)
      call xios_send_field("bvstr",bvstr*rho0)
      call xios_send_field("bostr",work2d)

      do j=1,Mm
         do i=1,Lm
            work2d2(i,j)=0.5*sqrt((sustr(i,j)+sustr(i+1,j))**2
     &                           +(svstr(i,j)+svstr(i,j+1))**2)
     &                                                    *rho0
         enddo
      enddo
      call xios_send_field("wstr",work2d)

      work2d=sustr*rho0
      call xios_send_field("sustr",work2d)
      work2d=svstr*rho0 
      call xios_send_field("svstr",work2d)

# ifdef WKB_WWAVE
      call xios_send_field("hrm",hrm(:,:,wstp))
      call xios_send_field("frq",frq(:,:,wstp))
      call xios_send_field("wac",wac(:,:,wstp))
      call xios_send_field("wkx",wkx(:,:,wstp))
      call xios_send_field("wke",wke(:,:,wstp))
      call xios_send_field("epb",wdsp)
      call xios_send_field("epd",wdrg)
#  ifdef WAVE_ROLLER
      call xios_send_field("war",war(:,:,wstp))
      call xios_send_field("epr",rdsp)
#  endif
# endif

# ifdef MRL_WCI
      call xios_send_field("sup",sup)
      call xios_send_field("ust2d",ust2d)
      call xios_send_field("vst2d",vst2d)
#  ifdef SOLVE3D
      call xios_send_field("ust",ust)
      call xios_send_field("vst",vst)
      call xios_send_field("wst",wst)
      call xios_send_field("Akb",Akb)
      call xios_send_field("Akw",Akw)
      call xios_send_field("kvf",kvf)
      call xios_send_field("calP",calP)
      call xios_send_field("Kapsrf",Kapsrf)
#  endif
# endif

! ------ 3D variables
! nstp vs nnew : present choice follows set_avg.F
# ifdef SOLVE3D
      call xios_send_field("u",u(:,:,:,nstp))
      call xios_send_field("v",v(:,:,:,nstp)) 

      call xios_send_field("temp",t(:,:,:,nstp,itemp)) 
#  ifdef SALINITY
      call xios_send_field("salt",t(:,:,:,nstp,isalt))
#  endif /* SALINITY */
! WARNING here passive tracers are missing
! we could easily add them in using all the vname stuff
! but I found it quiet ugly

      call xios_send_field("rho",rho)

      do k=0,N
         do j=0,Mm+1
            do i=0,Lm+1
               work(i,j,k)= ( We(i,j,k)
#  ifdef VADV_ADAPT_IMP
     &                    +   Wi(i,j,k)
#  endif                  
     &                           ) *pm(i,j)*pn(i,j)
#  ifdef NBQ
     &                         /rho_nbq_avg1(i,j,k)
#  endif
            enddo
         enddo
      enddo
      call xios_send_field("omega",work)

#  ifdef NBQ
      do k=1,N
         do j=0,Mm+1
            do i=0,Lm+1
               workr(i,j,k)=0.5*(wz(i,j,k,nstp)+wz(i,j,k-1,nstp))
            enddo
         enddo
      enddo
#  else
      call Wvlcty (tile, workr) 
#  endif
      call xios_send_field("w",workr)
        
#  ifdef VIS_COEF_3D
      call xios_send_field("visc3d",visc3d_r)
#  endif

#  ifdef DIF_COEF_3D
      do k=1,N
         do j=1,Mm
            do i=1,Lm
               workr(i,j,k)=
#   ifdef TS_DIF2
     &              diff2(i,j,itemp)
#    ifdef TS_DIF_SMAGO
     &              +diff3d_r(i,j,k)
#    endif
#   elif defined TS_DIF4
     &              diff4(i,j,itemp)
#    ifdef TS_DIF_SMAGO
     &              +diff3d_r(i,j,k)*om_r(i,j)*on_r(i,j)
#    endif
     &              +0.25*(diff3d_u(i,j,k)+diff3d_u(i+1,j,k)
     &              +diff3d_v(i,j,k)+diff3d_v(i,j+1,k))
#   endif
            enddo
         enddo
      enddo
      call xios_send_field("diff3d",workr)
#  endif

      call xios_send_field("AKv",Akv)
      work=Akt(:,:,:,itemp)
      call xios_send_field("AKt",work)
#  ifdef SALINITY      
      work=Akt(:,:,:,isalt)
      call xios_send_field("AKs",work)
#  endif
#  ifdef GLS_MIXING
      call xios_send_field("AKk",Akk)
      call xios_send_field("AKp",Akp)
#  endif
#  ifdef LMD_SKPP
#   ifdef LMD_SKPP2005
      work2d=hbls(:,:,nstp)
#   else
      work2d=hbl
#   endif
      call xios_send_field("hbl",work2d)
#  endif
#  ifdef LMD_BKPP
      call xios_send_field("hbbl",work2d)
#  endif
#  ifdef GLS_MIXING
      call xios_send_field("tke",tke(:,:,:,nstp))
      call xios_send_field("gls",gls(:,:,:,nstp))
      call xios_send_field("Lscale",Lscale)
#  endif

      work2d=stflx(:,:,itemp)*rho0*Cp
     &       SWITCH rmask
      call xios_send_field("shflx",work2d)
#  ifdef SALINITY    
         do j=0,Mm+1
            do i=0,Lm+1
               work2d(i,j)=stf_cff*stflx(i,j,isalt)/
     &              ( max(eps,t(i,j,N,nstp,isalt))) 
     &              SWITCH rmask(i,j)
            enddo
        enddo
      call xios_send_field("swflx",work2d)
#  endif

#  ifdef BULK_FLUX
      work2d=shflx_rsw*rho0*Cp
      call xios_send_field("radsw",work2d)
#  else 
      work2d=srflx*rho0*Cp
      call xios_send_field("swrad",work2d)
#  endif
      
#  ifdef BULK_FLUX
      work2d=shflx_rlw*rho0*Cp
      call xios_send_field("shflx_rlw",work2d)

      work2d=shflx_lat*rho0*Cp
      call xios_send_field("shflx_lat",work2d)

      work2d=shflx_sen*rho0*Cp
      call xios_send_field("shflx_sen",work2d)
#  endif
#  ifdef SST_SKIN
      call xios_send_field("sst_skin",sst_skin)
#  endif
     

#  if defined BIOLOGY
#   if defined PISCES
      CALL trc_wri_pisces
#   else
      call xios_send_field("hel",hel)     
#   if (defined BIO_NChlPZD ||  defined BIO_N2ChlPZD2)
      call xios_send_field("theta",theta)
#    ifdef OXYGEN
      call xios_send_field("U10",u10)
      call xios_send_field("KvO2",Kv_O2)
      call xios_send_field("O2sat",O2satu)
#    endif /* OXYGEN */
#   elif defined BIO_BioEBUS 
      call xios_send_field("AOU",AOU)
      call xios_send_field("wind10",wind10)
#   endif
#   endif
#  endif /* BIOLOGY */


#  ifdef SEDIMENT
      do i=1,NGRAV
         write(nametrc,'(A,I1)') 'gravel_',i
         call xios_send_field(TRIM(nametrc),
     &    t(:,:,:,nstp,1+ntrc_salt+ntrc_pas+ntrc_bio+i))
      enddo      
      do i=1,NSAND
         write(nametrc,'(A,I1)') 'sand_',i
         call xios_send_field(TRIM(nametrc),
     &    t(:,:,:,nstp,1+ntrc_salt+ntrc_pas+ntrc_bio+ngrav+i)) 
      enddo      
      do i=1,NMUD
         write(nametrc,'(A,I1)') 'mud_',i
         call xios_send_field(TRIM(nametrc),
     &    t(:,:,:,nstp,1+ntrc_salt+ntrc_pas+ntrc_bio+ngrav+nsand+i)) 
      enddo      

      call xios_send_field("bed_thick",bed_thick) 
      call xios_send_field("bed_poros",bed_poros) 

      do i=1,NGRAV
         write(nametrc,'(A,I1)') 'bed_frac_grav_',i
         call xios_send_field(TRIM(nametrc),
     &    bed_frac(:,:,:,i))
         write(nametrc,'(A,I1)') 'bed_mass_grav_',i
         call xios_send_field(TRIM(nametrc),
     &    bed_mass(:,:,:,nstp,i))
      enddo      
      do i=1,NSAND
         write(nametrc,'(A,I1)') 'bed_frac_sand_',i
         call xios_send_field(TRIM(nametrc),
     &    bed_frac(:,:,:,NGRAV+i))
         write(nametrc,'(A,I1)') 'bed_mass_sand_',i
         call xios_send_field(TRIM(nametrc),
     &    bed_mass(:,:,:,nstp,NGRAV+i))
      enddo      
      do i=1,NMUD
         write(nametrc,'(A,I1)') 'bed_frac_mud_',i
         call xios_send_field(TRIM(nametrc),
     &    bed_frac(:,:,:,NGRAV+NSAND+i))
         write(nametrc,'(A,I1)') 'bed_mass_mud_',i
         call xios_send_field(TRIM(nametrc),
     &    bed_mass(:,:,:,nstp,NGRAV+NSAND+i))
      enddo      

#   ifdef SUSPLOAD
      do i=1,NGRAV
         write(nametrc,'(A,I1)') 'dflux_grav_',i
         call xios_send_field(TRIM(nametrc),
     &    settling_flux(:,:,i))
         write(nametrc,'(A,I1)') 'eflux_grav_',i
         call xios_send_field(TRIM(nametrc),
     &    ero_flux(:,:,i))
      enddo      
      do i=1,NSAND
         write(nametrc,'(A,I1)') 'dflux_sand_',i
         call xios_send_field(TRIM(nametrc),
     &    settling_flux(:,:,NGRAV+i))
         write(nametrc,'(A,I1)') 'eflux_sand_',i
         call xios_send_field(TRIM(nametrc),
     &    ero_flux(:,:,NGRAV+i))
      enddo      
      do i=1,NMUD
         write(nametrc,'(A,I1)') 'dflux_mud_',i
         call xios_send_field(TRIM(nametrc),
     &    settling_flux(:,:,NGRAV+NSAND+i))
         write(nametrc,'(A,I1)') 'edflux_mud_',i
         call xios_send_field(TRIM(nametrc),
     &    ero_flux(:,:,NGRAV+NSAND+i))
      enddo      
#   endif

#   ifdef BEDLOAD
      do i=1,NGRAV
         write(nametrc,'(A,I1)') 'bdlu_gravel_',i
         call xios_send_field(TRIM(nametrc),
     &    bedldu(:,:,i))
         write(nametrc,'(A,I1)') 'bdlv_gravel_',i
         call xios_send_field(TRIM(nametrc),
     &    bedldv(:,:,i))
      enddo      
      do i=1,NSAND
         write(nametrc,'(A,I1)') 'bdlu_sand_',i
         call xios_send_field(TRIM(nametrc),
     &    bedldu(:,:,NGRAV+i))
         write(nametrc,'(A,I1)') 'bdlv_sand_',i
         call xios_send_field(TRIM(nametrc),
     &    bedldv(:,:,NGRAV+i))
      enddo      
      do i=1,NMUD
         write(nametrc,'(A,I1)') 'bdlu_mud_',i
         call xios_send_field(TRIM(nametrc),
     &    bedldu(:,:,NGRAV+NSAND+i))
         write(nametrc,'(A,I1)') 'bdlv_mud_',i
         call xios_send_field(TRIM(nametrc),
     &    bedldv(:,:,NGRAV+NSAND+i))
      enddo      
#   endif
#  endif /* SEDIMENT */
     
#  ifdef BBL
         call xios_send_field("Abed",Abed) 
         call xios_send_field("Hripple",Hripple)
         call xios_send_field("Lripple",Lripple) 
         call xios_send_field("Zbnot",Zbnot)
         call xios_send_field("Zbapp",Zbapp)

         work2d=sqrt(bustrw**2
     &                             +bvstrw**2)
     &                                   *rho0
         call xios_send_field("bostrw",work2d)
         call xios_send_field("bustrc",bustrc*rho0)
         call xios_send_field("bvstrc",bvstrc*rho0)
         call xios_send_field("bustrw",bustrw*rho0)
         call xios_send_field("bvstrw",bvstrw*rho0)
         call xios_send_field("bustrcwmax",bustrcwmax*rho0)
         call xios_send_field("bvstrcwmax",bvstrcwmax*rho0)
!     
#  endif /* BBL */
# endif /* SOLVE3D */


      return
      end

#else   /* XIOS */

      subroutine send_xios_empty
      return
      end

#endif /* XIOS */

