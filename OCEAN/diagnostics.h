! $Id: diagnostics.h 1458 2014-02-03 15:01:25Z gcambon $
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
! This is include file "diagnostics.h": tracer equation terms
! for output purposes:
!
!
#ifdef DIAGNOSTICS_TS
      real TXadv(GLOBAL_2D_ARRAY,N,NT)
      real TYadv(GLOBAL_2D_ARRAY,N,NT)
      real TVadv(GLOBAL_2D_ARRAY,N,NT)
      real THmix(GLOBAL_2D_ARRAY,N,NT)
      real TVmix(GLOBAL_2D_ARRAY,N,NT)
      real TForc(GLOBAL_2D_ARRAY,N,NT)
      real Trate(GLOBAL_2D_ARRAY,N,NT)
!
# ifdef DIAGNOSTICS_TS_MLD
      real TXadv_mld(GLOBAL_2D_ARRAY,NT)
      real TYadv_mld(GLOBAL_2D_ARRAY,NT)
      real TVadv_mld(GLOBAL_2D_ARRAY,NT)
      real THmix_mld(GLOBAL_2D_ARRAY,NT)
      real TVmix_mld(GLOBAL_2D_ARRAY,NT)
      real TForc_mld(GLOBAL_2D_ARRAY,NT)
      real Trate_mld(GLOBAL_2D_ARRAY,NT)
      real Tentr_mld(GLOBAL_2D_ARRAY,NT)
      integer kbl_nstp(GLOBAL_2D_ARRAY)
# endif
# ifdef AVERAGES
      real timedia_avg
      real TXadv_avg(GLOBAL_2D_ARRAY,N,NT)
      real TYadv_avg(GLOBAL_2D_ARRAY,N,NT)
      real TVadv_avg(GLOBAL_2D_ARRAY,N,NT)
      real THmix_avg(GLOBAL_2D_ARRAY,N,NT)
      real TVmix_avg(GLOBAL_2D_ARRAY,N,NT)
      real TForc_avg(GLOBAL_2D_ARRAY,N,NT)
      real Trate_avg(GLOBAL_2D_ARRAY,N,NT)
!
#  ifdef DIAGNOSTICS_TS_MLD
      real TXadv_mld_avg(GLOBAL_2D_ARRAY,NT)
      real TYadv_mld_avg(GLOBAL_2D_ARRAY,NT)
      real TVadv_mld_avg(GLOBAL_2D_ARRAY,NT)
      real THmix_mld_avg(GLOBAL_2D_ARRAY,NT)
      real TVmix_mld_avg(GLOBAL_2D_ARRAY,NT)
      real TForc_mld_avg(GLOBAL_2D_ARRAY,NT)
      real Trate_mld_avg(GLOBAL_2D_ARRAY,NT)
      real Tentr_mld_avg(GLOBAL_2D_ARRAY,NT)
#  endif
# endif	
      common /diag_TXadv/TXadv   
     &       /diag_TYadv/TYadv
     &       /diag_TVadv/TVadv  
     &       /diag_THmix/THmix
     &       /diag_TVmix/TVmix
     &       /diag_TForc/TForc
     &       /diag_Trate/Trate
!
# ifdef DIAGNOSTICS_TS_MLD
      common /diag_TXadv_mld/TXadv_mld
     &       /diag_TYadv_mld/TYadv_mld
     &       /diag_TVadv_mld/TVadv_mld
     &       /diag_THmix_mld/THmix_mld
     &       /diag_TVmix_mld/TVmix_mld
     &       /diag_TForc_mld/TForc_mld
     &       /diag_Trate_mld/Trate_mld
     &       /diag_Tentr_mld/Tentr_mld	  
     &       /diag_kbl_nstp/kbl_nstp
# endif
# ifdef AVERAGES
      common /diag_timedia_avg/timedia_avg
      common /diag_TXadv_avg/TXadv_avg  
     &       /diag_TYadv_avg/TYadv_avg
     &       /diag_TVadv_avg/TVadv_avg   
     &       /diag_THmix_avg/THmix_avg
     &       /diag_TVmix_avg/TVmix_avg
     &       /diag_TForc_avg/TForc_avg
     &       /diag_Trate_avg/Trate_avg
!
#  ifdef DIAGNOSTICS_TS_MLD
      common /diag_TXadv_mld_avg/TXadv_mld_avg
     &       /diag_TYadv_mld_avg/TYadv_mld_avg
     &       /diag_TVadv_mld_avg/TVadv_mld_avg
     &       /diag_THmix_mld_avg/THmix_mld_avg
     &       /diag_TVmix_mld_avg/TVmix_mld_avg
     &       /diag_TForc_mld_avg/TForc_mld_avg
     &       /diag_Trate_mld_avg/Trate_mld_avg
     &       /diag_Tentr_mld_avg/Tentr_mld_avg
#  endif       	
# endif       	
#endif /* DIAGNOSTICS_TS */
!
#ifdef DIAGNOSTICS_UV
      real MXadv(GLOBAL_2D_ARRAY,N,2)
      real MYadv(GLOBAL_2D_ARRAY,N,2)
      real MVadv(GLOBAL_2D_ARRAY,N,2)
      real MCor(GLOBAL_2D_ARRAY,N,2)
      real MPrsgrd(GLOBAL_2D_ARRAY,N,2)
      real MHmix(GLOBAL_2D_ARRAY,N,2)
      real MVmix(GLOBAL_2D_ARRAY,N,2)
      real Mrate(GLOBAL_2D_ARRAY,N,2)
# ifdef MRL_WCI
      real Mvf(GLOBAL_2D_ARRAY,N,2)
      real Mbrk(GLOBAL_2D_ARRAY,N,2)
      real MStCo(GLOBAL_2D_ARRAY,N,2)
      real MVvf(GLOBAL_2D_ARRAY,N,2)
      real MPrscrt(GLOBAL_2D_ARRAY,N,2)
      real Msbk(GLOBAL_2D_ARRAY,N,2) 
      real Mbwf(GLOBAL_2D_ARRAY,N,2)
      real Mfrc(GLOBAL_2D_ARRAY,N,2)
# endif      
# ifdef AVERAGES
      real timediaM_avg
      real MXadv_avg(GLOBAL_2D_ARRAY,N,2)
      real MYadv_avg(GLOBAL_2D_ARRAY,N,2)
      real MVadv_avg(GLOBAL_2D_ARRAY,N,2)
      real MCor_avg(GLOBAL_2D_ARRAY,N,2)
      real MPrsgrd_avg(GLOBAL_2D_ARRAY,N,2)
      real MHmix_avg(GLOBAL_2D_ARRAY,N,2)
      real MVmix_avg(GLOBAL_2D_ARRAY,N,2)
      real Mrate_avg(GLOBAL_2D_ARRAY,N,2)
#  ifdef MRL_WCI
      real Mvf_avg(GLOBAL_2D_ARRAY,N,2)
      real Mbrk_avg(GLOBAL_2D_ARRAY,N,2)
      real MStCo_avg(GLOBAL_2D_ARRAY,N,2)
      real MVvf_avg(GLOBAL_2D_ARRAY,N,2)
      real MPrscrt_avg(GLOBAL_2D_ARRAY,N,2)
      real Msbk_avg(GLOBAL_2D_ARRAY,N,2) 
      real Mbwf_avg(GLOBAL_2D_ARRAY,N,2)
      real Mfrc_avg(GLOBAL_2D_ARRAY,N,2)
#  endif      
# endif	
      common /diag_MXadv/MXadv   
     &       /diag_MYadv/MYadv
     &       /diag_MVadv/MVadv  
     &       /diag_MCor/MCor
     &       /diag_MPrsgrd/MPrsgrd
     &       /diag_MHmix/MHmix
     &       /diag_MVmix/MVmix
     &       /diag_Mrate/Mrate
# ifdef MRL_WCI       
     &       /diag_Mvf/Mvf
     &       /diag_Mbrk/Mbrk
     &       /diag_MStCo/MStCo
     &       /diag_MVvf/MVvf
     &       /diag_MPrscrt/MPrscrt
     &       /diag_Msbk/Msbk
     &       /diag_Mbwf/Mbwf
     &       /diag_Mfrc/Mfrc
# endif      
# ifdef AVERAGES
      common /diag_timediaM_avg/timediaM_avg
      common /diag_MXadv_avg/MXadv_avg
     &       /diag_MYadv_avg/MYadv_avg
     &       /diag_MVadv_avg/MVadv_avg
     &       /diag_MCor_avg/MCor_avg
     &       /diag_MPrsgrd_avg/MPrsgrd_avg
     &       /diag_MHmix_avg/MHmix_avg
     &       /diag_MVmix_avg/MVmix_avg
     &       /diag_Mrate_avg/Mrate_avg
#  ifdef MRL_WCI       
     &       /diag_Mvf_avg/Mvf_avg
     &       /diag_Mbrk_avg/Mbrk_avg
     &       /diag_MStCo_avg/MStCo_avg
     &       /diag_MVvf_avg/MVvf_avg
     &       /diag_MPrscrt_avg/MPrscrt_avg
     &       /diag_Msbk_avg/Msbk_avg
     &       /diag_Mbwf_avg/Mbwf_avg
     &       /diag_Mfrc_avg/Mfrc_avg
#  endif      
# endif       	
#endif /* DIAGNOSTICS_UV */
#ifdef DIAGNOSTICS_BIO
# ifdef PISCES 
#  ifdef key_trc_diaadd
      real bioFlux(GLOBAL_2D_ARRAY,N,NumFluxTerms)
      real bioVSink(GLOBAL_2D_ARRAY,NumVSinkTerms)
#  endif
# else
      real bioFlux(GLOBAL_2D_ARRAY,N,NumFluxTerms)
      real bioVSink(GLOBAL_2D_ARRAY,0:N,NumVSinkTerms)
#  if (defined BIO_NChlPZD && defined OXYGEN) || defined BIO_BioEBUS 
      real GasExcFlux(GLOBAL_2D_ARRAY,NumGasExcTerms)
#  endif
# endif
# ifdef AVERAGES
#  ifdef PISCES 
#    ifdef key_trc_diaadd
      real bioFlux_avg(GLOBAL_2D_ARRAY,N,NumFluxTerms)
      real bioVSink_avg(GLOBAL_2D_ARRAY,NumVSinkTerms)
#    endif
#  else
      real bioFlux_avg(GLOBAL_2D_ARRAY,N,NumFluxTerms)
      real bioVSink_avg(GLOBAL_2D_ARRAY,0:N,NumVSinkTerms)
#  if (defined BIO_NChlPZD && defined OXYGEN) || defined BIO_BioEBUS 
      real GasExcFlux_avg(GLOBAL_2D_ARRAY,NumGasExcTerms)
#    endif
#  endif
      real timediabio_avg
# endif
# ifdef PISCES 
#    ifdef key_trc_diaadd
      common /diag_bioFlux/bioFlux
      common /diag_bioVSink/bioVSink
#    endif
# else
      common /diag_bioFlux/bioFlux
     &       /diag_bioVSink/bioVSink
#  if (defined BIO_NChlPZD && defined OXYGEN) || defined BIO_BioEBUS 
     &       /diag_GasFlux/GasExcFlux
#   endif
# endif
# ifdef AVERAGES
#  ifdef PISCES 
#   ifdef key_trc_diaadd
      common /diag_bioFlux_avg/bioFlux_avg
      common /diag_bioVSink_avg/bioVSink_avg
#   endif
#  else
      common /diag_bioFlux_avg/bioFlux_avg
     &       /diag_bioVSink_avg/bioVSink_avg
#  if (defined BIO_NChlPZD && defined OXYGEN) || defined BIO_BioEBUS
     &       /diag_GasFlux_avg/GasExcFlux_avg
#   endif 
#  endif
      common /diag_timediabio_avg/timediabio_avg
# endif
#endif /* DIAGNOSTICS_BIO */

