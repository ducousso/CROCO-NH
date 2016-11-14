#ifdef NBQ

      integer ndtnbq
      common /time_nbq1/ ndtnbq
      real dtnbq
      common /time_nbq2/ dtnbq 
      real csound_nbq
      common /nbq_csound/ csound_nbq
      real visc2_nbq
      common /nbq_visc2/ visc2_nbq

      real rhobar_nbq(GLOBAL_2D_ARRAY,4)
      common /nbq_rhobar/ rhobar_nbq
      real rhobar_nbq_avg1(GLOBAL_2D_ARRAY)
      common /nbq_rhobar_AVG1/ rhobar_nbq_avg1

      real rhobar_nbq_int(GLOBAL_2D_ARRAY)
      common /nbq_rhobar_int/ rhobar_nbq_int

      real drhobardt(GLOBAL_2D_ARRAY)
      common /nbq_drhobardt/ drhobardt

      real rhobar_bak1(GLOBAL_2D_ARRAY)
      common /nbq_rhobar_bak1/ rhobar_bak1

      real rhobar_bak2(GLOBAL_2D_ARRAY)
      common /nbq_rhobar_bak2/ rhobar_bak2

      real rubar_nbq(GLOBAL_2D_ARRAY)
      common /nbq_rubar/ rubar_nbq
      real rvbar_nbq(GLOBAL_2D_ARRAY)
      common /nbq_rvbar/ rvbar_nbq

      real rho_nbq_ext(GLOBAL_2D_ARRAY,N)
      real rho_nbq_avg1(GLOBAL_2D_ARRAY,0:N)
      real rho_nbq_avg2(GLOBAL_2D_ARRAY,0:N)
      real rhos_nbq_int(GLOBAL_2D_ARRAY)
      common /nbq_rho_ext/ rho_nbq_ext
      common /avg1_rhonbq/ rho_nbq_avg1
      common /avg2_rhonbq/ rho_nbq_avg2
      common /int_rhonbq/ rhos_nbq_int

      real ru_nbq_ext(GLOBAL_2D_ARRAY,N)
      real ru_nbq_avg1(GLOBAL_2D_ARRAY,N)
      real ru_nbq_avg2(GLOBAL_2D_ARRAY,N)
      common /nbq_ru_ext/ ru_nbq_ext
      common /avg1_runbq/ ru_nbq_avg1
      common /avg2_runbq/ ru_nbq_avg2

      real rv_nbq_ext(GLOBAL_2D_ARRAY,N)
      real rv_nbq_avg1(GLOBAL_2D_ARRAY,N)
      real rv_nbq_avg2(GLOBAL_2D_ARRAY,N)
      common /nbq_rv_ext/ rv_nbq_ext
      common /avg1_rvnbq/ rv_nbq_avg1
      common /avg2_rvnbq/ rv_nbq_avg2

      real rw_nbq_ext(GLOBAL_2D_ARRAY,0:N)
      real rw_nbq_avg1(GLOBAL_2D_ARRAY,0:N)
      real rw_nbq_avg2(GLOBAL_2D_ARRAY,0:N)
      common /nbq_rw_ext/ rw_nbq_ext
      common /avg1_rwnbq/ rw_nbq_avg1
      common /avg2_rwnbq/ rw_nbq_avg2

      real ruint_nbq(GLOBAL_2D_ARRAY,N)
      common /nbq_ruint/ ruint_nbq
      real rvint_nbq(GLOBAL_2D_ARRAY,N)
      common /nbq_rvint/ rvint_nbq
      real rwint_nbq(GLOBAL_2D_ARRAY,0:N)
      common /nbq_rwint/ rwint_nbq

      real ruint_bak_nbq(GLOBAL_2D_ARRAY,N,2)
      common /coup_ruint/ ruint_bak_nbq
      real rvint_bak_nbq(GLOBAL_2D_ARRAY,N,2)
      common /coup_rvint/ rvint_bak_nbq
      real rwint_bak_nbq(GLOBAL_2D_ARRAY,0:N,2)
      common /coup_rwint/ rwint_bak_nbq

      real ruext_nbq(GLOBAL_2D_ARRAY,N)
      common /nbq_ruext/ ruext_nbq
      real rvext_nbq(GLOBAL_2D_ARRAY,N)
      common /nbq_rvext/ rvext_nbq

      real Hzr_half_nbq(GLOBAL_2D_ARRAY,0:N+1)
      common /grid_Hzr_half_nbq/ Hzr_half_nbq
      real Hzw_half_nbq(GLOBAL_2D_ARRAY,0:N)
      common /grid_Hzw_half_nbq/ Hzw_half_nbq

      real zr_half_nbq(GLOBAL_2D_ARRAY,N)
      common /grid_zr_half_nbq/ zr_half_nbq
      real zw_half_nbq(GLOBAL_2D_ARRAY,0:N)
      common /grid_zw_half_nbq/ zw_half_nbq

      real dzdt_nbq(GLOBAL_2D_ARRAY,0:N)
      common /grid_dzdt_nbq/ dzdt_nbq

#if defined NBQ_CONSOUT || defined NBQ_CONS0
      real dzdt2_nbq(GLOBAL_2D_ARRAY,0:N)
      common /grid_dzdt2_nbq/ dzdt2_nbq
#endif

      real e(GLOBAL_2D_ARRAY)
      common /nbq_e/ e
      real eomn(GLOBAL_2D_ARRAY)
      common /nbq_eomn/ eomn
      real cosa(GLOBAL_2D_ARRAY)
      common /nbq_cosa/ cosa
      real sina(GLOBAL_2D_ARRAY)
      common /nbq_sina/ sina

#endif
  
