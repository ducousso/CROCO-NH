! $Id: boundary.h 1615 2014-12-17 13:27:07Z rblod $
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
#ifndef ANA_BRY
      real bry_time(2)
      common /bry_indices_array/ bry_time
      real bry_cycle
      common /bry_indices_real/ bry_cycle
      integer bry_id, bry_time_id, bry_ncycle, bry_rec, itbry, ntbry
      common /bry_indices_integer/ bry_id, bry_time_id, bry_ncycle,
     &                             bry_rec, itbry, ntbry

#if defined BIOLOGY || defined PISCES
      logical got_tbry(NT)
      real bry_time1(2,NT)
      common /bry_indices_array1/ bry_time1
      real bry_cycle1(NT)
      common /bry_indices_real1/ bry_cycle1
      integer bry_tid(NT), bry_ncycle1(NT),
     $        bry_rec1(NT), itbry1(NT), ntbry1(NT)
      common /bry_indices_integer1/ bry_tid, bry_ncycle1,
     &                              bry_rec1, itbry1, ntbry1
      common /bry_logical/ got_tbry
  
#endif

# if defined OBC_WEST || defined AGRIF_OBC_WEST
#  ifdef Z_FRC_BRY
      integer zetabry_west_id
      common /zeta_west_id/ zetabry_west_id
#  endif
#  ifdef M2_FRC_BRY
      integer ubarbry_west_id, vbarbry_west_id
      common /ubar_west_id/ ubarbry_west_id, vbarbry_west_id
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
      integer ubry_west_id, vbry_west_id
      common /u_west_id/ ubry_west_id, vbry_west_id
#   endif
#   ifdef T_FRC_BRY
      integer tbry_west_id(NT)
      common /t_west_id/ tbry_west_id
#   endif
#  endif
# endif

# if defined OBC_EAST || defined AGRIF_OBC_EAST
#  ifdef Z_FRC_BRY
      integer zetabry_east_id
      common /zeta_east_id/ zetabry_east_id
#  endif
#  ifdef M2_FRC_BRY
      integer ubarbry_east_id, vbarbry_east_id
      common /ubar_east_id/ ubarbry_east_id, vbarbry_east_id
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
      integer ubry_east_id, vbry_east_id
      common /u_east_id/ ubry_east_id, vbry_east_id
#   endif
#   ifdef T_FRC_BRY
      integer tbry_east_id(NT)
      common /t_east_id/ tbry_east_id
#   endif
#  endif
# endif

# if defined OBC_SOUTH || defined AGRIF_OBC_SOUTH
#  ifdef Z_FRC_BRY
      integer zetabry_south_id
      common /zeta_south_id/ zetabry_south_id
#  endif
#  ifdef M2_FRC_BRY
      integer ubarbry_south_id, vbarbry_south_id
      common /ubar_south_id/ ubarbry_south_id, vbarbry_south_id
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
      integer ubry_south_id, vbry_south_id
      common /u_south_id/ ubry_south_id, vbry_south_id
#   endif
#   ifdef T_FRC_BRY
      integer tbry_south_id(NT)
      common /t_south_id/ tbry_south_id
#   endif
#  endif
# endif

# if defined OBC_NORTH || defined AGRIF_OBC_NORTH
#  ifdef Z_FRC_BRY
      integer zetabry_north_id
      common /zeta_north_id/ zetabry_north_id
#  endif
#  ifdef M2_FRC_BRY
      integer ubarbry_north_id, vbarbry_north_id
      common /ubar_north_id/ ubarbry_north_id, vbarbry_north_id
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
      integer ubry_north_id, vbry_north_id
      common /u_north_id/ ubry_north_id, vbry_north_id
#   endif
#   ifdef T_FRC_BRY
      integer tbry_north_id(NT)
      common /t_north_id/ tbry_north_id
#   endif
#  endif
# endif
#endif  /* ANA_BRY */

#if defined OBC_WEST || defined AGRIF_OBC_WEST
# ifdef Z_FRC_BRY
      real zetabry_west(GLOBAL_1D_ARRAYETA),
     &    zetabry_west_dt(GLOBAL_1D_ARRAYETA,2)
      common /bry_zeta_west/ zetabry_west, zetabry_west_dt
# endif
# ifdef M2_FRC_BRY
      real ubarbry_west(GLOBAL_1D_ARRAYETA),
     &    ubarbry_west_dt(GLOBAL_1D_ARRAYETA,2)
     &    ,vbarbry_west(GLOBAL_1D_ARRAYETA),
     &    vbarbry_west_dt(GLOBAL_1D_ARRAYETA,2)
      common /bry_ubar_west/ ubarbry_west, ubarbry_west_dt,
     &                       vbarbry_west, vbarbry_west_dt
# endif
# ifdef SOLVE3D
#  ifdef M3_FRC_BRY
      real ubry_west(GLOBAL_1D_ARRAYETA,N),
     &    ubry_west_dt(GLOBAL_1D_ARRAYETA,N,2)
     &    ,vbry_west(GLOBAL_1D_ARRAYETA,N),
     &    vbry_west_dt(GLOBAL_1D_ARRAYETA,N,2)
      common /bry_u_west/ ubry_west, ubry_west_dt,
     &                    vbry_west, vbry_west_dt
#  endif
#  ifdef T_FRC_BRY
      real tbry_west(GLOBAL_1D_ARRAYETA,N,NT),
     &    tbry_west_dt(GLOBAL_1D_ARRAYETA,N,2,NT)
      common /bry_t_west/ tbry_west, tbry_west_dt
#  endif
# endif
#endif

#if defined OBC_EAST || defined AGRIF_OBC_EAST
# ifdef Z_FRC_BRY
      real zetabry_east(GLOBAL_1D_ARRAYETA),
     &    zetabry_east_dt(GLOBAL_1D_ARRAYETA,2)
      common /bry_zeta_east/ zetabry_east, zetabry_east_dt
# endif
# ifdef M2_FRC_BRY
      real ubarbry_east(GLOBAL_1D_ARRAYETA),
     &    ubarbry_east_dt(GLOBAL_1D_ARRAYETA,2)
     &    ,vbarbry_east(GLOBAL_1D_ARRAYETA),
     &    vbarbry_east_dt(GLOBAL_1D_ARRAYETA,2)
      common /bry_ubar_east/ ubarbry_east, ubarbry_east_dt,
     &                       vbarbry_east, vbarbry_east_dt
# endif
# ifdef SOLVE3D 
#  ifdef M3_FRC_BRY
      real ubry_east(GLOBAL_1D_ARRAYETA,N),
     &    ubry_east_dt(GLOBAL_1D_ARRAYETA,N,2)
     &    ,vbry_east(GLOBAL_1D_ARRAYETA,N),
     &    vbry_east_dt(GLOBAL_1D_ARRAYETA,N,2)
      common /bry_u_east/ ubry_east, ubry_east_dt,
     &                    vbry_east, vbry_east_dt
#  endif
#  ifdef T_FRC_BRY
      real tbry_east(GLOBAL_1D_ARRAYETA,N,NT),
     &    tbry_east_dt(GLOBAL_1D_ARRAYETA,N,2,NT)
      common /bry_t_east/ tbry_east, tbry_east_dt
#  endif
# endif
#endif

#if defined OBC_SOUTH || defined AGRIF_OBC_SOUTH
# ifdef Z_FRC_BRY 
      real zetabry_south(GLOBAL_1D_ARRAYXI),
     &    zetabry_south_dt(GLOBAL_1D_ARRAYXI,2)
      common /bry_zeta_south/ zetabry_south, zetabry_south_dt
# endif
# ifdef M2_FRC_BRY
      real ubarbry_south(GLOBAL_1D_ARRAYXI),
     &    ubarbry_south_dt(GLOBAL_1D_ARRAYXI,2)
     &    ,vbarbry_south(GLOBAL_1D_ARRAYXI),
     &    vbarbry_south_dt(GLOBAL_1D_ARRAYXI,2)
      common /bry_ubar_south/ ubarbry_south, ubarbry_south_dt,
     &                        vbarbry_south, vbarbry_south_dt
# endif
# ifdef SOLVE3D
#  ifdef M3_FRC_BRY
      real ubry_south(GLOBAL_1D_ARRAYXI,N),
     &    ubry_south_dt(GLOBAL_1D_ARRAYXI,N,2)
     &    ,vbry_south(GLOBAL_1D_ARRAYXI,N),
     &    vbry_south_dt(GLOBAL_1D_ARRAYXI,N,2)
      common /bry_u_south/ ubry_south, ubry_south_dt,
     &                     vbry_south, vbry_south_dt
#  endif
#  ifdef T_FRC_BRY
      real tbry_south(GLOBAL_1D_ARRAYXI,N,NT),
     &    tbry_south_dt(GLOBAL_1D_ARRAYXI,N,2,NT)
      common /bry_t_south/ tbry_south, tbry_south_dt
#  endif
# endif
#endif

#if defined OBC_NORTH || defined AGRIF_OBC_NORTH
# ifdef Z_FRC_BRY
      real zetabry_north(GLOBAL_1D_ARRAYXI),
     &    zetabry_north_dt(GLOBAL_1D_ARRAYXI,2)
      common /bry_zeta_north/ zetabry_north, zetabry_north_dt
# endif
# ifdef M2_FRC_BRY
      real ubarbry_north(GLOBAL_1D_ARRAYXI),
     &    ubarbry_north_dt(GLOBAL_1D_ARRAYXI,2)
     &    ,vbarbry_north(GLOBAL_1D_ARRAYXI),
     &    vbarbry_north_dt(GLOBAL_1D_ARRAYXI,2)
      common /bry_ubar_north/ ubarbry_north, ubarbry_north_dt,
     &                        vbarbry_north, vbarbry_north_dt
# endif
# ifdef SOLVE3D
#  ifdef M3_FRC_BRY
      real ubry_north(GLOBAL_1D_ARRAYXI,N),
     &    ubry_north_dt(GLOBAL_1D_ARRAYXI,N,2)
     &    ,vbry_north(GLOBAL_1D_ARRAYXI,N),
     &    vbry_north_dt(GLOBAL_1D_ARRAYXI,N,2)
      common /bry_u_north/ ubry_north, ubry_north_dt,
     &                     vbry_north, vbry_north_dt
#  endif
#  ifdef T_FRC_BRY
      real tbry_north(GLOBAL_1D_ARRAYXI,N,NT),
     &    tbry_north_dt(GLOBAL_1D_ARRAYXI,N,2,NT)
      common /bry_t_north/ tbry_north, tbry_north_dt
#  endif
# endif
#endif

#ifdef WKB_WWAVE
# ifndef ANA_BRY_WKB
      real    brywkb_time(2)
      common /brywkb_indices_array/ brywkb_time
      real    brywkb_cycle
      common /brywkb_indices_real/ brywkb_cycle
      integer brywkb_id, brywkb_time_id, 
     &        brywkb_ncycle, brywkb_rec, itbrywkb, ntbrywkb
      common /brywkb_indices_integer/ brywkb_id, brywkb_time_id,
     &        brywkb_ncycle, brywkb_rec, itbrywkb, ntbrywkb
#  if defined WKB_OBC_WEST || defined AGRIF_OBC_WEST
      integer wacbry_west_id, wkxbry_west_id, wkebry_west_id
      common /wkbbry_west_id/ wacbry_west_id, wkxbry_west_id, 
     &        wkebry_west_id
#  endif
#  if defined WKB_OBC_EAST || defined AGRIF_OBC_EAST
      integer wacbry_east_id, wkxbry_east_id, wkebry_east_id
      common /wkbbry_east_id/ wacbry_east_id, wkxbry_east_id, 
     &        wkebry_east_id
#  endif
#  if defined WKB_OBC_SOUTH || defined AGRIF_OBC_SOUTH
      integer wacbry_south_id, wkxbry_south_id, wkebry_south_id
      common /wkbbry_south_id/ wacbry_south_id, wkxbry_south_id, 
     &        wkebry_south_id
#  endif
#  if defined WKB_OBC_NORTH || defined AGRIF_OBC_NORTH
      integer wacbry_north_id, wkxbry_north_id, wkebry_north_id
      common /wkbbry_north_id/ wacbry_north_id, wkxbry_north_id, 
     &        wkebry_north_id
#  endif
# endif  /* ANA_BRY_WKB */
# if defined WKB_OBC_WEST || defined AGRIF_OBC_WEST
      real wacbry_west(GLOBAL_1D_ARRAYETA),
     &     wacbry_west_dt(GLOBAL_1D_ARRAYETA,2),
     &     wkxbry_west(GLOBAL_1D_ARRAYETA),
     &     wkxbry_west_dt(GLOBAL_1D_ARRAYETA,2),
     &     wkebry_west(GLOBAL_1D_ARRAYETA),
     &     wkebry_west_dt(GLOBAL_1D_ARRAYETA,2),     
     &     warbry_west(GLOBAL_1D_ARRAYETA),
     &     warbry_west_dt(GLOBAL_1D_ARRAYETA,2)     
       common /bry_wkb_west/ wacbry_west, wacbry_west_dt,
     &                       wkxbry_west, wkxbry_west_dt,
     &                       wkebry_west, wkebry_west_dt,
     &                       warbry_west, warbry_west_dt
# endif
# if defined WKB_OBC_EAST || defined AGRIF_OBC_EAST
      real wacbry_east(GLOBAL_1D_ARRAYETA),
     &     wacbry_east_dt(GLOBAL_1D_ARRAYETA,2),
     &     wkxbry_east(GLOBAL_1D_ARRAYETA),
     &     wkxbry_east_dt(GLOBAL_1D_ARRAYETA,2),
     &     wkebry_east(GLOBAL_1D_ARRAYETA),
     &     wkebry_east_dt(GLOBAL_1D_ARRAYETA,2)     
      real warbry_east(GLOBAL_1D_ARRAYETA),
     &     warbry_east_dt(GLOBAL_1D_ARRAYETA,2)
       common /bry_wkb_east/ wacbry_east, wacbry_east_dt,
     &                       wkxbry_east, wkxbry_east_dt,
     &                       wkebry_east, wkebry_east_dt,
     &                       warbry_east, warbry_east_dt
# endif
# if defined WKB_OBC_SOUTH || defined AGRIF_OBC_SOUTH
      real wacbry_south(GLOBAL_1D_ARRAYXI),
     &     wacbry_south_dt(GLOBAL_1D_ARRAYXI,2),
     &     wkxbry_south(GLOBAL_1D_ARRAYXI),
     &     wkxbry_south_dt(GLOBAL_1D_ARRAYXI,2),
     &     wkebry_south(GLOBAL_1D_ARRAYXI),
     &     wkebry_south_dt(GLOBAL_1D_ARRAYXI,2),     
     &     warbry_south(GLOBAL_1D_ARRAYXI),
     &     warbry_south_dt(GLOBAL_1D_ARRAYXI,2)     
       common /bry_wkb_south/ wacbry_south, wacbry_south_dt,
     &                        wkxbry_south, wkxbry_south_dt,
     &                        wkebry_south, wkebry_south_dt,
     &                        warbry_south, warbry_south_dt
# endif
# if defined WKB_OBC_NORTH || defined AGRIF_OBC_NORTH
      real wacbry_north(GLOBAL_1D_ARRAYXI),
     &     wacbry_north_dt(GLOBAL_1D_ARRAYXI,2),
     &     wkxbry_north(GLOBAL_1D_ARRAYXI),
     &     wkxbry_north_dt(GLOBAL_1D_ARRAYXI,2),
     &     wkebry_north(GLOBAL_1D_ARRAYXI),
     &     wkebry_north_dt(GLOBAL_1D_ARRAYXI,2),
     &     warbry_north(GLOBAL_1D_ARRAYXI),
     &     warbry_north_dt(GLOBAL_1D_ARRAYXI,2)
       common /bry_wkb_north/ wacbry_north, wacbry_north_dt,
     &                        wkxbry_north, wkxbry_north_dt,
     &                        wkebry_north, wkebry_north_dt,
     &                        warbry_north, warbry_north_dt
# endif
#endif  /* WKB_WWAVE */ 


#if defined NBQ && defined NBQ_FRC_BRY
# if defined OBC_WEST || defined AGRIF_OBC_WEST
      real unbqbry_west(GLOBAL_1D_ARRAYETA,N),
     &     vnbqbry_west(GLOBAL_1D_ARRAYETA,N),
     &     wnbqbry_west(GLOBAL_1D_ARRAYETA,N),
     &     rnbqbry_west(GLOBAL_1D_ARRAYETA,N)
      common /bry_nbq_west/ unbqbry_west,
     &                      vnbqbry_west,
     &                      wnbqbry_west,
     &                      rnbqbry_west
# endif
# if defined OBC_EAST || defined AGRIF_OBC_EAST
      real unbqbry_east(GLOBAL_1D_ARRAYETA,N),
     &     vnbqbry_east(GLOBAL_1D_ARRAYETA,N),
     &     wnbqbry_east(GLOBAL_1D_ARRAYETA,N),
     &     rnbqbry_east(GLOBAL_1D_ARRAYETA,N)
      common /bry_nbq_east/ unbqbry_east,
     &                      vnbqbry_east,
     &                      wnbqbry_east,
     &                      rnbqbry_east
# endif
# if defined OBC_SOUTH || defined AGRIF_OBC_SOUTH
      real unbqbry_south(GLOBAL_1D_ARRAYETA,N),
     &     vnbqbry_south(GLOBAL_1D_ARRAYETA,N),
     &     wnbqbry_south(GLOBAL_1D_ARRAYETA,N),
     &     rnbqbry_south(GLOBAL_1D_ARRAYETA,N)
      common /bry_nbq_south/ unbqbry_south,
     &                       vnbqbry_south,
     &                       wnbqbry_south,
     &                       rnbqbry_south
# endif
# if defined OBC_NORTH || defined AGRIF_OBC_NORTH
      real unbqbry_north(GLOBAL_1D_ARRAYETA,N),
     &     vnbqbry_north(GLOBAL_1D_ARRAYETA,N),
     &     wnbqbry_north(GLOBAL_1D_ARRAYETA,N),
     &     rnbqbry_north(GLOBAL_1D_ARRAYETA,N)
      common /bry_nbq_west/ unbqbry_north,
     &                      vnbqbry_north,
     &                      wnbqbry_north,
     &                      rnbqbry_north
# endif
#endif /* NBQ */

#if defined NBQ && defined W_FRC_BRY
# if defined OBC_WEST || defined AGRIF_OBC_WEST
      real wbry_west(GLOBAL_1D_ARRAYETA,N)
      common /bry_w_west/ wbry_west
# endif
# if defined OBC_EAST || defined AGRIF_OBC_EAST
      real wbry_east(GLOBAL_1D_ARRAYETA,N)
      common /bry_w_east/ wbry_east
# endif
# if defined OBC_SOUTH || defined AGRIF_OBC_SOUTH
      real wbry_south(GLOBAL_1D_ARRAYETA,N)
      common /bry_w_south/ wbry_south
# endif
# if defined OBC_NORTH || defined AGRIF_OBC_NORTH
      real wbry_north(GLOBAL_1D_ARRAYETA,N)
      common /bry_w_west/ wbry_north
# endif
#endif /* NBQ */

