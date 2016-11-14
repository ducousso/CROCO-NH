!
! $Id: exchange.F 830 2012-05-16 17:02:20Z marchesiello $
!
#include "cppdefs.h"
#if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI

# define exchange_2d_tile exchange_r2d_tile
# define exchange_2d_3pts_tile exchange_r2d_3pts_tile
# define JSTART JstrR
# define ISTART IstrR
# include "exchange_2d_tile.h"
# undef ISTART 
# undef JSTART 
# undef exchange_2d_tile
# undef exchange_2d_3pts_tile

# define exchange_2d_tile exchange_u2d_tile
# define exchange_2d_3pts_tile exchange_u2d_3pts_tile
# define JSTART JstrR
# define ISTART Istr
# include "exchange_2d_tile.h"
# undef ISTART 
# undef JSTART 
# undef exchange_2d_tile
# undef exchange_2d_3pts_tile

# define exchange_2d_tile exchange_v2d_tile
# define exchange_2d_3pts_tile exchange_v2d_3pts_tile
# define JSTART Jstr
# define ISTART IstrR
# include "exchange_2d_tile.h"
# undef ISTART 
# undef JSTART 
# undef exchange_2d_tile
# undef exchange_2d_3pts_tile

# define exchange_2d_tile exchange_p2d_tile
# define exchange_2d_3pts_tile exchange_p2d_3pts_tile
# define JSTART Jstr
# define ISTART Istr
# include "exchange_2d_tile.h"
# undef ISTART 
# undef JSTART 
# undef exchange_2d_tile
# undef exchange_2d_3pts_tile

# ifdef SOLVE3D
#  define KSTART 1
#  define exchange_3d_tile exchange_r3d_tile
#  define exchange_3d_3pts_tile exchange_r3d_3pts_tile
#  define JSTART JstrR
#  define ISTART IstrR
#  include "exchange_3d_tile.h"
#  undef ISTART
#  undef JSTART
#  undef exchange_3d_tile
#  undef exchange_3d_3pts_tile

#  define exchange_3d_tile exchange_u3d_tile
#  define exchange_3d_3pts_tile exchange_u3d_3pts_tile
#  define JSTART JstrR
#  define ISTART Istr
#  include "exchange_3d_tile.h"
#  undef ISTART
#  undef JSTART
#  undef exchange_3d_tile
#  undef exchange_3d_3pts_tile

#  define exchange_3d_tile exchange_v3d_tile
#  define exchange_3d_3pts_tile exchange_v3d_3pts_tile
#  define JSTART Jstr
#  define ISTART IstrR
#  include "exchange_3d_tile.h"
#  undef ISTART
#  undef JSTART
#  undef exchange_3d_tile
#  undef exchange_3d_3pts_tile

#  define exchange_3d_tile exchange_p3d_tile
#  define exchange_3d_3pts_tile exchange_p3d_3pts_tile
#  define JSTART Jstr
#  define ISTART Istr
#  include "exchange_3d_tile.h"
#  undef ISTART
#  undef JSTART
#  undef exchange_3d_tile
#  undef exchange_3d_3pts_tile
#  undef KSTART

#  define KSTART 0
#  define exchange_3d_tile exchange_w3d_tile
#  define exchange_3d_3pts_tile exchange_w3d_3pts_tile
#  define JSTART JstrR
#  define ISTART IstrR
#  include "exchange_3d_tile.h"
#  undef ISTART
#  undef JSTART
#  undef exchange_3d_tile
#  undef exchange_3d_3pts_tile
#  undef KSTART
# endif
#else
      subroutine exchange_empty
      end
#endif




