! $Id: lmd_wscale_ws_only.h 1458 2014-02-03 15:01:25Z gcambon $
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
! PART OF KPP2005 (Shchepetkin et al. 2005)
!
! Compute turbulent velocity scales for tracers, ws=ws(zscale,Bfsfc),
! where where zscale is distance from the surface and Bfsfc buoyancy
! forcing.  The procedure of computation follows Eqs. (B1a)-(B1e) of
! LMD1994 paper with constants zeta_s, a_s, c_s specified in Eq.(B2).
! Mixing length scale "zscale" is limited by a specified fraction of
! boundary layer thickness in the case of unstable buoyancy forcing
! (as in the original 1994 code); or in both stable and unstable
! forcing (modification of Gokhan and Bill Large in 2003).
! Adapted from Bill Large 1995/2003 code.
!
! input variables: zscale, Bfsfc (both are scalars),
!                  ustar,  hbl (hbl is only for limiting of zscale)
! zeta_s, a_s, c_s are constants
!
! output: ws (scalar)
!
!
# ifdef LIMIT_UNSTABLE_ONLY
          if (Bfsfc .lt. 0.) zscale=min(zscale, 
     &                                  my_hbl(i,j)*epssfc)
# else
          zscale=min(zscale, my_hbl(i,j)*epssfc)
# endif
# ifdef MASKING
          zscale=zscale*rmask(i,j)
# endif
          zetahat=vonKar*zscale*Bfsfc
          ustar3=ustar(i,j)**3
!
! Stable regime.
!
          if (zetahat .ge. 0.) then
            ws=vonKar*ustar(i,j)*ustar3/max(ustar3+5.*zetahat,
     &                                                 1.E-20)
!
! Unstable regime: note that zetahat is always negative here, also
! negative is the constant "zeta_s", hence "ustar" must be positive 
! and bounded away from zero for this condition to be held.
!
          elseif (zetahat .gt. zeta_s*ustar3) then
            ws=vonKar*( (ustar3-16.*zetahat)/ustar(i,j) )**r2
!
! Convective regime: note that unlike the two cases above, this
! results in non-zero "ws" even in the case when ustar==0. 
!
          else
            ws=vonKar*(a_s*ustar3-c_s*zetahat)**r3
          endif
                     !--> discard zetahat, ustar3

