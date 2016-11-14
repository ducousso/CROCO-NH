! $Id: lmd_wscale_wm_and_ws.h 1458 2014-02-03 15:01:25Z gcambon $
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
# ifdef LIMIT_UNSTABLE_ONLY
            if (Bfsfc.lt.0.) zscale=min(zscale, 
     &                       my_hbl(i,j)*epssfc)
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
            if (zetahat.ge.0.) then
              wm=vonKar*ustar(i,j)*ustar3/max( ustar3+5.*zetahat,
     &                                                    1.E-20)
              ws=wm
!
! Unstable regime: note that zetahat is always negative here, also
! negative are constants "zeta_m" and "zeta_s".
!
            else
              if (zetahat .gt. zeta_m*ustar3) then
                wm=vonKar*( ustar(i,j)*(ustar3-16.*zetahat) )**r4
              else
                wm=vonKar*(a_m*ustar3-c_m*zetahat)**r3
              endif
              if (zetahat .gt. zeta_s*ustar3) then
                ws=vonKar*( (ustar3-16.*zetahat)/ustar(i,j) )**r2
              else
                ws=vonKar*(a_s*ustar3-c_s*zetahat)**r3
              endif
            endif
