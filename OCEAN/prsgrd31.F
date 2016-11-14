! $Id: prsgrd31.F 1458 2014-02-03 15:01:25Z gcambon $
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
#include "cppdefs.h"
#ifdef SOLVE3D

      subroutine prsgrd (tile)
      USE private_scratch
      implicit none
      integer tile, trd, omp_get_thread_num
c# include "param.h"
c# include "private_scratch.h"
# include "compute_tile_bounds.h" 
      trd=omp_get_thread_num()
      call prsgrd_tile (Istr,Iend,Jstr,Jend, A3d(1,1,trd),
     &                              A3d(1,2,trd),A2d(1,1,trd))
      return
      end

      subroutine prsgrd_tile (Istr,Iend,Jstr,Jend, ru,rv,pgrd)
!
! Compute pressure gradient term: STANDARD JACOBIAN or WEIGHTED 
! JACOBIAN of Tony Song. Both these approaches imply that the
! horizontal differencing of the density field is done before
! its vertical integration.
!
!  Input:   rho      Density anomaly [kg/m^3].
!
!  Output:  Initialize computation of right-hand-sides
!                            for the 3D momentum equations.
!    ru = - pgrd_XI      ! XI,ETA-components of pressure
!    rv = - pgrd_ETA     ! gradient terms.
!
!  Reference:
!
!  Song, Y.T. and D.G. Wright, 1997: A general pressure gradient
!          formutlation for numerical ocean models. Part I: Scheme
!          design and diagnostic analysis.  DRAFT.
!---------------------------------------------------------------------
!
      USE grid
      USE ocean3d
      USE scalars
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
c# include "param.h"
      real ru(PRIVATE_2D_SCRATCH_ARRAY,N),  cff,
     &     rv(PRIVATE_2D_SCRATCH_ARRAY,N),  cff1,
     &     pgrd(PRIVATE_1D_SCRATCH_ARRAY),  gamma
c# include "grid.h"
c# include "ocean3d.h"
c# include "scalars.h"
!
# include "compute_auxiliary_bounds.h"
!
! Switches: RHO_SURF:  Include/Disregard the barotropic part.
!           WJ_GRADP:  WEIGHTED/STANDARD jacobian form.
!
#define RHO_SURF
c**
c**   Original coefficient by Tony was 0.25
c**
#define WJ_GRADP 0.125
!
! Compute XI-component of pressure gradient term:
!----------------------------------------------------------------
! Compute pgrd(:,N) which is the pressure gradient at the topmost
! grid box around u(:,:,N) point, which includes the contribution
! due to the free surface elevation (barotropic part) and the
! contribution due to the density difference in the top-most grid
! box (baroclinic part). This operation also initializes vertical
! integration.
!
      do j=Jstr,Jend
        cff=0.5D0*g/rho0
c*      cff1=(1000.D0-rho0)*g/rho0

        cff1= 1000.D0      *g/rho0

        do i=IstrU,Iend
          pgrd(i)=cff*(rho(i,j,N)-rho(i-1,j,N))
     &                             *( z_w(i,j,N)+z_w(i-1,j,N)
     &                               -z_r(i,j,N)-z_r(i-1,j,N))
#ifdef RHO_SURF
     &                 +(cff1+cff*(rho(i,j,N)+rho(i-1,j,N)))
     &                              *(z_w(i,j,N)-z_w(i-1,j,N))
#endif
          ru(i,j,N)=-0.5*(Hz(i,j,N)+Hz(i-1,j,N))*on_u(i,j)
     &                                                 *pgrd(i)
        enddo
!
! Compute XI-component of interior [truly-]horizontal density
! gradient, using standard or weighted Jacobian, J(rho,z_r).
! Integrate it vertically, from top to bottom, properly scale
! it and add into r.h.s. "ru".
!
        cff=0.25D0*g/rho0
        do k=N-1,1,-1
          do i=IstrU,Iend
# ifdef WJ_GRADP
            gamma=WJ_GRADP *( z_r(i  ,j,k+1)-z_r(i-1,j,k+1)
     &                       +z_r(i  ,j,k  )-z_r(i-1,j,k  ))

     &                     *( z_r(i  ,j,k+1)-z_r(i-1,j,k+1)
     &                       -z_r(i  ,j,k  )+z_r(i-1,j,k  ))

     &                     /( (z_r(i  ,j,k+1)-z_r(i  ,j,k))
     &                       *(z_r(i-1,j,k+1)-z_r(i-1,j,k)))
# endif
            pgrd(i)=pgrd(i)+cff*(
# ifdef WJ_GRADP
     &            ( (1.+gamma)*(rho(i,j,k+1)-rho(i-1,j,k+1))
     &             +(1.-gamma)*(rho(i,j,k  )-rho(i-1,j,k  )))
     &                        *( z_r(i,j,k+1)+z_r(i-1,j,k+1)
     &                          -z_r(i,j,k  )-z_r(i-1,j,k  ))

     &                        -( rho(i,j,k+1)+rho(i-1,j,k+1)
     &                          -rho(i,j,k  )-rho(i-1,j,k  ))
     &           *( (1.+gamma)*(z_r(i,j,k+1)-z_r(i-1,j,k+1))
     &             +(1.-gamma)*(z_r(i,j,k  )-z_r(i-1,j,k  )))
# else
     &                         ( rho(i,j,k+1)-rho(i-1,j,k+1)
     &                          +rho(i,j,k  )-rho(i-1,j,k  ))
     &                        *( z_r(i,j,k+1)+z_r(i-1,j,k+1)
     &                          -z_r(i,j,k  )-z_r(i-1,j,k  ))

     &                        -( rho(i,j,k+1)+rho(i-1,j,k+1)
     &                          -rho(i,j,k  )-rho(i-1,j,k  ))
     &                        *( z_r(i,j,k+1)-z_r(i-1,j,k+1)
     &                          +z_r(i,j,k  )-z_r(i-1,j,k  ))
# endif
     &                                                      )

            ru(i,j,k)=-0.5*(Hz(i,j,k)+Hz(i-1,j,k))
     &                                    *on_u(i,j)*pgrd(i)
          enddo
        enddo     !--> discard pgrd
        if (j.ge.JstrV) then
!
! ETA-component of pressure gradient term:
!-----------------------------------------
! Same sequence as for the XI-component.
!
          cff=0.5D0*g/rho0
c*        cff1=g*(1000.D0-rho0)/rho0

          cff1= 1000.D0      *g/rho0

          do i=Istr,Iend
            pgrd(i)=cff*(rho(i,j,N)-rho(i,j-1,N))
     &                               *( z_w(i,j,N)+z_w(i,j-1,N)
     &                                 -z_r(i,j,N)-z_r(i,j-1,N))

#ifdef RHO_SURF
     &                   +(cff1+cff*(rho(i,j,N)+rho(i,j-1,N)))
     &                                *(z_w(i,j,N)-z_w(i,j-1,N))
#endif

            rv(i,j,N)=-0.5D0*(Hz(i,j,N)+Hz(i,j-1,N))
     &                                      *om_v(i,j)*pgrd(i)
          enddo

          cff=0.25*g/rho0
          do k=N-1,1,-1
            do i=Istr,Iend
# ifdef WJ_GRADP
              gamma=WJ_GRADP *( z_r(i,j  ,k+1)-z_r(i,j-1,k+1)
     &                         +z_r(i,j  ,k  )-z_r(i,j-1,k  ))

     &                       *( z_r(i,j  ,k+1)-z_r(i,j-1,k+1)
     &                         -z_r(i,j  ,k  )+z_r(i,j-1,k  ))

     &                       /( (z_r(i,j  ,k+1)-z_r(i,j  ,k))
     &                         *(z_r(i,j-1,k+1)-z_r(i,j-1,k)))
# endif
              pgrd(i)=pgrd(i)+cff*(
# ifdef WJ_GRADP
     &              ( (1.+gamma)*(rho(i,j,k+1)-rho(i,j-1,k+1))
     &               +(1.-gamma)*(rho(i,j,k  )-rho(i,j-1,k  )))
     &                         *( z_r(i,j,k+1)+z_r(i,j-1,k+1)
     &                           -z_r(i,j,k  )-z_r(i,j-1,k  ))

     &                         -( rho(i,j,k+1)+rho(i,j-1,k+1)
     &                           -rho(i,j,k  )-rho(i,j-1,k  ))
     &             *( (1.+gamma)*(z_r(i,j,k+1)-z_r(i,j-1,k+1))
     &               +(1.-gamma)*(z_r(i,j,k  )-z_r(i,j-1,k  )))
# else
     &                          ( rho(i,j,k+1)-rho(i,j-1,k+1)
     &                           +rho(i,j,k  )-rho(i,j-1,k  ))
     &                         *( z_r(i,j,k+1)+z_r(i,j-1,k+1)
     &                           -z_r(i,j,k  )-z_r(i,j-1,k  ))

     &                         -( rho(i,j,k+1)+rho(i,j-1,k+1)
     &                           -rho(i,j,k  )-rho(i,j-1,k  ))
     &                         *( z_r(i,j,k+1)-z_r(i,j-1,k+1)
     &                           +z_r(i,j,k  )-z_r(i,j-1,k  ))
# endif
     &                                                       )

              rv(i,j,k)=-0.5*(Hz(i,j,k)+Hz(i,j-1,k))
     &                                      *om_v(i,j)*pgrd(i)
            enddo
          enddo    !--> discard pgrd
        endif
      enddo
#else
      subroutine prsgrd_empty
#endif /* SOLVE3D */
      return
      end
