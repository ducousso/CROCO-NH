! $Id: buffer.h 1458 2014-02-03 15:01:25Z gcambon $
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
/* Buffer array to allow reshaping of input/output data fot
 the purpose of compatibility with the SCRUM plotting package.  
 This implies that variables defined at RHO-, VORTICITY-, U-
 and V-points are written into netCDF files in such a way as
 if they would be dimensioned as follows:

 Location     name                  dimensions 

   RHO-        r2dvar   zeta-type   (0:Lm+1,0:Mm+1)
   VORT-       p2dvar   vort-type   (1:Lm+1,1:Mm+1)
   U-          u2dvar   ubar-type   (1:Lm+1,0:Mm+1)
   V-          v2dvar   vbar-type   (0:Lm+1,1:Mm+1)
 
   RHO-,RHO-   r3dvar   RHO-type    (0:Lm+1,0:Mm+1,  N)
   VORT-,RHO-  p3dvar               (1:Lm+1,1:Mm+1,  N)
   U-,RHO-     u3dvar   U-type      (1:Lm+1,0:Mm+1,  N)
   V-,RHO-     v3dvar   V-type      (0:Lm+1,1:Mm+1,  N)
   RHO-,W-     w3dvar   W-type      (0:Lm+1,0:Mm+1,0:N) 
   RHO-,BED-   b3dvar   BED-type    (0:Lm+1,0:Mm+1,NLAY) 
*/
      real buff((Lm+5)*(Mm+5)*(N+1))
      common /zzz/ buff

