! $Id: compute_tile_bounds.h 1458 2014-02-03 15:01:25Z gcambon $
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
/* Auxiliary module "compute_tile_bounds.h":
------------------------------------------------------
 Bounds designed to cover interior points of an array for
 the purpose of shared memory subdomain partitioning (tiling.) 
                
 Input: tile -- usually from 0 to NSUB_X*NSUB_E-1 -- indicates
                the specified subdomain. tile=NSUB_X*NSUB_E
                corresponds to the whole domain of RHO points
                treated as a single block.
 Output: Istr,Iend -- starting and ending computational indices
         Jstr,Jend    in XI-    and ETA-directions.
*/
      integer chunk_size_X,margin_X,chunk_size_E,margin_E
      integer Istr,Iend,Jstr,Jend, i_X,j_E
 
#ifdef MPI
#define LOCALLM Lmmpi
#define LOCALMM Mmmpi
#else
#define LOCALLM Lm
#define LOCALMM Mm
#endif        
      chunk_size_X=(LOCALLM+NSUB_X-1)/NSUB_X
      margin_X=(NSUB_X*chunk_size_X-LOCALLM)/2
      chunk_size_E=(LOCALMM+NSUB_E-1)/NSUB_E
      margin_E=(NSUB_E*chunk_size_E-LOCALMM)/2   

#ifdef  ALLOW_SINGLE_BLOCK_MODE
      if (tile.eq.NSUB_X*NSUB_E) then
C$      trd=omp_get_thread_num()
C$      if (trd.gt.0) return !--> just return, if not master thread
        Istr=1
        Iend=LOCALLM       ! MONOBLOCK VERSION:
        Jstr=1        ! DO NOT DO THE PARTITION 
        Jend=LOCALMM   
      else
#endif

      j_E=tile/NSUB_X
      i_X=tile-j_E*NSUB_X

      Istr=1+i_X*chunk_size_X-margin_X
      Iend=Istr+chunk_size_X-1
      Istr=max(Istr,1)
      Iend=min(Iend,LOCALLM)

      Jstr=1+j_E*chunk_size_E-margin_E
      Jend=Jstr+chunk_size_E-1
      Jstr=max(Jstr,1)
      Jend=min(Jend,LOCALMM)      

#ifdef  ALLOW_SINGLE_BLOCK_MODE
      endif
#endif

