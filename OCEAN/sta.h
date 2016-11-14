! $Id: sta.h 1586 2014-07-30 14:57:11Z marchesiello $
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
#ifdef STATIONS
!  Include file "sta.h".
!  =====================
! NSTVARS    Number of stations variables.   
! stainfo    Station initial information.   
! istagrd    Index for station grid (embedding) location
! istastr    Index for station recording start time.           
! istaxgrd   Index for station x-grid location.               
! istaygrd   Index for station y-grid location.              
! istazgrd   Index for station z-grid location.             
! istalat    Index for station latitude location.            
! istalon    Index for station longitude location.          
! istadpt    Index for station depth.                      
! istaz      Index for station sea level
! istatem    Index for station potential temperature.     
! istasal    Index for station salinity.                 
! istaden    Index for station density anomaly.          
! istav      Index for station v-component velocity.    
! istav      Index for station v-component velocity.          
! nstas      Number of stations.                              
! stagrd     Station/grid embedding correspondance array.   
! stainfo    Station data at input.                        
! stadata    Station variables data collected for output   
! staSigm    Station data at all sigma levels
! diagsta    Flag taht determines if it is time step station 

      integer NSTAVARS,
     &        istagrd,           istatstr,
     &        istaxgrd,          istaygrd,        istazgrd,
     &        istalon,           istalat,         istadpt,
     &        istatem,           istasal,         istaden, 
     &        istau,             istav,           istaz
      parameter (NSTAVARS=12,
     &        istagrd=-1,        istatstr=0,  
     &        istaxgrd=1,        istaygrd=2,      istazgrd=3, 
     &        istalon=4,         istalat=5,       istadpt=6,
     &        istatem=7,         istasal=8,       istaden=9, 
     &        istau=10,          istav=11,        istaz=12) 

      logical diagsta
      integer nstas0,nstas, stagrd(Msta)
      common /stan/ nstas0,nstas, diagsta

      real stainfo(istagrd:istazgrd,Msta)
      common /sta_info/ stainfo

      real staspval, stadeltap2c
      common /sta_scalars/ staspval, stadeltap2c

# ifdef ALL_SIGMA
      real stadata(1:NSTAVARS,Msta), staSigm(istadpt:istav,Msta,N)
!     common /sta_data/ stadata,stagrd
      common /sta_data/ stadata, staSigm
# else
      real stadata(1:NSTAVARS,Msta)
!     common /sta_data/ stadata,stagrd
      common /sta_data/ stadata
# endif

#endif /*STATIONS*/
