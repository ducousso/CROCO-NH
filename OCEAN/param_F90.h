! $Id: param.h 1619 2015-01-07 13:53:03Z marchesiello $
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
!
# define F90CODE
# include "param.h"

# ifdef BIOLOGY
#  ifdef PISCES
#   ifdef DIAGNOSTICS_BIO 
#    ifdef key_trc_diaadd
     integer  Nhi,Nco3,Naksp,Netot,Nprorca   &
     &          , Nprorcad,Npronew,Npronewd  &
     &          , Nprobsi,Nprofed,Nprofen    &
     &          , Ngraztot,Nnitrifo2,Nfixo2,Nremino2 &
     &          , Npronewo2,Nprorego2      &
     &          , Nfld,Nflu16,Nkgco2,Natcco2,Nsinking &
     &          , Nsinkfer,Nsinksil        &
     &          , Nsinkcal,Nheup,Nirondep,Nnitrpot   &
#    endif
     &          , NumFluxTerms,NumVSinkTerms,NumGasExcTerms
#   endif

#   ifdef key_trc_diaadd
       parameter (Nhi       = 1, &
     &            Nco3      = 2,&
     &            Naksp     = 3,&
     &            Netot     = 4,&
     &            Nprorca   = 5,&
     &            Nprorcad  = 6,&
     &            Npronew   = 7,&
     &            Npronewd  = 8,&
     &            Nprobsi   = 9,&
     &            Nprofed   = 10,&
     &            Nprofen   = 11,&
     &            Npronewo2 = 12,&
     &            Nprorego2 = 13,&
     &            Ngraztot  = 14,&
     &            Nnitrifo2 = 15,&
     &            Nremino2  = 16,&
     &            Nfixo2    = 17,&
     &            NumFluxTerms = Nfixo2)

       parameter (Nfld      = 1,&
     &            Nflu16    = 2,&
     &            Nkgco2    = 3,&
     &            Natcco2   = 4,&
     &            Nsinking  = 5,&
     &            Nsinkfer  = 6,&
     &            Nsinksil  = 7,&
     &            Nsinkcal  = 8,&
     &            Nheup     = 9,&
     &            Nirondep  = 10,&
     &            Nnitrpot  = 11,&
     &            NumGasExcTerms = 0,&
     &            NumVSinkTerms = Nnitrpot)
#   else
       parameter (NumFluxTerms = 0)
       parameter (NumGasExcTerms = 0, NumVSinkTerms = 0)
#   endif
#endif
#endif

# undef  F90CODE
 



