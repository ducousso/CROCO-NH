#include "cppdefs.h"
#ifdef ONLINE_ANALYSIS

        module module_parameter_oa

        integer :: imax,jmax,kmax

# define F90CODE
# include "param.h"
# undef  F90CODE


#ifdef MPI
# define imax Lmmpi
# define jmax Mmmpi
#else
# define imax Lm
# define jmax Mm
#endif   


        end module module_parameter_oa
 
#else
        module module_parameter_oa_empty
        end module
#endif
