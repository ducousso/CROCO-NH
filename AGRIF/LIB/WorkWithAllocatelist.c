/******************************************************************************/
/*                                                                            */
/*     CONV (converter) for Agrif (Adaptive Grid Refinement In Fortran)       */
/*                                                                            */
/* Copyright or   or Copr. Laurent Debreu (Laurent.Debreu@imag.fr)            */
/*                        Cyril Mazauric (Cyril_Mazauric@yahoo.fr)            */
/* This software is governed by the CeCILL-C license under French law and     */
/* abiding by the rules of distribution of free software.  You can  use,      */
/* modify and/ or redistribute the software under the terms of the CeCILL-C   */
/* license as circulated by CEA, CNRS and INRIA at the following URL          */
/* "http://www.cecill.info".                                                  */
/*                                                                            */
/* As a counterpart to the access to the source code and  rights to copy,     */
/* modify and redistribute granted by the license, users are provided only    */
/* with a limited warranty  and the software's author,  the holder of the     */
/* economic rights,  and the successive licensors  have only  limited         */
/* liability.                                                                 */
/*                                                                            */
/* In this respect, the user's attention is drawn to the risks associated     */
/* with loading,  using,  modifying and/or developing or reproducing the      */
/* software by the user in light of its specific status of free software,     */
/* that may mean  that it is complicated to manipulate,  and  that  also      */
/* therefore means  that it is reserved for developers  and  experienced      */
/* professionals having in-depth computer knowledge. Users are therefore      */
/* encouraged to load and test the software's suitability as regards their    */
/* requirements in conditions enabling the security of their systems and/or   */
/* data to be ensured and,  more generally, to use and operate it in the      */
/* same conditions as regards security.                                       */
/*                                                                            */
/* The fact that you are presently reading this means that you have had       */
/* knowledge of the CeCILL-C license and that you accept its terms.           */
/******************************************************************************/
/* version 1.7                                                                */
/******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "decl.h"

/******************************************************************************/
/*                               Add_Allocate_Var_1                           */
/******************************************************************************/
/* Firstpass 1                                                                */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void Add_Allocate_Var_1(const char *nom, const char *nommodule)
{
   listallocate *newvar;
   listallocate *parcours;
   int out;

   if ( firstpass == 1 )
   {
      if ( !List_Allocate_Var )
      {
         newvar = (listallocate *)calloc(1,sizeof(listallocate));
         strcpy(newvar->a_nomvar,nom);
         strcpy(newvar->a_subroutine,subroutinename);
         strcpy(newvar->a_module,nommodule);
         Save_Length(nom,25);
         newvar->suiv = NULL;
         List_Allocate_Var = newvar;
      }
      else
      {
         parcours = List_Allocate_Var;
         out = 0 ;
         while ( parcours->suiv && out == 0 )
         {
            if (  !strcasecmp(parcours->a_nomvar, nom) &&
                  !strcasecmp(parcours->a_subroutine, subroutinename) &&
                  !strcasecmp(parcours->a_module, nommodule) ) out = 1;
            else
               parcours=parcours->suiv;
         }
         if ( out == 0 )
         {
            if (  !strcasecmp(parcours->a_nomvar,nom) &&
                  !strcasecmp(parcours->a_subroutine,subroutinename) &&
                  !strcasecmp(parcours->a_module,nommodule) ) out = 1;
            else
            {
               /* add the record                                              */
              newvar = (listallocate *)calloc(1,sizeof(listallocate));
              strcpy(newvar->a_nomvar, nom);
              strcpy(newvar->a_subroutine, subroutinename);
              strcpy(newvar->a_module, nommodule);
              Save_Length(nom,25);
              newvar->suiv = NULL;
              parcours->suiv = newvar;
            }
         }
      }
   }
}


/******************************************************************************/
/*                            IsVarAllocatable_0                              */
/******************************************************************************/
/* Firstpass 0                                                                */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
// int IsVarAllocatable_0(const char *ident)
// {
//    listallocate *parcours;
//    int out;
// 
//    out = 0 ;
//    if ( firstpass == 0 )
//    {
//       parcours = List_Allocate_Var;
//       while ( parcours && out == 0 )
//       {
//          if ( !strcasecmp(parcours->a_nomvar,ident)  ) out = 1 ;
//          else parcours=parcours->suiv;
//       }
//    }
//    return out;
// }
