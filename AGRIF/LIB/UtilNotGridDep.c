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
/*                     Add_NotGridDepend_Var_1                                */
/******************************************************************************/
/* This subroutine is used to add a record into List_NotGridDepend_Var        */
/*    This variable is add only if it is not present in the list              */
/*    This variable is add at the end of the list                             */
/******************************************************************************/
/*        _______     _______     _______     _______     _______             */
/*       + not  +    + not  +    +  not +    +  not +    +      +             */
/*       + grid +--->+ grid +--->+ grid +--->+ grid +--->+ NEW  +             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                                                            */
/******************************************************************************/
void Add_NotGridDepend_Var_1 (char *name)
{
   listvar *parcours;
   listvar *newvar;
   /*                                                                         */
   /* look in the List_NotGridDepend_Var if this variable exist               */
   parcours = List_NotGridDepend_Var;
   while (parcours)
   {
     if (!strcasecmp(parcours->var->v_nomvar,name))
     {
        /* if this variable exist -> exit of the program                      */
        printf(" The variable %s\n",name);
        printf(" has been declared twice \n");
        printf(" as a non grid dependent variable \n");
        exit(1);
     }
     parcours= parcours->suiv;
   }
   /* if variable does not exist, we add it                                   */
   newvar=(listvar *)calloc(1,sizeof(listvar));
   newvar->var=(variable *)calloc(1,sizeof(variable));
   strcpy(newvar->var->v_nomvar,name);
   strcpy(newvar->var->v_commoninfile,cur_filename);
   strcpy(newvar->var->v_subroutinename,subroutinename);
   newvar->var->v_notgrid = 1 ;
   newvar->suiv = List_NotGridDepend_Var;
   List_NotGridDepend_Var = newvar;
}

/******************************************************************************/
/*                      VarIsNonGridDepend                                    */
/******************************************************************************/
/* This subroutine is used to know if a variable has been declared as non     */
/* grid dependent                                                             */
/******************************************************************************/
/*                                                                            */
/*  notgriddepend variable;    ----------->  VarIsNonGridDepend = 1           */
/*                                                                            */
/*                                                                            */
/******************************************************************************/
int VarIsNonGridDepend(char *name)
{
   listvar *newvar;
   int out;

   newvar = List_NotGridDepend_Var;
   out=0;
   while (newvar && out == 0 )
   {
      if ( !strcasecmp(newvar->var->v_nomvar,name) ) out = 1;
      else newvar = newvar->suiv;
   }
   return out;
}
