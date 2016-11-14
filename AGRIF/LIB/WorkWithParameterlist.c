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
/*                       Add_GlobalParameter_Var_1                            */
/******************************************************************************/
/*  This subroutines is used to add the variable defined in common in the     */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void Add_GlobalParameter_Var_1(listvar *listin)
{
    if ( VariableIsParameter )
        List_GlobalParameter_Var =  AddListvarToListvar(listin, List_GlobalParameter_Var, 1);
}

/******************************************************************************/
/*                       Add_Parameter_Var_1                                  */
/******************************************************************************/
/*  This subroutines is used to add the variable defined in common in the     */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void Add_Parameter_Var_1(listvar *listin)
{
    listvar *parcours;

    if ( !VariableIsParameter )    return;

    if ( List_Parameter_Var == NULL )
    {
        List_Parameter_Var = listin;
    }
    else
    {
        parcours = List_Parameter_Var;
        while ( parcours->suiv )
            parcours = parcours->suiv;
        parcours->suiv = listin;
    }
}

/******************************************************************************/
/*                       Add_Dimension_Var_1                                  */
/******************************************************************************/
/*  This subroutines is used to add the variable defined in common in the     */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void Add_Dimension_Var_1(listvar *listin)
{
   listvar *parcours;

    if ( List_Dimension_Var == NULL )
    {
        List_Dimension_Var = listin;
    }
    else
    {
        parcours = List_Dimension_Var;
        while (parcours->suiv)
            parcours = parcours->suiv;
        parcours->suiv = listin;
    }
}
