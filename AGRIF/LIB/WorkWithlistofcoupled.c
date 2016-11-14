/******************************************************************************/
/*                                                                            */
/*     CONV (converter) for Agrif (Adaptive Grid Refinement In Fortran)       */
/*                                                                            */
/* Copyright or    or Copr. Laurent Debreu (Laurent.Debreu@imag.fr)           */
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
/*                          variscoupled_0                                    */
/******************************************************************************/
/* Firstpass 0                                                                */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
int variscoupled_0(const char *ident)
{
    listvarpointtovar *pointtmplist; 
    listcouple *coupletmp;   
    int out = 0;
    
    if (firstpass == 0 )
    {
        pointtmplist = List_CouplePointed_Var;
        while ( pointtmplist && out == 0)
        {
            coupletmp = pointtmplist->t_couple;
            while ( coupletmp && out == 0)
            {
                /* we should find the same variable name in the same subroutine    */
                if ( !strcasecmp(ident, coupletmp->c_namevar) &&
                     !strcasecmp(subroutinename, pointtmplist->t_cursubroutine) &&
                      strcasecmp(coupletmp->c_namepointedvar, "") )
                {
                    out = 1;
                }
                coupletmp = coupletmp->suiv;
            }
            pointtmplist = pointtmplist->suiv;
        }
    }
    return out;
}

const char * getcoupledname_0(const char *ident)
{
    listvarpointtovar *pointtmplist; 
    listcouple *coupletmp;   
    int out = 0;
    
    if (firstpass == 0 )
    {
        pointtmplist = List_CouplePointed_Var;
        while ( pointtmplist && out == 0)
        {
            coupletmp = pointtmplist->t_couple;
            while ( coupletmp && out == 0)
            {
                /* we should find the same variable name in the same subroutine    */
                if ( !strcasecmp(coupletmp->c_namevar,ident) &&
                     !strcasecmp(pointtmplist->t_cursubroutine,subroutinename) &&
                      strcasecmp(coupletmp->c_namepointedvar,"") )
                {
                    return coupletmp->c_namepointedvar;
                }
                coupletmp = coupletmp->suiv;
            }
            pointtmplist = pointtmplist->suiv;
        }
    }
    printf("end of getcoupledname_0 -- you should not be there !!! \n");
    return NULL;
}
