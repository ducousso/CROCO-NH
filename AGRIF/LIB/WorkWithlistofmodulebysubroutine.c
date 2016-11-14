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
/*                    RecordUseModulesVariables                               */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void RecordUseModulesVariables()
{
  listusemodule *tmplistmodule;

  /* we should record all variables defined in modules used in this file      */
  if ( List_NameOfModuleUsed )
  {
     tmplistmodule = List_NameOfModuleUsed;
     while ( tmplistmodule )
     {
        if ( tmplistmodule->u_firstuse == 1 )
        {
           /* check if the file .depend<u_usemodule> exist                    */
           List_ModuleUsed_Var = Readthedependfile
                               (tmplistmodule->u_usemodule,List_ModuleUsed_Var);
           List_GlobParamModuleUsed_Var = ReaddependParameterList
                      (tmplistmodule->u_usemodule,List_GlobParamModuleUsed_Var);

        }

        tmplistmodule = tmplistmodule->suiv;
     }
  }
}

/******************************************************************************/
/*                RecordUseModulesUseModulesVariables                         */
/******************************************************************************/
/******************************************************************************/
void RecordUseModulesUseModulesVariables()
{
    listusemodule *tmplistmodule;
    listusemodule *save_list;

    if ( ! List_NameOfModuleUsed )  return;

    /* we should record all variables defined in modules used in this file      */
    /* and we should read the .depend of the module used by the module used  */
    tmplistmodule = List_NameOfModuleUsed;
    while ( tmplistmodule )
    {
        Readthedependlistofmoduleused(tmplistmodule->u_usemodule);
        while( tmpuselocallist )
        {
            Addmoduletothelisttmp(tmpuselocallist->u_usemodule);
            save_list = tmpuselocallist->suiv;
            free(tmpuselocallist);
            tmpuselocallist = save_list;
        }
        tmplistmodule = tmplistmodule->suiv;
    }
    tmplistmodule = listofmoduletmp;
    while ( tmplistmodule )
    {
        Readthedependlistofmoduleused(tmplistmodule->u_usemodule);
        while( tmpuselocallist )
        {
            Addmoduletothelisttmp(tmpuselocallist->u_usemodule);
            save_list = tmpuselocallist->suiv;
            free(tmpuselocallist);
            tmpuselocallist = save_list;
        }
        tmplistmodule = tmplistmodule->suiv;
    }
    tmplistmodule = listofmoduletmp;
    while ( tmplistmodule )
    {
        // check if the file .depend<u_usemodule> exists
        List_ModuleUsedInModuleUsed_Var =
            Readthedependfile(tmplistmodule->u_usemodule,List_ModuleUsedInModuleUsed_Var);

        List_GlobParamModuleUsedInModuleUsed_Var =
            ReaddependParameterList(tmplistmodule->u_usemodule,List_GlobParamModuleUsedInModuleUsed_Var);
        tmplistmodule = tmplistmodule->suiv;
    }
}

/******************************************************************************/
/*                      Add_NameOfModuleUsed_1                                */
/******************************************************************************/
/* This subroutine is used to add a record to a list of struct                */
/* listusemodule                                                              */
/******************************************************************************/
/*                                                                            */
/*       subroutine sub ... USE mod1 ===> insert in list                      */
/*        _______     _______     _______     _______     _______             */
/*       +      +    +      +    +      +    +      +    +      +             */
/*       + NEW  +--->+ list +--->+ list +--->+ list +--->+ list +             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                                                            */
/*       list =  List_NameOfModuleUsed                                        */
/*                                                                            */
/******************************************************************************/
void Add_NameOfModuleUsed_1(char *name)
{
    listusemodule *newmodule;
    listusemodule *parcours;
    int out;

    newmodule = (listusemodule*) calloc(1, sizeof(listusemodule));
    strcpy(newmodule->u_usemodule, name);
    strcpy(newmodule->u_charusemodule, charusemodule);
    strcpy(newmodule->u_modulename, curmodulename);
    strcpy(newmodule->u_cursubroutine, subroutinename);
    newmodule->u_firstuse = 1 ;
    newmodule->suiv = NULL;

    if ( List_NameOfModuleUsed == NULL )
    {
        List_NameOfModuleUsed = newmodule ;
    }
    else
    {
        parcours = List_NameOfModuleUsed;
        while ( parcours && newmodule->u_firstuse )
        {
            if ( !strcasecmp(name,parcours->u_usemodule) )
            {
                newmodule->u_firstuse = 0 ;
            }
            parcours = parcours->suiv;
        }
        /* we can not add the same module twice for the same subroutine           */
        parcours = List_NameOfModuleUsed;
        out = 0 ;
        while ( parcours && out == 0 )
        {
            if ( !strcasecmp(name,parcours->u_usemodule) &&
                 !strcasecmp(subroutinename,parcours->u_cursubroutine) )
            {
                out = 1 ;
                free(newmodule);
            }
            else
                parcours = parcours->suiv;
        }
        if ( out == 0 )
        {
            newmodule->suiv = List_NameOfModuleUsed;
            List_NameOfModuleUsed = newmodule;
        }
    }
}

/******************************************************************************/
/*                        Addmoduletothelist                                  */
/******************************************************************************/
/* This subroutine is used to add a record to a list of struct                */
/* listusemodule                                                              */
/******************************************************************************/
/*                                                                            */
/*       subroutine sub ... USE mod1 ===> insert in list                      */
/*        _______     _______     _______     _______     _______             */
/*       +      +    +      +    +      +    +      +    +      +             */
/*       + NEW  +--->+ list +--->+ list +--->+ list +--->+ list +             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                                                            */
/*       list =  List_NameOfModuleUsed                                     */
/*                                                                            */
/******************************************************************************/
void Addmoduletothelist(const char *name)
{
    listusemodule *newmodule;
    listusemodule *parcours;
    int out;

    newmodule = (listusemodule*) calloc(1,sizeof(listusemodule));
    strcpy(newmodule->u_usemodule, name);
    strcpy(newmodule->u_charusemodule, charusemodule);
    strcpy(newmodule->u_cursubroutine, subroutinename);
    newmodule->u_firstuse = 1 ;
    newmodule->suiv = NULL;

    if ( !List_NameOfModuleUsed )
    {
        List_NameOfModuleUsed = newmodule ;
    }
    else
    {
        parcours = List_NameOfModuleUsed;
        while ( parcours && newmodule->u_firstuse == 1 )
        {
            if ( !strcasecmp(name,parcours->u_usemodule) )
            {
                newmodule->u_firstuse = 0 ;
            }
            parcours=parcours->suiv;
        }
        /* we can not add the same module twice for the same subroutine           */
        parcours = List_NameOfModuleUsed;
        out = 0 ;
        while ( parcours && out == 0 )
        {
            if ( !strcasecmp(name,parcours->u_usemodule) &&
                 !strcasecmp(subroutinename,parcours->u_cursubroutine) )
            {
                out = 1 ;
                free(newmodule);
            }
            else
                parcours=parcours->suiv;
        }
        if ( out == 0 )
        {
            newmodule->suiv = List_NameOfModuleUsed;
            List_NameOfModuleUsed = newmodule;
        }
    }
}


/******************************************************************************/
/*                        WriteUsemoduleDeclaration                           */
/******************************************************************************/
/* Firstpass 0                                                                */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void WriteUsemoduleDeclaration(const char *cursubroutinename)
{
    listusemodule     *newmodule;
    listvarpointtovar *pointtmp;
    long int          fictifpos;
    int               findcoupled;

    fprintf(fortran_out,"\n");
    newmodule = List_NameOfModuleUsed;

    while ( newmodule )
    {
        if ( !strcasecmp(newmodule->u_cursubroutine, cursubroutinename) )
        {
            if (strcmp(newmodule->u_charusemodule,""))
            {
/*
                findcoupled = 0;
                pointtmp = List_CouplePointed_Var;
                while(pointtmp)
                {
                    if ((!strcasecmp(pointtmp->t_usemodule, newmodule->u_charusemodule)) && \
                        (!strcasecmp(pointtmp->t_cursubroutine, cursubroutinename)))
                    {
                        fictifpos = setposcur();
                        variableisglobalinmodule(pointtmp->t_couple,newmodule->u_charusemodule,fortran_out,fictifpos);
                        findcoupled = 1;
                    }
                    pointtmp=pointtmp->suiv;
                }
                if (findcoupled == 0)   fprintf(fortran_out,"      use %s\n",newmodule->u_charusemodule);
*/
                fprintf(fortran_out,"      use %s\n", newmodule->u_charusemodule);
            }
        }
        newmodule = newmodule ->suiv;
    }
}
