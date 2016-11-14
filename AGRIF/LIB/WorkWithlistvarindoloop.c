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
//#include <stdlib.h>
//#include <string.h>

#include "decl.h"

/******************************************************************************/
/*                         Add_UsedInSubroutine_Var_1                         */
/******************************************************************************/
/* Firstpass 1                                                                */
/* We should complete the List_UsedInSubroutine_Var                           */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void Add_UsedInSubroutine_Var_1 (const char *ident)
{
  listvar *newvar;
  listvar *tmpvar;
  int out;

   /* In the first pass we record all variables presents in the do loop       */
   if (firstpass == 1 && insubroutinedeclare == 1 )
   {

  if ( !List_UsedInSubroutine_Var )
  {
      newvar=(listvar *)calloc(1,sizeof(listvar));
      newvar->var=(variable *)calloc(1,sizeof(variable));
      /*                                                                      */
      Init_Variable(newvar->var);
      /*                                                                      */
      newvar->suiv = NULL;
      strcpy(newvar->var->v_nomvar,ident);
      strcpy(newvar->var->v_modulename,curmodulename);
      strcpy(newvar->var->v_commoninfile,cur_filename);
      strcpy(newvar->var->v_subroutinename,subroutinename);
      newvar->var->v_pointedvar=pointedvar;
      List_UsedInSubroutine_Var = newvar ;
  }
  else
  {
      /* We should verify that this variable did not added                    */
      tmpvar = List_UsedInSubroutine_Var;
      out = 0 ;
      while (tmpvar && out == 0 )
      {
         if ( !strcasecmp(tmpvar->var->v_nomvar,ident) &&
              !strcasecmp(tmpvar->var->v_subroutinename,subroutinename))
                                                                      out  = 1 ;
         else tmpvar = tmpvar->suiv;
      }
      if ( out == 0 )
      {
         newvar=(listvar *)calloc(1,sizeof(listvar));
         newvar->var=(variable *)calloc(1,sizeof(variable));
         /*                                                                   */
         Init_Variable(newvar->var);
         /*                                                                   */
         strcpy(newvar->var->v_nomvar,ident);
         strcpy(newvar->var->v_commoninfile,cur_filename);
         strcpy(newvar->var->v_modulename,curmodulename);
         strcpy(newvar->var->v_subroutinename,subroutinename);
         newvar->var->v_pointedvar=pointedvar;
         newvar->suiv = List_UsedInSubroutine_Var;
         List_UsedInSubroutine_Var = newvar;
      }
  }

   }
}

/******************************************************************************/
/*                        AJOUTEVARINDOLOOP_DEFINEDIMENSION                   */
/******************************************************************************/
/* This subroutine is used to add a listvar to  List_UsedInSubroutine_Var     */
/******************************************************************************/
void ajoutevarindoloop_definedimension (char *name)
{
  listvar *newvar;
  listvar *tmpvar;
  listvar *tmpvarprec;
  int out;
  int tablemeet;

  if ( !List_UsedInSubroutine_Var )
  {
  printf("LISTE VIDE\n");
      newvar=(listvar *)calloc(1,sizeof(listvar));
      newvar->var=(variable *)calloc(1,sizeof(variable));
      /*                                                                      */
      Init_Variable(newvar->var);
      /*                                                                      */
      newvar->suiv = NULL;
      strcpy(newvar->var->v_nomvar,name);
      strcpy(newvar->var->v_modulename,curmodulename);
      strcpy(newvar->var->v_commoninfile,cur_filename);
      strcpy(newvar->var->v_subroutinename,subroutinename);
      newvar->var->v_pointedvar=pointedvar;
      List_UsedInSubroutine_Var = newvar ;
  }
  else
  {
      /* We should verify that this variable did not added                    */
      tmpvarprec = (listvar *)NULL;
      tmpvar = List_UsedInSubroutine_Var;
      out = 0 ;
      tablemeet = 0 ;
      while (tmpvar && out == 0 )
      {
         if ( tablemeet == 0 && tmpvar->var->v_nbdim != 0 ) tablemeet = 1 ;
         /*                                                                   */
         if ( !strcasecmp(tmpvar->var->v_nomvar,name) &&
              !strcasecmp(tmpvar->var->v_subroutinename,subroutinename))
         {
            out  = 1 ;
            /* if this variable has been define before a table we do nothing  */
            /*    else we should remove it                                    */
            if ( tablemeet == 1 )
            {
               tmpvarprec->suiv = tmpvar -> suiv;
               out = 2;
            }
         }
         else
         {
            tmpvarprec = tmpvar;
            tmpvar = tmpvar->suiv;
         }
      }
      
      if ( out == 2 || out == 0 )
      {
         newvar=(listvar *)calloc(1,sizeof(listvar));
         newvar->var=(variable *)calloc(1,sizeof(variable));
         /*                                                                   */
         Init_Variable(newvar->var);
         /*                                                                   */
         strcpy(newvar->var->v_nomvar,name);
         strcpy(newvar->var->v_modulename,curmodulename);
         strcpy(newvar->var->v_commoninfile,cur_filename);
         strcpy(newvar->var->v_subroutinename,subroutinename);
         newvar->var->v_pointedvar=pointedvar;

         /* we should find this new variable to know the tabvars indice       */

         if ( variableisglobal(newvar, List_Global_Var) == 1 )
         {
            newvar->suiv = List_UsedInSubroutine_Var;
            List_UsedInSubroutine_Var = newvar;
         }
         else if ( variableisglobal(newvar, List_ModuleUsed_Var) == 1 )
         {
            newvar->suiv = List_UsedInSubroutine_Var;
            List_UsedInSubroutine_Var = newvar;
         }
         else if ( variableisglobal(newvar, List_Common_Var) == 1 )
         {
            newvar->suiv = List_UsedInSubroutine_Var;
            List_UsedInSubroutine_Var = newvar;
         }
         else
         {
            free(newvar);
         }
     }
  }
}

/******************************************************************************/
/*                        ModifyThelistvarindoloop                            */
/******************************************************************************/
/* This subroutine is to give the old name to the which has been              */
/* declared as USE MOD, U => V in this case we should replace in the          */
/* name V by the old name U in the List_UsedInSubroutine_Var                  */
/******************************************************************************/
void  ModifyThelistvarindoloop()
{
  listvar *newvar;

  newvar = List_UsedInSubroutine_Var;
  while ( newvar )
  {
     if ( strcasecmp(newvar->var->v_oldname,"") )
     {
        strcpy(newvar->var->v_nomvar,newvar->var->v_oldname);
     }
     newvar = newvar->suiv;
  }
}

/******************************************************************************/
/*                          CompleteThelistvarindoloop                        */
/******************************************************************************/
/* This subroutine is to add to the List_UsedInSubroutine_Var all variables   */
/* which has been declared as USE MOD, U => V in this case we should replace  */
/* in the List_UsedInSubroutine_Var the word U by the word V                  */
/******************************************************************************/
void  CompleteThelistvarindoloop()
{
    listvar *newvar;
    listvarpointtovar *pointtmplist;
    listcouple *coupletmp;
    int outvar;

    pointtmplist = List_CouplePointed_Var;
    while ( pointtmplist )
    {
        coupletmp = pointtmplist->t_couple;
        while ( coupletmp )
        {
            newvar = List_UsedInSubroutine_Var;
            outvar = 0 ;
            while ( newvar && outvar == 0)
            {
                /* we should find the same variable name in the same subroutine    */
                if ( !strcasecmp(newvar->var->v_nomvar, coupletmp->c_namevar) &&
                     !strcasecmp(newvar->var->v_subroutinename, pointtmplist->t_cursubroutine) &&
                      strcasecmp(coupletmp->c_namepointedvar, "") )
                {
                    outvar = 1;
                    strcpy(newvar->var->v_oldname, newvar->var->v_nomvar);
                    strcpy(newvar->var->v_nomvar, coupletmp->c_namepointedvar);
                }
                else
                {
                    newvar = newvar->suiv;
                }
            }
            coupletmp = coupletmp->suiv;
        }
        pointtmplist = pointtmplist->suiv;
    }
}

/******************************************************************************/
/*                             Merge_Variables                                */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void Merge_Variables(variable *var1, variable *var2)
{

    if ( !strcasecmp(var1->v_typevar,"") )
            strcpy(var1->v_typevar,var2->v_typevar);
    else    strcpy(var2->v_typevar,var1->v_typevar);

    if ( !strcasecmp(var1->v_oldname,"") )
            strcpy(var1->v_oldname,var2->v_oldname);
    else    strcpy(var2->v_oldname,var1->v_oldname);

    if ( !strcasecmp(var1->v_dimchar,"") )
            strcpy(var1->v_dimchar,var2->v_dimchar);
    else    strcpy(var2->v_dimchar,var1->v_dimchar);

    if ( !strcasecmp(var1->v_commonname,"") )
            strcpy(var1->v_commonname,var2->v_commonname);
    else    strcpy(var2->v_commonname,var1->v_commonname);

    if ( !strcasecmp(var1->v_modulename,"") || (var1->v_module ==0))
            strcpy(var1->v_modulename,var2->v_modulename);
    else    strcpy(var2->v_modulename,var1->v_modulename);

    if ( !strcasecmp(var1->v_vallengspec,"") )
            strcpy(var1->v_vallengspec,var2->v_vallengspec);
    else    strcpy(var2->v_vallengspec,var1->v_vallengspec);

    if ( !strcasecmp(var1->v_nameinttypename,"") )
            strcpy(var1->v_nameinttypename,var2->v_nameinttypename);
    else    strcpy(var2->v_nameinttypename,var1->v_nameinttypename);

    if ( !strcasecmp(var1->v_commoninfile,"") )
            strcpy(var1->v_commoninfile,var2->v_commoninfile);
    else    strcpy(var2->v_commoninfile,var1->v_commoninfile);

    if ( !strcasecmp(var1->v_precision,"") )
            strcpy(var1->v_precision,var2->v_precision);
    else    strcpy(var2->v_precision,var1->v_precision);

//     if ( !strcasecmp(var1->v_initialvalue,"") )
//             strcpy(var1->v_initialvalue,var2->v_initialvalue);
//     else    strcpy(var2->v_initialvalue,var1->v_initialvalue);

    if ( var1->v_initialvalue )
            var2->v_initialvalue = var1->v_initialvalue;
    else    var1->v_initialvalue = var2->v_initialvalue;

    if ( var1->v_initialvalue_array )
            var2->v_initialvalue_array = var1->v_initialvalue_array;
    else    var1->v_initialvalue_array = var2->v_initialvalue_array;
    
    if ( var1->v_do_loop )
            var2->v_do_loop = var1->v_do_loop;
    else    var1->v_do_loop = var2->v_do_loop;
    
//     if ( !strcasecmp(var1->v_initialvalue_array,"") )
//             strcpy(var1->v_initialvalue_array,var2->v_initialvalue_array);
//     else    strcpy(var2->v_initialvalue_array,var1->v_initialvalue_array);
    
    if ( !strcasecmp(var1->v_IntentSpec,"") )
            strcpy(var1->v_IntentSpec,var2->v_IntentSpec);
    else    strcpy(var2->v_IntentSpec,var1->v_IntentSpec);

    if ( !strcasecmp(var1->v_readedlistdimension,"") )
            strcpy(var1->v_readedlistdimension,var2->v_readedlistdimension);
    else    strcpy(var2->v_readedlistdimension,var1->v_readedlistdimension);

    if ( var1->v_dimension )
            var2->v_dimension = var1->v_dimension ;
    else    var1->v_dimension = var2->v_dimension ;

    if ( var1->v_nbdim == 0 )
            var1->v_nbdim = var2->v_nbdim ;
    else    var2->v_nbdim = var1->v_nbdim ;

    if ( var1->v_common == 0 )
            var1->v_common = var2->v_common ;
    else    var2->v_common = var1->v_common ;

    if ( var1->v_positioninblock == 0 )
            var1->v_positioninblock = var2->v_positioninblock ;
    else    var2->v_positioninblock = var1->v_positioninblock ;

    if ( var1->v_module == 0 )
            var1->v_module = var2->v_module ;
    else    var2->v_module = var1->v_module ;

    if ( var1->v_save == 0 )
            var1->v_save = var2->v_save ;
    else    var2->v_save = var1->v_save ;

    if ( var1->v_VariableIsParameter == 0 )
            var1->v_VariableIsParameter = var2->v_VariableIsParameter ;
    else    var2->v_VariableIsParameter = var1->v_VariableIsParameter ;

    if ( var1->v_indicetabvars == 0 )
            var1->v_indicetabvars = var2->v_indicetabvars ;
    else    var2->v_indicetabvars = var1->v_indicetabvars ;

    if ( var1->v_ExternalDeclare == 0 )
            var1->v_ExternalDeclare = var2->v_ExternalDeclare ;
    else    var2->v_ExternalDeclare = var1->v_ExternalDeclare ;

    if ( var1->v_pointedvar == 0 )
            var1->v_pointedvar = var2->v_pointedvar ;
    else    var2->v_pointedvar = var1->v_pointedvar ;

    if ( var1->v_dimensiongiven == 0 )
            var1->v_dimensiongiven = var2->v_dimensiongiven;
    else    var2->v_dimensiongiven = var1->v_dimensiongiven ;

    if ( var1->v_c_star == 0 )
            var1->v_c_star = var2->v_c_star;
    else    var2->v_c_star = var1->v_c_star ;

    if ( var1->v_catvar == 0 )
            var1->v_catvar = var2->v_catvar;
    else    var2->v_catvar = var1->v_catvar ;

    if ( var1->v_pointerdeclare == 0 )
            var1->v_pointerdeclare = var2->v_pointerdeclare ;
    else    var2->v_pointerdeclare = var1->v_pointerdeclare ;

    if ( var1->v_notgrid == 0 )
            var1->v_notgrid = var2->v_notgrid ;
    else    var2->v_notgrid = var1->v_notgrid;

    if ( var1->v_optionaldeclare == 0 )
            var1->v_optionaldeclare = var2->v_optionaldeclare;
    else    var2->v_optionaldeclare = var1->v_optionaldeclare ;

    if ( var1->v_allocatable == 0 )
            var1->v_allocatable = var2->v_allocatable ;
    else    var2->v_allocatable = var1->v_allocatable ;

    if ( var1->v_target == 0 )
            var1->v_target = var2->v_target ;
    else    var2->v_target = var1->v_target ;

    if ( var1->v_dimsempty == 0 )
            var1->v_dimsempty = var2->v_dimsempty ;
    else    var2->v_dimsempty = var1->v_dimsempty ;
}


/******************************************************************************/
/*                      Update_List_Subroutine_Var                            */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void Update_List_Subroutine_Var(listvar *list_to_modify)
{
   listvar *parcours;
   listvar *parcoursprec;
   listvar *parcours1;
   int out;

   parcoursprec = (listvar *)NULL;
   parcours = list_to_modify;
   while( parcours )
   {
      /* looking in List_SubroutineDeclaration_Var                            */
      parcours1 = List_SubroutineDeclaration_Var;
      out = 0;
      while ( parcours1 && out == 0 )
      {
         if ( !strcasecmp(parcours->var->v_nomvar,         parcours1->var->v_nomvar)         &&
              !strcasecmp(parcours->var->v_subroutinename, parcours1->var->v_subroutinename) &&
              !strcasecmp(parcours->var->v_modulename,     parcours1->var->v_modulename)
            ) out = 1;
         else parcours1 = parcours1->suiv;
      }
      /* if variable has been found                                           */

      if ( out == 1 ) Merge_Variables(parcours->var,parcours1->var);

      /* looking in List_Dimension_Var                                        */
      if (out == 0 )
      {
        parcours1 = List_Dimension_Var;
        out = 0;
        while ( parcours1 && out == 0 )
        {
           if ( !strcasecmp(parcours->var->v_nomvar,
                            parcours1->var->v_nomvar)         &&
                !strcasecmp(parcours->var->v_subroutinename,
                            parcours1->var->v_subroutinename) &&
                !strcasecmp(parcours->var->v_modulename,
                            parcours1->var->v_modulename)
              ) out = 1;
           else
           {
              parcoursprec = parcours1;
              parcours1 = parcours1->suiv;
           }
        }
        /* if variable has been found                                         */

        if ( out == 1 )
        {
           Merge_Variables(parcours->var,parcours1->var);
           /* we should remove this record from the List_Dimension_Var        */
           if ( parcours1 == List_Dimension_Var )
           {
              List_Dimension_Var = List_Dimension_Var -> suiv;
           }
           else
           {
              parcoursprec->suiv = parcours1 -> suiv;
           }
        }
      }
      /*                                                                      */
      parcours = parcours->suiv;
   }
}

void Update_List_Global_Var_From_List_Save_Var()
{
   listvar *parcours;
   listvar *newvar;
   char tmpname[LONG_VNAME];

   parcours = List_Save_Var;
   while( parcours )
   {
      if ( !strcasecmp(parcours->var->v_modulename,"") )
      /* Save in subroutine which is not defined in a module                  */
      {
         newvar = (listvar *)calloc(1,sizeof(listvar));
         newvar->var = (variable *)calloc(1,sizeof(variable));
         /*                                                                   */
         Init_Variable(newvar->var);
         /*                                                                   */
         newvar->suiv = NULL;

         Merge_Variables(parcours->var,newvar->var);
         strcpy(newvar->var->v_subroutinename,parcours->var->v_subroutinename);
         strcpy(newvar->var->v_nomvar,parcours->var->v_nomvar);
         newvar->var->v_catvar=parcours->var->v_catvar;
         sprintf(tmpname,"save_%s",parcours->var->v_subroutinename);
         Add_NameOfCommon_1(tmpname,parcours->var->v_subroutinename);
         strcpy(newvar->var->v_commonname,tmpname);
         List_Common_Var = AddListvarToListvar(newvar,List_Common_Var,1);
      }
      else
      /* Save in subroutine which is defined in a module                      */
      {
         newvar = (listvar *)calloc(1,sizeof(listvar));
         newvar->var = (variable *)calloc(1,sizeof(variable));
         /*                                                                   */
         Init_Variable(newvar->var);
         /*                                                                   */
         newvar->suiv = NULL;

         Merge_Variables(parcours->var,newvar->var);
         strcpy(newvar->var->v_subroutinename,parcours->var->v_subroutinename);

         strcpy(newvar->var->v_nomvar,parcours->var->v_nomvar);

         newvar->var->v_catvar=parcours->var->v_catvar;
         strcpy(newvar->var->v_modulename,parcours->var->v_modulename);
         List_Global_Var = AddListvarToListvar(newvar,List_Global_Var,1);
      }
      parcours = parcours->suiv;
   }
}

/******************************************************************************/
/*                      Update_List_From_Common_Var                           */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void Update_List_From_Common_Var(listvar *list_to_modify)
{
   listvar *parcours;
   listvar *parcours1;
   int out;
   parcours = list_to_modify;
   while( parcours )
   {
      /* looking in List_Global_Var                                           */
      parcours1 = List_Common_Var;
      out = 0;
      while ( parcours1 && out == 0 )
      {

         if ( !strcasecmp(parcours->var->v_nomvar,         parcours1->var->v_nomvar) &&
              !strcasecmp(parcours->var->v_subroutinename, parcours1->var->v_subroutinename) )
         {
            out = 1;
         }
         else parcours1 = parcours1->suiv;
      }
      /* if variable has been found                                           */
      if ( out == 1 )
      {
         strcpy(parcours->var->v_commoninfile,parcours1->var->v_commoninfile);

         Merge_Variables(parcours->var,parcours1->var);
      }
      parcours = parcours->suiv;
   }
}

/******************************************************************************/
/*                          Update_List_Var                                   */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void Update_List_Var(listvar *list_to_modify)
{
    listvar *parcours;
    listvar *parcours1;
    int out;

    parcours = list_to_modify;

    while( parcours )
    {
        /*printf("LE NOM EST %s\n",parcours->var->v_nomvar);*/
        /* looking in List_Global_Var                                           */
        out = 0;
        parcours1 = List_Global_Var;

        while ( parcours1 && out == 0 )
        {
            if ( !strcasecmp(parcours->var->v_nomvar, parcours1->var->v_nomvar)         &&
                 !strcasecmp(parcours->var->v_subroutinename, parcours1->var->v_subroutinename) &&
                 !strcasecmp(parcours->var->v_modulename, parcours1->var->v_modulename) )
            {
                out = 1;
            }
            else parcours1 = parcours1->suiv;
        }

        /* if variable has been found                                           */
        if ( out == 1 )
        {
            Merge_Variables(parcours->var,parcours1->var);
        }
        /* looking in List_SubroutineDeclaration_Var                            */
        else
        {
            parcours1 = List_SubroutineDeclaration_Var ;
            out = 0;
            while ( parcours1 )
            {
                if ( !strcasecmp(parcours->var->v_nomvar, parcours1->var->v_nomvar)                 &&
                     !strcasecmp(parcours->var->v_subroutinename, parcours1->var->v_subroutinename) &&
                     !strcasecmp(parcours->var->v_modulename, parcours1->var->v_modulename) )
                {
                    out = 1;
                    break;
                }
                else parcours1 = parcours1->suiv;
            }
            /* if variable has been found                                        */
            if ( out == 1 )
            {
                Merge_Variables(parcours->var,parcours1->var);
            }
            else
            {
                parcours1 = List_Common_Var ;
                out = 0;
                while ( parcours1 && out == 0 )
                {
                    if ( !strcasecmp(parcours->var->v_nomvar, parcours1->var->v_nomvar)                 &&
                         !strcasecmp(parcours->var->v_subroutinename, parcours1->var->v_subroutinename) &&
                         !strcasecmp(parcours->var->v_modulename, parcours1->var->v_modulename) )
                    {
                        out = 1;
                    }
                    else parcours1 = parcours1->suiv;
                }
                /* if variable has been found                                     */
                if ( out == 1 )
                {
                    Merge_Variables(parcours->var,parcours1->var);
                }
            }
        }
        parcours = parcours->suiv;
    }
}


void List_UsedInSubroutine_Var_Update_From_Module_Used()
{
   listvar *parcours;
   listvar *parcours3;
   listusemodule *parcours2;
   int out;


   parcours = List_UsedInSubroutine_Var;
   while( parcours )
   {
      out = 0 ;
      if ( parcours->var->v_indicetabvars == 0 )
      {
         parcours2 = List_NameOfModuleUsed;
         while( parcours2 )
         {
            if ( !strcasecmp(parcours2->u_cursubroutine, "") &&
                 !strcasecmp(parcours2->u_modulename, parcours->var->v_modulename) )
            {
               parcours3 = List_Global_Var;
               out = 0 ;
               while ( parcours3 && out == 0 )
               {
                  if ( !strcasecmp(parcours->var->v_nomvar,
                                   parcours3->var->v_nomvar)
                     ) out = 1 ;
                  else parcours3 = parcours3->suiv;
               }
               if ( out == 1 ) Merge_Variables(parcours->var,parcours3->var);
            }
            else if ( !strcasecmp(parcours2->u_cursubroutine, parcours->var->v_subroutinename) &&
                      !strcasecmp(parcours2->u_modulename,    parcours->var->v_modulename) )
            {
               parcours3 = List_Global_Var;
               out = 0 ;
               while ( parcours3 && out == 0 )
               {
                  if ( !strcasecmp(parcours->var->v_nomvar,
                                   parcours3->var->v_nomvar)
                     ) out = 1 ;
                  else parcours3 = parcours3->suiv;
               }
               if ( out == 1 ) Merge_Variables(parcours->var,parcours3->var);
            }
            parcours2 = parcours2->suiv;
         }
         /*                                                                   */
         if ( out == 0 )
         {
            parcours3 = List_ModuleUsed_Var;
            out = 0 ;
            while ( parcours3 && out == 0 )
            {
               if ( !strcasecmp(parcours->var->v_nomvar,
                                parcours3->var->v_nomvar)
                  ) out = 1 ;
               else parcours3 = parcours3->suiv;
            }
            if ( out == 1 ) Merge_Variables(parcours->var,parcours3->var);
         }
         /*                                                                   */
      }
      parcours = parcours->suiv;
   }
}



/******************************************************************************/
/*                       Update_NotGridDepend_Var                             */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void Update_NotGridDepend_Var(listvar *list_to_modify)
{
   listvar *parcours;
   listvar *parcours1;
   int out;

   parcours = list_to_modify;
   while( parcours )
   {
      /* looking in List_Global_Var                                           */
      parcours1 = List_Global_Var;
      out = 0;
      while ( parcours1 && out == 0 )
      {
         if ( !strcasecmp(parcours->var->v_nomvar,
                          parcours1->var->v_nomvar)
            ) out = 1;
         else parcours1 = parcours1->suiv;
      }
      /* if variable has been found                                           */
      if ( out == 1 )
      {
         Merge_Variables(parcours->var,parcours1->var);
         strcpy(parcours->var->v_subroutinename,
                parcours1->var->v_subroutinename);
         strcpy(parcours->var->v_modulename,parcours1->var->v_modulename);
      }
      parcours = parcours->suiv;
   }
}

int LookingForVariableInList(listvar *listin,variable *var)
{
   listvar *parcours1;
   int out;

   parcours1 = listin;
   out = 0 ;
   while ( parcours1 && out == 0 )
   {
      if ( !strcasecmp(var->v_nomvar,parcours1->var->v_nomvar)                &&
           !strcasecmp(var->v_subroutinename,parcours1->var->v_subroutinename)&&
           !strcasecmp(var->v_modulename,parcours1->var->v_modulename)        &&
                       var->v_save == 0                                       &&
                       var->v_common == 0
         ) out = 1 ;
      else parcours1 = parcours1 -> suiv;
   }

   return out;
}

int LookingForVariableInListGlobal(listvar *listin,variable *var)
{
   listvar *parcours1;
   int out;

   parcours1 = listin;
   out = 0 ;
   while ( parcours1 && out == 0 )
   {
      if ( !strcasecmp(var->v_nomvar,parcours1->var->v_nomvar)                &&
           !strcasecmp(var->v_subroutinename,parcours1->var->v_subroutinename)&&
           !strcasecmp(var->v_modulename,parcours1->var->v_modulename)
         ) out = 1 ;
      else parcours1 = parcours1 -> suiv;
   }

   return out;
}

int LookingForVariableInListName(listvar *listin,const char *name)
{
   listvar *parcours1;
   int out;

   parcours1 = listin;
   out = 0 ;
   while ( parcours1 && out == 0 )
   {
      if ( !strcasecmp(name,parcours1->var->v_nomvar) &&
           ( !strcasecmp(subroutinename,parcours1->var->v_subroutinename) ||
             !strcasecmp(subroutinename,"") )
         ) out = 1 ;
      else parcours1 = parcours1 -> suiv;
   }

   return out;
}

variable *get_variable_in_list_from_name( listvar *listin, const char *name )
{
    listvar *parcours = listin;
    variable *var = NULL;

    while ( parcours && (!var) )
    {
        if ( !strcasecmp(name,parcours->var->v_nomvar) &&
           ( !strcasecmp(subroutinename,parcours->var->v_subroutinename) ||
             !strcasecmp(subroutinename,"") ) )
        {
            var = parcours->var;
        }
        else parcours = parcours -> suiv;
   }
   return var;
}

int LookingForVariableInListGlob(listvar *listin,variable *var)
{
   listvar *parcours1;
   int out;

   parcours1 = listin;
   out = 0 ;
   while ( parcours1 && out == 0 )
   {
      if ( !strcasecmp(var->v_nomvar,parcours1->var->v_nomvar)                &&
           !strcasecmp(var->v_modulename,parcours1->var->v_modulename)
         ) out = 1 ;
      else parcours1 = parcours1 -> suiv;
   }

   return out;
}

int LookingForVariableInListParamGlob(listparameter *listin,variable *var)
{
   listparameter *parcours1;
   int out;

   parcours1 = listin;
   out = 0 ;
   while ( parcours1 && out == 0 )
   {
      if ( !strcasecmp(var->v_nomvar,parcours1->p_name)
         ) out = 1 ;
      else parcours1 = parcours1 -> suiv;
   }

   return out;
}

void UpdateListDeclarationWithDimensionList()
{
   List_SubroutineDeclaration_Var = AddListvarToListvar(List_Dimension_Var, List_SubroutineDeclaration_Var,1);
}


/* Remove from List_UsedInSubroutine_Var all variables comming from :         */
/*       - List_SubroutineArgument_Var                                        */
/*       - List_SubroutineDeclaration_Var                                     */
/*       - List_Parameter_Var                                                 */
/*       - List_FunctionType_Var                                              */
/*       - List_GlobalParameter_Var                                           */
/*       -                                                                    */
/*       -                                                                    */
void Clean_List_UsedInSubroutine_Var()
{
   listvar *parcours;
   listvar *parcoursprec;
   int remove;

   parcoursprec = (listvar *)NULL;
   parcours = List_UsedInSubroutine_Var;
   while ( parcours )
   {
      remove = LookingForVariableInListGlobal(List_SubroutineArgument_Var,   parcours->var);
      if ( remove == 0 )
           remove = LookingForVariableInList(List_SubroutineDeclaration_Var, parcours->var);
      if ( remove == 0 )
           remove = LookingForVariableInList(List_Parameter_Var, parcours->var);
      if ( remove == 0 )
           remove = LookingForVariableInList(List_FunctionType_Var, parcours->var);
      if ( remove == 0 )
           remove = LookingForVariableInListGlob(List_GlobalParameter_Var, parcours->var);
      if ( remove == 0 )
           remove = LookingForVariableInListParamGlob(List_GlobParamModuleUsed_Var, parcours->var);
      if ( remove == 0 )
      {
         if ( VariableIsInList(parcours,List_Global_Var)                 == 1 ||
              VariableIsInListCommon(parcours,List_Common_Var)           == 1 ||
              VariableIsInList(parcours,List_ModuleUsed_Var)             == 1 ||
              VariableIsInList(parcours,List_ModuleUsedInModuleUsed_Var) == 1
            ) remove = 0;
         else remove = 1;
      }

      /************************************************************************/
      /*                         Remove                                       */
      /************************************************************************/

      if ( remove == 1 )
      {
         if ( parcours == List_UsedInSubroutine_Var )
         {
           List_UsedInSubroutine_Var = List_UsedInSubroutine_Var -> suiv;
           parcours = List_UsedInSubroutine_Var;
         }
         else
         {
            parcoursprec->suiv = parcours->suiv;
            parcours = parcoursprec -> suiv ;
         }
      }
      else
      {
         parcoursprec = parcours;
         parcours = parcours -> suiv ;
      }
   }
}


void Clean_List_ModuleUsed_Var()
{
   listvar *parcours;
   listvar *parcours1;
   listvar *parcoursprec;
   int remove;

   parcoursprec = (listvar *)NULL;
   parcours = List_ModuleUsed_Var;
   while ( parcours )
   {
      /*                                                                      */
      parcours1 = List_GlobalParameter_Var;
      remove = 0 ;
      while ( parcours1 && remove == 0 )
      {
         if ( !strcasecmp(parcours->var->v_nomvar,parcours1->var->v_nomvar)
            ) remove = 1 ;
         else parcours1 = parcours1 -> suiv;
      }
      /************************************************************************/
      /*                         Remove                                       */
      /************************************************************************/
      if ( remove == 1 )
      {
         if ( parcours == List_ModuleUsed_Var )
         {
           List_ModuleUsed_Var = List_ModuleUsed_Var -> suiv;
           parcours = List_ModuleUsed_Var;
         }
         else
         {
            parcoursprec->suiv = parcours->suiv;
            parcours = parcoursprec -> suiv ;
         }
      }
      else
      {
         parcoursprec = parcours;
         parcours = parcours -> suiv ;
      }
   }
}

void Clean_List_SubroutineDeclaration_Var()
{
   listvar *parcours;
   listvar *parcours1;
   listvar *parcoursprec;
   int out ;

   parcoursprec = (listvar *)NULL;
   parcours = List_SubroutineDeclaration_Var;
   while ( parcours )
   {
      parcours1 = List_FunctionType_Var;
      out = 0 ;
      while ( parcours1 && out == 0 )
      {
         if ( !strcasecmp(parcours->var->v_subroutinename,parcours1->var->v_subroutinename) &&
              !strcasecmp(parcours->var->v_nomvar,parcours1->var->v_nomvar)
            ) out = 1;
         else parcours1 = parcours1->suiv;
      }
      if ( out == 0 )
      {
         parcours1 = List_SubroutineArgument_Var;
         out = 0 ;
         while ( parcours1 && out == 0 )
         {
            if ( !strcasecmp(parcours->var->v_subroutinename,parcours1->var->v_subroutinename) &&
                 !strcasecmp(parcours->var->v_nomvar,parcours1->var->v_nomvar)
               ) out = 1;
            else parcours1 = parcours1->suiv;
         }
      }

      if ( out == 1 )
      {
          if ( parcours == List_SubroutineDeclaration_Var )
          {
             List_SubroutineDeclaration_Var =
                                         List_SubroutineDeclaration_Var -> suiv;
             parcours = List_SubroutineDeclaration_Var;
          }
          else
          {
             parcoursprec->suiv = parcours->suiv;
             parcours = parcoursprec->suiv;
          }
      }
      else
      {
         parcoursprec = parcours;
         parcours = parcours -> suiv;
      }
   }
}

void Clean_List_Global_Var()
{
   listvar *parcours;
   listvar *parcours2;
   listvar *parcoursprec;
   listvar *parcours2prec;

   parcoursprec = (listvar *)NULL;
   parcours2prec = (listvar *)NULL;
   parcours = List_Global_Var;
   while ( parcours )
   {
      if ( parcours->var->v_VariableIsParameter == 1 )
      {
         /* remove                                                            */
         if ( parcours == List_Global_Var )
         {
            List_Global_Var = List_Global_Var->suiv;
            free(parcours);
            parcours = List_Global_Var;
         }
         else
         {
            parcoursprec->suiv = parcours->suiv;
            free(parcours);
            parcours = parcoursprec->suiv;
         }
      }
      else
      {
         parcoursprec = parcours;
         parcours = parcours->suiv;
      }
   }
   /* looking for sevral declaration of the same variable                     */
   parcours = List_Global_Var;
   while ( parcours )
   {
      parcours2prec = parcours;
      parcours2 = parcours->suiv;
      while ( parcours2 )
      {
         if ( !strcasecmp(parcours->var->v_nomvar,
                         parcours2->var->v_nomvar)     &&
              !strcasecmp(parcours->var->v_modulename,
                         parcours2->var->v_modulename) )
         {
            Merge_Variables(parcours->var,parcours2->var);
            /* remove var from the parcours2                                  */
            parcours2prec ->suiv = parcours2->suiv;
            free(parcours2);
            parcours2 = parcours2prec ->suiv;
         }
         else
         {
            parcours2prec = parcours2;
            parcours2 = parcours2->suiv;
         }
      }
      parcours = parcours->suiv;
   }
}
/******************************************************************************/
/*                             ListClean                                      */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void ListClean()
{
   listvar *newvar;

   Clean_List_ModuleUsed_Var();
   Clean_List_UsedInSubroutine_Var();
   Clean_List_SubroutineDeclaration_Var();

   newvar = (listvar *)NULL;
/*   newvar = List_Common_Var;*/
   while(newvar)
   {
      printf("----- %s --- %s ---%s---%s---\n",newvar->var->v_nomvar,
         newvar->var->v_commonname,
         newvar->var->v_readedlistdimension,
         newvar->var->v_subroutinename
      );
      newvar = newvar -> suiv;
      printf("+++++++++++++++++++++++++\n");
   }

}


/******************************************************************************/
/*                             ListUpdate                                     */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void ListUpdate()
{
    listvar *newvar;

    Update_List_Subroutine_Var(List_SubroutineArgument_Var);
    Update_List_Subroutine_Var(List_FunctionType_Var);
    Update_List_Var(List_Parameter_Var);
    Update_List_Var(List_Dimension_Var);
    Update_List_Var(List_Data_Var);
    Update_List_Var(List_Save_Var);
    Update_List_Var(List_GlobalParameter_Var);
    Update_List_Var(List_Common_Var);
    Update_List_Var(List_SubroutineDeclaration_Var);
    Update_List_Var(List_UsedInSubroutine_Var);
    Update_List_From_Common_Var(List_UsedInSubroutine_Var);
    Update_List_From_Common_Var(List_SubroutineDeclaration_Var);
    Update_NotGridDepend_Var(List_NotGridDepend_Var);

    newvar = (listvar * ) NULL;
//   newvar = List_Common_Var;
//   newvar = List_UsedInSubroutine_Var;
//   newvar = List_Data_Var;
    while ( newvar )
    {
        printf("++++ %s - %s - %s - %d - %s - %s\n",
                newvar->var->v_modulename,
                newvar->var->v_subroutinename,
                newvar->var->v_nomvar,
                newvar->var->v_VariableIsParameter,
                newvar->var->v_typevar,
                newvar->var->v_initialvalue->n_name );
        newvar = newvar->suiv;
    }
}

void GiveTypeOfVariables()
{
   listvar *parcours;

   /*                                                                         */
   parcours = List_Common_Var;
   while ( parcours )
   {
      if ( !strcasecmp(parcours->var->v_typevar,"") )
      {
         if ( IsVariableReal(parcours->var->v_nomvar) == 1 )
                                        strcpy(parcours->var->v_typevar,"REAL");
         else strcpy(parcours->var->v_typevar,"INTEGER");
         parcours->var->v_catvar = get_cat_var(parcours->var);
      }
      parcours = parcours -> suiv ;
   }
   /*                                                                         */
   parcours = List_UsedInSubroutine_Var;
   while ( parcours )
   {
      if ( !strcasecmp(parcours->var->v_typevar,"") )
      {
         if ( IsVariableReal(parcours->var->v_nomvar) == 1 )
                                        strcpy(parcours->var->v_typevar,"REAL");
         else strcpy(parcours->var->v_typevar,"INTEGER");
         parcours->var->v_catvar = get_cat_var(parcours->var);
      }
      parcours = parcours -> suiv ;
   }
   /*                                                                         */
   parcours = List_SubroutineArgument_Var;
   while ( parcours )
   {
      if ( !strcasecmp(parcours->var->v_typevar,"") )
      {
         if ( IsVariableReal(parcours->var->v_nomvar) == 1 )
                                        strcpy(parcours->var->v_typevar,"REAL");
         else strcpy(parcours->var->v_typevar,"INTEGER");
         parcours->var->v_catvar = get_cat_var(parcours->var);
      }
      parcours = parcours -> suiv ;
   }
   /*                                                                         */
   parcours = List_SubroutineDeclaration_Var;
   while ( parcours )
   {
      if ( !strcasecmp(parcours->var->v_typevar,"") )
      {
         if ( IsVariableReal(parcours->var->v_nomvar) == 1 )
                                        strcpy(parcours->var->v_typevar,"REAL");
         else strcpy(parcours->var->v_typevar,"INTEGER");
         parcours->var->v_catvar = get_cat_var(parcours->var);
      }
      parcours = parcours -> suiv ;
   }
   
   /*                                                                         */
   parcours = List_Parameter_Var;
   while ( parcours )
   {
      if ( !strcasecmp(parcours->var->v_typevar,"") )
      {
         if ( IsVariableReal(parcours->var->v_nomvar) == 1 )
                                        strcpy(parcours->var->v_typevar,"REAL");
         else strcpy(parcours->var->v_typevar,"INTEGER");
         parcours->var->v_catvar = get_cat_var(parcours->var);
      }
      parcours = parcours -> suiv ;
   }
   
   /*                                                                         */
   parcours = List_GlobalParameter_Var;
   while ( parcours )
   {
      if ( !strcasecmp(parcours->var->v_typevar,"") )
      {
         if ( IsVariableReal(parcours->var->v_nomvar) == 1 )
                                        strcpy(parcours->var->v_typevar,"REAL");
         else strcpy(parcours->var->v_typevar,"INTEGER");
         parcours->var->v_catvar = get_cat_var(parcours->var);
      }
      parcours = parcours -> suiv ;
   }

}



void Sort_List_SubroutineArgument_Var()
{
   listvar *parcours;
   listvar *parcours1;
   int position;
   int out;
   char name_sub[LONG_M];

   parcours = List_SubroutineArgument_Var;
   position = 1;
   while ( parcours )
   {
      parcours1 = List_SubroutineDeclaration_Var;
      out = 0;
      while ( parcours1 && out == 0 )
      {
         if ( !strcasecmp(parcours->var->v_nomvar,
                         parcours1->var->v_nomvar)  &&
              !strcasecmp(parcours->var->v_subroutinename,
                         parcours1->var->v_subroutinename)
             )
         {
            parcours1->var->v_positioninblock = position;
            position = position +1 ;
            out = 1;
         }
         else parcours1 = parcours1->suiv;
      }
      parcours = parcours->suiv;
   }
   /*                                                                         */
   parcours = List_SubroutineDeclaration_Var;
   strcpy(name_sub,"");
   while ( parcours )
   {
      if ( !strcasecmp(name_sub,"") )
      {
         strcpy(name_sub,parcours->var->v_subroutinename);
         position = 1;
      }

      if ( parcours->var->v_positioninblock != 0 )
      {
         parcours1 = List_SubroutineArgument_Var;
         out = 0;
         while ( parcours1 && out == 0 )
         {
            if ( !strcasecmp(parcours->var->v_nomvar,
                            parcours1->var->v_nomvar)  &&
                 !strcasecmp(parcours->var->v_subroutinename,
                            parcours1->var->v_subroutinename)
                )
            {
               parcours1->var->v_positioninblock = position;
               position = position +1 ;
               out = 1;
            }
            else parcours1 = parcours1->suiv;
         }
      }
      if ( parcours->suiv )
         if ( strcasecmp(name_sub,parcours->suiv->var->v_subroutinename) )
            strcpy(name_sub,"");
      parcours = parcours->suiv;
   }

}



/******************************************************************************/
/*                      IndiceTabvars_Global_Var_Treated                      */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void IndiceTabvars_Global_Var_Treated(char *nom)
{
   listvar *parcours;
   listvar *parcoursprec;
   listvar *parcours1;
   listvar *List_ModuleUsed_Var;
   listindice *newindice;
   int out;

   parcoursprec = (listvar *)NULL;

   if ( todebug == 1 ) printf("MODULE Treated %s \n",nom);

   List_ModuleUsed_Var = (listvar *)NULL;
   List_ModuleUsed_Var = Readthedependfile(nom,List_ModuleUsed_Var);

   parcours = List_Global_Var;
   while( parcours )
   {
      if ( !strcasecmp(parcours->var->v_modulename,nom) )
      {
         parcours1 = List_ModuleUsed_Var;
         out = 0 ;
         while ( parcours1 && out == 0 )
         {
            if ( !strcasecmp(parcours->var->v_nomvar,parcours1->var->v_nomvar)
               ) out = 1;
            else
            {
               parcoursprec = parcours1 ;
               parcours1 = parcours1->suiv;
            }
         }
         /* if we found the var Module name in the old list                   */
         if ( out == 1 )
         {
            Merge_Variables(parcours->var,parcours1->var);
            /* Remove this variable from the List_ModuleUsed_Var              */
            if ( parcours1 == List_ModuleUsed_Var )
            {
               List_ModuleUsed_Var = List_ModuleUsed_Var->suiv ;
            }
            else
            {
               parcoursprec->suiv = parcours1->suiv;
               free(parcours1);
               parcours1 = parcoursprec->suiv;
            }
         }
         else
         /* if we do not found the var Module name in the old list            */
         {
  //       update_indicemaxtabvars(parcours->var,Listofavailableindices);
         update_indicemaxtabvars(parcours->var,Listofavailableindices_glob);
  //          if ( Listofavailableindices )
  //          {
  //             parcours->var->v_indicetabvars = Listofavailableindices ->
  //                                                                     i_indice;
  //             if ( Listofavailableindices->suiv )
  //                        Listofavailableindices = Listofavailableindices->suiv;
  //             else
  //                        Listofavailableindices = (listindice *)NULL;
  //          }
  //          else
  //          {
  //             indicemaxtabvars = indicemaxtabvars + 1 ;
  //             parcours->var->v_indicetabvars = indicemaxtabvars;
  //          }
         }
      }
      parcours = parcours->suiv;
   }
   /* if List_ModuleUsed_Var is not empty, some var have been removed from    */
   /*    the last treatement                                                  */
  parcours1 = List_ModuleUsed_Var;
  while ( parcours1 )
  {
     newindice=(listindice *) calloc(1,sizeof(listindice));
     newindice -> i_indice = parcours1 -> var -> v_indicetabvars;
     newindice -> suiv = Listofavailableindices_glob[parcours1 -> var -> v_catvar];
     Listofavailableindices_glob[parcours1 -> var -> v_catvar] = newindice;
     parcours1 = parcours1->suiv;
  }
}
/******************************************************************************/
/*                       IndiceTabvars_Global_Var_No_Treated                  */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void IndiceTabvars_Global_Var_No_Treated(char *nom)
{
   listvar *parcours;

   if ( todebug == 1 ) printf("MODULE No Treated %s \n",nom);

   parcours = List_Global_Var;
   while( parcours )
   {
      if ( !strcasecmp(parcours->var->v_modulename,nom) &&
           parcours->var->v_VariableIsParameter == 0    &&
           parcours->var->v_notgrid == 0
          )
      {
         indicemaxtabvars[parcours->var->v_catvar] = indicemaxtabvars[parcours->var->v_catvar] + 1 ;
         parcours->var->v_indicetabvars = indicemaxtabvars[parcours->var->v_catvar];
      }
      parcours = parcours->suiv;
   }
}


void UpdateTheRemainingList(listvar *record)
{
   listvar *parcours;

   parcours = record;
   while ( parcours )
   {
      if ( !strcasecmp(parcours->var->v_nomvar,record->var->v_nomvar) &&
           !strcasecmp(parcours->var->v_commonname,record->var->v_commonname)
         )
      {
         strcpy(parcours->var->v_commoninfile,record->var->v_commoninfile);
         Merge_Variables(parcours->var,record->var);
      }
      parcours = parcours -> suiv;
   }
}



/******************************************************************************/
/*                      IndiceTabvars_Common_Var_Treated                      */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void IndiceTabvars_Common_Var_Treated(char *nom)
{
   listvar *parcours;
   listvar *parcours1;
   listvar *List_CommonUsed_Var;
   listindice *newindice;
   int out;

   if ( todebug == 1 ) printf("COMMON Treated %s \n",nom);

   List_CommonUsed_Var = (listvar *)NULL;
   List_CommonUsed_Var = Readthedependfile(nom,List_CommonUsed_Var);

   parcours = List_Common_Var;
   while( parcours )
   {
      if ( !strcasecmp(parcours->var->v_commonname,nom) )
      {
         parcours1 = List_CommonUsed_Var;
         out = 0 ;
         while ( parcours1 && out == 0 )
         {

            if ( !strcasecmp(parcours1->var->v_commonname,nom) &&
                 !strcasecmp(parcours->var->v_nomvar,parcours1->var->v_nomvar)
               ) out = 1;
            else
            {
               parcours1 = parcours1->suiv;
            }
         }
         /* if we found the var common name in the old list                   */
         if ( out == 1 )
         {
            strcpy(parcours->var->v_commoninfile,
                   parcours1->var->v_commoninfile);
            Merge_Variables(parcours->var,parcours1->var);
         }
         else
         /* if we do not found the var common name in the old list            */
         {
    //     update_indicemaxtabvars(parcours->var,Listofavailableindices);
         update_indicemaxtabvars(parcours->var,Listofavailableindices_glob);
    //        if ( Listofavailableindices )
    //        {
    //           parcours->var->v_indicetabvars = Listofavailableindices ->
    //                                                                   i_indice;
    //           if ( Listofavailableindices->suiv )
    //                      Listofavailableindices = Listofavailableindices->suiv;
    //           else
    //                      Listofavailableindices = (listindice *)NULL;
    //        }
    //        else
    //        {
    //           indicemaxtabvars = indicemaxtabvars + 1 ;
    //           parcours->var->v_indicetabvars = indicemaxtabvars;
    //        }
         }
         /* Look in the remaining list in the variable is define              */
         UpdateTheRemainingList(parcours);
      }
      parcours = parcours->suiv;
   }
   /* if List_CommonUsed_Var is not empty, some var have been removed from    */
   /*    the last treatement                                                  */
  parcours1 = List_CommonUsed_Var;
  while ( parcours1 )
  {
     if ( parcours1 -> var -> v_indicetabvars == 0 )
     {
        newindice=(listindice *) calloc(1,sizeof(listindice));
        newindice -> i_indice = parcours1 -> var -> v_indicetabvars;
        newindice -> suiv = Listofavailableindices_glob[parcours1 -> var -> v_catvar];
        Listofavailableindices_glob[parcours1 -> var -> v_catvar] = newindice;
     }
     parcours1 = parcours1->suiv;
  }
}

void update_indicemaxtabvars(variable *var,listindice **Listofindices)
{
            if ( Listofindices[var->v_catvar] )
            {
               var->v_indicetabvars = Listofindices[var->v_catvar] -> i_indice;
               if ( Listofindices[var->v_catvar]->suiv )
                          Listofindices[var->v_catvar] = Listofindices[var->v_catvar]->suiv;
               else
                          Listofindices[var->v_catvar] = (listindice *)NULL;
            }
            else
            {
               indicemaxtabvars[var->v_catvar] = indicemaxtabvars[var->v_catvar] + 1 ;
               var->v_indicetabvars = indicemaxtabvars[var->v_catvar];
            }

}

/******************************************************************************/
/*                     IndiceTabvars_Common_Var_No_Treated                    */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void IndiceTabvars_Common_Var_No_Treated(char *nom)
{
   listvar *parcours;
   listvar *parcours2;

   if ( todebug == 1 ) printf("COMMON No Treated %s \n",nom);

   parcours = List_Common_Var;
   while( parcours )
   {
      if ( !strcasecmp(parcours->var->v_commonname,nom) &&
           strcasecmp(parcours->var->v_subroutinename,"") &&
           parcours->var->v_indicetabvars == 0
          )
      {
            /* The type may has not been given if the variable was only declared with dimension */

            if ( !strcasecmp(parcours->var->v_typevar,"") )
            {
                  if ( IsVariableReal(parcours->var->v_nomvar) == 1 )
                                        strcpy(parcours->var->v_typevar,"REAL");
                  else strcpy(parcours->var->v_typevar,"INTEGER");
                  parcours->var->v_catvar = get_cat_var(parcours->var);
             }
             
         indicemaxtabvars[parcours->var->v_catvar] = indicemaxtabvars[parcours->var->v_catvar] + 1 ;
         parcours->var->v_indicetabvars = indicemaxtabvars[parcours->var->v_catvar];
         parcours2 = parcours;
         while ( parcours2 )
         {
            if ( !strcasecmp(parcours->var->v_nomvar,
                             parcours2->var->v_nomvar) &&
                 !strcasecmp(parcours->var->v_commonname,
                             parcours2->var->v_commonname)
               )
               parcours2->var->v_indicetabvars = parcours->var->v_indicetabvars;
            parcours2 = parcours2->suiv;
         }
      }
      parcours = parcours->suiv;
   }
}


/******************************************************************************/
/*                       IndiceTabvarsIdentification                          */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void IndiceTabvarsIdentification()
{
   listnom *parcours_nom;

   /* Identification of tabvars indices in List_Global_Var                    */
   parcours_nom = List_NameOfModule;
   while ( parcours_nom )
   {
      if ( is_dependfile_created(parcours_nom->o_nom) == 1 )
      {
         IndiceTabvars_Global_Var_Treated(parcours_nom->o_nom);
      }
      else
      {
         IndiceTabvars_Global_Var_No_Treated(parcours_nom->o_nom);
      }
      parcours_nom = parcours_nom -> suiv;
   }
   /* Identification of tabvars indices in List_Common_Var                    */
   parcours_nom = List_NameOfCommon;
   while ( parcours_nom )
   {
      if ( is_dependfile_created(parcours_nom->o_nom) == 1 )
      {
         IndiceTabvars_Common_Var_Treated(parcours_nom->o_nom);
      }
      else
      {
         IndiceTabvars_Common_Var_No_Treated(parcours_nom->o_nom);
      }
      parcours_nom = parcours_nom -> suiv;
   }

}

void New_Allocate_Subroutine_Is_Necessary()
{
   listnom *parcours_nom;
   listvar *parcours;
   int out;

   parcours_nom = List_NameOfModule;
   while ( parcours_nom )
   {
      /*                                                                      */
      parcours = List_Global_Var;
      out = 0 ;
      while( parcours && out == 0 )
      {
         if ( !strcasecmp(parcours->var->v_modulename,parcours_nom->o_nom) &&
              !strcasecmp(parcours->var->v_subroutinename,"")            &&
              parcours->var->v_VariableIsParameter == 0                  &&
              ( parcours->var->v_allocatable == 0 || !strcasecmp(parcours->var->v_typevar,"type"))      &&
              parcours->var->v_notgrid == 0                              &&
              ( ( parcours->var->v_nbdim != 0 || !strcasecmp(parcours->var->v_typevar,"type") )
              || parcours->var->v_initialvalue )
            )
         {
            out = 1;
         }
         else parcours = parcours -> suiv;
      }
      if ( out )
      {
         parcours_nom->o_val = 1 ;
      }
      parcours_nom = parcours_nom -> suiv;
   }
}

void New_Allocate_Subroutine_For_Common_Is_Necessary()
{
   listnom *parcours_nom;
   listvar *parcours;
   int out;

   parcours_nom = List_NameOfCommon;
   while ( parcours_nom )
   {
      parcours = List_Common_Var;
      out = 0 ;
      while( parcours && out == 0 )
      {
         if ( !strcasecmp(parcours->var->v_commonname,parcours_nom->o_nom)  &&
              strcasecmp(parcours->var->v_subroutinename,"")                &&
              !strcasecmp(parcours->var->v_commoninfile,cur_filename)       &&
              ( ( parcours->var->v_nbdim != 0 || !strcasecmp(parcours->var->v_typevar,"type") )
              || parcours->var->v_initialvalue )
            )
         {
            out = 1;
         }
         else parcours = parcours -> suiv;
      }
      if ( out == 1 )
      {
         parcours_nom->o_val = 1 ;
      }
      parcours_nom = parcours_nom -> suiv;
   }
}

void NewModule_Creation_0()
{
   listnom *parcours_nom;

   parcours_nom = List_NameOfCommon;
   while ( parcours_nom )
   {
      if ( parcours_nom->o_val == 1 )
      {
         fprintf(fortran_out, "      module %s\n\n", parcours_nom->o_nom);
         WriteUsemoduleDeclaration(parcours_nom->o_subroutinename);
         fprintf(fortran_out, "        implicit none\n");
         fprintf(fortran_out, "        public :: Alloc_agrif_%s\n", parcours_nom->o_nom);
         fprintf(fortran_out, "      contains\n");
         fprintf(fortran_out, "      subroutine Alloc_agrif_%s(Agrif_Gr)\n", parcours_nom->o_nom);
         fprintf(fortran_out, "        use Agrif_Util\n");
         fprintf(fortran_out, "        type(Agrif_grid), pointer :: Agrif_Gr\n");
         fprintf(fortran_out, "        integer :: i\n");
         fprintf(fortran_out, "#include \"alloc_agrif_%s.h\"\n", parcours_nom->o_nom);
         fprintf(fortran_out, "      end subroutine Alloc_agrif_%s\n", parcours_nom->o_nom);
         fprintf(fortran_out, "      end module %s\n", parcours_nom->o_nom);
         /* List all Call Alloc_agrif                                      */
         Add_Subroutine_For_Alloc(parcours_nom->o_nom);
      }
      parcours_nom = parcours_nom->suiv;
   }
}

void UpdateList_SubroutineWhereAgrifUsed()
{
   listnom *parcours;
   listusemodule *parcours1;
   listallocate *parcours2;
   listname *parcours3;
   listvar *parcours4;
   int out;
   char name_module[LONG_M];

   /* We should integrate allocate and pointer variables                      */
//    parcours2 = List_Allocate_Var;
//    while ( parcours2 )
//    {
//       parcours4 = List_UsedInSubroutine_Var;
//       out = 0 ;
//       while ( parcours4 && out == 0 )
//       {
//          if ( !strcasecmp(parcours2->a_nomvar,parcours4->var->v_nomvar) )
//          {
//             Add_SubroutineWhereAgrifUsed_1(parcours4->var->v_subroutinename, parcours4->var->v_modulename);
//             out = 1;
//          }
//          else parcours4 = parcours4 -> suiv ;
//       }
//       parcours2 = parcours2->suiv;
//    }
//
//    parcours3 = List_Pointer_Var;
//    while ( parcours3 )
//    {
//       parcours4 = List_UsedInSubroutine_Var;
//       out = 0 ;
//       while ( parcours4 && out == 0 )
//       {
//          if ( !strcasecmp(parcours3->n_name, parcours4->var->v_nomvar) )
//          {
//             Add_SubroutineWhereAgrifUsed_1(parcours4->var->v_subroutinename, parcours4->var->v_modulename);
//             out = 1;
//          }
//          else parcours4 = parcours4 -> suiv ;
//       }
//       parcours3 = parcours3 -> suiv;
//    }
//    parcours4 = List_UsedInSubroutine_Var;
//    while ( parcours4 )
//    {
//       if ( parcours4->var->v_allocatable == 1 && strcasecmp(parcours4->var->v_typevar,"type"))
//       {
//          Add_SubroutineWhereAgrifUsed_1(parcours4->var->v_subroutinename, parcours4->var->v_modulename);
//       }
//       parcours4 = parcours4 -> suiv ;
//    }

   parcours = List_SubroutineWhereAgrifUsed;
   while ( parcours )
   {
      parcours1 = List_NameOfModuleUsed;
      out = 0 ;
      strcpy(name_module,"");
      while ( parcours1 && out == 0 )
      {
         if ( !strcasecmp(parcours->o_nom,parcours1->u_cursubroutine) &&
              !strcasecmp(parcours1->u_usemodule,"Agrif_Util")
            ) out = 1;
         else
         {
            if ( !strcasecmp(parcours->o_nom,parcours1->u_cursubroutine) )
            {
               strcpy(name_module, parcours->o_module);
            }
            parcours1 = parcours1->suiv;
         }
      }
      if ( out == 0 )   /* we should look in the module declaration */
      {
         parcours1 = List_NameOfModuleUsed;
         out = 0 ;
         while ( parcours1 && out == 0 )
         {
            if ( !strcasecmp(name_module,parcours1->u_modulename) &&
                 !strcasecmp(parcours1->u_cursubroutine,"")&&
                 !strcasecmp(parcours1->u_usemodule,"Agrif_Util")
               ) out = 1;
            else parcours1 = parcours1->suiv;
         }
      }
      if ( out == 0 ) parcours->o_val = 1;

      parcours = parcours->suiv;
   }
}


void UpdateList_UsedInSubroutine_With_dimension()
{
   listvar *parcours;

   parcours = List_UsedInSubroutine_Var;
   while ( parcours )
   {
      if ( parcours->var->v_nbdim != 0 )
      {
         strcpy(subroutinename,parcours->var->v_subroutinename);
         DecomposeTheName(parcours->var->v_readedlistdimension);
         strcpy(subroutinename,"");
      }
      parcours = parcours -> suiv;
   }
}

void Affiche(listvar *in_parcours)
{
/*   parcours = List_Global_Var;                  */
/*   parcours = List_SubroutineDeclaration_Var;   */
/*   parcours = List_SubroutineArgument_Var;      */
/*   parcours = List_FunctionType_Var;            */
/*   parcours = List_Data_Var;                    */
/*   parcours = List_Save_Var;                    */
/*   parcours = List_UsedInSubroutine_Var;        */
/*   parcours = List_Parameter_Var;               */
/*   parcours = List_GlobalParameter_Var;         */
/*   parcours = List_NotGridDepend_Var;           */
/*   parcours = List_Common_Var;                  */
   listvar *parcours = in_parcours;

   while( parcours )
   {
      printf("modulename     - %s \n", parcours->var->v_modulename);
      printf("subroutinename - %s \n", parcours->var->v_subroutinename);
      printf("nomvar         - %s \n", parcours->var->v_nomvar);
      printf("commonname     - %s \n", parcours->var->v_commonname);
      printf("commoninfile   - %s \n", parcours->var->v_commoninfile);
      printf("typevar        - %s \n", parcours->var->v_typevar);
      printf("catvar         - %d \n", parcours->var->v_catvar);
      printf("indicetabvars  - %d \n", parcours->var->v_indicetabvars);
      printf("isparameter    - %d \n", parcours->var->v_VariableIsParameter);
      printf("module         - %d \n", parcours->var->v_module);
      printf("save           - %d \n", parcours->var->v_save);
      printf("notgrid        - %d \n", parcours->var->v_notgrid);
      printf("nbdim          - %d \n", parcours->var->v_nbdim);
      printf("common         - %d \n", parcours->var->v_common);
      printf("dimensiongiven - %d \n", parcours->var->v_dimensiongiven);
      printf("dimsempty      - %d \n", parcours->var->v_dimsempty);
      printf("initialvalue   - %s \n", parcours->var->v_initialvalue->n_name);
      printf("readedlistdim  - %s \n", parcours->var->v_readedlistdimension);
      printf("-------------------------------------\n");

      parcours = parcours -> suiv ;
   }
   if ( todebug == 1 ) printf("Indicemaxtabvars = %d \n",indicemaxtabvars[0]);
}

int SubInList_ContainsSubroutine()
{
   int out;
   listnom *parcours;

   out = 0 ;
   parcours = List_ContainsSubroutine;
   while ( parcours && out == 0 )
   {
      if ( !strcasecmp(parcours->o_nom,subroutinename) ) out = 1 ;
      else parcours = parcours -> suiv;
   }

   return out;
}
