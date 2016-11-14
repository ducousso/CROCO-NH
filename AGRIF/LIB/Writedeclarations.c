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
/*                         WriteBeginDeclaration                              */
/******************************************************************************/
/* This subroutine is used to write the begin of a declaration                */
/* taken in a variable record                                                 */
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*       integer variable ----------->   INTEGER                              */
/*                                                                            */
/******************************************************************************/
void WriteBeginDeclaration(variable *v, char line[LONG_M], int visibility)
{
  char tmpligne[LONG_M];
  int precision_given ;

  if ( !strcasecmp(v->v_typevar,"") )
  {
     printf("# WARNING : The type of the variable %s is unknown.\n", v->v_nomvar);
     printf("#           CONV should define a type\n");
  }

  sprintf(line, "%s", v->v_typevar);
  if ( v->v_c_star == 1 ) strcat(line, "*");

  /* We should give the precision of the variable if it has been given        */
  precision_given = 0;
  
  if ( strcasecmp(v->v_precision,"") )
  {
     sprintf(tmpligne, "(%s)", v->v_precision);
     Save_Length(tmpligne, 49);
     strcat(line, tmpligne);
     precision_given = 1;
  }

  if (strcasecmp(v->v_dimchar,""))
  {
     sprintf(tmpligne,"(%s)",v->v_dimchar);
     Save_Length(tmpligne, 49);
     strcat(line,tmpligne);
  }

  if ((precision_given == 0) && ( strcasecmp(v->v_nameinttypename,"") ))
  {
     sprintf(tmpligne,"*%s",v->v_nameinttypename);
     Save_Length(tmpligne, 49);
     strcat(line,tmpligne);
  }
  if (strcasecmp (v->v_IntentSpec, ""))
  {
     sprintf(tmpligne,", intent(%s)", v->v_IntentSpec);
     Save_Length(tmpligne, 49);
     strcat(line,tmpligne);
  }
  if ( v->v_VariableIsParameter ) strcat(line, ", parameter");
  if ( visibility )
  {
      if ( v->v_PublicDeclare  )  strcat(line, ", public");
      if ( v->v_PrivateDeclare )  strcat(line, ", private");
  }
  if ( v->v_ExternalDeclare ) strcat(line, ", external");
  if ( v->v_allocatable     ) strcat(line, ", allocatable");
  if ( v->v_target          ) strcat(line, ", target");
  if ( v->v_optionaldeclare ) strcat(line, ", optional");
  if ( v->v_pointerdeclare  ) strcat(line, ", pointer");
  Save_Length(line, 45);
}


/******************************************************************************/
/*                         WriteScalarDeclaration                             */
/******************************************************************************/
/* This subroutine is used to write a scalar declaration                      */
/* taken in a variable record                                                 */
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*       integer variable ----------->   INTEGER :: VARIABLE                  */
/*                                                                            */
/******************************************************************************/
void WriteScalarDeclaration( variable *v, char line[LONG_M])
{
    strcat(line, " :: ");
    strcat(line, v->v_nomvar);

    if ( strcasecmp(v->v_vallengspec, "") ) strcat(line,v->v_vallengspec);
    if ( v->v_VariableIsParameter )
    {
        strcat(line," = ");
        strcat(line, v->v_initialvalue->n_name);
    }
    Save_Length(line, 45);
}

/******************************************************************************/
/*                         WriteTableDeclaration                              */
/******************************************************************************/
/* This subroutine is used to write a Table declaration                       */
/* taken in a variable record                                                 */
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*  integer variable(nb) ----------->                                         */
/*                      INTEGER, DIMENSION(1:nb) :: variable                  */
/*                                                                            */
/******************************************************************************/
void WriteTableDeclaration(variable * v,char ligne[LONG_M],int tmpok)
{
    char newname[LONG_M];

    strcat (ligne, ", dimension(");

    if ( v->v_dimensiongiven == 1 && tmpok == 1 )   strcat(ligne,v->v_readedlistdimension);
    if ( v->v_dimensiongiven == 1 && tmpok == 0 )
    {
        strcpy(newname,ChangeTheInitalvaluebyTabvarsName(v->v_readedlistdimension,List_Global_Var));
        if ( !strcasecmp(newname,"") ) strcat(newname,v->v_readedlistdimension);

        strcpy(newname,ChangeTheInitalvaluebyTabvarsName(newname,List_Common_Var));
        if ( !strcasecmp(newname,"") ) strcat(newname,v->v_readedlistdimension);

        strcpy(newname,ChangeTheInitalvaluebyTabvarsName(newname,List_ModuleUsed_Var));
        if ( !strcasecmp(newname,"") ) strcat(newname,v->v_readedlistdimension);

        Save_Length(newname,47);
        strcat(ligne,newname);
    }
    strcat(ligne, ") :: ");
    strcat(ligne, v->v_nomvar);
    if ( strcasecmp(vallengspec,"") ) strcat(ligne,v->v_vallengspec);

    if ( v->v_VariableIsParameter == 1 )
    {
        strcat(ligne," = ");
        strcat(ligne,v->v_initialvalue->n_name);
    }
    Save_Length(ligne,45);
}

/******************************************************************************/
/*                        WriteVarDeclaration                                 */
/******************************************************************************/
/* This subroutine is used to write the initial declaration in the file       */
/* fileout of a variable                                                      */
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*  integer variable(nb) ----------->                                         */
/*                      INTEGER, DIMENSION(1:nb),Pointer :: variable          */
/*                                                                            */
/******************************************************************************/
void WriteVarDeclaration( variable *v, FILE *fileout, int value, int visibility )
{
  FILE *filecommon;
  char ligne[LONG_M];

  filecommon = fileout;

  if ( v->v_save == 0 || inmodulemeet == 0 )
  {
     WriteBeginDeclaration(v, ligne, visibility);

     if ( v->v_nbdim == 0 )
        WriteScalarDeclaration(v, ligne);
     else
        WriteTableDeclaration(v, ligne, value);

     if ( v->v_VariableIsParameter != 1 && v->v_initialvalue)
     {
        strcat(ligne," = ");
        strcat(ligne,v->v_initialvalue->n_name);
     }
     tofich(filecommon, ligne, 1);
  }
  else
    printf("-- in writevardeclaration : |%s| -- MHCHECK\n", v->v_nomvar);
  Save_Length(ligne,45);
}


void WriteLocalParamDeclaration(FILE* tofile)
{
    listvar *parcours;

    parcours = List_Parameter_Var;
    while ( parcours )
    {
        if ( !strcasecmp(parcours->var->v_subroutinename,subroutinename) )
        {
            WriteVarDeclaration(parcours->var, tofile, 0, 1);
        }
        parcours = parcours -> suiv;
    }
}

void WriteFunctionDeclaration(FILE* tofile, int value)
{
    listvar *parcours;

    parcours = List_FunctionType_Var;
    while ( parcours )
    {
    if (!strcmp(parcours->var->v_typevar, ""))
    {
     /* Default type*/
          if ( IsVariableReal(parcours->var->v_nomvar) == 1 )
                                         strcpy(parcours->var->v_typevar,"REAL");
          else strcpy(parcours->var->v_typevar,"INTEGER");
     }
        if ( !strcasecmp(parcours->var->v_subroutinename, subroutinename) &&
              strcasecmp(parcours->var->v_typevar, "") )
        {
            WriteVarDeclaration(parcours->var, tofile, value, 1);
        }
        parcours = parcours -> suiv;
    }
}

void WriteSubroutineDeclaration(int value)
{
    listvar *parcours;
    variable *v;

    parcours = List_SubroutineDeclaration_Var;
    while ( parcours )
    {
        v = parcours->var;
        if ( !strcasecmp(v->v_subroutinename, subroutinename)   &&
             (v->v_save == 0)                                   &&
             (v->v_VariableIsParameter == 0)                    &&
             (v->v_common == 0) )
        {
            WriteVarDeclaration(v, fortran_out, value, 1);
        }
        else if ( !strcasecmp(v->v_subroutinename, subroutinename)  &&
                  (v->v_save == 0)                                  &&
                  (v->v_VariableIsParameter == 0)                   &&
                  (v->v_common == 0) )
        {
            WriteVarDeclaration(v, fortran_out, value, 1);
        }
        parcours = parcours -> suiv;
    }
}

void WriteArgumentDeclaration_beforecall()
{
    int position;
    listnom *neededparameter;
    FILE *paramtoamr;
    listvar *parcours;
    variable *v;
    char *ligne;
    size_t line_length;
    int res;
    int global_check;

    ligne = (char*) calloc(LONG_M, sizeof(char));
    line_length = LONG_M;
    
    global_check = 0;
   
   
    fprintf(fortran_out,"#include \"Param_BeforeCall_%s.h\"\n",subroutinename);

    sprintf(ligne,"Param_BeforeCall_%s.h",subroutinename);
    paramtoamr = open_for_write(ligne);

    neededparameter = (listnom * )NULL;
    position = 1;
    parcours = List_SubroutineArgument_Var;

    while ( parcours )
    {
        v = parcours->var;
        if ( !strcasecmp(v->v_subroutinename, subroutinename) && (v->v_positioninblock == position) )
        {
            position++;
            WriteVarDeclaration(v, fortran_out, 0, 1);
            res = writedeclarationintoamr(List_Parameter_Var, paramtoamr,
                                    v, v->v_subroutinename, &neededparameter, subroutinename, global_check);
            parcours = List_SubroutineArgument_Var;
        }
        else parcours = parcours -> suiv;
    }

    // Write interface for 'Sub_Loop_machin' in 'Param_BeforeCall_machin.h' when outside a module
    if ( IsTabvarsUseInArgument_0() && (inmodulemeet == 0) && (inprogramdeclare == 0) )
    {
        fprintf(paramtoamr, "      interface\n");
        if (isrecursive) sprintf(ligne,"  recursive subroutine Sub_Loop_%s(", subroutinename);
        else             sprintf(ligne,"  subroutine Sub_Loop_%s(", subroutinename);
        WriteVariablelist_subloop(&ligne,&line_length);
        WriteVariablelist_subloop_Def(&ligne,&line_length);
        strcat(ligne,")");

        tofich(paramtoamr,ligne,1);

        listusemodule *parcours_mod;
        parcours_mod = List_NameOfModuleUsed;
        while ( parcours_mod )
        {
            if ( !strcasecmp(parcours_mod->u_cursubroutine, subroutinename) )
            {
                fprintf(paramtoamr, "          use %s\n", parcours_mod->u_usemodule);
            }
            parcours_mod = parcours_mod->suiv;
        }
        fprintf(paramtoamr, "          implicit none\n");
        WriteLocalParamDeclaration(paramtoamr);
        writesub_loopdeclaration_scalar(List_UsedInSubroutine_Var, paramtoamr);
        writesub_loopdeclaration_tab(List_UsedInSubroutine_Var, paramtoamr);
        WriteArgumentDeclaration_Sort(paramtoamr);
        WriteFunctionDeclaration(paramtoamr, 1);

        sprintf(ligne,"  end subroutine Sub_Loop_%s\n", subroutinename);
        tofich(paramtoamr, ligne, 1);
        fprintf(paramtoamr, "      end interface\n");
    }
    fclose(paramtoamr);
}

void WriteArgumentDeclaration_Sort(FILE* tofile)
{
    int position = 1;
    listvar *parcours;

    parcours = List_SubroutineArgument_Var;
    
    while ( parcours )
    {
        if ( !strcasecmp(parcours->var->v_subroutinename, subroutinename) &&
                         parcours->var->v_positioninblock == position )
        {
            position = position + 1;
            WriteVarDeclaration(parcours->var, tofile, 1, 1);
            parcours = List_SubroutineArgument_Var;
        }
        else parcours = parcours -> suiv;
    }

    parcours = List_SubroutineArgument_Var;
    while ( parcours )
    {
        if ( !strcasecmp(parcours->var->v_subroutinename,subroutinename) &&
                         parcours->var->v_positioninblock == 0           &&
                        parcours->var->v_nbdim == 0 )
        {
            WriteVarDeclaration(parcours->var,tofile,1,1);
        }
        parcours = parcours -> suiv;
    }

    parcours = List_SubroutineArgument_Var;
    while ( parcours )
    {
        if ( !strcasecmp(parcours->var->v_subroutinename,subroutinename) &&
                         parcours->var->v_positioninblock == 0           &&
                         parcours->var->v_nbdim != 0 )
        {
            WriteVarDeclaration(parcours->var, tofile, 1, 1);
        }
        parcours = parcours -> suiv;
    }
}

/******************************************************************************/
/*                      writedeclarationintoamr                               */
/******************************************************************************/
/* This subroutine is used to write the declaration of parameters needed in   */
/*    allocation subroutines creates in toamr.c                               */
/******************************************************************************/
/*                                                                            */
/*                                                                            */
/******************************************************************************/
int writedeclarationintoamr (listvar * deb_common, FILE *fileout,
                              variable *var , const char *commonname,
                           listnom **neededparameter, const char *name_common, int global_check)
{
  listvar *newvar;
  variable *v;
  char ligne[LONG_M];
  int changeval;
  int out;
  int writeit;
  listnom *parcours;
  listname *parcours_name_array;
  int res;
  
  res = 0;

  /* we should list the needed parameter                                      */

  if ( !strcasecmp(name_common,commonname) )
     {
     *neededparameter = DecomposeTheNameinlistnom(var->v_readedlistdimension,*neededparameter);
     parcours_name_array = var->v_initialvalue_array;
     while (parcours_name_array)
     {
     *neededparameter = DecomposeTheNameinlistnom(parcours_name_array->n_name,*neededparameter);
     parcours_name_array=parcours_name_array->suiv;
     }
     }

  /*                                                                          */
  parcours = *neededparameter;

  while (parcours)
  {
     newvar = deb_common;

     out = 0 ;
     while ( newvar && out == 0 )
     {
        if ( (global_check == 0) && !strcasecmp(parcours->o_nom,newvar->var->v_nomvar) && !strcasecmp(var->v_subroutinename,newvar->var->v_subroutinename))
        {
           out=1;
        /* add the name to the list of needed parameter                       */
           *neededparameter = DecomposeTheNameinlistnom(
                 newvar->var->v_initialvalue->n_name,
                 *neededparameter );
        }
        else if ( (global_check == 1) && !strcasecmp(parcours->o_nom,newvar->var->v_nomvar) && !strcasecmp(var->v_modulename,newvar->var->v_modulename))
        {
           out=1;
        /* add the name to the list of needed parameter                       */
           *neededparameter = DecomposeTheNameinlistnom(
                 newvar->var->v_initialvalue->n_name,
                 *neededparameter );
        }
        else newvar=newvar->suiv;
     }
     parcours=parcours->suiv;
   }
  /*                                                                          */
  parcours = *neededparameter;
  
  while (parcours)
  {
     newvar = deb_common;
     out = 0 ;
     while ( newvar && out == 0 )
     {
        if ( (global_check == 0) && !strcasecmp(parcours->o_nom,newvar->var->v_nomvar) && !strcasecmp(var->v_subroutinename,newvar->var->v_subroutinename))
        {
           out=1;
        /* add the name to the list of needed parameter                       */
           *neededparameter = DecomposeTheNameinlistnom(
                 newvar->var->v_initialvalue->n_name,
                 *neededparameter );
        }
        else if ( (global_check == 1) && !strcasecmp(parcours->o_nom,newvar->var->v_nomvar) && !strcasecmp(var->v_modulename,newvar->var->v_modulename))
        {
           out=1;
        /* add the name to the list of needed parameter                       */
           *neededparameter = DecomposeTheNameinlistnom(
                 newvar->var->v_initialvalue->n_name,
                 *neededparameter );
        }
        else newvar=newvar->suiv;
     }
     parcours=parcours->suiv;
   }
  parcours = *neededparameter;
  while (parcours)
  {
     writeit = 0;
     newvar = deb_common;
     while ( newvar && writeit == 0 )
     {
        if ( (global_check == 0) && !strcasecmp(parcours->o_nom,newvar->var->v_nomvar) &&
            !strcasecmp(var->v_subroutinename,newvar->var->v_subroutinename) && parcours->o_val == 0 )
        {
           writeit=1;
           parcours->o_val = 1;
        }
        else if ( (global_check == 1) && !strcasecmp(parcours->o_nom,newvar->var->v_nomvar) &&
            !strcasecmp(var->v_modulename,newvar->var->v_modulename) && parcours->o_val == 0 )
        {
           writeit=1;
           parcours->o_val = 1;
        }
        else newvar = newvar->suiv;
     }

     if ( writeit == 1  )
     {
        changeval = 0;
        v = newvar->var;
//        if ( v->v_allocatable == 1 && strcasecmp(v->v_typevar,"type") )
//        {
//           changeval = 1;
//           v->v_allocatable = 0;
//        }
        WriteBeginDeclaration(v, ligne, 1);
        if ( v->v_nbdim == 0 ) WriteScalarDeclaration(v,ligne);
        else WriteTableDeclaration(v, ligne, 1);

        tofich(fileout, ligne, 1);
        if ( changeval == 1 )
        {
           v->v_allocatable = 1;
        }
        res = 1;
     }
     else
     {
        if (  strncasecmp(parcours->o_nom,"mpi_",4) == 0 &&
              shouldincludempif                     == 1 )
        {
           shouldincludempif = 0;
           fprintf(fileout,"      include \'mpif.h\'\n");
        }
     }
     parcours=parcours->suiv;
  }
  Save_Length(ligne,45);
  return res;
}


/******************************************************************************/
/*                       writesub_loopdeclaration_scalar                      */
/******************************************************************************/
/* This subroutine is used to write the declaration part of subloop           */
/*    subroutines                                                             */
/******************************************************************************/
/*                                                                            */
/*  integer variable(nb) ----------->                                         */
/*                                                                            */
/*          INTEGER, DIMENSION(1:nb)         :: variable                      */
/*                                                                            */
/******************************************************************************/
void writesub_loopdeclaration_scalar (listvar * deb_common, FILE *fileout)
{
  listvar *newvar;
  variable *v;
  char ligne[LONG_M];

//   tofich (fileout, "",1);
  newvar = deb_common;

  while (newvar)
  {
     if ( newvar->var->v_nbdim == 0 &&
          !strcasecmp(newvar->var->v_subroutinename,subroutinename)  &&
           (newvar->var->v_pointerdeclare >= 0 || !strcasecmp(newvar->var->v_typevar,"type")) )
     {
        v = newvar->var;
        WriteBeginDeclaration(v,ligne,1);
        WriteScalarDeclaration(v,ligne);
        tofich (fileout, ligne,1);
     }
     newvar = newvar->suiv;
  }
  Save_Length(ligne,45);
}

/******************************************************************************/
/*                       writesub_loopdeclaration_tab                         */
/******************************************************************************/
/* This subroutine is used to write the declaration part of subloop           */
/*    subroutines                                                             */
/******************************************************************************/
/*                                                                            */
/*  integer variable(nb) ----------->                                         */
/*                                                                            */
/*          INTEGER, DIMENSION(1:nb)         :: variable                      */
/*                                                                            */
/******************************************************************************/
void writesub_loopdeclaration_tab (listvar * deb_common, FILE *fileout)
{
  listvar *newvar;
  variable *v;
  char ligne[LONG_M];
  int changeval;

  newvar = deb_common;
  while (newvar)
  {
      v = newvar->var;
//  printf("newvar = %s %d %s\n",newvar->var->v_nomvar,newvar->var->v_pointerdeclare,newvar->var->v_typevar);
     if ( (v->v_nbdim != 0)  && !strcasecmp(v->v_subroutinename, subroutinename) &&
          (v->v_pointerdeclare >= 0 || !strcasecmp(v->v_typevar,"type")) )
     {
        changeval = 0;
        if ( v->v_allocatable == 1)
        {
          if (strcasecmp(v->v_typevar,"type"))
           {
      //     changeval = 1;
      //     v->v_allocatable = 0;
           }
          else
           {
           changeval = 2;
           v->v_allocatable = 0;
           v->v_pointerdeclare = 1;
           }
        }

        WriteBeginDeclaration(v, ligne, 1);
        WriteTableDeclaration(v, ligne, 1);
        tofich (fileout, ligne,1);
        if ( changeval >= 1 ) v->v_allocatable = 1;
        if ( changeval == 2 ) v->v_pointerdeclare = 0;
     }
     newvar = newvar->suiv;
  }

  Save_Length(ligne,45);
}

void ReWriteDeclarationAndAddTosubroutine_01(listvar *listdecl)
{
    listvar *parcours;
    variable *v;
    int out;

    if ( insubroutinedeclare )
    {
        parcours = listdecl;
        while ( parcours )
        {
            v = parcours->var;
                          out = LookingForVariableInList(List_SubroutineArgument_Var, v);
            if (out == 0) out = VariableIsInListCommon(parcours, List_Common_Var);
            if (out == 0) out = LookingForVariableInList(List_Parameter_Var, v);
            if (out == 0) out = LookingForVariableInList(List_FunctionType_Var, v);
            if (out == 0) out = LookingForVariableInListGlobal(List_Global_Var, v);

            if (firstpass == 0 && out == 0 && VariableIsParameter == 0 && SaveDeclare == 0)
            {
            
            /* The type may has not been given if the variable was only declared with dimension */

            if ( !strcasecmp(v->v_typevar,"") )
            {
                  if ( IsVariableReal(v->v_nomvar) == 1 )
                                        strcpy(v->v_typevar,"REAL");
                  else strcpy(v->v_typevar,"INTEGER");
                  v->v_catvar = get_cat_var(v);
             }
             
                WriteVarDeclaration(v, fortran_out, 1, 1);
            }
            if (firstpass == 1)
            {
                if (VariableIsParameter == 0 && SaveDeclare == 0)
                {
                    List_SubroutineDeclaration_Var = insertvar(List_SubroutineDeclaration_Var, v);
                }
            }
            parcours = parcours->suiv;
        }
    }
}

void ReWriteDataStatement_0(FILE * filout)
{
    listvar *parcours;
    int out;
    char ligne[LONG_M];
    char initialvalue[LONG_M];
    listname *parcours_name;
    
    if (insubroutinedeclare == 1)
    {
        parcours = List_Data_Var_Cur ;
        while (parcours)
        {
            out = VariableIsInListCommon(parcours,List_Common_Var);
            if (out)   break;

            out = LookingForVariableInListGlobal(List_Global_Var,parcours->var);
            if (out)   break;

            strcpy(initialvalue,"");
            parcours_name = parcours->var->v_initialvalue;
            while (parcours_name)
            {
            if (strncasecmp(parcours_name->n_name,"(/",2))
            {
                strcat(initialvalue,parcours_name->n_name);
                if (parcours_name->suiv)
                {
                strcat(initialvalue,",");
                }
            }
            else
            {
            printf("A TRAITER DANS REWRITEDATA STATEMETN ");
            exit(1);
                strncpy(initialvalue,&parcours_name->n_name[2],strlen(parcours_name->n_name)-4);
                strcpy(&initialvalue[strlen(parcours_name->n_name)-4],"\0");
            }
            parcours_name=parcours_name->suiv;
            }
            sprintf(ligne,"data %s/%s/",parcours->var->v_nomvar,initialvalue);
            tofich(filout,ligne,1);
            
            parcours = parcours->suiv;
        }
    }
}
