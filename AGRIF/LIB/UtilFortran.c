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
/*                            initdimprob                                     */
/******************************************************************************/
/* This subroutine is used to initialized grid dimension variable             */
/******************************************************************************/
void initdimprob(int dimprobmod, const char * nx, const char * ny, const char* nz)
{
    dimprob = dimprobmod;

    strcpy(nbmaillesX, nx);
    strcpy(nbmaillesY, ny);
    strcpy(nbmaillesZ, nz);
}

/******************************************************************************/
/*                      Variableshouldberemoved                               */
/******************************************************************************/
/* Firstpass 0                                                                */
/******************************************************************************/
/*                                                                            */
/*               Agrif_<toto>(variable) ====>     Agrif_<toto>(variable)      */
/*                                                                            */
/******************************************************************************/
int Variableshouldberemoved(const char *nom)
{
    return Agrif_in_Tok_NAME(nom);
}

/******************************************************************************/
/*                          variableisglobal                                  */
/******************************************************************************/
/* This subroutine is to know if a variable is global                         */
/******************************************************************************/
int variableisglobal(listvar *curvar, listvar *listin)
{
  int Globalite;
  listvar *newvar;


  Globalite = 0;
  newvar = listin;
  while ( newvar && Globalite == 0 )
  {
     if ( !strcasecmp(newvar->var->v_nomvar,curvar->var->v_nomvar) )
     {
        Globalite = 1;
        /* Now we should give the definition of the variable in the           */
        /* table List_UsedInSubroutine_Var                                    */
        strcpy(curvar->var->v_typevar, newvar->var->v_typevar);
        strcpy(curvar->var->v_dimchar, newvar->var->v_dimchar);
        curvar->var->v_nbdim          = newvar->var->v_nbdim;
        curvar->var->v_dimensiongiven = newvar->var->v_dimensiongiven;
        curvar->var->v_allocatable    = newvar->var->v_allocatable;
        curvar->var->v_target         = newvar->var->v_target;
        curvar->var->v_catvar         = newvar->var->v_catvar;
        curvar->var->v_pointerdeclare = newvar->var->v_pointerdeclare;
        curvar->var->v_indicetabvars  = newvar->var->v_indicetabvars;
        strcpy(curvar->var->v_nameinttypename, newvar->var->v_nameinttypename);
        strcpy(curvar->var->v_precision, newvar->var->v_precision);
        strcpy(curvar->var->v_readedlistdimension, newvar->var->v_readedlistdimension);
        strcpy(curvar->var->v_commoninfile, newvar->var->v_commoninfile);
     }
     else
     {
         newvar = newvar->suiv;
     }
  }

  return Globalite ;
}

int VariableIsInListCommon(listvar *curvar,listvar *listin)
{
  int present;
  listvar *newvar;

  present = 0;
  newvar = listin;

  while ( newvar && present == 0 )
  {
     if ( !strcasecmp(newvar->var->v_nomvar,curvar->var->v_nomvar) &&
          !strcasecmp(newvar->var->v_subroutinename, curvar->var->v_subroutinename) )
     {
        strcpy(curvar->var->v_commoninfile,newvar->var->v_commoninfile);
        Merge_Variables(curvar->var,newvar->var);
        present = 1;
     }
     else newvar = newvar->suiv;
  }

  return present;
}

int VariableIsInList(listvar *curvar,listvar *listin)
{
  int present;
  listvar *newvar;

  present = 0;
  newvar = listin;
  while ( newvar && present == 0 )
  {
     if ( !strcasecmp(newvar->var->v_nomvar,curvar->var->v_nomvar) )
     {
        Merge_Variables(curvar->var,newvar->var);
        present = 1;
     }
     else newvar = newvar->suiv;
  }

  return present;
}

/******************************************************************************/
/*                      variableisglobalinmodule                              */
/******************************************************************************/
/* This subroutine is to know if a variable is global                         */
/******************************************************************************/
void variableisglobalinmodule(listcouple *listin, const char *module, FILE *fileout, long int oldposcuruse)
{
  int Globalite;
  listcouple *newvar;
  listcouple *newvarprec;
  listvar *tempo;
  listvar *newvar2;
  int out;
  char truename[LONG_VNAME];

  Globalite = 1;
  newvarprec = (listcouple *)NULL;
  tempo = (listvar *)NULL;
  tempo = Readthedependfile(module,tempo);
  newvar = listin;

  while ( newvar )
  {
     if (!strcmp(newvar->c_namepointedvar,"")) {
       strcpy(truename,newvar->c_namevar);
     }
     else
     {
       strcpy(truename,newvar->c_namepointedvar);
     }

     out = 0;
     newvar2 = tempo;
     while ( newvar2 && out == 0 )
     {
        if ( !strcasecmp(newvar2->var->v_nomvar,truename) ) out = 1;
        else newvar2 = newvar2 ->suiv;
     }
     if ( out == 1 )
     {
        /* remove from the listin                                             */
        if ( newvar == listin )
        {
           listin = listin->suiv;
           newvar = listin;
        }
        else
        {
           newvarprec->suiv = newvar->suiv;
           newvar = newvar->suiv;
        }
     }
     else
     {
         newvarprec = newvar;
         newvar = newvar->suiv;
         Globalite = 0;
     }
  }
  if ( Globalite == 0 || !newvar)
  {
     pos_end = setposcurname(fileout);
     RemoveWordSET_0(fileout,oldposcuruse,pos_end-oldposcuruse);

     newvar = listin;
     while ( newvar )
     {
        fprintf(fileout,"      use %s, only : %s \n",module,newvar->c_namevar);
        newvar = newvar->suiv;
     }
  }
}

void Write_Word_end_module_0()
{
    if ( firstpass == 0 )
    {
        fprintf(fortran_out,"\n      end module %s",curmodulename);
    }
}

void Add_Subroutine_For_Alloc(const char *nom)
{
   listnom *parcours;
   listnom *newvar;
   int out;

   newvar = (listnom*) calloc(1, sizeof(listnom));
   strcpy(newvar->o_nom,nom);
   newvar->suiv = NULL;

   if ( !List_Subroutine_For_Alloc )
   {
      List_Subroutine_For_Alloc = newvar;
   }
   else
   {
      parcours = List_Subroutine_For_Alloc;
      out = 0 ;
      while ( parcours->suiv && out == 0 )
      {
         if ( !strcasecmp(parcours->o_nom,nom) ) out = 1 ;
         else parcours = parcours ->suiv;
      }
      /*                                                                      */
      if ( out == 0 )
      {
         if ( strcasecmp(parcours->o_nom,nom) ) parcours->suiv = newvar;
      }
   }
}

void Write_Closing_Module(int forend)
{
    listvar *parcours;
    listnom *parcours_nom;
    listnom *parcours_nomprec;
    variable *v;
    int out = 0;
    int headtypewritten = 0;
    char ligne[LONG_M];
    int changeval;

    // Write Global Parameter Declaration
    parcours = List_GlobalParameter_Var;
    while( parcours )
    {
        if ( !strcasecmp(parcours->var->v_modulename, curmodulename) )
        {
            WriteVarDeclaration(parcours->var, module_declar, 0, 1);
        }
        parcours = parcours -> suiv;
    }

    // Write Global Type declaration
    parcours = List_Global_Var;
    while( parcours )
    {
        v = parcours->var;
        if ( !strcasecmp(v->v_modulename, curmodulename) &&
             !strcasecmp(v->v_typevar, "type") )
        {
            if ( headtypewritten == 0 )
            {
                fprintf(fortran_out, "\n      type Agrif_%s\n", curmodulename);
                headtypewritten = 1;
            }
            changeval = 0;
            if ( v->v_allocatable )
            {
                changeval = 1;
                v->v_allocatable = 0;
                v->v_pointerdeclare = 1;
            }
            WriteVarDeclaration(v, fortran_out, 0, 0);
            if ( changeval )
            {
                v->v_allocatable = 1;
                v->v_pointerdeclare = 0;
            }
            out = 1;
        }
        parcours = parcours -> suiv;
    }
    if (out == 1)
    {
        fprintf(fortran_out, "      end type Agrif_%s\n", curmodulename);
        sprintf(ligne, "type(Agrif_%s), dimension(:), allocatable :: Agrif_%s_var",curmodulename, curmodulename);
        tofich(fortran_out,ligne,1);
        fprintf(fortran_out, "      public :: Agrif_%s\n", curmodulename);
        fprintf(fortran_out, "      public :: Agrif_%s_var\n", curmodulename);
    }

    // Write NotGridDepend declaration
    parcours = List_NotGridDepend_Var;
    while( parcours )
    {
        if ( !strcasecmp(parcours->var->v_modulename,curmodulename) )
        {
            WriteVarDeclaration(parcours->var, fortran_out, 0, 1);
        }
        parcours = parcours -> suiv;
    }

    // Write Alloc_agrif_'modulename' subroutine
    parcours_nomprec = (listnom*) NULL;
    parcours_nom = List_NameOfModule;
    out = 0 ;
    while ( parcours_nom && out == 0 )
    {
        if ( !strcasecmp(curmodulename,parcours_nom->o_nom) ) out = 1;
        else parcours_nom = parcours_nom -> suiv;
    }
    if ( ! out )
    {
        printf("#\n# Write_Closing_Module : OUT == 0   *** /!\\ ***\n");
        printf("# FIXME: POSSIBLE BUG in CONV !!!\n#\n");
    }
    if ( out )
    {
        if ( parcours_nom->o_val == 1 )
        {
            fprintf(fortran_out,"\n      public :: Alloc_agrif_%s\n",curmodulename);
        }
        if ( (forend == 0) || (parcours_nom->o_val == 1) )
        {
           fprintf(fortran_out,"\n      contains\n");
        }
        if ( parcours_nom->o_val == 1 )
        {
            fprintf(fortran_out, "      subroutine Alloc_agrif_%s(Agrif_Gr)\n", curmodulename);
            fprintf(fortran_out, "          use Agrif_Util\n");
            fprintf(fortran_out, "          type(Agrif_grid), pointer :: Agrif_Gr\n");
            fprintf(fortran_out, "          integer :: i\n");
            fprintf(fortran_out, "\n#include \"alloc_agrif_%s.h\"\n", curmodulename);
            fprintf(fortran_out, "      end subroutine Alloc_agrif_%s\n", curmodulename);
            Add_Subroutine_For_Alloc(curmodulename);
        }
        else
        {
            parcours_nom = List_Subroutine_For_Alloc;
            out = 0;
            while ( parcours_nom && out == 0 )
            {
                if ( !strcasecmp(parcours_nom->o_nom, curmodulename) ) out = 1;
                else
                {
                    parcours_nomprec = parcours_nom;
                    parcours_nom = parcours_nom->suiv;
                }
            }
            if ( out )
            {
                if ( parcours_nom == List_Subroutine_For_Alloc)
                {
                    List_Subroutine_For_Alloc = List_Subroutine_For_Alloc->suiv;
                }
                else
                {
                    parcours_nomprec->suiv = parcours_nom->suiv;
                    parcours_nom = parcours_nomprec->suiv ;
                }
            }
        }
    }
}

/******************************************************************************/
/*                          IsTabvarsUseInArgument_0                          */
/******************************************************************************/
/* Firstpass 1                                                                */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
int IsTabvarsUseInArgument_0()
{
   int out;
   int doloopout;
   listvar *parcours;

   out=1;

   if ( List_UsedInSubroutine_Var )
   {
      doloopout = 0;
      parcours = List_UsedInSubroutine_Var;
      while ( parcours && doloopout == 0 )
      {
         if ( !strcasecmp(parcours->var->v_subroutinename,subroutinename) )
                                                                  doloopout = 1;
         else parcours = parcours->suiv;
        
      }
      if (  doloopout == 0 ) out = 0;
      else out = 1 ;
   }
   else out = 0;

   return out;
}


/******************************************************************************/
/*                        ImplicitNoneInSubroutine                            */
/******************************************************************************/
/* Firstpass 0                                                                */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
int ImplicitNoneInSubroutine()
{
  listname *parcours;
  int out;

  parcours= List_ImplicitNoneSubroutine;
  out = 0 ;
  while ( parcours && out == 0 )
  {
     if ( !strcasecmp(parcours->n_name,subroutinename) ) out = 1;
     else parcours = parcours->suiv;
  }
  return out;
}

/******************************************************************************/
/*                            Add_Pointer_Var_From_List_1                     */
/******************************************************************************/
/* Firstpass 1                                                                */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void Add_Pointer_Var_From_List_1(listvar *listin)
{
   listvar *parcours;

   if ( firstpass == 1 )
   {
       parcours = listin;
       while ( parcours )
       {
          Add_Pointer_Var_1(parcours->var->v_nomvar);
          parcours = parcours -> suiv ;
       }
   }
}

/******************************************************************************/
/*                            Add_Pointer_Var_1                               */
/******************************************************************************/
/* Firstpass 1                                                                */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void Add_Pointer_Var_1(char *nom)
{
   listname *newvar;
   listname *parcours;
   int out;

   if ( firstpass == 1 )
   {
      if ( !List_Pointer_Var )
      {
         newvar = (listname*) calloc(1, sizeof(listname));
         strcpy(newvar->n_name, nom);
         newvar->suiv = NULL;
         List_Pointer_Var = newvar;
      }
      else
      {
         parcours = List_Pointer_Var;
         out = 0 ;
         while ( parcours->suiv && out == 0 )
         {
            if (  !strcasecmp(parcours->n_name,nom) ) out = 1;
            else
               parcours=parcours->suiv;
         }
         if ( out == 0 )
         {
            if (  !strcasecmp(parcours->n_name,nom) ) out = 1;
            else
            {
               /* add the record                                              */
              newvar = (listname*) calloc(1, sizeof(listname));
              strcpy(newvar->n_name,nom);
              newvar->suiv = NULL;
              parcours->suiv = newvar;
            }
         }
      }
   }
}

/******************************************************************************/
/*                          varispointer_0                                    */
/******************************************************************************/
/* Firstpass 0                                                                */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
int varispointer_0(char *ident)
{
   listname *newname;
   int out;

   out =0;
   if ( firstpass == 0 )
   {
      newname = List_Pointer_Var;
      while( newname && out == 0 )
      {
         if ( !strcasecmp(ident,newname->n_name) ) out = 1 ;
         else newname = newname->suiv;
      }
   }
   return out;
}

/******************************************************************************/
/*                          varistyped_0                                    */
/******************************************************************************/
/* Firstpass 0                                                                */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
int varistyped_0(char *ident)
{
   listvar *parcours;
   int out;

   out =0;
   if ( firstpass == 0 )
   {
      parcours = List_Global_Var;
      while( parcours && out == 0 )
      {
         if ( !strcasecmp(ident,parcours->var->v_nomvar) )
             {
             if (!strcasecmp(parcours->var->v_typevar,"type")) out = 1;
             }
         parcours = parcours->suiv;
      }
   }
   return out;
}


/******************************************************************************/
/*                          VariableIsFunction                                */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
int VariableIsFunction(const char *ident)
{
    int out;
    listvar *newvar;

    out = 0;

    return (out == 0);
    
    if ( !strcasecmp(ident,"size") ||
         !strcasecmp(ident,"if")   ||
         !strcasecmp(ident,"max")  ||
         !strcasecmp(ident,"min")  )
    {
    printf("ident = %s\n",ident);
        newvar = List_SubroutineDeclaration_Var;
        while ( newvar && out == 0 )
        {
            if ( !strcasecmp(subroutinename, newvar->var->v_subroutinename) &&
                 !strcasecmp(ident, newvar->var->v_nomvar) )
            {
                out = 1;
            }
            newvar = newvar -> suiv ;
        }
        if ( out == 0 ) /* if it has not been found */
        {
            newvar = List_Global_Var;
            while ( newvar && out == 0 )
            {
            printf("llll = %s\n",newvar->var->v_nomvar);
                if ( !strcasecmp(ident, newvar->var->v_nomvar) ) out = 1;
                newvar = newvar -> suiv ;
            }
        }
    }
    return (out == 0);
}

/* removenewline */
/* REMOVE UNWANTED character */
/* from a NAME{NEWLINE77]NAME flex match */

void removenewline(char *nom)
{
char temp_nom[LONG_VNAME];
int size_nom,i,j;

size_nom=strlen(nom);

j=0;
for (i=0;i<size_nom;)
{
if (nom[i]=='\n')
{
/* REMOVE RETURN - blank and column 6 character */
i=i+7;
}
else if (nom[i]==' ' || nom[i]=='\t')
{
i=i+1;
}
else
{
temp_nom[j]=nom[i];
j++;
i++;
}
}
temp_nom[j]='\0';

strcpy(nom,temp_nom);
}

void dump_var(const variable* var)
{
    fprintf(stderr, "   var->v_nomvar : %s\n",var->v_nomvar);
    fprintf(stderr, "   var->v_indice : %d\n",var->v_indicetabvars);
    fprintf(stderr, "   var->v_typevar: %s\n",var->v_typevar);
    fprintf(stderr, "   var->v_catvar : %d\n",var->v_catvar);
    fprintf(stderr, "   var->v_modulename: %s\n",var->v_modulename);
    fprintf(stderr, "   var->v_subroutinename: %s\n",var->v_subroutinename);
    fprintf(stderr, "   var->v_commonname: %s\n",var->v_commonname);
    fprintf(stderr, "   var->v_commoninfile: %s\n",var->v_commoninfile);
    fprintf(stderr, "   var->v_nbdim: %d\n",var->v_nbdim);
    fprintf(stderr, "   var->v_common: %d\n",var->v_common);
    fprintf(stderr, "   var->v_module: %d\n",var->v_module);
    fprintf(stderr, "   var->v_initialvalue: %s\n",var->v_initialvalue);
}
