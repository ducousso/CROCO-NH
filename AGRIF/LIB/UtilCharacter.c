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
/*                         FindAndChangeNameToTabvars                         */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
/* if  whichone = 0 ----> Agrif_tabvars(i) % array2                           */
/*                                                                            */
/* if  whichone = 1 ----> Agrif_tabvars(i) % parentvar % array2               */
/*                                                                            */
/******************************************************************************/
void FindAndChangeNameToTabvars(const char name[LONG_M],char toprint[LONG_M],
                                              listvar * listtosee, int whichone)
{
   listvar *newvar;
   int out;
   

   if ( strcasecmp(name,"") )
   {
      newvar=listtosee;
      out=0;
      while( newvar && out == 0 )
      {
         if ( !strcasecmp(newvar->var->v_nomvar,name) )
         {

            if ( LookingForVariableInListName(
                             List_SubroutineArgument_Var,name) == 0 )
            {
               out = 1;
               strcat(toprint,vargridcurgridtabvars(newvar->var, whichone));
            }
            else newvar=newvar->suiv;
         }
         else newvar=newvar->suiv;
      }
      if ( out == 0 ) strcat(toprint,name);
   }
   Save_Length(toprint,44);
}


/******************************************************************************/
/*                     ChangeTheInitalvaluebyTabvarsName                      */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/******************************************************************************/
const char *ChangeTheInitalvaluebyTabvarsName(const char *nom, listvar *listtoread)
{
    char toprinttmp[LONG_M];
    char chartmp[2];
    size_t i = 0;

    strcpy(toprintglob, "");
    strcpy(toprinttmp,  "");

    while ( i < strlen(nom) )
    {
        if ( (nom[i] == '+') || (nom[i] == '-') || (nom[i] == '*') || (nom[i] == '/') ||
             (nom[i] == '(') || (nom[i] == ')') || (nom[i] == ':') || (nom[i] == ',') )
        {
            FindAndChangeNameToTabvars(toprinttmp,toprintglob,listtoread,0);
            strcpy(toprinttmp, "");
            sprintf(chartmp, "%c", nom[i]);
            strcat(toprintglob, chartmp);
        }
        else
        {
            sprintf(chartmp, "%c", nom[i]);
            strcat(toprinttmp, chartmp);
        }
        i += 1;
    }
    FindAndChangeNameToTabvars(toprinttmp,toprintglob,listtoread,0);
    strcpy(toprinttmp,"");

    Save_Length(toprinttmp,44);
    Save_Length(toprintglob,39);

    return toprintglob;
}

/******************************************************************************/
/*                            IsVariableReal                                  */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/******************************************************************************/
int IsVariableReal(const char *nom)
{
    return ( ( nom[0] >= 'a' && nom[0] <= 'h' ) ||
        ( nom[0] >= 'A' && nom[0] <= 'H' ) ||
        ( nom[0] >= 'o' && nom[0] <= 'z' ) ||
        ( nom[0] >= 'O' && nom[0] <= 'Z' ) );
}
/******************************************************************************/
/*                         IsVarInUseFile                                     */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/******************************************************************************/
void IsVarInUseFile(const char *nom)
{
   listvar *parcours;
   listparameter *parcoursparam;
   int out;

   out = 0;

   parcours = List_Global_Var;
   while( parcours && out == 0 )
   {
      if ( !strcasecmp(nom,parcours->var->v_nomvar) ) out = 1 ;
     else parcours=parcours->suiv;
   }
   if ( out == 0 )
   {
      parcours = List_Common_Var;
      while( parcours && out == 0 )
      {
         if ( !strcasecmp(nom,parcours->var->v_nomvar) ) out = 1 ;
        else parcours=parcours->suiv;
      }
   }
   if ( out == 0 )
   {
      parcours = List_GlobalParameter_Var;
      while( parcours && out == 0 )
      {
         if ( !strcasecmp(nom,parcours->var->v_nomvar) ) out =1 ;
        else parcours=parcours->suiv;
      }
   }
   if ( out == 0 )
   {
      parcours = List_Parameter_Var;
      while( parcours && out == 0 )
      {
         if ( !strcasecmp(nom,parcours->var->v_nomvar) ) out = 1 ;
        else parcours=parcours->suiv;
      }
   }
   if ( out == 0 )
   {
      parcoursparam = List_GlobParamModuleUsed_Var;
      while( parcoursparam && out == 0 )
      {
         if ( !strcasecmp(nom,parcoursparam->p_name) ) out = 2 ;
         else parcoursparam=parcoursparam->suiv;
      }
   }
   if ( out == 0 )
   {
      parcours = List_ModuleUsed_Var;
      while( parcours && out == 0 )
      {
         if ( !strcasecmp(nom,parcours->var->v_nomvar) ) out = 2 ;
        else parcours=parcours->suiv;
      }
   }
   if ( out == 0 || out == 2 )
   {
      parcoursparam = List_GlobParamModuleUsedInModuleUsed_Var;
      while( parcoursparam && out != 1 )
      {
         if ( !strcasecmp(nom,parcoursparam->p_name) ) out = 1 ;
         else parcoursparam=parcoursparam->suiv;
      }
      if ( out == 1 )
      {
         strcpy(charusemodule,parcoursparam->p_modulename);
         Addmoduletothelist(parcoursparam->p_modulename);
      }
   }
   if ( out == 0 &&
        strcasecmp(nom,"MAX")             &&
        strcasecmp(nom,"mpi_status_size")
      )
   {
   /*   printf("--- in UtilCharacter we do not found the \n");
      printf("---  variable %s, the module where this \n",nom);
      printf("---  variable has been defined has not been\n");
      printf("---  found.\n");*/
   }
}

/******************************************************************************/
/*                      DecomposeTheNameinlistnom                             */
/******************************************************************************/
/* Firstpass 0                                                                */
/******************************************************************************/
/*                                                                            */
/*                                                                            */
/******************************************************************************/
listnom *DecomposeTheNameinlistnom(const char *nom, listnom * listout)
{
   char toprinttmp[LONG_M];
   char chartmp[2];
   size_t i = 0;

   strcpy(toprinttmp,"");

   while ( i < strlen(nom) )
   {
      if ( nom[i] == '+' ||
           nom[i] == '-' ||
           nom[i] == '*' ||
           nom[i] == '/' ||
           nom[i] == ')' ||
           nom[i] == '(' ||
           nom[i] == ',' ||
           nom[i] == ':'
         )
      {
         if (strcasecmp(toprinttmp,"") && ( toprinttmp[0] >= 'A' ) )
         {
             listout = Addtolistnom(toprinttmp,listout,0);
         }
         strcpy(toprinttmp,"");
      }
      else
      {
         sprintf(chartmp,"%c",nom[i]);
         strcat(toprinttmp,chartmp);
      }
      i=i+1;
   }
   if (strcasecmp(toprinttmp,"") && ( toprinttmp[0] >= 'A' ) )
   {
      listout = Addtolistnom(toprinttmp,listout,0);
   }
   Save_Length(toprinttmp,44);
   strcpy(toprinttmp,"");

   return listout;
}


/******************************************************************************/
/*                      DecomposeTheName                                      */
/******************************************************************************/
/* Firstpass 0                                                                */
/******************************************************************************/
/*                                                                            */
/*               Agrif_<toto>(variable) ====>     Agrif_<toto>(variable)      */
/*                                                                            */
/******************************************************************************/
void DecomposeTheName(const char *nom)
{
   char toprinttmp[LONG_M];
   char chartmp[2];
   size_t i = 0;
   
   strcpy(toprinttmp,"");

   while ( i < strlen(nom) )
   {
      if ( nom[i] == '+' ||
           nom[i] == '-' ||
           nom[i] == '*' ||
           nom[i] == '/' ||
           nom[i] == ')' ||
           nom[i] == '(' ||
           nom[i] == ',' ||
           nom[i] == ':'
         )
      {
         if (strcasecmp(toprinttmp,"") && ( toprinttmp[0] >= 'A' ) )
         {
            ajoutevarindoloop_definedimension (toprinttmp);
            /* Is this variable present in globvarofusefile                   */
            IsVarInUseFile(toprinttmp);
         }
         strcpy(toprinttmp,"");
      }
      else
      {
         sprintf(chartmp,"%c",nom[i]);
         strcat(toprinttmp,chartmp);
      }
      i=i+1;
   }
   if (strcasecmp(toprinttmp,"") && ( toprinttmp[0] >= 'A' ) )
   {
      ajoutevarindoloop_definedimension (toprinttmp);
      /* Is this variable present in globvarofusefile                         */
      IsVarInUseFile(toprinttmp);
   }
   Save_Length(toprinttmp,44);
   strcpy(toprinttmp,"");

}

void convert2lower(char *lowername, const char* inputname)
{
    int i, l, caractere;

    strcpy(lowername, inputname);
    l = strlen(lowername)-1;

    for ( i=0 ; i<=l ; i++)
    {
        caractere = lowername[i];
        if ( (caractere>=65 && caractere<=90) || (caractere>=192 && caractere<=221) )
        {
            lowername[i] += 32;
        }
    }
}

int convert2int(const char *name)
{
   int i;
   int caractere;
   int value;
   int value_tmp;
   int longueur;

   value = 0;

   longueur = strlen(name) - 1;
   for (i=0;i<=longueur;i++)
   {
      caractere=name[i];
      value_tmp = caractere -'0';
      if ( value_tmp > 9 ) return 0;
           if ( longueur+1-i == 6 ) value = value + value_tmp *100000;
      else if ( longueur+1-i == 5 ) value = value + value_tmp *10000;
      else if ( longueur+1-i == 4 ) value = value + value_tmp *1000;
      else if ( longueur+1-i == 3 ) value = value + value_tmp *100;
      else if ( longueur+1-i == 2 ) value = value + value_tmp *10;
      else if ( longueur+1-i == 1 ) value = value + value_tmp *1;
   }
   return value;
}
