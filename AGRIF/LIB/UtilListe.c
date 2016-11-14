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


void Init_Variable(variable *var)
{
   strcpy(var->v_typevar            , "");
   strcpy(var->v_nomvar             , "");
   strcpy(var->v_oldname            , "");
   strcpy(var->v_dimchar            , "");
   strcpy(var->v_modulename         , "");
   strcpy(var->v_commonname         , "");
   strcpy(var->v_vallengspec        , "");
   strcpy(var->v_nameinttypename    , "");
   strcpy(var->v_commoninfile       , "");
   strcpy(var->v_subroutinename     , "");
   strcpy(var->v_precision          , "");
   var->v_initialvalue = (listname *)NULL;
   var->v_initialvalue_array = (listname *)NULL;
   var->v_do_loop = NULL;
   strcpy(var->v_IntentSpec         , "");
   strcpy(var->v_readedlistdimension, "");
   var->v_nbdim               = 0 ;
   var->v_common              = 0 ;
   var->v_positioninblock     = 0 ;
   var->v_module              = 0 ;
   var->v_save                = 0 ;
   var->v_catvar              = 0 ;
   var->v_VariableIsParameter = 0 ;
   var->v_PublicDeclare       = 0 ;
   var->v_PrivateDeclare      = 0 ;
   var->v_ExternalDeclare     = 0 ;
   var->v_pointedvar          = 0 ;
   var->v_notgrid             = 0 ;
   var->v_dimensiongiven      = 0 ;
   var->v_c_star              = 0 ;
   var->v_indicetabvars       = 0 ;
   var->v_pointerdeclare      = 0 ;
   var->v_optionaldeclare     = 0 ;
   var->v_allocatable         = 0 ;
   var->v_target              = 0 ;
   var->v_dimsempty           = 0 ;
   var->v_dimension = (listdim *) NULL;
}
/******************************************************************************/
/*                            AddListvartolistvar                             */
/******************************************************************************/
/* This subroutine is used to add a listvar l at the end of a listvar         */
/* glob.                                                                      */
/*                                                                            */
/******************************************************************************/
/*        _______     _______     _______     _______     _______             */
/*       +      +    +      +    +      +    +      +    +      +             */
/*       + glob +--->+ glob +--->+ glob +--->+ glob +--->+  l   +             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                                                            */
/******************************************************************************/
listvar * AddListvarToListvar ( listvar *l, listvar *glob, int ValueFirstpass )
{
    listvar *newvar;
    if ( firstpass == ValueFirstpass )
    {
        if ( !glob ) glob = l;
        else
        {
            newvar = glob;
            while (newvar->suiv)
                newvar = newvar->suiv;
            newvar->suiv = l;
        }
        newvar=glob;
        while (newvar)
        {
        newvar=newvar->suiv;
        }
    }
    return glob;
}

/******************************************************************************/
/*                       CreateAndFillin_Curvar                               */
/******************************************************************************/
/* This subroutine is used to create the record corresponding to the          */
/* list of declaration                                                        */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void CreateAndFillin_Curvar(const char *type, variable *curvar)
{
listname *newvar;

    if ( !strcasecmp(type, "character") && strcasecmp(CharacterSize, "") )
    {
        strcpy(curvar->v_dimchar, CharacterSize);
    }

    /* On donne la precision de la variable si elle a ete donnee                */
    curvar->v_c_star = 0;
    if ( c_star == 1 )  curvar->v_c_star = 1;

    strcpy(curvar->v_vallengspec,"");
    if ( strcasecmp(vallengspec,"") )
    {
        strcpy(curvar->v_vallengspec,vallengspec);
        Save_Length(vallengspec,8);
    }

    strcpy(curvar->v_precision,"");
    if ( strcasecmp(NamePrecision,"") )
    {
        strcpy(curvar->v_precision,NamePrecision);
        addprecision_derivedfromkind(curvar);
        Save_Length(NamePrecision,12);
    }
    /* Si cette variable a ete declaree dans un module on met curvar->module=1  */
    if ( inmoduledeclare == 1 || SaveDeclare == 1 )
    {
        curvar->v_module = 1;
    }
    /* Puis on donne le nom du module dans curvar->v_modulename                */
    strcpy(curvar->v_modulename,curmodulename);
    /* Si cette variable a ete initialisee                                     */
    if (InitialValueGiven == 1 )
    {
    curvar->v_initialvalue=Insertname(curvar->v_initialvalue,InitValue,0);
    
//        strcpy(curvar->v_initialvalue,InitValue);
        
        Save_Length(InitValue,14);
    }
    /* Si cette variable est declaree en save                                  */
    if (SaveDeclare == 1 && !strcasecmp(curvar->v_typevar,"type")) curvar->v_save = 1;

    /* Si cette variable est v_allocatable                                     */
    if (Allocatabledeclare == 1 ) curvar->v_allocatable=1;

    /* Si cette variable est v_target                                     */
    if (Targetdeclare == 1 ) curvar->v_target=1;

    /* if INTENT spec has been given                                           */
    if ( strcasecmp(IntentSpec,"") )
    {
        strcpy(curvar->v_IntentSpec,IntentSpec);
        Save_Length(IntentSpec,13);
    }
}


void addprecision_derivedfromkind(variable *curvar)
{
    listnom *parcours;
    char kind[LONG_VNAME];
    char kind_val[LONG_C];

    sscanf(curvar->v_precision, "%100s =", kind_val);

    if ( !strcasecmp(kind_val, "kind") )
        sscanf(curvar->v_precision, "%50s = %50s", kind, kind_val);

    parcours = listofkind;
    while (parcours)
    {
        if ( !strcasecmp(parcours->o_nom, kind_val) )
        {
            sprintf(curvar->v_nameinttypename, "%d", parcours->o_val);
        }
        parcours=parcours->suiv;
    }
}

/******************************************************************************/
/*                        duplicatelistvar                                    */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
// void duplicatelistvar(listvar *orig)
// {
//    listvar *parcours;
//    listvar *tmplistvar;
//    listvar *tmplistvarprec;
//    listdim *tmplistdim;
//    variable *tmpvar;
//
//    tmplistvarprec = (listvar *)NULL;
//    parcours = orig;
//    while ( parcours )
//    {
//       tmplistvar = (listvar *)calloc(1,sizeof(listvar));
//       tmpvar = (variable *)calloc(1,sizeof(variable));
//       /*                                                                      */
//       Init_Variable(tmpvar);
//       /*                                                                      */
//       strcpy(tmpvar->v_typevar, parcours->var->v_typevar);
//       strcpy(tmpvar->v_nomvar,  parcours->var->v_nomvar);
//       strcpy(tmpvar->v_oldname, parcours->var->v_oldname);
//       strcpy(tmpvar->v_dimchar, parcours->var->v_dimchar);
//       if ( parcours->var->v_dimension )
//       {
//          tmplistdim = (listdim*) calloc(1,sizeof(listdim));
//          tmplistdim = parcours->var->v_dimension;
//          tmpvar->v_dimension = tmplistdim;
//       }
//       tmpvar->v_nbdim  = parcours->var->v_nbdim;
//       tmpvar->v_common = parcours->var->v_common;
//       tmpvar->v_module = parcours->var->v_module;
//       tmpvar->v_save   = parcours->var->v_save;
//       tmpvar->v_positioninblock = parcours->var->v_positioninblock;
//       tmpvar->v_VariableIsParameter = parcours->var->v_VariableIsParameter;
//       tmpvar->v_indicetabvars = parcours->var->v_indicetabvars;
//       tmpvar->v_pointedvar    = parcours->var->v_pointedvar;
//       tmpvar->v_dimensiongiven = parcours->var->v_dimensiongiven;
//       tmpvar->v_c_star = parcours->var->v_c_star;
//       tmpvar->v_catvar = parcours->var->v_catvar;
//       tmpvar->v_pointerdeclare = parcours->var->v_pointerdeclare;
//       tmpvar->v_optionaldeclare = parcours->var->v_optionaldeclare;
//       tmpvar->v_allocatable = parcours->var->v_allocatable;
//       tmpvar->v_target      = parcours->var->v_target;
//       tmpvar->v_dimsempty   = parcours->var->v_dimsempty;
//       strcpy(tmpvar->v_modulename,  parcours->var->v_modulename);
//       strcpy(tmpvar->v_commonname,  parcours->var->v_commonname);
//       strcpy(tmpvar->v_vallengspec, parcours->var->v_vallengspec);
//       strcpy(tmpvar->v_nameinttypename, parcours->var->v_nameinttypename);
//       strcpy(tmpvar->v_commoninfile, cur_filename);
//       strcpy(tmpvar->v_subroutinename, parcours->var->v_subroutinename);
//       strcpy(tmpvar->v_precision, parcours->var->v_precision);
//       strcpy(tmpvar->v_initialvalue, parcours->var->v_initialvalue);
//       strcpy(tmpvar->v_IntentSpec, parcours->var->v_IntentSpec);
//       strcpy(tmpvar->v_readedlistdimension, parcours->var->v_readedlistdimension);
//
//       tmplistvar->var = tmpvar;
//       tmplistvar->suiv = NULL;
//
//       if ( !listduplicated )
//       {
//          listduplicated = tmplistvar;
//          tmplistvarprec = listduplicated;
//       }
//       else
//       {
//          tmplistvarprec->suiv = tmplistvar;
//          tmplistvarprec = tmplistvar;
//       }
//       parcours = parcours->suiv;
//    }
// }

/******************************************************************************/
/*                           insertdim                                        */
/******************************************************************************/
/* This subroutine is used to insert a record in a list of                    */
/* struct : listdim                                                           */
/******************************************************************************/
/*        _______     _______     _______     _______     _______             */
/*       +      +    +      +    +      +    +      +    +      +             */
/*       + NEW  +--->+ lin  +--->+ lin  +--->+ lin  +--->+  lin +             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                                                            */
/******************************************************************************/
listdim * insertdim(listdim *lin,typedim nom)
{
   listdim *newdim ;
   listdim *parcours ;

   newdim=(listdim *) calloc(1,sizeof(listdim));
   newdim->dim=nom;
   newdim->suiv=NULL;

   if ( ! lin )
   {
      lin = newdim;
   }
   else
   {
      parcours = lin;
      while ( parcours->suiv ) parcours=parcours->suiv;
      parcours->suiv = newdim;
   }

   return lin;
}

/******************************************************************************/
/*                            change_dim_char                                 */
/******************************************************************************/
/* This subroutine is used to change the dimension in the list lin            */
/******************************************************************************/
/*        _______     _______                 _______     _______             */
/*       +  l   +    +  l   +                +  l   +    +   l  +             */
/*       + old  +--->+ old  +--------------->+ lin  +--->+  lin +             */
/*       +______+    +______+                +______+    +______+             */
/*                                                                            */
/******************************************************************************/
void change_dim_char(listdim *lin,listvar * l)
{
   listvar *parcours_var;
   variable *v;

   parcours_var=l;
   while(parcours_var)
   {
      v = parcours_var->var;
      strcpy(v->v_dimchar,(lin->dim).last);
      parcours_var=parcours_var->suiv;
   }
}


/******************************************************************************/
/*                              get_num_dims                                  */
/******************************************************************************/
/* This subroutine is used to know the dimension of a table                   */
/******************************************************************************/
/*                                                                            */
/*             Dimension(jpi,jpj,jpk) ----------> get_num_dims = 3            */
/*                                                                            */
/******************************************************************************/
int get_num_dims ( const listdim *d )
{
    listdim *parcours;
    int compteur = 0;

    parcours = (listdim *) d;
    while(parcours)
    {
        compteur++;
        parcours = parcours->suiv;
    }
    return compteur;
}


/******************************************************************************/
/*                          CREATEVAR                                         */
/******************************************************************************/
/* This subroutine is used to create and initialized a record of the          */
/*      struct : variable                                                     */
/******************************************************************************/
variable * createvar(const char *nom, listdim *d)
{
    variable *var;
    listdim *dims;
    char ligne[LONG_M];
    char listdimension[LONG_M];

    var = (variable *) calloc(1,sizeof(variable));

    Init_Variable(var);

    strcpy(listdimension,"");
    strcpy(var->v_nomvar,nom);
    strcpy(var->v_modulename,curmodulename);
    strcpy(var->v_commoninfile,cur_filename);
    strcpy(var->v_subroutinename,subroutinename);

    if ( strcasecmp(nameinttypename,"") )
    {
        strcpy(var->v_nameinttypename,nameinttypename);
    }

    if ( optionaldeclare     == 1 ) var->v_optionaldeclare = 1;
    if ( pointerdeclare      == 1 ) var->v_pointerdeclare = 1;
    if ( VariableIsParameter == 1 ) var->v_VariableIsParameter = 1 ;
    if ( PublicDeclare       == 1 ) var->v_PublicDeclare = 1 ;
    if ( PrivateDeclare      == 1 ) var->v_PrivateDeclare = 1;
    if ( ExternalDeclare     == 1 ) var->v_ExternalDeclare = 1;

   var->v_dimension = d;

   /* Creation of the string for the dimension of this variable               */
    dimsempty = 1;
    if ( d )
    {
        var->v_dimensiongiven = 1;
        dims = d;
        while (dims)
        {
            if ( strcasecmp(dims->dim.first,"") || strcasecmp(dims->dim.last,""))
            {
                dimsempty = 0;
            }
            sprintf(ligne,"%s:%s",dims->dim.first,dims->dim.last);
            strcat(listdimension,ligne);
            if ( dims->suiv )
            {
                strcat(listdimension,",");
            }
            dims = dims->suiv;
        }
        if ( dimsempty == 1 || GlobalDeclarationType == 1 ) var->v_dimsempty = 1;
    }
    strcpy(var->v_readedlistdimension,listdimension);
    Save_Length(listdimension,15);
    var->v_nbdim = get_num_dims(d);

    return var;
}

/******************************************************************************/
/*                            INSERTVAR                                       */
/******************************************************************************/
/* This subroutine is used to insert a record in a list of the                */
/*      struct : listvar                                                      */
/******************************************************************************/
/*        _______     _______     _______     _______     _______             */
/*       +      +    +      +    +      +    +      +    +      +             */
/*       +  lin +--->+  lin +--->+ lin  +--->+ lin  +--->+ NEW  +             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                                                            */
/*                                                                            */
/******************************************************************************/
listvar * insertvar(listvar *lin,variable *v)
{
   listvar *newvar ;
   listvar *tmpvar ;

   newvar=(listvar *) calloc(1,sizeof(listvar));
   newvar->var=v;
   newvar->suiv = NULL;
   if (!lin)
   {
      newvar->suiv=NULL;
      lin = newvar;
   }
   else
   {
      tmpvar = lin ;
      while (tmpvar->suiv)
      {
         tmpvar = tmpvar ->suiv ;
      }
      tmpvar -> suiv = newvar;
   }
   return lin;
}

/******************************************************************************/
/*                             SETTYPE                                        */
/******************************************************************************/
/* This subroutine is used to give the same variable type at each             */
/*      record of the list of the struct : listvar                            */
/******************************************************************************/
/*        _______     _______     _______     _______     _______             */
/*       + REAL +    + REAL +    + REAL +    + REAL +    + REAL +             */
/*       +  lin +--->+  lin +--->+ lin  +--->+ lin  +--->+ lin  +             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                                                            */
/*                                                                            */
/******************************************************************************/
listvar *settype(const char *nom, listvar *lin)
{
   listvar *newvar;
   variable *v;

   newvar = lin;
   while (newvar)
   {
      v = newvar->var;
      strcpy(v->v_typevar,nom);
      
      v->v_catvar = get_cat_var(v);

      newvar = newvar->suiv;
   }
   newvar = lin;
   return newvar ;
}

/******************************************************************/
/* printliste  */
/* print the list given in argulent */
/******************************************************************/

void printliste(listvar * lin)
{
   listvar *newvar;
   variable *v;

   newvar=lin;
   while (newvar)
   {
      v=newvar->var;
      newvar=newvar->suiv;
   }
}

/******************************************************************************/
/*   IsinListe : return 1 if name nom is in list lin                          */
/*                                                                            */
/******************************************************************************/
 int IsinListe(listvar *lin,char *nom)
{
   listvar *newvar;
   variable *v;
   int out ;

   newvar=lin;
   out = 0;
   while (newvar && (out == 0))
   {
      v=newvar->var;
      if (!strcasecmp(v->v_nomvar,nom) && !strcasecmp(v->v_subroutinename,subroutinename)) {
      out = 1;
      }
      newvar=newvar->suiv;
   }

   return out ;
}

listname *Insertname(listname *lin,char *nom, int sens)
{
   listname *newvar ;
   listname *tmpvar;

   newvar=(listname *) calloc(1,sizeof(listname));
   strcpy(newvar->n_name,nom);
   newvar->suiv = NULL;
   if (!lin)
   {
      newvar->suiv=NULL;
      lin = newvar;
   }
   else
   {
      if (sens == 0)
      {
      tmpvar = lin ;
      while (tmpvar->suiv)
      {
         tmpvar = tmpvar ->suiv ;
      }
      tmpvar -> suiv = newvar;
      }
      else
      {
      newvar->suiv = lin;
      lin = newvar;
      }
   }
   return lin;
}

int testandextractfromlist(listname **lin, char*nom)
{
listname *newvar;
int val_1, val_2;
int return_stmt;

printname(*lin);
if (!(*lin))
 {
  return 0;
 }
else
 {
 sscanf(nom,"%d",&val_1);
 sscanf((*lin)->n_name,"%d",&val_2);
 if (val_1==val_2)
   {
/*   newvar = *lin;
   *lin = (*lin)->suiv;
   free(newvar);*/
   /* continue to remove while the label stays the same */
/*   return_stmt=testandextractfromlist(lin,nom);*/
   return 1;
   }
 else
  {
  return 0;
  }
 }
}

void removefromlist(listname **lin, char*nom)
{
listname *newvar,*prev;
int val_1, val_2;
int return_stmt;
int out;

printname(*lin);
if (*lin)
 {
 sscanf(nom,"%d",&val_1);
 prev=(listname *) calloc(1,sizeof(listname));
 prev->suiv=*lin;
 *lin=prev;
 newvar=(*lin)->suiv;
 out = 0;
 while (newvar && out == 0)
 {
 sscanf((newvar)->n_name,"%d",&val_2);
 if (val_1==val_2)
   {
   prev->suiv=newvar->suiv;
   free(newvar);
   }
  if (prev->suiv) 
    {
    prev=prev->suiv;
    newvar=prev->suiv;
    }
   else
   {
   out = 1;
   }
  }
 prev=*lin;
 *lin=(*lin)->suiv;
 free(prev);
 }
}

listname *concat_listname(listname *l1, listname *l2)
{
   listname *tmpvar;

   tmpvar = l1;
   while (tmpvar->suiv)
   {
    tmpvar = tmpvar->suiv;
   }

   tmpvar->suiv = l2;

   return l1;
}

void createstringfromlistname(char *ligne, listname *lin)
{
    listname *tmpvar;

    strcpy(ligne,"");
    tmpvar = lin;

    while(tmpvar)
    {
        strcat(ligne,tmpvar->n_name);
        if (tmpvar->suiv) strcat(ligne,",");
        tmpvar=tmpvar->suiv;
    }
}

/******************************************************************/
/* printname  */
/* print the list given in argulent */
/******************************************************************/

void printname(listname * lin)
{
   listname *newvar;

   newvar=lin;
   while (newvar)
   {
      newvar=newvar->suiv;
   }
}

void removeglobfromlist(listname **lin)
{
  listname *parcours1;
  listvar *parcours2;
  listname * parcourspres;
  int out;

  parcours1 = *lin;
  parcourspres = (listname *)NULL;

  while (parcours1)
  {
  parcours2 = List_Global_Var;
  out = 0;
  while (parcours2 && out == 0)
  {
    if (!strcasecmp(parcours2->var->v_nomvar,parcours1->n_name))
    {
    out = 1;
    }
    parcours2 = parcours2->suiv;
  }
  if (out == 1)
  {
  if (parcours1 == *lin)
   {
   *lin = (*lin)->suiv;
   parcours1 = *lin;
   }
   else
   {
   parcourspres->suiv = parcours1->suiv;
   parcours1 = parcourspres->suiv;
   }
   }
   else
   {
   parcourspres = parcours1;
    parcours1 = parcours1->suiv;
    }
  }
}

void writelistpublic(listname *lin)
{
    listname *parcours1;
    char ligne[LONG_M];

    if (lin)
    {
        sprintf(ligne,"public :: ");
        parcours1 = lin;

        while ( parcours1 )
        {
            strcat(ligne, parcours1->n_name);
            if ( parcours1->suiv ) strcat(ligne,", ");
            parcours1 = parcours1->suiv;
        }
        tofich(fortran_out,ligne,1);
    }
}

void Init_List_Data_Var()
{
    listvar *parcours;

    parcours = List_Data_Var_Cur;

    if (List_Data_Var_Cur)
    {
        while (parcours)
        {
            List_Data_Var_Cur = List_Data_Var_Cur->suiv;
            free(parcours);
            parcours = List_Data_Var_Cur;
        }
    }
    List_Data_Var_Cur = NULL;
}

int get_cat_var(variable *var)
{

    if (!strcasecmp(var->v_typevar, "CHARACTER"))
        return 1;
    else if ((var->v_nbdim == 0 ) && (!strcasecmp(var->v_typevar, "REAL")))
        return 2;
    else if (!strcasecmp(var->v_typevar, "LOGICAL"))
        return 3;
    else if (!strcasecmp(var->v_typevar, "INTEGER"))
        return 4;
    else
        return 0;
}

void Insertdoloop(variable *var,char *do_var,char *do_begin, char *do_end, char *do_step)
{
listdoloop *new_do_loop;
listdoloop *tmploop;
new_do_loop = (listdoloop *) calloc(1,sizeof(listdoloop));

new_do_loop->cur_do_loop = (do_loop *) calloc(1,sizeof(do_loop));

strcpy(new_do_loop->cur_do_loop->do_variable,do_var);
strcpy(new_do_loop->cur_do_loop->do_begin,do_begin);
strcpy(new_do_loop->cur_do_loop->do_end,do_end);
strcpy(new_do_loop->cur_do_loop->do_step,do_step);
new_do_loop->suiv = NULL;

if (!var->v_do_loop)
{
  var->v_do_loop = new_do_loop;
}
else
{
  new_do_loop->suiv = var->v_do_loop;
  var->v_do_loop = new_do_loop;
      
//   tmploop = var->v_do_loop;
//   while (tmploop->suiv)
//   {
//     tmploop=tmploop->suiv;
//   }
//   tmploop->suiv = new_do_loop ;
//   }
}
}