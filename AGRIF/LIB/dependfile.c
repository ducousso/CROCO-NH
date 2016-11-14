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
/*            Creation and modification of .dependfile                        */
/******************************************************************************/
/*  .dependnbxnby            : this file contains tabvars indices of variables*/
/*                                given in agrif.in as number of cells        */
/*  .dependuse<module>       : this file contains all modules used in the     */
/*                                current file                                */
/*  .dependparameter<module> : this file contains all parmeters defined in    */
/*                                the current file                            */
/*  .depend<module name>     : this file contains all globals variables       */
/*                                informations (name, dim, etc ...)           */
/*  .dependavailable         : this file contains all tabvars indices which   */
/*                                are not used.                               */
/******************************************************************************/


/******************************************************************************/
/*                 Writethedependnbxnbyfile                                   */
/******************************************************************************/
/* This subroutine is used to create the .dependnbxnby                        */
/******************************************************************************/
/*                                                                            */
/*                     .dependnbxnby                                          */
/*                                                                            */
/*                     nbmaillesX                                             */
/*                     nbmaillesY                                             */
/*                     nbmaillesZ                                             */
/*                                                                            */
/******************************************************************************/
void Writethedependnbxnbyfile()
{
  FILE *dependfileoutput;
  listvar *parcours;
  int out;

  // We look in 'List_Global_Var' for all the variables of the current file to parse
  parcours = List_Global_Var;
  out = 0;
  while (parcours && out == 0 )
  {
     if ( !strcasecmp(parcours->var->v_nomvar,nbmaillesX) ) out = 1;
     else parcours = parcours->suiv;
  }
  if ( out == 0 )
  {
     parcours =List_Common_Var;
     while (parcours && out == 0 )
     {
        if ( !strcasecmp(parcours->var->v_nomvar,nbmaillesX) ) out = 1;
        else parcours = parcours->suiv;
     }
  }
  NbMailleXDefined = 0;
  if ( out == 1 )
  {
     NbMailleXDefined = 1;
     sprintf(dependfilename, "%s/.dependnbxnby", work_dir);
     dependfileoutput = fopen(dependfilename, "w");

     fprintf(dependfileoutput,"%d\n",parcours->var->v_indicetabvars);
     IndicenbmaillesX = parcours->var->v_indicetabvars;

     if ( dimprob > 1 )
     {
        parcours =List_Global_Var;
        out = 0;
        while (parcours && out == 0 )
        {
           if ( !strcasecmp(parcours->var->v_nomvar,nbmaillesY) ) out = 1;
           else parcours = parcours->suiv;
        }
        if ( out == 0 )
        {
           parcours =List_Common_Var;
           while (parcours && out == 0 )
           {
              if ( !strcasecmp(parcours->var->v_nomvar,nbmaillesY) ) out = 1;
              else parcours = parcours->suiv;
           }
        }
        if ( out == 1 )
        {
           fprintf(dependfileoutput,"%d\n",parcours->var->v_indicetabvars);
           IndicenbmaillesY = parcours->var->v_indicetabvars;
        }
     }

     if ( dimprob > 2 )
     {
        parcours =List_Global_Var;
        out = 0;
        while (parcours && out == 0 )
        {
           if ( !strcasecmp(parcours->var->v_nomvar,nbmaillesZ) ) out = 1;
           else parcours = parcours->suiv;
        }
        if ( out == 0 )
        {
           parcours =List_Common_Var;
           while (parcours && out == 0 )
           {
              if ( !strcasecmp(parcours->var->v_nomvar,nbmaillesZ) ) out = 1;
              else parcours = parcours->suiv;
           }
        }
        if ( out == 1 )
        {
           fprintf(dependfileoutput,"%d\n",parcours->var->v_indicetabvars);
           IndicenbmaillesZ = parcours->var->v_indicetabvars;
        }
     }

     if ( out == 1 ) fclose(dependfileoutput);
   }
}

/******************************************************************************/
/*                 Readthedependnbxnbyfile                                    */
/******************************************************************************/
/* This subroutine is used to create the .dependnbxnby                        */
/******************************************************************************/
/*                                                                            */
/*                     .dependnbxnby                                          */
/*                                                                            */
/*                     nbmaillesX                                             */
/*                     nbmaillesY                                             */
/*                     nbmaillesZ                                             */
/*                                                                            */
/******************************************************************************/
void Readthedependnbxnbyfile()
{
    FILE *dependfileoutput;

    sprintf(dependfilename, "%s/.dependnbxnby", work_dir);
    if ((dependfileoutput = fopen(dependfilename, "r"))!=NULL)
    {
        fscanf(dependfileoutput,"%d\n",&IndicenbmaillesX);
        if ( dimprob > 1 ) fscanf(dependfileoutput,"%d\n",&IndicenbmaillesY);
        if ( dimprob > 2 ) fscanf(dependfileoutput,"%d\n",&IndicenbmaillesZ);
        fclose(dependfileoutput);
    }
}

/******************************************************************************/
/*                     Writethedependlistofmoduleused                         */
/******************************************************************************/
/* This subroutine is used to create the .dependuse<module>                   */
/******************************************************************************/
/*                                                                            */
/*               .dependuse<name>                                             */
/*                                                                            */
/*               mod1                                                         */
/*               mod2                                                         */
/*                                                                            */
/******************************************************************************/
void Writethedependlistofmoduleused(const char *NameTampon )
{
    FILE *dependfileoutput;
    listusemodule *parcours;
    char lowername[LONG_VNAME];

    if ( ! List_NameOfModuleUsed ) return;

    convert2lower(lowername, NameTampon);
    sprintf(dependfilename, "%s/.depend_use%s", work_dir, lowername);
    dependfileoutput = fopen(dependfilename, "w");

    parcours = List_NameOfModuleUsed;
    while (parcours)
    {
    if ( !strcasecmp(lowername,parcours->u_modulename) &&
         !strcasecmp(parcours->u_cursubroutine,"") )
    {
        // We look in 'List_NameOfModuleUsed' for all the variables of the current file to parse
        fprintf(dependfileoutput,"%s\n",parcours->u_usemodule);
    }
    parcours = parcours->suiv;
    }
    fclose(dependfileoutput);
}

/******************************************************************************/
/*                    Readthedependlistofmoduleused                           */
/******************************************************************************/
/* This subroutine is used to create the .dependuse<module>                   */
/******************************************************************************/
/*                                                                            */
/*               .dependuse<name>                                             */
/*                                                                            */
/*               mod1                                                         */
/*               mod2                                                         */
/*                                                                            */
/******************************************************************************/
void Readthedependlistofmoduleused(const char *NameTampon)
{
  FILE *dependfileoutput;
  listusemodule *parcours;
  char lowername[LONG_VNAME];

  tmpuselocallist = (listusemodule *)NULL;

  convert2lower(lowername, NameTampon);
  sprintf(dependfilename, "%s/.depend_use%s", work_dir, lowername);

  if ((dependfileoutput = fopen(dependfilename, "r"))!=NULL)
  {
    /* if the file exist we should verify that this file has changed          */
      while (!feof(dependfileoutput))
      {
         parcours=(listusemodule *)calloc(1,sizeof(listusemodule));
         fscanf(dependfileoutput,"%s\n",parcours->u_usemodule);

         parcours->suiv = tmpuselocallist;
         tmpuselocallist = parcours;

         parcours = NULL;
      }
      fclose(dependfileoutput);
  }
}


/******************************************************************************/
/*                        WritedependParameterList                            */
/******************************************************************************/
/* This subroutine is used to create the .dependparameter<name>               */
/******************************************************************************/
/*                                                                            */
/*               .dependparameter<name>                                       */
/*                                                                            */
/*               mod1                                                         */
/*               mod2                                                         */
/*                                                                            */
/******************************************************************************/
void WritedependParameterList(const char *NameTampon )
{
  FILE *dependfileoutput;
  listvar *parcours;
  char lowername[LONG_VNAME];

  if ( List_GlobalParameter_Var )
  {
     convert2lower(lowername, NameTampon);
     sprintf(dependfilename, "%s/.depend_paramater_%s", work_dir, lowername);

     dependfileoutput = fopen(dependfilename, "w");
     parcours = List_GlobalParameter_Var;
     while (parcours)
     {
        if ( !strcasecmp(lowername, parcours->var->v_modulename) )
        {
           fprintf(dependfileoutput,"%s\n",parcours->var->v_nomvar);
           fprintf(dependfileoutput,"%s\n",parcours->var->v_modulename);
        }
        parcours = parcours->suiv;
     }
     fclose(dependfileoutput);
  }
}


/******************************************************************************/
/*                         ReaddependParameterList                            */
/******************************************************************************/
/* This subroutine is used to create the .dependparameter<name>               */
/******************************************************************************/
/*                                                                            */
/*               .dependparameter<name>                                       */
/*                                                                            */
/*               mod1                                                         */
/*               mod2                                                         */
/*                                                                            */
/******************************************************************************/
listparameter *ReaddependParameterList(const char *NameTampon,listparameter *listout)
{
  FILE *dependfileoutput;
  listparameter *parcours;
  char lowername[LONG_VNAME];

  convert2lower(lowername, NameTampon);
  sprintf(dependfilename, "%s/.depend_paramater_%s", work_dir, lowername);

  if ((dependfileoutput = fopen(dependfilename,"r"))!=NULL)
  {
    /* if the file exist we should verify that this file has changed          */
      while (!feof(dependfileoutput))
      {
         parcours=(listparameter *)calloc(1,sizeof(listparameter));
         fscanf(dependfileoutput,"%s\n",parcours->p_name);
         fscanf(dependfileoutput,"%s\n",parcours->p_modulename);

         parcours->suiv = listout;
         listout = parcours;

         parcours = NULL;
      }
      fclose(dependfileoutput);
  }
  return listout;
}

/******************************************************************************/
/*                   Writethedependfile                                       */
/******************************************************************************/
/* This subroutine is used to create the .depend<name>                        */
/******************************************************************************/
/*                                                                            */
/*                     .depend<name>                                          */
/*                                                                            */
/*                      REAL                                                  */
/*                      Variable                                              */
/*                      char dimension or T                                   */
/*                      table dimension                                       */
/*                      is type given                                         */
/*                      precision or T                                        */
/*                      initial value or T                                    */
/*                      indice in the tabvars                                 */
/*                      listdimension or T                                    */
/*                      -------------------------                             */
/*                                                                            */
/******************************************************************************/
void Writethedependfile(const char *NameTampon, listvar *input )
{
  FILE *dependfileoutput;
  listvar *parcours;
  listdim *dims;
  char ligne[LONG_M];
  char listdimension[LONG_M];
  char curname[LONG_M];
  char lowername[LONG_VNAME];
  int out;

  if ( input )
  {
    convert2lower(lowername, NameTampon);
    sprintf(dependfilename, "%s/.depend_%s", work_dir, lowername);

  dependfileoutput = fopen(dependfilename,"w");
  // We look in 'input' for all the variables of the current file to parse
  parcours =input;
  out = 0;
  strcpy(curname,"");
  while (parcours && out == 0 )
  {
     if ( !strcasecmp(parcours->var->v_modulename, lowername) ||
          !strcasecmp(parcours->var->v_commonname, lowername) )
     {
        /*                                                                    */
        if (  strcasecmp(curname,"") &&
             !strcasecmp(curname,parcours->var->v_nomvar) ) out = 1 ;
        if ( !strcasecmp(curname,"") ) strcpy(curname,parcours->var->v_nomvar);
        /*                                                                    */
        if ( out == 0 )
        {
           /********** TYPEVAR ************************************************/
           fprintf(dependfileoutput,"%s\n",parcours->var->v_typevar);
           /********** CATVAR ************************************************/
           fprintf(dependfileoutput,"%d\n",parcours->var->v_catvar);
           /********** NOMVAR *************************************************/
           fprintf(dependfileoutput,"%s\n",parcours->var->v_nomvar);
           /********** DIMCHAR ************************************************/
           if ( strcasecmp(parcours->var->v_dimchar, "") )
           {
              fprintf(dependfileoutput,"%s\n",parcours->var->v_dimchar);
           }
           else
           {
              fprintf(dependfileoutput,"T\n");
           }
           /********** COMMONINFILE *******************************************/
           if ( strcasecmp(parcours->var->v_commoninfile,"") )
           {
              fprintf(dependfileoutput,"%s\n",parcours->var->v_commoninfile);
           }
           else
           {
              fprintf(dependfileoutput,"T\n");
           }
           /********** COMMONNAME *********************************************/
           if ( strcasecmp(parcours->var->v_commonname,"") )
           {
              fprintf(dependfileoutput,"%s\n",parcours->var->v_commonname);
           }
           else
           {
              fprintf(dependfileoutput,"T\n");
           }
           /********** MODULENAME *********************************************/
           if ( strcasecmp(parcours->var->v_modulename,"") )
           {
              fprintf(dependfileoutput,"%s\n",parcours->var->v_modulename);
           }
           else
           {
              fprintf(dependfileoutput,"T\n");
           }
           /********** NBDIM **************************************************/
/*           fprintf(dependfileoutput,"%d\n",parcours->var->v_nbdim);*/
           /********** DIMENSIONGIVEN *****************************************/
/*           fprintf(dependfileoutput,"%d\n",parcours->var->v_dimensiongiven);*/
           /********** ALLOCATABLE ********************************************/
           fprintf(dependfileoutput,"%d\n",parcours->var->v_allocatable);
           /********** TARGET ********************************************/
           fprintf(dependfileoutput,"%d\n",parcours->var->v_target);
           /********** POINTERDECLARE *****************************************/
           fprintf(dependfileoutput,"%d\n",parcours->var->v_pointerdeclare);
           /********** PRECISION **********************************************/
           if ( strcasecmp(parcours->var->v_precision,"") )
           {
              fprintf(dependfileoutput,"%s\n",parcours->var->v_precision);
           }
           else
           {
              fprintf(dependfileoutput,"T\n");
           }
           /********** INITIALVALUE *******************************************/
/*           if ( strcasecmp(parcours->var->v_initialvalue,"") )
           {
              fprintf(dependfileoutput,"%s\n",parcours->var->v_initialvalue);
           }
           else
           {
              fprintf(dependfileoutput,"T\n");
           }*/
           /********** NAMEINTYPENAME *****************************************/
           if ( strcasecmp(parcours->var->v_nameinttypename,"") )
           {
              fprintf(dependfileoutput,"%s\n",parcours->var->v_nameinttypename);
           }
           else
           {
              fprintf(dependfileoutput,"T\n");
           }
           /********** PRIVATE *****************************************/
           fprintf(dependfileoutput,"%d\n",parcours->var->v_PrivateDeclare);

           /********** INDICETABVARS ******************************************/
           fprintf(dependfileoutput,"%d\n",parcours->var->v_indicetabvars);
           /********** READEDLISTDIMENSION ************************************/
           if ( parcours->var->v_dimensiongiven == 1 )
           {
              dims = parcours->var->v_dimension;
              strcpy(listdimension,"");
              while (dims)
              {
                 sprintf(ligne,"%s:%s",dims->dim.first,dims->dim.last);
                 strcat(listdimension,ligne);
                 if ( dims->suiv )
                 {
                    strcat(listdimension,",");
                 }
                 dims = dims->suiv;
              }
              Save_Length(listdimension,15);
              fprintf(dependfileoutput,"%s\n",listdimension);
           }
           else
           {
              fprintf(dependfileoutput,"T\n");
           }
           /*******************************************************************/
           fprintf(dependfileoutput,"------------------------\n");
        }
     }
     parcours = parcours->suiv;
  }
  fclose(dependfileoutput);
  }
}

/******************************************************************************/
/*                         Readthedependfile                                  */
/******************************************************************************/
/* This subroutine is used to read the .dependfile<name> and to insert new    */
/*    information in the listout list.                                        */
/******************************************************************************/
/*                                                                            */
/*           .dependmodule -------->                      = list of var       */
/*                                                                            */
/*        not.dependmodule -------->                                          */
/*                                                                            */
/******************************************************************************/
listvar *Readthedependfile(const char *NameTampon , listvar *listout)
{
  FILE *dependfileoutput;
  listvar *parcours0;
  listvar *parcours;
  listvar *parcoursprec;
  char nothing[LONG_M];
  char lowername[LONG_VNAME];
  size_t i;

  parcoursprec = (listvar *)NULL;

  convert2lower(lowername, NameTampon);
  sprintf(dependfilename, "%s/.depend_%s", work_dir, lowername);

  if ((dependfileoutput = fopen(dependfilename, "r"))==NULL)
  {
    /* if the file doesn't exist it means that it is the first time           */
    /*    we tried to parse this file                                         */
  }
  else
  {
    /* if the file exist we should verify that this file has changed          */
      while (!feof(dependfileoutput))
      {
         parcours=(listvar *)calloc(1,sizeof(listvar));
         parcours->var=(variable *)calloc(1,sizeof(variable));
         /*                                                                   */
         Init_Variable(parcours->var);
         /*                                                                   */
           /********** TYPEVAR ************************************************/
         fscanf(dependfileoutput,"%s\n",parcours->var->v_typevar);
           /********** CATVAR ************************************************/
         fscanf(dependfileoutput,"%d\n",&parcours->var->v_catvar);
           /********** NOMVAR *************************************************/
         fscanf(dependfileoutput,"%s\n",parcours->var->v_nomvar);
           /********** DIMCHAR ************************************************/
         fscanf(dependfileoutput,"%s\n",parcours->var->v_dimchar);
         if ( !strcasecmp(parcours->var->v_dimchar,"T") )
         {
            strcpy(parcours->var->v_dimchar,"");
         }
           /********** COMMONINFILE *******************************************/
         fscanf(dependfileoutput,"%s\n",parcours->var->v_commoninfile);
         if ( !strcasecmp(parcours->var->v_commoninfile,"T") )
         {
            strcpy(parcours->var->v_commoninfile,"");
         }
           /********** COMMONNAME *********************************************/
         fscanf(dependfileoutput,"%s\n",parcours->var->v_commonname);
         if ( !strcasecmp(parcours->var->v_commonname,"T") )
         {
            strcpy(parcours->var->v_commonname,"");
         }
           /********** MODULENAME *********************************************/
         fscanf(dependfileoutput,"%s\n",parcours->var->v_modulename);

         if ( !strcasecmp(parcours->var->v_modulename,"T") )
         {
            strcpy(parcours->var->v_modulename,"");
         }


           /********** NBDIM **************************************************/
/*         fscanf(dependfileoutput,"%d\n",&parcours->var->v_nbdim);*/
           /********** DIMENSIONGIVEN *****************************************/
/*         fscanf(dependfileoutput,"%d\n",&parcours->var->v_dimensiongiven);*/
           /********** ALLOCATABLE ********************************************/
         fscanf(dependfileoutput,"%d\n",&parcours->var->v_allocatable);
         if ( parcours->var->v_allocatable == 1 )
         {
            Add_Allocate_Var_1(parcours->var->v_nomvar, parcours->var->v_commonname);
         }
           /********** TARGET ********************************************/
         fscanf(dependfileoutput,"%d\n",&parcours->var->v_target);

           /********** POINTERDECLARE *****************************************/
         fscanf(dependfileoutput,"%d\n",&parcours->var->v_pointerdeclare);
         if ( parcours->var->v_pointerdeclare == 1 )
         {
            Add_Pointer_Var_1(parcours->var->v_nomvar);
         }
           /********** PRECISION **********************************************/
         fscanf(dependfileoutput,"%[^\n] \n",parcours->var->v_precision);
         if ( !strcasecmp(parcours->var->v_precision,"T") )
         {
            strcpy(parcours->var->v_precision,"");
         }
           /********** INITIALVALUE *******************************************/
/*         fscanf(dependfileoutput,"%[^\n] \n",parcours->var->v_initialvalue);
         if ( !strcasecmp(parcours->var->v_initialvalue,"T") )
         {
            strcpy(parcours->var->v_initialvalue,"");
         }*/
           /********** NAMEINTYPENAME *****************************************/
         fscanf(dependfileoutput,"%[^\n] \n",parcours->var->v_nameinttypename);
         if ( !strcasecmp(parcours->var->v_nameinttypename,"T") )
         {
            strcpy(parcours->var->v_nameinttypename,"");
         }
           /********** PRIVATE *****************************************/
         fscanf(dependfileoutput,"%d\n",&parcours->var->v_PrivateDeclare);

           /********** INDICETABVARS ******************************************/
         fscanf(dependfileoutput,"%d\n",&parcours->var->v_indicetabvars);
           /********** READEDLISTDIMENSION ************************************/
         fscanf(dependfileoutput,"%s\n",parcours->var->v_readedlistdimension);
         if ( !strcasecmp(parcours->var->v_readedlistdimension,"T") )
         {
            strcpy(parcours->var->v_readedlistdimension,"");
         }
         else
         {
            parcours->var->v_dimensiongiven = 1;
            parcours->var->v_nbdim = 1;
            i = 1;
            /*                                                                */
            while ( i < strlen(parcours->var->v_readedlistdimension) )
            {
               if ( parcours->var->v_readedlistdimension[i] == ',' )
               {
                  parcours->var->v_nbdim = parcours->var->v_nbdim + 1 ;
               }
               /*                                                             */
               i=i+1;
            }
         }
           /*******************************************************************/
         fscanf(dependfileoutput,"%s\n",nothing);
         parcours->suiv = NULL;
         if (parcours->var->v_PrivateDeclare == 0)
         {
         if ( !listout )
         {
            listout = parcours;
            parcoursprec = parcours;
         }
         else
         {
            if ( parcoursprec )
            {
               parcoursprec->suiv = parcours;
               parcoursprec = parcours;
            }
            else
            {
               parcours0 = listout;
               while ( parcours0->suiv ) parcours0=parcours0->suiv;
               parcours0->suiv = parcours;
               parcoursprec = parcours0->suiv;
            }
         }
         }
         parcours = NULL;
      }
      fclose(dependfileoutput);
  }
  return listout;
}

void Write_Subroutine_For_Alloc()
{
   FILE *dependfileoutput;
   listnom *parcours;

   if ( List_Subroutine_For_Alloc )
   {
      sprintf(dependfilename, "%s/.dependAllocAgrif", work_dir);

      if ((dependfileoutput=fopen(dependfilename, "w"))!=NULL)
      {
         parcours = List_Subroutine_For_Alloc;
         while (parcours)
         {
            fprintf(dependfileoutput,"%s\n",parcours->o_nom);
            parcours = parcours->suiv;
         }
         fclose(dependfileoutput);
      }
   }
}

void Read_Subroutine_For_Alloc()
{
    FILE *dependfileoutput;
    listnom *parcours;
    listnom *ref;

    ref = (listnom*) NULL;
    sprintf(dependfilename, "%s/.dependAllocAgrif", work_dir);

    if ( (dependfileoutput=fopen(dependfilename, "r")) != NULL )
    {
        List_Subroutine_For_Alloc = (listnom*) NULL;
        while ( !feof(dependfileoutput) )
        {
            parcours = (listnom*) calloc(1,sizeof(listnom));
            strcpy(parcours->o_nom,"");

            fscanf(dependfileoutput,"%s\n",parcours->o_nom);
            parcours->suiv = NULL;

            if ( !List_Subroutine_For_Alloc )
            {
                List_Subroutine_For_Alloc = parcours;
            }
            else
            {
                ref->suiv = parcours;
            }
            ref = parcours;
        }
        fclose(dependfileoutput);
    }
}

/******************************************************************************/
/*                        Writethedependavailablefile                         */
/******************************************************************************/
/* This subroutine is used to write the .dependfileavailable file             */
/******************************************************************************/
/*                                                                            */
/*                                  .dependavailable                          */
/*     tabvars(1) = var1                                                      */
/*     tabvars(3) = var1                  2                                   */
/*     tabvars(4) = var1         =====>   5                                   */
/*     tabvars(6) = var1                                                      */
/*     tabvars(7) = var1                                                      */
/*                                                                            */
/*                                                                            */
/*                                                                            */
/******************************************************************************/
void Writethedependavailablefile()
{
    FILE *dependfileoutput;
    listindice *parcours;
    int i;

    sprintf(dependfilename, "%s/.dependavailable", work_dir);

    if ((dependfileoutput=fopen(dependfilename, "w"))!=NULL)
    {
        /* We are looking for all the indices of the Listofavailableindices      */
        for (i=0;i<NB_CAT_VARIABLES;i++)
        {
            parcours = Listofavailableindices_glob[i];
            while (parcours)
            {
                if ( parcours->i_indice != 0 )
                {
                    fprintf(dependfileoutput,"%d %d\n",i,parcours->i_indice);
                }
                parcours = parcours->suiv;
            }
        }
        fclose(dependfileoutput);
    }
}

/******************************************************************************/
/*                        Readthedependavailablefile                          */
/******************************************************************************/
/* This subroutine is used to read the .dependfileavailable file              */
/******************************************************************************/
/*                                                                            */
/*                                  .dependavailable                          */
/*     tabvars(1) = var1                                                      */
/*     tabvars(3) = var1                  2                                   */
/*     tabvars(4) = var1         =====>   5  ==> Listofavailableindices       */
/*     tabvars(6) = var1                                                      */
/*     tabvars(7) = var1                                                      */
/*                                                                            */
/*                                                                            */
/*                                                                            */
/******************************************************************************/
void Readthedependavailablefile()
{
  FILE *dependfileoutput;
  listindice *parcours;
  int current_cat;

  sprintf(dependfilename, "%s/.dependavailable", work_dir);

  if ((dependfileoutput=fopen(dependfilename, "r"))!=NULL)
  {
     /* We are looking for all the indices of the Listofavailableindices      */
     Listofavailableindices_glob = (listindice **) calloc(NB_CAT_VARIABLES,sizeof(listindice *));
     while (!feof(dependfileoutput))
     {
        parcours=(listindice *)calloc(1,sizeof(listindice));
        fscanf(dependfileoutput,"%d %d\n",&current_cat,&parcours->i_indice);
        if ( parcours->i_indice != 0 && parcours->i_indice < 10000000 )
        {
           parcours -> suiv = Listofavailableindices_glob[current_cat];
           Listofavailableindices_glob[current_cat] = parcours;
        }
        else
        {
           free(parcours);
        }
     }
     fclose(dependfileoutput);
  }
}


/******************************************************************************/
/*                       is_dependfile_created                                */
/******************************************************************************/
/* This subroutine is used to know if the .depend<NameTampon> exist           */
/*    it means if the file has been ever parsed                               */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
int is_dependfile_created(const char *NameTampon)
{
  FILE *dependfileoutput;
  char lowername[LONG_VNAME];

  convert2lower(lowername, NameTampon);
  sprintf(dependfilename, "%s/.depend_%s", work_dir, lowername);

  dependfileoutput = fopen(dependfilename, "r");

  if ( (dependfileoutput = fopen(dependfilename, "r")) != NULL )
  {
    fclose(dependfileoutput);
    return 1;
  }
  else
    return 0;
}

void Write_val_max()
{
    FILE *dependfileoutput;

    sprintf(dependfilename, "%s/.dependvalmax", work_dir);

    if ((dependfileoutput=fopen(dependfilename, "w"))!=NULL)
    {
        fprintf(dependfileoutput,"length_last\n");
        fprintf(dependfileoutput,"%lu\n", length_last);
        fprintf(dependfileoutput,"length_first\n");
        fprintf(dependfileoutput,"%lu\n", length_first);
        fprintf(dependfileoutput,"length_v_vallengspec\n");
        fprintf(dependfileoutput,"%lu\n", length_v_vallengspec);
        fprintf(dependfileoutput,"length_v_commoninfile\n");
        fprintf(dependfileoutput,"%lu\n", length_v_commoninfile);
        fprintf(dependfileoutput,"length_v_precision\n");
        fprintf(dependfileoutput,"%lu\n", length_v_precision);
        fprintf(dependfileoutput,"length_v_IntentSpec\n");
        fprintf(dependfileoutput,"%lu\n", length_v_IntentSpec);
        fprintf(dependfileoutput,"length_v_initialvalue\n");
        fprintf(dependfileoutput,"%lu\n", length_v_initialvalue);
        fprintf(dependfileoutput,"length_v_readedlistdimension\n");
        fprintf(dependfileoutput,"%lu\n", length_v_readedlistdimension);
        fprintf(dependfileoutput,"length_a_nomvar\n");
        fprintf(dependfileoutput,"%lu\n", length_a_nomvar);
        fprintf(dependfileoutput,"length_toprintglob\n");
        fprintf(dependfileoutput,"%lu\n", length_toprintglob);
        fprintf(dependfileoutput,"Size_char0d\n");
        fprintf(dependfileoutput,"%d\n",value_char_size);
        fprintf(dependfileoutput,"Size_char1d\n");
        fprintf(dependfileoutput,"%d\n",value_char_size1);
        fprintf(dependfileoutput,"Size_char2d\n");
        fprintf(dependfileoutput,"%d\n",value_char_size2);
        fprintf(dependfileoutput,"Size_char3d\n");
        fprintf(dependfileoutput,"%d\n",value_char_size3);
        fprintf(dependfileoutput,"length_tmpvargridname\n");
        fprintf(dependfileoutput,"%lu\n", length_tmpvargridname);
        fprintf(dependfileoutput,"length_ligne_Subloop\n");
        fprintf(dependfileoutput,"%lu\n", length_ligne_Subloop);
        fprintf(dependfileoutput,"length_toprint_toamr\n");
        fprintf(dependfileoutput,"%lu\n", length_toprint_utilagrif);
        fprintf(dependfileoutput,"length_toprinttmp_utilchar\n");
        fprintf(dependfileoutput,"%lu\n", length_toprinttmp_utilchar);
        fprintf(dependfileoutput,"length_ligne_writedecl\n");
        fprintf(dependfileoutput,"%lu\n", length_ligne_writedecl);
        fprintf(dependfileoutput,"length_newname_toamr\n");
        fprintf(dependfileoutput,"%lu\n", length_newname_toamr);
        fprintf(dependfileoutput,"length_newname_writedecl\n");
        fprintf(dependfileoutput,"%lu\n", length_newname_writedecl);
        fprintf(dependfileoutput,"length_ligne_toamr\n");
        fprintf(dependfileoutput,"%lu\n", length_ligne_toamr);
        fprintf(dependfileoutput,"length_tmpligne_writedecl\n");
        fprintf(dependfileoutput,"%lu\n", length_tmpligne_writedecl);

        fclose(dependfileoutput);
    }
}


void Read_val_max()
{
    char nothing[LONG_M];
    FILE *dependfileoutput;

    sprintf(dependfilename, "%s/.dependvalmax", work_dir);

    if ((dependfileoutput=fopen(".dependvalmax","r"))!=NULL)
    {
       fscanf(dependfileoutput,"%s\n",nothing);
       fscanf(dependfileoutput,"%lu\n", &length_last);
       fscanf(dependfileoutput,"%s\n",nothing);
       fscanf(dependfileoutput,"%lu\n", &length_first);
       fscanf(dependfileoutput,"%s\n",nothing);
       fscanf(dependfileoutput,"%lu\n", &length_v_vallengspec);
       fscanf(dependfileoutput,"%s\n",nothing);
       fscanf(dependfileoutput,"%lu\n", &length_v_commoninfile);
       fscanf(dependfileoutput,"%s\n",nothing);
       fscanf(dependfileoutput,"%lu\n", &length_v_precision);
       fscanf(dependfileoutput,"%s\n",nothing);
       fscanf(dependfileoutput,"%lu\n", &length_v_IntentSpec);
       fscanf(dependfileoutput,"%s\n",nothing);
       fscanf(dependfileoutput,"%lu\n", &length_v_initialvalue);
       fscanf(dependfileoutput,"%s\n",nothing);
       fscanf(dependfileoutput,"%lu\n", &length_v_readedlistdimension);
       fscanf(dependfileoutput,"%s\n",nothing);
       fscanf(dependfileoutput,"%lu\n", &length_a_nomvar);
       fscanf(dependfileoutput,"%s\n",nothing);
       fscanf(dependfileoutput,"%lu\n", &length_toprintglob);
       fscanf(dependfileoutput,"%s\n",nothing);
       fscanf(dependfileoutput,"%d\n", &value_char_size);
       fscanf(dependfileoutput,"%s\n", nothing);
       fscanf(dependfileoutput,"%d\n", &value_char_size1);
       fscanf(dependfileoutput,"%s\n", nothing);
       fscanf(dependfileoutput,"%d\n", &value_char_size2);
       fscanf(dependfileoutput,"%s\n", nothing);
       fscanf(dependfileoutput,"%d\n", &value_char_size3);
       fscanf(dependfileoutput,"%s\n",nothing);
       fscanf(dependfileoutput,"%lu\n", &length_tmpvargridname);
       fscanf(dependfileoutput,"%s\n",nothing);
       fscanf(dependfileoutput,"%lu\n", &length_ligne_Subloop);
       fscanf(dependfileoutput,"%s\n",nothing);
       fscanf(dependfileoutput,"%lu\n", &length_toprint_utilagrif);
       fscanf(dependfileoutput,"%s\n",nothing);
       fscanf(dependfileoutput,"%lu\n", &length_toprinttmp_utilchar);
       fscanf(dependfileoutput,"%s\n",nothing);
       fscanf(dependfileoutput,"%lu\n", &length_ligne_writedecl);
       fscanf(dependfileoutput,"%s\n",nothing);
       fscanf(dependfileoutput,"%lu\n", &length_newname_toamr);
       fscanf(dependfileoutput,"%s\n",nothing);
       fscanf(dependfileoutput,"%lu\n", &length_newname_writedecl);
       fscanf(dependfileoutput,"%s\n",nothing);
       fscanf(dependfileoutput,"%lu\n", &length_ligne_toamr);
       fscanf(dependfileoutput,"%s\n",nothing);
       fscanf(dependfileoutput,"%lu\n", &length_tmpligne_writedecl);

       fclose(dependfileoutput);
    }
}
