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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "decl.h"

const char * tabvarsname(const variable *var)
{
    static char * tname[5] = {
        "tabvars",      // v_catvar == 0
        "tabvars_c",    // v_catvar == 1
        "tabvars_r",    // v_catvar == 2
        "tabvars_l",    // v_catvar == 3
        "tabvars_i"     // v_catvar == 4
    };

    return tname[var->v_catvar];    // v_catvar should never be ouside the range [0:4].
}

/******************************************************************************/
/*                        variablecurgridtabvars                              */
/******************************************************************************/
/* This subroutine is used to create the string                               */
/******************************************************************************/
/*                                                                            */
/*  ----------->  Agrif_Curgrid % tabvars (i)                                 */
/*                                                                            */
/******************************************************************************/
const char * variablecurgridtabvars(int which_grid)
{
    static char * varname[4] = {
        " Agrif_%s(%d)",                // which_grid == 0
        " Agrif_%s(%d) %% parent_var",  // which_grid == 1
        " Agrif_Mygrid %% %s(%d)",      // which_grid == 2
        " Agrif_Curgrid %% %s(%d)",     // which_grid == 3
    };

    return varname[which_grid];
}

void WARNING_CharSize(const variable *var)
{
    if ( var->v_nbdim == 0 )
    {
        if ( convert2int(var->v_dimchar) > 2400 )
        {
            printf("WARNING : The dimension of the character  %s   \n", var->v_nomvar);
            printf("   is upper than 2400. You must change         \n");
            printf("   the dimension of carray0                    \n");
            printf("   in the file AGRIF/AGRIF_FILES/modtypes.F90  \n");
            printf("   line 161. Replace 2400 with %d.              \n", convert2int(var->v_dimchar)+100);
        }
        Save_Length_int(convert2int(var->v_dimchar),1);
    }
    else if ( var->v_nbdim == 1 )
    {
        if ( convert2int(var->v_dimchar) > 200 )
        {
            printf("WARNING : The dimension of the character  %s   \n", var->v_nomvar);
            printf("   is upper than 200. You must change          \n");
            printf("   the dimension of carray1                    \n");
            printf("   in the file AGRIF/AGRIF_FILES/modtypes.F90  \n");
            printf("   line 162. Replace 200 with %d.              \n", convert2int(var->v_dimchar)+100);
        }
        Save_Length_int(convert2int(var->v_dimchar),2);
    }
    else if ( var->v_nbdim == 2 )
    {
        if ( convert2int(var->v_dimchar) > 200 )
        {
            printf("WARNING : The dimension of the character  %s   \n", var->v_nomvar);
            printf("   is upper than 200. You must change          \n");
            printf("   the dimension of carray2                    \n");
            printf("   in the file AGRIF/AGRIF_FILES/modtypes.F90  \n");
            printf("   line 163. Replace 200 with %d.              \n", convert2int(var->v_dimchar)+100);
        }
        Save_Length_int(convert2int(var->v_dimchar),3);
    }
    else if ( var->v_nbdim == 3 )
    {
        if ( convert2int(var->v_dimchar) > 200 )
        {
            printf("WARNING : The dimension of the character  %s   \n", var->v_nomvar);
            printf("   is upper than 200. You must change          \n");
            printf("   the dimension of carray3                    \n");
            printf("   in the file AGRIF/AGRIF_FILES/modtypes.F90  \n");
            printf("   line 164. Replace 200 with %d.              \n", convert2int(var->v_dimchar)+100);
        }
        Save_Length_int(convert2int(var->v_dimchar),4);
    }
}
/******************************************************************************/
/*                           vargridnametabvars                               */
/******************************************************************************/
/* This subroutine is used to create the string                               */
/******************************************************************************/
/*                                                                            */
/*  if iorindice == 0 ----------->  Agrif_Gr % tabvars (i) % array1           */
/*                                                                            */
/*  if iorindice == 1 ----------->  Agrif_Gr % tabvars (12) % array1          */
/*                                                                            */
/******************************************************************************/
const char *vargridnametabvars (const variable * var, int iorindice)
{
    static char tname_1[LONG_C];
    static char tname_2[LONG_C];
    
    if ( iorindice == 0 ) sprintf(tname_1, "Agrif_Gr %% %s(%d)", tabvarsname(var), var->v_indicetabvars);
    else                  sprintf(tname_1, "Agrif_Gr %% %s(i)",  tabvarsname(var));

    if (!strcasecmp(var->v_typevar, "real"))
    {
        if      ( !strcasecmp(var->v_nameinttypename,"8") ) sprintf(tname_2, "%% darray%d", var->v_nbdim);
        else if ( !strcasecmp(var->v_nameinttypename,"4") ) sprintf(tname_2, "%% sarray%d", var->v_nbdim);
        else if ( !strcasecmp(var->v_precision,"8") ) sprintf(tname_2, "%% darray%d", var->v_nbdim);
        else if ( !strcasecmp(var->v_precision,"4") ) sprintf(tname_2, "%% sarray%d", var->v_nbdim);        
        else  
          {
          sprintf(tname_2, "%% array%d",  var->v_nbdim);
          }
    }
    else if (!strcasecmp(var->v_typevar, "integer"))
    {
        sprintf(tname_2, "%% iarray%d", var->v_nbdim);
    }
    else if (!strcasecmp(var->v_typevar, "logical"))
    {
        sprintf(tname_2, "%% larray%d", var->v_nbdim);
    }
    else if (!strcasecmp(var->v_typevar, "character"))
    {
        WARNING_CharSize(var);
        sprintf (tname_2, "%% carray%d", var->v_nbdim);
    }

    strcat(tname_1, tname_2);
    Save_Length(tname_1, 46);

    return tname_1;
}

/******************************************************************************/
/*                           vargridcurgridtabvars                            */
/******************************************************************************/
/* This subroutine is used to create the string                               */
/******************************************************************************/
/*                                                                            */
/* if which_grid == 0 -->  Agrif_Curgrid % tabvars (i) % array1               */
/*                                                                            */
/* if which_grid == 1 -->  Agrif_tabvars (i) % parent_var % array1            */
/*                                                                            */
/* if which_grid == 2 -->  Agrif_Gr % tabvars (i) % array1                    */
/*                                                                            */
/******************************************************************************/
const char *vargridcurgridtabvars(const variable *var, int which_grid)
{
    static char tname_1[LONG_C];
    static char tname_2[LONG_C];

    if (!strcasecmp(var->v_typevar,"type"))
    {
        sprintf(tname_1, "Agrif_%s_var(Agrif_Curgrid%%fixedrank)%%%s", var->v_modulename, var->v_nomvar);
    }
    else
    {
        sprintf(tname_1, variablecurgridtabvars(which_grid), tabvarsname(var), var->v_indicetabvars);

        if (!strcasecmp(var->v_typevar, "REAL"))
        {
            if      ( !strcasecmp(var->v_nameinttypename,"8") ) sprintf(tname_2, "darray%d", var->v_nbdim);
            else if ( !strcasecmp(var->v_nameinttypename,"4") ) sprintf(tname_2, "sarray%d", var->v_nbdim);
            else if ( !strcasecmp(var->v_precision,"8") ) sprintf(tname_2, "darray%d", var->v_nbdim);
            else if ( !strcasecmp(var->v_precision,"4") ) sprintf(tname_2, "sarray%d", var->v_nbdim);
            else sprintf(tname_2, "array%d", var->v_nbdim);
        }
        else if (!strcasecmp(var->v_typevar, "INTEGER"))
        {
            sprintf(tname_2, "iarray%d", var->v_nbdim);
        }
        else if (!strcasecmp(var->v_typevar, "LOGICAL"))
        {
            sprintf(tname_2, "larray%d", var->v_nbdim);
        }
        else if (!strcasecmp(var->v_typevar, "CHARACTER"))
        {
            WARNING_CharSize(var);
            sprintf(tname_2, "carray%d", var->v_nbdim);
        }
        if (var->v_pointerdeclare)
        {
                strcat(tname_1,"%p");
                strcat(tname_1, tname_2);
        }
        else
        {
                strcat(tname_1,"%");
                strcat(tname_1, tname_2);
        }
    }
    Save_Length(tname_1, 46);

    return tname_1;
}

/******************************************************************************/
/*                  vargridcurgridtabvarswithoutAgrif_Gr                      */
/******************************************************************************/
/* This subroutine is used to create the string                               */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
const char *vargridcurgridtabvarswithoutAgrif_Gr(const variable *var)
{
    static char tname_1[LONG_C];
    static char tname_2[LONG_C];

    sprintf(tname_1, "(%d)", var->v_indicetabvars);
    
        if (!strcasecmp(var->v_typevar, "REAL"))
        {
            if      ( !strcasecmp(var->v_nameinttypename,"8") ) sprintf(tname_2, "darray%d", var->v_nbdim);
            else if ( !strcasecmp(var->v_nameinttypename,"4") ) sprintf(tname_2, "sarray%d", var->v_nbdim);
            else if ( !strcasecmp(var->v_precision,"8") ) sprintf(tname_2, "darray%d", var->v_nbdim);
            else if ( !strcasecmp(var->v_precision,"4") ) sprintf(tname_2, "sarray%d", var->v_nbdim);
            else sprintf(tname_2, "array%d", var->v_nbdim);
        }
        else if (!strcasecmp(var->v_typevar, "INTEGER"))
        {
            sprintf(tname_2, "iarray%d", var->v_nbdim);
        }
        else if (!strcasecmp(var->v_typevar, "LOGICAL"))
        {
            sprintf(tname_2, "larray%d", var->v_nbdim);
        }
        else if (!strcasecmp(var->v_typevar, "CHARACTER"))
        {
            WARNING_CharSize(var);
            sprintf(tname_2, "carray%d", var->v_nbdim);
        }
        if (var->v_pointerdeclare)
        {
                strcat(tname_1,"%p");
                strcat(tname_1, tname_2);
        }
        else
        {
                strcat(tname_1,"%");
                strcat(tname_1, tname_2);
        }

    Save_Length(tname_1, 46);

    return tname_1;
}

/******************************************************************************/
/*                               vargridparam                                 */
/******************************************************************************/
/* This subroutine is used to create the string which contains                */
/* dimension list                                                             */
/******************************************************************************/
/*                                                                            */
/*  DIMENSION(jpi,0:jpj) ----------->"1:jpi,0:jpj"                            */
/*                                                                            */
/******************************************************************************/
const char * vargridparam(const variable *var)
{
    typedim dim;
    listdim *newdim;
    char newname[LONG_M];

    newdim = var->v_dimension;
    if (!newdim) return "";

    strcpy (tmpvargridname, "(");
    while (newdim)
    {
        dim = newdim->dim;
        strcpy(newname,ChangeTheInitalvaluebyTabvarsName(dim.first,List_Global_Var));
        strcpy(newname,ChangeTheInitalvaluebyTabvarsName(newname,List_Common_Var));
        strcpy(newname,ChangeTheInitalvaluebyTabvarsName(newname,List_ModuleUsed_Var));
        strcat(tmpvargridname, newname);
        strcat(tmpvargridname, " : ");
        strcpy(newname,ChangeTheInitalvaluebyTabvarsName(dim.last,List_Global_Var));
        strcpy(newname,ChangeTheInitalvaluebyTabvarsName(newname, List_Common_Var));
        strcpy(newname,ChangeTheInitalvaluebyTabvarsName(newname, List_ModuleUsed_Var));
        strcat(tmpvargridname, newname);
        newdim = newdim->suiv;
        if (newdim) strcat(tmpvargridname, ",");
    }
    strcat(tmpvargridname, ")\0");
    Save_Length(tmpvargridname,40);
    return tmpvargridname;
}

/******************************************************************************/
/*                        write_probdimagrif_file                             */
/******************************************************************************/
/* This subroutine is used to create the file probdim_agrif.h                 */
/******************************************************************************/
/*                                                                            */
/*               probdim_agrif.h                                              */
/*                                                                            */
/*               Agrif_probdim = <number>                                     */
/*                                                                            */
/******************************************************************************/
void write_probdimagrif_file()
{
  FILE *probdim;
  char ligne[LONG_M];

  probdim = open_for_write("probdim_agrif.h");
  sprintf (ligne, "Agrif_Probdim = %d", dimprob);
  tofich (probdim, ligne,1);
  fclose (probdim);
}

/******************************************************************************/
/*                             write_keysagrif_file                           */
/******************************************************************************/
/* This subroutine is used to create the file keys_agrif.h                    */
/******************************************************************************/
/*                                                                            */
/*               keys_agrif.h                                                 */
/*                                                                            */
/*               AGRIF_USE_FIXED_GRIDS = 0                                    */
/*               AGRIF_USE_ONLY_FIXED_GRIDS = 0                               */
/*               AGRIF_USE_(ONLY)_FIXED_GRIDS = 1                             */
/*                                                                            */
/******************************************************************************/
void write_keysagrif_file()
{
  FILE *keys;

  keys = open_for_write("keys_agrif.h");
  fprintf(keys,"      AGRIF_USE_FIXED_GRIDS = %d\n", fixedgrids);
  fprintf(keys,"      AGRIF_USE_ONLY_FIXED_GRIDS = %d\n", onlyfixedgrids);
  fclose(keys);
}

/******************************************************************************/
/*                      write_modtypeagrif_file                               */
/******************************************************************************/
/* This subroutine is used to create the file typedata                        */
/******************************************************************************/
/*                                                                            */
/*               modtype_agrif.h                                              */
/*                                                                            */
/*               Agrif_NbVariables =                                          */
/*                                                                            */
/******************************************************************************/
void write_modtypeagrif_file()
{
  char ligne[LONG_M];
  FILE *typedata;
  int i;

  typedata = open_for_write("modtype_agrif.h");
  /* AGRIF_NbVariables : number of variables                                  */
  for (i=0;i<NB_CAT_VARIABLES;i++)
   {
    sprintf (ligne, "Agrif_NbVariables(%d) = %d",i,indicemaxtabvars[i]);
    tofich(typedata,ligne,1);
   }
  fclose (typedata);
}

/******************************************************************************/
/*                   write_createvarnameagrif_file                            */
/******************************************************************************/
/* This subroutine is used to create the file  createvarname                  */
/******************************************************************************/
/*                                                                            */
/*    Agrif_Gr % tabvars (i) % namevar = "variable"                           */
/*                                                                            */
/******************************************************************************/
void write_createvarnameagrif_file(variable *v,FILE *createvarname, int *InitEmpty)
{
    char ligne[LONG_M];

    *InitEmpty = 0 ;
    sprintf(ligne, "Agrif_Gr %% %s(%d) %% namevar = \"%s\"",tabvarsname(v),v->v_indicetabvars,v->v_nomvar);
    tofich(createvarname,ligne,1);
}

/******************************************************************************/
/*                        write_Setnumberofcells_file                         */
/******************************************************************************/
/* This subroutine is used to create the file  setnumberofcells               */
/******************************************************************************/
/*                                                                            */
/*              Agrif_Gr % n(i) = nbmailles                                   */
/*                                                                            */
/******************************************************************************/
void write_Setnumberofcells_file()
{
    char ligne[LONG_VNAME];
    char cformat[LONG_VNAME];
    FILE *setnumberofcells;

    if ( IndicenbmaillesX == 0 )  return;

    setnumberofcells = open_for_write("SetNumberofcells.h");

    if ( onlyfixedgrids == 1 )
        strcpy(cformat, "Agrif_Gr %% nb(%d) = Agrif_Curgrid %% tabvars_i(%d) %% iarray0");
    else
        strcpy(cformat, "Agrif_Gr %% nb(%d) = Agrif_Gr %% tabvars_i(%d) %% iarray0");

    sprintf(ligne, cformat, 1, IndicenbmaillesX);
    tofich(setnumberofcells, ligne, 1);

    if ( dimprob > 1 )
    {
        sprintf(ligne, cformat, 2, IndicenbmaillesY);
        tofich(setnumberofcells, ligne, 1);
    }
    if ( dimprob > 2 )
    {
        sprintf(ligne, cformat, 3, IndicenbmaillesZ);
        tofich(setnumberofcells, ligne, 1);
    }
    fclose(setnumberofcells);
}

/******************************************************************************/
/*                       write_Getnumberofcells_file                          */
/******************************************************************************/
/* This subroutine is used to create the file  getnumberofcells               */
/******************************************************************************/
/*                                                                            */
/*              nbmailles = Agrif_Gr % n(i)                                   */
/*                                                                            */
/******************************************************************************/
void write_Getnumberofcells_file()
{
    char ligne[LONG_VNAME];
    char cformat[LONG_VNAME];
    FILE *getnumberofcells;

    if ( IndicenbmaillesX == 0 )    return;

    strcpy(cformat, "Agrif_Curgrid %% tabvars_i(%d) %% iarray0 = Agrif_Gr %% nb(%d)");

    getnumberofcells = open_for_write("GetNumberofcells.h");

    sprintf(ligne, cformat, IndicenbmaillesX, 1);
    tofich(getnumberofcells, ligne, 1);

    if (dimprob > 1)
    {
        sprintf(ligne, cformat, IndicenbmaillesY, 2);
        tofich(getnumberofcells, ligne,1);
    }
    if (dimprob > 2)
    {
        sprintf(ligne, cformat, IndicenbmaillesZ, 3);
        tofich(getnumberofcells, ligne,1);
    }
    fclose(getnumberofcells);
}


/******************************************************************************/
/*                      write_initialisationsagrif_file                       */
/******************************************************************************/
/* This subroutine is used to create the file initproc                        */
/******************************************************************************/
/*                                                                            */
/*              ! variable                                                    */
/*              Agrif_Gr % tabvars(i) % nbdim = 1                             */
/*                                                                            */
/******************************************************************************/
void write_initialisationsagrif_file(variable *v,FILE *initproc,int *VarnameEmpty)
{
    char ligne[LONG_M];

    if ( v->v_nbdim != 0 )
    {
        *VarnameEmpty = 0 ;
        sprintf(ligne,"Agrif_Mygrid %% %s(%d) %% nbdim = %d", tabvarsname(v), v->v_indicetabvars, v->v_nbdim);
        tofich (initproc, ligne,1);
    }
}


void Write_Alloc_Agrif_Files()
{
   listnom *parcours;
   FILE *alloccalls;
   FILE *AllocUSE;

   AllocUSE= open_for_write("include_use_Alloc_agrif.h");
   alloccalls = open_for_write("allocations_calls_agrif.h");

   parcours = List_Subroutine_For_Alloc;
   while ( parcours )
   {
      fprintf(AllocUSE,"      use %s, only: Alloc_agrif_%s\n", parcours -> o_nom, parcours -> o_nom );
      fprintf (alloccalls,"      call Alloc_agrif_%s(Agrif_Gr)\n", parcours -> o_nom );
      parcours = parcours -> suiv;
   }

   fclose (AllocUSE);
   fclose (alloccalls);
}

int IndiceInlist(int indic, listindice *listin)
{
   listindice *parcoursindic;
   int out;

   out = 0 ;

   parcoursindic = listin;
   while ( parcoursindic && out == 0 )
   {
      if ( parcoursindic->i_indice == indic ) out = 1;
      else parcoursindic = parcoursindic -> suiv;
   }

   return out;
}

void write_allocation_Common_0()
{
    listnom *parcours_nom;
    listnom *neededparameter;
    listvar *parcours;
    listvar *parcoursprec;
    listvar *parcours1;
    listname *parcours_name;
    listname *parcours_name_array;
    listdoloop *parcours_loop;
    FILE *allocationagrif;
    FILE *paramtoamr;
    char ligne[LONG_M];
    char ligne2[LONG_M];
    char ligne3[LONG_M];
    variable *v;
    int IndiceMax;
    int IndiceMin;
    int compteur;
    int out;
    int indiceprec;
    int ValeurMax;
    char initialvalue[LONG_M];
    listindice **list_indic;
    listindice *parcoursindic;
    int i;
    int nb_initial;
    int is_parameter_local;
    int global_check;

    parcoursprec = (listvar *) NULL;
    parcours_nom = List_NameOfCommon;
    ValeurMax = 2;
    while ( parcours_nom  )
    {
        if ( parcours_nom->o_val == 1 )
        {
            /* Open the file to create the Alloc_agrif subroutine                */
            sprintf(ligne,"alloc_agrif_%s.h",parcours_nom->o_nom);
            allocationagrif = open_for_write(ligne);

            fprintf(allocationagrif,"#include \"Param_toamr_%s.h\" \n", parcours_nom->o_nom);
            sprintf(ligne,"Param_toamr_%s.h",parcours_nom->o_nom);
            paramtoamr = open_for_write(ligne);
            neededparameter = (listnom *) NULL;
            list_indic = (listindice **) calloc(NB_CAT_VARIABLES,sizeof(listindice *));

             shouldincludempif = 1 ;
            parcours = List_Common_Var;
            while ( parcours )
            {
                if ( !strcasecmp(parcours->var->v_commonname,parcours_nom->o_nom) &&
                    IndiceInlist(parcours->var->v_indicetabvars,list_indic[parcours->var->v_catvar]) == 0 )
                {
                    v = parcours->var;
                    IndiceMax = 0;
                    IndiceMin = indicemaxtabvars[v->v_catvar];
                    /* body of the file */
                    if ( !strcasecmp(v->v_commoninfile,cur_filename) )
                    {
                        if ( (onlyfixedgrids != 1) && (v->v_nbdim != 0) )
                        {
                            sprintf(ligne,"if (.not. allocated(%s)) then", vargridnametabvars(v,0));
                            tofich(allocationagrif,ligne,1);
                        }
                        if ( (v->v_allocatable != 1) && (v->v_dimsempty != 1) )
                        {
                            /*                ALLOCATION                                          */
                            if ( v->v_dimension != 0 )
                            {
                                if ( v->v_indicetabvars < IndiceMin || v->v_indicetabvars > IndiceMax )
                                {
                                    parcours1 = parcours;
                                    compteur = -1;
                                    out = 0;
                                    indiceprec = parcours->var->v_indicetabvars -1 ;
                                    while ( parcours1 && out == 0
                                        && !strcasecmp(parcours->var->v_readedlistdimension,parcours1->var->v_readedlistdimension)
                                        && !strcasecmp(parcours->var->v_typevar,            parcours1->var->v_typevar)
                                        && (parcours1->var->v_indicetabvars == indiceprec+1) )
                                    {
                                        if ( !strcasecmp(parcours1->var->v_modulename,parcours_nom->o_nom) ||
                                             !strcasecmp(parcours1->var->v_commonname,parcours_nom->o_nom) )
                                        {
                                            compteur = compteur +1 ;
                                            indiceprec = parcours1->var->v_indicetabvars;
                                            parcoursprec = parcours1;
                                            parcours1 = parcours1->suiv;
                                        }
                                        else out = 1;
                                    }
                                    sprintf(ligne,"!! ALLOCATION OF VARIABLE : %s",v->v_nomvar);
                                    tofich(allocationagrif,ligne,1);
                                    if ( compteur > ValeurMax )
                                    {
                                        sprintf(ligne,"do i = %d,%d", parcours->var->v_indicetabvars,
                                                                      parcours->var->v_indicetabvars+compteur);
                                        tofich(allocationagrif,ligne,1);
                                        IndiceMin = parcours->var->v_indicetabvars;
                                        IndiceMax = parcours->var->v_indicetabvars+compteur;
                                        sprintf(ligne,"    if (.not. allocated(%s)) then", vargridnametabvars(v,1));
                                        tofich(allocationagrif,ligne,1);
                                        sprintf(ligne,"    allocate(%s", vargridnametabvars(v,1));
                                        sprintf(ligne2,"%s)", vargridparam(v));
                                        strcat(ligne,ligne2);
                                        tofich(allocationagrif,ligne,1);
                                        if (!strcasecmp(parcours->var->v_typevar,"real") || !strcasecmp(parcours->var->v_typevar,"integer"))
                                        {
                                        sprintf(ligne,"    %s = 0", vargridnametabvars(v,1));
                                        tofich(allocationagrif,ligne,1);
                                        }
                                        tofich(allocationagrif,"endif",1);
                                        tofich(allocationagrif,"enddo",1);
                                        i = parcours->var->v_indicetabvars;
                                        do
                                        {
                                            parcoursindic =  (listindice *)calloc(1,sizeof(listindice));
                                            parcoursindic -> i_indice = i;
                                            parcoursindic -> suiv = list_indic[parcours->var->v_catvar];
                                            list_indic[parcours->var->v_catvar] = parcoursindic;
                                            i = i + 1;
                                        } while ( i <= parcours->var->v_indicetabvars+compteur );
                                        parcours = parcoursprec;
                                    }
                                    else
                                    {
                                        sprintf(ligne,"    if (.not. allocated(%s)) then", vargridnametabvars(v,0));
                                        tofich(allocationagrif,ligne,1);
                                        sprintf(ligne,"    allocate(%s", vargridnametabvars(v,0));
                                        sprintf(ligne2,"%s)", vargridparam(v));
                                        strcat(ligne,ligne2);
                                        tofich(allocationagrif,ligne,1);
                                        if (!strcasecmp(parcours->var->v_typevar,"real") || !strcasecmp(parcours->var->v_typevar,"integer"))
                                        {
                                        sprintf(ligne,"    %s = 0", vargridnametabvars(v,0));
                                        tofich(allocationagrif,ligne,1);
                                        }
                                        tofich(allocationagrif,"endif",1);

                                        parcoursindic =  (listindice *) calloc(1,sizeof(listindice));
                                        parcoursindic -> i_indice = parcours->var->v_indicetabvars;
                                        parcoursindic -> suiv = list_indic[parcours->var->v_catvar];
                                        list_indic[parcours->var->v_catvar] = parcoursindic;
                                    }

                                    global_check = 0;
                                    is_parameter_local = writedeclarationintoamr(List_Parameter_Var,
                                                        paramtoamr,v,parcours_nom->o_nom,&neededparameter,v->v_commonname,global_check);
                                    if (is_parameter_local == 0)
                                    {
                                    global_check = 1;
                                    is_parameter_local = writedeclarationintoamr(List_GlobalParameter_Var,
                                                        paramtoamr,v,parcours_nom->o_nom,&neededparameter,v->v_commonname,global_check);
                                    }
                                }
                            } /* end of the allocation part                                       */
                            /*                INITIALISATION                                      */
                            if ( v->v_initialvalue )
                            {
                            parcours_name = v->v_initialvalue;
                            parcours_name_array = v->v_initialvalue_array;
                            if (parcours_name_array)
                            {
                            while (parcours_name)
                            {
                                strcpy(ligne, vargridnametabvars(v,0));
                                if (parcours_name_array)
                                {
                                if (strcasecmp(parcours_name_array->n_name,"") )
                                {
                                sprintf(ligne2,"(%s)",parcours_name_array->n_name);
                                strcat(ligne,ligne2);
                                }
                                }
                                /* We should modify the initialvalue in the case of variable has been defined with others variables */
                                strcpy(initialvalue,ChangeTheInitalvaluebyTabvarsName(parcours_name->n_name,List_Global_Var));
                                if ( !strcasecmp(initialvalue,parcours_name->n_name) )
                                {
                                    strcpy(initialvalue,ChangeTheInitalvaluebyTabvarsName(parcours_name->n_name,List_Common_Var));
                                }
                                if ( !strcasecmp(initialvalue,parcours_name->n_name) )
                                {
                                    strcpy(initialvalue,ChangeTheInitalvaluebyTabvarsName(parcours_name->n_name,List_ModuleUsed_Var));
                                }
                                strcat (ligne," = ");

                                if (v->v_nbdim >= 0)
                                {
                                    strcpy(ligne2,initialvalue);
                                }
                                else
                                {
                                    sprintf(ligne2,"reshape(%s,shape(%s))",initialvalue,vargridnametabvars(v,0));
                                }
                                strcat(ligne,ligne2);
                                tofich(allocationagrif,ligne,1);
                             
                             parcours_name = parcours_name->suiv;
                             if (parcours_name_array) parcours_name_array = parcours_name_array->suiv;
                            }
                            }
                            else
                            {
                            strcpy(ligne, vargridnametabvars(v,0));
                            strcat (ligne," = ");
                            strcpy(ligne2,"");
                            nb_initial = 0;
                            
                            while (parcours_name)
                            {
                            nb_initial = nb_initial + 1;
                                /* We should modify the initialvalue in the case of variable has been defined with others variables */
                                strcpy(initialvalue,ChangeTheInitalvaluebyTabvarsName(parcours_name->n_name,List_Global_Var));
                                if ( !strcasecmp(initialvalue,parcours_name->n_name) )
                                {
                                    strcpy(initialvalue,ChangeTheInitalvaluebyTabvarsName(parcours_name->n_name,List_Common_Var));
                                }
                                if ( !strcasecmp(initialvalue,parcours_name->n_name) )
                                {
                                    strcpy(initialvalue,ChangeTheInitalvaluebyTabvarsName(parcours_name->n_name,List_ModuleUsed_Var));
                                }

                                strcat(ligne2,initialvalue);
                             if (parcours_name->suiv)
                             {
                             strcat(ligne2,",");
                             }
                             
                             parcours_name = parcours_name->suiv;
                            }
                            if (nb_initial > 1)
                            {
                            sprintf(ligne3,"reshape((/%s/),shape(%s))",ligne2,vargridnametabvars(v,0));
                            }
                            else
                            {
                            strcpy(ligne3,ligne2);
                            }
                            strcat(ligne,ligne3);
                            tofich(allocationagrif,ligne,1);
                            }
                            }
                        }
                        if ( (onlyfixedgrids != 1) && (v->v_nbdim != 0) )
                        {
                            tofich(allocationagrif,"endif",1);
                        }
                    }
                }
                parcours = parcours -> suiv;
            }
            /* Close the file Alloc_agrif                                        */
            fclose(allocationagrif);
            fclose(paramtoamr);
        }
        parcours_nom = parcours_nom -> suiv;
    }
}

void write_allocation_Global_0()
{
    listnom *parcours_nom;
    listvar *parcours;
    listvar *parcoursprec;
    listvar *parcours1;
    FILE *allocationagrif;
    char ligne[LONG_M];
    char ligne2[LONG_M];
    char ligne3[LONG_M];
    listname *parcours_name;
    listname *parcours_name_array;
    variable *v;
    int IndiceMax;
    int IndiceMin;
    int compteur;
    int out;
    int indiceprec;
    int ValeurMax;
    char initialvalue[LONG_M];
    int typeiswritten ;
    int nb_initial;

    parcoursprec = (listvar *) NULL;
    parcours_nom = List_NameOfModule;
    ValeurMax = 2;

    while ( parcours_nom  )
    {
        if ( parcours_nom->o_val == 1 )
        {
            IndiceMax = 0;
            IndiceMin = indicemaxtabvars[0];
            /* Open the file to create the Alloc_agrif subroutine                */
            sprintf(ligne,"alloc_agrif_%s.h",parcours_nom->o_nom);
            allocationagrif = open_for_write(ligne);

//             if ( ModuleIsDefineInInputFile(parcours_nom->o_nom) == 1 )
//             {
//                 /* add the call to initworkspace         */
//                 tofich(allocationagrif,"if (.not. Agrif_Root() ) then",1);
//                 tofich(allocationagrif,"#include \"GetNumberofcells.h\"\n",0);
//                 tofich(allocationagrif,"else",1);
//                 tofich(allocationagrif,"#include \"SetNumberofcells.h\"\n",0);
//                 tofich(allocationagrif,"endif",1);
//                 tofich(allocationagrif,"call Agrif_InitWorkspace",1);
//             }

            typeiswritten = 0;
            parcours = List_Global_Var;
            while ( parcours )
            {
                if ( !strcasecmp(parcours->var->v_modulename,parcours_nom->o_nom) &&
                       parcours->var->v_VariableIsParameter == 0                  &&
                       parcours->var->v_notgrid == 0  )
                {
                    v = parcours->var;
                    IndiceMax = 0;
                    IndiceMin = indicemaxtabvars[v->v_catvar];
                    /* body of the file */
                    if ( !strcasecmp(v->v_commoninfile,cur_filename) )
                    {
                        if ( (onlyfixedgrids != 1) && (v->v_nbdim != 0) )
                        {
                            sprintf(ligne,"if (.not. allocated(%s)) then", vargridnametabvars(v,0));
                            tofich(allocationagrif,ligne,1);
                        }
                        if ( (v->v_allocatable != 1) && (v->v_dimsempty != 1) )
                        {
                            /*                ALLOCATION                                          */
                            if ( v->v_dimension != 0 )
                            {
                                if ( v->v_indicetabvars < IndiceMin || v->v_indicetabvars > IndiceMax )
                                {
                                    parcours1 = parcours;
                                    compteur = -1;
                                    out = 0;
                                    indiceprec = parcours->var->v_indicetabvars -1 ;
                                    while ( parcours1 && out == 0
                                        && !strcasecmp(parcours->var->v_readedlistdimension,parcours1->var->v_readedlistdimension)
                                        && !strcasecmp(parcours->var->v_typevar,            parcours1->var->v_typevar)
                                        && (parcours1->var->v_indicetabvars == indiceprec+1) )
                                    {
                                        if ( !strcasecmp(parcours1->var->v_modulename, parcours_nom->o_nom) ||
                                             !strcasecmp(parcours1->var->v_commonname, parcours_nom->o_nom) )
                                        {
                                            compteur = compteur +1 ;
                                            indiceprec = parcours1->var->v_indicetabvars;
                                            parcoursprec = parcours1;
                                            parcours1 = parcours1->suiv;
                                        }
                                        else out = 1;
                                    }
                                    sprintf(ligne,"!! ALLOCATION OF VARIABLE : %s",v->v_nomvar);
                                    tofich(allocationagrif,ligne,1);
                                    if ( compteur > ValeurMax )
                                    {
                                        sprintf(ligne,"do i = %d,%d", parcours->var->v_indicetabvars,
                                                                      parcours->var->v_indicetabvars+compteur);
                                        tofich(allocationagrif,ligne,1);
                                        IndiceMin = parcours->var->v_indicetabvars;
                                        IndiceMax = parcours->var->v_indicetabvars+compteur;
                                        sprintf(ligne,"    if (.not. allocated(%s)) then", vargridnametabvars(v,1));
                                        tofich(allocationagrif,ligne,1);
                                        sprintf(ligne,"    allocate(%s", vargridnametabvars(v,1));
                                        sprintf(ligne2,"%s)", vargridparam(v));
                                        strcat(ligne,ligne2);
                                        tofich(allocationagrif,ligne,1);
                                        if (!strcasecmp(parcours->var->v_typevar,"real") || !strcasecmp(parcours->var->v_typevar,"integer"))
                                        {
                                        sprintf(ligne,"    %s = 0", vargridnametabvars(v,1));
                                        tofich(allocationagrif,ligne,1);
                                        }
                                        tofich(allocationagrif,"endif",1);
                                        tofich(allocationagrif,"enddo",1);
                                        parcours = parcoursprec;
                                    }
                                    else
                                    {
                                        sprintf(ligne,"    if (.not. allocated(%s)) then", vargridnametabvars(v,0));
                                        tofich(allocationagrif,ligne,1);
                                        sprintf(ligne,"    allocate(%s", vargridnametabvars(v,0));
                                        sprintf(ligne2,"%s)", vargridparam(v));
                                        strcat(ligne,ligne2);
                                        tofich(allocationagrif,ligne,1);
                                        if (!strcasecmp(parcours->var->v_typevar,"real") || !strcasecmp(parcours->var->v_typevar,"integer"))
                                        {
                                        sprintf(ligne,"    %s = 0", vargridnametabvars(v,0));
                                        tofich(allocationagrif,ligne,1);
                                        }
                                        tofich(allocationagrif,"endif",1);
                                    }
                                }
                            } /* end of the allocation part                                       */
                            /*                INITIALISATION                                      */

               if ( v->v_initialvalue )
                            {
                            parcours_name = v->v_initialvalue;
                            parcours_name_array = v->v_initialvalue_array;
                            if (parcours_name_array)
                            {
                            while (parcours_name)
                            {
                                strcpy(ligne, vargridnametabvars(v,0));
                                if (parcours_name_array)
                                {
                                if (strcasecmp(parcours_name_array->n_name,"") )
                                {
                                sprintf(ligne2,"(%s)",parcours_name_array->n_name);
                                strcat(ligne,ligne2);
                                }
                                }
                                /* We should modify the initialvalue in the case of variable has been defined with others variables */
                                strcpy(initialvalue,ChangeTheInitalvaluebyTabvarsName(parcours_name->n_name,List_Global_Var));
                                if ( !strcasecmp(initialvalue,parcours_name->n_name) )
                                {
                                    strcpy(initialvalue,ChangeTheInitalvaluebyTabvarsName(parcours_name->n_name,List_Common_Var));
                                }
                                if ( !strcasecmp(initialvalue,parcours_name->n_name) )
                                {
                                    strcpy(initialvalue,ChangeTheInitalvaluebyTabvarsName(parcours_name->n_name,List_ModuleUsed_Var));
                                }
                                strcat (ligne," = ");

                                if (v->v_nbdim >= 0)
                                {
                                    strcpy(ligne2,initialvalue);
                                }
                                else
                                {
                                    sprintf(ligne2,"reshape(%s,shape(%s))",initialvalue,vargridnametabvars(v,0));
                                }
                                strcat(ligne,ligne2);
                                tofich(allocationagrif,ligne,1);
                             
                             parcours_name = parcours_name->suiv;
                             if (parcours_name_array) parcours_name_array = parcours_name_array->suiv;
                            }
                            }
                            else
                            {
                            strcpy(ligne, vargridnametabvars(v,0));
                            strcat (ligne," = ");
                            strcpy(ligne2,"");
                            nb_initial = 0;
                            
                            while (parcours_name)
                            {
                            nb_initial = nb_initial + 1;
                                /* We should modify the initialvalue in the case of variable has been defined with others variables */
                                strcpy(initialvalue,ChangeTheInitalvaluebyTabvarsName(parcours_name->n_name,List_Global_Var));
                                if ( !strcasecmp(initialvalue,parcours_name->n_name) )
                                {
                                    strcpy(initialvalue,ChangeTheInitalvaluebyTabvarsName(parcours_name->n_name,List_Common_Var));
                                }
                                if ( !strcasecmp(initialvalue,parcours_name->n_name) )
                                {
                                    strcpy(initialvalue,ChangeTheInitalvaluebyTabvarsName(parcours_name->n_name,List_ModuleUsed_Var));
                                }

                                strcat(ligne2,initialvalue);
                             if (parcours_name->suiv)
                             {
                             strcat(ligne2,",");
                             }
                             
                             parcours_name = parcours_name->suiv;
                            }
                            if (nb_initial > 1)
                            {
                            sprintf(ligne3,"reshape((/%s/),shape(%s))",ligne2,vargridnametabvars(v,0));
                            }
                            else
                            {
                            strcpy(ligne3,ligne2);
                            }
                            strcat(ligne,ligne3);
                            tofich(allocationagrif,ligne,1);
                            }
                            }
                        }
                        /* Case of structure types */
                        if ( (typeiswritten == 0) && !strcasecmp(v->v_typevar,"type") )
                        {
                            sprintf(ligne,"if (.not. allocated(Agrif_%s_var)) then",v->v_modulename);
                            tofich(allocationagrif, ligne, 1);
                            sprintf(ligne,"    allocate(Agrif_%s_var(0:Agrif_NbMaxGrids))",v->v_modulename);
                            tofich(allocationagrif, ligne, 1);
                            tofich(allocationagrif, "endif", 1);
                            typeiswritten = 1;
                        }
                        if ( (onlyfixedgrids != 1) && (v->v_nbdim != 0) )
                        {
                            tofich(allocationagrif,"endif",1);
                        }
                    }
                }
                parcours = parcours -> suiv;
            }
            if ( ModuleIsDefineInInputFile(parcours_nom->o_nom) == 1 )
            {
                fprintf(allocationagrif, "      if ( .not.Agrif_Root() ) then\n");
                fprintf(allocationagrif, "#include \"GetNumberofcells.h\"\n");
                fprintf(allocationagrif, "      else\n");
                fprintf(allocationagrif, "#include \"SetNumberofcells.h\"\n");
                fprintf(allocationagrif, "      endif\n");
                fprintf(allocationagrif, "      call Agrif_InitWorkspace\n");
            }
            fclose(allocationagrif);
        }
        parcours_nom = parcours_nom -> suiv;
    }
}

/******************************************************************************/
/*                           creefichieramr                                   */
/******************************************************************************/
/* This subroutine is the main one to create AGRIF_INC files                  */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void creefichieramr ()
{
    listvar *newvar;
    variable *v;
    int erreur;
    char filefich[LONG_M];

    int InitEmpty;
    int VarnameEmpty;
    int donotwrite;

    FILE *initproc;
    FILE *initglobal;
    FILE *createvarname;
    FILE *createvarnameglobal;

    if ( todebug == 1 ) printf("Enter in creefichieramr\n");

    sprintf(filefich, "cd %s", include_dir);
    erreur = system (filefich);
    if (erreur)
    {
        sprintf(filefich, "mkdir -p %s", include_dir);
        system(filefich);
        printf("%s: Directory created\n", include_dir);
    }

/******************************************************************************/
/******************** Creation of AGRIF_INC files *****************************/
/******************************************************************************/

    if ( todebug == 1 )
    {
        const char *NameTampon = "toto";
        sprintf(filefich,"initialisations_agrif_%s.h", NameTampon);
        initproc = open_for_write(filefich);

        sprintf(filefich,"createvarname_agrif_%s.h", NameTampon);
        createvarname = open_for_write(filefich);

        InitEmpty = 1 ;
        VarnameEmpty = 1 ;

        newvar = List_Global_Var;
        while ( newvar )
        {
            donotwrite = 0;
            v = newvar->var;

            if ( ( v->v_common == 1 || v->v_module == 1 ) && donotwrite == 0 )
            {
                write_createvarnameagrif_file(v,createvarname,&VarnameEmpty);
                write_initialisationsagrif_file(v,initproc,&InitEmpty);
            }
            newvar = newvar->suiv;
        }
        fclose (createvarname);
        fclose (initproc);

        if ( is_dependfile_created(curmodulename) == 0 )
        {
            if ( InitEmpty != 1  )
            {
                initglobal = open_for_append("initialisations_agrif.h");
                fprintf(initglobal,"#include \"initialisations_agrif_%s.h\"\n", NameTampon);
                fclose(initglobal);
            }
            if ( VarnameEmpty != 1 )
            {
                createvarnameglobal= open_for_append("createvarname_agrif.h");
                fprintf(createvarnameglobal,"#include \"createvarname_agrif_%s.h\"\n", NameTampon);
                fclose(createvarnameglobal);
            }
        }
    }
    write_allocation_Common_0();
    write_allocation_Global_0();

    Write_Alloc_Agrif_Files();
    write_probdimagrif_file();
    write_keysagrif_file();
    write_modtypeagrif_file();

    if ( NbMailleXDefined == 1 )
    {
        write_Setnumberofcells_file();
        write_Getnumberofcells_file();
    }

    if ( todebug == 1 ) printf("Out of creefichieramr\n");
}
