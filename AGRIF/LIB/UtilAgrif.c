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
/*                      Vartonumber                                           */
/******************************************************************************/
/* This subroutine is used to know if Agrif_ is locate in the char            */
/* tokname                                                                    */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
int Vartonumber(const char *tokname)
{
   int agrifintheword;

   agrifintheword = 0;
        if ( !strcasecmp(tokname,"Agrif_parent")         ) agrifintheword = 1;
   else if ( !strcasecmp(tokname,"Agrif_set_type")       ) agrifintheword = 1;
   else if ( !strcasecmp(tokname,"Agrif_set_raf")        ) agrifintheword = 1;
   else if ( !strcasecmp(tokname,"Agrif_set_bc")         ) agrifintheword = 1;
   else if ( !strcasecmp(tokname,"Agrif_set_bcinterp")   ) agrifintheword = 1;
   else if ( !strcasecmp(tokname,"Agrif_Root")           ) agrifintheword = 1;
   else if ( !strcasecmp(tokname,"Agrif_CFixed")         ) agrifintheword = 1;
   else if ( !strcasecmp(tokname,"Agrif_Fixed")          ) agrifintheword = 1;
   else if ( !strcasecmp(tokname,"Agrif_bc_variable")    ) agrifintheword = 1;
   else if ( !strcasecmp(tokname,"Agrif_set_parent")     ) agrifintheword = 1;
   else if ( !strcasecmp(tokname,"Agrif_interp_variable")) agrifintheword = 1;
   else if ( !strcasecmp(tokname,"Agrif_init_variable")  ) agrifintheword = 1;
   else if ( !strcasecmp(tokname,"Agrif_update_variable")) agrifintheword = 1;
   else if ( !strcasecmp(tokname,"Agrif_Set_interp")     ) agrifintheword = 1;
   else if ( !strcasecmp(tokname,"Agrif_Set_Update")     ) agrifintheword = 1;
   else if ( !strcasecmp(tokname,"Agrif_Set_UpdateType") ) agrifintheword = 1;
   else if ( !strcasecmp(tokname,"Agrif_Set_restore")    ) agrifintheword = 1;
   else if ( !strcasecmp(tokname,"Agrif_Save_Forrestore")) agrifintheword = 1;
   else if ( !strcasecmp(tokname,"Agrif_init_grids")     ) agrifintheword = 1;
   else if ( !strcasecmp(tokname,"Agrif_step")           ) agrifintheword = 1;
/**************************************************/
/* adding specific adjoint agrif subroutine names */
/**************************************************/
   else if ( !strcasecmp(tokname,"Agrif_bc_variable_adj")    ) agrifintheword = 1;
   else if ( !strcasecmp(tokname,"Agrif_update_variable_adj")) agrifintheword = 1;

   return agrifintheword;
}

/******************************************************************************/
/*                              Agrif_in_Tok_NAME                             */
/******************************************************************************/
/* This subroutine is used to know if Agrif_ is locate in the char            */
/* tokname                                                                    */
/******************************************************************************/
/*                                                                            */
/*                 Agrif_name --------------> Agrif_in_Tok_NAME = 1           */
/*                       name --------------> Agrif_in_Tok_NAME = 0           */
/*                                                                            */
/******************************************************************************/
int Agrif_in_Tok_NAME(const char *tokname)
{
    return ( strncasecmp(tokname,"Agrif_",6) == 0 );
}

/******************************************************************************/
/*                     ModifyTheVariableName_0                                */
/******************************************************************************/
/* Firstpass 0                                                                */
/******************************************************************************/
/*                                                                            */
/*               Agrif_<toto>(variable) ====>     Agrif_<toto>(variable)      */
/*                                                                            */
/******************************************************************************/
void ModifyTheVariableName_0(const char *ident, int lengthname)
{
    listvar *newvar;
    int out;

    if ( firstpass )  return;

    newvar = List_Global_Var;
    out = 0;
    while ( newvar && out == 0 )
    {
        if ( !strcasecmp(newvar->var->v_nomvar, ident) ) out = 1;
        else newvar = newvar->suiv;
    }
    if ( out == 0 )
    {
        newvar = List_ModuleUsed_Var;
        while ( newvar && out == 0 )
        {
            if ( !strcasecmp(newvar->var->v_nomvar,ident) ) out = 1;
            else newvar = newvar->suiv;
        }
    }
    if ( out && !strcasecmp(newvar->var->v_typevar,"type")) return;

    if ( out == 0 )
    {
        newvar = List_Common_Var;
        while ( newvar && out == 0 )
        {
            if ( !strcasecmp(newvar->var->v_nomvar,ident) ) out = 1;
            else newvar = newvar->suiv;
        }
    }
    if ( out == 0 )
    {
        newvar = List_ModuleUsedInModuleUsed_Var;
        while ( newvar && out == 0 )
        {
            if ( !strcasecmp(newvar->var->v_nomvar,ident) ) out = 1;
            else newvar = newvar->suiv;
        }
    }
    if ( out == 1 && strcasecmp(newvar->var->v_typevar,"type"))
    {
        // remove the variable
 //       RemoveWordCUR_0(fortran_out,lengthname);
        // then write the new name
 //        if ( inagrifcallargument == 1 && agrif_parentcall == 0 )
//             fprintf(fortran_out,"%d",newvar->var->v_indicetabvars);
//         else
//         {
//             if ( retour77 == 0 )
//                 fprintf(fortran_out,"Agrif_%s & \n      ", tabvarsname(newvar->var));
//             else
//             {
//                fprintf(fortran_out,"Agrif_%s", tabvarsname(newvar->var));
//                fprintf(fortran_out," \n     & ");
//             }
//             fprintf(fortran_out,"%s",vargridcurgridtabvarswithoutAgrif_Gr(newvar->var));
//         }
    }
    else
    {
        // we should look in the List_ModuleUsed_Var
        if ( inagrifcallargument != 1 )
        {
            newvar = List_ModuleUsed_Var;
            while ( newvar && out == 0 )
            {
                if ( !strcasecmp(newvar->var->v_nomvar,ident) ) out = 1;
                else newvar = newvar->suiv;
            }
            if ( out == 1 && strcasecmp(newvar->var->v_typevar, "type"))
            {
                // remove the variable
                RemoveWordCUR_0(fortran_out,lengthname);
                // then write the new name
                if ( retour77 == 0 )
                    fprintf(fortran_out,"Agrif_%s & \n      ",tabvarsname(newvar->var));
                else
                {
                    fprintf(fortran_out," \n     &Agrif_%s",tabvarsname(newvar->var));
                }
                fprintf(fortran_out,"%s",vargridcurgridtabvarswithoutAgrif_Gr(newvar->var));
            }
        }
    }
}

/******************************************************************************/
/*                         Add_SubroutineWhereAgrifUsed_1                     */
/******************************************************************************/
/* This subroutine is used to add a record to                                 */
/* List_SubroutineWhereAgrifUsed                                              */
/******************************************************************************/
/*                                                                            */
/*       subroutine sub ... Agrif_<something>                                 */
/*                                                                            */
/*        _______     _______     _______     _______     _______             */
/*       +      +    +      +    +      +    +      +    +      +             */
/*       + list +--->+ list +--->+ list +--->+ list +--->+ sub  +             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                                                            */
/*       list = List_SubroutineWhereAgrifUsed                                 */
/*                                                                            */
/******************************************************************************/
void Add_SubroutineWhereAgrifUsed_1(const char *sub, const char *mod)
{
    listnom *listnomtmp;
    listnom *parcours;

    if ( firstpass == 1 )
    {
        if ( !List_SubroutineWhereAgrifUsed )
        {
            listnomtmp = (listnom*) calloc(1, sizeof(listnom));
            strcpy(listnomtmp->o_nom, sub);
            strcpy(listnomtmp->o_module, mod);
            listnomtmp->suiv = NULL;
            List_SubroutineWhereAgrifUsed = listnomtmp;
        }
        else
        {
            parcours = List_SubroutineWhereAgrifUsed;
            while ( parcours && strcasecmp(parcours->o_nom,sub) )
            {
                parcours = parcours->suiv;
            }
            if ( !parcours )
            {
                listnomtmp = (listnom*) calloc(1, sizeof(listnom));
                strcpy(listnomtmp->o_nom, sub);
                strcpy(listnomtmp->o_module, mod);
                listnomtmp->suiv = List_SubroutineWhereAgrifUsed;
                List_SubroutineWhereAgrifUsed = listnomtmp;
            }
        }
    }
}

/******************************************************************************/
/*                                AddUseAgrifUtil_0                           */
/******************************************************************************/
/* Add use Agrif_Util at the beginning of the subroutine definition           */
/* if it is necessary                                                         */
/******************************************************************************/
/*                                                                            */
/*       subroutine sub            |  subroutine sub                          */
/*                                 |  USE Agrif_Util                          */
/*       implicit none             |  implicit none                           */
/*       ...                       |  ...                                     */
/*       ... Agrif_<something>     |  ... Agrif_<something>                   */
/*       ...                       |  ...                                     */
/*       end                       |  end                                     */
/*                                                                            */
/*                                                                            */
/******************************************************************************/
void  AddUseAgrifUtil_0(FILE *fileout)
{
  listnom *parcours;

  if ( firstpass == 0 )
  {
     parcours = List_SubroutineWhereAgrifUsed;
     while ( parcours && strcasecmp(parcours->o_nom,subroutinename) )
     {
        parcours = parcours -> suiv;
     }
     if ( parcours && parcours->o_val != 0 )
        fprintf(fileout,"\n      use Agrif_Util\n");
     else
        fprintf(fileout,"\n      use Agrif_Types, only : Agrif_tabvars\n");
  }
}

void  AddUseAgrifUtilBeforeCall_0(FILE *fileout)
{
    listusemodule *parcours;

    int out;

    if ( firstpass == 0 )
    {
        parcours = List_NameOfModuleUsed;
        out = 0 ;
        while ( parcours && out == 0 )
        {
            if ( !strcasecmp(parcours->u_usemodule, "Agrif_Util")   &&
                 !strcasecmp(parcours->u_modulename, curmodulename) &&
                 !strcasecmp(parcours->u_cursubroutine, subroutinename) )
                out = 1;
            else
                parcours = parcours->suiv;
        }
        if ( out == 0 )
        {
            fprintf(fileout,"\n      use Agrif_Util\n");
        }
    }
}

/******************************************************************************/
/*                         NotifyAgrifFunction_0                              */
/******************************************************************************/
/* Firstpass 0                                                                */
/******************************************************************************/
/*                                                                            */
/*               Agrif_<toto>(variable) ====>     Agrif_<toto>(variable)      */
/*                                                                            */
/******************************************************************************/
void NotifyAgrifFunction_0(const char *ident)
{
    if ( firstpass == 1 )   return;

    if ( !strcasecmp(ident,"Agrif_parent") )
    {
        InAgrifParentDef = 1;
        pos_curagrifparent = setposcur()-12;
    }
    else if ( !strcasecmp(ident,"Agrif_Get_Coarse_grid") )
    {
        InAgrifParentDef = 2;
        pos_curagrifparent = setposcur()-21;
    }
    else if ( !strcasecmp(ident,"Agrif_Rhox") )
    {
        InAgrifParentDef = 3;
        pos_curagrifparent = setposcur()-10;
    }
    else if ( !strcasecmp(ident,"Agrif_Parent_Rhox") )
    {
        InAgrifParentDef = 4;
        pos_curagrifparent = setposcur()-17;
    }
    else if ( !strcasecmp(ident,"Agrif_IRhox") )
    {
        InAgrifParentDef = 5;
        pos_curagrifparent = setposcur()-11;
    }
    else if ( !strcasecmp(ident,"Agrif_Parent_IRhox") )
    {
        InAgrifParentDef = 6;
        pos_curagrifparent = setposcur()-18;
    }
    else if ( !strcasecmp(ident,"Agrif_Rhoy") )
    {
        InAgrifParentDef = 7;
        pos_curagrifparent = setposcur()-10;
    }
    else if ( !strcasecmp(ident,"Agrif_Parent_Rhoy") )
    {
        InAgrifParentDef = 8;
        pos_curagrifparent = setposcur()-17;
    }
    else if ( !strcasecmp(ident,"Agrif_IRhoy") )
    {
        InAgrifParentDef = 9;
        pos_curagrifparent = setposcur()-11;
    }
    else if ( !strcasecmp(ident,"Agrif_Parent_IRhoy") )
    {
        InAgrifParentDef = 10;
        pos_curagrifparent = setposcur()-18;
    }
    else if ( !strcasecmp(ident,"Agrif_Rhoz") )
    {
        InAgrifParentDef = 11;
        pos_curagrifparent = setposcur()-10;
    }
    else if ( !strcasecmp(ident,"Agrif_Parent_Rhoz") )
    {
        InAgrifParentDef = 12;
        pos_curagrifparent = setposcur()-17;
    }
    else if ( !strcasecmp(ident,"Agrif_IRhoz") )
    {
        InAgrifParentDef = 13;
        pos_curagrifparent = setposcur()-11;
    }
    else if ( !strcasecmp(ident,"Agrif_Parent_IRhoz") )
    {
        InAgrifParentDef = 14;
        pos_curagrifparent = setposcur()-18;
    }
    else if ( !strcasecmp(ident,"Agrif_NearCommonBorderX") )
    {
        InAgrifParentDef = 15;
        pos_curagrifparent = setposcur()-23;
    }
    else if ( !strcasecmp(ident,"Agrif_NearCommonBorderY") )
    {
        InAgrifParentDef = 16;
        pos_curagrifparent = setposcur()-23;
    }
    else if ( !strcasecmp(ident,"Agrif_NearCommonBorderZ") )
    {
        InAgrifParentDef = 17;
        pos_curagrifparent = setposcur()-23;
    }
    else if ( !strcasecmp(ident,"Agrif_DistantCommonBorderX") )
    {
        InAgrifParentDef = 18;
        pos_curagrifparent = setposcur()-26;
    }
    else if ( !strcasecmp(ident,"Agrif_DistantCommonBorderY") )
    {
        InAgrifParentDef = 19;
        pos_curagrifparent = setposcur()-26;
    }
    else if ( !strcasecmp(ident,"Agrif_DistantCommonBorderZ") )
    {
        InAgrifParentDef = 20;
        pos_curagrifparent = setposcur()-26;
    }
    else if ( !strcasecmp(ident,"Agrif_Get_parent_id") )
    {
        InAgrifParentDef = 21;
        pos_curagrifparent = setposcur()-19;
    }
    else if ( !strcasecmp(ident,"Agrif_Get_grid_id") )
    {
        InAgrifParentDef = 22;
        pos_curagrifparent = setposcur()-17;
    }
    else if ( !strcasecmp(ident,"Agrif_Parent_Iz") )
    {
        InAgrifParentDef = 23;
        pos_curagrifparent = setposcur()-15;
    }
    else if ( !strcasecmp(ident,"Agrif_Parent_Iy") )
    {
        InAgrifParentDef = 24;
        pos_curagrifparent = setposcur()-15;
    }
    else if ( !strcasecmp(ident,"Agrif_Parent_Ix") )
    {
        InAgrifParentDef = 25;
        pos_curagrifparent = setposcur()-15;
    }
    else if ( !strcasecmp(ident,"Agrif_Iz") )
    {
        InAgrifParentDef = 26;
        pos_curagrifparent = setposcur()-8;
    }
    else if ( !strcasecmp(ident,"Agrif_Iy") )
    {
        InAgrifParentDef = 27;
        pos_curagrifparent = setposcur()-8;
    }
    else if ( !strcasecmp(ident,"Agrif_Ix") )
    {
        InAgrifParentDef = 28;
        pos_curagrifparent = setposcur()-8;
    }
    else if ( !strcasecmp(ident,"Agrif_Nb_Fixed_Grids") )
    {
        InAgrifParentDef = 29;
        pos_curagrifparent = setposcur()-20;
    }
    else if ( !strcasecmp(ident,"Agrif_Nb_Fine_Grids") )
    {
        InAgrifParentDef = 29;
        pos_curagrifparent = setposcur()-19;
    }
    else if ( !strcasecmp(ident,"AGRIF_Nb_Step") )
    {
        InAgrifParentDef = 30;
        pos_curagrifparent = setposcur()-13;
    }
}

/******************************************************************************/
/*                       ModifyTheAgrifFunction_0                             */
/******************************************************************************/
/* Firstpass 0                                                                */
/******************************************************************************/
/*                                                                            */
/*               Agrif_<toto>(variable) ====>     Agrif_<toto>(variable)      */
/*                                                                            */
/******************************************************************************/
void ModifyTheAgrifFunction_0(const char *ident)
{
   if ( InAgrifParentDef != 0 )
          AgriffunctionModify_0(ident,InAgrifParentDef);
   InAgrifParentDef = 0;
}


/******************************************************************************/
/*                         AgriffunctionModify_0                              */
/******************************************************************************/
/* Firstpass 0                                                                */
/******************************************************************************/
/* if whichone = 1 Agrif_parent ===>                                          */
/*                                                                            */
/* if whichone = 2 Agrif_Get_coarse_grid ===>                                 */
/*                                                                            */
/* if whichone = 3 Agrif_Rhox ===>                                            */
/*                                                                            */
/* if whichone = 4 Agrif_Parent_Rhox ===>                                     */
/*                                                                            */
/* if whichone = 5 Agrif_IRhox ===>                                           */
/*                                                                            */
/* if whichone = 6 Agrif_Parent_IRhox ===>                                    */
/*                                                                            */
/* if whichone = 7 Agrif_Rhoy ===>                                            */
/*                                                                            */
/* if whichone = 8 Agrif_Parent_Rhoy ===>                                     */
/*                                                                            */
/* if whichone = 9 Agrif_IRhoy ===>                                           */
/*                                                                            */
/* if whichone = 10 Agrif_Parent_IRhoy ===>                                   */
/*                                                                            */
/* if whichone = 11 Agrif_Rhoz ===>                                           */
/*                                                                            */
/* if whichone = 12 Agrif_Parent_Rhoz ===>                                    */
/*                                                                            */
/* if whichone = 13 Agrif_IRhoz ===>                                          */
/*                                                                            */
/* if whichone = 14 Agrif_Parent_IRhoz ===>                                   */
/*                                                                            */
/* if whichone = 15 Agrif_NearCommonBorderX ===>                              */
/*                                                                            */
/* if whichone = 16 Agrif_NearCommonBorderX ===>                              */
/*                                                                            */
/* if whichone = 17 Agrif_NearCommonBorderX ===>                              */
/*                                                                            */
/* if whichone = 18 Agrif_DistantCommonBorderX ===>                           */
/*                                                                            */
/* if whichone = 19 Agrif_DistantCommonBorderY ===>                           */
/*                                                                            */
/* if whichone = 20 Agrif_DistantCommonBorderZ ===>                           */
/*                                                                            */
/* if whichone = 21 Agrif_Get_parent_id ===>                                  */
/*                                                                            */
/* if whichone = 22 Agrif_Get_grid_id ===>                                    */
/*                                                                            */
/* if whichone = 23 Agrif_Parent_Iz ===>                                      */
/*                                                                            */
/* if whichone = 24 Agrif_Parent_Iy ===>                                      */
/*                                                                            */
/* if whichone = 25 Agrif_Parent_Ix ===>                                      */
/*                                                                            */
/* if whichone = 26 Agrif_Iz ===>                                             */
/*                                                                            */
/* if whichone = 27 Agrif_Iy ===>                                             */
/*                                                                            */
/* if whichone = 28 Agrif_Ix ===>                                             */
/*                                                                            */
/* if whichone = 29 Agrif_Nb_Fixed_Grids ===>                                 */
/*                                                                            */
/* if whichone = 29 Agrif_Nb_Fine_Grids ===>                                  */
/*                                                                            */
/* if whichone = 30 AGRIF_Nb_Step ===>                                        */
/*                                                                            */
/*                                                                            */
/******************************************************************************/
void AgriffunctionModify_0(const char *ident,int whichone)
{
    char toprint[LONG_M];
    if ( firstpass == 0 )
    {
        strcpy(toprint,"");
        pos_end = setposcur();
        fseek(fortran_out,pos_curagrifparent,SEEK_SET);
        if ( whichone == 1 || whichone == 2 )
        {
            FindAndChangeNameToTabvars(ident,toprint,List_Global_Var,1);
            if ( !strcasecmp(ident,toprint) )
            {
                /* la liste des use de cette subroutine                           */
                strcpy(toprint,"");
                FindAndChangeNameToTabvars(ident,toprint,List_Common_Var,whichone);
            }
            if ( !strcasecmp(ident,toprint) )
            {
                /* la liste des use de cette subroutine                           */
                strcpy(toprint,"");
                FindAndChangeNameToTabvars(ident,toprint,List_ModuleUsed_Var,whichone);
            }
        }
        else if ( whichone == 3 ) /* Agrif_Rhox                                 */
        {
            sprintf(toprint,"REAL(");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"Agrif_Curgrid % spaceref(1))");
        }
        else if ( whichone == 4 ) /* Agrif_Parent_Rhox                          */
        {
            sprintf(toprint,"REAL(");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"Agrif_Curgrid % parent % spaceref(1))");
        }
        else if ( whichone == 5 ) /* Agrif_Rhox                                 */
        {
            sprintf(toprint,"Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% spaceref(1)");
        }
        else if ( whichone == 6 ) /* Agrif_Parent_Rhox                          */
        {
            sprintf(toprint,"Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% parent % spaceref(1)");
        }
        else if ( whichone == 7 ) /* Agrif_Rhoy                                 */
        {
            sprintf(toprint,"REAL(Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% spaceref(2))");
        }
        else if ( whichone == 8 ) /* Agrif_Parent_Rhoy                          */
        {
            sprintf(toprint,"REAL(Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% parent % spaceref(2))");
        }
        else if ( whichone == 9 ) /* Agrif_Rhoy                                 */
        {
            sprintf(toprint,"Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% spaceref(2)");
        }
        else if ( whichone == 10 ) /* Agrif_Parent_Rhoy                         */
        {
            sprintf(toprint,"Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% parent % spaceref(2)");
        }
        else if ( whichone == 11 ) /* Agrif_Rhoz                                */
        {
            sprintf(toprint,"REAL(Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% spaceref(3))");
        }
        else if ( whichone == 12 ) /* Agrif_Parent_Rhoz                         */
        {
            sprintf(toprint,"REAL(Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% parent % spaceref(3))");
        }
        else if ( whichone == 13 ) /* Agrif_Rhoz                                */
        {
            sprintf(toprint,"Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% spaceref(3)");
        }
        else if ( whichone == 14 ) /* Agrif_Parent_Rhoz                         */
        {
            sprintf(toprint,"Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% parent % spaceref(3)");
        }
        else if ( whichone == 15 ) /* Agrif_NearCommonBorderX                   */
        {
            sprintf(toprint,"Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% NearRootBorder(1)");
        }
        else if ( whichone == 16 ) /* Agrif_NearCommonBorderY                   */
        {
            sprintf(toprint,"Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% NearRootBorder(2)");
        }
        else if ( whichone == 17 ) /* Agrif_NearCommonBorderZ                   */
        {
            sprintf(toprint,"Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% NearRootBorder(3)");
        }
        else if ( whichone == 18 ) /* Agrif_NearCommonBorderX                   */
        {
            sprintf(toprint,"Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
         strcat(toprint,"% DistantRootBorder(1)");
        }
        else if ( whichone == 19 ) /* Agrif_NearCommonBorderY                   */
        {
            sprintf(toprint,"Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% DistantRootBorder(2)");
        }
        else if ( whichone == 20 ) /* Agrif_NearCommonBorderZ                   */
        {
            sprintf(toprint,"Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% DistantRootBorder(3)");
        }
        else if ( whichone == 21 ) /* Agrif_Get_parent_id                       */
        {
            sprintf(toprint,"Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% parent % grid_id");
        }
        else if ( whichone == 22 ) /*  Agrif_Get_grid_id                        */
        {
            sprintf(toprint,"Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% grid_id");
        }
        else if ( whichone == 23 ) /*  Agrif_Parent_Iz                          */
        {
            sprintf(toprint,"Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% parent % ix(3)");
        }
        else if ( whichone == 24 ) /*  Agrif_Parent_Iy                          */
        {
            sprintf(toprint,"Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% parent % ix(2)");
        }
        else if ( whichone == 25 ) /*  Agrif_Parent_Ix                          */
        {
            sprintf(toprint,"Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% parent % ix(1)");
        }
        else if ( whichone == 26 ) /* Agrif_Iz                                  */
        {
            sprintf(toprint,"Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint," % ix(3)");
        }
        else if ( whichone == 27 ) /* Agrif_Iy                                  */
        {
            sprintf(toprint,"Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% ix(2)");
        }
        else if ( whichone == 28 ) /* Agrif_Ix                                  */
        {
            sprintf(toprint,"Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% ix(1)");
        }
        else if ( whichone == 29 ) /* Agrif_Nb_Fixed_Grids                      */
        {
            sprintf(toprint,"Agrif_nbfixedgrids");
        }
        else if ( whichone == 30 ) /* AGRIF_Nb_Step                             */
        {
            sprintf(toprint,"Agrif_Curgrid");
            if( retour77 == 0 ) strcat(toprint," & \n");
            else                strcat(toprint,"\n     & ");
            strcat(toprint,"% ngridstep");
        }

        Save_Length(toprint,43);

        if ( whichone == 1 || whichone == 2 )   tofich(fortran_out,toprint,0);
        else                                    fprintf(fortran_out,"%s",toprint);
    }
}

/******************************************************************************/
/*                             Instanciation_0                                */
/******************************************************************************/
/* Firstpass 0                                                                */
/******************************************************************************/
/*                                                                            */
/*               Agrif_<toto>(variable) ====>     Agrif_<toto>(variable)      */
/*                                                                            */
/******************************************************************************/
void Instanciation_0(const char *ident)
{
    listvar *newvar;
    int out;

    if ( firstpass == 0 && sameagrifargument == 1 )
    {
        newvar = List_Global_Var;
        out = 0;
        while ( newvar && out == 0 )
        {
            if ( !strcasecmp(newvar->var->v_nomvar,ident) ) out = 1;
            else newvar = newvar->suiv;
        }
        if ( out == 0 )
        {
            newvar = List_Common_Var;
            while ( newvar && out == 0 )
            {
                if ( !strcasecmp(newvar->var->v_nomvar,ident) ) out = 1;
                else newvar = newvar->suiv;
            }
        }
        if ( out == 0 )
        {
            newvar = List_ModuleUsed_Var;
            while ( newvar && out == 0 )
            {
                if ( !strcasecmp(newvar->var->v_nomvar,ident) ) out = 1;
                else newvar = newvar->suiv;
            }
        }
//         if ( out == 1 )
//         {
//             /* then write the instanciation                                      */
//             fprintf(fortran_out,"\n      %s = %s",ident,vargridcurgridtabvars(newvar->var,3));
//             printf("#\n# Instanciation_0: |%s = %s|\n#\n", ident,vargridcurgridtabvars(newvar->var,3));
//         }
    }
    sameagrifargument = 0;
}
