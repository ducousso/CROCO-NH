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
/*     preparation and write of the argument list of a subroutine             */
/******************************************************************************/


/******************************************************************************/
/*                           WriteBeginof_SubLoop                             */
/******************************************************************************/
/* We should write the head of the subroutine sub_loop_<subroutinename>       */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void WriteBeginof_SubLoop()
{
   if (todebug == 1) printf("##\n## Enter in HEAD SUBLOOP for |%s|\n##\n", subroutinename);
   if ( IsTabvarsUseInArgument_0() == 1 )
   {
      if ( todebug == 1 ) printf("> enter in WriteBeginof_SubLoop : IsTabvarsUseInArgument_0() == 1\n");
      /* we should add the use agrif_uti l if it is necessary                 */
      if (todebug == 1) fprintf(fortran_out,"\n      !DEBUG: Avant WriteHeadofSubroutineLoop\n");
      WriteHeadofSubroutineLoop();
      if (todebug == 1) fprintf(fortran_out,"      !DEBUG: Apres WriteHeadofSubroutineLoop\n");
      fflush(fortran_out);
      if (todebug == 1) {
      fprintf(fortran_out,"      !DEBUG: Avant WriteUsemoduleDeclaration\n");
      }
      
      WriteUsemoduleDeclaration(subroutinename);
      if ( ImplicitNoneInSubroutine() == 1 ) fprintf(fortran_out, "      implicit none\n");
      WriteIncludeDeclaration(fortran_out);
      if (todebug == 1) fprintf(fortran_out,"      !DEBUG: Avant WriteIncludeDeclaration\n");
      /*                                                                      */
      /* We should write once the declaration of tables (extract              */
      /*    from pointer) in the new subroutine                               */
      if ( SubInList_ContainsSubroutine() == 0 ) WriteLocalParamDeclaration(fortran_out);

      writesub_loopdeclaration_scalar(List_UsedInSubroutine_Var,fortran_out);
      writesub_loopdeclaration_tab(List_UsedInSubroutine_Var,fortran_out);
      WriteArgumentDeclaration_Sort(fortran_out);
      WriteFunctionDeclaration(fortran_out, 1);
   }
   else
   {
      if ( todebug == 1 ) printf("> enter in WriteBeginof_SubLoop : IsTabvarsUseInArgument_0() == 0\n");
      AddUseAgrifUtil_0(fortran_out);
      WriteUsemoduleDeclaration(subroutinename);
      if ( ImplicitNoneInSubroutine() == 1 ) fprintf(fortran_out, "      implicit none\n");
      WriteIncludeDeclaration(fortran_out);
      WriteLocalParamDeclaration(fortran_out);
      WriteArgumentDeclaration_beforecall();
      if (todebug == 1) fprintf(fortran_out,"      !DEBUG: Avant WriteFunctionDeclaration\n");
      if ( functiondeclarationisdone == 0 ) WriteFunctionDeclaration(fortran_out, 1);
/*    writesub_loopdeclaration_scalar(List_SubroutineArgument_Var,fortran_out);
      writesub_loopdeclaration_tab(List_SubroutineArgument_Var,fortran_out);*/
   }
   if ( todebug == 1 ) printf("<   out of WriteBeginof_SubLoop\n");
   if ( todebug == 1 ) printf("## EXIT HEAD SUBLOOP (%s)\n\n", subroutinename);
}

/******************************************************************************/
/*                    WriteVariablelist_subloop                               */
/******************************************************************************/
/* This subroutine is used to write the list of the variable which            */
/* should be called by the sub_loop_<name> subroutine                         */
/* The first part is composed by the list of the local variables              */
/******************************************************************************/
/*                                                                            */
/*    List_SubroutineDeclaration_Var    a,b,c,  &                             */
/*                                      d,e,f,  &                             */
/*     a,b,c,d,e,f,g,h     ========>    g,h                                   */
/*                                                                            */
/******************************************************************************/
void WriteVariablelist_subloop(char **ligne, size_t *line_length)
{
   listvar *parcours;

   if ( todebug == 1 ) printf("> enter in WriteVariablelist_subloop\n");
   parcours = List_SubroutineArgument_Var;
   didvariableadded = 0;

   while ( parcours )
   {
      /* if the readed variable is a variable of the subroutine               */
      /*    subroutinename we should write the name of this variable          */
      /*    in the output file                                                */
      if ( !strcasecmp(parcours->var->v_subroutinename,subroutinename) )
      {
         if ( didvariableadded == 1 )   strcat(*ligne,",");
         if ( (strlen(*ligne)+strlen(parcours->var->v_nomvar)+100) > *line_length )
         {
            *line_length += LONG_M;
            *ligne = realloc( *ligne, *line_length*sizeof(char) );
         }
         strcat(*ligne,parcours->var->v_nomvar);
         didvariableadded = 1;
      }
      parcours = parcours -> suiv;
   }
   parcours = List_FunctionType_Var;
   while ( parcours )
   {
      if ( !strcasecmp(parcours->var->v_subroutinename,subroutinename) )
      {
         if ( didvariableadded == 1 )   strcat(*ligne,",");
         if ( (strlen(*ligne)+strlen(parcours->var->v_nomvar)+100) > *line_length )
         {
            *line_length += LONG_M;
            *ligne = realloc( *ligne, *line_length*sizeof(char) );
         }
         strcat(*ligne,parcours->var->v_nomvar);
         didvariableadded = 1;
      }
      parcours = parcours -> suiv;
   }
   if ( todebug == 1 ) printf("<   out of WriteVariablelist_subloop\n");
}


/******************************************************************************/
/*                     WriteVariablelist_subloop_Call                         */
/******************************************************************************/
/* This subroutine is used to write the list of the variable which            */
/* should be called by the sub_loop_<name> subroutine into the called         */
/* The second part is composed by the list of the global table                */
/******************************************************************************/
/*                                                                            */
/*   List_UsedInSubroutine_Var SubloopScalar = 0 | SubloopScalar = 1          */
/*                                a,b,c,  &      |  a,b(1,1),c,      &        */
/*     a,b,c,d,e,f,g,h  =====>    d,e,f,  &      |  d(1),e(1,1,1),f, &        */
/*                                g,h            |  g,h(1,1)                  */
/*                                                                            */
/******************************************************************************/
void WriteVariablelist_subloop_Call(char **ligne, size_t *line_length)
{
   listvar *parcours;
   char ligne2[LONG_M];
   int i;

   if ( todebug == 1 ) printf("> enter in WriteVariablelist_subloop_Call\n");
   parcours = List_UsedInSubroutine_Var;

   while ( parcours )
   {
      /* if the readed variable is a variable of the subroutine               */
      /*    subroutinename we should write the name of this variable          */
      /*    in the output file                                                */
      if ( !strcasecmp(parcours->var->v_subroutinename,subroutinename)  &&
           (parcours->var->v_pointerdeclare >= 0 || !strcasecmp(parcours->var->v_typevar,"type"))
         )
      {
         if ( didvariableadded == 1 )   strcat(*ligne,",");
         const char *vres = vargridcurgridtabvars(parcours->var, 0);
         if ( (strlen(*ligne)+strlen(parcours->var->v_nomvar)+100) > *line_length )
         {
            *line_length += LONG_M;
            *ligne = realloc( *ligne, *line_length*sizeof(char) );
         }
         strcat(*ligne, vres);
         /* if it is asked in the call of the conv we should give             */
         /* scalar in argument, so we should put (1,1,1) after the            */
         /* the name of the variable                                          */
         if (  SubloopScalar != 0 &&
               (
               (parcours->var->v_pointerdeclare >= 0 || !strcasecmp(parcours->var->v_typevar,"type"))) &&
               parcours->var->v_nbdim != 0 )
         {
             i = 1;
             while ( i <=  parcours->var->v_nbdim )
             {
                if ( i == 1 ) strcat(*ligne,"( ");
                if ( SubloopScalar == 2 )
                {
                   strcat(*ligne,":");
                   if ( i != parcours->var->v_nbdim ) strcat(*ligne,",");
                }
                else
                {
                   sprintf(ligne2,"lbound(%s,%d",vargridcurgridtabvars(parcours->var,0),i);
                   strcat(*ligne,ligne2);
                   if ( i != parcours->var->v_nbdim ) strcat(*ligne,"),");
                }
                if ( i == parcours->var->v_nbdim ) strcat(*ligne,"))");
                i++;
             }
         }
         didvariableadded = 1;
      }
      parcours = parcours -> suiv;
   }
   if ( todebug == 1 ) printf("<   out of WriteVariablelist_subloop_Call\n");
}


/******************************************************************************/
/*                       WriteVariablelist_subloop_Def                        */
/******************************************************************************/
/* This subroutine is used to write the list of the variable which            */
/* should be called by the sub_loop_<name> subroutine into the def            */
/* The second part is composed by the list of the global table                */
/* <name>_tmp                                                                 */
/******************************************************************************/
/*                                                                            */
/*       List_UsedInSubroutine_Var                                            */
/*                                a-tmp,b-tmp,c_tmp, &                        */
/*     a,b,c,d,e,f,g,h  =====>    d_tmp,e_tmp,f_tmp, &                        */
/*                                g_tmp,h_tmp                                 */
/*                                                                            */
/******************************************************************************/
void WriteVariablelist_subloop_Def(char **ligne, size_t *line_length)
{
   listvar *parcours;

   if ( todebug == 1 ) printf("> enter in WriteVariablelist_subloop_Def\n");
   parcours = List_UsedInSubroutine_Var;

   while ( parcours )
   {
      /* if the readed variable is a variable of the subroutine               */
      /*    subrotinename we should write the name of this variable           */
      /*    in the output file                                                */
      if ( !strcasecmp(parcours->var->v_subroutinename,subroutinename)  &&
           (parcours->var->v_pointerdeclare >= 0 || !strcasecmp(parcours->var->v_typevar,"type")) )
      {
         if ( didvariableadded == 1 )   strcat(*ligne,",");
         if ( (strlen(*ligne)+strlen(parcours->var->v_nomvar)+100) > *line_length )
         {
            *line_length += LONG_M;
            *ligne = realloc( *ligne, *line_length*sizeof(char) );
         }
         strcat(*ligne,parcours->var->v_nomvar);
         didvariableadded = 1;
      }
      parcours = parcours -> suiv;
   }

   if ( todebug == 1 ) printf("<   out of WriteVariablelist_subloop_Def\n");
}

/******************************************************************************/
/*                      WriteHeadofSubroutineLoop                             */
/******************************************************************************/
/* This subroutine is used to write the head of the subroutine                */
/* Sub_Loop_<name>                                                            */
/******************************************************************************/
/*                 Sub_loop_subroutine.h                                      */
/*                                                                            */
/*                 subroutine Sub_Loop_subroutine ( &                         */
/*                 a,b,c, &                                                   */
/* SubLoopScalar   d,e(1,1),f(1,1,1), &                                       */
/*                 g,h  &                                                     */
/*                 )                                                          */
/******************************************************************************/
void WriteHeadofSubroutineLoop()
{
   char *ligne;
   FILE * subloop;
   size_t line_length;

   ligne = (char*) calloc(LONG_M, sizeof(char));
   line_length = LONG_M;

   if ( todebug == 1 ) printf("> enter in WriteHeadofSubroutineLoop subroutine %s\n",subroutinename);
   tofich(fortran_out,"\n",1);

   /* Open this newfile                                                       */
   sprintf(ligne,"Sub_Loop_%s.h",subroutinename);
   subloop = open_for_write(ligne);
   /*                                                                         */
   if (isrecursive) sprintf(ligne,"recursive subroutine Sub_Loop_%s(",subroutinename);
   else             sprintf(ligne,"subroutine Sub_Loop_%s(",subroutinename);

   /*                                                                         */
   if (todebug == 1) fprintf(subloop,"      !DEBUG: Avant WriteVariablelist_subloop\n");
   WriteVariablelist_subloop(&ligne,&line_length);
   WriteVariablelist_subloop_Def(&ligne,&line_length);
   /*                                                                         */
   strcat(ligne,")");
   tofich(subloop,ligne,1);

   /* if USE agrif_Uti l should be add                                        */
   if (todebug == 1) fprintf(subloop,"      !DEBUG: Avant AddUseAgrifUtil_0\n");
   AddUseAgrifUtil_0(subloop);

   /*                                                                         */
   if (todebug == 1) fprintf(subloop,"      !DEBUG: Apres AddUseAgrifUtil_0\n");
   oldfortran_out = fortran_out;
   fortran_out = subloop;
   
   if ( todebug == 1 ) printf("<   out of WriteHeadofSubroutineLoop\n");
   
   free(ligne);
}

/******************************************************************************/
/*                closeandcallsubloopandincludeit_0                           */
/******************************************************************************/
/* Firstpass 0                                                                */
/* We should close the sub_loop subroutine, call it and close the             */
/* function (suborfun = 0)                                                    */
/* subroutine (suborfun = 1)                                                  */
/* end (suborfun = 2)                                                         */
/* end program (suborfun = 3)                                                 */
/* and include the sub_loop subroutine after                                  */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void closeandcallsubloopandincludeit_0(int suborfun)
{
   char *ligne;
   size_t line_length;

   if ( firstpass == 1 )    return;
   if ( todebug == 1 ) printf("> enter in closeandcallsubloopandincludeit_0\n");

   ligne = (char*) calloc(LONG_M, sizeof(char));
   line_length = LONG_M;

   if ( IsTabvarsUseInArgument_0() == 1 )
   {
      /* We should remove the key word end subroutine                         */
      RemoveWordCUR_0(fortran_out,setposcur()-pos_endsubroutine);
      /* We should close the loop subroutine                                  */
      tofich(fortran_out,"\n",1);
      sprintf(ligne,"end subroutine Sub_Loop_%s\n",subroutinename);
      tofich(fortran_out,ligne,1);
      fclose(fortran_out);
      fortran_out = oldfortran_out;

      AddUseAgrifUtilBeforeCall_0(fortran_out);
           
      WriteArgumentDeclaration_beforecall();
      if (todebug == 1) fprintf(fortran_out,"      !DEBUG: Avant WriteFunctionDeclaration\n");

      if ( functiondeclarationisdone == 0 ) WriteFunctionDeclaration(fortran_out, 0);
      if ( !strcasecmp(subofagrifinitgrids,subroutinename) )
            fprintf(fortran_out,"      call Agrif_Init_Grids()\n");
      /* Now we add the call af the new subroutine                            */
      tofich(fortran_out,"\n",1);
      sprintf(ligne,"  call Sub_Loop_%s(",subroutinename);
      /* Write the list of the local variables used in this new subroutine    */
      WriteVariablelist_subloop(&ligne,&line_length);
      /* Write the list of the global tables used in this new subroutine      */
      /*    in doloop                                                         */
      WriteVariablelist_subloop_Call(&ligne, &line_length);
      /* Close the parenthesis of the new subroutine called                   */
      strcat(ligne,")\n");
      tofich(fortran_out,ligne,1);
      /* we should include the above file in the original code                */

      /* We should close the original subroutine                              */
      if ( suborfun == 3 ) fprintf(fortran_out, "      end program %s\n"   , subroutinename);
      if ( suborfun == 2 ) fprintf(fortran_out, "      end\n");
      if ( suborfun == 1 ) fprintf(fortran_out, "      end subroutine %s\n", subroutinename);
      if ( suborfun == 0 ) fprintf(fortran_out, "      end function %s\n"  , subroutinename);

      fprintf(fortran_out,"\n\n#include \"Sub_Loop_%s.h\"\n",subroutinename);
    }
    oldfortran_out = (FILE *)NULL;
    if ( todebug == 1 ) printf("<   out of closeandcallsubloopandincludeit_0\n");
    
    free(ligne);
}

void closeandcallsubloop_contains_0()
{
   char *ligne;
   size_t line_length;

   if ( todebug == 1 ) printf("> enter in closeandcallsubloop_contains_0\n");
   if ( IsTabvarsUseInArgument_0() == 1 )
   {
      ligne = (char*) calloc(LONG_M, sizeof(char));
      line_length = LONG_M;
      RemoveWordCUR_0(fortran_out,9);   // Remove word 'contains'
      tofich(fortran_out,"\n",1);
      sprintf(ligne,"end subroutine Sub_Loop_%s\n",subroutinename);
      tofich(fortran_out,ligne,1);
      fclose(fortran_out);
      fortran_out = oldfortran_out;

      AddUseAgrifUtilBeforeCall_0(fortran_out);
      if ( ImplicitNoneInSubroutine() == 1 ) fprintf(fortran_out, "      implicit none\n");
      WriteLocalParamDeclaration(fortran_out);
            printf("ICI3\n");
      WriteArgumentDeclaration_beforecall();
      if (todebug == 1) fprintf(fortran_out,"      !DEBUG: Avant WriteFunctionDeclaration\n");
      if ( functiondeclarationisdone == 0 ) WriteFunctionDeclaration(fortran_out, 0);
/*      WriteSubroutineDeclaration(0);*/
      if ( !strcasecmp(subofagrifinitgrids,subroutinename) )
          fprintf(fortran_out,"      call Agrif_Init_Grids()\n");
      /* Now we add the call af the new subroutine                            */
      tofich(fortran_out,"\n",1);
      sprintf(ligne,"  call Sub_Loop_%s(",subroutinename);
      /* Write the list of the local variables used in this new subroutine    */
      WriteVariablelist_subloop(&ligne,&line_length);
      /* Write the list of the global tables used in this new subroutine      */
      /*    in doloop                                                         */
      WriteVariablelist_subloop_Call(&ligne, &line_length);
      /* Close the parenthesis of the new subroutine called                   */
      strcat(ligne,")\n");
      tofich(fortran_out,ligne,1);
      /* We should close the original subroutine                              */
      fprintf(fortran_out, "      contains\n");
      /* we should include the above file in the original code                */
      fprintf(fortran_out,"#include \"Sub_Loop_%s.h\"\n",subroutinename);
      }
   oldfortran_out = (FILE *)NULL;
   if ( todebug == 1 ) printf("<   out of closeandcallsubloop_contains_0\n");
}
