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

void Save_Length(const char *nom, int whichone)
{
    size_t len_nom = strlen(nom);
    
   if ( whichone == 1  && len_nom > length_last )
   {
      length_last = len_nom;
      if ( length_last > LONG_M )
            printf("WARNING 1 : The value of LONG_M (defined in decl.h) should be upgrated to %lu\n", length_last+100);
   }
   if ( whichone == 2  && len_nom > length_first )
   {
      length_first = len_nom;
      if ( length_first > LONG_M )
           printf("WARNING 2 : The value of LONG_M (defined in decl.h) should be upgrated to %lu\n", length_first+100);
   }
   if ( whichone == 8  && len_nom > length_v_vallengspec )
   {
      length_v_vallengspec = len_nom;
      if ( length_v_vallengspec > LONG_M )
           printf("WARNING 8 : The value of LONG_M (defined in decl.h) should be upgrated to %lu\n", length_v_vallengspec+100);
   }
   if ( whichone == 12 && len_nom > length_v_precision )
   {
      length_v_precision = len_nom;
      if ( length_v_precision > LONG_M )
           printf("WARNING 12 : The value of LONG_M (defined in decl.h) should be upgrated to %lu\n", length_v_precision+100);
   }
   if ( whichone == 13 && len_nom > length_v_IntentSpec )
   {
      length_v_IntentSpec = len_nom;
      if ( length_v_IntentSpec > LONG_M )
           printf("WARNING 13 : The value of LONG_M (defined in decl.h) should be upgrated to %lu\n", length_v_IntentSpec+100);
   }
   if ( whichone == 14 && len_nom > length_v_initialvalue )
   {
      length_v_initialvalue = len_nom;
      if ( length_v_initialvalue > LONG_M )
           printf("WARNING 14 : The value of LONG_M (defined in decl.h) should be upgrated to %lu\n", length_v_initialvalue+100);
   }
   if ( whichone == 15 && len_nom > length_v_readedlistdimension )
   {
      length_v_readedlistdimension = len_nom;
      if ( length_v_readedlistdimension > LONG_M )
           printf("WARNING 15 : The value of LONG_M (defined in decl.h) should be upgrated to %lu\n", length_v_readedlistdimension+100);
   }
   if ( whichone == 25 && len_nom > length_a_nomvar )
   {
      length_a_nomvar = len_nom;
      if ( length_a_nomvar > LONG_C )
           printf("WARNING 25 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_a_nomvar+100);
   }
   if ( whichone == 39 && len_nom > length_toprintglob )
   {
      length_toprintglob = len_nom;
      if ( length_toprintglob > LONG_M )
           printf("WARNING 39 : The value of LONG_M (defined in decl.h) should be upgrated to %lu\n", length_toprintglob+100);
   }
   if ( whichone == 40 && len_nom > length_tmpvargridname )
   {
      length_tmpvargridname = len_nom;
      if ( length_tmpvargridname > LONG_M )
           printf("WARNING 40 : The value of LONG_M (defined in decl.h) should be upgrated to %lu\n", length_tmpvargridname+100);
   }
   if ( whichone == 41 && len_nom > length_ligne_Subloop )
   {
       length_ligne_Subloop = len_nom;
      if ( length_ligne_Subloop > LONG_M )
           printf("WARNING 41 : The value of LONG_M (defined in decl.h) should be upgrated to %lu\n",length_ligne_Subloop+100);
   }
   if ( whichone == 43 && len_nom > length_toprint_utilagrif )
   {
      length_toprint_utilagrif = len_nom;
      if ( length_toprint_utilagrif > LONG_M )
           printf("WARNING 43 : The value of LONG_M (defined in decl.h) should be upgrated to %lu\n", length_toprint_utilagrif+100);
   }
   if ( whichone == 44 && len_nom > length_toprinttmp_utilchar )
   {
      length_toprinttmp_utilchar = len_nom;
      if ( length_toprinttmp_utilchar > LONG_M)
           printf("WARNING 44 : The value of LONG_M (defined in decl.h) should be upgrated to %lu\n", length_toprinttmp_utilchar+100);
   }
   if ( whichone == 45 && len_nom > length_ligne_writedecl )
   {
      length_ligne_writedecl = len_nom;
      if ( length_ligne_writedecl > LONG_M )
           printf("WARNING 45 : The value of LONG_M (defined in decl.h) should be upgrated to %lu\n", length_ligne_writedecl+100);
   }
   if ( whichone == 46 && len_nom > length_newname_toamr )
   {
      length_newname_toamr = len_nom;
      if ( length_newname_toamr > LONG_C )
           printf("WARNING 46 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_newname_toamr+100);
   }
   if ( whichone == 47 && len_nom > length_newname_writedecl )
   {
      length_newname_writedecl = len_nom;
      if ( length_newname_writedecl > LONG_M )
           printf("WARNING 47 : The value of LONG_M (defined in decl.h) should be upgrated to %lu\n", length_newname_writedecl +100);
   }
   if ( whichone == 48 && len_nom > length_ligne_toamr )
   {
      length_ligne_toamr = len_nom;
      if ( length_ligne_toamr > LONG_M )
           printf("WARNING 48 : The value of LONG_M (defined in decl.h) should be upgrated to %lu\n", length_ligne_toamr +100);
   }
   if ( whichone == 49 && len_nom > length_tmpligne_writedecl )
   {
      length_tmpligne_writedecl = len_nom;
      if ( length_tmpligne_writedecl > LONG_M )
           printf("WARNING 49 : The value of LONG_M (defined in decl.h) should be upgrated to %lu\n", length_tmpligne_writedecl+100);
   }
}

void Save_Length_int(int val, int whichone)
{
   if ( whichone == 1 && val > value_char_size )    value_char_size  = val;
   if ( whichone == 2 && val > value_char_size1 )   value_char_size1 = val;
   if ( whichone == 3 && val > value_char_size2 )   value_char_size2 = val;
   if ( whichone == 4 && val > value_char_size3 )   value_char_size3 = val;
}
