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
/*                            tofich_reste                                    */
/******************************************************************************/
/* This subroutine is used to write the string s into the fileout             */
/******************************************************************************/
void tofich_reste (FILE * filout, const char *s, int do_returnline)
{
    const size_t line_length = 66;
    char temp[line_length+1];
    size_t s_length;

    s_length = strlen(s);

    if ( !strcmp(&s[strlen(s)-1], "\n") )
        s_length = strlen(s)-1;

    if ( s_length <= line_length )
    {
        if ( do_returnline ) fprintf(filout, "     &%s\n", s);
        else                 fprintf(filout, "&%s", s);
    }
    else
    {
        strncpy(temp, s, line_length);
        temp[line_length] = '\0';
        if ( retour77 == 0 && (s_length-strlen(temp) > 0) )
                fprintf(filout, "     &%s&\n", temp);
        else    fprintf(filout, "     &%s\n", temp);

        if ( s_length-strlen(temp) > 0 )
            tofich_reste(filout, (char *) &s[line_length], do_returnline);
    }
}

/******************************************************************************/
/*                            tofich                                          */
/******************************************************************************/
/* This subroutine is used to write the string s into the fileout             */
/******************************************************************************/
void tofich (FILE * filout, const char *s, int do_returnline)
{
    const size_t line_length = 66;
    char temp[line_length+1];
    size_t s_length;

    s_length = strlen(s);

    if ( !strcmp(&s[strlen(s)-1], "\n") )
        s_length = strlen(s)-1;

    if ( s_length <= line_length )
    {
        if ( do_returnline ) fprintf(filout, "      %s\n", s);
        else                 fprintf(filout, "%s", s);
    }
    else
    {
        strncpy(temp, s, line_length);
        temp[line_length] = '\0';

        if ( retour77 == 0 )    fprintf(filout, "      %s&\n", temp);
        else                    fprintf(filout, "      %s\n", temp);

        tofich_reste(filout, (char *) &s[line_length], do_returnline);
    }
}

/******************************************************************************/
/*                       tofich_blanc                                         */
/******************************************************************************/
/* This subroutine is used to write size blank into the fileout               */
/******************************************************************************/
void tofich_blanc (FILE * filout, int size)
{
    const char* empty_char = " ";
    int i = 0;

    if (size <= 65)
        fprintf(filout, "%*s\n", size, empty_char);
    else
    {
        do
        {
            fprintf(filout, "%*s\n", 65, empty_char);
            i++;
        }
        while ( i <= size / 65 );

        fprintf(filout, "%*s\n", size%65, empty_char);
    }
}

/******************************************************************************/
/*                           RemoveWordSET_0                                  */
/******************************************************************************/
/* This subroutine is used to remove a sentence in the file filout            */
/******************************************************************************/
void RemoveWordSET_0(FILE * filout, long int position, int sizetoremove)
{
    if ( inside_type_declare || firstpass ) return;

    fseek(filout, position, SEEK_SET);
    tofich_blanc(filout, sizetoremove);

}

/******************************************************************************/
/*                         RemoveWordCUR_0                                    */
/******************************************************************************/
/* This subroutine is used to remove a sentence in the file filout            */
/******************************************************************************/
void RemoveWordCUR_0(FILE * filout, int sizetoremove)
{
    if ( inside_type_declare || firstpass ) return;

    fseek(filout, (long int)(-sizetoremove), SEEK_CUR);
    tofich_blanc(filout, sizetoremove);
    fseek(filout, (long int)(-sizetoremove), SEEK_CUR);
    if ( strstr(fortran_text, "\n") )   fprintf(filout, "\n");
}
