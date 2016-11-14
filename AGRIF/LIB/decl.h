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
#define LONG_VNAME 80		// Max length for a variable name
#define LONG_FNAME 1000		// Max length for a file name
#define LONG_C     200
#define LONG_M     1500

#define NB_CAT_VARIABLES 5

/******************************************************************************/
/*********** Declaration of structures used in conv ***************************/
/******************************************************************************/

typedef struct
{
   char first[LONG_M];
   char last[LONG_M];
} typedim ;                /* fortran dimension as 'ndeb:nfin'                */

typedef struct listdim
{
   typedim dim;
   struct listdim *suiv;
} listdim;                 /* list of the dimensions of a variable            */

typedef struct listname
{
   char n_name[LONG_M];
   struct  listname* suiv;
} listname ;            /* list of names                                  */

typedef struct do_loop
{
   char do_variable[LONG_VNAME];
   char do_begin[LONG_VNAME];
   char do_end[LONG_VNAME];
   char do_step[LONG_VNAME];
} do_loop ;

typedef struct listdoloop
{
   do_loop *cur_do_loop;
   struct listdoloop* suiv;
} listdoloop;

typedef struct variable
{
   char v_typevar[LONG_VNAME];
   char v_nomvar[LONG_VNAME] ;
   char v_oldname[LONG_VNAME] ;
   char v_dimchar[LONG_VNAME];
   char v_modulename[LONG_VNAME];
   char v_commonname[LONG_VNAME];
   char v_vallengspec[LONG_VNAME];
   char v_nameinttypename[LONG_VNAME];
   char v_commoninfile[LONG_FNAME];
   char v_subroutinename[LONG_VNAME];
   listdoloop *v_do_loop;
   char v_precision[LONG_C];
   listname *v_initialvalue;
   listname *v_initialvalue_array;
   char v_IntentSpec[LONG_M];
   char v_readedlistdimension[LONG_M];
   int  v_nbdim;
   int  v_common;
   int  v_positioninblock;
   int  v_module;
   int  v_save;
   int  v_catvar;
   int  v_VariableIsParameter;
   int  v_PublicDeclare;
   int  v_PrivateDeclare;
   int  v_ExternalDeclare;
   int  v_pointedvar;
   int  v_notgrid;
   int  v_dimensiongiven;
   int  v_c_star;
   int  v_indicetabvars;
   int  v_pointerdeclare;
   int  v_optionaldeclare;
   int  v_allocatable;
   int  v_target;
   int  v_dimsempty;
   listdim *v_dimension;
} variable ;               /* type of a variable                              */
                           /* v_typevar : type (integer, real, ...)           */
                           /* v_nomvar : name of the variable                 */
                           /* v_dimension : list of dimensions of the variable*/
                           /* v_nbdim: 1 if the variable is 1d, etc ...       */
                           /* precision : Name of the variable which          */
                           /* determine the precision. example : wp in the    */
                           /* case where REAL(wp)                             */

typedef struct listvar
{
   variable *var ;
   struct listvar * suiv;
} listvar ;                /* list of variables                               */


typedef struct listusemodule
{
   char u_usemodule[LONG_VNAME];
   char u_charusemodule[LONG_VNAME];
   char u_cursubroutine[LONG_VNAME];
   char u_modulename[LONG_VNAME];
   int  u_firstuse;
   struct listusemodule * suiv;
} listusemodule;           /* list of names                                   */

typedef struct listparameter
{
   char p_name[LONG_M];
   char p_modulename[LONG_M];
   struct listparameter * suiv;
} listparameter ;           /* list of names                                  */

typedef struct listcouple
{
   char c_namevar[LONG_VNAME];
   char c_namepointedvar[LONG_VNAME];
   struct listcouple * suiv;
} listcouple;              /* list of names                                   */


typedef struct listnom
{
   char o_nom[LONG_C];
   char o_module[LONG_VNAME];
   char o_subroutinename[LONG_M];
   int  o_val;
   listcouple *couple;
   struct listnom * suiv;
} listnom;                 /* list of names                                   */


typedef struct listallocate
{
   char a_nomvar[LONG_C];
   char a_subroutine[LONG_VNAME];
   char a_module[LONG_VNAME];
   struct listallocate * suiv;
} listallocate ;


typedef struct listvarpointtovar
{
   char t_usemodule[LONG_VNAME];
   char t_cursubroutine[LONG_VNAME];
   listcouple *t_couple;
   struct  listvarpointtovar* suiv;
}listvarpointtovar ;       /* list of names                                   */


typedef struct listindice
{
   int i_indice;
   struct  listindice * suiv;
} listindice;              /* list of indiced                                 */

 variable *curvar;

 listvar *List_ModuleUsedInModuleUsed_Var;
 listvar *List_ModuleUsed_Var;
 listvar *listduplicated;

 listvar *List_GlobalParameter_Var;
 listvar *List_Global_Var;
 listvar *List_Data_Var;
 listvar *List_Data_Var_Cur;
 listvar *List_Save_Var;
 listvar *List_SubroutineArgument_Var;
 listvar *List_SubroutineDeclaration_Var;
 listvar *List_UsedInSubroutine_Var;
 listvar *List_Parameter_Var;
 listvar *List_Dimension_Var;
 listvar *List_FunctionType_Var;
 listvar *List_NotGridDepend_Var;
 listvar *List_Common_Var;


 listname *List_Pointer_Var;
 listname *List_ImplicitNoneSubroutine;
 
 listname *List_Do_labels; 
 /* A list that contains the do labels if any */

 listusemodule *List_NameOfModuleUsed;
 listusemodule *List_Include;
 listusemodule *listofmoduletmp;
 listusemodule *tmpuselocallist;

 listparameter *List_GlobParamModuleUsedInModuleUsed_Var;
 listparameter *List_GlobParamModuleUsed_Var;

 listnom *List_ContainsSubroutine;
 listnom *List_Subroutine_For_Alloc;
 listnom *listofmodules;
 listnom *listofkind;
 listnom *List_NameOfModule;
 listnom *List_NameOfCommon;
 listnom *List_SubroutineWhereAgrifUsed;

 listallocate *List_Allocate_Var;

 listvarpointtovar *List_CouplePointed_Var;
                           /*  variables which are pointed to an other one    */

 listindice *Listofavailableindices;
                           /* List of available indices in the tabvars table  */
 listindice **Listofavailableindices_glob;

 listdim *curdim;
 listdim *commondim;

/******************************************************************************/
/****************   *** COMMON Variables ***  *********************************/
/******************************************************************************/

 int positioninblock;
 char commonvar[LONG_VNAME];
 char commonblockname[LONG_VNAME];

/******************************************************************************/
/****************   *** AGRIF Variables ***   *********************************/
/******************************************************************************/
 int inagrifcallargument;
 int afterpercent;
 int sameagrifargument;
 int InAgrifParentDef;
 char sameagrifname[LONG_VNAME];
/******************************************************************************/
/****************   *** VAR DEF Variables ***   *******************************/
/******************************************************************************/
 int indicemaxtabvars[NB_CAT_VARIABLES];     /* Number of variables in the model i.e. last      */
                           /*    indice used in  the tabvars table            */
 int PublicDeclare;        /* Variable has been declared as PUBLIC */
 int PrivateDeclare;       /* Variable has been declared as PRIVATE */
 int ExternalDeclare;      /* Variable has been declared as EXTERNAL */
 int InitialValueGiven;    /* An initial value has been given */
 int Allocatabledeclare;
 int Targetdeclare;
 int SaveDeclare;
 int functiondeclarationisdone;
 int pointerdeclare;
 int optionaldeclare;
 int inside_type_declare;
 int VariableIsParameter;
 int dimsgiven;
 int shouldincludempif;
 int c_star;
 char DeclType[LONG_VNAME];
 char nameinttypename[LONG_VNAME];
 char nameinttypenameback[LONG_VNAME];
 int GlobalDeclaration;
 int GlobalDeclarationType;
 char InitValue[LONG_M];
 char IntentSpec[LONG_M];
 char NamePrecision[LONG_C];
 char CharacterSize[LONG_VNAME];
 char vallengspec[LONG_VNAME];
 int isrecursive;
 int is_result_present;

/******************************************************************************/
/****************   *** CONV Variables ***   **********************************/
/******************************************************************************/
 int dimprob ;             /* dimension of the problem : 1 for 1D,2 for 2D,   */
                           /*    3 for 3D                                     */
 int onlyfixedgrids;       /* = 1 if onlyfixedgrids is true                   */
 int todebug;
 int fixedgrids;           /* = 1 if fixedgrids is true                       */
 char nbmaillesX[LONG_VNAME];	// number of cells in the x direction
 char nbmaillesY[LONG_VNAME];	// number of cells in the y direction
 char nbmaillesZ[LONG_VNAME];	// number of cells in the z direction
 int IndicenbmaillesX;
 int IndicenbmaillesY;
 int IndicenbmaillesZ;

 int inmodulemeet;
 int incalldeclare;
 int aftercontainsdeclare; /* Signale si l'on vient d'un contains ou non */
 int retour77;
 int callagrifinitgrids;
 int callmpiinit;
 int firstpass;
 int pointedvar;
 int NbMailleXDefined;
 int agrif_parentcall;
 int didvariableadded;
 int SubloopScalar;        /* = 1 we should put in argument of sub_loop       */
                           /*    only                                         */
                           /*    scalar and not table u(1,1,1) in place of u  */
 int inprogramdeclare;
 int insubroutinedeclare;
 int inmoduledeclare;
 int dimsempty;
 int created_dimensionlist;
 int incontainssubroutine;

 char meetagrifinitgrids[LONG_M];
 char mpiinitvar[LONG_M];
 char toprintglob[LONG_M];
 char tmpvargridname[LONG_M];
 char dependfilename[LONG_FNAME];
 char charusemodule[LONG_VNAME];
 char subofagrifinitgrids[LONG_M];
 char curmodulename[LONG_VNAME];
 char subroutinename[LONG_VNAME];
 char old_subroutinename[LONG_VNAME]; // For internal subprogramm
 char cur_filename[LONG_FNAME];		// Name of the current parsed Fortran file
 char config_file[LONG_FNAME];		// Name of conv configuration file (ex: amr.in)
 char work_dir[LONG_FNAME];			// Work directory         (default: './')
 char include_dir[LONG_FNAME];		// Include directory      (default: './AGRIF_INC')
 char output_dir[LONG_FNAME];		// output directory       (default: './AGRIF_MODELFILES')
 char input_dir[LONG_FNAME];		// source input directory (default: './')

 FILE *fortran_out;          /* Output File                                    */
 FILE *fortran_in;           /* Input File                                     */
 FILE *oldfortran_out;
 FILE *old_oldfortran_out; // For internal subprogramm
 FILE *subloop;
 FILE *module_declar;
 FILE *allocationagrif;

 long int pos_cur;         /* current position in the output file             */
 long int pos_curagrifparent;
                           /* current position in the output file             */
 long int pos_curcall;     /* current position in the output file             */
 long int pos_curuse;      /* current position in the output file             */
 long int pos_curuseold;   /* current position in the output file             */
 long int pos_curfunction; /* current position in the output file             */
 long int pos_cur_decl;    /* current position in the output file             */
 long int pos_curdata;     /* current position in the output file             */
 long int pos_curparameter;/* current position in the output file             */
 long int pos_curcommon;   /* current position in the output file             */
 long int pos_cursave;     /* current position in the output file             */
 long int pos_curdimension;/* current position in the output file             */
 long int pos_curinclude;  /* final position of a line in file                */
 long int pos_end;         /* final position of a line in file                */
 long int pos_endsubroutine;
                           /* final position of a line in file                */

size_t length_last;
size_t length_first;
size_t length_v_vallengspec;
size_t length_v_commoninfile;
size_t length_v_precision;
size_t length_v_IntentSpec;
size_t length_v_initialvalue;
size_t length_v_readedlistdimension;
size_t length_a_nomvar;
size_t length_toprintglob;
size_t length_tmpvargridname;
size_t length_ligne_Subloop;
size_t length_toprint_utilagrif;
size_t length_toprinttmp_utilchar;
size_t length_ligne_writedecl;
size_t length_newname_toamr;
size_t length_newname_writedecl;
size_t length_ligne_toamr;
size_t length_tmpligne_writedecl;
 int value_char_size;
 int value_char_size1;
 int value_char_size2;
 int value_char_size3;


 int inallocate;
 int infixed;
 int infree;
/******************************************************************************/
/*********** Declaration of externals subroutines *****************************/
/***************************************************** ************************/
extern char *fortran_text;
/******************************************************************************/
/*********** convert.y ********************************************************/
/******************************************************************************/
extern int main(int argc,char *argv[]);
extern int convert_error(const char *s);
/******************************************************************************/
/*********** fortran.y ********************************************************/
/******************************************************************************/
extern void process_fortran(const char *input_file);
extern int fortran_error(const char *s);
/******************************************************************************/
/*********** dependfile.c *****************************************************/
/******************************************************************************/
extern void Writethedependnbxnbyfile();
extern void Readthedependnbxnbyfile();
extern void Writethedependlistofmoduleused(const char *NameTampon );
extern void Readthedependlistofmoduleused(const char *NameTampon);
extern void WritedependParameterList(const char *NameTampon );
extern listparameter *ReaddependParameterList(const char *NameTampon, listparameter *listout);
extern void Writethedependfile(const char *NameTampon, listvar *input );
extern listvar *Readthedependfile(const char *NameTampon , listvar *listout);
extern void Write_Subroutine_For_Alloc();
extern void Read_Subroutine_For_Alloc();
extern void Writethedependavailablefile();
extern void Readthedependavailablefile();
extern int is_dependfile_created(const char *NameTampon);
extern void Write_val_max();
extern void Read_val_max();
/******************************************************************************/
/*********** DiversListe.c ****************************************************/
/******************************************************************************/
extern void Add_Common_var_1();
extern listnom  *Addtolistnom(const char *nom, listnom *listin, int value);
extern listname *Addtolistname(const char *nom, listname *input);
extern int ModuleIsDefineInInputFile(const char *name);
extern void Addmoduletothelisttmp(const char *name);
extern void Add_NameOfModule_1(const char *nom);
extern void Add_NameOfCommon_1(const char *nom, const char *cursubroutinename);
extern void Add_CouplePointed_Var_1(const char *namemodule, listcouple *couple);
extern void Add_Include_1(const char *name);
extern void Add_ImplicitNoneSubroutine_1();
extern void WriteIncludeDeclaration(FILE* tofile);
extern void Add_Save_Var_1 (const char *name,listdim *d);
extern void Add_Save_Var_dcl_1 (listvar *var);
/******************************************************************************/
/*********** SubLoopCreation.c ************************************************/
/******************************************************************************/
extern void WriteBeginof_SubLoop();
extern void WriteVariablelist_subloop(char **ligne, size_t *line_length);
extern void WriteVariablelist_subloop_Call(char **ligne, size_t *line_length);
extern void WriteVariablelist_subloop_Def(char **ligne, size_t *line_length);
extern void WriteHeadofSubroutineLoop();
extern void closeandcallsubloopandincludeit_0(int suborfun);
extern void closeandcallsubloop_contains_0();
/******************************************************************************/
/*********** toamr.c **********************************************************/
/******************************************************************************/
extern void WARNING_CharSize(const variable *var);
extern const char * tabvarsname(const variable *var);
extern const char * variablecurgridtabvars(int which_grid);
extern const char * vargridnametabvars(const variable *var, int iorindice);
extern const char * vargridcurgridtabvars(const variable *var, int which_grid);
extern const char * vargridcurgridtabvarswithoutAgrif_Gr(const variable *var);
extern const char * vargridparam(const variable *var);
extern void write_probdimagrif_file();
extern void write_keysagrif_file();
extern void write_modtypeagrif_file();
extern void write_createvarnameagrif_file(variable *v,FILE *createvarname,int *InitEmpty);
extern void write_initialisationsagrif_file(variable *v,FILE *initproc,int *VarnameEmpty);
extern void write_Setnumberofcells_file();
extern void write_Getnumberofcells_file();
extern void Write_Alloc_Agrif_Files();
extern int IndiceInlist(int indic, listindice *listin);
extern void write_allocation_Common_0();
extern void write_allocation_Global_0();
extern void creefichieramr();
/******************************************************************************/
/*********** UtilAgrif.c ******************************************************/
/******************************************************************************/
extern int Vartonumber(const char *tokname);
extern int Agrif_in_Tok_NAME(const char *tokname);
extern void ModifyTheVariableName_0(const char *ident,int lengthname);
extern void Add_SubroutineWhereAgrifUsed_1(const char *sub, const char *mod);
extern void AddUseAgrifUtil_0(FILE *fileout);
extern void AddUseAgrifUtilBeforeCall_0(FILE *fileout);
extern void NotifyAgrifFunction_0(const char *ident);
extern void ModifyTheAgrifFunction_0(const char *ident);
extern void AgriffunctionModify_0(const char *ident,int whichone);
extern void Instanciation_0(const char *ident);
/******************************************************************************/
/*********** UtilCharacter.c **************************************************/
/******************************************************************************/
extern void FindAndChangeNameToTabvars(const char name[LONG_M],char toprint[LONG_M],
                                             listvar * listtosee, int whichone);
extern const char *ChangeTheInitalvaluebyTabvarsName(const char *nom,listvar *listtoread);
extern int IsVariableReal(const char *nom);
extern void IsVarInUseFile(const char *nom);
extern listnom *DecomposeTheNameinlistnom(const char *nom, listnom * listout);
extern void DecomposeTheName(const char *nom);
extern void convert2lower(char *lowername, const char* inputname);
extern int convert2int(const char *name);
/******************************************************************************/
/*********** UtilFile.c *******************************************************/
/******************************************************************************/
extern FILE * open_for_write (const char *filename);
extern FILE * open_for_append (const char *filename);
extern long int setposcur();
extern long int setposcurname(FILE *fileout);
extern void copyuse_0(const char *namemodule);
extern void copyuseonly_0(const char *namemodule);
/******************************************************************************/
/*********** UtilFortran.c ****************************************************/
/******************************************************************************/
extern void initdimprob(int dimprobmod, const char *nx, const char *ny, const char *nz);
extern int Variableshouldberemoved(const char *nom);
extern int variableisglobal(listvar *curvar, listvar *listin);
extern int VariableIsInListCommon(listvar *curvar,listvar *listin);
extern int VariableIsInList(listvar *curvar,listvar *listin);
extern void variableisglobalinmodule(listcouple *listin, const char *module,
                                                                 FILE *fileout,long int oldposcuruse);
extern void Write_Word_end_module_0();
extern void Add_Subroutine_For_Alloc(const char *nom);
extern void Write_Closing_Module();
extern int IsTabvarsUseInArgument_0();
extern int ImplicitNoneInSubroutine();
extern void Add_Pointer_Var_From_List_1(listvar *listin);
extern void Add_Pointer_Var_1(char *nom);
extern int varispointer_0(char *ident);
extern int VariableIsFunction(const char *ident);
extern int varistyped_0(char *ident);
extern void dump_var(const variable* var);
extern void removenewline(char *nom);
/******************************************************************************/
/*********** UtilListe.c ******************************************************/
/******************************************************************************/
extern void Init_Variable(variable *var);
extern listvar * AddListvarToListvar(listvar *l,listvar *glob, int ValueFirstpass);
extern void CreateAndFillin_Curvar(const char *type, variable *curvar);
// extern void duplicatelistvar(listvar *orig);
extern listdim * insertdim(listdim *lin,typedim nom);
extern void change_dim_char(listdim *lin,listvar * l);
extern int get_num_dims(const listdim *d);
extern variable * createvar(const char *nom, listdim *d);
extern listvar * insertvar(listvar *lin,variable *v);
extern listvar * settype(const char *nom,listvar *lin);
extern void printliste(listvar * lin);
extern int IsinListe(listvar *lin,char *nom);
extern listname *Insertname(listname *lin,char *nom,int sens);
extern int testandextractfromlist(listname **lin, char*nom);
extern void removefromlist(listname **lin, char*nom);
extern listname *concat_listname(listname *l1, listname *l2);
extern void createstringfromlistname(char *ligne, listname *lin);
extern void printname(listname * lin);
extern void removeglobfromlist(listname **lin);
extern void writelistpublic(listname *lin);
extern void Init_List_Data_Var();
extern void  addprecision_derivedfromkind(variable *curvar);
extern int get_cat_var(variable *var);
extern void Insertdoloop(variable *var,char *do_var, char *do_begin, char *do_end, char *do_step);
/******************************************************************************/
/*********** UtilNotGridDep.c *************************************************/
/******************************************************************************/
extern void Add_NotGridDepend_Var_1 (char *name);
extern int VarIsNonGridDepend(char *name);
/******************************************************************************/
/*********** WorkWithAllocatelist.c *******************************************/
/******************************************************************************/
extern void Add_Allocate_Var_1(const char *nom, const char *nommodule);
extern int IsVarAllocatable_0(const char *ident);
/******************************************************************************/
/*********** WorkWithglobliste.c **********************************************/
/******************************************************************************/
extern void Add_Globliste_1(listvar *listtoadd);
extern void Add_SubroutineDeclarationSave_Var_1(listvar *listtoadd);
extern void checkandchangedims(listvar *listsecondpass);
/******************************************************************************/
/*********** WorkWithlistdatavariable.c ***************************************/
/******************************************************************************/
extern void Add_Data_Var_1 (listvar **curlist,char *name,char *values);
extern void Add_Data_Var_Names_01 (listvar **curlist,listvar *l1, listname *l2);
/******************************************************************************/
/*********** WorkWithlistmoduleinfile.c ***************************************/
/******************************************************************************/
extern void Save_Length(const char *nom, int whichone);
extern void Save_Length_int(int val, int whichone);
/******************************************************************************/
/*********** WorkWithlistofmodulebysubroutine.c *******************************/
/******************************************************************************/
extern void RecordUseModulesVariables();
extern void  RecordUseModulesUseModulesVariables();
extern void Add_NameOfModuleUsed_1(char *name);
extern void Addmoduletothelist(const char *name);
extern void WriteUsemoduleDeclaration(const char *cursubroutinename);
/******************************************************************************/
/*********** WorkWithlistvarindoloop.c ****************************************/
/******************************************************************************/
extern void Add_UsedInSubroutine_Var_1 (const char *ident);
extern void ajoutevarindoloop_definedimension (char *name);
extern void  ModifyThelistvarindoloop();
extern void  CompleteThelistvarindoloop();
extern void Merge_Variables(variable *var1,variable *var2);
extern void Update_List_Subroutine_Var(listvar *list_to_modify);
extern void Update_List_Global_Var_From_List_Save_Var();
extern void Update_List_From_Common_Var(listvar *list_to_modify);
extern void Update_List_Var(listvar *list_to_modify);
extern void List_UsedInSubroutine_Var_Update_From_Module_Used();
extern void Update_NotGridDepend_Var(listvar *list_to_modify);
extern int LookingForVariableInList(listvar *listin,variable *var);
extern int LookingForVariableInListGlobal(listvar *listin,variable *var);
extern int LookingForVariableInListName(listvar *listin,const char *var);
extern int LookingForVariableInListGlob(listvar *listin,variable *var);
extern int LookingForVariableInListParamGlob(listparameter *listin, variable *var);
extern variable *get_variable_in_list_from_name(listvar *listin, const char *name);
extern void UpdateListDeclarationWithDimensionList();
extern void Clean_List_UsedInSubroutine_Var();
extern void Clean_List_ModuleUsed_Var();
extern void Clean_List_SubroutineDeclaration_Var();
extern void Clean_List_Global_Var();
extern void ListClean();
extern void ListUpdate();
extern void GiveTypeOfVariables();
extern void Sort_List_SubroutineArgument_Var();
extern void IndiceTabvars_Global_Var_Treated(char *nom);
extern void IndiceTabvars_Global_Var_No_Treated(char *nom);
extern void UpdateTheRemainingList(listvar *record);
extern void IndiceTabvars_Common_Var_Treated(char *nom);
extern void IndiceTabvars_Common_Var_No_Treated(char *nom);
extern void IndiceTabvarsIdentification();
extern void New_Allocate_Subroutine_Is_Necessary();
extern void New_Allocate_Subroutine_For_Common_Is_Necessary();
extern void NewModule_Creation_0();
extern void UpdateList_SubroutineWhereAgrifUsed();
extern void UpdateList_UsedInSubroutine_With_dimension();
extern void Affiche(listvar *parcours);
extern int SubInList_ContainsSubroutine();
extern void update_indicemaxtabvars(variable *var,listindice **Listofindices);
/******************************************************************************/
/*********** WorkWithParameterlist.c ******************************************/
/******************************************************************************/
extern void Add_GlobalParameter_Var_1(listvar *listin);
extern void Add_Parameter_Var_1(listvar *listin);
extern void Add_Dimension_Var_1(listvar *listin);
/******************************************************************************/
/*********** WorkWithvarofsubroutineliste.c ***********************************/
/******************************************************************************/
extern void Add_SubroutineArgument_Var_1(listvar *listtoadd);
extern void Add_FunctionType_Var_1(const char *nom);
// extern void Add_SubroutineDeclaration_Var_1 (listvar *listtoadd);
/******************************************************************************/
/*********** Writedeclarations.c **********************************************/
/******************************************************************************/
extern void WriteBeginDeclaration(variable *v,char ligne[LONG_M],int visibility);
extern void WriteScalarDeclaration(variable *v,char ligne[LONG_M]);
extern void WriteTableDeclaration(variable * v,char ligne[LONG_M],int tmpok);
extern void WriteVarDeclaration( variable *v, FILE *fileout, int value, int visibility );
extern void WriteLocalParamDeclaration(FILE* tofile);
extern void WriteFunctionDeclaration(FILE* tofile, int value);
extern void WriteSubroutineDeclaration(int value);
extern void WriteArgumentDeclaration_beforecall();
extern void WriteArgumentDeclaration_Sort(FILE* tofile);
extern int writedeclarationintoamr(listvar *deb_common, FILE *fileout, variable *var,
						const char *commonname, listnom **neededparameter, const char *name_common, int global_check);
extern void writesub_loopdeclaration_scalar(listvar *deb_common, FILE *fileout);
extern void writesub_loopdeclaration_tab(listvar *deb_common, FILE *fileout);
extern void ReWriteDeclarationAndAddTosubroutine_01(listvar *listdecl);
extern void ReWriteDataStatement_0(FILE * filout);
/******************************************************************************/
/*********** WriteInFile.c ****************************************************/
/******************************************************************************/
extern void tofich_reste (FILE * filout, const char *s, int do_returnline);
extern void tofich (FILE * filout, const char *s, int do_returnline);
extern void tofich_blanc (FILE * filout, int size);
extern void RemoveWordSET_0(FILE * filout, long int position, int sizetoremove);
extern void RemoveWordCUR_0(FILE * filout, int sizetoremove);

/******************************************************************************/
/*********** WorkWithlistofcoupled.c **********************************************/
/******************************************************************************/
extern int variscoupled_0(const char *ident) ;
extern const char * getcoupledname_0(const char *ident);
