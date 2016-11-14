OBJS = main.o WriteInFile.o toamr.o fortran.o  \
       dependfile.o SubLoopCreation.o WorkWithlistvarindoloop.o \
       WorkWithvarofsubroutineliste.o WorkWithParameterlist.o \
       Writedeclarations.o WorkWithglobliste.o UtilFortran.o \
       UtilNotGridDep.o WorkWithlistdatavariable.o \
       DiversListe.o UtilAgrif.o WorkWithAllocatelist.o \
       UtilCharacter.o UtilListe.o UtilFile.o \
       WorkWithlistofmodulebysubroutine.o WorkWithlistmoduleinfile.o \
       WorkWithlistofcoupled.o

.SUFFIXES:
.SUFFIXES: .c .o

all: conv
	@echo
	@echo ===================================================
	@echo   CONV is ok
	@echo ===================================================
	@echo
# Modif Laurent Debreu car Version de bison trop ancienne => Eviter l'appel a Bison
#main.c: convert.y convert.lex
#	@echo ===================================================
#	@echo   Rebuilding main.c ...
#	@echo ===================================================
#	$(MAKE) -f Makefile.lex main.c
#
#fortran.c: fortran.y fortran.lex
#	@echo ===================================================
#	@echo   Rebuilding fortran.c ...
#	@echo ===================================================
#	$(MAKE) -f Makefile.lex fortran.c

conv: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o ./$@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

main.o : main.c
fortran.o : fortran.c	
toamr.o : toamr.c decl.h
WriteInFile.o : WriteInFile.c decl.h
dependfile.o : dependfile.c decl.h
SubLoopCreation.o : SubLoopCreation.c decl.h
WorkWithglobliste.o : WorkWithglobliste.c decl.h
WorkWithlistvarindoloop.o : WorkWithlistvarindoloop.c decl.h
WorkWithvarofsubroutineliste.o : WorkWithvarofsubroutineliste.c decl.h
Writedeclarations.o : Writedeclarations.c decl.h
UtilFortran.o : UtilFortran.c decl.h
WorkWithParameterlist.o : WorkWithParameterlist.c decl.h
UtilNotGridDep.o : UtilNotGridDep.c decl.h
WorkWithlistdatavariable.o : WorkWithlistdatavariable.c decl.h
DiversListe.o : DiversListe.c decl.h
UtilAgrif.o : UtilAgrif.c decl.h
WorkWithAllocatelist.o : WorkWithAllocatelist.c decl.h
UtilCharacter.o : UtilCharacter.c decl.h
UtilListe.o : UtilListe.c decl.h
UtilFile.o : UtilFile.c decl.h
WorkWithlistofmodulebysubroutine.o : WorkWithlistofmodulebysubroutine.c decl.h
WorkWithlistmoduleinfile.o : WorkWithlistmoduleinfile.c decl.h
WorkWithlistofcoupled.o : WorkWithlistofcoupled.c decl.h

clean:
	$(MAKE) -f Makefile.lex clean
	$(RM) *.o conv
	
clean-all: clean
	$(MAKE) -f Makefile.lex clean-all
