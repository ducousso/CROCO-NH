#- Creation des elements relatifs a AGRIF (lib, config)
#---------------------------------------------------------------------
SHELL = /bin/sh
#---------------------------------------------------------------------

DIR_OBJS      = AGRIF_OBJS
DIR_FILES     = AGRIF_FILES
DIR_YOURFILES = AGRIF_YOURFILES

FILENAMES = 	modprocs modseq modbc modcluster modinit modinitvars modinterp modinterpbasic \
				modtypes modgrids modbcfunction modutil modcurgridfunctions \
				modmask modsauv modupdate modmpp \
				modupdatebasic modlinktomodel modarrays modvariables
           
OBJS=$(addsuffix .o,$(addprefix $(DIR_OBJS)/,$(FILENAMES)))
FILES=$(addsuffix .F90,$(addprefix $(DIR_FILES)/,$(FILENAMES)))

all: conv libagrif.a
	@echo
	@echo ===================================================
	@echo   AGRIF is OK
	@echo ===================================================
	@echo

.PHONY: conv

conv: 
	@echo
	@echo ===================================================
	@echo   Compile conv...
	@echo ===================================================
	$(MAKE) -C LIB conv
	mv -f LIB/conv .
	
libagrif.a : prep_lib $(OBJS)
	$(AR) -r $@ $(OBJS)
	ranlib $@

prep_lib:
	@echo
	@echo ===================================================
	@echo   Compile Agrif library...
	@echo ===================================================
	mkdir -p $(DIR_YOURFILES)
	mkdir -p $(DIR_OBJS)

$(DIR_OBJS)/%.o : $(DIR_FILES)/%.F90
	$(RM) $(DIR_YOURFILES)/$(*F).f90
	$(CPP) $(CPPFLAGS) $(DIR_FILES)/$(*F).F90 > $(DIR_YOURFILES)/$(*F).f90
	$(FC) $(FFLAGS) -I.. -c $(DIR_YOURFILES)/$(*F).f90 -o $(DIR_OBJS)/$(*F).o

$(DIR_OBJS)/modarrays.o: $(DIR_OBJS)/modgrids.o

$(DIR_OBJS)/modbc.o: $(DIR_OBJS)/modinterp.o

$(DIR_OBJS)/modbcfunction.o: $(DIR_OBJS)/modupdate.o 	\
							 $(DIR_OBJS)/modbc.o 		\
							 $(DIR_OBJS)/modinterp.o 	\
							 $(DIR_OBJS)/modtypes.o

$(DIR_OBJS)/modcluster.o: $(DIR_OBJS)/modtypes.o 		\
						  $(DIR_OBJS)/modlinktomodel.o 	\
						  $(DIR_OBJS)/modsauv.o 		\
						  $(DIR_OBJS)/modinitvars.o 	\
						  $(DIR_OBJS)/modseq.o			\
						  $(DIR_OBJS)/modcurgridfunctions.o

$(DIR_OBJS)/modcurgridfunctions.o: $(DIR_OBJS)/modinit.o \
								   $(DIR_OBJS)/modtypes.o

$(DIR_OBJS)/modinit.o: $(DIR_OBJS)/modlinktomodel.o \
					   $(DIR_OBJS)/modtypes.o \
					   $(DIR_OBJS)/modmpp.o

$(DIR_OBJS)/modinitvars.o: $(DIR_OBJS)/modlinktomodel.o \
						   $(DIR_OBJS)/modgrids.o

$(DIR_OBJS)/modinterpbasic.o: $(DIR_OBJS)/modtypes.o

$(DIR_OBJS)/modinterp.o: $(DIR_OBJS)/modcurgridfunctions.o \
						 $(DIR_OBJS)/modmask.o \
						 $(DIR_OBJS)/modarrays.o \
						 $(DIR_OBJS)/modmpp.o \
						 $(DIR_OBJS)/modinterpbasic.o

$(DIR_OBJS)/modlinktomodel.o: $(DIR_OBJS)/modgrids.o

$(DIR_OBJS)/modgrids.o: $(DIR_OBJS)/modtypes.o

$(DIR_OBJS)/modmask.o: $(DIR_OBJS)/modtypes.o

$(DIR_OBJS)/modsauv.o: $(DIR_OBJS)/modarrays.o $(DIR_OBJS)/modlinktomodel.o \
					   $(DIR_OBJS)/modtypes.o $(DIR_OBJS)/modvariables.o

$(DIR_OBJS)/modupdatebasic.o: $(DIR_OBJS)/modtypes.o

$(DIR_OBJS)/modupdate.o: $(DIR_OBJS)/modmask.o \
						 $(DIR_OBJS)/modcurgridfunctions.o \
						 $(DIR_OBJS)/modarrays.o \
						 $(DIR_OBJS)/modmpp.o \
						 $(DIR_OBJS)/modupdatebasic.o

$(DIR_OBJS)/modutil.o:	$(DIR_OBJS)/modtypes.o \
						$(DIR_OBJS)/modcurgridfunctions.o \
						$(DIR_OBJS)/modlinktomodel.o \
						$(DIR_OBJS)/modsauv.o \
						$(DIR_OBJS)/modcluster.o \
						$(DIR_OBJS)/modseq.o

$(DIR_OBJS)/modmpp.o:	$(DIR_OBJS)/modgrids.o \
						$(DIR_OBJS)/modarrays.o

$(DIR_OBJS)/modseq.o:	$(DIR_OBJS)/modinit.o \
						$(DIR_OBJS)/modarrays.o \
						$(DIR_OBJS)/modprocs.o

.PHONY: doc doc_user

doc:
	(cd doc ; doxygen Doxyfile )
	
doc_user:
	(cd doc ; doxygen Doxyfile_user )

clean: clean-conv
	$(RM) libagrif.a *.mod $(DIR_OBJS)/* $(DIR_YOURFILES)/*

clean-all: clean
	$(MAKE) -C LIB clean-all

clean-conv:
	$(MAKE) -C LIB clean
	$(RM) conv
