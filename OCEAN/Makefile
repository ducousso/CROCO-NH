# $Id: Makefile 1483 2014-03-15 17:05:10Z rblod $
#
#======================================================================
# CROCO is a branch of ROMS developped at IRD and INRIA, in France
# The two other branches from UCLA (Shchepetkin et al) 
# and Rutgers University (Arango et al) are under MIT/X style license.
# CROCO specific routines (nesting) are under CeCILL-C license.
# 
# CROCO website : http://www.croco-ocean.org
#======================================================================
#
# Universal machine independent makefile for ROMS model 
#
#======================================================================
# Set machine dependent definitions and rules.
#======================================================================

include Makedefs

#======================================================================
# Model Configuration:
#======================================================================
# SRCS: source codes files are sorted into groups, separated by
# blanc lines:
#   1) main driving part;	   2) 2D time stepping engine;
#   3) 3D time stepping engine;	   4) sea-water EOS routines;
#   5) vertical mixing schemes;    6) on-fly model diagnostic routines;
#   7) netCDF I/O routines;	   8) main forcing routines;
#   9) Wave forcing routines;	  10) Online surface forcing routines;
#  11) Floats routines;		  12) Stations routines;
#  13) biology/sediment routines; 14) PISCES routines
#  15) MPI routines;		  16) AGRIF routines;           
#  17) non-hydrostatic engine     18) OASIS coupling interface;
#
# SRC90: additional F90 routines for the non-hydrostatic engine
#======================================================================

 SRCS = main.F		step.F		read_inp.F\
	timers_roms.F	init_scalars.F	init_arrays.F	set_weights.F\
	set_scoord.F	ana_grid.F	setup_grid1.F	setup_grid2.F\
	set_nudgcof.F	ana_initial.F	analytical.F	zonavg.F\
\
	step2d.F	u2dbc.F		v2dbc.F		zetabc.F\
	obc_volcons.F\
\
	pre_step3d.F	step3d_t.F	step3d_uv1.F	step3d_uv2.F\
	prsgrd.F	rhs3d.F		set_depth.F	omega.F\
	uv3dmix.F	uv3dmix_spg.F	t3dmix.F	t3dmix_spg.F\
	hmix_coef.F	wetdry.F\
	u3dbc.F		v3dbc.F		t3dbc.F\
\
	rho_eos.F	ab_ratio.F	alfabeta.F\
\
	ana_vmix.F	bvf_mix.F	lmd_vmix.F 	gls_mixing.F\
	lmd_skpp.F	lmd_bkpp.F	lmd_swfrac.F	lmd_wscale.F\
	gls_prestep.F\
\
	diag.F		wvlcty.F	checkdims.F	grid_stiffness.F\
	bio_diag.F	setup_kwds.F    check_kwds.F	check_srcs.F\
	check_switches1.F		check_switches2.F\
	debug.F\
\
	output.F	put_global_atts.F\
	nf_fread.F	nf_fread_x.F	nf_fread_y.F	nf_read_bry.F\
	get_date.F	lenstr.F	closecdf.F	insert_node.F\
	fillvalue.F	nf_add_attribute.F		set_cycle.F\
	def_grid_2d.F   def_grid_3d.F   def_his.F       def_rst.F\
	def_diags.F	def_diagsM.F	def_bio_diags.F\
	wrt_grid.F      wrt_his.F       wrt_avg.F	wrt_rst.F\
	wrt_diags.F	wrt_diags_avg.F	wrt_diagsM.F	wrt_diagsM_avg.F\
	wrt_bio_diags.F	wrt_bio_diags_avg.F\
	set_avg.F	set_diags_avg.F	set_diagsM_avg.F\
	set_bio_diags_avg.F\
\
	get_grid.F	get_initial.F	get_vbc.F	get_wwave.F\
	get_tclima.F    get_uclima.F    get_ssh.F       get_sss.F\
	get_smflux.F    get_stflux.F    get_srflux.F    get_sst.F\
	get_tides.F     clm_tides.F     get_bulk.F      bulk_flux.F\
	get_bry.F       get_bry_bio.F	sstskin.F\
	get_psource.F   get_psource_ts.F\
\
	mrl_wci.F  	wkb_wwave.F 	wkbbc.F		get_bry_wkb.F\
\
	online_bulk_var.F		online_get_bulk.F\
	online_interp.F			online_interpolate_bulk.F\
	online_set_bulk.F\
\
	init_floats.F	wrt_floats.F	step_floats.F	rhs_floats.F\
	interp_rho.F	def_floats.F	init_arrays_floats.F\
	random_walk.F	get_initial_floats.F\
\
	init_sta.F	wrt_sta.F	step_sta.F	interp_sta.F\
	def_sta.F	init_arrays_sta.F\
\
	biology.F	o2sato.F	sediment.F	bbl.F\
\
	MPI_Setup.F	MessPass2D.F	MessPass3D.F	exchange.F\
	autotiling.F\
\
	zoom.F		update2D.F	set_nudgcof_fine.F\
	zoombc_2D.F	zoombc_3D.F	uv3dpremix.F\
	t3dpremix.F     update3D.F	Agrif2Model.F\
\
	init_xios.F	send_xios_diags.F\
\
	step3d_w.F	rhs3d_w_nh.F	w3dbc.F\
	unbq_bc.F	vnbq_bc.F	wnbq_bc.F	rnbq_bc.F\
\
	cpl_prism_define.F	cpl_prism_put.F 	cpl_prism_init.F\
	cpl_prism_get.F 	cpl_prism_getvar.F 	cpl_prism_getlistvals.F

 SRCS90 = \
	module_nh.F90		module_nbq.F90		module_qsort.F90\
	module_parallel_nbq.F90 module_tracetxt_out.F90	module_param.F90\
	nump_nh.F90		mat_cont_init_nh.F90	mat_cont_nh.F90\
	mat_mom_init_nh.F90	mat_mom_nh.F90		initial_nh.F90\
	grid_coef_nh.F90 	ru_nbq.F90		viscous_nbq.F90\
	step3d_nbq.F90		density_nbq.F90		spkitlocal_nh.F90\
	MPI_setup_nbq.F90	parallele_nbq.F90	output_nbq.F90\
	implicit_nbq.F90        amub2_tri.F90	        numuvw_nh.F90\
	grid_def_nh.F90		grid_exchange.F90	cons_nbq.F90\
	consout_nbq.F90\
\
	par_pisces.F90	ocean2pisces.F90	trc.F90		sms_pisces.F90\
	p4zche.F90	p4zint.F90		p4zlys.F90	p4zflx.F90\
	p4zlim.F90	p4zsink.F90		p4zmicro.F90	p4zmeso.F90\
	p4zmort.F90	p4zopt.F90		p4zprod.F90	p4zrem.F90\
	p4zsed.F90	p4zbio.F90		trcwri_pisces.F90\
	trcsms_pisces.F90			trcini_pisces.F90\
	pisces_ini.F90\
\
	module_oa_parameter.F90\
	module_oa_time.F90	module_oa_space.F90	module_oa_periode.F90\
	module_oa_variables.F90	module_oa_type.F90	module_oa_stock.F90\
	module_oa_level.F90	module_oa_interface.F90\
	module_oa_upd.F90	croco_oa.F90		var_oa.F90

AMRDIR = AGRIF/AGRIF_YOURFILES

#======================================================================

 RCS = $(SRCS:.F=.f)
 RCS90 = $(SRCS90:.F90=.f90)

 OBJS = $(RCS:.f=.o)
 OBJS90 = $(RCS90:.f90=.o)

 SBIN = croco

 AMRRCS=$(AMRSRCS:.F=.f)

 AMROBJS=$(AMRRCS:.f=.o)

#
# Eecutable file.
# ========= =====
#
$(SBIN):  $(OBJS90) $(OBJS)
	 $(LDR) $(FFLAGS) $(LDFLAGS) -o a.out $(OBJS90) $(OBJS) $(LCDF) $(LMPI) 
	mv a.out $(SBIN)
#
# Everything
# ==========
all: tools depend $(SBIN)

#
# Auxiliary utility programs and List of Dependecies:
# ========= ======= ======== === ==== == ============
#
  TOOLS = mpc cross_matrix cppcheck srcscheck checkkwds partit ncjoin ncrename

tools: $(TOOLS)

mpc: mpc.F
	$(CPP) -P $(CPPFLAGS) mpc.F > mpc_.f
	$(LDR) $(FFLAGS) $(LDFLAGS) -o mpc mpc_.f
cross_matrix: cross_matrix.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o cross_matrix cross_matrix.o
cppcheck: cppcheck.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o cppcheck cppcheck.o
srcscheck: srcscheck.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o srcscheck srcscheck.o
	rm -f check_srcs.F
checkkwds: checkkwds.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o checkkwds checkkwds.o
	rm -f setup_kwds.F
checkdefs: check_switches1.F setup_kwds.F

check_switches1.F: cppcheck cppdefs.h
	cat cppdefs.h cppdefs_dev.h > mergcpp.txt
	./cppcheck
check_srcs.F: srcscheck Makefile
	./srcscheck
setup_kwds.F: checkkwds read_inp.F
	./checkkwds
partit: partit.o insert_node.o lenstr.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o partit partit.o insert_node.o lenstr.o $(LCDF)
ncjoin: ncjoin.o lenstr.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o ncjoin ncjoin.o lenstr.o $(LCDF)
ncrename: ncrename.o lenstr.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o ncrename ncrename.o lenstr.o $(LCDF)

depend: checkdefs cross_matrix
	./cross_matrix *.F

mymodules: $(MOBJS) $(AMROBJS)
#
# Target to create tar file.
# ====== == ====== === =====
#
tarfile:
	tar cvf croco.tar Make* *.h *.F *.F90 *.in *.in.*
#
# Cleaning:
# =========
#
rmtools:
	/bin/rm -f $(TOOLS)
clean:
	/bin/rm -rf core *.o $(AMRDIR)/*.o *.i *.s *.f *.f90 *.trace *.mod ${COMP_FILES}

clobber: clean
	/bin/rm -rf $(SBIN) $(TOOLS) ./rii_files


plotter: plotter.F
	f77 -n32 -o plotter plotter.F $(LIBNCAR)

#
# Special treatment for barrier function:
# THERE SHALL BE NO OPTIMIZATION HERE!!!!
#
my_barrier.o: my_barrier.f
	$(CFT) -c -O0 my_barrier.f
#
# Include automatically generated dependency list:
#

include Make.depend

