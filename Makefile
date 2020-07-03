# $Id: Makefile,v 1.2 2003/09/02 14:46:08 dturner Release_ddt_1_13 dturner $


#  To add in any special RCS flags during the "make checkout" or 
#  "make checkin".  Do not add them here (in other words, leave this
#  blank), but add them in during the make command.  For example:
#	make RCSFLAG=-l checkin   - this does a make checkin but locks
#					each file as it checks it in.
RCSFLAG=

#  Add 'FortranLibs' to the following line if you are using Fortran
#  code in your procedure.  Ditto for the ECDSlibrary, EbuLibrary
#  and any other common library that you use.
LIBS= 

# The PROG macro contains the name of your procedure.  This lets us
# standardize most of the following targets, and it makes it easier to
# change the name of a procedure during development.
PROG = idl_lib

# Source and object file macros. 
INCFILES = 
SRCFILES = aeri_zerofill.pro boxplot_by_height.pro calc_p.pro dloadct.pro dpt2rh.pro \
	dpt2w.pro e2tvirt.pro e2w.pro edens.pro epres.pro esat.pro extract_subset.pro \
	hhmmss2hh.pro hypsometric.pro intersection.pro invplanck.pro matchtimes.pro \
	w2e.pro planck.pro pltaeri_qmeresids.pro pltaerisummary.pro pltmwr.pro \
	pltqmecloud_flag.pro pltsmos.pro ps_form.pro rh2dpt.pro rh2w.pro rho2dpt.pro \
	rho2rh.pro rrtm.pro rundecker.pro systime2ymdhms.pro tvirt.pro ui2jt.pro \
	w2rh.pro w2tvirt.pro wdel.pro woplot.pro wplot.pro wsat.pro \
	ymdhms2systime.pro zt2hhmmss.pro zt2julian.pro idl_tools_version.pro \
	colors1.tbl t2density.pro rh2pwv.pro w2pwv.pro systime2julian.pro \
	ymdhms2julian.pro ymd2dow.pro julian2ymdhms.pro dcd.pro dpushd.pro \
	get_datastream_version.pro find_glatt.pro find_flatt.pro \
	rh2rho.pro w2rho.pro rho2w.pro plt_tape27.pro check_data.pro \
	t_zt2hhmmss.pro t_zt2julian.pro bessel.pro qmeaerilbl_compute_integral.pro \
	saveimage.pro draw.pro lbl_read.pro dabort.pro read_tape7.pro write_tape7.pro \
	pca_expand.pro pca_generate_pcs.pro pca_project.pro hypsometric2.pro \
	apodize_kaiser_bessel.pro apodize_norton_beer.pro apodizer.pro pie_chart.pro \
	gamma_distribution.pro get_scat_properties.pro  read_scat_databases.pro \
	convolve_to_aeri.pro numdens2od.pro od2numdens.pro dcontour.pro radxfer.pro \
	get_aeri_bb_emis.pro aeri_recal.pro aeri_calaccuracy.pro plot_histogram.pro \
	get_quicklook_dir.pro inv_hypsometric.pro increment_date.pro dmedian.pro \
	get_sgp_lse.pro check_aeri_vlaser.pro fix_aeri_vlaser.pro dsign.pro \
	plot_mmcr0.pro plot_mmcr.pro compute_reflectivity.pro compute_adiabatic_lwp.pro \
	lookup_reflectivity.pro generate_lookup_reflectivity_table.pro read_lbldis.pro \
	rad2flux.pro latlon_to_distance.pro pca_ind.pro stackplot.pro aeri2irt.pro \
	read_toms.pro mmcr_z_iwc.pro doplot_letter.pro investigate_ssp.pro \
	gauss_quad.pro clear.pro isstruc.pro ncdf_varexist.pro fix_lat_lon_alt.pro \
	is_ncdf.pro read_ncdf.pro plot_vceil.pro plot_wacr.pro ncdf_subset.pro \
	check_qc_bits.pro check_aeri_vlaser_mod.pro fix_aeri_vlaser_mod.pro \
	write_arm_sonde_file.pro read_aeri_dmv_1d.pro read_aeri_dmv_2d.pro \
	irspectrum2irt.pro tape13_to_transmittances.pro ncdf_find_field_nans.pro \
	smooth_aeri.pro plot_skewt.pro wind_barb.pro

OBJFILES = 

# Info files.
INFO = 

# This macro holds any auxilliary files that that your code needs to run,
# such as calibrations or regression coefficients.  These files should be
# under RCS, and should have the $(PROG) name attached to them so we can
# figure out who belonsgs to what in the VIP_INFO directory.
# An example:
# AUX = $(PROG)_retcoef.clear.wpl $(PROG)_retcoef.cloudy.wpl \
#                 $(PROG)_tmr.wpl
AUX = qmeaerilbl.mapping

# Documentation files: NOTIFICATION forms, READMEs, etc.  All these
# things will be checked into RCS, too, and should have the $(PROG) name 
# attached to them in some way so we can tell them apart when they get
# installed into VIP_DOCS.

DOCS = 

# This macro is for everything that goes into RCS.  It helps with the 
# checkout and checkin and release targets

RCSFILES = $(SRCFILES) $(INCFILES) $(INFO) $(AUX) $(DOCS) Makefile

# For most single procedures, you shouldn't have to change anything else 
# after this, but go ahead and check all the targets to see if they act  
# the way you want them to.  If you are doing something dangerous and
# funky like having more than one procedure in a Makefile, then you
# probably will have to modify or duplicate some of the targets, and you
# will probably end up with somebody mad at you because of it.
#
# Finally, the targets.  The very first target MUST be for the primary
# executable; if you have more than one, put an 'all' target first that
# depends on all the executables.  This is so that you only have to type
# 'make' with no arguments to compile and link everything you need.

all: $(PROG) 

$(PROG): 
	@echo "This is an IDL repository.  It does not make"

# The Release_% target attaches an overall version label to all your files
# by using the symbolic name and state features of RCS.  This will happen
# when your code is released by the 'vip' user; in other words, you will
# probably never have to use this target yourself.  The target name given
# is the version label - so 'make Release_2_3' will use 'Release_2_3' as
# the the version label. This label gets attached to all the files in
# RCSFILES; if you have more than one procedure in this makefile you will
# probably have to do something more complicated to keep from attaching
# the same version label to all of them when you release one of them.
# Only a truly devoted individual should attempt to mess with this target.

Release_%:
	-$(MAKE) checkin
	-@FOUND=0 ; \
	RCSdir=RCS/ ; RCSdir=$$RCSdir* ; \
	for FILE in $$RCSdir ; do \
	 { foo=`rlog -h $$FILE | grep $@ | awk '{ print $$1 }'` ; \
	   if (test $$foo ) then FOUND=1 ; fi ; } ; \
	done; \
	if (test $$FOUND -eq 0) then \
	  { rcs -s$@ -n$@:$$ $(RCSFILES); \
	    $(MAKE) checkout ; \
	    echo "Done tagging release $@.  Continue with release" ; } ; \
	else echo \
	 "Release number $@ has already been used.  Select another" ; fi ;

# The 'release' target should place the executables in VIP_BIN, the
# documentation files into VIP_DOCS, and the info and auxilliary files in
# VIP_INFO.  This means that you should make sure your code reads aux files
# from VIP_INFO.  'install' and 'release' are synonymous targets.

install: release

release: release_vap

# This target is for VAPs.  
release_vap: 
	ginstall -c -m 444 $(SRCFILES) $(IDLTOOLS_HOME)/lib
	ginstall -c -m 444 $(AUX) $(VAP_HOME)/info

# The purpose of the 'tar' target is to create a tar file that contains
# everything we need to bundle up your code and move it to the production
# machine.  That includes source files, info files, aux files, makefiles,
# NOTIFICATION forms, and the testing subdirectories.  Any READMEs or
# other documentation should go as well.
tar: 
	tar cf $(PROG).tar $(RCSFILES) Makefile test 

# The purpose of this target is to create a temporary file that contains 
# the output of the ident RCS command.  This command looks in each file
# specified, and outputs any RCS Id identifiers that it finds.  This will aid
# the user as he/she attempts to keep the version numbers of the source 
# files in the NOTIFICATION file correct.
get_ids:
	ident $(RCSFILES) | grep "Id:" > version_list.foo

# This target is useful for making sure that everything is checked in
# at once; just do a 'make checkin' and be ready to write log updates
# for each file.  If the file has already been checked in nothing will
# happen to it, so this target is safe.
checkin:
	ci -t- -u $(RCSFLAG) $(RCSFILES)

# This checks out the most recent version of everything.  Useful after
# a "make clean"
checkout:
	co -u $(RCSFLAG) $(RCSFILES)

setrcs:
	rcs -c" * " $(SRCFILES) 
	rcs -c"# " $(INFO) $(AUX) $(DOCS)
	rcs -c"# " Makefile

# Very important to have a 'clean' target which gets rid of the junk and
# object files in your directory.  It should also do an `rcsclean', which
# deletes all checked out source files that are not different than the
# most recent version.  After doing a 'make clean', a 'make checkout' will
# check out the most recent version of the code, and a 'make' should
# recompile and link every file in this source directory, which insures
# that everything is up to date.
clean:
	rm -f *~ $(OBJFILES) Makefile.bak *.% $(PROG) 

rcsclean:
	make clean
	rcsclean 
