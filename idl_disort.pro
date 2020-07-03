; $Id: idl_disort.pro,v 1.5 2009/03/11 17:32:08 dturner Exp $




; The main routine "idl_disort()" is below, to allow the other needed routines
; to compile first...




;_++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; This routine integrates radiance over a (small) spectral band using simple
; trapezoidal integration.
function integrate_radiance, wnum, rad, wnum1, wnum2
  	; First, linearly interpolate the input radiance to a wavenumber grid that
	; includes the points we desire (wnum1,wnum2).  
  tmp = [wnum, wnum1, wnum2]
  srt = sort(tmp)
  tmp = tmp(srt)
  twnum = tmp(uniq(tmp))
  trad  = interpol(rad,wnum,twnum)
  	; Now we perform the integration
  foo = where(wnum1 le twnum and twnum le wnum2, nfoo)
  integral = int_tabulated(twnum(foo),trad(foo))
  
  return, integral
end
;_++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Determines if the input variable is the proper structure containing the 
; single scattering properties (ssp).  Returns a zero (0) if the variable is
; ok, otherwise a positive number will be returned.
function check_sspdb, sspdb
  reread_ssp = 0
  if(size(sspdb,/type) ne 8) then reread_ssp = 1 $
  else begin
    dtags = ['u_wnum','u_reff','wnum','reff','xsext','xsabs','w0','g', $
    		'Qext','Qabs','filename']
    tags = tag_names(sspdb)
    if(n_elements(tags) ne n_elements(dtags)) then reread_ssp = 2 $
    else begin
      for i=0,n_elements(dtags)-1 do $
        if(strlowcase(dtags(i)) ne strlowcase(tags(i))) then reread_ssp = 3
      if(reread_ssp eq 0) then begin
        if(n_elements(sspdb.wnum) ne n_elements(sspdb.reff) or $
	   n_elements(sspdb.wnum) ne n_elements(sspdb.xsext) or $
	   n_elements(sspdb.wnum) ne n_elements(sspdb.xsabs) or $
	   n_elements(sspdb.wnum) ne n_elements(sspdb.w0) or $
	   n_elements(sspdb.wnum) ne n_elements(sspdb.g) or $
	   n_elements(sspdb.wnum) ne n_elements(sspdb.Qext) or $
	   n_elements(sspdb.wnum) ne n_elements(sspdb.Qabs) or $
	   n_elements(sspdb.wnum) ne $
	   	n_elements(sspdb.u_wnum)*n_elements(sspdb.u_reff)) then reread_ssp = 4
      endif
    endelse
  endelse
  return,reread_ssp
end
;_++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; This function reads in the SSP database file
function read_ssp_file, filename
    openr,lun,filename,/get_lun
    header = replicate(' ',7)
    readf,lun,header
    nlines = long(header(2))
    nphase = long(header(3))
    ncols  = 13
    data = dblarr(ncols+nphase,nlines)
    readf,lun,data
    free_lun,lun
        
        ; Capture this data
    wnum   = reform(data(1,*))
    reff   = reform(data(2,*))
    xsext  = reform(data(3,*))
    xsabs  = reform(data(5,*))
    w0     = reform(data(6,*))
    g      = reform(data(7,*))
    Qext   = reform(data(8,*))
    Qabs   = reform(data(9,*))
        
        ; Find the unique wavenumbers
    s_wnum = wnum(sort(wnum))
    u_wnum = s_wnum(uniq(s_wnum))
        ; Find the unique reff
    s_reff = reff(sort(reff))
    u_reff = s_reff(uniq(s_reff))

    	; Get the filename of the SSP database, for documentation purposes
 	; But strip off the path first...
    parts = str_sep(filename, '/')
    xfile = parts(n_elements(parts)-1)

    return,{u_wnum:u_wnum, u_reff:u_reff, wnum:wnum, reff:reff, $
	xsext:xsext, xsabs:xsabs, w0:w0, g:g, Qext:Qext, Qabs:Qabs, $
	filename:xfile}
end
;_++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Determines if the input variable is the proper structure containing the 
; solar angles, times, date, etc.  Returns a zero (0) if the variable is
; ok, otherwise a positive number will be returned.
function check_solar, solar
  reread = 0
  if(size(solar,/type) ne 8) then reread = 1 $
  else begin
    dtags = ['year','month','day','hour','latitude','longitude']
    tags = tag_names(solar)
    if(n_elements(tags) ne n_elements(dtags)) then reread = 2 $
    else begin
      for i=0,n_elements(dtags)-1 do $
        if(strlowcase(dtags(i)) ne strlowcase(tags(i))) then reread = 3
      if(reread eq 0) then begin
        if(n_elements(solar.year) ne 1 or $
	   n_elements(solar.month) ne 1 or $
	   n_elements(solar.day) ne 1 or $
	   n_elements(solar.hour) ne 1 or $
	   n_elements(solar.latitude) ne 1 or $
	   n_elements(solar.longitude) ne 1) then reread = 4
      endif
      if(reread eq 0) then begin
        if(solar.latitude ge 90 or solar.latitude le -90) then reread = 5
        if(solar.longitude ge 180 or solar.longitude le -180) then reread = 5
        if(solar.hour lt 0 or solar.hour gt 24) then reread = 5
	if(solar.day lt 1 or solar.day gt 31) then reread = 5
	if(solar.month lt 1 or solar.month gt 12) then reread = 5
      endif
    endelse
  endelse
  return,reread
end
;_++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Determines if the input variable is the proper structure containing the 
; solar source function (ssf).  Returns a zero (0) if the variable is
; ok, otherwise a positive number will be returned.
function check_ssfdb, ssfdb
  reread_ssf = 0
  if(size(ssfdb,/type) ne 8) then reread_ssf = 1 $
  else begin
    dtags = ['wnum','rad','rad_units','filename']
    tags = tag_names(ssfdb)
    if(n_elements(tags) ne n_elements(dtags)) then reread_ssf = 2 $
    else begin
      for i=0,n_elements(dtags)-1 do $
        if(strlowcase(dtags(i)) ne strlowcase(tags(i))) then reread_ssf = 3
      if(reread_ssf eq 0) then begin
        if(n_elements(ssfdb.wnum) ne n_elements(ssfdb.rad)) then reread_ssf = 4
      endif
    endelse
  endelse
  return,reread_ssf
end
;_++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; This function reads in the solar source function file
function read_ssf_file, filename
  openr,lun,filename,/get_lun
  header = replicate('',22)
  readf,lun,header
  nlines = long(header(n_elements(header)-1))
  data = fltarr(3,nlines)
  readf,lun,data
  free_lun,lun

    	; Get the filename of the SSF database, for documentation purposes
 	; But strip off the path first...
    parts = str_sep(filename, '/')
    xfile = parts(n_elements(parts)-1)

  return,{wnum:reform(data(0,*)), rad:reform(data(1,*)), $
  		rad_units:'mW / (m2 cm-1)', filename:xfile}
end
;_++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; This function extracts the desired single scattering property from the SSP database
; by interpolating in wavenumber and then effective radius.
function get_ssp_value, sspdb, wnum, reff, flag, result

  if(reff lt min(sspdb.u_reff) or reff gt max(sspdb.u_reff)) then begin
    print,'Error in get_ssp_value: Effective radius outside range in SSP DB'
    return,1
  endif

  if(flag eq 'w0') then data = sspdb.w0 $
  else if(flag eq 'g') then data = sspdb.g $
  else if(flag eq 'Qext') then data = sspdb.Qext $
  else if(flag eq 'Qabs') then data = sspdb.Qabs $
  else begin
    print,'Error in get_ssp_value: undefined flag'
    return,1
  endelse

  	; Extract the spectra at the bounding wavenumbers
  foo1 = where(sspdb.u_wnum le wnum, nfoo1)
  foo2 = where(wnum lt sspdb.u_wnum, nfoo2)
  if(nfoo1 eq 0 or nfoo2 eq 0) then begin
    print,'Error in get_ssp_value: Outside of the wavenumber limits of SSP DB'
    return,1
  endif
  bar = where(sspdb.wnum eq sspdb.u_wnum(foo1(nfoo1-1)))
  datal = data(bar)
  bar = where(sspdb.wnum eq sspdb.u_wnum(foo2(0)))
  datah = data(bar)
  if(n_elements(datah) ne n_elements(sspdb.u_reff) or $
     n_elements(datah) ne n_elements(sspdb.u_reff)) then begin
    print,'Error in get_ssp_value: Logic error and arrays are not matching up'
    return,1
  endif

  	; Interpolate these data to a common wavenumber grid
  data = fltarr(n_elements(sspdb.u_reff))
  for i=0,n_elements(sspdb.u_reff)-1 do data(i) = interpol([datal(i),datah(i)], $
  	[sspdb.u_wnum(foo1(nfoo1-1)),sspdb.u_wnum(foo2(0))], wnum)
  result = interpol(data, sspdb.u_reff, reff)
  return,0
end

;_++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function get_idl_disort_version
  version = '$Id: idl_disort.pro,v 1.5 2009/03/11 17:32:08 dturner Exp $'
  return,version
end

;_++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
; $Id: idl_disort.pro,v 1.5 2009/03/11 17:32:08 dturner Exp $
;
; Abstract: ;	This routine is written to call DISORT via my "LBLDIS" interface.  It
;    requires that LBLDIS has been compiled with "make idl" so that the shared
;    object (.so file) was created.  
;
; Author:
;	Dave Turner
;	SSEC / University of Wisconsin - Madison
; 	dturner@ssec.wisc.edu
;
; Date implemented:
;	May 2008
;
; Notes:
;	If the LBLDIS code is recompiled, IDL may need to be restarted in order
;    to use the new .so shared object.  I don't know why this is the case, but
;    it seems to be true.
;
;	I am not allowing the treatment of real phase functions in this 
;    implementation, as they are not correctly implemented in the IDL interface
;    of LBLDIS.  Therefore, you will always be forced to use Henyey-Greenstein
;    phase functions derived from the asymmetry parameter.
;
; Call:
  function idl_disort, $	; Returns 0 is successful, otherwise there was a failure
		; Input arguments
  	wnum1, $ 		; The wavenumber at beginning of the interval [cm-1]
  	wnum2, $ 		; The wavenumber at   ending  of the interval [cm-1]
	nlay, $ 		; The number of layers
	prof_ht, $ 		; The height array (nlay+1), from surface to TOA [km AGL]
	prof_pres, $		; The pressure array (nlay+1) [mb]
	prof_temp, $		; The temperature array (nlay+1) [K]
	prof_gasod, $		; The gaseous optical depth array (nlay) [unitless]
	cld_ht, $		; The height of the cloud [km AGL].  For each cloud,
				;    the cloud will be uniformly distributed into
				;    the entire layer that contains this height.
	cld_od, $		; The optical depth of the clouds (could be an array).
				;    This needs to be in the geometrical limit.  Note
				;    that if an array is sent in, these calculations
				;    are being performed for the same cloud boundaries.
				;    Entering zero is effectively no cloud.  Note that 
				;    if there are multiple cloud heights, then this 
				;    variable should be a 2-dimension matrix.
        cld_reff, $		; The effective radius of the cloud particles [microns]
				;    for each cloud in "cld_ht".
	cld_phase, $		; An array of indices (same size as "cld_ht") that 
				;    indicates which SSP DB to use.  A cloud phase
				;    of zero is the first SSB DB...

		; Additional input as keywords 
	cb_temp=cb_temp, $	; The temperature of the cloud base [K].  Size of the
				;     array must match "cld_ht", if provided
	ct_temp=ct_temp, $	; The temperature of the cloud top  [K].  Size of the
				;     array must match "cld_ht", if provided
 	zangle=zangle, $	; The zenith angle [deg] of the calc (180 is downwelling)
	sspdb=sspdb, $		; The single scatttering property database information,
				;     inserted into an array of "pointers".  If this
				;     variable is not defined, then this code will 
				;     read in the data from the file "sspfile" and 
				;     populate this variable.  
	sspfile=sspfile, $	; If "sspdb" is not properly defined, then this 
				;     file will be read to get the SSP data.  Again,
				;     this is an array of file names.  It must have
				;     the same number of entries as defined in the
				;     variable "cld_phase" above.
	reread_ssp=reread_ssp, $; If set, then reread in the SSP DBs, regardless if
				;     if the variable "sspdb" is already defined or not
	sfc_temp=sfc_temp, $	; The temperature of the surface [K].  If not provided,
				;     then the lowest level from the profile is used
				;     as the surface temperature.
	sfc_emis=sfc_emis, $	; The emissivity of the surface (assumed lambertian)
        verbose=verbose, $	; How noisy the DISORT calc should be, with the values
				;     ranging from 0->quiet to 3->very noisy
	solver=solver, $	; Which RT solver to use 
				;		0 -> DISORT, -1 -> Absorption only
	solar=solar, $		; Set this to the following structure, if the solar
				;     contribution should be included in the calc:
				;     {year, month, day, hour, latitude, longitude}, 
				;     where the time is in UTC, latitude is in deg North,
				;     and longitude is in degrees East.  The year 
				;     should be in 4 digit format (YYYY) as an integer,
				;     and month (1->Jan) and day are integers, and 
				;     the hour can be a fraction between 0 and 24.  
	ssfdb=ssfdb, $		; This variable is like "sspdb" above.  If it is not
				;     defined or improperly defined, then this routine
				;     will read the data in again.  This structure
				;     contains the solar source function.
	ssffile=ssffile, $	; The solar source filename (like "sspfile" above)
	lbldis_dir=lbldis_dir,$	; Location of the disort_ramp_sensitivity.so file

		; The output arguments
	radiance, $		; The radiance [mW / (m2 sr cm-1)].  The size of the
				;     output is the same dimensions as cld_od.
		; The output keywords
	flux_up=flux_up, $	; The upwelling flux [mW / (m2 cm-1)] at this height
	flux_down=flux_down, $ 	; The downwelling flux [mW / (m2 cm-1)] at this height
	flux_beam=flux_beam 	; The direct beam flux [mW / (m2 cm-1)] at this height

	; The default values for the keywords
  if(n_elements(zangle) eq 0) then   zangle = 180.
  if(n_elements(sfc_temp) eq 0) then sfc_temp = prof_temp(0)
  if(n_elements(sfc_emis) eq 0) then sfc_emis = 0.985
  if(n_elements(verbose) eq 0) then  verbose = 0
  if(n_elements(solver) eq 0) then   solver = 0
  if(n_elements(sspfile) eq 0) then  $
  	sspfile='/home/dturner/vip/src/radiation/newmie/ssp_db.mie_wat.gamma_sigma_0p100'
  if(n_elements(ssffile) eq 0) then  $
  	ssffile='/home/dturner/vip/src/radiation/solar_spectrum/data/solar.kurucz.rad.1cm-1binned.full_disk.asc'
  if(n_elements(lbldis_dir) eq 0) then  $
        lbldis_dir = '/home/dturner/vip/src/radiation/lblrtm_disort'
;- 

	;_+++++++
	; Perform some basic QC to make sure the input arguments are valid
	; I'm sure that many more tests can be implemented.  Just set the 'error'
	; string to the error message give, and then check it after all of the tests
	; have been performed.  This makes the code a bit easier to write...
  error = ''
  if(wnum1 ge wnum2) then error = 'Incorrect wavenumber range'
  if((n_elements(prof_ht) ne nlay+1 or $
      n_elements(prof_temp) ne nlay+1 or $
      n_elements(prof_pres) ne nlay+1) and error ne '') then $
  	error = 'Improper specification of the height/temperature/pressure profiles'
  if(n_elements(prof_gasod) ne nlay and error ne '') then $
        error = 'Improper specification of the gaseous optical depth profile'
  if(zangle lt 0 or zangle gt 180) then $
        error = 'Improper specification of the (viewing) zenith angle'
  if(sfc_emis lt 0 or sfc_emis gt 1) then $
        error = 'Improper specification of the surface emissivity'
  del = prof_ht(1:nlay)-prof_ht(0:nlay-1)
  foo = where(del le 0, nfoo)
  if(nfoo gt 0) then error = 'Height array must be monotonically ascending (SFC to TOA)'
  foo = where(cld_od lt 0, nfoo)
  if(nfoo gt 0) then error = 'Cloud optical depths can not be negative'

  ncld = long(n_elements(cld_ht))
  if(ncld lt 1) then error = 'Code requires at least 1 "cloud" layer '+$
  		'(but it could have optical depth of zero)'
  if(n_elements(cld_reff) ne ncld) then $
        error = 'Cloud effective radius (cld_reff) must have the same dimension as cld_ht'
  if(n_elements(cld_phase) ne ncld) then $
        error = 'Cloud phase (cld_phase) must have the same dimension as cld_ht'
  size_cld_od = size(cld_od)
  if(size_cld_od(0) eq 1 and size_cld_od(1) ne ncld) then $
        error = 'Cloud optical depth (cld_od) must have the same dimension as cld_ht' $
  else if(size_cld_od(0) eq 2 and size_cld_od(1) ne ncld) then $
        error = 'Cloud optical depth (cld_od) must have dimension [ncld, ntau] (1) ' $
  else if(size_cld_od(0) ne 1 and size_cld_od(0) ne 2) then $
        error = 'Cloud optical depth (cld_od) must have dimension [ncld, ntau] (2) '

  if(n_elements(cb_temp) gt 0 and n_elements(cb_temp) ne ncld) then $
        error = 'Cloud base temperature (cb_temp) must have same dimension as cld_ht'
  if(n_elements(ct_temp) gt 0 and n_elements(ct_temp) ne ncld) then $
        error = 'Cloud top temperature (ct_temp) must have same dimension as cld_ht'

  if(n_elements(sspdb) eq 0) then sspdb = ptrarr(n_elements(sspfile),/allocate)

  if(n_elements(sspdb) ne n_elements(sspfile) and not keyword_set(reread_ssp)) then $
        error = 'The number of elements in sspdb must equal the '+$
		'number of strings in sspfile.  Please delete the variable sspdb'

  if(error ne '') then begin
    print,'idl_lbldis error: '+error
    return,1
  endif

	; If reread_ssp is set, then free up memory before the action below
  if(keyword_set(reread_ssp)) then begin
    for i=0,n_elements(sspdb)-1 do begin
      if(ptr_valid(sspdb(i))) then ptr_free, sspdb(i)
    endfor
    sspdb = ptrarr(n_elements(sspfile),/allocate)
  endif

	; Check to see if the "sspdb" dataset is properly defined.  If not, then
	; read in the SSP data from "sspfile"
  for i=0,n_elements(sspdb)-1 do begin
    ssp_not_ok = check_sspdb(*sspdb(i))
    if(ssp_not_ok eq 1) then begin
      files = file_search(sspfile(i), count=count)
      if(count ne 1) then begin
        print,'Error: Unable to unambiguously determine '+sspfile(i)
        return,1
      endif
      		; If this is a valid pointer, let's free up its memory
		; before we reassign a value to it
      if(ptr_valid(sspdb(i))) then ptr_free, sspdb(i)
      sspdb(i) = ptr_new(1,/allocate)
      		; Assign the new datastructure to this pointer
      print,'    Reading in '+files(0)
      *sspdb(i) = read_ssp_file(files(0))
    endif
  endfor

  	; A few more error checks
  if(keyword_set(reread_ssp)) then begin
    if(min(cld_phase) lt 0) then error = 'Cld_phase must be a non-negative integer'
    if(n_elements(sspdb) le max(cld_phase)) then $
  	error = 'Undefined phase -- no matching sspdb entry'
  endif

	; If the solar keyword is added, make sure its format is correct, and
	; if so, read in the information needed.  Otherwise, set the variables
	; associated with the solar function to zeros.
  phi0  = 0.		; Relative azimuth, in degrees
  fisot = 0.		; Isotropic flux, in W/m2
  if(size(solar,/type) ne 8) then begin
    fbeam = 0.
    umu0  = 0.
  endif else begin
    if(check_solar(solar) eq 0) then begin
      		; Make sure the SSF information is read in correctly, else read it
		; in again.
      if(check_ssfdb(ssfdb) ne 0) then begin
	files = file_search(ssffile, count=count)
        if(count ne 1) then begin
          print,'Error: Unable to unambiguously determine '+ssffile
          return,1
        endif
        ssfdb = read_ssf_file(files(0))
      endif
		; Get the solar altitude and the earth/sun distance
      if(solar.hour lt 0 or solar.hour gt 24) then begin
        print,'Error: The hour in the "solar" structure is ill-defined'
        return,1
      endif
      hh = fix(solar.hour)
      ns = (solar.hour - hh)*60.
      nn = fix(ns)
      ss = (ns - nn)*60.
      solarpos, fix(solar.year+0.5), fix(solar.month+0.5), fix(solar.day+0.5), $
      		hh, nn, ss, solar.latitude, solar.longitude, $
		tmp, ra, sd, salt, rc, sa, esd

		; If the solar elevation is more than 5 degrees above the horizon
		; then we will include it in the calculation; otherwise not
      if(salt(0) lt 5) then begin
        fbeam = 0.
        umu0  = 0.
      endif else begin
      		; Integrate the solar radiance over the desired wavenumber range
		; Then normalize it by the earth-sun distance, and finally
		; convert the units from [mW / (m2 cm-1)] to [W / m2]
        fbeam = integrate_radiance(ssfdb.wnum, ssfdb.rad, wnum1, wnum2)
        fbeam = fbeam / (esd(0) * esd(0))
        fbeam = fbeam * (wnum2 - wnum1)
        fbeam = fbeam / 1000.
      	
		; Specify the other solar parameters
        umu0 = cos((90 - salt(0))/180.*!pi)
      endelse
    endif else begin
      print,'Error: The structure of the solar keyword is bad'
      return,1
    endelse
  endelse

	; Extract out the cloud properties from the SSP databases for all 
	; of the clouds in the profile.  Note that the properties I desire 
	; depend on the cloud "phase" (i.e., which SSP DB to read from)
		; Get the single scatter albedo, asymmetry parameter, and Qext
  cwnum = mean([wnum1,wnum2])
  cld_w0 = fltarr(ncld)
  cld_g  = fltarr(ncld)
  cld_Qe = fltarr(ncld)
  cld_Qa = fltarr(ncld)
  for i=0,ncld-1 do begin
    flag = get_ssp_value(*sspdb(cld_phase(i)), cwnum, cld_reff(i), 'w0', tmp)
    if(flag ne 0) then return,1
    cld_w0(i) = tmp
    flag  = get_ssp_value(*sspdb(cld_phase(i)), cwnum, cld_reff(i), 'g', tmp)
    if(flag ne 0) then return,1
    cld_g(i) = tmp
    flag = get_ssp_value(*sspdb(cld_phase(i)), cwnum, cld_reff(i), 'Qext', tmp)
    if(flag ne 0) then return,1
    cld_Qe(i) = tmp
    flag = get_ssp_value(*sspdb(cld_phase(i)), cwnum, cld_reff(i), 'Qabs', tmp)
    if(flag ne 0) then return,1
    cld_Qa(i) = tmp
  endfor

	; Change the cloud optical depth from geometric limit to the desired 
	; wavelength.  If we are in 'absorption only' mode, then we need to 
	; use the absorption efficiency for this conversion (and set the single
	; scatter albedo to zero), otherwise use the extinction efficiency. Note
	; that our assumption that we are in the geometric limit is why we are
	; normalizing by the factor of 2.   Finally, I am changing the name of 
	; the optical depth variable here so that I don't overwrite anything
	; important (since IDL passes variables by address).
  cld_tau = float(cld_od) * 0.
  if(solver eq -1) then begin
    for i=0,ncld-1 do begin
      cld_tau(i,*) = cld_od(i,*) * cld_Qa(i) / 2.
      cld_w0(i)  = cld_w0(i) * 0.
    endfor
  endif else begin
    for i=0,ncld-1 do begin
      cld_tau(i,*) = cld_od(i,*) * cld_Qe(i) / 2.
    endfor
  endelse

	; Make sure these cloud properties are all of the correct type
  cld_w0  = float(cld_w0)
  cld_g   = float(cld_g)
  cld_tau = float(cld_tau)
  cld_re  = float(cld_reff)

  nopt_depths = [long(n_elements(cld_tau(0,*)))]

  	; For the meantime, I will not allow real phase functions as I need to 
	; modify disort_ramp_sensitivity to pass in doubles (cld_pf).  So I will
	; only use Henyey-Greenstein phase functions
  use_henyey_greenstein = 1L
  nphase = [long(5)]				; I'm making an imaginary phase function
  pf_angle = fltarr(ncld, nphase)		; Will not put anything in here
  cld_pf   = fltarr(ncld, nphase)		; Will not put anything in here
  pf_angle = fltarr(5, 5)		; Will not put anything in here
  cld_pf   = fltarr(5, 5)		; Will not put anything in here

	; Create the modified temperature profile that has the cloud base and cloud
	; top temperatures correct, just like I'm doing in MIXCRA.
  mod_prof_temp = prof_temp
  for i=0,ncld-1 do begin
    bar = where(cld_ht(i) lt prof_ht, nbar)
    if(nbar eq 0) then begin
      print,'Error: Cloud height above maximum profile height'
      return,1
    endif
    if(bar(0) eq 0) then begin
      print,'Error: Cloud height below minimum profile height'
      return,1
    endif
    if(n_elements(cb_temp) eq ncld) then begin
      if(cb_temp(i) lt 50 or cb_temp(i) gt 350) then begin
        print,'Error: The cloud base temperature seems unrealistic'
	return,1
      endif
      mod_prof_temp(bar(0)-1) = cb_temp(i)
    endif
    if(n_elements(ct_temp) eq ncld) then begin
      if(ct_temp(i) lt 50 or ct_temp(i) gt 350) then begin
        print,'Error: The cloud top temperature seems unrealistic'
	return,1
      endif
      mod_prof_temp(bar(0))   = ct_temp(i)
    endif 
  endfor

	; I had the user enter the profiles from surface to TOA in the call
	; statement, but DISORT really wants them from TOA to surface, so 
	; I will reverse them here.
  profile_ht    = float(reverse(prof_ht))
  profile_temp  = float(reverse(mod_prof_temp))
  profile_pres  = float(reverse(prof_pres))
  profile_gasod = float(reverse(prof_gasod))

	; A few other minor points
  which_solver = long(solver)
  debug        = long(verbose)
  nstreams     = 16L
  radiance     = fltarr(nopt_depths)*0-1
  flux_up      = fltarr(nopt_depths)*0-1
  flux_down    = fltarr(nopt_depths)*0-1
  flux_beam    = fltarr(nopt_depths)*0-1

  	; Make sure some of the data types are correct (especially those that come
	; directly from the user via input argument/keywords)
  nlay     = long(nlay)
  wnum1    = float(wnum1)
  wnum2    = float(wnum2)
  zangle   = float(zangle)
  sfc_temp = float(sfc_temp)
  sfc_emis = float(sfc_emis)
  cld_ht   = float(cld_ht)

	; Now make the actual call to DISORT
  pushd,lbldis_dir
  flag = call_external('disort_ramp_sensitivity.so','disort_ramp_sensitivity', $
	fbeam, umu0, phi0, fisot, wnum1, wnum2, nlay, zangle, sfc_temp, sfc_emis, $
	nopt_depths, ncld, cld_tau, cld_w0, cld_g, nphase, pf_angle, cld_pf, $
	cld_re, cld_ht, profile_ht, profile_pres, profile_temp, profile_gasod, $
	nstreams, radiance, flux_up, flux_down, flux_beam, $
	debug, use_henyey_greenstein, which_solver)
  popd

	; Make sure that DISORT ran 
  if(flag ne 0) then begin
    print,'Error: Problem in the Radiative Transfer Solver'
    return,1
  endif

	; Convert the radiance from W/(m2 sr cm-1) into mW / (m2 sr cm-1)
	; and the fluxes from W/(m2 cm-1) to mW / (m2 cm-1)
  radiance   = radiance  * 1000.
  flux_up    = flux_up   * 1000.
  flux_down  = flux_down * 1000.
  flux_beam  = flux_beam * 1000.

  return,0
end
;_++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;_+++++++++++++++
; $Log: idl_disort.pro,v $
; Revision 1.5  2009/03/11 17:32:08  dturner
;   Updated to output the radiative fluxes (up, diffuse down, and direct beam) that
; are now being output by LBLDIS.
;
; Revision 1.4  2008/06/18 21:45:44  dturner
;   Updated to fix some details with the documentation and to remove one of the
; keywords (abs_only) that I had not implemented.  I should note that the code
; can be put in absorption mode by selecting the other solver...
;
; Revision 1.3  2008/06/18 21:41:20  dturner
;   Updated to move the routine idl_disort() to the end of the file, so that all
; of the other necessary routines are compiled first.
;
; Revision 1.2  2008/06/18 21:34:49  dturner
;   Significant modification to allow multiple layer clouds to be input.  I also
; performed more extensive validation, ensuring that the solar component works
; correctly (it did not check for negative SZA before) and to make sure that the
; variable types (e.g., long, float) were correct before calling
; disort_ramp_sensitivity().  I also moved to using pointers for the SSP DB
; to allow for multiple DBs to be read in.
;   I have compared the output of this routine, in both DISORT (e.g., with scattering)
; and in absorption_only mode, with calculations made directly with LBLDIS.  There
; is very good agreement (I had slightly different gaseous optical depth input), but
; I am pretty confident that I have this working correctly at this time.
;
; Revision 1.1  2008/05/27 18:42:25  dturner
; Initial revision
;
;_+++++++++++++++

