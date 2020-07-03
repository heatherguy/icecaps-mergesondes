; $Id: rundecker.pro,v 1.30 2008/05/01 14:53:41 dturner Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Abstract:
;	  This script builds rundecks which can be used for both the LBLRTM
;	and the RRTM. Clear sky runs only. Can use standard atmospheres
;	for the PTU data (one or all variables).  Able to compute either 
;	radiance/transmittance or optical depths.  Layer structure used in the
;	models can be specified.  Lots of features here.
;
;	  Profiles for other gasses can be specified to be one of the default
;       atmospheres.  In fact, its a good idea to always specify the background
;	atmosphere when z/p/t/w are entered, so the correct atmosphere is added
;	above the highest level in the z/p/t/w profile...
;
; Author:	Dave Turner, PNNL
; Date:		February, 1998
;
; Arguments:
;	MODEL:			0 - RRTM, 1 - LBLRTM ch1, 2 - LBLRTM ch2, or 
;					3 - LBLRTM user defined wavenumber range
;	APROFILE:		Must be one of:
;				  1 - Tropical model
;				  2 - Midlatitude summer model
;				  3 - Midlatitude winter model
;				  4 - Subarctic summer model
;				  5 - Subarctic winter model
;				  6 - US Standard 1976 (default)
;				    Note that a profile should be selected, even 
;				    though z/p/t/w are entered, because often these
;				    standard profiles are added to the uppermost
;				    levels to extend the calculation high enough.
;	Z:			Altitude profile (km MSL)
;	P:			Pressure profile (units: mb, atm, or torr    - def=mb)
;	T:			Temperature profile (units: K or C           - def=K)
;	W:			Humidity profile (units: ppmv, cm-3, g/kg, g/m3,
;				    partial pres mb, dewpt C, dewpt K, rh %  - def=g/kg)
;	
; Keywords:
;	o3_profile:		Used to input the Ozone profile.  Requires that 
;				   "profile" is set to zero, and that this profile
;				   has the same number of levels as the Z/P/T/W profiles
;				   The default unit is ppmv.
;	TAPE5:			If set, the name of the TAPE5 file.  Else it will 
;				   default to one of several standard names.
;	IOUT:			Output flag. See RRTM instructions. Meaningless for LBL.
;				   Values are {-1,0,1,2,...15,16,99}.  def=0
;	ICLD:			Cloud input flag. See RRTM instructions. 
;				   Meaningless for LBL. Default=0
;	NUMANGS:		Number of radiance angles used in RRTM calculation.
;				   Meaningless for LBL.  Values are {-1,0,1,2,3,4}. def=0
;
;	VIEW_ANGLE:		Angle for the calculation.  Values range from 0-180,
;				   with zero being associated with zenith (downwelling).
;				   The default is zero.
;	SFC_TEMP:		The surface temperature. This is required for upwelling
;				   calculations.
;	SFC_EMIS:		The surface emissivity.  A constant between 0-1, or 
;				   a 2-d array of wnum x emissivity, where the 
;				   wavenumber grid has a constant dv
;       SFC_REFL:		The surface reflectivity.  A constant between 0-1, or
;				   a 2-d array of wnum x reflectivity, where the 
;				   wavenumber grid has a constant dv
;	SFC_TYPE:		The type of surface reflectance: "l" for Lambertian
;				   (the default) or "s" for Specular
;	CO2_MIX:		Value for co2 mixing ratio (ppm).  Default is 360 ppm.
;	COMMENT:		An optional comment string to be inserted into rundeck
;	P_COMMENT:		An optional comment string describing the profile
;
;	P_UNITS:		Options are: "mb", "atm", "torr", or "saX" 
;	T_UNITS:		Options are: "K", "C", or "saX"
;	W_UNITS:		Options are: "ppmv", "cm-3", "g/kg", "g/m3", "mb", 
;				    "C", "K", "%", or "saX"
;	O3_UNITS:		Options are: "ppmv", "cm-3", "g/kg", "g/m3", "mb", 
;				    "C", "K", "%", or "saX"
;		NOTE: in any of the units above (P,T,W), the standard atmosphere 
;			can be selected instead with the "saX", where X indicates
;			which atmosphere to take the profile from (1-6)
;
;	SHORT			Set to use the 'short' profile of model layers
;				    (only up to 20 km) -- the default model layer
;				    profile goes up to 66 km.  This keyword is 
;				    ignored if MLAYERS is set.  This keyword is 
;				    only used for LBLRTM calculations.
;	MLAYERS			An array of heights (km MSL) that specify the levels
;				    to use in the LBLRTM calculation.
;	ALTITUDE		If mlayers is not passed in and the height profile (z)
;				    was also not passed in, then this variable is 
;				    added to the default mlayers to account for the
;				    instruments altitude [in km] above mean sea level.
;	WNUM1			Set to the wavenumber for the start of the interval.
;				    Set to -50 cm-1 below what you actually desire
; 	WNUM2			Set to the wavenumber for the end of the interval.
;				    Set to +50 cm-1 below what you actually desire
;				    Note: WNUM1 and WNUM2 are only valid if MODEL=3,
;				    and the difference between the ending and starting
;				    wavenumber must be less than 2020 cm-1 and greater
;				    than 120 cm-1. Radiance/transmittance output will 
;				    be at AERI resolution and frequencies.  Optical
;				    depth data will be monocromatic.
;	OD_ONLY:		Set to 1 to output optical depths, otherwise
;				    radiances and transmittances are output.
;				    OD data are monochromatic.
;	XSEC:			Set to 1 to use cross sections for CCL4, F11, and F12,
;					zero otherwise (default is 1) -- LBLRTM only
;	CCL4_SFACTOR		If xsec is on, then this is the scale factor to 
;				    apply to the CCL4 profile.  Default = 1.0
;						default_ccl4_xsec = 110.5 ppbv
;	F11_SFACTOR		If xsec is on, then this is the scale factor to
;				    apply to the F11 (CFC11) profile.  Default = 1.0
;						default_f11_xsec  = 278.3 ppbv
;	F12_SFACTOR		If xsec is on, then this is the scale factor to
;				    apply to the F12 (CFC12) profile.  Default = 1.0
;						default_f12_xsec  = 502.7 ppbv
;	SC:			Set this to zero to not apply any convolution to the 
;				    output; setting it to one convolves it with the
;				    AERI's instrument function (an interferometer with
;				    a pathlength of 1.03702766 cm).
;				    Default value is 1 for radiance calculations, and 
;				    0 for optical depth calculations.
;	CNTNM:			Takes scalar values from 0-5, else needs to be a 
;					7-element floating point array.  If none 
;					of these, then CNTNM is set to default value.
;				    0 -> No continuum applied	
;				    1 -> All continua applied -- THIS IS DEFAULT VALUE
;				    2 -> No self-broadened h2o continuum applied
;				    3 -> No foreign-broadened h2o continuum applied
;				    4 -> No self or foreign broadened h2o ctnm applied
;				    5 -> No Rayleigh broadening
;				    7-element floating point array: scale factors
;					  for continua: self-h2o, foreign-h2o, 
;					  co2, o3, o2, n2, rayleigh (vals btwn 0-1)
; 	TAPE7:			If this is set (a string to a TAPE7 file), then
;				    this file (model levels, pressures, temperatures,
;				    gas amounts, etc) will be the input profiles
;				    for the model run.  Only valid for LBLRTM runs.
;	monortm:		Set this to create a monoRTM rundeck, with calculations
;				    at the standard 5 frequencies (two around 23.8, 
;				    two around 31.4, and one at 90 GHz)
; 	freqs:			The frequencies in GHz for the monoRTM calculations
;				    The default is [23.6640, 23.9320, 31.2690, 31.5310] 
;				    (average the first two to get the ARM MWR's 23.8 Tb
;				    and the second two to get ARM MWR's 31.4 Tb)
;	silent:			Set this keyword to shutdown some of the noisiness
;	v10:			Set this keyword if LBLRTM v10 or above is being used
;	
; Call:
	pro rundecker, model, aprofile, z, p, t, w, o3_profile=o3_profile, $
		iout=iout, icld=icld, numangs=numangs, $
		comment=comment, p_comment=p_comment, $
		p_units=p_units, t_units=t_units, w_units=w_units, o3_units=o3_units, $
		co2_mix=co2_mix, cntnm=cntnm, xsec=xsec, od_only=od_only, sc=sc, $
		ccl4_sfactor=ccl4_sfactor, $
		f11_sfactor=f11_sfactor, f12_sfactor=f12_sfactor, $
		view_angle=view_angle, sfc_emis=sfc_emis, sfc_refl=sfc_refl, $
		sfc_temp=sfc_temp, sfc_type=sfc_type, $
		wnum1=wnum1, wnum2=wnum2, short=short, $
		mlayers=mlayers, altitude=altitude, v10=v10, $
		tape5=tape5, tape7=tape7, monortm=monortm, freqs=freqs, silent=silent
;-
  	; For capturing the version number
  rcsid = '$Id: rundecker.pro,v 1.30 2008/05/01 14:53:41 dturner Exp $'
  parts = str_sep(rcsid, ' ')
  version = parts(2)

  nparms = n_params()
  if(nparms ne 2 and nparms ne 6) then begin
    print,'Usage error: must be called as either'
    print,'There were ' + string(nparms)
    print,'	rundecker model profile'
    print,'	rundecker model profile z p t w'
    return
  endif
  
  if(n_elements(aprofile) eq 0) then aprofile = 6	; Default is US Standard Atmos
  if(aprofile lt 1 or aprofile gt 6) then begin
    print,'Error: Value for keyword aprofile is out of range - aborting'
    return
  endif

  if(n_elements(z) ne n_elements(p) or $
  	n_elements(z) ne n_elements(t) or $
	n_elements(z) ne n_elements(w)) then begin
    print,'Error: The length of the z/p/t/w vectors must be the same - aborting'
    return
  endif
  if(n_elements(z) gt 0) then have_profile = 1 else have_profile = 0

  if(n_elements(ccl4_sfactor) eq 0) then ccl4_sfactor = 1.0
  if(n_elements(f11_sfactor) eq 0)  then f11_sfactor = 1.0
  if(n_elements(f12_sfactor) eq 0)  then f12_sfactor = 1.0

  if(n_elements(view_angle) eq 0) then view_angle = 0.0	; Calculate downwelling radiation

  if(keyword_set(monortm)) then begin
    if(view_angle ne 0) then begin
      print,'MonoRTM calcs (in this tool) can only be made in the downwelling direction'
      print,'	Please set view_angle to zero degrees'
      return
    endif
    	; We don't need heavy molecule cross-sections
    if(n_elements(xsec) eq 0) then xsec = 0
    if(xsec ne 0) then begin
      print,'There is no need for heavy molecule cross sections -- turning them off'
      xsec = 0
    endif
    	; We won't be applying any instrument filter function
    if(n_elements(sc) eq 0) then sc = 0
    if(sc ne 0) then begin
      print,'No filter function is applied to monoRTM calculations -- turning sc off'
      sc = 0
    endif
    	; We can define wnum1 and wnum2 here so that they don't need to be
	; entered.  They aren't used anyway...
    wnum1 =  400
    wnum2 = 1400
  endif

  if(90 lt view_angle mod 360 and view_angle mod 360 lt 270) then direction='upwelling' $
  else direction = 'downwelling'

  if(direction eq 'upwelling') then begin
    if(not keyword_set(silent)) then $
      print,'  Writing tape5 for an upwelling calculation...'
    if(n_elements(sfc_temp) eq 0) then begin
      print,'ERROR: Surface temperature (sfc_temp) is required for upwelling calculations'
      return
    endif
    if(n_elements(sfc_type) eq 0) then sfc_type = 'l'
    if(sfc_type ne 'l' and sfc_type ne 's') then begin
      print,'Error: Incorrectly defined surface (reflectance) type'
      return
    endif
  endif else $
    if(not keyword_set(silent)) then $
      print,'  Writing tape5 for a downwelling calculation...'

  iemit = 1		; Calculate radiance and transmittance
  merge = 0		; Normal merge
  if(n_elements(sc) eq 0) then $
    scan  = 3 $		; Filter function set for FFT-based instrument (AERI)
  else if(sc eq 0) then scan = 0 else scan = 3

  if(n_elements(od_only) eq 0) then od_only = 0
  if(n_elements(xsec) eq 0)    then xsec  = 1 
  	
	; Handle the continuum options and scale factors
  cntnm_default = 1
  reset_cntnm   = 'FALSE'
  if(n_elements(cntnm) eq 0) then cntnm = cntnm_default
  if(n_elements(cntnm) ne 1 and n_elements(cntnm) ne 7) then reset_cntnm = 'TRUE' $
  else begin
    if(n_elements(cntnm) eq 1) then begin
      if(cntnm lt 0 or cntnm gt 5) then reset_cntnm = 'TRUE'
    endif else begin 
      		; Now check to make sure cntnm isn't set to a negative number
      foo = where(cntnm lt 0, nfoo)
      if(nfoo gt 0) then cntnm(foo) = 0
      cntnm_array = cntnm	; The array of continuum scale factors
      cntnm = 6			; The flag indicating scale factors will be used
    endelse
  endelse
  if(reset_cntnm eq 'TRUE') then begin
    print,'WARNING: continuum flag is reset to the default - continuing'
    cntnm = cntnm_default
  endif

  		; If we only want optical depths, then need to reset some flags
  if(od_only gt 0.5) then begin
    iemit = 0		; Optical depths only
    scan  = 0		; No filter function applied (monocromatic results)
    merge = 1		; Optical depths only; results from each layer in separate file
  endif

  	; If this is the RRTM, then turn these off as they are meaningless anyway...
  if(model eq 0) then begin
    xsec = 0
    cntnm = 0
    iemit = 1
  endif
  	; Don't need cross sections for channel 2
  if(model eq 2) then xsec = 0

	; Unable to insert cross sections if we don't have z/p/t/w profiles
  if(have_profile eq 0) then xsec=0

  if(n_elements(co2_mix) eq 0) then co2_mix = 360.0
  if(n_elements(model) eq 0) then model = 0
  if((model lt 0) or (model gt 3)) then begin
    print,'Undefined model - quitting'
    return
  endif
  if(n_elements(numangs) eq 0) then numangs = 0
  if(n_elements(iout) eq 0) then iout = 0
  if(n_elements(icld) eq 0) then icld = 0

  if(n_elements(o3_profile) gt 0) then begin
    if(n_elements(o3_profile) ne n_elements(p)) then begin
      print,'Ozone profile does not have same number of levels as pressure profile'
      return
    endif
  endif

	; If TAPE7 is set, validate that the file actually exists.  If it does,
	; then use this file as the input into the model run.
  if(keyword_set(tape7)) then begin
    file = findfile(tape7, count=count)
    if(count ne 1) then begin
      print,'Unable to determine the TAPE7 file ' + tape7
      print,'  and therefore the code is aborting!'
      return
    endif
    tape7 = file(0)
    iatm = 0
  endif else iatm = 1

  	; If this is an RRTM run, then MLAYERS and SHORT should have no affect
  if(model eq 0 and not keyword_set(monortm)) then begin
;    mlayers = -1
    short = 0
  endif

	; The altitudes for the model layers
  if(n_elements(mlayers) le 0) then mlayers = -1
  if(mlayers(0) lt 0) then begin
    if(n_elements(altitude) eq 0) then altitude = 0
    if(keyword_set(short)) then begin
      		; The old layering used for the SGP QME...
      mlayers = [0.320, 0.360, 0.400, 0.500, 0.600, 0.700, 0.800, $
      	0.900, 1.000, 1.200, 1.400, 1.600, 1.800, 2.000, 2.200, 2.400, 2.600, $
      	2.800, 3.000, 3.200, 3.400, 3.600, 3.800, 4.000, 4.500, 5.000, 5.500, $
      	6.000, 6.500, 7.000, 7.500, 8.000, 8.500, 9.000, 9.500, 10.000, 11.000, $
      	12.000, 13.000, 14.000, 15.000, 16.500, 18.000, 20.000] 

      		; Better layering in general...
      mlayers = [findgen(11)*0.1,findgen(10)*0.25+1.25, $
      		findgen(23)*0.5+4.0,findgen(5)+16]
    endif else begin
      		; The old layering used for the SGP QME...
      mlayers = [0.320, 0.360, 0.400, 0.500, 0.600, 0.700, 0.800, $
      	0.900, 1.000, 1.200, 1.400, 1.600, 1.800, 2.000, 2.200, 2.400, 2.600, $
      	2.800, 3.000, 3.200, 3.400, 3.600, 3.800, 4.000, 4.500, 5.000, 5.500, $
      	6.000, 6.500, 7.000, 7.500, 8.000, 8.500, 9.000, 9.500, 10.000, 11.000, $
      	12.000, 13.000, 14.000, 15.000, 16.500, 18.000, 20.000, 22.000, 24.000, $
	25.500, 27.000, 28.500, 30.000, 32.000, 34.000, 36.000, 38.000, 40.000, $
	42.000, 44.000, 46.000, 48.000, 50.000, 52.000, 54.000, 56.000, 58.000, $
	60.000, 64.000, 68.000]

      		; Better layering in general...
      mlayers = [findgen(11)*0.1,findgen(10)*0.25+1.25, $
      		findgen(23)*0.5+4.0,findgen(5)+16, findgen(10)*2+22, findgen(8)*4+42]
    endelse

    		; If we have entered a profile (z),
    if(have_profile) then mlayers = mlayers + z(0) $
    else mlayers = mlayers + altitude
  endif

	; Put this in the rundeck until I am sure I am 
	;    getting everything in the right place
  numbers0 = $
    '         1         2         3         4         5         6         7         8         9'
  numbers1 = $
    '123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 '

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Standard LBLRTM parameters
	; Meaning of some of these flags:
	;    CN: continuum off/on
	;    AE: aerosols off/on (set to 1 uses LOWTRAN aerosols, others available)
	;    EM: 0->optical depth only, 1->radiance and transmittance, others
	;    SC: scanning function (one of 3 types)
	;    FI: filter? no/yes
	;    PL: plot (i.e., write out output) no/yes
	;    XS: Use cross sections no/yes
  rec_1_2  = $
      ' HI=1 F4=1 CN=' + string(format='(I1)', cntnm)
  rec_1_2 = rec_1_2 + ' AE=0 EM=' + string(format='(I1)',iemit) + $
      ' SC=' + string(format='(I1)', scan) + ' FI=0 PL=0 TS=0 AM=' + $
      string(format='(I1)',iatm) + ' MG=' + $
      string(format='(I1)',merge) + ' LA=0 MS=0 XS=' + string(format='(I1)',xsec)
  rec_1_2 = rec_1_2 + '   00   00'

  rec_1_3_a = $
    '420.094   1849.855                                          0.0002    0.001'
  rec_1_3_b = $
    '1750.338  3070.000                                          0.0002    0.001'
  if(model eq 3) then begin
    if(n_elements(wnum1) eq 0 or n_elements(wnum2) eq 0) then begin
      print,'Undefined starting or ending wavenumber for custom calculation - aborting'
      return
    endif
    if(wnum2 - wnum1 ge 2020) then begin
      print,'Maximum difference allowed between starting and ending wnum is 2020 - abort'
      return
    endif
    if(wnum2 - wnum1 le 120) then begin
      print,'Ending wnum must be at least 120 cm-1 larger than starting wnum - abort'
      return
    endif
    if(keyword_set(v10)) then $
      rec_1_3_c = string(format='(F10.3)',wnum1) + string(format='(F10.3)',wnum2) + $
     	 '                                        0.0002    0.001' + $
	 '                             2' $
    else $
      rec_1_3_c = string(format='(F10.3)',wnum1) + string(format='(F10.3)',wnum2) + $
     	 '                                        0.0002    0.001'
  endif
    
  	; Set the boundary temperature, emissivity, and reflectivity.
	; This is only really important for the upwelling calculations.
  	; Only need to output these if we are calculating radiances (iemit = 1)
  if(iemit eq 1) then begin
    if(direction eq 'downwelling') then rec_1_4 = '0.000     0.000' $
    else begin
    		; Get the surface temperature
      rec_1_4 = string(format='(F10.3)',sfc_temp)

		; Set a default set of values
      if(n_elements(sfc_emis) eq 0) then begin
        if(n_elements(sfc_refl) eq 0) then sfc_emis = 1.0 $
        else begin
  	  if((size(sfc_refl))(0) eq 0) then sfc_emis = 1.0 - sfc_refl else begin
	    if((size(sfc_refl))(0) eq 2) then begin
	      if((size(sfc_refl))(1) ne 2) then sfc_ref = transpose(sfc_refl)
	      sfc_emis = sfc_refl
	      sfc_emis(1,*) = 1.0 - sfc_refl(1,*)
	    endif else begin
	      print,'ERROR: Problem with the format of the sfc_refl array'
	      return
	    endelse
	  endelse
        endelse
      endif
      if(n_elements(sfc_refl) eq 0) then begin
  	if((size(sfc_emis))(0) eq 0) then sfc_refl = 1.0 - sfc_emis else begin
	  if((size(sfc_emis))(0) eq 2) then begin
	    if((size(sfc_emis))(1) ne 2) then sfc_emis = transpose(sfc_emis)
	    sfc_refl = sfc_emis
	    sfc_refl(1,*) = 1.0 - sfc_emis(1,*)
	  endif else begin
	    print,'ERROR: Problem with the format of the sfc_emis array'
	    return
	  endelse
	endelse
      endif 

    		; Handle the emissivity.  If a 2 x n array is passed in, then
		; write out the file "EMISSIVITY" properly
      foo = size(sfc_emis)
      if(foo(0) eq 0) then rec_1_4 = rec_1_4 + string(format='(F10.3)',sfc_emis) + $
    	  string(format='(F10.3)',0.) + string(format='(F10.3)',0.) $
      else if(foo(0) eq 2) then begin
      			; The -1 implies to look for emissivity file
        rec_1_4 = rec_1_4 + string(format='(F10.3)',-1) + $
      	      string(format='(F10.3)',0.) + string(format='(F10.3)',0.) 
        if(foo(1) ne 2) then emis = reform(sfc_emis) else emis = sfc_emis
        openw,nlun,'EMISSIVITY',/get_lun
        n = n_elements(emis(0,*))
        printf,nlun,format='(F10.3,F10.3,F10.3,5x,I5)',emis(0,0),emis(0,n-1), $
      		emis(0,1)-emis(0,0),n
        for i=0,n-1 do printf,nlun,format='(F10.3)',emis(1,i)
        free_lun,nlun
      endif

    		; Handle the reflectivity.  If a 2 x n array is passed in, then
		; write out the file "REFLECTIVITY" properly
      foo = size(sfc_refl)
      if(foo(0) eq 0) then rec_1_4 = rec_1_4 + string(format='(F10.3)',sfc_refl) + $
    	      string(format='(F10.3)',0.) + string(format='(F10.3)',0.) $
      else if(foo(0) eq 2) then begin
      			; The -1 implies to look for reflectivity file
        rec_1_4 = rec_1_4 + string(format='(F10.3)',-1) + $
    	        string(format='(F10.3)',0.) + string(format='(F10.3)',0.)
        if(foo(1) ne 2) then refl = reform(sfc_refl) else refl = sfc_refl
        openw,nlun,'REFLECTIVITY',/get_lun
        n = n_elements(refl(0,*))
        printf,nlun,format='(F10.3,F10.3,F10.3,5x,I5)',refl(0,0),refl(0,n-1), $
      		refl(0,1)-refl(0,0),n
        for i=0,n-1 do printf,nlun,format='(F10.3)',refl(1,i)
        free_lun,nlun
      endif
      rec_1_4 = rec_1_4 + string(format='(4x,1A)',sfc_type)
    endelse
  endif

  if(have_profile eq 1) then flag = 0 else flag = aprofile
  if(keyword_set(v10)) then $
    rec_3_1 = $
    	'    ' + string(format='(I1)',flag) + $
    	'    2' + string(format='(I5)',n_elements(mlayers)) + $
    	'    1    1    7    1' $
  else rec_3_1 = $
    	'    ' + string(format='(I1)',flag) + $
    	'    2' + string(format='(I5)',n_elements(mlayers)) + $
    	'    1    1    7    1                                   ' + $
    	string(format='(F10.3)',co2_mix)


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Additional portion for the RRTM part
  if(model eq 0) then begin
    rec_1_2 = rec_1_2 + $
      '    ' + string(format='(I0)',numangs) + '  ' + string(format='(I3)',iout) + $
      '    ' + string(format='(I0)',icld)
    rec_1_4 = '-1'
  endif

  	; If this is a monoRTM run, then override what is in rec_1_2
	; This is very default behavior and should be better treated...
  if(keyword_set(monortm)) then $
    rec_1_2 = '    1    0    1    0    1    0    0    1    0    1         0    0    0    0    0    1'

	; Get the date for the default comment string
  spawn,'date',foo
  date = foo(0)

	; Get the proper output filename
  if(n_elements(tape5) eq 0) then begin
    if(keyword_set(monortm)) then tape5='MONORTM.IN' $
    else begin
      if(model eq 0) then tape5 = 'INPUT_RRTM' $
      else if(model eq 1) then tape5 = 'TAPE5_ch1' $
      else if(model eq 2) then tape5 = 'TAPE5_ch2' $
      else tape5 = 'TAPE5_custom'
    endelse
  endif

	; Open the file, and write the rundeck
  if(not keyword_set(silent)) then $
    print,'Writing ' + tape5 + '...'
  openw,lun,tape5,/get_lun

  acomment = 'Rundeck created on ' + date + ' by rundecker.pro (v' + version + ')'
  printf,lun,acomment

  printf,lun,numbers0
  printf,lun,numbers1

  if(n_elements(comment) eq 0) then comment = 'None'
  if(n_elements(comment) gt 75) then comment = comment(0:75)
  printf,lun,'$ ' + comment
  
  printf,lun,rec_1_2

  if(keyword_set(monortm)) then begin
    printf,lun,'-0.100E+00 0.100E+02 0.000E+00 0.100E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00    0      0.000E+00'
    if(n_elements(freqs) gt 0) then begin
      clight = 2.99792458E+10	; cm / s
      printf,lun,format='(I0)',n_elements(freqs)
      for kk=0,n_elements(freqs)-1 do $
        printf,lun,format='(E19.7)',freqs(kk)/clight*1e9
    endif else begin
      printf,lun,'5'
      printf,lun,'0.789344'
      printf,lun,'0.79828'
      printf,lun,'1.043027'
      printf,lun,'1.051763'
      printf,lun,'3.000000'
    endelse
    printf,lun,' 0.275E+01 0.100E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00'
    goto,model_layers
  endif

	; Add the continuum scale factors, if desired (Record 1_2a)
  if(cntnm eq 6) then printf,lun,format='(7(F7.4,2x))',cntnm_array

  if(model eq 1) then printf,lun,rec_1_3_a
  if(model eq 2) then printf,lun,rec_1_3_b
  if(model eq 3) then printf,lun,rec_1_3_c
  if(keyword_set(v10)) then begin
    printf,lun,' m'
    printf,lun,format='(2(E15.7))',0,(co2_mix/1e6)	; Convert to volume mix ratio
  endif
  if(iemit eq 1) then printf,lun,rec_1_4

  	; If this flag is not set, then add the TAPE7 file to the rundeck
  if(iatm eq 0) then begin
    line = ''
    openr,lun2,tape7,/get_lun
    readf,lun2,line		; This is the comment line
    readf,lun2,line		; This line has the number of layers, etc.
    		; We will replace the SECNTO flag with the direction (view_angle)
    secnto = 1./cos(view_angle/180.*!pi)
    secnto_string = string(format='(F9.6)',secnto)
    strput, line, secnto_string, 11
    		; Now place this line in the tape5
    printf,lun,line
    nlayers = fix(strmid(line,2,3))
    print,string(nlayers) + ' in the gas profiles'

    	; We replace the IPATH flag to zero that way the SECNTO flag dictates the direction
    for i=0,nlayers-1 do begin
      readf,lun2,line
      strput,line,' 0',38
      printf,lun,line
      readf,lun2,line
      printf,lun,line
    endfor

    readf,lun2,line
    printf,lun,line
    readf,lun2,line
    printf,lun,line
    readf,lun2,line
    		; We will replace the SECNTO flag with the direction (view_angle)
    strput, line, secnto_string, 11
    printf,lun,line
    nlayers = fix(strmid(line,2,3))
    print,string(nlayers) + ' in the xsec profiles'

    	; We replace the IPATH flag to zero that way the SECNTO flag dictates the direction
    for i=0,nlayers-1 do begin
      readf,lun2,line
      strput,line,' 0',38
      printf,lun,line
      readf,lun2,line
      printf,lun,line
    endfor
    free_lun,lun2
  endif else begin

	; Add in the section on the model layers
  model_layers:
  printf,lun,rec_3_1
  if(direction eq 'upwelling') then begin
    h1 = mlayers(n_elements(mlayers) -1)
    h2 = mlayers(0)
  endif else begin
    h1 = mlayers(0)
    h2 = mlayers(n_elements(mlayers) -1)
  endelse
  line = string(format='(F10.3)',h1) + $
	 string(format='(F10.3)',h2) + $
	 string(format='(F10.3)',view_angle)
  printf,lun,line
  line = ''
  for i=1, n_elements(mlayers) do begin
    line = line + string(format='(F10.3)',mlayers(i-1))
    if(i mod 8 eq 0) then begin
      printf,lun,line
      line = ''
    endif
  endfor
  if(n_elements(mlayers) mod 8 ne 0) then printf,lun,line

	; We are assuming that the input profiles are all the same length. We need to
	;   verify that they actually extend to the top layer needed for the model
	;   else we will use the U.S. Standard Atmosphere value
  if(have_profile eq 1) then begin
    if(not keyword_set(p_units))  then p_units  = 'mb'
    if(not keyword_set(t_units))  then t_units  = 'K'
    if(not keyword_set(w_units))  then w_units  = 'g/kg'
    if(not keyword_set(o3_units)) then o3_units = 'ppmv'

    case p_units of
      'mb':  	JCHARP = 'A'
      'atm':	JCHARP = 'B'
      'torr':	JCHARP = 'C'
      'sa1': 	JCHARP = '1'
      'sa2': 	JCHARP = '2'
      'sa3': 	JCHARP = '3'
      'sa4': 	JCHARP = '4'
      'sa5': 	JCHARP = '5'
      'sa6': 	JCHARP = '6'
      else: begin
	print,'Unidentified pressure unit - quitting'
	return
      endelse
    endcase
    case t_units of
      'K':	JCHART = 'A'
      'C':	JCHART = 'B'
      'sa1':	JCHART = '1'
      'sa2':	JCHART = '2'
      'sa3':	JCHART = '3'
      'sa4':	JCHART = '4'
      'sa5':	JCHART = '5'
      'sa6':	JCHART = '6'
      else: begin
	print,'Unidentified temperature unit - quitting'
	return
      endelse
    endcase
    case w_units of
      'ppmv':	JCHAR = 'A'
      'cm-3':	JCHAR = 'B'
      'g/kg':	JCHAR = 'C'
      'g/m3':	JCHAR = 'D'
      'mb':	JCHAR = 'E'
      'K':	JCHAR = 'F'
      'C':	JCHAR = 'G'
      '%':	JCHAR = 'H'
      'sa1':	JCHAR = '1'
      'sa2':	JCHAR = '2'
      'sa3':	JCHAR = '3'
      'sa4':	JCHAR = '4'
      'sa5':	JCHAR = '5'
      'sa6':	JCHAR = '6'
      else: begin
	print,'Unidentified mosture profile unit - quitting'
	return
      endelse
    endcase
    JCHAR = JCHAR + string(format='(I0)',aprofile)	; Use climatology for co2
    if(n_elements(o3_profile) gt 0) then begin
      case o3_units of
         'ppmv':	JCHAR = JCHAR + 'A'
         'cm-3':	JCHAR = JCHAR + 'B'
         'g/kg':	JCHAR = JCHAR + 'C'
         'g/m3':	JCHAR = JCHAR + 'D'
         'sa1':		JCHAR = JCHAR + '1'
         'sa2':		JCHAR = JCHAR + '2'
         'sa3':		JCHAR = JCHAR + '3'
         'sa4':		JCHAR = JCHAR + '4'
         'sa5':		JCHAR = JCHAR + '5'
         'sa6':		JCHAR = JCHAR + '6'
         else: begin
           print,'Unidentified ozone profile unit - quitting'
           return
         endelse
       endcase
     endif else JCHAR = JCHAR + string(format='(I0)',aprofile)

		; And use defaults for rest of the gases...
    JCHAR = JCHAR + string(format='(I0)',aprofile)
    JCHAR = JCHAR + string(format='(I0)',aprofile)
    JCHAR = JCHAR + string(format='(I0)',aprofile)
    JCHAR = JCHAR + string(format='(I0)',aprofile)

		; Make and use the copies of the profiles versus the actual profiles
    zz = z
    tt = t
    pp = p
    ww = w
    if(n_elements(o3_profile) gt 0) then oo3 = o3_profile $
    else oo3 = replicate(0.0, n_elements(pp))

		; Need to apply some QC to the pressure profile for the RRTM
		;    Make sure the pressures are always decreasing, and that no two
		;    levels have the same pressure value
    foo = sort(pp)
    foo = pp(foo)
    bar = where(pp ne reverse(foo), nhits)
    if(nhits gt 0) then begin
      print,'  Pressure array is not monotonically increasing - quitting'
      free_lun,lun
      return
    endif

	; Now find and keep only the unique pressure levels
	;   for all of the input variables
    foo = uniq(pp)
    pp = pp(foo)
    zz = zz(foo)
    tt = tt(foo)
    ww = ww(foo)
    oo3 = oo3(foo)

    inlayers = n_elements(zz)
    foo = where(mlayers gt zz(inlayers-1), nhits)
    if(nhits ge 1) then begin
      foo = foo(0:nhits-1)
      zz = [zz, mlayers(foo)]
      pp = [pp, replicate(0, nhits)]
      tt = [tt, replicate(0, nhits)]
      ww = [ww, replicate(0, nhits)]
      oo3 = [oo3, replicate(0, nhits)]
      inlayers = inlayers + nhits
    endif

    if(n_elements(p_comment) eq 0) then p_comment = 'User supplied profile'
    if(n_elements(p_comment) gt 23) then p_comment = p_comment(0:23)
    printf,lun,string(format='(I5)',inlayers) + ' ' + p_comment
    for i=0, inlayers-nhits -1 do begin
      printf,lun, string(format='(F10.4)',zz(i)) + string(format='(1x,F9.4)',pp(i)) + $
        string(format='(E10.3)',tt(i)) + '     ' + JCHARP + JCHART + '   ' + JCHAR
      printf,lun, string(format='(E10.3)',ww(i)) + $	; water vapor
	string(format='(E10.3)',0) + $			; co2
        string(format='(E10.3)',oo3(i)) + $		; ozone
        string(format='(E10.3)',0) + $			; n2o
	string(format='(E10.3)',0) + $			; co
        string(format='(E10.3)',0) + $			; ch4
	string(format='(E10.3)',0)			; o2
    endfor

    JCHARP = string(format='(I0)',aprofile)
    JCHART = string(format='(I0)',aprofile)
    JCHAR  = string(format='(7(I0))', $
    	aprofile,aprofile,aprofile,aprofile,aprofile,aprofile,aprofile)
    for i=inlayers-nhits, inlayers -1 do begin
      printf,lun, string(format='(F10.3)',zz(i)) + string(format='(E10.3)',pp(i)) + $
        string(format='(E10.3)',tt(i)) + '     ' + JCHARP + JCHART + '   ' + JCHAR
      printf,lun, string(format='(F10.3)',ww(i)) + $
        string(format='(E10.3)',oo3(i)) + string(format='(E10.3)',0) + $
        string(format='(E10.3)',0) + string(format='(E10.3)',0) + $
        string(format='(E10.3)',0) + string(format='(E10.3)',0)
    endfor
  endif

  	; If it is ch1 or ch2 rundecks, then we can add options for heavy molecule
	;	cross sections, aerosols, and filter functions...
  if(model gt 0) then begin
    		; Add in the stuff for the cross sections, if desired, here
		;   There are no appreciable cross sections for ch2...
    if(xsec eq 1 and have_profile eq 1) then begin
      			; Default cross sections (ppbv)
      default_ccl4_xsec = 110.5
      default_f11_xsec  = 278.3
      default_f12_xsec  = 502.7
      			; Convert to ppmv and apply the scale factors
      ccl4_xsec = default_ccl4_xsec / 1e6 * ccl4_sfactor
      f11_xsec  = default_f11_xsec  / 1e6 *  f11_sfactor
      f12_xsec  = default_f12_xsec  / 1e6 *  f12_sfactor

      printf,lun, $
      	'    3    0    0  The following cross-sections were selected:'
      printf,lun, $
      	'CCL4      F11       F12'

      		; We can enter these profiles at any height resolution (i.e.,
		; it can be different than the gas profiles defined earlier).
		; Since these are constant profiles, let's just use two levels.
      index = indgen(n_elements(zz))	; For each level in the input profile
      index = [0, n_elements(zz)-1]	; The first and last level of the input profile
      printf,lun, $
      	string(format='(I5)',n_elements(index)) + '    0' + ' XS 1995 UNEP values'
      for i=0,n_elements(index)-1 do begin
        printf,lun,string(format='(F10.3)',zz(index(i))) + '     AAA'
	printf,lun,string(format='(E10.3)',ccl4_xsec) + $
		   string(format='(E10.3)',f11_xsec) + $
		   string(format='(E10.3)',f12_xsec)
      endfor
    endif
  endif		; if (model gt 0)
  endelse		; else (iatm ne 0)
		
  if(model gt 0) then begin
		; Add in the stuff for the aerosols, if desired, here

      		; This initial logic is attempting to tie the results to AERI
		; observed frequencies (wavenumber), as that is probably the 
		; most desired operation
    if(model eq 3) then begin
      delv = 1000.45522779D - 999.97308078D
      aerispot = 1000.45522779D
      varray = dindgen(40000L)*delv + 100.
      foo = where(varray ge aerispot)
      varray = varray - (varray(foo(0)) - aerispot)	; This array is now at AERI freqs
    endif

		; Add in the stuff for the filter functions, if desired, here
    			; These two lines indicate that the radiance is to be written
			;   to TAPE13, the transmittance to TAPE14.  The last number
			;   indicates the TAPE number
    if(scan eq 3) then begin		; Only if the filter-function is turned to FFT
      if(model eq 1) then begin
        printf,lun, $
      	  '1.03702766 497.575891803.71268    1   -4     0.48214700   12    1    1   13'
        printf,lun, $
      	  '1.03702766 497.575891803.71268    0   -4     0.48214700   12    1    1   14'
      endif else if(model eq 2) then begin
        printf,lun, $
      	  '1.037027661796.480423022.09850    1   -4     0.48214700   12    1    1   13'
        printf,lun, $
	  '1.037027661796.480423022.09850    0   -4     0.48214700   12    1    1   14'
      endif else if(model eq 3) then begin
        foo1 = where(varray gt wnum1 + 50., nfoo1)
        foo2 = where(varray gt wnum2 - 50., nfoo2)
        if(nfoo1 eq 0 or nfoo2 eq 0) then begin
          print,'Error determining range for _filtering_ in rundeck - aborting'
	  print,'Rundeck is incomplete!!!'
	  free_lun,lun
	  return
        endif

        printf,lun, '1.03702766' + string(format='(F10.5)',varray(foo1(0))) + $
      	  string(format='(F10.5)',varray(foo2(0))) + $
	  '    1   -4     0.48214700   12    1    1   13'
        printf,lun, '1.03702766' + string(format='(F10.5)',varray(foo1(0))) + $
      	  string(format='(F10.5)',varray(foo2(0))) + $
	  '    0   -4     0.48214700   12    1    1   14'
      endif
    endif

		; End of this set of commands to LBLRTM.  
    printf,lun,'-1.'

    		; Add in the commands to control the plotting (i.e., the 
		;    way the output files (TAPE27 and TAPE28) are created)
    if(iemit eq 1 and scan ne 0) then begin
      printf,lun, '$ Transfer to ASCII plotting data (TAPES 27 and 28)'
      printf,lun, $
    	' HI=0 F4=0 CN=0 AE=0 EM=0 SC=0 FI=0 PL=1 TS=0 AM=0 MG=0 LA=0 MS=0 XS=0    0    0'
      printf,lun, '# Plot title not used'
      if(model eq 1) then begin
        printf,lun, $
      	' 499.986651799.85550   10.2000  100.0000    5    0   13    0     1.000 0  0    0'
        printf,lun, $
  	'    0.0000    1.2000    7.0200    0.2000    4    0    1    1    0    0 0    3 27'
        printf,lun, $
	' 499.986651799.85550   10.2000  100.0000    5    0   14    0     1.000 0  0    0'
        printf,lun, $
	'    0.0000    1.2000    7.0200    0.2000    4    0    1    0    0    0 0    3 28'
      endif else if (model eq 2) then begin
        printf,lun, $
      	'1800.337523020.16992   10.2000  100.0000    5    0   13    0     1.000 0  0    0'
        printf,lun, $
	'    0.0000    1.2000    7.0200    0.2000    4    0    1    1    0    0 0    3 27'
        printf,lun, $
	'1800.337523020.16992   10.2000  100.0000    5    0   14    0     1.000 0  0    0'
        printf,lun, $
	'    0.0000    1.2000    7.0200    0.2000    4    0    1    0    0    0 0    3 28'
      endif else if (model eq 3) then begin
        foo1 = where(varray gt wnum1 + 55., nfoo1)
        foo2 = where(varray gt wnum2 - 55., nfoo2)
        if(nfoo1 eq 0 or nfoo2 eq 0) then begin
          print,'Error determining range for _plotting_ in rundeck - aborting'
	  print,'Rundeck is incomplete!!!'
	  free_lun,lun
	  return
        endif
        printf,lun, string(format='(F10.5)',varray(foo1(0))) + $
        	string(format='(F10.5)',varray(foo2(0))) + $
        	'   10.2000  100.0000    5    0   13    0     1.000 0  0    0'
        printf,lun, $
	'    0.0000    1.2000    7.0200    0.2000    4    0    1    1    0    0 0    3 27'
        printf,lun, string(format='(F10.5)',varray(foo1(0))) + $
          string(format='(F10.5)',varray(foo2(0))) + $
	  '   10.2000  100.0000    5    0   14    0     1.000 0  0    0'
        printf,lun, $
	'    0.0000    1.2000    7.0200    0.2000    4    0    1    0    0    0 0    3 28'
      endif

		; End of this set of commands to LBLRTM.  
      printf,lun,'-1.'
    endif		; End of if(iemit eq 1)

  endif

	; And finally, the closing statement
  printf,lun,'%%%'
  free_lun, lun

  if(not keyword_set(silent)) then $
    print,tape5 + ' completed'

  return
end
