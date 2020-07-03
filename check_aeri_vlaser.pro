; $Id: check_aeri_vlaser.pro,v 1.2 2004/05/10 18:14:48 dturner Exp $
;+
; Abstract:
;	This script is used to check the accuracy of the laser wavenumber (vlaser) 
;   used in spectrally calibrating the AERI data.  It reads in the AERI data and 
;   will create an averaged observed spectrum for the time interval that is 
;   specified.  It will then take the entered radiosonde profile, plot it, make
;   a LBLRTM run, and then use a series of wavenumber 'stretches' to determine
;   the best stretch factor (multiplier) that should be applied to the laser
;   wavenumber.
;
;   Just FYI: the default laser wavenumber value is 15799.0 cm-1.
;
; Author:
;	Dave Turner
;		Pacific Northwest National Laboratory
;
; Date:
;	May 2004
;
; Returned output:
;	Structure containing {multipliers, sqr_diffs, best_multiplier}
;		where:
;		    multipliers 	an array of wavenumber scale factors 
;		    sqr_diffs   	the squared differences over the fit region 
;					  for the scaled AERI spectrum and LBLRTM 'truth'
;		    best_multiplier 	the best correction factor to apply to the 
;					  AERI's laser wavenumber.  Note that this is
;					  defined as "desired_vlaser / orig_vlaser",
;					  where the orig_vlaser is specified in the 
;					  netCDF file.
;
; Call:
  function check_aeri_vlaser, $		; Output structure (see above)
  	aeri_filename, $		; Filename of the AERI data to use
	sonde_filename, $		; Filename of the sonde data to use
	timerange, $			; 2-d array [start_hour, end_hour] used 
					;  	to compute the average AERI spectrum
	ch2=ch2				; Set to perform ch2 analysis (default is ch1)
;-
  	; What I will return if there's an error
  errorval = {multipliers:0., sqr_diffs:0., best_multiplier:0.} 
  
  	; Read in the AERI data
  files = findfile(aeri_filename, count=count)
  if(count ne 1) then begin
    print,'Error: Unable to determine ' + aeri_filename
    return, errorval
  endif
  fid = ncdf_open(files(0))
  ncdf_varget,fid,'base_time',bt
  ncdf_varget,fid,'time_offset',to
  ncdf_varget,fid,'wnum',awnum
  ncdf_varget,fid,'mean_rad',rad
  ncdf_close,fid
  systime2ymdhms,bt+to,yy,mm,dd,hh,nn,ss
  hour = hh + nn/60. + ss/3600.

  	; Some simple QC
  if(n_elements(timerange) ne 2) then begin
    print,'Error: Timerange must be [start_hour, end_hour]'
    return, errorval
  endif
  if(timerange(0) gt timerange(1)) then begin
    print,'Error: timerange(0) must be <= timerange(0)'
    return, errorval
  endif

	; Find the times within the desired window
  foo = where(timerange(0) le hour and hour le timerange(1), nfoo)
  if(nfoo le 0) then begin
    print,'Error: No AERI spectra found in the given timerange'
    return, errorval
  endif

	; Compute the average AERI spectrum for this time window
  arad = awnum
  for i=0,n_elements(awnum)-1 do arad(i) = mean(rad(i,foo))

  	; Now read in the radiosonde data
  files = findfile(sonde_filename, count=count)
  if(count ne 1) then begin
    print,'Error: Unable to determine ' + sonde_filename
    return, errorval
  endif
  pltsonde, filename=files(0), tdry=t, pres=p, mixr=w, alt=z, date=sdate, time=stime
  z = (z-z(0))/1000.

  if(keyword_set(ch2)) then begin
    wnum1a = 1800	; for the LBLRTM calc
    wnum2a = 3000	; for the LBLRTM calc
    wnum1b = 2207	; the co2 region we will fit to
    wnum2b = 2220	; the co2 region we will fit to
    channel = 2
  endif else begin
    wnum1a = 500	; for the LBLRTM calc
    wnum2a = 1500	; for the LBLRTM calc
    wnum1b = 730	; the co2 region we will fit to
    wnum2b = 740	; the co2 region we will fit to
    channel = 1
  endelse

  	; Create a rundeck and run the LBLRTM
  tp5    = 'tp5.vlaser'
  lblout = 'lblout.vlaser'
  rundecker, 3, 6, z, p, t, w, t_units='C', /short, tape5=tp5, $
  	wnum1=wnum1a, wnum2=wnum2a, xsec=0
  spawn,'lblrun ' + tp5 + ' ' + lblout

  	; Read in the tape27, convert the radiance units
  plt_tape27, dir=lblout, wnum=cwnum, rad=crad, /readonly
  crad = crad * 1.e7

	; Clean up the LBLRTM output
  spawn,'rm -rf ' + lblout + ' ' + tp5

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  	; The actual meat of the routine
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ; This array stores the range of stretch factors
  multipliers = 1+(dindgen(901)/300-1.500) / 15799.0D

        ; This array stores the squared difference for each stretch factor
  sqr_diffs   = dblarr(n_elements(multipliers))

	; Zerofill the spectral data 
  result1 = aeri_zerofill(awnum, arad, channel)
  result2 = aeri_zerofill(cwnum, crad, channel)

	; Determine the fit region
  foo1 = where(wnum1b le result1(0,*) and result1(0,*) le wnum2b)
  foo2 = where(wnum1b le result2(0,*) and result2(0,*) le wnum2b)

	; Loop over the stretch factors
  for j=0,n_elements(multipliers)-1 do begin
		; Interpolate the zerofilled AERI data to the new stretched spectrum
    arad_new = interpol(result1(1,*), result1(0,*)*multipliers(j), result1(0,foo1))

    		; Compute the squared difference over the spectral region
    sqr_diffs(j) = total((arad_new - result2(1,foo2))^2)
  endfor

	; Now find the stretch factor that has the minimum squared difference
  foo = where(min(sqr_diffs) eq sqr_diffs, nfoo)
  if(nfoo gt 2) then print,'Warning: more than one minimum squared difference found'
  
  	; Return the results...
  return, {multipliers:multipliers, sqr_diffs:sqr_diffs, $
		best_multiplier:multipliers(foo(0))} 
end
