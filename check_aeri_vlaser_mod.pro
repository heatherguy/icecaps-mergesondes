; $Id: check_aeri_vlaser_mod.pro,v 1.1 2008/01/04 15:01:54 dturner Exp dturner $
;+
; Abstract:
;	This script is a modified form of the script "check_aeri_vlaser.pro"
;
;   Just FYI: the default laser wavenumber value is 15799.0 cm-1.
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
  function check_aeri_vlaser_mod, $		; Output structure (see above)
	awnum, $			; AERI wavenumber array
	arad, $				; A single (typically mean) AERI spectrum
	cwnum, $			; LBLRTM wavenumber array
	crad, $				; LBLRTM computed spectrum
	ch2=ch2				; Set this if channel 2
;-
  	; What I will return if there's an error
  errorval = {multipliers:0., sqr_diffs:0., best_multiplier:0.} 

  	; The limits for the fitting
  if(keyword_set(ch2)) then begin
    channel = 2
    wnum1b = 2207	; the co2 region we will fit to
    wnum2b = 2220	; the co2 region we will fit to
  endif else begin
    channel = 1
    wnum1b = 730	; the co2 region we will fit to
    wnum2b = 740	; the co2 region we will fit to
  endelse

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  	; The actual meat of the routine
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ; This array stores the range of stretch factors
  multipliers = 1+(dindgen(901)/300-1.500) / 15799.0D
;  multipliers = 1+(dindgen(901)/100-1.500) / 15799.0D

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
