; $Id: get_scat_properties.pro,v 1.5 2004/09/21 17:24:00 dturner Exp $
;+
; Abstract:
;	This routine extracts out the single scattering properties from the 
;   matrices that were previously read in.  It then extracts out the slices 
;   associated with the wavenumbers just above and below the desired wavenumber, 
;   and then interpolates these to the desired wavenumber.  Finally, it 
;   interpolates the properties to the effective radius and returns.
;	There is some error checking in this routine, but if an error is found
;   the code just stops there.  Probably the best answer if the code ever stops
;   in one of these traps is to improve the scattering property database(s) to 
;   include the wavelength/size desired.  
;
;     	This version of the code (1.5) also assumes that the database include
;   the scattering phase function, which made the database a structure instead
;   of just a square array.
;
function get_scat_properties, $		; Returns the desired scattering property
	field, $			; Which field from the DB: indicate one of
					;  'ext_xsec', 'sca_xsec', 'abs_xsec', 'w0', 'g'
					;  'Qe', 'Qa', 'Qs', 'vol', 'proj_area', or 'pf'
	wnum, $				; For this wavelength [cm-1]
	r, $				; For this effective radius [um]
	database, $			; This is the database array, returned from
					;   an earlier call to read_scat_databases().
	verbose=verbose			; Set this to have some status messages displayed
;-

  case field of
	'ext_xsec':	index = 3
	'sca_xsec':	index = 4
	'abs_xsec':	index = 5
  	'omega': 	index = 6
	'w0':		index = 6
	'asym' : 	index = 7
	'g' :		index = 7
	'Qe' : 		index = 8
	'Qa' : 		index = 9
	'Qs' : 		index = 10
	'vol' : 	index = 11
	'proj_area' : 	index = 12
	'pf' :		index = indgen(database.nphase)+13
	else: begin
	  print,'Undefined field used in get_scat_properties()'
	  stop,'Aborting inside get_scat_properties()'
	endelse
  endcase

  scattdb = database.data

	; Select the wavenumbers just below the desired wavenumber
  wnumdb = reform(scattdb(1,*))
  wnumdb_uniq = wnumdb(uniq(wnumdb))
  foo = where(wnum ge wnumdb_uniq, nfoo)
  if(nfoo le 0) then begin
    print,'Unable to find any data at wavenumbers below the desired wavenumber'
    stop,'Aborting inside get_scat_properties()'
  endif
  wnum_lower = max(wnumdb_uniq(foo))
  foo = where(wnumdb eq wnum_lower)
  scattdb_lower = scattdb(*,foo)

	; Select the wavenumber just above the desired wavenumber
  foo = where(wnum lt wnumdb_uniq, nfoo)
  if(nfoo le 0) then begin
    if(wnum_lower eq wnum) then begin
      scattdb = scattdb_lower
      goto, after_wnum_interpolation
    endif
    print,'Unable to find any data at wavenumbers above the desired wavenumber'
    stop,'Aborting inside get_scat_properties()'
  endif
  wnum_upper = min(wnumdb_uniq(foo))
  foo = where(wnumdb eq wnum_upper)
  scattdb_upper = scattdb(*,foo)

	; Now interpolate the scattering properties to the desired wavenumber
  scattdb = scattdb_lower		; Overwrite old scattering database
  	; Don't need to interpolate the flag, wavel, wnum, or reff
  for i=3,n_elements(scattdb_lower(*,0))-1 do begin
    for j=0,n_elements(scattdb_lower(0,*))-1 do $
      scattdb(i,j) = interpol([scattdb_lower(i,j),scattdb_upper(i,j)], $
      			[wnum_lower, wnum_upper], wnum)
  endfor

after_wnum_interpolation:

	; Now interpolate to the desired radius
  datar = reform(scattdb(2,*))		; Get the effective radius from the database
  datav = reform(scattdb(index,*))	; Get the desired data from the database
  		; Make sure we don't interpolate beyond array end
  if(r lt min(datar) or r gt max(datar)) then begin
    print,'The desired radius is outside the bounds of the radii in the database'
    stop,'Aborting inside get_scat_properties()'
  endif
  if(field ne 'pf') then value = interpol(datav, datar, r) else begin
    value = reform(datav(*,0))
    for i=0,n_elements(value)-1 do $
      value(i) = interpol(datav(i,*),datar,r)
  endelse

  return,value
end


