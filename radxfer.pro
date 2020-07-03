; $Id: radxfer.pro,v 1.3 2002/08/07 14:35:04 dturner Release_ddt_1_13 $
;+
; Abstract:
;	Clear-sky downwelling radiance using optical depths previously calculated.
;    It uses a "linear in tau" approach.  Note that the profiles *MUST* extend from
;    the surface (index 0) to the TOA...
;
; Author:
;	Dave Turner
;
; Date:
;	July 2002
;
; Comment:
;	This routine was tested by using optical depths computed using the LBLRTM
;     with radiances computed by the same model.  The results were very good.  There was
;     still some minor disagreement in the center of the CO2 band, but it was MUCH better
;     that the previous version (1.1) that was linear-in-transmittance...
;
; Call:
    function radxfer, $		; downwelling radiance in mW / (m2 ster cm-1)
    		wnum, $		; wavenumber array 
		t, $ 		; temperature profile, one element longer than od profile [K]
		od, $		; optical depth profiles, one for each wavenumber
		sfc_t=sfc_t, $	; surface temperature [K]; only needed for upwelling calculations
		sfc_e=sfc_e, $	; surface emissivity; only needed for upwelling calculations
		upwelling=upwelling, $	; Set this to perform an upwelling calculation
		dostop=dostop
;-

  	; Some basic checks to ensure that the arrays are dimesioned correctly
  if(n_elements(wnum) eq 1) then begin
    opd = reform(od) 
    a = size(opd)
    if(a(0) ne 1) then begin
      print,'Error: Optical depth array dimension does not match wnum dimension'
      return,-1
    endif
  endif else begin
    opd = od
    a = size(opd)
    if(a(1) ne n_elements(wnum)) then begin
      opd = transpose(opd)
      a = size(opd)
      if(a(1) ne n_elements(wnum)) then begin
        print,'Error: Optical depth array dimension does not match wnum dimension'
        return,-3
      endif
    endif 
  endelse
  if(a(1) ne n_elements(t)-1) then begin
    if(a(0) eq 2) then begin
      opd = transpose(opd)
      a = size(opd)
    endif
    if(a(1) eq n_elements(t)-1) then looking_good = 1 $
    else begin
      print,'Error: Optical depth array dimension does not match temperature dimension'
      return,-2
    endelse
  endif
  foo = where(t lt 0, nfoo)
  if(nfoo gt 0) then begin
    print,'Error: temperature array has negative values; units should be Kelvin'
    return,-4
  endif

  if(keyword_set(upwelling)) then begin
    if(n_elements(sfc_e) eq 0) then sfce = 1.0 else sfce = sfc_e
    if(sfce lt 0 or sfce gt 1) then begin
      print,'Error: surface emissivity is outside [0,1]'
      return,-5
    endif
    		; If no surface temperature is given, then use temp of lowest level
    if(n_elements(sfc_t) eq 0) then sfct = t(0) else sfct = sfc_t

    		; Get the radiance for a blackbody emitting at sfct
    planck, wnum, sfct, sfc_b

    		; Set the indices for the loop
    k_start = 0
    k_end   = n_elements(opd(*,0))-1
    k_step  = 1
  endif else begin
  		; No surface emission since this is downwelling
    sfce   = 0.
    		; Set the indices for the loop
    k_start = n_elements(opd(*,0))-1
    k_end   = 0
    k_step  = -1
  endelse

  	; Now we are set to do the radiative transfer.  
	; Will use the linear in tau approach
  opd = transpose(opd) > 1.e-6			; Add a small fudge factor to avoid singularity
  rad = fltarr(n_elements(wnum)) * 0.
  for i=0,n_elements(rad)-1 do begin
    if(sfce gt 0) then rad(i) = sfc_b(i) * sfce

    for k=k_start, k_end, k_step do begin
      		; Get the transmission of the layer
      trans = exp(-1*opd(i,k))
      		; Compute the blackbody radiance at the boundary closest to the observer
      if(keyword_set(upwelling)) then planck, wnum(i), t(k+1), b_boundary $
      else planck, wnum(i), t(k), b_boundary
      		; Compute the blackbody radiance at the middle of the layer (average temp)
      planck, wnum(i), (t(k)+t(k+1))/2., b_avg
      		; Compute the radiance using Eq 13 in Clough et al 1992
      rad(i) = rad(i) * trans + $
      		(1.-trans) * (b_boundary+2*(b_avg-b_boundary)*(1./opd(i,k) - trans / (1.-trans)))
    endfor
  endfor

  if(keyword_set(dostop)) then stop,'Stopped in radxfer as desired'

  return, rad
end
