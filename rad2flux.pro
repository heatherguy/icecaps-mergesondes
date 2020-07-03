; $Id: rad2flux.pro,v 1.1 2005/05/17 20:18:00 dturner Exp $
;+
; Abstract:
;	This routine computes the radiative flux (for the hemisphere) from the
;    input vertical transmittance and vertical radiance data.  It is based upon
;    the work in Clough et al. ARM STM proceedings in 2000.  The coefficients 
;    are in the directory /home/dturner/work/tools/rad2flux.
;
; Author:
;	Dave Turner
;		Pacific Northwest National Laboratory
; Date:
;	May 2005
;
; Call:
  function rad2flux, $		; Returns the flux computed from the radiance
  	transmittance, $	; The vertical transmittance
	radiance, $		; The zenith/nadir radiance
	coef_file=coef_file, $	; The name of the coefficient file (default is below)
	best_angle=best_angle, $; The best angle to use for the diffusivity angle approx.
	dostop=dostop

  if(n_elements(coef_file) eq 0) then $
  	coef_file = '/home/dturner/work/tools/rad2flux/RAD-TO-FLUX-CONVERT2'
;-

  	; Read in the coefficient file
  files = findfile(coef_file, count=count)
  if(count ne 1) then begin
    print,'Error: Unable to find the coefficient file'
    return,-1
  endif
  openr,lun,files(0),/get_lun
  foo = ''
  readf,lun,foo
  nlines = long(foo)
  readf,lun,foo
  data = fltarr(4,nlines)
  readf,lun,data
  free_lun,lun

	; QC the transmittance and radiance spectrum
  trans = transmittance
  foo = where(trans lt 0, nfoo)
  if(nfoo gt 0) then begin
    print,'Warning: There are transmittances below zero.  Setting these to zero.'
    trans(foo) = 0.
  endif
  foo = where(trans gt 1, nfoo)
  if(nfoo gt 0) then begin
    print,'Warning: There are transmittances above one.  Setting these to one.'
    trans(foo) = 1.
  endif
  rad = radiance
  foo = where(rad lt 0, nfoo)
  if(nfoo gt 0) then begin
    print,'Warning: There are radiances below zero.  Setting these to zero.'
    rad(foo) = 0.
  endif

	; Compute the flux from the radiance using the conversion factor
  flux = fltarr(n_elements(rad))
  bang = fltarr(n_elements(rad))
  for i=0L,n_elements(rad)-1 do begin
    flux(i) = rad(i) * interpol(data(3,*),data(0,*),trans(i))
    bang(i) = interpol(data(2,*),data(0,*),trans(i))
  endfor

  if(keyword_set(dostop)) then stop,'Stopped inside the routine'
  best_angle = bang
  return,flux
end
