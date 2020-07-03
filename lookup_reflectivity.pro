; $Id: lookup_reflectivity.pro,v 1.1 2004/08/17 21:14:47 dturner Exp $
;+
; Abstract:
;  	This routine uses a lookup table to compute the MMCR's reflectivity
;   for the given input cloud parameters.  It assumes that the radars wavelength
;   is 8.66 mm and that cloud is all liquid water at a temperature of 280 K.  
;   This routine uses lookup tables computed by the more robust script
;   compute_reflectivity.pro, which was driven repeatedly by the script
;   create_reflectivity_lookup_table.pro.
;
; Author:
;   Dave Turner
;   Pacific Northwest National Laboratory
;
; Date:
;   August 2004
;
; Call:
  function lookup_reflectivity, $ 	; Computed reflectivity [dBZ]
  	lwc, $				; Input liquid water content [g/m3]
	reff, $				; Input effective radius [um]
	size_distribution, $		; Input size distribution form:
					;    0 -> modified gamma
					;    1 -> log-normal
	width, $			; The width parameter for the size distribution
	path=path, $			; The path to the lookup tables
	dostop=dostop			; Set this to stop in the routine

  if(n_elements(path) eq 0) then path = '/home/dturner/vip/info'
;-

  sd = fix(float(size_distribution)+0.5)
  if(sd ne 0 and sd ne 1) then begin
    print,'Error: Undefined size distribution'
    if(keyword_set(dostop)) then stop,'Here'
    return,-9999.
  endif

  sd_type  = ['Gamma','Lognormal']
  filename = path + '/reflectivity_lookup_table.' + sd_type(sd) + '.' + $
          string(format='(I0,"p",I0)',fix(width), $
	          fix(1000*(width-fix(width))+0.5))
  fname = findfile(filename, count=count)
  if(count ne 1) then begin
    print,'Error: Undefined lookup table ' + filename
    if(keyword_set(dostop)) then stop,'Here'
    return,-9999.
  endif
  restore,fname(0)

  	; Now use linear interpolation to get our desired point
  foo = where(lwc lt lu_lwc, nfoo)
  if(nfoo eq 0) then begin
    print,'Error: Lookup table is too small in LWC space'
    return,-9999.
  endif
  idx_a1 = foo(0)
  foo = where(lu_lwc le lwc, nfoo)
  if(nfoo eq 0) then begin
    print,'Error: Lookup table is too small in LWC space'
    return,-9999.
  endif
  idx_a0 = foo(nfoo-1)
  sf = (lwc - lu_lwc(idx_a0)) / (lu_lwc(idx_a1) - lu_lwc(idx_a0))
  reff_dbz = sf * lu_dbz(*,idx_a1) + (1-sf) * lu_dbz(*,idx_a0)
  dbz = interpol(reff_dbz, lu_reff, reff)

  if(keyword_set(dostop)) then stop,'Here'
  return, dbz
end
