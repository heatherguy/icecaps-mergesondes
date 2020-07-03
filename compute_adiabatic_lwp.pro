; $Id: compute_adiabatic_lwp.pro,v 1.2 2006/12/18 15:21:12 dturner Exp $
;+
; Abstract:
;	This routine computes the cloud LWP in the adiabatic limit.  The needed
;    inputs are a temperature profile and the heights of the cloud base and top.
;    Note that the uncertainty in the cloud boundaries (assumed to be the same
;    value "delta" for both) is used to compute a minimum and maximum LWPs that are
;    possible by going from 
;		max_LWP = integral_LWC(cbh-delta to cth+delta)
;		min_LWP = integral_LWC(cbh+delta to cth-delta)
;
; Author:
;	Dave Turner
;		Pacific Northwest National Laboratory
;
; Date:
;	August 2004
;
; Returns:
;	Likely liquid water path [g m-2]
;	Minimum possible liquid water path [g m-2]
;	Maximum possible liquid water path [g m-2]
;
; Call:
    function compute_adiabatic_lwp, $		; Output structure defined above...
    		t_profile, $			; Temperature profile [degC]
		z_profile, $			; Height profile [km AGL]
		p_profile, $			; Pressure profile [mb]
		cbh, $				; Cloud base height [km AGL]
		cth, $				; Cloud top height [km AGL]
		delta, $			; The 1-sigma uncertainty in the height 
						;     of the cloud boundaries [km]
		dostop=dostop
;-

  if(n_elements(t_profile) ne n_elements(z_profile) or $
     n_elements(t_profile) ne n_elements(p_profile)) then begin
    print,'Error in compute_adiabatic_lwp: ' + $
    	'T_profile, P_profile, and Z_profie must be same length'
    return,{lwp:-999., max_lwp:-999., min_lwp:-999}
  endif

	; Define the density of dry air [kg / m3]
  foo = t2density(t_profile, p_profile, dens=dryair_density)

	; Define the moist adiabatic lapse rate, from Pruppacher and Klett
  lapse = -5.8		; [degC / km]

  	; Compute the likely LWP
  t_base  = interpol(t_profile, z_profile, cbh)	; Temperature at cloud base
  p_base  = interpol(p_profile, z_profile, cbh)	; Pressure at cloud base
  p_top   = interpol(p_profile, z_profile, cth)	; Pressure at cloud top
  		; Compute the temperature at cloud top assuming an adiabatic profile
  t_top   = (cth - cbh)*lapse + t_base
  ws_base = wsat(t_base, p_base)	; Saturated mixing ratio at cloud base [g/kg]
  ws_top  = wsat(t_top,  p_top)		; Saturated mixing ratio at cloud top  [g/kg]
  lwp     = (ws_base - ws_top) * dryair_density * (cth - cbh) * 1000.	; [g/m2]

  	; Compute the maximum LWP.  This assumes the worst case scenario leading 
	; to the thickest cloud possible within the uncertainties...
  t_base  = interpol(t_profile, z_profile, cbh-delta)	; Temperature at cloud base
  p_base  = interpol(p_profile, z_profile, cbh-delta)	; Pressure at cloud base
  p_top   = interpol(p_profile, z_profile, cth+delta)	; Pressure at cloud top
  		; Compute the temperature at cloud top assuming an adiabatic profile
  t_top   = ((cth+delta) - (cbh-delta))*lapse + t_base
  ws_base = wsat(t_base, p_base)	; Saturated mixing ratio at cloud base [g/kg]
  ws_top  = wsat(t_top,  p_top)		; Saturated mixing ratio at cloud top  [g/kg]
  lwp_max = (ws_base - ws_top) * dryair_density * $
  		((cth+delta) - (cbh-delta)) * 1000.	; [g/m2]

  	; Compute the minimum LWP.  This assumes the worst case scenario leading 
	; to the thickest cloud possible within the uncertainties...
  t_base  = interpol(t_profile, z_profile, cbh+delta)	; Temperature at cloud base
  p_base  = interpol(p_profile, z_profile, cbh+delta)	; Pressure at cloud base
  p_top   = interpol(p_profile, z_profile, cth-delta)	; Pressure at cloud top
  		; Compute the temperature at cloud top assuming an adiabatic profile
  t_top   = ((cth-delta) - (cbh+delta))*lapse + t_base
  ws_base = wsat(t_base, p_base)	; Saturated mixing ratio at cloud base [g/kg]
  ws_top  = wsat(t_top,  p_top)		; Saturated mixing ratio at cloud top  [g/kg]
  lwp_min = (ws_base - ws_top) * dryair_density * $
  		((cth-delta) - (cbh+delta)) * 1000.	; [g/m2]
		; Add a sanity check: if the cloud top used here is below the 
		; cloud base, then the LWP must be zero...
  if(((cth-delta) - (cbh+delta)) le 0) then lwp_min = 0.

	; And return the structure...
  if(keyword_set(dostop)) then stop,'Stopped inside routine'
  return, {lwp:lwp(0), max_lwp:lwp_max(0), min_lwp:lwp_min(0)>0}
end
