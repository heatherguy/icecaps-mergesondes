; $Id: rh2w.pro,v 1.4 1999/02/18 15:06:07 turner Release_ddt_1_13 $
;***********************************************************************************
;+
;Abstract:
; This function computes the mixing ratio of water (ratio of the mass of water 
;	vapor to the mass of dry air) from the relative humidity
;     Original fortran code written by Jim Liljegren
;     Copied into IDL by Dave Turner
;     Copy Date:    April 19, 1996
;
;     INPUTS:	temp = the ambient temperature, in Celcius
;		rh = the relative humidity, as a fraction
;		pres = the atmospheric pressure, in mb
;
;     OUTPUTS:	w = mixing ratio of water to dry air, in g/kg
;
;     KEYWORDS:	rh_err = the error in the relative humidity profile
;	        sonde_rh_err: If this keyword is set, then use the standard
;			uncertainties for the error in the sonde RH profile
;		w_err  = the derived error in the water vapor mixing ratio 
;			profile, assuming the only contribution to the error 
;			is from the RH profile
;     CALLS: 	epres() - to compute the vapor pressure
;
; Call:
	function rh2w, temp, rh, pres, $
		rh_err=rh_err, sonde_rh_err=sonde_rh_err, w_err=w_err
;-

  e = epres(temp, rh)	; Get the vapor pressure
  e = e / 100		; Convert the vapor pressure to mb (same as pres)
  w = 0.622 * e / (pres - e)   ; this ratio is in g/g
  w = w * 1000		; convert to g/kg

  if(keyword_set(sonde_rh_err)) then begin
    rh_err = replicate(0.02, n_elements(pres))
    foo = where(rh gt 0.80, nfoo)
    if(nfoo gt 0) then rh_err(foo) = 0.03
  endif
  if(n_elements(rh_err) gt 0) then begin
    if(n_elements(rh_err) ne n_elements(rh)) then stop, 'Whoaa!  Error in rh2w!'
    dp = replicate(0.5, n_elements(pres))
    dp2 = dp^2
    du = rh_err
    du2 = du^2
    e  = epres(temp,rh)/100.0
    de2 = du2 * esat(temp,0)^2 / 100^2

    dw2 = ( de2 * (pres/(e*(pres-e)))^2 + dp2/(pres-e)^2 ) * w^2
    dw  = sqrt(dw2)

    w_err = dw
  endif

  return, w
end

