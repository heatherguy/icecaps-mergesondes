; $Id: rh2pwv.pro,v 1.1 1999/01/05 20:52:04 turner Release_ddt_1_13 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Abstract:
;    This routine calculates total precipitable water vapor from arrays of
;       relative humidity, pressure, and temperature.  It is the same routine
;       as that used in many of the VAPs.
; 
; Author: Dave Turner, PNNL
; Date:	  October, 1997
;
; Arguments:
;	RH:	Array of relative humidities (%)
;	T:	Array of temperatures	     (C)
;	Z:	Array of altitudes	     (km)
;
; Output:	
;	PWV	Total precipitable water vapor (cm)
;
;  function rh2pwv, rh, t, z
;-
function rh2pwv, rh, t, z

  u  = rh / 100.0	; Convert rh from % to a fraction

  	; Now calculate the vapor density with rh
  d = t * 0
  for i=0,n_elements(t) -1 do $
    d(i) = edens(t(i), u(i))

        ; Now integrate this density profile with altitude
  npts = n_elements(z)
  delta_alt = z * 0
  for i=1, npts -1 do delta_alt(i) = z(i) - z(i-1)
 
  pwv = 0.0
  for i=1, npts -1 do begin
    if(abs(d(i) - d(i-1)) lt 1.0e-9) then layer = d(i) $
    else if((d(i-1) le 0) or (d(i) le 0)) then layer = 0 $
    else layer = (d(i) - d(i-1)) / alog(d(i) / d(i-1))
    pwv = pwv + layer * delta_alt(i)
  endfor
 
        ; Convert the integrated water vapor densities to precipitable cm
  pwv = pwv * 0.1

  return, pwv
end
