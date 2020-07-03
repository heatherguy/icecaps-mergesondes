; $Id: e2tvirt.pro,v 1.3 1998/05/27 18:17:49 turner Release_ddt_1_13 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Abstract:
;   This function computes virtual temperature given the ambient temperature,
; the barametric pressure, and the vapor pressure.
;	
;  Author: Dave Turner
;  Last Modified: 7/3/96
;
; 	INPUTS:	   t = ambient temperature, in degC
;		   p = barametric pressure, in mb
;	  	   e = vapor pressure, in mb
;
;	OUTPUT:	   vt = virtual temperature, in degC
;
; Call:
	function e2tvirt, t, e, p
;-

	; This comes directly from the ideal gas law
  vt = t * 1/(1 - 0.378 * (e / p))   

  return, vt
end


