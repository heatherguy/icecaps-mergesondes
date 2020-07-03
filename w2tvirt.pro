; $Id: w2tvirt.pro,v 1.2 1998/05/27 18:37:44 turner Release_ddt_1_13 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Abstract:
;   This function computes virtual temperature given the ambient temperature
; and the water vapor mixing ratio
;	
;  Author: Dave Turner
;  Last Modified: 12/97
;
; 	INPUTS:	   t = ambient temperature, in degC
;	  	   w = water vapor mixing ratio, in g/kg
;
;	OUTPUT:	   vt = virtual temperature, in degC
; 
; Call:
	function w2tvirt, t, w
;-

  tzero = 273.15

  temp = t + tzero	; Convert from degC to degK
  ww = w / 1000.0	; Convert from g/kg to g/g

  vt = temp * (1 + 0.61 * ww)
  vt = vt - tzero

  return, vt
end


