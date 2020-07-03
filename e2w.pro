; $Id: e2w.pro,v 1.3 1998/05/27 18:18:56 turner Release_ddt_1_13 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Abstract:
;   This function provides an alternate way to calculate mixing ratio, when 
; given barametric pressure and vapor pressure.
;
;	Author: Dave Turner
;	Last modified: 7/3/96
;
;	INPUTS:    e = vapor pressure, in mb
;		   p = barametric pressure, in mb
;
;	OUTPUT:	   w = mixing ratio, in g/kg
;
; Call
	function e2w, e, p
;-
; function mxrt_ep, e, p  -- old function call
  
  w = 0.622 * e / (p - e)	; This ratio is in g/g
  w = w * 1000			; Convert g/g to g/kg

  return,w
end


