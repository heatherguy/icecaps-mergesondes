; $Id: w2e.pro,v 1.2 2001/09/28 21:36:54 dturner Release_ddt_1_13 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Abstract:
;   This function converts water vapor mixing ratio to the partial pressure
; of water vapor
;
;	Author: Dave Turner
;	Last modified: 9/2001
;
;	INPUTS:    
;	  	   w = mixing ratio, in g/kg
;		   p = barametric pressure, in mb
;
;	OUTPUT:	   
;		   e = vapor pressure, in mb
;
; Call
	function w2e, w, p
;-
  
  ww = w / 1000.D		; Convert g/kg to g/g.  Also use a temporary variable.
  e = p * ww / (0.622 + ww)
  return,e
end


