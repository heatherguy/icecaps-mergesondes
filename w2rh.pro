; $Id: w2rh.pro,v 1.5 2001/09/28 21:14:06 dturner Release_ddt_1_13 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;Abstract:
; This function calculates relative humidity given the mixing ratio, the
;	barametric pressure, and temperature.
;
;	Author: Dave Turner
;	Last modified: 4/29/97
;
;	INPUTS:	  w = mixing ratio, in g/kg
;		  p = pressure, in mb
;		  t = temperature, in C
;
; 	OUPUT:	  rh = relative humidity, as a fraction
; 
;	KEYWORDS: ICE  Set on if this is to be calcated over ice
; 
; Call:
	function w2rh, w, p, t, ice=ice
;-
  if(keyword_set(ice)) then ice=1 else ice=0

  ;;; This way is not the proper way to do this.  RH = e / e_sat, which is
  ;;; not the same as w / w_sat.
  ; es = esat(t, ice)
  ; es = es / 100.0
  ; ws = ((0.622 * es) / (p - es)) * 1000.0
  ; rh = w / ws

  e = w2e(w, p)
  es = esat(t, ice) / 100.0	; To convert it to the correct units (mb)
  rh = e / es

  return,rh
end


