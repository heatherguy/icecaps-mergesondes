; $Id: ui2jt.pro,v 1.3 1998/05/27 18:36:46 turner Release_ddt_1_13 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Abstract:
;	This function converts a date/time in YYMMDD and HHMMSS into julian day.
; 
; Author: Dave Turner, PNNL
; Date:   September, 1997
;
; Arguments:
;     YYMMDD	An array of dates
;     HHMMSS	An array of times
;			Note that both of these should be the same length
; Caveats:
;	This routine assumes all of the data are from the same year.  Crossing
;	   yearly boundaries has the potential to screw up the julian calculation
;	   especially concerning leap years.
;
; Call:
	function ui2jt, yymmdd, hhmmss
;-

	; The number of days elapsed at the beginning of each month
  month = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]
  
  yy = fix(yymmdd / 10000L)
  if((yy(0) mod 4) eq 0) then month(1) = 29
  mmdd = yymmdd - yy*10000L
  mm = fix(mmdd / 100L)
  dd = mmdd - mm*100L
  
  hours = hhmmss2hh(hhmmss)
  hours = hours/24.0

  jt = month(mm-1) + dd + hours
  return, jt
end
