; $Id: hhmmss2hh.pro,v 1.3 1998/05/27 18:23:45 turner Release_ddt_1_13 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;Abstract:
;   This little routine converts time in hhmmss format to hours plus hour fraction
;	I.e., 123000 is returned as 12.5
;
; Author: Dave Turner
; Date:   June 16, 1997
;
; Call:
	function hhmmss2hh, hms
;-

	; Since hhmmss is very regular, let's break it into pieces
  h = fix(hms/10000L)
  ms = hms - h*10000L
  m = fix(ms/100L)
  s = ms - m*100L
	; Recombine the pieces
  hour = h + m/60.0 + s/3600.0

  return,hour
end
