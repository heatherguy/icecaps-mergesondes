; $Id: hypsometric.pro,v 1.3 2002/01/18 13:02:38 dturner Release_ddt_1_13 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Abstract:
;   This function computes the geometric altitude using the hypsometric
; equation, given pressure and virtual temperature profiles, and the
; initial altitude.
;
; Note: the inverse to this is the script calc_p.pro in this directory.
;	
;  Author: Dave Turner
;  Last Modified: 12/97
;
; Call:
    function hypsometric, $          ; Array of heights, in km
                 p, $                ; Array of pressures, in mb
                 t, $                ; Array of (virtual) temperatures, in K
                 z0                  ; Height of lower boundary, in m
;-

  	; Quick QC
  foo = where(t le 0, nfoo)
  if(nfoo gt 0) then begin
    print,'ERROR in hypsometric: Temperature is assumed to be Kelvin -- abort'
    return,-999
  endif

  a = 29.2911		; Gas constant in dry air divided by the 
				;   acceleration due to gravity
  temp = t 	
  pres = p / 100.0	; Convert from mb (hPa) to Pa
  z = fltarr(n_elements(pres))
  z(0) = z0 * 1000.	; Convert from km to m

  for i=1, n_elements(pres) -1 do $
    z(i) = a * (temp(i) + temp(i-1))/2.0 * alog(pres(i-1)/float(pres(i))) + z(i-1)

  z = z / 1000.		; Convert from m to km
  return, z
end


