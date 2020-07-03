; Abstract: 
;	See below.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Abtract:
;	This function is the inverse of the hypsometric function; i.e., it takes
;    two heights and a reference pressure and returns the other pressure value.
;
; Author:
;	Dave Turner
;		Pacific Northwest National Laboratory
;
; Logic:
;	The inverse of the routine hypsometric.pro
;
; Call:
  function inv_hypsometric, $	; Array of pressures, in mb
  		z, $		; Array of heights, in km
		t, $ 		; Array of (virtual) temperatures, in K
		p0		; Pressure of the lower boundary, in mb
;-
       ; Quick QC
  foo = where(t le 0, nfoo)
  if(nfoo gt 0) then begin
    print,'ERROR in inv_hypsometric: Temperature is assumed to be Kelvin -- abort'
    return,-999
  endif

  a = 29.2911           ; Gas constant in dry air divided by the
                                ;   acceleration due to gravity
  temp = t
  p    = fltarr(n_elements(z))
  p(0) = p0 / 100.0     ; Convert from mb (hPa) to Pa
  zz   = z * 1000.	; Convert from km to m

  for i=1, n_elements(zz) -1 do $
    p(i) = p(i-1) / exp( 2*(zz(i)-zz(i-1)) / (a * (temp(i)+temp(i-1))))

  p = p * 100.		; Convert from Pa to hPa (mb)
  return, p
end


