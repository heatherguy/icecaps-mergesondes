; $Id: apodize_kaiser_bessel.pro,v 1.2 2002/01/14 16:59:50 dturner Release_ddt_1_13 $
;+
; Abstract:
;	Kaiser-Bessel apodization function
; 
; Author:
;	Dave Turner
;		Pacific Northwest National Laboratory
;		University of Wisconsin - Madison
;
; Date:
;	January 2002
;
; Logic from Dave Tobin, SSEC/UW-Madison.  Very similar to apodize_norton_beer.
;
; Call:
    function apodize_kaiser_bessel, $	; Returns apodization function
                 n, $		; Length of apodization function (double sided ifg)
                 md, $		; Index to point where opd = MOPD (for single sided ifg)
		 k=k		; Kaiser-Bessel parameter (> 0).  Default is 6.
;-
 
  n = long(n)
  if(n_elements(k) le 0) then k = 6
  k = fix(k)
  if(k lt 1) then begin
    print,'ERROR in apodize_kaiser_bessel: Coefficient must be an integer > 6'
    stop,'ABORTING'
  endif

  apod = replicate(double(0), n)
  d    = findgen(n/2)
  d    = d / double(max(d))
  d    = d * (n/2)
  x    = k * sqrt(1 - (d/double(md))^2)
  r    = replicate(1.d, n/2)
  f    = 1L
  for j=1,8 do begin
    f = f * j
    r = r + (x/2)^(2L*j) / f^2
  endfor

  s = 1.
  f = 1L
  for j=1,8 do begin
    f = f * j
    s = s + (k/2.)^(2L*j) / f^2
  endfor

  foo = where(abs(d) le md)
  c = 1 * r(foo) / s
  apod(0:n/2-1) = c

  if(n mod 2 eq 0) then $
    apod(n/2:n-1) = reverse(apod(0:n/2-1)) $
  else $
    apod(n/2:n-1) = reverse(apod(0:n/2)) 

  return, apod

end
