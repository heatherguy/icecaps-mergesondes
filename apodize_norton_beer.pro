; $Id: apodize_norton_beer.pro,v 1.1 2002/01/14 16:53:11 dturner Release_ddt_1_13 $
;+
; Abstract:
;	Norton-Beer apodization function
; 
; Author:
;	Dave Turner
;		Pacific Northwest National Laboratory
;		University of Wisconsin - Madison
;
; Date:
;	January 2002
;
; Logic from Dave Tobin, SSEC/UW-Madison
;
; Call:
    function apodize_norton_beer, $	; Returns apodization function
                 n, $		; Length of apodization function (double sided ifg)
                 md		; Index to point where opd = MOPD (for single sided ifg)
;-
 
  beer = replicate(double(0), n)
  beer(0) = 1.
  for i=1,n/2-1 do begin
;    if i le md then beer(i) = (1-((i-1)/MD)^2)^2 $
    if i le md then beer(i) = (1-((i-1)/double(MD))^2)^2 $
    else beer(i) = 0.
  endfor

  if(n mod 2 eq 0) then $
    beer(n/2:n-1) = reverse(beer(0:n/2-1)) $
  else $
    beer(n/2:n-1) = reverse(beer(0:n/2))
  return, beer
end
