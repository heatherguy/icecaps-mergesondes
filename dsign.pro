; $Id: dsign.pro,v 1.1 2004/05/17 16:10:56 dturner Exp $
;+
; Abstract
;	This routine returns the sign of the value: -1 if the number is negative, 
;    1 otherwise
;
; Author: 		Dave Turner, PNNL
; Date created:		May 2004
; Date last modified:	$Date: 2004/05/17 16:10:56 $
;
; Call:
	function dsign, value
;-

  if(n_elements(value) eq 1) then begin
    if(value lt 0) then return,-1 else return,1
  endif else begin
    vals = intarr(n_elements(value))
    for i=0,n_elements(value)-1 do $
      if(value(i) lt 0) then vals(i) = -1 else vals(i) = 1
    return,vals
  endelse
end

