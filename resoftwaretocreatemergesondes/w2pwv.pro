; $Id: w2pwv.pro,v 1.6 2001/11/15 14:29:22 dturner Release_ddt_1_13 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Abstract:
;    This routine calculates total precipitable water from mixing ratio and
;     	pressure profiles.
;
; Author: Dave Turner, PNNL  (copied from a routine from Rich Ferrare, NASA/LaRC)
; Date:   October, 1997
;
; Arguments:
;    	W:		1-D Profile of mixing ratio (g/kg)
;   	P:		1-D Profile of pressure 	(mb)
;
; Keywords:
;	PWV_ERR:	The error in PWV, calculated from W_ERR
;	W_ERR:		Profile of error in the mixing ratio (g/kg)
;	ERR_THRES:	Maximum error (in %) to allow in the calculation of PWV, 
;				when the W_ERR profile exists.  Default is 25%
;	MAX_HT_INDEX:	The maximum height index to use in the computation of PWV
;	PWV_MAX_HT_INDEX:  The maximum height index used in the computation of 
;				the PWV -- may be different than MAX_HT_INDEX 
;				because of the error profile
;	MIN_W:		The minimum value allowed in the mixing ratio profile.
;				The default is -5 g/kg (allowing for noise)
;	MIN_W_ERR:	The minimum value allowed in the error profile.
;				The default value is 0
;	PWV_PROFILE:	A profile of PWV from the surface up.  
;	
; Output:
;	PWV:		Total precipitable water vapor (cm)
;
; Call:
	function w2pwv, w, p, $
		pwv_err=pwv_err, w_err=w_err, err_thres=err_thres, $
		max_ht_index=max_ht_index, pwv_max_ht_index=pwv_max_ht_index, $
		min_w=min_w, min_w_err=min_w_err, pwv_profile=pwv_profile
;-

	; Handle the keywords, if they exist
  if(n_elements(w_err) ne n_elements(w)) then err = w * 0 else err = w_err
  if(n_elements(err_thres) eq 0) then err_thres = 25
  if(n_elements(max_ht_index) eq 0) then max_ht_index = n_elements(p) $
  else if(max_ht_index le 0) then max_ht_index = n_elements(p)
  if(n_elements(min_w_err) ne 1) then min_w_err = 0
  if(n_elements(min_w) ne 1) then min_w = -5

  w1 = w
  p1 = p
  err1 = err
  index = indgen(n_elements(p))
  error = abs(100.0 * err / w)
  foo = where(((finite(error) eq 1) and (err ge min_w_err) and $
  	(error gt err_thres) and (p lt 750)) or (index gt max_ht_index), nfoo)
  if(nfoo gt 0) then begin
    w1(foo(0):n_elements(w1)-1) = -999
    p1(foo(0):n_elements(w1)-1) = -999
    err1(foo(0):n_elements(w1)-1) = -999
    pwv_max_ht_index = index(foo(0))
  endif else pwv_max_ht_index = max_ht_index

		; Don't allow any bad error values (which are probably missing
		;  values due to the merging of different RL channels) whack us out
  if(nfoo gt 0) then first_bad_point = foo(0) $
  else first_bad_point = n_elements(err1)

  foo = where((err1 lt min_w_err) and (index le first_bad_point), nfoo)
  if(nfoo gt 0) then err1(foo) = min_w_err

		; Look for any values where the mixing ratio is bad or missing
		;   and delete these.  By deleting the same levels in the error
		;   profile, we will help ensure that the error is representative.
  foo = where(w1 lt min_w, nfoo)
  if(nfoo gt 0) then begin
    w1(foo) = -999
    err1(foo) = -999
    p1(foo) = -999
  endif

	;		Equation used:
	;	  PWV = 1/g * integral( q dp )   

  np = p1 / 10.0	; Convert pressure from mb (hPa) to kPa
  r  = w1   / 1000.0	; Convert from g/kg to g/g
  re = err1 / 1000.0	; Convert the error from g/kg to g/g
  q  = r  / (1.0 + r)	; Convert mixing ratio to specific humidity
  qe = re / (1.0 + re)	; Convert the error into specific humidity
;  qe = re * (r / (1.0 + r)^2 )

  pwv_profile = fltarr(n_elements(w1)) * 0 - 999

  foo = where(w1 gt -800, nfoo)
  bar = where((err1 gt -800) and (w1 gt -800), nbar)
  if(nfoo eq 0) then begin
    pwv = -999.
    pwv_err = -999.
  endif else begin

    			; Calculate the PWV
    pwv = 0
    for i=1, nfoo -1 do begin
      pwv = pwv + ((q(foo(i)) + q(foo(i-1)))/2.0) * (np(foo(i-1)) - np(foo(i)))
      pwv_profile(foo(i)) = pwv
    endfor
    
    			; Calculate the PWV error
    if(nbar eq 0) then pwv_err = -999 $
    else begin
      if(nbar(0) eq 0) then pwv_err = 0 $
      else begin
			; If the first valid error value is not the first point
			;   in the error profile, extend the first valid value
			;   down to the surface by assuming the same error at the
			;   surface as there is at the first valid point
        pwv_err = 2*qe(bar(0))^2 * ((np(0) - np(bar(0)))/2.0)^2
      endelse 

      			; Compute the rest of the error
      for i=1, nbar -1 do begin
        pwv_err = pwv_err + (qe(bar(i))^2 + qe(bar(i-1))^2) * $
			(((np(bar(i-1)) - np(bar(i)))/2.0)^2)
      endfor
    endelse
  endelse

		; Account for the gravitational constant, and convert
		;   to the right units
  pwv = pwv / 9.8		; Correct for G
  pwv = pwv * 100.0		; Convert from m to cm

  foo = where(pwv_profile le 0, nfoo)
  pwv_profile = pwv_profile / 9.8	; Correct for G
  pwv_profile = pwv_profile * 100.0	; Convert from m to cm
  if(nfoo gt 0) then pwv_profile(foo) = -999
 
  pwv_err = pwv_err / (9.8)^2	; Correct for G
  pwv_err = pwv_err * (100.0)^2	; Convert from m to cm
  
  pwv_err = sqrt(pwv_err)

  return, pwv
end

