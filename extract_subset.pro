; $Id: extract_subset.pro,v 1.4 1997/11/11 21:26:07 d3h797 Release_ddt_1_13 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;  Dave Turner
;  Last modified: December 29, 1996
;
;Abstract:
;  This function extracts a subset of an list of an array, based on looking
;	for a particular value in another array of same length.  For example,
;	suppose the two arrays are:
;		a = [ 1, 2, 3, 4, 5, 6]
;		b = [ 1, 0, 1, 1, 0, 1]
;	and we called the function a = extract_subset(a, b, 1), then we have
;		a = [ 1, 3, 4, 6]
;  Note:  b must be a one dimensional array, but if a is more than one dimensional
;	then this we will loop over the different dimensions of a.  For example:
;		a = [[ 1, 2, 3, 4, 5, 6], [ 9, 8, 7, 6, 5, 4]]	(transpose first)
;		with b above will cause the function to return
;		a = [[ 1, 3, 4, 6], [ 9, 7, 6, 4]]		(transpose)
;
;  This new function replaces the old version, as this one takes advantage of
;  	internal IDL functions to speed up its processing.  It performs 
;	exactly as the old "extract_subset" function does.
;
; CALL:
function extract_subset, a, b, val
;-

  if((size(a))(0) eq 1) then begin
    if (N_ELEMENTS(a) ne N_ELEMENTS(b)) then stop, $
		"Arrays passed into extract_subset are not the same length"
    tmp = a(where(b eq val))
  endif else if((size(a))(0) eq 2) then begin
    sz = n_elements(where(b eq val))
    tmp = dblarr((size(a))(1),sz)
    for j = long(0), long((size(a))(1) - 1) do begin
      if (N_ELEMENTS(a(j,*)) ne N_ELEMENTS(b)) then stop, $
		"Arrays passed into extract_subset are not the same length"
      tmp(j,*) = a(j,where(b eq val))
    endfor
  endif else if((size(a))(0) eq 3) then begin
    sz = n_elements(where(b eq val))
    tmp = fltarr((size(a))(1), (size(a))(2), sz)
    for j = long(0), long((size(a))(1) - 1) do begin
      for k = long(0), long((size(a))(2) - 1) do begin
        if (N_ELEMENTS(a(j,k,*)) ne N_ELEMENTS(b)) then stop, $
		"Arrays passed into extract_subset are not the same length"
	tmp(j,k,*) = a(j,k,where(b eq val))
      endfor
    endfor
  endif else stop, "A is more than three dimensional in extract_subset"

  return, tmp
end


