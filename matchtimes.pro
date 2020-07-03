; $Id: matchtimes.pro,v 1.6 1999/03/05 20:39:34 turner Release_ddt_1_13 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;  Dave Turner
;  Last modified: March 5, 1999
;
;Abstract:
;  This function takes two arrays of julian times, and compares the two,
;  	flagging the indices (in a new array) of the second that have 
;	matches in the first.  This allows the user to quickly reduce the
;	second array of times to have a subset of the first.  The user must
;	specify the delta that can exist between any two times.  If the
;	delta (in julian day) is too large and/or two or more values in the 
;	second array match a sample in the first, the procedure aborts.
;
;		Example: a = [ 0, 1, 5, 7]
;			 b = [ 1, 2, 3, 5, 7, 8]
;		   and we call matchTimes2, a, b, 0.5, d then
;		   a and b remain the same, and d will be
;			 d = [ 1, 0, 0, 1, 1, 0]
;  Note: is is possible for that the last parameter d will contain less 1's than 
;	the length of the vector a, as there may be one or more times that are
;	in a that are not in b.  
;
;  Main modification in the 3/5/99 version:  logic has been implemented to 
;       prevent the return of one-to-many matches as well as many-to-one matches.
;       In other words, each time in time2 is matched to a _single_ time in time1
;       if and only if the delta between the two is less than that specified, and
;       each element in time1 can at most be associated with only one element
;       in time2.  Some speed was sacrificed in order to make this so.
;
; CALL:
pro matchtimes, time1, time2, delta, flag
	; Possible values for delta:
	;		0.000025  -- two seconds		 in julian time
	;		0.005	  -- 7 minutes, 11 seconds	 in julian time
	;		0.025	  -- ~36 minutes		 in julian time
;-

  flag = intarr(n_elements(time2))
  indx = lonarr(n_elements(time2)) * 0 -1
 
  for i=0L,n_elements(flag) -1 do begin
    foo = where(min(abs(time1 - time2(i))) eq abs(time1-time2(i)),nfoo)
    if(nfoo eq 0) then stop, 'How did this happen???'
    if(abs(time1(foo(0)) - time2(i)) le delta) then begin
      flag(i) = 1
      indx(i) = foo(0)
    endif
  endfor
 
                ; By using the min distance above, we have eliminated the
                ;    one-to-many scenario.  We now need to eliminate the
                ;    many-to-one possibility.
  vals = indx(uniq(indx, sort(indx)))
  foo = where(vals ge 0, nfoo)
  for i=0L, nfoo - 1 do begin
    bar = where(indx eq vals(foo(i)), nbar)
    if(nbar gt 1) then begin
      duh = where(min(abs(time1(vals(foo(i))) - time2(bar))) eq $
                        abs(time1(vals(foo(i))) - time2(bar)), nduh)
      if(nduh le 0) then stop, 'This should not have happened'
      flag(bar) = 0             ; reset to zero
      flag(bar(duh(0))) = 1     ; and set only one
    endif
  endfor
 
end

