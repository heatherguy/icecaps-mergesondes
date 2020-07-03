; $Id: increment_date.pro,v 1.1 2003/12/12 17:01:52 dturner Exp $
;***********************************************************************************
;+
;Abstract:
; This routine returns the date (YYYYMMDD) for the day some increment away from 
;    the entered date. 
;
; Call:
    function increment_date, date, increment
;-
 
  if(increment eq 0) then return, date $
  else if(increment gt 0) then isign = 1 $
  else isign = -1
  incr = abs(increment)

  	; Make sure input date is a long integer
  odate = long(date)

  	; Start the iterations for the "recursion"
  for i=0,incr-1 do begin

  		; Decompose to get the year, month, and day
    yy = odate / 10000L
    md = odate - yy*10000L
    mm = md / 100
    dd = md - mm*100

  		; Now modify by the increment
    dd = dd + isign

		; Now perform the tests to make sure the day is "realistic"
    if(dd le 0) then begin
      mm = mm - 1 
      if(mm eq 2 and yy mod 4 eq 0) then dd = 29 $
      else if(mm eq 2 and yy mod 4 ne 0) then dd = 28 $
      else if(mm eq 4 or mm eq 6 or mm eq 9 or mm eq 11) then dd = 30 $
      else dd = 31
    endif else if(dd gt 31) then begin
      dd = 1
      mm = mm + 1
    endif else if(dd gt 30 and (mm eq 4 or mm eq 6 or mm eq 9 or mm eq 11)) then begin
      dd = 1 
      mm = mm + 1
    endif else if(dd gt 29 and mm eq 2 and yy mod 4 eq 0) then begin
      dd = 1 
      mm = mm + 1
    endif else if(dd gt 28 and mm eq 2 and yy mod 4 ne 0) then begin
      dd = 1 
      mm = mm + 1
    endif 

		; Now perform the tests to make sure the month is "realistic"
    if(mm eq 0) then begin
      mm = 12
      yy = yy - 1
    endif else if(mm gt 12) then begin
      mm = 1
      yy = yy + 1
    endif
	
		; Recompose the new date
    odate = yy*10000L + mm*100 + dd
  endfor

  return, odate
end

