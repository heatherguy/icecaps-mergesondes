; $Id: ymd2dow.pro,v 1.3 2000/03/21 22:02:29 turner Release_ddt_1_13 $
;
;+ 
; Abstract:
;	This routine calculates the day of the week, given the yy, mm, dd.
;
; Author:
;	Dave Turner, PNNL
;
; Date last modified:
;	$Date: 2000/03/21 22:02:29 $
;
; Call:
	pro ymd2dow, yyyy, mm, dd, dow
;-

  dow_strings = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat']

  nows = systime(0)
  nowd = bin_date(nows)
  nowjt = julday(nowd(1), nowd(2), nowd(0))

  parts = str_sep(nows, ' ')
  offset = where(parts(0) eq dow_strings, found)
  if(found ne 1) then stop, 'Unable to determine the day of the week from today!'

  jt = lonarr(n_elements(yyyy))
  for i=0, n_elements(yyyy) -1 do $
    jt(i) = julday(mm(i), dd(i), yyyy(i))

  ndays = long(jt - nowjt)

  minv = min(ndays)
  minv = (minv / 7 - 1) * 7

  ndays = ((-1)*minv + offset(0) - (long(nowjt) - long(jt))) mod 7

  dow = dow_strings(ndays)

  return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro test_ymd2dow

  ;;  These are the days we will test the routine on.  The array ndow contains 
  ;;     the correct answers we hope the routine ymd2dow will return.
  nyy = [1996,1996,1996,1997,1998,1998,1999,1999, 1999, 1999, 1999, 1999, 1999, 1999]
  nmm = [2,2,3,1,11,12,1,1,1,1,1,1,1,2]
  ndd = [28,29,1,1,30,1,9,10,11,12,25,26,27,1]
  ndow= ['Wed','Thu','Fri','Wed','Mon','Tue','Sat','Sun','Mon','Tue','Mon', $
  			'Tue','Wed','Mon']

  ymd2dow, nyy, nmm, ndd, dow

  print,'------------------------------'
  print,'/--- Actual ----\    DOW from'
  print,'YYYY  MM  DD  DOW    function'
  print,'------------------------------'
  for i=0, n_elements(nyy) -1 do $
	print,string(format='(I0)',nyy(i)) + '  ' + $
	      string(format='(I2)',nmm(i)) + '  ' + $ 
	      string(format='(I2)',ndd(i)) + '  ' + $
	      ndow(i) + '      ' + dow(i) 
  print,'------------------------------'
  return
end

