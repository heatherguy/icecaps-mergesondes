; $Id: julian2ymdhms.pro,v 1.2 2001/09/13 12:00:16 turner Release_ddt_1_13 $
;***********************************************************************************
;+
;  Abstract:
;     This routine computes the YYYY, MM, DD, HH, Min, SS from the first of the
;     	   given year (4-digit notation) from the julian time (day.dayfraction).
;
; Author: Dave Turner, PNNL
;
; Call:
    pro julian2ymdhms, year, jultime, yy, mm, dd, hh, nn, ss
;-

  ndays = long(jultime)
  jday0 = julday(1,1,year)
  yy = intarr(n_elements(jultime))
  mm = intarr(n_elements(jultime))
  dd = intarr(n_elements(jultime))
  hh = intarr(n_elements(jultime))
  nn = intarr(n_elements(jultime))
  ss = intarr(n_elements(jultime))
  for i=0L, n_elements(jultime) -1 do begin
    caldat, jday0+ndays(i)-1, tmm, tdd, tyy
    yy(i) = tyy
    mm(i) = tmm
    dd(i) = tdd
  endfor

  fraction = jultime - ndays
  hh = fix(fraction * 24 + 0.00035)
  fraction = fraction * 24 - hh
  nn = fix(fraction * 60 + 0.009)
  fraction = fraction * 60 - nn
  ss = fix(fraction * 60 + 0.5)

  return
end


