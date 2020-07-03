; $Id: ymdhms2julian.pro,v 1.3 2006/05/22 19:17:04 dturner Exp $
;***********************************************************************************
;+
;  Abstract:
;     This routine computes the julian time (day.dayfraction since the first of the
;          year) from the arrays of YYYY, MM, DD, HH, Min, SS
;
; Author: Dave Turner, PNNL
;
; Call:
    pro ymdhms2julian, yy, mm, dd, hh, nn, ss, jultime
;-

  sday = julday(1,1,byte(yy(0)))
  jday = dblarr(n_elements(yy))
  for i=0L, n_elements(yy) -1 do $
    jday(i) = julday(byte(mm(i)), byte(dd(i)), byte(yy(i)))
  jultime = (jday - sday + 1) + hh/24.0D + nn/(24*60.0D) + ss/(24*60*60.0D)

  return
end


