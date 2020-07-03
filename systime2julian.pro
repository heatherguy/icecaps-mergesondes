; $Id: systime2julian.pro,v 1.5 2004/07/27 20:35:07 dturner Exp $
;***********************************************************************************
;+
; Abstract:
;   This routine computes the julian time (day.dayfraction since the first of the
;        given year) from the number of seconds since 1-Jan-1970 (systime).
;
; Author: Dave Turner, PNNL
;
; Call:
    pro systime2julian, time, yyyy, jultime
;-

  systime2ymdhms, time(0), yy, mm, dd, hh, nn, ss

  sday = julday(1,1,long(yyyy))
  jday = julday(mm, dd, yy, hh, nn, ss) + 0.5D

  jultime = (jday - sday + 1) + (time-time(0))/(24.*60.*60D)

  return
end

