; Writeen by Tim Shippert
;
; $Id: t_zt2julian.pro,v 1.1 2000/02/04 21:55:17 shippert Release_ddt_1_13 $
;
; $Log: t_zt2julian.pro,v $
; Revision 1.1  2000/02/04 21:55:17  shippert
; Initial revision
;
;
; Takes basetime and time offset, and returns julian days from this year
;+
PRO t_zt2julian, BT, TO, jt
;-

  secPerDay = Long(24L*60L*60L)

  ;; get IDL julian day of day 1 for zeb
  day1 = julday(1,1,1970)

  ;; get julian day of first time sample
  jday_foo = day1 + (BT + TO[0])/secPerDay

  ;; now find the year for this day
  caldat, jday_foo, mm, dd, yyyy

  ;; so get julian day for first day of this year
  jday_1 = julday(1,1,yyyy)

  ;; add it all up
  jt = 1.0D + double(day1) + double(BT + TO)/double(secPerDay) - double(jday_1)
  return
END 
  
