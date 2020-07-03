; Written by Tim Shippert
;
; $Id: t_zt2hhmmss.pro,v 1.1 2000/02/04 19:28:47 turner Release_ddt_1_13 $
;
; $Log: t_zt2hhmmss.pro,v $
; Revision 1.1  2000/02/04 19:28:47  turner
; Initial revision
;
;
; Take zeb time, return yymmdd and hhmmss
;+
PRO t_zt2hhmmss, BT, TO, dateYYMMDD, timeHHMMSS, YY=YY
; If you set /YY, it returns a two digit year , otherwise, YYYYMMDD
;-
  secPerDay = Long(24L*60L*60L)

  ;; get IDL julian day of day 1 for zeb
  day1 = julday(1,1,1970)  

  ;; get julian day of first time sample
  jday_foo = long(day1 + (BT + TO[0])/secPerDay)

  ;; now find the day and year for this day
  caldat, jday_foo, mm, dd, yyyy

  IF (n_elements(YY) GT 0) THEN BEGIN 
    dateYYMMDD = long((yyyy mod 100)*10000L + mm*100L + dd)
  ENDIF ELSE BEGIN
    dateYYMMDD = long(yyyy*10000L + mm*100L + dd)
  ENDELSE 
  
  ;; now find the time
  ss = long(BT+TO) MOD secPerDay
  
  hh=long(ss/3600)
  ss=ss MOD 3600
  mm = long(ss/60)
  ss = long(ss MOD 60)

  timeHHMMSS = long (hh*10000L + mm*100L + ss*1L)

  return
END 
  
  

  
