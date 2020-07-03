; $Id: zt2hhmmss.pro,v 1.4 1997/12/18 15:37:04 d3h797 Release_ddt_1_13 $
;***********************************************************************************
;+
;Abstract:
; This procedure converts ZebTime to a YYMMDD and an array of HHMMSS  
;	Note that both the YYMMDD and HHMMSS arrays are the same length
;	as the TO (time_offset) array), so this procedure should be used
;	when the zebtimes span more than one day.
;
; Author:  Dave Turner
; Date:    September 20, 1995
;
;
; This procedure converts Zebra's BaseTime (BT) and time offset (TO) into Julian days
;- 
pro d_zt2hhmmss, BT, TO, dateYYMMDD, timeHHMMSS

  secPerDay = Long(24L * 60L * 60L)	; number of seconds per day
  daysPerMonth = [31,28,31,30,31,30,31,31,30,31,30,31]
  secPerYear = Long(365 * secPerDay)

  size = N_ELEMENTS(TO)
  dateYYMMDD = dblarr(size)
  timeHHMMSS = dblarr(size)

for k=0,long(size - 1) do begin
  BaseTime = BT		; Store into temporary variables
  timeOffset = TO	; Store into temporary variables
  dateYYMMDD(k) = 700000L

; Add the first element from timeOffset to basetime to find the correct date
;  (the first element of timeOffset is not always zero as advertised)
  BaseTime = long(timeoffset(k)) + BaseTime

; Subtract off the appropriate number of seconds from BaseTime until it is 
;   less than one year.

  while(BaseTime ge secPerYear) do begin
    if((dateYYMMDD(k) / 10000) MOD 4 eq 0) then begin
	BaseTime = Long(BaseTime - (secPerYear + secPerDay))
      endif else begin
	BaseTime = Long(BaseTime - secPerYear) 
      endelse
    dateYYMMDD(k) = dateYYMMDD(k) + 10000L
  endwhile

; Now, capture the correct month
  if (dateYYMMDD(k) / 10000) MOD 4 eq 0 then daysPerMonth(1) = 29 $
  else daysPerMonth(1) = 28
  dateYYMMDD(k) = dateYYMMDD(k) + 100L
  secPerMonth = Long(daysPerMonth) * Long(secPerDay)
  i = 0
  while (i le 11) AND (BaseTime ge secPerMonth(i)) do begin
      BaseTime = Long(BaseTime) - Long(secPerMonth(i))
      dateYYMMDD(k) = dateYYMMDD(k) + 100L
      i = i + 1
  endwhile

; Now lets get the correct day
  dateYYMMDD(k) = dateYYMMDD(k) + 1L
  for i = 0, 30 do begin
    if BaseTime ge secPerDay then begin
      BaseTime = Long(BaseTime - secPerDay)
      dateYYMMDD(k) = dateYYMMDD(k) + 1L
    endif
  endfor
 
; Now compute the HHMMSS part
 
  BaseTime = BaseTime / double(secPerDay)
  BaseTime = BaseTime * 24.0
  HH = Fix(BaseTime)
  BaseTime = (BaseTime - HH) * 60.0
  MM = Fix(BaseTime)
  BaseTime = (BaseTime - MM) * 60.0
  SS = round(BaseTime)

; Verify that the times are ok
  for i = 0,n_elements(BaseTime) - 1 do begin
    if(SS(i) ge 60) then begin
      MM(i) = MM(i) + 1
      SS(i) = SS(i) - 60
    endif
    if(MM(i) ge 60) then begin
      HH(i) = HH(i) + 1
      MM(i) = MM(i) - 60
    endif
  endfor
   
  timeHHMMSS(k) = Long(HH * 10000L + MM * 100L + SS *1L)

endfor

  return
end

;***********************************************************************************
; Convert zebtimes given in base_time and time_offset into UItimes.
;	This routine takes advantage of IDL's vector processing to significantly
;	speed up the computation over d_zt2hhmmss.  However, while hhmmss
;	is the same length as TO (time_offset), yymmdd is only a singleton
;	and therefore this routine can only be used if the Zebtimes span
;	at most one day.
;
; author:  Tim Shippert
;
pro t_zt2hhmmss, BT, TO, dateYYMMDD, timeHHMMSS

  BaseTime = BT
  TimeOffset = TO

  secPerDay = Long(24L * 60L * 60L)	; number of seconds per day
  daysPerMonth = [31,28,31,30,31,30,31,31,30,31,30,31]
  secPerYear = double(365 * secPerDay)
  year_offsets = [ 0L , 31536000L , 63072000L , 94608000L , $
    126230400L , 157766400L , 189302400L , 220838400L , $
    252460800L , 283996800L , 315532800L , 347068800L , $
    378691200L , 410227200L , 441763200L , 473299200L , $
    504921600L , 536457600L , 567993600L , 599529600L , $
    631152000L , 662688000L , 694224000L , 725760000L , $
    757382400L , 788918400L , 820454400L , 852076800L , $
    883612800L ,  915148800L ]

  secpermonth = [ 2678400L, 5097600L, 7776000L, 10368000L, 13046400L, $
		 15638400L, 18316800L, 20995200L, 23587200L, 26265600L, $ 
		 28857600L, 31536000L ]

; Find the base year
  foo = where(year_offsets - BaseTime gt 0)
  BaseTime = BaseTime - year_offsets(foo(0)-1)
  Year = 1969 + foo(0)

; convert for leap years
  if (Year MOD 4 eq 0) then begin
    secpermonth(1:11) = secpermonth(1:11) + secPerDay
    daysPerMonth(1) = 29
    daysPerYear = 366.0D
  end

; secs will contain the number of seconds beyond the first of this year

; now find the right month

  foo = where(secpermonth - BaseTime gt 0)
  month = foo(0)+1 ; january = 1

  if (month gt 1) then begin
    BaseTime = Basetime - secpermonth(month-2) 
    ; month -1 is zero_offset index for given month
  endif

; now find the right day
  day = 1 + Basetime/secPerDay

; Now adjust the basetime so that it is the time for the start of the day,
;	which means that the timeOffset must be adjusted also to compensate
  fractionDayInSec = basetime - (day - 1) * secPerDay
  timeoffset = timeoffset + fractionDayInSec
  basetime = basetime  - fractionDayInSec

  secs = double(TimeOffset)+double(BaseTime)
  days = long(TimeOffset) / long(secPerDay)
  ss = secs mod secPerDay
 
  year = replicate(long(year mod 100), n_elements(TimeOffset))
  month= replicate(month *1L, n_elements(TimeOffset))
  day  = replicate(day *1L, n_elements(TimeOffset))
  day = day + days
 
; now add the number of days to the "base_day"
; Increment month as appropriate
  repeat begin
    flag = 1
    foo = where((month eq 1) and (day gt 31), nhits)
    if(nhits gt 0) then begin
      month(foo) = month(foo) + 1
      day(foo) = day(foo) - 31
      flag = 0
    endif
    foo = where((month eq 2) and (day gt 28) and (year mod 4 ne 0), nhits)
    if(nhits gt 0) then begin
      month(foo) = month(foo) + 1
      day(foo) = day(foo) - 28
      flag = 0
    endif
    foo = where((month eq 2) and (day gt 29) and (year mod 4 eq 0), nhits)
    if(nhits gt 0) then begin
      month(foo) = month(foo) + 1
      day(foo) = day(foo) - 29
      flag = 0
    endif
    foo = where((month eq 3) and (day gt 31), nhits)
    if(nhits gt 0) then begin
      month(foo) = month(foo) + 1
      day(foo) = day(foo) - 31
      flag = 0
    endif
    foo = where((month eq 4) and (day gt 30), nhits)
    if(nhits gt 0) then begin
      month(foo) = month(foo) + 1
      day(foo) = day(foo) - 30
      flag = 0
    endif
    foo = where((month eq 5) and (day gt 31), nhits)
    if(nhits gt 0) then begin
      month(foo) = month(foo) + 1
      day(foo) = day(foo) - 31
      flag = 0
    endif
    foo = where((month eq 6) and (day gt 30), nhits)
    if(nhits gt 0) then begin
      month(foo) = month(foo) + 1
      day(foo) = day(foo) - 30
      flag = 0
    endif
    foo = where((month eq 7) and (day gt 31), nhits)
    if(nhits gt 0) then begin
      month(foo) = month(foo) + 1
      day(foo) = day(foo) - 31
      flag = 0
    endif
    foo = where((month eq 8) and (day gt 31), nhits)
    if(nhits gt 0) then begin
      month(foo) = month(foo) + 1
      day(foo) = day(foo) - 31
      flag = 0
    endif
    foo = where((month eq 9) and (day gt 30), nhits)
    if(nhits gt 0) then begin
      month(foo) = month(foo) + 1
      day(foo) = day(foo) - 30
      flag = 0
    endif
    foo = where((month eq 10) and (day gt 31), nhits)
    if(nhits gt 0) then begin
      month(foo) = month(foo) + 1
      day(foo) = day(foo) - 31
      flag = 0
    endif
    foo = where((month eq 11) and (day gt 30), nhits)
    if(nhits gt 0) then begin
      month(foo) = month(foo) + 1
      day(foo) = day(foo) - 30
      flag = 0
    endif
    foo = where((month eq 12) and (day gt 31), nhits)
    if(nhits gt 0) then begin
      year(foo) = year(foo) + 1
      month(foo) = 1
      day(foo) = day(foo) - 31
      flag = 0
    endif
  endrep until (flag eq 1)
  dateYYMMDD = year*10000L + month*100L + day*1L

; hours
  hh = fix(ss/3600)
  ss = ss mod 3600
  mm = fix(ss/60)
  ss = fix(ss mod 60)

  timeHHMMSS = Long(hh * 10000L + mm * 100L + ss *1L)
    
  return
end

;***********************************************************************************
; Use Dave's function to convert zebtime to UItime
pro zt2hhmmss, BT, TO, dateYYMMDD, timeHHMMSS
  t_zt2hhmmss, BT, TO, dateYYMMDD, timeHHMMSS
  ;; d_zt2hhmmss, BT, TO, dateYYMMDD, timeHHMMSS
end

