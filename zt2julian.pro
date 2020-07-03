; $Id: zt2julian.pro,v 1.2 1997/10/05 17:51:25 d3h797 Release_ddt_1_13 $
;****************************************************************************
;+
;Abstract:
; This procedure converts Zebra's BaseTime (BT) and time offset (TO) into Julian days
;       jt needs to be a floating point array with sz elements available.  sz is 
;	the number of points in timeOffset.
;-
;
; Author:  Dave Turner
; Date:    June 11, 1995
;
; Last modification: June 11, 1995
;
; This procedure converts Zebra's BaseTime (BT) and time offset (TO) into Julian days
;       jt needs to be a floating point array with sz elements available.  sz is 
;	the number of points in timeOffset.
pro d_zt2julian, BT, TO, jt

  BaseTime = BT		; Store into temporary variables
  timeOffset = TO	; Store into temporary variables

  secPerDay = Long(24L * 60L * 60L)	; number of seconds per day

  secPerYear = Long(365 * secPerDay)

  Year = 1970

  sz = N_ELEMENTS(TO)
  jt = dblarr(sz)

;
; Subtract off the appropriate number of seconds from BaseTime until it is 
;   less than one year.
;
  while(BaseTime gt secPerYear) do begin
    if(Year MOD 4 eq 0) then begin
	BaseTime = Long(BaseTime - (secPerYear + secPerDay))
      endif else begin
	BaseTime = Long(BaseTime - secPerYear) 
      endelse
    Year = Year + 1
  endwhile

;
; Now add BaseTime to timeOffset and 
;
  timeOffset = timeOffset + BaseTime

;
; Now compute the Julian day (number of days and day fraction 
;   from the beginning of the year)
;
  for i=long(0),long((sz - 1)) do begin
    jt(i) = 1
    while(timeOffset(i) gt secPerDay) do begin
      jt(i) = jt(i) + 1
      timeOffset(i) = timeOffset(i) - secPerDay
    endwhile
    jt(i) = jt(i) + (timeoffset(i) / secPerDay)
  endfor
    
  return
end


;***********************************************************************************
; Convert zebtimes given in base_time and time_offset into julian times
;	This routine takes advantage of IDL's vector processing to significantly
;	speed up the computation over d_zt2julian
;
; author:  Tim Shippert
;
pro t_zt2julian, BT, TO, jt

  BaseTime = BT		; Store into temporary variables
  timeOffset = TO	; Store into temporary variables

; Contains the time offsets of January 1 of each year from 
; 1970 to 1999
  year_offsets = [ 0L , 31536000L , 63072000L , 94608000L , $
    126230400L , 157766400L , 189302400L , 220838400L , $
    252460800L , 283996800L , 315532800L , 347068800L , $
    378691200L , 410227200L , 441763200L , 473299200L , $
    504921600L , 536457600L , 567993600L , 599529600L , $
    631152000L , 662688000L , 694224000L , 725760000L , $
    757382400L , 788918400L , 820454400L , 852076800L , $
    883612800L ,  915148800L ]

  secPerDay = Long(24L * 60L * 60L)	; number of seconds per day

  secPerYear = 365.0D * double(secPerDay)

;
; Subtract off the appropriate number of seconds from BaseTime until it is 
;   less than one year.
;

  foo = where(year_offsets - BaseTime gt 0)
  BaseTime = BaseTime - year_offsets(foo(0)-1)
  Year = 1970 + foo(0)

;
; Now add BaseTime to timeOffset and 
;
  timeOffset = timeOffset + BaseTime

;
; Now compute the Julian day (number of days and day fraction 
;   from the beginning of the year)
;
  if (Year MOD 4 eq 0) then begin
    daysPerYear = 366.0D
    secPerYear = secPerYear + double(secPerDay)
  endif else begin
    daysPerYear = 365.0D
  endelse

  jt = 1 + daysPerYear*double(timeOffset)/secPerYear
    
  return
end

;***********************************************************************************
; Use Tim's function to convert zebtime to julian time
;
pro zt2julian, BT, TO, jt
  t_zt2julian, BT, TO, jt
  ;; d_zt2julian, BT, TO, jt
end

