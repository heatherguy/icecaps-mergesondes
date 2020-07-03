; $Id: ymdhms2systime.pro,v 1.3 1999/11/10 20:38:38 turner Release_ddt_1_13 $
;***********************************************************************************
;+
;Abstract:
;  This routine converts human readable times (YYYY, MM, DD, HH, Min, SS)
;  	into system time (seconds since 1-Jan-1970).
;
; Call:
    pro ymdhms2systime, yyyy, mm, dd, hh, nn, ss, time
;-

  if((n_elements(yyyy) ne n_elements(mm)) or $
     (n_elements(yyyy) ne n_elements(dd)) or $
     (n_elements(yyyy) ne n_elements(hh)) or $
     (n_elements(yyyy) ne n_elements(nn)) or $
     (n_elements(yyyy) ne n_elements(ss))) then begin
                print, 'Time arrays not the same size in YMDHMStoSystime()'
		time = -1
		return
  endif
 
  for i=0L, n_elements(yyyy) -1 do begin
    date_0 = julday(01,01,1970)
    date_1 = julday(mm(i),dd(i),yyyy(i))
    ntime = (date_1 - date_0) * (24L * 60L * 60L) + $
                hh(i) * (60L * 60L) + nn(i) * 60L + ss[i]
    if(i eq 0) then time = ntime else time = [time, ntime]
  endfor
  return
end

