; $Id: mmcr_z_iwc.pro,v 1.1 2006/02/07 14:12:02 dturner Exp $
;+
;   Abstract:
; 	This routine computes an estimate of the cirrus cloud properties from the
;     MMCR's reflectivity data.  It is based upon the very simple IWC = a * Z ^ b
;     relationship.
;
;   Author:
;	Dave Turner
;		SSEC / Univ. of Wisconsin - Madison
;
;   Date:
;	Feb 2006
;
;   Call:
  function mmcr_z_iwc, $
  	filename, $		; The name of the ARSCL file to process
	a = a, $		; The "a" coefficient
	b = b, $		; The "b" coefficient
	dostop=dostop		; If set, then stop inside the routine

  if(n_elements(a) eq 0) then a = 0.07		; Default from Shupe's SHEBA analysis
  if(n_elements(b) eq 0) then b = 0.63		; Default from Shupe's SHEBA analysis

;-

  files = findfile(filename, count=count)
  if(count ne 1) then begin
    print,'Error: Unable to uniquely determine ' + filename
    return,-1
  endif
  fid = ncdf_open(files(0))
  ncdf_varget,fid,'base_time',bt
  ncdf_varget,fid,'time_offset',to
  ncdf_varget,fid,'Heights',ht
  ncdf_varget,fid,'ReflectivityBestEstimate',dbZ
  ncdf_varget,fid,'CloudBaseCeilometerStd',vceil0
  ncdf_varget,fid,'CloudBaseCeilometerCloth',vceil1
  ncdf_close,fid
  dbZ = dbZ / 100.		; As specified by the netCDF file to convert to dBZ
  systime2ymdhms,bt+to,yy,mm,dd,hh,nn,ss,hour=hour
  Z = 10^(dbZ/10.)		; Convert dBZ to Z
  iwc = Z*0.
  iwp = fltarr(n_elements(hour))
  for i=0,n_elements(hour)-1 do begin
    foo = where(ht ge vceil0(i) and ht ge vceil1(i),nfoo)
    if(nfoo le 0) then iwp(i) = -999 $
    else begin
      iwc(foo,i) = a * Z(foo,i) ^ b			; g/m3
      iwp(i)     = total( (iwc(foo(1:nfoo-1),i)+iwc(foo(0:nfoo-2),i)) * $
      				(ht(foo(1:nfoo-1)) - ht(foo(0:nfoo-2))) / 2. )   ; g/m2
    endelse
  endfor
  if(keyword_set(dostop)) then stop,'Stopped inside routine'
  return,{secs:bt+to, hour:hour, yy:yy, mm:mm, dd:dd, ht:ht, iwc:iwc, iwp:iwp}
end
