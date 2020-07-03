; $Id: read_toms.pro,v 1.2 2006/02/06 21:10:16 dturner Exp $
;+
;  Abstract:
;	This routine reads in the TOMS ozone data for an input lat/lon (DegN,DegE)
;
;  Author:
;	Dave Turner
;		SSEC / University of Wisconsin - Madison
;
;  Date:
;	Feb 2005;
;
;  Call:
function read_toms, $		; A structure with the date, day, and ozone amount [DU]
	dyy, $			; The year  we desire
	dmm, $ 			; The month we desire
	dlat, $			; The latitude  we desire [DegN]
	dlon, $			; The longitude we desire [DegE]
	path=path, $ 		; The path to the netCDF TOMS data from ARM
	dostop=dostop		; Set this to stop inside the routine

  if(n_elements(path) eq 0) then path = '/yukon/data/gec/gectomsX1.a1'
;-

  filename = 'gectomsX1.a1.'+string(format='(I0,I2.2,A)',dyy(0),dmm(0),'*cdf')
  dpushd,path,flag=flag
  if(flag ne 1) then begin
    print,'Error: Unable to move to the directory ' + path
    return,-1
  endif
  files = findfile(filename, count=count)
  if(count le 0) then begin
    popd
    print,'Error: Unable to find any TOMS data with this name: ' + filename
    return,-1
  endif
  for i=0,count-1 do begin
    fid = ncdf_open(files(i))
    ncdf_varget,fid,'base_time',bt
    ncdf_varget,fid,'time_offset',to
    ncdf_varget,fid,'ozone',ozone
    ncdf_varget,fid,'lat',lat
    ncdf_varget,fid,'lon',lon
    ncdf_close,fid

    		; Find the closest location in the dataset...
    delta = abs(dlat - lat)
    foolat = (where(delta eq min(delta)))(0)
    delta = abs(dlon - lon)
    foolon = (where(delta eq min(delta)))(0)

    if(i eq 0) then begin
      secs = bt+to
      o3   = reform(ozone(foolat,foolon,*))
    endif else begin
      secs = [secs, bt+to]
      o3   = [o3, reform(ozone(foolat,foolon,*))]
    endelse
  endfor
  popd
  
  		; Screen out the bad/missing data
  foo = where(o3 gt 0, nfoo)
  if(nfoo le 0) then begin
    print,'Error: All TOMS data failed the QC check'
    return,-1
  endif
  secs = secs(foo)
  o3   = o3(foo)
  systime2ymdhms,secs,yy,mm,dd,hour=hour
  day = dd + hour/24.

  if(keyword_set(dostop)) then stop,'Stopped inside routine'

  return, {secs:secs, yy:yy, mm:mm, dd:dd, day:day, o3:o3}
end
