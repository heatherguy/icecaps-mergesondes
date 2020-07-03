;+
; $Id: ncdf_find_field_nans.pro,v 1.1 2008/06/11 12:23:30 dturner Exp $
;
; Abstract:
;	This script looks at all of the fields in a netCDF file and reports how many
;   NaNs/INFs/etc (i.e., not finite numbers) were found in each.
;
; Author:
;	Dave Turner, SSEC / University of Wisconsin - Madison
;
; Date:
;	June 2008
;
; Call:
   pro ncdf_find_field_nans, filename
;-

  files = file_search(filename,count=count)
  if(count ne 1) then begin
    print,'Error: Unable to uniquely determine ' + filename
    return
  endif
  print,'Checking the file '+files(0)
  fid = ncdf_open(files(0))
  info = ncdf_inquire(fid)
  fields_with_nans = 0
  for i=0,info.nvars-1 do begin
    vinfo = ncdf_varinq(fid,i)
    ncdf_varget,fid,vinfo.name,data
    foo = where(finite(data) ne 1, nfoo)
    if(nfoo gt 0) then begin
      print,format='(I6,2x,A,A)',nfoo,' NaNs/INFs in ',vinfo.name
      fields_with_nans = fields_with_nans+1
    endif
  endfor
  ncdf_close,fid
  print,format='(I3,A)',fields_with_nans,' fields were found to have NaNs/INFs/etc.'
  return
end
	
