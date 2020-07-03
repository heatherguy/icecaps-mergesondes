; $Id: ncdf_varexist.pro,v 1.1 2006/06/23 14:28:48 dturner Exp $
;+ 
;  Abstract:
;	This routine determines if a field exists inside a netCDF file.  If
;    it does, then the variable ID is passed back; otherwise, -1 is returned.
;
;  Author:
;	Dave Turner, SSEC / University of Wisconsin - Madison
;
;  Date:
;	June 2006
;
;  Call:
function ncdf_varexist, $ 	; If field exists, then return varID; else return -1
	fid, $			; The netCDF file ID
	field			; The field name to search for
;-

  	; Check to make sure the file is a netCDF file and that it is open

	; Get the number of fields from the file
  info = ncdf_inquire(fid)

  	; Get the field names of each of the fields
  names = replicate('',info.nvars)
  for vid=0,info.nvars-1 do begin
    vinfo = ncdf_varinq(fid,vid)
    names(vid) = vinfo.name
  endfor

  	; Do we have a match anywhere?
  foo = where(field eq names, nfoo)
  if(nfoo eq 0) then return,-1 $
  else if(nfoo eq 1) then return,foo(0) $
  else begin
    print,'Error: Multiple variables in the netCDF were found with the name '+field
    print,'	I do not think this should be able to happen'
    return,-1
  endelse

end
