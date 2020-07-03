; $Id: find_flatt.pro,v 1.1 1999/11/11 16:40:15 turner Release_ddt_1_13 $
;+
; Abstract:
;	This routine is designed to find a field level attribute and return
;    its value (as a string), else it returns an empty string.
;
; Author:
;	Dave Turner, PNNL
;
; Date:
;	November 1999
;
; Call:
	function find_flatt, Cdfid, fieldname, attname
;-

  varid = ncdf_varid(Cdfid, fieldname)
  if(varid lt 0) then return, ""

  inqresults = ncdf_varinq(Cdfid, varid)
  natts = inqresults.natts
  for i=0, natts -1 do begin
    result = ncdf_attname(Cdfid, varid, i)
    if(result eq attname) then begin
      ncdf_attget, Cdfid, varid, attname, value
      return, string(value)
    endif
  endfor

  return, ""
end
