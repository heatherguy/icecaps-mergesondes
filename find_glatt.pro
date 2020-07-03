; $Id: find_glatt.pro,v 1.1 1999/07/20 14:14:45 turner Release_ddt_1_13 $
;+
; Abstract:
;	This routine is designed to find a global attribute and return
;    its value (as a string), else it returns an empty string.
;
; Author:
;	Dave Turner, PNNL
;
; Date:
;	July 1999
;
; Call:
	function find_glatt, Cdfid, attname
;-

  inqresults = ncdf_inquire(Cdfid)
  ngatts = inqresults.ngatts
  for i=0, ngatts -1 do begin
    result = ncdf_attname(Cdfid, /global, i)
    if(result eq attname) then begin
      ncdf_attget, Cdfid, /global, attname, value
      return, string(value)
    endif
  endfor

  return, ""
end
