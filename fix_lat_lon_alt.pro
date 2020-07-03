; $Id: fix_lat_lon_alt.pro,v 1.2 2006/07/24 19:14:49 dturner Exp $
;+
; Abstract:
; 	This routine is used to fix the lat/lon/alt of a datafile or set of netCDF
;    data files whose information is incorrect.
;
; Author:
;	Dave Turner, SSEC, Univ. of Wisconsin - Madison
;
; Date:
;	July 2006
;
; Call:
   pro fix_lat_lon_alt, $
   	filenames, $		; The names of the files to fix
	lat, $			; The instrument latitude, in deg N
	lon, $			; The instrument longitude, in deg E
	alt			; The instrument altitude, in m MSL
;-

  files = file_search(filenames, count=count)
  for i=0,count-1 do begin
    fid = ncdf_open(files(i),/write)
    		; Get the original values of the 3 fields
    ncdf_varget,fid,'lat',olat
    ncdf_varget,fid,'lon',olon
    ncdf_varget,fid,'alt',oalt
    curtime = systime(/utc)

    		; Let's add some new attributes to these fields to indicate
		; what was done

    ncdf_control, fid, /redef
    foo = string(format='(A,A,A,F12.4,A,F12.4,A)','On ',curtime, $
    	', the original value of ',olat,' degN was converted to ',lat, ' degN')
    ncdf_attput,fid,'lat','Comment',foo
    foo = string(format='(A,A,A,F12.4,A,F12.4,A)','On ',curtime, $
    	', the original value of ',olon,' degE was converted to ',lon, ' degE')
    ncdf_attput,fid,'lon','Comment',foo
    foo = string(format='(A,A,A,F12.4,A,F12.4,A)','On ',curtime, $
    	', the original value of ',oalt,' m MSL was converted to ',alt, ' m MSL')
    ncdf_attput,fid,'alt','Comment',foo

    		; Insert the new values and close the files
    ncdf_control, fid, /endef
    ncdf_varput,fid,'lat',lat
    ncdf_varput,fid,'lon',lon
    ncdf_varput,fid,'alt',alt
    ncdf_close,fid
  endfor
  
  print,format='(A,I0,A)','There were ',count,' files updated with the new lat/lon/alt'
  return
end
