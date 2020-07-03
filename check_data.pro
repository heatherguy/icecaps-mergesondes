; $Id: check_data.pro,v 1.1 2000/01/28 21:30:51 turner Release_ddt_1_13 $
;+
; Abstract:
;	This script is used to quickly plot up some data (any field from
;    any netCDF file) and plot it.  This allows the user to get an idea of
;    what the data looks like, as well as if there are any holes in the 
;    data.
;
; Author:
;	Dave Turner, PNNL
; Date:
;	January 2000
;
; Arguments:
;	Filename:	The name (with UNIX wildcards, if necessary, to read
; 	Fieldname:	The name of the field to plot.  Note this field is
;			    assumed to be a scalar (time-varying) field only.
;
; Keywords:
;	Dostop:		If set, then stop in the routine before exiting
;
; Call:
	pro check_data, filename, fieldname, dostop=dostop
;-

	; Handle the possible wildcards in the file name
  files = findfile(filename, count=count)
  if (count eq 0) then begin
    print,'Unable to find the file ' + filename
    return
  endif else if (count ne 1) then begin
    print,'Unable to uniquely determine the file ' + filename
    return
  endif

	; Open the netCDF file and read the time fields
  fid = ncdf_open(files(0))
  ncdf_varget,fid,'base_time',bt
  ncdf_varget,fid,'time_offset',to

  	; Determine if the desired field is actually a field in this file
  glob = ncdf_inquire(fid)
  for i=0, glob.nvars -1 do begin
    field = ncdf_varinq(fid, i)
    if(field.name eq fieldname) then goto, break_loop
  endfor
  print, 'Unable to find the field ' + fieldname + ' in the netCDF file ' + files(0)
  ncdf_close,fid
  return

	; If so, we are here.  Read in the data and the units of this field
  break_loop:
  ncdf_varget,fid,fieldname,field
  units = find_flatt(fid, fieldname, 'units')
  ncdf_close,fid

	; Convert base_time/time_offset to julian time (more useful, anyway)
  systime2ymdhms,bt+to,yy,mm,dd,hh,nn,ss
  systime2julian,bt+to,yy(0),julian

  	; Create the plot
  plot, julian, field, /xstyle, xtitle='julian day', ystyle=16, ytitle=units, $
  	psym=1, symsize=0.7, $
  	title='Field ' + fieldname + ' from the file ' + files(0)

	; If the keyword "dostop" was set, then stop here so user can muck with plot
  if(keyword_set(dostop)) then stop, 'Stopped in routine as indicated'

  return
end
