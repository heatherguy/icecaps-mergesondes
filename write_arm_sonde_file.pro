; $Id: write_arm_sonde_file.pro,v 1.1 2008/01/23 13:52:00 dturner Exp $
;+
; Abstract:
;      This routine writes a netCDF file that looks like an ARM radiosonde file.
;    This routine is most useful for the "mrtm" runs that I might make.
;
; Author:
;	Dave Turner, SSEC / UW-Madison
;
; Date July 2007
;
; Call:
   pro write_arm_sonde_file, $
	z, $			; altitude, in m MSL
	p, $			; pressure, in hPa or mb
	t, $			; temperature, in C
	u, $			; relative humidity, in %
	name, $			; name of output file to create
	glatt=glatt, $		; An array of strings to add as global attributes
	base_time=base_time, $	; The base_time, if known
	time_offset=time_offset, $	; The time_offset, if known
	silent=silent		; If set, then operate silently
;-

  if(n_elements(z) ne n_elements(p) or $
     n_elements(z) ne n_elements(t) or $
     n_elements(z) ne n_elements(u)) then begin
    print,'Error: Profiles are not the same length'
    print,'   No radiosonde netCDF file was created'
    return
  endif
  if(not keyword_set(silent)) then print,'Creating the file '+name
  fid = ncdf_create(name,/clobber)
  did = ncdf_dimdef(fid,'time',/unlimit)
  vid = ncdf_vardef(fid,'base_time',/long)
    ncdf_attput,fid,vid,'long_name','Time since 1970-01-01 00:00:00'
    ncdf_attput,fid,vid,'units','seconds'
    if(not keyword_set(base_time)) then $
      ncdf_attput,fid,vid,'comment','This is a dummy field and has no real meaning; '+$
    	'it is only included to match the ARM data format'
  vid = ncdf_vardef(fid,'time_offset',did,/double)
    ncdf_attput,fid,vid,'long_time','Time since base_time'
    ncdf_attput,fid,vid,'units','seconds'
    if(not keyword_set(time_offset)) then $
      ncdf_attput,fid,vid,'comment','This is a dummy field and has no real meaning; '+$
    	'it is only included to match the ARM data format'
  vid = ncdf_vardef(fid,'pres',did,/float)
    ncdf_attput,fid,vid,'long_name','Pressure'
    ncdf_attput,fid,vid,'units','hPa'
  vid = ncdf_vardef(fid,'tdry',did,/float)
    ncdf_attput,fid,vid,'long_name','Dry bulb temperature (i.e., ambient temperature)'
    ncdf_attput,fid,vid,'units','C'
  vid = ncdf_vardef(fid,'rh',did,/float)
    ncdf_attput,fid,vid,'long_name','Relative humidity'
    ncdf_attput,fid,vid,'units','%'
  vid = ncdf_vardef(fid,'alt',did,/float)
    ncdf_attput,fid,vid,'long_name','Altitude'
    ncdf_attput,fid,vid,'units','m MSL'
  if(not keyword_set(glatt)) then $
    ncdf_attput,fid,/global,'comment','ad-hoc created sonde' $
  else for i=0,n_elements(glatt)-1 do $
    ncdf_attput,fid,/global,string(format='(A,I2.2)','comment_',i),glatt(i)
  ncdf_control,fid,/endef
  if(keyword_set(base_time)) then ncdf_varput,fid,'base_time',long(base_time) $
  else ncdf_varput,fid,'base_time',0
  if(keyword_set(time_offset)) then $
    if(n_elements(time_offset) eq n_elements(z)) then $
      ncdf_varput,fid,'time_offset',time_offset $
    else ncdf_varput,fid,'time_offset',dindgen(n_elements(z)) $
  else ncdf_varput,fid,'time_offset',dindgen(n_elements(z))
  ncdf_varput,fid,'pres',p
  ncdf_varput,fid,'tdry',t
  ncdf_varput,fid,'rh',u
  ncdf_varput,fid,'alt',z
  ncdf_close,fid
  return
end

