; $Id: ncdf_subset.pro,v 1.1 2007/01/24 20:46:46 dturner Exp $
;+
; Abstract:
; 	This routine extracts out the list of fields from the given netCDF file
;   and copies them over to a new file.  It does not subset with respect to time.
;
; Author:
;	Dave Turner
;		Univ of Wisconsin - Madison
;
; Date:
;	Jan 2007
;
; Call:
    pro ncdf_subset, $
	infile, $		; The name of the input netCDF file
	outfile, $		; The name of the output netCDF file
	field_list		; An array of field names to copy over to the new
				;   file.  Do not include the fields 'base_time' and 
				;   'time_offset' as they will be included automatically.
;-

  	; Make sure the two time fields are included
  foo = where(field_list eq 'time_offset',nfoo)
  if(nfoo eq 0) then field_list = ['time_offset',field_list]
  foo = where(field_list eq 'base_time',nfoo)
  if(nfoo eq 0) then field_list = ['base_time',field_list]

  ifilename = findfile(infile, count=count)
  if(count ne 1) then begin
    print,'Error: Unable to uniquely determine ' + infile
    return
  endif
  ofilename = findfile(outfile, count=count)
  if(count ne 0) then begin
    print,'Error: A file already exists with that name: ' + outfile
    return
  endif
  fidi = ncdf_open(ifilename(0))
  fido = ncdf_create(outfile)
  info = ncdf_inquire(fidi)
  for i=0,info.ndims-1 do begin
    ncdf_diminq,fidi,i,dname,dsize
    foo = ncdf_dimdef(fido,dname,dsize)
  endfor
  for i=0,n_elements(field_list)-1 do begin
    if(ncdf_varexist(fidi,field_list(i)) ge 0) then begin
      vid = ncdf_varexist(fidi,field_list(i))
      vinfo = ncdf_varinq(fidi,field_list(i))
      if(vinfo.ndims eq 0) then begin
        if(vinfo.datatype eq 'DOUBLE') then $
          foo = ncdf_vardef(fido,field_list(i),/double) $
        else if(vinfo.datatype eq 'FLOAT') then $
          foo = ncdf_vardef(fido,field_list(i),/float) $
        else if(vinfo.datatype eq 'SHORT') then $
          foo = ncdf_vardef(fido,field_list(i),/short) $
        else if(vinfo.datatype eq 'LONG') then $
          foo = ncdf_vardef(fido,field_list(i),/long) $
        else begin
          print,'  Unable to copy over the requested field due to inadequacy in code'
  	  continue
        endelse
      endif else begin
        if(vinfo.datatype eq 'DOUBLE') then $
          foo = ncdf_vardef(fido,field_list(i),vinfo.dim,/double) $
        else if(vinfo.datatype eq 'FLOAT') then $
          foo = ncdf_vardef(fido,field_list(i),vinfo.dim,/float) $
        else if(vinfo.datatype eq 'SHORT') then $
          foo = ncdf_vardef(fido,field_list(i),vinfo.dim,/short) $
        else if(vinfo.datatype eq 'LONG') then $
          foo = ncdf_vardef(fido,field_list(i),vinfo.dim,/long) $
        else begin
          print,'  Unable to copy over the requested field due to inadequacy in code'
  	  continue
        endelse
      endelse
      for j=0,vinfo.natts-1 do begin
        aname = ncdf_attname(fidi,vid,j)
        ainfo = ncdf_attinq(fidi,vid,aname)
	avalue = find_flatt(fidi,field_list(i),aname)
        ncdf_attput,fido,field_list(i),aname,avalue
      endfor
      ncdf_varget,fidi,field_list(i),data
      ncdf_control,fido,/endef
      ncdf_varput,fido,field_list(i),data
      ncdf_control,fido,/redef
    endif
  endfor
  ncdf_attput,fido,/global,'Subset_comment', $
  	'Data subsetted from the file ' + ifilename(0)
  ncdf_attput,fido,/global,'Subset_date', 'Data subsetted on ' + systime()
  for j=0,info.ngatts-1 do begin
    aname = ncdf_attname(fidi,j,/global)
    value = find_glatt(fidi,aname)
    ncdf_attput,fido,/global,aname,value
  endfor

  ncdf_close,fido
  ncdf_close,fidi
end
