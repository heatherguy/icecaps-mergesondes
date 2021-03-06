; $Id: plt_tape27.pro,v 1.4 2001/10/08 19:42:15 dturner Release_ddt_1_13 $
;+
; Abstract:
; 	This routine plots up the radiance/transmittance data found in the 
;    TAPE27/TAPE28 output files generated by the LBLRTM.
;
; Author: Dave Turner, PNNL
; Date:   Jan 2000
;
; Arguments: None:
; Keywords:
;	DIR:	The directory where the TAPE27 is located
;	SCALE:	The scale factor to apply to the radiance.  1e7 converts it 
;			to the "standard" radiance unit of mW/(m2 ster cm-1)
;	FILEN:	The filename of the tape to read in.  Default is TAPE27.
;	WNUM:	Set this to have the wavenumber array returned
;	RAD: 	Set this to have the radiance array returned
;	COMMENT: Set tihs to have the comment string in the TAPE27 returned
;	READONLY: Set this to only read the data (do NOT plot it)
;	EXTRA:	IDL's extra feature to pass keywords into the PLOT routine
;
; Call:
	pro plt_tape27, dir=dir, scale=scale, filen=filen, $
		wnum=wnum, rad=rad, comment=comment, readonly=readonly, _extra=extra
;-

  if(n_elements(dir) eq 0) then dir = '.'
  if(n_elements(scale) eq 0) then scale = 1
  if(n_elements(filen) eq 0) then filen = 'TAPE27'

  dpushd,dir,flag=flag
  if(flag ne 1) then begin
    print,'Unable to change to the directory ' + dir
    return
  endif

  file = findfile(filen, count=count)
  if(count ne 1) then begin
    popd
    print,'Unable to find ' + filen + ' in the directory ' + dir
    return
  endif

  command = 'wc -l ' + file(0)
  spawn, command, result
  nlines = long(result(n_elements(result)-1))
  nheader = 27		; This many header lines
  openr,lun,file(0),/get_lun
  header = replicate('',nheader)
  data = dblarr(2, nlines - nheader)
  readf,lun,header
  readf,lun,data
  free_lun,lun
  popd
  comment = header(1)

  if(not keyword_set(readonly)) then $
    plot, data(0,*), data(1,*) * scale, _extra=extra

  wnum = transpose(data(0,*))
  rad  = transpose(data(1,*))
end
