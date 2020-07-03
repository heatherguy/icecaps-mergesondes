; $Id: fix_aeri_vlaser.pro,v 1.2 2004/05/10 19:08:46 dturner Exp $
;+ 
; Abstract
; 	This routine's purpose is to spectrally calibrate the AERI.  It requires
;    input from a routine like "check_aeri_vlaser.pro".  
;
; Author:
;	Dave Turner
;		Pacific Northwest National Laboratory
;
; Date:
;	May 2004
;
; Call:
  pro fix_aeri_vlaser,  $
  	aeri_filename, $		; Name of the AERI file to correct
	multiplier, $			; The multiplier to apply to the laser 
					;    wavenumber.  This should come directly 
					;    from the routine check_aeri_vlaser.pro
					;    (i.e., be defined as "desired_vlaser / 
					;    original_vlaser")
	overwrite=overwrite		; Set this to overwrite the old data,
					;   otherwise a new file will be created
;-

  files = findfile(aeri_filename, count=count)
  if(count ne 1) then begin
    print,'Error: Unable to determine ' + aeri_filename
    return
  endif
  ofile = files(0)

  if(keyword_set(overwrite)) then nfile = ofile $
  else begin
    nfile = ofile + '.spectrally_calibrated'
    command = string(format='(A,1x,A,1x,A)','cp',ofile,nfile)
    spawn,command
  endelse

  fid = ncdf_open(nfile, /write)
  ncdf_varget,fid,'wnum',wnum
  ncdf_varget,fid,'mean_rad',rad
  orig_vlaser = find_glatt(fid,'originalLaserWavenumber')

	; Determine the original laser wavenumber, as this will have to be undone
  ovlaser = double(orig_vlaser)

	; And now combine this with the shift that is specified by the multiplier
	; entered in as an argument (note this is referenced to the output laser
	; wavenumber of 15799.)
  shift_to_apply = multiplier
  nvlaser        = ovlaser * multiplier

  foo = where(wnum gt 900 and wnum lt 901,nfoo)
  if(nfoo gt 0) then channel = 1 else channel = 2

  for i=0,n_elements(rad(0,*)) -1 do begin
    result1 = aeri_zerofill(wnum, reform(rad(*,i)), channel)
    newrad  = interpol(result1(1,*), result1(0,*)*shift_to_apply, wnum)
    rad(*,i) = newrad
  endfor

  ncdf_varput, fid, 'mean_rad', rad
  ncdf_attput, fid, /global, 'originalLaserWavenumber', $
  	string(format='(F10.4,1x,A)',nvlaser,'cm-1')

  ncdf_close,fid

  return
end
