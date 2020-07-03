; $Id: aeri2irt.pro,v 1.3 2008/05/30 14:19:41 dturner Exp $
;+
; Abstract:
;    This script's purpose it to convolve the AERI's radiance with the
;    IRT's spectral response function and output a time-series of BT
;    from the AERI that can be compared against the IRT.
; Author:
;    Dave Turner, SSEC / Univ of Wisconsin - Madison
; Date:
;    Nov 2005
; Call:
  function aeri2irt, $				; A structure with the output
  	filename, $				; Name of the AERI ch1 file(s)
	output_filename=output_filename, $	; If set, write to this file
	irt_srf=irt_srf, $			; If set, use this SRF
						;   Must be a 2xN array, with
						;   the wavelength in um
	dostop=dostop				; Set this to stop inside routine
;-
  	; Handle the keyword irt_srf, if not defined
  if(n_elements(irt_srf) eq 0) then irt_srf = -1

  	; Read in the AERI datafiles
  files = findfile(filename, count=count)
  for i=0,count-1 do begin
    fid = ncdf_open(files(i))
    ncdf_varget,fid,'base_time',bt
    ncdf_varget,fid,'time_offset',to
    ncdf_varget,fid,'wnum',wnum
    ncdf_varget,fid,'mean_rad',rad
    ncdf_varget,fid,'hatchOpen',hatch
    ncdf_varget,fid,'BBsupportStructureTemp',bbtemp
    ncdf_close,fid

    mbt = fltarr(n_elements(to))
    for j=0,n_elements(to)-1 do $
      mbt(j) = irspectrum2irt(wnum,rad(*,j),irt_srf=irt_srf)
    if(i eq 0) then begin
      secs = bt+to
      ibt  = mbt
      bbst = bbtemp
      hopen = hatch
    endif else begin
      secs = [secs, bt+to]
      ibt  = [ibt, mbt]
      bbst = [bbst, bbtemp]
      hopen = [hopen, hatch]
    endelse
  endfor
  hopen = fix(hopen+0.5)
  systime2ymdhms,secs,yy,mm,dd,hh,nn,ss,hour=hour
  systime2julian,secs,yy(0),jday

  if(keyword_set(output_filename)) then begin
    print,'Writing the file ' + output_filename
    openw,lun,output_filename,/get_lun
    printf,lun,'Secs      YYYY MM DD HH NN SS Jday       IRT_BT[K]  Internal_temp[K]  Hatch'
    for i=0,n_elements(secs)-1 do $
      printf,lun,format='(I0,1x,I0,1x,5(I2.2,1x),F10.6,1x,F6.2,1x,F6.2,1x,I2)', $
      	secs(i),yy(i),mm(i),dd(i),hh(i),nn(i),ss(i),jday(i),ibt(i),bbst(i),hopen(i)
    free_lun,lun
  endif

  if(keyword_set(dostop)) then stop,'Stopped inside routine'
  return,{secs:secs, yy:yy, mm:mm, dd:dd, hour:hour, jday:jday, ibt:ibt, $
  	bb_support_temp:bbst, hatch_open:hopen}
end
