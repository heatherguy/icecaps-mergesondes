; $Id: investigate_ssp.pro,v 1.1 2006/05/10 19:46:24 dturner Exp $
;+
; Abstract:
;     This routine is designed to help investigate the scattering properties that
;   are encoded in the SSP file.  In particular, I want to be able to quickly
;   get at the wnum and reff array, as well as the SSPs.
;
; Author:
;     Dave Turner, SSEC
;
; Date:
;     April 2006
; 
; Output:
;	wnum: 	All of the wavenumbers in the SSP DB
;	reff:	All of the effective radii in the SSP DB
;	field:	The field desired from the DB (ext, sca, abs, w0, g, Qext, Qabs, Qsca)
;	u_wnum:	The unique wavenumbers in the DB
;	u_reff: The unique effective radii in the DB
;
; Example:
;	Here's an example of how to use this routine
;
;    IDL> a = investigate_ssp('ssp_mie_wat.txt','Qext')
;    IDL> plot,a.u_wnum,psym=-1		; Shows the wnums in the database
;		; Show the data as function of reff for a given wavenumber
;    IDL> foo = where(895 le a.wnum and a.wnum lt 905)	
;    IDL> plot,a.reff(foo),a.field(foo),psym=-1		
;
; Call:
  function investigate_ssp, ssp_filename, field, dostop=dostop
;-

  case field of 
  	'ext' : idx = 3
  	'sca' : idx = 4
  	'abs' : idx = 5
  	'w0'  : idx = 6
  	'g'   : idx = 7
  	'Qext': idx = 8
  	'Qabs': idx = 9
  	'Qsca': idx = 10
         else:  idx = -1
  endcase

  if(idx lt 0) then begin
    print,'Error: Field must be one of:'
    print,'  ext, sca, abs, w0, g, Qext, Qabs, or Qsca'
    return,-2
  endif

  files = findfile(ssp_filename, count=count)
  if(count ne 1) then begin
    print,'Error: Unable to unambiguously determine ' + ssp_filename
    return,-1
  endif
  openr,lun,files(0),/get_lun
  header = replicate(' ',7)
  readf,lun,header
  nlines = long(header(2))
  nphase = long(header(3))
  ncols  = 13
  data = dblarr(ncols+nphase,nlines)
  readf,lun,data
  free_lun,lun

  	; Capture this data
  wnum = reform(data(1,*))
  reff = reform(data(2,*))
  fld  = reform(data(idx,*))

	; Find the unique wavenumbers
  s_wnum = wnum(sort(wnum))
  u_wnum = s_wnum(uniq(s_wnum))
	; Find the unique reff
  s_reff = reff(sort(reff))
  u_reff = s_reff(uniq(s_reff))

  if(keyword_set(dostop)) then stop,'Stopped inside routine'
  return,{wnum:wnum,reff:reff,field:fld,u_wnum:u_wnum,u_reff:u_reff,ssp_name:files(0)}
end
