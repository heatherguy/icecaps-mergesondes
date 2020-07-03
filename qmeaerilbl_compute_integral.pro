; $Id: qmeaerilbl_compute_integral.pro,v 1.1 2000/04/07 14:38:19 turner Release_ddt_1_13 $
;+
; Abstract:
;	This little routine is the poor-man's version of the QME AERI/LBLRTM.
;    It will provide integrated values of (radiance/residuals) as a function
;    of wavenumber range or process.  It reads in the qmeaerilbl.mapping file, 
;    which is a composite of the two spectral mapping files used by the QME.
;    Note: the spectral mapping file (qmeaerilbl.mapping) is expected to live
;    in the $VIP_INFO directory.
;    
; Author: Dave Turner
; Date:   Jan 2000
;
; Arguments:
;	WNUM:		Array of wavenumbers.  These are expected to match up
;			  to the AERI wavenumber array.
;	DATA:		Array of data (radiances, residuals, etc)
;
; Keywords:
;	Process:	The process to integrate over: 1 H2O, 2 CO2, 3 O3, 
;				4 N2O, 5 CO, 6 CH4, 7 O2, 8 Self H2O Cntnm,
;				9 Foreign H2O Cntnm, 10 CO2 Cntnm, 11 N2 Cntnm,
;				0 turns off spectral selection (default).
;				-1 outputs results for all of these.
;	WN_RNG:		The wavenumber range to use in the integral.  Default
;				is to use the entire wnum range
;	USE_ALL:	If set, then the all associated elements are used in the
;				integral, else only the unsaturated elements 
;				are used
;	RETURN_DATA:	If set, the integral(s) will be returned
;	DOSTOP:		If set, then stop in the routine
;
; Call:
	pro qmeaerilbl_compute_integral, wnum, data, $
		process=process, wn_rng=wn_rng, use_all=use_all, $
		return_data=return_data, dostop=dostop
;-

  if(n_elements(use_all) eq 0) then use_all = 0

  nprocess = 1
  if(n_elements(process) eq 0) then process = 0
  if(process lt 0) then begin
    process = 0
    nprocess = 12
  endif 

  vip_info = getenv("VIP_INFO")
  if(vip_info eq '') then begin
    print,'Unable to determine the env variable VIP_INFO'
    return
  endif

  sm_filename = vip_info + '/qmeaerilbl.mapping'
  sm_file = findfile(sm_filename, count=count)
  if(count ne 1) then begin
    print,'Unable to find spectral mapping file ' + sm_filename
    return
  endif

  	; Read in the Spectral mapping data
  openr,lun,sm_file(0),/get_lun
  header = ''
  readf,lun,header
  parts = str_sep(header,' ')
  nheader = long(parts(0))
  nlines  = long(parts(1))
  for i=0,nheader-1 do readf,lun,header
  sm_data = dblarr(5,nlines)
  readf,lun,sm_data
  free_lun,lun

  	; Wavenumber delta
  wn_delta = sm_data(1,1) - sm_data(1,0)

  	; Determine the overall wavenumber range.
  if(n_elements(wn_rng) gt 0) then begin
    min_wn = wn_rng(0)
    max_wn = wn_rng(1)
  endif else begin
    min_wn = 0
    max_wn = 5000
  endelse
  sm_wn_array = transpose(sm_data(1,*))
  wn_array    = wnum
  min_wn = max([min(sm_wn_array), min(wn_array), min_wn]) - wn_delta/2.0
  max_wn = min([max(sm_wn_array), max(wn_array), max_wn]) + wn_delta/2.0

	; Make sure the input wavenumbers match some continuous subset in the
	;    spectral mapping file
  foo = where((min_wn le sm_wn_array) and (sm_wn_array le max_wn), nfoo)
  if(nfoo eq 0) then begin
    print,'Min/Max wavenumbers resulted in no spectal mapping data to analyze'
    if(keyword_set(dostop)) then stop, 'Stopped in routine as indicated'
    return
  endif
  sm_data = sm_data(*,foo)
  sm_wn_array = transpose(sm_data(1,*))

  foo = where((min_wn le wn_array) and (wn_array le max_wn), nfoo)
  if(nfoo eq 0) then begin
    print,'Min/Max wavenumbers resulted in no data to analyze'
    if(keyword_set(dostop)) then stop, 'Stopped in routine as indicated'
    return
  endif
  wn_array = wn_array(foo)
  data_array = data(foo)

  if(n_elements(wn_array) ne n_elements(sm_wn_array)) then begin
    print,'The entered data does not match the spectal mapping files wavenumbers'
    if(keyword_set(dostop)) then stop, 'Stopped in routine as indicated'
    return
  endif

  process_strings = ['    All Processes', '        H2O Lines', '        CO2 Lines', $
  	'         O3 Lines', '        N2O Lines', '         CO Lines', $
	'        CH4 Lines', '         O2 Lines', '   Self H2O Cntnm', $
	'Foreign H2O Cntnm', '        CO2 Cntnm', '         N2 Cntnm']
  sat_string = ['only the unsaturated','every']

  print,' '
  print,'Using ' + sat_string(use_all) + ' elements over'
  print,'    the range of ' + string(format='(F7.2)',min_wn) + ' - ' + $
		string(format='(F7.2)',max_wn) + ' wavenumbers (cm-1)'
  print,' '
  print,'    Process         Integral    Npts
  print,'------------------------------------'

  rdata = fltarr(nprocess)

  for i=process,process+nprocess-1 do begin
    		; Find all of the sensitive (unsaturated) points
    if((i eq 0) and (use_all eq 0)) then $
    	foo = where(sm_data(2,*) eq 1, nfoo) $

		; Or Find all of the points (unsat or not)
    else if(i eq 0) then $
    	foo = where(sm_data(2,*) ge 0, nfoo) $

    		; Or find the sensitive points for the given process
    else if(use_all eq 0) then $
    	foo = where((sm_data(2,*) eq 1) and (sm_data(3,*) eq i), nfoo) $

		; Or find all of the points for the given process
    else foo = where(sm_data(3,*) eq i, nfoo)

    if(nfoo gt 0) then total = wn_delta * total(data_array(foo)) $
    else total = -999.
    print,process_strings(i) + '  ' + string(format='(F10.3)',total) + $
    		'  ' + string(format='(I5)',nfoo)
    rdata(i-process) = total

  endfor
  print,'------------------------------------'

  return_data = rdata
  if(keyword_set(dostop)) then stop, 'Stopped in routine as indicated'
end

