;+
; $Id: tape13_to_transmittances.pro,v 1.1 2008/06/08 13:47:26 dturner Exp $
; 
; Abstract:
;	This function reads in the multi-level TAPE13 file, and extracts out the
;   instrument-to-layer transmittances, convolves them with the AERI instrument
;   function, apodizes them, and returns both the instrument-to-layer 
;   transmittances and the layer optical depths.
;
; Author:
;	Dave Turner, SSEC / University of Wisconsin - Madison
;
; Date:
;	June 2008
;
; Call:
  function tape13_to_transmittances, $		; Returns 0 if successful, 1 othewise
			; Input arguments
	nlayers, $				; The number of layers to read in
	dir=dir, $				; The location of the TAPE13 file
			; Output arguments
	wnum, $					; The wavenumber array
	transmittances, $			; Instrument-to-layer transmittances
	tau, $					; Layer (gaseous) optical depths
	dostop=dostop

		; Default value for the keywords
  if(n_elements(dir) eq 0) then dir = '.'
;-

  files = file_search(dir+'/TAPE13',count=count)
  if(count ne 1) then begin
    print,'Error: Unable to unambiguously determine the TAPE13 file'
    return,1
  endif
  if(nlayers le 0) then begin
    print,'Error: The number of layers must be at least 1!'
    return,1
  endif
  for i=0,nlayers-1 do begin

    		; Read in the ith layer's radiance (idx 0) and transmittance (idx 1)
    a = lbl_read(files(0),lun,s,v)

    		; Test for the error condition
    if(a le 0) then begin
      free_lun,lun
      print,'Error: Problem reading the TAPE13'
      return,1
    endif

		; Convolve it with the AERI's instrument function
    a = convolve_to_aeri(v,reform(s(*,1)))

    		; Apodize the transmittance
    trans = float(apodizer(a.spec,0))

    		; Store the data into matrices
    if(i eq 0) then begin
      wnum = a.wnum
      transmittances = transpose(trans)
      od  = -1.*alog(trans)
      			; Check the optical depths for NaNs, INFs, etc.
      foo = where(finite(od) eq 1 and od gt 0.000001, nfoo)
      if(nfoo lt n_elements(wnum)) then begin
        od = exp(interpol(alog(od(foo)),wnum(foo),wnum))
      endif
      			; Store the optical depths
      tau = transpose(od)
    endif else begin
      transmittances = [transmittances, transpose(trans)]
      od = -1.*alog(transmittances(i,*)/transmittances(i-1,*))
      			; Check the optical depths for NaNs, INFs, etc.
      foo = where(finite(od) eq 1 and od gt 0.000001, nfoo)
      if(nfoo lt n_elements(wnum)) then begin
        od = exp(interpol(alog(od(foo)),wnum(foo),wnum))
      endif
      			;  Store the optical depths
      tau = [tau, transpose(od)]
    endelse
  endfor
  transmittances = transpose(transmittances)
  tau = transpose(tau)
  
  return,0
end
