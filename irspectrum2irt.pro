; $Id: irspectrum2irt.pro,v 1.1 2008/05/30 14:20:24 dturner Exp $
;+
; Abstract: 
;    This routine convolves the input high-spectral-resolution infrared spectrum
;	with the spectral response function of the infrared thermometer (IRT).
;	In short, this routine is useful to compare the AERI to the IRT...
; Author:
;    Dave Turner, SSEC / Univ of Wisconsin - Madison
; Date:
;    Nov 2005
; Call:
  function irspectrum2irt, $		; The brightness temperature from the "IRT"
  	wnum, $				; The input wavenumber spectrum [cm-1]
	radiance, $			; The input radiance spectrum [RU]
	irt_srf=irt_srf			; If set to a 2 x N matrix, with wavelength
					;    in wavenumber
;-
  if(n_elements(irt_srf) eq 0) then irt_srf = -1
  if(n_elements(wnum) ne n_elements(radiance)) then begin
    print,'Error: The wavenumber and radiance arrays are not the same size - aborting'
    return,0
  endif

  if(irt_srf(0) eq -1) then begin
    		; This spectral response function was provided by Vic Morris.
		; To my knowledge, its the only SRF he has...
    srf_wnum = [ 9.40, 9.46, 9.58, 9.70, 9.82, 9.94, 10.06, 10.18, 10.30, $
	 10.42, 10.54, 10.66, 10.78, 10.90, 11.02, 11.14, 11.26, 11.38, 11.50, $
	 11.62, 11.74, 11.80]
    srf_wnum = 10000./srf_wnum
    srf_resp = [ 0.00, 4.58, 12.27, 29.65, 48.18, 56.02, 59.59, 59.58, $
	 61.14, 63.12, 64.79, 66.22, 67.29, 66.58, 65.58, 65.60, 66.97, $
     	 67.70, 42.63, 14.43, 4.82, 0.00]
  endif else begin
    sz = size(irt_srf)
    if(sz(0) ne 2) then begin
      print,'Error: the irt_srf needs to be a 2xN array'
      return,-999
    endif
    if(sz(1) ne 2) then begin
      print,'Error: the irt_srf needs to be a 2xN array'
      return,-999
    endif
    srf_wnum = 10000. / reform(irt_srf(0,*))
    srf_resp = reform(irt_srf(1,*))
    foo = where(700 le srf_wnum and srf_wnum lt 1400, nfoo)
    if(nfoo le 0) then begin
      print,'Error: There are no spectral elements in the 7-14 um band'
      print,'	Be sure the wavelength of the irt_srf is in microns'
      return,-999
    endif
  endelse

	; Reverse the SRF arrays to put them in ascending wavenumber order
  srf_wnum = reverse(srf_wnum)
  srf_resp = reverse(srf_resp)

	; Pad the ends of the SRF to be zero to avoid extrapolation error
  srf_wnum = [0,srf_wnum,3000]
  srf_resp = [0,srf_resp,0]

  	; Interpolate the spectral response to the AERI wavenumber grid
  weight = interpol(srf_resp,srf_wnum,wnum)
  	; Determine the mean wavenmber based on the spectral response
  mwnum  = total(wnum * weight) / total(weight)
  	; Determine the mean radiance based on the spectral response
  mrad = total(radiance*weight) / total(weight)
  	; Convert the mean radiance to brightness temperature
  invplanck,mwnum,mrad,mbt

  return,mbt
end


