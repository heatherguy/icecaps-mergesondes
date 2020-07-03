; $Id: fix_aeri_vlaser_mod.pro,v 1.1 2008/01/04 18:27:19 dturner Exp $
;+ 
; Abstract
; 	This routine's purpose is to spectrally calibrate the AERI.  It requires
;    input from a routine like "check_aeri_vlaser.pro".    It is based upon the
;    logic in fix_aeri_vlaser.pro.  Note this only works with one channel at 
;    a time (i.e., do not try to fix both ch1 and ch2 simultaneously).
;
; Author:
;	Dave Turner
;		SSEC / University of Wisconsin - Madison
;
; Date:
;	May 2004
;
; Call:
  function fix_aeri_vlaser_mod,  $		; The corrected data
  	wnum, $				; The AERI wavenumber array
	irad, $				; The input radiance spectra
	multiplier, $			; The multiplier to apply to the laser 
					;    wavenumber.  This should come directly 
					;    from the routine check_aeri_vlaser.pro
					;    (i.e., be defined as "desired_vlaser / 
					;    original_vlaser")
	dostop=dostop
;-

	; Determine which channel this is
  foo = where(wnum gt 900 and wnum lt 901,nfoo)
  if(nfoo gt 0) then channel = 1 else channel = 2

	; Apply the spectral calibration
  nrad = irad * 0.			; Initialize
  for i=0,n_elements(irad(0,*)) -1 do begin
    result1 = aeri_zerofill(wnum, reform(irad(*,i)), channel)
    newrad  = interpol(result1(1,*), result1(0,*)*multiplier, wnum)
    nrad(*,i) = newrad
  endfor

  return,nrad
end
