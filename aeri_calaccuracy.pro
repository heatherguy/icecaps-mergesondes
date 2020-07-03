; $Id: aeri_calaccuracy.pro,v 1.3 2003/01/02 14:45:36 dturner Release_ddt_1_13 $
;+
; Abstract:
;	This routine computes the uncertainty in the AERI radiance observation
;   given some stated uncertaities in the blackbody temperatures and emissivities.
;
; Author:
;	Dave Turner
;		Pacific Northwest National Laboratory
;		University of Wisconsin - Madison
;
; Date:
;	October 2002
;
; Original code from Dave Tobin, UW-Madison
; See notebook SSEC/CIMSS pg 55, or email from Tobin dated Aug 2, 2002
;
; Call:
    pro aeri_calaccuracy, $		;
    		wn, $			; Wavenumber array
		rad1, $			; Radiance spectrum [RU]
		th, $			; Temperature of the hot BB [K]
		ta, $			; Temperature of the ambient BB [K]
		tr, $			; Reflected temperature [K]
		abssum, $		; Spectrum with absolute sum of errors
		rss, $			; Spectrum with root sum of square errors
		err_th, $		; Spectrum with errors due to hot BB T error
		err_ta, $		; Spectrum with errors due to amb BB T error
		err_tr, $		; Spectrum with errors due to reflected T error
		err_eh, $		; Spectrum with errors due to hot BB emis error
		err_ea, $		; Spectrum with errors due to amb BB emis error
 		dth = dth, $		; The uncertainty in the hot BB temp [K]
 		dta = dta, $		; The uncertainty in the abmient BB temp [K]
 		dtr = dtr, $		; The uncertainty in the reflected temp [K]
 		deh = deh, $		; The uncertainty in the hot BB emissivity []
 		dea = dea, $		; The uncertainty in the ambient BB emissivity []
		bb_option=bb_option, $	; BB modeling option for get_aeri_bb_emis.pro
					;     Default is 0...
		quiet = quiet		; Set this to make the routine quiet
;-

  	; Start with simple QC
  if(n_elements(wn) ne n_elements(rad1)) then begin
    print,'Error: the wavenumber and radiance arrays must be same length'
    return
  endif

  if(n_elements(bb_option) eq 0) then bb_option = 0

  if(n_elements(th) ne 1 or $
     	n_elements(ta) ne 1 or $
     	n_elements(tr) ne 1) then begin
    print,'Error: the various temperatures must all be scalars'
    return
  endif


  	; Original uncertainty values used by DaveT
;  dth = 0.057
;  dta = 0.057
;  dtr = 5.0
;  deh = 0.0012
;  dea = 0.0012

	; Newer uncertainty values provided by BobK on 6/19/2002
  if(n_elements(dth) ne 1) then dth = 0.10
  if(n_elements(dta) ne 1) then dta = 0.10
  if(n_elements(dtr) ne 1) then dtr = 5.0
  if(bb_option eq 0) then begin
    if(n_elements(deh) ne 1) then deh = 0.001
    if(n_elements(dea) ne 1) then dea = 0.001
  endif else begin
    if(n_elements(deh) ne 1) then deh = 0.00026	; Computed via error propagation (pg 63
    if(n_elements(dea) ne 1) then dea = 0.00026 ;      in my SSEC/CIMSS notebook)
  endelse

	; State what the various deltas are, if desired
  if(not keyword_set(quiet)) then begin
    print,'The uncertainty in the hot BB temp is 			' + $
    		string(format='(F6.3)',dth) + ' K'
    print,'The uncertainty in the ambient BB temp is 		' + $
    		string(format='(F6.3)',dta) + ' K'
    print,'The uncertainty in the reflected temp is 		' + $
    		string(format='(F6.3)',dtr) + ' K'
    print,'The uncertainty in the hot BB emissivity is 		' + $
    		string(format='(F7.5)',deh)
    print,'The uncertainty in the ambient BB emissivity is 	' + $
    		string(format='(F7.5)',dea)
  endif

  	; Get the emissivity of the blackbodies
  cavity_factor = 12.79		; Cavity factor for AERI blackbodies
  eh = get_aeri_bb_emis(wn, cavity_factor, option=bb_option)
  ea = get_aeri_bb_emis(wn, cavity_factor, option=bb_option)
  
  	; Compute the perturbations
  err_th = aeri_recal(wn,rad1,th,th+dth,ta,ta,tr,tr,eh,eh,ea,ea) - rad1
  err_ta = aeri_recal(wn,rad1,th,th,ta,ta+dta,tr,tr,eh,eh,ea,ea) - rad1
  err_tr = aeri_recal(wn,rad1,th,th,ta,ta,tr+dtr,tr,eh,eh,ea,ea) - rad1
  err_eh = aeri_recal(wn,rad1,th,th,ta,ta,tr,tr,eh,eh+deh,ea,ea) - rad1
  err_ea = aeri_recal(wn,rad1,th,th,ta,ta,tr,tr,eh,eh,ea,ea+dea) - rad1

	; Compute the root sum of square error
  rss = sqrt(double(err_th)^2 + double(err_ta)^2 + double(err_tr)^2 + $
  		double(err_eh)^2 + double(err_ea)^2)

	; And the absolute sum of the errors
  abssum = abs(double(err_th)) + abs(double(err_ta)) + abs(double(err_tr)) + $
  		abs(double(err_eh)) + abs(double(err_ea))

  return
end
