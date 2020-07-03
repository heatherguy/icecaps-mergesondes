; $Id: aeri_recal.pro,v 1.2 2002/10/07 05:56:23 dturner Release_ddt_1_13 $
;+
; Abstract:
;	This routine recalibrates AERI a spectrum based upon the temperature
;   and emissivities of the blackbodies.  It can also be used to estimate
;   the uncertainty in the AERI radiance based upon uncertainties in the
;   measured BB temperatures/emissivities.  Note that it is designed to only
;   process one radiance spectrum at a time.
;
;	Note that a Radiance unit (RU) is mW / (m2 ster cm-1)
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
; See notebook SSEC/CIMSS pg 55, or email from BobK dated June 19, 2002
;
; Method:
;
;   rad1 = Re{(Cs-Ca)/(Ch-Ca)}*(B(Th1,Tr1,eh1)-B(Ta1,Tr1,ea1))+B(Ta1,Tr1,ea1)
;   rad2 = Re{(Cs-Ca)/(Ch-Ca)}*(B(Th2,Tr2,eh2)-B(Ta2,Tr2,ea2))+B(Ta2,Tr2,ea2)
;        = ((rad1-B(Ta1,Tr1,ea1)))/(B(Th1,Tr1,ea1)-B(Ta1,Tr1,ea1))*
;          (B(Th2,Tr2,eh2)-B(Ta2,Tr2,ea2)) + B(Ta2,Tr2,ea2)
;    with B(T,Tr,e) = e*planck(T) + (1-e)*planck(Tr)
;
;
; Call:
  function aeri_recal, $	; Newly calibrated radiance spectrum [RU]
  		wn, $		; Wavenumber array [cm-1]
		rad1, $		; Orig radiance spectrum [RU]
		th1, $		; Orig temperature of hot BB [K]
		th2, $		; New temperature of hot BB [K]
		ta1, $		; Orig temperature of ambient BB [K]
		ta2, $		; New temperature of ambient BB [K]
		tr1, $		; Orig reflected temperature [K]
		tr2, $		; New reflected temperature [K]
		eh1, $		; Orig emis spectrum for hot BB []
		eh2, $		; New emis spectrum for hot BB []
		ea1, $		; Orig emis spectrum for ambient BB []
		ea2		; New emis spectrum for ambient BB []
;-

  	; Start with simple QC
  n = n_elements(wn)
  if(n ne n_elements(rad1) or $
   	n ne n_elements(eh1) or $
     	n ne n_elements(eh2) or $
     	n ne n_elements(ea1) or $
     	n ne n_elements(ea2)) then begin
    print,'Error: the wavenumber, radiance, and emissivity arrays must be same length'
    return,0
  endif

  if(n_elements(th1) ne 1 or $
     	n_elements(th2) ne 1 or $
     	n_elements(ta1) ne 1 or $
     	n_elements(ta2) ne 1 or $
     	n_elements(tr1) ne 1 or $
     	n_elements(tr2) ne 1) then begin
    print,'Error: the various temperatures must all be scalars'
    return,0
  endif

	; Compute the radiances for the various temperatures
  planck, wn, th1, b_th1
  planck, wn, th2, b_th2
  planck, wn, ta1, b_ta1
  planck, wn, ta2, b_ta2
  planck, wn, tr1, b_tr1
  planck, wn, tr2, b_tr2

	; Compute pseudo radiances
  Bh1 = eh1 * b_th1 + (1-eh1) * b_tr1
  Ba1 = ea1 * b_ta1 + (1-ea1) * b_tr1
  Bh2 = eh2 * b_th2 + (1-eh2) * b_tr2
  Ba2 = ea2 * b_ta2 + (1-ea2) * b_tr2

  	; and recalibrate
  rad2 = ((rad1 - Ba1) / (Bh1 - Ba1)) * (Bh2 - Ba2) + Ba2

  return, rad2
end
