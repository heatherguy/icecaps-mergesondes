; $Id: get_aeri_bb_emis.pro,v 1.5 2006/02/08 14:54:09 dturner Exp $
;+
; Abstract:
;	This routine returns the emissivity spectrum for the AERI blackbodies,
;   interpolated onto the correct wavenumber grid.
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
; Yao Te's calculations are from a Monte Carlo simulation he performed at SSEC
; during the fall of 2002.  The results were passed to me by BobK on 12/17/02
; in an email
;
; Call:
     function get_aeri_bb_emis, $	; The emissivity spectrum
     		wn, $			; The desired wavenumber array
		cavity_factor, $	; The cavity factor
		option = option		; Calculation option:
					;  -1 -> Return the emissivity of the paint
					;          (i.e., no cavity factor applied)
					;   0 -> original way 
					;         (wavelength independent cavity factor)
					;   1 -> hemispherical FOV
					;   2 -> normal FOV
					;   3 -> restricted (46 mrad full-angle) FOV
					;   4 -> Yao Te's diffuse reflectance
					;   5 -> Yao Te's specular reflectance
					;      (All of Yao's calcs use restricted FOV)
					;   9 -> new "blessed" way
					;         (wavelength dependent cavity factor)
;-

  if(n_elements(option) eq 0) then option = 0

  if(n_elements(cavity_factor) ne 1 and option eq 0) then begin
    cavity_factor = 12.79
    print,'WARNING: Setting cavity factor to the default of '+ $
    	string(format='(F)',cavity_factor)
  endif

  	; The emissivity spectrum, from DaveT's file
  v = [400.00000D, 500.00000, 550.00000, 600.00000, 650.00000, 700.00000, $
	740.00000, 765.00000, 800.00000, 850.00000, 900.00000, 950.00000, $
	1000.0000, 1060.0000, 1100.0000, 1150.0000, 1200.0000, 1300.0000, $
	1400.0000, 1510.0000, 1550.0000, 1650.0000, 1700.0000, 1732.0000, $
	1746.0000, 1800.0000, 1850.0000, 1900.0000, 2030.0000, 2100.0000, $
	2200.0000, 2300.0000, 2400.0000, 2500.0000, 2600.0000, 2700.0000, $
	2800.0000, 2900.0000, 3000.0000, 3100.0000]
  e = [0.96230000D, 0.96280000, 0.96200000, 0.95940000, 0.95600000, 0.95560000, $
	0.95410000, 0.95480000, 0.95410000, 0.95560000, 0.95600000, 0.95560000, $
	0.95030000, 0.94130000, 0.94950000, 0.96860000, 0.97320000, 0.97600000, $
	0.97660000, 0.97700000, 0.97700000, 0.97700000, 0.97670000, 0.97510000, $
	0.97440000, 0.96690000, 0.96440000, 0.96360000, 0.96400000, 0.96400000, $
	0.96460000, 0.96540000, 0.96460000, 0.96460000, 0.96540000, 0.96540000, $
	0.96610000, 0.96690000, 0.96760000, 0.96760000]

		; Interpolate this to our desired wavenumbers
  emis = interpol(e, v, wn)

		; Using the curves provided by HankR on 5 Oct 2002
		; These were derived from Monte Carlo simulations of the BBs
  		    ; Emissivity of the "wall" (i.e., paint) for each of the others
  paint_emis = [0.94, 0.95, 0.96, 0.97, 0.98]

  		; The emissivity numbers below are based upon some 
		; research into various models of the AERI BBs.  This
		; research was done primarily in the late 1990's - early 2000's.
  		    ; Hemispherical FOV emissivity
  hemis_emis = [0.995032, 0.996407, 0.997149, 0.997867, 0.998587]
  		    ; Normal FOV
  normal_emis = [0.998911, 0.999107, 0.999298, 0.999482, 0.999661]
  		    ; FOV is 46 mrad full-angle about normal
  restricted_emis = [0.998895, 0.999097, 0.999291, 0.999475, 0.999658]
  		    ; FOV is 46 mrad, full-angle, but reflectance is diffuse
  yaote_emis00 = [0.9993954, 0.9995041, 0.9996146, 0.9997120, 0.9998141]
		    ; FOV is 46 mrad, full-angle, but reflectance is specular
  yaote_emis99 = [0.9997791, 0.9998307, 0.9998798, 0.9999208, 0.9999522]

  		; Apply the cavity factor and convert to blackbody emissivity
  		; Note that the method chosen depends on the option selected
  if(option eq -1) then begin
    print,'Returning emissivity spectrum of the paint (no cavity factor applied)'
    return, emis
  endif else if(option eq 0) then begin
    		; The original way to use the wavelength-independent cavity factor
    rcf = 1.D / cavity_factor
    bb_emis = emis / (emis + rcf * (1 - emis))
  endif else if(option eq 9) then begin
    		; This spectrally-dependent cavity factor, along with the 
		; coefficients in "a", are the latest results from the Monte
		; Carlo models performed by UW-Madison as of Feb 2006.
    a = [93.,-3.9,0.06]
    cavity_factor = poly(10000./wn,a)  ; Polynomial as function of wavelength [um]
    rcf = 1.D / cavity_factor
    bb_emis = emis / (emis + rcf * (1 - emis))
  endif else begin
    if(option eq 1) then fov_emis = hemis_emis $
    else if(option eq 2) then fov_emis = normal_emis $
    else if(option eq 3) then fov_emis = restricted_emis $
    else if(option eq 4) then fov_emis = yaote_emis00 $
    else if(option eq 5) then fov_emis = yaote_emis99 $
    else begin
      print,'Error: Undefined value for option in get_aeri_bb_emis()'
      return,0
    endelse

    bb_emis = emis * 0.
    for i=0,n_elements(bb_emis)-1 do $
      bb_emis(i) = interpol(fov_emis, paint_emis, emis(i))
  endelse

  return, bb_emis
end
