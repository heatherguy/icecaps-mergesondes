; $Id: planck.pro,v 1.5 2002/08/08 12:50:37 dturner Release_ddt_1_13 $
;****************************************************************************
;+
;Abstract:
;  Planck :  procedure to compute planck blackbody radiance using wavelength
;
;  Input:
;          wl   = wavelength (microns)
;          temp = temperature (K)
;  Output:
;          rad  = blackbody radiance (mW/(m2 sr microns))
;
; Call:
	PRO  Planck_wavelength, wl, temp, rad
;-

  c1 = 1.1911e8
  c2 = 1.439e4
  rad = 1000*c1*wl^(-5) / (exp(c2/(wl*temp))-1)
return
end

