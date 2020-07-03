; $Id: invplanck.pro,v 1.5 2002/08/08 12:50:37 dturner Release_ddt_1_13 $
;****************************************************************************
;+
;Abstract:
;  invPlanck : procedure to compute the inverse planck function
;
;  temp = C2 * wn / ln[1 + (C1 * wn**3 / (pi * rad))]
;
;  where  wn   = wavenumber = 1/wavelength, [cm-1]		   (INPUT)
;         rad  = equivalent blackbody radiance, [mW/(m2 sr cm-1)]  (INPUT)
;         temp = absolute temperature, [K]			   (OUTPUT)
;
;         C1   = 2 pi hc**2, [W cm2]
;         C2   = hc/k, [cm K]
;         h    = Planck's constant
;         k    = Boltzmann's constant
;         c    = speed of light in vacuum
;
; Call:
	PRO invPlanck, wn, rad, temp
;-

	; From the LBLRTM v6.01. These radiation constants, according 
	; to inline LBLRTM documentation, are taken from NIST.
C1 = 1.191042722E-12		
C2 = 1.4387752D			; units are [K cm]
C1 = C1 * 1e7			; units are now [mW/m2/ster/cm-4]
temp = C2 * wn / (alog(1.0D + (C1 * (double(wn))^3 / rad)))
return
end

