; $Id: planck.pro,v 1.5 2002/08/08 12:50:37 dturner Release_ddt_1_13 $
;****************************************************************************
;+
;Abstract:
;  Planck :  procedure to compute planck blackbody radiance
;
;  Input:
;          wn   = wavenumber (cm-1)
;          temp = temperature (K)
;  Output:
;          rad  = blackbody radiance (mW/(m2 sr cm-1))
;
; Call:
	PRO  Planck, wn, temp, rad
;-

	; From the LBLRTM v6.01. These radiation constants, according 
	; to inline LBLRTM documentation, are taken from NIST.
C1 = 1.191042722E-12		
C2 = 1.4387752D			; units are [K cm]
C1 = C1 * 1e7			; units are now [mW/m2/ster/cm-4]
rad = c1 * wn * wn * wn / (exp(c2 * wn / temp)-1.D)

return
end

