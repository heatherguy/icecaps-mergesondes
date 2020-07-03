; $Id: t2density.pro,v 1.2 2006/12/18 13:30:52 dturner Exp $
;--------------------------------------------------------------------------------
;+
; Abstract: 
;	This routine calculates the density (molecules/m^3) from the profiles 
;  of temperature (degC) and pressure (mb).  Logic by Rich Ferrare, NASA/LaRC.
;  It also provides the density of the (dry) air in kg/m3 as a keyword option.
;
; Author:
;	Dave Turner, PNNL
;
; Date Created:
;	September, 1998
;
; Date Last Modified:
;	$Date: 2006/12/18 13:30:52 $
;
function t2density, $		; The density of the dry air [molecules / m3]
	t, $			; The ambient temperature of the air [degC]
	p, $			; The air pressure [mb]
	dens=dens		; The density of the dry air [kg / m3]
;-
  density = (p * 100.0D) / (1.381e-23 * (t + 273.16D))
  foo = where(p lt 0, nfoo)
  if(nfoo gt 0) then density(foo) = -999

  Rv = 287.05				; Gas constant for dry air [J / (kg K)]
  dens = (p * 100.) / (Rv * (t+273.16))	; dry air density [kg / m3]

  return,density
end


