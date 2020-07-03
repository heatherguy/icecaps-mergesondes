; $Id: rho2dpt.pro,v 1.3 1998/05/27 18:35:09 turner Release_ddt_1_13 $
;***********************************************************************************
;+
;Abstract:
; This function computes the dew point temperature given temperature and water
;	vapor density
;     Original fortran code written by Jim Liljegren
;     Copied into IDL by Dave Turner
;     Copy Date:    September 28, 1995
;
;     INPUTS:  temp = the temperature at which the water vapor density was measured,
;				in degrees Celsius
;	       rho  = the water vapor density, in g/m3
;
;     OUTPUT:  rho2dpt = the dew point temperature, in degrees Celsius
; 
; Call:
	function rho2dpt, temp, rho
;-
  es0 = 611.0
  gascon = 461.5
  trplpt = 273.16
  tzero = 273.15
  latent = 2.5e06 - 2.386e03*temp
  es = es0 * exp(latent/gascon*(1.0/trplpt - 1.0/(temp + tzero)))
  e = 1e-3 * rho * (temp + tzero) * gascon
  dpt = 1.0 / (1.0 / trplpt - gascon/latent * alog(e/es0)) - tzero
  return, dpt
end


