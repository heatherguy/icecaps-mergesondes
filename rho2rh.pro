; $Id: rho2rh.pro,v 1.3 1998/05/27 18:35:35 turner Release_ddt_1_13 $
;***********************************************************************************
;+
;Abstract:
; This function computes relative humidity given temperature and water 
;	vapor density
;     Original fortran code written by Jim Liljegren
;     Copied into IDL by Dave Turner
;     Copy Date:    September 28, 1995
;
;     INPUTS:  temp = the temperature at which the water vapor density was measured,
;				in degrees Celsius
;	       rho  = the water vapor density, in g/m3
;
;     OUTPUT:  rho2rh = the relative humidity as a fraction between 0 and 1
; 
; Call:
	function rho2rh, temp, rho
;-
  gascon = 461.5
  tzero = 273.15
  e = 1e-3 * rho * (temp + tzero) * gascon
  rh = e/esat(temp, 0)
  return, rh
end


