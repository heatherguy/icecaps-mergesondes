; $Id: rh2rho.pro,v 1.1 1999/10/18 19:31:26 turner Release_ddt_1_13 $
;***********************************************************************************
;+
;Abstract:
; This function computes water vapor density given the relative humidity 
;	and temperature
;     Created by Dave Turner : inverse of rho2rh.pro
;     Copy Date:    October 14 1999
;
;     INPUTS:  temp = the temperature at which the water vapor density was measured,
;				in degrees Celsius
;	       rh   = the relative humidity as a fraction between 0 and 1
;
;     OUTPUT:  rh2rho = the water vapor density, in g/m3 
; 
; Call:
	function rh2rho, temp, rh
;-

  gascon = 461.5
  tzero = 273.15
  e = rh * esat(temp, 0)
  rho = e / (1e-3 * (temp + tzero) * gascon)
  return,rho

end


