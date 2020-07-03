; $Id: rh2dpt.pro,v 1.3 1998/05/27 18:34:33 turner Release_ddt_1_13 $
;***********************************************************************************
;+
;Abstract:
; This function computes the dew point given the temperature and relative humidity.
;	This routine can use the temperature dependence of the latent heat, if desired.
;     Original fortran code written by Jim Liljegren
;     Copied into IDL by Dave Turner
;     Copy Date:    September 28, 1995
;
;     INPUTS:  temp = the temperature at which the saturation pressure is desired,
;				in degrees Celsius
;	       rh   = relative humidity as a fraction between 0 and 1
;
;     OUTPUT:  rh2dpt = dew point in degrees Celsius
;
; Call:
	function rh2dpt, temp, rh
;-

  es0 = 611.0
  gascon = 461.5
  trplpt = 273.16
  tzero = 273.15
  
  latent = 2.5e06 - 2.386e03*temp
  dpt = temp
  iter = 1
  
  rh2dpt_spot:
  latdew = 2.5e06 - 2.386e03*dpt
  dpt = 1.0 / ((latent / latdew) * (1.0 / (temp + tzero) - gascon/latent * alog(rh)) + $
		1.0 / trplpt * (1.0 - (latent/latdew))) - tzero

  if(iter eq 1) then begin
    iter = 0
    goto,rh2dpt_spot
  endif

  return, dpt
end


