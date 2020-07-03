; $Id: dpt2rh.pro,v 1.3 1998/05/27 18:16:52 turner Release_ddt_1_13 $
;***********************************************************************************
;+
;Abstract:
;   This function computes the relative humidity given the dew point temperature
;     Original fortran code written by Jim Liljegren
;     Copied into IDL by Dave Turner
;     Copy Date:    September 28, 1995
;
;     INPUTS:  temp = the temperature at which the saturation pressure is desired,
;					in degrees Celsius
;	       dpt  = the dew point temperature in degrees Celsius
;
;     OUTPUT:  dpt2rh = the relative humidity as a fraction between 0 and 1
;
; Call:
	function dpt2rh, temp, dpt
;-
  rh = esat(dpt,0)/esat(temp,0)
  return, rh
end


