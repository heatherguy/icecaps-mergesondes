; $Id: tvirt.pro,v 1.3 1998/05/27 18:36:08 turner Release_ddt_1_13 $
;***********************************************************************************
;+
;Abstract:
; This function computes the virtual temperature given the ambient temperature,
;	the relative humidity, and the ambient pressure
;     Original fortran code written by Jim Liljegren
;     Copied into IDL by Dave Turner
;     Copy Date:    September 28, 1995
;
;     INPUTS:  temp = the temperature at which the saturation pressure is desired,
;					in degrees Celsius
;	       rh   = relative humidity as a fraction between 0 and 1
;	       pres = barometric pressure in Pascals
;
;     OUTPUT:  tvirt = the virtual temperature; the temperature that dry air would 
;			have at the same density, in degrees Celsius
; 
; Call:
	function tvirt, temp, rh, pres
;-
  rvap = 461.5
  tzero = 273.15
  vt = (temp + tzero) / (1.0 - 0.378 * epres(temp, rh)/pres) - tzero
  return, vt
end


