; $Id: epres.pro,v 1.3 1998/05/27 18:20:40 turner Release_ddt_1_13 $
;***********************************************************************************
;+
;Abstract:
;   This function computes the vapor pressure given the temperature and relative humidity
;     Original fortran code written by Jim Liljegren
;     Copied into IDL by Dave Turner
;     Copy Date:    September 28, 1995
;
;     INPUTS:  temp = ambient temperature in degrees Celsius
;	       rh   = relative humidity as a fraction between 0 and 1
;
;     OUTPUT:  epres = the vapor pressure in Pascals
;				1 millibar = 100 Pa = 0.1 kPa
; Call:
	function epres, temp, rh
;-
  ep = rh * esat(temp, 0)
  return, ep
end


