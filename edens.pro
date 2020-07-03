; $Id: edens.pro,v 1.3 1998/05/27 18:19:44 turner Release_ddt_1_13 $
;***********************************************************************************
;+
;Abstract:
;   This function computes the vapor density given the temperature and relative humidity
;     Original fortran code written by Jim Liljegren
;     Copied into IDL by Dave Turner
;     Copy Date:    September 28, 1995
;
;     INPUTS:  temp = ambient temperature in degrees Celsius
;	       rh   = relative humidity as a fraction between 0 and 1
;
;     OUTPUT:  edens = the vapor density in g/m3
;
; Call:
	function edens, temp, rh
;-
  rvap = 461.5
  tzero = 273.15
  ed = epres(temp, rh) / (rvap * (temp + tzero)) * 1000.0
  return, ed
end


