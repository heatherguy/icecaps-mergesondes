; $Id: w2rho.pro,v 1.1 1999/10/18 19:31:26 turner Release_ddt_1_13 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;  Abstract: 
;	To compute water vapor density from mixing ratio and temperature
;
;  Author:
;	Dave Turner
;	Logic provided by Holger Linne
;
;  Date:
;	October 1999
;
;  Arguments:
;	w:	mixing ratio in g/kg
;	t: 	temperature in Celcius
;	p: 	pressure in mb
;
;  Returns:	water vapor density in g/m3
;
   function w2rho, w, t, p
;-

  rho = w * p * 0.3477 / (t + 273.16)

  return,rho
end
	

