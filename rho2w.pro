; $Id: rho2w.pro,v 1.1 2000/01/11 17:25:54 turner Release_ddt_1_13 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;  Abstract: 
;	To compute water vapor mixing ratio from water vapor density 
;		and temperature
;
;  Author:
;	Dave Turner
;	Logic provided by Holger Linne (inverse of w2rho)
;
;  Date:
;	Jan 2000
;
;  Arguments:
;	rho:	water vapor density in g/m3
;	t: 	temperature in Celcius
;	p: 	pressure in mb
;
;  Returns:	water vapor mixing ratio in g/kg
;
   function rho2w, rho, t, p
;-

  w = rho * (t + 273.16) / (p * 0.3477)

  return,w

end
	

