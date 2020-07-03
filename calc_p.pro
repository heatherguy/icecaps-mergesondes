; $Id: calc_p.pro,v 1.4 2002/01/19 15:22:12 dturner Release_ddt_1_13 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Abstract:
;   This function calculates an estimate of the pressure at a given altitude,
; given the temperature at the altitude as well as a reference level where
; we have the temperature, pressure, and altitude.  For example, this can
; be used to derive an estimate of the barametric pressure of the 60 meter 
; tower, given the tower's temperature measurement, as well as the SMOS tower's
; temperature and pressure measurements (we know the altitudes of these already).
; Note: since R is the dry air gas constant, the temperature should really
; be virtual temperature; however, a rough estimate can be found using 
; the ambient temperature.
;   Essentially, we are just using the hypsometric equation here...
;
;	Author: Dave Turner
;	Last modified: 1/2002
;
;     INPUTS:	t = temperature at the unknown pressure level, in degK
;		z = altitude of the unknown pressure level, in km
;		p0 = pressure at the reference level, in mb
;		t0 = temperature at the reference level, in degK
;		z0 = altitude of the reference level, in km
;
;      OUTPUT:	p = pressure estimate at the unknown pressure level, in mb
; 
; Call:
	function calc_p, t, z, p0, t0, z0
;-

  R = 0.28704 	; the gas constant of dry air (J/g * K)
  g = 9.806	; gravity (m/s^2)

  	; Some quick QC
  if(n_elements(t0) ne 1 or n_elements(z0) ne 1 or n_elements(p0) ne 1) then begin
    print,'ERROR: t0, z0, and p0 are supposed to be scalar values'
    stop, 'Stopping in calc_p'
  endif
  foo = where(t le 0, nfoo)
  if(nfoo gt 0 or t0 le 0) then begin
    print,'ERROR: Temperature is assumed to be in Kelvin (i.e., > 0)'
    stop, 'Stopping in calc_p'
  endif

  ; The initial equation is ln(p/p0) is (approximately) -g/(R * (t-t0)/2) * (z - z0)
  ; This comes almost directly from the hypsometric equation

  avg_t = (t + t0) / 2		; average the temperatures together

  ; equation is: p = exp(-g / (R * t_avg) * (z - z0)) * p0

  foo = double((-1) * g) / double(R * avg_t)
  p = exp(double(z - z0)*foo) * p0

  return,p
end


