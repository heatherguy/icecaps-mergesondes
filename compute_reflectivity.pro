; $Id: compute_reflectivity.pro,v 1.3 2004/08/06 15:37:06 dturner Exp $
;+
; Abstract:
;   This routine computes the MMCR's reflectivity for the given input parameters.
;   It assumes that the particles are spherical water drops, and that the sizes
;   are small enough to be in the Rayleigh limit for the radar...
;
; Author:
;   Dave Turner
;   Pacific Northwest National Laboratory
;
; Date:
;   June 2004
;
; Compile:
;	.compile ~/work/main/vip/src/radiation/newmie/src/modified_gamma_dist.pro
;	.compile ~/work/main/vip/src/radiation/newmie/src/lognormal_dist.pro
;	.compile ~/work/main/vip/src/radiation/newmie/src/compute_dist_stats.pro
;
; Call:
  function compute_reflectivity, $ 	; Computed reflectivity [dBZ]
  	lwc, $				; Input liquid water content [g/m3]
	reff, $				; Input effective radius [um]
	size_distribution, $		; Input size distribution form:
					;    0 -> modified gamma
					;    1 -> log-normal
	width, $			; The width parameter for the size distribution
	temperature=temperature, $	; The temperature of the liquid water [K]
	wavelength=wavelength, $	; The wavelength of the radar [mm]
	dostop=dostop			; Set this to stop in the routine

  if(n_elements(wavelength) eq 0) then wavelength = 8.66    ; [mm], wavelength of MMCR
  if(n_elements(temperature) eq 0) then temperature = 275.  ; [degK]
;-

  npts = 10000
  r = dindgen(npts)*0.1 + 0.1	; Generate a radius array

  if(size_distribution eq 0) then nd = modified_gamma_dist(r,reff,width) $
  else if(size_distribution eq 1) then begin
    		; Since we enter the mode of the size distribution for the 
		; lognormal calc, we have to perform one iteration to get the
		; correct size distribution for the desired effective radius...
    nd = lognormal_dist(r,reff,width)
    compute_dist_stats, r, nd, nd_units='cm-3 um-1', /silent, reff=reffp
    nd = lognormal_dist(r,reff*reff/reffp,width)
  endif else begin
    print,'Error: Undefined size distribution - aborting'
    return,-999.
  endelse

  	; Compute the liquid water content and effective radius of this size 
	; distribution.  The LWC calculation is assuming spherical water drops
	; and that the number density units are in [cm-3 um-1], giving the LWC
	; in units of [g/m3]
  compute_dist_stats, r, nd, nd_units='cm-3 um-1', /silent, reff=reffp, $
  	density=1000., wc=lwcp

	; Quick QC 
  if(0.99 ge reffp/reff or reffp/reff ge 1.01) then begin
    print,'Error: The range of radii is inadequate for this effective radius - aborting'
    return,-999.
  endif

  	; Now scale the derived size distribution to get the correct LWC
  sf = lwc / lwcp
  nd = nd * sf

  	; Quick double check on the LWC
  compute_dist_stats, r, nd, nd_units='cm-3 um-1', /silent, reff=reffp, $
  	density=1000., wc=lwcp
  if(0.99 ge lwcp/lwc or lwcp/lwc ge 1.01) then begin
    print,'Error: The LWC for this size distribution is inadequate - aborting'
    return,-999.
  endif

	; Get the K term, which depends on the refractive index, which depends on
	; the wavelength of the radar and the temperature of the water droplets.
  ww = refractive(wavelength, 1, temperature, /water)
  k  = (ww.m ^ 2 - 1) / (ww.m^2 + 2)

	; Now compute the reflectivity.  The units needed for the number density
	; are the particle size (diameter) in [mm], and the number density in 
	; [m-3 mm-1].
  d = r*2 / 1e3		; Convert radius in [um] to diameter in [mm]
  n = nd  * 1e6		; Convert number density in [cm-3 um-1] to [m-3 um-1]
  deld = r(1)-r(0)	; The delta between sizes is given in [um]
  Z = 0.
  for i=0L, n_elements(d)-1 do $
    Z = Z + d(i)^6 * n(i) * deld
 
  	; And account for the refractive index...
  Z = Z * (abs(k))^2

  	; And then compute dBZ
  Zo  = 1.	; [mm6 m-3]
  dBZ = 10 * alog10(Z / Zo)

  if(keyword_set(dostop)) then stop,'Stopped inside routine'

  return,dBZ
end
