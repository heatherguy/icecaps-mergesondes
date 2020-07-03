; $Id;$
;+
; Abstract:
;	This routine is used to convert cloud optical depth to cloud particle number 
;    density.  One must be careful of the units used here.  The extinction
;    cross section coefficient is assumed to have come from the single scattering
;    databases, and hence have units [um^2].  All other units are in meters to
;    some power.  Note that if the user wishes to use equivalent volume spheres,
;    or equivalent area spheres, or equivalent volume/area spheres, the adjustment
;    of the effective radius (which was used to get the extinction coefficient)
;    and the number density needs to be done outside of this function.
;
; 	This is the same logic that is in the C-code file numdens_tau.c in the
;    lblrtm_disort package.  If this code changes, then that code should be 
;    changed also.
;
; Author:
;	Dave Turner
;		Pacific Northwest National Laboratory
;		University of Wisconsin - Madison
;
; Date:
;	Mar 2002
;
; Call:
    function od2numdens, $		; Number density of cloud particles [m^-3]
               ext_coef, $		; Extinction cross section coef [um^2]
	       thickness, $		; Thickness of cloud layer [m]
	       od			; Optical depth [unitless]
;-

  	; Convert from [um^2] to [m^2]
  extcoefxsec = double(ext_coef) / (10.D^6)^2

  	; Get the volume extinction coef [m-1] from the optical depth
  extcoefvol = od / double(thickness)

  	; Back out the number density from the two extinction coefficients
	; Units are [m^-1] / [m^2] = [m^-3], which is correct
  numdens = extcoefvol / extcoefxsec	

  return, float(numdens)
end
