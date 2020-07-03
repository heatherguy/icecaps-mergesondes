; $Id;$
;+
; Abstract:
;	This routine is used to convert cloud particle number density into
;    optical depth.  One must be careful of the units used here.  The extinction
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
    function numdens2od, $		; Optical depth [unitless]
               ext_coef, $		; Extinction cross section coef [um^2]
	       thickness, $		; Thickness of cloud layer [m]
	       numdens			; Number density of cloud particles [m^-3]
;-

  	; Convert from [um^2] to [m^2]
  extcoefxsec = double(ext_coef) / (10.D^6)^2

  	; Convert extinction cross section [m^2]
	;   to a volume extinction coefficient [m-1]
  extcoefvol = extcoefxsec * numdens

  	; And assuming a constant extinction coef with thickness, compute optical depth
  od = extcoefvol * thickness

  return, float(od)
end
