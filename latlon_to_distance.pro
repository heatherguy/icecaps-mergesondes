; $Id: latlon_to_distance.pro,v 1.1 2005/07/15 17:48:09 dturner Exp $
;+
; Abstract:
;	This routine is a simple routine to compute the distance from a given
;    "center" lat/lon point to a moving track (such as an airplane).  It is an
;    approximateion that is only valid in the local area near the center point 
;    and assumes a spherical earth.
;
; Author:
;	Dave Turner
;	Pacific Northwest National Laboratory
;
; Output structure:
;	dist		; The geometric distance between the center point and aircraft
;	ns_dist		; The distance north-south (N is positive) of the center point
;	ew_dist		; The distance  east-west  (E is positive) of the center point
;
; Call:
    function latlon_to_distance, $	; Returns the structure above
	center_lat, $			; The center point latitude  [deg N]
	center_lon, $			; The center point longitude [deg E]
	lat, $				; The latitude  of the aircraft [deg N]
	lon, $				; The longitude of the aircraft [deg E]
	earth_radius=earth_radius, $	; The radius of the earth [km]
	no_plot=no_plot, $		; Set this if no plot is desired
	dostop=dostop			; Set this to stop inside the routine

  if(n_elements(earth_radius) eq 0) then earth_radius = 6.378388e3 ; km
;-

  distance_per_degree = (2 * earth_radius * !pi) / 360.0

  ns_distance = (lat - center_lat) * distance_per_degree
  ew_distance = (lon - center_lon) * distance_per_degree * cos(center_lat/180.*!pi)
  distance = sqrt( ns_distance^2 + ew_distance^2 )

  if(not keyword_set(no_plot)) then begin
    window,!d.window+1
    xr=[min([center_lon,lon]),max([center_lon,lon])]
    yr=[min([center_lat,lat]),max([center_lat,lat])]
    !p.multi=[0,1,2]
    !p.region=[0,0.3,1,1]
    pchars = 1.3
    plot,ew_distance,ns_distance,chars=pchars, psym=-1, $
    	xtit='E-W Distance [km]', ytit='N-S Distance [km]'
    !p.region=[0,0,1,0.3]
    plot,distance,chars=pchars, psym=-1, $
     	xtit='Sample number', ytit='Distance [km]'
    !p.multi=0
    !p.region=0
  endif

  if(keyword_set(dostop)) then stop,'Stopped inside routine'

  return,{dist:distance, ns_dist:ns_distance, ew_dist:ew_distance}
end
