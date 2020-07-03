; $Id: get_sgp_lse.pro,v 1.1 2004/05/07 14:28:31 dturner Exp $
;+
; Abstract:
;	This routine returns the land surface emissivity (LSE) using the simple
;   parameterization developed by the AERI group at the University of Wisconsin
;   from their scanning-AERI observations made with the AERI-bago at the SGP site.
;   Note that the LSE is a function of day of year, which attempts to capture
;   the seasonal changes from bare soil to vegetated surfaces.
;	Reference: Knuteson et al. 13th ARM STM
;
; Author:
;	Dave Turner
;		Pacific Northwest National Laboratory
;
; Date:
;	May 2004
;
; Call:
  function get_sgp_lse, $	; Returns a structure with {wnum, lse}
  	yyyy, mm, dd, $ 	; The date to use
	soil_only=soil_only, $	; Set this keyword to get bare soil emis spectrum
	vege_only=vege_only	; Set this keyword to get vegatation emis spectrum
;-

  if(n_elements(yyyy) ne 1 or n_elements(mm) ne 1 or n_elements(dd) ne 1) then begin
    print,'Error: The yyyy, mm, and dd inputs must be scalars (not arrays)'
    return,{wnum:0,lse:0}
  endif

  ymdhms2julian,yyyy,mm,dd,0,0,0,jday

  	; Percent of the surface that is bare soil, by julian day
  sfraction_jday = [0.0, 60.0, 120, 182, 244, 274, 306, 366]
  sfraction_percent = [40., 40., 20, 20, 50, 50, 40, 40]

	; Surface emissivity of bare soil
  soil_wnum = [500.000, 730.326, 768.714, 773.512, 797.505, 802.303, 816.699, $
  	850.288, 927.063, 984.645, 1042.23, 1075.82, 1119.00, 1147.79, 1157.39, $
	1171.79, 1214.97, 1253.36, 1310.94, 1992.32, 2045.11, 2083.49, 2126.68, $
	2169.87, 2419.39, 2529.75, 2630.52, 2731.29, 2880.04, 3000.00]
  soil_emis = [0.980488, 0.980488, 0.971951, 0.961585, 0.971341, 0.968902, 0.984756, $
  	0.968902, 0.964634, 0.950000, 0.915244, 0.872561, 0.886585, 0.890854, $
	0.918902, 0.892073, 0.882317, 0.951219, 0.999390, 0.999390, 0.884756, $
	0.853659, 0.862805, 0.842683, 0.793902, 0.801220, 0.817073, 0.848171, $
	0.923171, 1.00000]

	; Surface emissivity of "pasture" or "vegatation"
  vege_wnum = [500.000, 785.233, 861.935, 895.492, 938.637, 1005.75, 1053.69, $
  	1087.25, 1279.00, 1413.23, 1988.49, 3000.00]
  vege_emis = [0.996951, 0.996341, 0.989634, 0.989634, 0.985976, 0.992683, 0.992073, $
  	0.995732, 0.995732, 0.996341, 0.985366, 0.985366]

  	; Handle the keywords
  if(keyword_set(soil_only) and keyword_set(vege_only)) then begin
    print,'Error: Both soil_only and vege_only keywords were set'
    return,{wnum:0,lse:0}
  endif else if(keyword_set(soil_only)) then begin
    return,{wnum:soil_wnum,lse:soil_emis}
  endif else if(keyword_set(vege_only)) then begin
    return,{wnum:vege_wnum,lse:vege_emis}
  endif else begin
    		; Get the fraction of the surface that is bare soil for this date
    fraction = interpol(sfraction_percent/100.,sfraction_jday,jday)
    fraction = fraction(0)
    		; Build a wavenumber array that has all of inflection points in the
		; bare soil and vegatation arrays
    wnum = [soil_wnum, vege_wnum]
    wnum = wnum(sort(wnum))
    wnum = wnum(uniq(wnum))
  		; Interpolate the two surface type spectrums to this new wnum grid
    s_emis = interpol(soil_emis, soil_wnum, wnum)
    v_emis = interpol(vege_emis, vege_wnum, wnum)
    		; And now linearly combine the two spectra
    emis = fraction * s_emis + (1.-fraction) * v_emis
    return,{wnum:wnum, lse:emis}
  endelse

end
