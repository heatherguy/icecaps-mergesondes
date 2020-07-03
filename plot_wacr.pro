;  $Id: plot_wacr.pro,v 1.2 2007/01/08 19:14:53 dturner Exp $
;+
; Abstract: 
; 	This script plots the WACR data
;
; 	The "field" fieldname has several numerical values.  These values are:
;		0 -> Reflectivity, co-polarization (default)
;		1 -> Mean Doppler Velocity
;		2 -> Spectral Width
;		3 -> Signal-to-Noise ratio
; 	The keyword "crosspol" should be set if you want to see the cross-pol data
;
; Author: 
; 	Dave Turner, SSEC / Univ. of Wisconsin - Madison
;
; Date:
;	Jan 2007
;
; Call:
  pro plot_wacr, $
	date, $			; yyyymmdd format
	field=field, $		; See above
	path=path, $		; Path to the WACR data
	crosspol=crospol, $	; See above
	apply_mask=apply_mask,$	; Set this to apply a simple cloud mask
	dostop=dostop

  if(n_elements(path) eq 0) then path = '.'
  if(n_elements(field) eq 0) then field = 0
;-
  field = fix(field)
  if(field eq 0) then begin
    fieldn = 'Reflectivity'
    ztickn = string(format='(I0)',[-50,-40,-30,-20,-10,0,10,20])
    ztit = 'Reflectivity [dBZ]'
  endif else if(field eq 1) then begin
    fieldn = 'MeanDopplerVelocity'
    ztickn = string(format='(I0)',[-4,-3,-2,-1,0,1,2,3,4])
    ztit = 'Mean Doppler Velocity [m/s]'
  endif else if(field eq 2) then begin
    fieldn = 'SpectralWidth'
    ztickn = string(format='(I0)',[0,1,2,3])
    ztit = 'Doppler Spectral Width [m/s]'
  endif else if(field eq 3) then begin
    fieldn = 'SignalToNoiseRatio'
    ztickn = string(format='(I0)',[0,2,4,6,8,10])
    ztit = 'Signal-to-Noise Ratio [dB]'
  endif else begin
    print,'Error: Invalid field value'
    return
  endelse
  
  filename = string(format='(A,A,I0,A)',path,'/*wacr*',date,'*cdf')
  files = findfile(filename, count=count)
  if(count ne 1) then begin
    print,'Error: Unable to unambigously determine ' + filename
    return
  endif
  fid = ncdf_open(files(0))
  ncdf_varget,fid,'base_time',bt
  ncdf_varget,fid,'time_offset',to
  ncdf_varget,fid,'heights',ht
  ncdf_varget,fid,'alt',alt
  ht = (ht - alt) / 1000.
  ncdf_varget,fid,fieldn,data
  ncdf_varget,fid,'Polarization',polflag
  ncdf_close,fid
  if(keyword_set(crosspol)) then foo = where(polflag eq 1, nfoo) $
  else foo = where(polflag eq 0, nfoo)
  if(nfoo le 0) then begin
    print,'Error: Unable to find any samples with the desired polarization'
    return
  endif
  systime2ymdhms,bt+to,yy,mm,dd,hour=hour
  datestring = string(format='(I0,2(I2.2))',yy(0),mm(0),dd(0))

	; The height dependent background (noise) level for the WACR.  This
	; was determined from clear sky WACR data on 15 Dec 2006 from 10-17 UTC.
  	; mn_noise is in dBZ, ht_noise in m MSL, sd_noise in dBZ
  mn_noise = [-60.0000, -59.5299, -54.1295, -50.8177, -48.4601, -46.5488, -44.9865, $
  		-43.6761, -42.5362, -41.5551, -40.6279, -39.8514, -39.0785, -38.3915, $
		-37.7648, -37.1366, -36.5821, -36.1255]
  sd_noise = 2.5		; 1-sigma Height independent stdev of the noise profile
  ht_noise = [447.010, 1304.15, 2161.30, 3018.44, 3875.58, 4732.72, 5589.86, $
  		6447.00, 7304.14, 8161.28, 9018.43, 9875.57, 10732.7, 11589.9,  $
		12447.0, 13304.2, 14161.3, 15018.5]
  ht_noise = (ht_noise - alt) / 1000.		; Convert m MSL to km AGL

	; If the keyword is set, the mask out the reflectivity below the noise
	; level of the instrument.  But only do this for the reflectivity data
  if(keyword_set(apply_mask) and field eq 0) then begin
    noise = interpol(mn_noise + 3*sd_noise, ht_noise, ht)
    for i=0,n_elements(ht) -1 do begin
      bar = where(data(i,*) lt noise(i), nbar)
      if(nbar gt 0) then data(i,bar) = -999
    endfor
  endif

  window,!d.window+1,xs=640,ys=512
  !p.multi=[0,1,2]
  pchars=1.3
  xtickn = string(format='(I2.2)',[0,3,6,9,12,15,18,21,24])
  xr = [min(float(xtickn)),max(float(xtickn))]
  xminor = 6
  ytickn = string(format='(I0)',[0,2,4,6,8,10,12,14,16])
  yr = [min(float(ytickn)),max(float(ytickn))]
  yminor = 6
  zr = [min(float(ztickn)),max(float(ztickn))]
  zminor = 6
  
  dloadct,42
  dcontour,data(*,foo),hour(foo),ht, $
  	'Hour [UTC]',xtickn,xr,xminor, $
	'Altitude [km AGL]',ytickn,yr,yminor,$
	ztit,ztickn,zr(0),zr(1),zminor, $
	[0,0,0.87,0.96],[0.83,0,1,0.96],pchars, $
	'WACR '+fieldn+' data for '+datestring
  xyouts,0.99,0.01,/normal,/align,files(0)
  if(keyword_set(crosspol)) then xyouts,0.99,0.04,/nor,align=1,'Cross-pol data'
  !p.multi=0
  !p.region=0
  !p.font=-1
  if(keyword_set(dostop)) then stop,'Stopped inside routine'
  return
end
