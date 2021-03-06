; $Id: plot_mmcr.pro,v 1.2 2007/11/02 20:00:00 dturner Exp $
;+ 
; Abstract:
;	This routine is used to read in and plot MMCR mode data
;
; Author:
;	Dave Turner
;		Pacific Northwest National Laboratory
;
; Date:
;	May 2004
;
; Call:
  pro plot_mmcr, $
  	path, $		; Path to the MMCR MOM data
	dates, $	; Date string to plot; i.e., 'yyyymmdd'
	dmode, $	; Desired mode to plot
			;	1 - Stratus mode
			;	2 - Cirrus mode
			;	3 - General mode
			;	4 - Robust mode
 	dmoment, $	; The moment to plot
			; 	0 - Reflectivity
			;	1 - Vertical velocity
			;	2 - Spectral width
	dostop=dostop	; Set this to stop inside routine
;-

  if(dmoment eq 0) then begin
    dfield = 'Reflectivity'
    ztickn = string(format='(I0)',[-50,-40,-30,-20,-10,0,10,20])
    zname  = 'Reflectivity [dBZ]'
    ptitle = 'MMCR Reflectivity'
  endif else if(dmoment eq 1) then begin
    dfield = 'MeanDopplerVelocity'
    ztickn = string(format='(F4.1)',[-2,-1,0,1,2])
    zname  = 'Velocity [m/s]'
    ptitle = 'MMCR Doppler Velocity'
  endif else if(dmoment eq 2) then begin
    dfield = 'SpectralWidth'
    ztickn = string(format='(F4.1)',[0,1,2,3])
    zname  = 'Spectral Width [m/s]'
    ptitle = 'MMCR Spectral Width'
  endif else begin
    print,'Error: This value for dmoment was not defined'
    return
  endelse

  ;+++++++++++++++++++++++++++++++++++++
  ; Now get the MMCR data
  filename = path +'/*mmcrmomC1.b1.'+dates+'*.cdf'
  files = findfile(filename, count=count)
  if(count ne 1) then begin
    print,'No MMCR data found for ' + dates
    return
    mhour = [0,24]
    mht   = [0,15]
    mdata = [[!values.f_nan,!values.f_nan],[!values.f_nan,!values.f_nan]]
  endif else begin
    fid = ncdf_open(files(0))
    ncdf_varget,fid,'base_time',bt
    ncdf_varget,fid,'time_offset',to
    ncdf_varget,fid,'heights',mht
    ncdf_varget,fid,'ModeNum',mmode
    ncdf_varget,fid,dfield,mdata
    ncdf_varget,fid,'ModeDescription',mmodedesc
    ncdf_varget,fid,'alt',ht_offset		; This is in m 
    ncdf_close,fid
    print,'  Using the MMCRs ' + string(mmodedesc(*,dmode))
    foo = where(mmode eq dmode,nfoo)
    if(nfoo le 0) then begin
      print,'Error: This mode ('+string(format='(I0)',dmode)+ $
      	') does not exist in this file'
      return
    endif
    mdata = mdata(*,foo)
    mht  = reform(mht(*,dmode) - ht_offset) / 1000. 
    to   = to(foo)
    systime2ymdhms,bt+to,yy,mm,dd,hh,nn,ss
    mhour = hh + nn/60. + ss/3600. + (dd - dd(0))*24
    foo = where(mht gt 0, nfoo)
    if(nfoo le 0) then begin
      print,'Error: All heights are determined to be negative'
      return
    endif
    mht   = mht(foo)
    mdata = mdata(foo,*)
  endelse
  
  !p.multi=[0,1,2]
  window,!d.window+1,ys=400
  dloadct,44

  pchars = 1.3
  qchars = 1.0
  xtickn = string(format='(I2)',[0,3,6,9,12,15,18,21,24])
  xr = [min(float(xtickn)),max(float(xtickn))]
  xminor = 10
  ytickn = string(format='(I0)',[0,3,6,9,12,15])
  yr = [min(float(ytickn)),max(float(ytickn))]
  yminor = 6

  zr = [min(float(ztickn)),max(float(ztickn))]
  zminor = 6
  dloadct,44
  dcontour, mdata, mhour, mht, $
    'Hour [UTC]', xtickn, xr, xminor, $
    'Altitude [km AGL]', ytickn, yr, yminor, $
    zname, ztickn, zr(0), zr(1), zminor, $
    [0,0.05,0.85,0.95], [0.82,0.10,1,0.90], pchars, ptitle

  xyouts, 0.5, 0.96, dates, /nor, align=0.5, color=0, chars=pchars
  xyouts, 0.99, 0.01, /align, /nor, color=0, string(mmodedesc(*,dmode))

  if(keyword_set(dostop)) then stop,'Stopping inside routine'
end
