; $Id: plot_mmcr0.pro,v 1.1 2004/07/23 20:50:57 dturner Exp $
;+ 
; Abstract:
;	This routine is used to read in and plot MMCR mode data.  It is designed for
;    the old-style (original processor) MMCR data files.  The only moment in these
;    original netCDF files (mmcrcal) are the reflectivity.
;
; Author:
;	Dave Turner
;		Pacific Northwest National Laboratory
;
; Date:
;	July 2004
;
; Call:
  pro plot_mmcr0, $
  	path, $		; Path to the MMCR MOM data
	dates, $	; Date string to plot; i.e., 'yyyymmdd'
	dmode		; Desired mode to plot
			;	1 - Stratus mode
			;	2 - Cirrus mode
			;	3 - General mode
			;	4 - Robust mode
;-

  if(dmode lt 1 or dmode gt 4) then begin
    print,'Error: the desired mode (dmode) must be between 1 and 4'
    return
  endif
  
  dfield = 'Reflectivity'
  ztickn = string(format='(I0)',[-50,-40,-30,-20,-10,0,10,20])
  zname  = 'Reflectivity [dBZ]'
  ptitle = 'MMCR Reflectivity'

  	; These are the modes of the MMCR, and the number of 
	; spectra averaged for each mode (which is how to determine between them)
  mmodedesc = ['Null', 'Stratus', 'Cirrus', 'General', 'Robust']
  mmodenspc = [-1,     64,        21,       60,        29]

  ;+++++++++++++++++++++++++++++++++++++
  ; Now get the MMCR data
  filename = path +'/*mmcrcalC1.a1.'+dates+'*.cdf'
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
    ncdf_varget,fid,'Heights',mht
    ncdf_varget,fid,'NumSpectraAveraged',nspect
    ncdf_varget,fid,dfield,mdata
    ncdf_varget,fid,'alt',ht_offset		; This is in m 
    ncdf_close,fid
    print,'  Using the MMCRs ' + mmodedesc(dmode)
    foo = where(nspect eq mmodenspc(dmode),nfoo)
    if(nfoo le 0) then begin
      print,'Error: This mode ('+string(format='(I0)',dmode)+ $
      	') does not exist in this file'
      return
    endif
    mdata = mdata(*,foo)
    mht  = reform(mht(*,foo(0)) - ht_offset) / 1000. 
    to   = to(foo)
    systime2ymdhms,bt+to,yy,mm,dd,hh,nn,ss
    mhour = hh + nn/60. + ss/3600.
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
  xyouts, 0.99, 0.01, /align, /nor, color=0, mmodedesc(dmode)

end
