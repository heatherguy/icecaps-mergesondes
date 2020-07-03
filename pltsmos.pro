; $Id: pltsmos.pro,v 1.3 2006/11/30 19:04:00 dturner Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; Abstract:
;    This file plots up some of the SMOS fields for our viewing pleasure
;
; Author: 
;  	Dave Turner, PNNL
; Date:
;	October, 1997
;
; Arguments:
; 	None
;
; Keywords:
;     FILENAME:         If set, this file is used as input
;     PATH:             Path to the SMOS data.  Default is '.'
;     DESIRE_PNG:       If set, a png image is produced (if !d.name ne 'PS'). Default=0
;     PNGPATH:          The path where the png image is placed.  Default='.'
;     DESIRE_ZBUFFER:   If set, then the image is created in the Z-buffer.  Note this
;                               automatically sets DESIRE_png. Default=0
;
; Call:
	pro pltsmos, filename=filename, path=path, dostop=dostop, $
		desire_png=desire_png, pngpath=pngpath, desire_zbuffer=desire_zbuffer
;-

  if(n_elements(desire_png) eq 0) then desire_png = 0
  if(n_elements(desire_zbuffer) eq 0) then desire_zbuffer = 0 else begin
    desire_zbuffer = 1
    desire_png = 1
    set_plot,'z'
    device,set_resolution=[800,600]
    erase
  endelse
  if((desire_png eq 1) and (n_elements(pngpath) eq 0)) then pngpath='.'

  if(n_elements(path) eq 0) then path = '.'

  pushd,path
  if(n_elements(filename) eq 0) then begin
    filename = dialog_pickfile(filter='*smos*cdf')
    if(filename eq '') then begin
      print, 'Operation cancelled'
      popd
      return
    endif
  endif

  file = findfile(filename, count=count)
  if(count ne 1) then begin
    print, 'Unable to determine the file ' + filename
    if(keyword_set(dostop)) then stop, 'Stopping in procedure as indicated'
    popd
    return
  endif

  fid = ncdf_open(file(0))
  ncdf_varget,fid,'base_time',bt
  ncdf_varget,fid,'time_offset',to
  ncdf_varget,fid,'temp',t
  ncdf_varget,fid,'rh',u
  ncdf_varget,fid,'precip',precip
  ncdf_varget,fid,'bar_pres',p
  ncdf_varget,fid,'wspd',wspd
  ncdf_varget,fid,'wdir',wdir
  ncdf_close,fid
  popd
  systime2ymdhms,bt+to,yy,mm,dd,hour=hour
  month_string=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
  dstring = string(format='(I0,1x,A,1x,I0)',dd(0),month_string(mm(0)-1),yy(0))

  p = p * 10		; Convert kPa to hPa (mb)
  w = rh2w(t,u/100.,p)

  if(!d.name eq 'X') then window,!d.window+1,xsize=640,ysize=640

  !p.multi=[0,2,3]
  pchars = 2.0

  plot,hour,p,yst=16,chars=pchars,title='SMOS data for '+dstring, $
  	xtit='Hour [UTC]', xr=[0,24], /xst, xticks=8, xminor=6, $
	ytit='Pressure [mb]'
  plot,hour,t,yst=16,chars=pchars,title='SMOS data for '+dstring, $
  	xtit='Hour [UTC]', xr=[0,24], /xst, xticks=8, xminor=6, $
	ytit='Temperature [degC]'
  plot,hour,u,yr=[0,100],chars=pchars, $
  	xtit='Hour [UTC]', xr=[0,24], /xst, xticks=8, xminor=6, $
	ytit='Relative Humidity [%]'
  plot,hour,w,chars=pchars, $
  	xtit='Hour [UTC]', xr=[0,24], /xst, xticks=8, xminor=6, $
	ytit='WV Mixing Ratio [g/kg]'
  plot,hour,wspd,chars=pchars, $
  	xtit='Hour [UTC]', xr=[0,24], /xst, xticks=8, xminor=6, $
	ytit='Wind Speed [m/s]'
  plot,hour,wdir,chars=pchars, psym=4, syms=0.5, $
  	xtit='Hour [UTC]', xr=[0,24], /xst, xticks=8, xminor=6, $
	ytit='Wind Direction [deg]', yr=[0,360], /yst, yticks=8, yminor=6

  !p.multi=0

  if(desire_png eq 1) then begin
    gname = pngpath + '/smos.' + date_string + '.png'
    saveimage,gname
  endif

  return
end
