; $Id: pltmwr.pro,v 1.8 2003/08/05 22:50:03 dturner Release_ddt_1_13 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Abstract:
;	This script quickly plots up the MWR LOS data, so we can see how it looks
;
; Author: Dave Turner, PNNL
; Date:   September, 1997
;
; Auguments:
; Keywords:
;     FILENAME:		If set, the MWR file to read in
;     PATH:             Path to the MWRLOS data.  Default is '.'
;     DESIRE_PNG:       If set, a PNG image is produced (if !d.name ne 'PS'). Default=0
;     PNGPATH:          The path where the PNG image is placed.  Default='.'
;     DESIRE_ZBUFFER:   If set, then the image is created in the Z-buffer.  Note this
;                               automatically sets DESIRE_PNG. Default=0
;     UPPER_LEFT:       Short string to be entered in upper left corner of plot
;     PWV_MAX:		If set, the maximum PWV to plot
;     PWV_MIN:		If set, the minimum PWV to plot
;     BT_MAX:		If set, the maximum BT to plot
;     BT_MIN:		If set, the minimum BT to plot
;     TITLE:		If set, the title used for the plot
;     DOSTOP:		If set, the procedure will stop before exitting
;
; Call:
	pro pltmwr, filename=filename, path=path, title=title, dostop=dostop, $
	    desire_png=desire_png, pngpath=pngpath, desire_zbuffer=desire_zbuffer, $
	    upper_left=upper_left, pwv_max=pwv_max, pwv_min=pwv_min, $
	    bt_max=bt_max, bt_min=bt_min
;-

  @color_syms.include
  
; Manage the keywords
  if(n_elements(desire_png) eq 0) then desire_png = 0
  if(n_elements(desire_zbuffer) eq 0) then desire_zbuffer = 0 else begin
    desire_zbuffer = 1
    desire_png = 1
    set_plot,'z'
    device,set_resolution=[640,512]
    erase
  endelse
  if((desire_png eq 1) and (n_elements(pngpath) eq 0)) then pngpath='.'
  if(n_elements(path) eq 0) then path = '.'
  if(n_elements(upper_left) eq 0) then upper_left = ''
  if(keyword_set(pwv_max) eq 0) then pwv_max = 5.0
  if(keyword_set(pwv_min) eq 0) then pwv_min = 0.0
  if(keyword_set(bt_min) eq 0) then bt_min = 0.
  if(keyword_set(bt_max) eq 0) then bt_max = 100.

;  Input netCDF file
  pushd,path
  if(n_elements(filename) eq 0) then begin
    filename = dialog_pickfile(/read,filter='*mwr*cdf')
    if(filename eq '') then begin
      print,'Operation cancelled'
      popd
      return
    endif
  endif else if(filename eq '0') then begin
    files = findfile('*mwr*cdf', count=count)
    if(count eq 0) then begin
      popd
      print,'Unable to find any MWR files - aborting'
      return
    endif
    filename = files(count-1)
  endif

;  Read in the data
  fid = ncdf_open(filename)
  ncdf_varget,fid,'base_time',bt
  ncdf_varget,fid,'time_offset',to
  systime2ymdhms,bt+to,yy,mm,dd,hh,nn,ss
  ymd = yy*10000L + mm*100L + dd
  hms = hh*10000L + nn*100L + ss
  systime2julian, bt+to, yy(0), jt
  result = ncdf_inquire(fid)
  for i=0,result.nvars-1 do begin
    var = ncdf_varinq(fid, i)
    if(var.name eq 'tbsky23') then begin
      ncdf_varget, fid, 'tbsky23', tb23
      ncdf_varget, fid, 'tbsky31', tb31
      goto, next_read
    endif
  endfor
  ncdf_varget, fid, '23tbsky', tb23
  ncdf_varget, fid, '31tbsky', tb31
  next_read:
  ncdf_varget, fid, 'vap', vap
  ncdf_varget, fid, 'liq', liq
  ncdf_attget, fid, /global, 'zeb_platform', foo
  if(string(foo) eq 'sgp5mwravgC1.c1') then $
    ncdf_varget, fid, 'water_flag_fraction', ww $
  else $
    ncdf_varget, fid, 'wet_window', ww
  ncdf_close, fid
  popd

  if(ymd(0) eq ymd(n_elements(ymd)-1)) then datestring=string(format='(I0)',ymd(0)) $
  else datestring = string(format='(I0)',ymd(0)) + '.' + $
	string(format='(I0)',ymd(n_elements(ymd)-1))
  xrng = [fix(jt(0)), fix(jt(n_elements(jt)-1))+1]

  !p.multi=[0,1,2]
  !p.region=[0,0.45,1,0.95]
  plot, jt, tb23, yr=[bt_min,bt_max], ystyle=1, xstyle=1, xr=xrng, $
    ytitle='brightness temperature (K)', xtitle='julian day', color=pmain, /nodata
  oplot, jt, tb23, color=d_green
  oplot, jt, tb31, color=d_red
  foo = where(ww gt 0, nhits)
  if(nhits gt 0) then begin
    for i=0, n_elements(foo) -1 do $
      oplot, [jt(foo(i)), jt(foo(i))], [0, 10], color=d_blue
  endif
  xyouts, 0.11, 0.57, '23tbsky', color=d_green, /normal
  xyouts, 0.11, 0.54, '31tbsky', color=d_red, /normal

  !p.region=[0,0,1,0.5]
  plot, jt, vap, yr=[pwv_min,pwv_max], xr=xrng, ystyle=1, xstyle=1, $
    ytitle='PWV in cm, LWC in mm', xtitle='julian day', color=pmain, /nodata
  oplot, [0,400], [0,0], color=pmain
  oplot, jt, vap, color=d_green
  oplot, jt, liq*10, color=d_red
  xyouts, 0.11, 0.12, 'PWV', color=d_green, /normal
  xyouts, 0.11, 0.09, 'LWC', color=d_red, /normal

  if(n_elements(title) eq 0) then $
    title='MWR LOS data for ' + datestring

  xyouts, 0.5, 0.95, title, /normal, color=pmain, charsize=1.3, align=0.5

  if(n_elements(upper_left) gt 0) then $
    xyouts, 0.02, 0.98, upper_left, /normal, color=pmain

  if(desire_png eq 1) then begin
    pname = pngpath + '/mwr.' + datestring + '.png'
    saveimage,pname,/quiet
  endif

  !p.multi=0
  !p.region=0
  if(keyword_set(dostop)) then stop, 'Stopped in procedure as indicated'
  return
end
