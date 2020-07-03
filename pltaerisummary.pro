; $Id: pltaerisummary.pro,v 1.3 1998/05/27 18:31:31 turner Release_ddt_1_13 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;Abstract:
; This script plots some of the AERI summary fields.  To create a postscript
;       plot of this data, "set_plot,'ps'" and run.  The plot will be placed
;       in the PlotHome directory.  Gif plots can also be created.  The image
;       is first drawn in either the X-window or the Z-buffer (you set the
;       option) and then copied to the gif file, which is placed in PlotHome.
;   The upper plot contains the calculated brightness temperatures at
;	675, 700, 985, 2295, 2282, and 2510.  The corresponding temps are the
;       same type of line, but the channel 2 temperature is lighter (example:
;       the 675 and 2295 BT which are both temperatures of the surface air are
;	solid lines, however, the 2295 temp is in a lighter shade of grey).
;	The paired ones are 675/2295, 700/2282, and 985/2510.
;   The middle plot is of the wave_num_avg_stan_dev_rad at 675 and 2295 wavenumbers.
;	Note that the channel 2 field is multiplied by a factor of 10.  These
;	are indicators to use to decide if the data is too noisy.
;   The lower plot is of the wave_num_avg_stan_dev_rad at 985 and 2510 wavenumbers.
;	Note that the channel 2 field is multiplied by a factor of 10.  These
;	indicate the variability in the atmosphere (cloudiness) during the scan time.
;  
; Note that it can read both AERI 01 and AERI 00 data
;
; Author: Dave Turner, PNL
; Date:   June, 1997
;
; KEYWORDS:
; 	READFILE:	Set to read all of the files in the specified directory
;				If unset, the user will interactively select a file
;	AERISUM_PATH:	Set to change the path where the data is expected
;	USE_ZBUFFER:	Set to use the Z-buffer, otherwise X or PS will be used
;				depending on the state of the system.  Setting this
;				is only meaningful if we are outputting gif images
;	DESIRE_GIF:	Set to output a gif image to the directory PLOT_HOME
;
; Call:
	PRO pltaerisum, readfile=readfile, aerisum_path=aerisum_path, 
		desire_gif=desire_gif, use_zbuffer=use_zbuffer
;-

;;;;;;;;;;;;; Some definition statements ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if(keyword_set(readfile)) then readfile = 1 else readfile = 0

if(n_elements(aerisum_path) eq 0) then $
  if(readfile eq 1) then aerisum_path = '/data/sgp/sgpaeri01summaryC1.a1' $
  else aerisum_path = '.' 

aerisumRootName = 'sgpaeri*summaryC1.a1.'	; root name of the platform

if(keyword_set(DESIRE_GIF)) then desire_gif = 1 else desire_gif = 0
 
if(keyword_set(USE_ZBUFFER)) then use_zbuffer = 1 else use_zbuffer = 0

	; Default plot home if the environment variable `PLOT_HOME' is not set
PlotHome1 = '.'
	; Get the environment variable PLOT_HOME, if it exists
result = getenv('PLOT_HOME')
if(result(0) eq '') then PlotHome = PlotHome1 else PlotHome = result

	; Load in the colors
  @color_syms.include

;;; Let's read in the data
pushd, aerisum_path
if readfile eq 1 then begin
  aerisumFilesRead = 0
  filename = aerisumRootName + '*.cdf'
  filename = findfile(filename,count=num_files)
endif else begin
  aerisumFilesRead = 0
  filename = dialog_pickfile(/read, filter=aerisumRootName+'*.cdf')
  if(filename eq '') then stop,'Operation cancelled'
  num_files = 1
endelse
if num_files eq 0 then begin
  popd
  print,'No AERI summary files found'
  return
endif
for i=0,num_files - 1 do begin
  fid = ncdf_open(filename(i))
  ncdf_varget, fid, 'base_time', BaseTime
  ncdf_varget, fid, 'time_offset', TimeOffset

  ; Find out how many samples exist in this file
  timeID = ncdf_dimid(fid, 'time')
  ncdf_diminq,fid, timeID, name, tSamps

  ; Is this the AERI01 instrument, or the AERI00?
  ncdf_attget,fid,/global,'zeb_platform',instrument
  instrument = string(instrument)
  if(instrument eq 'sgpaeri01summaryC1.a1') then aeri01 = 'TRUE' else aeri01 = 'FALSE'

  ; Convert the times to something useful
  zt2julian, BaseTime, TimeOffSet, julianTime
  zt2hhmmss, BaseTime, TimeOffset, dateYYMMDD, timeHHMMSS

  ; Read in the scientific fields
  if(aeri01 eq 'TRUE') then begin
    ncdf_varget, fid, 'surfaceLayerAirTemp675_680', BT_675_temp
    ncdf_varget, fid, 'elevatedLayerAirTemp700_705', BT_700_temp
    ncdf_varget, fid, 'longwaveWindowAirTemp985_990', BT_985_temp
    ncdf_varget, fid, 'surfaceLayerAirTemp2295_2300', BT_2295_temp
    ncdf_varget, fid, 'elevatedLayerAirTemp2282_2287', BT_2282_temp
    ncdf_varget, fid, 'shortwaveWindowAirTemp2510_2515', BT_2510_temp
    ncdf_varget, fid, 'skyViewStdDevRadiance675_680', sdev_rad_675_temp
    ncdf_varget, fid, 'skyViewStdDevRadiance2295_2300', sdev_rad_2295_temp
    ncdf_varget, fid, 'skyViewStdDevRadiance985_990', sdev_rad_985_temp
    ncdf_varget, fid, 'skyViewStdDevRadiance2510_2515', sdev_rad_2510_temp
  endif else begin
    ncdf_varget, fid, 'wave_num_avg_BT_675', BT_675_temp
    ncdf_varget, fid, 'wave_num_avg_BT_700', BT_700_temp
    ncdf_varget, fid, 'wave_num_avg_BT_985', BT_985_temp
    ncdf_varget, fid, 'wave_num_avg_BT_2295', BT_2295_temp
    ncdf_varget, fid, 'wave_num_avg_BT_2282', BT_2282_temp
    ncdf_varget, fid, 'wave_num_avg_BT_2510', BT_2510_temp
    ncdf_varget, fid, 'wave_num_avg_stan_dev_rad_675', sdev_rad_675_temp
    ncdf_varget, fid, 'wave_num_avg_stan_dev_rad_2295', sdev_rad_2295_temp
    ncdf_varget, fid, 'wave_num_avg_stan_dev_rad_985', sdev_rad_985_temp
    ncdf_varget, fid, 'wave_num_avg_stan_dev_rad_2510', sdev_rad_2510_temp
  endelse

  ; Merge the data into one array, no matter how many files there are
  if aerisumFilesRead gt 0 then begin
    BT_675 = [BT_675, BT_675_temp]
    BT_700 = [BT_700, BT_700_temp]
    BT_985 = [BT_985, BT_985_temp]
    BT_2295 = [BT_2295, BT_2295_temp]
    BT_2282 = [BT_2282, BT_2282_temp]
    BT_2510 = [BT_2510, BT_2510_temp]
    sdev_rad_675 = [sdev_rad_675, sdev_rad_675_temp]
    sdev_rad_2295 = [sdev_rad_2295, sdev_rad_2295_temp]
    sdev_rad_985 = [sdev_rad_985, sdev_rad_985_temp]
    sdev_rad_2510 = [sdev_rad_2510, sdev_rad_2510_temp]
    hms = [hms, timeHHMMSS]
    jtime = [jtime, julianTime]
  endif else begin
    BT_675 = BT_675_temp
    BT_700 = BT_700_temp
    BT_985 = BT_985_temp
    BT_2295 = BT_2295_temp
    BT_2282 = BT_2282_temp
    BT_2510 = BT_2510_temp
    sdev_rad_675 = sdev_rad_675_temp
    sdev_rad_2295 = sdev_rad_2295_temp
    sdev_rad_985 = sdev_rad_985_temp
    sdev_rad_2510 = sdev_rad_2510_temp
    hms = timeHHMMSS
    jtime = julianTime
    sdate = dateYYMMDD(0)
  endelse

  aerisumFilesRead = aerisumFilesRead + 1
  if aerisumFilesRead eq num_files then edate = dateYYMMDD(0)
  ncdf_close, fid
endfor

popd


; If there is only one day's worth of data, then let the axis be time of day
if(sdate eq edate) then begin
  jtime = hhmmss2hh(hms)
  xtit = 'hour of day'
endif else xtit = 'julian day'

;;;;;;;;;;;;;;;;;;;;;;;;;;; Plotting time ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Build the title for the plot
Ptitle = 'AERI summary data!C' + $
        string(format='(I6.6)', sdate) + ' - ' + string(format='(I6.6)', edate)

; set plotting scales for each plot
  BTlo=170
  BThi=320
  sdev_opaque_lo=0.
  sdev_opaque_hi=1.0
  sdev_cloud_lo=0
  sdev_cloud_hi=0.5

  if(!d.name eq 'PS') then pmain = d_black else pmain = d_white

if(!d.name eq 'PS') then begin
  ; open the postscript file
  fname = PlotHome + '/' + 'aerisum.' + string(format='(I6.6)', sdate) + '.' $
		+ string(format='(I6.6)', edate) + '.ps'
  device,filename=fname,/landscape,/color
endif else begin
  ; or prepare for reading in the gif image
  ; by writing the image first to the Z buffer, then reading this out
  if(desire_gif eq 1) then begin
    if(use_zbuffer eq 1) then begin
      set_plot,'z',set_resolution=[640,512]
    endif else window,0,/pixmap
    fname = PlotHome + '/' + 'aerisum.' + string(format='(I6.6)', sdate) + '.' $
		+ string(format='(I6.6)', edate) + '.gif'
  endif
  erase
endelse

;;; Plotting time.  Three plotting areas on the window

!p.multi=[0,1,3]

!p.multi(0)= 3
!p.region=[0.0,0.40,1.0,0.95]
plot, jTime, BT_675, yrange=[BTlo, BThi], color=pmain, $
	ytitle = "Brightness temperature (K)", $
	title = Ptitle,charsize=2, linestyle=0, xstyle=1, ystyle=1, xcharsize=0.01
oplot, jTime, BT_700, color=pmain, linestyle=1
oplot, jTime, BT_985, color=pmain, linestyle=3
oplot, jTime, BT_2295, color=d_red, linestyle=0
oplot, jTime, BT_2282, color=d_red, linestyle=1
oplot, jTime, BT_2510, color=d_red, linestyle=3

!p.multi(0)=2
!p.region=[0.0,0.20,1.0,0.5]
plot, jTime, sdev_rad_675, yrange=[sdev_opaque_lo, sdev_opaque_hi], $
	color=pmain, ytitle = "Opaque sdevs", $
	charsize=2, xstyle=1, ystyle=1, xcharsize=0.01
sdev_rad_2295 = sdev_rad_2295 * 10
oplot, jTime, sdev_rad_2295, color=d_red,linestyle=1

!p.multi(0)=1
!p.region=[0.0,0.0,1.0,0.3]
plot, jTime, sdev_rad_985, yrange=[sdev_cloud_lo, sdev_cloud_hi], $
	color=pmain, ytitle = "Atm var sdevs", $
	xtitle=xtit, charsize=2, xstyle=1, ystyle=1

sdev_rad_2510 = sdev_rad_2510 * 10
oplot, jTime, sdev_rad_2510, color=d_red,linestyle=1

if(!d.name eq 'PS') then begin
  ; close the file appropriately
  device,/close
endif else begin
  ; read the gif
  if(desire_gif eq 1) then begin
    foo = tvrd()
    write_gif,fname,foo
    erase
    if(use_zbuffer eq 1) then set_plot,'x'
  endif
endelse
  
; reset to default settings
!p.region=0
!p.multi=0

return
end
