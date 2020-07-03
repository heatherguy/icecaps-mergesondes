; $Id: pltaeri_qmeresids.pro,v 1.3 1998/05/27 18:29:23 turner Release_ddt_1_13 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;+
; Abstract:
;	This script plots up the QME AERI/LBLRTM residual as a function of wavenumber,
;    and includes in the AERI spectrum as a point of reference.  The region of each
;    dominating species is also indicated across the spectrum
;
; Author:       Dave Turner, PNNL
; Date created: Auguest, 1997
;
; Keywords:
;	CH:		0 (the default) is channel 1, 1 plots channel 2
;	SCALE:		0 (the default) uses normal LBLRTM runs, 1 uses LSSONDE 
;				LBLRTM runs
;	AERI_PATH:	The path to the AERI data
;	QME_PATH:	The path to the QME AERI/LBLRTM residual data
;	NOEXTRACT:	If set, all of the QME samples in the validation set are 
;				retained; otherwise only the ones driven with the
;				AERI surface temperature are kept (one per validation)
;
; Call:
	pro pltaeri_qmeresids, aeri_path=aeri_path, qme_path=qme_path, ch=ch, $
		scale=scale, noextract=noextract
;-

  @color_syms.include

  if(keyword_set(ch)) then ch = 1 else ch = 0

  if(ch eq 0) then begin
    if(n_elements(aeri_path) eq 0) then $
      aeri_path = '/data/sgp/sgpaeri01ch1C1.a1'
    aeriroot = '*aeri*ch1*'
    aeri_string = 'AERI channel 1 (5.5 - 18 microns) radiance data'
    xrng=[550,1800]
    yrng=[-10,15]
  endif else begin
    if(n_elements(aeri_path) eq 0) then $
      aeri_path = '/data/sgp/sgpaeri01ch2C1.a1'
    aeriroot = '*aeri*ch2*'
    aeri_string = 'AERI channel 2 (3.3 - 5.5 microns) radiance data'
    xrng=[1800,3000]
    yrng=[-0.5,1.0]
  endelse

  if(keyword_set(scale)) then scale = 1 else scale = 0

  if(scale eq 0) then begin
    if(n_elements(qme_path) eq 0) then $
      qme_path = '/data/sgp/sgpaerilbldiffC1.c1'
    qmeroot = '*aerilbldiff*cdf'
    qme_title = 'QME AERI/LBLRTM residual (observed - calculated)'
  endif else begin
    if(n_elements(qme_path) eq 0) then $
      qme_path = '/data/sgp/sgpaerilbldifflsC1.c1'
    qmeroot = '*aerilbldiffls*cdf'
    qme_title = 'QME AERI/LBLRTM residual (observed - calculated) -- sonde input scaled'
  endelse

  pushd, qme_path
  qmefile = dialog_pickfile(filter=qmeroot)
  if(qmefile eq '') then begin
    print,'Operation cancelled'
    return
  endif

  fid = ncdf_open(qmefile)
  ncdf_varget,fid,'base_time',bt
  ncdf_varget,fid,'time_offset',to
  ncdf_varget,fid,'surface_temp_from_aeri_used',aeri_flag
  ncdf_varget,fid,'wavenumber',qwnum
  ncdf_varget,fid,'rad_difference',rad_diff
  ncdf_close,fid
  zt2hhmmss, bt, to, qymd, qhms
  popd
  
	; Reduce the QME data to include only that with the aeri_flag
	; set, unless the keyword indicated not to
  if(not keyword_set(noextract)) then begin
    foo = where(aeri_flag gt 0, nhits)
    if(nhits le 0) then begin
      print,'No samples where flagged with the QME aeri flag, implying no data'
      return
    endif
    qymd = qymd(foo)
    qhms = qhms(foo)
    rad_diff = rad_diff(*,foo)
  endif

  pushd, aeri_path
  aerifilename = aeriroot + string(format='(I6.6)', qymd(0)) + '*cdf'
  aerifile = findfile(aerifilename,count=count)
  if(count ne 1) then begin
    print,'Unable to find/determine ' + aerifilename
    popd
    return
  endif

  fid = ncdf_open(aerifile(0))
  ncdf_varget,fid,'base_time',bt
  ncdf_varget,fid,'time_offset',to
  ncdf_varget,fid,'wnum',awnum
  ncdf_varget,fid,'mean_rad',mean_rad
  ncdf_close,fid
  zt2hhmmss, bt, to, aymd, ahms
  popd
  
                ; List of the main process before the given wavenumber
  bound = [631,$  ; h2o
          705,$   ; sat co2
          799,$   ; co2
          1001,$  ; h2o
          1067,$  ; o3
          1353,$  ; h2o
          1872,$  ; sat h2o
          2286,$  ; h2o
          2386,$  ; sat co2
          2598,$  ; n2
          3500]   ; h2o

                ; These processes are h2o, co2 (sat), co2, o3, h2o (sat), n2o, and n2
  process = ['!CH!D2!NO', 'Sat!CCO!D2!N', '!CCO!D2!N', '!CO!D3!N', $
                'Sat!CH!D2!NO!C!D', '!CN!D2!NO', '!CN!D2!N']

  again:
  index = -1
  print,'Select the QME index to plot:'
  for i=0, n_elements(qhms) -1 do $
    print,'  ' + string(format='(I2)',i) + '  ' + string(format='(I6.6)', qymd(i)) + $
		'.' + string(format='(I6.6)', qhms(i))

  read,index,prompt='Or -1 to quit > '
  if(index lt 0) then goto, quit
  if(index ge n_elements(qhms)) then goto, again

  foo = where((aymd eq qymd(index)) and (ahms eq qhms(index)), nhits)
  if(nhits le 0) then begin
    print, 'Unable to find an AERI sample for index ' + string(format='(I2)',index)
    goto, again
  endif 
  if(nhits gt 1) then begin
    stop, 'Whooa!  Found too many AERI samples!'
    return
  endif

  !p.multi=[0,1,2]
  !p.region=[0,0.3,1,0.90]
  rad_max = max(mean_rad(*, foo))
  if(ch eq 0) then rad_max = (fix(rad_max) / 10) * 10 + 10 $
  else rad_max = fix(rad_max) + 1
  plot, awnum, mean_rad(*, foo), color=pmain, /nodata, $
	ytitle='mW / (m2 ster cm-1)', xcharsize=0.01, $
	xstyle=1, xr=xrng, ystyle=1, yr=[0,rad_max]
  xyouts, 0.5, 0.96, aeri_string + '!C' + string(format='(I6.6)', aymd(foo)) + $
	'.' + string(format='(I6.6)', ahms(foo)), $
	/normal, charsize=1.5, align=0.5, color=pmain
  oplot, awnum, mean_rad(*,foo), color=d_red
  if(ch eq 0) then begin
    oplot, [bound(0),bound(0)], [-100,200], color=d_green
    oplot, [bound(1),bound(1)], [-100,200], color=d_green
    oplot, [bound(2),bound(2)], [-100,200], color=d_green
    oplot, [bound(3),bound(3)], [-100,200], color=d_green
    oplot, [bound(4),bound(4)], [-100,200], color=d_green
    oplot, [bound(5),bound(5)], [-100,200], color=d_green
    oplot, [bound(6),bound(6)], [-100,200], color=d_green
    xyouts, 0.12, 0.90, process(0), charsize=1.2, color=d_green, align=0.5, /normal
    xyouts, 0.18, 0.90, process(1), charsize=1.2, color=d_green, align=0.5, /normal
    xyouts, 0.24, 0.90, process(2), charsize=1.2, color=d_green, align=0.5, /normal
    xyouts, 0.33, 0.90, process(0), charsize=1.2, color=d_green, align=0.5, /normal
    xyouts, 0.43, 0.90, process(3), charsize=1.2, color=d_green, align=0.5, /normal
    xyouts, 0.55, 0.90, process(0), charsize=1.2, color=d_green, align=0.5, /normal
    xyouts, 0.80, 0.90, process(4), charsize=1.2, color=d_green, align=0.5, /normal
  endif else begin
    oplot, [bound(6),bound(6)], [-100,200], color=d_green
    oplot, [bound(7),bound(7)], [-100,200], color=d_green
    oplot, [bound(8),bound(8)], [-100,200], color=d_green
    oplot, [bound(9),bound(9)], [-100,200], color=d_green
    oplot, [bound(10),bound(10)], [-100,200], color=d_green
    xyouts, 0.12, 0.90, process(4), charsize=1.2, color=d_green, align=0.5, /normal
    xyouts, 0.30, 0.90, process(0), charsize=1.2, color=d_green, align=0.5, /normal
    xyouts, 0.48, 0.90, process(1), charsize=1.2, color=d_green, align=0.5, /normal
    xyouts, 0.60, 0.90, process(6), charsize=1.2, color=d_green, align=0.5, /normal
    xyouts, 0.84, 0.90, process(0), charsize=1.2, color=d_green, align=0.5, /normal
  endelse

  !p.region=[0,0,1,0.35]
  plot, qwnum, rad_diff(*,index), color=pmain, /nodata, $
	ytitle='mW / (m2 ster cm-1)', xtitle='wavenumber (cm-1)', $
	xstyle=1, xr=xrng, ystyle=1, yr=yrng, $
	title=qme_title
  oplot, [0,50000], [0,0], color=pmain, linestyle=2
  oplot, qwnum, rad_diff(*,index), color=d_red
  if(ch eq 0) then begin
    oplot, [bound(0),bound(0)], [-100,200], color=d_green
    oplot, [bound(1),bound(1)], [-100,200], color=d_green
    oplot, [bound(2),bound(2)], [-100,200], color=d_green
    oplot, [bound(3),bound(3)], [-100,200], color=d_green
    oplot, [bound(4),bound(4)], [-100,200], color=d_green
    oplot, [bound(5),bound(5)], [-100,200], color=d_green
    oplot, [bound(6),bound(6)], [-100,200], color=d_green
  endif else begin
    oplot, [bound(6),bound(6)], [-100,200], color=d_green
    oplot, [bound(7),bound(7)], [-100,200], color=d_green
    oplot, [bound(8),bound(8)], [-100,200], color=d_green
    oplot, [bound(9),bound(9)], [-100,200], color=d_green
    oplot, [bound(10),bound(10)], [-100,200], color=d_green
  endelse


  goto, again

  quit:
  !p.region=0
  !p.multi=0
end
