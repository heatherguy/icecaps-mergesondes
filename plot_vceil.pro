; $Id: plot_vceil.pro,v 1.7 2010/10/05 15:40:00 dturner Exp $
;+
; Abstract:
;	This routine plots up a time-height cross-section of the Vaisala Ceilometer
;    (VCEIL) data.  The routine also has the capability to derive a simple cloud
;    mask for the data and to create a DABUL-like netCDF file (so the data can be
;    used in MIXCRA easily).  
;
; Author:
;	Dave Turner
;		Pacific Northwest National Laboratory
;
; Date created:
;	Dec 2004
;
; Call:
  pro plot_vceil, $
	filename, $			; Name of the file to plot
	dostop=dostop, $		; Set this to stop inside the routine
	no_new_window=no_new_window, $	; Set this if you do NOT want a new window to open
	makeplot=makeplot, $		; Set this to make a PNG image of the data
	maxcloud=maxcloud, $		; The maximum height a cloud may exist
	mincloud=mincloud, $		; The minimum height a cloud may exist
	maxbscat=maxbscat, $		; The maximum backscatter value to use in plot
	bscatmult=bscatmult, $		; Multiplier for backscattering field
	mask=mask, $			; Set this to apply the cloud mask to the data

			; Keywords associated with pixel classification.  Needed
			; for the updated version of IPT.
	classify=classify, $		; Set this to apply the pixel 
					;    classification to the data. The bit values 
					;    are: 0 -> clear, 1-> liquid, 2 -> ice, 
					;         3 -> aerosol
	maxaerosol=maxaerosol, $	; Maximum height an aerosol layer may exist
	cldphase=cldphase, $		; Phase of the cloud to assign (1->liq, 2->ice)
	thres_aer=thres_aer, $		; Min threshold for aerosol signal
	thres_cld=thres_cld, $		; Min threshold for cloud signal

			; Output
	output_dabul=output_dabul	; Set this to output a DABUL-like file for MIXCRA
;-

  		; Set some default values
  if(n_elements(classify) eq 0)   then classify = 0
  if(n_elements(maxaerosol) eq 0) then maxaerosol = 1.0
  if(n_elements(cldphase) eq 0)   then cldphase = 1
  if(n_elements(thres_aer) eq 0)  then thres_aer = 30.
  if(n_elements(thres_cld) eq 0)  then thres_cld = 50.
  if(n_elements(maxbscat) eq 0)   then maxbscat = 150.
  if(n_elements(bscatmult) eq 0)  then bscatmult = 1.

  cldphase = fix(cldphase)
  if(cldphase lt 1 or cldphase gt 2) then begin
    print,'Error in plot_vceil: Cloud phase must be either 1 or 2'
    return
  endif

  mnth = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
  
  files = findfile(filename, count=count)
  if(count ne 1) then begin
    print,'Error: Unable to unambiguously determine ' + filename
    return
  endif
  fid = ncdf_open(files(0))
  ncdf_varget,fid,'base_time',bt
  ncdf_varget,fid,'time_offset',to
  ncdf_varget,fid,'range',ht
  ncdf_varget,fid,'detection_status',dstatus
  ncdf_varget,fid,'first_cbh',cbh1
  ncdf_varget,fid,'second_cbh',cbh2
  ncdf_varget,fid,'third_cbh',cbh3
  if(ncdf_varexist(fid,'alt_highest_signal') eq 1) then $
  	ncdf_varget,fid,'alt_highest_signal',maxht $
  else maxht = replicate(max(ht),n_elements(to))
  ncdf_varget,fid,'backscatter',bscat
  ncdf_close,fid
  systime2ymdhms,bt+to,yy,mm,dd,hour=hour
  bscat = bscat * bscatmult

  	; This little test was added to fix a problem with Matt Shupe's ingest
	; of Summit data.  But this should be removed at some point in the future.
  if(n_elements(bscat(*,0)) ne n_elements(ht)) then bscat = transpose(bscat)

  ht = ht / 1000.	; Convert m to km
  maxht = maxht / 1000.	; Convert m to km
  cbh1 = cbh1 / 1000.	; Convert m to km
  cbh2 = cbh2 / 1000.	; Convert m to km
  cbh3 = cbh3 / 1000.	; Convert m to km

  if(n_elements(mincloud) eq 0) then mincloud = 0
  if(n_elements(maxcloud) eq 0) then maxcloud = max(ht) + 0.1

  xtickn = string(format='(I2.2)',[0,3,6,9,12,15,18,21,24])
  xr = [min(float(xtickn)),max(float(xtickn))]
  xminor = 6
  ytickn = string(format='(I0)',[0,1,2,3,4,5,6,7])
  yr = [min(float(ytickn)),max(float(ytickn))]
  yminor = 5
  vals = fix(findgen(5)/4*maxbscat + 0.5)
  ztickn = string(format='(I0)',vals)
  zr = [min(float(ztickn)),max(float(ztickn))]
  zminor = 5

  loadct,0
  gamma_ct,0.6
  if(!d.name eq 'X' and not keyword_set(no_new_window)) then  $
  	window,!d.window+1,xs=800,ys=500
  !p.multi=[0,1,3]
  pchars = 2.0
  dcontour,bscat,hour,ht, $
  	'Hour [UTC]', xtickn, xr, xminor, $
	'Altitude [km]', ytickn, yr, yminor, $
	'Backscatter [arbitrary]', ztickn, zr(0), zr(1), zminor, $
	[0.02,0.35,0.90,0.95], [0.89,0.35,1,0.95], pchars, $
	'VCEIL Data for ' + string(format='(I0,1x,A,1x,I0)',dd(0),mnth(mm(0)-1),yy(0))
  oplot,[0,24],[1,1]*maxcloud,color=255,thick=2
  oplot,[0,24],[1,1]*mincloud,color=255,thick=2

  @color_syms.include
  usersym,d_circle
  oplot,hour,cbh3,psym=3,color=d_blue
  oplot,hour,cbh2,psym=3,color=d_green
  oplot,hour,cbh1,psym=3,color=d_red

  !p.region=[0.02,0,0.90,0.35]
  ytickn2 = ['Clr','1 CBH','2 CBH','3 CBH','Obscured','Unknown']
  yr2 = [-0.2,5.2]
  yminor2 = 1
  plot,hour,dstatus,chars=pchars, psym=7, syms=0.5, $
        xticklen=-0.03,yticklen=-0.005, $
  	xtickn=xtickn, xticks=n_elements(xtickn)-1, xr=xr, /xst, xminor=xminor, $
	xtit='Hour [UTC]', yr=yr2, /yst, ytickn=ytickn2, ytit='Detection Status', $
	yticks=n_elements(ytickn2)-1, yminor=yminor2

  !p.multi=0
  !p.region=0

  if(keyword_set(makeplot)) then begin
    fname = 'vceil.' + string(format='(I0,I2.2,I2.2)',yy(0),mm(0),dd(0)) + '.png'
    saveimage,fname
  endif

  if(keyword_set(mask) or keyword_set(output_dabul)) then begin
    		
		;------------------
		; Apply a threshold from the data to find aerosol pixels
    mask = bscat * 0
    thres = thres_aer		
    foo = where(bscat gt thres, nfoo)
    if(nfoo gt 0) then mask(foo) = 1

		; Now apply a median filter
    amask = median(mask,9)
    		; Now apply a smoothing filter, with more emphasis on the time dimension
    amask = smooth(amask, [3,15])
    		; Now make it a binary mask again using another threshold value
    thres = 0.8
    foo = where(amask gt thres,nfoo)
    amask = amask * 0
    if(nfoo gt 0) then amask(foo) = 1

		; Now remove any aerosol pixes below/above the aerosol max height
    foo = where(ht gt maxaerosol, nfoo)
    if(nfoo gt 0) then amask(foo,*) = 0


		;------------------
		; Apply a threshold from the data to find cloudy pixel
    mask = bscat * 0
    thres = thres_cld
    foo = where(bscat gt thres, nfoo)
    if(nfoo gt 0) then mask(foo) = 1

		; Now apply a median filter
    nmask = median(mask,9)
    		; Now apply a smoothing filter, with more emphasis on the time dimension
    nmask = smooth(nmask, [3,15])
    		; Now make it a binary mask again using another threshold value
    thres = 0.8
    foo = where(nmask gt thres,nfoo)
    nmask = nmask * 0
    if(nfoo gt 0) then nmask(foo) = 1

    		; Finally, if a cloud base is identified in cbh1, cbh2, or cbh3, then
		; make sure that it is recorded in the cloud mask too.
    foo = where(cbh1 gt 0, nfoo)
    for i=0,nfoo-1 do begin
      bar = where(ht ge cbh1(foo(i)),nbar)
      if(nbar gt 0) then nmask(bar(0),foo(i)) = 1
    endfor
    foo = where(cbh2 gt 0, nfoo)
    for i=0,nfoo-1 do begin
      bar = where(ht ge cbh2(foo(i)),nbar)
      if(nbar gt 0) then nmask(bar(0),foo(i)) = 1
    endfor
    foo = where(cbh3 gt 0, nfoo)
    for i=0,nfoo-1 do begin
      bar = where(ht ge cbh3(foo(i)),nbar)
      if(nbar gt 0) then nmask(bar(0),foo(i)) = 1
    endfor

		; Now remove any clouds below/above the cloud min/max
    foo = where(ht gt maxcloud, nfoo)
    if(nfoo gt 0) then nmask(foo,*) = 0
    foo = where(ht lt mincloud, nfoo)
    if(nfoo gt 0) then nmask(foo,*) = 0

		; Now generate a backscatter and depolarization field
    nbscat = bscat * 0.
    ndepol = bscat * 0.
    foo = where(nmask gt 0, nfoo)
    if(nfoo gt 0) then begin
      nbscat(foo) = bscat(foo)
      ndepol(foo) = 0.10			; linear depol as a fraction
    endif

		; Combine the two masks (cloud and aerosol) into a single 
		; classification mask with the proper values:
		;	Clear -> 0, liquid -> 1, ice -> 2, aerosol -> 3
    class = amask
    foo = where(amask gt 0, nfoo)
    if(nfoo gt 0) then class(foo) = 2^(3-1)
    foo = where(nmask gt 0, nfoo)
    if(nfoo gt 0) then class(foo) = 2^(cldphase-1)

    loadct,0
    gamma_ct,0.6
    if(!d.name eq 'X' and not keyword_set(no_new_window)) then  $
  	window,!d.window+1,xs=800,ys=500
    !p.multi=[0,1,3]
    dcontour,nbscat,hour,ht, $
  	'Hour [UTC]', xtickn, xr, xminor, $
	'Altitude [km]', ytickn, yr, yminor, $
	'Backscatter [arbitrary]', ztickn, zr(0), zr(1), zminor, $
	[0.02,0.35,0.90,0.95], [0.89,0.35,1,0.95], pchars, $
	'Masked VCEIL Data for ' + $
	string(format='(I0,1x,A,1x,I0)',dd(0),mnth(mm(0)-1),yy(0))
    oplot,[0,24],[1,1]*maxcloud,color=255,thick=2
    oplot,[0,24],[1,1]*mincloud,color=255,thick=2
  
    @color_syms.include
    usersym,d_circle
    oplot,hour,cbh3,psym=3,color=d_blue
    oplot,hour,cbh2,psym=3,color=d_green
    oplot,hour,cbh1,psym=3,color=d_red

    !p.region=[0.02,0,0.90,0.35]
    plot,hour,dstatus,chars=pchars, psym=7, syms=0.5, $
        xticklen=-0.03,yticklen=-0.005, $
  	xtickn=xtickn, xticks=n_elements(xtickn)-1, xr=xr, /xst, xminor=xminor, $
	xtit='Hour [UTC]', yr=yr2, /yst, ytickn=ytickn2, ytit='Detection Status', $
	yticks=n_elements(ytickn2)-1, yminor=yminor2

    !p.multi=0
    !p.region=0

    if(keyword_set(makeplot)) then begin
      fname = 'vceil.masked.'+string(format='(I0,I2.2,I2.2)',yy(0),mm(0),dd(0))+'.png'
      saveimage,fname
    endif

    		; Make a plot, if classify is set
    if(classify) then begin ; {
      dloadct,42
      if(!d.name eq 'X' and not keyword_set(no_new_window)) then  $
    	window,!d.window+1,xs=800,ys=500
      !p.multi=[0,1,3]
      zr = [0,5]
      ztickn = ['C','L','I','M','A',' ']
      dcontour,class,hour,ht, $
  	'Hour [UTC]', xtickn, xr, xminor, $
	'Altitude [km]', ytickn, yr, yminor, $
	'Classification', ztickn, zr(0), zr(1), zminor, $
	[0.02,0.35,0.90,0.95], [0.89,0.35,1,0.95], pchars, $
	'Classified VCEIL Data for ' + $
	string(format='(I0,1x,A,1x,I0)',dd(0),mnth(mm(0)-1),yy(0))
      oplot,[0,24],[1,1]*maxcloud,color=255,thick=2
      oplot,[0,24],[1,1]*mincloud,color=255,thick=2
  
      !p.multi=0
      !p.region=0

      if(keyword_set(makeplot)) then begin
        fname = 'vceil.classified.'+string(format='(I0,I2.2,I2.2)', $
		yy(0),mm(0),dd(0))+'.png'
        saveimage,fname
      endif
    endif ; end of if classify }

    if(keyword_set(output_dabul)) then begin
      		; Convert the backscatter to dB
		;   There will be some samples where the backscatter is zero
		;   or negative yet a cloud was identified there.  Replace these
		;   values with the mean BSCAT values.
      feh = where(nmask gt 0 and nbscat gt 0, nfeh)
      if(nfeh gt 0) then mean_nbscat = 10*alog10(mean(nbscat(feh))) $
      else mean_nbscat = 20.
      foo = where(nmask gt 0, nfoo)
      if(nfoo gt 0) then nbscat(foo) = 10.* alog10(nbscat(foo))
      foo = where(finite(nbscat) eq 0, nfoo)
      if(nfoo gt 0) then nbscat(foo) = mean_nbscat

      filename = string(format='(A,I0,I2.2,I2.2,A)', $
      		"dabul.mask.",yy(0),mm(0),dd(0),".cdf")
      print,'  Creating the file ' + filename
      fid = ncdf_create(filename, /clobber)
      dimtid = ncdf_dimdef(fid,'record',/unlimited)
      dimhid = ncdf_dimdef(fid,'level',n_elements(ht))
      vid = ncdf_vardef(fid,'time',dimtid,/float)
      vid = ncdf_vardef(fid,'range',dimhid,/float)
      vid = ncdf_vardef(fid,'depolarization',[dimhid,dimtid],/float)
      vid = ncdf_vardef(fid,'far_parallel',[dimhid,dimtid],/float)
      vid = ncdf_vardef(fid,'mask_original',[dimhid,dimtid],/short)
      vid = ncdf_vardef(fid,'mask_new',[dimhid,dimtid],/short)
      if(classify) then vid = ncdf_vardef(fid,'classification',[dimhid,dimtid],/short)
      ncdf_attput,fid,/global,'Comment', $
	'Mocked up DABUL-like file created from VCEIL data'
      ncdf_attput,fid,/global,'MaxCloudAlt',string(format='(F7.3,A)',maxcloud,' km')
      ncdf_attput,fid,/global,'MinCloudAlt',string(format='(F7.3,A)',mincloud,' km')
      ncdf_control, fid, /endef
      ncdf_varput, fid, 'time', hour
      ncdf_varput, fid, 'range', ht*1000.		; Convert km to m
      ncdf_varput, fid, 'depolarization', ndepol
      ncdf_varput, fid, 'far_parallel', nbscat
      ncdf_varput, fid, 'mask_original', fix(nmask+0.5)
      ncdf_varput, fid, 'mask_new', fix(nmask+0.5)
      if(classify) then ncdf_varput, fid, 'classification', fix(class+0.5)
      ncdf_close, fid
    endif

  endif

  if(keyword_set(dostop)) then stop,'Stopped inside routine'
  return
end
