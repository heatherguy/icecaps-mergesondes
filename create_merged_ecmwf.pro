;
;create interpolated ECMWF fields for a given day
;
;input: date in the form yyyymmdd
;output: temp_interp (K) ;interpolated temperature profiles
;        q_interp (kg/kg)       ; interpolated Specific Humidity
;        u_interp (m/s)         ; interpolated V velocity
;        v_interp (m/s)         ; interpolated U velocity
;        height   (km)          ; height grid
;        t_interp    (sec)      ; time grid
;        pres_interp (mb)       ; pressure grid
;
;Nate Miller Jan 2014
;    updated April 2014

pro create_merged_ecmwf,yyyymmdd,temp_interp,q_interp,u_interp,v_interp,height,t_interp,pres_interp

yyyy = fix(yyyymmdd/10000.)
mm   = fix((yyyymmdd - yyyy*10000.)/100.)
dd   =  ((yyyymmdd - yyyy*10000l - mm*100l))

a=dblarr(137)
openr,1,'a_coeff_after20130625'
readf,1,a
close,1

b=dblarr(137)
openr,2,'b_coeff_after20130625'
readf,2,b
close,2

efiles = file_search('/gws/nopw/j04/ncas_radar_vol2/data/ICECAPSarchive/ecmwf/ecmwf_oper_*12to36_greenlandsummitarea_ml.nc')
esfiles = file_search('/gws/nopw/j04/ncas_radar_vol2/data/ICECAPSarchive/ecmwf/ecmwf_oper_*12to36_greenlandsummitarea_sl.nc')

esdate = strmid(esfiles,67,8)
edate = strmid(efiles,67,8)
eday  = fix(strmid(efiles,67,8),type=3)
esday  = fix(strmid(esfiles,67,8),type=3)
eyyyy = fix(strmid(edate,0,4))
emm   = fix(strmid(edate,4,2))
edd   = fix(strmid(edate,6,2))
esyyyy = fix(strmid(esdate,0,4))
esmm   = fix(strmid(esdate,4,2))
esdd   = fix(strmid(esdate,6,2))

euseday = where(eday eq yyyymmdd)
esuseday = where(esday eq yyyymmdd)
if total(euseday) lt 0 or total(esuseday) lt 0 then begin
    print,'no ecmwf files for this day'
    noecmwf = 1
endif else begin
  if emm(euseday[0]) eq 1 and edd(euseday[0]) eq 1 then begin
     GOTO,here                  ; assume that there are 2 files on the 31st of december
  endif

  ymdhms2julian,eyyyy(euseday[0]-1),emm(euseday[0]-1),edd(euseday[0]-1),0,0,0,jday_file
  ymdhms2julian,esyyyy(esuseday[0]-1),esmm(esuseday[0]-1),esdd(esuseday[0]-1),0,0,0,sjday_file
  ymdhms2julian,yyyy[0],mm[0],dd[0],0,0,0,jday_use

  ; if eday(euseday[0]-1) ne yyyymmdd-1 or  esday(esuseday[0]-1) ne yyyymmdd-1 then begin
   if jday_file ne jday_use-1 or  sjday_file ne jday_use-1 then begin
      print,'no ecmwf file the day before'
      noecmwf = 1
   
   endif else begin
  here: ;skipping down from above
    nuseday = n_elements(euseday)
    nsuseday = n_elements(esuseday)
    efilesday = efiles[euseday[0]-1:euseday[nuseday-1]]
    esfilesday = esfiles[esuseday[0]-1:esuseday[nsuseday-1]]
    nsday = n_elements(efilesday)
    nssday = n_elements(esfilesday)

     if nsday eq 2 and nssday eq 2 then noecmwf = 0 else begin
        noecmwf = 1
        print,'only one ECMWF file'
     endelse
   endelse
 ; endelse
endelse

site_alt = 3255./1000.
summit_lat = 72.596
summit_lon = -38.422+360.
;ECMWF indices
ilon = 9
ilat = 21



height = [0.000, 0.020, 0.040, 0.060, 0.080, 0.100, 0.120, 0.140, 0.160, $
    0.180, 0.200, 0.220, 0.240, 0.260, 0.280, 0.300, 0.320, 0.340, 0.360, $
    0.380, 0.400, 0.420, 0.440, 0.460, 0.480, 0.500, 0.520, 0.540, 0.560, $
    0.580, 0.600, 0.620, 0.640, 0.660, 0.680, 0.700, 0.720, 0.740, 0.760, $
    0.780, 0.800, 0.820, 0.840, 0.860, 0.880, 0.900, 0.920, 0.940, 0.960, $
    0.980, 1.000, 1.020, 1.040, 1.060, 1.080, 1.100, 1.120, 1.140, 1.160, $
    1.180, 1.200, 1.220, 1.240, 1.260, 1.280, 1.300, 1.320, 1.340, 1.360, $
    1.380, 1.400, 1.420, 1.440, 1.460, 1.480, 1.500, 1.520, 1.540, 1.560, $
    1.580, 1.600, 1.620, 1.640, 1.660, 1.680, 1.700, 1.720, 1.740, 1.760, $
    1.780, 1.800, 1.820, 1.840, 1.860, 1.880, 1.900, 1.920, 1.940, 1.960, $
    1.980, 2.000, 2.020, 2.040, 2.060, 2.080, 2.100, 2.120, 2.140, 2.160, $
    2.180, 2.200, 2.220, 2.240, 2.260, 2.280, 2.300, 2.320, 2.340, 2.360, $
    2.380, 2.400, 2.420, 2.440, 2.460, 2.480, 2.500, 2.520, 2.540, 2.560, $
    2.580, 2.600, 2.620, 2.640, 2.660, 2.680, 2.700, 2.720, 2.740, 2.760, $
    2.780, 2.800, 2.820, 2.840, 2.860, 2.880, 2.900, 2.920, 2.940, 2.960, $
    2.980, 3.000, 3.050, 3.100, 3.150, 3.200, 3.250, 3.300, 3.350, 3.400, $
    3.450, 3.500, 3.550, 3.600, 3.650, 3.700, 3.750, 3.800, 3.850, 3.900, $
    3.950, 4.000, 4.100, 4.200, 4.300, 4.400, 4.500, 4.600, 4.700, 4.800, $
    4.900, 5.000, 5.100, 5.200, 5.300, 5.400, 5.500, 5.600, 5.700, 5.800, $
    5.900, 6.000, 6.100, 6.200, 6.300, 6.400, 6.500, 6.600, 6.700, 6.800, $
    6.900, 7.000, 7.200, 7.400, 7.600, 7.800, 8.000, 8.200, 8.400, 8.600, $
    8.800, 9.000, 9.200, 9.400, 9.600, 9.800, 10.000, 10.200, 10.400, 10.600, $
    10.800, 11.000, 11.200, 11.400, 11.600, 11.800, 12.000, 12.200, 12.400, $
    12.600, 12.800, 13.000, 13.200, 13.400, 13.600, 13.800, 14.000, 14.200, $
    14.400, 14.600, 14.800, 15.000, 15.200, 15.400, 15.600, 15.800, 16.000, $
    16.200, 16.400, 16.600, 16.800,17.0,17.5,18.0,18.5,19.0,19.5,20.0,20.5,21.0,$
          21.5,22.0,22.5,findgen(38)+23.0]
    ;offset for Summit elevation
    height = height + (3.255)
    time = findgen(1440)*60.0



if noecmwf eq 1 then print, 'no file created for the day'+string(yyyymmdd) else begin

  ;expect 2 files
    fid = ncdf_open(efilesday[0])
    if edate(euseday) gt 20101109 then  ncdf_varget,fid,'forecast_time',fc_time
    if edate(euseday) le 20101109 then  ncdf_varget,fid,'forecast_time0',fc_time

    fc_time = fc_time - 24.0
     if edate(euseday) gt 20101109 then ncdf_varget,fid,'longitude',lon $
       else ncdf_varget,fid,'g0_lon_5',lon
      if edate(euseday) gt 20101109 then ncdf_varget,fid,'latitude',lat $
        else ncdf_varget,fid,'g0_lat_4',lat

    ;the format changed after 20130625
    if edate(euseday) le 20130625l then begin
      ncdf_varget,fid,'P0',p0
       if edate(euseday) gt 20101109 then begin
           ncdf_varget,fid,'hybrid_level_a_coeff',lv_HYBL1_a
           ncdf_varget,fid,'hybrid_level_b_coeff',lv_HYBL1_b
       endif else begin
           ncdf_varget,fid,'lv_HYBL1_a',lv_HYBL1_a
           ncdf_varget,fid,'lv_HYBL1_b',lv_HYBL1_b
       endelse
    endif else begin
      lv_HYBL1_a = a
      lv_HYBL1_b = b
      p0 = 1.0
    endelse
    ncdf_varget,fid,'q',q
    ncdf_varget,fid,'t',t
    ncdf_varget,fid,'u',u
    ncdf_varget,fid,'v',v
    ncdf_close,fid

    fid=ncdf_open(esfilesday[0])
    ncdf_varget,fid,'surface_pressure',sfc_pres
    ncdf_close,fid
    sfc_pres = sfc_pres/100.    ; Convert to mb
   
                          
;expect 9 forcast time per day
t0 = fltarr(9,n_elements(height))
q0 = fltarr(9,n_elements(height))
u0 = fltarr(9,n_elements(height))
v0 = fltarr(9,n_elements(height))
pres0 = fltarr(9,n_elements(height))
  ;start with hour 0 on theday in question (index 4)
  ; fillin the hours 0-9 UTC
    for idx=4,7 do begin
        
        tt = reform(t(ilon,ilat,*,idx))
        qq = reform(q(ilon,ilat,*,idx))
        uu = reform(u(ilon,ilat,*,idx))
        vv = reform(v(ilon,ilat,*,idx))
        pp = reform(sfc_pres(ilon,ilat,idx) * lv_HYBL1_b + lv_HYBL1_a*(p0/100.))

        tmp = hypsometric(reverse(pp),reverse(tt),3.225)
        
         t0(idx-4,*) = interpol(reverse(reform(tt)),tmp,height)
         q0(idx-4,*) = interpol(reverse(reform(qq)),tmp,height)
         u0(idx-4,*) = interpol(reverse(reform(uu)),tmp,height)
         v0(idx-4,*) = interpol(reverse(reform(vv)),tmp,height)
         pres0(idx-4,*) = interpol(reverse(reform(pp)),tmp,height)
    endfor
;repeat for the second file and to fill in the forcasttimes 12-24UTC
    fid = ncdf_open(efilesday[1])
    if edate(euseday) ge 20101109 then  ncdf_varget,fid,'forecast_time',fc_time1
    if edate(euseday) lt 20101109 then  ncdf_varget,fid,'forecast_time0',fc_time1

;    ncdf_varget,fid,'forecast_time',fc_time1
;    ncdf_varget,fid,'longitude',lon1
;    ncdf_varget,fid,'latitude',lat1
     if edate(euseday) ge 20101109 then ncdf_varget,fid,'longitude',lon1 $
       else ncdf_varget,fid,'g0_lon_5',lon1
      if edate(euseday) ge 20101109 then ncdf_varget,fid,'latitude',lat1 $
        else ncdf_varget,fid,'g0_lat_4',lat1

    if edate(euseday) lt 20130625l then begin
       ncdf_varget,fid,'P0',p01
              if edate(euseday) ge 20101109 then begin
           ncdf_varget,fid,'hybrid_level_a_coeff',lv_HYBL1_a1
           ncdf_varget,fid,'hybrid_level_b_coeff',lv_HYBL1_b1
       endif else begin
           ncdf_varget,fid,'lv_HYBL1_a',lv_HYBL1_a1
           ncdf_varget,fid,'lv_HYBL1_b',lv_HYBL1_b1
       endelse
       ;ncdf_varget,fid,'hybrid_level_a_coeff',lv_HYBL1_a1
       ;ncdf_varget,fid,'hybrid_level_b_coeff',lv_HYBL1_b1
   endif else begin
       lv_HYBL1_a1 = a
       lv_HYBL1_b1 = b
       p01 = 1.0
   endelse
    ncdf_varget,fid,'q',q1
    ncdf_varget,fid,'t',t1
    ncdf_varget,fid,'u',u1
    ncdf_varget,fid,'v',v1
    ncdf_close,fid

    fid=ncdf_open(esfilesday[1])
    ncdf_varget,fid,'surface_pressure',sfc_pres1
    ncdf_close,fid
    sfc_pres1 = sfc_pres1/100.    ; Convert to mb

    forecast_time = [fc_time[4:7],fc_time1[0:4]]
    
    ;start with hour 0 on theday in question (index 4)
    for idx=0,4 do begin
        
        tt = reform(t1(ilon,ilat,*,idx))
        qq = reform(q1(ilon,ilat,*,idx))
        uu = reform(u1(ilon,ilat,*,idx))
        vv = reform(v1(ilon,ilat,*,idx))
        pp = reform(sfc_pres1(ilon,ilat,idx) * lv_HYBL1_b1 + lv_HYBL1_a1*(p01/100.))
        tmp = hypsometric(reverse(pp),reverse(tt),3.225)
        
         t0(idx+4,*) = interpol(reverse(reform(tt)),tmp,height)
         q0(idx+4,*) = interpol(reverse(reform(qq)),tmp,height)
         u0(idx+4,*) = interpol(reverse(reform(uu)),tmp,height)
         v0(idx+4,*) = interpol(reverse(reform(vv)),tmp,height)
         pres0(idx+4,*) = interpol(reverse(reform(pp)),tmp,height)

    endfor

   
    t_interp = findgen(24*60.0)*60 ;find the number of seconds in a day
    temp_interp = fltarr(n_elements(t_interp),n_elements(height)) - 999.0
    q_interp    = fltarr(n_elements(t_interp),n_elements(height)) - 999.0
    u_interp    = fltarr(n_elements(t_interp),n_elements(height)) - 999.0
    v_interp    = fltarr(n_elements(t_interp),n_elements(height)) - 999.0
    pres_interp    = fltarr(n_elements(t_interp),n_elements(height)) - 999.0
    
    for j=0,n_elements(height)-1 do begin
        temp_interp[*,j] = INTERPOL(t0[*,j],forecast_time*3600l,t_interp)
        q_interp[*,j] = INTERPOL(q0[*,j],forecast_time*3600l,t_interp)
        u_interp[*,j] = INTERPOL(u0[*,j],forecast_time*3600l,t_interp)
        v_interp[*,j] = INTERPOL(v0[*,j],forecast_time*3600l,t_interp)
        pres_interp[*,j] = INTERPOL(pres0[*,j],forecast_time*3600l,t_interp)

    endfor
 

endelse


end
