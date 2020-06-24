;Nate Miller
;Jan 2014
;
;this program will produce profile values for a given day
;    and interpoled profile values (every minute)
;
;input: date in the form yyyymmdd
;
;output: (-999.0  and -9999.0 are non exisitant values)
;nosonde = 0 if there is a sounding on the specified day and at least 2 soundings
;          over the 3day period, nosonde = 1 if not true
;stime = time of the soundings launch (sec)
;temp  = temperature profiles (degC)
;rh    = relative humidity profiles (%)
;dpt   = dew point profiles (degC)
;pres  = pressure profiles  (mb)
;wspd  = wind speed profiles (m/s)
;wdir  = wind direction (deg from N)
;t_interp = time of the interpolated profiles (sec)
;temp_interp  = interpolated temperature profiles (degC)
;rh_interp    = interpolated relative humidity profiles (%)
;dpt_interp   = interpolated dew point profiles (degC)
;pres_interp  = interpolated pressure profiles  (mb)
;wspd_interp  = interpolated wind speed profiles (m/s)
;wdir_interp  = interpolated wind direction (deg from N)
;mixr_interp  = interpolated water vapor mixing ratios (g/kg) 
;height       =  height grid (km agl)
;
;

pro create_merged_sonde,yyyymmdd,nosonde,stime,temp,rh,dpt,pres,wspd,wdir,t_interp,$
            temp_interp,rh_interp,dpt_interp,pres_interp,wspd_interp,wdir_interp,mixr_interp,height


;find the radiosonde files around that day
sfiles = file_search('/gws/nopw/j04/ncas_radar_vol2/data/ICECAPSarchive/radiosonde/processed/smtsondewnpnX1*.cdf',count=scount)


stime=strmid(sfiles,63,15)
;get the sonde time components. year,month,day, hour, min
sdate=fix(strmid(stime,0,8),type=3)
syyyy=fix(strmid(stime,0,4))
smm=fix(strmid(stime,4,2))
sdd=fix(strmid(stime,6,2))
shh=fix(strmid(stime,9,2))
snn=fix(strmid(stime,11,2))
sss=fix(strmid(stime,13,2))
ymdhms2julian,syyyy,smm,sdd,shh,snn,sss,sjtime
stime = -999

;sondefiles during the day, plus adjacent soundings
suseday = where(sdate eq yyyymmdd)
if total(suseday) lt 0 then begin
    print,'no sonde files for this day'
    nosonde=1

    
endif else begin
    nosonde = 1
    nuseday = n_elements(suseday)
    sfilesday = sfiles[suseday[0]-1:suseday[nuseday-1]+1]
    nsday = n_elements(sfilesday)
    if nsday gt 1 then nosonde = 0 ; note: there must be 2 soundings
endelse



if nosonde eq 1 then print, 'no file created for the day'+string(yyyymmdd) else begin

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
    
    yy = fix(yyyymmdd/10000.,type=3)
    mm = fix(yyyymmdd/100.,type=3) - yy*100
    dd = fix(yyyymmdd,type=3) - yy*10000 - mm*100
    ymdhms2systime,yy,mm,dd,0,0,0,epoch
    basetime = epoch

    ;find the sonde times for theday and the interpolated profiles
    ;first interpolate to the height grid
    for i = 0,n_elements(sfilesday)-1 do begin
        ncdf_read_structure,sfilesday[i],scurrent
        btime_curr = scurrent.base_time
        alt_curr   = (scurrent.alt)/1000.0
        temp_curr   = scurrent.tdry
        rh_curr   = scurrent.rh
        dpt_curr   = scurrent.dpt
        pres_curr   = scurrent.pres
        wspd_curr   = scurrent.wspd
        wdir_curr   = scurrent.wdir    
        
        if i eq 0 then begin
            maxh = alt_curr[n_elements(altcurr)-1]
            
            btime = btime_curr
            if maxh gt alt_curr[0] then begin; make sure more than 1 level
                temp = transpose(INTERPOL(temp_curr,alt_curr,height))
                rh = transpose(INTERPOL(rh_curr,alt_curr,height))
                dpt = transpose(INTERPOL(dpt_curr,alt_curr,height))
                pres = transpose(INTERPOL(pres_curr,alt_curr,height))
                wspd = transpose(INTERPOL(wspd_curr,alt_curr,height))
                wdir = transpose(INTERPOL(wdir_curr,alt_curr,height))
            ;maxh = alt_curr[n_elements(altcurr)-1]
            endif else begin

                temp_curr_all = fltarr(n_elements(height)) - 999.0
                rh_curr_all = fltarr(n_elements(height)) - 999.0
                dpt_curr_all = fltarr(n_elements(height)) - 999.0
                pres_curr_all = fltarr(n_elements(height)) - 999.0
                wspd_curr_all = fltarr(n_elements(height)) - 999.0
                wdir_curr_all = fltarr(n_elements(height)) - 999.0
                
                temp_curr_all[0] = temp_curr
                rh_curr_all[0] = rh_curr
                dpt_curr_all[0] = dpt_curr
                pres_curr_all[0] = pres_curr
                wspd_curr_all[0] = wspd_curr
                wdir_curr_all[0] = wdir_curr

                temp = transpose(temp_curr_all)
                rh = transpose(rh_curr_all)
                dpt =  transpose(dpt_curr_all)
                pres =  transpose(pres_curr_all)
                wspd =  transpose(wspd_curr_all)
                wdir =  transpose(wdir_curr_all)
            endelse

            
        endif else begin
            btime = [btime,btime_curr]
            maxh = [maxh,alt_curr[n_elements(altcurr)-1]]
            if maxh[i] gt alt_curr[0] then begin ; make sure more than 1 level
                temp = [temp,transpose(INTERPOL(temp_curr,alt_curr,height))]
                rh = [rh,transpose(INTERPOL(rh_curr,alt_curr,height))]
                dpt = [dpt,transpose(INTERPOL(dpt_curr,alt_curr,height))]
                pres = [pres,transpose(INTERPOL(pres_curr,alt_curr,height))]
                wspd = [wspd,transpose(INTERPOL(wspd_curr,alt_curr,height))]
                wdir = [wdir,transpose(INTERPOL(wdir_curr,alt_curr,height))]
            endif else begin
                temp_curr_all = fltarr(n_elements(height)) - 999.0
                rh_curr_all = fltarr(n_elements(height)) - 999.0
                dpt_curr_all = fltarr(n_elements(height)) - 999.0
                pres_curr_all = fltarr(n_elements(height)) - 999.0
                wspd_curr_all = fltarr(n_elements(height)) - 999.0
                wdir_curr_all = fltarr(n_elements(height)) - 999.0
                
                temp_curr_all[0] = temp_curr
                rh_curr_all[0] = rh_curr
                dpt_curr_all[0] = dpt_curr
                pres_curr_all[0] = pres_curr
                wspd_curr_all[0] = wspd_curr
                wdir_curr_all[0] = wdir_curr

                temp = [temp,transpose(temp_curr_all)]
                rh = [rh,transpose(rh_curr_all)]
                dpt = [dpt,transpose(dpt_curr_all)]
                pres = [pres,transpose(pres_curr_all)]
                wspd = [wspd,transpose(wspd_curr_all)]
                wdir = [wdir,transpose(wdir_curr_all)]

            endelse

            ;maxh = [maxh,alt_curr[n_elements(altcurr)-1]]

            
        endelse
        ;insert -999 values above the max height of the sounding
        badh = where(height ge maxh[i])
       if total(badh) ge 0 then begin
        temp[i,badh] = -999.0
        rh[i,badh] = -999.0
        dpt[i,badh] = -999.0
        pres[i,badh] = -999.0
        wspd[i,badh] = -999.0
        wdir[i,badh] = -999.0
       endif
       
    endfor

    mixr = rh*0.0 -999.0
    frh = rh/100.0

 ;get water vapor mixing ratio g/kg
    for k=0,n_elements(sfilesday)-1 do begin
            goodrh = where(rh[k,*] ge 0.0 and rh[k,*] le 105.0)
            mixr[k,goodrh] = rh2w(temp[k,goodrh],frh[k,goodrh],pres[k,goodrh],/sonde_rh_err)
        endfor


    ;the time (sec) with respect to the day
    stime = btime - basetime
   
    ;then interpolate each level to the timegrid
    t_interp = findgen(24*60.0)*60.0 
    temp_interp = fltarr(n_elements(t_interp),n_elements(height)) - 999.0
    rh_interp = fltarr(n_elements(t_interp),n_elements(height))   - 999.0
    dpt_interp = fltarr(n_elements(t_interp),n_elements(height))  - 999.0
    pres_interp = fltarr(n_elements(t_interp),n_elements(height)) - 999.0
    wspd_interp = fltarr(n_elements(t_interp),n_elements(height)) - 999.0
    wdir_interp = fltarr(n_elements(t_interp),n_elements(height)) - 999.0
    mixr_interp = fltarr(n_elements(t_interp),n_elements(height)) - 999.0


  ;only interpolate if there is more than 1 value for that height level
    for j=0,n_elements(height)-1 do begin
    
        temp_good = where(temp[*,j] gt -273.0)
        if total(n_elements(temp_good))gt 1 then begin
            t_good   = where(t_interp ge stime[temp_good[0]] and t_interp le stime[temp_good[n_elements(temp_good)-1]])  
            temp_interp[t_good,j] = INTERPOL(temp[temp_good,j],stime[temp_good],t_interp[t_good])
        endif
        rh_good = where(rh[*,j] ge 0)
        if total(n_elements(rh_good))gt 1 then begin
            t_good   = where(t_interp ge stime[rh_good[0]] and t_interp le stime[rh_good[n_elements(rh_good)-1]])  
            rh_interp[t_good,j] = INTERPOL(rh[rh_good,j],stime[rh_good],t_interp[t_good])
        endif
        dpt_good = where(dpt[*,j] ge -273.0)
        if total(n_elements(dpt_good))gt 1 then begin
            t_good   = where(t_interp ge stime[dpt_good[0]] and t_interp le stime[dpt_good[n_elements(dpt_good)-1]])  
            dpt_interp[t_good,j] = INTERPOL(dpt[dpt_good,j],stime[dpt_good],t_interp[t_good])
        endif
        pres_good = where(pres[*,j] ge 0.0)
        if total(n_elements(pres_good))gt 1 then begin
            t_good   = where(t_interp ge stime[pres_good[0]] and t_interp le stime[pres_good[n_elements(pres_good)-1]])  
            pres_interp[t_good,j] = INTERPOL(pres[pres_good,j],stime[pres_good],t_interp[t_good])
        endif
        wspd_good = where(wspd[*,j] ge 0)
        if total(n_elements(wspd_good))gt 1 then begin
            t_good   = where(t_interp ge stime[wspd_good[0]] and t_interp le stime[wspd_good[n_elements(wspd_good)-1]])  
            wspd_interp[t_good,j] = INTERPOL(wspd[wspd_good,j],stime[wspd_good],t_interp[t_good])
        endif
        wdir_good = where(wdir[*,j] ge 0)
        if total(n_elements(wdir_good))gt 1 then begin
            t_good   = where(t_interp ge stime[wdir_good[0]] and t_interp le stime[wdir_good[n_elements(wdir_good)-1]])  
            wdir_interp[t_good,j] = INTERPOL(wdir[wdir_good,j],stime[wdir_good],t_interp[t_good])
        endif

         mixr_good = where(mixr[*,j] ge 0)
        if total(n_elements(mixr_good))gt 1 then begin
            t_good   = where(t_interp ge stime[mixr_good[0]] and t_interp le stime[mixr_good[n_elements(mixr_good)-1]])  
            mixr_interp[t_good,j] = INTERPOL(mixr[mixr_good,j],stime[mixr_good],t_interp[t_good])
        endif
        ;rh_interp[*,j] = INTERPOL(rh[*,j],stime,t_interp)
        ;dpt_interp[*,j] = INTERPOL(dpt[*,j],stime,t_interp)
        ;pres_interp[*,j] = INTERPOL(pres[*,j],stime,t_interp)
        ;if n_elements(wspd_good) gt 1 then wspd_interp[*,j] = INTERPOL(wspd[wspd_good,j],stime[wspd_good],t_interp)
        ;if n_elements(wdir_good) gt 1 then wdir_interp[*,j] = INTERPOL(wdir[wdir_good,j],stime[wdir_good],t_interp)
    endfor

endelse




end
