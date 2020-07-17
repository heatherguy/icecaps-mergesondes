;Heather Guy
;July 2020
;(Adapted from create_merged_statret.pro by Nate Miller)
;
;input: date in the form yyyymmdd
;
;
;if there is a mwrret file for that day create an interpolated product
;
;
;output:
;
;nomwroe = 1 if no data, = 0 if there is a mwroe retrieval for the day
;mtime = time (sec) of the mwroe
;mtemp = temp profiles interpolated to height grid
;t_interp = time (sec) for the time fully interpolated profiles 
;temp_interp = temperature interpolated to time and height grid
;height = alt grid up to 2km
;


pro create_merged_mwroe,yyyymmdd,nomwroe,mtime,mtemp,t_interp,temp_interp,height



;find the mwroe files around that day
mfiles = file_search('/gws/nopw/j04/ncas_radar_vol1/heather/mwroe/smt10mwroe1turnC1.d1.*.cdf',count=mcount)
mtime=strmid(mfiles,65,15)
;get the time components. year,month,day, hour, min
mdate=fix(strmid(mtime,0,8),type=3)
myyyy=fix(strmid(mtime,0,4))
mmm=fix(strmid(mtime,4,2))
mdd=fix(strmid(mtime,6,2))
mhh=fix(strmid(mtime,9,2))
mnn=fix(strmid(mtime,11,2))
mss=fix(strmid(mtime,13,2))
ymdhms2julian,myyyy,mmm,mdd,mhh,mnn,mss,mjtime


;mwroe during the day, plus adjacent files
museday = where(mdate eq yyyymmdd)
if total(museday) lt 0 then begin
    print,'no mwroe files for this day'
    nomwroe = 1
    endif else begin
    mfilesday = mfiles[museday-1:museday+1]
    nmday = n_elements(mfilesday)
    nomwroe = 0
endelse




if nomwroe eq 1 then print, 'no file created for the day'+string(yyyymmdd) else begin

    ; output heights in km asl
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
    2.380, 2.400, 2.420, 2.440, 2.460, 2.480, 2.500]
  ;, 2.520, 2.540, 2.560, $
  ;  2.580, 2.600, 2.620, 2.640, 2.660, 2.680, 2.700, 2.720, 2.740, 2.760, $
  ;  2.780, 2.800, 2.820, 2.840, 2.860, 2.880, 2.900, 2.920, 2.940, 2.960, $
  ;  2.980, 3.000, 3.050, 3.100, 3.150, 3.200, 3.250, 3.300, 3.350, 3.400, $
  ;  3.450, 3.500, 3.550, 3.600, 3.650, 3.700, 3.750, 3.800, 3.850, 3.900, $
  ;  3.950, 4.000, 4.100, 4.200, 4.300, 4.400, 4.500, 4.600, 4.700, 4.800, $
  ;  4.900, 5.000, 5.100, 5.200, 5.300, 5.400, 5.500, 5.600, 5.700, 5.800, $
  ;  5.900, 6.000, 6.100, 6.200, 6.300, 6.400, 6.500, 6.600, 6.700, 6.800, $
  ;  6.900, 7.000, 7.200, 7.400, 7.600, 7.800, 8.000, 8.200, 8.400, 8.600, $
  ;  8.800, 9.000, 9.200, 9.400, 9.600, 9.800, 10.000, 10.200, 10.400, 10.600, $
  ;  10.800, 11.000, 11.200, 11.400, 11.600, 11.800, 12.000, 12.200, 12.400, $
  ;  12.600, 12.800, 13.000, 13.200, 13.400, 13.600, 13.800, 14.000, 14.200, $
  ;  14.400, 14.600, 14.800, 15.000, 15.200, 15.400, 15.600, 15.800, 16.000, $
  ;  16.200, 16.400, 16.600, 16.800]
    ;offset for Summit elevation
    height = height + (3.255)
    time = findgen(1440)*60.0
    
    yy = fix(yyyymmdd/10000.,type=3)
    mm = fix(yyyymmdd/100.,type=3) - yy*100
    dd = fix(yyyymmdd,type=3) - yy*10000 - mm*100
    ymdhms2systime,yy,mm,dd,0,0,0,epoch
    basetime = epoch

    ;find the mwrret times for theday and the interpolated profiles
    ;first interpolate to the height grid ( first 2km)
    for i = 0,n_elements(mfilesday)-1 do begin
        ncdf_read_structure,mfilesday[i],mcurrent
        btime_curr = mcurrent.base_time
        
        ; altitude is in km agl - convert to km asl
        alt_curr   = (mcurrent.height) + (3.255)


        temp_curr   = mcurrent.temperature
        
                                ;the time (sec) with respect to the
                                ;day
        time_offset_curr = mcurrent.time_offset + btime_curr - basetime
        tempint_curr = fltarr(n_elements(height),n_elements(time_offset_curr))
         for k=0,n_elements(time_offset_curr)- 1 do begin
             tempint_curr[*,k] = INTERPOL(temp_curr[*,k],alt_curr,height)
         endfor
    
        if i eq 0 then begin
            btime = btime_curr
             
            mtime = time_offset_curr
            temp = transpose(tempint_curr)
            
        endif else begin
            btime = [btime,btime_curr]
            mtime = [mtime,time_offset_curr]
            temp = [temp,transpose(tempint_curr)]
        endelse

    endfor

    
   mtemp = temp
    ;then interpolate each level to the timegrid
   
    t_interp = findgen(24*60.0)*60.0 
    temp_interp = fltarr(n_elements(t_interp),n_elements(height)) - 999.0

    for j=0,n_elements(height)-1 do begin

        temp_interp[*,j] = INTERPOL(temp[*,j],mtime,t_interp)

    endfor


endelse

;stop


end
