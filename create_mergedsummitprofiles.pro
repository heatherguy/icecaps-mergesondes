;Nate Miller
;Jan - April 2014
;
;some of the variables can be adjusted such as the time between
;sondes. ie. if the time between sondes is X hours and the time
;between sondes is  less than X there will onlt be a linear
;interploation between the two. if greater, the model data will be
;blended into the profiles according to the variable = window_sonde
;
;The profiles will always extend up to 20km where the ECMWF modeled
;data will be blended to fill in above the top of the sounding. 
;
;Temperature retrievals from mwrstatret are blended with the
;temperture profiles below 2km. From 2-2.5 km the profiles are blended
;as well. 
;
;PWV from MWRRET is used to scale the blended (radiosonde and ECMWF)
;moisture profiles;
;if the percent error if gt 50% the profile isn't
;scaled. between 20%-50% the scaling is weighted. 
;
;input: yyyymmdd = day to be merged (yearmonthday)
;
;
;output:
;       t_interp    = time of the day (seconds) 1440 data points
;       height      = height (km above MSL) 250 data points
;       temp_merge  = merged temperature profiles (degC) [t_interp,height]
;       mixr_merge  = merged and scaled mixing ratio profiles (g/kg) [t_interp,height]
;       pres_merge  = merged pressure profiles [t_interp,height]
;       model_w     = weight of modeled temperture profile [0-1]
;       sonde_w     = weight of radiosonde temperture profile [0-1]
;       stat_w     = weight of statistical temperture profile [0-1]
;       model_w_moist     = weight of modeled mixing ratio profile [0-1]
;       sonde_w_moist     = weight of radiosonde mixing ratio profile [0-1]
;       model_w_pres     = weight of modeled pressure profile [0-1]
;       sonde_w_pres     = weight of radiosonde pressure profile [0-1]
;       sf_merge         = scale factor for getting the mixr_merge
;                          profile to equal the MWRRET PWV. sf =
;                              sonde_pwv/merged_pwv
;       qc_temp          = all temp weights add to 1? [0=good or 1=bad]
;       qc_mixr          = all mixr weights add to 1? [0=good or 1=bad]
;       qc_pres          = all pres weights add to 1? [0=good or 1=bad]
;       qc_pwv           = residuals of the scaled profiles less than 0.01? [0=good or 1=bad]
;
;
;
;updated July 2014
pro create_mergedsummitprofiles,yyyymmdd,height,t_interp,temp_merge,mixr_merge,pres_merge,$
                                model_w,sonde_w,stat_w,model_w_moist,sonde_w_moist,$
                                model_w_pres,sonde_w_pres,sf_merge,qc_temp,qc_mixr,qc_pres,qc_pwv

;amount of time (seconds) to linearly decease the sonde weighting to zero
window_sonde = 10.0 * 60.0 * 60.0

hours_between_sondes = 14.0 ;mimimun time between sondes before linear interpolation is performed.
not_at_end = 0

;get the modeled profiles
create_merged_ecmwf,yyyymmdd,mtemp_interp,mq_interp,mu_interp,mv_interp,height,t_interp,mpres_interp

if keyword_set(mtemp_interp) eq 0 then print, 'no file created for this day' else begin

    mtemp_interp = mtemp_interp -273.15

    model_w = fltarr(n_elements(t_interp),n_elements(height))
    sonde_w = fltarr(n_elements(t_interp),n_elements(height))
    stat_w = fltarr(n_elements(t_interp),n_elements(height))


    ;get the sonde interpolated values
    create_merged_sonde,yyyymmdd,nosonde,stime,stemp,srh,sdpt,spres,swspd,swdir,st_interp,$
            stemp_interp,srh_interp,sdpt_interp,spres_interp,swspd_interp,swdir_interp,smixr_interp,sheight

                                ;nosonde =0 if there are at least 2
                                ;sondes and one during the day in
                                ;question. 
    if nosonde eq 1 then begin
        temp_merge = mtemp_interp
        model_w[*,*]  = 1.0
    endif else begin
        temp_merge = mtemp_interp*0
      ;fill in the upper levels with model data if neccesary
        for j=0,n_elements(st_interp)-1 do begin
            ;number of vertical levels to smooth over
            nv = 5
            curr_bad = where(stemp_interp[j,*] lt -273.)
            curr_good = where(stemp_interp[j,*] ge -273.)
            
            sonde_w[j,curr_good] = 1 ;set weight to one if good data
            
            if n_elements(curr_bad) lt nv then nv = n_elements(curr_bad)
                                ; make sure there are -999 points and
                                ; that the entire profile isn't -999.
            if curr_bad[0] gt 0 and n_elements(curr_bad) ne n_elements(height) then begin
                for w=0,nv-1 do begin
                                ;average the sounding value one level
                                ;below the current level and the model
                                ;value at that level
                    stemp_interp[j,curr_bad[w]] = stemp_interp[j,curr_bad[w]-1]/2.0 + (mtemp_interp[j,curr_bad[w]])/2.0
                    sonde_w[j,curr_bad[w]] = sonde_w[j,curr_bad[w]-1]/2.0
                    model_w[j,curr_bad[w]] = model_w[j,curr_bad[w]]+1 - sonde_w[j,curr_bad[w]]
                endfor
                if n_elements(curr_bad) gt nv then begin
                    stemp_interp[j,curr_bad[0]+nv:curr_bad[n_elements(curr_bad)-1]] = $
                                 mtemp_interp[j,curr_bad[0]+nv:curr_bad[n_elements(curr_bad)-1]]
                    model_w[j,curr_bad[0]+nv:curr_bad[n_elements(curr_bad)-1]] = 1.
                endif
            endif
        
        endfor
        temp_merge = stemp_interp
        
                                ;only use the model values below the
                                ;sonde max height if there is more than x hours
                                ;between soundings
        
        theday = where(stime ge 0 and stime le 86340)
                                ;I except at least one sounding for
                                ;the day and one adjacent sounding on
                                ;each side
        t_start = 0
        t_end = n_elements(t_interp)-1
      for i=0,n_elements(theday)-1 do begin
         
          if stime[theday[i]] - stime[theday[i]-1] gt hours_between_sondes*60*60. then begin
              ;mesh the time period before the sounding
              window_time = where(t_interp ge stime[theday[i]]-window_sonde and t_interp le stime[theday[i]])
             
              sonde_w_slope = (1.0/window_sonde)*(t_interp[window_time]-stime[theday[i]]) + 1.0
              for k=0,n_elements(height)-1 do begin
                  sonde_w[window_time,k] = sonde_w[window_time,k]*sonde_w_slope
                  model_w[window_time,k] = 1.0 - sonde_w[window_time,k]
                                ;set sonde weight to 0 between t_start
                                ;and beginning of window time
                  if window_time[0] gt t_start then sonde_w[t_start:window_time[0]-1,k] = 0
                  
                 
                  temp_merge[window_time,k] = sonde_w[window_time,k]*stemp_interp[window_time,k] + $
                    model_w[window_time,k]*mtemp_interp[window_time,k]

              endfor
              
                                
              
           endif
           if stime[theday[i]+1] - stime[theday[i]] gt hours_between_sondes*60*60. then begin
              ;mesh the time period after the sounding
              window_time = where(t_interp le stime[theday[i]]+window_sonde and t_interp ge stime[theday[i]])
             
              sonde_w_slope = (-1.0/window_sonde)*(t_interp[window_time]-stime[theday[i]]) + 1.0
              
              for k=0,n_elements(height)-1 do begin
                  sonde_w[window_time,k] = sonde_w[window_time,k]*sonde_w_slope
                  model_w[window_time,k] = 1.0 - sonde_w[window_time,k]
                  
                  
                  temp_merge[window_time,k] = sonde_w[window_time,k]*stemp_interp[window_time,k] + $
                    model_w[window_time,k]*mtemp_interp[window_time,k]

              endfor
              t_start = window_time[n_elements(window_time)-1]
              not_at_end = 1 ;set to 1 if interpolated after the sounding
            endif
            
        

      endfor
   ;after the loop through all the soundings, fill in the data after the
   ;meshed data with modeled data
      if not_at_end eq 1 then begin
          if  t_start lt n_elements(t_interp)-1 then begin
              sonde_w[t_start+1:n_elements(t_interp)-1,*] = 0.
              temp_merge[t_start+1:n_elements(t_interp)-1,*] = mtemp_interp[t_start+1:n_elements(t_interp)-1,*]
              model_w[t_start+1:n_elements(t_interp)-1,*] = 1.0
          endif
      endif

      for w=0,n_elements(height)-1 do begin
           fillin = where(sonde_w[*,w] eq 0)
          if total(fillin) ge 0 then begin
             
              model_w[fillin,w] = 1.0
              temp_merge[fillin,w] = mtemp_interp[fillin,w]
          endif
      endfor

    endelse


;this is the end of the merging of the sondes and ECMWF modeled
;temperature data

;next merge in the MWRSTATRET data and the MWRRET data. 
;find the mwrstatret files around that day
mfiles = file_search('/psd3data/arctic/summit/mwr/mwrstattempret/smtmwrprof1millX1*.cdf',count=mcount)

mtime=strmid(mfiles,64,15)


;get the time components. year,month,day, hour, min
mdate=fix(strmid(mtime,0,8),type=3)
;mdate=fix(strmid(mtime,0,8),type=3) - 20100000l ; make with out MWRSTATRET



;mwrstatretfiles during the day, plus adjacent files
museday = where(mdate eq yyyymmdd)


create_merged_statret,yyyymmdd,nomwrstatret,stattime,stattemp,statt_interp,stattemp_interp,statheight

;amount of time (seconds) to linearly decease the sonde weighting to zero
window_sonde = 5.0 * 60.0 * 60.0
;top index of the fully weighted mwrstatret, 100 =2 km
tindex = 100
;number of points above the mwrstatret to merge, 124 = 2.5km
nabove = 25

;replace the information below 2.0km with sonde and mwrstatret values
;think about how to deal with incomplete mwrstatret days
;assume that theya  are full? have minumum # of times?
if nomwrstatret eq 0 then begin ; there is data

 
    stattheday = where(stattime ge 0. and stattime le (86340.))
                                ;before 20120701 there where about 36
                                ;proflies per day and about 66 after
                                ;this time. Only replace the values
                                ;below 2km with statret values if
                                ;there is a minimum of 30 and 60
                                ;profiles respectively for the day.
    
  if yyyymmdd le 20120701 then tot = 30
  if yyyymmdd gt 20120701 then tot = 60
  if n_elements(stattheday) gt tot then begin
   if nosonde eq 0 then begin
    theday = where(stime ge -3600. and stime le (86340+3600.))
    if total(stime(theday)) gt 0 then begin
        for j = 0,n_elements(stime(theday))-1 do begin
            ;time after the sonde
            window_time = where(t_interp le stime[theday[j]]+window_sonde and t_interp ge stime[theday[j]])
            if total(window_time) ge 0 then begin
             both_w_slope = (-1.0/window_sonde)*(t_interp[window_time]-stime[theday[j]]) + 1.0
            
             for k=0,n_elements(statheight[0:tindex])-1 do begin
                  sonde_w[window_time,k] = sonde_w[window_time,k]*both_w_slope
                  model_w[window_time,k] = model_w[window_time,k]*both_w_slope
                  stat_w[window_time,k] = 1.0 - sonde_w[window_time,k] - model_w[window_time,k]

                   temp_merge[window_time,k] = stemp_interp[window_time,k]*sonde_w[window_time,k]+ $ 
                    mtemp_interp[window_time,k]*model_w[window_time,k] $
                    + stat_w[window_time,k]*stattemp_interp[window_time,k]

                  
                  ;temp_merge[window_time,k] = sonde_w[window_time,k]*stemp_interp[window_time,k] + $
                   ; stat_w[window_time,k]*stattemp_interp[window_time,k]
              endfor
             endif
              ;time before the sonde
             window_time = where(t_interp ge stime[theday[j]]-window_sonde and t_interp le stime[theday[j]])
             if total(window_time) ge 0 then begin
              
              
              both_w_slope = (1.0/window_sonde)*(t_interp[window_time]-stime[theday[j]]) + 1.0
            
              for k=0,n_elements(statheight[0:tindex])-1 do begin
                  sonde_w[window_time,k] = sonde_w[window_time,k]*both_w_slope
                  model_w[window_time,k] = model_w[window_time,k]*both_w_slope
                  stat_w[window_time,k] = 1.0 - sonde_w[window_time,k] - model_w[window_time,k]
                  
               
                  temp_merge[window_time,k] = stemp_interp[window_time,k]*sonde_w[window_time,k]+ $ 
                    mtemp_interp[window_time,k]*model_w[window_time,k] $
                    + stat_w[window_time,k]*stattemp_interp[window_time,k]

             
              endfor

             endif
         endfor

    endif  
         
    ;fillin the rest of the data with mwrstatret values below 2km
     onlytheday = where(stime ge 0.0 and stime le (86340))
     for j = 0,n_elements(statheight[0:tindex])-1 do begin
        
        fill = where(stat_w[*,j] eq 0)
        stat_w[fill,j] = 1.0
        sonde_w[fill,j] = 0.0
        model_w[fill,j] = 0.0
                               ;reintroduce the sonde weights at
                                ;sonde time to equal 1 if there are
                                ;sondes on the day
        if total([onlytheday]) ge 0 then begin
            for k=0,n_elements(stime[onlytheday])-1 do begin
                refill = where(statt_interp eq stime[onlytheday[k]])
                stat_w[refill,j] = 0.0
                sonde_w[refill,j] = 1.0
            endfor   
        endif
      stattemp_fill = where(stat_w[*,j] eq 1.0)
      temp_merge[stattemp_fill,j] = stattemp_interp[stattemp_fill,j]
     endfor
    
    ;start with the weights of (sonde + model)
    w_other= sonde_w+model_w
    ;the step size varies according to the weight over time
    steps = (1.0 - w_other[*,tindex])/(nabove)
   endif else begin
       ;if there is no sonde data
       for j = 0,n_elements(statheight[0:tindex])-1 do begin
        
        fill = where(stat_w[*,j] eq 0)
        stat_w[fill,j] = 1.0
        sonde_w[fill,j] = 0.0
        model_w[fill,j] = 0.0
       
        stattemp_fill = where(stat_w[*,j] eq 1.0)
        temp_merge[stattemp_fill,j] = stattemp_interp[stattemp_fill,j]

       endfor

       ;start with the weights of (sonde + model)
       w_other= sonde_w+model_w
       ;the step size varies according to the weight over time
       steps = (1.0 - w_other[*,tindex])/(nabove)

   endelse


   
    ;smooth profiles above 2km to 2.5km
    for n=tindex+1,tindex+nabove do begin
        
        w_other[*,n] = w_other[*,n-1] + steps ; increment the weights of the others

        w =  (1.0 - w_other[*,n]) ;weight for statret for the height
;      
        temp_merge[*,n] = w*stattemp_interp[*,n] + w_other[*,n]*temp_merge[*,n]
        stat_w[*,n] = w
        sonde_w[*,n] = w_other[*,n]*sonde_w[*,n]
        model_w[*,n] = w_other[*,n]*model_w[*,n]

       
        
        
    endfor

    
  endif
endif
check1 = sonde_w[*,*] + model_w[*,*] + stat_w[*,*]
check = where((check1) ne 1)


if max(check1) lt 1.0001 and min(check1) gt 0.00009 then begin

    print,'phew:the temp weights all add up to 1.'
    qc_temp = 0
endif

if max(check1) gt 1.0001 or min(check1) lt 0.00009 then begin
    print,'warning: the temp weights do not add up to 1'
    qc_temp = 1
endif

;find the mwrret files around that day
;rfiles = file_search('/psd3data/arctic/summit/mwr/mwrret/smtmwrret2turn*.cdf',count=rcount)

;rtime=strmid(rfiles,55,15)
;find the mwrret3 files around that day
rfiles = file_search('/psd3data/arctic/summit/mwr/mwrret3/smtmwrret3turn*.cdf',count=rcount)

rtime=strmid(rfiles,56,15)
;get the time components. year,month,day, hour, min
rdate=fix(strmid(rtime,0,8),type=3)
ryyyy=fix(strmid(rtime,0,4))
rmm=fix(strmid(rtime,4,2))
rdd=fix(strmid(rtime,6,2))
rhh=fix(strmid(rtime,9,2))
rnn=fix(strmid(rtime,11,2))
rss=fix(strmid(rtime,13,2))
;ymdhms2julian,ryyyy,rmm,rdd,rhh,rnn,rss,rjtime


;mwrretfiles during the day, plus adjacent files
ruseday = where(rdate eq yyyymmdd)
if total(ruseday) lt 0 then begin
    print,'no mwrret files for this day'
    nomwrret = 1
endif else begin
    nomwrret  = 0

    ncdf_read_structure,rfiles(ruseday[0]),mwrret
    qctest = where(mwrret.manual_qc_hatpro eq 0 and mwrret.manual_qc_hf eq 0 and mwrret.phys_pwv ge 0 $
                  and mwrret.elevation eq 90.0)
    if total(qctest) lt 0 or n_elements(qctest) eq 1 then nomwrret = 1

endelse

if nomwrret eq 0 then begin


    qctest = where(mwrret.manual_qc_hatpro eq 0 and mwrret.manual_qc_hf eq 0 and mwrret.elevation eq 90)

    yy = fix(yyyymmdd/10000.,type=3)
    mm = fix(yyyymmdd/100.,type=3) - yy*100
    dd = fix(yyyymmdd,type=3) - yy*10000 - mm*100
    ymdhms2systime,yy,mm,dd,0,0,0,epoch
    basetime = epoch

    btime_curr = mwrret.base_time
    timeofday = mwrret.time_offset(qctest) + btime_curr - basetime
    ;only interpolate to times with in 3 minutes of pwv values
    interptime = where(t_interp ge timeofday[0]-180.0 and t_interp le timeofday[n_elements(timeofday)-1]+180.0)

    t_interp_pwv = t_interp(interptime)
    
    lwp = mwrret.phys_lwp(qctest)
    pwv = mwrret.phys_pwv(qctest)
    pwv_sigma = mwrret.PHYS_PWV_UNCERTAINTY(qctest)
    pwv_perror = (pwv_sigma*1.0/pwv*1.0)*100.0 ;find the percent error


    pwv_interp = INTERPOL(pwv,timeofday,t_interp_pwv)
    lwp_interp = INTERPOL(lwp,timeofday,t_interp_pwv)
    pwv_perror_interp = INTERPOL(pwv_perror,timeofday,t_interp_pwv)
    


endif else begin
    
    pwv_interp = -999.0
    lwp_interp = -999.0
    t_interp_pwv = -999.0
    
endelse

;; q  = r  / (1.0 + r)	; Convert mixing ratio to specific humidity

model_w_moist = fltarr(n_elements(t_interp),n_elements(height))
sonde_w_moist = fltarr(n_elements(t_interp),n_elements(height))

model_w_pres = fltarr(n_elements(t_interp),n_elements(height))
sonde_w_pres = fltarr(n_elements(t_interp),n_elements(height))


;get the merged moisture profiles
    
                                ;nosonde =0 if there are at least 2
                                ;sondes and one during the day in
                               ;question. 

;amount of time (seconds) to linearly decease the sonde weighting to
;zero
;if commented out then use the vlaues at the top
;;window_sonde = 10.0 * 60.0 * 60.0

;;hours_between_sondes = 14.0 ;mimimun time between sondes before linear interpolation is performed.
not_at_end = 0


;convert model profiles from specific humidity to mixing ratio
mmixr_interp = mq_interp/(1-mq_interp)
mmixr_interp = mmixr_interp*1000.0
    if nosonde eq 1 then begin
        mixr_merge = mmixr_interp  
        pres_merge = mpres_interp
        model_w_moist[*,*]    = 1.0
        model_w_pres[*,*]    = 1.0
    endif else begin
        mixr_merge = mtemp_interp*0 - 999.0
        pres_merge = mtemp_interp*0 - 999.0

        
             ;fill in the upper levels with model data if neccesary
        for j=0,n_elements(st_interp)-1 do begin
            ;number of vertical levels to smooth over
            nv = 5
            nv_p1 = 5 ; pressure levels missing might be different
            curr_bad = where(smixr_interp[j,*] lt 0.0)
            curr_good = where(smixr_interp[j,*] ge 0.0)
            curr_badpres = where(spres_interp[j,*] lt 0.0)
            curr_goodpres = where(spres_interp[j,*] ge 0.0)
            
            if total(curr_good) ge 0 then sonde_w_moist[j,curr_good] = 1 ;set weight to one if good data
            if total(curr_goodpres) ge 0 then sonde_w_pres[j,curr_goodpres] = 1 

            if n_elements(curr_bad) lt nv then nv = n_elements(curr_bad)
            if n_elements(curr_badpres) lt nv_p1 then nv_p1 = n_elements(curr_badpres)

                                ; make sure there are -999 points and
                                ; that the entire profile isn't -999.
            if curr_bad[0] gt 0 and n_elements(curr_bad) ne n_elements(height) then begin
                for w=0,nv-1 do begin
                                ;average the sounding value one level
                                ;below the current level and the model
                                ;value at that level
                    smixr_interp[j,curr_bad[w]] = smixr_interp[j,curr_bad[w]-1]/2.0 + (mmixr_interp[j,curr_bad[w]])/2.0
                    sonde_w_moist[j,curr_bad[w]] = sonde_w_moist[j,curr_bad[w]-1]/2.0
                    model_w_moist[j,curr_bad[w]] = model_w_moist[j,curr_bad[w]]+1 - sonde_w_moist[j,curr_bad[w]]
                endfor
                
                if n_elements(curr_bad) gt nv then begin
                    smixr_interp[j,curr_bad[0]+nv:curr_bad[n_elements(curr_bad)-1]] = $
                                 mmixr_interp[j,curr_bad[0]+nv:curr_bad[n_elements(curr_bad)-1]]
                    model_w_moist[j,curr_bad[0]+nv:curr_bad[n_elements(curr_bad)-1]] = 1.
                endif
            endif

            ;repeat for missing pressure values
            ; make sure there are -999 points and
                                ; that the entire profile isn't -999.
            if curr_badpres[0] gt 0 and n_elements(curr_badpres) ne n_elements(height) then begin
                for w=0,nv_p1-1 do begin
                                ;average the sounding value one level
                                ;below the current level and the model
                                ;value at that level
                                ;convert spres below to estimate level
                                ;above using Hypometric equation
                    ;estimate scale height
                      Tmean = ((stemp_interp[j,curr_badpres[w]]+stemp_interp[j,curr_badpres[w]-1])/2.0)+273.15
                      H = 287.0*Tmean/9.8
                      Z = (height[curr_badpres[w]]-height[curr_badpres[w]-1])*1000.0
                    spres_interp[j,curr_badpres[w]] = (spres_interp[j,curr_badpres[w]-1]*exp(-1.0*Z/H))/2.0 + (mpres_interp[j,curr_badpres[w]])/2.0
                    ;spres_interp[j,curr_badpres[w]] = (spres_interp[j,curr_badpres[w]-1])/2.0 + (mpres_interp[j,curr_badpres[w]])/2.0
                    sonde_w_pres[j,curr_badpres[w]] = sonde_w_pres[j,curr_badpres[w]-1]/2.0
                    model_w_pres[j,curr_badpres[w]] = model_w_pres[j,curr_badpres[w]]+1 - sonde_w_pres[j,curr_badpres[w]]
                endfor
                
                if n_elements(curr_badpres) gt nv_p1 then begin
                    spres_interp[j,curr_badpres[0]+nv_p1:curr_badpres[n_elements(curr_badpres)-1]] = $
                                 mpres_interp[j,curr_badpres[0]+nv_p1:curr_badpres[n_elements(curr_badpres)-1]]
                    model_w_pres[j,curr_badpres[0]+nv_p1:curr_badpres[n_elements(curr_badpres)-1]] = 1.
                endif
            endif
        
        endfor

        mixr_merge = smixr_interp
        pres_merge = spres_interp
        
          ;only use the model values below the
                                ;sonde max height if there is more than x hours
                                ;between soundings
        
        theday = where(stime ge 0 and stime le 86340)
                                ;I except at least one sounding for
                                ;the day and one adjacent sounding on
                                ;each side
        t_start = 0
        t_end = n_elements(t_interp)-1
      for i=0,n_elements(theday)-1 do begin
         
          if stime[theday[i]] - stime[theday[i]-1] gt hours_between_sondes*60*60. then begin
              ;mesh the time period before the sounding
              window_time = where(t_interp ge stime[theday[i]]-window_sonde and t_interp le stime[theday[i]])
             
              sonde_w_slope = (1.0/window_sonde)*(t_interp[window_time]-stime[theday[i]]) + 1.0
              for k=0,n_elements(height)-1 do begin
                  sonde_w_moist[window_time,k] = sonde_w_moist[window_time,k]*sonde_w_slope
                  model_w_moist[window_time,k] = 1.0 - sonde_w_moist[window_time,k]
                  sonde_w_pres[window_time,k] = sonde_w_pres[window_time,k]*sonde_w_slope
                  model_w_pres[window_time,k] = 1.0 - sonde_w_pres[window_time,k]
                                ;set sonde weight to 0 between t_start
                                ;and beginning of window time
                  if window_time[0] gt t_start then sonde_w_moist[t_start:window_time[0]-1,k] = 0
                  if window_time[0] gt t_start then sonde_w_pres[t_start:window_time[0]-1,k] = 0
                  
                  
                  mixr_merge[window_time,k] = sonde_w_moist[window_time,k]*smixr_interp[window_time,k] + $
                    model_w_moist[window_time,k]*mmixr_interp[window_time,k]
                  pres_merge[window_time,k] = sonde_w_pres[window_time,k]*spres_interp[window_time,k] + $
                    model_w_pres[window_time,k]*mpres_interp[window_time,k]

              endfor                           
              
          endif

           if stime[theday[i]+1] - stime[theday[i]] gt hours_between_sondes*60*60. then begin
              ;mesh the time period after the sounding
              window_time = where(t_interp le stime[theday[i]]+window_sonde and t_interp ge stime[theday[i]])
             
              sonde_w_slope = (-1.0/window_sonde)*(t_interp[window_time]-stime[theday[i]]) + 1.0
              
              for k=0,n_elements(height)-1 do begin
                  sonde_w_moist[window_time,k] = sonde_w_moist[window_time,k]*sonde_w_slope
                  model_w_moist[window_time,k] = 1.0 - sonde_w_moist[window_time,k]

                  sonde_w_pres[window_time,k] = sonde_w_pres[window_time,k]*sonde_w_slope
                  model_w_pres[window_time,k] = 1.0 - sonde_w_pres[window_time,k]
                                 
                  mixr_merge[window_time,k] = sonde_w_moist[window_time,k]*smixr_interp[window_time,k] + $
                    model_w_moist[window_time,k]*mmixr_interp[window_time,k]

                  pres_merge[window_time,k] = sonde_w_pres[window_time,k]*spres_interp[window_time,k] + $
                    model_w_pres[window_time,k]*mpres_interp[window_time,k]

              endfor
              t_start = window_time[n_elements(window_time)-1]
              not_at_end = 1 ;set to 1 if interpolated after the sounding
            endif
            
        

        endfor

   ;after the loop through all the soundings, fill in the data after the
   ;meshed data with modeled data
      if not_at_end eq 1 then begin
          if  t_start lt n_elements(t_interp)-1 then begin
              sonde_w_moist[t_start+1:n_elements(t_interp)-1,*] = 0.
              mixr_merge[t_start+1:n_elements(t_interp)-1,*] = mmixr_interp[t_start+1:n_elements(t_interp)-1,*]
              model_w_moist[t_start+1:n_elements(t_interp)-1,*] = 1.0

              sonde_w_pres[t_start+1:n_elements(t_interp)-1,*] = 0.
              pres_merge[t_start+1:n_elements(t_interp)-1,*] = mpres_interp[t_start+1:n_elements(t_interp)-1,*]
              model_w_pres[t_start+1:n_elements(t_interp)-1,*] = 1.0
          endif
      endif
      
      for w=0,n_elements(height)-1 do begin
           fillin = where(sonde_w_moist[*,w] eq 0)
           fillinpres = where(sonde_w_pres[*,w] eq 0)
          if total(fillin) ge 0 then begin
             
              model_w_moist[fillin,w] = 1.0
              mixr_merge[fillin,w] = mmixr_interp[fillin,w]
          endif
          if total(fillinpres) ge 0 then begin
             
              model_w_pres[fillinpres,w] = 1.0
              pres_merge[fillinpres,w] = mpres_interp[fillinpres,w]
          endif
      endfor

      
      
    endelse

check2 = sonde_w_moist[*,*] + model_w_moist[*,*]
checkme = where((check2) ne 1)
if total(checkme) lt 0 then begin
 print,'phew:the moisture weights all add up to 1.'
 qc_mixr = 0
endif else begin
    print,'warning: the  moisture weights do not add up to 1'
    qc_mixr = 1
endelse

check3 = sonde_w_pres[*,*] + model_w_pres[*,*]
checkmeplease = where((check3) ne 1)
if total(checkmeplease) lt 0 then begin
    print,'phew:the pressure weights all add up to 1.'
    qc_pres = 0
endif else begin
    print,'warning: the  pressure weights do not add up to 1'
    qc_pres = 1
endelse

;store the mixr values before scaling to PWV
mixr_merge_nosf = mixr_merge

sf_merge = fltarr(n_elements(t_interp)) - 999.0

if nomwrret eq 0 then begin
   pwv_merge = fltarr(n_elements(t_interp)) - 999.0
  

   for w=0,n_elements(t_interp)-1 do begin

       mixr_curr = reform(mixr_merge[w,*])
       pres_curr = reform(pres_merge[w,*])
       pwv_curr = w2pwv(mixr_curr,pres_curr)
       
       pwv_merge[w] = pwv_curr
   
   endfor


pcent_high = 40.0
pcent_low = 20.0
   ;scale the merged profiles if pwv from MWRRET exists
   match,t_interp,t_interp_pwv,ti,tip ;matches the indices for the two times
   for i=0,n_elements(ti)-1 do begin

       perror_curr = pwv_perror_interp[tip[i]]

; thought - - if less than 20% error then scale. If greater than 40%
;           error do not scale. weight the scaling in between. 

       ; sf_merge[ti[i]] = pwv_interp[tip[i]]/pwv_merge[ti[i]]
      ;

       if perror_curr le pcent_low then sf_merge[ti[i]] = pwv_interp[tip[i]]/pwv_merge[ti[i]]
       if perror_curr ge pcent_high then sf_merge[ti[i]] = -99.0

       if perror_curr lt pcent_high and perror_curr gt pcent_low then begin
           ;weight_pwv = abs((perror_curr - 50.0)/30.0)
           
           sf_temp =  (pwv_interp[tip[i]]/pwv_merge[ti[i]])
           rise = 1.0 - sf_temp
           run  = pcent_high - pcent_low
           slope = rise/run
           b =  (pwv_interp[tip[i]]/pwv_merge[ti[i]])
           y = slope*(perror_curr - pcent_low) + b
           sf_merge[ti[i]] = y

                                ;         sf_merge[ti[i]] =
                                ;         (pwv_interp[tip[i]]/pwv_merge[ti[i]]) * weight_pwv(need to fix)
       endif


       if  sf_merge[ti[i]] ne -999.0 and  sf_merge[ti[i]] ne -99.0 then begin
          mixr_merge[ti[i],*] = reform(mixr_merge[ti[i],*]) * sf_merge[ti[i]]
       endif
       
   endfor


  ;test if the pwv for the scaled profiles are the same as pwv_interp
    pwv_merge_test = fltarr(n_elements(t_interp)) - 999.0
   for w=0,n_elements(t_interp)-1 do begin
       if sf_merge[w] gt 0 then begin
           mixr_curr = reform(mixr_merge[w,*])
           pres_curr = reform(pres_merge[w,*])
           pwv_curr = w2pwv(mixr_curr,pres_curr)
       
           pwv_merge_test[w] = pwv_curr
       endif
   endfor

  totresids = total(pwv_merge_test[ti]-pwv_interp[tip])
  if totresids lt 0.05 then begin
    print,'PWV scaling ok'
    qc_pwv = 0
  endif else begin
    print,'PWV scaling not equal'
    qc_pwv = 1
  endelse

 endif else begin

    qc_pwv = -999
 endelse


endelse


end

