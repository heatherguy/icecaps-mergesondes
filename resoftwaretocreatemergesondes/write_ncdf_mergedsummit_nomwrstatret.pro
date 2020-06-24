;create daily netcdf files
;
;
pro write_ncdf_mergedsummit_nomwrstatret,yyyymmdd


create_mergedsummitprofiles_nomwrstatret2,yyyymmdd,height,t_interp,temp_merge,mixr_merge,pres_merge,$
                                model_w_temp,sonde_w_temp,stat_w_temp,model_w_mixr,sonde_w_mixr,$
                                model_w_pres,sonde_w_pres,pwv_sf_merge,qc_temp,qc_mixr,qc_pres,qc_pwv

qctest = qc_temp+qc_mixr+qc_pres;+qc_pwv

;if there are no PWV vales from MWRRET the qc_pwv will be -999.0(noscaling).
;if all the weights add up to 1 then the qc field = 0
if qctest ne 0 $; and qctest ne -999 
   then print,'weights do not add up to 1, no file created.'$; or PWV scaling is wrong, no file created.'$
   else begin

yyyy = fix(yyyymmdd/10000.)
mm   = fix((yyyymmdd - yyyy*10000.)/100.)
dd   =  ((yyyymmdd - yyyy*10000l - mm*100l))
hh   = fltarr(n_elements(yyyy))
nn   = fltarr(n_elements(yyyy))
ss   = fltarr(n_elements(yyyy))
ymdhms2systime,yyyy[0],mm[0],dd[0],hh[0],nn[0],ss[0],base_time

time_offset = t_interp
time = time_offset

;create the ncdf filename
name = string(format='(A,A,I0,I2.2,I2.2,A,3(I2.2),A)', $
	'/psd3data/arctic/summit/mergesonde/nomwrstatret/', $
	'smtmergesondeX1.a1.',yyyy[0],mm[0],dd[0],'.',hh[0],nn[0],ss[0],'.cdf')

comment1 = "Location: Summit Camp, Greenland. Measurements made by the ICECAPS project. Modeled data from ECMWF forecasts."
glatt = ['Contact = Nate Miller (millernb@colorado.edu), Matthew Shupe (matthew.shupe@noaa.gov)']
created = string('date this NetCDF file was created was ',systime())
glatt = [comment1,glatt, created]

alt = 3255./1000.
summit_lat = 72.596
summit_lon = -38.422+360.

 if(n_elements(latitude) eq 0) then latitude = summit_lat
  if(n_elements(longitude) eq 0) then longitude = summit_lon

if(not keyword_set(silent)) then print,'Creating the file '+name
  fid = ncdf_create(name,/clobber)
  did = ncdf_dimdef(fid,'time',1440)
  did2 = ncdf_dimdef(fid,'height',300)

vid = ncdf_vardef(fid,'base_time',/long)
    ncdf_attput,fid,vid,'long_name','Time since 1970-01-01 00:00:00'
    ncdf_attput,fid,vid,'units','seconds'



 vid = ncdf_vardef(fid,'lat',/float)
    ncdf_attput,fid,vid,'long_name','Latitude'
    ncdf_attput,fid,vid,'units','deg N'
  vid = ncdf_vardef(fid,'lon',/float)
    ncdf_attput,fid,vid,'long_name','Longitude'
    ncdf_attput,fid,vid,'units','deg E'
   vid = ncdf_vardef(fid,'alt',/float)
    ncdf_attput,fid,vid,'long_name','Altitude above mean sea level'
    ncdf_attput,fid,vid,'units','km'


 vid = ncdf_vardef(fid,'time_offset',did,/double)
    ncdf_attput,fid,vid,'long_time','Time since base_time'
    ncdf_attput,fid,vid,'units','seconds'

 vid = ncdf_vardef(fid,'height',did2,/float)
    ncdf_attput,fid,vid,'long_name','height'
    ncdf_attput,fid,vid,'units','km MSL'

 vid = ncdf_vardef(fid,'temp_merge',[did,did2],/float)
    ncdf_attput,fid,vid,'long_name','Merged ECMWF, radiosone and MWRSTATRET temperature profiles'
    ncdf_attput,fid,vid,'units','Celsius'

 vid = ncdf_vardef(fid,'mixr_merge',[did,did2],/float)
    ncdf_attput,fid,vid,'long_name','Merged ECMWF and radiosone mixing ratio profiles, scaled by '$
      +'MWRRET PWV values.'
    ncdf_attput,fid,vid,'units','g/kg'

 vid = ncdf_vardef(fid,'pres_merge',[did,did2],/float)
    ncdf_attput,fid,vid,'long_name','Merged ECMWF and radiosone pressure profiles'
    ncdf_attput,fid,vid,'units','mb'

 vid = ncdf_vardef(fid,'model_w_temp',[did,did2],/float)
    ncdf_attput,fid,vid,'long_name','Weight of ECMWF contribution to merged temperature profiles'
    ncdf_attput,fid,vid,'units','0-1'

 vid = ncdf_vardef(fid,'model_w_mixr',[did,did2],/float)
    ncdf_attput,fid,vid,'long_name','Weight of ECMWF contribution to merged mixing ratio profiles'
    ncdf_attput,fid,vid,'units','0-1'

 vid = ncdf_vardef(fid,'model_w_pres',[did,did2],/float)
    ncdf_attput,fid,vid,'long_name','Weight of ECMWF contribution to merged pressure profiles'
    ncdf_attput,fid,vid,'units','0-1'

 vid = ncdf_vardef(fid,'stat_w_temp',[did,did2],/float)
    ncdf_attput,fid,vid,'long_name','Weight of MWRSTATRET contribution to merged temperature profiles'
    ncdf_attput,fid,vid,'units','0-1'

 vid = ncdf_vardef(fid,'sonde_w_temp',[did,did2],/float)
    ncdf_attput,fid,vid,'long_name','Weight of radiosonde contribution to merged temperature profiles'
    ncdf_attput,fid,vid,'units','0-1'

 vid = ncdf_vardef(fid,'sonde_w_mixr',[did,did2],/float)
    ncdf_attput,fid,vid,'long_name','Weight of radiosonde contribution to merged mixing ratio profiles'
    ncdf_attput,fid,vid,'units','0-1'

 vid = ncdf_vardef(fid,'sonde_w_pres',[did,did2],/float)
    ncdf_attput,fid,vid,'long_name','Weight of radiosonde contribution to merged pressure profiles'
    ncdf_attput,fid,vid,'units','0-1'

 vid = ncdf_vardef(fid,'pwv_sf_merge',[did],/float)
    ncdf_attput,fid,vid,'long_name','Scale factor = (MWRRET PWV)/(PWV from merged mixing ratio profiles), -99 = not scaled by MWRRET because PWV percent error too large.'
    ncdf_attput,fid,vid,'units','unitless'

if(not keyword_set(glatt)) then begin
   ncdf_attput,fid,/global,'comment','ad-hoc created file'
endif else begin
   for i=0,(n_elements(glatt)-1) do $
    ncdf_attput,fid,/global,string(format='(A,I2.2)','comment_',i),glatt(i)
endelse

 
  ncdf_attput,fid,/global,'missing_data_flag',string(-999)
  ncdf_control,fid,/endef

 if(keyword_set(base_time)) then ncdf_varput,fid,'base_time',long(base_time) $
  else ncdf_varput,fid,'base_time',0
  ncdf_varput,fid,'time_offset',time_offset

  ncdf_varput,fid,'alt',alt
  ncdf_varput,fid,'lat',summit_lat
  ncdf_varput,fid,'lon',summit_lon
  ncdf_varput,fid,'height',height
  ncdf_varput,fid,'temp_merge',temp_merge
  ncdf_varput,fid,'mixr_merge',mixr_merge
  ncdf_varput,fid,'pres_merge',pres_merge
  ncdf_varput,fid,'model_w_temp',model_w_temp
  ncdf_varput,fid,'stat_w_temp',stat_w_temp
  ncdf_varput,fid,'sonde_w_temp',sonde_w_temp
  ncdf_varput,fid,'model_w_mixr',model_w_mixr
  ncdf_varput,fid,'sonde_w_mixr',sonde_w_mixr
  ncdf_varput,fid,'model_w_pres',model_w_pres
  ncdf_varput,fid,'sonde_w_pres',sonde_w_pres
  ncdf_varput,fid,'pwv_sf_merge',pwv_sf_merge

  ncdf_close,fid

endelse

end
