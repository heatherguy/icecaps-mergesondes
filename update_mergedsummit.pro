


pro update_mergedsummit




 ; daysofayear_yyyymmdd,2010,doy0
  daysofayear_yyyymmdd,2011,doy1
  daysofayear_yyyymmdd,2012,doy2
  daysofayear_yyyymmdd,2013,doy3
  daysofayear_yyyymmdd,2014,doy4
  daysofayear_yyyymmdd,2015,doy5
  daysofayear_yyyymmdd,2016,doy6
  daysofayear_yyyymmdd,2017,doy7

  doyall = [doy1,doy2,doy3,doy4,doy5,doy6]
  
;  doy= doyall[1734:2191]

;  doy = doy4[294:364]
  doy = [doy7[267]]
;  doy = doy5[273:364]


  
 for i = 0,n_elements(doy)-1 do begin
 print,i
  
  write_ncdf_mergedsummit,doy[i]

 endfor
  
  


  

end

