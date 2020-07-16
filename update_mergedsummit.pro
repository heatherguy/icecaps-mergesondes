


pro update_mergedsummit




 ; daysofayear_yyyymmdd,2010,doy0
  daysofayear_yyyymmdd,2019,doy1
  daysofayear_yyyymmdd,2020,doy2

  doyall = [doy1,doy2]
  
 ; doy= doyall[1734:2191]
 doy = doyall[230:290]


  
 for i = 0,n_elements(doy)-1 do begin
 print,i
  
  write_ncdf_mergedsummit,doy[i]

 endfor
  
  


  

end

