; $Id: stackplot.pro,v 1.1 2005/10/28 14:16:54 dturner Exp $
;+
; Abstract:
;	 This routine plots a series of vector data, with offsets between each plot,
;   on the same plot.
;
; Author:
;	Dave Turner
;	SSEC / University of Wisconsin - Madison
;
; Date:
;	October 2005
;
; Call:
  pro stackplot, $
  	data, $			; 2-d matrix of data (m x n)
	coordinate, $		; The coordinate field of the data (1-d, size m)
	offset=offset, $	; The offset to use between plots (default is 1)
	colors=colors, $	; The colors to use for the lines
	vertical=vertical, $	; If set, the data are "vertical" (e.g., profiles)
	_extra=extra, $		; Extra keywords to send the plot command
	wplot=wplot, $		; If set, then use 'wplot' instead of 'plot'
	dostop=dostop
;-

  if(n_elements(offset) eq 0) then offset = 1
  if(n_elements(colors) eq 0) then begin
    @color_syms.include
    colors = [pmain, d_red, d_green, d_orange, d_blue, d_cyan, d_purple, d_dbrown]
  endif

  nsamp = n_elements(data(0,*))
  minv  = min(data(*,0))
  maxv  = max(data(*,nsamp-1))+offset*nsamp

  if(n_elements(coordinate) gt 0) then begin
    if(n_elements(coordinate) ne n_elements(data(*,0))) then begin
      print,'Error: the dimension of the coordinate field does not match data'
      return
    endif
  endif else coordinate = indgen(n_elements(data(*,0)))

  if(keyword_set(vertical)) then begin
    if(keyword_set(wplot)) then begin
      wplot,data(*,0),coordinate,color=colors(0),xr=[minv,maxv]
      for i=1,nsamp-1 do $
        woplot,data(*,i)+offset*i,coordinate,color=colors(i mod n_elements(colors))
    endif else begin
      plot,data(*,0),coordinate,color=colors(0),xr=[minv,maxv],_extra=extra
      for i=1,nsamp-1 do $
        oplot,data(*,i)+offset*i,coordinate,color=colors(i mod n_elements(colors))
    endelse
  endif else begin
    if(keyword_set(wplot)) then begin
      wplot,coordinate,data(*,0),color=colors(0),yr=[minv,maxv],_extra=extra
      for i=1,nsamp-1 do $
        woplot,coordinate,data(*,i)+offset*i,color=colors(i mod n_elements(colors))
    endif else begin
      plot,coordinate,data(*,0),color=colors(0),yr=[minv,maxv],_extra=extra
      for i=1,nsamp-1 do $
        oplot,coordinate,data(*,i)+offset*i,color=colors(i mod n_elements(colors))
    endelse
  endelse
   
  if(keyword_set(dostop)) then stop,'Stopped inside routine'
return
end
