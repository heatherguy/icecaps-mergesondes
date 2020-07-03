; $Id: plot_histogram.pro,v 1.12 2006/05/11 12:33:51 dturner Exp $
;+
; Abstract:
;	This routine plots up the data in a histogram, with the bars being
;    in the vertical direction (i.e., count is the y-axis).  The color bars
;    can alternate between two colors.  The minimum and maximum value for
;    the plot can be given, whereby any values smaller/larger than these are
;    included into the end bars of the distribution.  Note that the p.region
;    needs to be set beforehand, if desired.
;
; Author:
;	Dave Turner
;		Pacific Northwest National Laboratory
;		University of Wisconsin - Madison
;
; Date:
;	September 2002
;
; Call:
  pro plot_histogram, data, step, minv, maxv, $
	percent=percent, $	; Set this to display percentages, not total counts
	pchars=pchars, 	$	; The size of the plotting characters
	ymax=ymax, 	$	; The maximum y-value
	pcolormain=pcolormain, $	; The primary color of the plot boarders/text
	pcolor1=pcolor1, $	; The color of the color bars
	cuttaillo=cuttaillo, cuttailhi=cuttailhi, $ ; Set these to cut the tail(s) off
						    ;      vs put them in the last bin
	hist_index=hist_index, hist_count=hist_count, $	; Set these to output the results
	vertical=vertical, $			; Set this to create a vertical histogram
	noplot=noplot, $			; Set this to only perform the binning
	xtitle=xtitle, ytitle=ytitle, title=title, subtitle=subtitle, _extra=_extra
;-

  ncount = fix((maxv-minv) / float(step) + 1)
  count  = lonarr(ncount) * 0
  index  = findgen(ncount) * step + minv
  
  	; Handle the 'cuttail' keywords
  ndata = data
  if(keyword_set(cuttaillo)) then begin
  		; If set, only keep the data above the minimum value
    foo = where(ndata ge minv, nfoo)
    if(nfoo gt 0) then ndata = ndata(foo)
  endif
  if(keyword_set(cuttailhi)) then begin
  		; If set, only keep the data below the maximum value
    foo = where(ndata le maxv, nfoo)
    if(nfoo gt 0) then ndata = ndata(foo)
  endif

  foo = where(ndata le (index(0)+index(1))/2., nfoo)
  count(0) = nfoo
  for i=1,ncount-2 do begin
    foo = where((index(i-1)+index(i))/2. lt ndata and $
    		 ndata le (index(i)+index(i+1))/2., nfoo)
    count(i) = nfoo
  endfor
  foo = where((index(i-1)+index(i))/2. lt ndata, nfoo)
  count(i) = nfoo

  if(keyword_set(percent)) then count = 100 * (count / float(total(count)))

  if(not keyword_set(noplot)) then begin
    if(n_elements(pcolormain) eq 0) then pcolormain = 0
    if(n_elements(pcolor1) eq 0) then pcolor1 = 0
    if(n_elements(pchars)  eq 0) then pchars = 1.0
    if(n_elements(xtitle) eq 0) then xtitle = ' '
    if(n_elements(ytitle) eq 0) then ytitle = ' '
    if(n_elements(title)  eq 0) then title  = ' '
    if(n_elements(subtitle) eq 0) then subtitle  = ' '

    if(n_elements(ymax) eq 0) then begin
      yr  = [0,max(count)]
      yst = 0
    endif else begin
      yr  = [0,ymax]
      yst = 1
    endelse

    st = step/2.
    if(not keyword_set(vertical)) then begin
      plot, index, count, /nodata, chars=pchars, color=pcolormain, $
  	xtitle=xtitle, ytitle=ytitle, title=title, subtitle=subtitle, $
	xr=[index(0)-step,index(ncount-1)+step], /xst, $
	yr=yr, yst=yst, _extra=_extra
      for i=0, ncount-1 do $
        polyfill, [index(i)-st,index(i)+st,index(i)+st,index(i)-st,index(i)-st], $
         [0, 0, count(i), count(i), 0], color=pcolor1
    endif else begin
      plot, count, index, /nodata, chars=pchars, color=pcolormain, $
  	xtitle=xtitle, ytitle=ytitle, title=title, subtitle=subtitle, $
	yr=[index(0)-step,index(ncount-1)+step], /yst, $
	xr=xr, xst=xst, _extra=_extra
      for i=0, ncount-1 do $
        polyfill, [0, 0, count(i), count(i), 0], color=pcolor1, $
      	 [index(i)-st,index(i)+st,index(i)+st,index(i)-st,index(i)-st]
    endelse
  endif ; not noplot

  	; Set these keywords.
  hist_index = index
  hist_count = count

  return
end

