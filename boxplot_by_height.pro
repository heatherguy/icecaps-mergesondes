; $Id: boxplot_by_height.pro,v 1.5 1998/05/27 18:05:41 turner Release_ddt_1_13 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Abstract:
;   This little function plots the error as a function of height using the boxplot.
;   The boxplot captures the middle two quartiles in a box, indicates the spread of
;   the data with error bars which are 3 times the standard deviation about the
;   mean, and then highlights the median value of the dataset.
;
; Author:
;   Dave Turner, PNNL
;
; Date: 
;   March, 1998
;
; Arguments:
;   ERROR:	The array of errors or differences
;   HT:		The height corresponding to the differences (singleton value)
;   OFFSET:	The half-width of the boxplots which capture the middle quartiles
;   MED:	The array of median values, which is returned
;
; Keywords:
;   DESIRE_MEDIAN:	If set, then plot the median value in the box
;   NO_QUARTILES:	If set, then the middle quartiles are not displayed
;   XRNG:		If set to a 1x2 array, the limits of the plot
;   YRNG:		If set to a 1x2 array, the limits of the plot
;   SDEV_SIZE:		The scale factor for the standard deviation bars.  Traditional
;				box plots use 3 times the sdev (the default)
;   DOSTOP:		If set, then stop before exitting the procedure
;
; Call:
    pro boxplot_by_height, error, ht, offset, med, desire_median=desire_median, $
	no_quartiles=no_quartiles, xrng=xrng, yrng=yrng, sdev_size=sdev_size, $
	dostop=dostop
;-

  @color_syms.include

  if(!d.name eq 'PS') then pquart = d_ltgray2 else pquart = pmain

  if(not keyword_set(sdev_size)) then sdev_size = 3.0

  quartile = fltarr(2)
  sdev = fltarr(2)
  mean = (moment(error,sd=sd))(0)
;  print,ht, mean, sd, n_elements(error)
  sdev(0) = mean-sdev_size*sd
  sdev(1) = mean+sdev_size*sd
  med = median(error)
	; Now find the 1st and 3rd quartiles
  foo = sort(error)
  error = error(foo)
  n = n_elements(error)
  quartile(0) = error(n/4)
  quartile(1) = error(3*n/4)
    
  oplot, [sdev(0),sdev(1)],[ht,ht],color=d_red
  oplot, [sdev(0),sdev(0)],[ht-offset,ht+offset], color=d_red
  oplot, [sdev(1),sdev(1)],[ht-offset,ht+offset], color=d_red

  if(not keyword_set(no_quartiles)) then begin
    if(keyword_set(yrng)) then $
      if((ht+offset le yrng(1)) and (ht-offset ge yrng(0))) then begin
        if(keyword_set(xrng)) then $
          polyfill, [quartile(0) > xrng(0),quartile(0) > xrng(0),$
  	    quartile(1) < xrng(1),quartile(1) < xrng(1)], $
	    [ht+offset,ht-offset,ht-offset,ht+offset], color=pquart $
        else $
          polyfill, [quartile(0),quartile(0), quartile(1),quartile(1)], $
	    [ht+offset,ht-offset,ht-offset,ht+offset], color=pquart 
      endif else if((ht le yrng(1)) and (ht ge yrng(0))) then begin
        if(keyword_set(xrng)) then $
          polyfill, [quartile(0) > xrng(0),quartile(0) > xrng(0),$
  	    quartile(1) < xrng(1),quartile(1) < xrng(1)], $
	    [(ht+offset)<yrng(1),(ht-offset)>yrng(0),$
	    (ht-offset)>yrng(0),(ht+offset)<yrng(1)], color=pquart $
        else $
          polyfill, [quartile(0),quartile(0), quartile(1),quartile(1)], $
	    [(ht+offset)<yrng(1),(ht-offset)>yrng(0),$
	    (ht-offset)>yrng(0),(ht+offset)<yrng(1)], color=pquart
      endif
  endif

  if(keyword_set(desire_median)) then $
     oplot, [med,med], [ht-offset,ht+offset], color=d_green, thick=2

  if(keyword_set(dostop)) then stop, 'Stopped in boxplot_by_height as indicated'

end

