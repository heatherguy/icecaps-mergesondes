; $Id: dmedian.pro,v 1.3 2006/03/13 11:52:46 dturner Exp $
;+
; Abstract:
;	This routine computes the median and "standard deviation" (using quartile
;   statistics) from the input distribution.
;
; Author:
;	Dave Turner
;		Pacific Northwest National Laboratory
;
; Date:
;	Mar 2004
;
; Call:
  function dmedian, $		; Returns the median of the distribution
	data, $			; Input data
  	sd=sd, $		; Returns a two element vector with the
				;    "standard deviation" values, or the values 
				;    that are 16% and 84% of the range (since the 
				;    standard deviation covers 68% of the center 
				;    of the distribution)
	quart=quart, $		; Returns a two element vector with the 25% and 75% 
				;    values of the distribution
	ten=ten, $		; Returns a two element vector with the 10% and 90% 
				;    values of the distribution
	five=five, $		; Returns a two element vector with the 5% and 95% 
				;    values of the distribution
	one=one			; Returns a two element vector with the 1% and 99% 
				;    values of the distribution
;-

  ndata = data(sort(data))
  npts  = n_elements(ndata)
  	; The median value
  dmedian = ndata(npts/2)

  	; The "standard deviation" arrays
  ddist = long(npts*0.16)
  sd = [ndata(ddist),ndata(npts-ddist-1)]
  
  	; The quartile points
  ddist = long(npts*0.25)
  quart = [ndata(ddist),ndata(npts-ddist-1)]
  
  	; The 10/90 points
  ddist = long(npts*0.10)
  ten = [ndata(ddist),ndata(npts-ddist-1)]

  	; The 5/95 points
  ddist = long(npts*0.05)
  five = [ndata(ddist),ndata(npts-ddist-1)]

  	; The 1/99 points
  ddist = long(npts*0.01)
  one = [ndata(ddist),ndata(npts-ddist-1)]

  return, dmedian
end
