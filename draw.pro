; $Id: draw.pro,v 1.1 2001/09/26 15:13:55 dturner Release_ddt_1_13 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+ 
; Abstract:
;	  This routine is a drawing (tracing) program.  The user enters the
;	number of points desired and then uses the mouse on the current
;	window to select the points, which are then stuffed into the x and y
;	arrays, which are returned.
;
; Author: 	Dave Turner, PNNL
; Date:		February, 1998
;
; Arguments:
;	NPTS:	The number of points desired in the arrays
;	X:	The x coordinate data, in data coordinates
;	Y:	The y coordinate data, in data coordinates
;
; Keywords:
; 	DATA:	Keyword passed to "cursor" routine
; 	NORMAL:	Keyword passed to "cursor" routine
; 	DEVICE:	Keyword passed to "cursor" routine
;
; Call:
	pro draw, npts, x, y, _extra=extra
;-

x = fltarr(npts)
y = fltarr(npts)

for i=0, npts-1 do begin
  print,string(format='(I0)',npts-i) + ' points left to enter'
  cursor, q, w, 3, _extra=extra
  x(i) = q
  y(i) = w
endfor

end
