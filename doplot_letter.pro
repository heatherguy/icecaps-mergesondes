; $Id: doplot_letter.pro,v 1.2 2006/03/19 20:30:34 dturner Exp $
;+
; Abstract:
;	This routine is used to overplot data, where the data points are letters
;    versus symbols.  Liam Gumley provided advice to get the letters centered
;    properly in the vertical and horizontal directions over the actual values.
;
; Author:
;	Dave Turner
;	Space Science and Engineering Center, University of Wisconsin - Madison
;
; Date:
;	Feb 2006
;
; Call:
pro doplot_letter, x, y, letter, color=color, charsize=chars

  if(n_elements(x) ne n_elements(y)) then begin
    print,'Error in doplot_letter: dimensions of x and y must be the same'
    return
  endif
  if(n_elements(letter) ne 1) then begin
    print,'Error in doplot_letter: the letter must be a single string (of a single character)'
    return
  endif

  	; This computes one-half of the vertical height of the 
	; letter, in data coordinates (from the normal and device
	; coordinates and sizes).  This is required to get the letter
	; symbol placed correctly on the vertical axis.
  del = !d.y_size * (!y.window(1)-!y.window(0)) / (!y.crange(1)-!y.crange(0))
  del = 1./del * !d.y_ch_size / 2.
  
  xyouts,x,y-del,replicate(letter,n_elements(x)),align=0.5,color=color,charsize=chars
  return
end
;-
