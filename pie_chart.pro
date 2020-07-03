; $Id: pie_chart.pro,v 1.3 2004/04/21 14:43:46 dturner Exp $
;+
; Abstract:
;	This routine creates a pie chart of the entered data.
;  
; Author:
;	Dave Turner
;		Pacific Northwest National Laboratory
;		University of Wisconsin - Madison
;
; Date:
;	January, 2002
;
; Call:
    pro pie_chart, $
               n, $		; The data for the Pie chart (n element array)
               label, $		; Labels for each pie sector (n element array)
               title, $		; Title for the plot
               pcolors, $	; Colors for each pie sector (n element array)
               pregion, $	; Plot region for the pie chart (4 element array)
               tregion, $	; Coordinates for the key; x, y, delta-y
               pchars		; Character size for the key
;-

  fraction = n
  nn = float(total(fraction))
  fraction = fraction / nn

  a = findgen(1001)/1000*2*!pi
  a_sin = sin(a)
  a_cos = cos(a)
;  a_sin(1000) = 0
  !p.region=pregion
  plot, a_sin, a_cos, color=pcolors(0), xst=4, yst=4, /nodata
  polyfill, a_sin, a_cos, color=pcolors(0)
  pt = 0
  index = indgen(n_elements(a))
  for i=0,n_elements(fraction)-1 do begin
    foo = where(index lt fix(fraction(i)*1000+0.5), nfoo)
    if(nfoo gt 0) then begin
      pindex = index(foo) + pt
      pindex = [pindex, pindex(n_elements(pindex)-1)+1]
      polyfill, [a_sin(pindex), 0, a_sin(pindex(0))], $
      		[a_cos(pindex), 0, a_cos(pindex(0))], $
      		color=pcolors(i)
      pt = pt + nfoo
    endif
    xyouts, tregion(0), tregion(1)-i*tregion(2), /normal, align=1, label(i), $
    	chars=pchars, color=pcolors(i)
    xyouts, tregion(0)+0.01, tregion(1)-i*tregion(2), /normal, $
    	string(format='(F5.1)', fraction(i)*100) + ' %', chars=pchars
  endfor
  xyouts, 0.5, 0.95, title, /normal, color=pcolors(0), chars=pchars*1.5, align=0.5
end

