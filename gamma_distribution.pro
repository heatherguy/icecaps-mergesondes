; $Id: gamma_distribution.pro,v 1.3 2002/11/22 06:14:39 dturner Release_ddt_1_13 $
;+
; Abstract:
;	This routine computes gamma distribution, given an effective 
;   radiance and a variance.  
; 
;   	This code was copied from Bryan Baum's xmie code.
;
; Author:
;	Dave Turner
;		Pacific Northwest National Laboratory
;		University of Wisconsin - Madison
;
; Date:
;	Feb 2002
;
; Call:
  function gamma_distribution,  $    	; Returns a structure with *radii, *number
  	      r_eff, $	    		; Desired effective radius [um]
	      var, $ 			; Desired variance [um]
	      vol_eff=vol_eff		; Effective volume of the distribution [um^3]
;-
  
   const = 1.
   rmin = 0.1
   rmax = 20.1
   nradii = 500

start_over:

   delr = rmax - rmin
   dr = delr / nradii

   power = (1. - 3.*var) / double(var)
   abinv = 1.D / (r_eff * var)

   	; Compute the radii and the number concentration
   en = dblarr(nradii)
   r  = dblarr(nradii)
   for i=0,nradii-1 do begin
     r(i)  = rmin + i*dr
     x     = r(i) * abinv
     ex    = exp(-1 * x)
     en(i) = const * (r(i)^power) * ex
   endfor

   	; Now a quick check: is the effective radius from this distribution
	; close enough to the desire one?  If not, then it is (usually) because
	; there aren't enough large particles.  Increase rmax and start over.
  	; At the same time, let's calculate the integral of the size distribution
	; (using simple trapeziodal integration) so we can normalize it.
  sum = 0.D
  sumnum = 0.D
  sumden = 0.D
  sumvol = 0.D
  for i=1,nradii-1 do begin
    df = (en(i) + en(i-1)) / 2.
    sum = sum + df * dr
    pirsq = !pi * r(i) * r(i)
    sumvol = sumvol + r(i)^3 * pirsq * en(i) * dr
    sumnum = sumnum + r(i)   * pirsq * en(i) * dr
    sumden = sumden +          pirsq * en(i) * dr
  endfor
  dist_r_eff = sumnum / sumden
  vol_eff    = sumvol / sumden * 4/3. * !pi
  if(r_eff - dist_r_eff gt 0.0001) then begin
    rmax = rmax + 20.
    goto, start_over
  endif

  	; Normalize the distribution
  en = en / sum

  	; Return the distribution
  return, {r:r, en:en}
end
