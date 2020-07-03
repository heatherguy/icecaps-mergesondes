; $Id: convolve_to_aeri.pro,v 1.4 2008/06/08 13:46:04 dturner Exp $
;+
; Abstract:
;	This routine takes the input spectrum, and convolves it with the AERI filter
;   function to get data at the AERI resolution.  Actually, instead of performing
;   a convolution, it does this by multiplying the interferogram by the boxcar
;   that is associated with the AERI's Max OPD.  To do this, the input spectrum
;   is tapered and zeropadded to create a smooth spectrum, which is then interpolated
;   to a multiple of the AERI's spectral resolution, which is then transformed back
;   into interferogram space for chopping. 
;
; Author:
;	Dave Turner
;		Pacific Northwest National Laboratory
;		University of Wisconsin - Madison
;
; Date:
;	Mar 2002
;
; Call:
    function convolve_to_aeri, $	; Structure with the new spectrum
     		wnum, $			; Input wavenumber array [cm-1]
		radiance, $		; Input radiance spectrum [arbitrary units]
		dostop=dostop
;- 

  	; These will be needed later
  minv = min(wnum)
  maxv = max(wnum)

	; Work only with 1-D arrays
  x = reform(double(wnum))
  y = reform(double(radiance))

	; Quick QC
  if(n_elements(y) ne n_elements(x)) then begin
    print,'ERROR: wnum and radiance should have the same number of elements!'
    return,0
  endif

	; The AERI's delta nu
  AERI_dv = 15799.D / 2L^15
	; And the AERI's maximum optical path delay
  AERI_OPD = 1.D / (2*AERI_dv)

	; Apply tapers to the end of the radiance spectrum
	; This will also result in the spectrum having 2^n + 1 points

  	; Find the mean wavenumber delta.  I want to use the mean, rather than
	; the difference of the first two elements, in case I accidentally pass
	; the wnum/radiance data in as floats...
  delx = mean(x(1:n_elements(x)-1) - x(0:n_elements(x)-2))

	; First step: taper the head (long wavelength side) of the spectrum
  npts = (x(0) - 0) / delx - 1
  xx = dindgen(npts+1) * delx
  yy = replicate(0.,npts+1)
  x = [xx, x]
  y = [yy, y]

        ; Second step: insert taper at tail to get the proper number of
        ;    points in the spectrum (2^n + 1 points)
  for i=0L,100 do if(2L^i ge n_elements(x)) then break
  npts = 2L^(i+1) - n_elements(x) + 1
  xx = dindgen(npts) * delx + delx + x(n_elements(x)-1)
  yy = replicate(0., n_elements(xx))
  x = [x,xx]
  y = [y,yy]

	; Determine the size of the rolloff to apply; it should be 
	; no more than 100 cm-1, but may need to be smaller...
  rolloffsize = min([max(x)-maxv, minv, 100.])

  	; Find the spectral regions that require a roll-off
  v_rolloff1 = where(minv - rolloffsize le x and x le minv, nfoo1)
  v_rolloff2 = where(maxv le x and x le maxv + rolloffsize, nfoo2)

	; Apply the roll-off
  bar = (cos(findgen(nfoo1)/(nfoo1-1.) * !pi - !pi) + 1) / 2.
  y(v_rolloff1) = bar * y(max(v_rolloff1))
  bar = (cos(findgen(nfoo2)/(nfoo2-1.) * !pi) + 1) / 2.
  y(v_rolloff2) = bar * y(min(v_rolloff2))

	; If the wavenumber resolution is "coarse", then we need to zeropad
	; the spectrum to allow us to interpolate to a multiple of the AERI
	; resolution...
  		; This threshold is a bit arbitrary...
  if(delx gt 0.01) then begin	; {
	
	    ; Now fold the spectrum to get a new spectrum with 2^(n+1) points
    n = n_elements(y)
    yfold = [y, reverse(y(1:n-2))]

            ; Compute the interferogram
    n = n_elements(yfold)
    inter = fft(yfold, /inverse, /double)
    yyi = double(shift(inter, -1*n/2))

	    ; Now we want to zeropad the spectrum to have 2^14 points
	    ; so we need to figure out how many zeros to put at the ends of
	    ; the interferogram.  And we need to keep track of the factor that
	    ; we are expanding the interferogram by so we can multiply the 
	    ; spectrum by it in a later step to put the energy back into it.
    for i=0L,100 do if(2L^i ge n_elements(x)) then break
    if(i lt 18) then begin
      npts = 2L^18 - 2L^i
      factor = 2L^18 / 2L^i
      yyi_pad = [replicate(0.D, npts/2), yyi, replicate(0.D, npts/2.)]
    endif else begin
      factor = 1
      yyi_pad = yyi
    endelse

	    ; Now compute the spectrum from this zeropadded spectrum
    n_pad = n_elements(yyi_pad)
    yyi_pad_shift = shift(yyi_pad, n_pad/2)
    new_spec = fft(yyi_pad_shift, /double)
    new_dv   = delx / double(factor)

	    ; Cut the spectrum down (i.e., throw away the folded part) 
	    ; 	and put the energy back in
    new_x = dindgen(n_elements(new_spec)/2) * new_dv
    new_y = factor * double(new_spec(0:n_elements(new_spec)/2-1))

  endif else begin	; }{

  	    ; Just reassign the vectors, as the spectral resolution is high enough
    new_x = x
    new_y = y

  endelse		; }

	; Now we need to interpolate this spectrum to a multiple of the AERI's
	; wavenumber grid.  For chuckles, use 16 times AERI's delta nu
  new_aeri_dv = aeri_dv / 16.
  max_v = max(new_x)
  new_aeri_wnum = dindgen(max_v / new_aeri_dv) * new_aeri_dv
  new_aeri_spec = interpol(new_y, new_x, new_aeri_wnum)

  	; In our desire to have a spectrum with 2^n + 1 points, I may need
	;     to throw away a few points (but these are probably in the taper)
  for i=0L,100 do if(2L^i ge n_elements(new_aeri_wnum)) then break
  npts = 2L^(i-1)
  new_aeri_wnum = new_aeri_wnum(0:npts)
  new_aeri_spec = new_aeri_spec(0:npts)

  	; Now fold this spectrum, and compute its interferogram
  n_aeri = n_elements(new_aeri_spec)
  new_aeri_spec_fold = [new_aeri_spec, reverse(new_aeri_spec(1:n_aeri-2))]
  n_fold = n_elements(new_aeri_spec_fold)
  new_aeri_inter = fft(new_aeri_spec_fold, /inverse, /double)
  new_aeri_inter = double(new_aeri_inter)
  new_aeri_inter = shift(new_aeri_inter, -1*n_fold/2)
  new_aeri_opd = 1.D / (2 * new_aeri_dv)
  new_aeri_xx  = (dindgen(n_fold)/double(n_fold)*2 - 1) * new_aeri_opd
	
	; Now chop this at the desired optical path delay
  foo = where(-1*aeri_opd le new_aeri_xx and new_aeri_xx lt aeri_opd, nfoo)
  aeri_chop_inter = new_aeri_inter(foo)
  
  	; And transform back into the spectral domain
  n_chop = n_elements(aeri_chop_inter)
  aeri_chop_inter_shift = shift(aeri_chop_inter, n_chop/2)
  final_aeri_spec = fft(aeri_chop_inter_shift, /double)
  final_aeri_spec = double(final_aeri_spec)

  	; Compute the scale factor that will account for the energy redistribution
  factor = n_elements(final_aeri_spec) / double(n_elements(new_aeri_inter))
  final_aeri_spec = factor * final_aeri_spec
  
  	; And compute the wavenumber grid for this data
  final_aeri_dv = 1.D / (2 * aeri_opd)
  final_aeri_wnum = dindgen(n_elements(final_aeri_spec)/2) * final_aeri_dv
  final_aeri_spec = final_aeri_spec(0:n_elements(final_aeri_wnum)-1)

  	; And the last step: cut off data before and after the actual
	;    minimum and maximum wavenumber intervals of the input data
  foo = where(minv le final_aeri_wnum and final_aeri_wnum le maxv, nfoo)
  final_aeri_wnum = final_aeri_wnum(foo)
  final_aeri_spec = final_aeri_spec(foo)

  if(keyword_set(dostop)) then stop,'Stopped inside routine'

  return, {wnum:final_aeri_wnum, spec:final_aeri_spec}
end
