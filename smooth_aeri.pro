; $Id: smooth_aeri.pro,v 1.2 2008/09/16 19:38:15 dturner Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Abstract:
;   This function reduces the AERI's spectral resolution (smooths the data)
;      using FFTs and zeropadding.
;
; Author: Dave Turner, SSEC/U of Wisconsin - Madison
;
; result = smooth_aeri( aeri_v, aeri_r, channel, factor )
;    result is a 1-dimensional array of the same size as aeri_v
;	with the smoothed radiance spectrum
;
; Arguments:
;   aeri_v is in cm-1
;   aeri_r is in mW/m2.sr.cm-1
;   channel is 1 or 2
;   factor is the amount of resolution to reduce (2, 4, or 8)
;
; Call:
  	function smooth_aeri, aeri_v, aeri_r, channel, factor
;-
  ffactor = fix(factor+0.1)
  if(ffactor ne 2 and ffactor ne 4 and ffactor ne 8) then begin
    print,'Error: factor must be one of these values {2, 4, 8}'
    return,-999.
  endif

  band    = channel - 1
  v_laser = 15799.0d
  rf      = [ 4.0d, 2.0d ]

  v_nyquist = v_laser / ( 2.0d * rf[ band ] )
  if(channel eq 1) then n_pts     = 4097L $
  else if(channel eq 2) then n_pts = 8193L $
  else stop,'Error: undefined channel'

  v         = v_nyquist * findgen( n_pts ) / float( n_pts - 1 )


; ------------------------------------------------
; Determine temperature to use for Planck function
; ------------------------------------------------

  v_teststart = [ 627.0d0, 2380.0d0 ]
  v_testend   = [ 632.0d0, 2384.0d0 ]

  loc = where( aeri_v ge v_teststart[ band ] and $
               aeri_v le v_testend[ band ] , count )

  ; Paul's original statement
  ; t = total( planck_temperature( aeri_v( loc ), aeri_r( loc ) ) ) / float( count )
  invplanck, aeri_v(loc), aeri_r(loc), t
  t = total(t) / float(count)

  ; Paul's original statement
  ; r = planck_radiance( v, t )
  planck, v, t, r
  r( 0 ) = 0.0d


; -------------------------------------
; Determine the bounds of the AERI data
; -------------------------------------
  
  n_apts = n_elements( aeri_v )
  loc = where( v ge ( aeri_v[ 0 ] - 0.1 ) and $
               v le ( aeri_v[ n_apts - 1 ] + 0.1 ), count )


; --------------------------------------------------
; Replace the AERI data in the Planck radiance curve
; --------------------------------------------------
  
  r( loc ) = aeri_r


; -------------------------------
; Fourier transform the radiances
; -------------------------------
  
  rr = [ r, reverse( r[ 1 : n_pts - 2 ] ) ]
  ifg = double( fft( rr ) )

;---------------------------------------------------------------------
; Reduce the resolution but maintain the spectral spacing by inserting
; zeros into the tail end of the interferogram
;---------------------------------------------------------------------

  nn = (n_pts - 1) / ffactor
  mm = n_pts - 1 - nn
  fill = replicate(0.d, 2*mm)
  ifg = [ifg(0:nn-1), fill, ifg(nn+2*mm:2*(n_pts-1)-1)]


; ----------------------------------------------
; Fourier transform the zerofilled interferogram
; ----------------------------------------------

  spc = double( fft( ifg, /inverse ) )
  n_total = n_elements(ifg)
  spc = spc( 0 : n_total/2 )
  v = v_nyquist * findgen( n_total/2 + 1 ) / float( n_total/2 )

  loc = where( v ge (aeri_v( 0 )-0.1) and v le (aeri_v( n_apts - 1 )+0.1) )

  return, spc(loc)

end

