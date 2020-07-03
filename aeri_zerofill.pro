; $Id: aeri_zerofill.pro,v 1.3 2002/04/29 10:11:14 dturner Release_ddt_1_13 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Abstract:
;   function aeri_zerofill accepts an AERI spectrum and performs
;     Fourier interpolation from the nominal 4097 points to
;     2^14 + 1 points.
;
; Author: Paul van Delst, SSEC/U of Wisconsin - Madison
;
; result = aeri_zerofill( aeri_v, aeri_r, channel )
;    result is a 2 dimensional array
;	result(0,*) is the new wavenumbers
;       result(1,*) is the zerofilled radiances
;
; Arguments:
;   aeri_v is in cm-1
;   aeri_r is in mW/m2.sr.cm-1
;   channel is 1 or 2
;
; Call:
  	function aeri_zerofill, aeri_v, aeri_r, channel
;-

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


; --------------------------
; Zerofill the interferogram
; --------------------------

  n_total = 2L^18
  fill = fltarr( n_total - n_elements( ifg ) )
  ifg = [ ifg[ 0: n_pts - 1 ], fill, ifg[ n_pts : ( ( 2 * ( n_pts - 1 ) ) - 1 ) ] ]


; ----------------------------------------------
; Fourier transform the zerofilled interferogram
; ----------------------------------------------

  spc = double( fft( ifg, /inverse ) )
  spc = spc( 0 : n_total/2 )
  v = v_nyquist * findgen( n_total/2 + 1 ) / float( n_total/2 )

  loc = where( v ge aeri_v( 0 ) and v le aeri_v( n_apts - 1 ) )

  return, transpose( [ [ v( loc ) ], [ spc( loc ) ] ] )

end

