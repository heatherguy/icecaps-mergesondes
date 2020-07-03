; $Id: apodizer.pro,v 1.2 2002/01/14 16:59:17 dturner Release_ddt_1_13 $
;+
; Abstract:
;	Apodize input spectrum, using one of a few apodization functions
; 
; Author:
;	Dave Turner
;		Pacific Northwest National Laboratory
;		University of Wisconsin - Madison
;
; Date:
;	January 2002
;
; Logic from Dave Tobin, SSEC/UW-Madison
;
; Call:
    function apodizer, $		; Returns apodized spectrum
                 spectrum, $		; Input spectrum
                 aflag			; Which apodization function to use:
		 			;    0 -> Norton-Beer (heavy)
					;    1 -> Kaiser-Bessel #6
;-

  n = n_elements(spectrum)
  imd = n / 2

  case aflag of
    0: apod = apodize_norton_beer(n, imd)
    1: apod = apodize_kaiser_bessel(n, imd)
    else: begin
      print,'ERROR in apodizer: Undetermined apodization function - abort'
      stop,'Aborted in apodizer.'
    end
  endcase

  new_spectrum = fft(fft(spectrum, /inverse)*apod)

  return, new_spectrum
end
