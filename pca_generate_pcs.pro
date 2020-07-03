; $Id: pca_generate_pcs.pro,v 1.3 2008/04/04 20:50:40 dturner Exp $
;+
; Name:
;	pca_generate_pcs
;
; Abstract:
;	Routine to compute the principal components of a matrix, as well
;    as the reconstruction matrices needed for a principal component analysis.
;
; Original Author:
; 	Paolo Antonelli and Raymond Garcia
;		University of Wisconsin - Madison
;
; Ported into IDL by:
;	Dave Turner
;		University of Wisconsin - Madison 
;		Pacific Northwest National Laboratory
;
; Copyright information:
;	(C) Copyright UW/SSEC  All Rights Reserved, 2000
;		Space Science & Engineering Center
;		University of Wisconsin - Madison
;
; Calling sequence:
;	Result = pca_generate_pcs ( InputData )
;
; Inputs:
;	InputData  :   A two-dimensional matrix, with time as the second dimension
;		          e.g., radiance = dblarr(wnum, time)
;		       So, if I wanted to properly enter the radiance data into
;		       this function, I want the array set up so that the command
;		       		IDL> plot, wnum, rad(*,0) 
;		       makes a plot of the spectrum.
;
; Keywords:
;	None.
;
; Outputs:
;	Result     :   A structure with the principal components (D), the 
;			  decomposition / reconstruction matrix (U), and the
;			  vector of the mean of the first dimension (M)
;- 
function pca_generate_pcs, x

  	; Compute the mean spectrum
  m = fltarr(n_elements(x(*,0)))
  for i=0,n_elements(m)-1 do $
    m(i) = mean(x(i,*))

  c = correlate(x, /covar)
  svdc, c, d, u, v
  c = 0		; Free up the memory

  	; The eigenvalues aren't necessarily in decending order like they 
	; should be so we need to rearrange them and the matrices u and v
  z = reverse(sort(d))			
  u = u(z,*)
  v = v(z,*)
  d = d(z)
  
  PC = {U:u, D:d, M:m}
  return, PC
end

