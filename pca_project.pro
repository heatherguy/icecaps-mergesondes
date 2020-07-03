; $Id: pca_project.pro,v 1.3 2009/06/13 12:24:43 dturner Exp $
;+
; Name:
;	pca_project
;
; Abstract:
;	Routine to project (compress) along eigenvectors determined from the
;    principal component analysis.
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
;	Result = pca_project ( InputData, Pcount, PC_struct )
;
; Inputs:
;	InputData  :   A two-dimensional matrix, with time as the second dimension
;		          e.g., radiance = dblarr(wnum, time)
;		       So, if I wanted to properly enter the radiance data into
;		       this function, I want the array set up so that the command
;		       		IDL> plot, wnum, rad(*,0) 
;		       makes a plot of the spectrum.
;	Pcount     :   How many principal components to use in the projection
;	PC_struct  :   Structure containing PCA reconstruction matrix, returned
;			  from earlier call to pca_generate_pcs()
;
; Keywords:
;	None.
;
; Outputs:
;	Result     :   Columns of principal component coefficients
;- 

function pca_project, x, ntr, PC
  
  nvals = (size(x))(1)
  nX    = (size(x))(2)


	; This is the original method that uses a loop.  Very expensive in IDL
;  coef = dblarr(ntr, nX)
;  for i=0, nX - 1 do begin
;    col = (x(*,i) - PC.M) ## (PC.U(0:ntr-1,*))
;    coef(*,i) = col
;  endfor

	; Penny Rowe's contribution using matrix math.  MUCH faster!
  coef = x-temporary(replicate(1.,1,nX))##PC.M
  coef = temporary(coef) ## pc.u(0:ntr-1,*)

  return, coef
end


