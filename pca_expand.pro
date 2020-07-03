; $Id: pca_expand.pro,v 1.3 2009/06/13 12:24:43 dturner Exp $
;+
; Name:
;	pca_expand
;
; Abstract:
;	Routine to expand (decompress) the PCA-compressed data to reconstruct
;    the original data.
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
;	Result = pca_expand ( Coef, PC_struct )
;
; Inputs:
;	Coef      :    Columns of principal component coefficients
;	PC_struct :    Structure containing PCA reconstruction matrix, returned
;			  from earlier call to pca_generate_pcs()
;
; Keywords:
;	None.
;
; Outputs:
;	Result     :   Data columns expanded using principal components.
;			  Results should be identical/similar to the data 
;			  originally input into pca_generate_pcs(), depending
;			  on how the PCA data was projected.
;- 

function pca_expand, coef, PC

  ntr = (size(coef))(1)
  nX  = (size(coef))(2)
  nvals = n_elements(PC.M)

	; This is the original method that uses a loop.  Very expensive in IDL
;  x = dblarr(nvals, nX)
;
;  for i=0, nX - 1 do begin
;    col = PC.U(0:ntr-1,*) ## coef(*,i) + PC.M
;    x(*,i) = col
;  endfor

	; Penny Rowe's contribution using matrix math.  MUCH faster!
  x = temporary(transpose(PC.U(0:ntr-1,*))) # coef +  $
  		temporary(PC.M # temporary(replicate(1.,1,nX)))
  
  return, x
end
