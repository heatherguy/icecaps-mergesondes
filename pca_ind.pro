; $Id: pca_ind.pro,v 1.2 2006/07/21 16:05:58 dturner Exp $
;+
; Abstract:
;	This script takes as input an array of eigenvalues and computes from them
;   the IND, IE, RE, PCV, and other parameters.  But its main function is to compute
;   the number of eigenvectors that should be used in the reconstruction of the PCA 
;   data, based upon the Error Indicator Function (IND).
;
; Reference:
;	Turner, Knuteson, Revercomb, Lo, and Dedecker, 2006: Noise reduction of 
;  	Atmospheric Emitted Radiance Interferometer (AERI) observations using 
;	principal component analysis. J. Atmos. Oceanic Technol., accepted.
;
; Author: 
; 	Dave Turner
;	SSEC / University of Wisconsin - Madison
;
; Date:
;	October 2005
;
; Call
   function pca_ind, $		; Returns the IND index (i.e., number of e-vects to use)
   	evals, $		; Input: array of eigenvalues
	nsamp, $		; Input: Number of datasamples used
	xe=xe, $		; Output: extracted error for the IND index 
	re=re, $		; Output: real error for the IND index
	pcv=pcv, $		; Output: percent cummulative variance at IND index
	f_ie=f_ie, $		; Output: function of IE(k)
	f_re=f_re, $		; Output: function of RE(k)
	f_xe=f_xe, $		; Output: function of XE(k)
	f_pcv=f_pcv, $		; Output: function of PCV(k)
	doplot=doplot, $	; Set this to make a plot
	dostop=dostop
;-

  c = double(n_elements(evals))

  f_ie  = dblarr(c-1)
  f_re  = dblarr(c-1)
  f_xe  = dblarr(c-1)
  f_ind = dblarr(c-1)
  f_pcv = dblarr(c-1)
  for n=0,c-2 do begin
    f_ie(n) = sqrt( (n * total(evals(n+1:c-1))) / (nsamp*c*(c-n)) )
    f_re(n) = sqrt( total(evals(n+1:c-1)) / (nsamp*(c-n)) )
    f_xe(n) = sqrt( total(evals(n+1:c-1)) / (nsamp * c) )
    f_pcv(n) = 100 * total(evals(0:n)) / total(evals)
    f_ind(n) = f_re(n) / (c-n)^2
  endfor

  idx = indgen(c)+1
  if(keyword_set(doplot)) then begin
    window,!d.window+1,ys=900
    !p.multi=[0,1,3]
    pchars = 1.8
    bar = where(idx ge 5)
    foo = where(f_ie(bar) eq min(f_ie(bar)))
    print,format='(A,I0)','  The  IE optimal number of PCs to use is ',idx(bar(foo(0)))
    foo = where(f_ind eq min(f_ind))
    print,format='(A,I0)','  The IND optimal number of PCs to use is ',idx(foo(0))
    plot,idx,f_re,chars=pchars,title='Real Error',yst=16,/xlog
    plot,idx,f_ie,chars=pchars,title='Imbedded Error',yst=16,/xlog
    plot,idx,f_ind,chars=pchars,title='Error Indicator Function',yst=16,/ylog,/xlog
    !p.multi=0
  endif

  foo = where(f_ind eq min(f_ind))
  xe  = f_xe(foo(0))
  re  = f_re(foo(0))
  pcv = f_pcv(foo(0))

  if(keyword_set(dostop)) then stop,'stopped inside routine'
return,idx(foo(0))
end
