; $Id: bessel.pro,v 1.1 2000/02/10 15:18:57 turner Release_ddt_1_13 $
;+ 
; Abstract:
;	This function returns the value of the Bessel function of the first
;    kind of integral order n.  This calculation is carried out for 
;    a specified number of terms.  Equation was taken from Arfken's 
;    "Mathematical Methods for Physicists", 3rd Ed.
;
; Author:
;	Dave Turner, PNNL
;
; Date:
;	9 Feb 2000
;
; Arguments:
;	Order:		An integer specifying the order of the function
;	X:		The independent variable of the function
;
; Keywords:
;	NTERMS:		The number of terms to use in the calculation.
;				The default is 20.
;
; Call:
	function bessel, order, x, nterms=nterms
;-

  	; Ensure that the order is an integer
  order = fix(order+0.5)

  	; J(-n) of x is equivalent to (-1)^n times J(n) of x, where n is the order
  if(order lt 0) then begin
    order = (-1) * order
    coeff = (-1)^order
  endif else coeff = 1

  if(n_elements(nterms) eq 0) then nterms = 20

  sum = 0.0
  for i=0,nterms-1 do $
    sum = sum + (-1.)^i / (factorial(i)*factorial(i+order)) * (x/2.0)^(order+2*i)

  sum = coeff * sum
  return,sum
end
