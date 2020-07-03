; $Id: gauss_quad.pro,v 1.1 2006/05/10 19:46:24 dturner Exp $
;+
; Abstract:
;	This routine provides the node values (x) and weights (w) needed to
;   integrate a function from [-1,1] using Gaussian Quadrature.  The user must
;   specify the number of points to use, and actually perform their own 
;   integration using the following:
;
;	  integral_f_of_x_over_[-1,1] = total( f(x) * w )
;
;       Note: if we wish to evaluate a function over the interval [a,b], then
;   we need to conduct a change of variable: Let t = c + m x where 
;   c = 0.5 * (b + a) and m = 0.5 * (b - a).  We can now write the integral as
; 
;	integral_f_of_t_over_[a,b] = m * integral_f_of_x_over_[-1,1]
;				   = m * total( f(c + m * x) * w)
;
; Author:
;	Dave Turner, SSEC / University of Wisconsin - Madison
;
; Call:
    pro gauss_quad, $
    	n, $		; The number of points to use in the integration (input)
	x, $		; The node values (output)
	w		; The weights (output)
;-

  case n of
  	1: begin
	     x = 0.
	     w = 2.0
	   end
  	2: begin
	     x = [-1*sqrt(1./3),sqrt(1./3)]
	     w = [1.0,1.0]
	   end
  	3: begin
	     x = [-1*sqrt(3./5),0,sqrt(3./5)]
	     w = [5./9,8./9,5./9]
	   end
  	4: begin
	     x = [-0.86113631, -0.33998104, 0.33998104, 0.86113631]
	     w = [ 0.34785485,  0.65214515, 0.65214515, 0.34785485]
	   end
  	5: begin
	     x = [-0.90617985, -0.53846931, 0.0, 0.53846931, 0.90617985]
	     w = [0.23692689, 0.47862867, 0.56888889, 0.47862867, 0.23692689]
	   end
  	6: begin
	     x = [-0.93246951, -0.66120939, -0.23861918, $
	     		0.23861918, 0.66120939, 0.93246951]
	     w = [0.17132449, 0.36076157, 0.46791393, $
	     		0.46791393, 0.36076157, 0.17132449]
	   end
  	7: begin
	     x = [-0.94910791, -0.74153119, -0.40584515, 0.0, $
	     		0.40584515, 0.74153119, 0.94910791]
	     w = [0.12948497, 0.27970539, 0.38183005, 0.41795918, $
	     		0.38183005, 0.27970539, 0.12948497]
	   end
  	8: begin
	     x = [-0.96028986, -0.79666648, -0.52553241, -0.18343464, $
	     		0.18343464, 0.52553241, 0.79666648, 0.96028986]
	     w = [0.10122854, 0.22238103, 0.31370665, 0.36268378, $
	     		0.36268378, 0.31370665, 0.22238103, 0.10122854]
	   end
	else: begin
	    if(n lt 1) then print,'n must be strictly positive' $
	    else print,'n must be lower than 8'
	    x = !values.f_nan
	    w = 0.
	   endelse
  endcase
  return
end
