; $Id: isstruc.pro,v 1.1 2006/06/23 14:28:48 dturner Exp $
;+
;   Abstract:
;	This routine returns true (1) if the variable is a structure, 
;		false (0) otherwise.  Logic from Liam Gumley's book, pg 69.
;
;   Author:
;	Dave Turner, SSEC / University of Wisconsin - Madison
;
;   Date:
;	June 2006
;
; Call:
  function isstruc, var
;-
    val = size(var,/type)
    if(val eq 8) then return, 1 else return, 0
end
