; $Id: check_qc_bits.pro,v 1.5 2009/01/01 14:31:47 dturner Exp $
;+
; Abstract:
; 	This routine 'decodes' a bit-packed QC flag to see if particular bits are set.  
;    However, we may not want to evaluate all of the bits, but rather just a few 
;    specific bits of info (since its a bit-packed field), and this routine easily
;    allows us to do that.
;
;	To understand the bit-packed field, think in terms of the integer powers
;    that are needed to derive each number in the (decimal) value.  For example,
;    the QC value of 25 is the binary number "11001", which would mean that the
;    QC value 25 has the 0th, 3rd, and 4th bit set.   
;
;	The user must specify which bits to monitor, and if any of the bits are
;    "on" then the returned flag will be 1.  The returned flag will only be zero
;    if all of the bits being monitored are "off". 
;
; Author:
;	Dave Turner
;	SSEC / Univ. of Wisconsin - Madison
;
; Date:
;	May 2007
;
; Keywords:
;	Bits:	the bits that I would like to monitor.  
;			Remember that 0 refers to 2^0 (i.e., odd number)
;
; Call:
  function check_qc_bits, $ ; Returns 1 if any of bits being checked are 1, zero otherwise
    		qcflag,   $ ; The decimal number (long or int) that is bit packed
		bits=bits   ; the bits are being checked.  0 is 2^0, 1 is 2^1, etc.

		; The default bits that I am looking at here correspond
		; to the flags I want to identify in the MWRRET stat2 data as bad
  if(n_elements(bits) eq 0) then bits = [0,1,6,7]
;-

  trigger = 0L
  for j=0,n_elements(bits)-1 do trigger = trigger + 2L^bits(j)

  flag = bytarr(n_elements(qcflag))
  for i=0L,n_elements(qcflag)-1 do begin
    if((qcflag(i) AND trigger) gt 0) then flag(i) = 1
  endfor

  return,flag
end
