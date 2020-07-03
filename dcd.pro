; $Id: dcd.pro,v 1.4 2003/08/29 14:37:16 dturner Release_ddt_1_13 $
;+
; Abstract
;	This routine is like the IDL routine 'cd', except it checks for the
;    existence of the directory before trying to move there.  It returns
;    a keyword flag indicating whether or not is was successful (1) or not (0).
;
; Author: 		Dave Turner, PNNL
; Date created:		Feb 19, 1999
; Date last modified:	$Date: 2003/08/29 14:37:16 $
;
; Arguments:
;	DIR:		The directory to change to, if possible
;
; Keywords:
;	flag:		If the directory exists, then this will be one, else 0
;
; Call:
	pro dcd, dir, flag=flag
;-

;  command = 'echo "if -d ' + dir + ' echo 1" | csh -fst'
;  spawn, command, result
;  if(fix(result(n_elements(result)-1)) eq 1) then begin
  if (file_test (dir, /directory)) then begin
    cd, dir
    flag=1
    return
  endif 

  flag=0
end
