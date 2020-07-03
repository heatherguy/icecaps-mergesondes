; $Id: dpushd.pro,v 1.5 2003/08/29 14:40:03 dturner Release_ddt_1_13 $
;+
; Abstract
;	This routine is like the IDL routine 'pushd', except it checks for the
;    existence of the directory before trying to move there.  It returns
;    a keyword flag indicating whether or not is was successful (1) or not (0).
;
; Author: 		Dave Turner, PNNL
; Date created:		Feb 19, 1999
; Date last modified:	$Date: 2003/08/29 14:40:03 $
;
; Arguments:
;	DIR:		The directory to change to, if possible
;
; Keywords:
;	flag:		If the directory exists, then this will be one, else 0
;
; Call:
	pro dpushd, dir, flag=flag, opsys=opsys
;-

	if (keyword_set(opsys) eq 0) then opsys = 0

	if (opsys ne 1) then begin
;		command = 'echo "if -d ' + dir + ' echo 1" | csh -fst'
;		spawn, command, result
;		if(fix(result(n_elements(result)-1)) eq 1) then begin
    if (file_test (dir, /directory, /write)) then begin
			pushd, dir
			flag=1
			return
		endif
	endif else begin
			pushd, dir
			flag=1
			return
	endelse

  flag=0
end

