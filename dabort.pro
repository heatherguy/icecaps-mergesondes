; $Id: dabort.pro,v 1.1 2001/11/02 08:48:26 dturner Release_ddt_1_13 $
;+
; Abstract:
;	This routine is used to abort a procedure/function in one of three
;   ways, depending on the flag set.  It can abort by stopping in the current
;   function, abort all levels (i.e., return to the main level), or it can 
;   exit IDL.  Before aborting, the message string is output
;
; Author:
;	Dave Turner
;		PNNL and UW-Madison
;
; Date:
;	November 2001
;
; Arguments:
;	FLAG:	Level of abort: 0 - stop in current procedure
;			        1 - return all levels to main level
;			        2 - exit IDL
;	STRING:	Error message output before the action indicated above is taken
;
  pro dabort, flag, string
;-

    print, string
    case flag of
    	0: stop, '    STOPPED in routine'
	1: retall
	else: exit 
    endcase
end
