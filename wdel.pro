; $Id: wdel.pro,v 1.4 2002/02/10 18:18:30 dturner Release_ddt_1_13 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;Abstract:
; This little routine shuts down all of the graphics windows.  IDL remains up
;
; Call:
	pro wdel, win
;-
  if(n_params() eq 0) then $
    while(!d.window ne -1) do wdelete $
  else wdelete, win(0)

  return
end
