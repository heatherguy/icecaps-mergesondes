;+
; $Id: idl_tools_version.pro,v 1.1 1998/11/09 19:25:53 turner Release_ddt_1_13 $
;
; Abstract:
;     This function returns the RCS State string, which is used to indicate
;  the version of the other source code in this "library".
;
; Call:
    function idl_tools_version, null
;-

  rcsstate = '$State: Release_ddt_1_13 $'
  return, rcsstate
end

