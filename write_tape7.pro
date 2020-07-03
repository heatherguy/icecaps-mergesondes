; $Id: write_tape7.pro,v 1.1 2001/11/02 09:48:09 dturner Release_ddt_1_13 $
;
;  --------------------- Paul vanDelst's RCS information -------------------------
; Author: paulv $
; Id: write_tape7.pro,v 1.1 1997/03/29 16:56:45 paulv Exp $
; Log: write_tape7.pro,v $
; Revision 1.1  1997/03/29 16:56:45  paulv
; Initial revision
;
; Revision: 1.1 $
; Source: /home4/cvsroot/paulv/idl/LBLRTM/write_tape7.pro,v $
;
;+
; NAME:
;       write_tape7
;
; PURPOSE:
;       Procedure to write LBLRTM/FASCODE TAPE7 format files.
;
; CATEGORY:
;       LBL
;
; CALLING SEQUENCE:
;       write_tape7, tape7_file, tape7_data
;
; INPUTS:
;       tape7_file:  TAPE7 data filename.
;       tape7_data:  Structure containing the TAPE7 data.  The output can have
;                    two forms depending on the input format. If there is only 
;                    molecular information in the TAPE7 input, then the returned
;                    structure has the following tag names:
;                      tape7_data = { i_form n_layers n_mols secnt0 h1 h2 ang len
;                                     pressure temperature moldens p_top p_bot t_top
;                                     t_bot z_top z_bot }
;                    If, however, CFC data is present, the returned structure has
;                    the following tag names, each of which is a structure:
;                      tape7_data = { atm_data cfc_data }
;                    The cfc_data structure is identical to that of atm_data except
;                    for the tag cfc_hdr.
;
; SIDE EFFECTS:
;        None known.
;
; RESTRICTIONS:
;        Not fully tested, so unknown.
;
; EXAMPLE:
;        write_tape7, 'TAPE7', tape7_structure
;
; MODIFICATION HISTORY:
; 	Written by:      Paul van Delst, CIMSS/SSEC 27-Mar-1997
;                        Under RCS control.
;-

function p_layer_format, p_layer

  case 1 of

   ( p_layer ge 1000.0 ) : $
       fmt = '3x, f8.3, 4x'

   ( p_layer lt 1000.0 ) and ( p_layer ge 100.0 ) : $
       fmt = '3x, f8.4, 4x'

   ( p_layer lt 100.0 ) and ( p_layer ge 10.0 ) : $
       fmt = '3x, f8.5, 4x'

   ( p_layer lt 10.0 ) and ( p_layer ge 1.0 ) : $
       fmt = '3x, f8.6, 4x'

   ( p_layer lt 1.0 ) and ( p_layer ge 0.1 ) : $
       fmt = '2x, f9.7, 4x'

   ( p_layer lt 0.1 )  : $
       fmt = 'e15.7'

  endcase

  return, fmt

end 



function p_boundary_format, p_boundary

  case 1 of

   ( p_boundary ge 1000.0 ) : $
       fmt = 'f8.2'

   ( p_boundary lt 1000.0 ) and ( p_boundary ge 100.0 ) : $
       fmt = 'f8.3'

   ( p_boundary lt 100.0 ) and ( p_boundary ge 10.0 ) : $
       fmt = 'f8.4' 

   ( p_boundary lt 10.0 ) and ( p_boundary ge 1.0 ) : $
       fmt = 'f8.5'

   ( p_boundary lt 1.0 ) : $
       fmt = 'f8.6'

  endcase

  return, fmt

end



pro write_data, lun_data, ds


;------------------------------------------------------------------------------
;                      -- Write header and first layer --
;------------------------------------------------------------------------------

; ------
; Header
; ------

  hdr_fmt = '( " 1", i3, i5, f10.6, 16x, " H1=", f8.2, " H2=", f8.2, " ANG=", f8.3, " LEN=", i2 )'
  printf, lun_data, format = hdr_fmt, $
                    ds.n_layers, ds.n_mols, ds.secnt0, ds.h1, ds.h2, ds.ang, ds.len


; -----------
; First layer
; -----------


  pl_fmt = p_layer_format( ds.pressure( 0 ) )
  pb_fmt = p_boundary_format( ds.p_bot( 0 ) )
  pt_fmt = p_boundary_format( ds.p_top( 0 ) )

  fmt = '( ' + pl_fmt + ', 3x, f7.2, 14x, i1, 1x, f7.3, ' + $
               pb_fmt + ', f7.2, f7.3, ' + pt_fmt + ', f7.2 )'
  printf, lun_data, format = fmt, $
                    ds.pressure( 0 ), ds.temperature( 0 ), ds.path( 0 ), $
                    ds.z_bot( 0 ), ds.p_bot( 0 ), ds.t_bot( 0 ), $
                    ds.z_top( 0 ), ds.p_top( 0 ), ds.t_top( 0 )

  case 1 of 

    ( ds.n_mols lt 7 ) : $
      printf, lun_data, format = '(8e15.7)', $
                        ds.moldens( 0 : ds.n_mols - 1, 0 ), $
                        fltarr( 7 - ds.n_mols ), $
                        ds.broaddens( 0 )

    ( ds.n_mols eq 7 ) : $
      printf, lun_data, format = '(8e15.7)', $
                        ds.moldens( 0 : ds.n_mols - 1, 0 ), $
                        ds.broaddens( 0 )

    ( ds.n_mols gt 7 ) : $
      printf, lun_data, format = '(8e15.7)', $
                        ds.moldens( 0 : 6, 0 ), ds.broaddens( 0 ), $
                        ds.moldens( 7 : *, 0 )
  endcase



; ----------------
; Loop over layers
; ----------------

  for i = 1, ds.n_layers - 1 do begin

    pl_fmt = p_layer_format( ds.pressure( i ) )
    pt_fmt = p_boundary_format( ds.p_top( i ) )

    fmt = '( ' + pl_fmt + ', 3x, f7.2, 14x, i1, 23x, f7.3, ' + pt_fmt + ', f7.2 )'
    printf, lun_data, format = fmt, $
                      ds.pressure( i ), ds.temperature( i ), ds.path( i ), $
                      ds.z_top( i ), ds.p_top( i ), ds.t_top( i )

    case 1 of 

      ( ds.n_mols lt 7 ) : $
        printf, lun_data, format = '(8e15.7)', $
                          ds.moldens( 0 : ds.n_mols - 1, i ), $
                          fltarr( 7 - ds.n_mols ), $
                          ds.broaddens( i )

      ( ds.n_mols eq 7 ) : $
        printf, lun_data, format = '(8e15.7)', $
                          ds.moldens( 0 : ds.n_mols - 1, i ), $
                          ds.broaddens( i )
  
      ( ds.n_mols gt 7 ) : $
        printf, lun_data, format = '(8e15.7)', $
                          ds.moldens( 0 : 6, i ), ds.broaddens( i ), $
                          ds.moldens( 7 : *, i )
    endcase

  endfor

  return

end






pro write_tape7, tape7_file, tape7_data



;------------------------------------------------------------------------------
;                             -- Check input --
;------------------------------------------------------------------------------

  if n_params() ne 2 then $
    message, 'Invalid number of arguments'



;------------------------------------------------------------------------------
;                -- Determine if CFC data is to be written --
;------------------------------------------------------------------------------

  if ( n_elements( tag_names( tape7_data ) ) eq 2 ) then begin
    cfc_write = 1 
    atm_data = tape7_data.atm_data
    cfc_data = tape7_data.cfc_data
  endif else begin
    cfc_write = 0
    atm_data = tape7_data
  endelse
    



;------------------------------------------------------------------------------
;                              -- Write data --
;------------------------------------------------------------------------------

; ---------
; Open file
; ---------

  openw, lun_tape7, tape7_file, /get_lun


; ----------------------------
; Write molecular data to file
; ----------------------------

  write_data, lun_tape7, atm_data


; --------------------------
; Write CFC data if required
; --------------------------

  if ( cfc_write eq 1 ) then begin

    printf, lun_tape7, cfc_data.cfc_hdr(0)
    printf, lun_tape7, cfc_data.cfc_hdr(1)
    write_data, lun_tape7, cfc_data

  endif



;------------------------------------------------------------------------------
;                      -- Close TAPE7 output file --
;------------------------------------------------------------------------------

  free_lun, lun_tape7



;------------------------------------------------------------------------------
;                               --  Done --
;------------------------------------------------------------------------------

  return

end




















