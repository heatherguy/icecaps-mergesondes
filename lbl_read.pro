; $Id: lbl_read.pro,v 1.1 2001/10/30 15:21:14 dturner Release_ddt_1_13 $
;  
; See the rcsId below for the version of the code according to Paul vanDelst
;
;+
; 
;
; (C) Cooperative Institute for Meteorological Satellite Studies, 1999
;
; NAME:
;       lbl_read
;
; PURPOSE:
;       This function reads a layer of data from a LBLRTM/FASCODE format file.
;
; CATEGORY:
;       LBLRTM
;
; LANGUAGE:
;       IDL v5
;
; CALLING SEQUENCE:
;       result = lbl_read( lbl_file, $                           ; Input
;                          lbl_file_lun, $                       ; Input
;                          lbl_spc, $                            ; Output
;                          v_spc, $                              ; Output
;                          lbl_file_type   = lbl_file_type, $    ; Input keyword
;                          dpf             = dpf, $              ; Input keyword
;                          no_lbl_data     = no_lbl_data, $      ; Input keyword
;                          lbl_file_header = lbl_file_header )   ; Output keyword
;
; INPUTS:
;       lbl_file:         File name of the LBLRTM/FASCODE format file to read.
;       lbl_file_lun:     Logical Unit Number of the LBLRTM/FASCODE format file to
;                         read. This is an argument to allow :
;                           1) More than 1 LBLRTM/FASCODE file to read at the
;                              same time, i.e. it's returned to be used in
;                              subsequent calls,
;                           2) Allow opening of the file within this routine.  This
;                              is done as the file MUST be opened with the
;                              f77_unformatted flag.
;
; INPUT KEYWORD PARAMETERS:
;       lbl_file_type:    Set this keyword to either 'SINGLE' or 'DOUBLE' to 
;                         explicitly specify the LBL file type. This keyword 
;                         overrides any automatic file type detection.
;       dpf:              Diagnostic Print Flag.  If set, diagnostic output about
;                         the file header and panel header frequency bounds and
;                         number of points read are output.
;       no_lbl_data:      Set this keyword to read only the current layer file
;                         header and NOT the layer data. This keyword should be
;                         used in tandem with the LBL_FILE_HEADER output keyword.
;                         Note that after reading the file header, the file pointer
;                         is positioned back to the same location it was in BEFORE
;                         the file header read so subsequent "regular" calls (i.e.
;                         those that read the LBL data panels) are not affected.
;
; OUTPUTS:
;       lbl_spc:  Array containing the required layer spectral data. 
;                   If SINGLE panel, dimension( lbl_spc ) = [ N ],
;                     where N is the no. of points.
;                   If DOUBLE panel, dimension( lbl_spc ) = [ N, 2 ],
;                     where lbl_spc[ *, 0 ] = first item spectral data (radiance)
;                           lbl_spc[ *, 1 ] = second item spectral data (transmittance)
; 
;       v_spc:    Array of wavenumbers corresponding to the data in lbl_spc.
;
;       The function returns a flag indicating whether or not the data read was successful.
;        -1 = Error occurred in function.
;         0 = Read "failed"
;         1 = Read successful to EOF (end-of-file)
;         2 = Read successful to EOL (end-of-layer)
;       The quotes above are included because even though a fail condition is
;       returned, all of the data *may* have been read in.  An example of this
;       is if the user calls this function in a loop and the number of loops
;       exceeds the number of layers in the file.  In that case, all of the data
;       in the file will have been read but the function will be looking for the
;       "extra" layers that don't exist - hence the return error.
;
; OUTPUT KEYWORD PARAMETERS:
;       lbl_file_header:  Set this keyword to a named variable to return the file
;                         header structure for the current layer being read. The 
;                         structure is 1056 bytes long and contains the following
;                         fields :
;
;         user_id      : BYTE(80) 80 characters of user ID information.
;         secant       : DOUBLE Column amount scale factor used in the 
;                          LBL calculation for the current layer.
;                          If +ve, looking up; if -ve, looking down.
;         p_ave        : FLOAT Average layer pressure.
;         t_ave        : FLOAT Average layer temperature.
;         molecule_id  : BYTE(8,64) Character data identifying moelcule.
;         mol_col_dens : FLOAT(64) Molecular column densities.
;         broad_dens   : FLOAT Broadening gases column densities.
;         dv           : FLOAT Frequency interval.
;         v1           : DOUBLE Calculation start frequency.
;         v2           : DOUBLE Calculation end frequency.
;         t_bound      : FLOAT Boundary temperature at H2.
;         emis_bound   : FLOAT Boundary emissivity at H2.
;         LBL_id       : Structure of input flags (HIRAC, LBLF4, etc.).
;         n_mol        : LONG Number of molecules used in LBL caclulation.
;         layer        : LONG Number of current layer.
;         yi1          : FLOAT unknown
;         yid          : BYTE(8,10) Character data containing time, date and 
;                          other bits and pieces.
; CONTAINS:
;       lbl_open:  Function to check if the LBL file data needs to be byte-swapped
;                    and opens the file with the SWAP_ENDIAN keyword appropriately set.
;
; CALLS:
;       None.
;
; COMMON BLOCKS:
;       None
;
; SIDE EFFECTS:
;       Files opened by this routine are *NOT* closed *IF* the input LBLRTM/FASCODE
;       files consist of MORE THAN ONE layer of data. Multiple reads are required
;       for multi-level files. 
;
;       Files are closed if an EOF is detected.
;
;       Radiances are automatically scaled by 1.0e+07 to convert them to
;       mW/m2.sr.cm-1 units IF THE LBL FILE IS SET TO, OR DETECTED AS,
;       "DOUBLE".
;
; RESTRICTIONS:
;       Currently only works for LBLRTM compiled in SINGLE PRECISION mode.
;
; PROCEDURE:
;       Before any data is read in, the file is checked to determine if byte swapping
;       needs to be performed, e.g. if the file was created on a big-endian platform
;       and is being read on a little-endian platform or vice versa. This is done by
;       checking the value of the file header LBL_id.hirac flag. If the flag is not
;       within an accepted value range (0-9), the LBL file is opened with the
;       SWAP_ENDIAN keyword.
;
;       The file header is read in and then panels are read in until an end-of-layer
;       (-99) or end-of-file (0) marker is encountered.  If the former, then the next
;       layer is read until the required layer data has been read in.
;
;       Single- or double-panel files are detected by checking the LBL_id(4) value.
;       If LBL_id(4) = 0, Single panel file,
;                    = 1, Double panel file.
;
;       Explicit end-of-file checking is carried out rather than using the 
;       "while ( not eof(lun) ) do" construct.  This is because the end-of-file can
;       occur at a file header read or a panel header read depending on the way
;       LBLRTM/FASCODE was run (in my experience anyway). The data read is done in
;       an open loop so no jumping out of explicit loops is required. This does
;       require the use of GOTO statements but, hey, I didn't create the file format.
;
; EXAMPLE:
;       Read the current layer of data from an LBLRTM/FASCODE file without
;       specifying the file type and store the data in an array DATA and 
;       the frequency info in an array V :
; 
;         result = lbl_read( 'TAPE13', lun, data, v )
;
;       Read layers of data within a loop.  Set the lbl_file_type keyword
;       so that the first call will bring up the file_type widget but subsequent
;       calls will not.  Assuming 20 layers of data in a file :
;
;         n_layers = 20
;         FOR i = 0, n_layers - 1 DO BEGIN
;           IF ( lbl_read( 'TAPE13', lun, data, v ) ) NE 0 ) THEN BEGIN
;                    .
;                    .
;                process data
;                    .
;                    .
;           ENDIF ELSE BEGIN
;                    .
;                    .
;               handle error condition
;                    .
;                    .
;           ENDELSE
;         ENDFOR
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC 07-Jul-1996
;                       paul.vandelst@ssec.wisc.edu
;
;                       Paul van Delst, 25-Mar-1997
;                       - Put under RCS control.
;-


;###############################################################################
;                  ## CONTAINED PROCEDURES/FUNCTIONS ##
;###############################################################################


;-----------------------------------------------------------------------------
; NAME:
;       lbl_open
;
; PURPOSE:
;       Function to check if the LBL file data needs to be byte-swapped and
;         opens the file with the SWAP_ENDIAN keyword appropriately set.
;
; CALLING SEQUENCE:
;       result = lbl_open( lbl_file, $       ; Input
;                          lbl_file_lun )    ; Input/Output
;
; INPUTS:
;       lbl_file:      File name of the LBL format file to open.
;       lbl_file_lun:  Logical Unit Number of the LBL format file to open.
;
; OUTPUTS:
;       lbl_file_lun:  If not defined on input, it is set to a valid value
;                      on output.
;
;       The function returns a flag indicating the status of the open file
;       procedure:
;         result = -1 if an error occurred
;                =  1 if the file was opened successfully
;
; CALLS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; MODIFICATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC, 28-Feb-1999
;                       paul.vandelst@ssec.wisc.edu
;
;-----------------------------------------------------------------------------


FUNCTION lbl_open, lbl_file, $     ; Input
                   lbl_file_lun    ; Input/Output


; ------------------------------------------
; Determine if data needs to be byte swapped
; ------------------------------------------

; -- Open file as "direct access"

  OPENR, lbl_file_lun, lbl_file, $
         ERROR = open_error, $
         /GET_LUN

  IF ( open_error NE 0 ) THEN BEGIN
    MESSAGE, 'Error opening LBL file for byte-swapping test.', /INFO
    RETURN, -1
  ENDIF


; -- Set the file pointer position to the LBL_id.hirac file header value

  byte_offset = 4 + $     ; Fortran sequential access begin-of-record longword marker
                896       ; No. of bytes from file_header.user_id to file_header.LBL_id

  POINT_LUN, lbl_file_lun, byte_offset


; -- Read HIRAC variable

  hirac = 0L

  READU, lbl_file_lun, hirac


; -- Check HIRAC. Valid values are 0-4, and 9

  IF ( hirac LT 0 OR hirac GT 9 ) THEN BEGIN
    swap = 1     ; Data must be byte swapped
    MESSAGE, 'Byte-swapping set on file open', /INFO
  ENDIF ELSE BEGIN
    swap = 0     ; Data does not need byte-swapping
  ENDELSE


; -- Close file

  FREE_LUN, lbl_file_lun


; -------------------
; Open file correctly
; -------------------

  OPENR, lbl_file_lun, lbl_file, $
         ERROR = open_error, $
         /F77_UNFORMATTED, $
         /GET_LUN, $
         SWAP_ENDIAN = swap

  IF ( open_error NE 0 ) THEN BEGIN
    MESSAGE, 'Error opening LBL file', /INFO
    RETURN, -1
  ENDIF


  RETURN, 1

END





;###############################################################################
;                    ## MAIN PROCEDURE/FUNCTION ##
;###############################################################################

FUNCTION lbl_read, lbl_file, $                           ; Input
                   lbl_file_lun, $                       ; Input
                   lbl_spc, $                            ; Output
                   v_spc, $                              ; Output
                   lbl_file_type   = lbl_file_type, $    ; Input keyword
                   dpf             = dpf, $              ; Input keyword
                   no_lbl_data     = no_lbl_data, $      ; Input keyword
                   lbl_file_header = lbl_file_header     ; Output keyword



;------------------------------------------------------------------------------
;                             -- RCS Id info --
;------------------------------------------------------------------------------

  rcsId = 'Id: lbl_read.pro,v 1.7 1999/06/11 15:50:56 paulv Exp'



;------------------------------------------------------------------------------
;                          -- Set error protocol --
;------------------------------------------------------------------------------

  ON_ERROR, 2



;------------------------------------------------------------------------------
;           -- Define LBLRTM/FASCODE file and panel structures --
;------------------------------------------------------------------------------

; ---------------------
; LBL ID flag structure
; ---------------------

  LBL_id = { hirac : 0L, $
             lblf4 : 0L, $
             xscnt : 0L, $
             aersl : 0L, $
             emit  : 0L, $
             scan  : 0L, $
             plot  : 0L, $
             path  : 0L, $
             jrad  : 0L, $
             test  : 0L, $
             merge : 0L, $
             scnid : 0.0, $
             hwhm  : 0.0, $
             idabs : 0L, $
             atm   : 0L, $
             layr1 : 0L, $
             nlayr : 0L }


; ---------------------
; File header structure
; ---------------------

  file_header = { user_id      : BYTARR( 80 ), $
                  secant       : 0.0d, $
                  p_ave        : 0.0, $
                  t_ave        : 0.0, $
                  molecule_id  : BYTARR( 8, 64 ), $
                  mol_col_dens : FLTARR( 64 ), $
                  broad_dens   : 0.0, $
                  dv           : 0.0, $
                  v1           : 0.0d, $
                  v2           : 0.0d, $
                  t_bound      : 0.0, $
                  emis_bound   : 0.0, $
                  LBL_id       : LBL_id, $
                  n_mol        : 0L, $
                  layer        : 0L, $
                  yi1          : 0.0, $
                  yid          : BYTARR( 8, 10 ) }


; ----------------------
; Panel header structure
; ----------------------

  panel_header = { v1    : 0.0d, $
                   v2    : 0.0d, $
                   dv    : 0.0, $
                   n_pts : 0L }



;------------------------------------------------------------------------------
;                    -- Check argument and keyword list --
;------------------------------------------------------------------------------

; -------------------------------------
; Check for correct number of arguments
; -------------------------------------

  n_arguments = 4
  IF ( N_PARAMS() NE n_arguments ) THEN BEGIN
    MESSAGE, 'Invalid number of arguments', /INFO
    RETURN, -1
  ENDIF


; ---------------------------------------
; Check that filename is actually defined
; ---------------------------------------

; -- Was filename defined? If not, ask user.
  IF ( N_ELEMENTS( lbl_file ) EQ 0 ) THEN BEGIN
    lbl_file = DIALOG_PICKFILE( /MUST_EXIST )
    IF ( STRLEN( lbl_file ) EQ 0 ) THEN BEGIN
      MESSAGE, 'No LBLRTM file specified', /INFO
      RETURN, -1
    ENDIF
  ENDIF

; -- Filename defined, but does it contain anything?
  IF ( STRLEN( lbl_file ) EQ 0 ) THEN BEGIN
    lbl_file = DIALOG_PICKFILE( /MUST_EXIST )
    IF ( STRLEN( lbl_file ) EQ 0 ) THEN BEGIN
      MESSAGE, 'No LBLRTM file specified', /INFO
      RETURN, -1
    ENDIF
  ENDIF


; -----------------------------------------------------
; Check that lbl_file_type has been specified correctly
; -----------------------------------------------------

  IF ( KEYWORD_SET( lbl_file_type ) ) THEN BEGIN

    file_type = STRUPCASE( lbl_file_type )

    IF ( ( file_type NE 'SINGLE' ) AND ( file_type NE 'DOUBLE' ) ) THEN BEGIN
      MESSAGE, 'LBL_FILE_TYPE keyword must be SINGLE or DOUBLE. Using auto-detect.', /INFO
      lbl_file_type = ''
    ENDIF

  ENDIF



;------------------------------------------------------------------------------
;          -- Check if file is already open.  If not, open it. --
;------------------------------------------------------------------------------

; -------------------------------------
; Check if unit number variable defined
; -------------------------------------

  IF ( N_ELEMENTS( lbl_file_lun ) EQ 0 ) THEN BEGIN

    open_status = lbl_open( lbl_file, lbl_file_lun )

    IF ( open_status NE 1 ) THEN BEGIN
      MESSAGE, 'LBL file open unsuccessful', /INFO
      RETURN, -1
    ENDIF

  ENDIF


; -------------------------------------------
; Check if file is open (in case lbl_file_lun
; is defined but file has been closed)
; -------------------------------------------

  file_info = FSTAT( lbl_file_lun )

  IF ( file_info.open EQ 0 ) THEN BEGIN

    FREE_LUN, lbl_file_lun
    open_status = lbl_open( lbl_file, lbl_file_lun )

    IF ( open_status NE 1 ) THEN BEGIN
      MESSAGE, 'LBL file open unsuccessful', /INFO
      RETURN, -1
    ENDIF

  ENDIF



;------------------------------------------------------------------------------
;             -- Check for end of input file (this may not --
;             -- be the first call to this routine)        --
;------------------------------------------------------------------------------

  IF ( EOF( lbl_file_lun ) ) THEN BEGIN
    MESSAGE, 'EOF on input file', /INFO
    FREE_LUN, lbl_file_lun
    RETURN, 0
  ENDIF



;------------------------------------------------------------------------------
;       -- Read file header and assign it to return keyword variable --
;------------------------------------------------------------------------------

; ----------------------------
; Assign current file position
; ----------------------------

  POINT_LUN, -1 * lbl_file_lun, file_position


; ----------------
; Read file header
; ----------------

  READU, lbl_file_lun, file_header

  lbl_file_header = file_header
  dv = lbl_file_header.dv


; ---------------------------------
; Return if NO_LBL_DATA keyword set
; ---------------------------------

  IF ( KEYWORD_SET( no_lbl_data ) ) THEN BEGIN

;   -- Set file position to what it was before the file header read
    POINT_LUN, lbl_file_lun, file_position

;   -- Return in "Read successful to EOL" mode
    RETURN, 2

  ENDIF



;------------------------------------------------------------------------------
;             -- Check if file is double or single panel file --
;------------------------------------------------------------------------------

  IF ( NOT KEYWORD_SET( lbl_file_type ) ) THEN BEGIN
    IF ( lbl_file_header.LBL_id.emit EQ 0 ) THEN $
      file_type = 'SINGLE' $
    ELSE $
      file_type = 'DOUBLE'
  ENDIF

  PRINT, FORMAT = '( 5x, "Reading ", a, " panel file..." )', STRLOWCASE( file_type )



;------------------------------------------------------------------------------
;      -- Estimate number of spectral points in this layer + some slack --
;------------------------------------------------------------------------------
    
  n_pts = LONG( ( ( file_header.v2 - file_header.v1 ) / file_header.dv ) + 5.5 )


; -----------------
; Diagnostic output
; -----------------

  IF ( KEYWORD_SET( dpf ) ) THEN BEGIN

    PRINT, FORMAT = '( /5x, "Frequency limits from file header   : ", f12.6, ", ", f12.6 )', $
                     file_header.v1, file_header.v2
    PRINT, FORMAT = '( 5x, "Frequency interval from file header : ", f12.6 )', $
                     file_header.dv
    PRINT, FORMAT = '( 5x, "Total number of points to be read   : ", i10 )', $
                     n_pts

  ENDIF



;------------------------------------------------------------------------------
;                        -- Create output array --
;------------------------------------------------------------------------------
    
  CASE file_type OF

    'SINGLE': lbl_spc = FLTARR( n_pts )
    'DOUBLE': lbl_spc = FLTARR( n_pts, 2 )

  ENDCASE



;------------------------------------------------------------------------------
;                       -- Read data panel by panel --
;------------------------------------------------------------------------------

  ON_IOERROR, READ_ERROR

  bksp = MAKE_ARRAY( 5, VALUE = 8B )
  PRINT, FORMAT = '( 5x, "Reading panel # :      ", $ )'


; ------------------------
; Initialise some counters
; ------------------------

  n_panels   = 0L
  n_pts_read = 0L

READ_NEXT_PANEL:

  PRINT, FORMAT = '( a, i5, $ )', STRING( bksp ), n_panels + 1


; ---------------------------
; Check for end of input file
; ---------------------------

  IF ( EOF( lbl_file_lun ) ) THEN BEGIN

;   -- Truncate output array if required
    IF ( n_pts_read LT n_pts ) THEN BEGIN
      CASE file_type OF
        'SINGLE': lbl_spc = lbl_spc[ 0 : n_pts_read - 1 ]
        'DOUBLE': lbl_spc = lbl_spc[ 0 : n_pts_read - 1, * ]
      ENDCASE
    ENDIF

;   -- Determine last point wavenumber
    v2 = v1 + ( DOUBLE( n_pts_read - 1 ) * DOUBLE( dv ) )

;   -- Construct return wavenumber array
    v_spc = DINDGEN( n_pts_read ) / DOUBLE( n_pts_read - 1 )
    v_spc = v_spc * ( v2 - v1 ) + v1

;   -- Close the input file
    FREE_LUN, lbl_file_lun

;   -- Done
    PRINT, FORMAT = '( /5x, "EOF on input file after panel : ", i5 )', n_panels
    PRINT, FORMAT = '(  5x, "Number of points in layer     : ", i8 )', n_pts_read
    RETURN, 1

  ENDIF


; -------------------------------------------------
; Read panel header and save first point wavenumber
; -------------------------------------------------
 
  READU, lbl_file_lun, panel_header

  IF ( n_panels EQ 0 ) THEN v1 = panel_header.v1


; ----------------------
; Check for end-of-level
; ----------------------

  IF ( panel_header.n_pts EQ -99 ) THEN BEGIN

;   -- Truncate output array if required
    IF ( n_pts_read LT n_pts ) THEN BEGIN
      CASE file_type OF
        'SINGLE': lbl_spc = lbl_spc[ 0 : n_pts_read - 1 ]
        'DOUBLE': lbl_spc = lbl_spc[ 0 : n_pts_read - 1, * ]
      ENDCASE
    ENDIF

;   -- Determine last point wavenumber
    v2 = v1 + ( DOUBLE( n_pts_read - 1 ) * DOUBLE( dv ) )

;   -- Construct return wavenumber array
    v_spc = DINDGEN( n_pts_read ) / DOUBLE( n_pts_read - 1 )
    v_spc = v_spc * ( v2 - v1 ) + v1

;   -- Done
    PRINT, FORMAT = '( /5x, "EOL on input file after panel : ", i5 )', n_panels
    PRINT, FORMAT = '(  5x, "Number of points in layer     : ", i8 )', n_pts_read
    RETURN, 2

  ENDIF


; -----------------------------------------------------------------
; Read current panel data. The method I use of concatenating arrays
; as they are read is not too clever for very large files since the
; bigger the array to concatenate the more copying that goes on in
; memory == data read slows down. This is noticeable in the panel
; counter that slows down as the panel # increases.
; -----------------------------------------------------------------

  CASE file_type OF

    'SINGLE': BEGIN

;     -- Create array for LBLRTM data
      od = FLTARR( panel_header.n_pts, /NOZERO )

;     -- Read in a single panel
      READU, lbl_file_lun, od

;     -- Calculate indices for output array storage
      index_begin = n_pts_read
      index_end   = n_pts_read + panel_header.n_pts - 1

;     -- Make sure end index is within bounds. If not, 
;     -- tack on some extra space on the end of the array.
      IF ( index_end GT ( n_pts - 1 ) ) THEN BEGIN
        n_pts_required = index_end - n_pts + 1
        MESSAGE, 'Expanding output array by ' + $
                 STRCOMPRESS( STRING( n_pts_required ), /REMOVE_ALL ), /INFO
        n_pts          = n_pts + n_pts_required
        lbl_spc = [ TEMPORARY( lbl_spc ), FLTARR( n_pts_required, /NOZERO ) ]
      ENDIF

;     -- Slot data into output array
      lbl_spc[ index_begin : index_end ] = TEMPORARY( od )

    END

    'DOUBLE': BEGIN


;     -- Create arrays for LBLRTM data
      rad = FLTARR( panel_header.n_pts, /NOZERO )
      tau = FLTARR( panel_header.n_pts, /NOZERO )

;     -- Read in the two panels
      READU, lbl_file_lun, rad
      READU, lbl_file_lun, tau

;     -- Calculate indices for output array storage
      index_begin = n_pts_read
      index_end   = n_pts_read + panel_header.n_pts - 1

;     -- Make sure end index is within bounds. If not, 
;     -- tack on some extra space on the end of the array.
      IF ( index_end GT ( n_pts - 1 ) ) THEN BEGIN
        n_pts_required = index_end - n_pts + 1
        MESSAGE, 'Expanding output array by ' + $
                 STRCOMPRESS( STRING( n_pts_required ), /REMOVE_ALL ), /INFO
        n_pts          = n_pts + n_pts_required
        lbl_spc = TRANSPOSE( [ [ TEMPORARY( TRANSPOSE( lbl_spc ) ) ], $
                               [ FLTARR( 2, n_pts_required, /NOZERO ) ] ] )
      ENDIF

;     -- Slot data into output array
      lbl_spc[ index_begin : index_end, 0 ] = 1.0e+07 * TEMPORARY( rad )
      lbl_spc[ index_begin : index_end, 1 ] = TEMPORARY( tau )

    END

  ENDCASE
         

; ----------------------------
; Increment point read counter
; ----------------------------

  n_pts_read = n_pts_read + panel_header.n_pts
  

; -----------------------
; Increment panel counter
; -----------------------

  n_panels = n_panels + 1


; -----------------
; Diagnostic output
; -----------------

  IF ( KEYWORD_SET( dpf ) ) THEN BEGIN

    PRINT, FORMAT = '( /10x, "Panel number			   : ", i5 )', $
    		     n_panels
    PRINT, FORMAT = '( 10x, "Frequency limits from panel header   : ", f12.6, ", ", f12.6 )', $
    		     panel_header.v1, panel_header.v2
    PRINT, FORMAT = '( 10x, "Frequency interval from panel header : ", f12.6 )', $
    		     panel_header.dv
    PRINT, FORMAT = '( 10x, "Number of points in panel  	  : ", i6 )', $
    		     panel_header.n_pts
    PRINT, FORMAT = '( 10x, "Number of points read so far	  : ", i6 )', $
                     n_pts_read

  ENDIF


; ---------------
; Read next panel
; ---------------

  GOTO, READ_NEXT_PANEL



;------------------------------------------------------------------------------
;                    -- I/O error occurred reading data --
;------------------------------------------------------------------------------

  READ_ERROR:
  MESSAGE, !ERR_STRING, /INFO
  MESSAGE, 'Error reading LBL file ' + lbl_file, /INFO
  RETURN, -1


END

;==============================================================================
; CVS/RCS keyword modification history:  (for Paul vanDelst)
;
; Log: lbl_read.pro,v 
; Revision 1.7  1999/06/11 15:50:56  paulv
; Added NO_LBL_DATA keyword to allow the current layer file header to be
;   read in without reading the actual layer data.
;
; Revision 1.6  1999/06/03 17:58:35  paulv
; Changed method used to read in panel data. Previously, each panel was
;   read in and concatenated to the end of the output array. This made
;   reading large LBLRTM files very slow as the output array grew in size.
;   Now the output array is created up front using an extimated number of
;   points (plus some slack) and the panel data is simply slotted into
;   the correct positions within the array. Preliminary tests provided
;   an 8x speed up in reading a 1.5million point TAPE12 file.
;
; Revision 1.5  1999/03/01 16:24:14  paulv
; Added LBL_OPEN function to check if input LBL format file needs to have the
;   SWAP_ENDIAN keyword set upon opening the file.
;
; Revision 1.4  1998/11/16 18:51:47  paulv
; - Updated syntax.
; - Added RCS Id keyword as a variable.
;
; Revision 1.3  1997/04/18 13:47:45  paulv
; Fixed bug with file type auto-detect.
; Replaced lbl_file_type keyword to accept 'SINGLE' or 'DOUBLE' value.
;
; Revision 1.2  1997/03/25 20:29:04  paulv
; Added single-/double-panel auto detect.
; Both rad and tau read in for double panel files.
;
; Revision 1.1  1997/03/25 20:11:26  paulv
; Initial revision
;
;==============================================================================
