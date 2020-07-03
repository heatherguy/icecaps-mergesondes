;+
; $Id: read_ncdf.pro,v 1.2 2006/09/05 16:34:35 dturner Exp $
;
; $pvd_Id: Unknown; downloaded from Fanning's site on 5 Sep 2006 $
;
; NAME:
;       read_ncdf
;
; PURPOSE:
;       Function to read variable and attribute data from NetCDF 
;       format files.
;
; CATEGORY:
;       NCDF
;
; LANGUAGE:
;       IDL v5
;
; CALLING SEQUENCE:
;       result = read_ncdf( ncdf_file, $                                     ; Input
;                           data, $                                          ; Output
;                           variable_list         = variable_list, $         ; Input keyword
;                           count                 = count, $                 ; Input keyword
;                           offset                = offset, $                ; Input keyword
;                           stride                = stride, $                ; Input keyword
;                           variable_attributes   = variable_attributes, $   ; Input keyword
;                           global_attributes     = global_attributes, $     ; Input keyword
;                           no_var_byte_to_string = no_var_byte_to_string, $ ; Input keyword
;                           no_att_byte_to_string = no_att_byte_to_string, $ ; Input keyword
;                           quiet                 = quiet )                  ; Input keyword
;
; INPUTS:
;       ncdf_file:     The name of the NetCDF file to read
;
; INPUT KEYWORD PARAMETERS:
;       variable_list:          A string array of variable name to read from
;                               the NetCDF file. If not specified, ALL the
;                               variables are read.
;       count:                  Set this keyword to a vector containing the
;                               number of points in each dimension that are
;                               required for a variable read. It is a 1-based
;                               vector and defaults to match the size of all
;                               dimensions so that all data is read.
;       offset:                 Set this keyword to a vector containing the
;                               starting index position for each dimension of
;                               the variable required. It is a 0-based
;                               vector and defaults to zero for every dimension
;                               so that all data is read.
;       stride:                 Set this keyword to a vector containing the
;                               strides, or sampling intervals, between accessed
;                               values of the required variable. It is a 1-based
;                               vector and defaults to one for every dimension
;                               so that all data is read.
;       variable_attributes:    Set this keyword to return variable
;                               attribute data. Using this keyword modified the
;                               the form of the output structure. See the 
;                               OUTPUTS description below.
;       global_attributes:      Set this keyword to return global
;                               attribute data.
;       no_var_byte_to_string:  Set this keyword to prevent the
;                               conversion of BYTE variable data
;                               to STRING type. (IDL 5.2 and earlier only)
;       no_att_byte_to_string:  Set this keyword to prevent the
;                               conversion of BYTE attribute data
;                               to STRING type. (IDL 5.2 and earlier only)
;       quiet:                  Set this keyword to suppress informational
;                               output.
;
; OUTPUTS:
;       data:          The data structure containing the file data
;                      requested.
;
;                      OUTPUT DATA STRUCTURE FORM
;                      --------------------------
;                      o The file dimensions are always returned,
;
;                          data.dim1
;                              .dim2
;                              .dim3
;                            .....
;                              .dimN
;
;                      o If variable data is read in, they are present in
;                        the output structure like so:
;
;                          data.var1
;                              .var2
;                              .var3
;                            .....
;                              .varN
;  
;                      o If variable attributes are also requested, the variable
;                        portion of the output structure has the form:
;  
;                          data.var1.DATA
;                                   .att1
;                                   .att2
;                                 .....
;                                   .attN
;                              .var2.DATA
;                                   .att1
;                                   .att2
;                                 .....
;                                   .attN
;                            .....
;                              .varN.DATA
;                                   .att1
;                                   .att2
;                                 .....
;                                   .attN
;
;                        where the capitalised tag DATA is the actual tag name
;                        used for the variable data.
;
;                      o If global attributes are requested, they are present 
;                        in the output structure like so:
;  
;                          data.gatt1
;                              .gatt2
;                              .gatt3
;                            .....
;                              .gattN
;
;
; FUNCTION RESULT:
;       Error_Status:         The return value is an integer defining the error status.
;                             The error codes are defined in the error_codes.pro file.
;                             If == SUCCESS the netCDF data read was successful.
;                                == FAILURE an unrecoverable error occurred.
;                             UNITS:      N/A
;                             TYPE:       INTEGER
;                             DIMENSION:  Scalar
;
; CALLS:
;       is_ncdf:   Function to determine if a file is NetCDF format.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;       None.
;
; PROCEDURE:
;       As each variable/attribute is read in, it is appended to the output data
;       structure.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC, 23-Sep-1999
;                       paul.vandelst@ssec.wisc.edu
;
; (C) Paul van Delst, 1999, 2006
;
;-

;===============================================================================
;
; Function to check the validity of the COUNT, OFFSET, and STRIDE vector
; keywords.
;
; Adapted from Liam Gumley's NC_READ.PRO
;
;===============================================================================

FUNCTION check_vectors, ncdf_id, $
                        variable_info, $
                        count, $
                        offset, $
                        stride, $
                        count_vector, $
                        offset_vector, $
                        stride_vector



;------------------------------------------------------------------------------
;                         -- SET UP ERROR HANDLER --
;------------------------------------------------------------------------------

  @error_codes

  CATCH, Error_Status

  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF    



;------------------------------------------------------------------------------
;                         -- Get the variable dimensions --
;------------------------------------------------------------------------------

; Create the variable dimension array
  variable_dimensions = LONARR( variable_info.NDIMS )

; Loop over the dimensions
  FOR i = 0, variable_info.NDIMS - 1 DO BEGIN

;   Get the dimension name and size
    NCDF_DIMINQ, ncdf_id, $                 ; Input
                 variable_info.DIM[ i ], $  ; Input
                 dimension_name, $          ; Output
                 dimension_size             ; Output

;   Save the dimension size
    variable_dimensions[ i ] = dimension_size

  ENDFOR



;------------------------------------------------------------------------------
;    -- Check for COUNT, OFFSET, and STRIDE vectors of the wrong length --
;------------------------------------------------------------------------------

; Count vector
  IF ( N_ELEMENTS( count ) NE 0 AND $
       N_ELEMENTS( count ) NE variable_info.NDIMS ) THEN BEGIN
    MESSAGE, 'COUNT vector must have ' + $
             STRTRIM( variable_info.NDIMS, 2 ) + $
             ' dimensions for ' + $
             STRUPCASE( variable_info.NAME ) + $
             ' variable', /NONAME, /NOPRINT
  ENDIF

; Offset vector
  IF ( N_ELEMENTS( offset ) NE 0 AND $
       N_ELEMENTS( offset ) NE variable_info.NDIMS ) THEN BEGIN
    MESSAGE, 'OFFSET vector must have ' + $
             STRTRIM( variable_info.NDIMS, 2 ) + $
             ' dimensions for ' + $
             STRUPCASE( variable_info.NAME ) + $
             ' variable', /NONAME, /NOPRINT
  ENDIF

; Stride vector
  IF ( N_ELEMENTS( stride ) NE 0 AND $
       N_ELEMENTS( stride ) NE variable_info.NDIMS ) THEN BEGIN
    MESSAGE, 'STRIDE vector must have ' + $
             STRTRIM( variable_info.NDIMS, 2 ) + $
             ' dimensions for ' + $
             STRUPCASE( variable_info.NAME ) + $
             ' variable', /NONAME, /NOPRINT
  ENDIF



;------------------------------------------------------------------------------
;  -- Check for definition and range of COUNT, OFFSET, and STRIDE vectors --
;------------------------------------------------------------------------------

; Offset vector
; Is it defined?
  IF ( N_ELEMENTS( offset ) EQ 0 ) THEN $
    offset_vector = REPLICATE( 0L, variable_info.NDIMS ) $
  ELSE $
    offset_vector = LONG( offset )
; Is it valid?
  offset_vector = ( offset_vector < ( variable_dimensions - 1L ) ) > $
                  REPLICATE( 0L, variable_info.NDIMS )

; Stride vector
; Is it defined?
  IF ( N_ELEMENTS( stride ) EQ 0 ) THEN $
    stride_vector = REPLICATE( 1L, variable_info.NDIMS ) $
  ELSE $
    stride_vector = LONG(stride)
; Is it valid?
  stride_vector = ( stride_vector < ( variable_dimensions - offset_vector ) ) > $
                  REPLICATE( 1L, variable_info.NDIMS )

; Count vector
; Is it defined?
  IF ( N_ELEMENTS( count ) EQ 0 ) THEN $
    count_vector = ( variable_dimensions - offset_vector ) / stride_vector $
  ELSE $
    count_vector = LONG( count )
; Is it valid?
  count_vector = ( count_vector < ( ( variable_dimensions - offset_vector ) / stride_vector ) ) > $
                 REPLICATE( 1L, variable_info.NDIMS )



;------------------------------------------------------------------------------
;                                  -- Done --
;------------------------------------------------------------------------------

  CATCH, /CANCEL
  RETURN, SUCCESS

END


;===============================================================================
;
; Function to convert BYTE/CHAR values to a STRING data type.
;
; Need this because all string netCDF data types were returned as BYTE arrays.
; For pre IDL v5.3 the returned data type string is "BYTE"
; For IDL v5.3 the returned data type string is "CHAR"
;
;
;===============================================================================

FUNCTION convert_string, data_type, input_string, $
                         no_convert = no_convert

  ; Set the IDL version with the bug fix
  fixed_IDL_version = 5.3

  ; If the data type is CHAR, then we have a string and convert it
  IF ( STRUPCASE( data_type ) EQ 'CHAR' ) THEN $
    RETURN, STRING( input_string )

  ; The data type is not CHAR. Maybe it needs converting, maybe not.
  IF ( FLOAT( !VERSION.RELEASE ) LT fixed_IDL_version AND $
       STRUPCASE( data_type ) EQ 'BYTE'               AND $
       ( NOT KEYWORD_SET( no_convert ) )                  ) THEN $
    RETURN, STRING( input_string )

  ; Don't do anything
  RETURN, input_string

END




;===============================================================================
;
; Main function
;
;===============================================================================

FUNCTION Read_NCDF, ncdf_file, $         ; Input
                    data, $              ; Output

                    ; -- Which variables to read keyword
                    variable_list = variable_list, $    ; Input keyword

                    ; -- How to read the variables keywords
                    count  = count, $    ; Input keyword
                    offset = offset, $   ; Input keyword
                    stride = stride, $   ; Input keyword

                    ; -- Attribute keywords
                    variable_attributes = variable_attributes, $    ; Input keyword
                    global_attributes   = global_attributes, $      ; Input keyword

                    ; -- Conversion keywords
                    no_var_byte_to_string = no_var_byte_to_string, $   ; Input keyword
                    no_att_byte_to_string = no_att_byte_to_string, $   ; Input keyword

                    ; -- Shhhhh.
                    quiet = quiet        ; Input keyword



;------------------------------------------------------------------------------
;                         -- SET UP ERROR HANDLER --
;------------------------------------------------------------------------------

  @error_codes

  CATCH, Error_Status

  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    IF ( N_ELEMENTS( NCDF_Id ) NE 0 ) THEN NCDF_CLOSE, ncdf_id
    RETURN, FAILURE
  ENDIF    



;------------------------------------------------------------------------------
;                            -- Check input --
;------------------------------------------------------------------------------

  n_arguments = 2
  IF ( N_PARAMS() LT n_arguments ) THEN BEGIN
    MESSAGE, 'Invlaid number of arguments', /NONAME, /NOPRINT
  ENDIF

; Check that required arguments are defined
  IF ( N_ELEMENTS( ncdf_file ) EQ 0 ) THEN BEGIN
    MESSAGE, 'Input NCDF_FILE argument not defined!', /NONAME, /NOPRINT
  ENDIF

; Check that file argument is a string
  IF ( SIZE( ncdf_file, /TNAME ) NE 'STRING' ) THEN BEGIN
    MESSAGE, 'Input NCDF_FILE argument must be a string', /NONAME, /NOPRINT
  ENDIF

; Check variable_list keyword. If the variable_list
; keyword is NOT set, the default action is to read
; ALL the data in the NetCDF file
  IF ( KEYWORD_SET( variable_list ) ) THEN $
    all_variables = 0 $
  ELSE $
    all_variables = 1



;------------------------------------------------------------------------------
;                   -- Make sure that file is in NetCDF format --
;------------------------------------------------------------------------------

  result = is_ncdf( ncdf_file )

  IF ( result NE SUCCESS ) THEN BEGIN
    MESSAGE, ncdf_file + ' is not a NetCDF format file', /NONAME, /NOPRINT
  ENDIF



;------------------------------------------------------------------------------
;                        -- Open the netCDF data file --
;------------------------------------------------------------------------------

  ncdf_id = NCDF_OPEN( ncdf_file, /NOWRITE )

  IF ( ncdf_id EQ -1 ) THEN BEGIN
    MESSAGE, 'Error opening file ' + ncdf_file, /NONAME, /NOPRINT
  ENDIF



;------------------------------------------------------------------------------
;                   -- Print out some dimension information --
;------------------------------------------------------------------------------

  ncdf_file_info = NCDF_INQUIRE( ncdf_id )

  IF ( NOT KEYWORD_SET( quiet ) ) THEN BEGIN

    PRINT, FORMAT = '( 10x,"Number of dimensions        : ",i7 )', $
                    ncdf_file_info.NDIMS
    PRINT, FORMAT = '( 10x,"Number of variables         : ",i7 )', $
                    ncdf_file_info.NVARS
    PRINT, FORMAT = '( 10x,"Number of global attributes : ",i7 )', $
                    ncdf_file_info.NGATTS
    PRINT, FORMAT = '( 10x,"ID of unlimited dimension   : ",i7 )', $
                    ncdf_file_info.RECDIM

  ENDIF
  
;------------------------------------------------------------------------------
;                       -- Get the dimension information --
;------------------------------------------------------------------------------

  FOR i=0, ncdf_file_info.NDIMS-1 DO BEGIN

;   Get dimension info
    NCDF_DIMINQ, ncdf_id, i, DimName, DimSize

;   Load into structure
    IF ( i EQ 0 ) THEN $
      data = CREATE_STRUCT( 'dim_'+DimName, DimSize ) $
    ELSE $
      data = CREATE_STRUCT( data, 'dim_'+DimName, DimSize )

  ENDFOR


;------------------------------------------------------------------------------
;                     -- Read the global attributes? --
;------------------------------------------------------------------------------

  IF ( KEYWORD_SET( global_attributes ) ) THEN BEGIN

;   Determine the number of global attributes
    n_global_attributes = ncdf_file_info.NGATTS

;   Are there any global attributes to read?
    IF ( n_global_attributes GT 0 ) THEN BEGIN

;     loop over global attributes
      FOR i = 0, n_global_attributes - 1 DO BEGIN

;       Get global attribute name
        attribute_name = NCDF_ATTNAME( ncdf_id, $  ; Input
                                       i, $        ; Input
                                       /GLOBAL )   ; Input keyword

;       Get global attribute value
        NCDF_ATTGET, ncdf_id, $         ; Input
                     attribute_name, $  ; Input
                     attribute, $       ; Output
                     /GLOBAL            ; Input keyword

;       Get global attribute info
        attribute_info = NCDF_ATTINQ( ncdf_id, $         ; Input
                                      attribute_name, $  ; Input
                                      /GLOBAL )          ; Input keyword

;       If necessary and required, convert BYTE/CHAR attribute to STRING
        attribute = convert_string( attribute_info.datatype, $
                                    attribute, $
                                    no_convert = no_att_byte_to_string )

;       Append to structure
        data = CREATE_STRUCT( data, $                ; Input
                              attribute_name, $      ; Input
                              attribute )            ; Input

      ENDFOR      ; Loop over global attributes

;   No global attributes to read
    ENDIF ELSE BEGIN
      MESSAGE, 'No global attributes to read!', /INFO
    ENDELSE     ; n_global_attributes > 0 IF statement

  ENDIF


;------------------------------------------------------------------------------
;                             -- Read the data --
;------------------------------------------------------------------------------

; Set the number of variables to read and
; initialise the valid variable counter
  IF ( all_variables EQ 0 ) THEN $
    n_variables = N_ELEMENTS( variable_list ) $
  ELSE $
    n_variables = ncdf_file_info.NVARS

; Are there any variables to read?
  IF ( n_variables GT 0 ) THEN BEGIN

;   Loop over variables
    FOR i = 0, n_variables - 1 DO BEGIN

;     Get the variable ID
      IF ( all_variables EQ 0 ) THEN BEGIN

;       Only getting requested data so
;       set the current variable name
        variable_name = variable_list[ i ]

;       Get the current variable ID
        variable_id = NCDF_VARID( ncdf_id, $       ; Input
                                  variable_name )  ; Input

;       Is the current variable present in the NetCDF file?
        IF ( variable_id LT 0 ) THEN BEGIN
          MESSAGE, 'Variable ' + variable_name + ' not present in ' + ncdf_file, /NONAME, /NOPRINT
        ENDIF

      ENDIF ELSE BEGIN

;       Getting all data. Use loop counter as variable ID
        variable_id = i

      ENDELSE

;     Get the variable info
      variable_info = NCDF_VARINQ( ncdf_id, $     ; Input
                                   variable_id )  ; Input

;     Make sure we have the variable name
      variable_name = variable_info.NAME

;     Does the current variable have dimensions?
      IF ( variable_info.NDIMS EQ 0 ) THEN BEGIN

;       No. It is scalar. Simply read it.
        NCDF_VARGET, ncdf_id, $          ; Input
                     variable_id, $      ; Input
                     variable_data       ; Output

      ENDIF ELSE BEGIN

;       Yes. Check COUNT, OFFSET, and STRIDE vectors for this variable.
        result = check_vectors( ncdf_id, $         ; Input
                                variable_info, $   ; Input
                                count, $           ; Input
                                offset, $          ; Input
                                stride, $          ; Input
                                count_vector, $    ; Output
                                offset_vector, $   ; Output
                                stride_vector )    ; Output
        IF ( result NE SUCCESS ) THEN BEGIN
          MESSAGE, 'COUNT, OFFSET, and/or STRIDE vector check failed.', /NONAME, /NOPRINT
        ENDIF

;       Read the variable data
        NCDF_VARGET, ncdf_id, $                 ; Input
                     variable_id, $             ; Input
                     variable_data, $           ; Output
                     count  = count_vector, $   ; Input keyword
                     offset = offset_vector, $  ; Input keyword
                     stride = stride_vector     ; Input keyword

      ENDELSE      ; Scalar or array? Variable dimension IF statement

;     If necessary and required, convert BYTE/CHAR variable data to STRING
      variable_data = convert_string( variable_info.datatype, $
                                      variable_data, $
                                      no_convert = no_var_byte_to_string )

;     Determine the number of variable attributes
      n_variable_attributes = variable_info.NATTS

;     Retrieve the variable attributes if required
      IF ( KEYWORD_SET( variable_attributes ) AND $
           n_variable_attributes GT 0             ) THEN BEGIN

;       Create the variable data structure with generic DATA tag
        variable_data = CREATE_STRUCT( 'data', $        ; Input
                                       variable_data )  ; Input

;       loop over current variable's attribute
        FOR j = 0, n_variable_attributes - 1 DO BEGIN

;         Get the current attribute name
          attribute_name = NCDF_ATTNAME( ncdf_id, $       ; Input
                                         variable_id, $   ; Input
                                         j )              ; Input

;         Get the current attribute value
          NCDF_ATTGET, ncdf_id, $         ; Input
                       variable_id, $     ; Input
                       attribute_name, $  ; Input
                       attribute          ; Output

;         Get the current attribute info
          attribute_info = NCDF_ATTINQ( ncdf_id, $        ; Input
                                        variable_id, $    ; Input
                                        attribute_name )  ; Input

;         If necessary and required, convert BYTE attribute to STRING
          attribute = convert_string( attribute_info.datatype, $
                                      attribute, $
                                      no_convert = no_att_byte_to_string )

;         Add current attribute to variable structure
          variable_data = CREATE_STRUCT( variable_data, $   ; Input
                                         attribute_name, $  ; Input
                                         attribute )        ; Input

        ENDFOR      ; Loop over current variable attributes

      ENDIF       ; Get attributes IF statement

;     Append data to return structure
      data = CREATE_STRUCT( data, $                       ; Input
                            variable_name, $              ; Input
                            TEMPORARY( variable_data ) )  ; Input

    ENDFOR      ; Loop over requested variables

; No data to read
  ENDIF ELSE BEGIN

    MESSAGE, 'No variables to read!', /INFO

  ENDELSE     ; n_variables > 0 IF statement



;------------------------------------------------------------------------------
;                       -- Close the NetCDF data file --
;------------------------------------------------------------------------------

  NCDF_CLOSE, ncdf_id



;------------------------------------------------------------------------------
;                                  -- Done --
;------------------------------------------------------------------------------

  CATCH, /CANCEL
  RETURN, SUCCESS


END ; FUNCTION Read_NCDF
