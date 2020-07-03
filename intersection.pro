;$Id: intersection.pro,v 1.1 1997/08/10 17:25:04 d3h797 Release_ddt_1_13 $
;
; Copyright (c) 1997, RESEARCH SYSTEMS, INC.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       INTERSECTION
;
; PURPOSE:
;       This function finds the intersection of two data sets.
;       The intersection can be based on an exact match or matches
;       based on a given delta.  The result is an array containing
;       the elements of the B array found in the A array.  The result
;       has the same datatype as the B array.
;
; CATEGORY:
;       Mathematics.
;
; CALLING SEQUENCE:
;       Result = Intersection(A, B, [DELTA , COUNT = , DIGITS = ])
;
; ARGUMENTS:
;       A:      An array of numbers.  Valid datatypes are: byte, integer,
;               long integer, float, double.
;	        If the DELTA keyword is used then the values of A plus
;	        or minus DELTA are compared to the values of B.
;
;       B:      An array of numbers.  Valid datatypes are: byte, integer,
;               long integer, float, double.
;
;	DELTA:  Determines how close a value in A must be to a value in B.
;		The default is 0 (exact matching).
;
; KEYWORDS:
;       COUNT:  The number of elements in the intersection
;
;       DIGITS: Used to specify the number of decimal places to consider 
;               when operating on floating point data.  Default is 3.  
;               NOTE:  When both DELTA and the DIGITS keyword are used, the
;               one which specifies the greatest precision overrides. 
;
; EXAMPLES:
;		IDL > a = [ -34, 135, -4, 0, 25, 100]
;               IDL > b = [ 45, -30, 100, 23]
;               IDL > result = INTERSECTION(a, b)
;               IDL > PRINT, result
;                     100
;               IDL > result = INTERSECTION(a, b, 2)
;               IDL > PRINT, result
;                     23    100
; 
;		IDL > a = [ -34.05, 135.34, -4.76, 0.01, 25.93, 100.34]
;               IDL > b = [ 45.54, -30.88, 100.26, 23.18]
;               IDL > result = INTERSECTION(a, b, COUNT = count)
;               IDL > PRINT, count
;                     0
;               IDL > PRINT, result
;                     -1
;               IDL > result = INTERSECTION(a, b, 0.2, COUNT = count)
;               IDL > PRINT, result
;                     100.300
;               IDL > result= INTERSECTION(a, b, 0.2, COUNT= count, DIGITS = 2)
;               IDL > PRINT, result
;                     100.260
;
; MODIFICATION HISTORY:
;       Created by:  GJH & SCK, RSI, January 1997
;-

FUNCTION Intersection, origA, origB, delta, Count = count , Digits = digits

  IF N_PARAMS() LT 2 THEN $
	Message, "Requires two arguments"
  IF N_PARAMS() EQ 2 THEN $
	delta = 0 $
  ELSE $
     IF delta LT 0 THEN $
	Message, "DELTA must be >= zero"
  
; Determine Datatype of A and B arrays
  dimA = (Size(origA))(0)
  dimB = (Size(origB))(0)
  dtA = (Size(origA))(dimA+1)
  dtB = (Size(origB))(dimB+1)
  ;Print, dtA, dtB

; Create temporary copies of arrays A and B.
  a = origA
  b = origB
  idelta = delta
  factor = 1

  IF dtA GE 4 OR dtB GE 4 THEN BEGIN  ; Float?


    IF idelta NE 0 THEN BEGIN  
; Determine the scale factor needed to convert floats to integers.
      idelta = idelta* factor
      WHILE ((idelta) - Fix(idelta)) NE 0 DO BEGIN  
        factor = factor * 10.
        IF factor EQ 1e8 THEN $
           Message, 'Too many significant digits'
        idelta = delta * factor    ; Determine the integer-based DELTA
      ENDWHILE

      idelta = Fix(idelta)
     ;Help, factor, idelta
    ENDIF

    IF Keyword_Set(digits) THEN BEGIN 
      IF digits GE 8 THEN $
         Message, 'Too many significant digits'
      IF (10. * digits) GT factor  THEN BEGIN
           factor = 10. ^ digits
           idelta = factor * delta
      ENDIF
      ;Help,idelta, factor
    ENDIF ELSE IF idelta EQ 0 THEN  BEGIN
      factor = 1000.
      idelta = factor*delta
      ;Help,idelta, factor
    ENDIF

; Create integer versions of A and B
    a = Round(Temporary(a) * factor)
    ;Help,a
    b = Round(Temporary(b) * factor)
  ENDIF ELSE BEGIN  ; Byte or Integers
    IF ((idelta) - Fix(idelta)) NE 0 THEN $
      Message, 'DELTA must be integer when A, B not type FLOAT or DOUBLE'
  ENDELSE

  offset = (Min(a) - idelta) < Min(b)
 ; Print, offset
  num = ((Max(a) + idelta) > Max(b)) - offset + 1
  a = Temporary(a) - offset

; Create a mask image based on A
  maskA = BytArr(num)
  maskA(a) = 1B

; Create expanded mask based on DELTA  
  IF idelta GT 0 THEN BEGIN
   ; Print, "inside expand mask"
    minMaskA = Shift(maskA, -1 * idelta)
    maxMaskA = Shift(maskA, idelta)
    maskA    = Temporary(maskA) OR Temporary(minMaskA) OR $
                 Temporary(maxMaskA)
    maskA    = NOT(Temporary(maskA)) - 254B
    s = BytArr(idelta) + 1B
    maskA  = Dilate(Erode(Temporary(maskA), s), s)
    s = 0B
    maskA  = NOT( Temporary(maskA)) - 254B
  ENDIF

  maskB = BytArr(num)
  b = Temporary(b) - offset
  maskB(b) = 1B
  both   = Temporary(maskA) * Temporary(maskB)
  match = Where(both EQ 1, count)
  IF count GT 0 THEN BEGIN 
    result = Double(match + offset) / factor
   ; Print, 'Data Type B: ', dtB
    CASE dtB OF
      1: RETURN, Byte(result) 
      2: RETURN, Fix(result) 
      3: RETURN, Long(result) 
      4: RETURN, Float(result)
      5: RETURN, result 
    ENDCASE
  ENDIF ELSE BEGIN 
    RETURN, -1
  ENDELSE
END
  

