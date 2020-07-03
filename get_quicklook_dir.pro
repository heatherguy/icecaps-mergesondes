;*******************************************************************************
; COPYRIGHT (C) 2001 Battelle Memorial Institute. 
;    All Rights Reserved. (Now, with this out of the way...) 
;
; RCS INFORMATION: 
;   $RCSfile: get_quicklook_dir.pro,v $ 
;   $Revision: 1.1 $ 
;   $Author: dturner $ 
;   $Locker:  $ 
;   $Date: 2003/08/29 15:02:54 $ 
;   $State: Release_ddt_1_13 $ 
;   $Name:  $ 
;   $Id: get_quicklook_dir.pro,v 1.1 2003/08/29 15:02:54 dturner Release_ddt_1_13 $ 
;
; COPYRIGHT (C) 2001 Battelle Memorial Institute.  All Rights Reserved
;
; PROCEDURES IN THIS FILE:
;
; FUNCTIONS IN THIS FILE:
; 	get_quicklook_home
;
; ABSTRACT:
;
; KEYWORDS:
;	  Date:	         The date of the quicklook
;   Rootname:	     Name of the VAP or platform creating the quicklook
;	  Site:          The site for the data (SGP, NSA, TWP)
;   Facility:      The facility for the data (B1,B4, B5, B6, C1)
;		DataLevel:     ARM data level (a1, b1, c1, etc)
;		Create:        If set will create the directory
;		QuickLook_Dir: Variable to put path into
;		ErrorNum:      Variable to write error number into:
;		                 1:  QUICKLOOK_DATA dir does not exist
;		                 2:  could not create directory
;		                 3:  could not set mode on new directory
;		                 4:  QUICKLOOK_DATA env var does not exist
;
; EXAMPLE:  
;           setenv, 'QUICKLOOK_DATA = /data/quicklook'
;           get_quicklook_dir, '20010103', 'test', 'sgp', 'B1', 'c1', $
;                                quicklook_dir = qd, /create, errornum = en
;           
;           print, qd
;           This will print out:
;             /data/quicklook/sgp/sgptestB1.c1/2001/01/03
;
;           print, en
;           This will print out:
;             0
;
;           The directory will have been created because of the /create
;*******************************************************************************

pro get_quicklook_dir, date, rootname, site, facility, datalevel, $
                       create = create, quicklook_dir = quicklook_dir, $
                       errornum = errornum

	; get the initial quicklook data directory
	;		should be something like /data/quicklook
	quicklook_data_dir = getenv("QUICKLOOK_DATA")

	;	if the env var does not exist exit.
	if (quicklook_data_dir eq "") then begin
		errornum = 4
		return
	endif

	ldate = long(date)

	; Convert the date to an 8 digit date, if it is not already so.
	;    However, since the data may still have a six digit date in the name,
	;    then build the datestring to search for only the six digit date, as
	;    it will find the appropriate data in either case.
  if(ldate le 700000) then ldate = ldate + 20000000L
  if(ldate le 999999) then ldate = ldate + 19000000L

	;	break out the year
  year = LONG(ldate/10000)
  yearstr = string(format='(I0)', year)

	;	break out the month
  month = LONG((ldate - year*10000)/100)
  monthstr = string(format='(I2.2)', month)

	;	break out the day 
  day = LONG(ldate - year*10000 - month*100)
  daystr = string(format='(I2.2)', day)

	;	build the quicklook directory paht
	if (datalevel ne '') then datalevel = '.' + datalevel 
	if (site eq '') then site = strmid(rootname, 0, 3) $
	else rootname = site + rootname
	datastream_dir = quicklook_data_dir + '/' + site + '/' + $
	                 rootname + facility + datalevel
	quicklook_year = datastream_dir + '/' + yearstr
	quicklook_month = quicklook_year + '/' + monthstr
	quicklook_day = quicklook_month + '/' + daystr
	quicklook_dir = quicklook_day

	;	if create is ture create the directory and do error checking
	if (keyword_set(create)) then begin
		if (file_test(quicklook_data_dir, /directory, /write) eq 1) then begin
			file_mkdir, quicklook_dir
			catch, error_status
			if (error_status eq 0) then $
				file_chmod, quicklook_year, '2775'o
			error_status = 0
			catch, error_status
			if (error_status eq 0) then $
				file_chmod, quicklook_month, '2775'o
			error_status = 0
			catch, error_status
			if (error_status eq 0) then $
				file_chmod, quicklook_day, '2775'o
			error_status = 0
			if (file_test(quicklook_dir, /directory, get_mode=mode) eq 1) then begin
				if (mode eq '2775'o) then errornum = 0 $
				else begin
					errornum = 3
					quicklook_dir = ""
				endelse
			endif else begin
				errornum = 2
				quicklook_dir = ""
			endelse
		endif else errornum = 1
	endif

end
