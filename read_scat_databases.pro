; $Id: read_scat_databases.pro,v 1.7 2006/12/04 21:02:02 dturner Exp $
;+
; Abstract:
; 	This routine reads in the scattering properties from databases that were
;   created by Mie code (newmie_iteration) and by Ping Yang's computations (that
;   database was compiled by write_database.pro in IceScatProperties), and other
;   similar scattering property databases.  Rectangular matrices are returned, 
;   which can then be processed by the routine get_scat_properties.pro to extract 
;   out the properties depending on wavelength and effective radius.  Note that 
;   there are two Mie databases: one for ice particles and one for water particles.
;
;	Previous versoins of this algorithm read in multiple scattering DBs at one
;   time; however, it made more sense to only read in one DB at a time...
;
;	This version (v1.6 and greater) assumes that there is a scattering phase
;   function for each wnum/radius in the database.
;
; Author:
;	Dave Turner
;		Pacific Northwest National Laboratory
;		University of Wisconsin - Madison
;
; Date:
;	March 2002
;
; Call:
    function read_scat_databases, $	; Error status: 0 -> read ok, 1 -> error
	database, $			; Array with the scattering properties
	dbname=dbname, $		; Name of the scattering database
	mie_wat=mie_wat, $		; If set and no path, then use default path
	mie_ice=mie_ice, $		; If set and no path, then use default path
	dostop=dostop			; If set, then stop in the routine

  	; Put in the default paths to the databases
  if(keyword_set(mie_wat) and keyword_set(mie_ice)) then begin
    print,'Error: Can not have both mie_ice and mie_wat set simultaneously'
    return, 1
  endif

  if(keyword_set(mie_wat) and n_elements(dbname) eq 0) then $
    dbname = '/home/dturner/vip/src/radiation/newmie/ssp_db.mie_wat.gamma_sigma_0p100'
  if(keyword_set(mie_ice) and n_elements(dbname) eq 0) then $
    dbname = '/home/dturner/vip/src/radiation/newmie/ssp_db.mie_ice.gamma_sigma_0p100'
;-

  	; See if the database actually exists
  database_name = findfile(dbname, count=count)
  if(count ne 1) then begin
    print,'ERROR: Unable to determine/find the scattering database'
    return,1
  endif

	; Number of header lines (before the phase function angles)
  nheader = 5

  	; Number of columns of data (not including the phase function)
  ncols = 13

	; Open and read the single scattering property database
  print,'Reading: ' + database_name(0)
  openr,lun,database_name(0),/get_lun
  header = replicate(' ', nheader)
  readf,lun,header
  nlines = fix(header(nheader - 3))
  if(nlines le 0) then begin
    print,'ERROR: There were no datalines found in this database -- this should not occur'
    free_lun,lun
    return,1
  endif 
  nphase = fix(header(nheader - 2))
  if(nlines le 0) then begin
    print,'ERROR: The scattering phase function was not defined in this database'
    free_lun,lun
    return,1
  endif 
  data   = fltarr(ncols+nphase, nlines)
  pangle = fltarr(nphase)
  readf,lun,pangle
  another_string = ' '
  readf,lun,another_string
  readf,lun,data
  free_lun,lun

  fields = ['wavelength [um]','wavenumber [cm-1]', $
  	'effective radius', 'extinction cross section', $
  	'scattering cross section','absorption cross section',$
	'single scatter albedo','asymmetry parameter','Extinction efficiency', $
	'Absorption efficiency','Scattering efficiency','Volume','Projected area',$
	'Rest of the elements are phase function']
  database = {dbname:dbname, ncols:ncols, nphase:nphase, pangle:pangle, data:data, $
  	columns:fields}

  return,0
end
