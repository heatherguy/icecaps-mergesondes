; $Id: read_lbldis.pro,v 1.1 2004/12/16 19:25:45 dturner Exp $
;+
; Abstract:
;	This routine reads in an arbitrary LBLDIS output file, returning the
;    data in a structure.
;
; Author:
;	Dave Turner
;		Pacific Northwest National Laboratory
;
; Date:
;	Dec 2004
;
; Keywords:
;	BT:	If set, then convert the radiance data to brightness temperature
;
; Call:
   function read_lbldis, filename, bt=bt, dostop=dostop
;-

  	; Anticipate two header lines at the top of the file
  nheader = 2

  files = findfile(filename, count=count)
  if(count ne 1) then begin
    print,'Error: Unable to uniquely determine ' + filename
    return,0
  endif

  command = 'wc -l ' + files(0)
  spawn,command,result
  nlines = long(result(n_elements(result)-1)) - nheader

  	; Open the file, skip the header, and get one row of data.  Then use
	; this one row to determine how many columns there are in the data
  openr,lun,files(0),/get_lun
  header = replicate(' ',nheader)
  readf,lun,header
  foo = ''
  readf,lun,foo
  free_lun,lun
  line = strcompress(foo)
  parts = str_sep(line, ' ')
  ncols = 0
  for i=0,n_elements(parts)-1 do $
    if(parts(i) ne '' and parts(i) ne ' ') then ncols = ncols + 1

  	; Open the file, skip the header, and read in the data.
  openr,lun,files(0),/get_lun
  data = fltarr(ncols,nlines)
  readf,lun,header
  readf,lun,data
  free_lun,lun

	; If desired, convert the radiance data to brightness temperature
  if(keyword_set(bt)) then begin
    for i=1,ncols-1 do begin
      invplanck,data(0,*),data(i,*),bt
      data(i,*) = bt
    endfor
  endif

	; Stop, if desired
  if(keyword_set(dostop)) then stop,'Stopped inside routine as desired'

  	; Return the data
  return, data
end

  
