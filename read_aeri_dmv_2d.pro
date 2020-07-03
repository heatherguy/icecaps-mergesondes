;+
; $Id: read_aeri_dmv_2d.pro,v 1.1 2008/05/20 13:19:27 dturner Exp $
;
; Abstract:
;	This routine read in a 2-dimensional field from the binary DMV datafiles
;   that are used to store the raw AERI data.  I apologize in advance for the 
;   crudeness of this script.
;
; Author:
;	Dave Turner
;	SSEC / University of Wisconsin - Madison
; 
; Date:
;	May 2008
;
; Comment:
;	The binary "read_aeri_dmv" must be in the excutable path, or specified
;    via the keyword option.
;
function read_aeri_dmv_2d, $		; Returns a structure with the data
	filename, $			; The DMV file to read
	fieldname, $			; The field to read
	exec=exec, $			; If the executable 'read_aeri_dmv' is not
					;   in the path, then this keyword must be
					;   set to point to this file
	tmpfile=tmpfile, $		; The name of the temporary file to use
	dostop=dostop
;-

  if(n_elements(tmpfile) eq 0) then begin 
    notok = 1
    while(notok gt 0) do begin
      tmpfile = string(format='(A,I0)','tmp',long(randomu(seed)*100000))
      foo = findfile(tmpfile, count=count)
      if(count eq 0) then notok = 0 else notok = notok + 1
      if(notok gt 10) then begin
        print,'Error: Unable to find a name for the temporary file -- aborting'
	return,0
      endif
    endwhile
  endif
  if(n_elements(exec) eq 0) then exec = 'read_aeri_dmv'

  command = string(format='(A,1x,A,1x,A,1x,A,1x,A)', $
  		exec,filename,fieldname,">",tmpfile)
  spawn,'rm -f ' + tmpfile + ' ; ' + command

  openr,lun,tmpfile,/get_lun
  foo = ''
  readf,lun,foo
  dim = 0
  readf,lun,dim
  if(dim ne 2) then begin
    print,'Error: the field '+fieldname+' is not a 2-d array in '+filename
    return,0
  endif
  dim = intarr(2)
  readf,lun,dim
  data = fltarr(dim(1)+1,dim(0))
  readf,lun,data
  free_lun,lun
  wnum = transpose(data(0,*))
  rad  = transpose(data(1:dim(1),*))
  spawn,'rm -f ' + tmpfile
  return,{wnum:wnum, data:rad}
end

