;-------------------------------------------------------------
;+
; NAME:
;       ncdf_read_structure
; PURPOSE:
;       Reads a ncdf file into a structure
;	Caution: it puts every thing into main memory
;
; CATEGORY:In/Output
;
; CALLING SEQUENCE:
;
;	ncdf_read_structure, filename, structure
;
; MODIFICATION HISTORY:
;       Written by R. Preusker, Dez, 1998.
;		added global attribut functionality by M. Reuter, okt, 2000
;
; Copyright (C) 1998, Freie Universitaet Berlin
; This software may be used, copied, or redistributed as long
; as it is not sold and this copyright notice is reproduced on
; each copy made.  This routine is provided as is without any
; express or implied warranties whatsoever.
;-

function replace_space,in
out=in
while 1 do begin
	res= STRPOS(out," ")
	if res eq -1 then return,out
	strput,out,"_",res
endwhile
end

function good_name,in
	out=in
	verboten=["#",":",".","/","+","-","*","!","$",";",'"',"&",":",",","?","@","\","[","]"]
	out=replace_space(out)
	for i=0,n_elements(verboten)-1  do out=replace_string(out,verboten(i),"_")
	return,out
end

pro ncdf_read_structure, filename, struc

	s=shift(size(filename),2)
	if s(0) eq 0 then begin    ; undefinend
		;filename=pickfile(title='Select File',filter='*.nc')
	endif


	result=findfile(filename, count=c)
	if c ne 1 then begin
		print, 'File does not exist: ', filename
		stop
                ;filename=pickfile(title='Select File',filter='*.nc')
		if filename eq '' then goto,ende
	endif

	id=ncdf_open(filename)

	result = ncdf_inquire(id)

	tag_name = strarr(result.nvars + result.ngatts)

	for i = 0,result.nvars - 1 do begin
		dum = ncdf_varinq(id,i)
		tag_name(i) = dum.name
	endfor
	for i = 0,result.ngatts - 1 do begin
		dum = ncdf_attname(id, i, /global)
		tag_name(i + result.nvars) = dum
	endfor

	ncdf_varget,id,tag_name(0),dum

	struc=create_struct(good_name(tag_name(0)),dum)

	for i = 1, result.nvars - 1 do begin
		ncdf_varget, id, tag_name(i),dum
		struc = create_struct(struc, good_name(tag_name(i)), dum)
	end
	for i = 0, result.ngatts - 1 do begin
		ncdf_attget, id, tag_name(i + result.nvars), dum, /global
		struc = create_struct(struc, good_name(tag_name(i + result.nvars)), dum)
	end


	ncdf_close,id
ende:
end




