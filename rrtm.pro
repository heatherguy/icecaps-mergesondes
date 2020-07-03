; $Id: rrtm.pro,v 1.5 2006/12/13 13:33:24 dturner Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Abstract:
;	  This routine reads in the output from the RRTM and plots up
;	the various fields, as desired.  The best use of this script is
;	to just read the data into IDL, afterwhich it can be manipulated
;	by hand.  The output file is assumed to have the name OUTPUT_RRTM.
;
; Author:	Dave Turner, PNNL
; Date:		February, 1998
;
; Arguments:
;	NLAYERS:	The number of model layers in the RRTM calculation
;	NREGIONS:	The number of spectral regions computed
;
; Keywords:
;	PATH:		The path to the RRTM output
;	OLR:		The calculated outgoing longwave flux, by band 
;				(0th position is total)
;	SLR:		The calculated surface longwave flux, by band
;				(0th position is total)
;	WNUM:		The center wavenumber of each band
;	WBOUNDS:	The starting and ending wavenumber of each band
;	PRES:		The pressure array [mb]
;	HR:		The heating rate profile [K / day]
;	DOSTOP:		If set, then stop within the procedure before exitting
;
; Call:
	pro rrtm, nregions=nregions, path=path, filename=filename, $
		olr=olr, slr=slr, wnum=wnum, wbounds=wbounds, $
		pres=pres, hr=hr, dostop=dostop
;-

  if(n_elements(filename) eq 0) then filename = 'OUTPUT_RRTM'
  if(n_elements(path) eq 0) then path = '.'
  if(n_elements(nregions) eq 0) then nregions = 1
  nregions = fix(nregions)
  if(nregions lt 1 or nregions gt 17) then nregions = 1

   region_string = strarr(nregions)
   field_names = ''
   units = ''

	; read in the data
   pushd,path
   files = findfile(filename, count=count)
   if(count ne 1) then begin
     print,'Unable to unambiguously determine ' + filename
     print,'Aborting!'
     popd
     return
   endif

   openr,lun,files(0),/get_lun
   	; Figure out how many layers are in the file...
   foo = replicate(' ',4)
   readf,lun,foo
   nlayers = fix(foo(3))+1
   point_lun,lun,0		; And rewind the pointer to the beginning of the file

   	; Allocate space for data
   data = fltarr(nregions,6,nlayers)
   foo  =  fltarr(6,nlayers)
   line = ''

   for i=0, nregions -1 do begin
     readf,lun,line
     region_string(i) = line
	
	; Take a moment and figure out the mean wavenumber for the regime
     case strmid(line,0,34) of
	' Wavenumbers:   10.0 - 3250.0 cm-1':	begin
		wnum = -1
		wnumb = transpose([10,3250])
	  end
	' Wavenumbers:   10.0 -  350.0 cm-1':	begin
		wnum = [wnum, 120]
		wnumb = [wnumb,transpose([10,350])]
	  end
	' Wavenumbers:  350.0 -  500.0 cm-1':	begin
		wnum = [wnum, 375]
		wnumb = [wnumb,transpose([350,500])]
	  end
	' Wavenumbers:  500.0 -  630.0 cm-1':	begin
		wnum = [wnum, 565]
		wnumb = [wnumb,transpose([500,630])]
	  end
	' Wavenumbers:  630.0 -  700.0 cm-1':	begin
		wnum = [wnum, 665]
		wnumb = [wnumb,transpose([630,700])]
	  end
	' Wavenumbers:  700.0 -  820.0 cm-1':	begin
		wnum = [wnum, 760]
		wnumb = [wnumb,transpose([700,820])]
	  end
	' Wavenumbers:  820.0 -  980.0 cm-1':   begin
		wnum = [wnum, 900]
		wnumb = [wnumb,transpose([820,980])]
	  end
	' Wavenumbers:  980.0 - 1080.0 cm-1':   begin
		wnum = [wnum, 1030]
		wnumb = [wnumb,transpose([980,1080])]
	  end
	' Wavenumbers: 1080.0 - 1180.0 cm-1':   begin
		wnum = [wnum, 1130]
		wnumb = [wnumb,transpose([1080,1180])]
	  end
	' Wavenumbers: 1180.0 - 1390.0 cm-1':   begin
		wnum = [wnum, 1285]
		wnumb = [wnumb,transpose([1180,1390])]
	  end
	' Wavenumbers: 1390.0 - 1480.0 cm-1':   begin
		wnum = [wnum, 1435]
		wnumb = [wnumb,transpose([1390,1480])]
	  end
	' Wavenumbers: 1480.0 - 1800.0 cm-1':   begin
		wnum = [wnum, 1640]
		wnumb = [wnumb,transpose([1480,1800])]
	  end
	' Wavenumbers: 1800.0 - 2080.0 cm-1':   begin
		wnum = [wnum, 1940]
		wnumb = [wnumb,transpose([1800,2080])]
	  end
	' Wavenumbers: 2080.0 - 2250.0 cm-1':   begin
		wnum = [wnum, 2165]
		wnumb = [wnumb,transpose([2080,2250])]
	  end
	' Wavenumbers: 2250.0 - 2380.0 cm-1':   begin
		wnum = [wnum, 2315]
		wnumb = [wnumb,transpose([2250,2380])]
	  end
	' Wavenumbers: 2380.0 - 2600.0 cm-1':   begin
		wnum = [wnum, 2490]
		wnumb = [wnumb,transpose([2380,2600])]
	  end
	' Wavenumbers: 2600.0 - 3250.0 cm-1':   begin
		wnum = [wnum, 2800]
		wnumb = [wnumb,transpose([2600,3250])]
	  end
	else: begin
	    print,'Unidentified spectral window in the RRTM output - quitting'
	    free_lun,lun
	    return
	  endelse
     endcase
     wbounds = transpose(wnumb)

	; Ok, continue reading the file
     readf,lun,line
     field_names = line
     readf,lun,line
     units = line
     bar = ' '
     for jj=0,nlayers-1 do begin
       readf,lun,bar
       bar = strcompress(bar)
       parts = str_sep(bar, ' ')
       if(parts(0) eq ' ' or parts(0) eq '') then parts = parts(1:n_elements(parts)-1)

       if(strmid(parts(0),0,2) eq '**') then foo(0,jj) = !values.f_nan $
       else foo(0,jj) = float(parts(0))

       if(strmid(parts(1),0,2) eq '**') then foo(1,jj) = !values.f_nan $
       else foo(1,jj) = float(parts(1))

       if(strmid(parts(2),0,2) eq '**') then foo(2,jj) = !values.f_nan $
       else foo(2,jj) = float(parts(2))

       if(strmid(parts(3),0,2) eq '**') then foo(3,jj) = !values.f_nan $
       else foo(3,jj) = float(parts(3))

       if(strmid(parts(4),0,2) eq '**') then foo(4,jj) = !values.f_nan $
       else foo(4,jj) = float(parts(4))

       if(strmid(parts(5),0,2) eq '**') then foo(5,jj) = !values.f_nan $
       else foo(5,jj) = float(parts(5))
     endfor
     data(i,*,*) = foo
     readf,lun,line
   endfor
   
   free_lun,lun
   popd

	; Now dump out a few of the more useful quantities
   olr = data(*,2,0)		; Outgong longwave radiation
   slr = data(*,3,nlayers-1) 	; Surface longwave radiation

   pres = reform(data(*,1,*))	; The pressure profile
   hr   = reform(data(*,5,*))	; The heating rate profile
   if(keyword_set(dostop)) then stop, 'Stopping in procedure as indicated'
end
