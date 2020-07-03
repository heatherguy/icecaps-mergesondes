; $Id: pltqmecloud_flag.pro,v 1.1 1997/12/10 22:46:52 d3h797 Release_ddt_1_13 $
;+
; Abstract:
;    This little routine plots binary flag "nonclear_flag" for the QMECLOUD 
;      database files.  This provides a quick way to view a month's worth of
;      data (for instance) in order to find clear sky periods to reprocess.
;      Note that this is really designed to be used with monthly database 
;      files, and therefore the axis titles might not make a lot of sense
;      if used for other files.
;
; Author: Dave Turner, PNNL
; Date:   December, 1997
; 
; Arguments:
; 	DATE:		The date of the file to read in, in YYMM format.  From 
;				this, the filename will be built.  The filename
;				is expected to be "qmecloud_db_yymm.cdf"
;
; Keywords:
;	PATH:		The path to the data.  Default is '.'
;	DATE_STRING:	If set, this string will override the default date 
;				string used in the caption of the plot
;	DESIRE_GIF:	If set, then a gif image is produced automatically
;	GIFPATH:	If DESIRE_GIF is set, then this is the path to where the
;				gif image will be deposited.  Default is '.'
;	DESIRE_ZBUFFER:	If set, then the image is created in the Z-buffer.  Note this 
;				automatically sets DESIRE_GIF. Default=0
;	
; Call:
    pro pltqmecloud_flag, date, path=path, date_string=date_string, $
	  desire_gif=desire_gif, gifpath=gifpath, desire_zbuffer=desire_zbuffer
;-

  
  if(n_elements(path) eq 0) then path = '.'
  if(n_elements(desire_gif) eq 0) then desire_gif = 0
  if(n_elements(desire_zbuffer) eq 0) then desire_zbuffer = 0 else begin
    desire_zbuffer = 1
    desire_gif = 1
    set_plot,'z'
    device,set_resolution=[1200,200]
    erase
  endelse
  if((desire_gif eq 1) and (n_elements(gifpath) eq 0)) then gifpath='.'

  if(n_elements(date_string) eq 0) then $
    date_string = string(format='(I4)',date)

  pushd,path
  filename = 'qmecloud_db_' + string(format='(I4)',date) + '.cdf'
  file = findfile(filename, count=count)
  if(count ne 1) then begin
    print,'Unable to determine the file ' + filename
    popd
    return
  endif

  fid = ncdf_open(file(0))
  ncdf_varget,fid,'base_time',bt
  ncdf_varget,fid,'time_offset',to
  ncdf_varget,fid,'nonclear_flag',flag
  ncdf_close,fid
  popd

  zt2hhmmss, bt, to, ymd, hms
  hh = hhmmss2hh(hms)
  day = ymd mod 100 + hh / 24.0

  @color_syms.include

  if(!d.name eq 'X') then window,!d.window+1,xsize=1200,ysize=200

  plot, day, flag, psym=1, color=pmain, $
      xtitle='day of the month', /xstyle, yr=[-1,2], /ystyle, $
      yticklen=0.001, ytickname=[' ',' ','"clear"',' ','non clear',' ',' '], $
      title='QME Cloud Product Nonclear Flag for ' + date_string

  if(desire_gif eq 1) then begin
    gifname = gifpath + '/qmecloud_ncflag.' + date_string + '.gif'
    write_gif,gifname,tvrd(),gred,ggreen,gblue
  endif

  return
end
