; $Id: generate_lookup_reflectivity_table.pro,v 1.1 2004/08/17 21:14:47 dturner Exp $
;+
; Abstract:
; 	This routine's goal is to create a lookup table of radar reflectivity [dBZ]
;     as a function of cloud LWP [g/m2] and effective radius [um].  It saves these
;     lookup tables, which are then used by the routine lookup_reflectivity.pro
;
; Author:
;	Dave Turner
;		Pacific Northwest National Laboratory
;
; Date:
;	Aug 2004
;
; Comment: 
;	The routine should be better written as a function, but is still written
;    as a script.  Therefore, one shouldn't call it routinely, but instead use it
;    periodically to write out (new) lookup tables.  Be sure to update the routine
;    lookup_reflectivity.pro accordingly if new size distributions are added here.
;-

sd = 1			; Lognormal size distribution
sd = 0			; Gamma size distribution
sd_type = ['Gamma','Lognormal']
swidth = [0.1, 1.5]	; Width of the gamma and lognormal size distributions
temperature = 280.	; Temperature of the liquid water

reff = [findgen(37)/4. + 1, findgen(10)+11]
lwp  = [2.5,5,10,20,30,40,50,60,70,80,90,100,120,140,160,180,200,250,300,350,400, $
	450,500,550,600,650,700,750,800,850,900,950,1000]
thick = 200.		; 200 m thick cloud, to convert the LWP to LWC

sd_name = sd_type(sd) + ' with a variance (width) of ' + string(swidth(sd))
fname   = 'reflectivity_lookup_table.' + sd_type(sd) + '.' + $
	string(format='(I0,"p",I0)',fix(swidth(sd)), $
	fix(1000*(swidth(sd)-fix(swidth(sd)))+0.5))

dBZ = fltarr(n_elements(reff),n_elements(lwp))
for i=0,n_elements(reff)-1 do begin
  for j=0,n_elements(lwp)-1 do begin
    dBz(i,j) = compute_reflectivity(lwp(j)/thick, reff(i), $
    	sd, swidth(sd), temp=temperature)
  endfor
endfor

	; Show the contours of the reflectivity as a function of reff and LWC
window,!d.window+1,ys=900,xs=450
!p.multi=[0,1,3]
pchars = 1.9
zlines = [-50,-45,-40,-35,-30,-25,-20,-15,-10,-5,0]
contour,dbz,reff,lwp/thick,levels=zlines, $
	xtit='Effective Radius [um]', $
	ytit='Liquid Water Content [g m!u-3!n]', yr=[0,2], /yst, $
	chars=pchars, title='Radar Reflectivity [dBZ]', $
	c_annotation=string(format='(2x,I0,1x)', zlines), c_chars=1.2

	; Show the dependence on the LWC...
Z = findgen(51)-50
W = fltarr(n_elements(reff),n_elements(Z))
for i=0,n_elements(reff)-1 do $
  W(i,*) = interpol(lwp/thick, dbz(i,*), Z)
zlines = [0.025, 0.05, 0.1, 0.2, 0.5, 1.0, 2]
contour,W,reff,Z,levels=zlines, $
	xtit='Effective Radius [um]', ytit='Radar Reflectivity [dBZ]', $
	chars=pchars, title='Liquid Water Content [g m!u-3!n]', $
	c_annotation=string(format='(2x,F5.3,1x)', zlines), c_chars=1.2

	; Show the dependence on the reff...
R = fltarr(n_elements(lwp),n_elements(Z))
for i=0,n_elements(lwp)-1 do $
  R(i,*) = interpol(reff, dbz(*,i), Z)
zlines = [1,2,3,4,5,6,8,10,12,15,20]
contour,R,lwp/thick,Z,levels=zlines, $
	xtit='Liquid Water Content [g m!u-3!n]', xr=[0,2], /xst, $
	ytit='Radar Reflectivity [dBZ]', $
	chars=pchars, title= 'Effective Radius [um]', $
	c_annotation=string(format='(2x,I0,1x)', zlines), c_chars=1.2
xyouts, 0.90, 0.42, /nor, align=1, string(format='(A,A,F4.2)', $
	sd_type(sd),' Size Distribution with Width of ', swidth(sd))
!p.multi=0
!p.region=0

lu_reff = reff
lu_lwc  = lwp / thick
lu_dbz  = dbz
save,filename=fname,sd_name,temperature,lu_reff,lu_lwc,lu_dBZ
end
