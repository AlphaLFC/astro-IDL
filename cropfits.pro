;+
; :Purpose
;  For the convenience of cropping fits, especially cube fits.
;  Dependent on idlastro package (http://idlastro.gsfc.nasa.gov/).
;  
; :Parameters
;  infile: a string of an input file name
;  
;  range1[2,3]: demical vector
;            Input as coordinate values, like RA & DEC or
;            GLON & GLAT DEMICALS or [v1,v2](km/s), supporting 
;            standard WCS coordinated with different projections.
;            OTHERWISE as integrate pixel values (from 1) if 
;            keyword /PIX is set.
;            Tips: Use function ten() to convert R.A. & DEC. form 
;            to demicals.
;
; :Keywords
;  Dim: string, 'x' or 'y','v','xy','xv','yv','xyv'
;     'x' means cropping the 1st coord only, 'y' 2nd coord only,
;     'xy' the 1st and 2nd coord, etc.. 
;     Default DIM = 'x' when only one range vector; 
;                   'xy' when only two range vectors;
;                   'xyv' when three range vectors.
;     
;  /pix
;     Use it manually when fits file doesn't support WCS.
;     
;  outfile: a string of the output file name
;     Specify a user defined output file name
;     
;     
; :Examples
;
;     
; :History
;  v1.6 2015-1-30
;   Fix a bug if crpix is not an integer value.
; 
;  v1.5 2014-12-28
;   Fix a potential bug when setting a range beyond the edge of
;   an fits file whose reference pixel is far beyond the array
;   elements. This bug occurs when using the newest version of 
;   ad2xy.pro script from the nasa idlastro package.
; 
;  v1.4 2014-12-12 & 2012-12-27
;   Support MWISP fits with GLS projection by regarding it as SFL 
;   if the version of astroidl scripts are not updated;
;   Fix a problem when cropping 2D fits.
;
;  v1.3 2014-05-07 
;   Fix a bug when input-range is beyond the fits range the 'CRPIX'
;   value in the header will be wrong.
;
;  v1.2 2014-04-29 & 2014-6-6
;   fix a bug when reading range if 
;   the version of IDL is lower than 8.0.
;
;  v1.1 2014-04-21
;   reverse 'CDELT3' if it is less than 0 and /pix is not set.
;
;  v1.0 2014-03-24
;  
; :Author
;  Facheng Li
;
; :Version: 1.6
;
; :Contact: lifc@pmo.ac.cn
;-

;;;;;compatible idlastro scripts;;;;;;;;;;;

pro adxy_old, hdr, a, d, x, y, PRINT = print, ALT = alt

 Compile_opt idl2
 On_error,2

 npar = N_params()

 if ( npar EQ 0 ) then begin
        print,'Syntax - ADXY, hdr, [a, d, x, y, /PRINT, ALT= ]'
        print,'If supplied, A and D must be in decimal DEGREES'
        return
 endif                                                                  
 
 extast, hdr, astr, noparams, ALT = alt   ;Extract astrometry from FITS header
  if ( noparams LT 0 ) then begin
        if alt EQ '' then $
        message,'ERROR - No astrometry info in supplied FITS header' $
  else  message, $
  'ERROR  - No alt=' + alt + ' astrometry info in supplied FITS header'
  endif 

 
 if npar lt 3 then begin
   RD: print,'Coordinates must be entered in either decimal (2 parameter) ' 
   print,'  or sexagesimal (6 parameter) format'
   inp = ''
   read,'ADXY: Enter coordinates: ',inp
   radec = getopt(inp,'F')
   case N_elements(radec) of 
      2: begin 
         a = radec[0] & d = radec[1]
         end
      6: begin
         a = ten(radec[0:2]*15.) & d = ten(radec[3:5])
         end
   else: begin
         print,'ADXY: ERROR - Either 2 or 6 parameters must be entered'
         return
         end
   endcase 
 endif

 case strmid( astr.ctype[0], 5,3) of
 'GSS': gsssadxy, astr, a, d, x, y       ;HST Guide star astrometry
 else:  ad2xy_old, a, d, astr, x, y          ;All other cases
 endcase

 if (npar lt 5) || keyword_set( PRINT ) then begin
        npts = N_elements(a)
        tit = strmid(astr.ctype,0,4)
         spherical = strmid(astr.ctype[0],4,1) EQ '-'
  if spherical then begin
        fmt = '(2F9.4,A,2X,2F8.2)'
        str = adstring(a,d,1)
        tit = strmid(astr.ctype,0,4)
        tit = repchr(tit,'-',' ')
        if (tit[0] EQ 'DEC ') || (tit[0] EQ 'ELAT') || $
           (tit[0] EQ 'GLAT') then tit = rotate(tit,2)
        print,'    ' + tit[0] + '    ' + tit[1] + '       ' + tit[0]  + $
              '         ' + tit[1]  + '        X       Y'
        for i = 0l, npts-1 do $
        print,FORMAT = fmt, a[i], d[i], str[i], x[i], y[i] 
        endif else begin
   unit1 = strtrim( sxpar( hdr, 'CUNIT1'+alt,count = N_unit1),2)
   if N_unit1 EQ 0 then unit1 = ''
   unit2 = strtrim( sxpar( hdr, 'CUNIT2'+alt,count = N_unit2),2)
   if N_unit2 EQ 0 then unit2 = ''
   print,'   ' + tit[0] + '     ' + tit[1] + '         X       Y'
   if (N_unit1 GT 0) || (N_unit2 GT 0) then $
       print,unit1 ,unit2,f='(t5,a,t14,a)'
       for i=0l, npts-1 do $
   print, a[i], d[i], x[i], y[i], f='(2F9.4,2X,2F8.2)'
       endelse
  endif
 
 return
 end

pro ad2xy_old, a, d, astr, x, y

 On_error,2
 compile_opt idl2

 if N_params() lT 4 then begin
        print,'Syntax -- AD2XY, a, d, astr, x, y'
        return
 endif

 radeg = 180.0D/!DPI                 ;Double precision !RADEG
 ctype = astr.ctype
 crval = astr.crval

 coord = strmid(ctype,0,4)
 reverse = ((coord[0] EQ 'DEC-') && (coord[1] EQ 'RA--')) || $
           ((coord[0] EQ 'GLAT') && (coord[1] EQ 'GLON')) || $
           ((coord[0] EQ 'ELAT') && (coord[1] EQ 'ELON'))
 if reverse then crval = rotate(crval,2)        ;Invert CRVAL?

 if (ctype[0] EQ '') then begin   
      ctype = ['RA---TAN','DEC--TAN']
      message,'No CTYPE specified - assuming TANgent projection',/INF
 endif      
     
  spherical = strmid(astr.ctype[0],4,1) EQ '-'
  if spherical then begin
  wcssph2xy, a, d, xsi, eta, CTYPE = ctype, PV2 = astr.pv2, $
        LONGPOLE = astr.longpole, CRVAL = crval, LATPOLE = astr.latpole
  endif else begin
        xsi = a - crval[0] & eta = d - crval[1]
  endelse 
  cd = astr.cd
  cdelt = astr.cdelt

  if cdelt[0] NE 1.0 then begin
         cd[0,0] *= cdelt[0] & cd[0,1] *= cdelt[0]
         cd[1,1] *= cdelt[1] & cd[1,0] *= cdelt[1]
     endif

 if reverse then begin
     temp = xsi &  xsi = eta & eta = temp
 endif

 crpix = astr.crpix - 1
 cdinv = invert(cd)
 xdif = ( cdinv[0,0]*xsi + cdinv[0,1]*eta  )
 ydif = ( cdinv[1,0]*xsi + cdinv[1,1]*eta  )

 if tag_exist(astr,'DISTORT') then begin
      if astr.distort.name EQ 'SIP' then begin
           distort  = astr.distort
           ap = distort.ap
           bp = distort.bp
           na = ((size(ap,/dimen))[0])
           xdif1 = xdif
           ydif1 = ydif
           
           for i=0,na-1 do begin
               for j=0,na-1 do begin
                  if ap[i,j] NE 0.0 then xdif1 += xdif^i*ydif^j*ap[i,j]            
                  if bp[i,j] NE 0.0 then ydif1 += xdif^i*ydif^j*bp[i,j]
           endfor
           endfor

           xdif = xdif1
           ydif = ydif1
           
      endif
 endif

 x = xdif + crpix[0] 
 y = ydif + crpix[1] 
 return
 end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro cropfits,infile,range1,range2,range3,dim=dim,pix=pix,outfile=outfile

Compile_opt idl2

if n_params() lt 2 then begin
    print, '************************************************************************'
    print, '  Syntax - CROPFITS, fitsfile, range1[, range2, range3]' 
    print, '            [,dim=''x[y,v,xy,xv,yv,xyv]'', /pix, outfile=outfile,/GLS]'
    print, '************************************************************************'
    print, '    Note - Keyword DIM = ''x'' means cropping the 1st coord only,'
    print, '            ''y'' 2nd coord only, ''xy'' the 1st and 2nd coord, etc..'
    print, '           Default DIM = ''x'' when only one range vector;'
    print, '                         ''xy'' when only two range vectors;'
    print, '                         ''xyv'' when three range vectors.'
    print, '    Note - Parameters range1[2,3] vector should be input as coordinate'
    print, '            values [x1,x2] or [y1,y2](RA & DEC or GLON & GLAT DEMICALS)'
    print, '            or [v1,v2](km/s),'
    print, '            OTHERWISE as pixel values (from 1) if keyword /PIX is set.'
    print, '    Tips - Use function ten() to convert R.A. & DEC. form to demicals.'
    print, '************************************************************************'
    return
endif

if ~file_test(infile) then begin
    print, 'ERROR: fits file does not exist!'
    return
endif

if ~keyword_set(outfile) then outfile=file_basename(infile,'.fits',/fold_case)

if n_elements(range1) lt 2 then begin
    print, 'ERROR: invalid range1!'
    return
endif else begin 
    if float(!version.release) lt 8 then range1=[range1[0],range1[n_elements(range1)-1]]$
       else range1=[range1[0],range1[-1]]
endelse

if ~keyword_set(dim) && n_params() eq 2 then dim='x'
if ~keyword_set(dim) && n_params() eq 3 then dim='xy'
if ~keyword_set(dim) && n_params() eq 4 then dim='xyv'
dim = where(strcmp(dim,['x','y','v','xy','xv','yv','xyv'],3,/fold_case))+1
if dim eq 0 then begin
    print,'ERROR: Please set DIM as ''x'' or ''y'',''v'',''xy'',''xv'',''yv'',''xyv''.'
    print, '      Default DIM = ''x'' when only one range vector;'
    print, '                    ''xy'' when only two range vectors;'
    print, '                    ''xyv'' when three range vectors.'
    return
endif

;read fits
fits_read,infile,dat,hdr
sz=size(dat)
if sz[0] lt 2 || sz[0] gt 3 then begin
  print,'ERROR: Only 2D or 3D fits files are supported.'
  return
endif

cr1=double(sxpar(hdr,'CRVAL1'))
cp1=double(sxpar(hdr,'CRPIX1'))
cr2=double(sxpar(hdr,'CRVAL2'))
cp2=double(sxpar(hdr,'CRPIX2'))

if sz[0] eq 3 then begin
  cr3=double(sxpar(hdr,'CRVAL3'))
  cp3=double(sxpar(hdr,'CRPIX3'))
  dl3=double(sxpar(hdr,'CDELT3'))
;reverse velocity resolution as a positive value
  if ~keyword_set(pix) && dl3 lt 0 then begin
    dat = reverse(dat,3)
    dl3 = abs(dl3)
    cp3 = double(sxpar(hdr,'NAXIS3'))+1-cp3
    sxaddpar,hdr,'CRPIX3',cp3
    sxaddpar,hdr,'CDELT3',dl3,format='(e14.7)'
  endif
endif

;projection GLS regarded as SFL
pos = where(strmatch(hdr,'CTYPE*GLS*'))
if pos[0] ne -1 then begin
  ntype = n_elements(pos)
  for i = 0, ntype-1 do begin
    ctype=hdr[pos[i]]
    strput,ctype,'SFL',16
    hdr[pos[i]] = ctype
  endfor
endif

;cropping
case dim of
  1:begin
    if n_params() gt 2 then begin
    print, 'Warning: only one range needs to be input.'
    print, '         range1 is set to coord.x to crop.'
    endif
      if keyword_set(pix) then begin
        x=[round(range1[0])-1,round(range1[1])-1]
        x=x[sort(x)]
      endif else begin
        adxy_old,hdr,range1,[cr2,cr2],x,y
        x=x[sort(x)]
        x=[round(x[0]),round(x[1])]
      endelse
      if x[0] lt 0 || x[1] gt sz[1]-1 then begin
        print,'Warning: Out of range!'
      endif 
      dat = dat[x[0]>0:x[1]<(sz[1]-1),*,*]
      ncp1=cp1-(x[0]>0)
      sxaddpar,hdr,'CRPIX1',ncp1
      end
      
  2:begin
    if n_params() gt 2 then begin
    print, 'Warning: only one range needs to be input.'
    print, '         range2 is set to coord.y to crop.'
    range1=range2
    endif
      if keyword_set(pix) then begin
        y=[round(range1[0])-1,round(range1[1])-1]
        y=y[sort(y)]
      endif else begin
        adxy_old,hdr,[cr1,cr1],range1,x,y
        y=y[sort(y)]
        y=[round(y[0]),round(y[1])]
      endelse
      if y[0] lt 0 || y[1] gt sz[2]-1 then begin
        print,'Warning: Out of range!'
      endif 
      dat = dat[*,y[0]>0:y[1]<(sz[2]-1),*]
      ncp2=cp2-(y[0]>0)
      sxaddpar,hdr,'CRPIX2',ncp2
      end
      
  3:begin
    if n_params() gt 2 then begin
    print, 'Warning: only one range needs to be input.'
    print, '         range3 is set to coord.v to crop.'
    range1=range3
    endif
      if keyword_set(pix) then begin
        v=[round(range1[0])-1,round(range1[1])-1]
        v=v[sort(v)]
      endif else begin
        v=(range1*1000.0-cr3)/dl3+cp3-1
        v=v[sort(v)]
        v=[round(v[0]),round(v[1])]
      endelse
      if v[0] lt 0 || v[1] gt sz[3]-1 then begin
        print,'Warning: Out of range!'
      endif 
      dat = dat[*,*,v[0]>0:v[1]<(sz[3]-1)]
      ncp3=cp3-(v[0]>0)
      sxaddpar,hdr,'CRPIX3',ncp3
      end
      
  4:begin
    if n_params() gt 3 then begin
    print, 'Warning: only two ranges need to be input.'
    print, '         range1 and range2 are set to coords.xy to crop.'
    endif
       if n_elements(range2) lt 2 then begin
       print, 'ERROR: please input range2!'
       return
       endif else begin
       if float(!version.release) lt 8 then range2=[range2[0],range2[n_elements(range2)-1]]$
          else range2=[range2[0],range2[-1]]
       endelse

      if keyword_set(pix) then begin
        x=[round(range1[0])-1,round(range1[1])-1]
        x=x[sort(x)]
        y=[round(range2[0])-1,round(range2[1])-1]
        y=y[sort(y)]
      endif else begin
        adxy_old,hdr,range1,range2,x,y
        x=x[sort(x)]
        x=[round(x[0]),round(x[1])]
        y=y[sort(y)]
        y=[round(y[0]),round(y[1])]
      endelse
      if x[0] lt 0 || x[1] gt sz[1]-1 || y[0] lt 0 || y[1] gt sz[2]-1 then begin
        print,'Warning: Out of range!'
      endif 
      dat = dat[x[0]>0:x[1]<(sz[1]-1),y[0]>0:y[1]<(sz[2]-1),*]
      ncp1=cp1-(x[0]>0)
      ncp2=cp2-(y[0]>0)
      sxaddpar,hdr,'CRPIX1',ncp1
      sxaddpar,hdr,'CRPIX2',ncp2
      end
      
  5:begin
    if n_params() gt 3 then begin
    print, 'Warning: only two ranges need to be input.'
    print, '         range1 and range3 are set to coords.xv to crop.'
    range2=range3
    endif
       if n_elements(range2) lt 2 then begin
       print, 'ERROR: please input range2!'
       return
       endif else begin
       if float(!version.release) lt 8 then range2=[range2[0],range2[n_elements(range2)-1]]$
          else range2=[range2[0],range2[-1]]
       endelse

      if keyword_set(pix) then begin
        x=[round(range1[0])-1,round(range1[1])-1]
        x=x[sort(x)]
        v=[round(range2[0])-1,round(range2[1])-1]
        v=v[sort(v)]
      endif else begin
        adxy_old,hdr,range1,[cr2,cr2],x,y
        x=x[sort(x)]
        x=[round(x[0]),round(x[1])]
        v=(range2*1000.0-cr3)/dl3+cp3-1
        v=v[sort(v)]
        v=[round(v[0]),round(v[1])]
      endelse
      if x[0] lt 0 || x[1] gt sz[1]-1 || v[0] lt 0 || v[1] gt sz[3]-1 then begin
        print,'Warning: Out of range!'
      endif 
      dat = dat[x[0]>0:x[1]<(sz[1]-1),*,v[0]>0:v[1]<(sz[3]-1)]
      ncp1=cp1-(x[0]>0)
      ncp3=cp3-(v[0]>0)
      sxaddpar,hdr,'CRPIX1',ncp1
      sxaddpar,hdr,'CRPIX3',ncp3
      end
      
  6:begin
    if n_params() gt 3 then begin
    print, 'Warning: only two ranges need to be input.'
    print, '         range2 and range3 are set to coords.xv to crop.'
    range1=range2
    range2=range3
    endif
       if n_elements(range2) lt 2 then begin
       print, 'ERROR: please input range2!'
       return
       endif else begin
       if float(!version.release) lt 8 then range2=[range2[0],range2[n_elements(range2)-1]]$
          else range2=[range2[0],range2[-1]]
       endelse

      if keyword_set(pix) then begin
        y=[round(range1[0])-1,round(range1[1])-1]
        y=y[sort(y)]
        v=[round(range2[0])-1,round(range2[1])-1]
        v=v[sort(v)]
      endif else begin
        adxy_old,hdr,[cr1,cr1],range1,x,y
        y=y[sort(y)]
        y=[round(y[0]),round(y[1])]
        v=(range2*1000.0-cr3)/dl3+cp3-1
        v=v[sort(v)]
        v=[round(v[0]),round(v[1])]
      endelse
      if y[0] lt 0 || y[1] gt sz[2]-1 || v[0] lt 0 || v[1] gt sz[3]-1 then begin
        print,'Warning: Out of range!'
      endif 
      dat = dat[*,y[0]>0:y[1]<(sz[2]-1),v[0]>0:v[1]<(sz[3]-1)]
      ncp2=cp2-(y[0]>0)
      ncp3=cp3-(v[0]>0)
      sxaddpar,hdr,'CRPIX2',ncp2
      sxaddpar,hdr,'CRPIX3',ncp3
      end
     
  7:begin
       if n_elements(range2) lt 2 then begin
       print, 'ERROR: please input range2!'
       return
       endif else begin
       if float(!version.release) lt 8 then range2=[range2[0],range2[n_elements(range2)-1]]$
          else range2=[range2[0],range2[-1]]
       endelse
       
       if n_elements(range3) lt 2 then begin
       print, 'ERROR: please input range3!'
       return
       endif else begin
       if float(!version.release) lt 8 then range3=[range3[0],range3[n_elements(range3)-1]]$
          else range3=[range3[0],range3[-1]]
       endelse
       
      if keyword_set(pix) then begin
        x=[round(range1[0])-1,round(range1[1])-1]
        x=x[sort(x)]
        y=[round(range2[0])-1,round(range2[1])-1]
        y=y[sort(y)]
        v=[round(range3[0])-1,round(range3[1])-1]
        v=v[sort(v)]
      endif else begin
        adxy_old,hdr,range1,range2,x,y
        x=x[sort(x)]
        x=[round(x[0]),round(x[1])]
        y=y[sort(y)]
        y=[round(y[0]),round(y[1])]
        v=(range3*1000.0-cr3)/dl3+cp3-1
        v=v[sort(v)]
        v=[round(v[0]),round(v[1])]
      endelse
      if x[0] lt 0 || x[1] gt sz[1]-1 || y[0] lt 0 || y[1] gt sz[2]-1 || v[0] lt 0 || v[1] gt sz[3]-1 then begin
        print,'Warning: Out of range!'
      endif 
      dat = dat[x[0]>0:x[1]<(sz[1]-1),y[0]>0:y[1]<(sz[2]-1),v[0]>0:v[1]<(sz[3]-1)]
      ncp1=cp1-(x[0]>0)
      ncp2=cp2-(y[0]>0)
      ncp3=cp3-(v[0]>0)
      sxaddpar,hdr,'CRPIX1',ncp1
      sxaddpar,hdr,'CRPIX2',ncp2
      sxaddpar,hdr,'CRPIX3',ncp3
      end
       
endcase

fits_write,outfile+'_C.fits',dat,hdr

end
