pro skyview,file=file,position=pos,survey=survey,coordinates=coord,$
projection=proj,pixels=pix,size=size,scaling=scal,catalog=cat,$
smooth=smooth,sampler=samp,resolver=res,grid=grid,$
lut=lut,contour=con,rgb=rgb,gif=returngif
if ~keyword_set(pos) || ~keyword_set(survey) then begin
  print," Keywords 'position' and 'survey' must be required!"
  print," Synax: skyview,file=file,position=pos,survey=survey,coordinates=coord,$"
  print,"          projection=proj,pixels=pix,size=size,scaling=scal,catalog=cat,$"
  print,"          smooth=smooth,sampler=samp,resolver=res,grid=grid,$"
  print,"          lut=lut,contour=con,rgb=rgb,gif=gif"
  return
endif
;cd, current=path
pos_str="'"+string(pos)+"' "
survey_str="'"+string(survey)+"' "
shell_keys=['file','Coordinates','Projection','Pixels','Size','Scaling','Catalog',$
  'Smooth','Sampler','Resolver','Grid','LUT','Contour','RGB','Return']
idl_keys=['file','coord','proj','pix','size','scal','cat','smooth','samp','res',$
  'grid','lut','con','rgb','returngif']
foreach key, idl_keys, index do begin
  if keyword_set(scope_varfetch(key,/enter)) then begin
    key_str="'"+string((scope_varfetch(key,/enter)))+"'"
    if key eq 'file' && file_test(string((scope_varfetch(key,/enter)))) then begin
        key_str="'skyview_"+string((scope_varfetch(key,/enter)))+"'"
        print, ' Target file changed to '+key_str
    endif
    shell_keys[index]=shell_keys[index]+'='+key_str+' '
  endif else begin
    shell_keys[index]=''
  endelse
endforeach
if ~file_test('/bin/skvbatch_wget') then begin
  print,' No skyviewclient installed, please refer to'
  print,' http://skyview.gsfc.nasa.gov/current/docs/batchpage.html'
  return
endif
shellcmd='skvbatch_wget position='+pos_str+'Survey='+survey_str+strjoin(shell_keys)
;print,shellcmd
;time0=systime(/seconds)
pixsz=long(strsplit(pix,',',/extract))
filesz=pixsz[0]*pixsz[-1]*4
print,' Downloading from SURVEY '+survey+'......'
spawn,shellcmd
if keyword_set(file) then begin
  fileinfo=file_info(file)
  while fileinfo.size lt filesz do begin
    print, 'Invalid file, re-downloading...'
    spawn,shellcmd
    fileinfo=file_info(file)
  endwhile
endif
;print,filesz
;print,fileinfo.size
print,' Done!'

end
