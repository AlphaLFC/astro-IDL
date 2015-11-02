
pro user_legend,xloc,yloc,items,colors,thicks=thicks,linestyles=linestyles,fcolors=fcolors,tcolors=tcolors,$
  flines=flines,fthicks=fthicks,fspaces=fspaces,forients=forients,charsizes=charsizes,dbox=dbox,vspace=vspace
  
  compile_opt idl2
  
  if ~keyword_set(dbox) then dbox=0.05
  
  x0=xloc
  y0=yloc
  dx0=dbox*(!x.CRange[1]-!x.Crange[0])
  dy0=dbox*(!y.CRange[1]-!y.Crange[0])
  
  num=n_elements(items)<4
  ;num=num>3
  
  ynum=replicate(y0,num)
  
  if ~keyword_set(vspace) then vspace=0
  for i=1, num-1 do ynum[i]=y0-2*(vspace+i)*dy0
  
  if ~keyword_set(thicks) then thicks=[5,3,3,5]
  if ~keyword_set(fcolors) then fcolors=colors
  if ~keyword_set(tcolors) then tcolors=colors
  if ~keyword_set(flines) then begin
    flines=[0,1,1,0]
    fcolors[0]='black'
    tcolors[0]='black'
  endif
  if ~keyword_set(fthicks) then fthicks=[3,3,3,3]
  if ~keyword_set(fspaces) then fspaces=[0,0.1,0.1,0]
  if ~keyword_set(forients) then forients=[0,45,-45,0]
  if ~keyword_set(linestyles) then linestyles=[0,0,0,1]
  if ~keyword_set(charsizes) then charsizes=[!P.CHARSIZE,!P.CHARSIZE,!P.CHARSIZE]
  
  xlocs=[x0,x0+dx0,x0+dx0,x0,x0]
  ylocs0=[y0,y0,y0+dy0,y0+dy0,y0]
  
  for i=1, num-1 do (scope_varfetch('ylocs'+string(i,format='(I1)'),/enter))=$
    [ynum[i],ynum[i],ynum[i]+dy0,ynum[i]+dy0,ynum[i]]
    
  if flines[0] eq 0 then cgcolorfill,xlocs,ylocs0,color=colors[0] else cgcolorfill,xlocs,ylocs0,color=colors[0],/line_fill,orientation=forients[0],thick=fthicks[0],spacing=fspaces[0]
  cgplot,xlocs,ylocs0,/overplot,thick=thicks[0],linestyle=linestyles[0],color=fcolors[0]
  cgtext,x0+1.5*dx0,y0,items[0],charsize=charsizes[0],color=tcolors[0]
  
  if num eq 2 then begin
    if flines[1] eq 0 then cgcolorfill,xlocs,ylocs1,color=colors[1] else cgcolorfill,xlocs,ylocs1,line_fill=flines[1],color=colors[1],orientation=forients[1],thick=fthicks[1],spacing=fspaces[1]
    cgplot,xlocs,ylocs1,/overplot,thick=thicks[1],color=fcolors[1],linestyle=linestyles[1]
    cgtext,x0+1.5*dx0,ynum[1],items[1],color=tcolors[1],charsize=charsizes[1]
  endif
  
  if num eq 3 then begin
    if flines[1] eq 0 then cgcolorfill,xlocs,ylocs1,color=colors[1] else cgcolorfill,xlocs,ylocs1,line_fill=flines[1],color=colors[1],orientation=forients[1],thick=fthicks[1],spacing=fspaces[1]
    cgplot,xlocs,ylocs1,/overplot,thick=thicks[1],color=fcolors[1],linestyle=linestyles[1]
    cgtext,x0+1.5*dx0,ynum[1],items[1],color=tcolors[1],charsize=charsizes[1]
    
    if flines[2] eq 0 then cgcolorfill,xlocs,ylocs2,color=colors[2] else cgcolorfill,xlocs,ylocs2,line_fill=flines[2],color=colors[2],orientation=forients[2],thick=fthicks[2],spacing=fspaces[2]
    cgplot,xlocs,ylocs2,/overplot,thick=thicks[2],color=fcolors[2],linestyle=linestyles[2]
    cgtext,x0+1.5*dx0,ynum[2],items[2],color=tcolors[2],charsize=charsizes[2]
  endif
  
  if num eq 4 then begin
    if flines[1] eq 0 then cgcolorfill,xlocs,ylocs1,color=colors[1] else cgcolorfill,xlocs,ylocs1,line_fill=flines[1],color=colors[1],orientation=forients[1],thick=fthicks[1],spacing=fspaces[1]
    cgplot,xlocs,ylocs1,/overplot,thick=thicks[1],color=fcolors[1],linestyle=linestyles[1]
    cgtext,x0+1.5*dx0,ynum[1],items[1],color=tcolors[1],charsize=charsizes[1]
    
    if flines[2] eq 0 then cgcolorfill,xlocs,ylocs2,color=colors[2] else cgcolorfill,xlocs,ylocs2,line_fill=flines[2],color=colors[2],orientation=forients[2],thick=fthicks[2],spacing=fspaces[2]
    cgplot,xlocs,ylocs2,/overplot,thick=thicks[2],color=fcolors[2],linestyle=linestyles[2]
    cgtext,x0+1.5*dx0,ynum[2],items[2],color=tcolors[2],charsize=charsizes[2]
    
    if flines[3] eq 0 then cgcolorfill,xlocs,ylocs3,color='white' else cgcolorfill,xlocs,ylocs3,line_fill=flines[3],color=colors[3],orientation=forients[3],thick=fthicks[3],spacing=fspaces[3]
    cgplot,xlocs,ylocs3,/overplot,thick=thicks[3],color=fcolors[3],linestyle=linestyles[3]
    cgtext,x0+1.5*dx0,ynum[3],items[3],color=tcolors[3],charsize=charsizes[3]
  endif
  
end