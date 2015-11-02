function num2str,num,_extra=ex
Compile_Opt idl2
  return,strcompress(string(num,_extra=ex),/remove_all)
end

pro plotlinfit,x,y,psym=psym,intercept=intercept,slope=slope,$
  sigma=sigma,cc=cc,covar=covar,$                             
  fcolor=fcolor,flinestyle=flinestyle,fthick=fthick,linemargin=linemargin,$
  _ref_extra=ex
Compile_Opt idl2                                               
  if ~keyword_set(psym) then psym=16                          
  if ~keyword_set(fcolor) then fcolor='red'                   
  if ~keyword_set(flinestyle) then flinestyle=0
  if ~keyword_set(fthick) then fthick=!p.thick               
                                                              
  fit=linfit(x,y,sigma=sigma,/double)                         
  intercept=fit[0]                                            
  slope=fit[1]                                                
  cc=correlate(x,y,/double)                                   
  covar=correlate(x,y,/double,/covariance)                    
                                                              
  cgplot,x,y,psym=psym,_strict_extra=ex                       
  if ~keyword_set(linemargin) then linemargin=0.015            
  xfit=[!x.crange[0]+linemargin*(!x.crange[1]-!x.crange[0]),$ 
    !x.crange[1]-linemargin*(!x.crange[1]-!x.crange[0])]      
  yfit=fit[0]+fit[1]*xfit                                  
  cgplot,xfit,yfit,color=fcolor,linestyle=flinestyle,thick=fthick,/overplot
                                                              
end                                                           