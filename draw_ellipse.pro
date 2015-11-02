;cgPlotS, draw_ellipse(5,5,2,1,pa=30), Color='red',NOCLIP=0;,linestyle=2

FUNCTION draw_ellipse, xcenter, ycenter, rmajor, rminor, pa=pa, n_points=n_points, points_array=points_array
  compile_opt idl2
  if ~keyword_set(n_points) then n_points=100 else n_points=round(n_points > 1)
  n_points = (2. * !PI / (n_points)) * FINDGEN(n_points+1)
  
  if n_params() eq 3 then begin
  x = xcenter + rmajor * COS(n_points)
  y = ycenter + rmajor * SIN(n_points)
  endif
  
  if n_params() eq 4 then begin  
  xx = rmajor * COS(n_points)
  yy = rminor * SIN(n_points)
  if ~keyword_set(pa) then pa=0. else pa=!pi*pa/180.
  x=cos(pa)*xx+sin(pa)*yy+xcenter
  y=sin(pa)*xx-cos(pa)*yy+ycenter
  endif
  
  if ~keyword_set(points_array) then begin
  RETURN, TRANSPOSE([[x],[y]]) 
  endif else begin
  strdata={x:x,y:y}
  return,strdata
  endelse
END