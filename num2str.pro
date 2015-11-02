function num2str,num,_extra=ex
  return,strcompress(string(num,_extra=ex),/remove_all)
end