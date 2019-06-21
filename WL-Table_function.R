######################
### Win-Lose-TABLE ###
######################

# usefull function to label your Dataframe into "lose/win", "win/lose", "win/win", "lose/lose" 
# for non-parametric analysis of performance persistence

WL_Table = function(DF){
  x = t(apply(DF, 1, function(x) x > median(x, na.rm=TRUE))) 
  nr = nrow(x)
  dx = diff(x)
  
  lw = (dx == 1)*1
  wl = (dx == -1)*2
  dd = (dx == 0)
  ww = (dd & x[-nr,] == 1)*3
  ll = (dd & x[-nr,] == 0)*4
  
  tab = c("lose/win", "win/lose", "win/win", "lose/lose")[lw + wl + ww + ll]
  d0 = DF
  d0[-1,] = tab
  
  return(d0)
}