## Update data on a daila basis

library(esd)
##Parameters to display
params <- c('tmax','tmin','precip','t2m')

for (param in params) {  
  print(param)
  fname <- paste('~/OpenClimateData/data/',param,'.metnod.nc',sep='')
  x <- station(param=param,src='metnod',user='metno',nmin=30,save2file=FALSE)
  print(paste('save the data in',fname))
  write2ncdf4(x,fname)
}
