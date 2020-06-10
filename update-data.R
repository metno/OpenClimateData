## Update data on a daila basis

library(esd)
##Parameters to display
params <- c('tmax','tmin','precip','t2m','fg','fx','dd','sd')
ftp.url <- 'https://thredds.met.no/thredds/fileServer/metusers/rasmusb/'
print(paste('Fetch data from',ftp.url))

for (param in params) {  
  print(param)
  src.name <- paste(ftp.url,param,'.metnod.nc',sep='')
  dest.name  <- paste('~/OpenClimateData/data/',param,'.metnod.nc',sep='')
  download.file(src.name,dest.name)
  print(dest.name)
}
