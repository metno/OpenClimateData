Once the app is installed, it can be used to present daily data and from the ECA&D data set openly available from [https://www.ecad.eu/](https://www.ecad.eu/dailydata/predefinedseries.php). The zipped data files need to be downloaded to a local directory (e.g. `~/dataECAD`as in the example below). I created subfolders with names `ECA_nonblend_cc` etc, moved the zip-file `ECA_nonblend_cc.zip` into this file, and used `unzip ECA_nonblend_cc.zip` to unpack it (and after that, I removed the zip-file). 

Once the inefficient and cumbersome ASCII files with are in place, then you can use the following R-script to save the data either as an R-binary or a netCDF file. The OpenClimateData prototype uses these netCDF files to present the data - it will work in a plug-and-play manner if they are copied into `OpenClimateData/data`subfolder.

```r2
## This R-script convert ECAD ASCII station data to netCDF
## @RasmusBenestad, 2022-02-17

## R-package for dealing with memory usage:
library(pryr)
rm(list=ls())
gc(reset=TRUE)
mem_used()
## Clear memory
mem_change(rm(list=ls()))
## The esd-package is available from https://github.com/metno/esd
library(esd)
## R-package that contains IS03 land codes:
library(rworldmap)
data(ISO03)

## Function that translates DD:MM:SS into degrees with decimals
deg2num <- function(x) {
  if (nchar(x)==9) {
    deg <- as.numeric(substr(x,2,3))
    min <- as.numeric(substr(x,5,6))
    sec <- as.numeric(substr(x,8,9))
  } else {
    deg <- as.numeric(substr(x,2,4))
    min <- as.numeric(substr(x,7,7))
    sec <- as.numeric(substr(x,9,10))
  }
  num <- deg + min/60 + sec/3600
  if (substr(x,1,1)=='-') num <- -num
  return(num)
}

## Function that attempts to deal with multicharacters
rmmultibytechars <- function(x) {
  ## https://en.wikipedia.org/wiki/ISO/IEC_8859-15
  ## UNICODE UTF-16
  #gsub('\u0022', " ", x, fixed = TRUE)
  x <- iconv(x,"latin1","UTF-8")
  return(x)
}

path <- '~/data/ECAD'
setwd('~/R')
eles <- list.files(path='/lustre/storeB/project/ECAD',pattern='ECA_nonblend_')
print(eles)

for (ele in eles) {
  ## Old file
  #staid <- read.table(paste(path,ele,'stations.txt',sep='/'),skip=17,sep=',',header=TRUE, quote='"')
  ## Read in the text and check it to remove irregularities and problem of incomplete records before
  ## reading the data as a table: "UTF-8-BOM"
  checktext <- rmmultibytechars(readLines(paste(path,ele,'sources.txt',sep='/'),encoding="latin1"))
  print(table(Encoding(checktext)))
  print(table(nchar(checktext)))
  hl <- grep('SOUID,SOUNAME',checktext)
  header <- checktext[1:(hl-1)]
  ## Drop the header
  checktext <- checktext[hl:length(checktext)]
  ## check number of columns in the files -separated by commas
  icoms <- gregexpr(',',checktext)
  ncols <- length(icoms[[1]])
  icols <- unlist(lapply(icoms,length))
  print(table(icols))
  checktext <- checktext[icols==ncols]
  ## Generate a temporary clean file of the ECA&D metadata:
  writeLines(checktext,con='meta.txt')
  souid <- read.table('meta.txt',sep=',',header=TRUE, quote='"')
  
  meta <- souid
  n <- length(meta$SOUID)
  param <- tolower(sub('ECA_nonblend_','',ele))
  ii <- 1 ## counter to keep track of number of stations saved
  lname <- switch(param,'tg'='daily_mean_temperature','tx'='Daily_maximum_tempbperature','tn'='Daily_minimum_temperature','rr'='Daily_precipitation_amount',
                  'pp'='daily_mean_sea-level_pressure', 'cc'='daily_cloud_cover','hu'='daily_humidity','sd'='daily_snow-depth','ss'='daily_sunshine-duration',
                  'qq'='global_radiation','fg'='daily_mean_wind-speed', 'fx'='daily_maximum_wind_gust','dd'='daily_wind-direction')
  unit <- switch(param,'tg'='daily_mean_temperature','tx'='Daily_maximum_tempbperature','tn'='Daily_minimum_temperature','rr'='mm/day','pp'='hPa',
                 'cc'='oktas','hu'='percentage?','sd'='cm','ss'='hours','qq'='W*m**-2','fg'='m/s','fx'='m/s','dd'='degrees')
  variable <- switch(param,'tg'='t2m','tx'='tmax','tn'='tmin','rr'='precip','pp'='slp')
  if (is.null(variable)) variable <- sub('ECA_nonblend_','',ele)
  print(param)
  fname <- paste(param,'ecad','ncx',sep='.')
  cntrs <- rownames(table(meta$CN))
  cntrs <- cntrs[nchar(cntrs)==2]
  ## Replace bad character "'" that trips up the processing
  locs <- trimws(gsub("'","",meta$SOUNAME))
  ## Tidy up - remove som bad entries without a country
  good <- (nchar(meta$CN)==2)
  meta <- meta[good,]
  print(table(meta$CN))
  #if (file.exists(fname)) file.remove(fname)
  Y <- NULL
  for (is in 1:length(meta$SOUID)) {
    stid <- as.character(meta$SOUID[is])
    while (nchar(as.character(stid)) < 6) stid <- paste0('0',stid)
    fname <- paste0(toupper(param),'_SOUID',stid,'.txt')
    filname <- paste(path,ele,fname,sep='/')
    #print(fname)
    if (file.exists(filname)) { 
      Z <- read.table(filname,skip=18, sep=',',header=TRUE)
      t <- as.Date(paste(substr(Z$DATE,1,4),substr(Z$DATE,5,6),substr(Z$DATE,7,8),sep='-'))
      x <- zoo(x=0.1*as.numeric(Z[,4]),order.by=t)
      coredata(x)[abs(coredata(x)) > 99] <- NA
      scale <- 0.1
      if (sum(is.element(param,c('cc','hu','qq','sd')))>0) scale <- 1
      x <- x*scale
      lon <- deg2num(meta$LON[is]); lat <- deg2num(meta$LAT[is]); alt <- meta$HGHT[is]
      print(paste(round(100*is/n),'% - ',fname,locs[is],ISO03$country[is.element(ISO03$alpha.2.code,meta$CN[is])],paste(range(year((x))),collapse='-'),
                  round(lon,3),round(lat,3),alt))
      y <- as.station(x,param=variable,unit=unit,loc=locs[is],lon=lon,lat=lat,alt=alt,stid=stid,
                      cntr=ISO03$country[is.element(ISO03$alpha.2.code,meta$CN[is])],longname=lname,
                      ref='Klein Tank, A.M.G. and Coauthors, 2002. Daily dataset of 20th-century surface air temperature and precipitation series for the European Climate Assessment. Int. J. of Climatol., 22, 1441-1453.',
                      info='EUROPEAN CLIMATE ASSESSMENT & DATASET (ECA&D)',
                      url='http://www.ecad.eu')
      if (is.null(Y)) Y <- y else Y <- combine.stations(Y,y)
    } else print(paste(paste(path,ele,fname,sep='/'),'does not exists'))
  }
  
  print(paste('Save the data in',paste0(variable,'.ecad.rda')))
  save(Y,file=paste0(variable,'.ecad.rda'))
  
  if (file.exists(paste0(variable,'.ecad.nc'))) file.remove(paste0(variable,'.ecad.nc'))
  Y <- subset(Y,it=c(1900,2021))
  print(paste('Save data as netCDF',format(object.size(Y),units='Mb')))
  attr(Y,'info') <- readLines('ecad2nc4.R')
  write2ncdf4(Y,file=paste0(variable,'.ecad.nc'))
  print(mem_change(rm('Y'))); rm('Y'); gc(reset=TRUE)
  
}
```