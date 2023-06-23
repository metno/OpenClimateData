It is possible to use the esd-package to retrieve GHCND data and then save them in netCDF files that can be read by the app:
```r2
## An R-script that reads African station data from GHCND and stores the different parameters in 
## different netCDF-files. The script is written with the goal of being robust and that it will
## not be tripped by bad data files. 
## CORDEX FPS Southeast Africa
## @RasmusBenestad, The Norwegian Meteorological Institute 2021-10-12
library(esd)

## R-script to read GHCND data country-wise in case of interruption 
AfricanCtrs <- c("Algeria", "Angola", "Benin", "Botswana","Burkina Faso","Burundi","Cabo Verde","Cameroon",
"Central African Republic","Chad","Comoros","Congo","Cote d'Ivoire","Djibouti","Egypt","Equatorial Guinea",
"Eritrea","Eswatini","Ethiopia","Gabon","Gambia","Ghana","Guinea","Guinea-Bissau","Kenya","Lesotho","Liberia",
"Libya","Madagascar","Malawi","Mali","Mauritania","Mauritius","Morocco","Mozambique","Namibia","Niger",
"Nigeria","Rwanda","Sao Tome and Principe","Senegal","Seychelles","Sierra Leone","Somalia","South Africa",
"South Sudan","Sudan", "Tanzania","Togo","Tunisia","Uganda","Zambia","Zimbabwe")
print('Fetch the GHCND metadata')
meta <- ghcnd.meta()

setwd('~/R')
reread <- FALSE

if ( (reread) & (file.exists('ghnd.africa.tmp.rda'))) file.remove('ghnd.africa.tmp.rda')
tmpfile <- 'ghnd.africa.tmp.rda' ## Temporary file to keep the data read so far
if (file.exists(tmpfile)) load(tmpfile) else GHCND <- list()
print(names(GHCND))

print('Fetch the GHCND data')
for (cntr in AfricanCtrs[is.na(match(AfricanCtrs,names(GHCND)))]) {
  print(cntr)
  m <- subset(meta,cntr=cntr)
  y <- try(ghcnd.station(m,verbose=FALSE))
  if (!inherits(y,'try-error')) {
    GHCND[[cntr]] <- y
    save(GHCND,file=tmpfile)
  }
}

## All the data is now read - collect the data and save as netCDF

print('Organise the GHCND data')
rain <- NULL; t2m <- NULL; tmax <- NULL; tmin <- NULL
for (i in 1:length(GHCND)) {
  if (!is.null(GHCND[[i]]$precip))
    if (!is.null(rain)) rain <- combine.stations(rain,GHCND[[i]]$precip) else
      rain <- GHCND[[i]]$precip
  if (!is.null(GHCND[[i]]$t2m))
    if (!is.null(t2m)) t2m <- combine.stations(t2m,GHCND[[i]]$t2m) else
      t2m <- GHCND[[i]]$t2m
  if (!is.null(GHCND[[i]]$tmax)) 
    if (!is.null(tmax)) tmax <- combine.stations(tmax,GHCND[[i]]$tmax) else
      tmax <- GHCND[[i]]$tmax
  if (!is.null(GHCND[[i]]$tmin)) 
    if (!is.null(tmin)) tmin <- combine.stations(tmin,GHCND[[i]]$tmin) else
      tmin <- GHCND[[i]]$tmin
}

## Save the data as netCDF-file
rain <- subset(rain,it=c(1950,2020))
rain <- subset(rain,is=list(lon=c(-50,50),lat=c(-50,50)))
attr(rain,'variable') <- 'precip'
attr(rain,'longname') <- '24hr_precipitation'
write2ncdf4(rain,file='precip.Africa-GHCND.nc')

t2m <- subset(t2m,it=c(1950,2020))
t2m <- subset(t2m,is=list(lon=c(-50,50),lat=c(-50,50)))
attr(t2m,'variable') <- 't2m'
attr(t2m,'longname') <- 'daily_mean_temperature'
write2ncdf4(t2m,file='t2m.Africa-GHCND.nc')

tmax <- subset(tmax,it=c(1950,2020))
tmax <- subset(tmax,is=list(lon=c(-50,50),lat=c(-50,50)))
attr(tmax,'variable') <- 'tmax'
attr(tmax,'longname') <- 'daily_maximum_temperature'
write2ncdf4(tmax,file='tmax.Africa-GHCND.nc')

tmin <- subset(tmin,it=c(1950,2020))
tmin <- subset(tmin,is=list(lon=c(-50,50),lat=c(-50,50)))
attr(tmin,'variable') <- 'tmin'
attr(tmin,'longname') <- 'daily_minimum_temperature'
write2ncdf4(tmin,file='tmin.Africa-GHCND.nc')
```