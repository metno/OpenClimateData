## Save the ECA&D station data as a netCDF4 file - ecad2nc4
## Go through the data co untry and element wise to generate several netCDF files which 
## then can be combined into one. 
## REB 2020-04-21: updated to use previousliy existing netCDF files and only read the 
## data that is needed to redice the data traffic and demands on the database. Some databases
## using JSON are slow for large data volumes.

## This is a dirty hack to try to get data from Frost. It's not easy....
## based on https://github.com/metno/rscripts/blob/master/R/Datalaster.Frost.CSV.R
##
## 2021-07-20
## Use the data from seNorge job: /lustre/storeB/project/metkl/senorge2/case/case_real_v2/README
## 2023-01-20
## Use Ketil Tunheim's code to read directly from Frost

library(esd)


print('Put metnod-data in netCDF files - updated version 2023-07-25 OpenClimateData version')
test <-FALSE; if (test) print('Test mode...')
verbose=FALSE
add.latest=TRUE # In case some of the latest data is suspect or corrupt, then set to FALSE. Used for testing purposes.  
## Generate a new file from scratch at the start of each month:
if (format(Sys.time(),'%d')=="01") new.data.file <- TRUE else new.data.file <- FALSE
new.data.file <- FALSE
active.stations <- FALSE
nsby <- 35
#fromenddate <- '1961-01-01'
fromenddate <- NULL
i30path <- 'nc30'
t1 <- Sys.time()

## Select one or several of the elements through an index
params <- c('precip','t2m','tmax','tmin','fg','fx','sd','pp','dd')[1:7] ## problems with dd & pp

if (test) {
  print('Test with test.nc')
  file.remove('test.nc')
  download.file('https://thredds.met.no/thredds/fileServer/metusers/rasmusb/precip.metnod.nc','test.nc')
  Y <- retrieve.stationsummary('test.nc')
  print(attr(Y,'period'))
  path <- './'
  params <- 'precip'
} 

## Loop through the list of parameters
for (param in params) {  
  print(param)
  if (!test) { 
    #path <- './'
    path <- '~/OpenClimateData/data/'
    testfile <- paste0(path,param,'.metnod.nc')
    if (!file.exists(testfile)) {
      print(paste('Unable to read',testfile))
      thredds.url <- 'https://thredds.met.no/thredds/fileServer/metusers/rasmusb/'
      print('Download file from thredds...')
      download.file(paste0(thredds.url,param,'.metnod.nc'),paste0(param,'.metnod.nc'))
      print(paste0('Reading from thredds and working on local ',param,'.metnod.nc'))
      print(paste('Local path:', getwd()))
      path <- './'
    }
  }
  
  ## Generate the name of the netCDF file depending on parameter:
  fname <- paste0(path,param,'.metnod.nc') 
  if (test) fname='test.nc'
  print(paste('Saving the data in',fname))
  if ( (file.exists(fname)) & !(new.data.file) ) { 
    print('updating')
    ## If the file already exists and is to be updated, then read the metadata from the netCDF file 
    ## to save computational demands, data traffic and demand on the database: 
    meta <- retrieve.stationsummary(fname)
    print(paste('retrieved metadata from',fname))
    ## Find the dates missing in the netCDF files:
    it <- c(as.character(as.Date(attr(meta,'period')[2])+1),as.character(Sys.Date()))
    is <- meta$station_id
    if (!is.null(fromenddate)) it[1] <- fromenddate
    print(it)
    ## Select the stations present in the netCDF file
    SS <- select.station(src='metnod.frost',is=meta$station.id,param=param)
    print(paste('fetch',length(meta$station.id),'stations'))
    nmin <- NULL
  } else {
    print('generate new netCDF file')
    ## Select all stations in the database from scratch
    it <- c("1950-01-01",as.character(Sys.Date()))
    #SS <- select.station(src='metnod.frost',param=param,nmin=nmin)
    nmin <- 30  ## Minimum 30 years
    meta <- select.station(src='metnod.frost',param=param)
    is <- meta$station_id
    attr(meta,'period') <- it
  }
  
  if (!is.null(meta)) {  
    if ( ((as.Date(attr(meta,'period')[2])+1) < Sys.Date()) & (diff(as.Date(it))>0) ) { 
      ## Read the actual data based on the selection criterea:
      ## Check the dates retrieved:para
      print(paste('New dates retieved',paste(it,collapse=' - ')))
      # if (param!='pp') 
      #   x <- try(station(param=param,stid=meta$station.id,src='metnod.frost',verbose=verbose,it=it)) else
      #   x <- try(station(param='slp',stid=meta$station.id,src='metnod.frost',verbose=verbose,it=it))
      
                                        #x <- seNorgeStations(param=param,it=it,meta=meta)
      if (param=="pp") param <- "slp"
      x <- station(param=param,src='metnod.frost',it=it,is=is,nmin=nmin, verbose=verbose)
      x0 <- x
      if (!is.null(x)) print('Retrieved Frost data:') else stop('Failed to read data from Frost')
      if (is.null(dim(x))) dim(x) <- c(1,length(x))
      print(range(index(x))); print(dim(x))
      
      ## If the read was successful, then save the data in the netCDF file. 
      if (!inherits(x,'try-error')) {
        print(paste('Save the data in netCDF file',fname))
        print(dim(x))
        
        if (!(new.data.file)) { 
          print('rewrite the netCDF file')
          if (!is.null(meta)) {
            it2 <- as.character(as.Date(attr(meta,'period')))
            if (!is.null(fromenddate)) it2 <- c(as.character(as.Date(attr(meta,'period')[1])),fromenddate)
          }  
          if (active.stations) it2[1] <- '1961-01-01'
          print(paste('read the old file',fname,'... time:',it2[1],'-',it2[2]))
          y <- retrieve.station(file=fname,it=it2)
          it0 <- it
          it <- !is.element(index(x),index(y))
          if (sum(it)>0) {
            print(paste('Add',sum(it),'new days from Frost API'))
            print(paste(length(intersect(stid(x),stid(y))),'common stations'))
            print(dim(y)); print(dim(x))
            ## Remove the stations read from Frost that are not in the original file.
            is0 <- is
            x <- subset(x,it=NULL,is=is.element(stid(x),stid(y)),verbose=verbose)
            is <- match(stid(x),stid(y)); is <- is[is.finite(is)]
            print(dim(x))
            nt <- sum(it); ns <- dim(y)[2]
            print(paste('Found',length(is),'stations of',ns,'and there are',nt,'new days to add'))
            if (sum(is)==0) browser()
            if (nt > 0) { 
              X <- matrix(rep(NA,nt*ns),nt,ns)
              dim(x) <- c(length(it),length(is))
              X[,is] <- x
              print('Combine old and new data')
              colnames(y) <- NULL
              z <- c(zoo(y),zoo(X,order.by=index(x)[it]))
              print(range(index(z)))
            } else {
              print('No data to add')
              add.latest <- FALSE
            }
            if (add.latest) {
              z <- attrcp(y,z); class(z) <- class(y)
            } else {
              print('<=== No new data ===>')
              z <- y
            }
          } 
        } else z <- x
        ## If active.station=TRUE, reduce the number os stations to those currently actve
        if (active.stations) {
          ## reduce the data volume
          z <- subset(z,it=c(1961,year(Sys.time())))
          n3ly <- apply(subset(z,it=c(year(Sys.time())-2,year(Sys.time()))),2,'nv')
          z <- subset(z,is=n3ly > 2*366)
          print('Reduceded selection of stations to active ones')
          print(dim(z))
        }
        print(table(src(z)))
        #plot(subset(z,is=stid(z)==18700,it=c(2010,2020)))
        ## Test to see if the path has write access - if not, use the local path
        test.writeaccess <- file.access(path, 2)
        if (test.writeaccess == -1) path <- './'
        Fname <- paste0(path,param,'.metnod.nc'); fname <-  paste0(param,'.metnod.nc')
        ## The OCDP uses 'pp' from ECA&D notation rater than 'slp' in its file name convention 
        fname <- sub('slp','pp',fname); Fname <- sub('slp','pp',Fname)
        n <- length(index(z))
        print(paste0('write2ncdf4.station: ',fname,'x dates: ',index(z)[1],' - ',index(z)[n]))
        
        ## Split data to save with write2ncdf4.station into smaller chunks of 30 stations
        ns <- dim(z)[2]
        #nctemp <- list()
       # append <- FALSE
        if (!file.exists(i30path)) dir.create(i30path) else 
          if (length(list.files(path=i30path))>0) file.remove(list.files(path=i30path,full.names = TRUE))
        for (i30 in seq(1,ns,by=nsby)) {
          z30 <- subset(z,is=i30:min(c(i30+nsby-1,ns)))
          #nctemp[[paste0(fname,i30,'nc')]] <- paste0(fname,i30,'nc')
          #write2ncdf4.station(z30,file=paste0(fname,'x'),append = append,verbose=verbose)
          #append - TRUE
          if (is.na(src(z30)[1])) attr(z30,'source') <- rep('MET Norway',length(loc(z30)))
          ci30 <- as.character(i30); while (nchar(ci30) < 4) ci30 <- paste0('0',ci30)
          print(paste('Saving stations,',i30,'to',min(c(i30+nsby-1,ns)),'in',
                      file.path(i30path,paste0(fname,'_',ci30,'.nc'))))
          if (dim(z)[2]>1)
            write2ncdf4.station(z30,file=file.path(i30path,paste0(fname,'_',ci30,'.nc')),institution='MET Norway',verbose=verbose)
        }
        print('Finished saving all station subsets into nc-files - now combine them')
        nc30files <- list.files(path=i30path,pattern=fname,full.names=TRUE)
        nc4combine.stations(nc30files,out=paste0(fname,'x'),verbose=verbose)
        file.remove(nc30files); file.remove(i30path)
        file.rename(paste0(fname,'x'),Fname)
        #file.remove(names(nctemp))
        print("test new file")
        meta.test <- retrieve.stationsummary(Fname)
        if (!is.null(meta)) print(paste('Old netCDF file:',paste(attr(meta,'period')),collapse = ' - '))
        print(paste('New netCDF file:',paste(attr(meta.test,'period')),collapse = ' - '))
      } else print('<<< File is up-to-date >>>')
      print('|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||')
      rm('z','x'); gc(reset = TRUE)
    } else if (diff(as.Date(it))>0) {
        print(paste('No new dates to add, but generate new file with',length(is),'station for',param))
        z <- station(param=param,src='metnod.frost',it=it, verbose=verbose)
        fname <- paste0(path,param,'.metnod.nc') 
        ## The OCDP uses 'pp' from ECA&D notation rater than 'slp' in its file name convention 
        fname <- sub('slp','pp',fname)
        n <- length(index(z))
        print(paste0('write2ncdf4.station: ',fname,'x dates: ',index(z)[1],' - ',index(z)[n]))
        write2ncdf4.station(z,file=paste0(fname,'x'),verbose=verbose)
        print('|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||')
    } else print(paste('Do nothing with',fname))
  }
}

t2 <- Sys.time()
print(paste('The update took',round(t2-t1),'seconds'))
gc(reset=TRUE)

