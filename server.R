## See comments and explanations in global.R
## Rasmus Benestad


# Load the ggplot2 package which provides
# the 'mpg' dataset.

# Define a server for the Shiny app 
server <- function(input, output, session) {
  
  ## Functions ---------------------------------------------------------------------------------------------------------
  
  points <- eventReactive(input$recalc, {
    print('<01: points')
    cbind(lon(y),lat(y))
  }, ignoreNULL = FALSE)
  
  
  ## Show a popup at the given location
  showMetaPopup <- function(mapid,stid, lat, lng, ci) {
    print('<02: showMetaPopup')
    ## Make sure that only one station ID is selected and only one latitude/longitude
    if (length(stid)>1) stid <- stid[1]
    if (length(lat)>1) lat <- lat[1]
    if (length(lng)>1) lng <- lng[1]
    
    #print(paste('showMetaPopup() ===',stid,round(lat,2),round(lng,2),ci))
    Y <- updatemetadata()
    statistic <- vals()
    #print("Y <- retrieve.stationsummary")
    selLon <- round(Y$longitude[Y$station.id == stid],2)
    selLat <- round(Y$latitude[Y$station.id == stid],2)
    selAlt <- round(Y$altitude[Y$station.id == stid])
    location <- Y$location[Y$station.id == stid]
    value <- paste(input$statistic,names(ci)[ci],'=',round(statistic[Y$station.id == stid],1),collapse = ',')
    #print(c(stid,selLon,selLat,location))
    content <- paste(sep = "<br/>",
                     tags$strong(HTML(toupper(location))),
                     tags$strong(HTML(paste('LON ',selLon,'W',sep=''),
                                      paste(' LAT ',selLat,'N',sep=''),
                                      paste(' ALT ',selAlt,'m',sep=''))),
                     sprintf("Station ID: %s", as.character(stid)),
                     sprintf("%s", value),
                     sprintf("Interval: %s", paste(Y$first.year[Y$station.id == stid],Y$last.year[Y$station.id == stid],sep = '-'))
    )
    
    mapid <- mapid %>% addPopups(lng, lat, content,layerId = stid)
    return(mapid)
  }
  
  ## Events ---------------------------------------------------------------------------------------------------------
  ## Used to perform an action in response to an event. 
  ##When new data source/region is selected
  observeEvent(input$src, {
    ## Get the file names of the data
    print(paste("<03: observeEvent(input$src=",input$src,') - update {Y, varids, statistics}:'))
    Y <- updatemetadata()
    varids <- updatevarids()
    statistic <- vals()
    ci <- updateci()
    #print('varids:');print(varids); print('ci:');print(ci)
    ## For new data source reset the initial choice to precipitation
    prec <- ci[is.element(varids,'precip')]
    if (is.na(prec)) prec <- 1   ## "Safety option"
    filter <- rep(TRUE,length(statistic))
    #print(paste("ci=",paste(names(ci),'[',ci,']',sep='',collapse=", "),"prec=",prec))
    updateSelectInput(session=session,inputId="ci",choices=ci,selected=prec)
  })
  
  ## Click on the map marker - this updates the station location in the left panel
  observeEvent(input$map_marker_click,{
    print("<04: observeEvent() - click")
    fnames <- updatefilenames()
    Y <- updatemetadata()
    event <- input$map_marker_click
    #print(paste('Updated ',input$location)); print(event$id)
    selected <- which(Y$station.id == event$id)
    
    #if (input$exclude== 'Selected') filter[selected] <- FALSE
    updateSelectInput(session,inputId = 'location',label=lab.location[as.numeric(input$lingo)],
                      choices=Y$location,selected = Y$location[selected])
    #print('---click---')
  })
  
  
  ## Change language in some of the roll-down menues
  observeEvent(input$lingo, {
    print(paste("<05: observeEvent(input$lingo=",input$lingo,')'))
    #tscales <- c("day","month","season","year")
    #print(varids)
    names(tscales) <- timescales[as.numeric(input$lingo),]
    updateSelectInput(session=session,inputId="tscale",choices=tscales,selected=input$tscale)
    
    ci <- updateci()
    updateSelectInput(session=session,inputId="ci",choices=ci,selected=input$ci)
    
    names(src) <- regions[as.numeric(input$lingo),match(src,source.regions)]
    #print(src)
    updateSelectInput(session=session,inputId="src",choices=src,selected=input$src)
    
    names(stattype) <- type2name(stattype,input$lingo,types)
    #print(stattype)
    updateSelectInput(session=session,inputId="statistic",choices=stattype,selected=input$statistic)
    
    names(timespace) <- timespacenames[as.numeric(input$lingo),]
    updateSelectInput(session=session,inputId="timespace",choices=timespace,selected=input$timespace)
    #print('---lingo---')
  })
  
  ## Change the time scale for the time series box
  observeEvent(input$tscale, {
    print(paste("<06: observeEvent(input$tscale=",input$tscale,')'))
    if (input$tscale=='year') newseaTS <- seaTS[1] else
      if (input$tscale=='season') newseaTS <- seaTS[1:5] else newseaTS <- seaTS
      
    updateSelectInput(session=session,inputId="seasonTS",choices=newseaTS)
    #print('---tscale---')
  })
  
  ## Observe ---------------------------------------------------------------------------------------------------------
  ## Reactive expressions that read reactive values and call reactive expressions, and will automatically 
  ## re-execute when those dependencies change.
  
  ## Update the list of available statistics in the drop-down menues:
  observe({
    print('<07: observe - Update statistics')
    updateSelectInput(session=session,inputId="statistic",
                      choices=getstattype(updatefile(),lingo=input$lingo),selected="mean")
  })
  
  ## Update the list of locations in the drop-down menues
  observe({
    print('<08 observe - Update location')
    Y <- updatemetadata()
    loc1 <- switch(input$src,'metnod'='Oslo - blindern','ecad'='De bilt','ghcnd'=Y$location[1])
    #print(paste('New default location:',loc1))
    sel <- is.element(locations(),loc1)
    if (sum(sel)==0) print(locations()) else
      print(paste(locations()[sel][1],'from',length(locations()),'sites'))
    updateSelectInput(session=session,inputId="location", choices = locations(), selected=loc1)
  })
  
  
  ## Update the slider for range of values shown on the map
  observe({
    print('<09: observe - update slider')
    statistic <- vals()
    if (max(abs(statistic),na.rm=TRUE)>10) digits <- 0 else
      if (max(abs(statistic),na.rm=TRUE)>1) digits <- 1 else
        digits <- 2
    if (tolower(substr(input$statistic,1,5)) != 'trend') {
      statisticmin <- round(min(statistic,na.rm=TRUE),digits)
      statisticmax <- round(max(statistic,na.rm=TRUE),digits)
      start1 <- round(quantile(statistic,probs = 0.005,na.rm=TRUE, names=FALSE),digits)
      start2 <- round(quantile(statistic,probs = 0.995,na.rm=TRUE, names=FALSE),digits)
    } else {
      statisticmax <-  round(max(abs(statistic),na.rm=TRUE),digits)
      statisticmin <- -statisticmax
      start2 <- round(quantile(abs(statistic),0.975,na.rm=TRUE),digits)
      start1 <- -start2
    }
    
    step <- max(c(min(c(1,round((start2 - start1)/100,1))),0.1))
    
    #print(statistic)
    #print(paste('Slider max & min= [',statisticmin,', ',statisticmax,'] n=',length(statistic),
    #            ' default:',start1,start2,' input$ci=',input$ci,sep=''))
    updateSliderInput(session=session,inputId="statisticrange",value = c(start1,start2),
                      min=statisticmin,max=statisticmax,step=step)
  })
  
  # Update the list of aspects in the past weather box:
  observe({
    print(paste('<10: observe - Update aspects: input$ci=',input$ci,'input$lingo=',input$lingo))
    varids <- updatevarids()
    ii <- as.numeric(input$ci)
    ## The next lines is  fudge to avoid crash if input$ci is not properly updated
    if (ii > length(varids)) ii <- (1:length(varid))[is.element(varids,'precip')]
    if (!is.finite(ii)) ii <- 1
    if (varids[ii]=='precip') {
      aspects <- aspectsP
      names(aspects) <- aspectnameP[as.numeric(input$lingo),]
    } else {
      aspects  <- aspectsT
      names(aspects) <- aspectnameT[as.numeric(input$lingo),]
    }
    #print(aspects)
    updateSelectInput(session=session,inputId="aspect",choices=aspects,selected=aspects[1])
  })
  
  ## Updates the countries in thedrop-down list
  observe({
    print('<11: observe - Update country list')
    Y <- updatemetadata()
    #print(table(Y$country)); print(names(Y))
    updateSelectInput(session=session,inputId="country",choices=c('All',rownames(table(Y$country))),
                      selected='All')
  })
  
  
  
  ## Reactive expressions to update information -------------------------------------------------------------- 
  
  ## The following expression extracts the variable IDs from the files found in the data folder: 
  ## NB! This has to come before the subsequent expressions to avoid runing code several times.
  updatevarids <- reactive({
    print('<12: reactive - updatevarids()')
    fnames <- list.files(path='data',pattern='.nc',full.names = TRUE)
    fnames <- fnames[grep('.nc',fnames,fixed=TRUE)]
    #print(fnames); print(src); print(input$src)
    fnames <- fnames[grep(src[match(input$src,src)],fnames)]
    varids <- substr(fnames,6,nchar(fnames))
    varids <- substr(varids,1,regexpr('.',varids,fixed=TRUE)-1)
    names(varids) <- vari2name(varids,names=varnames[as.numeric(input$lingo),])
    return(varids)
  })
  
  ## The following reactive expressions get updated file information and metadata
  updatefilenames <- reactive({
    print('<13: reactive - updatefilenames()')
    fnames <- list.files(path='data',pattern='.nc',full.names = TRUE)
    fnames <- fnames[grep('.nc',fnames,fixed=TRUE)]
    fnames <- fnames[grep(src[match(input$src,src)],fnames)]
    #print(fnames) #; print(src); print(input$src)
    return(fnames)
  })
  
  ## The following expression utdaptes file names in the list of available names, given the region.
  updatefile <- reactive({
    print('<14: reactive - updatefile()')
    fnames <- updatefilenames()
    ## The climate indicators (files) are not listed in the same order since the
    ## list is 'translated' into a more reader-friendly list
    varids <- updatevarids()
    ii <- as.numeric(input$ci)
    ## If the climate index is not updated, try to chose the right climate index
    if (ii > length(fnames)) {
      #print('ii is outside range of index')
      varids <- updatevarids()
      ii <- (1:length(varids))[is.element(varids,'precip')]
    }
    if (!is.finite(ii)) ii <- 1
    #print(paste('Selected file number',ii,': ',fnames[ii]))
    return(fnames[ii])
  })
  
  getstid <- reactive({
    print('<15: reactive - getstid()')
    #fnames <- updatefilenames()
    #print(paste('vals: Y$',input$statistic,sep='')); print(fnames); print(input$ci)
    ## Get summary data from the netCDF file
    iss <- (1:length(locations()))[is.element(toupper(locations()),'DE BILT')]
    if (length(iss) > 0) is <- iss else
      iss <- (1:length(locations()))[is.element(toupper(locations()),'OSLO BLIND')]
    if (length(iss) > 0) is <- iss else is <- 1
    return(Y$station.id[is])
  })
  
  updatemetadata <- reactive({
    print('<16: reactive - updatemetadata()')
    Y <- retrieve.stationsummary(updatefile())
    ok <- is.finite(Y$lon) & is.finite(Y$lat)
    Y <- Y[ok,]
    #print(paste('Retrieved from ',updatefile(),'with',dim(Y)[1],'locations and',dim(Y)[2],'elements:',
    #            paste(names(Y),collapse=', ')))
    return(Y)
  })
  
  updateci <- reactive({
    print('<17: reactive - updateci()')
    varids <- updatevarids()
    ci <- 1:length(varids); names(ci) <- names(varids)
    return(ci)
  })
  
  ## update the selected station series 
  updatestation <- reactive({
    print('<18: reactive - updatestation()')
    Y <- updatemetadata()
    Y <- retrieve.stationsummary(updatefile())
    il <- is.element(tolower(Y$location),tolower(input$location))
    if (sum(il)>0) selectedStid <- Y$station.id[il][1] else {
      print(input$location);print('Something is wrong!')
      selectedStid <- getstid()
    }
    print(paste('selectedID: ',selectedStid,' = ',input$location,'from',updatefile(),
                "  - y <- retrieve.station(",updatefile(),selectedStid,")"))
    y <- retrieve.station(updatefile(),stid=selectedStid,verbose=verbose)
    ## Update slider
    updateSliderInput(session=session,inputId="itt",value = range(year(y)),
                      min=min(year(y)),max=max(year(y)))
    return(y)
  })
  
  
  
  updatetimeseries <- reactive({
    print('<19: reactive - updatetimeseries()')
    y <- updatestation()
    x0 <- as.numeric(input$x0)
    ## If anomaly
    if (input$aspect=='anomaly') y <- anomaly(y)
    if (input$seasonTS != 'all') y <- subset(y,it=tolower(input$seasonTS))
    
    ## If the data is precipitation
    if (is.precip(y)) {
      if (input$aspect=='wetmean') FUN<-'wetmean' else
        if (input$aspect=='wetfreq') FUN<-'wetfreq' else
          if (input$aspect=="Number_of_days") {
            y <- test.rainequation(y,x0=x0,plot=FALSE)
            return(y)
          } else FUN<- 'sum'
    } else if ((input$aspect=="Number_of_days") | (input$aspect=="Days_above_normal")) {
      if (input$aspect=="Days_above_normal") {
        x0 <- 0
        y <- anomaly(y)
      }
      print('Number of days and not precipitation')
      meanx <- switch(input$tscale,
                      'month'=as.monthly(y,FUN='mean',nmin=25),
                      'season'=as.4seasons(y,FUN='mean',nmin=80),
                      'year'=as.annual(y,FUN='mean',nmin=300))
      sdx <- switch(input$tscale,
                    'month'=as.monthly(y,FUN='sd',nmin=25),
                    'season'=as.4seasons(y,FUN='sd',nmin=80),
                    'year'=as.annual(y,FUN='sd',nmin=300))
      y <- switch(input$tscale,
                  'month'=as.monthly(y,FUN='count',threshold=x0,nmin=25),
                  'season'=as.4seasons(y,FUN='count',threshold=x0,nmin=80),
                  'year'=as.annual(y,FUN='count',threshold=x0,nmin=300))
      nds <- switch(input$tscale,
                    'month'=30,
                    'season'=90,
                    'year'=365.25)
      print('Probability')
      pr <- 1 - pnorm(x0,mean=meanx,sd=sdx)
      return(merge(pr,y/nds))
    } else FUN<-'mean'
    #if (is.null(FUN)) FUN='mean'
    
    ## Time series
    #if ( (input$tscale != 'day') & (input$timespace != 'Data_matrix') ) {
    #  print(paste('Use',FUN,'to aggregate the time series. input$tscale=',input$tscale))
    #  print(c(aspects,input$aspect)); print(x0); print(esd::unit(y))
    #}
    
    if (input$timespace != 'Data_matrix') { 
      if (FUN != 'count')  
        y <- switch(input$tscale,
                    'day'=y,'month'=as.monthly(y,FUN=FUN),
                    'season'=as.4seasons(y,FUN=FUN),'year'=as.annual(y,FUN=FUN,nmin=300)) else
                      y <- switch(input$tscale,
                                  'day'=y,'month'=as.monthly(y,FUN=FUN,threshold=x0,nmin=25),
                                  'season'=as.4seasons(y,FUN=FUN,threshold=x0,nmin=80),
                                  'year'=as.annual(y,FUN=FUN,threshold=x0,nmin=300))
    }
    #if (is.T(y)) browser()
    
    if ( (input$aspect=='wetfreq') & (input$tscale != 'day') ) {
      y <- 100*y
      attr(y,'unit') <- '%'
    }
    y <- subset(y,it=c(input$itt[1],input$itt[2]))
    return(y)
  })
  
  locations <- reactive({
    Y <- updatemetadata()
    return(Y$location)
  })
  
  ## The following are more general ractive expressions
  zoom <- reactive({
    print('<20: reactive - zoom()')
    zoomscale <- switch(input$src,
                        'metnod'=6,'ecad'=4,'Asia'=4,'Pacific'=4,'LatinAmerica'=4,'Africa'=3,'USA'=3,'Australia'=4,
                        'INAM'=6,'CLARIS'=6)
    return(zoomscale)
  })
  
  # Computing indices - values used in the map
  vals <- reactive({
    print('<21: reactive - vals()')
    
    ## Get summary data from the netCDF file
    Y <-updatemetadata()  
    x0 <- as.numeric(input$x0)
    #print(summary(Y))
    showseason <- switch(input$season,
                         'all'='','DJF'='_DJF','MAM'='_MAM','JJA'='_JJA','SON'='_SON')
    #print(paste('Y$',input$statistic,showseason,sep=''))
    if (tolower(input$statistic)=='10.year.return.value') Z <- -Y$wetmean*log(1/(Y$wetfreq*36.525))*1.4 - 3.82 else 
      if ( (tolower(input$statistic)!='number_of_days') &
           (tolower(input$statistic)!='days_above_normal') &
           (tolower(input$statistic)!='specific_day') ) {
        #print('Get the summary statistics from the netCDF file'); print(tolower(input$statistic))
        Z <- eval(parse(text=paste('Y$',input$statistic,showseason,sep=''))) 
      } else {
        if (tolower(input$statistic)=='number_of_days') {
          #print('Number_of_days'); print(paste('threshold x0=',x0))
          
          if (!is.null(Y$wetfreq)) Z <- switch(input$season,
                                               'all'=3.6525*Y$wetfreq*exp(-x0/Y$wetmean),
                                               'DJF'=0.90*Y$wetfreq_DJF*exp(-x0/Y$wetmean_DJF),
                                               'MAM'=0.90*Y$wetfreq_MAM*exp(-x0/Y$wetmean_MAM),
                                               'JJA'=0.90*Y$wetfreq_JJA*exp(-x0/Y$wetmean_JJA),
                                               'SON'=0.90*Y$wetfreq_SON*exp(-x0/Y$wetmean_SON)) else
                                                 Z <- switch(input$season,
                                                             'all'=365.25*(1-pnorm(x0,mean=0,sd=Y$sd)),
                                                             'DJF'=90*(1-pnorm(x0,mean=Y$mean_DJF,sd=Y$sd_DJF)),
                                                             'MAM'=90*(1-pnorm(x0,mean=Y$mean_MAM,sd=Y$sd_MAM)),
                                                             'JJA'=90*(1-pnorm(x0,mean=Y$mean_JJA,sd=Y$sd_JJA)),
                                                             'SON'=90*(1-pnorm(x0,mean=Y$mean_SON,sd=Y$sd_SON))) 
          Z <- as.numeric(Z)
        } else {
          ## Get a specific date it
          it <- input$it
          ## Make sure the date is within the range of valid dates in the netCDF file 
          #print("reactive vals - specific date"); print(it); print(attr(Y,'period'))
          if (as.Date(it) < as.Date(attr(Y,'period')[1])) it <- attr(Y,'period')[1]
          if (as.Date(it) > as.Date(attr(Y,'period')[2])) it <- attr(Y,'period')[2]
          #print("Read a specific date from the netCDF file"); print(it); print(updatefile())
          x <- retrieve.station(updatefile(),it=it,verbose=verbose)
          Z <- c(coredata(x))
          dim(Z) <- c(length(Z),1)
          ## The stations are sorted according to alphabetic order
          Z <- Z[match(Y$station.id,stid(x))]
          #print(cbind(stid(x),Y$station.id,stid(x)[match(Y$station.id,stid(x))])[1:10,])
          #print(summary(Z))
          #print('...')
        }
      } 
    if (input$statistic=='number.valid') Z <- eval(parse(text=paste('Y$',input$statistic,sep='')))/365.25
    if (input$statistic=='records') Z <- 100*Z
    if (input$statistic=='lows') Z <- 100*Z
    if (tolower(input$statistic)=='sigma2') Z[Z > 20000] <- NA
    Z[Z <= -99] <- NA
    Z <- round(Z,3)
    #print(paste('+++ Values returned by vals(): n=',length(Z),'range= [',min(Z,na.rm=TRUE),max(Z,na.rm=TRUE),'] +++'))
    return(Z) 
  })
  
  legendtitle <- reactive({
    print('<22: reactive - legendtitle()')
    Y <- updatemetadata()
    #print('legendtitle'); print(stattype); print(input$statistic); print(input$lingo)
    #title <- type2name(input$statistic,input$lingo,stattype)
    #if ( (length(grep(attr(Y,'unit')$value,title))==0) & (length(grep('%',title))==0) )
    #  title <- paste(title,' (',attr(Y,'unit')$value,')',sep='')
    title <- attr(Y,'unit')
    if (length(grep(tolower('trend'),tolower(input$statistic)))>0) title <- paste(title,decade[as.numeric(input$lingo)],sep='/')
    if ( (length(grep(tolower('records'),tolower(input$statistic)))>0) | 
         (length(grep(tolower('wetfreq'),tolower(input$statistic)))>0) ) title <- '%'
    if (length(grep(tolower('altitude'),tolower(input$statistic)))>0) title <- 'm'
    if ( (length(grep(tolower('longitude'),tolower(input$statistic)))>0) | 
         (length(grep(tolower('latitude'),tolower(input$statistic)))>0) ) title <- degree[as.numeric(input$lingo)]
    if ( (length(grep(tolower('first.year'),tolower(input$statistic)))>0) | 
         (length(grep(tolower('last.year'),tolower(input$statistic)))>0) ) title <- yr[as.numeric(input$lingo)]
    if (length(grep(tolower('valid'),tolower(input$statistic)))>0) title <- yrs[as.numeric(input$lingo)]
    if (sum(is.element(tolower(input$statistic), c('lastrains','mean_wetdur','mean_drydur','number_of_days')))>0) 
      title <- days[as.numeric(input$lingo)]
    if (input$statistic=='Specific_day') title=paste(input$it,' (',attr(Y,'unit'),')',sep='')
    if ( (length(grep(tolower('mean'),tolower(input$statistic)))>0) & 
         (length(grep(tolower('day'),tolower(title)))>0) ) title <- sub('/day','',title)
    if (length(grep(tolower('sigma2'),tolower(input$statistic)))>0) title <- 'mm^2'
    #print(paste('legend title',title))
    return(title)
  })
  
  ## Output rendering ------------------------------------------------------------------------------------------------
  
  observe({
    print('<23: observe - marker click')
    
    mapid2 <- leafletProxy("mapid") %>% clearPopups()
    event <- input$map_marker_click
    #print('Data Explorer from map'); print(event$id)
    if (is.null(event))
      return()
    
    #print('Click --->'); print(event); print('<--- Click')
    isolate({
      mapid <- showMetaPopup(mapid2,stid=event$id,lat=event$lat, lng = event$lng,ci = as.numeric(input$ci))
    })
    
    #removeMarker("map",layerId = event$id)
    leafletProxy("mapid",data = event) %>%
      addCircles(lng = event$lng, lat = event$lat,color = 'red',layerId = 'selectID', weight = 12)
    
    #selectedStid <- event$id
  })
  
  ## The map panel 
  output$map <- renderLeaflet({
    print('<24: output$map - render')
    Y <- updatemetadata()
    vids <- updatevarids()
    statistic <- vals()
    
    #print('Stastistic shown on map');print(summary(statistic))
    
    if (input$country=='All') filter <- rep(TRUE,length(statistic)) else {
      filter <- rep(FALSE,length(statistic))
      filter[(Y$country == input$country)] <- TRUE
    }
    
    #print('        <<< input$ci is not updated!!! >>>              ')
    #isolate({print(paste('Range shown in map',input$statisticrange[1],'-',input$statisticrange[1],' ci=',input$ci))})
    filter[statistic < input$statisticrange[1]] <- FALSE
    filter[statistic > input$statisticrange[2]] <- FALSE
    
    highlight <- NULL
    lhighlight <- paste0('#',1:10)
    if (tolower(input$highlight) == "top 10") 
      highlight <- order(statistic[filter],decreasing=TRUE)[1:10] else 
        if (tolower(input$highlight) == "low 10") 
          highlight <- order(statistic[filter],decreasing=FALSE)[1:10] else 
            if (tolower(input$highlight) == "new records") {
              #print('--- Higlight new records ---')
              ## For a specific day, check against the maximum or minimum
              if (tolower(input$statistic)!='specific_day') {
                x <- retrieve.station(updatefile(),it=attr(Y,'period')[2],verbose=verbose)
                Z <- c(coredata(x))
                dim(Z) <- c(length(Z),1)
                ## The stations are sorted according to alphabetic order
                Z <- Z[match(Y$station.id,stid(x))]
              } else Z <- statistic
              #print(paste('Minimum?',is.null(Y$min)))
              if (!is.null(Y$wetmean)) 
                highlight <- (Z[filter] - Y$max[filter] > -0.001) & is.finite(Z[filter]) else 
                  highlight <- ((Z[filter] - Y$max[filter] > -0.001) | 
                                  (Z[filter] - Y$min[filter] < 0.001)) & is.finite(Z[filter])
              #print('RECORDS?')
              highlight[is.na(highlight)] <- FALSE
              #print(paste('Number of records on',input$it,'is',sum(highlight,na.rm=TRUE)))
              #print(paste(Y$location[filter][highlight],Z[filter][highlight]))
              #print(table(highlight)); print(table(filter))
              lhighlight <- rep('Record',sum(highlight,nn.rm=TRUE))
            } 
    
    lon.highlight <- Y$longitude[filter][highlight]
    lat.highlight <- Y$latitude[filter][highlight]
    #if (tolower(input$highlight) != "none") {
    #  print(paste('Highlight',input$highlight))
    #  print(statistic[filter][highlight]); print(lon.highlight); print(lat.highlight)
    #}
    if (sum(filter)==0) {
      #print(paste(input$ci,esd::varid(y),min(statistic),max(statistic),input$statisticrange[1],input$statisticrange[2]))
      filter <- rep(TRUE,length(statistic))  
    }
    if (sum(is.element(vids[as.numeric(input$ci)],c('precip','sd')))>0) reverse <- TRUE else reverse <- FALSE
    if ((input$statistic=="lastrains") | (input$statistic=="mean_drydur")) reverse <- FALSE
    if((input$statistic=="trend_wetfreq") | (input$statistic=="trend_wetmean") |
       (input$statistic=="wetfreq") | (input$statistic=="wetmean")) reverse <- TRUE
    #print(paste('Reverse palette =',reverse)); print(summary(statistic))
    #print(c(sum(filter),length(filter),length(statistic)))
    pal <- colorBin(colscal(pal = 't2m',n=10),
                    seq(input$statisticrange[1],input$statisticrange[2],length=10),bins = 10,pretty = TRUE,reverse=reverse)    
    
    ## Only show good data
    good <- is.finite(Y$longitude) & is.finite(Y$latitude)
    Y <- Y[good,]; statistic <- statistic[good]; filter <- filter[good]
    
    ## Mark the selected location
    is <- which(tolower(Y$location[filter]) == tolower(input$location))[1]
    if ( (length(is)==0) | is.na(is) ) is <- 1
    radius <- rep(input$rad,length(statistic[filter]))
    radius[!is.finite(statistic[filter])] <- 1
    
    # print(paste('The map is being rendered:','Number of locations shown=',sum(filter),'with',sum(!is.finite(statistic)),
    #             'bad points - range of values= [',min(statistic,na.rm=TRUE),max(statistic,na.rm=TRUE),'] - slider:',
    #             input$statisticrange[1],'-',input$statisticrange[2],' ci=',input$ci,'is=',is))
    # print(summary(statistic)); print(summary(Y$longitude)); print(summary(Y$latitude))
      print(paste('Filter: is=',is,'l=',length(filter),'s=',sum(filter),'ID=',Y$station.id[filter][is],
                Y$longitude[filter][is],Y$latitude[filter][is],Y$location[filter][is]))
    
    leaflet("mapid") %>% 
      addCircleMarkers(lng = Y$longitude[filter], # longitude
                       lat = Y$latitude[filter],fill = TRUE, # latitude
                       label = paste(Y$location[filter],as.character(round(statistic[filter],digits = 2))),
                       labelOptions = labelOptions(direction = "right",textsize = "12px",opacity=0.6),
                       popup = Y$location[filter],popupOptions(keepInView = TRUE),
                       radius =radius,stroke=TRUE,weight = 1, color='black',
                       layerId = Y$station.id[filter],
                       fillOpacity = 0.4,fillColor=pal(statistic[filter])) %>% 
      addCircleMarkers(lng = Y$longitude[filter][is], lat = Y$latitude[filter][is],fill=TRUE,
                       label = paste(Y$location[filter][is],as.character(round(statistic[filter][is],digits = 2))),
                       labelOptions = labelOptions(direction = "right",textsize = "12px",opacity=0.6),
                       radius=6,stroke=TRUE, weight=3, color='green',
                       fillOpacity = 0.5,fillColor = pal(statistic[filter])[is],
                       layerId = Y$station.id[filter][is]) %>%
      addCircleMarkers(lng = lon.highlight, lat = lat.highlight,fill=TRUE,
                       label=paste0(lhighlight,': ',Y$location[filter][highlight],' - ',
                                    as.character(round(statistic[filter][highlight],digits = 2))),
                       labelOptions = labelOptions(direction = "right",textsize = "12px",opacity=0.6),
                       radius=6,stroke=TRUE, weight=3, color='black',
                       layerId = Y$station.id[filter][highlight],
                       fillOpacity = 0.5,fillColor=pal(statistic[filter])[highlight]) %>%
      addLegend("bottomright", pal=pal, values=round(statistic[filter], digits = 2), 
                title=legendtitle(),
                layerId="colorLegend",labFormat = labelFormat(big.mark = "")) %>%
      addProviderTiles(providers$Esri.WorldStreetMap,
                       #addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = FALSE)) %>% 
      setView(lat=Y$latitude[filter][is],lng = Y$longitude[filter][is], zoom = zoom())
  })
  
  output$plotstation <- renderPlotly({
    print('<25: output$plotstation - render')
    # print(paste('Time series for',input$location,'ci=',input$ci,'season=',input$season,
    #             'tscale=',input$tscale,'aspect=',input$aspect))
    y <- updatetimeseries()
    #print(summary(coredata(y)))
    #if (is.precip(y)) thresholds <- seq(10,50,by=10) else thresholds <- seq(-30,30,by=5)
    
    ## Marking the top and low 10 points
    #print('10 highs and lows')
    if (tolower(input$highlightTS)=='top 10') 
      highlight10 <- y[order(coredata(y),decreasing=TRUE)[1:10]] else
        if (tolower(input$highlightTS)=='low 10') 
          highlight10 <- y[order(coredata(y),decreasing=FALSE)[1:10]] else
            if (tolower(input$highlightTS)=='new records') {
              #print('new records')
              dim(y) <- NULL
              recs <- records(y)
              #print(recs)
              highlight10 <- y[attr(recs,'t')]
            } else
              highlight10 <- y[1:10]+NA
    #print(highlight10)
    
    withProgress(message = 'Updating ...',
                 detail = 'This may take a while...', value = 0,
                 { for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.07)}
                 })
    
    #print('The timeseries is being rendered')
    if ( (!is.null(dim(y))) & ((input$aspect=="Number_of_days")  | (input$aspect=="Days_above_normal"))  ){
      ndps <- switch(input$tscale,
                     'year'=365.25,"day"=1,"month"=30,"season"=90)
      timeseries <- data.frame(date=index(y),pr=ndps*coredata(y[,1]),obs= ndps*coredata(y[,2]),
                               trend=coredata(trend(ndps*y[,2])))
      #print(summary(timeseries))
      TS <- plot_ly(timeseries,x=~date,y=~obs,type = 'scatter',mode='lines',name='observed',color=rgb(0,0,0))
      TS <- TS %>% add_trace(y=~trend,name='trend',color=rgb(1,0,0)) %>% 
        add_trace(y=~pr,name="predicted",color=rgb(0,0,1,0.5)) %>%
        layout(title=loc(y),yaxis = list(title='days/year'))
    } else {
      timeseries <- data.frame(date=index(y),y=coredata(y),trend=coredata(trend(y)))
      #print(summary(timeseries))
      TS <- plot_ly(timeseries,x=~date,y=~y,type = 'scatter',mode='lines',name='data')
      if (tolower(input$highlightTS)=='none') { 
        TS = TS %>% add_trace(y=~trend,name='trend') %>% 
          layout(title=loc(y),yaxis = list(title=esd::unit(y)))
      } else {
        TS = TS %>% add_trace(y=~trend,name='trend') %>% 
          add_markers(x=index(highlight10),y=coredata(highlight10),hoveron=input$highlightTS) %>% 
          layout(title=loc(y),yaxis = list(title=esd::unit(y)))
      }
    }
    #TS$elementID <- NULL
    
  })
  
  output$histstation <- renderPlotly({
    print('<26: output$histstation - render')
    #print(paste('Time series for',input$location,'ci=',input$ci,'season=',input$season,
    #            'tscale=',input$tscale,'aspect=',input$aspect))
    ## Get summary data from the netCDF file
    Y <- updatemetadata()
    statistic <- vals()
    
    ## Apply filter to highlight stations selected in the map
    if (input$country=='All') filter <- rep(TRUE,length(statistic)) else {
      filter <- rep(FALSE,length(statistic))
      filter[(Y$country == input$country)] <- TRUE
    }
    #print('        <<< input$ci is not updated!!! >>>              ')
    #isolate({print(paste('Range shown in map',input$statisticrange[1],'-',input$statisticrange[1],' ci=',input$ci))})
    filter[statistic < input$statisticrange[1]] <- FALSE
    filter[statistic > input$statisticrange[2]] <- FALSE
    statistic <- statistic[filter]
    
    y0 <- updatestation()
    y <- updatetimeseries()
    
    #print(input$timespace)
    if (input$timespace == 'Histogram_location') yH <- coredata(y) else yH <- statistic
    
    withProgress(message = 'Updating ...',
                 detail = 'This may take a while...', value = 0,
                 { for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.05)
                 }
                 })
    #mx <- ceiling(1.1*max(abs(y),na.rm=TRUE))
    #print('histstation'); print(summary(yH))
    if (input$timespace=='Data_matrix') {
      #print('Plot the data matrix - vis(y)')
      z <- vis(y,plot=FALSE)
      p <- plot_ly(x=attr(z,'x'),y=attr(z,'y'), z = t(z), type = "heatmap")
    } else if (substr(input$timespace,1,12) != 'Annual_cycle') {
      fit <- density(yH[is.finite(yH)])
      #breaks <- seq(floor(min(yH,na.rm=TRUE)),ceiling(max(yH,na.rm=TRUE)),length=100)
      pdf <- dnorm(fit$y,mean=mean(yH,na.rm=TRUE), sd = sd(yH,na.rm=TRUE))
      dist <- data.frame(y=coredata(yH))
      #print(summary(dist))
      #syH <- round(summary(yH)[c(1,4,6)],1)
      syH <- summary(yH)[c(1,4,6)]
      #print(syH); print(class(syH))
      if (input$timespace=='Histogram_map') 
        title <- paste(input$statistic,': ',paste(names(syH),round(syH,1),collapse=', ',sep='='),sep='') else
          title <- paste(loc(y),': ',paste(syH,collapse=', ',sep='='),sep='')
      #print(title)
      H <- plot_ly(data=dist,x=~y,name='data',type='histogram',histnorm='probability')
      H = H %>% #add_trace(y=fit$x,x=pdf,name='pdf',mode='lines') %>% 
        #add_trace(x = fit$x, y = fit$y, mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>%
        layout(xaxis=list(title=esd::unit(y)),yaxis=list(title='probability density'))
      #H = H %>% layout(title=title)
    } else {
      y <- y0 # subset(y0,it=input$dateRange)
      dim(y) <- NULL
      if (is.precip(y)) {
        FUN <- 'sum'
        ylab='mm/month'
      } else {
        FUN <- 'mean'
        ylab <- 'deg C'
      }
      title <- loc(y)
      if (input$timespace=='Annual_cycle_month') {
        mac <- data.frame(y=as.monthly(y,FUN=FUN)) 
        mac$Month <- month(as.monthly(y))
        mac$Year <- year(as.monthly(y))
        nv <- as.monthly(y,FUN='nv')
        mac$y[nv < 28] <- NA
        yrnow <- format(Sys.time(),'%Y')
        it <- is.element(mac$Year,as.numeric(yrnow))
        print(c(length(it),sum(it)))
        type='box'
        AC <- plot_ly(mac,x=~Month,y=~y,name='mean_annual_cycle',type=type)  %>% 
          layout(yaxis=list(title=esd::unit(y)),xaxis=list(title='Calendar month'))
        ## REB add the last month
        lyr <- data.frame(y=mac$y[it],Month=mac$Month[it])
        print(lyr)
        AC <- AC %>% add_trace(AC,data=lyr,x=~Month,y=~y,name=yrnow,type='scatter',size=10)
        clim6190 <- aggregate(subset(as.monthly(y),it=1961:1990),month,FUN=FUN)
        clim8120 <- aggregate(subset(as.monthly(y),it=1981:2020),month,FUN=FUN)
        klim <- data.frame(Month=1:12,y1=clim6190,y2=clim8120)
        AC <- AC %>% add_trace(AC,data=klim,x=~Month,y=~y1,name='1961-1990',type='scatter',mode='lines',line=list(width = 4))
        AC <- AC %>% add_trace(AC,data=klim,x=~Month,y=~y2,name='1981-2020',type='scatter',mode='lines',line=list(width = 2, dash = 'dash'))
      } else {
        y <- updatestation()
        clim <- climatology(y)
        if (is.precip(y)) fun <- 'sum' else fun <- 'mean'
        if (input$timespace!='Annual_cycle_cumugram') Z <- diagram(y,plot=FALSE) else 
                                                      Z <- attr(cumugram(y,FUN=fun,plot=FALSE),'Y')[1:365,]
        ## Check if the years of data include current year
        iyr <- is.element(colnames(Z),format(Sys.Date(),'%Y'))
        #print(paste(sum(iyr),'cases for',format(Sys.Date(),'%Y')))
        nyrs <- dim(Z)[2]
        #colorpal<-colorRampPalette(brewer.pal(9,'Spectral'),nyrs)
        colorpal <- rgb(seq(0,1,length=nyrs),sin(seq(0,pi,length=nyrs))^2,seq(1,0,length=nyrs),0.3)
        colnames(Z) <- paste('y',colnames(Z),sep='')
        yrs <- colnames(Z)
        ## For current year, use black colour
        if (sum(iyr)>0) colorpal[iyr] <- rgb(0,0,0)
        mac <- as.data.frame(cbind(coredata(clim),Z))
        mac$day <- 1:365
  
        AC <- plot_ly(mac,name='annual_cycle',type='scatter',mode='markers', showlegend = (input$showlegend=='Show')) 
        
        for (i in 1:length(yrs)) {
          cl <- paste("AC <- AC %>% add_lines(data=mac, y = ~",yrs[i],", x = ~day,",
                      "type = 'scatter', mode = 'markers', line = list(width = 2,shape ='spline',color=colorpal[",
                      i,"]), name ='",substr(yrs[i],2,5),"')",sep = '')
          #print(cl)
          eval(parse(text=cl))
        }
        #print(names(mac))
        if (input$timespace!='Annual_cycle_cumugram') 
          AC <- AC %>% add_lines(data=mac,x=~day,y=~V1,name='Climatology',line = list(width = 2,shape ='spline',color='white')) 
        AC <- AC %>%
          layout(yaxis=list(title=esd::unit(y)),xaxis=list(title='Julian day'))
      }
    }
    
  })
  
  
  output$scatterplot <- renderPlotly({
    print('<27: output$scatterplot - render')
    statistic <- vals()
    Y <- updatemetadata()
    n <- dim(Y)[1]
    #print(n)
    if (input$xy_col=="Red") {
      Col <- rep('rgba(255, 182, 193, .9)',n) 
      lcol <- 'rgba(152, 0, 0, .8)'
      lwidth <- 2
    } else {
      Col <-Y[[input$xy_col]]
      lcol <- 'rgba(150, 150, 150, .8)'
      lwidth <- 1
    }
    
    ## Apply filter to highlight stations selected in the map
    if (input$country=='All') filter <- rep(TRUE,n) else {
      filter <- rep(FALSE,n)
      filter[(Y$country == input$country)] <- TRUE
    }
    #print('        <<< input$ci is not updated!!! >>>              ')
    #isolate({print(paste('Range shown in map',input$statisticrange[1],'-',input$statisticrange[1],' ci=',input$ci))})
    filter[statistic < input$statisticrange[1]] <- FALSE
    filter[statistic > input$statisticrange[2]] <- FALSE
    Y <- Y[filter,]
    
    if (input$xy_size=="Uniform") size <- rep(10,sum(filter)) else {
      z <- Y[[input$xy_size]]
      size <- 30*sqrt( (z - min(z,na.rm=TRUE))/(max(z,na.rm=TRUE) - min(z,na.rm=TRUE)) )
    }
    
    x0 <- as.numeric(input$x0)
    if (!is.null(Y$wetmean)) { 
      Y[['10.year.return.value']] <- -Y$wetmean*log(1/(Y$wetfreq*36.525))*1.4 - 3.82
      Y[['Number_of_days']] <- Y$wetfreq*exp(-x0/Y$wetmean)
    } else Y[['Number_of_days']] <- 365.25*(1-pnorm(x0,mean=0,sd=Y$sd))
    print(names(Y))
    print(c(input$x_variable,input$y_variable))
    Z <- data.frame(x=Y[[input$x_variable]],y=Y[[input$y_variable]], Col = Col[filter], 
                    size=size,name=as.character(paste(Y$location,Y$station.id))[filter],
                    stringsAsFactors = FALSE)
    size <- size[is.finite(Z[,1]) & is.finite(Z[,2])]
    Z <- Z[is.finite(Z[,1]) & is.finite(Z[,2]),]
    #print(summary(Z))
    fit <- lm(y ~ x, data=Z)
    #print(summary(fit))
    
    p <- plot_ly(data = Z, x = ~x, y = ~y, color= ~Col, text=~name, showlegend = FALSE,
                 marker = list(size=size,line = list(color = lcol, width = lwidth))) %>%
      layout(title = 'x/y',
             yaxis = list(title=input$y_variable, zeroline = TRUE),
             xaxis = list(title=input$x_variable, zeroline = TRUE))
    #add_lines(p,fit)
  })
  
  ## Multi-language support for Text, menues, and labels
  
  output$maintitle <- renderText({
    maintitle[as.numeric(input$lingo)]})
  output$maptitle <- renderText({
    maptitle[as.numeric(input$lingo)]})
  output$tstitle <- renderText({
    tstitle[as.numeric(input$lingo)]})
  output$htitle <- renderText({
    htitle[as.numeric(input$lingo)]})
  output$etitle <- renderText({
    etitle[as.numeric(input$lingo)]})
  output$cftitle <- renderText({
    cftitle[as.numeric(input$lingo)]})
  output$timespacelabel <- renderText({
    lab.timespace[as.numeric(input$lingo)]})
  output$timeperiodlabel <- renderText({
    lab.timeperiod[as.numeric(input$lingo)]})
  output$timescalelabel <- renderText({
    lab.timescale[as.numeric(input$lingo)]})
  output$seasonlabel <- renderText({
    lab.season[as.numeric(input$lingo)]})
  output$aspectlabel <- renderText({
    lab.aspect[as.numeric(input$lingo)]})
  output$locationlabel <- renderText({
    lab.location[as.numeric(input$lingo)]})
  output$statisticslabel <- renderText({
    lab.statitics[as.numeric(input$lingo)]})
  output$threshold <- renderText({
    lab.threshold[as.numeric(input$lingo)]})
  output$specificdate <- renderText({
    lab.date[as.numeric(input$lingo)]})
  output$highlightlabel <- renderText({
    lab.highlight[as.numeric(input$lingo)]})
  output$highlightTSlabel <- renderText({
    lab.highlight[as.numeric(input$lingo)]})
  output$daylabel <- renderText({
    lab.specificday[as.numeric(input$lingo)]})
  output$enddate <- renderText({
    Y <- updatemetadata()
    #print('output$enddate'); print(attr(Y,'period'))
    attr(Y,'period')[2]})
  output$excludelabel <- renderText({
    lab.exclude[as.numeric(input$lingo)]})
  output$seasonTSlabel <- renderText({
    lab.season[as.numeric(input$lingo)]})
  output$mapdescription <- renderText({
    paste(descrlab[as.numeric(input$lingo)],explainmapstatistic(input$statistic,input$lingo,types))})
  output$datainterval <- renderText({
    Y <- updatemetadata()
    #print('output$datainterval'); print(paste('Source',input$src))
    paste(sources[match(input$src,source.regions),as.numeric(input$lingo)],
          attr(Y,'period')[1],' - ',attr(Y,'period')[2])})
  output$cntr <- renderText({
    y <- updatetimeseries()
    average <- round(mean(y[,1],na.rm=TRUE),1)[1]; slope <- round(trend.coef(y[,1]),2)[1]
    varpro <- round(100*var(trend(y[,1],na.rm=TRUE))/var(y[,1],na.rm=TRUE))[1]
    trendpvalue <- round(100*trend.pval(y[,1]),2)[1]
    #print(c(average,slope,varpro,trendpvalue))
    paste(esd::loc(y)[1],'in',esd::cntr(y)[1],'mean=',average,'. Trend=', slope,esd::unit(y)[1],'per decade and explains', 
          varpro,'% of the variance with a probability of',trendpvalue,'% that it is due to chance.')
  })
  output$hdes <- renderText({
    y <- updatetimeseries()
    Y <- updatemetadata()
    statistic <- vals()
    ## Apply filter to highlight stations selected in the map
    n <- length(statistic)
      if (input$country=='All') filter <- rep(TRUE,n) else {
      filter <- rep(FALSE,n)
      filter[(Y$country == input$country)] <- TRUE
    }
    #print('        <<< input$ci is not updated!!! >>>              ')
    #isolate({print(paste('Range shown in map',input$statisticrange[1],'-',input$statisticrange[1],' ci=',input$ci))})
    filter[statistic < input$statisticrange[1]] <- FALSE
    filter[statistic > input$statisticrange[2]] <- FALSE
    statistic <- statistic[filter]
    
    if (input$timespace == 'Histogram_location') {
      yH <- coredata(y) 
    } else yH <- statistic
    syH <- summary(yH)[c(1,4,6)]
    if (substr(input$timespace,1,12) != 'Annual_cycle') {
      if (input$timespace=='Histogram_map') 
        hsum <- paste('Data= ',input$statistic,' ', attr(Y,'longname'),': ',paste(names(syH),round(syH,1),collapse=', ',sep='='),sep='') else
          hsum <- paste(syH,collapse=', ',sep='=')
    } else {
      hsum <- paste(attr(y,'longname'),'period=',paste(range(index(y)),collapse='-'))
    }
    print(input$timespace); print(timespace); print(match(input$timespace,timespace))
    hdescr <- timespacedescr[as.numeric(input$lingo),match(input$timespace,timespace)]
    if (input$timespace != 'Histogram_map') hdescr <- paste(hdescr,esd::loc(y),'in',esd::cntr(y)[1])
    if (input$timespace == 'Histogram_location') {
      if (is.precip(y)) hdescr <- paste(hdescr,'. Time scale= ',input$tscale,', aspect= ',
                                        aspectnameP[as.numeric(input$lingo),is.element(aspectsP,input$aspect)],sep='') else
                                          hdescr <- paste(hdescr,'. Time scale= ',input$tscale,', aspect= ',
                                                          aspectnameT[as.numeric(input$lingo),is.element(aspectsT,input$aspect)],sep='')
    }
    #print(c(hdescr,hsum))
    paste(hdescr,'. ',hsum,'. Sample size= ',sum(is.finite(yH)),' data points.',sep='')
  })
  output$yearlabel <- renderText({
    showhideyears[as.numeric(input$lingo)]})
  output$relations <- renderText({
    relationstitle[as.numeric(input$lingo)]})
}
