## See comments and explanations in global.R
## Rasmus Benestad


# Load the ggplot2 package which provides
# the 'mpg' dataset.

# Define a server for the Shiny app
server <- function(input, output, session) {
  
  ## Functions ---------------------------------------------------------------------------------------------------------
  
  points <- eventReactive(input$recalc, {
    cbind(lon(y),lat(y))
  }, ignoreNULL = FALSE)
  
  
  # Show a popup at the given location
  showMetaPopup <- function(stid, lat, lng) {
    #print(paste('showMetaPopup() ===',stid,round(lat,2),round(lng,2)))
    fnames <- updatefilenames()
    Y <- retrieve.stationsummary(fnames[as.numeric(input$ci)])
    #print("Y <- retrieve.stationsummary")
    selLon <- round(Y$longitude[Y$station.id == stid],2)
    selLat <- round(Y$latitude[Y$station.id == stid],2)
    selAlt <- round(Y$altitude[Y$station.id == stid])
    location <- Y$location[Y$station.id == stid]
    #print(c(stid,selLon,selLat,location))
    content <- paste(sep = "<br/>",
                     tags$strong(HTML(toupper(location))),
                     tags$strong(HTML(paste('LON ',selLon,'W',sep=''), 
                                      paste(' LAT ',selLat,'N',sep=''), 
                                      paste(' ALT ',selAlt,'m',sep=''))), 
                     sprintf("Period: %s",paste(attr(Y,'period')[1],'-',attr(Y,'period')[2])),
                     sprintf("Station ID: %s", as.character(stid)),
                     sprintf("Parameter: %s", paste(toupper(varid(y)),collapse = ',')),
                     sprintf("Start year: %s", paste(Y$first.year[Y$station.id == stid],collapse = ',')),
                     sprintf("End year: %s", paste(Y$last.year[Y$station.id == stid],collapse = ',')),
                     sprintf("Data provider: Meteorologisk institutt"))
    
    leafletProxy("map") %>% addPopups(lng, lat, content,layerId = stid)
    #print('((()))')
  }
  
  ## Reactive expressions to update information -------------------------------------------------------------- 
  
  # thresh <- reactive({
  #   return(as.numeric(input$thresh))
  # })
  
  zoom <- reactive({
     zoomscale <- switch(input$src,
                         'metnod'=4,'ecad'=3,'ghcnd'=1)
     return(zoomscale)
  })
  
  updatefilenames <- reactive({
    print('updatefilenames')
    fnames <- list.files(path='data',pattern='.nc',full.names = TRUE)
    fnames <- fnames[grep('.nc',fnames,fixed=TRUE)]
    print(fnames); print(src); print(input$src)
    fnames <- fnames[grep(src[match(input$src,src)],fnames)]
    return(fnames)
  })
  
  # Computing indices
  vals <- reactive({
    #browser()
    fnames <- updatefilenames()
    print(paste('vals: Y$',input$statistic,sep='')); print(fnames); print(input$ci)
    ## Get summary data from the netCDF file
    Y <- retrieve.stationsummary(fnames[as.numeric(input$ci)])
    
    showseason <- switch(input$season,
                         'all'='','DJF'='_DJF','MAM'='_MAM','JJA'='_JJA','SON'='_SON')
    print(paste('Y$',input$statistic,showseason,sep=''))
    if ( (tolower(input$statistic)!='number_of_days') &
         (tolower(input$statistic)!='specific_day') ) {
      #print('Get the summary statistics from the netCDF file'); print(tolower(input$statistic))
      Z <- eval(parse(text=paste('Y$',input$statistic,showseason,sep=''))) 
      } else {
        if (tolower(input$statistic)=='number_of_days') {
          print('Number_of_days')
          x0 <- as.numeric(input$x0)
          print(paste('threshold x0=',x0))
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
          #print("Read a specific date from the netCDF file"); print(it)
          x <- retrieve.station(fnames[as.numeric(input$ci)],it=it,verbose=verbose)
          Z <- c(coredata(x))
          dim(Z) <- c(length(Z),1)
          ## The stations are sorted according to alphabetic order
          Z <- Z[match(Y$station.id,stid(x))]
          #print(rbind(stid(x),Y$station.id))
          #print('...')
        }
      } 
    if (input$statistic=='number.valid') Z <- eval(parse(text=paste('Y$',input$statistic,sep='')))/365.25
    if (input$statistic=='records') Z <- 100*Z
    if (is.character(Z)) Z <- as.numeric(as.factor(Z))
    print('Values returned by vals():');print(length(Z)); print(summary(Z)); print('---')
    return(Z) 
  })
  
  ## Events ---------------------------------------------------------------------------------------------------------
  
  ## When new data source/region is selected
  observe({
    ## Get the file names of the data
    fnames <- updatefilenames()
    Y <- retrieve.stationsummary(fnames[as.numeric(input$ci)])
    print('input$src - location'); Y$location[1]
    browser()
    loc1 <- switch(input$src,'metnod'='Oslo - blind','ecad'='De bilt','ghcnd'=Y$location[1])
    print(paste('New default location:',loc1))
    updateSelectInput(session=session,inputId="location", choices = Y$location, selected=loc1)
    print(input$location)
    
    varids <- substr(fnames,6,nchar(fnames))
    varids <- substr(varids,1,regexpr('.',varids,fixed=TRUE)-1)
    ci <- c(1:length(varids)); names(ci) <- vari2name(varids,names=varnames[as.numeric(input$lingo),])
    print('input$src - ci'); print(input$src); print(fnames); print(ci)
    updateSelectInput(session=session,inputId="ci",choices=ci,selected=1)
    
    statistic <- vals()
    statisticmin <- round(min(statistic,na.rm=TRUE))
    statisticmax <- round(max(statistic,na.rm=TRUE))
    print(c(statisticmin,statisticmax))
    updateSliderInput(session=session,inputId="statisticrange",
                      min=statisticmin,max=statisticmax,value = c(statisticmin,statisticmax))
    
    updateSelectInput(session=session,inputId="country",choices=c('All',rownames(table(Y$country))),selected='All')
    
    statisticmin <- round(min(statistic,na.rm=TRUE))
    statisticmax <- round(max(statistic,na.rm=TRUE))
    print('max & min')
    updateSliderInput(session=session,inputId="statisticrange",
                      min=statisticmin,max=statisticmax,value = c(statisticmin,statisticmax))
  })
  
  ## Click on the map marker
  observeEvent(input$map_marker_click,{
    #print("observeEvent() - click")
    fnames <- updatefilenames()
    Y <- retrieve.stationsummary(fnames[as.numeric(input$ci)])
    event <- input$map_marker_click
    #print(paste('Updated ',input$location)); print(event$id)
    selected <- which(Y$station.id == event$id)
    #if (input$exclude== 'Selected') filter[selected] <- FALSE
    updateSelectInput(session,inputId = 'location',label='Sted',choices=Y$location,
                      selected = Y$location[selected])  
  })
  
  ## Change language
  observeEvent(input$lingo, {
    #tscales <- c("day","month","season","year")
    names(tscales) <- timescales[as.numeric(input$lingo),]
    updateSelectInput(session=session,inputId="tscale",choices=tscales,selected=tscales[4])
    
    varids <- 1:length(varids)
    names(varids) <- varnames[as.numeric(input$lingo),]
    print(varids)
    updateSelectInput(session=session,inputId="ci",choices=varids,selected=input$ci)
    
    names(src) <- regions[as.numeric(input$lingo),]
    print(src)
    updateSelectInput(session=session,inputId="src",choices=src,selected=input$src)
    
    names(stattype) <- type2name(stattype,input$lingo,types)
    #print(stattype)
    updateSelectInput(session=session,inputId="statistic",choices=stattype,selected=input$statistic)
    
    names(timespace) <- timespacenames[as.numeric(input$lingo),]
    updateSelectInput(session=session,inputId="timespace",choices=timespace,selected=input$timespace)
  })
  
  ## Change statistics to show in the map
  observeEvent(input$statistic, {
    print(paste('Update range for',input$statistic))
    statistic <- vals()
    print('statistics retrieved')
    statisticmin <- round(min(statistic,na.rm=TRUE))
    statisticmax <- round(max(statistic,na.rm=TRUE))
    print('max & min')
    updateSliderInput(session=session,inputId="statisticrange",
                      min=statisticmin,max=statisticmax,value = c(statisticmin,statisticmax))
  })
  
  ## Change climate indicator for all boxes (precip, temperature)
  observeEvent(input$ci, {
    print('Update range')
    statistic <- vals()
    print('statistics retrieved')
    statisticmin <- round(min(statistic,na.rm=TRUE))
    statisticmax <- round(max(statistic,na.rm=TRUE))
    print('max & min')
    updateSliderInput(session=session,inputId="statisticrange",
                      min=statisticmin,max=statisticmax,value = c(statisticmin,statisticmax))
    
    if (as.numeric(input$ci)==1) {
      aspects <- aspectsP 
      names(aspects) <- aspectnameP[as.numeric(input$lingo),] 
    } else {
      aspects  <- aspectsT
      names(aspects) <- aspectnameT[as.numeric(input$lingo),]
    }                
    #print(aspects)
    updateSelectInput(session=session,inputId="aspect",choices=aspects,selected=aspects[1])
    
    updateSelectInput(session=session,inputId="statistic",
                      choices=getstattype(fnames[as.numeric(input$ci)],lingo=input$lingo),selected="mean")
  })
  
  ## Change the time scale for the time series box
  observeEvent(input$tscale, {
    if (input$tscale=='year') newseaTS <- seaTS[1] else
      if (input$tscale=='season') newseaTS <- seaTS[1:5] else newseaTS <- seaTS
      updateSelectInput(session=session,inputId="seasonTS",choices=newseaTS)
  })
  
  observeEvent(input$country, {
    statistic <- vals()
    if (input$country != 'All') statistic[!is.element(Y$country,input$country)] <- NA
    statisticmin <- round(min(statistic,na.rm=TRUE))
    statisticmax <- round(max(statistic,na.rm=TRUE))
    print('max & min')
    updateSliderInput(session=session,inputId="statisticrange",
                      min=statisticmin,max=statisticmax,value = c(statisticmin,statisticmax))
  })
  
  ## Observe ---------------------------------------------------------------------------------------------------------
  
  # When map is clicked, show a popup with location info
  observe({
    fnames <- updatefilenames()
    Y <- retrieve.stationsummary(fnames[as.numeric(input$ci)])
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    #print('Data Explorer from map'); print(event$id)
    selectedStid <- Y$station.id[which(tolower(input$location) == tolower(Y$location))]
    
    if (is.null(event))
      return()
    #print('Click --->'); print(event); print('<--- Click')
    isolate({
      showMetaPopup(stid=event$id,lat=event$lat, lng = event$lng)
    })
    
    #removeMarker("map",layerId = event$id)
    leafletProxy("map",data = event) %>% 
      addCircles(lng = event$lng, lat = event$lat,color = 'red',layerId = 'selectID', weight = 12)
  })
  
  ## Data Explorer roll down menu
  observe({
    fnames <- updatefilenames()
    print('Data Explorer from roll-down menu')
    Y <- retrieve.stationsummary(fnames[as.numeric(input$ci)])
    print(input$location); print(fnames[as.numeric(input$ci)])
    statistic <- vals()
    if ( (input$src=='ecad') & (input$location=='Oslo - blind') ) loc1 <- 'De bilt' else loc1 <- input$location
    selectedStid <- Y$station.id[which(tolower(loc1) == tolower(Y$location))]
    ## Read single time series from the netCDF file
    print(paste('selectedID: ',selectedStid,' = ',input$location,'from',fnames[as.numeric(input$ci)]))
    if (is.null(selectedStid) | length(selectedStid)!=1) {
      print(input$location);print('Something is wrong!')
      if (is.null(selectedStid)) selectedStid <- Y$station.id[1] else
        if (length(selectedStid)>1) selectedStid <- selectedStid[1] else
          selectedStid <- Y$station.id[1]
    }
    print("y <- retrieve.station")
    if (!is.null(selectedStid)) 
      if (sum(is.element(Y$station.id,selectedStid))>0)
        y <- retrieve.station(fnames[as.numeric(input$ci)],stid=selectedStid,verbose=verbose)
    
    #if (is.precip(y)) thresholds <- seq(10,50,by=10) else thresholds <- seq(-30,30,by=5)
    
    #print(input$season); print(input$tscale); print(input$aspect)
    if (is.precip(y)) {
      if (input$aspect=='wetmean') FUN<-'wetmean' else
        if (input$aspect=='wetfreq') FUN<-'wetfreq' else
          if (input$aspect=="Number_of_days") FUN<-'count' else FUN<-'sum'
    } else if (input$aspect=="Number_of_days") FUN<-'count' else FUN<-'mean'
    #if (is.null(FUN)) FUN='mean'
    
    ## Time series
    print('Time series')
    x0 <- as.numeric(input$x0)
    y0 <- y # Original daily data
    if (FUN != 'count') 
      y <- switch(input$tscale,
                  'day'=y,'month'=as.monthly(y,FUN=FUN),
                  'season'=as.4seasons(y,FUN=FUN),'year'=as.annual(y,FUN=FUN)) else
                    y <- switch(input$tscale,
                                'day'=y,'month'=as.monthly(y,FUN=FUN,threshold=x0),
                                'season'=as.4seasons(y,FUN=FUN,threshold=x0),
                                'year'=as.annual(y,FUN=FUN,threshold=x0))
    #if (is.T(y)) browser()
    #print(c(aspects,input$aspect)); print(input$ci); print(x0); print(FUN); print(esd::unit(y))
    if (input$aspect=='anomaly') y <- anomaly(y)
    if (input$seasonTS != 'all') y <- subset(y,it=tolower(input$seasonTS))
    if (input$aspect=='wetfreq') {
      y <- 100*y
      attr(y,'unit') <- '%'
    }
    
    ## Marking the top and low 10 points
    print('10 highs and lows')
    if (tolower(input$highlightTS)=='top 10') highlight10 <- y[order(coredata(y),decreasing=TRUE)[1:10]] else
      if (tolower(input$highlightTS)=='low 10') highlight10 <- y[order(coredata(y),decreasing=FALSE)[1:10]] else
        if (tolower(input$highlightTS)=='new records') {
          print('new records')
          dim(y) <- NULL
          recs <- records(y)
          #print(recs)
          highlight10 <- y[attr(recs,'t')]
        } else
        highlight10 <- y[1:10]+NA
    print(highlight10)
    
    #print('Extract data for histogram')
    if (input$timespace == 'Histogram_location') yH <- coredata(y) else yH <- statistic
    
    #print('Add marker for selected location')
    leafletProxy("map",data = y) %>% clearPopups() %>% 
      addCircles(lng = lon(y), lat = lat(y), color = 'red', layerId = 'selectID', weight = 12)
    
    isolate({
      showMetaPopup(stid=stid(y),lat=lat(y), lng = lon(y))
    })
  
  ## Output rendering ------------------------------------------------------------------------------------------------
  
  ## The map panel 
  output$map <- renderLeaflet({
    ## Get summary data from the netCDF file
    fnames <- updatefilenames()
    print('The map panel'); print(input$ci); print(fnames)
    Y <- retrieve.stationsummary(fnames[as.numeric(input$ci)])
    statistic <- vals()
    #print('Stastistic shown on map');print(summary(statistic))
    
    if (input$country=='All') filter <- rep(TRUE,length(statistic)) else {
      filter <- rep(FALSE,length(statistic))
      filter[(Y$country == input$country)] <- TRUE
    }
    filter[statistic < input$statisticrange[1]] <- FALSE
    filter[statistic > input$statisticrange[2]] <- FALSE
    
    if (tolower(input$higlight) == "top 10") highlight <- order(statistic[filter],decreasing=TRUE)[1:10] else 
    if (tolower(input$higlight) == "low 10") highlight <- order(statistic[filter],decreasing=FALSE)[1:10] else 
    if (tolower(input$higlight) == "New records") {
      if ( (!is-null(Y$last_element_highest)) & (!is-null(Y$last_element_lowest)) ) 
            highlight <- Y$last_element_highest > 0 & Y$last_element_lowest > 0 else
      if (!is-null(Y$last_element_highest)) highlight <- Y$last_element_highest > 0 else
            highlight <- rep(FALSE,length(statistic))
    } 
      highlight <- NULL
    lon.highlight <- Y$longitude[filter][highlight]
    lat.highlight <- Y$latitude[filter][highlight]
    print('Highlight');print(statistic[highlight]); print(lon.highlight); print(lat.highlight)
    if (!is.null(Y$wetfreq)) reverse <- TRUE else reverse <- FALSE
    print(paste('Reverse palette =',reverse)); print(summary(statistic))
    #print(c(sum(filter),length(filter),length(statistic)))
    pal <- colorBin(colscal(col = 't2m',n=100),
                                 statistic[filter],bins = 10,pretty = TRUE,reverse=reverse)    
    legendtitle <- input$statistic
    if (legendtitle=='Specific_day') legendtitle=input$it
    
    leaflet() %>% 
      addCircleMarkers(lng = Y$longitude[filter], # longitude
                       lat = Y$latitude[filter],fill = TRUE, # latitude
                       label = as.character(round(statistic[filter],digits = 2)),
                       labelOptions = labelOptions(direction = "right",textsize = "12px",opacity=0.6),
                       popup = Y$location[filter],popupOptions(keepInView = TRUE),
                       radius =4,stroke=TRUE,weight = 1, color='black',
                       layerId = Y$station.id[filter],
                       fillOpacity = 0.4,fillColor=pal(statistic[filter])) %>% 
      addCircleMarkers(lng = lon.highlight, lat = lat.highlight,fill=TRUE,
                       label=as.character(1:10),
                       labelOptions = labelOptions(direction = "right",textsize = "12px",opacity=0.6),
                       radius=5,stroke=TRUE, weight=5, color='black',
                       layerId = Y$station.id[filter][highlight],
                       fillOpacity = 0.6,fillColor=rep("black",10)) %>%
      addLegend("bottomleft", pal=pal, values=round(statistic[filter], digits = 2), 
                title=legendtitle,
                layerId="colorLegend",labFormat = labelFormat(big.mark = "")) %>%
      addProviderTiles(providers$Esri.WorldStreetMap,
                       #addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>% 
      setView(lat=mean(Y$latitude),lng = mean(Y$longitude), zoom = zoom())
  })
    
    output$plotstation <- renderPlotly({
      withProgress(message = 'Updating ...',
                   detail = 'This may take a while...', value = 0,
                   { for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.05)}
                   })
      
      timeseries <- data.frame(date=index(y),y=coredata(y),trend=coredata(trend(y)))
      TS <- plot_ly(timeseries,x=~date,y=~y,type = 'scatter',mode='lines',name='data')
      TS = TS %>% add_trace(y=~trend,name='trend') %>% 
        add_markers(x=index(highlight10),y=coredata(highlight10),label=input$highlightTS) %>% 
        layout(title=loc(y),yaxis = list(title=esd::unit(y)))
      #TS$elementID <- NULL
    })
    
    output$histstation <- renderPlotly({
      withProgress(message = 'Updating ...',
                   detail = 'This may take a while...', value = 0,
                   { for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.05)
                   }
                   })
      #mx <- ceiling(1.1*max(abs(y),na.rm=TRUE))
      print('histstation'); print(summary(yH))
      if (substr(input$timespace,1,12) != 'Annual_cycle') {
        fit <- density(yH[is.finite(yH)])
        #breaks <- seq(floor(min(yH,na.rm=TRUE)),ceiling(max(yH,na.rm=TRUE)),length=100)
        pdf <- dnorm(fit$y,mean=mean(yH,na.rm=TRUE), sd = sd(yH,na.rm=TRUE))
        dist <- data.frame(y=coredata(yH))
        #print(summary(dist))
        syH <- summary(yH)[c(1,4,6)]
        #print(syH); print(class(syH))
        if (input$timespace=='Histogram_map') 
          title <- paste(input$statistic,': ',paste(names(syH),round(syH,1),collapse=', ',sep='='),sep='') else
          title <- paste(loc(y),': ',paste(syH,collapse=', ',sep='='),sep='')
        #print(title)
        H <- plot_ly(dist,x=~y,name='data',type='histogram',histnorm='probability')
        H = H %>% #add_trace(y=fit$x,x=pdf,name='pdf',mode='lines') %>% 
          #add_trace(x = fit$x, y = fit$y, mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>%
          layout(title=title)
        #H = H %>% layout(title=title)
      } else {
        y <- subset(y0,it=input$dateRange)
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
        } else {
          mac <- data.frame(y) 
          mac$Month <- month(y)  
        }
        
        print(summary(mac)); print(input$dateRange); print(title)
        AC <- plot_ly(mac,x=~Month,y=~y,name='mean_annual_cycle',type='box')  %>% 
          layout(title=title,yaxis=list(ylab))
       
      }
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
      lab.speficicday[as.numeric(input$lingo)]})
    output$excludelabel <- renderText({
      lab.exclude[as.numeric(input$lingo)]})
    output$mapdescription <- renderText({
      paste(descrlab[as.numeric(input$lingo)],explainmapstatistic(input$statistic,input$lingo,types))})
  })
}