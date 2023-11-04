## This R-shiny app searches the data directory for netCDF files with station data. These
## netCDF files have been generated with the esd-package using write2nc4.station and contain
## daily data and some summary statistics. The precalculated summary statistics are used for
## plotting the maps. The intention is that this app is flexible and adjusts the output according
## to the netCDf files and teir contents. To make the app as speedy as possible, it only reads the
## actual data presented, such as pre-calculated summary statistics and selected time series or 
## date. The netCDF files allow for direct access, which means that only the required data are 
## extracted. Some numbers may be estimated, such as expected number of events per year: 365*Pr(X) -
## For temperature use the normal distribution, for precipitation use the "rain equation" - together 
## with the pre-computed summary statistics: (mean, sd) or (fw, mu)
## Rasmus Benestad & Abdelkader Mezghani, 2018-06-21

## The esd-package is available from github.com/metno/esd
library(esd)
library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(RColorBrewer)

## Functions used by the app: -------------------------------------------------------------------------------------

## Use getncattr to retrieve attributes/metadata for the app dashboard display/information
## e.g. source, countries
getncinfo <- function(file) {
  print(paste('getncinfo',file,'wd=',getwd()))
  ncid <- nc_open(file)
  varid <- names(ncid$var)
  loc <- ncvar_get(ncid,'loc')
  cntr <- ncvar_get(ncid,'cntr')
  src <- ncatt_get(ncid,0,'source')$value
  latmin <- floor(ncatt_get(ncid,0,'geospatial_lat_min')$value)
  latmax <- ceiling(ncatt_get(ncid,0,'geospatial_lat_max')$value)
  lonmin <- floor(ncatt_get(ncid,0,'geospatial_lon_min')$value)
  lonmax <- ceiling(ncatt_get(ncid,0,'geospatial_lon_max')$value)
  t.start <- ncatt_get(ncid,0,'time_coverage_start')$value
  t.end <- ncatt_get(ncid,0,'time_coverage_end')$value
  nc_close(ncid)
  cntrtab <- table(cntr)
  if (is.null(src) | is.na(src) | length(src)==0 | nchar(src)==0) src <- substr(file,
                                               gregexpr('.',file,fixed =TRUE)[[1]][1]+1,gregexpr('.',file,fixed =TRUE)[[1]][2]-1)
  if (length(rownames(cntrtab))==1) reg <- rownames(cntrtab)[1] else if (length(rownames(cntrtab))==2) { 
    reg <- paste(rownames(cntrtab)[1],rownames(cntrtab)[length(rownames(cntrtab))],sep=' and ')
  } else { 
    reg <- paste(length(rownames(cntrtab)),' countries from ', rownames(cntrtab)[1],rownames(cntrtab)[length(rownames(cntrtab))],sep=' to ')
  }
  info <- paste0(vari2name(varid[1]),' from ',length(loc),' stations for the period ',t.start,' to ',t.end,' within ',reg,': ',
                 round(latmin),'-',latmax,'N/',lonmin,'-',lonmax,'E. Source: ',src[1],'. File: ',sub('data/','',file))
  attr(info,'cntr') <- table(cntr)
  attr(info,'src') <- src
  attr(info,'varid') <- varid
  return(info)
} 

# 
## The following reactive expressions get updated file information and metadata
updatefilenames <- function(src,datainfo) {
  print('<13: function - updatefilenames()')
  #fnames <- list.files(path='data',pattern='.nc',full.names = TRUE)
  fnames <- names(datainfo)
  fnames <- fnames[grep(src,fnames)]
  #fnames <- fnames[grep('.nc',fnames,fixed=TRUE)]
  #fnames <- fnames[grep(src[match(input$src,src)],fnames)]
  print(fnames); print(src)
  return(fnames)
}

getdatainfo <- function() {
  fnames <- list.files(path='data',pattern='.nc',full.names = TRUE)
  print(getwd())
  print(fnames)
  datainfo <- list()
  for (fname in fnames) {
    ncinfo <- getncinfo(fname)
    datainfo[[fname]] <- ncinfo
  }
  return(datainfo)
}

## Organises the names of the statistic presented in the map and their appearances in the menu. Supports multiple languages
type2name <-function(stattype,lingo,types) {
  print('<A: type2name')
  names <- rbind(
    c("Høyde over havet","Start år","Dager siden siste regn","siste dager i strekk med nedbør","Siste år","Breddegrad",           
      "Lengdegrad","Maksimumsverdi","Gjennomsnitt","Minimumsverdi","Antall år med data",
      "Antall høye rekorder","Trend","Trend: dager med nedbør","Trend: nedbørintensitet", 
      "Dager med nedbør","Typisk nedbørsintensitet",
      "Antall dager over terskelverdi","Utvalgt dato","Standardavvik","Antall lave rekorder","Varians",
      "Varianstrend","10-år-returverdi","Gjennomsnittlig opphold","Gjennomsnittlig regnvarighet",
      "Dager over normalen","Andel med flere våte dager","Andel med flere tørre dager"),
    c("Høgde over havet","Start år","Dagar siden siste regn","siste dagar i strekk med nedbør","Siste år","Breddegrad",           
      "Lengdegrad","Maksimumsverdi","Gjennomsnitt","Minimumsverdi","Antall år med data",
      "Antall høge rekordar","Trend","Trend: dager med nedbør","Trend: nedbørintensitet", 
      "Dager med nedbør","Typisk nedbørsintensitet",
      "Antall dagar over terskelverdi","Utvalgt dato","Standardavvik","Antall låge rekordar","Varians",
      "Varianstrend","10-år-returverdi","Gjennomsnittlig opphold","Gjennomsnittlig regnvarighet",
      "Dagar over normalen","Andel med fleire våte dagar","Andel med fleire tørre dagar"),
    c("Altitude","Start year","Days since last rain","Continuous ongoing rainy days","End year","Latitude",           
      "Longitude","Maximum","Average","Minimum","Years with data"," Number of record-highs",
      "Trend","Trend in wet days","Trend in rain intensity", 
      "Number of wet days","Mean rain intensity","Number of days above threshold",
      "Specific date","Standard deviation","Number of record-lows","Variance",
      "Trend in variance","10-year-return-value","Average dry spell","Average wet spell",
      "Days above normal","Proportion with longer wet spells","Proportion with longer dry spells")
  )
  matchingname <- names[as.numeric(lingo),]
  ipick <- match(tolower(stattype),tolower(types))
  ipick <- ipick[!is.na(ipick)]
  descr <- matchingname[ipick]
  print(descr)
  print('... A>')
  return(descr)
}

explainmapstatistic <- function(stattype,lingo,types) {
  print('<B: explainmapstatistic')
  descriptions <- rbind(
    c("Høyde over havet i meter (metadata)","Året da målingene startet (metadata)",
      "Hvor mange dager det har gått uten nedbør","Hvor mange dager det har gått med nedbør",
      "Siste år med målinger (metadata)","Målestasjonens breddegrad (metadata)",
      "Målestasjoens lengdegrad (metadata)","Maksimumsverdi",           
      "Gjennomsnittlig over flere år eller sesonger.",
      "Minste målte verdi","Hvor lang er måleserien (i antall år)",
      "Forholdet (i %) mellom antall registrerte rekorder og hva man forventer i et stabilt klima",
      "Trendene viser gjennomsnittlig endring over tid. Her vises estimert endring per tiår. Brudd i datamålingene kan gi feilaktige tall.",
      "Trend i dager med nedbør (%/tiår)","Trend i nedbørsintensitet (mm/dag per tiår)", 
      "Dager med nedbør (%)","Gjennomsnittlig nedbørsmengde for dager det regner mer enn 1mm (mm/dag)",
      "Forventet antall dager over terskelverdi","Målte verdier for en valgt dag",
      "Standardavvik av avvik fra normalen over et år eller en sesong",
      "Forholdet (i %) mellom antall registrerte rekorder og hva man forventer i et stabilt klima",
      "Estimert varians i døgnnedbør","Trend i estimert varians i døgnnedbør",
      "Et grovt estimat av 10-års-returverdi basert på enkel modell",
      "Gjennomsnittlig varighet på oppholdsperioder (dager)",
      "Gjennomsnittlig varighet på nedbørsepisoder (dager)",
      "Dager over normalen",
      "Hvor stor andel av våte perioder er lengre enn denne (basert på geometrisk fordelign)",
      "Hvor stor andel av tørre perioder er lengre enn denne (basert på geometrisk fordelign)"),
    c("Høgde over havet i meter (metadata)","Året da mælingane starta (metadata)",
      "Kor mange dagar det har gått utan nedbør","Hvor mange dager det har gått med nedbør",
      "Siste år med mælinger (metadata)","Mælestasjonens breddegrad (metadata)",
      "Mælestasjoens lengdegrad (metadata)","Maksimumsverdi",           
      "Gjennomsnittleg over fleire år eller sesongar.",
      "Minste mælte verdi","Kor mykje mæledata som fins (i antall år)",
      "Forholdet (i %) mellom antall registrerte rekordar og kva ein forventar i eit stabilt klima",
      "Trendane viser gjennomsnittleg endring over tid. Her vises estimert endring per tiår. Brot i datamælingene kan gi feilaktige tall.",
      "Trend i dagar med nedbør (%/tiår)","Trend i nedbørsintensitet (mm/dag per tiår)", 
      "Dagar med nedbør (%)","Gjennomsnittleg nedbørsmengde for dager det regner meir enn 1mm (mm/dag)",
      "Forventa antall dagar over terskelverdi","Mælte verdier for ein vald dag",
      "Standardavvik av avvik fra normalen over eit år eller ein sesong",
      "Forholdet (i %) mellom antall registrerte rekordar og kva ein forventer i eit stabilt klima",
      "Estimert varians i døgnnedbør","Trend i estimert varians i døgnnedbør",
      "Eit grovt estimat av 10-års-returverdi basert på enkel modell",
      "Gjennomsnittlig varighet på oppholdsperioder (dagar)",
      "Gjennomsnittlig varighet på nedbørsepisoder (dagar)",
      "Dager over normalen",
      "Hvor stor andel av våte perioder er lengre enn denne (basert på geometrisk fordelign)",
      "Hvor stor andel av tørre perioder er lengre enn denne (basert på geometrisk fordelign)"),
    c("The elevation of the station in m above sea level (metadata))",
      "The year when the observations started (metadata)","Number of days without rain",
      "Number of days with rain",
      "The last year with observations (metadata)","The latitude of the station in degrees north (metadata)",
      "The longitude of the station in degrees east (metadata)","The highest recorded value",           
      "The average over the years or seasons",
      "The smallest value","How many years there is with data (metadata)",
      "The number of record-highs compared to expected number given a stable climate (%)",
      "The trend shows the average change over time. Here it is the estimated change per decade. Discontinuities in the data can give misleading estimates",
      "Trend in number of wet days",
      "Trend in the annual or seasonal wet-day mean precipitation", 
      "The mean number of wet days over the whole year or seasons (%)",
      "The mean wet-day mean precipitation over the whole year or seasons (mm/day)",
      "The expected number of days above threshold",
      "Observations made for a specific date",
      "The standard deviation of anomalies over the whole year or seasons",
      "The number of record-highs compared to expected number given a stable climate (%)",
      "Estimated variance in daily rainfall","Trend in estimated daily rainfall variance",
      "Simple and approximate estimate of the 10-year-return-value",
      "Mean duration of a dry spell (days)","Mean duration of a wet spell (days)",
      "Day with measurements above the normal",
      "The proportion of wet spells longer than the current one (according to a geometric distribution)",
      "The proportion of dry spells longer than the current one (according to a geometric distribution)")
  )
  print(paste('explainmapstatistic: language=',lingo,'stattype=',stattype))
  description <- descriptions[as.numeric(lingo),]
  ipick <- match(tolower(stattype),tolower(types))
  ipick <- ipick[!is.na(ipick)]
  descr <- description[ipick]
  print(descr)
  print('... B>')
  return(descr)
}


## Extract the avaiable statistics to present on the map based on the aggregated statistics stored in the netCDF files
getstattype <- function(fname,lingo=NULL) {
  print(paste('<C: getstattype',fname,lingo))
  meta <- retrieve.stationsummary(fname)
  #print(names(meta))
  doubleuscrs <- unlist(lapply(gregexpr('_',names(meta)),length)) > 1
  names(meta)[doubleuscrs] <- sub('_','-',names(meta)[doubleuscrs])
  names(meta) <- paste(names(meta),'_',sep='')
  stattype <- rownames(table(substr(names(meta),1,regexpr('_',names(meta))-1)[sapply(meta,is.numeric)]))
  stattype <- stattype[!is.element(stattype,c('station.id'))]
  stattype <- sub('-','_',stattype)
  if (length(grep("lehr|last_element_highest",stattype))>0) {
    stattype <- stattype[-grep("lehr|last_element_highest",stattype)]
  }
  if (length(grep("lelr|last_element_lowest",stattype))>0) {
    stattype <- stattype[-grep("lelr|last_element_lowest",stattype)]
  }
  if ( (length(grep('wetmean',names(meta)))>0) & (length(grep('wetfreq',names(meta)))>0) )
    stattype <- c(stattype,'10.year.return.value')
  if (length(grep('wetdur',names(meta)))) {
    stattype <- c(stattype,'mean_drydur','mean_wetdur','prob_long_wet','prob_long_dry')
  }
  if (length(grep('sd_',names(meta)))) {
    stattype <- c(stattype,'Days_Above_normal') 
  }
  stattype <- c(stattype,'Number_of_days','Specific_day')
  if (!is.null(lingo)) {
    names(stattype) <- type2name(stattype,lingo,types)
  }
  print(names(stattype))
  return(stattype)
}

## Plain text description of the statistics presented on the climate indicators for ordinary people.
vari2name <- function(x,vars=c('pre','t2m','tmax','tmin',
                               'cc','ss','pp','fg','fx','sd','dd','qq','hu'),
                      vnames=c('Precipitation','Daily mean temperature',
                               'Daily max temperature','Daily min temperature',
                               'Cloud cover','Sunshine','Sea-level pressure','Wind speed',
                               'Max wind gust','Snow depth','Wind direction',
                               'Global Radiation','Humidity'),nc=3) {
  print('<C: vari2name')
  y <- x
  if (length(vars) != length(vnames)) stop("vars have different length to vnames in 'variname'")
  for (i in 1:length(x)) {
    pattern <- tolower(substr(x[i],1,nc))
    #print(pattern);print(nc)
    ii <- grep(pattern = pattern,substr(vars,1,nc))
    if (length(ii)>0) y[i] <- vnames[ii]
  }
  print(y)
  return(y)
}

monthtrends <- function(x,FUN=NULL) {
  print('monthtrends:')
  if (is.null(FUN)) 
    if (is.precip(x)) FUN='sum' else FUN='mean'
    y <- rep(NA,12); dy <- y; p <- y
    for (i in 1:12) {
      z <- subset(as.monthly(x,FUN=FUN),it=month.abb[i])
      y[i] <- trend(z,result='coef')
      dy[i] <- trend(z,result='err')
      p[i] <- trend(z,result='pval')
    }
    Y <- data.frame(trend=cbind(y,y+dy,y-dy),pval=p,month=1:12)
    print(Y)
    return(Y)
}

## Include the southern Oct-Mar rainy season in the set of seasons 
as.5seasons <- function(x) {
  rainyseason <- as.OctMar(x)
  northseasons <- as.4seasons(x)
  y <- c(northseasons,rainyseason)
}

## The start-up settings - global variables etc used in the UI and server. Supports several languages
print('--- <Initial settings> ---')
datainfo <- getdatainfo()
## Defaults
verbose <-FALSE                     ## For debugging
lingo <- 1                         ## Default language option                
#firstlocation <- 'Oslo - blind'   ## Default location
#zoom <- 5                         ## Default zooming in the map

## Get a list of files with data - Get the file names of the data
#fnames <- list.files(path='data',pattern='.nc',full.names = TRUE)
fnames <- names(datainfo)
print(fnames)
if (length(fnames)==0) stop(paste('OpenClimateData: there is a problem - there are no data files in data!'))
dots <- gregexpr('.',fnames,fixed=TRUE)
#src <- fnames
#for (i in 1:length(fnames)) src[i] <- substr(fnames[i],dots[[i]][1]+1,dots[[i]][2]-1)
src <- unlist(lapply(datainfo,function(x) attr(x,'src')))
src <- rownames(table(src))
print(src)

## Start with data from Met Norway or the first in alphabetic order in not found.
if (length(src) > 1) { 
  reg1 <- (1:length(src))[is.element(src,'Norway')]                          ## Default source of dataset/region
  if (length(reg1)==0) reg1 <- 1 else if (is.na(reg1)) reg1 <- 1
  fnames <- fnames[grep('.nc',fnames,fixed=TRUE)]
  fnames <- fnames[grep(src[reg1],fnames)]
} else reg1 <- 1

## Extract variables
#varids <- list.files(path='data',pattern='.nc',full.names = FALSE)
varids <- substr(fnames,6,nchar(fnames))
varids <- varids[grep(src[reg1],fnames)]
varids <- substr(varids,1,regexpr('.',varids,fixed=TRUE)-1)
#print(varids)

## Setting for menues etc. 
ci <- c(1:length(varids)); names(ci) <- vari2name(varids)
print(ci); print(fnames); print(varids)

## Extract information about summary statistics from the netCDF-files
ipre <- ci[varids=='precip']
print(ipre)
stattype <- getstattype(fnames[ipre])
print(stattype); print(varids)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


descrlab <- c('Forklaring:','Forklaring:','Description:')
decade <- c('tiår','tiår','decade')
degree <- c('grader','grader','degrees')
yr <- c('år','år','year')
yrs <- c('år','år','years')
days <- c('dager','dagar','days')

## Types of statistics
types <- c("altitude","first.year","lastrains","lastdry","last.year","latitude","longitude","max",           
           "mean","min","number.valid","records","trend","trend_wetfreq","trend_wetmean", 
           "wetfreq","wetmean","Number_of_days","Specific_day","sd","lows","sigma2",
           "trend_sigma2","10.year.return.value","mean_drydur","mean_wetdur","Days_above_normal",
           "prob_long_wet","prob_long_dry")

## Seasons for the statistics presented in the maps
sea <- c('All year'='all','Dec-Feb'='DJF',
         'Mar-May'='MAM','Jun-Aug'='JJA','Sep-Nov'='SON','Oct-Mar'='ONDJFM')
## Seasons to present in the time series plots
seaTS <- c('All year'='all','Dec-Feb'='DJF',
           'Mar-May'='MAM','Jun-Aug'='JJA','Sep-Nov'='SON','Oct-Mar'='ONDJFM',month.abb)

## Thesholds
#thresholds <- seq(10,50,by=10)

timespace <- c('Annual_cycle_month','Annual_cycle_day','Annual_cycle_cumugram','Data_matrix',
               'Histogram_location','Histogram_map','Trends','IDF')
timespacenames <- rbind(c('Månedstatistikk','Døgnstatistikk','Oppsummert år','Dag og år',
                          'Stedsstatistikk','Alle stedene','Endringer','IVF'),
                        c('Månedstatistikk','Døgnstatistikk','Oppsummert år','Dag og år',
                          'Stedsstatistikk','Alle stadane','Trendar','IVF'),
                        c('Monthly statistics','Daily statistics','Cumulated daily','Year and day',
                          'Histogram','Spatial statstics','Monthly trends','IDFs'))
timespacedescr <- rbind(c('Sesongvariasjon på månedsbasis for','Sesongvariasjon på dagsbasis for',
                          'Dager oppsummert','Dag og år','Histogram for','Statistikk for steder vist på kartet',
                          'Estimert endring/trend for hver av de 12 månedene',
                          'Tilnærmet intensitet-varighet-frekvens-analyse for'),
                        c('Sesongvariasjon på månedsbasis for','Sesongvariasjon på dagsbasis for',
                          'Dagar oppsummert','Dag og år','Histogram for','Statistikk for stadar vist på kartet',
                          'Estimert endring/trend for kvar av dei 12 månedane',
                          'Tilnærma intensitet-varighet-frekvens analyse for'),
                        c('Annual cycle (monthly) for','Annual cycle (daily) for',
                          'Days accumulated','Day and year','Histogram for','Histogram for the locations shown in map',
                          'Estimated trends for each of the 12 calendar months',
                          'Approximated intensity-duration-frequency analysis for'))

names(timespace) <- timespacenames[1,]

## Set the optional labels and titles in the menu based on chosen language 
languages <- 1:3; language.names <- c('Bokmål','Nynorsk','English')
maintitle <- c('Klimamålinger','Klimamælinger','Climatology')
maptitle <- c('Velg sted','Vel stad','Map of stations')
tstitle <- c('Historisk vær','Historisk vêr','Past weather')
etitle <- c('Utforsk','Utforsk','Explore')
htitle <- c('Værtatistikk','Vêrstatistikk','Past climate')
cftitle <- c('Om portalen & Tilbakemeldinger','Om portalen & Tilbakemeldingar','About & feedback')
lab.timescale <- c("Tidsskala","Tidsskala","Time scale")
lab.season <- c("Årstid","Årstid","Season")
lab.timespace <- c("Tidsmessig/romlig statistikk","Tidsmessig/romlig statistikk","Temporal/spatial statistics")
lab.season <- c("Årstid","Årstid","Season")
lab.highlight <- c("Uthev","Uthev","Higlight")
lab.aspect <- c("Vis","Vis","Show")
lab.timeperiod <- c("Tidsperiode","Tidsperiode","Time period")
lab.threshold <- c("Terskelverdi","Treskelverdi","Threshold")
lab.location <- c("Sted","Stad","Location")
lab.statitics <- c("Nøkkeltall","Nøkkeltal","Show quantity")
lab.date <- c("Utvalgt dato","Utvald dato","A specific day")
lab.highlight <- c('Uthev','Uthev','Highlight')
lab.exclude <- c('Ekcluder','Eksluder','Exclude')
relationstitle <- c("Relasjoner","Samanhengar","Relations")
aspectsP <- c("sum","wetfreq","wetmean","Number_of_days")
aspectnameP <- rbind(c("Nedbørsmengde","Nedbørsfrekvens","Nedbørsintensitet","Antall dager med mye nedbør"),
                     c("Nedbørsmengde","Nedbørsfrekvens","Nedbørsintensitet","Antall dagar med mykje nedbør"),
                     c("Total amount","Rain frequency","Mean rain intensity","Days with heavy rain"))
aspectsT <- c("mean","anomaly","Number_of_days","Days_above_normal")
aspectnameT <- rbind(c("Målt","Avvik fra normalen","Antall dager","Dager over normalen"),
                     c("Mæling","Avvik fra normalen","Antal dagar","Dagar over normalen"),
                     c("Measured","Anomaly","Number of days","Days above normal"))
varnames=rbind(c('Nedbør','Middeltemperatur','Maksimumstemperatur','Minimumstemperatur',
                 'Skydekke','Soltimer','Trykk','Vindhastighet',
                 'Maks vindhastighet','Snødybde','Vindretning','Stråling','Luftfuktighet'),
               c('Nedbør','Middeltemperatur','Maksimumstemperatur','Minimumstemperatur',
                 'Skydekke','Soltimar','Trykk','Vindhastighet',
                 'Maks vindhastighet','Snødybde','Vindretning','Stråling','Luftfuktighet'),
               c('Precipitation','Daily mean temperature','Daily max temperature','Daily min temperature',
                 'Cloud cover','Sunshine','Pressure','Wind speed',
                 'Wind gust','Snow depth','Wind direction','Global radiation','Air humidity'))
timescales <- rbind(c('Dag','Måned','Sesong','År'),
                    c('Dag','Måned','Sesong','År'),
                    c('Day','Month','Season','Year'))
lab.specificday <- c('Utvalgt dag','Utvald dag','Specific day')
showhideyears <- c('Enkelte år','Enkelte år','Individual years')
showyears <- rbind(c('Vis','Skjul'), c('Vis','Skjul'), c('Show','Hide'))
aspects <- aspectsP
tscales <- c("day","month","season","year"); names(tscales) <- tscales
highlighting <- c('None','New records','Top 10','Low 10')

##-----------------------------------------------------------------------------------------------------------

names(languages) <- language.names

## Get the names of locations, etc.
print('Get metadata & summary statistics')
Y <- retrieve.stationsummary(fnames[ipre])
print(Y$location)

if (sum(is.element(Y$station.id,18700))>0) stid <- 18700 else 
  stid <- Y$station.id[(Y$number.valid == max(Y$number.valid))][1] 
print(paste('Get first station: station_id=',stid))
is <- (1:length(Y$station.id))[is.element(Y$station.id,stid)]
y <- retrieve.station(fnames[ipre],is=is,verbose=verbose)
print(loc(y))
#y <- retrieve.station(fnames[1],stid=Y$station.id[Y$location=="De bilt"],verbose=verbose)

print('Get range for the sliding bar')
statisticmin <- round(min(Y$mean,na.rm=TRUE))
statisticmax <- round(max(Y$mean,na.rm=TRUE))

cntrs <- c('All',rownames(table(Y$country)))

## The variable 'filter' is used for zooming in on the data based on their range of values
filter <- rep(TRUE,length(Y$station.id))

pdf(file = NULL)

print(ci); print(varids)
print(ci[varids=='precip'])
print('--- <Settings OK> ---')



