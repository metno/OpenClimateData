### title: OpenClimateDataPortal
### See comments and explanations in global.R
### Rasmus Benestad
###----------------------------------

titlePanel("title panel")

ui <- dashboardPage(
  skin="green",
  dashboardHeader(title = textOutput("maintitle")),
  dashboardSidebar(
    selectInput("src", "Region", 
                choices= src,selected='metnod'),
    box(textOutput("datainterval"),background='black',width=12),
    selectInput("ci", "Climate Index", choices= ci, selected=ci[varids=='precip']),
    selectInput("location", textOutput("locationlabel"), choices= Y$location,
                selected = 'Oslo - blind'),
    
    #tags$style(type="text/css", "#string { margine-left: 30px; margin-right: 30px; font-size: 30px;}"),
    selectInput("country", "Show", 
                choices= cntrs,selected='All'),
    conditionalPanel(condition="input.statistic == 'Number_of_days' || input.aspect == 'Number_of_days'",
                     numericInput("x0",textOutput("threshold"), 20)), 
    selectInput("lingo", "Language", 
                choices= languages,selected=3),
    collapsed=FALSE
  ),
  dashboardBody(
    #include google analytics
    #tags$head(HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-123370594-1'></script> # Internal
    tags$head(HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-108282573-5'></script> 
                               <script>
                               window.dataLayer = window.dataLayer || [];
                               function gtag(){dataLayer.push(arguments);}
                               gtag('js', new Date());
                               gtag('config', 'UA-108282573-5', { 'anonymize_ip': true }); 
                               </script>"
    )),
    tags$head(
      tags$style(
        HTML(".shiny-notification {
                         position:fixed;
                         top: calc(0%);;
                         left: calc(5%);;
                         }"
        )
      )
    ),
    #send information to google analytics
    #this includes the event name and the value it is set to
    #omit sending plotting information (i.e., events starting with .client)
    tags$script(HTML(
      "$(document).on('shiny:inputchanged', function(event) {
                  if (event.name.substr(1,6) !== 'client') {
                  newname = event.name+' set to '+event.value;
                  gtag('event', newname, {'event_category': 'User interaction'});
                  }
                  });"
    )), 
    
    fluidPage( 
      tabsetPanel(
        tabPanel('Map of elements',fluidPage(
          #title=textOutput("maptitle"),status = "success",collapsed = FALSE, 
          #collapsible = TRUE, width="100%", solidHeader = TRUE, 
          fluidRow(width="100%",
                   column(1),
                   column(3, selectInput("statistic", textOutput("statisticslabel"), 
                                         choices= stattype,selected='mean')),
                   column(3, selectInput("season", textOutput("seasonlabel"), choices= sea)),
                   column(2, selectInput("highlight", textOutput("highlightlabel"), choices= highlighting)),
                   
                   column(3, conditionalPanel(condition="input.statistic == 'Specific_day'",
                                              dateInput("it",textOutput("daylabel"), value=attr(Y,'period')[2]))),
                   fluidRow(leafletOutput("map",height = 700)),
                   fluidRow(box(tags$h4(textOutput('mapdescription')),width=12),
                   column(6, sliderInput("statisticrange", "Range:", min = statisticmin, max=statisticmax, 
                                         value = c(statisticmin,statisticmax),sep='',round=TRUE)),
                   column(6, sliderInput("rad", "Symbol size:", min = 1, max=10, value = 4,sep=''))))
        )
        ),
        tabPanel('Past Weather',fluidPage(
          #box(title=textOutput("tstitle"),status = "success",collapsed = TRUE,
          #    collapsible = TRUE, width="100%", solidHeader = TRUE,
          fluidRow(plotlyOutput("plotstation", height = 500,width = '100%')),
          fluidRow(box(textOutput("cntr"),background='light-blue',width=14)),
          fluidRow(column(2,selectInput("tscale", textOutput("timescalelabel"), choices= tscales, selected='year')),
                   column(3,selectInput("aspect", textOutput("aspectlabel"), choices= aspects,selected = aspects[1])),
                   column(2,selectInput("highlightTS", textOutput("highlightTSlabel"), choices= highlighting)),
                   column(2,selectInput("seasonTS", textOutput("seasonTSlabel"), choices= seaTS)),
                   column(3,sliderInput("itt", "Period:", min = min(year(y)), max=max(year(y)), 
                          value = c(min(year(y)),max(year(y))),sep='',round=TRUE))
                   # conditionalPanel(condition="tscale == 'day'",
                   #                  selectInput("seasonTS", textOutput("seasonTSlabel"), choices= seaTS))
          ))
        ),
        tabPanel('Statistics & climate',fluidPage(
          #box(title=textOutput("htitle"),status = "success",collapsed = TRUE,
          #    collapsible = TRUE, width="100%", solidHeader = TRUE,
          fluidRow(plotlyOutput("histstation", height = 500,width = '100%')),
          fluidRow(box(textOutput("hdes"),background='light-blue',width=14),
                 selectInput("timespace", textOutput("timespacelabel"), choices= timespace,selected = timespace[1]),
                 conditionalPanel(condition="input.timespace == 'Annual_cycle_day'",
                                  radioButtons("showlegend", textOutput("yearlabel"), choices = c('Show','Hide'),
                                               selected = "Hide",
                                               inline = FALSE, width = NULL, choiceNames = NULL, choiceValues = NULL))
          ))
        ),
        tabPanel('Explore',fluidPage(
          fluidRow(plotlyOutput("scatterplot", height = 500,width = '100%')),
          fluidRow(column(3,selectInput("x_variable", "X", choices= stattype,selected = "mean")),
                   column(3,selectInput("y_variable", "Y", choices= stattype,selected = "altitude")),
                   column(3,selectInput("xy_col", "Colouring", choices= c('Red',stattype),selected = "None")),
                   column(3,selectInput("xy_size", "Marker size", choices= c('Uniform',stattype),selected = "Uniform"))
          ))
        ),
        tabPanel('About/Disclaimer',fluidPage(
          #box(title=textOutput("cftitle"),status = "success",collapsed = TRUE, 
          #    collapsible = TRUE, width="100%", solidHeader = TRUE, 
          tags$div(class="header", checked=NA,
                   tags$h1("About"),
                   tags$p("This is an experimental prototype and a blue print for an app that is meant to be used together", 
                          "with climate services. The data presented may be subject to errors and this exploration tool is", 
                          "mean to experimental purposes rather than an official data site. We have tried to design this app", 
                          "in a clever way so that it is smart enough to analyse netCDF files containing the station data and", 
                          "use that information for its menus and set-up. It also is designed to be as fast as flexible as ",
                          "possible by only reading the data it needs for the visualisation - to minimize the memory requirements.", 
                          "The summary statistics have",
                          "allready been pre-calulated and are stored in the netCDF files together with the data. It has also been",
                          "designed  to allow for mltiple languages."),
                   tags$p("The main 'engine' behind this app is the",
                          tags$a(href="https://github.com/metno/esd/wiki","'esd' R-package,"),
                          "avalable from",tags$a(href="https://github.com/metno/esd","github.com/metno/esd"),
                          ". The source code for this app is available from ",
                          tags$a(href="https://github.com/metno/OpenClimateData","github.com/metno/OpenClimateData")),
                   tags$br(),
                   tags$h1("Data"),
                   tags$p("The data from the Norwegian Meteorological Institute are available from ",
                          tags$a(href="http://thredds.met.no/thredds/catalog/metusers/rasmusb/catalog.html",
                                 "http://thredds.met.no/thredds/catalog/metusers/rasmusb/catalog.html")),
                   tags$br(),
                   tags$h1("Help, tips and assistance"),
                   tags$p("We have used a wiki-page to",
                          tags$a(href="https://github.com/metno/OpenClimateData/wiki","help"),
                          "you to install this prototype and to provide more information about it. Please use the ",
                          tags$a(href="https://github.com/metno/OpenClimateData/issues","'Issues'"), " page if you have any problem that is not described on he wiki-page"),
                   tags$h1("Comments and feedbacks"),
                   tags$p("Do you have any comments or feedbacks? If so, please post an isssue on",
                          tags$a(href="https://github.com/metno/OpenClimateData/issues","GitHub"))
          )
        ))
      )
    )
  )
)

      
      
      
      
      