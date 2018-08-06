## See comments and explanations in global.R
## Rasmus Benestad
# Load libraries

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
###----------------------------------

ui <- dashboardPage(
  skin="green",
  dashboardHeader(title = textOutput("maintitle")),
  dashboardSidebar(
    selectInput("ci", "Climate Index", choices= ci),
    selectInput("lingo", "Language", 
                choices= languages,selected='English'),
    selectInput("src", "Region", 
                choices= src,selected='ecad'),
    conditionalPanel(condition="input.statistic == 'Number_of_days' || input.aspect == 'Number_of_days'",
                     numericInput("x0",textOutput("threshold"), 0))
    ),
  dashboardBody(
    fluidPage( 
      box(title=textOutput("maptitle"),status = "success",collapsed = FALSE, 
          collapsible = TRUE, width="100%", solidHeader = TRUE, 
          column(9,leafletOutput("map",height = 700)),
          column(3,selectInput("location", textOutput("locationlabel"), choices= Y$location), 
                 selectInput("statistic", textOutput("statisticslabel"), 
                             choices= stattype,selected='mean'),
                 tags$h3(textOutput('mapdescription')),
                 selectInput("season", textOutput("seasonlabel"), choices= sea),
                 conditionalPanel(condition="input.statistic == 'Specific_day'",
                                   dateInput("it",textOutput("daylabel"), value=Sys.Date()-1)),
                 selectInput("higlight", textOutput("highlightlabel"), choices= higlighting),
                 sliderInput("statisticrange", "Range:", min = statisticmin, max=statisticmax, 
                             value = c(statisticmin,statisticmax),sep='',round=TRUE)
                 )
      )
    ),
    fluidPage(
      box(title=textOutput("tstitle"),status = "success",collapsed = TRUE,
          collapsible = TRUE, width="100%", solidHeader = TRUE,
          column(9, plotlyOutput("plotstation", height = 500,width = '100%')),
          column(3,
                 selectInput("tscale", textOutput("timescalelabel"), choices= tscales, selected='year'),
                 selectInput("aspect", textOutput("aspectlabel"), choices= aspects,selected = aspects[1]),
                 selectInput("highlightTS", textOutput("highlightTSlabel"), choices= higlighting),
                 selectInput("seasonTS", textOutput("seasonTSlabel"), choices= seaTS)
                 # conditionalPanel(condition="tscale == 'day'",
                 #                  selectInput("seasonTS", textOutput("seasonTSlabel"), choices= seaTS))
          ))
    ),
    fluidPage(
      box(title=textOutput("htitle"),status = "success",collapsed = TRUE,
          collapsible = TRUE, width="100%", solidHeader = TRUE,
          column(9, plotlyOutput("histstation", height = 500,width = '100%')),
          column(3,
                 selectInput("timespace", textOutput("timespacelabel"), choices= timespace,selected = timespace[1]) #,
                 #dateRangeInput('dateRange',
                 #                label = textOutput("timeperiodlabel"),
                 #              start = as.Date('1951-01-01'),
                 #             end = date(),startview = 'year')
          ))
    ),
  fluidPage(
    box(title=textOutput("cftitle"),status = "success",collapsed = TRUE, 
        collapsible = TRUE, width="100%", solidHeader = TRUE, 
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
                 tags$a(href="https:github.com/metno/esd/wiki","'esd' R-package,"),
                 "avalable from",tags$a(href="https:github.com/metno/esd","github.com/metno/esd"),
                 ". The source code for this app is available from ",
                 tags$a(href="https:github.com/metno/OpenClimateData","github.com/metno/OpenClimateData")),
                 tags$br(),
                 tags$h1("Comments and feedbacks"),
                 tags$p("Do you have any comments or feedbacks? If so, please respond through this",
                 tags$a(href="https://goo.gl/forms/GuzqO1GIUFfz5L2K2","link"))
        )
    ))
))



