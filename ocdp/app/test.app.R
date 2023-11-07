##https://shiny.rstudio.com/articles/debugging.html
library(shiny)
options(shiny.reactlog=TRUE)
shiny::runApp('~/OpenClimateData',display.mode="showcase")
#shiny::runApp('~/OpenClimateData')
## Press control-F3 in th app..