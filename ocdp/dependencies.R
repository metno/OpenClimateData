packages <- c(

  'crayon',
  'leaflet',
  'memoise',
  'ncdf4',
  'plotly',
  'rmarkdown',
  'shiny',
  'shinydashboard',
  'yaml',
  'zoo',

'')
options(Ncpus = -1)
for (pkg in packages) {
  if (pkg == '') {
    next
  }
  install.packages(pkg, repos='http://cran.rstudio.com')
  if ( ! library(pkg, character.only=TRUE, logical.return=TRUE) ) {
    quit(status=1, save='no')
  }
}
options(timeout=1800)
library(devtools)
install_github('metno/esd')
if ( ! library('esd', character.only=TRUE, logical.return=TRUE) ) {
  quit(status=1, save='no')
}
