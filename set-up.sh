cat > set-up.R << EOF
  install.packages(c('devtools','shiny','shinydashboard','leaflet','plotly'),repos='http://cran.uib.no')
  library(devtools)
  install_github('metno/esd')
EOF

R --slave --no-restore -e 'source("set-up.R")'
ln -s OpenClimateData/launch.sh 
