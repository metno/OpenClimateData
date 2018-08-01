cat > set-up.R << EOF
  install.packages(c('devtools','shiny','shinydashboard','leaflet','plotly'),repos='http://cran.uib.no')
  library(devtools)
  install_github('metno/esd')
EOF

#sudo apt-get install -f
#sudo apt-get install libnetcdf-dev
#sudo apt-get install liblapack-dev
R --slave --no-restore -e 'source("set-up.R")'
ln -s OpenClimateData/launch.sh 
