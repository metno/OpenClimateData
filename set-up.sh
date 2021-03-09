#/bin/bash

## Set up R:
if ! command -v R    &> /dev/null
then
  sudo apt-get install r-base-core
  sudo su - -c “R -e \”install.packages(‘shiny’,repos=’https://cran.rstudio.com/’)\””
fi

## Set up the shiny-server:
if ! command -v gdebi 	&> /dev/null
then
  sudo apt-get install gdebi-core
fi

wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.7.907-amd64.deb
sudo gdebi shiny-server-1.5.7.907-amd64.deb

## Install necessary libraries
sudo apt-get install -f
sudo apt-get install libnetcdf-dev
sudo apt-get install liblapack-dev
sudo apt-get install libssl-dev
sudo apt-get install libgit2-dev

## Install needed R-packages:
cat > set-up.R << EOF
  install.packages(c('devtools','shiny','shinydashboard','leaflet','plotly'),repos='http://cran.uib.no')
  library(devtools)
  install_github('metno/esd')
EOF

## Launch the app:
R --slave --no-restore -e 'source("set-up.R")'
ln -s OpenClimateData/launch.sh 
