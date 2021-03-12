#/bin/bash

## Set up R:

if ! command -v R    &> /dev/null
then
## Set up -the R-version 4.0 - recipe from https://cran.r-project.org/
  sudo apt update -qq
  sudo apt install --no-install-recommends software-properties-common dirmngr
  sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
  sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/"  
  sudo apt install --no-install-recommends r-base
  sudo add-apt-repository ppa:c2d4u.team/c2d4u4.0+
  #sudo apt-get install r-base-core
  sudo su - -c "R -e \"install.packages('shiny',repos='https://cran.rstudio.com/')\""
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
sudo apt-get install libxml2-dev
sudo su - -c "R -e \"install.packages('devtools',repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('shinydashboard',repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('leaflet',repos='https://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('plotly',repos='https://cran.rstudio.com/')\""
mkdir Rlibs

## Install needed R-packages:
cat > set-up.R << EOF
  ## This line does not work well on a VM
  ##install.packages(c('devtools','shiny','shinydashboard','leaflet','plotly'),repos='http://cran.uib.no')
  library(devtools)
  install_github('metno/esd',lib='~/Rlibs')
EOF

## Launch the app:
R --slave --no-restore -e 'source("set-up.R")'
## Let R know where the local R-libraries are
export R_LIBS=~/Rlibs:$R_LIBS
ln -s OpenClimateData/launch.sh ~/launch.sh
