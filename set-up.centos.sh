#/bin/bash

## Set up R:

if ! command -v R    &> /dev/null
then
## Set up -the R-version 4.0 - recipe from https://cran.r-project.org/
  su yum update -qq
  su yum install --no-install-recommends software-properties-common dirmngr
  su yum-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
  #su yum upgrade "deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/"  
  su yum install --no-install-recommends r-base
  #sudo add-apt-repository ppa:c2d4u.team/c2d4u4.0+
  su yum install r-base-core
  sudo su - -c "R -e \"install.packages('shiny',repos='https://cran.rstudio.com/')\""
fi

## Set up the shiny-server:
if ! command -v gdebi 	&> /dev/null
then
  su yum install gdebi-core
fi

wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.7.907-amd64.deb
sudo gdebi shiny-server-1.5.7.907-amd64.deb

## Install necessary libraries
su yum install -f
su yum install libnetcdf-dev
su yum install liblapack-dev
su yum install libssl-dev
su yum install libgit2-dev
su yum install libxml2-dev
su yum install firefox
su su - -c "R -e \"install.packages('devtools',repos='https://cran.rstudio.com/')\""
su su - -c "R -e \"install.packages('shinydashboard',repos='https://cran.rstudio.com/')\""
su su - -c "R -e \"install.packages('leaflet',repos='https://cran.rstudio.com/')\""
su su - -c "R -e \"install.packages('plotly',repos='https://cran.rstudio.com/')\""
su su - -c "R -e \"install.packages('rmarkdown',repos='https://cran.rstudio.com/')\""
su su - -c "R -e \"install.packages('zoo',repos='https://cran.rstudio.com/')\""
su su - -c "R -e \"install.packages('ncdf4',repos='https://cran.rstudio.com/')\""
mkdir Rlibs

## Install needed R-packages:
cat > set-up.R << EOF
  ## This line does not work well on a VM
  install.packages(c('devtools','shiny','shinydashboard','leaflet','plotly'),repos='http://cran.uib.no')
  library(devtools)
  install_github('metno/esd')
EOF

## Launch the app:
R --slave --no-restore -e 'source("set-up.R")'
## Let R know where the local R-libraries are
export R_LIBS=~/Rlibs:$R_LIBS
## Test the app
ln -s OpenClimateData/launch.sh ~/launch.sh
su cp OpenClimateData/shiny-server.conf /etc/shiny-server/shiny-server.conf
./launch.sh
## Start the shiny-server
#/etc/shiny-server/shiny-server.conf
#location /name_of_the_app {
#
#	# Host the shiny app in this directory
#	app_dir /home/ubuntu/git/esd_Rshiny/path_to_apps_folder;
#
#	# Log all shiny output from the app to this directory
#	log_dir /var/log/shiny-server;
#  }
# sudo systemctl restart shiny-server
## Maintanance:
# sudo systemctl restart shiny-server




