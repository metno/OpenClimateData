#/bin/bash
R --slave --no-restore -e 'shiny::runApp("OpenClimateData/", launch.browser=TRUE)'