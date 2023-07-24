#/bin/bash
## Batch job for the R-script
echo "Update data for OpenClimateData R-shiny app"
R CMD BATCH ~/OpenClimateData/all2nc.R
