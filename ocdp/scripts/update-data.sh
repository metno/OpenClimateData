#/bin/bash
## Batch job for the R-script
R CMD BATCH $(dirname $0)/update-data.R
