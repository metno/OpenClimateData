#!/bin/sh
cd $(dirname $0) && \
wget -N -e robots=off -nH --cut-dirs 4 -r -l5 -A '*.metnod.nc' -R 'catalog*' -I /thredds/fileServer/,/thredds/catalog/ 'https://thredds.met.no/thredds/catalog/metusers/rasmusb/catalog.html' -P ./data
