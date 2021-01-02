## Script that copies latest netCDF files for stations from thredds         
## Rasmus.Benestad@met.no, 2018-10-14          
## /thredds/fileServer/metusers/rasmusb/t2m.metnod.nc
## https://thredds.met.no/thredds/fileServer/metusers/rasmusb/t2m.metnod.nc
wget http://thredds.met.no/thredds/fileServer/metusers/rasmusb/dd.metnod.nc dd.metnod.nc
wget http://thredds.met.no/thredds/fileServer/metusers/rasmusb/fx.metnod.nc fx.metnod.nc
wget http://thredds.met.no/thredds/fileServer/metusers/rasmusb/precip.metnod.nc precip.metnod.nc
wget https://thredds.met.no/thredds/fileServer/metusers/rasmusb/t2m.metnod.nc t2m.metnod.nc 
wget http://thredds.met.no/thredds/fileServer/metusers/rasmusb/tmin.metnod.nc tmin.metnod.nc
wget http://thredds.met.no/thredds/fileServer/metusers/rasmusb/fg.metnod.nc fg.metnod.nc
wget http://thredds.met.no/thredds/fileServer/metusers/rasmusb/pp.metnod.nc pp.metnod.nc
wget http://thredds.met.no/thredds/fileServer/metusers/rasmusb/sd.metnod.nc sd.metnod.nc
wget http://thredds.met.no/thredds/fileServer/metusers/rasmusb/tmax.metnod.nc tmax.metnod.nc
mv *.metnod.nc ~/OpenClimateData/data/.
sudo systemctl restart shiny-server
