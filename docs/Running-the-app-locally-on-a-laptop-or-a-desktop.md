# Get the code
You can also run this lap locally with your own data and on a Windows machine or a Mac. To do so, you need to [download the code](https://github.com/metno/OpenClimateData) (either clone with git or download the zipped code and then unzip it).

# Set up the app locally
When you have generated the folder with the R-scripts, you can use R-studio to open `OpenClimateData/global.R`. If R-studio now gives you the option `Run App` above the window with the R-code, then it should work: run the app.

# Add new data
The app comes with a limited set of sample data from GHCND. You can add your own data if you have saved it in the netCDF4 format with `esd`'s `write2ncdf4`-command. It's like plug-and-play, because you can replace the sample data with your own. If you want to add your data to the sample data, then you need to modify `global.R`. 