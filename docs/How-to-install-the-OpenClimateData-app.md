This app can be installed on a local machine (Linux) or a R shiny-server so that it generates a web solution to present climate station data. It may also be installed on a Windows machine and a Mac, and the easiest way to do so is via GitHub (see [how to install Git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git) on Windows or [Git for Windows](https://gitforwindows.org/))

To install: 

```r
git clone https://github.com/metno/OpenClimateData.git
```

You can now use the app if you have R-studio installed (e.g. on Windows or Mac) and open either `global.R`, `ui.R` or `server.R` in the main directory `OpenClimateData` by running the app in R-studio. It's also possible to add your own data by adding netCDF files with station data in `OpenClimateData/data` with a similar structure and format as the sample data already provided. This may require some lines of R-code and the [R-package `esd`](https://github.com/metno/esd):

```r
> library(esd)
> X <- read.table(<datafile>)
> write2ncdf4(X,file=<filename>)
```

Here '<...>' means that you replace it with an actual file name such as "myfilename". The filenames of netCDF files have the suffix '.nc' and the app uses a strict name convention: `<element>.<source>.nc`. Element is in the set ['t2m' ,tmax', 'tmin', 'precip', 'sd', 'pp', 'fx', 'fg', 'dd'], and this set is defined in `global.R` (it's fairly straight-forward to add your own elements here). Likewise, the source is predefined: ['Africa', 'ecad', 'Australia', 'Asia', 'LatinAmerica', 'USA', 'Pacific', 'metnod',' 'eustance']. 

## Linux (Ubuntu): 

You may have to install some libraries before you can set up this app:

```r
sudo apt-get install -f
sudo apt-get install libnetcdf-dev
sudo apt-get install liblapack-dev
```

Then to set up on the local machine, try:

```r
sudo OpenClimateData/set-up.sh
```

Notice that the app is set up to allow google analytics to keeps statistics of the visits to the app if it's run on a server. This is done in the file `ui.R`, and needs to be changed if run through a web-site (for help on setting up the app on a shiny-server, see [tips on setting up a shiny-server](https://github.com/metno/OpenClimateData/wiki/Setting-up-a-shiny-server)).

If everything worked, you should be able to run the app in a test-mode by typing:
```r
./launch.sh
```

A [demo video](https://youtu.be/RHwznAzMp1g) providing a screencast of the process is available at YouTube (Linux)

# How to set up the app together with a shiny server (Ubuntu):

The set-up should now be simplified through one script.  

These are the command lines needed to set up the OpenClimateDataPrototype with a shiny-server on Ubuntu: 

```r2
git clone https://github.com/metno/OpenClimateData.git
sudo ./OpenClimateData/set-up.sh
```

In addition, there are further steps needed to set up SSL-certificate, i.e. giving thr IP-address a name within [DNS](https://en.wikipedia.org/wiki/Domain_Name_System) protocol.

More help on setting up shiny servers are provided [here](https://docs.rstudio.com/shiny-server).
