### Packages & installation
library(ade4)
library(adegenet)
library(akima)
library(dplyr)
library(drake)
library(esri2sf)
library(geosphere)
library(ggmap)
library(ggplot2)
library(lme4)
library(lubridate)
library(maps)
library(maptools)
library(poppr)
library(raster)
library(RColorBrewer)
library(rgdal)
library(RSelenium)
library(rvest)
library(sf)
library(sp)
library(spdep)
library(tidyr)
library(vcfR)

### Selenium install
system("gksudo docker install selenium/standalone-firefox")

#### Start RSelenium
# make sure the docker version of selenium is installed first!
system("gksudo docker run -d -p 4445:4444 selenium/standalone-firefox")
