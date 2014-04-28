# THIS CODE IS FOR TIME-AVERAGING AMERIFLUX DATA

library(lubridate)
library(chron)
library(ggplot2)
library(data.table)
# source("read_ameriflux.R")
source("../../r_functions/plotobj.R")

### INPUT AMERIFLUX AND MODIS NDVI DATA
input.dir.path <- "../../data/format"
flux.data <- read.table(file.path(input.dir.path,"ameriflux_selected_data.csv"),header=TRUE,sep=",",stringsAsFactors=FALSE)
flux.data$Date <- as.Date(strptime(flux.data$pdates,format="%Y-%m-%d"))
flux.data$modis_date <- rep(as.Date(NA),dim(flux.data)[1])
modis.ndvi <- read.table(file.path(input.dir.path,"MODIS_NDVI.csv"),header=TRUE,sep=",",stringsAsFactors=FALSE)
modis.ndvi$Date <- as.Date(strptime(modis.ndvi$Date,"%Y-%m-%d"))


class(flux.data$pdates)


#### average the data to 16-day periods
for (i in 1:dim(modis.ndvi)[1]) {
  #   i <- 89
  startdate <- modis.ndvi$Date[i]
  enddate <- startdate + 16
  ind <- which(flux.data$Date >= startdate & flux.data$Date < enddate & modis.ndvi$site[i] == flux.data$site)
  flux.data$modis_date[ind] <- as.Date(modis.ndvi$Date[i])
}
# 
# ggplot(flux.data,aes(Date,LE,col=site)) + geom_point()
# 
# table(flux.data$site)
# table(modis.ndvi$site)
# 
# table(ind)
# 
# table(!is.na(flux.data$modis_date))
