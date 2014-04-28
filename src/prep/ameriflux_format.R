#

library(lubridate)
library(chron)
library(ggplot2)
library(data.table)
source("read_ameriflux.R")
source("../../r_functions/plotobj.R")

### 1 INPUT FLUX TOWER DATA
output.dir.path <- "../../data/format"
input.dir.path <- output.dir.path

# Lodz, Poland
L2.path <- file.path("../../../Ameriflux/L2/")
# Ameriflux sites: "bartlett" "chestnut" "dukeforest" "kendall" "meadrainfed" "ncloblolly" "santarita" "tonzi"
locations <- c("bartlett","chestnut","dukeforest","kendall","meadrainfed","ncloblolly","santarita","tonzi")
# loc <- "chestnut"
# i <- 3
# loc <- "santarita"
# i <- 5
# data.source <- "AMR"
# tag <- "baseline"

#### READ IN FLUX TOWER DATA
flux.all <- NULL
for (loc in locations) {
  flux <- read.AMR(loc,L2.path)
  flux$site <- rep(loc,dim(flux)[1])
  flux.all <- rbind(flux.all,flux)
}

###### add modis date to each line
modis.ndvi <- read.table(file.path(input.dir.path,"MODIS_NDVI.csv"),header=TRUE,sep=",",stringsAsFactors=FALSE)
modis.ndvi$Date <- as.Date(strptime(modis.ndvi$Date,"%Y-%m-%d"))
flux.all$Date <- as.Date(strptime(flux.all$pdates,format="%Y-%m-%d"))

flux.all$modis_date <- rep(as.Date(NA),dim(flux.all)[1])
flux.all$NDVI <- rep(NA,dim(flux.all)[1])
for (i in 1:dim(modis.ndvi)[1]) {
  #   i <- 89
  startdate <- modis.ndvi$Date[i]
  enddate <- startdate + 16
  ind <- which(flux.all$Date >= startdate & flux.all$Date < enddate & modis.ndvi$site[i] == flux.all$site)
  flux.all$modis_date[ind] <- as.Date(modis.ndvi$Date[i])
  flux.all$NDVI[ind] <- modis.ndvi$NDVI[i]
}
#####

write.table(flux.all,file.path(output.dir.path,"ameriflux_selected_data.csv"),sep=",",row.names=FALSE)

# ggplot(flux.all) + theme() +  geom_point(aes(Rg,Rn,col=sqrt(LE)),alpha=0.25) + ggtitle("Net Radiation vs Global Shortwave Radiation") + facet_wrap(~ site)
# 
# ggplot(flux.all) + theme() +  geom_point(aes(LE,H,col=Rn)) + ggtitle("Sensible vs Latent Heat") + facet_wrap(~ site)
# ggplot(flux.all) + theme() +  geom_point(aes(Rn,H/LE,col=Ta),alpha=0.25) + ggtitle("Sensible vs Latent Heat") + facet_wrap(~ site) +
#   scale_y_continuous(limits=c(0,10))