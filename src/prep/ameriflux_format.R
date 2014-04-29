#

# output.dir.path <- "../../data/format"
# locations <- c("Bartlett","Chestnut","Duke_Forest_H","Kendall","Mead_R","North_Carolina_Loblolly_P","Santa_Rita_M","Tonzi")
# L2.path <- "../../../Ameriflux/L2"
# sites <- system(paste("ls",L2.path),intern=TRUE)
# selected.sites <- sites[pmatch(locations,sites)]
# write.table(data.frame(sites=selected.sites),file.path(output.dir.path,"selected_sites.txt"),row.names=FALSE)

library(lubridate)
library(chron)
library(ggplot2)
library(data.table)
source("read_ameriflux.R")
source("../../r_functions/plotobj.R")

### 1 INPUT FLUX TOWER DATA

L2.path <- "../../../Ameriflux/L2"
sel.sites <- read.table("../../data/format/selected_sites.txt",header=TRUE,stringsAsFactors=FALSE)[,1]

#### READ IN FLUX TOWER DATA
flux.daytime <- NULL
for (site in sel.sites) {
  flux <- read.l2.ameriflux.site(file.path(L2.path,site))
  flux$site <- rep(loc,dim(flux)[1])
  
  dt.flux <- data.table(flux)
  flux.daytime <- as.data.frame(dt.flux[Rn > 0,])
  
  flux.all.daytime <- rbind(flux.all.daytime,flux)
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