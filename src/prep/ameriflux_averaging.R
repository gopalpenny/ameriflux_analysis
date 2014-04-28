# THIS CODE IS FOR TIME-AVERAGING AMERIFLUX DATA

library(lubridate)
library(chron)
library(ggplot2)
library(data.table)
# source("read_ameriflux.R")
source("../../r_functions/plotobj.R")



### INPUT AMERIFLUX AND MODIS NDVI DATA
input.dir.path <- "../../data/format"
output.dir.path <- input.dir.path
flux.data <- read.table(file.path(input.dir.path,"ameriflux_selected_data.csv"),header=TRUE,sep=",",stringsAsFactors=FALSE)
flux.data$modis_date <- rep(as.Date(NA),dim(flux.data)[1])
modis.ndvi <- read.table(file.path(input.dir.path,"MODIS_NDVI.csv"),header=TRUE,sep=",",stringsAsFactors=FALSE)
modis.ndvi$Date <- as.Date(strptime(modis.ndvi$Date,"%Y-%m-%d"))

# #### average the data to 16-day periods
# for (i in 1:dim(modis.ndvi)[1]) {
#   #   i <- 89
#   startdate <- modis.ndvi$Date[i]
#   enddate <- startdate + 16
#   ind <- which(flux.data$Date >= startdate & flux.data$Date < enddate & modis.ndvi$site[i] == flux.data$site)
#   flux.data$modis_date[ind] <- as.Date(modis.ndvi$Date[i])
# }

flux.data$key.factor <- as.factor(paste(flux.data$site,flux.data$modis_date,sep=""))
modis.ndvi$key.factor <- as.factor(paste(modis.ndvi$site,modis.ndvi$Date,sep=""))
dt.flux <- data.table(flux.data)
# setkey(dt.flux,modis_date,site)
dt.flux_16day <- as.data.frame(dt.flux[,list(Rn=mean(Rn,na.rm=TRUE),Rg=mean(Rg,na.rm=TRUE),LE=mean(LE,na.rm=TRUE), 
                                             Prec=sum(Prec,na.rm=TRUE),H=mean(H,na.rm=TRUE),Date=max(modis_date,na.rm=TRUE),
                                             site=max(site)),by=key.factor])

flux.modis <- merge(dt.flux_16day,modis.ndvi,by=c("site","Date","key.factor"))

write.table(flux.modis,file.path(output.dir.path,"Ameriflux_MODIS_16day.csv"),sep=",",col.names=TRUE,row.names=FALSE)
# 
# ggplot(flux.data,aes(Date,LE,col=site)) + geom_point()
# 
# table(flux.data$site)
# table(modis.ndvi$site)
# 
# table(ind)
# 
# table(!is.na(flux.data$modis_date))
