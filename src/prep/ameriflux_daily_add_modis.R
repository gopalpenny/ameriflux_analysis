#ameriflux_daily_add_modis

format.path <- "../../data/format"
sel.sites <- read.table(file.path(format.path,"selected_sites.txt"),header=TRUE,sep=",",stringsAsFactors=FALSE)

#### READ DATA
# read daily data
daily.path <- file.path(format.path,"sel_ameriflux_daily.csv")
daily <- read.table(daily.path,sep=",",header=TRUE,stringsAsFactors=FALSE)
daily$DATE <- as.Date(strptime(daily$DATE,"%Y-%m-%d"))

# read daytime data
daytime.path <- file.path(format.path,"sel_ameriflux_daytime.csv")
daytime <- read.table(daytime.path,sep=",",header=TRUE,stringsAsFactors=FALSE)
daytime$DATE <- as.Date(strptime(daytime$DATE,"%Y-%m-%d"))

##### read modis timeseries
modis.ndvi <- read.table(file.path(format.path,"MODIS_NDVI.csv"),header=TRUE,sep=",",stringsAsFactors=FALSE)
modis.ndvi$Date <- as.Date(strptime(modis.ndvi$Date,"%Y-%m-%d"))


###### CREATE MODIS DATA FRAME WITH NDVI FOR EACH DATE
# linearly interpolate modis dataset for each site


modis.ndvi.daily <- NULL
##### LINEARLY INTERPOLATE 16-DAY NDVI DATA
for (site in sel.sites$sites) {
  modis.ndvi.site <- modis.ndvi[which(modis.ndvi$site==site),]
  startdate <- min(modis.ndvi.site$Date)
  enddate <- max(modis.ndvi.site$Date)
  modis.ndvi.site.daily <- as.data.frame(approx(modis.ndvi.site$Date,y=modis.ndvi.site$NDVI,xout=seq(startdate,enddate,by=1)))
  names(modis.ndvi.site.daily) <- c("DATE","NDVI")
  modis.ndvi.site.daily$site.name <- rep(site,dim(modis.ndvi.site.daily)[1])
  modis.ndvi.daily <- rbind(modis.ndvi.daily, modis.ndvi.site.daily)
  #   ggplot(NULL) + geom_point(data=modis.ndvi.site.daily,aes(Date,NDVI),col="#FF0000",shape=3) + geom_point(data=modis.ndvi.site,aes(Date,NDVI),col="#00FF00",size=3)
}

daytime.modis <- merge(daytime,modis.ndvi.daily,by=c("site.name","DATE"),all=TRUE)
daily.modis <- merge(daily,modis.ndvi.daily,by=c("site.name","DATE"),all=TRUE)

write.table(daytime.modis,file.path(format.path,"sel_ameriflux_daytime_modis.csv"),sep=",",row.names=FALSE,col.names=TRUE)
write.table(daily.modis,file.path(format.path,"sel_ameriflux_daily_modis.csv"),sep=",",row.names=FALSE,col.names=TRUE)
