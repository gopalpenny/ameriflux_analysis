MODIS.processing <- function() {
  
}

modis.amerflux.path <- "../../../Ameriflux/MODIS"

modis.meta <- NULL
modis.meta <- rbind(modis.meta,c("santarita","MOD13Q1.fn_usazsant.txt","X379"))
modis.meta <- rbind(modis.meta,c("mead_rainfed","MOD13Q1.fn_usmeadrf.txt","X380"))
modis.meta <- rbind(modis.meta,c("nc_loblolly","MOD13Q1.fn_usnclobl.txt","X406"))
modis.meta <- rbind(modis.meta,c("tonzi","MOD13Q1.fn_ustonzir.txt","X352"))
modis.meta <- rbind(modis.meta,c("chestnut","MOD13Q1.fn_uschridg.txt","X379"))
modis.meta <- rbind(modis.meta,c("duke","MOD13Q1.fn_usdukehd.txt","X349"))
modis.meta <- rbind(modis.meta,c("kendall","MOD13Q1.fn_usazkend.txt","X407"))

modis.ndvi <- data.frame(dates=c())
for (i in 1:dim(modis.meta)[1]) {
  i <- 2
  modis.data <- read.table(file.path(modis.amerflux.path,modis.meta[i,2]),sep=",",header=TRUE,stringsAsFactors=FALSE)
  modis.data$dates <- strptime(substr(modis.data$Date,2,8),"%Y %j")
  row.ind <- which(modis.data$Band == "250m_16_days_NDVI")
  col.ind <- c(which(names(modis.data)=="dates"),which(names(modis.data)==modis.meta[i,3]))
  modis.site.ndvi <- modis.data[row.ind,col.ind]
  ndvi.names <- names(modis.site.ndvi)
  ndvi.names[-1] <- modis.meta[i,1]
  names(modis.site.ndvi) <- ndvi.names
  
  if (i == 1) {modis.ndvi <- modis.site.ndvi
  } else {modis.ndvi <- merge(modis.ndvi,modis.site.ndvi,by="dates")}
}

ggplot(modis.site.ndvi,aes(dates,ndvi/10000)) + geom_point(size=3) + geom_line()
