# This script is used to prep MODIS NDVI data, extracting the NDVI at the flux tower pixel
# for each 16 day period
# MODIS.processing <- function() {
#   
# }

modis.amerflux.path <- "../../../Ameriflux/MODIS_data"
output.dir.path <- "../../data/format"

modis.meta <- NULL
modis.meta <- rbind(modis.meta,c("bartlett","MOD13Q1.fn_usnhbart.txt","X349"))
modis.meta <- rbind(modis.meta,c("chestnut","MOD13Q1.fn_uschridg.txt","X379"))
modis.meta <- rbind(modis.meta,c("dukeforest","MOD13Q1.fn_usdukehd.txt","X349"))
modis.meta <- rbind(modis.meta,c("kendall","MOD13Q1.fn_usazkend.txt","X407"))
modis.meta <- rbind(modis.meta,c("meadrainfed","MOD13Q1.fn_usmeadrf.txt","X380"))
modis.meta <- rbind(modis.meta,c("ncloblolly","MOD13Q1.fn_usnclobl.txt","X406"))
modis.meta <- rbind(modis.meta,c("santarita","MOD13Q1.fn_usazsant.txt","X379"))
modis.meta <- rbind(modis.meta,c("tonzi","MOD13Q1.fn_ustonzir.txt","X352"))

modis.ndvi <- NULL
for (i in 1:dim(modis.meta)[1]) {
#   i <- 2
  modis.data <- read.table(file.path(modis.amerflux.path,modis.meta[i,2]),sep=",",header=TRUE,stringsAsFactors=FALSE)
  modis.data$Date <- strptime(substr(modis.data$Date,2,8),"%Y %j")
  row.ind <- which(modis.data$Band == "250m_16_days_NDVI")
  col.ind <- c(which(names(modis.data)=="Date"),which(names(modis.data)==modis.meta[i,3]))
  modis.site.ndvi <- modis.data[row.ind,col.ind]
  ndvi.names <- names(modis.site.ndvi)
  ndvi.names[-1] <- "NDVI"
  #   modis.site.ndvi$ndvi <- modis.site.ndvi$ndvi/1e4
  names(modis.site.ndvi) <- ndvi.names
  modis.site.ndvi$site <- rep(modis.meta[i,1],dim(modis.site.ndvi)[1])
  
  #   if (i == 1) {modis.ndvi <- modis.site.ndvi
  #   } else {
  modis.ndvi <- rbind(modis.ndvi,modis.site.ndvi)
  # }
}

write.table(modis.ndvi,file.path(output.dir.path,"MODIS_NDVI.csv"),sep=",",col.names=TRUE,row.names=FALSE)

# ggplot(modis.ndvi,aes(Date,NDVI,col=site)) + geom_point(size=3) + geom_line() + ggtitle("NDVI at Ameriflux Sites")
