# This script is used to prep MODIS NDVI data, extracting the NDVI at the flux tower pixel
# for each 16 day period
# MODIS.processing <- function() {
#   
# }

modis.amerflux.path <- "../../../Ameriflux/MODIS_data"
format.path <- "../../data/format"
output.dir.path <- "../../data/format"

modis.filenames <- read.table(file.path(format.path,"modis_filenames.csv"),stringsAsFactors=FALSE,sep=",")

modis.ndvi <- NULL
for (i in 1:dim(modis.filenames)[1]) {
#   i <- 2
  modis.data <- read.table(file.path(modis.amerflux.path,modis.filenames[i,2]),sep=",",header=TRUE,stringsAsFactors=FALSE)
  modis.data$DATE <- strptime(substr(modis.data$Date,2,8),"%Y %j")
  row.ind <- which(modis.data$Band == "250m_16_days_NDVI")
  col.ind <- c(which(names(modis.data)=="DATE"),which(names(modis.data)==modis.filenames[i,3]))
  modis.site.ndvi <- modis.data[row.ind,col.ind]
  ndvi.names <- names(modis.site.ndvi)
  ndvi.names[-1] <- "NDVI"
  #   modis.site.ndvi$ndvi <- modis.site.ndvi$ndvi/1e4
  names(modis.site.ndvi) <- ndvi.names
  modis.site.ndvi$site.name <- rep(modis.filenames[i,1],dim(modis.site.ndvi)[1])
  
  #   if (i == 1) {modis.ndvi <- modis.site.ndvi
  #   } else {
  modis.ndvi <- rbind(modis.ndvi,modis.site.ndvi)
  # }
}

modis.ndvi$NDVI <- modis.ndvi$NDVI/1e4

write.table(modis.ndvi,file.path(output.dir.path,"MODIS_NDVI.csv"),sep=",",col.names=TRUE,row.names=FALSE)

# ggplot(modis.ndvi,aes(DATE,NDVI,col=site)) + geom_point(size=3) + geom_line() + ggtitle("NDVI at Ameriflux Sites")
