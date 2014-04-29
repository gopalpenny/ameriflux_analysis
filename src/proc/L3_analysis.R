# L3.path <- "../../../L3"
# sites <- system(paste("ls",L3.path),intern=TRUE)
# site.path <- file.path(L3.path,sites[1])
# site.files <- system(paste("ls",site.path),intern=TRUE)
# site.data.files <- grep("_L3.txt",site.files)


input.dir.path <- "../../data/format"
# flux.modis <- read.table(file.path(input.dir.path,"Ameriflux_MODIS_16day.csv"),sep=",",header=TRUE,stringsAsFactors=FALSE)
# flux.modis$Date <- strptime(flux.modis$Date,"")
flux.all <- read.table(file.path(input.dir.path,"ameriflux_selected_data.csv"),sep=",",header=TRUE,stringsAsFactors=FALSE)
flux.all$pdates <- strptime(flux.all$pdates,"%Y-%m-%d %H:%M:%S")
flux.modis <- read.table(file.path(input.dir.path,"Ameriflux_MODIS_16day.csv"),sep=",",header=TRUE)

ggplot(flux.all,aes(NDVI,G/Rn,col=site)) + facet_wrap(~site) + geom_point(alpha=0.01) + scale_y_continuous(limits=c(0,1))


ggplot(flux.all,aes(Rg,Rn,col=site)) + facet_wrap(~site) + geom_point(alpha=0.01) + scale_y_continuous()
