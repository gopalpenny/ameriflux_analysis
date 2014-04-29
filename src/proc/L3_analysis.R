# L3.path <- "../../../L3"
# sites <- system(paste("ls",L3.path),intern=TRUE)
# site.path <- file.path(L3.path,sites[1])
# site.files <- system(paste("ls",site.path),intern=TRUE)
# site.data.files <- grep("_L3.txt",site.files)


input.dir.path <- "../../data/format"
flux.modis <- read.table(file.path(input.dir.path,"Ameriflux_MODIS_16day.csv"),sep=",",header=TRUE)

