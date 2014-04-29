#### READ DATA FROM AMERIFLUX L2

# if (0) {
# l2.path <- "../../../Ameriflux/L2"
# site.name <- "Chestnut_Ridge"
# l2.site.path <- file.path(l2.path,site.name)
# }

read.l2.ameriflux.site <- function(l2.site.path,years=0,print.years=FALSE) {
  
  site.dirs <- system(paste("ls",l2.site.path),intern=TRUE)
  if (max(site.dirs == "gap_filled") == 1) {
    data.dir.path <- file.path(l2.site.path,"gap_filled")
  } else if (max(site.dirs == "with_gaps") == 1) {
    data.dir.path <- file.path(l2.site.path,"with_gaps")
  } else {
    return(NULL)
  }
  
  site.files <- system(paste("ls",data.dir.path),intern=TRUE)
  site.data.files <- site.files[grep(".csv",site.files)]
  
  #### return years
  if (print.years == TRUE) {
    site.yrs <- as.numeric(lapply(strsplit(site.data.files,split="_"),function(x) x[3]))
    return(site.yrs)
  }

  #### draw from selected years, if requested
  if (max(years != 0)) {
    site.yrs <- as.numeric(lapply(strsplit(site.data.files,split="_"),function(x) x[3]))
    site.data.files <- site.data.files[site.yrs %in% years]
  }

  ########### READ IN THE DATA ###################3
  flux.data <- NULL
  for (data.file in site.data.files) {
    file <- file.path(data.dir.path,data.file)
    flux.data.yr <- read.table(file,sep=",",skip=20,colClasses= c("character",rep("numeric",44)))
    headers <- read.table(file,sep=",",skip=17,nrows=1, colClasses = "character",strip.white=TRUE)
    names(flux.data.yr) <- headers
    flux.data <- rbind(flux.data,flux.data.yr)
  }
  
  ## set NAs
  flux.data[flux.data == -9999 | flux.data == -6999] <- NA
  
  ## get date time
  year.start <- as.chron(paste(flux.data$YEAR,"-01-01 00:00:00",sep=""))
  cdates <- year.start + (flux.data$DTIME-1)
  
  #   # drop extraneous date columns
  #   colnames <- c("YEAR","DTIME","DOY","HRMIN")
  #   flux.data <- flux.data[,!(names(flux.data) %in% colnames)]
  
  return(flux.data)
}
  
#END READ AMERIFLUX DATA