#### READ DATA FROM AMERIFLUX TOWERS
read.AMR <- function(loc,L2.path) {
  
  if (loc == "bartlett") { #Bartlett, NH
    yrs <- 2004:2011
    # year <- yrs[i]
    folder <- "Bartlett_Experimental_Forest/with_gaps/"
    filename <- paste("AMF_USBar_","YYYY","_L2_WG_V004.csv",sep="") #2004-2011
    z.ws <- 24.5 
    h <- 19 
    nyrs <- 5 #wind speed height z.ws [m], canopy heigh h [m]
  } else if (loc == "chestnut") { #Chestnut Ridge, TN
    yrs <- 2006:2010
    # year <- yrs[i]
    folder <- "Chestnut_Ridge/with_gaps/"
    filename <- paste("AMF_USChR_","YYYY","_L2_WG_V002.csv",sep="") #2005-2010
    z.ws <- 43 
    h <- 25 
    nyrs <- 8
  } else if (loc == "dukeforest") { # #Duke Hardwood Forest, NC
    yrs <- 2001:2008
    # year <- yrs[i]
    folder <- "Duke_Forest_Hardwoods/gap_filled/"
    filename <- paste("AMF_USDk2_","YYYY","_L2_GF_V003.csv",sep="") #2001-2008
    z.ws <- 30 
    h <- 25 
    nyrs <- 7 #z IS ABOVE CANOPY BUT RANDOMLY SELECTED. DON"T KNOW ACTUAL HEIGHT
  } else if (loc == "kendall") { # #Kendall Grassland, AZ
    yrs <- 2005:2011
    # year <- yrs[i]
    folder <- "Kendall_Grassland/with_gaps/"
    filename <- paste("AMF_USWkg_","YYYY","_L2_WG_V005.csv",sep="") #2004-2011
    z.ws <- 7 
    h <- 0.5 
    nyrs <- 8
  } else if (loc == "meadrainfed") { # #Mead Rainfed, NE
    yrs <- 2002:2009
    # year <- yrs[i]
    folder <- "Mead_Rainfed/gap_filled/"
    filename <- paste("AMF_USNe3_","YYYY","_L2_GF_V005.csv",sep="") #2001-2010
    z.ws <- 3 
    h <- 1.7 
    nyrs <- 5 #z.ws <- 3 or 6.2. Does not say in data....
  } else if (loc == "ncloblolly") { # #Loblolly, NC
    yrs <- 2006:2010
    # year <- yrs[i]
    folder <- "North_Carolina_Loblolly_Pine/with_gaps/"
    filename <- paste("AMF_USNC2_","YYYY","_L2_WG_V003.csv",sep="") #2005-2010
    z.ws <- 18 
    h <- 14 
    nyrs <- 8
  } else if (loc == "santarita") { # #Santa Rita Mesquite, AZ
    yrs <- 2004:2011
    # year <- yrs[i]
    folder <- "Santa_Rita_Mesquite_Savanna/with_gaps/"
    filename <- paste("AMF_USSRM_","YYYY","_L2_WG_V006.csv",sep="") #2004-2011
    z.ws <- 7 
    h <- 2.5 
    nyrs <- 9
  } else if (loc == "tonzi") { # #Tonzi Ranch, CA
    yrs <- 2002:2010
    # year <- yrs[i]
    folder <- "Tonzi_Ranch/gap_filled/"
    filename <- paste("AMF_USTon_","YYYY","_L2_GF_V005.csv",sep="") #2001-2010
    z.ws <- 23 
    h <- 9.4   #9.4 is average canopy height
    nyrs <- 1 
  } else {stop("INVALID SITE NAME")} # #filename is now known

  flux.data <- NULL
  for (year in yrs) {
    file <- paste(L2.path,folder,gsub("YYYY",as.character(year),filename),sep="")
    flux.data.yr <- read.table(file,sep=",",skip=20,colClasses= c("character",rep("numeric",44)))
    headers <- read.table(file,sep=",",skip=17,nrows=1, colClasses = "character",strip.white=TRUE)
    names(flux.data.yr) <- headers
    flux.data <- rbind(flux.data,flux.data.yr)
  }
  
  flux.data[flux.data == -9999 | flux.data == -6999] <- NA
  
  #ytest <- "2007"
  #as.POSIXct(paste(flux.data$year[1],"-01-01 00:00:00",sep=""))
  year.start <- as.chron(paste(flux.data$YEAR[1],"-01-01 00:00:00",sep=""))
  cdates <- year.start + (flux.data$DTIME-1)

  dv.m <- as.character(as.numeric(months(cdates)))
  dv.d <- as.character(days(cdates))
  dv.H <- as.character(hours(cdates))
  dv.M <- as.character(minutes(cdates))
  dv.S <- as.character(seconds(cdates))
  
  flux <- data.frame(#cdates=cdates, Y=flux.data$YEAR, m=dv.m, d=dv.d, H=dv.H, M=dv.M,
                     Rn=flux.data$Rn, Rg=flux.data$Rg, LE=flux.data$LE, Prec=flux.data$PREC,
                     Ta=flux.data$TA, press=flux.data$PRESS, WS=flux.data$WS,G=flux.data$FG,
                     H=flux.data$H)

  dtimes <- paste(flux.data$YEAR,"-",dv.m,"-",dv.d," ",dv.H,":",dv.M,":00",sep="")
  flux$pdates <- ymd_hms(dtimes)


  von_karman <- 0.4
  z0 <- h*0.1
  d <- 0.65*h #[m] from Campbell and Norman, intro to biophysics, or thom or szeicz
  Ga <- von_karman*flux$WS/log((z.ws-d)/z0)^2 #[m/s] bulk aerodynamic conductance, thom 1977, szeicz 1969

  flux$Ga <- Ga
  return(flux)
  
}
  
#END READ AMERIFLUX DATA