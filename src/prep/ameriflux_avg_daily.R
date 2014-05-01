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
# results.path <- "../../results/output"
format.path <- "../../data/format"

#### READ IN FLUX TOWER DATA
flux.all.daytime <- NULL
flux.all.daily <- NULL
for (site in sel.sites) {
  cat("Getting data for",site,"\n")
  flux <- read.l2.ameriflux.site(file.path(L2.path,site))
  cat("Looking in",file.path(L2.path,site),"\n")
  flux$site.name <- rep(site,dim(flux)[1])
  flux$year.day <- as.factor(paste(flux$YEAR,"_",flux$DOY,sep=""))
  
  dt.flux <- data.table(flux)
  flux.daytime <- as.data.frame(dt.flux[Rn>0,list(YEAR=YEAR[1],DOY=DOY[1],TA=mean(TA,na.rm=TRUE),WS=mean(WS,na.rm=TRUE),
                                              NEE=mean(NEE,na.rm=TRUE),FC=mean(FC,na.rm=TRUE),SFC=mean(SFC,na.rm=TRUE),
                                              H=mean(H,na.rm=TRUE),SH=mean(SH,na.rm=TRUE),LE=mean(LE,na.rm=TRUE),
                                              SLE=mean(SLE,na.rm=TRUE),FG=mean(FG,na.rm=TRUE),TS1=mean(TS1,na.rm=TRUE),
                                              TSdepth1=mean(TSdepth1,na.rm=TRUE),TS2=mean(TS2,na.rm=TRUE),TSdepth2=mean(TSdepth2,na.rm=TRUE),
                                              PREC=mean(PREC,na.rm=TRUE),RH=mean(RH,na.rm=TRUE),PRESS=mean(PRESS,na.rm=TRUE),
                                              CO2=mean(CO2,na.rm=TRUE),VPD=mean(VPD,na.rm=TRUE),SWC1=mean(SWC1,na.rm=TRUE),
                                              SWC2=mean(SWC2,na.rm=TRUE),Rn=mean(Rn,na.rm=TRUE),PAR=mean(PAR,na.rm=TRUE),
                                              Rg=mean(Rg,na.rm=TRUE),Rgdif=mean(Rgdif,na.rm=TRUE),PARout=mean(PARout,na.rm=TRUE),
                                              RgOut=mean(RgOut,na.rm=TRUE),Rgl=mean(Rgl,na.rm=TRUE),RglOut=mean(RglOut,na.rm=TRUE),
                                              H20=mean(H2O,na.rm=TRUE),RE=mean(RE,na.rm=TRUE),GPP=mean(GPP,na.rm=TRUE),
                                              CO2top=mean(CO2top,na.rm=TRUE),CO2height=mean(CO2height,na.rm=TRUE),APAR=mean(APAR,na.rm=TRUE),
                                              PARdif=mean(PARdif,na.rm=TRUE),APARpct=mean(APARpct,na.rm=TRUE),ZL=mean(ZL,na.rm=TRUE),
                                              site.name=site.name[1]),by=year.day])
  flux.daytime$DATE <- strptime(as.character(flux.daytime$year.day),"%Y_%j")
  flux.daily <- as.data.frame(dt.flux[,list(YEAR=YEAR[1],DOY=DOY[1],TA=mean(TA,na.rm=TRUE),WS=mean(WS,na.rm=TRUE),
                                              NEE=mean(NEE,na.rm=TRUE),FC=mean(FC,na.rm=TRUE),SFC=mean(SFC,na.rm=TRUE),
                                              H=mean(H,na.rm=TRUE),SH=mean(SH,na.rm=TRUE),LE=mean(LE,na.rm=TRUE),
                                              SLE=mean(SLE,na.rm=TRUE),FG=mean(FG,na.rm=TRUE),TS1=mean(TS1,na.rm=TRUE),
                                              TSdepth1=mean(TSdepth1,na.rm=TRUE),TS2=mean(TS2,na.rm=TRUE),TSdepth2=mean(TSdepth2,na.rm=TRUE),
                                              PREC=mean(PREC,na.rm=TRUE),RH=mean(RH,na.rm=TRUE),PRESS=mean(PRESS,na.rm=TRUE),
                                              CO2=mean(CO2,na.rm=TRUE),VPD=mean(VPD,na.rm=TRUE),SWC1=mean(SWC1,na.rm=TRUE),
                                              SWC2=mean(SWC2,na.rm=TRUE),Rn=mean(Rn,na.rm=TRUE),PAR=mean(PAR,na.rm=TRUE),
                                              Rg=mean(Rg,na.rm=TRUE),Rgdif=mean(Rgdif,na.rm=TRUE),PARout=mean(PARout,na.rm=TRUE),
                                              RgOut=mean(RgOut,na.rm=TRUE),Rgl=mean(Rgl,na.rm=TRUE),RglOut=mean(RglOut,na.rm=TRUE),
                                              H20=mean(H2O,na.rm=TRUE),RE=mean(RE,na.rm=TRUE),GPP=mean(GPP,na.rm=TRUE),
                                              CO2top=mean(CO2top,na.rm=TRUE),CO2height=mean(CO2height,na.rm=TRUE),APAR=mean(APAR,na.rm=TRUE),
                                              PARdif=mean(PARdif,na.rm=TRUE),APARpct=mean(APARpct,na.rm=TRUE),ZL=mean(ZL,na.rm=TRUE),
                                              site.name=site.name[1]),by=year.day])
  flux.daily$DATE <- strptime(as.character(flux.daily$year.day),"%Y_%j")
  
  flux.all.daytime <- rbind(flux.all.daytime,flux.daytime)
  flux.all.daily <- rbind(flux.all.daily,flux.daily)
}

write.table(flux.all.daily,file.path(format.path,"sel_ameriflux_daily.csv"),sep=",",row.names=FALSE)
write.table(flux.all.daytime,file.path(format.path,"sel_ameriflux_daytime.csv"),sep=",",row.names=FALSE)

