# Urban Budyko Model
# This code is used to:
# 1. Input flux tower data
# 2. Calculate EP from Priestley Taylor or Modified Priestley-Taylor
# 3. Sum data to hourly, daily, and yearly values
# 4. Calculate the aridity and Budyko metric (E and P required)
# 5. Plot the Budyko metric
# 6. Other plotting functions, including timeseries and code for comparing different estimates of EP

library(lubridate)
library(chron)
library(ggplot2)
library(data.table)
source("read_ameriflux.R")
source("../../r_functions/plotobj.R")

### 1 INPUT FLUX TOWER DATA

# Lodz, Poland
L2.path <- file.path("../../../Ameriflux/L2/")
# Ameriflux sites: "bartlett" "chestnut" "dukeforest" "kendall" "meadrainfed" "ncloblolly" "santarita" "tonzi"
loc <- "chestnut"
i <- 3
loc <- "santarita"
i <- 5
# data.source <- "AMR"
# tag <- "baseline"

#### READ IN FLUX TOWER DATA
flux <- read.AMR(loc,L2.path,i)
timestep <- median(difftime(flux$pdates[2:length(flux$pdates)],flux$pdates[1:(length(flux$pdates)-1)],units="hours"),
             na.rm=TRUE)


p <- ggplot(NULL) + theme()
p1 <- geom_point(data=flux,aes(Rg,Rn,col=sqrt(LE)))
p2 <- geom_line(aes(0:1000,0:1000),col=cbPalette[3])
p + p1 + p2


p <- ggplot(NULL) + theme() + scale_y_continuous(limits=c(0,5))
p1 <- geom_point(data=flux,aes(LE,Rg/Rn,col=sqrt(LE)))
# p2 <- geom_line(aes(0:20,0:),col=cbPalette[3])
p + p1

#### MODIFICATION BASED ON TAG
# flux$Ta <- flux$Ta - 5
# flux$Ga <- flux$Ga/10

# LE <- flux$LE #[W/m^2, rate]
Rn <- flux$Rn #[W/m^2, rate]
Rn[Rn<0] <- 0
Rg <- flux$Rg
Rg[flux$Ta < 0] <- 0

#### SET FLUX TOWER PARAMETERS
f.veg <- 0.5
albedo <- 0.15 #from: http://www.javeriana.edu.co/arquidis/educacion_continua/documents/Urbanclimates.pdf


#### 2 PREPARATION AND EXECUTION OF ET MODELS
albedo <- 0.3 #[-], loblolly
emis <- 0.97 #[-], emissivity of air

#### Set constants
P <- 101.3 # [kPa] Pressure, STP
Cp <- 1.005e3 #[J/kg/K], specific heat of dry air at 20C
rho_a <- 1.2041 #[kg/m^3], density of air
lambda <- 2260e3 #[J/kg], latent heat of vaporization
MWratio <- 0.622
psychro <- Cp * P/(lambda * MWratio) #[kPa/deg C], psychrometric constant
alpha <- 1.26 #PT coefficient
Gs.frac <- 0.1 #


# PM.Ga_mod <- 1
# PM.Ga <- PM.Ga*PM.Ga_mod #[m/s], Bulk aerodynamic conductance
# PM.Ta <- PM.Ta #[deg C], Temp
# PT <- PM
#Move data into lists for PT function
PT.li <- list(P = P,Cp = Cp,rho_a=rho_a,lambda=lambda,psychro = psychro,
              alpha=alpha,Gs.frac =Gs.frac,emis=emis, albedo=albedo)
PT.df <- data.frame(flux$pdates,flux$Rn,Rg,flux$Ta,flux$WS,flux$press,flux$Prec,flux$Ga)
names(PT.df) <- c("date","Rn","Rg","Ta","U","Press","Prec","Ga")

####
#### Priestley-Taylor equation
sourceme("PriestleyTaylor")
LE.PT <- PriestleyTaylor(PT.df,PT.li) #[W/m^2, 1hr timestep]

####
#### Priestley-Taylor Isothermal equation
sourceme("PriestleyTaylorIsothermal")
LE.PTiso <- PriestleyTaylorIsothermal(PT.df,PT.li)
####
EP <- LE.PT/lambda * 3600*as.numeric(timestep) #[mm/timestep] (convert from [W/m^2])
# EP <- LE.PTiso/lambda * 3600*as.numeric(timestep) #[mm/timestep] (convert from [W/m^2])
ET <- flux$LE/lambda * 3600*as.numeric(timestep) #[mm/timestep] (convert from [W/m^2])
Prec <- flux$Prec  #[mm, amount]
pdates <- flux$pdates

#### 3 CALCULATE HOURLY, DAILY, YEARLY VALUES
#hourly
hourly <- data.frame(pdates,Prec,EP,ET,flux$Ta)

#daily
hourly$days.factor <- as.factor(paste(flux$Y,"-",flux$m,"-",flux$d,sep=""))
hourly.t <- data.table(hourly)
daily <- as.data.frame(hourly.t[,list(
  Prec=sum(Prec), EP=sum(EP,na.rm=TRUE), ET=sum(ET,na.rm=TRUE) ), by=days.factor])
daily$pdates <- ymd(daily$days.factor)

#yearly
hourly$years.factor <- as.factor(paste(flux$Y,sep=""))
hourly.t <- data.table(hourly)
yearly <- as.data.frame(hourly.t[,list(Prec=sum(Prec,na.rm=TRUE),
                                      EP=sum(EP,na.rm=TRUE),
                                      ET=sum(ET,na.rm=TRUE)),by=years.factor])
yearly$pdates <- as.character(yearly$years.factor)
aridity <- yearly$EP/yearly$P
bET <- yearly$ET
# bET <- 1/(1-sum(is.na(hourly$ET))/length(hourly$ET))*yearly$ET #this corrects for missing ET values
Bnat <- bET/yearly$P


### 4 CALCULATE BUDYKO PROPERTIES

#### CALCULATE RETENTION STORAGE / EVAP
source("BudykoImperviousRetention") #
daily$R <- BudykoImperviousRetention(daily$Prec,daily$EP)
yearly$R <- sum(daily$R,na.rm=TRUE)

#### Prepare actual Budyko curve
Budyko1972 <- function(aridity) {1/(sqrt(1+(1/aridity)^2))} #function for 1972 Budyko curve
phi <- seq(0,5,0.01)
Bud1972 <- Budyko1972(phi)   #BUDYKO CURVE
Bud.nat <- data.frame(phi,Bud1972)

#Budyko urban NO irrigation
nirr.veg <- Budyko1972(aridity)*f.veg
nirr.imp <- yearly$R* (1-f.veg)/yearly$Prec
nirr <- nirr.veg + nirr.imp
#Budyko urban WITH irrigation
irr.veg <- aridity*f.veg
irr.imp <- yearly$R* (1-f.veg)/yearly$Prec
irr <- irr.veg + irr.imp
#data frame:
Bud.urb <- data.frame(aridity=rep(aridity,7),B=c(Bnat,nirr.veg,nirr.imp,nirr,irr.veg,irr.imp,irr),
                      Scenario = c("Natural","nirr.veg","nirr.imp","No irrigation","irr.veg","irr.imp","Fully irrigated"),tag=rep(tag,7))

assign(paste(loc,flux$Y[1],".",tag,".urb",sep=""),Bud.urb)
Bud.limits <- data.frame(x=c(0,1.5,5), wl=c(1,1,1), el=c(0,1.5,NA))

### 5 PLOT BUDYKO POINT

if (FALSE) {
  colorpalette <- cbPalette[c(4,1,2,8)]
#### PLOT URBAN BUDYKO VALUES (AFTER STORING DIFFERENT SCENARIOS OF Ta, Ga, etc)
p.plot <- ggplot(NULL) + scale_x_continuous(name="Aridity, EP/P") +
  scale_y_continuous(name="Budyko metric, ET/P",limits=c(0,1.5)) +
  theme(axis.title = element_text(face="bold", size=22), axis.text=element_text(size=14,face="bold"),
        legend.text=element_text(size=16),legend.title=element_text(size=16,face="bold"))
p.wl <- geom_line(data=Bud.limits,aes(x,wl),linetype="dashed",size=.75)
p.el <- geom_line(data=Bud.limits,aes(x,el),linetype="dashed",size=.75)
p.theor <- geom_line(data=Bud.nat,aes(phi,Bud1972),size=1)
  
  p.plot + p.theor + p.wl + p.el
#### PLOT .nat (.baseline)
pC.nat <- geom_point(data=chestnut2008.baseline.urb[1,],aes(aridity,B,color=Scenario),size=5)
pS.nat <- geom_point(data=santarita2008.baseline.urb[1,],aes(aridity,B,color=Scenario),size=5)
p.plot + p.theor + p.wl + p.el + pC.nat + pS.nat + scale_colour_manual(values=colorpalette)
#### PLOT .baseline
pC.base <- geom_point(data=chestnut2008.baseline.urb[4,],aes(aridity,B,color=tag),size=5)
pS.base <- geom_point(data=santarita2008.baseline.urb[4,],aes(aridity,B,color=tag),size=5)
p.plot + p.theor + p.wl + p.el + pC.nat + pS.nat + pC.base + pS.base + scale_colour_manual(values=colorpalette)
#### PLOT .Ta+-5
pC.Ta5 <- geom_point(data=chestnut2008.Ta5.urb[4,],aes(aridity,B,color=tag),size=5)
pS.Ta5 <- geom_point(data=santarita2008.Ta5.urb[4,],aes(aridity,B,color=tag),size=5)
pC.Ta.neg5 <- geom_point(data=chestnut2008.Ta.neg5.urb[4,],aes(aridity,B,color=tag),size=5)
pS.Ta.neg5 <- geom_point(data=santarita2008.Ta.neg5.urb[4,],aes(aridity,B,color=tag),size=5)
p.plot + p.theor + p.wl + p.el + pC.base + pS.base + pC.Ta5 + pS.Ta5 + pC.Ta.neg5 + pS.Ta.neg5 + 
    scale_colour_manual(values=colorpalette[2:4]) + guide()
#### PLOT .GA*/10
pC.Ga10 <- geom_point(data=chestnut2008.Ga10.urb[4,],aes(aridity,B,color=tag),size=5)
pS.Ga10 <- geom_point(data=santarita2008.Ga10.urb[4,],aes(aridity,B,color=tag),size=5)
pC.Ga.neg10 <- geom_point(data=chestnut2008.Ga.neg10.urb[4,],aes(aridity,B,color=tag),size=5)
pS.Ga.neg10 <- geom_point(data=santarita2008.Ga.neg10.urb[4,],aes(aridity,B,color=tag),size=5)
p.plot + p.theor + p.wl + p.el + pC.base + pS.base + pC.Ga10 + pS.Ga10 + pC.Ga.neg10 + pS.Ga.neg10 + scale_colour_manual(values=colorpalette[2:4])
#### PLOT .veg25 .veg75
pC.fveg25 <- geom_point(data=chestnut2008.fveg25.urb[4,],aes(aridity,B,color=tag),size=5)
pS.fveg25 <- geom_point(data=santarita2008.fveg25.urb[4,],aes(aridity,B,color=tag),size=5)
pC.fveg75 <- geom_point(data=chestnut2008.fveg75.urb[4,],aes(aridity,B,color=tag),size=5)
pS.fveg75 <- geom_point(data=santarita2008.fveg75.urb[4,],aes(aridity,B,color=tag),size=5)
p.plot + p.theor + p.wl + p.el + pC.base + pS.base + pC.fveg25 + pS.fveg25 + pC.fveg75 + pS.fveg75 + scale_colour_manual(values=colorpalette[c(3,4,2,1)])
  
  #### PLOT irrigation
  pC.ir <- geom_point(data=chestnut2008.baseline.urb[7,],aes(aridity,B,color=tag),size=5)
  pS.ir <- geom_point(data=santarita2008.baseline.urb[7,],aes(aridity,B,color=tag),size=5)
  p.plot + p.theor + p.wl + p.el + pC.base + pS.base + pC.ir + pS.ir + scale_colour_manual(values=colorpalette[c(3,2)])

}
#### NOTES ON SIMULATIONS:
print(paste(as.character(round(sum(is.na(hourly$ET))/length(hourly$ET)*100,1)),"% missing ET values",sep=""))
print(paste(as.character(round(sum(is.na(hourly$EP))/length(hourly$EP)*100,1)),"% missing EP values",sep=""))
print(paste(as.character(round(sum(is.na(hourly$Prec))/length(hourly$Prec)*100,1)),"% missing Prec values",sep=""))

### 6 OTHER PLOTS

if (FALSE) {
#### PLOT INSTANTANEOUS VALUES
# Precip
pI <- ggplot(NULL)
pI1 <- geom_point(data=hourly,aes(pdates,Prec))
pI+ pI1

# EP and ET
hourly$Pnan <- hourly$Prec
hourly$Pnan[hourly$Prec==0] <- NA
p1 <- ggplot(NULL) + scale_x_datetime(name="",limits=ymd(c("2002-08-01","2002-09-01"))) +
  scale_y_continuous(name="Water flux [mm/h]")
p1 <- ggplot(NULL) + scale_x_datetime(name="") +#,limits=ymd(c("2007-08-01","2007-09-01"))) +
  scale_y_continuous(name="Water flux [mm/h]")
p1a <- geom_line(data=hourly, aes(pdates,EP),color=cbPalette[8])
p1b <- geom_line(data=hourly, aes(pdates,ET),color=cbPalette[4],size=1.25)
p1c <- geom_point(data=hourly, aes(pdates,sign(hourly$Pnan)*-0.2),color=cbPalette[3],size=3)
p1 + p1a + p1b + p1c
  
##### PLOT DAILY VALUES
p2 <- ggplot(NULL)
p2a <- geom_line(data=daily,aes(pdates,EP),color=cbPalette[8],size=2)
p2b <- geom_line(data=daily,aes(pdates,Prec),color=cbPalette[3],size=2)
p2c <- geom_line(data=daily,aes(pdates,ET),color=cbPalette[4],size=2)
p2d <- geom_line(data=daily,aes(pdates,EP*f.veg+R*(1-f.veg)),color=cbPalette[2],size=2)
p2 + p2a +p2b + p2d + p2c 

}

if (FALSE) { #### TAG FIX
  chestnut2008.baseline.urb$tag <- rep("Urban Baseline",7)
  santarita2008.baseline.urb$tag <- rep("Urban Baseline",7)
  chestnut2008.Ta5.urb$tag <- rep("Urban Ta+5",7)
  santarita2008.Ta5.urb$tag <- rep("Urban Ta+5",7)
  chestnut2008.Ta.neg5.urb$tag <- rep("Urban Ta-5",7)
  santarita2008.Ta.neg5.urb$tag <- rep("Urban Ta-5",7)
  
  chestnut2008.Ga10.urb$tag <- rep("Urban Ga*10",7)
  santarita2008.Ga10.urb$tag <- rep("Urban Ga*10",7)
  chestnut2008.Ga.neg10.urb$tag <- rep("Urban Ga/10",7)
  santarita2008.Ga.neg10.urb$tag <- rep("Urban Ga/10",7)
  
  chestnut2008.fveg25.urb$tag <- rep("Urban 25% veg",7)
  santarita2008.fveg25.urb$tag <- rep("Urban 25% veg",7)
  chestnut2008.fveg75.urb$tag <- rep("Urban 75% veg",7)
  santarita2008.fveg75.urb$tag <- rep("Urban 75% veg",7)
  
  chestnut2008.baseline.urb$tag[c(4,7)] <-  c("Urban Baseline","Urb fully irrigated")
  santarita2008.baseline.urb$tag[c(4,7)] <- c("Urban Baseline","Urb fully irrigated")

}


##### data fill
# hourly.backup <- hourly
# hourly.mod <- hourly
if (FALSE) {
# trange <- 1*24*2 #10 days, convert to index
# for (i in seq(1,length(hourly$ET)-trange-1,by=1)) {
#   if (min(is.na(hourly.mod$ET[i:(i+trange)]))) { #if all NA
#     hourly.mod$ET[i:(i+trange)] <- hourly.mod$ET[(i-trange-1):(i-1)]
#   }
#   if (is.na(hourly.mod$EP[i])) {
#     hourly.mod$EP[i] <- hourly.mod$EP[length(hourly.mod$EP)-i]
#   }
#   
# }
# 
# hourly <- hourly.mod
# qplot(pdates,ET,data=hourly.mod)
# qplot(pdates,EP,data=hourly.mod)
  hourly$Ta <- flux$Ta

}

if (FALSE) {
#### COMPARE PT AND PTiso
#### PLOT LE.PT for a comparison @ 2 times of the year
  sourceme("multiplot")
PT.a <- data.frame(pdates, LE.PT, LE.PTiso,flux$LE)
PT.a$LE.PT[PT.a$LE.PT < 0] <- 0
p.Mar <- ggplot(NULL) + scale_x_datetime(name="",limits=ymd(c("2002-04-01","2002-04-15"))) +
  scale_y_continuous(name="LE, W/m^2") +# +
  theme(axis.title = element_text(face="bold", size=18), axis.text=element_text(size=14,face="bold"),
        legend.text=element_text(size=16),legend.title=element_blank(),
        legend.position=c(0.2,0.7))
  
p.Sep <- ggplot(NULL) + scale_x_datetime(name="",limits=ymd(c("2002-07-15","2002-07-31"))) +
    scale_y_continuous(name="LE, W/m^2") +# +
    theme(axis.title = element_text(face="bold", size=18), axis.text=element_text(size=14,face="bold"),
          legend.text=element_text(size=16),legend.position="none")
  
p.PT1 <- geom_line(data=PT.a,aes(pdates,flux.LE,color="Latent heat (measured)"),size=1)
p.PT2 <- geom_line(data=PT.a,aes(pdates,LE.PT,color="Priestley-Taylor EP"),size=1)
p.PT3 <- geom_line(data=PT.a,aes(pdates,LE.PTiso,color="Modified Priestley-Taylor EP"),size=1)

pMar <- p.Mar + p.PT1 + p.PT2 + p.PT3 + scale_colour_manual(values=cbPalette[c(3,8,2)])
pSep <- p.Sep + p.PT1 + p.PT2 + p.PT3 + scale_colour_manual(values=cbPalette[c(3,8,2)])
  
multiplot(pMar,pSep,cols=1)
  
  #### COMPARE EP AND ET
  
}