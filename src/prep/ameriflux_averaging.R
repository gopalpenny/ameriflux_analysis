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
output.dir.path <- "../../data/format"

# Lodz, Poland
L2.path <- file.path("../../../Ameriflux/L2/")
# Ameriflux sites: "bartlett" "chestnut" "dukeforest" "kendall" "meadrainfed" "ncloblolly" "santarita" "tonzi"
locations <- c("bartlett","chestnut","dukeforest","kendall","meadrainfed","ncloblolly","santarita","tonzi")
# loc <- "chestnut"
i <- 3
loc <- "santarita"
i <- 5
# data.source <- "AMR"
# tag <- "baseline"

#### READ IN FLUX TOWER DATA
flux.all <- NULL
for (loc in locations) {
  flux <- read.AMR(loc,L2.path,i)
  flux$site <- rep(loc,dim(flux)[1])
  flux.all <- rbind(flux.all,flux)
}

#### average the data to 16-day periods


write.table(flux.all,file.path(output.dir.path,"ameriflux_selected_data.csv"),sep=",",row.names=FALSE)

# ggplot(flux.all) + theme() +  geom_point(aes(Rg,Rn,col=sqrt(LE)),alpha=0.25) + ggtitle("Net Radiation vs Global Shortwave Radiation") + facet_wrap(~ site)
# 
# ggplot(flux.all) + theme() +  geom_point(aes(LE,H,col=Rn)) + ggtitle("Sensible vs Latent Heat") + facet_wrap(~ site)
# ggplot(flux.all) + theme() +  geom_point(aes(Rn,H/LE,col=Ta),alpha=0.25) + ggtitle("Sensible vs Latent Heat") + facet_wrap(~ site) +
#   scale_y_continuous(limits=c(0,10))