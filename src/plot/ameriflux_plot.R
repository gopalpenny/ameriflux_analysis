# ameriflux plot

results.path <- "../../results/output"
# read daily data
daily.path <- file.path(results.path,"sel_ameriflux_daily.csv")
daily <- read.table(daily.path,sep=",",header=TRUE,stringsAsFactors=FALSE)
daily$DATE <- strptime(daily$DATE,"%Y-%m-%d")

# read daytime data
daytime.path <- file.path(results.path,"sel_ameriflux_daytime.csv")
daytime <- read.table(daytime.path,sep=",",header=TRUE,stringsAsFactors=FALSE)
daytime$DATE <- strptime(daytime$DATE,"%Y-%m-%d")


# DAYTIME Rn vs Rg
# entire year
ggplot(daytime,aes(Rg,Rn,col=DOY)) + geom_point() + facet_wrap(~ site.name)
# growing season
ggplot(daytime[daytime$DOY %in% 120:235,],aes(Rg,Rn,col=DOY)) + geom_point() + facet_wrap(~ site.name)
ggplot(daytime[daytime$DOY %in% 120:235,],aes(Rg,Rn,col=site.name)) + geom_point()# + facet_wrap(~ site.name)
ggplot(daytime[daytime$DOY %in% 120:235 & daytime$site.name=="Kendall_Grassland",],aes(Rg,Rn,col=SWC1)) + geom_point() + facet_wrap(~ site.name)

# Rn / Rg vs SWC1  ##### this one is good!
ggplot(daytime[daytime$DOY %in% 135:235,],aes(SWC1,Rn/Rg,col=DOY)) + geom_point() + facet_wrap(~ site.name) + scale_y_continuous(limits=c(0,1),breaks=seq(0,1,by=0.2))


ggplot(flux.daily,aes(DATE,FG)) + geom_point()
ggplot(daytime[daytime$DOY %in% 105:235,],aes(Rg,Rn,col=site.name)) + geom_point() # + facet_wrap(~ site.name)
ggplot(daytime[daytime$DOY %in% 120:235,],aes(Rg,Rn,col=site.name)) + geom_point() #+ facet_wrap(~ site.name)
