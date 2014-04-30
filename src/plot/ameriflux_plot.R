# ameriflux plot

require(ggplot2)
source("../../r_functions/multiplot.R")
source("../../r_functions/plotobj.R")

colors <- cbPalette

format.path <- "../../data/format"
results.path <- "../../results/output"
sel.sites <- read.table(file.path(format.path,"selected_sites.txt"),stringsAsFactors=FALSE)
sites.cross <- sel.sites[c(3,7,5,6),]
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
ggplot(daytime[daytime$DOY %in% 135:235,],aes(SWC1,Rn/Rg,col=site.name,alpha=DOY)) + geom_point() + scale_y_continuous(limits=c(0,1),breaks=seq(0,1,by=0.2)) + facet_wrap(~ site.name)
ggplot(daytime[daytime$DOY %in% 135:235,],aes(SWC1,Rn/Rg,col=site.name)) + geom_point() + scale_y_continuous(limits=c(0.4,0.9),breaks=seq(0.4,0.9,by=0.1))
ggplot(daytime[daytime$DOY %in% 135:235,],aes(SWC1,Rn/Rg,col=Rg)) + geom_point() + scale_y_continuous(limits=c(0,1),breaks=seq(0,1,by=0.2)) + facet_wrap(~ site.name)


ggplot(daytime,aes(DATE,Rg)) + geom_point() + facet_wrap(~site.name)

#### FIGURE 1: 2008, energy fluxes
ind <- daytime$site.name %in% sites.cross & daytime$YEAR == 2006
p1 <- ggplot(daytime[ind,]) + facet_wrap(~site.name,nrow=1)
a <- p + geom_point(aes(DATE,Rg),col=colors[1])
b <- p + geom_point(aes(DATE,SWC1),col=colors[3])
c <- p + geom_point(aes(DATE,LE),col=colors[6])
d <- p + geom_point(aes(DATE,H),col=colors[7])
e <- p + geom_point(aes(DATE,FG),col=colors[4])
multiplot(a,b,c,d,e,cols=1)

#### FIGURE 2, energy partitioning
p1 <- ggplot(daytime[ind,]) + facet_wrap(~site.name,nrow=1)
a <- p1 + geom_point(aes(DATE,LE/Rn),col=colors[6]) + geom_point(aes(DATE,H/Rn),col=colors[7]) +
  geom_point(aes(DATE,FG/Rn),col=colors[4]) + geom_point(aes(DATE,Rn/Rn),col=colors[5]) +
  scale_y_continuous(limits=c(0,1))
b <- p1 + stat_smooth(aes(DATE,LE/Rn),col=colors[6],size=1) + stat_smooth(aes(DATE,H/Rn),col=colors[7],size=1) +
  stat_smooth(aes(DATE,FG/Rn),col=colors[4],size=1) + geom_line(aes(DATE,rep(1,length(DATE))),col=colors[5],size=1) +
  scale_y_continuous(limits=c(0,1))
multiplot(a,b,cols=1)

#### FIGURE 3, Rn vs Rg
#######################################################33
# figuring out how to plot lines.... with multiple sites
Rg <- data.frame(vals=c(0,800))
SWC <- data.frame(vals=c(0,50))
#######################################################33
ind2 <- daytime$site.name %in% sites.cross & daytime$YEAR == 2006 & daytime$DOY %in% 190:280
p2 <- ggplot(daytime[ind2,]) + facet_wrap(~site.name,nrow=1)
a <- p1 + geom_point(aes(Rg,Rn,col=site.name)) + geom_line(data=Rg,aes(Rg,Rg*)) + scale_x_continuous("Rg (Jan-Dec)")
# b <- p1 + geom_point(aes(DOY,Rn/Rg,col=site.name))
b2 <- p2 + geom_point(aes(Rg,Rn,col=site.name)) + scale_x_continuous("Rg (Jul-Sep)")
c <- p1 + geom_point(aes(SWC1,Rn/Rg,col=site.name)) + scale_x_continuous("SWC (Jan-Dec)")
# d <- p2 + geom_point(aes(DOY,Rn/Rg,col=site.name))
d2 <- p2 + geom_point(aes(SWC1,Rn/Rg,col=site.name)) + scale_x_continuous("SWC (Jul-Sep)")
multiplot(a,c,b2,d2,cols=1)
ggplot(daytime[daytime$DOY %in% 105:235,],aes(Rg,Rn,col=site.name)) + geom_point() # + facet_wrap(~ site.name)
ggplot(daytime[daytime$DOY %in% 120:235,],aes(Rg,Rn,col=site.name)) + geom_point() #+ facet_wrap(~ site.name)



