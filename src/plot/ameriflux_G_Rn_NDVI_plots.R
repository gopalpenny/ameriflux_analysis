# ameriflux plot

require(ggplot2)
require(scales)
require(gridBase)
source("../../r_functions/multiplot.R")
source("../../r_functions/plotobj.R")
colors <- cbPalette

format.path <- "../../data/format"
figure.path <- "../../results/fig"
sel.sites <- read.table(file.path(format.path,"selected_sites.txt"),header=TRUE,sep=",",stringsAsFactors=FALSE)
sites.cross <- sel.sites[c(2,4,5,6),]
sites.veg.type <- data.frame(site.name=sel.sites$sites,VEGETATION=c("Forest","Forest","Forest","Grass","Crops","Forest","Shrubs","Mixed"))

ggplot(daytime,aes(NDVI,FG/Rn)) + geom_point() + facet_wrap(~site.name) + scale_y_continuous(limits=c(-0.25,0.5))
ggplot(daytime,aes(DATE,FG)) + geom_point() + facet_wrap(~site.name)


#### READ DATA
# read daily data
daily.path <- file.path(format.path,"sel_ameriflux_daily_modis.csv")
daily <- read.table(daily.path,sep=",",header=TRUE,stringsAsFactors=FALSE)
daily$DATE <- as.Date(strptime(daily$DATE,"%Y-%m-%d"))

# read daytime data
daytime.path <- file.path(format.path,"sel_ameriflux_daytime_modis.csv")
daytime <- read.table(daytime.path,sep=",",header=TRUE,stringsAsFactors=FALSE)
daytime$DATE <- as.Date(strptime(daytime$DATE,"%Y-%m-%d"))
daytime <- merge(daytime,sites.veg.type,by="site.name",all.x=TRUE)

##### read modis timeseries
modis.ndvi <- read.table(file.path(format.path,"MODIS_NDVI.csv"),header=TRUE,sep=",",stringsAsFactors=FALSE)
modis.ndvi$DATE <- as.Date(strptime(modis.ndvi$DATE,"%Y-%m-%d"))

# #### FIGURE 1: 2006, 
# ind1 <- daytime$site.name %in% sites.cross & strftime(daytime$DATE,"%Y") == "2006"
# p1 <- ggplot(daytime[ind1,]) + facet_wrap(~site.name,nrow=1) + scale_x_date(labels=date_format("%b"))
# a1 <- p1 + geom_point(aes(DATE,Rg),col=colors[1])
# b1 <- p1 + geom_point(aes(DATE,SWC1),col=colors[3])
# c1 <- p1 + geom_point(aes(DATE,LE),col=colors[6])
# d1 <- p1 + geom_point(aes(DATE,H),col=colors[7])
# e1 <- p1 + geom_point(aes(DATE,FG),col=colors[4])
# f1 <- p1 + geom_point(aes(DATE,NDVI/1e4)) + scale_y_continuous(limits=c(0,1))
# if (!interactive()) {jpeg(file.path(figure.path,"Rn_Rg_annual_vars.jpg"),width=1000,height=1000,quality=90)}
# multiplot(a1,b1,e1,f1,cols=1)
# if (!interactive()) {dev.off()}

#### FIGURE 2, energy partitioning
ind2 <- daytime$site.name %in% sites.cross & strftime(daytime$DATE,"%Y") == "2006"
p2 <- ggplot(daytime[ind2,]) + facet_wrap(~site.name,nrow=1) + scale_x_date(labels=date_format("%b"))
a2a <- p2 + geom_point(aes(DATE,LE/Rn),col=colors[6]) + geom_point(aes(DATE,H/Rn),col=colors[7]) +
  geom_point(aes(DATE,FG/Rn),col=colors[4]) + geom_point(aes(DATE,Rn/Rn),col=colors[5]) +
  scale_y_continuous(limits=c(0,1))
# a2b <- p2 + stat_smooth(aes(DATE,LE/Rn),col=colors[6],size=1) + stat_smooth(aes(DATE,H/Rn),col=colors[7],size=1) +
#   stat_smooth(aes(DATE,FG/Rn),col=colors[4],size=1) + geom_line(aes(DATE,rep(1,length(DATE))),col=colors[5],size=1) +
#   scale_y_continuous(limits=c(0,1))
b2 <- p2 + geom_point(aes(DATE,SWC1,col=NEE))
c2 <- p2 + geom_point(aes(DATE,NDVI,col=NEE),size=3)
d2 <- p2 + geom_point(aes(DATE,FG/Rn,col=NEE))
# p2 + geom_point(aes(DATE,Rn/Rg,col=NDVI))
if (!interactive()) {jpeg(file.path(figure.path,"Rn_Rg_radiation_partitioning.jpg"),width=1000,height=1000,quality=90)}
multiplot(a2a,b2,c2,d2,cols=1)
if (!interactive()) {dev.off()}

#### FIGURE 3, G/Rn vs NDVI
ind3ab <- strftime(daytime$DATE,"%Y") == "2006"
p3ab <- ggplot(daytime[ind3ab,]) + facet_wrap(~site.name,nrow=1)
a3 <- ggplot(daytime) + geom_point(aes(NDVI,FG/Rn,size=Rn,col=site.name,shape=VEGETATION)) + scale_y_continuous(limits=c(-0.2,0.5)) + scale_x_continuous(limits=c(0,10000))
print(a3)
b3 <- ggplot(daytime[daytime$VEGETATION=="Forest",]) + geom_point(aes(NDVI,FG/Rn,size=Rn,col=site.name,shape=VEGETATION,alpha=SWC1)) + scale_y_continuous(limits=c(-0.1,0.1)) + scale_x_continuous(limits=c(0,10000))
c3 <- ggplot(daytime[daytime$VEGETATION!="Forest",]) + geom_point(aes(NDVI,FG/Rn,size=Rn,col=site.name,shape=VEGETATION)) + scale_y_continuous(limits=c(-0.2,0.5)) + scale_x_continuous(limits=c(0,10000))

attach(mtcars)
layout(matrix(c(1,2,2), 3, 1, byrow = TRUE))
print(b3)
print(c3)
multiplot(b3,c3,cols=1)
print(a3)


# 
# ind3cd <- daytime$site.name %in% sites.cross & strftime(daytime$DATE,"%Y") == "2006" & as.numeric(strftime(daytime$DATE,"%j")) %in% 190:280
# p3cd <- ggplot(daytime[ind3cd,]) + facet_wrap(~site.name,nrow=1)
# a3 <- p3ab + geom_point(aes(,col=site.name)) + scale_x_continuous("Rg (Full year)",limits=c(0,700))# + geom_line(data=Rn.Rg[Rn.Rg.ind3cd,],aes(Rg,Rn,col=site.name))
# # b <- p1 + geom_point(aes(DOY,Rn/Rg,col=site.name))
# b3 <- p3ab + geom_point(aes(SWC1,Rn/Rg,col=site.name)) + scale_x_continuous("SWC (Full year)",limits=c(0,50))
# # d <- p2 + geom_point(aes(DOY,Rn/Rg,col=site.name))
# e3 <- p3cd + geom_point(aes(NEE,Rn/Rg,col=site.name,alpha=LE))
# f3 <- p3cd + geom_point(aes(NDVI,FG/Rn,col=SWC1))
# print(f3)

if (!interactive()) {jpeg(file.path(figure.path,"Rn_Rg_with_envelope.jpg"),width=1000,height=1400,quality=90)}
multiplot(a3,b3,e3,cols=1)
if (!interactive()) {dev.off()}}


#### FIGURE 4, energy partitioning
ind4 <- daytime$site.name %in% sites.cross & strftime(daytime$DATE,"%Y") == "2006"
p4 <- ggplot(daytime[ind4,]) + facet_wrap(~site.name,nrow=1) + scale_x_date(labels=date_format("%b"))
a4 <- p4 + geom_point(aes(DATE,LE/Rn),col=colors[6]) + geom_point(aes(DATE,H/Rn),col=colors[7]) +
  geom_point(aes(DATE,FG/Rn),col=colors[4]) + geom_point(aes(DATE,Rn/Rn),col=colors[5]) +
  scale_y_continuous(limits=c(0,1))
b4 <- p4 + stat_smooth(aes(DATE,LE/Rn),col=colors[6],size=1) + stat_smooth(aes(DATE,H/Rn),col=colors[7],size=1) +
  stat_smooth(aes(DATE,FG/Rn),col=colors[4],size=1) + geom_line(aes(DATE,rep(1,length(DATE))),col=colors[5],size=1) +
  scale_y_continuous(limits=c(0,1))
c4 <- p4 + geom_point(aes(DATE,FG/Rn,col=SWC1))
d4 <- p4 + geom_point(aes(DATE,NDVI,col=SWC1))
e4 <- ggplot(daytime[ind4,]) + facet_wrap(~site.name,nrow=1) + geom_point(aes(NDVI,FG/Rn,col=SWC1))
multiplot(a4,c4,d4,e4,cols=1)

# p2 + geom_point(aes(DATE,Rn/Rg,col=NDVI))
if (!interactive()) {jpeg(file.path(figure.path,"Rn_Rg_radiation_partitioning.jpg"),width=1000,height=1000,quality=90)}
multiplot(a2,b2,c2,cols=1)
if (!interactive()) {dev.off()}

##### FIGURE 4, NDVI
p4 <- ggplot(daytime,aes(NDVI,FG/Rn)) + geom_point() + facet_wrap(~site.name)
print(p4)

##### FIGURE 4, NDVI
ind4 <- daytime$site.name %in% sites.cross & strftime(daytime$DATE,"%Y") == "2006" & as.numeric(strftime(daytime$DATE,"%j")) %in% 190:280
p4 <- ggplot(daytime[ind4,]) + facet_wrap(~site.name,nrow=1)
a4 <- p4 + geom_point(aes(NDVI,FG/Rn,col=LE))
print(a4)
