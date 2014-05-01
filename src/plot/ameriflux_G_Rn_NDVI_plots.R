# ameriflux plot

require(ggplot2)
require(scales)
require(grid)
require(wq)
source("../../r_functions/multiplot.R")
source("../../r_functions/plotobj.R")
colors <- cbPalette

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
ggcolors <- gg_color_hue(8)

format.path <- "../../data/format"
figure.path <- "../../results/fig"
sel.sites <- read.table(file.path(format.path,"selected_sites.txt"),header=TRUE,sep=",",stringsAsFactors=FALSE)
sites.cross <- sel.sites[c(2,4,5,6),]
sites.veg.type <- data.frame(site.name=sel.sites$sites,VEGETATION=c("Forest","Forest","Forest","Grass","Crops","Forest","Shrubs","Mixed"))

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

b2 <- p2 + geom_point(aes(DATE,SWC1,col=NEE))
c2 <- p2 + geom_point(aes(DATE,FG/Rn,col=NEE))
d2 <- p2 + geom_point(aes(DATE,NDVI,col=site.name,shape=site.name,size=Rn)) + scale_color_manual(values=ggcolors[c(1,3,4,5)]) +
  scale_shape_manual(values=c(17,18,16,17)) + scale_size_continuous(range=c(1,4),breaks=c(100,500))

if (!interactive()) {jpeg(file.path(figure.path,"G_fraction_annual_timeseries.jpg"),width=1000,height=1000,quality=90)}
multiplot(a2a,b2,c2,d2,cols=1)
if (!interactive()) {dev.off()}

#### FIGURE 3, G/Rn vs NDVI
ind3ab <- strftime(daytime$DATE,"%Y") == "2006"
p3ab <- ggplot(daytime[ind3ab,]) + facet_wrap(~site.name,nrow=1)
a3 <- ggplot(daytime[daytime$site.name!="Bartlett_Experimental_Forest",]) + geom_point(aes(NDVI,FG/Rn,size=Rn,col=site.name,shape=VEGETATION)) + 
  scale_y_continuous(limits=c(-0.2,0.5)) + scale_x_continuous(limits=c(0,1)) +
  scale_color_manual(values=ggcolors[c(1,2,3,4,5,6,7,8)]) + scale_shape_manual(values=c(16,17,18,8,15))

b3 <- ggplot(daytime[daytime$VEGETATION=="Forest"&daytime$site.name!="Bartlett_Experimental_Forest",]) + geom_point(aes(NDVI,FG/Rn,size=Rn,col=site.name,alpha=SWC1),shape=17) + 
  scale_y_continuous(limits=c(-0.1,0.1),breaks=seq(-0.1,0.1,by=0.1)) + scale_x_continuous(limits=c(0,1)) + 
  scale_color_manual(values=ggcolors[c(1,2,5)]) + scale_size_continuous(breaks=c(100,300,500)) + scale_alpha_continuous(breaks=c(20,60))

c3 <- ggplot(daytime[daytime$VEGETATION!="Forest",]) + geom_point(aes(NDVI,FG/Rn,size=Rn,col=site.name,shape=VEGETATION,alpha=SWC1)) +
  scale_y_continuous(limits=c(-0.1,0.4),breaks=seq(-0.2,0.5,by=0.1)) + scale_x_continuous(limits=c(0,1)) +
  scale_color_manual(values=ggcolors[c(3,4,6,7,8)]) + scale_size_continuous(breaks=c(100,300,500)) + scale_shape_manual(values=c(16,18,8,15)) + scale_alpha_continuous(range=c(0.2,1))

if (!interactive()) {jpeg(file.path(figure.path,"G_fraction_vs_NDVI.jpg"),width=1000,height=1000,quality=90)}
print(a3)
if (!interactive()) {dev.off()}
if (!interactive()) {jpeg(file.path(figure.path,"G_fraction_vs_NDVI_veg_type.jpg"),width=1000,height=1000,quality=90)}
layOut(list(b3,1,1),list(c3,2:3,1))
if (!interactive()) {dev.off()}