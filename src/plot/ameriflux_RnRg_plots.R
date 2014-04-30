# ameriflux plot

require(ggplot2)
require(scales)
source("../../r_functions/multiplot.R")
source("../../r_functions/plotobj.R")

colors <- cbPalette

format.path <- "../../data/format"
results.path <- "../../results/output"
figure.path <- "../../results/fig"
sel.sites <- read.table(file.path(format.path,"selected_sites.txt"),header=TRUE,sep=",",stringsAsFactors=FALSE)
sites.cross <- sel.sites[c(2,4,5,6),]


#### READ DATA
# read daily data
daily.path <- file.path(results.path,"sel_ameriflux_daily.csv")
daily <- read.table(daily.path,sep=",",header=TRUE,stringsAsFactors=FALSE)
daily$DATE <- as.Date(strptime(daily$DATE,"%Y-%m-%d"))

# read daytime data
daytime.path <- file.path(results.path,"sel_ameriflux_daytime.csv")
daytime <- read.table(daytime.path,sep=",",header=TRUE,stringsAsFactors=FALSE)
daytime$DATE <- as.Date(strptime(daytime$DATE,"%Y-%m-%d"))



#### FIGURE 1: 2008, energy fluxes
ind1 <- daytime$site.name %in% sites.cross & daytime$YEAR == 2006
p1 <- ggplot(daytime[ind1,]) + facet_wrap(~site.name,nrow=1) + scale_x_date(labels=date_format("%b"))
a1 <- p1 + geom_point(aes(DATE,Rg),col=colors[1])
b1 <- p1 + geom_point(aes(DATE,SWC1),col=colors[3])
c1 <- p1 + geom_point(aes(DATE,LE),col=colors[6])
d1 <- p1 + geom_point(aes(DATE,H),col=colors[7])
e1 <- p1 + geom_point(aes(DATE,FG),col=colors[4])
jpeg(file.path(figure.path,"Rn_Rg_annual_vars.jpg"),width=1000,height=1000,quality=90)
multiplot(a1,b1,c1,d1,e1,cols=1)
dev.off()

#### FIGURE 2, energy partitioning
ind2 <- daytime$site.name %in% sites.cross & daytime$YEAR == 2006
p2 <- ggplot(daytime[ind2,]) + facet_wrap(~site.name,nrow=1) + scale_x_date(labels=date_format("%b"))
a2 <- p2 + geom_point(aes(DATE,LE/Rn),col=colors[6]) + geom_point(aes(DATE,H/Rn),col=colors[7]) +
  geom_point(aes(DATE,FG/Rn),col=colors[4]) + geom_point(aes(DATE,Rn/Rn),col=colors[5]) +
  scale_y_continuous(limits=c(0,1))
b2 <- p2 + stat_smooth(aes(DATE,LE/Rn),col=colors[6],size=1) + stat_smooth(aes(DATE,H/Rn),col=colors[7],size=1) +
  stat_smooth(aes(DATE,FG/Rn),col=colors[4],size=1) + geom_line(aes(DATE,rep(1,length(DATE))),col=colors[5],size=1) +
  scale_y_continuous(limits=c(0,1))
c2 <- p2 + geom_point(aes(DATE,Rn/Rg,col=NEE))

jpeg(file.path(figure.path,"Rn_Rg_radiation_partitioning.jpg"),width=1000,height=1000,quality=90)
multiplot(a2,b2,c2,cols=1)
dev.off()


# DETERMINE THE SCALING FACTOR
Rn.Rg <- data.frame(site.name=sel.sites[,1],factor=rep(0,dim(sel.sites)[1]))
for (i in 1:dim(sel.sites)[1]) {
  site <- sel.sites[i,1]
  indRnRg <- daytime$site.name == site & daytime$YEAR == 2006 & daytime$DOY %in% 190:280
  Rn.Rg$factor[Rn.Rg$site.name == site] <- as.numeric(quantile(daytime$Rn[indRnRg]/daytime$Rg[indRnRg],0.8))
}

##### calculate data table for factors
Rn.Rg1 <- cbind(Rn.Rg,data.frame(SWC=rep(0,dim(sel.sites)[1]),Rg=rep(0.01,dim(sel.sites)[1])))
Rn.Rg2 <- cbind(Rn.Rg,data.frame(SWC=rep(50,dim(sel.sites)[1]),Rg=rep(700,dim(sel.sites)[1])))
Rn.Rg <- rbind(Rn.Rg1,Rn.Rg2)
Rn.Rg$Rn <- Rn.Rg$Rg * Rn.Rg$factor


#### FIGURE 3, Rn vs Rg
ind3ab <- daytime$site.name %in% sites.cross & daytime$YEAR == 2006
ind3cd <- daytime$site.name %in% sites.cross & daytime$YEAR == 2006 & daytime$DOY %in% 190:280
Rn.Rg.ind3cd <- Rn.Rg$site.name %in% sites.cross
p3ab <- ggplot(daytime[ind3ab,]) + facet_wrap(~site.name,nrow=1)
p3cd <- ggplot(daytime[ind3cd,]) + facet_wrap(~site.name,nrow=1)
a3 <- p3ab + geom_point(aes(Rg,Rn,col=site.name)) + scale_x_continuous("Rg (Full year)",limits=c(0,700))# + geom_line(data=Rn.Rg[Rn.Rg.ind3cd,],aes(Rg,Rn,col=site.name))
# b <- p1 + geom_point(aes(DOY,Rn/Rg,col=site.name))
b3 <- p3ab + geom_point(aes(SWC1,Rn/Rg,col=site.name)) + scale_x_continuous("SWC (Full year)",limits=c(0,50))
# d <- p2 + geom_point(aes(DOY,Rn/Rg,col=site.name))
c3 <- p3cd + geom_point(aes(SWC1,Rn/Rg,col=site.name,alpha=LE)) + scale_x_continuous("SWC (Jul-Sep)",limits=c(0,50)) + geom_line(data=Rn.Rg[Rn.Rg.ind3cd,],aes(SWC,Rn/Rg,col=site.name))
d3 <- p3cd + geom_point(aes(Rg,Rn,col=site.name,alpha=LE)) + scale_x_continuous("Rg (Jul-Sep)",limits=c(0,700)) + geom_line(data=Rn.Rg[Rn.Rg.ind3cd,],aes(Rg,Rn,col=site.name))
e3 <- p3cd + geom_point(aes(NEE,Rn/Rg,col=site.name,alpha=LE))
jpeg(file.path(figure.path,"Rn_Rg_with_envelope.jpg"),width=1000,height=1400,quality=90)
multiplot(a3,b3,c3,d3,e3,cols=1)
dev.off()


