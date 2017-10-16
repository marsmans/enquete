#----------------------------------------------------------------------
#
# Plaatjes maken van enquete data
#
#----------------------------------------------------------------------

library(ggplot2)
library(mc2d)
library(sensitivity)
library(lhs)
#library(Hmisc)
#library(ks)
library(pse)
library(xtable)
library(data.table)
library(tidyr)
library(dplyr)
require(reshape2)

setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Expert elicitation/Responses/Enquetev1")


# csv's inlezen
T2010 <- read.csv(file = "./oud_T2010.csv", header = TRUE, sep = ";")
TCRE <- read.csv(file = "./oud_TCRE.csv", header = TRUE, sep = ";")
FnonCO2 <- read.csv(file = "./oud_FnonCO2.csv", header = TRUE, sep = ";")
Costs <- read.csv(file = "./oud_Costs.csv", header = TRUE, sep = ";")
Other <- read.csv(file = "./oud_Other.csv", header = TRUE, sep = ";")

Red.T2010 <- read.csv(file = "./oud_RedT2010.csv", header = TRUE, sep = ";")
Red.TCRE <- read.csv(file = "./oud_RedTCRE.csv", header = TRUE, sep = ";")
Red.FnonCO2 <- read.csv(file = "./oud_RedFnonCO2.csv", header = TRUE, sep = ";")
Red.Costs <- read.csv(file = "./oud_RedCosts.csv", header = TRUE, sep = ";")
Red.Other <- read.csv(file = "./oud_RedOther.csv", header = TRUE, sep = ";")


# # haal niet bruikbare kolommen weg
# T2010 <- subset(T2010, select = -c(Recorded.Date,X,X.1) )
# TCRE <- subset(TCRE, select = -c(Recorded.Date,X,X.1,X.2) )
# FnonCO2 <- subset(FnonCO2, select = -c(Recorded.Date) )
# Costs <- subset(Costs, select = -c(Recorded.Date,X,X.1) )
# Other <- subset(Other, select = -c(Recorded.Date,X,X.1) )
# 
# Red.T2010 <- subset(Red.T2010, select = -c(Recorded.Date,X,X.1) )
# Red.TCRE <- subset(Red.TCRE, select = -c(Recorded.Date,X,X.1) )
# Red.FnonCO2 <- subset(Red.FnonCO2, select = -c(Recorded.Date,X,X.1) )
# Red.Costs <- subset(Red.Costs, select = -c(Recorded.Date,X,X.1) )
# Red.Other <- subset(Red.Other, select = -c(Recorded.Date,X,X.1) )

#T2010
# maak geschikt data.frame
df.T2010 <- data.frame(Ttarget = rep(c(1.5,2,4), each = 9), parameter = "T2010", value = c(T2010$X1.5,T2010$X2,T2010$X4))
df.T2010$WG = as.character(T2010$WG)
df.T2010$Resp = as.character(1:9)

# maak plaatje
p <- ggplot(df.T2010, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
p <- p + geom_point()
p <- p + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
p <- p + theme_bw()
p <- p + coord_cartesian(ylim = c(0,10))
p <- p + ggtitle("T2010")
p


#TCRE
# maak geschikt data.frame
df.TCRE <- data.frame(Ttarget = rep(c(1.5,2,4), each = 9), parameter = "TCRE", value = c(TCRE$X1.5,TCRE$X2,TCRE$X4))
df.TCRE$WG = as.character(TCRE$WG)
df.TCRE$Resp = as.character(1:9)

# maak plaatje
p <- ggplot(df.TCRE, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
p <- p + geom_point()
p <- p + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
p <- p + theme_bw()
p <- p + coord_cartesian(ylim = c(0,10))
p <- p + ggtitle("TCRE")
p


#FnonCO2
# maak geschikt data.frame
df.FnonCO2 <- data.frame(Ttarget = rep(c(1.5,2,4), each = 9), parameter = "FnonCO2", value = c(FnonCO2$X1.5,FnonCO2$X2,FnonCO2$X4))
df.FnonCO2$WG = as.character(FnonCO2$WG)
df.FnonCO2$Resp = as.character(1:9)

# maak plaatje
p <- ggplot(df.FnonCO2, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
p <- p + geom_point()
p <- p + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
p <- p + theme_bw()
p <- p + coord_cartesian(ylim = c(0,10))
p <- p + ggtitle("FnonCO2")
p


#Costs
# maak geschikt data.frame
df.Costs <- data.frame(Ttarget = rep(c(1.5,2,4), each = 9), parameter = "Costs", value = c(Costs$X1.5,Costs$X2,Costs$X4))
df.Costs$WG = as.character(Costs$WG)
df.Costs$Resp = as.character(1:9)

# maak plaatje
p <- ggplot(df.Costs, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
p <- p + geom_point()
p <- p + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
p <- p + theme_bw()
p <- p + coord_cartesian(ylim = c(0,10))
p <- p + ggtitle("Costs")
p


#Other
# maak geschikt data.frame
df.Other <- data.frame(Ttarget = rep(c(1.5,2,4), each = 9), parameter = "Other", value = c(Other$X1.5,Other$X2,Other$X4))
df.Other$WG = as.character(Other$WG)
df.Other$Resp = as.character(1:9)

# maak plaatje
p <- ggplot(df.Other, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
p <- p + geom_point()
p <- p + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
p <- p + theme_bw()
p <- p + coord_cartesian(ylim = c(0,10))
p <- p + ggtitle("Other")
p


#Red.T2010
# maak geschikt data.frame
df.Red.T2010 <- data.frame(Ttarget = rep(c(1.5,2,4), each = 9), parameter = "Red.T2010", value = c(Red.T2010$X1.5,Red.T2010$X2,Red.T2010$X4))
df.Red.T2010$WG = as.character(Red.T2010$WG)
df.Red.T2010$Resp = as.character(1:9)

# maak plaatje
p <- ggplot(df.Red.T2010, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
p <- p + geom_point()
p <- p + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
p <- p + theme_bw()
p <- p + coord_cartesian(ylim = c(0,10))
p <- p + ggtitle("Reduction in uncertainty of T2010")
p


#Red.TCRE
# maak geschikt data.frame
df.Red.TCRE <- data.frame(Ttarget = rep(c(1.5,2,4), each = 9), parameter = "Red.TCRE", value = c(Red.TCRE$X1.5,Red.TCRE$X2,Red.TCRE$X4))
df.Red.TCRE$WG = as.character(Red.TCRE$WG)
df.Red.TCRE$Resp = as.character(1:9)

# maak plaatje
p <- ggplot(df.Red.TCRE, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
p <- p + geom_point()
p <- p + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
p <- p + theme_bw()
p <- p + coord_cartesian(ylim = c(0,10))
p <- p + ggtitle("Reduction in uncertainty of TCRE")
p


#Red.FnonCO2
# maak geschikt data.frame
df.Red.FnonCO2 <- data.frame(Ttarget = rep(c(1.5,2,4), each = 9), parameter = "Red.FnonCO2", value = c(Red.FnonCO2$X1.5,Red.FnonCO2$X2,Red.FnonCO2$X4))
df.Red.FnonCO2$WG = as.character(Red.FnonCO2$WG)
df.Red.FnonCO2$Resp = as.character(1:9)

# maak plaatje
p <- ggplot(df.Red.FnonCO2, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
p <- p + geom_point()
p <- p + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
p <- p + theme_bw()
p <- p + coord_cartesian(ylim = c(0,10))
p <- p + ggtitle("Reduction in uncertainty of FnonCO2")
p


#Red.Costs
# maak geschikt data.frame
df.Red.Costs <- data.frame(Ttarget = rep(c(1.5,2,4), each = 9), parameter = "Red.Costs", value = c(Red.Costs$X1.5,Red.Costs$X2,Red.Costs$X4))
df.Red.Costs$WG = as.character(Red.Costs$WG)
df.Red.Costs$Resp = as.character(1:9)

# maak plaatje
p <- ggplot(df.Red.Costs, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
p <- p + geom_point()
p <- p + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
p <- p + theme_bw()
p <- p + coord_cartesian(ylim = c(0,10))
p <- p + ggtitle("Reduction in uncertainty of Costs")
p


#Red.Other
# maak geschikt data.frame
df.Red.Other <- data.frame(Ttarget = rep(c(1.5,2,4), each = 9), parameter = "Red.Other", value = c(Red.Other$X1.5,Red.Other$X2,Red.Other$X4))
df.Red.Other$WG = as.character(Red.Other$WG)
df.Red.Other$Resp = as.character(1:9)

# maak plaatje
p <- ggplot(df.Red.Other, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
p <- p + geom_point()
p <- p + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
p <- p + theme_bw()
p <- p + coord_cartesian(ylim = c(0,10))
p <- p + ggtitle("Reduction in uncertainty of Other")
p


require(reshape2)

AR5bakjes.melt <- melt(AR5bakjes ,  id.vars = 'deltaCO2', variable.name = 'minmedmax')
SSPbakjes.melt <- melt(SSPbakjes ,  id.vars = 'deltaCO2', variable.name = 'minmedmax')

ggplot(AR5bakjes.melt, aes(deltaCO2,value)) + geom_line(aes(colour = minmedmax))
ggplot(SSPbakjes.melt, aes(deltaCO2,value)) + geom_line(aes(colour = minmedmax))


AR5_SSPbakjes <- rbind(AR5bakjes, SSPbakjes)




# plot ze beide in 1

AR5bakjes.melt$group <- "AR5"
SSPbakjes.melt$group <- "SSP"

AR5_SSPbakjes.melt <- rbind(AR5bakjes.melt, SSPbakjes.melt)

p <- ggplot(AR5_SSPbakjes.melt, aes(x=deltaCO2, y=value, group=interaction(minmedmax,group), colour=group, shape=minmedmax)) #col=interaction(minmedmax,group)
p <- p + geom_point()
p <- p + geom_line() #aes(x=deltaCO2, y=value, group=minmedmax, colour = minmedmax)
#p <- p + geom_smooth(size=1)
p
  
  
  

# krijgt een CC matrix
s.seed <- 21
CCmat <- f.costs.CCmatrix(N,s.seed)
CCdata = data.table(CCmat[[1]])
# maak er een 'werkbaarder' format van
CC <-gather(CCdata,variable,value,c('T2010','TCRE','nonCO2','cumuCO2result','sampletrans01'))
CC=data.table(CC)
CC$Ttarget <- as.character(seq(1.4, 3.4, by = 0.1))

# plotting (probeersel) staven naast elkaar
p = ggplot(CC[variable %in% c('T2010','TCRE','nonCO2','cumuCO2result','sampletrans01')])
p = p + geom_bar(aes(x=Ttarget,y=value,fill=variable),stat="identity",position="dodge") #position="dodge"
p = p + theme_bw()# + theme(axis.text.x=element_text(size=12))
p = p + scale_fill_manual(values=c("cumuCO2result"="dark blue","cs"="dark red","T2010"="black","TCRE"="green", "nonCO2"="blue", "sampletrans01"="orange"))
p = p + ggtitle("CC values (Pearson) for costs, SSP-data")
p
# ggsave(paste("CC_GE_lin.png"),p)




s <- ggplot(kostenSSP.gather[variable %in% c('Cost.Estimate..ktrillion.','MAC.Costs..ktrillion.','Consumption.Loss..ktrillion.')])
s = s + geom_point(aes(x=cumuCO2,y=value, col=variable),stat="identity")
s = s + theme_bw()
s = s + scale_color_manual(values=c("Cost.Estimate..ktrillion."="blue","MAC.Costs..ktrillion."="green", "Consumption.Loss..ktrillion."="red"),
                           #s = s + scale_shape_manual(values=c("tempStijging_door_F_nonCO2"=1,"tempStijging_door_F_nonCO2.met.avg"=4), #if you want shapes
                           labels=c("Carbon price total costs","Area under MAC curve","Consumption loss"))
s = s + guides(col=guide_legend(title=NULL))
s = s + theme(legend.justification=c(0.9,0.65), legend.position=c(0.9,0.65), 
              legend.text = element_text(size = 12))
s = s + labs(x = expression(Cumulative~carbon~emissions~(2010-2100)~(TtCO[2])), y = "Mitigation costs (Ktrillion US$2005)")
s = s + coord_cartesian(xlim = c(0,5), ylim = c(0,0.25))
s