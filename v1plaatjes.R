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

source("~/disks/y/ontwapps/Timer/Users/Stijn/Model/enquete/multiplot.R")

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
p2010 <- ggplot(df.T2010, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
p2010 <- p2010 + theme_bw()
p2010 <- p2010 + geom_point(size = 5)
p2010 <- p2010 + scale_shape_manual(values=c(49, 50, 51))
p2010 <- p2010 + theme(legend.position="none")
p2010 <- p2010 + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
p2010 <- p2010 + coord_cartesian(ylim = c(0,10))
p2010 <- p2010 + ggtitle("T2010")
p2010


#TCRE
# maak geschikt data.frame
df.TCRE <- data.frame(Ttarget = rep(c(1.5,2,4), each = 9), parameter = "TCRE", value = c(TCRE$X1.5,TCRE$X2,TCRE$X4))
df.TCRE$WG = as.character(TCRE$WG)
df.TCRE$Resp = as.character(1:9)

# maak plaatje
pTCRE <- ggplot(df.TCRE, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pTCRE <- pTCRE + theme_bw()
pTCRE <- pTCRE + geom_point(size = 5)
pTCRE <- pTCRE + scale_shape_manual(values=c(49, 50, 51))
pTCRE <- pTCRE + theme(legend.position="none")
pTCRE <- pTCRE + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pTCRE <- pTCRE + coord_cartesian(ylim = c(0,10))
pTCRE <- pTCRE + ggtitle("TCRE")
pTCRE


#FnonCO2
# maak geschikt data.frame
df.FnonCO2 <- data.frame(Ttarget = rep(c(1.5,2,4), each = 9), parameter = "FnonCO2", value = c(FnonCO2$X1.5,FnonCO2$X2,FnonCO2$X4))
df.FnonCO2$WG = as.character(FnonCO2$WG)
df.FnonCO2$Resp = as.character(1:9)

# maak plaatje
pFnonCO2 <- ggplot(df.FnonCO2, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pFnonCO2 <- pFnonCO2 + theme_bw()
pFnonCO2 <- pFnonCO2 + geom_point(size = 5)
pFnonCO2 <- pFnonCO2 + scale_shape_manual(values=c(49, 50, 51))
pFnonCO2 <- pFnonCO2 + theme(legend.position="none")
pFnonCO2 <- pFnonCO2 + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pFnonCO2 <- pFnonCO2 + coord_cartesian(ylim = c(0,10))
pFnonCO2 <- pFnonCO2 + ggtitle("FnonCO2")
pFnonCO2


#Costs
# maak geschikt data.frame
df.Costs <- data.frame(Ttarget = rep(c(1.5,2,4), each = 9), parameter = "Costs", value = c(Costs$X1.5,Costs$X2,Costs$X4))
df.Costs$WG = as.character(Costs$WG)
df.Costs$Resp = as.character(1:9)

# maak plaatje
pCosts <- ggplot(df.Costs, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pCosts <- pCosts + theme_bw()
pCosts <- pCosts + geom_point(size = 5)
pCosts <- pCosts + scale_shape_manual(values=c(49, 50, 51))
pCosts <- pCosts + theme(legend.position="none")
pCosts <- pCosts + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pCosts <- pCosts + coord_cartesian(ylim = c(0,10))
pCosts <- pCosts + ggtitle("t")
pCosts


#Other
# maak geschikt data.frame
df.Other <- data.frame(Ttarget = rep(c(1.5,2,4), each = 9), parameter = "Other", value = c(Other$X1.5,Other$X2,Other$X4))
df.Other$WG = as.character(Other$WG)
df.Other$Resp = as.character(1:9)

# maak plaatje
pOther <- ggplot(df.Other, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pOther <- pOther + theme_bw()
pOther <- pOther + geom_point(size = 5)
pOther <- pOther + scale_shape_manual(values=c(49, 50, 51))
pOther <- pOther + theme(legend.position="none")
pOther <- pOther + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pOther <- pOther + coord_cartesian(ylim = c(0,10))
pOther <- pOther + ggtitle("Other")
pOther


#Red.T2010
# maak geschikt data.frame
df.Red.T2010 <- data.frame(Ttarget = rep(c(1.5,2,4), each = 9), parameter = "Red.T2010", value = c(Red.T2010$X1.5,Red.T2010$X2,Red.T2010$X4))
df.Red.T2010$WG = as.character(Red.T2010$WG)
df.Red.T2010$Resp = as.character(1:9)

# maak plaatje
pRT2010 <- ggplot(df.Red.T2010, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pRT2010 <- pRT2010 + theme_bw()
pRT2010 <- pRT2010 + geom_point(size = 5)
pRT2010 <- pRT2010 + scale_shape_manual(values=c(49, 50, 51))
pRT2010 <- pRT2010 + theme(legend.position="none")
pRT2010 <- pRT2010 + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pRT2010 <- pRT2010 + coord_cartesian(ylim = c(0,10))
pRT2010 <- pRT2010 + ggtitle("T2010")
pRT2010


#Red.TCRE
# maak geschikt data.frame
df.Red.TCRE <- data.frame(Ttarget = rep(c(1.5,2,4), each = 9), parameter = "Red.TCRE", value = c(Red.TCRE$X1.5,Red.TCRE$X2,Red.TCRE$X4))
df.Red.TCRE$WG = as.character(Red.TCRE$WG)
df.Red.TCRE$Resp = as.character(1:9)

# maak plaatje
pRTCRE <- ggplot(df.Red.TCRE, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pRTCRE <- pRTCRE + theme_bw()
pRTCRE <- pRTCRE + geom_point(size = 5)
pRTCRE <- pRTCRE + scale_shape_manual(values=c(49, 50, 51))
pRTCRE <- pRTCRE + theme(legend.position="none")
pRTCRE <- pRTCRE + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pRTCRE <- pRTCRE + coord_cartesian(ylim = c(0,10))
pRTCRE <- pRTCRE + ggtitle("TCRE")
pRTCRE


#Red.FnonCO2
# maak geschikt data.frame
df.Red.FnonCO2 <- data.frame(Ttarget = rep(c(1.5,2,4), each = 9), parameter = "Red.FnonCO2", value = c(Red.FnonCO2$X1.5,Red.FnonCO2$X2,Red.FnonCO2$X4))
df.Red.FnonCO2$WG = as.character(Red.FnonCO2$WG)
df.Red.FnonCO2$Resp = as.character(1:9)

# maak plaatje
pRTFnonCO2 <- ggplot(df.Red.FnonCO2, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pRTFnonCO2 <- pRTFnonCO2 + theme_bw()
pRTFnonCO2 <- pRTFnonCO2 + geom_point(size = 5)
pRTFnonCO2 <- pRTFnonCO2 + scale_shape_manual(values=c(49, 50, 51))
pRTFnonCO2 <- pRTFnonCO2 + theme(legend.position="none")
pRTFnonCO2 <- pRTFnonCO2 + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pRTFnonCO2 <- pRTFnonCO2 + coord_cartesian(ylim = c(0,10))
pRTFnonCO2 <- pRTFnonCO2 + ggtitle("FnonCO2")
pRTFnonCO2


#Red.Costs
# maak geschikt data.frame
df.Red.Costs <- data.frame(Ttarget = rep(c(1.5,2,4), each = 9), parameter = "Red.Costs", value = c(Red.Costs$X1.5,Red.Costs$X2,Red.Costs$X4))
df.Red.Costs$WG = as.character(Red.Costs$WG)
df.Red.Costs$Resp = as.character(1:9)

# maak plaatje
pRCosts <- ggplot(df.Red.Costs, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pRCosts <- pRCosts + theme_bw()
pRCosts <- pRCosts + geom_point(size = 5)
pRCosts <- pRCosts + scale_shape_manual(values=c(49, 50, 51))
pRCosts <- pRCosts + theme(legend.position="none")
pRCosts <- pRCosts + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pRCosts <- pRCosts + coord_cartesian(ylim = c(0,10))
pRCosts <- pRCosts + ggtitle("Costs")
pRCosts


#Red.Other
# maak geschikt data.frame
df.Red.Other <- data.frame(Ttarget = rep(c(1.5,2,4), each = 9), parameter = "Red.Other", value = c(Red.Other$X1.5,Red.Other$X2,Red.Other$X4))
df.Red.Other$WG = as.character(Red.Other$WG)
df.Red.Other$Resp = as.character(1:9)

# maak plaatje
pROther <- ggplot(df.Red.Other, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pROther <- pROther + theme_bw()
pROther <- pROther + geom_point(size = 5)
pROther <- pROther + scale_shape_manual(values=c(49, 50, 51))
pROther <- pROther + theme(legend.position="none")
pROther <- pROther + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pROther <- pROther + coord_cartesian(ylim = c(0,10))
pROther <- pROther + ggtitle("Other")
pROther

#3x2
multiplot(p2010, pTCRE, pFnonCO2, pCosts, pOther,cols=2) #layout = matrix(c(1,4,7,10,2,5,8,11,3,6,9,12),
#2x3
multiplot(p2010, pTCRE, pFnonCO2, pCosts, pOther,cols=3,layout = matrix(c(1,4,2,5,3,6),nrow=2)) #layout = matrix(c(1,4,7,10,2,5,8,11,3,6,9,12),

#3x2 red
multiplot(pRT2010, pRTCRE, pRTFnonCO2, pRCosts, pROther,cols=3,layout = matrix(c(1,4,2,5,3,6),nrow=2)) #layout = matrix(c(1,4,7,10,2,5,8,11,3,6,9,12),


