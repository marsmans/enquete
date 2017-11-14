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

setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Expert elicitation/Responses/Enquetev2")


# csv's inlezen
T2010v2 <- read.csv(file = "./T2010v2.csv", header = TRUE, sep = ";")
TCREv2 <- read.csv(file = "./TCREv2.csv", header = TRUE, sep = ";")
FnonCO2v2 <- read.csv(file = "./TFnonCO2v2.csv", header = TRUE, sep = ";")
Costsv2 <- read.csv(file = "./Costsv2.csv", header = TRUE, sep = ";")
Otherv2 <- read.csv(file = "./Otherv2.csv", header = TRUE, sep = ";")
T2010v2 <- T2010v2[-c(4,5,6,7),]

Red.T2010v2 <- read.csv(file = "./red_T2010v2.csv", header = TRUE, sep = ";")
Red.TCREv2 <- read.csv(file = "./red_TCREv2.csv", header = TRUE, sep = ";")
Red.FnonCO2v2 <- read.csv(file = "./red_TFnonCO2v2.csv", header = TRUE, sep = ";")
Red.Costsv2 <- read.csv(file = "./red_Costsv2.csv", header = TRUE, sep = ";")
Red.Otherv2 <- read.csv(file = "./red_Otherv2.csv", header = TRUE, sep = ";")
Red.T2010v2$X <- NULL

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
df.T2010v2 <- data.frame(Ttarget = rep(c(1.5,2,3), each = 3), parameter = "T2010", value = c(T2010v2$X1.5,T2010v2$X2,T2010v2$X3))
df.T2010v2$WG = as.character(T2010v2$WG)
df.T2010v2$Resp = as.character(1:3)

# maak plaatje
p2010 <- ggplot(df.T2010v2, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
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
df.TCREv2 <- data.frame(Ttarget = rep(c(1.5,2,3), each = 3), parameter = "TCRE", value = c(TCREv2$X1.5,TCREv2$X2,TCREv2$X3))
df.TCREv2$WG = as.character(TCREv2$WG)
df.TCREv2$Resp = as.character(1:3)

# maak plaatje
pTCRE <- ggplot(df.TCREv2, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
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
df.FnonCO2v2 <- data.frame(Ttarget = rep(c(1.5,2,3), each = 3), parameter = "FnonCO2", value = c(FnonCO2v2$X1.5,FnonCO2v2$X2,FnonCO2v2$X3))
df.FnonCO2v2$WG = as.character(FnonCO2v2$WG)
df.FnonCO2v2$Resp = as.character(1:3)

# maak plaatje
pFnonCO2 <- ggplot(df.FnonCO2v2, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
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
df.Costsv2 <- data.frame(Ttarget = rep(c(1.5,2,3), each = 3), parameter = "Costs", value = c(Costsv2$X1.5,Costsv2$X2,Costsv2$X3))
df.Costsv2$WG = as.character(Costsv2$WG)
df.Costsv2$Resp = as.character(1:3)

# maak plaatje
pCosts <- ggplot(df.Costsv2, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pCosts <- pCosts + theme_bw()
pCosts <- pCosts + geom_point(size = 5)
pCosts <- pCosts + scale_shape_manual(values=c(49, 50, 51))
pCosts <- pCosts + theme(legend.position="none")
pCosts <- pCosts + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pCosts <- pCosts + coord_cartesian(ylim = c(0,10))
pCosts <- pCosts + ggtitle("Costs")
pCosts


#Other
# maak geschikt data.frame
df.Otherv2 <- data.frame(Ttarget = rep(c(1.5,2,3), each = 3), parameter = "Other", value = c(Otherv2$X1.5,Otherv2$X2,Otherv2$X3))
df.Otherv2$WG = as.character(Otherv2$WG)
df.Otherv2$Resp = as.character(1:3)

# maak plaatje
pOther <- ggplot(df.Otherv2, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pOther <- pOther + theme_bw()
pOther <- pOther + geom_point(size = 5)
pOther <- pOther + scale_shape_manual(values=c(49, 50, 51))
pOther <- pOther + theme(legend.position="none")
pOther <- pOther + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pOther <- pOther + coord_cartesian(ylim = c(0,10))
pOther <- pOther + ggtitle("Other")
pOther


#------ Reductievragen ---------------
#Red.T2010
# maak geschikt data.frame
df.Red.T2010v2 <- data.frame(Investment = rep(c("C","S"), each = 3), parameter = "Red.T2010", value = c(Red.T2010v2$current.trend,Red.T2010v2$sig.investment))
df.Red.T2010v2$WG = as.character(Red.T2010v2$WG)
df.Red.T2010v2$Resp = as.character(1:3)

# maak plaatje
p2010r <- ggplot(df.Red.T2010v2, aes(x=Investment, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
p2010r <- p2010r + theme_bw()
p2010r <- p2010r + geom_point(size = 5)
p2010r <- p2010r + scale_shape_manual(values=c(51, 49, 50))
p2010r <- p2010r + theme(legend.position="none")
p2010r <- p2010r + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
p2010r <- p2010r + coord_cartesian(ylim = c(0,150))
p2010r <- p2010r + ggtitle("Reduction in uncertainty of T2010")
p2010r


#Red.TCRE
# maak geschikt data.frame
df.Red.TCREv2 <- data.frame(Investment = rep(c("C","S"), each = 3), parameter = "Red.TCRE", value = c(Red.TCREv2$current.trend,Red.TCREv2$sig.investment))
df.Red.TCREv2$WG = as.character(Red.TCREv2$WG)
df.Red.TCREv2$Resp = as.character(1:3)

# maak plaatje
pTCREr <- ggplot(df.Red.TCREv2, aes(x=Investment, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pTCREr <- pTCREr + theme_bw()
pTCREr <- pTCREr + geom_point(size = 5)
pTCREr <- pTCREr + scale_shape_manual(values=c(51, 49, 50))
pTCREr <- pTCREr + theme(legend.position="none")
pTCREr <- pTCREr + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pTCREr <- pTCREr + coord_cartesian(ylim = c(0,150))
pTCREr <- pTCREr + ggtitle("Reduction in uncertainty of TCRE")
pTCREr


#Red.FnonCO2
# maak geschikt data.frame
df.Red.FnonCO2v2 <- data.frame(Investment = rep(c("C","S"), each = 3), parameter = "Red.FnonCO2", value = c(Red.FnonCO2v2$current.trend,Red.FnonCO2v2$sig.investment))
df.Red.FnonCO2v2$WG = as.character(Red.FnonCO2v2$WG)
df.Red.FnonCO2v2$Resp = as.character(1:3)

# maak plaatje
pFnonCO2r <- ggplot(df.Red.FnonCO2v2, aes(x=Investment, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pFnonCO2r <- pFnonCO2r + theme_bw()
pFnonCO2r <- pFnonCO2r + geom_point(size = 5)
pFnonCO2r <- pFnonCO2r + scale_shape_manual(values=c(51, 49, 50))
pFnonCO2r <- pFnonCO2r + theme(legend.position="none")
pFnonCO2r <- pFnonCO2r + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pFnonCO2r <- pFnonCO2r + coord_cartesian(ylim = c(0,150))
pFnonCO2r <- pFnonCO2r + ggtitle("Reduction in uncertainty of FnonCO2")
pFnonCO2r


#Red.Costs
# maak geschikt data.frame
df.Red.Costsv2 <- data.frame(Investment = rep(c("C","S"), each = 3), parameter = "Red.Costs", value = c(Red.Costsv2$current.trend,Red.Costsv2$sig.investment))
df.Red.Costsv2$WG = as.character(Red.Costsv2$WG)
df.Red.Costsv2$Resp = as.character(1:3)

# maak plaatje
pCostsr <- ggplot(df.Red.Costsv2, aes(x=Investment, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pCostsr <- pCostsr + theme_bw()
pCostsr <- pCostsr + geom_point(size = 5)
pCostsr <- pCostsr + scale_shape_manual(values=c(51, 49, 50))
pCostsr <- pCostsr + theme(legend.position="none")
pCostsr <- pCostsr + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pCostsr <- pCostsr + coord_cartesian(ylim = c(0,150))
pCostsr <- pCostsr + ggtitle("Reduction in uncertainty of Costs")
pCostsr


#Red.Other
# maak geschikt data.frame
df.Red.Otherv2 <- data.frame(Investment = rep(c("C","S"), each = 3), parameter = "Red.Other", value = c(Red.Otherv2$current.trend,Red.Otherv2$sig.investment))
df.Red.Otherv2$WG = as.character(Red.Otherv2$WG)
df.Red.Otherv2$Resp = as.character(1:3)

# maak plaatje
pOtherr <- ggplot(df.Red.Otherv2, aes(x=Investment, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pOtherr <- pOtherr + theme_bw()
pOtherr <- pOtherr + geom_point(size = 5)
pOtherr <- pOtherr + scale_shape_manual(values=c(51, 49, 50))
pOtherr <- pOtherr + theme(legend.position="none")
pOtherr <- pOtherr + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pOtherr <- pOtherr + coord_cartesian(ylim = c(0,150))
pOtherr <- pOtherr + ggtitle("Reduction in uncertainty of Other")
pOtherr






