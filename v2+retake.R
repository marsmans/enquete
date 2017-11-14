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

setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Expert elicitation/Responses/Enqueteretake")


# v2 csv's inlezen
T2010v2 <- read.csv(file = "../Enquetev2/T2010v2.csv", header = TRUE, sep = ";")
TCREv2 <- read.csv(file = "../Enquetev2/TCREv2.csv", header = TRUE, sep = ";")
FnonCO2v2 <- read.csv(file = "../Enquetev2/TFnonCO2v2.csv", header = TRUE, sep = ";")
Costsv2 <- read.csv(file = "../Enquetev2/Costsv2.csv", header = TRUE, sep = ";")
Otherv2 <- read.csv(file = "../Enquetev2/Otherv2.csv", header = TRUE, sep = ";")
T2010v2 <- T2010v2[-c(4,5,6,7),]

Red.T2010v2 <- read.csv(file = "../Enquetev2/Red_T2010v2.csv", header = TRUE, sep = ";")
Red.TCREv2 <- read.csv(file = "../Enquetev2/Red_TCREv2.csv", header = TRUE, sep = ";")
Red.FnonCO2v2 <- read.csv(file = "../Enquetev2/Red_TFnonCO2v2.csv", header = TRUE, sep = ";")
Red.Costsv2 <- read.csv(file = "../Enquetev2/Red_Costsv2.csv", header = TRUE, sep = ";")
Red.Otherv2 <- read.csv(file = "../Enquetev2/Red_Otherv2.csv", header = TRUE, sep = ";")
Red.T2010v2$X <- NULL


# retake csv's inlezen
Red.T2010v2re <- read.csv(file = "./red_T2010re.csv", header = TRUE, sep = ";")
Red.TCREv2re <- read.csv(file = "./red_TCREre.csv", header = TRUE, sep = ";")
Red.FnonCO2v2re <- read.csv(file = "./red_TFnonCO2re.csv", header = TRUE, sep = ";")
Red.Costsv2re <- read.csv(file = "./red_Costsre.csv", header = TRUE, sep = ";")
Red.Otherv2re <- read.csv(file = "./red_Otherre.csv", header = TRUE, sep = ";")
Red.T2010v2re$X <- NULL


# maak geschikte dataframes
df.T2010v2 <- data.frame(Ttarget = rep(c(1.5,2,3), each = 3), parameter = "T2010", value = c(T2010v2$X1.5,T2010v2$X2,T2010v2$X3))
df.T2010v2$WG = as.character(T2010v2$WG)
df.T2010v2$Resp = as.character(1:3)

# maak geschikt data.frame
df.TCREv2 <- data.frame(Ttarget = rep(c(1.5,2,3), each = 3), parameter = "TCRE", value = c(TCREv2$X1.5,TCREv2$X2,TCREv2$X3))
df.TCREv2$WG = as.character(TCREv2$WG)
df.TCREv2$Resp = as.character(1:3)

# maak geschikt data.frame
df.FnonCO2v2 <- data.frame(Ttarget = rep(c(1.5,2,3), each = 3), parameter = "FnonCO2", value = c(FnonCO2v2$X1.5,FnonCO2v2$X2,FnonCO2v2$X3))
df.FnonCO2v2$WG = as.character(FnonCO2v2$WG)
df.FnonCO2v2$Resp = as.character(1:3)

df.Costsv2 <- data.frame(Ttarget = rep(c(1.5,2,3), each = 3), parameter = "Costs", value = c(Costsv2$X1.5,Costsv2$X2,Costsv2$X3))
df.Costsv2$WG = as.character(Costsv2$WG)
df.Costsv2$Resp = as.character(1:3)

# maak geschikt data.frame
df.Otherv2 <- data.frame(Ttarget = rep(c(1.5,2,3), each = 3), parameter = "Other", value = c(Otherv2$X1.5,Otherv2$X2,Otherv2$X3))
df.Otherv2$WG = as.character(Otherv2$WG)
df.Otherv2$Resp = as.character(1:3)

# maak geschikt data.frame
df.Red.T2010v2 <- data.frame(Investment = rep(c("C","S"), each = 3), parameter = "Red.T2010", value = c(Red.T2010v2$current.trend,Red.T2010v2$sig.investment))
df.Red.T2010v2$WG = as.character(Red.T2010v2$WG)
df.Red.T2010v2$Resp = as.character(1:3)

# maak geschikt data.frame
df.Red.TCREv2 <- data.frame(Investment = rep(c("C","S"), each = 3), parameter = "Red.TCRE", value = c(Red.TCREv2$current.trend,Red.TCREv2$sig.investment))
df.Red.TCREv2$WG = as.character(Red.TCREv2$WG)
df.Red.TCREv2$Resp = as.character(1:3)

# maak geschikt data.frame
df.Red.FnonCO2v2 <- data.frame(Investment = rep(c("C","S"), each = 3), parameter = "Red.FnonCO2", value = c(Red.FnonCO2v2$current.trend,Red.FnonCO2v2$sig.investment))
df.Red.FnonCO2v2$WG = as.character(Red.FnonCO2v2$WG)
df.Red.FnonCO2v2$Resp = as.character(1:3)

# maak geschikt data.frame
df.Red.Costsv2 <- data.frame(Investment = rep(c("C","S"), each = 3), parameter = "Red.Costs", value = c(Red.Costsv2$current.trend,Red.Costsv2$sig.investment))
df.Red.Costsv2$WG = as.character(Red.Costsv2$WG)
df.Red.Costsv2$Resp = as.character(1:3)

# maak geschikt data.frame
df.Red.Otherv2 <- data.frame(Investment = rep(c("C","S"), each = 3), parameter = "Red.Other", value = c(Red.Otherv2$current.trend,Red.Otherv2$sig.investment))
df.Red.Otherv2$WG = as.character(Red.Otherv2$WG)
df.Red.Otherv2$Resp = as.character(1:3)

# maak geschikt data.frame1
df.Red.T2010v2re <- data.frame(Investment = rep(c("C","S"), each = 4), parameter = "Red.T2010", value = c(Red.T2010v2re$current.trend,Red.T2010v2re$sig.investment))
df.Red.T2010v2re$WG = as.character(Red.T2010v2re$WG)
df.Red.T2010v2re$Resp = as.character(4:7)

# maak geschikt data.frame
df.Red.TCREv2re <- data.frame(Investment = rep(c("C","S"), each = 4), parameter = "Red.TCRE", value = c(Red.TCREv2re$current.trend,Red.TCREv2re$sig.investment))
df.Red.TCREv2re$WG = as.character(Red.TCREv2re$WG)
df.Red.TCREv2re$Resp = as.character(4:7)

# maak geschikt data.frame
df.Red.FnonCO2v2re <- data.frame(Investment = rep(c("C","S"), each = 4), parameter = "Red.FnonCO2", value = c(Red.FnonCO2v2re$current.trend,Red.FnonCO2v2re$sig.investment))
df.Red.FnonCO2v2re$WG = as.character(Red.FnonCO2v2re$WG)
df.Red.FnonCO2v2re$Resp = as.character(4:7)

# maak geschikt data.frame
df.Red.Costsv2re <- data.frame(Investment = rep(c("C","S"), each = 4), parameter = "Red.Costs", value = c(Red.Costsv2re$current.trend,Red.Costsv2re$sig.investment))
df.Red.Costsv2re$WG = as.character(Red.Costsv2re$WG)
df.Red.Costsv2re$Resp = as.character(4:7)

# maak geschikt data.frame
df.Red.Otherv2re <- data.frame(Investment = rep(c("C","S"), each = 4), parameter = "Red.Other", value = c(Red.Otherv2re$current.trend,Red.Otherv2re$sig.investment))
df.Red.Otherv2re$WG = as.character(Red.Otherv2re$WG)
df.Red.Otherv2re$Resp = as.character(4:7)


# merge v2 en retake
df.red.T2010v2_r <- rbind(df.Red.T2010v2,df.Red.T2010v2re)
df.red.TCREv2_r <- rbind(df.Red.TCREv2,df.Red.TCREv2re)
df.red.FnonCO2v2_r <- rbind(df.Red.FnonCO2v2,df.Red.FnonCO2v2re)
df.red.Costsv2_r <- rbind(df.Red.Costsv2,df.Red.Costsv2re)
df.red.Otherv2_r <- rbind(df.Red.Otherv2,df.Red.Otherv2re)


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

# maak plaatje
p2010r <- ggplot(df.Red.T2010v2re, aes(x=Investment, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
p2010r <- p2010r + theme_bw()
p2010r <- p2010r + geom_point(size = 5)
p2010r <- p2010r + scale_shape_manual(values=c(50, 51, 49))
p2010r <- p2010r + theme(legend.position="none")
p2010r <- p2010r + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
p2010r <- p2010r + coord_cartesian(ylim = c(0,150))
p2010r <- p2010r + ggtitle("Reduction in uncertainty of T2010")
p2010r


#Red.TCRE

# maak plaatje
pTCREr <- ggplot(df.Red.TCREv2re, aes(x=Investment, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pTCREr <- pTCREr + theme_bw()
pTCREr <- pTCREr + geom_point(size = 5)
pTCREr <- pTCREr + scale_shape_manual(values=c(50, 51, 49))
pTCREr <- pTCREr + theme(legend.position="none")
pTCREr <- pTCREr + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pTCREr <- pTCREr + coord_cartesian(ylim = c(0,150))
pTCREr <- pTCREr + ggtitle("Reduction in uncertainty of TCRE")
pTCREr


#Red.FnonCO2

# maak plaatje
pFnonCO2r <- ggplot(df.Red.FnonCO2v2re, aes(x=Investment, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pFnonCO2r <- pFnonCO2r + theme_bw()
pFnonCO2r <- pFnonCO2r + geom_point(size = 5)
pFnonCO2r <- pFnonCO2r + scale_shape_manual(values=c(50, 51, 49))
pFnonCO2r <- pFnonCO2r + theme(legend.position="none")
pFnonCO2r <- pFnonCO2r + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pFnonCO2r <- pFnonCO2r + coord_cartesian(ylim = c(0,150))
pFnonCO2r <- pFnonCO2r + ggtitle("Reduction in uncertainty of FnonCO2")
pFnonCO2r


#Red.Costs

# maak plaatje
pCostsr <- ggplot(df.Red.Costsv2re, aes(x=Investment, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pCostsr <- pCostsr + theme_bw()
pCostsr <- pCostsr + geom_point(size = 5)
pCostsr <- pCostsr + scale_shape_manual(values=c(50, 51, 49))
pCostsr <- pCostsr + theme(legend.position="none")
pCostsr <- pCostsr + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pCostsr <- pCostsr + coord_cartesian(ylim = c(0,150))
pCostsr <- pCostsr + ggtitle("Reduction in uncertainty of Costs")
pCostsr


#Red.Other

# maak plaatje
pOtherr <- ggplot(df.Red.Otherv2re, aes(x=Investment, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pOtherr <- pOtherr + theme_bw()
pOtherr <- pOtherr + geom_point(size = 5)
pOtherr <- pOtherr + scale_shape_manual(values=c(50, 51, 49))
pOtherr <- pOtherr + theme(legend.position="none")
pOtherr <- pOtherr + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pOtherr <- pOtherr + coord_cartesian(ylim = c(0,150))
pOtherr <- pOtherr + ggtitle("Reduction in uncertainty of Other")
pOtherr



#------------- plaatjes van v2+retake -------------
# maak plaatje
p2010v2r <- ggplot(df.red.T2010v2_r, aes(x=Investment, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
p2010v2r <- p2010v2r + theme_bw()
p2010v2r <- p2010v2r + geom_point(size = 5)
p2010v2r <- p2010v2r + scale_shape_manual(values=c(50, 51, 49))
p2010v2r <- p2010v2r + theme(legend.position="none")
p2010v2r <- p2010v2r + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
p2010v2r <- p2010v2r + coord_cartesian(ylim = c(0,150))
p2010v2r <- p2010v2r + ggtitle("T2010")
p2010v2r


#Red.TCRE

# maak plaatje
pTCREv2r <- ggplot(df.red.TCREv2_r, aes(x=Investment, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pTCREv2r <- pTCREv2r + theme_bw()
pTCREv2r <- pTCREv2r + geom_point(size = 5)
pTCREv2r <- pTCREv2r + scale_shape_manual(values=c(50, 51, 49))
pTCREv2r <- pTCREv2r + theme(legend.position="none")
pTCREv2r <- pTCREv2r + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pTCREv2r <- pTCREv2r + coord_cartesian(ylim = c(0,150))
pTCREv2r <- pTCREv2r + ggtitle("TCRE")
pTCREv2r


#Red.FnonCO2

# maak plaatje
pFnonCO2v2r <- ggplot(df.red.FnonCO2v2_r, aes(x=Investment, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pFnonCO2v2r <- pFnonCO2v2r + theme_bw()
pFnonCO2v2r <- pFnonCO2v2r + geom_point(size = 5)
pFnonCO2v2r <- pFnonCO2v2r + scale_shape_manual(values=c(50, 51, 49))
pFnonCO2v2r <- pFnonCO2v2r + theme(legend.position="none")
pFnonCO2v2r <- pFnonCO2v2r + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pFnonCO2v2r <- pFnonCO2v2r + coord_cartesian(ylim = c(0,150))
pFnonCO2v2r <- pFnonCO2v2r + ggtitle("FnonCO2")
pFnonCO2v2r


#Red.Costs

# maak plaatje
pCostsv2r <- ggplot(df.red.Costsv2_r, aes(x=Investment, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pCostsv2r <- pCostsv2r + theme_bw()
pCostsv2r <- pCostsv2r + geom_point(size = 5)
pCostsv2r <- pCostsv2r + scale_shape_manual(values=c(50, 51, 49))
pCostsv2r <- pCostsv2r + theme(legend.position="none")
pCostsv2r <- pCostsv2r + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pCostsv2r <- pCostsv2r + coord_cartesian(ylim = c(0,150))
pCostsv2r <- pCostsv2r + ggtitle("Costs")
pCostsv2r


#Red.Other

# maak plaatje
pOtherv2r <- ggplot(df.red.Otherv2_r, aes(x=Investment, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
pOtherv2r <- pOtherv2r + theme_bw()
pOtherv2r <- pOtherv2r + geom_point(size = 5)
pOtherv2r <- pOtherv2r + scale_shape_manual(values=c(50, 51, 49))
pOtherv2r <- pOtherv2r + theme(legend.position="none")
pOtherv2r <- pOtherv2r + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
pOtherv2r <- pOtherv2r + coord_cartesian(ylim = c(0,150))
pOtherv2r <- pOtherv2r + ggtitle("Other")
pOtherv2r

# multiplot 3x2
multiplot(p2010v2r, pTCREv2r, pFnonCO2v2r, pCostsv2r, pOtherv2r,cols=3,layout = matrix(c(1,4,2,5,3,6),nrow=2)) #layout = matrix(c(1,4,7,10,2,5,8,11,3,6,9,12),


