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

setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Expert elicitation/Responses/Enquetev2")


# csv's inlezen
T2010v2 <- read.csv(file = "./T2010v2.csv", header = TRUE, sep = ";")
TCREv2 <- read.csv(file = "./TCREv2.csv", header = TRUE, sep = ";")
FnonCO2v2 <- read.csv(file = "./TFnonCO2v2.csv", header = TRUE, sep = ";")
Costsv2 <- read.csv(file = "./Costsv2.csv", header = TRUE, sep = ";")
Otherv2 <- read.csv(file = "./Otherv2.csv", header = TRUE, sep = ";")
T2010v2 <- T2010v2[-c(4,5,6,7),]

Red.T2010v2 <- read.csv(file = "./Red_T2010v2.csv", header = TRUE, sep = ";")
Red.TCREv2 <- read.csv(file = "./Red_TCREv2.csv", header = TRUE, sep = ";")
Red.FnonCO2v2 <- read.csv(file = "./Red_TFnonCO2v2.csv", header = TRUE, sep = ";")
Red.Costsv2 <- read.csv(file = "./Red_Costsv2.csv", header = TRUE, sep = ";")
Red.Otherv2 <- read.csv(file = "./Red_Otherv2.csv", header = TRUE, sep = ";")


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
p <- ggplot(df.T2010v2, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
p <- p + geom_point(size = 3)
p <- p + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
p <- p + theme_bw()
p <- p + coord_cartesian(ylim = c(0,10))
p <- p + ggtitle("T2010 Q2")
p


#TCRE
# maak geschikt data.frame
df.TCREv2 <- data.frame(Ttarget = rep(c(1.5,2,3), each = 3), parameter = "TCRE", value = c(TCREv2$X1.5,TCREv2$X2,TCREv2$X3))
df.TCREv2$WG = as.character(TCREv2$WG)
df.TCREv2$Resp = as.character(1:3)

# maak plaatje
p <- ggplot(df.TCREv2, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
p <- p + geom_point(size = 3)
p <- p + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
p <- p + theme_bw()
p <- p + coord_cartesian(ylim = c(0,10))
p <- p + ggtitle("TCRE")
p


#FnonCO2
# maak geschikt data.frame
df.FnonCO2v2 <- data.frame(Ttarget = rep(c(1.5,2,3), each = 3), parameter = "FnonCO2", value = c(FnonCO2v2$X1.5,FnonCO2v2$X2,FnonCO2v2$X3))
df.FnonCO2v2$WG = as.character(FnonCO2v2$WG)
df.FnonCO2v2$Resp = as.character(1:3)

# maak plaatje
p <- ggplot(df.FnonCO2v2, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
p <- p + geom_point(size = 3)
p <- p + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
p <- p + theme_bw()
p <- p + coord_cartesian(ylim = c(0,10))
p <- p + ggtitle("FnonCO2")
p


#Costs
# maak geschikt data.frame
df.Costsv2 <- data.frame(Ttarget = rep(c(1.5,2,3), each = 3), parameter = "Costs", value = c(Costsv2$X1.5,Costsv2$X2,Costsv2$X3))
df.Costsv2$WG = as.character(Costsv2$WG)
df.Costsv2$Resp = as.character(1:3)

# maak plaatje
p <- ggplot(df.Costsv2, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
p <- p + geom_point(size = 3)
p <- p + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
p <- p + theme_bw()
p <- p + coord_cartesian(ylim = c(0,10))
p <- p + ggtitle("Costs")
p


#Other
# maak geschikt data.frame
df.Otherv2 <- data.frame(Ttarget = rep(c(1.5,2,3), each = 3), parameter = "Other", value = c(Otherv2$X1.5,Otherv2$X2,Otherv2$X3))
df.Otherv2$WG = as.character(Otherv2$WG)
df.Otherv2$Resp = as.character(1:3)

# maak plaatje
p <- ggplot(df.Otherv2, aes(x=Ttarget, y=value, color=Resp, group=Resp, shape=WG)) #col=interaction(minmedmax,group)
p <- p + geom_point(size = 3)
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
p <- p + geom_point(size = 3)
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
p <- p + geom_point(size = 3)
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
p <- p + geom_point(size = 3)
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
p <- p + geom_point(size = 3)
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
p <- p + geom_point(size = 3)
p <- p + geom_line() # aes(x=Ttarget, y=value, group=, colour = minmedmax)
p <- p + theme_bw()
p <- p + coord_cartesian(ylim = c(0,10))
p <- p + ggtitle("Reduction in uncertainty of Other")
p






