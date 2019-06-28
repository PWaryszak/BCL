#Load libraries and set working directory:
install.packages("tidyverse")
library(tidyverse)
setwd("~/Documents/0.DeakinUni/RTK")#set working directory (where you store your data)

#GGPLOT BASS:====
rtk <- read.csv("WP_Bass_Fencing.csv") #read in data
str(rtk)#check structure of your data
rtk$year <- factor(rtk$year, levels = c("12YR", "2YR","2019"))#order levels of year so that they are showed chronologically

ggplot(rtk,aes(x=location, y=elevation, color = location)) +
  geom_boxplot()+
  labs(x = "",y="Elevation (m)")+
  geom_boxplot()+ geom_jitter()+
  facet_grid(.~year,scale="fixed") +
  theme_bw()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=24),
        axis.title.x=element_blank(),
        legend.position = "none",
        strip.text=element_text(size=24))


#GGPLOT Gippsland Control:=====
ctr <- read.csv("Gippsland_Control.csv")
ctr$location <- factor(ctr$location, levels = c("LOW", "MEDIUM","HIGH"))
str(ctr)

ggplot(ctr,aes(x=location, y=elevation, color = location))+
  #labs(x = "",y="",colour="year type")+
  geom_boxplot()+ geom_jitter()+
  facet_grid(.~transect,scale="fixed") +
  theme_bw()+
  ggtitle("Gippsland Control/Disturbed")+
    theme(axis.text.x=element_text(vjust=0.5,size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=24),
        axis.title.x=element_blank(),
        legend.position = "none",
        strip.text=element_text(size=24))


#GGPLOT Gippsland Control:=====
gipps <- read.csv("Gippsland_Rehab.csv")
gipps$location <- factor(gipps$location, levels = c("LOW", "MEDIUM","HIGH"))
str(gipps)

ggplot(gipps,aes(x=location, y=elevation, color = location))+
  #labs(x = "",y="",colour="year type")+
  geom_boxplot()+ geom_jitter()+
  ggtitle("Gippsland Rehab")+
  facet_grid(.~transect,scale="fixed") +
  theme_bw()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=24),
        axis.title.x=element_blank(),
        legend.position = "none",
        strip.text=element_text(size=24))
