#Richmond River DATA:====
library(tidyverse)
setwd("~/00DeakinUni/R/BCL_R/BCL")
aa <- read.csv("RR_aboveground.csv")
bb <- read.csv("RR_belowground.csv")

#Compute mean +- SE for belowground C-Stock:
belowSum <- select(bb, Site, Site_Core, Carbon_stock_in_section_Mg_Cha)%>%
  group_by(Site_Core) %>% #to sum total Carbon Stock per core.
  mutate(TotalPerCore = sum (Carbon_stock_in_section_Mg_Cha)) %>%
  group_by(Site) %>%
           summarise(AV = mean(TotalPerCore, na.rm = T),
                     SD = sd(TotalPerCore),
                      N = length(TotalPerCore),
                     SE = SD / sqrt(N)) %>%
  mutate (Stock = "Belowground")
belowSum

#Compute mean +- SE for aboveground C-Stock:
aboveSum <- select (aa, Site, Total_Aboveground_Biomass_kg_100m2) %>%
  mutate(Total_Aboveground_Biomass_Mg_ha = Total_Aboveground_Biomass_kg_100m2 / 10 )%>% #1 kg/100m2 = 0.1 tonnes/ha
  gather(key = treat, value = mass, Total_Aboveground_Biomass_Mg_ha) %>% 
  group_by(Site) %>%
  summarise(AV=mean(mass, na.rm = T),
            SD=sd(mass),
            N = length(mass),
            SE= SD / sqrt(N)) %>%
  mutate (Stock = "Aboveground")


#Join above and below Stocks:
ab <- rbind (aboveSum,belowSum) %>%
  mutate(AV= ifelse(Stock =="Aboveground",AV,AV*-1))#turn belowground negative

#Draw a figure:
MyBreaks <- c(-300, -200,-100, -50, 0, 50, 100, 200,300 ,400,500)

ggplot(ab, aes(x=Site, y=AV, fill = Stock))+
 geom_bar(position="identity", stat="identity")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  geom_hline(yintercept=0)+
  scale_y_continuous(breaks = MyBreaks,labels = abs(MyBreaks))+ #abs to remove negative values on y-axis below 0
  scale_fill_manual(values = c("lightblue","darkgreen"))+
  xlab("")+ylab("Organic Carbon Stock (Mg/ha)")+
  ggtitle("Richmond River Project")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12, angle = 90),
         axis.text.y=element_text(size=12),
         axis.title.y=element_text(size=20),
         legend.position = c(.85, .93),
         legend.text = element_text(size = 16),
         legend.title = element_text(face = "italic",size=16),
         plot.title = element_text(lineheight=1.2, face="bold",size=20))
