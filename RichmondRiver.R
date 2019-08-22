#LOAD Richmond River DATA:=====
library(tidyverse)
library(gridExtra)
library(broom)

setwd("~/00DeakinUni/R/BCL_R/BCL")
aa <- read.csv("RR_aboveground.csv")
bb <- read.csv("RR_belowground.csv")

#Compute mean +- SE for belowground C-Stock:==============
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

#Compute mean +- SE for aboveground C-Stock:=========
aboveSum <- select (aa, Site, Total_Aboveground_Biomass_kg_100m2) %>%
  mutate(Total_Aboveground_Biomass_Mg_ha = Total_Aboveground_Biomass_kg_100m2 / 10 )%>% #1 kg/100m2 = 0.1 tonnes/ha
  gather(key = treat, value = mass, Total_Aboveground_Biomass_Mg_ha) %>% 
  group_by(Site) %>%
  summarise(AV=mean(mass, na.rm = T),
            SD=sd(mass),
            N = length(mass),
            SE= SD / sqrt(N)) %>%
  mutate (Stock = "Aboveground")

#Join/Plot above and below Stocks:=====
ab <- rbind (aboveSum,belowSum) %>%
  mutate(AV= ifelse(Stock =="Aboveground",AV,AV*-1))#turn belowground negative

#Draw a figure:
MyBreaks <- c(-300, -200,-100, -50, 0, 50, 100, 200,300 ,400,500)

ggplot(ab, aes(x=Site, y=AV, fill = Stock))+
 geom_bar(position="identity", stat="identity")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  geom_hline(yintercept=0)+
  scale_y_continuous(breaks = MyBreaks,labels = abs(MyBreaks))+ #abs to remove negative values on y-axis below 0
  scale_fill_manual(values = c("darkgreen","lightblue"))+
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

#Fig of Below carbon by treatment:====
TreatSum <- select(bb, Site, Treatment, Site_Core, Carbon_stock_in_section_Mg_Cha)%>%
  filter(Treatment != "Saltmarsh natural") %>%
  group_by(Site_Core) %>% #to sum total Carbon Stock per core.
  mutate(TotalPerCore = sum (Carbon_stock_in_section_Mg_Cha)) %>%
  group_by(Treatment) %>%
  summarise(AV = mean(TotalPerCore, na.rm = T),
            SD = sd(TotalPerCore),
            N = length(TotalPerCore),
            SE = SD / sqrt(N)) %>%
  mutate (Stock = "Belowground")
TreatSum
ggplot(TreatSum, aes(x=Treatment, y=AV, fill = Treatment))+
  geom_bar(position="identity", stat="identity") +
geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4) +
  ylab("Organic Carbon Stock (Mg/ha)")+
  theme(axis.text.x = element_text(size=16),
        axis.title.x = element_blank(),
        legend.position = "none")+
  ggtitle("Richmond River Project",
          subtitle = "Mean Carbon stock by treatment (whole core)")
  
  
#Fig of Below carbon by treatment and Depth_Range:====
levels(bb$Depth_Range)#"000_015" "015_030" "030_050" "050_075" "075_100" "100_125"
unique(bb$Depth_to)#"000_015" "015_030" "030_050" "050_075" "075_100" "100_125"

DepthTreatSum <- select(bb, Site,Site_Core, Treatment,Depth_to, Depth_Range, Site_Core, Carbon_stock_in_section_Mg_Cha)%>%
  filter(Treatment != "Saltmarsh natural") %>%
  filter(Depth_to <= 50 ) %>%
  group_by( Depth_Range,Treatment) %>% #to sum total Carbon Stock per core.
  summarise(AV = mean(Carbon_stock_in_section_Mg_Cha, na.rm = T),
            SD = sd(Carbon_stock_in_section_Mg_Cha),
            N = length(Carbon_stock_in_section_Mg_Cha),
            SE = SD / sqrt(N)) %>%
  mutate (Stock = "Belowground")
DepthTreatSum

ggplot(DepthTreatSum, aes(x=Treatment, y=AV, fill = Treatment))+
  geom_bar(position="identity", stat="identity") +
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4) +
  facet_grid(Depth_Range~.)+
  theme_bw () +
  ggtitle("Richmond River Project",
          subtitle = "Mean Carbon stock by Depth_Range till 50 cm")+
  ylab("Organic Carbon Stock (Mg/ha)")+
    theme(axis.text.x = element_text(),
        legend.position = "none")

#Histogram: of Carbon across Depth_Ranges=======
bbb <- select(bb,Depth_Range, Site, Site_Core, Carbon_stock_in_section_Mg_Cha)%>%
  group_by(Site_Core) %>% #to sum total Carbon Stock per core.
  mutate(TotalPerCore = sum (Carbon_stock_in_section_Mg_Cha)) 
  
ggplot(bb, aes(Carbon_stock_in_section_Mg_Cha, fill = Treatment))+
  geom_histogram() +facet_grid(Depth_Range~.)+ theme_bw()+
  theme(axis.text.x=element_text(vjust=0.5,size=12, angle = 90),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=20),
        legend.position = "bottom")

#Average Max Depth Reached=====
MaxDepth <- select(bb,Site_Core, Depth_to)%>%
  group_by(Site_Core) %>% #to sum total Carbon Stock per core.
  slice( which.max(Depth_to)) %>% #slice out rows with max Depth_to 
  ungroup() %>%
  mutate(Average_Max_Depth = mean(Depth_to, na.rm = T),
            SD = sd(Depth_to),
            N = length(Depth_to),
            SE = SD / sqrt(N)) 
range (MaxDepth$Depth_to)#38 110
length (MaxDepth[ MaxDepth$Depth_to < 50,]) # 6 cores were below 50 cm
100 - (6/48 * 100) #87.5% of cores were longer than 50 cm.


#Model:=======
Mangroves <- filter (bb, Treatment != "Saltmarsh natural" ) %>%
  filter (Depth_to <= 50)
summary(lm(Carbon_stock_in_section_Mg_Cha ~ Depth_Range+Treatment,
           Mangroves))
augment(lm(Carbon_stock_in_section_Mg_Cha ~ Depth_Range+Treatment,
           Mangroves))

#Corrected for compaction:=========
CorrectedDepthCarbon <- select(bb, Site,Site_Core, Treatment,Depth_to, Depth_Range,
                        Site_Core, Carbon_stock_in_section_Mg_Cha,
                        Lab_Compaction_Correction_Value)%>%
  filter(Treatment != "Saltmarsh natural") %>%
  filter(Depth_to <= 50 ) %>%
  mutate( DepthCorrected = Depth_to / Lab_Compaction_Correction_Value,
          DepthChange = DepthCorrected-Depth_to,
          DepthChangePerc = 100-(DepthChange/Depth_to *100), #Percent increase in depth
          CorrectedCarbonStock = Carbon_stock_in_section_Mg_Cha * DepthChangePerc/100) ##Percent decrease in Carbon Stock when cut at 50cm

CorrectedDepthCarbon

BelowSum50_corrected<- select(CorrectedDepthCarbon, Site, Site_Core,
                             Carbon_stock_in_section_Mg_Cha,CorrectedCarbonStock)%>%
  group_by(Site) %>% #to sum total Carbon Stock per core.
  summarise(AV = mean(CorrectedCarbonStock, na.rm = T),
            SD = sd(CorrectedCarbonStock),
            N = length(CorrectedCarbonStock),
            SE = SD / sqrt(N)) %>%
  mutate (CarbonStock = "Corrected")

c1<-ggplot(BelowSum50_corrected , aes(x=Site, y=AV, fill = Site))+
  geom_bar(position="identity", stat="identity")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4) +
  theme(legend.position = "none")+
  scale_y_continuous(limits = c(0,80))+
  ggtitle("Richmond River Project", subtitle = "CarbonStock = CORRECTED")



BelowSum50 <- select(CorrectedDepthCarbon, Site,Carbon_stock_in_section_Mg_Cha)%>%
  group_by(Site) %>%
  summarise(AV = mean(Carbon_stock_in_section_Mg_Cha, na.rm = T),
            SD = sd(Carbon_stock_in_section_Mg_Cha),
            N = length(Carbon_stock_in_section_Mg_Cha),
            SE = SD / sqrt(N)) %>%
  mutate (CarbonStock = "Old")

c2<-ggplot(BelowSum50 , aes(x=Site, y=AV, fill =Site))+
  geom_bar(position="identity", stat="identity")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0,80))+
  ggtitle("Richmond River Project", subtitle = "CarbonStock = OLD")

grid.arrange(c1,c2)
