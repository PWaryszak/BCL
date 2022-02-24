#Libraries needed:
library(tidyverse)#install.packages ("tidyverse")
library(lubridate)#install.packages ("lubridate")
library(corrplot)
library(Hmisc)
library(vegan)
library(gridExtra)
library(ggpmisc)
library(grid)


#COMPUTE EMISSION RATE OFF UGGA txt FILE =====
#Subset only pre-defined start and stop time (off our field notes).
ugga_data <- read.delim("gga_2019-07-03_f0000.txt",sep = "," , skip =1) %>% #Read in gas flux data
  mutate(Datetime = as.POSIXct(strptime(Time,"%d/%m/%Y %H:%M:%S"))) %>%
  mutate(Site = ifelse(Datetime>='2019-07-03 08:08:08' & Datetime <='2019-07-03 08:14:49',"SITE1",
                       ifelse(Datetime >='2019-07-03 08:14:58' & Datetime <='2019-07-03 08:19:59',"SITE2",
                              "BLANK")))%>%
  filter (Site != "BLANK") %>% #remove all records in between chambers.  I called them BLANK
  group_by(Site)%>%
  mutate ( NumTime = seq_along(Site)) %>% #produce numeric values for time to run lm on.
  mutate (Slope = coef(summary(lm (X.CO2._ppm ~ NumTime)))[2,1]) %>% #extract slope from lm
  mutate ( Chamber_Volume.m3 = 0.0353250, #This may need change if chamber was flooded
           Chamber_Surface.m2 = 0.07065,
           ConvertPPM_to_ugm3 = 1798.45,
           ConvertSec_to_Days = 86400,
           Gas_Flux = (Slope *ConvertPPM_to_ugm3*86400*Chamber_Volume.m3)/
             (Chamber_Surface.m2*1000)) #Gas flux in micro-grams per m2 per Day (ug)

unique(ugga_data$Gas_Flux)/1000

#Draw the plot for SITE1:
ggplot(ugga_data[ugga_data$Site=="SITE1",] , aes(x = NumTime, y = X.CO2._ppm)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

###################################################################
#Look at CO2 emission against two time intervals ============
library(grid)
library(gridExtra)

#Points around NumTime = 75 fluctute. Let's cut them out:
ugga_data_old <- filter(ugga_data, Site =="SITE1")
ugga_data_new <- filter(ugga_data, NumTime >= 105 & Site =="SITE1")

#Compare different plots using this function:
ggplotRegression <- function (fit) {
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(subtitle = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

#Compare plots:
u1<- ggplotRegression(lm(X.CO2._ppm ~ NumTime, data = ugga_data_old)) +labs(title = "SITE1_OLD")
u2<- ggplotRegression(lm(X.CO2._ppm ~ NumTime, data = ugga_data_new))+labs(title = "SITE1_NEW")
grid.arrange(u1,u2) #Very small difference in R2

#4MARTINO (See "NSW farm dams UGGA log.xlsx" for times) ======

# 02 May was not sampled:
ugga_data <- read.delim("FarmDamUGGA_20210502_f0000.txt",sep = "," , skip =1) %>% #Read in gas flux data
  mutate(Datetime = as.POSIXct(strptime(Time,"%d/%m/%Y %H:%M:%S"))) %>%
  mutate(Site = ifelse(Datetime>='2021-05-02 21:11:04' & Datetime <='2021-05-02 21:44:307',"SITE1","BLANK")) %>%
  filter (Site != "BLANK") %>% #remove all records in between chambers.  Icalled them BLANK
  group_by(Site)%>%
  mutate ( NumTime = seq_along(Site)) %>% #produce numeric values for time to run lm on.
  mutate (Slope = coef(summary(lm (X.CO2._ppm ~ NumTime)))[2,1]) %>% #extract slope from lm
  mutate ( Chamber_Volume.m3 = 0.0353250, #This may need change if chamber was flooded
           Chamber_Surface.m2 = 0.07065,
           ConvertPPM_to_ugm3 = 1798.45,
           ConvertSec_to_Days = 86400,
           Gas_Flux = (Slope *ConvertPPM_to_ugm3*86400*Chamber_Volume.m3)/
             (Chamber_Surface.m2*1000))

unique(ugga_data$Gas_Flux)

# 03 May CO2 was sampled during heavy rain:====
ugga_data <- read.delim("FarmDamUGGA_20210503_f0000.txt",sep = "," , skip =1) %>% #Read in gas flux data
  mutate(Datetime = as.POSIXct(strptime(Time,"%d/%m/%Y %H:%M:%S"))) %>%
  mutate(Site = ifelse(Datetime>='2021-05-03 07:57:00' & Datetime <='2021-05-03 08:02:00',"SITE1","BLANK")) %>%
  filter (Site != "BLANK") %>% #remove all records in between chambers.  Icalled them BLANK
  group_by(Site)%>%
  mutate ( NumTime = seq_along(Site)) %>% #produce numeric values for time to run lm on.
  mutate (Slope = coef(summary(lm (X.CO2._ppm ~ NumTime)))[2,1]) %>% #extract slope from lm
  mutate ( Chamber_Volume.m3 = 0.0353250, #This may need change if chamber was flooded
           Chamber_Surface.m2 = 0.07065,
           ConvertPPM_to_ugm3 = 1798.45,
           ConvertSec_to_Days = 86400,
           Gas_Flux = (Slope *ConvertPPM_to_ugm3*86400*Chamber_Volume.m3)/
             (Chamber_Surface.m2*1000))

#Draw the plot for SITE1:
ggplot(ugga_data[ugga_data$Site=="SITE1",] , aes(x = NumTime, y = X.CO2._ppm)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+ ggtitle("03-May-2021 record (heavy rain day)")

# 03 May CH4 was sampled during heavy rain:=========
ugga_data <- read.delim("FarmDamUGGA_20210503_f0000.txt",sep = "," , skip =1) %>% #Read in gas flux data
  mutate(Datetime = as.POSIXct(strptime(Time,"%d/%m/%Y %H:%M:%S"))) %>%
  mutate(Site = ifelse(Datetime>='2021-05-03 07:57:00' & Datetime <='2021-05-03 08:02:00',"SITE1","BLANK")) %>%
  filter (Site != "BLANK") %>% #remove all records in between chambers.  Icalled them BLANK
  group_by(Site)%>%
  mutate ( NumTime = seq_along(Site)) %>% #produce numeric values for time to run lm on.
  mutate (Slope = coef(summary(lm (X.CH4._ppm ~ NumTime)))[2,1]) %>% #extract slope from lm
  mutate ( Chamber_Volume.m3 = 0.0353250, #This may need change if chamber was flooded
           Chamber_Surface.m2 = 0.07065,
           ConvertPPM_to_ugm3 = 1798.45,
           ConvertSec_to_Days = 86400,
           Gas_Flux = (Slope *ConvertPPM_to_ugm3*86400*Chamber_Volume.m3)/
             (Chamber_Surface.m2*1000))

#Draw the plot for SITE1:
ggplot(ugga_data[ugga_data$Site=="SITE1",] , aes(x = NumTime, y = X.CH4._ppm)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+ ggtitle("03-May-2021 record (heavy rain day)")

# 03 May CO2  STEP2 (Cloudy):====
ugga_data <- read.delim("FarmDamUGGA_20210503_f0001.txt",sep = "," , skip =1) %>% #Read in gas flux data
  mutate(Datetime = as.POSIXct(strptime(Time,"%d/%m/%Y %H:%M:%S"))) %>%
  mutate(Site = ifelse(Datetime>='2021-05-03 08:56:00' & Datetime <='2021-05-03 09:05:00',"SITE1","BLANK")) %>%
  filter (Site != "BLANK") %>% #remove all records in between chambers.  Icalled them BLANK
  group_by(Site)%>%
  mutate ( NumTime = seq_along(Site)) %>% #produce numeric values for time to run lm on.
  mutate (Slope = coef(summary(lm (X.CO2._ppm ~ NumTime)))[2,1]) %>% #extract slope from lm
  mutate ( Chamber_Volume.m3 = 0.0353250, #This may need change if chamber was flooded
           Chamber_Surface.m2 = 0.07065,
           ConvertPPM_to_ugm3 = 1798.45,
           ConvertSec_to_Days = 86400,
           Gas_Flux = (Slope *ConvertPPM_to_ugm3*86400*Chamber_Volume.m3)/
             (Chamber_Surface.m2*1000))

unique(ugga_data$Gas_Flux)
View(ugga_data)#first slope = 0 to 90, second slope = 143 to 249, third slope = 325 to 444

#First Slope:
ugga_data <- read.delim("FarmDamUGGA_20210503_f0001.txt",sep = "," , skip =1) %>% #Read in gas flux data
  mutate(Datetime = as.POSIXct(strptime(Time,"%d/%m/%Y %H:%M:%S"))) %>%
  mutate(Site = ifelse(Datetime>='2021-05-03 08:56:00' & Datetime <='2021-05-03 09:05:00',"SITE1","BLANK")) %>%
  filter (Site != "BLANK") %>% #remove all records in between chambers.  Icalled them BLANK
  group_by(Site)%>%
  mutate ( NumTime = seq_along(Site)) %>% #produce numeric values for time to run lm on.
  
  #Filter for first slope at STEP2 farm dam
  filter(NumTime > 0 & NumTime <90) %>%
  mutate (Slope = coef(summary(lm (X.CO2._ppm ~ NumTime)))[2,1]) %>% #extract slope from lm
  mutate ( Chamber_Volume.m3 = 0.0353250, #This may need change if chamber was flooded
           Chamber_Surface.m2 = 0.07065,
           ConvertPPM_to_ugm3 = 1798.45,
           ConvertSec_to_Days = 86400,
           Gas_Flux = (Slope *ConvertPPM_to_ugm3*86400*Chamber_Volume.m3)/
             (Chamber_Surface.m2*1000))

unique(ugga_data$Gas_Flux)# 14160.39
round(unique(ugga_data$Gas_Flux)/1000, 2) #14.16 mg/m2/Day, Convertd gas flux to mg per m2 per Day

summary(lm (X.CO2._ppm ~ NumTime, data = ugga_data ))#R-squared:  0.951

#Draw the plot for SITE1:
ggplot(ugga_data[ugga_data$Site=="SITE1",] , aes(x = NumTime, y = X.CO2._ppm)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+ ggtitle("03-May-2021 record (STEP2 farm dam)")+
  stat_fit_glance(method = "lm",
                  label.x = c(0.5,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)+
  ggtitle ("STEP2 Farm Dam CO2 flux of 14.16 (mg/m2/day)")


# 03 May CH4 STEP2 (Cloudy):=========
#First Slope
ugga_data <- read.delim("FarmDamUGGA_20210503_f0001.txt", sep = "," , skip =1) %>% #Read in gas flux data
  mutate(Datetime = as.POSIXct(strptime(Time,"%d/%m/%Y %H:%M:%S"))) %>%
  mutate(Site = ifelse(Datetime>='2021-05-03 08:56:00' & Datetime <='2021-05-03 09:05:00',"SITE1","BLANK")) %>%
  filter (Site != "BLANK") %>% #remove all records in between chambers.  Icalled them BLANK
  group_by(Site)%>%
  mutate ( NumTime = seq_along(Site)) %>% #produce numeric values for time to run lm on
  
  #First slope:
  filter(NumTime < 91) %>% #First slope stops here
  mutate (Slope = coef(summary(lm (X.CH4._ppm ~ NumTime)))[2,1]) %>% #extract slope from lm
  mutate ( Chamber_Volume.m3 = 0.0353250, #This may need change if chamber was flooded
           Chamber_Surface.m2 = 0.07065,
           ConvertPPM_to_ugm3 = 1798.45,
           ConvertSec_to_Days = 86400,
           Gas_Flux = (Slope *ConvertPPM_to_ugm3*86400*Chamber_Volume.m3)/
             (Chamber_Surface.m2*1000))

unique(ugga_data$Gas_Flux)#309.2979
round(unique(ugga_data$Gas_Flux)/1000, 2) #0.31 mg/m2/Day Convert gas flux to mg per m2 per Day



#Draw the plot for SITE1:
ggplot(ugga_data[ugga_data$Site=="SITE1",] , aes(x = NumTime, y = X.CH4._ppm)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")+ 
  stat_fit_glance(method = "lm",
                  label.x = c(0.5,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)+
  ggtitle ("STEP2 Farm Dam CH4 flux of 0.31 (mg/m2/day)")


