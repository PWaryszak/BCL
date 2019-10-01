#Libraries needed:
library(tidyverse)#install.packages ("tidyverse")
library(lubridate)#install.packages ("lubridate")


#COMPUTE EMISSION RATE OFF UGGA txt FILE =====
#Subset only pre-defined start and stop time (off our field notes).
ugga_data <- read.delim("gga_2019-07-03_f0000.txt",sep = "," , skip =1) %>% #Read in gas flux data
  mutate(Datetime = as.POSIXct(strptime(Time,"%d/%m/%Y %H:%M:%S"))) %>%
  mutate(Site = ifelse(Datetime>='2019-07-03 08:08:08' & Datetime <='2019-07-03 08:14:49',"SITE1",
                       ifelse(Datetime >='2019-07-03 08:14:58' & Datetime <='2019-07-03 08:19:59',"SITE2",
                              "BLANK")))%>%
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
ugga_data_new <- filter(ugga_data, NumTime >= 75 & Site =="SITE1")

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