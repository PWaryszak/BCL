#Read data in:
library(tidyverse)
library(lubridate)

#LOAD & VISUZALIZE CO2 DATA off one UGGA chamber=======
#Subset only pre-defined start and stop time (off our field notes).
ugga_data <- read.delim("gga_2019-07-03_f0000.txt",sep = "," , skip =1) %>%
  mutate(DateTime = as.POSIXct(strptime(Time,"%d/%m/%Y %H:%M:%S"))) %>%
  filter(DateTime >= '2019-07-03 08:08:08' & #filter time interval recorded in the field
           DateTime <= '2019-07-03 08:14:49' ) %>% #Time stop here.
  mutate ( Site =as.factor("STON_T3_1996_HIGH")) %>% #Assign this time interal to a site as per field notes
  mutate ( NumTime = row_number()) %>% #produce numeric values for time to run lm on.
  mutate (slope = coef(summary(lm (X.CO2._ppm ~ NumTime)))[2,1]) #extract slope from lm

#Draw the plot. Look for outliers if any:
  ggplot(ugga_data, aes(x = NumTime, y = X.CO2._ppm)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")


  
###################################################################
#Look at plots of CO2 vs time ============
library(grid)
library(gridExtra)

#Points around NumTime = 75 fluctute. Let's cut them out:
ugga_data_new <- filter(ugga_data, NumTime >= 75)

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
u1<- ggplotRegression(lm(X.CO2._ppm ~ NumTime, data = ugga_data)) +labs(title = "STON_T3_1996_HIGH")
u2<- ggplotRegression(lm(X.CO2._ppm ~ NumTime, data = ugga_data_new))+labs(title = "STON_T3_1996_HIGH")
grid.arrange(u1,u2) #Very small difference.









#########################################################################
#Below is Work in progress = TO DO a LOOP:======
#Load StopStart Data (off field notes):
StopStart <- read.csv("StonStartStop.csv") %>%
  mutate(TimeStart = as.POSIXct(strptime(start,"%d/%m/%Y %H:%M:%S"))) %>%
  mutate(TimeStop = as.POSIXct(strptime(stop,"%d/%m/%Y %H:%M:%S"))) %>%
  mutate (CutStart = TimeStart - 1)

u <- read.delim("gga_2019-07-03_f0000.txt",sep = "," , skip =1) %>% #Flux Data
  mutate(DateTime = as.POSIXct(strptime(Time,"%d/%m/%Y %H:%M:%S")))

# Assign site category:
CutTime <- StopStart$CutStart
http://r.789695.n4.nabble.com/problem-with-rbind-when-data-frame-contains-an-date-time-variable-quot-POSIXt-quot-quot-POSIXlt-quot-td3312088.htmlcut2 <- StopStart[1,"TimeStop"]
CutTimes2 <- rbind(CutTime,cut2)

sites <- as.vector(StopStart[1:8, "Site.ID"])
u$site <- cut(u$DateTime, breaks = CutTimes,
              labels = sites,
              include.lowest=T)
check <- filter(u, site == "STON_1996_T3_HIGH")

#EXAMPLES of one chamber:
start <- '2019-07-03 08:08:08'
stop  <- '2019-07-03 08:14:49'
site <- "STON_T3_1996_HIGH"

CutTime <- function (x) ( 
  filter(x, DateTime >= start & Datetime <= stop ) 

CutTime (ugga_data)

coef(summary(lm (X.CO2._ppm ~ NumTime, data = z)))[2,1]
z1 <- mutate ( slope = coef(summary(lm (X.CO2._ppm ~ NumTime, data = z)))[2,1])


