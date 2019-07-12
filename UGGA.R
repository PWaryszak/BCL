#Read data in:
library(tidyverse)
library(lubridate)
ugga_data <- read.delim("gga_2019-07-03_f0000.txt",sep = "," , skip =1)
head(ugga_data)
d2 <- separate (ugga_data, Time, into =  c("bla","bla2", "date","time"), sep = " ") %>%
  select(date, time, X.CH4._ppm, X.CH4.d_ppm, X.CO2._ppm, X.CO2.d_ppm,
         GasT_C, AmbT_C, RD0_us, RD1_us, Fit_Flag, MIU_DESC)

d2$time <- as.factor((d2$time))
d3<- separate(d2, time, into = c("Min","Msec"), sep = "[.]",extra = "merge") %>%
  unite(DateTime, c(date, Min), remove=FALSE, sep = " ") %>%
  mutate (StartTime = dmy_hms(DateTime)) %>%
  filter (StartTime == "03/07/2019 08:08:43" + minutes(5))

View(d3)


#Time is still a character. We need to convert it to time format:
https://r4ds.had.co.nz/dates-and-times.html#time-spans



TimeStart <- data.frame(DataStart = c("07:55:29","08:11:19"))
TimeStart

CutData <- function (x), 
myfunction <- function(x, y, ... ){
  
  statements
  return(object)
}
View(d3)
