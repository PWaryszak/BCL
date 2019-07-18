#Read data in:
library(tidyverse)
library(lubridate)
#Define your time intervals:
start <- '2019-07-03 07:55:28'
stop  <-  '2019-07-03 08:00:28'

#Read in gas flux data
ugga_data <- read.delim("gga_2019-07-03_f0000.txt",sep = "," , skip =1) %>%
  mutate(Datetime = as.POSIXct(strptime(Time,"%d/%m/%Y %H:%M:%S"))) %>% #Convert Time to POSIXct format:
  filter(Datetime >= start & Datetime >= stop) #subset the time pre-define time interval
