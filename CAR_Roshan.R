#Calculating Carbon Accretion Rate (CAR):
#LOAD PACKAGES:
library(tidyverse)
library(gridExtra)
library(readxl)

#LOAD 210Pb Data:=========
#for cores where average value for MAR using the CF.CS model is produced,
# multiply this by the % C in each section to obtain the CAR in gC/m2/yr (Carbon Accretion Rate):
#MAR values (g/cm2/y) are based on file sent from Pere: "Report Pb-210 dating Western Australia Macreadie 2023"

#Create dataset off Pere's file listing Mass Accretion Rates (MAR, units = g/cm2/y):
MAR <- data.frame( CoreID =   c("Wils_Age","Broo_Age","Derb_Age","Arro_Age","Smit_Age","Sams_Age","Burr_Age", "Carn_Age"),
                   MAR =    c(0.034025, 0.28984,   NA,   0.11369,  NA,    0.12383, 0.01425, NA ),
                   MAR_SE = c(0.0025538,NA,       NA, 0.0063439,  NA,    0.01425, 0.03844, NA ))
MAR 

#LOAD Carbon Stock data=======
stock <- read_excel("WA_Blue_CarbonStock_new_estimates_from_pawel.xlsx", sheet = "Sheet1")
names(stock)

#Join Sites with MAR data:
stock_MAR <- left_join(stock,MAR, by = "CoreID")
View(stock_MAR)



#Compute Carbon Accretion Rate at Rehabilitated sites:
CAR_Roshan <- stock_MAR %>%
  select(CoreID, MAR, MAR_SE, CarbonStock.Mgha, C.percent) %>%
  mutate(CAR_gcm2y = (C.percent/100) * MAR *100) %>%# *100 to conver to tonnes per ha
  mutate (Stock = "Western_Australia_North") %>%
  group_by(CoreID) %>%
  summarise(Mean_CAR = weighted.mean (CAR_gcm2y, na.rm=T),
            N_CAR = length (CAR_gcm2y),
            SD_CAR =sd (CAR_gcm2y, na.rm=T),
            SE_CAR =sd (CAR_gcm2y, na.rm=T)/sqrt(N_CAR))

CAR_Roshan
write.csv(CAR_Roshan, file = "CAR_Roshan.csv", row.names = F) #Save as csv file
