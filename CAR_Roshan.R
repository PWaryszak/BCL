#Calculating Carbon Accretion Rate (CAR)========

#LOAD 210Pb Data:=========
#for cores where average value for MAR using the CF.CS model is produced,
# multiply this by the % C in each section to obtain the CAR in gC/m2/yr (Carbon Accretion Rate):
#MAR values (g/cm2/y) are based on file sent from Pere: "Report Pb-210 dating Western Australia Macreadie 2023"

library(tidyverse)
library(gridExtra)

bb <- read.csv("RR_Soil.csv") #All Data
SiteTreat <- read.csv("RR_SiteTreatment_PC.csv")# Treatments to join to All Data
bb <- bb %>%  filter(Treatment != "Saltmarsh_natural")#Remove this habitat, not assessed
bb$Site <- factor(bb$Site) #Remove the extra factor level of SNND

#Create dataset off Pere's file above. MAR's units = g/cm2/y
MAR <- data.frame( Site = c("MNND","SNND","MNDC","MDBP",
                            "MDSD","MR1991","MR1992","MRAP",
                            "MRCH1","MRCH2","MRPL","MDTUCK", "MNPL"),
                   MAR = c(0.095, 0.096, NA,     0, 
                           0,    0.0324 ,0.076,0.32,
                           NA,  0.16,NA, NA   ,0.045),
                   MAR_SE = c(0.008,0.016, NA, 0,
                              0, 0.0010,0.005,0.11,
                              NA,0.02,NA, NA, 0.002))

#Join Sites with MAR data:
SiteTreatMAR <- left_join(SiteTreat,MAR, by = "Site")
SiteTreatMAR#list sites and corresponding treatments


#Correct Soil data for compaction and #Compute corrected C-stock (Off Bulk Density):
NewDATA <- bb
NewDATA$C_percent <- ifelse(NewDATA$C_perc == 0, 0.001, NewDATA$C_perc)#convert 0 into 0.001 to run log-models if any
NewDATA$SliceLength.cm <- (NewDATA$Depth_to - NewDATA$Depth_from) #cm
NewDATA$PipeDiameter.cm <- 5 #Diameter of coring pipes was 5 cm
NewDATA$SampleVolume.cm3 <- (pi*(NewDATA$PipeDiameter.cm/2)^2)*NewDATA$SliceLength.cm  #slice volume
NewDATA$dry_bulk_density.gcm3_corrected <- NewDATA$Dry_bulk_density_gcm3 * NewDATA$Lab_Compaction_Correction_Value
NewDATA$CarbonDensity.gcm3 <- NewDATA$dry_bulk_density.gcm3_corrected * NewDATA$C_percent/100
NewDATA$CarbonStock.Mgha <- ((NewDATA$CarbonDensity.gcm3  * 100) * NewDATA$SliceLength.cm )

#Compute Carbon Accretion Rate at Rehabilitated sites:
CAR_Rehabiliated <-
  left_join(bb,SiteTreatMAR, by = "Site") %>%
  select(C_perc, Site, Site_Core, MAR, MAR_SE, SiteRenamed,SiteNew) %>%
  mutate(CAR_gcm2y = (C_perc/100) * MAR *100) %>%# *100 to conver to tonnes per ha
  mutate (Stock = "Belowground") %>%
  filter(SiteRenamed == "Rehabilitated") %>%
  group_by(Site,SiteNew) %>%
  summarise(Mean_CAR = weighted.mean (CAR_gcm2y, na.rm=T),
            N_CAR = length (CAR_gcm2y),
            SD_CAR =sd (CAR_gcm2y, na.rm=T),
            SE_CAR =sd (CAR_gcm2y, na.rm=T)/sqrt(N_CAR))

CAR_Rehabiliated

#Compute Mean CAR for Established site (abline):
CAR_Established <-   left_join(bb, SiteTreatMAR, by = "Site") %>%
  select(C_perc, Site, Site_Core, MAR, MAR_SE, SiteRenamed) %>%
  mutate(CAR_gcm2y = (C_perc/100) * MAR  *100) %>% # *100 to conver to tonnes per ha
  filter(SiteRenamed == "Established") %>%
  mutate (Stock = "Belowground",
          MaxCar = max (CAR_gcm2y, na.rm = T),
          MinCar = min (CAR_gcm2y, na.rm = T)) 
  

#To create a Mean =- SE riboon add this:
#%>%  summarise(Mean_Remnant_Burial = weighted.mean(CAR_gcm2y,na.rm=T),
            N_CAR = length (CAR_gcm2y),
            SD_CAR =sd (CAR_gcm2y, na.rm=T),
            SE_CAR =sd (CAR_gcm2y, na.rm=T)/sqrt(N_CAR)) 