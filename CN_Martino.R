#Functions======
#Install 2 R-packages we need if not on your comp by now:
if (!require(readxl)) library(readxl) # load that package if not done already
if (!require(tidyverse)) library(tidyverse) # load that package if not done already

#Load these packages into your working environment
library("readxl")
library("tidyverse")

#IMPORTANT:
#YOUR SAMPLE_ID (I named it CNCode) should be consistent across all your spreadsheets.

#Specify path to folder where you store youre CN spreadsheets and list them all:
files <- list.files(path = "C:/Users/BlueCarbon/Documents/00DeakinUni/R/BCL_R/FarmDams/DATA/CN_RawData",
                    pattern = "*.xls", full.names = T)
files

#Create function to export data from sheet = 1 (Sample Table)
ReadCN_smp <- function(x) read_xlsx (path = x,
                                     sheet = 1,
                                     skip = 7,
                                     range = cell_cols("C:E"))

#Export "Sample Table" data from all files in your folder:
tbl1 <- sapply(files, ReadCN_smp, simplify=FALSE) %>%
  bind_rows(.id = "id1")

tbl1 

#Create function to export data from sheet = 2 (Element%)
ReadCN <- function(x) read_xlsx (path = x,sheet = 2,skip = 7, range = cell_cols("C:D"))

#Export "Element%" data from all files in your folder using sapply:
tbl2 <- sapply(files, 
               ReadCN, simplify=FALSE) %>% 
  bind_rows(.id = "id2")

tbl2

#Bind sample (tbl1) and CN (tbl2) data together
CN_DATA <- cbind(tbl1,tbl2)#bind smp and CN data

#Double check if data alligns well:
all.equal(tbl1$id1,tbl2$id2) #should be TRUE!

#Clean up the file to keep data you need in a form you/R likes (no special signs):
CN_DATA_clean <- CN_DATA %>%
  filter(Type == "Smp") %>%
  select("id1", "Sample Name","Weight", "(N) %", "(C) %" ) %>%
  rename(file = "id1", CNCode = "Sample Name", Weight.mg = "Weight",
         N.percent = "(N) %", C.percent = "(C) %")

#Check for duplicates:
anyDuplicated(CN_DATA_clean$CNCode)#Should be 0!!!
#If not 0, some samples are dupliacted, You have to decide what to do with duplicates (e.g., average them, remove them)


#MERGE to Master File=======
#Merge new CN data with your MASTER file:
getwd()#Navigate to MASTER FILE direction;
setwd("C:/Users/BlueCarbon/Documents/00DeakinUni/R/BCL_R/FarmDams/DATA")

MASTER_DATA <- read_xlsx ("CNanalysis_FarmDamSediments.xlsx", sheet = 1)
names(MASTER_DATA )
NewDATA <- left_join(MASTER_DATA, CN_DATA_clean, by = "CNCode")

dim(MASTER_DATA)# 64  8
names(NewDATA)#64 12

#Compute corrected DBH and C-stock
NewDATA$dry_bulk_density_gcm3 <- NewDATA$DryWeight_g / 79.52 #79.52 is the core volume
NewDATA$CarbonDensityCorrected_gcm3   <- NewDATA$dry_bulk_density_gcm3 * NewDATA$C.percent/100
NewDATA$CarbonStockCorrected_Mgha     <- (((NewDATA$CarbonDensityCorrected_gcm3  / 1000000 ) *100000000) * 5 ) #5 cm core lenght
#write.csv(NewDATA, file = "CN_Martino_FarmDams.csv")

##PLOT CN-DATA for Exp A=====
names(NewDATA)
par(mfrow= c(1,2))
plot(NewDATA$MartinoStock_MgHa, main = "Martino's C-stock")
plot(NewDATA$CarbonStockCorrected_Mgha, main = "Pawel's C-stock")

plot(NewDATA$MartinoC, main = "Martino's C (%)")
plot(NewDATA$C.percent, main = "Pawel's C (%)")

ggplot(NewDATA, aes(x = Deg_Level, y = C.percent), color=Species) +
  geom_boxplot(outlier.shape = NA) +
  #facet_grid(.~Deg_Level)+
  geom_jitter(alpha = 0.7, aes(color=Species),size=3)+
  ylab("Organic Carbon (%)") + xlab("") +
  labs(color = "Experiment A:")+
  theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "bottom",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        strip.text=element_text(size=16),
        strip.background =  element_rect(fill = "white")) 


#PLOT CN-DATA for SMartino  =====
setwd("C:/Users/BlueCarbon/Documents/00DeakinUni/R/BCL_R/FarmDams/DATA")
NewDATA <- read.csv(NewDATA, file = "CN_Martino_FarmDams.csv")
NewDATA$Site <- substr(NewDATA$SiteName, 1, 4)

View(NewDATA)

ggplot(NewDATA, aes(x = C.percent, y = CarbonStockCorrected_Mgha))+
  #geom_point() +
  #facet_grid(.~Deg_Level)+
  geom_jitter(alpha = 0.7, aes(color=Site),size=3)+
  ylab("Organic Stock (Mg/Ha)") + xlab("Organic Carbon (%)") +
  labs(color = "Farm Name:")+
  theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "bottom",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        strip.text=element_text(size=16),
        strip.background =  element_rect(fill = "white")) 
