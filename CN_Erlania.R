#Functions to extract CN data======
#Install 2 R-packages we need if not on your comp by now:
if (!require(readxl)) library(readxl) # load that package if not done already
if (!require(tidyverse)) library(tidyverse) # load that package if not done already

#Load these packages into your working environment
library("readxl")
library("tidyverse")

#IMPORTANT:
#YOUR SAMPLE_ID (I named it CNCode) should be consistent across all your spreadsheets.

#Specify path to folder where you store youre CN spreadsheets and list them all:
files <- list.files(path = "C:/Users/BlueCarbon/Documents/00DeakinUni/R/BCL_R/TOC_Erlania/CN_DATA_Erlania",
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
setwd("C:/Users/BlueCarbon/Documents/00DeakinUni/R/BCL_R/TOC_Erlania")


#PLOT CN-DATA for Exp A=====
MASTER_DATA <- read_xlsx ("TOC_Erlania_samples.xlsx", sheet = 1) #Sheet 1 = ExpA
str(MASTER_DATA )

NewDATA <- left_join(MASTER_DATA, CN_DATA_clean, by = "CNCode")# #Merge MASTER with NewDATA:

#write.csv(NewDATA, file = "CN_Erlania_ExpA.csv")
NewDATA<- read.csv( "CN_Erlania_ExpA.csv")
names(NewDATA)

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


#PLOT CN-DATA for Exp B=====
MASTER_DATA <- read_xlsx ("TOC_Erlania_samples.xlsx", sheet = 2) #Sheet 2 = ExpB
str(MASTER_DATA )

NewDATA <- left_join(MASTER_DATA, CN_DATA_clean, by = "CNCode")# #Merge MASTER with NewDATA:
#write.csv(NewDATA, file = "CN_Erlania_ExpB.csv")

NewDATA<- read.csv( "CN_Erlania_ExpB.csv")
names(NewDATA)

options("scipen"=100, "digits"=10) # To not use E+05 abbreviation on plot
ggplot(NewDATA, aes(x = as.factor(Seaweed_Conc_.), y = C.percent), color=Species) +
  #geom_boxplot(outlier.shape = NA) +
  #facet_grid(.~Deg_Level)+
  geom_point(alpha = 0.7, aes(color=Species),size=3)+
  #geom_jitter(alpha = 0.7, aes(color=Species),size=3)+
  ylab("Organic Carbon (%)") + xlab("Seaweed Concentration (%)") +
  labs(color = "Experiment B:")+
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




#PLOT CN-DATA for Standards (SeaWeed) =====
MASTER_DATA <- read_xlsx ("TOC_Erlania_samples.xlsx", sheet = 3)
str(MASTER_DATA )

NewDATA <- left_join(MASTER_DATA, CN_DATA_clean, by = "CNCode")# #Merge MASTER with NewDATA:
#write.csv(NewDATA, file = "CN_Erlania_Standards.csv")

NewDATA<- read.csv( "CN_Erlania_ExpA.csv")
names(NewDATA)

ggplot(NewDATA, aes(x = Conc_ID, y = C.percent), color=Species) +
  #geom_point() +
  #facet_grid(.~Deg_Level)+
  geom_jitter(alpha = 0.7, aes(color=Species),size=3)+
  ylab("Organic Carbon (%)") + xlab("") +
  labs(color = "Seaweed standards:")+
  theme_bw() +
  theme(axis.text.x = element_text(size = 14,angle=90),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "bottom",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        strip.text=element_text(size=16),
        strip.background =  element_rect(fill = "white")) 
